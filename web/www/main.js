// Front-end do interpretador de linguagens do TAPL.
//
// Toda a lógica (parser, tipos, semânticas, máquina) roda em Rust, compilado
// para WebAssembly. Este arquivo só liga a interface a `Playground.submit`.

import init, { Playground, languages, modes } from "./pkg/tapl_web.js";

const $ = (id) => document.getElementById(id);

const el = {
  language: $("language"),
  mode: $("mode"),
  fuel: $("fuel"),
  log: $("log"),
  form: $("form"),
  line: $("line"),
  prompt: $("prompt"),
  description: $("description"),
  examples: $("examples"),
  definitionsPanel: $("definitions-panel"),
  definitions: $("definitions"),
  share: $("share"),
  clear: $("clear"),
  insertLambda: $("insert-lambda"),
};

let playground = null;
let lastTerm = "";

// --- histórico de linhas (setas ↑ ↓) ----------------------------------------

const inputHistory = [];
let cursor = 0; // posição ao navegar; `inputHistory.length` = linha em edição
let draft = "";

function remember(line) {
  if (inputHistory[inputHistory.length - 1] !== line) inputHistory.push(line);
  cursor = inputHistory.length;
  draft = "";
}

// --- utilidades --------------------------------------------------------------

function fill(select, values) {
  select.replaceChildren(
    ...values.map((value) => {
      const option = document.createElement("option");
      option.value = option.textContent = value;
      return option;
    }),
  );
}

async function copyText(text) {
  try {
    await navigator.clipboard.writeText(text);
    return true;
  } catch {
    // contexto sem permissão para a área de transferência (http fora de localhost)
    const area = document.createElement("textarea");
    area.value = text;
    area.style.position = "fixed";
    area.style.opacity = "0";
    document.body.append(area);
    area.select();
    const ok = document.execCommand("copy");
    area.remove();
    return ok;
  }
}

function readHash() {
  return Object.fromEntries(new URLSearchParams(location.hash.slice(1)));
}

function shareUrl() {
  const params = new URLSearchParams({
    lang: playground.language(),
    mode: playground.mode(),
    fuel: String(playground.fuel()),
  });
  const term = el.line.value.trim() || lastTerm;
  if (term) params.set("q", term);
  return `${location.href.split("#")[0]}#${params}`;
}

// --- console -----------------------------------------------------------------

function scrollToEnd() {
  el.log.scrollTop = el.log.scrollHeight;
}

function addOutput(parent, kind, text) {
  const pre = document.createElement("pre");
  pre.className = `out ${kind}`;
  pre.textContent = text.replace(/\n$/, "");

  const copy = document.createElement("button");
  copy.type = "button";
  copy.className = "copy";
  copy.textContent = "copiar";
  copy.addEventListener("click", async () => {
    copy.textContent = (await copyText(text)) ? "copiado" : "falhou";
    setTimeout(() => (copy.textContent = "copiar"), 1200);
  });

  pre.append(copy);
  parent.append(pre);
}

function addEntry(input, kind, text) {
  const entry = document.createElement("div");
  entry.className = "entry";

  if (input !== null) {
    const row = document.createElement("div");
    row.className = "in";
    const prompt = document.createElement("span");
    prompt.className = "prompt";
    prompt.textContent = "❯";
    row.append(prompt, document.createTextNode(input));
    entry.append(row);
  }
  if (text) addOutput(entry, kind, text);

  el.log.append(entry);
  scrollToEnd();
}

function clearLog() {
  el.log.replaceChildren();
}

// --- painel lateral ----------------------------------------------------------

function syncControls() {
  el.mode.value = playground.mode();
  el.fuel.value = playground.fuel();
  el.prompt.textContent = `${playground.mode()} ❯`;
}

function renderExamples() {
  const flat = playground.examples();
  el.examples.replaceChildren();

  for (let i = 0; i < flat.length; i += 2) {
    const [title, source] = [flat[i], flat[i + 1]];

    const item = document.createElement("li");
    item.className = "example";

    const head = document.createElement("div");
    head.className = "title";
    const label = document.createElement("span");
    label.textContent = title;
    const run = document.createElement("button");
    run.type = "button";
    run.textContent = "▶";
    run.title = "Executar";
    run.addEventListener("click", () => execute(source));
    head.append(label, run);

    const code = document.createElement("code");
    code.textContent = source;
    code.title = "Copiar para a linha de comando";
    code.addEventListener("click", () => {
      el.line.value = source;
      el.line.focus();
    });

    item.append(head, code);
    el.examples.append(item);
  }
}

function renderDefinitions() {
  const supported = playground.supportsDefinitions();
  el.definitionsPanel.hidden = !supported;
  if (!supported) return;

  const flat = playground.definitions();
  el.definitions.replaceChildren();

  if (flat.length === 0) {
    const empty = document.createElement("li");
    empty.className = "definition";
    empty.textContent = "nenhuma — digite nome = termo";
    el.definitions.append(empty);
    return;
  }

  for (let i = 0; i < flat.length; i += 2) {
    const item = document.createElement("li");
    item.className = "definition";
    const code = document.createElement("code");
    code.textContent = `${flat[i]} = ${flat[i + 1]}`;
    code.title = "Inserir o nome na linha de comando";
    code.addEventListener("click", () => insertAtCaret(flat[i]));
    item.append(code);
    el.definitions.append(item);
  }
}

function insertAtCaret(text) {
  const { selectionStart: start, selectionEnd: end, value } = el.line;
  el.line.value = value.slice(0, start) + text + value.slice(end);
  el.line.setSelectionRange(start + text.length, start + text.length);
  el.line.focus();
}

// --- execução ----------------------------------------------------------------

function execute(line) {
  line = line.trim();
  if (!line) return;

  remember(line);

  let kind;
  let text;
  try {
    const reply = playground.submit(line);
    kind = reply.kind;
    text = reply.text;
    reply.free();
  } catch (error) {
    kind = "error";
    text =
      `erro interno: ${error}\n` +
      "A sessão foi reiniciada; se o problema persistir, recarregue a página.";
    openLanguage(playground.language(), {}, { quiet: true });
  }

  addEntry(line, kind, text);
  syncControls();
  renderDefinitions();

  if (!line.startsWith(":")) lastTerm = line;
}

function openLanguage(name, params = {}, { quiet = false } = {}) {
  playground?.free();
  playground = new Playground(name);

  el.language.value = name;
  el.description.textContent = playground.description();

  if (params.mode) {
    try {
      playground.setMode(params.mode);
    } catch {
      /* modo inválido no link: usa o padrão */
    }
  }
  const fuel = Number(params.fuel);
  if (Number.isFinite(fuel) && fuel > 0) playground.setFuel(fuel);

  syncControls();
  renderExamples();
  renderDefinitions();

  if (!quiet) {
    clearLog();
    inputHistory.length = 0;
    cursor = 0;
    lastTerm = "";
    addEntry(null, "info", `${name}: ${playground.description()}\nDigite um termo, ou :help.`);
  }
}

// --- eventos -----------------------------------------------------------------

el.form.addEventListener("submit", (event) => {
  event.preventDefault();
  const line = el.line.value;
  el.line.value = "";
  execute(line);
});

el.line.addEventListener("input", () => {
  // `\` vira λ (mesmo comprimento, então o cursor não se move)
  if (el.line.value.includes("\\")) {
    const caret = el.line.selectionStart;
    el.line.value = el.line.value.replaceAll("\\", "λ");
    el.line.setSelectionRange(caret, caret);
  }
});

el.line.addEventListener("keydown", (event) => {
  if (event.key === "ArrowUp") {
    if (cursor === inputHistory.length) draft = el.line.value;
    if (cursor > 0) el.line.value = inputHistory[--cursor];
    event.preventDefault();
  } else if (event.key === "ArrowDown") {
    if (cursor < inputHistory.length) {
      cursor++;
      el.line.value = cursor === inputHistory.length ? draft : inputHistory[cursor];
    }
    event.preventDefault();
  } else if (event.ctrlKey && event.key.toLowerCase() === "l") {
    clearLog();
    event.preventDefault();
  }
});

el.insertLambda.addEventListener("click", () => insertAtCaret("λ"));

el.language.addEventListener("change", () => openLanguage(el.language.value));

el.mode.addEventListener("change", () => {
  playground.setMode(el.mode.value);
  syncControls();
});

el.fuel.addEventListener("change", () => {
  el.fuel.value = playground.setFuel(Number(el.fuel.value) || 0);
});

el.clear.addEventListener("click", () => {
  clearLog();
  el.line.focus();
});

el.share.addEventListener("click", async () => {
  const url = shareUrl();
  window.history.replaceState(null, "", url);
  const ok = await copyText(url);
  addEntry(null, "info", ok ? "link copiado" : `link: ${url}`);
});

// --- início ------------------------------------------------------------------

function fatal(error) {
  const box = document.createElement("div");
  box.className = "fatal";

  const title = document.createElement("h2");
  title.textContent = "Não foi possível carregar o WebAssembly";

  const how = document.createElement("p");
  how.append(
    "Compile o módulo e sirva a pasta por HTTP (não por file://): ",
    Object.assign(document.createElement("code"), {
      textContent: "wasm-pack build web --target web --out-dir www/pkg --release",
    }),
  );

  const detail = document.createElement("pre");
  detail.textContent = String(error);

  box.append(title, how, detail);
  document.body.replaceChildren(box);
}

async function main() {
  try {
    await init();
  } catch (error) {
    fatal(error);
    return;
  }

  const names = languages();
  fill(el.language, names);
  fill(el.mode, modes());

  const params = readHash();
  openLanguage(names.includes(params.lang) ? params.lang : names[0], params);

  if (params.q) execute(params.q);
  el.line.focus();
}

main();
