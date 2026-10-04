// Confere, com o KaTeX embutido, todo o LaTeX que o interpretador gera para a
// web. Lê as linhas JSON de `cargo run -q --example dump_latex`.
//
//     cargo run -q --example dump_latex | node web/tools/check-latex.mjs

import { createRequire } from "node:module";
import readline from "node:readline";

const require = createRequire(import.meta.url);
const katex = require("../www/vendor/katex/katex.min.js");

const failures = [];
const counts = new Map();
let total = 0;

const lines = readline.createInterface({ input: process.stdin });
for await (const line of lines) {
  if (!line.trim()) continue;
  const item = JSON.parse(line);
  total++;

  const key = `${item.language}/${item.origin === "syntax" || item.origin === "rules" ? item.origin : "output"}`;
  counts.set(key, (counts.get(key) ?? 0) + 1);

  try {
    // `strict: "error"` também reprograma avisos (comandos não suportados, etc.)
    katex.renderToString(item.web, { displayMode: true, throwOnError: true, strict: "error", trust: false });
  } catch (error) {
    failures.push({ ...item, error: String(error.message ?? error) });
  }
}

for (const [key, n] of [...counts].sort()) console.log(`${String(n).padStart(4)}  ${key}`);
console.log(`${total} fórmulas, ${failures.length} com erro`);

for (const f of failures.slice(0, 8)) {
  console.log(`\n[${f.language} / ${f.origin}] ${f.source}\n  ${f.error}\n  ${f.web.slice(0, 200)}`);
}
process.exit(failures.length ? 1 : 0);
