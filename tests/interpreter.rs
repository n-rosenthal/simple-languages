//! Testes do interpretador interativo, pela API pública.

use simple_languages::common::driver::Command;
use simple_languages::common::interpreter::{
    Interpreter, Reply, ReplyKind, MAX_FUEL, MAX_INPUT_CHARS, MAX_NESTING,
};
use simple_languages::registry;

fn session(language: &str) -> Box<dyn Interpreter> {
    registry::session(language).expect("registered language")
}

fn ok(reply: Reply) -> String {
    assert_ne!(reply.kind, ReplyKind::Error, "unexpected error: {}", reply.text);
    reply.text
}

fn err(reply: Reply) -> String {
    assert_eq!(reply.kind, ReplyKind::Error, "expected an error, got: {}", reply.text);
    reply.text
}

// --- modos e comandos --------------------------------------------------------

#[test]
fn the_default_mode_is_full() {
    let mut s = session("stlc");
    assert_eq!(s.mode(), Command::Full);
    assert!(ok(s.submit("λx:A. x")).contains("== type =="));
}

#[test]
fn colon_commands_switch_modes() {
    let mut s = session("arith");

    assert_eq!(ok(s.submit(":small")), "mode: small");
    assert_eq!(s.mode(), Command::Small);
    assert_eq!(ok(s.submit("1 + 2")), "(1 + 2)\n→ 3  [E-BinConst]\n");

    assert_eq!(ok(s.submit(":mode type")), "mode: type");
    assert!(ok(s.submit("1 + 2")).starts_with("type: Integer\n"));

    assert_eq!(ok(s.submit(":mode")), "mode: type");
    err(s.submit(":mode nope"));
}

#[test]
fn unknown_commands_are_errors() {
    let mut s = session("arith");
    assert!(err(s.submit(":frobnicate")).contains("unknown command `:frobnicate`"));
}

#[test]
fn quit_sets_the_flag() {
    let mut s = session("arith");
    assert!(s.submit(":quit").quit);
    assert!(s.submit(":q").quit);
    assert!(!s.submit("1").quit);
}

#[test]
fn blank_lines_are_ignored() {
    let mut s = session("arith");
    let reply = s.submit("   ");
    assert_eq!(reply.kind, ReplyKind::Info);
    assert!(reply.text.is_empty());
}

#[test]
fn help_mentions_the_language_and_definitions_only_where_supported() {
    let stlc = ok(session("stlc").submit(":help"));
    let arith = ok(session("arith").submit(":help"));

    assert!(stlc.starts_with("stlc:") && stlc.contains("name = <term>"));
    assert!(arith.starts_with("arith:") && !arith.contains("name = <term>"));
}

// --- fuel --------------------------------------------------------------------

#[test]
fn fuel_is_validated() {
    let mut s = session("stlc");

    assert_eq!(ok(s.submit(":fuel 50")), "fuel: 50");
    assert_eq!(s.fuel(), 50);
    assert_eq!(ok(s.submit(":fuel")), "fuel: 50");

    err(s.submit(":fuel 0"));
    err(s.submit(":fuel abc"));
    err(s.submit(&format!(":fuel {}", MAX_FUEL + 1)));
    assert_eq!(s.fuel(), 50);
}

#[test]
fn set_fuel_clamps() {
    let mut s = session("stlc");
    assert_eq!(s.set_fuel(0), 1);
    assert_eq!(s.set_fuel(usize::MAX), MAX_FUEL);
}

#[test]
fn fuel_stops_a_divergent_term() {
    let mut s = session("stlc");
    s.submit(":small");
    s.submit(":fuel 20");

    let out = ok(s.submit("(λx:A. x x) (λx:A. x x)"));
    assert!(out.ends_with("(out of fuel)\n"), "{out}");
}

#[test]
fn long_traces_are_summarized() {
    let mut s = session("stlc");
    s.submit(":small");

    let out = ok(s.submit("(λx:A. x x) (λx:A. x x)"));
    assert!(out.contains("more steps omitted"), "{out}");
    assert!(out.lines().count() < 130);
}

// --- definições --------------------------------------------------------------

#[test]
fn definitions_are_expanded_in_later_lines() {
    let mut s = session("stlc");

    assert_eq!(ok(s.submit("id = λx:A. x")), "id = λx:A. x");
    s.submit(":small");
    assert_eq!(
        ok(s.submit("id (λy:A. y)")),
        "(λx:A. x) (λy:A. y)\n→ λy:A. y  [E-AppAbs]\n"
    );
}

#[test]
fn definitions_can_use_earlier_definitions() {
    let mut s = session("stlc");
    s.submit("id = λx:A. x");
    assert_eq!(ok(s.submit("twice_id = λy:A. id y")), "twice_id = λy:A. (λx:A. x) y");

    s.submit(":type");
    assert!(ok(s.submit("twice_id")).starts_with("type: A->A\n"));
}

#[test]
fn bound_variables_shadow_definitions() {
    let mut s = session("stlc");
    s.submit("x = λz:A. z");
    s.submit(":parse");

    // o `x` ligado pelo λ não é substituído; o `x` livre depois dele, sim
    assert_eq!(ok(s.submit("(λx:B. x) x")), "(λx:B. x) (λz:A. z)\n");
}

#[test]
fn redefining_a_name_replaces_it() {
    let mut s = session("stlc");
    s.submit("f = λx:A. x");
    s.submit("f = λy:B. y");

    assert_eq!(s.definitions(), vec![("f".to_string(), "λy:B. y".to_string())]);
}

#[test]
fn defs_and_reset() {
    let mut s = session("stlc");
    assert_eq!(ok(s.submit(":defs")), "no definitions");

    s.submit("a = λx:A. x");
    s.submit("b = λy:B. y");
    assert_eq!(ok(s.submit(":defs")), "a = λx:A. x\nb = λy:B. y\n");

    assert_eq!(ok(s.submit(":reset")), "definitions cleared");
    assert!(s.definitions().is_empty());
}

#[test]
fn definitions_report_syntax_errors_and_missing_terms() {
    let mut s = session("stlc");
    assert!(err(s.submit("f = λx. x")).starts_with("syntax error:"));
    assert!(err(s.submit("f =")).contains("missing term"));
    assert!(s.definitions().is_empty());
}

#[test]
fn arith_has_no_definitions() {
    let mut s = session("arith");
    assert!(!s.supports_definitions());
    assert!(err(s.submit("x = 1")).contains("no variables"));
}

#[test]
fn equality_in_arith_is_not_a_definition() {
    let mut s = session("arith");
    s.submit(":big");
    assert!(ok(s.submit("true == (1 < 2)")).starts_with("value: true\n"));
}

// --- exemplos ----------------------------------------------------------------

#[test]
fn examples_are_listed_and_runnable() {
    for language in registry::names() {
        let mut s = session(language);
        let examples = s.examples();
        assert!(!examples.is_empty(), "{language}");

        let listing = ok(s.submit(":examples"));
        assert!(listing.contains(examples[0].source), "{language}");

        for n in 1..=examples.len() {
            let reply = s.submit(&format!(":example {n}"));
            assert!(
                reply.text.starts_with(&format!("> {}", examples[n - 1].source)),
                "{language} #{n}: {}",
                reply.text
            );
        }
    }
}

#[test]
fn a_bad_example_number_is_an_error() {
    let mut s = session("stlc");
    err(s.submit(":example 0"));
    err(s.submit(":example 99"));
    err(s.submit(":example x"));
}

#[test]
fn every_example_runs_in_every_mode_without_panicking() {
    for language in registry::names() {
        let mut s = session(language);
        s.set_fuel(500);

        for command in Command::ALL {
            s.set_mode(command);
            for example in s.examples() {
                s.submit(example.source);
            }
        }
    }
}

// --- erros e limites ---------------------------------------------------------

#[test]
fn syntax_errors_are_errors() {
    let mut s = session("arith");
    assert!(err(s.submit("1 +")).starts_with("syntax error:"));
    assert!(err(s.submit("foo")).contains("unknown word `foo`"));
}

#[test]
fn type_errors_surface_in_type_mode() {
    let mut s = session("arith");
    s.submit(":type");
    assert!(err(s.submit("true + 1")).starts_with("type error:"));
}

#[test]
fn stuck_terms_are_output_not_errors() {
    let mut s = session("arith");
    s.submit(":small");
    assert!(ok(s.submit("true + 1")).ends_with("(stuck)\n"));
}

#[test]
fn oversized_input_is_rejected() {
    let mut s = session("arith");
    let long = "1 + ".repeat(MAX_INPUT_CHARS) + "1";
    assert!(err(s.submit(&long)).contains("too long"));
}

#[test]
fn deep_nesting_is_rejected_before_parsing() {
    let mut s = session("arith");
    let deep = "(".repeat(MAX_NESTING + 1) + "1" + &")".repeat(MAX_NESTING + 1);
    assert!(err(s.submit(&deep)).contains("nested"));

    let fine = "(".repeat(MAX_NESTING) + "1" + &")".repeat(MAX_NESTING);
    s.submit(":parse");
    assert_eq!(ok(s.submit(&fine)), "1\n");
}

#[test]
fn the_longest_allowed_chains_do_not_overflow_the_stack() {
    // Em build debug os quadros são muito maiores que em release (que é o que
    // roda no navegador, e passa até com 1 MiB de pilha), então este teste usa
    // uma thread com pilha folgada.
    std::thread::Builder::new()
        .stack_size(64 << 20)
        .spawn(|| {
            // 1 + 1 + ... + 1 em ~2000 caracteres: uma árvore de ~500 níveis
            let mut arith = session("arith");
            let chain = vec!["1"; MAX_INPUT_CHARS / 4].join(" + ");
            assert!(chain.chars().count() <= MAX_INPUT_CHARS);
            ok(arith.submit(&chain));

            // λx:A. λx:A. ... com o maior número de binders que cabe
            let mut stlc = session("stlc");
            let binders = "λx:A. ".repeat((MAX_INPUT_CHARS - 1) / 6) + "x";
            assert!(binders.chars().count() <= MAX_INPUT_CHARS);
            ok(stlc.submit(&binders));
        })
        .unwrap()
        .join()
        .unwrap();
}

#[test]
fn sessions_are_independent() {
    let mut a = session("stlc");
    let b = session("stlc");

    a.submit("f = λx:A. x");
    a.set_fuel(7);

    assert!(b.definitions().is_empty());
    assert_ne!(b.fuel(), 7);
}

#[test]
fn language_metadata() {
    let s = session("stlc");
    assert_eq!(s.language(), "stlc");
    assert!(s.description().contains("lambda calculus"));
    assert!(s.supports_definitions());
}
