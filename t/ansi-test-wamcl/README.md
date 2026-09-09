# WAM-CL ANSI test adapter

This directory adapts the maintained Paul Dietz ANSI Common Lisp tests in
[`../ansi-test`](../ansi-test) to WAM-CL without modifying the vendored suite.
The exact upstream repository, revision, and license are recorded in
[`upstream-revision.txt`](upstream-revision.txt).

## Current baseline

The first executable slice is `ansi-test/iteration/loop.lsp`. Its seven
ordinary `LOOP` tests pass:

```text
SLOOP.1 PASS
SLOOP.2 PASS
SLOOP.3 PASS
SLOOP.4 PASS
SLOOP.5 PASS
SLOOP.6 PASS
SLOOP.7 PASS
```

Run the slice from the repository root:

```powershell
swipl -s t\ansi-test-wamcl\run.pl -g run_tests,halt -t "halt(1)"
```

The command fails if the child WAM-CL process fails, a selected test is
missing, a test reports an error or wrong value, or not all seven result
markers are present. It also enforces a three-minute child timeout and rejects
any failed bootstrap form.

Each run writes a full Markdown transcript and a machine-readable summary to
`results/latest.md` and `results/latest.json`. These generated files are
ignored by Git; [`BASELINE.md`](BASELINE.md) records the reviewed baseline.

## Adapter design

`run.pl` concatenates the required upstream bootstrap files and feeds them to
a fresh WAM-CL process one form at a time. This avoids WAM-CL's current crash
when these files are loaded through `CL:LOAD`.

`child.pl` evaluates registered tests only after the REPL reaches end of file.
Dynamic `EVAL` currently interferes with an active stdin reader, so this keeps
test execution isolated from source ingestion. It captures all returned
values and compares them with the regression framework's
`EQUALP-WITH-CASE`.

`loop-prelude.lsp` contains only the `DEF-MACRO-TEST` registration helper
needed by `iteration/loop.lsp`. The generated error tests are intentionally
not selected yet because they require `SIGNALS-ERROR` and complete condition
handling. They must not be counted as passes until those facilities work.

## Random auxiliary bootstrap

```powershell
swipl -s t\ansi-test-wamcl\run-random.pl -g run_random_tests,halt -t "halt(1)"
```

This gate concatenates the unmodified shared sources through `universe.lsp`
and `auxiliary/random-aux.lsp`, followed by `random-bootstrap.lsp`. It requires
all **8 source-end markers**, **zero failed forms**, and **9 passing assertions**.
The assertions exercise both macro expansion and execution of `RCASE` and
`RANDOM-CASE`, cumulative weights, sequence selection, and universe values.
The child timeout is ten minutes; `results/random-latest.log` is ignored.

The runtime fixes include checked random-state copying and isolation,
`INTEGER-LENGTH`, `BUTLAST`, `EVERY`/`SOME`, and a bounded `COERCE` subset.
`ASSERT` supports empty/omitted places, custom conditions, and retry through
the `CONTINUE` restart; interactive place correction is not implemented and
nonempty places are rejected. `COERCE` supports the simple string/list/vector
type names, real-to-float conversions, character designators, and functions;
this is not a complete implementation of compound type specifiers.
Coercion preserves existing sequence identity when the supported target type
already matches; lambda coercion creates a runtime closure that receives
argument data without re-evaluating it. `EVERY` and `SOME` return exactly one
value, even when their predicates return multiple values.

Compiler diagnostics no longer install printed, numbered-variable clauses.
Top-level macros (including those in top-level `PROGN`, active `EVAL-WHEN`,
and macro expansions) are installed by compilation, independent of verbosity.
Nested `DEFMACRO` forms install their definitions only when executed. `LIST`
allocates fresh cons cells at runtime instead of retaining compiler argument
templates. `TYPEP` uses the correct argument order and subtype checks cannot
backtrack through failed class lookups to spuriously succeed.

## Next compliance barrier

Passing these gates does not establish full ANSI compliance or a successful
load of the complete shared `ansi-aux.lsp`/symbol-name/notes bootstrap.
The full auxiliary gate must be checked separately before enabling the
maintained suite's generated macro-error tests.

A quiet, bounded full-load probe reached `auxiliary/ansi-aux.lsp:548`,
`(defparameter *displaced* (make-int-array 100000))`, after seven complete
upstream source markers and zero failed forms, but did not finish within
240 seconds. A prefix probe verified the base-, standard-, and code-character
sequence lengths as **95**, **96**, and **256**, respectively. Accessing those
global strings with `ELT` still fails: globals can contain native Prolog
strings, but `f_elt/3` delegates to `get_adata/2`, which lacks that representation.
The same probe also stalled on a three-element `MAKE-INT-ARRAY`; the full-load
stall therefore cannot be attributed to large-array performance alone.
Direct three-element construction, `FUNCALL` construction, and `f_aset/4`
storage probes succeed, leaving the Lisp-level array-update/iteration path
to investigate. Neither this prefix nor the full shared bootstrap is a
passing gate. Diagnostic transcripts are retained in ignored
`results/ansi-aux-latest.log` and `results/ansi-aux-prefix.log`.

The project sanity suite currently passes **256/256** assertions (including
**123** bootstrap assertions, up from 47); the maintained LOOP adapter remains
**7/7**, independently of the nine random-bootstrap assertions above.
