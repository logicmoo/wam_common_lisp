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

## Next compliance barrier

Implement `HANDLER-BIND`, `HANDLER-CASE`, and condition construction, then
load the upstream `SIGNALS-ERROR` helper and enable the LOOP macro-error tests.
After that, extend the manifest by chapter while keeping each file
subprocess-isolated.
