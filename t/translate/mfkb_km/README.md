# KM — The Knowledge Machine (source, tests & docs)

KM is a powerful, frame-based knowledge representation language with clear
first-order logic semantics. It supports reasoning by selection-by-description,
unification, automatic classification, and reasoning about actions via a
*situations* mechanism. Its origins were the Theo language and the (now
obsolete) KRL. KM is implemented in Common Lisp.

- **Authors:** Peter Clark (`peterc@allenai.org`) and Bruce Porter (`porter@cs.utexas.edu`)
- **Upstream:** <http://www.cs.utexas.edu/users/mfkb/km/>
- **Licence:** Simplified BSD (see `LICENCE.txt`) — Copyright (c) 1994–2015 Peter Clark & Bruce Porter.
- **Warranty:** none. Free software; redistributable under the conditions in the licence.

These files were downloaded from the upstream site for use as translation
sources / test fixtures.

---

## Files in this folder

### Engine source — packaged vs. unpackaged

Every release ships in two single-file flavours. They are **identical Lisp
code**; the only difference is the Common Lisp *package* declarations:

| Flavour | Filename pattern | Package declarations | When to use |
|---|---|---|---|
| **Packaged** (recommended) | `km-X-Y-Z-packaged.lisp` | Active — KM lives in its own Lisp package | Normal use; keeps KM's symbols from clashing with your own. Good practice. |
| **Unpackaged** | `km-X-Y-Z.lisp` | Commented out — everything loads into the current package | Only for users unfamiliar with Lisp packages. Upstream **discourages** this. |

Both are machine-built concatenations of the ~20 constituent KM source files
(each original file marked by a `;;; FILE: <name>` header inside). They load and
compile directly; you do **not** need to split them up to run KM.

### Versions included

| Version | Packaged | Unpackaged | Build date | Notes |
|---|---|---|---|---|
| **2.5.45** (latest / recommended) | `km-2-5-45-packaged.lisp` | `km-2-5-45.lisp` | 2015-11-07 | Bug fixes for CLisp compatibility. |
| **2.5.43** | `km-2-5-43-packaged.lisp` | `km-2-5-43.lisp` | 2013-12-14 | "Rebundle the release" (2.5.42 = bug fixes + efficiency; 2.5.41 = `locked-instance-of`). |
| **2.5.33** | `km-2-5-33-packaged.lisp` | `km-2-5-33.lisp` | 2011-04-19 | First release under the Simplified BSD licence. |
| **2.4.6** | `km-2-4-6-packaged.lisp` | `km-2-4-6.lisp` | — | Older 2.4.x line. |
| **2.4.0** | `km-2-4-0-packaged.lisp` | `km-2-4-0.lisp` | — | Semantics change: `instances of` / `all-instances` exclude protoinstances. |

### Tests & documentation

| File | Description |
|---|---|
| `test-suite.km` | Official test suite for the current release; includes the examples from the manuals. Loaded from within KM. |
| `km-overview.script` | Annotated sample KM session illustrating the language's main features (mirrors the tutorial). |
| `RELEASE-NOTES.txt` | Full changelog — see "Reading the changelog" below. |
| `LICENCE.txt` | Simplified BSD licence text. |

### Manuals, tutorials & reference

| File | Description |
|---|---|
| `userman.pdf` | KM (v2.0+) **Users Manual** (Clark & Porter). |
| `situations.pdf` | KM (v1.4+) **Situations Manual** — reasoning about/comparing situations. |
| `km-reference-sheet.doc` | Quick reference sheet (Word format). |
| `km-tutorial-2006f.ppt` | KM Tutorial, Fall 2006 (main features). |
| `km-tutorial-2005f.ppt` | KM Tutorial, Fall 2005 (older version). |
| `km-algorithm.ppt` | Top-level description of KM's inference algorithm. |
| `situations-implementation.pdf` | Note on the implementation of KM's situation mechanism. |

### Background papers

| File | Reference |
|---|---|
| `cg97.pdf` / `cg97.PPT` | Clark & Porter, *Using Access Paths to Guide Inference with Conceptual Graphs*, ICCS'97. Details KM's inference algorithm (backward chaining + classification). |
| `aaai97.pdf` | Clark & Porter, *Building Concept Representations from Reusable Components*, AAAI'97. |
| `aarati.pdf` / `aarati.ppt` | Parmar, *The Representation of Actions in KM and Cyc* (Stanford TR FRG-1, 2001). |

> Note: the `cg97.*` and `aaai97.*` papers and the `cg97.PPT` presentation are no
> longer at their original UT-Austin URLs; they were retrieved from the Internet
> Archive Wayback Machine. All other files came directly from
> <http://www.cs.utexas.edu/users/mfkb/km/>. Files ≥10 MB (none applied here) were
> intentionally skipped.

---

## Running KM

Save/point Lisp at one of the engine files (packaged version recommended):

```lisp
> (load "km-2-5-45-packaged.lisp")
> (km)          ; start the query interpreter
KM>
```

For speed, compile once and load the faster compiled output afterwards:

```lisp
> (compile-file "km-2-5-45-packaged.lisp")
> (load "km-2-5-45-packaged")
```

To run the bundled tests, load the engine, then load `test-suite.km` (it uses
`(nocomments)` and `(fail-noisily)` so NIL answers are reported as errors).

> **Allegro 8.2+ note (KM 2.5.0+):** any *Lisp* file that uses KM's `#$` dispatch
> macro must first set the readtable:
> ```lisp
> (eval-when (:execute :load-toplevel :compile-toplevel)
>   (setq *readtable* *km-readtable*))   ; use km::*km-readtable* from another package
> ```
> At the REPL, run `(hash-dollar)` after loading KM to enable `#$`. This is not
> needed for `.km` KB files. See `RELEASE-NOTES.txt` for details.

---

## Reading the changelog (`RELEASE-NOTES.txt`)

The changelog uses a versioning convention that tells you *how much changed*:

- **2nd-level bumps** (e.g. `2.4 → 2.5`): existing **semantics/behaviour changed**
  relative to the KM User Manual. These are the important ones — the notes list
  all `2.*.0` changes together at the top of the file.
- **3rd-level bumps** (e.g. `2.5.4 → 2.5.5`): **added functionality or bug fixes**;
  the full per-release list is in the second half of the file.

Highlights relevant to the versions here (newest first):

- **2.5.45** — bug fixes for compatibility with CLisp.
- **2.5.43** — rebundle the release.
- **2.5.42** — bug fixes and efficiency optimizations.
- **2.5.41** — added `locked-instance-of`: prevents *heuristic* (set) unification
  from removing a locked class from an instance (forced unification still works).
- **2.5.34–40** — minor efficiency improvements.
- **2.5.33** — re-release under the Simplified BSD licence.
- **2.5.0** — Allegro 8.2 `#$` readtable requirement (see note above).
- **2.4.0** — `(the instances of <class>)` / `all-instances` etc. no longer
  include protoinstances (only affects users of KM's prototype mechanism).

For anything else, open `RELEASE-NOTES.txt`: the top section summarizes the
behaviour-changing `2.*.0` releases, and the lower section lists every 3rd-level
release in descending order.

---

## Support tools & further reading (upstream)

- **KMgen** — ontology editor for KM: <http://www.algo.be/cl/KMgen/download.htm>
- **Manuals** — User Manual and Situations Manual (PDF/PS) at the upstream site.
- **Mailing list / Q&A** — KM Users Group: <http://groups.google.com/group/km-qa>
