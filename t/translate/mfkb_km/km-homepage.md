# KM: The Knowledge Machine

> Markdown conversion of the KM homepage
> (<https://www.cs.utexas.edu/~mfkb/km.html>).
>
> **Link convention:** links written as bare filenames (e.g. `userman.pdf`) are
> **local** — the file lives in this folder. Full `http(s)://` links are
> **external**. Formats that were intentionally not kept locally (e.g. the
> `.ps` / `.ps.Z` PostScript copies of files we have as PDF) point to their
> original external URLs.

KM is a powerful, frame-based language with clear first-order logic semantics.
It contains sophisticated machinery for reasoning, including selection by
description, unification, classification, and reasoning about actions using a
situations mechanism. Its origins were the Theo language and the (now obsolete)
language KRL. It is implemented in Lisp.

---

## Tutorials

- **[KM Tutorial (Fall 2006)](km-tutorial-2006f.ppt)** (PowerPoint), covering the main features of KM.
  (an older version of this tutorial is also available **[here](km-tutorial-2005f.ppt)**)
- **[km-overview.script](km-overview.script)** — an annotated, sample session with KM, illustrating some of the language's main features. (Approximately the same script used throughout the tutorial above.)

## Manuals

- KM (v2.0 and later): **Users Manual**. P. Clark and B. Porter.
  (**[pdf](userman.pdf)**,
   [postscript](https://www.cs.utexas.edu/~mfkb/km/userman.ps),
   [compressed postscript](https://www.cs.utexas.edu/~mfkb/km/userman.ps.Z))
- KM (v1.4 and later): **Situations Manual**. P. Clark and B. Porter.
  (**[pdf](situations.pdf)**,
   [compressed postscript](https://www.cs.utexas.edu/~mfkb/km/situations.ps.Z))
  This describes KM's mechanism for reasoning about and comparing different *situations*.
- **[RELEASE-NOTES](RELEASE-NOTES.txt)** — important release notes! Documents extensions and changes to KM since the manuals were written.
- **[Quick Reference Sheet for KM (Word format)](km-reference-sheet.doc)**

## Software

KM is released under the Simplified BSD Licence. A copy of the licence is
below and also bundled with the software. If you would like a copy under a
different licencing agreement, please contact one of the authors. KM comes with
ABSOLUTELY NO WARRANTY. This is free software, and you are welcome to
redistribute it under certain conditions. See the LICENCE for details.

**Important!** Please see the RELEASE-NOTES for changes made to KM since the manuals were written.

- **[LICENCE.txt](LICENCE.txt)** — a copy of the Simplified BSD Licence, under which KM is released.
- **[km-2-5-45-packaged.lisp](km-2-5-45-packaged.lisp)** — the very latest version of KM! (in one single file) **(Recommended version to use)**.
- **[RELEASE-NOTES.txt](RELEASE-NOTES.txt)** — important release notes. Documents changes to KM since the manuals were written.
- **[test-suite.km](test-suite.km)** — test suite (for current release), includes the examples from the manuals.

### Alternative packagings of KM 2.5.45

- **[km-2-5-45.lisp](km-2-5-45.lisp)** — single file version, **unpackaged** release, for users not familiar with packages (identical to the packaged release above except the package declarations are commented out). Use of this is discouraged, as using packages is good practice. For information on how to use packages, see:
  - [Chapter 21: Programming in the Large: Packages and Symbols (Practical Common Lisp, Peter Seibel)](http://www.gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html)
  - [The Complete Idiot's Guide to Common Lisp Packages (Erann Gat)](http://www.flownet.com/gat/packages.pdf)

### Older versions (archived locally)

Not listed on the upstream page, but retrieved for reference. Each ships in
**packaged** and **unpackaged** form (see `README.md` for details):

| Version | Packaged | Unpackaged |
|---|---|---|
| 2.5.43 | [km-2-5-43-packaged.lisp](km-2-5-43-packaged.lisp) | [km-2-5-43.lisp](km-2-5-43.lisp) |
| 2.5.33 | [km-2-5-33-packaged.lisp](km-2-5-33-packaged.lisp) | [km-2-5-33.lisp](km-2-5-33.lisp) |
| 2.4.6  | [km-2-4-6-packaged.lisp](km-2-4-6-packaged.lisp)   | [km-2-4-6.lisp](km-2-4-6.lisp) |
| 2.4.0  | [km-2-4-0-packaged.lisp](km-2-4-0-packaged.lisp)   | [km-2-4-0.lisp](km-2-4-0.lisp) |

## Support Tools

- **[KMGen](http://www.algo.be/cl/KMgen/download.htm)** — a very nice **ontology editor** for the KM language. See these **[screenshots](http://www.algo.be/cl/KMgen/snapshots.htm)** showing some of its features and the Component Library in KMgen. Created by, and with thanks to, Francis Leboutte.

## KM Mailing List

You can stay up-to-date with KM, post and answer questions, be alerted to new
releases of KM, etc. by joining the KM Users Group hosted by Google Groups at
<http://groups.google.com/group/km-qa>.

## Additional Background Information

- **[KM's Algorithm](km-algorithm.ppt)** (PowerPoint), providing a top-level description of KM's inference algorithm.
- P. Clark and B. Porter. **Using Access Paths to Guide Inference with Conceptual Graphs.** In *Proc Int Conf on Conceptual Structures — ICCS'97 (Lecture Notes in AI vol 1257)*, pages 521–535, Eds: D. Lukose, H. Delugach, M. Keeler, L. Searle, J. Sowa, Berlin: Springer, 1997.
  This provides details of KM's inferencing algorithm, and gives examples of how backward chaining (path following) and automatic classification interleave to solve problems.
  (**[PDF](cg97.pdf)**,
   [compressed postscript](https://www.cs.utexas.edu/users/pclark/papers/cg97.ps.Z),
   **[animated PowerPoint slide presentation](cg97.PPT)**,
   [HTML slide presentation](https://www.cs.utexas.edu/users/pclark/presentations/cg97))
- **[The implementation of situations](situations-implementation.pdf)** (pdf) — a brief note on the implementation of KM's situation mechanism.
- P. Clark and B. Porter. **Building Concept Representations from Reusable Components.** In *AAAI'97*, pages 369–376, CA: AAAI Press, 1997.
  (**[PDF](aaai97.pdf)**,
   [compressed postscript](https://www.cs.utexas.edu/users/pclark/papers/aaai97.ps.Z)).
  Includes an illustrated example of KM's interleaving of backward chaining and classification.
- **The Representation of Actions in KM and Cyc** — a comparison by Aarati Parmar (Stanford University Technical Report FRG-1, 2001).
  (**[PDF report](aarati.pdf)** and **[PowerPoint presentation](aarati.ppt)**).

## Other

- [How to use Lisp on the UT/CS machines.](http://www.cs.utexas.edu/users/mooney/cs351/lisp.html)
- [**Back to KBS Group Home Page**](https://www.cs.utexas.edu/users/mfkb/index.html)

---

Maintained upstream by [porter@cs.utexas.edu](https://www.cs.utexas.edu/users/porter).
