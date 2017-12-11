# Dislike having tons of forks that are several commits behind the main git repo?

(Why feel obligated to maintain a git fork just to contribute ?)

Please ask to be added to TeamSPoon !



# Releasing Common Lisp-Prolog

There're no hard rules about when to release wam_common_lisp. Release bug fixes frequently, features not so frequently and breaking API changes rarely.

### Release

Run tests, check that all tests succeed locally.

```prolog
?- run_tests(wam_common_lisp).

```

Increment the version, modify [pack.pl](pack.pl).

*  Increment the third number if the release has bug fixes and/or very minor features, only (eg. change `0.0.1` to `0.0.2`).
*  Increment the second number if the release contains major features or breaking API changes (eg. change `0.0.1` to `0.2.0`).

Remove the line with "Your contribution here.", since there will be no more contributions to this release.

Remove the "Stable Release" section in README that warns users that they are reading the documentation for an unreleased version.

Commit your changes.

```
git add README.md CHANGELOG.md pack.pl
git commit -m "Preparing for release, 0.0.2."
git push origin master
```

Release.

```
$ @TODO
 
Tagged v0.0.2.
Pushed git commits and tags.
Pushed wam_common_lisp 0.0.2 to swi-prolog.org.
```

### Prepare for the Next Version

Add the next release to [CHANGELOG.md](CHANGELOG.md).

```
Next Release
============

* Your contribution here.
```

Increment the third version number in [pack.pl](pack.pl).

Commit your changes.

```
git add CHANGELOG.md pack.pl
git commit -m "Preparing for next development iteration, 0.0.2."
git push origin master
```



[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and TeamSPoon
All rights reserved.

