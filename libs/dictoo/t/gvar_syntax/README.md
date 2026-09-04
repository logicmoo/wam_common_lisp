# SWI-Prolog Pack that adds a new Global Variable syntax to Prolog


# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install('https://github.com/logicmoo/gvar_syntax.git').



Source code available and pull requests accepted at
http://github.com/logicmoo/gvar_syntax

```prolog
?- use_module(library(gvar_syntax)).
true.

?- $foo.unify() = 1.
true.

?- $foo.unify() = 2.
false.

?- writeln($foo.get()).
1
true.

?- writeln($foo.get()).
1
true.

?- $foo.clear().
true.

?- writeln($foo.get()).
_8350

?- writeln($bar.set(2).get()).
2

?- $foo.set() = xxxxxxxx.
true.

?- $baz.set(point{ x: ($foo.get()) , y:vy, z:vz}).
true.

?- writeln($baz.get().x).
xxxxxxxx
true.

?- writeln($baz.x). % will error as you havent accessed the get()

```

Another Pack  called [dictoo](https://github.com/logicmoo/dictoo) 
adds better OO API on these values


# Some TODOs

Document this pack!

Write tests

Untangle the 'pack' install deps 
(Moving predicates over here from logicmoo_base)


# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to logicmoo and Contribute directly !

Still, we wont stop you from doing it the Fork+PullRequest method

# [BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
logicmoo and Douglas Miles <logicmoo@gmail.com> 
All rights reserved.


