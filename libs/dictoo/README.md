# Dict-like OO Syntax


Installation using SWI-Prolog 7.1 or later:

  `?- pack_install('https://github.com/logicmoo/dictoo.git'). `


Source code available and pull requests accepted at
http://github.com/logicmoo/dictoo

```prolog
?- use_module(library(dictoo)).
true.

?- use_module(library(jpl)).
true.

?- jpl_get('java.awt.Cursor', 'NE_RESIZE_CURSOR', $cursor.value ).
true.

?- $cursor.value == 7.
true.

?- jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array.value).
true.

?- writeln($my_array.value.3 = then).
if=then
true.

?- writeln(3-5 = $my_array.value.(3-5)).
3-5=[if, then, else]
true.

?- writeln(length = $my_array.value.length).
length=9
true.


```



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

