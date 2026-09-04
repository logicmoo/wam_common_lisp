# xlisting/xlisting_web
Provides Cross Referenced Listing and Source Variables at Console
Manipulate and browse prolog runtime over www


Installation using SWI-Prolog 7.1 or later:

    `?- pack_install('https://github.com/logicmoo/xlisting.git'). `

Source code available and pull requests accepted at
http://github.com/logicmoo/xlisting

```prolog

?- use_module(library(xlisting)).
true.

?- xlisting(member(X,[1,2,3])).
 

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


