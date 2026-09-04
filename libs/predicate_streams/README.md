# Implement your own Abstract Predicate Streams

18+ Years ago I remember these predicates existed as the building 
blocks for writing your own Sockets in some Prolog I cannot remember.


Installation using SWI-Prolog 7.1 or later:

   `?- pack_install('https://github.com/logicmoo/predicate_streams.git').`


Source code available and pull requests accepted at http://github.com/logicmoo/predicate_streams

# Example usages

```prolog
?- with_output_to_predicate({}/[X]>>assert(saved_output(X)),
     (write("hi there"),nl,writeln("how are you?"))),
     listing(saved_output/1).

saved_output("hi there\n").
saved_output("how are you?\n").
```

```prolog
?- with_input_from_predicate(=('hello.\n'), read(World)).
World = hello.
```

@ BUG
```prolog
% this works 
?- call(({}/[X]>>(repeat,X='Y')),Y).
Y = 'Y' ;
Y = 'Y' ;
Y = 'Y' ;
Y = 'Y' ;
Y = 'Y' ;
Y = 'Y' .

% but not this ?
?- with_input_from_predicate(({}/[X]>>(repeat,X='YN')),(get_char(C0),get_char(C1),get_char(C2),get_char(C3))).
C0 = 'Y',
C1 = 'N',
C2 = C3, C3 = end_of_file.

expected 
C0 = C2, C1 = C3,
C2 = 'Y',
C3 = 'N'.

% Auto presses Y Multiple times
?- with_input_from_predicate({}/[X]>>X='YYYYYYYYYYYYYYYYYYYYYYYY', poor_interactive_goal).


```

```prolog

?- with_error_to_predicate(write,threads).
... writes thread info to stdout instead of stderr...

```

```prolog
?- with_output_to_predicate(print_as_html_pre,
    (writeln("hi there"),writeln("how are you?"))).

<pre>hi there
</pre>
<pre>how are you?
</pre>
```


# Some TODOs

Document this pack!
Write tests


[BSD 2-Clause License](LICENSE.md)

Copyright (c) 2017, 
Douglas Miles <logicmoo@gmail.com> and logicmoo
All rights reserved.

# Not _obligated_ to maintain a git fork just to contribute

Dislike having tons of forks that are several commits behind the main git repo?

Be old school - Please ask to be added to logicmoo and Contribute directly !
Still, we wont stop you from doing it the Fork+PullRequest method




