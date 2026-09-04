
:- use_module(library(gvar_syntax)).

:- use_module(library(dictoo)).

:- if(exists_source(library(jpl))).
:- use_module(library(jpl)).
:- endif.


:- if(exists_source(library(must_trace))).
:- use_module(library(must_trace)).
:- endif.


test(0):- jpl_get('java.awt.Cursor', 'NE_RESIZE_CURSOR', $cursor.value ).

test(1):- $cursor.value == 7.

test(2):- jpl_new(array(class([java,lang],['String'])), [for,while,do,if,then,else,try,catch,finally], $my_array.value).

test(3):- writeln($my_array.value.3 = then).

test(4):- writeln(3-5 = $my_array.value.(3-5)).

test(5):- writeln(length = $my_array.value.length).

all_tests:- forall(test(_),true).

:- listing(test(_)).

:- user: $foo.set(v3{ x: -1.0 , y:0.0, z:1.0}).

