
:- include(sanity_tests).

:- use_module(library(attvar_reader)).

:- read_attvars(false).

test(0):- 
  deserialize_attvars(sk_in(avar([vn='Ex'], [sk='SKF-666'])),O),
  copy_term(O,OO,PP),display(O),nl,display(OO),nl,display(PP),nl.

:- read_attvars(true).

test(2):- 
  deserialize_attvars(sk_in(avar([vn='Ex'], [sk='SKF-666'])),O),
  copy_term(O,OO,PP),display(O),nl,display(OO),nl,display(PP),nl.

:- read_attvars(false).
