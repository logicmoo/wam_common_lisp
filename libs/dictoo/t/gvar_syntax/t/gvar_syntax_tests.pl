
:- use_module(library(gvar_syntax)).
% :- use_module(library(must_rtrace)).

%:- debug(dictoo(goal_expand)).
% :- debug(gvs(_)).
%:- debug(dictoo(_)).

:- set_prolog_flag(must_saftey,3).
:- set_prolog_flag(must_debug,0).
:- set_prolog_flag(must_speed,0).

:- set_prolog_flag(must_type,keep_going).


:- $foo.set() = 1.


test(0):- $foo.set() = 1,  \+ ($foo.current() = 2).

test(1):- writeln($foo.current()).

test(2):- writeln($foo.get()).

test(3):- writeln($foo.clear()).

test(4):- $bar.set(2), $foo.set($bar.get()), test(1).

test(5):- writeln($foo.set(33).set(34).current()).

test(6):- $baz.set(point{x:vx,y:vy,z:vz}).

test(7):- writeln($baz.get().z).

test(8):- $baz.set(point{ x: ($foo.get()) , y:vy, z:vz}).

test(9):- writeln($baz.current().x).

test(10):- $baz.set($baz.current().put(y,yYYYY)).

test(11):- $baz.current().y == yYYYY.

all_tests:- forall(clause(test(X),Body),(dmsg(test(X)),must(Body))).

:- listing(test(_)).

