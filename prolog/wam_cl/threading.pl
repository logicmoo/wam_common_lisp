/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(thread1ng, []).

:- include('./header').

:- meta_predicate(call_engine(?,0,-,-)).
call_engine(Templ,Goal,Engine,Det):-
  call_engine_start(Templ,Goal,Engine),
  call_engine_next(Engine,Templ,Det).

:- meta_predicate(call_engine_start(?,0,-)).
call_engine_start(Templ,Goal,Engine):-
   engine_create(Templ-TF0,(Goal,deterministic(TF0)),Engine).

call_engine_next(Engine,Templ,Det):-
   repeat,
    engine_next(Engine,Templ-Det),
     (Det==true->!;true).



:- fixup_exports.
end_of_file.






