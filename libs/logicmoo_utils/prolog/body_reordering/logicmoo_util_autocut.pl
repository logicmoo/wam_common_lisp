/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/
:- if(\+ current_predicate(system:call_using_first_responder/1)).
:- module(logicmoo_util_autocut, [call_using_first_responder/1]).
:- endif.

% :- '$set_source_module'(system).

:- if(\+ current_predicate(system:call_using_first_responder/1)).
:- user:ensure_loaded(logicmoo_util_autocut).
:- endif.

:- export(call_using_first_responder/1).
:- meta_predicate(call_using_first_responder(0)).

call_using_first_responder(Call):- clause(Call, Body),
  Responded = responded(_), Cutted = was_cut(_),
  CheckCut = (ignore(deterministic(HasCut)), (HasCut=true->nb_setarg(1, Cutted, cut);true)),

  clause(Call, Body),
  \+ ground(Cutted),
  (FakeBody = (Body;fail)),

  ((( (call((FakeBody, CheckCut)), nb_setarg(1, Responded, answered)) *-> true ; (CheckCut, fail))
     ; (CheckCut, ground(Responded), ((HasCut==true->!;true)), fail))).


 % one_must(C1, one_must(C2, one_must(C3, one_must(C4, C5)))).



call_using_first_responder(Goal) :-
	predicate_property(Goal, built_in), % <--- check for a built in predicate
	!, call(Goal).
call_using_first_responder(Goal) :-
        Responded = responded(_), %  Cutted = was_cut(_),

	clause(Goal, Body), % <--- assume anything else is interpreted
	do_body(Body, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		do_body(AfterCut)
	;   HadCut = no
	).


do_body(Body) :-
	do_body(Body, AfterCut, HadCut),
	(   HadCut = yes,
		!,
		do_body(AfterCut)
	;   HadCut = no
	).


do_body((!, AfterCut), AfterCut, yes) :- !.
do_body((Goal, Body), AfterCut, HadCut) :- !,
	call_using_first_responder(Goal),
	do_body(Body, AfterCut, HadCut).
do_body(!, true, yes).
do_body((Disj1;_), AfterCut, HadCut) :-
	do_body(Disj1, AfterCut, HadCut).
do_body((_;Disj2), AfterCut, HadCut) :- !,
	(do_body(Disj2, AfterCut, HadCut)*->true;AfterCut=fail).
do_body(Goal, TF, no) :-
	(call_using_first_responder(Goal)*->TF=true;TF=fail).



last_clause(Any, Result):- (call(Any), deterministic(Det))*->(Det==true->Result=!;Result=true);Result=fail.
last_clause(Any):- call(Any), dmsg(error(cont_first_responder(Any))).

goal_expansion(last_clause(Any), (call(Any), deterministic(yes)->!;true)).


:- fixup_exports.

:- if(true).
% some tests

a:- !, fail.
a:- throw(failed_test).
fr1:- \+ call_using_first_responder(a).


b:- !.
b:- throw(failed_test).
fr2:- call_using_first_responder(b).

wa(A):-writeln(A), asserta(A).

c:- wa(c(1)).
c:- !, (wa(c(2));wa(c(3))).
c:- throw(failed_test).
fr3:- call_using_first_responder(c).

d:- wa(d(1));(wa(d(2));wa(d(3))).
d:- throw(failed_test).
fr4:- call_using_first_responder(d).

e:- wa(c(1)).
e:- last_clause(wa(c(2));wa(c(3))).
e:- throw(failed_test).

fr5:- \+ (e, fail).

:- endif.

%:- break.


