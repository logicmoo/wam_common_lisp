/*******************************************************************
 *
 * A Lisp interpreter, written in Prolog
 *
 * (lisp_interpreter.pl)
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 * This file: implements a small amount of EVAL for bootstrapping the compiler
 *  
 *
 * Neil''s Notes:
 *
 * (c) Neil Smith, 2001
 *
 * This program is a small interpreter for Lisp.  It was written
 * in LPA Prolog v3.6, running under Windows.  It should be fairly
 * easy to convert it to other Prologs.
 *
 * It supports real Lisp syntax, excessive brackets and all.  You don't 
 * need to terminate input with a full stop.  It also understands 
 * 'x for (quote x) and #'func for (function func)
 *
 * Variables are lexically scoped, except where defined as special.
 *
 * read_words code from "The Craft of Prolog", R.A.O'Keefe
 * lisp evaluator from "Lisp" (3rd ed), Winston & Horn.
 *
 *******************************************************************/

:- module(interp, []).

:- set_module(class(library)).

:- include('header.pro').


eval(Expression, Bindings, Result):- 
   debug(lisp(eval),'~N~p -> ',[eval3(Expression, Bindings)]),
   eval3(Expression, Bindings, Result),
   debug(lisp(eval),' -> ~p ~n ',[result(Result)]),!.

eval3([defvar, Name], _, Name):-
	!,
	retract(lisp_global_bindings(GlobalBindings)),
	asserta(lisp_global_bindings([bv(Name, [[]|_])|GlobalBindings])),
	!.
eval3([setq, Name, Value], Bindings, EvalValue):-
	!,
	lisp_global_bindings(GlobalBindings),
	append(Pre, [bv(Name, [_|_])|Post], GlobalBindings),
	eval(Value, Bindings, EvalValue),
	retract(lisp_global_bindings(GlobalBindings)),
	append(Pre, [bv(Name, [EvalValue|_])|Post], GlobalBindings1),
	asserta(lisp_global_bindings(GlobalBindings1)),
	!.
eval3([defmacro, Name, FormalParms | Body0], _, Name):-
      maybe_get_docs(defun,Name,Body0,Body),
	!,
        retractall(user:macro_lambda(_Scope,Name,_,_,_)),
	asserta(user:macro_lambda(defmacro,Name, FormalParms, Body,AlphaRenames)),
	!.
eval3([defun, Name, FormalParms, Body0], _, Name):-
        maybe_get_docs(defun,Name,Body0,Body),
	!,        
        retractall(named_lambda(Name, [lambda, _, _])),
	asserta(named_lambda(Name, [lambda, FormalParms, Body])),
	!.

eval3(['$BQ',One], Bindings, Out):-!, expand_commas(Bindings,One,Out).
eval3([('`'),One], Bindings, Out):-!, expand_commas(Bindings,One,Out).

eval3([apply|Arguments], Bindings, Result):-
	!,
	evalL(Arguments, Bindings, [Function, ActualParams]),
	apply_f(Bindings,Function, ActualParams, Result),
	!.


eval3([function, [lambda,  FormalParams, Body]], Bindings, 
		[closure, FormalParams, Body, Bindings]):-!.

eval3(['eval*',Arguments], Bindings, Result):-!,
   eval(Arguments, Bindings, MResult),
   eval(MResult,Result).
eval3(['eval*'|Arguments], Bindings, OutR):-!,
   evalL(Arguments, Bindings, MResults),
   evalL(MResults, [none], Results),
   once(last(Results,OutR);OutR=[]).

eval3([Procedure|Arguments], Bindings, Result):-  lisp_operator(Procedure),!,
  apply_f(Bindings,Procedure, Arguments, Result),
  !.

eval3([Procedure|Arguments], Bindings, Result):-
  user:macro_lambda(_Scope,Procedure, FormalParams, LambdaExpression,AlphaRenames),
  bind_formal_parameters(FormalParams, Arguments, Bindings, NewBindings),
  eval(['eval*'|LambdaExpression], NewBindings, Result).

eval3([Procedure|Arguments], Bindings, Result):-
	evalL(Arguments, Bindings, EvalArguments),
	apply_f(Bindings,Procedure, EvalArguments, Result),
	!.
eval3(X, Bindings, Val):-
   zotrace((atom(X),
      (member(bv(X, [Val|_]), Bindings)
	;	(lisp_global_bindings(GlobalBindings),
		 member(bv(X, [Val|_]), GlobalBindings)))
	)),!.
eval3(X, _, X):- zotrace(is_self_evaluationing_object(X)),!.
eval3(X, Bindings, Y):- \+ is_list(X),compound(X),!,X=..XL,eval3(XL, Bindings, Y).
eval3(X, _, []):- (debugging(lisp(eval))->dumpST;true),
	write('SYSTEM::READ-EVAL-PRINT: variable '),write(X),write(' has no value'),nl,!.



evalL([], _, []):-!.
evalL([H|T], Bindings, [EvalH|EvalT]):-
	eval(H, Bindings, EvalH),
	evalL(T, Bindings, EvalT),
	!.

pf_car(A,Out):- \+ is_list(A),type_error(list,A,car(A),pf_car(A,Out)).
pf_car([A|_],A).
pf_car(_,[]).

apply_f(_Binds,function, [A],[function,A]).
apply_f(_Binds,car, LIST, Result):-!,(LIST=[[Result|_]]->true;Result=[]).
apply_f(_Binds,cdr, LIST, Result):-!,(LIST=[[_|Result]]->true;Result=[]).
apply_f(_Binds,list, Args, Args):-!.
apply_f(_Binds,cons, [Arg1, Arg2], [Arg1|Arg2]):-!.
apply_f(_Binds,eq, [Arg1, Arg2], Result):-
	(Arg1 = Arg2 -> Result = Arg1 
		      ; Result = []),
	!.
apply_f(Bindings,if_wrong, [Test, Success, Failure], Result):-
	eval(Test,Bindings, TestResult),
	eval(Success,Bindings, EvalSuccess),
	eval(Failure,Bindings, EvalFailure),
	(TestResult = [] -> Result = EvalFailure
			  ; Result = EvalSuccess),
	!.
apply_f(Bindings,if, [Test, Success, Failure], Result):-  !,
	eval(Test, Bindings, TestResult),
	(TestResult \== [] -> eval(Success, Bindings, Result)
			  ; eval(Failure, Bindings, Result)),
	!.
apply_f(Binds,[lambda, FormalParams, Body], ActualParams, Result):-
	!,
	bind_formal_parameters(FormalParams, ActualParams,Binds, Bindings),!,
	eval(Body, Bindings, Result),
	!.
apply_f(_Binds,[closure, FormalParams, Body, Bindings0], ActualParams, Result):-
	!,
	bind_formal_parameters(FormalParams, ActualParams, Bindings0, Bindings),
	eval(Body, Bindings, Result),
	!.

apply_f(_Binds,ProcedureName, ActualParams, Result):-
	user:macro_lambda(_Scope,ProcedureName,FormalParams, LambdaExpression,AlphaRenames),!,
	bind_formal_parameters(FormalParams, ActualParams, Bindings),
        eval(LambdaExpression, Bindings, Result),
	!.
apply_f(Bindings,ProcedureName, Args, Result):-
	named_lambda(ProcedureName, LambdaExpression),!,
	apply_f(Bindings,LambdaExpression, Args, Result),
	!.

apply_f(_,=,[X,Y],R):-!, X \= Y -> R=[] ; R=t.
apply_f(_,-,[X,Y],R):-!, R is X - Y.
apply_f(_,'1+',[X],R):-!, R is X + 1.
apply_f(_,+,[X,Y],R):-!, R is X + Y.
apply_f(_,F,ARGS,R):- atom(F),append(ARGS,[R],RARGS),length(RARGS,A),current_predicate(F/A),!,apply(F,RARGS),!.
apply_f(_,F,ARGS,R):- atom(F),CALL=..[F|ARGS],current_predicate(_,CALL),!,(catch(CALL,E,(dumpST,dmsg(CALL->E),!,fail))->R=t;R=[]).
apply_f(Binds,X, _, R):- ignore(R=[]),
        (debugging(lisp(eval))->dumpST;true),
	write('ERROR!  Cannot apply a procedure description for `'),
	write(X),
	write(''''),nl,
        write('Binds'=Binds),nl,
        
	!.



/*
:- eval_string("(defun append (x y) (if x (cons (car x) (append (cdr x) y))  y))").
:- eval_string("(defmacro foo (a) `,a)").
:- eval_string("(defmacro fooq (a) `',a)").
:- eval_string("(fooq b)").
:- eval_string("(nodebug)").
:- eval_string("(debug)").
*/



:- fixup_exports.

