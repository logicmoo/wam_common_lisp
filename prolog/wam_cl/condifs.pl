/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 * Changes since 2001:
 *
 *
 *******************************************************************/
:- module(c0ndif, []).
:- set_module(class(library)).
:- include('header').
:- ensure_loaded(utils_for_swi).


:- discontiguous(compile_condifs/5).

must_compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody):-
  always(compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody)),!.

:- discontiguous(compile_test_body/6).

% IF (null ...)
compile_test_body(Ctx,Env,TestResult,[null,Test],TestBody,TestResultBody):-
   debug_var("TestNullResult",TestResult),
   must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
   TestResultBody = (TestResult == []).

if_op_1_then_test(stringp,is_stringp).
if_op_1_then_test(symbolp,is_symbolp).
if_op_1_then_test(OP,PRED):- 
  find_lisp_function(OP,1,FUN),
  is_defined(FUN,2),   
  PFUN=..[FUN,A1,RET],
  clause_interface(PFUN,t_or_nil(PPRED,Ret)),
  PPRED=..[PRED,AA1],
  RET==Ret,AA1==A1.
compile_test_body(Ctx,Env,Unused,[OP,Arg1],Arg1Body,PredBody):- if_op_1_then_test(OP,Pred),   
   must_compile_body(Ctx,Env,Arg1Result,Arg1,Arg1Body),
   debug_var("Unused",Unused),
   debug_var("PredArgResult",Arg1Result),
   PredBody =.. [Pred,Arg1Result].

if_oper_then_test(OP,A1,PredBody):- 
  find_lisp_function(OP,1,FUN),
  is_defined(FUN,2),   
  PFUN=..[FUN,A1,RET],
  clause_interface(PFUN,t_or_nil(PredBody,Ret)),
  RET==Ret.
compile_test_body(Ctx,Env,Unused,[OP,Arg1],Arg1Body,PredBody):- if_oper_then_test(OP,Arg1Result,PredBody),   
   must_compile_body(Ctx,Env,Arg1Result,Arg1,Arg1Body),
   debug_var("Unused",Unused),debug_var("PredArgResult",Arg1Result).
   


if_op_2_then_test('=','=:=').
if_op_2_then_test(OP,PRED):- 
  find_lisp_function(OP,2,FUN),
  is_defined(FUN,3),   
  PFUN=..[FUN,A1,A2,RET],
  clause_interface(PFUN,t_or_nil(PPRED,Ret)),
  PPRED=..[PRED,AA1,AA2],
  RET==Ret,AA1==A1,AA2==A2.
%cl_eq(A,B,Ret):- t_or_nil( is_eq(A,B) , Ret).
compile_test_body(Ctx,Env,Unused,[OP,Arg1,Arg2],(Arg1Body,Arg2Body),PredBody):- if_op_2_then_test(OP,Pred),
   must_compile_body(Ctx,Env,Arg1Result,Arg1,Arg1Body),
   must_compile_body(Ctx,Env,Arg2Result,Arg2,Arg2Body),
   debug_var("Unused",Unused),
   debug_var("PredArg1Result",Arg1Result),
   debug_var("PredArg2Result",Arg2Result),
   PredBody =.. [Pred,Arg1Result,Arg2Result].

% Default TEST compilation
compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody):-
   debug_var("GTestResult",TestResult),
   must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
   TestResultBody = (TestResult \== []).


% IF-3
compile_condifs(Ctx,Env,Result,[if, Test, IfTrue, IfFalse], Body):-
	!,
   debug_var("IFTEST",TestResult),
   must_compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody),
   must_compile_body(Ctx,Env,TrueResult,IfTrue, TrueBody),
   must_compile_body(Ctx,Env,FalseResult,IfFalse, FalseBody),
   debug_var("TrueResult",TrueResult),
   debug_var("ElseResult",FalseResult),

        Body = (	TestBody,
			( TestResultBody
				->     ( TrueBody,
					Result      = TrueResult)
				;  	(FalseBody,
					Result      = FalseResult)	) ).



%   (case A ((x...) B C...)...)  -->
%   (let ((@ A)) (cond ((memv @ '(x...)) B C...)...))
compile_condifs(Ctx,Env,Result,[CASE,VarForm|Clauses], Body):-  member(CASE,[case,ecase,ccase]),
  must_compile_body(Ctx,Env,Key,VarForm,VarBody),
   debug_var('Key',Key),
   make_holder(SOf),    
   preserved_prolog_var(Key),

   cases_to_conds(SOf,Key,Clauses,Conds),
   nb_holder_value(SOf,Values),
   (CASE\==case -> (Values==t -> true ; nb_set_last_tail(Conds,[[t,['type_error',Key,[quote,[member|Values]]]]])) ; true),
   dbginfo(CASE=Clauses),dbginfo(conds=Conds),
   (CASE==ccase -> (make_restartable_block(Key,[cond|Conds],LispCode),must_compile_body(Ctx,Env,Result,LispCode, Body0)) ;
     compile_condifs(Ctx,Env,Result,[cond|Conds], Body0)),
   Body = (VarBody,Body0).

cases_to_conds(_SOf,_,[],[]) :- !.
cases_to_conds(SOf,_,[[otherwise|Tail]],  [[t,[progn|Tail]]]):- nb_holder_setval(SOf,t).
cases_to_conds(SOf,_,[[t|Tail]],  [[t,[progn|Tail]]]):- nb_holder_setval(SOf,t).
cases_to_conds(SOf,V,[[One|Tail]|Tail2], [[['eq',V,[quote,Realy1]],[progn|Tail]]|X]) :- is_list(One),One=[Realy1],nb_holder_append(SOf,Realy1),
    cases_to_conds(SOf,V,Tail2,X).
cases_to_conds(SOf,V,[[Set|Tail]|Tail2], [[['sys_memq',V,[quote,Set]],[progn|Tail]]|X]) :- \+ atomic(Set),nb_holder_append(SOf,[or|Set]),
    cases_to_conds(SOf,V,Tail2,X).
cases_to_conds(SOf,V,[[Item|Tail]|Tail2], [[['eq',V,[quote,Item]],[progn|Tail]]|X]) :- nb_holder_append(SOf,Item),
   cases_to_conds(SOf,V,Tail2,X).

preserved_prolog_var(Key):- put_attr(Key,preserved_var,t).

% Macro TYPECASE, CTYPECASE, ETYPECASE
%   (typecase A ((x...) B C...)...)  -->
%   (let ((@ A)) (cond ((memv @ '(x...)) B C...)...))
compile_condifs(Ctx,Env,Result,[CASE,VarForm|Clauses], Body):-  member(CASE,[typecase,etypecase,ctypecase]),
  must_compile_body(Ctx,Env,Key,VarForm,VarBody),
   debug_var('Key',Key),
   preserved_prolog_var(Key),
   make_holder(SOf),    
   typecases_to_conds(SOf,Key,Clauses,Conds),
   nb_holder_value(SOf,Values),
   (CASE\==typecase -> (Values==t -> true ; nb_set_last_tail(Conds,[[t,['type_error',Key,[quote,[or|Values]]]]])) ; true),
   dbginfo(CASE:-Clauses),dbginfo(conds:-Conds),
   (CASE==ctypecase -> (make_restartable_block(Key,[cond|Conds],LispCode),must_compile_body(Ctx,Env,Result,LispCode, Body0)) ;
     compile_condifs(Ctx,Env,Result,[cond|Conds], Body0)),
   Body = (VarBody,Body0).

typecases_to_conds(_SOf,_,[],[]) :- !.
typecases_to_conds(SOf,_,[[otherwise|Tail]],  [[t,[progn|Tail]]]):- nb_holder_setval(SOf,t).
typecases_to_conds(SOf,_,[[t|Tail]],  [[t,[progn|Tail]]]):- nb_holder_setval(SOf,t).
typecases_to_conds(SOf,V,[[One|Tail]|Tail2], [[['typep',V,[quote,Realy1]],[progn|Tail]]|X]) :- is_list(One),One=[Realy1],nb_holder_append(SOf,Realy1),
    typecases_to_conds(SOf,V,Tail2,X).
typecases_to_conds(SOf,V,[[Set|Tail]|Tail2], [[['typep',V,[quote,[or|Set]]],[progn|Tail]]|X]) :- \+ atomic(Set),nb_holder_append(SOf,[or|Set]),
    typecases_to_conds(SOf,V,Tail2,X).
typecases_to_conds(SOf,V,[[Item|Tail]|Tail2], [[['typep',V,[quote,Item]],[progn|Tail]]|X]) :- nb_holder_append(SOf,Item),
   typecases_to_conds(SOf,V,Tail2,X).


% COND
compile_condifs(_Cx,_Ev,Result,[cond ], Result=[]):- !.
compile_condifs(_Cx,_Ev,Result,[cond,[] ], Result=[]):- !.
compile_condifs(Ctx,Env,Result,[cond, [t|Progn]|_], Body):-   
  must_compile_progn(Ctx,Env,Result, Progn, Body).
compile_condifs(Ctx,Env,Result,[cond, [Test|ResultForms] |Clauses], Body):- 
  compile_condifs(Ctx,Env,Result,[if,Test,[progn|ResultForms],[cond |Clauses]], Body).


make_holder('$hldr'([])).
nb_set_last_tail(HT,Last):- HT=[_|T], ((compound(T),functor(T,_,2)) -> nb_set_last_tail(T,Last);  nb_setarg(2,HT,Last)).
nb_holder_append(Obj,Value):- is_list(Obj),!,nb_set_last_tail(Obj,[Value]). 
nb_holder_append(Obj,Value):- functor(Obj,_,A),arg(A,Obj,E),
  (E==[] -> nb_setarg(A,Obj,[Value]) ;
   (is_list(E)-> nb_set_last_tail(E,[Value]) ; nb_setarg(A,Obj,[E,Value]))).
nb_holder_setval(Obj,Value):- is_list(Obj),!,nb_setarg(1,Obj,Value),nb_setarg(2,Obj,[]).
nb_holder_setval(Obj,Value):- functor(Obj,_,A),nb_setarg(A,Obj,Value).
nb_holder_value(Obj,Value):- is_list(Obj),!,([Value]=Obj->true;Value=Obj).
nb_holder_value(Obj,Value):- functor(Obj,_,A),arg(A,Obj,Value). 


:- fixup_exports.

