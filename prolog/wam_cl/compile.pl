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
:- module(comp, []).
:- set_module(class(library)).
:- include('header').

lisp_compiled_eval(SExpression):-
  quietly(as_sexp_interned(SExpression,Expression)),
  lisp_compiled_eval(Expression,Result),
  userout(result(Result)).

lisp_compiled_eval(SExpression,Result):-
  lquietly(as_sexp_interned(SExpression,Expression)),
  %dbginfo(lisp_compiled_eval(Expression)),
  always(lisp_compile(Result,Expression,Code)),  
  % dbginfo((lisp_compiled_eval(Expression):- Code)),
  (always((Code))),!.

%lisp_compile(SExpression):- source_location(_,_),!,dbginfo((:-lisp_compile(SExpression))).
lisp_compile(SExpression):-
  quietly(as_sexp_interned(SExpression,Expression)),
  userout(:- lisp_compile(Expression)),
  lisp_compile(Expression,Code),!,
  userout(:- Code).

lisp_compile(SExpression,Body):-
   quietly(as_sexp_interned(SExpression,Expression)),
   debug_var('_Ignored',Result),
   lisp_compile(Result,Expression,Body).

lisp_compile(Result,SExpression,Body):-
   debug_var('TLEnv',Env),
   lisp_compile(Env,Result,SExpression,Body).

lisp_compile(Env,Result,Expression,Body):-
   ensure_env(Ctx),
   always(lisp_compile(Ctx,Env,Result,Expression,Body)).

lisp_compile(Ctx,Env,Result,SExpression,Body):-
   ensure_env(Ctx),
   quietly(as_sexp(SExpression,Expression)),
   always(compile_forms(Ctx,Env,Result,[Expression],Body)).
   


compile_forms(Ctx,Env,Result,FunctionBody,Code):-
   must_compile_progn(Ctx,Env,Result,FunctionBody, [], Body),!,
   body_cleanup_keep_debug_vars(Ctx,Body,Code).

:- nop( debug_var('FirstForm',Var)),
   nb_linkval('$compiler_PreviousResult',the(Var)).




p_or_s([F|Args],F0,Args0):-!,F0=F,Args0=Args.
p_or_s(POrSTerm,F,Args):- POrSTerm=..[F|Args].

% same_symbol(OP1,OP2):-!, OP1=OP2.
same_symbol(OP1,OP2):- quietly(same_symbol0(OP1,OP2)).

%prologcase_name_or_string(S,N):-prologcase_name(S,N).

same_symbol0(OP1,OP2):- var(OP1),var(OP2),trace_or_throw(same_symbol(OP1,OP2)).
same_symbol0(OP1,OP2):- var(OP1),!,same_symbol0(OP2,OP1).
same_symbol0(OP1,OP2):- var(OP2),!,freeze(OP2,((nonvar(OP2),same_symbol(OP1,OP2)))).

same_symbol0(OP1,OP2):- string(OP1),to_prolog_string(OP2,N2),!,OP1==N2.
same_symbol0(OP1,OP2):- string(OP2),!,same_symbol0(OP2,OP1).

same_symbol0(OP1,OP2):- atom(OP1),atom(OP2),!, same_reduced_atoms(OP1,OP2),!.
same_symbol0(P,OP2):- compound(P),!,arg(1,P,OP1),same_symbol0(OP1,OP2).
same_symbol0(OP1,P):- compound(P),!,arg(1,P,OP2),same_symbol0(OP1,OP2).

same_reduced_atoms(X,X).
same_reduced_atoms(X,Y):- reduce_atom(X,XX),X\==XX,!,same_reduced_atoms(XX,Y).
same_reduced_atoms(Y,X):- reduce_atom(X,XX),X\==XX,!,same_reduced_atoms(Y,XX).

reduce_atom(X,XX):- atom(X),reduce_atom0(X,XX),XX\==''.
reduce_atom0(X,XX):- downcase_atom(X,XX)->X\==XX.
%reduce_atom(X,XX):- atom_concat_or_rtrace('%',XX,X).
%reduce_atom(X,XX):- atom_concat_or_rtrace('$',XX,X).
reduce_atom0(X,XX):- prologcase_name(X,XX)->X\==XX.
reduce_atom0(X,XX):- atom_concat_or_rtrace(':',XX,X).
reduce_atom0(X,XX):- atom_concat_or_rtrace('cl_',XX,X).
reduce_atom0(X,XX):- atom_concat_or_rtrace('f_',XX,X).
/*
reduce_atom(X,XX):- atom_concat_or_rtrace('u_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace('kw_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace('sys_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace('ext_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace(XX,'_mexpand1',X).
*/


:- '$hide'(same_symbol/2).



get_value_or_default(Ctx,Name,Value,IfMissing):- oo_get_attr(Ctx,Name,Value)->true;Value=IfMissing.

get_alphas(Ctx,Alphas):- get_tracker(Ctx,Ctx0),get_alphas0(Ctx0,Alphas).
get_alphas0(Ctx,Alphas):- get_value_or_default(Ctx,alphas,Alphas,[]).

add_alphas(_,_):-!.
add_alphas(Ctx,Alphas):- always((get_tracker(Ctx,Ctx0),add_alphas0(Ctx0,Alphas))).
add_alphas0(Ctx,Alpha):- atom(Alpha),!,get_value_or_default(Ctx,alphas,Alphas,[]),oo_put_attr(Ctx,alphas,[Alpha|Alphas]).
add_alphas0(_Ctx,Alphas):- \+ compound(Alphas),!.
add_alphas0(Ctx,Alphas):- Alphas=..[_|ARGS],maplist(add_alphas0(Ctx),ARGS).

f_sys_memq(E,L,R):- t_or_nil((member(Q,L),Q==E),R).



must_compile_progn(Ctx,Env,Result,FormsIn, PreviousResult, Body):-
  %quietly((maybe_debug_var('_rCtx',Ctx),
  %maybe_debug_var('_rEnv',Env),
  %maybe_debug_var('_rResult',Result),
  %maybe_debug_var('_rPrevRes',PreviousResult),
 % maybe_debug_var('_rForms',Forms),
  %maybe_debug_var('_rBody',Body))),
  lquietly(resolve_reader_macros(FormsIn,Forms)),!,
   always(((compile_progn(Ctx,Env,Result,Forms,PreviousResult,Body0),nonvar(Body0)))),
   lquietly((sanitize_true(Ctx,Body0,Body))).

compile_progn(_Cx,_Ev,Result,Var,_PreviousResult,Out):- quietly(is_ftVar(Var)),!,Out=cl_eval([progn|Var],Result).
compile_progn(_Cx,_Ev,Result,[], PreviousResult,true):-!, PreviousResult = Result.
compile_progn(Ctx,Env,Result,[Form | Forms], PreviousResult, Body):-  !,
	must_compile_progbody(Ctx,Env,FormResult, Form,PreviousResult,FormBody),
	must_compile_progn(Ctx,Env,Result, Forms, FormResult, FormSBody),
        Body = (FormBody,FormSBody).
compile_progn(Ctx,Env,Result, Form , PreviousResult, Body):-
        % locally(
  % local_override('$compiler_PreviousResult',the(PreviousResult)),
       must_compile_progbody(Ctx,Env,Result,Form,PreviousResult, Body).


% Compiler Plugin
must_compile_progbody(Ctx,Env,Result,Form,PreviousResult,FormBody):-  
	shared_lisp_compiler:plugin_expand_progbody(Ctx,Env,Result,Form,PreviousResult,FormBody),!.
must_compile_progbody(Ctx,Env,Result,Form,_PreviousResult,FormBody):-
        % locally(
  % local_override('$compiler_PreviousResult',the(PreviousResult)),
	must_compile_body(Ctx,Env,Result,Form,FormBody).
        %).




tst:is_local_test("
(defun sum_with_map (xs)
  (let (( running_total 0))
    (let ((summer
    (function
       (lambda (n)
        (setq running_total (+ running_total n))))))
       (mapcar summer  xs) running_total)))
 "
  ).

tst:is_local_test("(defun accumulate (op seq &optional (init 0)) (if (null seq) init (funcall op (car seq) (accumulate op (cdr seq) init))))").


:- fixup_exports.
