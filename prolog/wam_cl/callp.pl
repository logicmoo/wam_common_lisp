/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (builtin_lisp_functions.pl)
 *
 * (c) Neil Smith, 2001
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(callp,[]).

:- set_module(class(library)).

:- include('header').


cl:cl_trace(t):- rtrace,trace.

/*
TODO fix prolog_call

tst:is_local_test(block2,[block,block2,[tagbody,setq(b,2),[go,tag2],setq(a,1),(tag1),
                     prolog_call([a,b],plus(a,b,C)),prolog_call(writeln(C)),
                     'return-from'(block2,c),(tag2),setq(a,4),[go,tag1]]],6).
*/

shared_lisp_compiler:plugin_expand_progbody(Ctx,Env,Result,InstrS,Prev,Code):- 
  compile_prolog_call(Ctx,Env,Result,InstrS,Prev,Code),!.


compile_prolog_subst_call(Prev,Ctx,Env,ResultOut,[],Body,BodyOut):-
  subst(Body,'$out',ResultOut,BodyMid),
  expand_prolog(Prev,Ctx,Env,BodyMid,BodyOut).


compile_prolog_subst_call(Prev,Ctx,Env,ResultOut,[R|Resolve],Body,(Code,BodyResolved)):-
  subst(Body,R,Result,BodyMid),
  must_compile_body(Ctx,Env,Result,R,Code),
  compile_prolog_subst_call(Prev,Ctx,Env,ResultOut,Resolve,BodyMid,BodyResolved).


expand_prolog(Prev,_Ctx,Env,Term0,Term):-
  subst(Term0,'$env',Env,Term1),subst(Term1,'$in',Prev,Term).

read_prolog_from_lisp('$OBJ'(claz_prolog,Term),Term):-!.
read_prolog_from_lisp(Call,Term):- to_prolog_string(Call,Str),read_term_from_atom(Str,Term,[]).

 :- discontiguous compile_prolog_call/6.
% (prolog-trace)
% (sys:trace)
wl:interned_eval('`sys:prolog-trace').
wl:interned_eval('`sys:trace').
compile_prolog_call(_Ctx,_Env,Prev,[sys_prolog_trace],Prev, trace).
compile_prolog_call(_Ctx,_Env,Prev,[sys_trace],Prev, trace).

% (sys:prolog)
% (sys:break)
wl:interned_eval('`sys:prolog').
wl:interned_eval('`sys:break').
compile_prolog_call(_Ctx,_Env,Prev,[sys_prolog],Prev, break).
compile_prolog_call(_Ctx,_Env,Prev,[sys_break],Prev, break).

%compile_body_h(_Ctx,_Env,Result, nop(X),  nop(X)):- !, debug_var("_NopResult",Result).
compile_prolog_call(Ctx,Env,Result,call_for(Body0,Result),Prev, Body):-!,
  expand_prolog(Prev,Ctx,Env,Body,Body0).


compile_prolog_call(Ctx,Env,Prev,call_for(Body0),Prev, Body):-!,
  expand_prolog(Prev,Ctx,Env,Body,Body0).

wl:interned_eval('`sys:prolog-call').

% (prolog-call "ls.")
compile_prolog_call(Ctx,Env,Result,[sys_prolog_call,Call], Prev, (Term->Result=t;Result=[]) ):-
   read_prolog_from_lisp(Call,Term0),
   expand_prolog(Prev,Ctx,Env,Term0,Term).   

% (prolog-call "X=1" "X")
compile_prolog_call(Ctx,Env,Result,[sys_prolog_call,Call,ResultL], Prev, (Term->true;Result=[]) ):-
   to_prolog_string(Call,Str),to_prolog_string(ResultL,Str2),
   atomic_list_concat(['((',Str,')):-((',Str2,'))'],Read),
   read_term_from_atom(Read,(Term0:-Result0),[]),
   expand_prolog(Prev,Ctx,Env,Term0:-Result0,Term:-Result).

% (prolog-inline "trace" )
wl:interned_eval('`sys:prolog-inline').
compile_prolog_call(Ctx,Env,Prev,[sys_prolog_inline,Call],Prev, Term ):-
   read_prolog_from_lisp(Call,Term0),
   expand_prolog(Prev,Ctx,Env,Term0,Term).

as_prolog_object(Operand,PrologArg):- is_stringp(Operand),to_prolog_string(Operand,PrologArg).
as_prolog_object(Operand,PrologArg):- is_unmberp(Operand),to_prolog_number(Operand,PrologArg).
as_prolog_object(PrologArg,PrologArg).

read_prolog_object(Operand):- read(Operand).

:- fixup_exports.

