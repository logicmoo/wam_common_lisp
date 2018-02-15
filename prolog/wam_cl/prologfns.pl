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


cl:f_trace(t):- rtrace,trace.

/*
TODO fix prolog_call

tst:is_local_test(block2,[block,block2,[tagbody,setq(b,2),[go,tag2],setq(a,1),(tag1),
                     prolog_call([a,b],plus(a,b,C)),prolog_call(writeln(C)),
                     'return-from'(block2,c),(tag2),setq(a,4),[go,tag1]]],6).
*/

wl:plugin_expand_progbody(Ctx,Env,Result,InstrS,Prev,Code):- 
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

wl:init_args(x, sys_rtrace).
wl:declared(sys_rtrace,kw_special).
wl:interned_eval('`sys:rtrace').
f_sys_rtrace(Eval,Ret):- lisp_compile(Ret,Eval,Body), rtrace(Body).
f_sys_rtrace(t):- rtrace.

% (sys:prolog)
% (sys:break)
wl:init_args(x, sys_prolog).
wl:interned_eval('`sys:prolog').
compile_prolog_call(_Ctx,_Env,Prev,[sys_prolog],Prev, prolog).

wl:init_args(0, break).
wl:interned_eval('`cl:break').
%compile_prolog_call(_Ctx,_Env,Prev,[break],Prev, f_break).

f_break(Options,[]):- wdmsg(Options), break.

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


% (rtrace ?progn )
compile_prolog_call(Ctx,Env,Result,[sys_rtrace|Progn],Prev, rtrace(Body) ):-
   rtrace(must_compile_progn(Ctx,Env,Result, Progn, Prev, Body)).

as_prolog_object(Operand,PrologArg):- is_stringp(Operand),to_prolog_string(Operand,PrologArg).
as_prolog_object(Operand,PrologArg):- is_unmberp(Operand),to_prolog_number(Operand,PrologArg).
as_prolog_object(PrologArg,PrologArg).

read_prolog_object(Operand):- read(Operand).

do_interned_eval(MG):- strip_module(MG,_,G),G=call(_),!,call_interned_eval(MG).
do_interned_eval(G):- call_interned_eval(lisp_compiled_eval(G,_)).

call_interned_eval(G):- always(subst_castifies(G,GG)),!,always(GG),!.

% call_interned_eval(M,G):- locally_let(xx_package_xx=pkg_prolog, M:G ).


locally_bind(Env,N,V,G):- 
   Binding=bv(N,Was),
   \+ \+ ((
   append_open_list(Env,Binding),
   nb_setarg(2,Binding,V))),
   call_cleanup(call(G),
     nb_setarg(2,Binding,Was)).


locally_set(N,V,B):- locally_let(N=V,B).

locally_let([N=V|More],G):- castify(V,Value),V\==Value,!,locally_let([N=Value|More],G).
locally_let([N=V|More],G):- castify(N,Symbol),N\==Symbol,!,locally_let([Symbol=V|More],G).
locally_let(N=V,G):-!,locally_let([N=V],G).
locally_let([N=V|More],G):- 
 always(get_var(N,Was)),
  setup_call_cleanup(
     set_opv(N,symbol_value,V),
     %once(locally($(N)=V,..)),
     (locally_let(More,G),set_opv(N,symbol_value,Was)),
        set_opv(N,symbol_value,Was)).
locally_let([],G):- !,call_interned_eval(G). 

subst_castifies(G,G):- \+ compound(G),!.
subst_castifies(G,GG):- castify(G,GG),!.
subst_castifies(C1,C2):- compound_name_arguments(C1,F,C1O),
  must_maplist(subst_castifies,C1O,C2O),C2=..[F|C2O].

castify(O,O):- \+compound(O),!,fail.
castify(str(O),S):-!, castify1(O,M),to_lisp_string(M,S).
castify(plstr(O),S):-!, castify1(O,M),to_prolog_string(M,S).
castify(path(O),S):-!, castify1(O,M),f_pathname(M,S).
castify(sym(O),S):-!, castify1(O,M),reader_intern_symbols(M,S).
castify(value(O),S):- castify1(O,M),always(get_opv(M,symbol_value,S)).
castify(value(O),S):- castify1(O,M),always(get_opv(M,symbol_value,S)).
castify(get_slot(Slot,O),S):- castify1(O,M),castify1(Slot,LSlot),always(get_opv(M,LSlot,S)).

castify1(O,O):- \+compound(O),!.
castify1(O,O):- is_list(O),!.
castify1(I,O):- castify(I,O).
castify1(O,O).



:- fixup_exports.

