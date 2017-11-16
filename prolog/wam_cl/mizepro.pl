/*******************************************************************
 *
 * C1 Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(mizepro, []).
:- set_module(class(library)).
:- include('header.pro').

body_cleanup(Ctx,CodeIn,CodeOut):- quietly(body_cleanup_keep_debug_vars(Ctx,CodeIn,CodeOut)).

body_cleanup_keep_debug_vars(Ctx,CodeIn,CodeOut):- 
 must_det_l(( 
   term_attvars(CodeIn,AttVars),maplist(del_attr_rev2(freeze),AttVars),
   inline_operation([],Ctx,',',CodeIn,Code0),
   body_cleanup_keep_debug_vars1(Ctx,Code0,Code2),
   body_cleanup_keep_debug_vars1(Ctx,Code2,Code3),
                  env_mize(Ctx,',',Code3,Code4),
                  inline_operation([],Ctx,',',Code4,Code5),
   body_cleanup_keep_debug_vars1(Ctx,Code5,CodeOut))).


body_cleanup_keep_debug_vars1(Ctx,Code0,CodeOut):-
 must_det_l((
  oper_mize(Code0,Ctx,',',Code0,Code1),
          mize_body3(Ctx,',',Code1,Code2),
                     mize_body(Ctx,',',Code2,CodeOut))).



skip_optimize(Var):-var(Var),!.
skip_optimize(retractall(_)).
skip_optimize(retract(_)).
skip_optimize(erase(_)).


always_true(G):- \+ ground(G),fail.

always_true(true).
always_true(t\==[]).
always_true([]\==t).


opt_arg1(F,_):- atom_concat(assert,_,F).
opt_arg1(((:-)),1).

%oper_mize(_Whole,_Ctx,_,Code,Code):-!.
oper_mize(_Whole,_Ctx,_,Code,Out):- \+ compound(Code),!,Out=Code.
oper_mize(_Whole,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
%oper_mize(_Whole,Ctx,F,(:-C1),(:-C2)):-!, oper_mize(C1,Ctx,F,C1,C2).

oper_mize(_Whole,_Ctx,_F,(C1,C2),U_x_Param=CondResult):- 
   C1= (U_x_Param=S1) , 
   C2= (CondResult=S2),
   var(S1),
   S1==S2,!.



oper_mize(_Whole,Ctx,F,(C1,C2),Joined):-!,
   oper_mize(C1,Ctx,F,C1,C1Better),
   oper_mize(C2,Ctx,F,C2,C2Better),
   conjoin_0(C1Better,C2Better,Joined).
oper_mize(_Whole,Ctx,F,[C1|C2],Joined):-!,
   oper_mize(C1,Ctx,F,C1,C1Better),
   oper_mize(C2,Ctx,F,C2,C2Better),
   ([C1Better|C2Better] = Joined).
oper_mize(Whole,_Ctx,_,Var1 = Var2, true):- var(Var1),var(Var2),  occurrences_of_var(Var1,Whole,N)-> N==2.

%oper_mize(_Whole,Ctx,_,C1=C2, true):- var(C1),var(C2),maybe_keep,C1=C2,!.

oper_mize(_Whole,Ctx,FF,PAB,PABO):- PAB=..[F,C1|Rest],functor(PAB,F,A),opt_arg1(F,A),
    oper_mize(C1,Ctx,FF,C1,C2),PABO=..[F,C2|Rest].

oper_mize(_Whole,Ctx,FF,(H:-C1),(H:-C2)):- nonvar(H),!,functor(H,F,A), body_mize([F/A],(H:-C1),Ctx,FF,C1,C2).
oper_mize(Whole,Ctx,F,call(C1),call(C2)):-!,oper_mize(Whole,Ctx,F,(C1),(C2)).
oper_mize(Whole,Ctx,F,C1,C2):- body_mize([],Whole,Ctx,F,C1,C2).

body_mize(_Skip,_Whole,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
body_mize(Skip,_Whole,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Skip),Out=Code.
%body_mize(_Skip,Whole,_Ctx,_,Var = Ground, true):- var(Var),ground(Ground), trace, occurrences_of_var(Var,Whole,N)-> N==2.
body_mize(_Skip,_Whole,_Ctx,_,C1,C1):-!.
body_mize(Skip,Whole,Ctx,F,(C1,C2),Joined):-!,
   body_mize(Skip,Whole,Ctx,F,C1,C1Better),
   body_mize(Skip,Whole,Ctx,F,C2,C2Better),conjoin_0(C1Better,C2Better,Joined).
%body_mize(Skip,_Whole,_Ctx,_,C1,Out):- maybe_optimize(C1), get_optimized(C1,Out).
body_mize(__Skip,_Whole,_Ctx,_,C1,C1):- \+ compound(C1),!.
body_mize(Skip,Whole,Ctx,F,call(C1),call(C2)):-!, oper_mize(Skip,Whole,Ctx,F,C1,C2).
body_mize(Skip,Whole,Ctx,F,(C1,C2),Joined):-!,
   oper_mize(Skip,Whole,Ctx,F,C1,C1Better),
   oper_mize(Skip,Whole,Ctx,F,C2,C2Better),
   conjoin_0(C1Better,C2Better,Joined).
body_mize(Skip,Whole,Ctx,F,(C1;C2),(C1Better;C2Better)):-!,
   oper_mize(Skip,Whole,Ctx,F,C1,C1Better),
   oper_mize(Skip,Whole,Ctx,F,C2,C2Better).

/*body_mize(Skip,Whole,Ctx,F,(P1->C1;C2),(P1Better->C1Better;C2Better)):-!,
   oper_mize(Skip,Whole,Ctx,F,P1,P1Better),
   oper_mize(Skip,P1->C1,Ctx,F,C1,C1Better),
   oper_mize(Skip,P1->C2,Ctx,F,C2,C2Better).
*/
body_mize(Skip,Whole,Ctx,_F,C1,C2):- 
  compound_name_arguments(C1,F,C1ARGS),
  must_maplist(body_mize(Skip,Whole,Ctx,F),C1ARGS,C2Better),
  C2=..[F|C2Better].
body_mize(_Skip,_Whole,_Ctx,_,C1,C1):-!.


maybe_optimize(_).


del_attr_rev2(Name,Var):- del_attr(Var,Name).

conjoin_0(C1,C2,C1):- C2==true,!.
conjoin_0(C1,C2,C2):- C1==true,!.
conjoin_0(C1,C2,C1):- C2==!,!.
conjoin_0(C1,C2,C2):- C1==!,!.
conjoin_0((C1,C1Better),C2,(C1,AAB)):-!, conjoin(C1Better,C2,AAB).
conjoin_0(C1,C2,(C1,C2)).



mize_body(_Ctx,_,C1,C1):- \+ compound(C1),!.
mize_body(Ctx,F, :-(C1), :-(C1Better)):-!,mize_body(Ctx,F,C1,C1Better).
mize_body(Ctx,F,(C1,C2),CodeJoined):-!,mize_body(Ctx,F,C1,C1Better),mize_body(Ctx,F,C2,C2Better),conjoin_0(C1Better,C2Better,CodeJoined).
%mize_body(Ctx,_,(C1 -> C2 ; _),C2Better):- mize_body(Ctx,->,C1,C1Better),always_true(C1Better),mize_body(Ctx,';',C2,C2Better),!.
mize_body(_Ctx,_,(C1 -> C2 ; _),C2):- always_true(C1),!.
mize_body(Ctx,_,(C1 -> C2 ; CodeC),(C1Better -> C2Better ; CodeCCBetter)):-!,mize_body(Ctx,->,C1,C1Better),mize_body(Ctx,';',C2,C2Better),mize_body(Ctx,';',CodeC,CodeCCBetter).
mize_body(Ctx,_,(C2 ; CodeC),( C2Better ; CodeCCBetter)):-!,mize_body(Ctx,';',C2,C2Better),mize_body(Ctx,';',CodeC,CodeCCBetter).
mize_body(Ctx,F,C1,CodeC):- mize_body1(Ctx,F,C1,C2),mize_body2(Ctx,F,C2,CodeC),!.
mize_body(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(mize_body(Ctx,F),C1Better,C2Better),C2=..[F|C2Better].

/*
mize_body(_Ctx,_F,(C1,C2),A=B):-
   C1= (S1=A) , 
   C2= (S2=B) ,
   var(S1),var(S2),var(A),var(B),
   (S1==B;S1==S2;A==B;A==S2),
   S1=S2.
*/
mize_body(_Ctx,_,C1,C1):-!.


ifthenelse(A->B;C):-nonvar(A),nonvar(B),nonvar(C).


mize_body1(_Ctx,_F,(A=B),true):- A==B,!.
mize_body1(_Ctx,_,C1,C1):- \+ compound(C1),!.
mize_body1(Ctx,F,(C1,C2),CodeJoined):-!,mize_body1(Ctx,F,C1,C1Better),mize_body1(Ctx,F,C2,C2Better),conjoin_0(C1Better,C2Better,CodeJoined).
mize_body1(Ctx,F,C1,C2):- is_list(C1),must_maplist(mize_body1(Ctx,F),C1,C2).
mize_body1(Ctx,_,symbol_value(_Env, Sym, Sym_Get),Was=Sym_Get):- % fail,maybe_keep,
  
  get_var_tracker(Ctx,Sym,Dict),
  Dict.w==1,Dict.vars=[Was|_],
  Was=Sym_Get.

mize_body1(_Ctx,_,C1,L=[R]):- C1 =@= (L=[R, []]), fail,maybe_keep.
%mize_body1(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(mize_body1(Ctx,F),C1Better,C2Better),C2=..[F|C2Better].
mize_body1(_Ctx,_,C1,C1):-!.

mize_body2(_Ctx,_,C1,C1):- \+ compound(C1),!.
mize_body2(_Ctx,_,t_or_nil(G, t),G).
mize_body2(_Ctx,_,(PARG,A=B), PARG):- compound(PARG),functor(PARG,_,Ar),arg(Ar,PARG,PP),(A==PP;B==PP),!,A=B.
mize_body2(_Ctx,_,G,true):- /*fail,maybe_keep,*/ always_true(G).
mize_body2(_Ctx,_,Var is Ground,Var = Result):- fail,maybe_keep, var(Var),ground(Ground), Result is Ground.
mize_body2(_Ctx,_,Number=:=Var,Number==Var):- (number(Number),var(Var));number(Var),var(Number),!.

mize_body2(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(mize_body2(Ctx,F),C1Better,C2Better),C2=..[F|C2Better].
mize_body2(_Ctx,_,C1,C1):-!.

mize_body3(_Ctx,_,C1,C1):- var(C1),del_attr(C1,rwstate).
mize_body3(_Ctx,_,C1=C2, true):- var(C1),var(C2),fail,maybe_keep,C1=C2,!.
mize_body3(_Ctx,_,C1,C1):- \+ compound(C1),!.

% mize_body3(_Ctx,_F,(C1,A=B),C1):- ifthenelse(C1),var(A),var(B),A=B.

mize_body3(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(mize_body3(Ctx,F),C1Better,C2Better),C2=..[F|C2Better].
mize_body3(_Ctx,_,C1,C1):-!.


%env_mize(_Ctx,_,C1,C1):-!.
env_mize(_Ctx,_,C1,C1):- \+ compound(C1),!.
env_mize(Ctx,F,C1,CodeOut):- C1 = ( Env=[bv(N, Var)|Rest], C2), var(Env),atom(N), Rest==[],  
  \+ contains_var(N,C2),
   \+ contains_var(Env,C2),
  contains_var(Var,C2),!,
  env_mize(Ctx,F, C2,CodeOut),!.

env_mize(Ctx,F,C1,CodeOut):- C1 = ( Env=[bv(N, Var)|Rest], C2), var(Env),atom(N), Rest\==[], 
  \+ contains_var(N,C2),
   \+ contains_var(Env,C2),
  contains_var(Var,C2),!,
  env_mize(Ctx,F,( Env=Rest, C2),CodeOut).

env_mize(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(env_mize(Ctx,F),C1Better,C2Better),C2=..[F|C2Better].
env_mize(_Ctx,_,C1,C1):-!.

% inline_operation(_,_,_,C1,C1).
inline_operation(_Never,_Ctx,_,C1,C1):- var(C1),!.
inline_operation(_Never,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
inline_operation(Never,Ctx,FF,(:-C1),(:-C2)):-   
   inline_body(Never,Ctx,FF,C1,C2),C1\==C2,!.
inline_operation(Never,Ctx,FF,(:-C1),(:-C2)):-   
   inline_operation(Never,Ctx,FF,C1,C2).
inline_operation(Never,Ctx,F,(C1,C2),CodeJoined):-!,
  inline_operation(Never,Ctx,F,C1,C1Better),
  inline_operation(Never,Ctx,F,C2,C2Better),
  conjoin_0(C1Better,C2Better,CodeJoined).  

%inline_operation(Never,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Never),Out=Code.

inline_operation(_Never,_Ctx,_FF,(H:-C1),(H:-C1)):-  compound(C1), functor(C1,start_tabling,_),!.
inline_operation(Never,Ctx,FF,(MH:-C1),(MH:-C2)):- 
  strip_module(MH,_M,H),functor(H,F,A),atom_concat(_,' tabled',F),!,
   inline_body([F/A|Never],Ctx,FF,C1,C2).

/*


inline_operation(Never,Ctx,FF,(asserta(MH:-C1)),Wrapper):-  fail,
   strip_module(MH,M,H),
   functor(H,F,A), \+ atom_concat(_,' tabled',F),
   tabling:rename_term(H,HH),  
   %wdmsg(Wrapper),
   must_or_rtrace(ensure_tabled(M,H)),
   inline_body([F/A|Never],Ctx,FF,C1,C2),
    call(call, (Wrapper = 
         (
         ( (  
              asserta(M:H :- start_tabling(M:H,HH)),
              asserta(M:HH:-C2)
             ))))).

*/

inline_operation(Never,Ctx,FF,(asserta(MH:-C1)),Wrapper):- 
   strip_module(MH,_,H),
   functor(H,F,A),
   %tabling:rename_term(H,HH),  
   %wdmsg(Wrapper),
   %must_or_rtrace(ensure_tabled(M,H)),
   inline_body([F/A|Never],Ctx,FF,C1,C2),
   Wrapper = asserta(MH :- C2).


/*
inline_operation(Never,Ctx,FF,asserta(MH:-C1),Wrapper):- 
   strip_module(MH,M,H),
   functor(H,F,A), \+ atom_concat(_,' tabled',F),
   tabling:rename_term(H,HH),  
 %  wdmsg(Wrapper),
   inline_body([F/A|Never],Ctx,FF,C1,C2),
    (Wrapper = (
         ( asserta(H :- start_tabling(H,HH))),
         ( multifile('$tabled'/1)),
         ( multifile('$table_mode'/3)),
         ( multifile('$table_update'/4)),
         ( '$flushed_predicate'(M:'$tabled'(_))), 
         ( call(M:'$tabled'(H))),
         (asserta(M:HH:-C2)))).
*/


% asserta/ (:- / 1)
inline_operation(Never,Ctx,FF,PAB,Conjs):- PAB=..[F,C1|Rest],
    functor(PAB,F,A),opt_arg1(F,A),!,
    inline_operation(Never,Ctx,FF,C1,C2),
    do_conjs(F,C2,Rest,Conjs).

inline_operation(_Never,_Ctx,_,C1,C1):-!.
inline_operation(_,_,_,C1,C1).

ensure_tabled(M,H):-
 M:(
  (use_module(library(tabling))),
  (multifile('$tabled'/1)),
  %(dynamic('$tabled'/1)),asserta(M:'$tabled'(H)),
  (multifile('$table_mode'/3)),
  (multifile('$table_update'/4)),
  
  must_or_rtrace(prolog:'$flushed_predicate'(M:'$tabled'(_))),
  (must_or_rtrace(prolog:call(M:'$tabled'(H))))).
 

/*
inline_operation(Never,Ctx,FF,(MH:-C1),Wrapper):- 
  strip_module(MH,M,H),
  functor(H,F,A), \+ atom_concat(_,' tabled',F),
  tabling:rename_term(H,HH),  
%  wdmsg(Wrapper),
  inline_body([F/A|Never],Ctx,FF,C1,C2),!,
   (Wrapper = (
       (H :- start_tabling(H,HH)),
        unwrapped(:- multifile('$tabled'/1)),
        unwrapped(:- multifile('$table_mode'/3)),
        unwrapped(:- multifile('$table_update'/4)),
        unwrapped(:- '$flushed_predicate'(M:'$tabled'(_))), 
        unwrapped(:- call(M:'$tabled'(H))),
        M:HH:-C2)).
*/

% inline_operation(Never,Ctx,FF,(H:-C1),(H:-C2)):-!,functor(H,F,A), inline_body([F/A|Never],Ctx,FF,C1,C2).
%inline_operation(Never,Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(inline_operation(Never,Ctx,F),C1Better,C2Better),C2=..[F|C2Better].

do_conjs(F,C1,Rest,Conjs):-var(C1),!,Conjs=..[F,C1|Rest].
do_conjs(_F,unwrapped(C2),_Rest,C2):-!.
do_conjs(F,C1,Rest,C2):- !,do_conjs2(F,C1,Rest,C2).
do_conjs2(F,(C1,C2),Rest,Conjs):- !, do_conjs(F,C1,Rest,P1),do_conjs(F,C2,Rest,P2),conjoin_0(P1,P2,Conjs).
do_conjs2(F,C2,Rest,Conjs):- Conjs=..[F,C2|Rest].


%inline_body(_,_,_,C1,C1).
inline_body(_Never,_Ctx,_,C1,C1):- var(C1),!.
inline_body(_Never,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
inline_body(Never,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Never),Out=Code.
inline_body(Never,Ctx,F,(C1,C2),CodeJoined):-!,inline_body(Never,Ctx,F,C1,C1Better),inline_body(Never,Ctx,F,C2,C2Better),conjoin_0(C1Better,C2Better,CodeJoined).
inline_body(_Never,_Ctx,_,C1,Out):- maybe_inline(C1), get_inlined(C1,Out).
inline_body(_Never,_Ctx,_,C1,C1):- \+ compound(C1),!.
inline_body(Never,Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1Better),must_maplist(inline_body(Never,Ctx,F),C1Better,C2Better),C2=..[F|C2Better].
inline_body(_Never,_Ctx,_,C1,C1):-!.

list_to_disj([C1],(C1Better)):-!, list_to_disj(C1,C1Better).
list_to_disj([C1,C2],(C1Better;C2Better)):-!, list_to_disj(C1,C1Better),list_to_disj(C2,C2Better).
list_to_disj([C1|C2],(C1Better;C2Better)):-!, list_to_disj(C1,C1Better),list_to_disj(C2,C2Better).
list_to_disj(C1,C1).

get_inlined(P,Out):- bagof(I,clause(P,I),DisjL),list_to_disj(DisjL,Out),
  dmsg(inlined(P):-Out),!.

never_inline(P):- \+ callable(P),!.
never_inline(P):- predicate_property(P,foreign).
never_inline(P):- predicate_property(P,imported_from(system)).
never_inline(P):- predicate_property(P,number_of_clauses(N)),N==0.
never_inline(P):- compound(P),functor(P,F,C1),never_inline_fa(F,C1).
never_inline_fa(place_op,_).
never_inline_fa(F,_):- atom_concat(_,' tabled',F).
never_inline_fa(start_tabling,_).
never_inline_fa(set_symbol_value,_).
never_inline_fa(get_opv,_).
never_inline_fa(t_or_nil,_).

lisp_compiler_option(_,false).

maybe_inline(C1):- \+ never_inline(C1), 
  predicate_property(C1,interpreted),% lisp_compiler_option(inline,true),
  % predicate_property(C1,static),
  %predicate_property(C1,number_of_clauses(1)).
  !.


:- fixup_exports.

