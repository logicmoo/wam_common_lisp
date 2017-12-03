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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(mizepro, []).
:- set_module(class(library)).
:- include('header.pro').

body_cleanup(Ctx,CodeIn,CodeOut):- quietly(body_cleanup_keep_debug_vars(Ctx,CodeIn,CodeOut)).

body_cleanup_keep_debug_vars(Ctx,CodeSt,CodeOutNow):- 
 must_det_l((
   sanitize_true(Ctx,CodeSt,CodeIn),
   term_attvars(CodeIn,AttVars),maplist(del_attr_rev2(freeze),AttVars),
   inline_operation([],Ctx,',',CodeIn,Code0),
   body_cleanup_keep_debug_vars1(Ctx,Code0,Code2),
   body_cleanup_keep_debug_vars1(Ctx,Code2,Code3),
   env_mize(Ctx,',',Code3,Code4),
   inline_operation([],Ctx,',',Code4,Code5a),
   fast_get_sets(Ctx,',',Code5a,Code5),
   term_attvars(Code5,AttVars5),maplist(del_attr_rev2(rwstate),AttVars5),   
   body_cleanup_keep_debug_vars1(Ctx,Code5,CodeOut),
   add_type_checks_maybe(Ctx,CodeOut,CodeOutNow))).

add_type_checks_maybe(_,IO,IO).


body_cleanup_keep_debug_vars1(Ctx,Code0,CodeOut):-
 must_det_l((oper_mize(Code0,Ctx,',',Code0,Code1), mize_body(Ctx,',',Code1,CodeOut))).

fast_get_sets(_Ctx,_,Code5,Code5).

skip_optimize(Var):-var(Var),!.
skip_optimize(NC):- \+ compound(NC),!.
skip_optimize([_|_]):-!.
skip_optimize(_:P):-!,skip_optimize(P).
skip_optimize(P):- functor(P,F,_),atom_concat('$',_,F).
skip_optimize(retractall(_)).
skip_optimize(retract(_)).
skip_optimize(erase(_)).


always_true(G):- \+ ground(G),fail.
always_true(true).
always_true(t\==[]).
always_true([]\==t).


functor_arg_is_body(F,_):- atom_concat_or_rtrace(assert,_,F).
functor_arg_is_body(((:-)),1).

%oper_mize(_Whole,_Ctx,_,Code,Code):-!.
oper_mize(_Whole,_Ctx,_,Code,Out):- \+ compound(Code),!,Out=Code.
oper_mize(_Whole,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
%oper_mize(_Whole,Ctx,F,(:-C1),(:-C2)):-!, oper_mize(C1,Ctx,F,C1,C2).

oper_mize(_Whole,_Ctx,_F,(C1,C2),U_x_Param=CondResult):- 
   %lisp_compiler_option(elim_vars,true),
   C1= (U_x_Param=S1) , 
   C2= (CondResult=S2),
   var(S1),
   S1==S2,!.                            

oper_mize(_Whole,Ctx,F,(C1,C2),Joined):-!,
   oper_mize(C1,Ctx,F,C1,C1O),    
   oper_mize(C2,Ctx,F,C2,C2O),
   conjoin_0(C1O,C2O,Joined).

oper_mize(_Whole,Ctx,F,[C1|C2],Joined):-!,oper_mize(C1,Ctx,F,C1,C1O),oper_mize(C2,Ctx,F,C2,C2O),([C1O|C2O] = Joined).

oper_mize(Whole,_Ctx,_,Var1 = Var2, true):- 
  lisp_compiler_option(elim_vars,true),
  var(Var1),var(Var2),  occurrences_of_var(Var1,Whole,N)-> N==2.

%oper_mize(_Whole,Ctx,_,C1=C2, true):- var(C1),var(C2),maybe_keep,C1=C2,!.

oper_mize(_Whole,Ctx,FF,PAB,PABO):- PAB=..[F,C1|Rest],functor(PAB,F,A),functor_arg_is_body(F,A),
    oper_mize(C1,Ctx,FF,C1,C2),PABO=..[F,C2|Rest].

oper_mize(_Whole,Ctx,FF,(H:-C1),(H:-C2)):- nonvar(H),!,functor(H,F,A), body_mize([F/A],(H:-C1),Ctx,FF,C1,C2).
oper_mize(Whole,Ctx,F,always(C1),always(C2)):-!,oper_mize(Whole,Ctx,F,(C1),(C2)).
oper_mize(Whole,Ctx,F,call(C1),call(C2)):-!,oper_mize(Whole,Ctx,F,(C1),(C2)).
oper_mize(Whole,Ctx,F,C1,C2):- body_mize([],Whole,Ctx,F,C1,C2).

body_mize(_Skip,_Whole,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
body_mize(Skip,_Whole,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Skip),Out=Code.
%body_mize(_Skip,Whole,_Ctx,_,Var = Ground, true):- var(Var),ground(Ground), trace, occurrences_of_var(Var,Whole,N)-> N==2.
body_mize(_Skip,_Whole,_Ctx,_,C1,C1):-!.
body_mize(Skip,Whole,Ctx,F,(C1,C2),Joined):-!,
   body_mize(Skip,Whole,Ctx,F,C1,C1O),
   body_mize(Skip,Whole,Ctx,F,C2,C2O),conjoin_0(C1O,C2O,Joined).
%body_mize(Skip,_Whole,_Ctx,_,C1,Out):- maybe_optimize(C1), get_optimized(C1,Out).
body_mize(__Skip,_Whole,_Ctx,_,C1,C1):- \+ compound(C1),!.
body_mize(Skip,Whole,Ctx,F,call(C1),call(C2)):-!, oper_mize(Skip,Whole,Ctx,F,C1,C2).
body_mize(Skip,Whole,Ctx,F,(C1,C2),Joined):-!,
   oper_mize(Skip,Whole,Ctx,F,C1,C1O),
   oper_mize(Skip,Whole,Ctx,F,C2,C2O),
   conjoin_0(C1O,C2O,Joined).
body_mize(Skip,Whole,Ctx,F,(C1;C2),(C1O;C2O)):-!,
   oper_mize(Skip,Whole,Ctx,F,C1,C1O),
   oper_mize(Skip,Whole,Ctx,F,C2,C2O).

/*body_mize(Skip,Whole,Ctx,F,(P1->C1;C2),(P1O->C1O;C2O)):-!,
   oper_mize(Skip,Whole,Ctx,F,P1,P1O),
   oper_mize(Skip,P1->C1,Ctx,F,C1,C1O),
   oper_mize(Skip,P1->C2,Ctx,F,C2,C2O).
*/
body_mize(Skip,Whole,Ctx,_F,C1,C2):- 
  compound_name_arguments(C1,F,C1ARGS),
  must_maplist(body_mize(Skip,Whole,Ctx,F),C1ARGS,C2O),
  C2=..[F|C2O].
body_mize(_Skip,_Whole,_Ctx,_,C1,C1):-!.


maybe_optimize(_).


del_attr_rev2(Name,Var):- del_attr(Var,Name).

sanitize_true(_, C1,C2):- \+ compound(C1),!,C2=C1.
sanitize_true(Ctx,(C1,C2),Joined):-!,sanitize_true(Ctx,C1,C1O),sanitize_true(Ctx,C2,C2O),conjoin_0(C1O,C2O,Joined).
sanitize_true(Ctx,(C1 -> C2 ; CodeC),(C1O -> C2O ; CodeCCO)):-!,sanitize_true(Ctx,C1,C1O),sanitize_true(Ctx,C2,C2O),sanitize_true(Ctx,CodeC,CodeCCO).
sanitize_true(Ctx,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(sanitize_true(Ctx),C1O,C2O),C2=..[F|C2O].

conjoin_0(C1,C2,C3):- C2==true,!,visit_lit(C1,C3).
conjoin_0(C1,C2,C3):- C1==true,!,visit_lit(C2,C3).
conjoin_0(C1,C2,C3):- C2==!,visit_lit(C1,C3).
conjoin_0(C1,C2,C3):- C1==!,visit_lit(C2,C3).
%conjoin_0(C1,clean_escape(_),C1):- trace,!.
conjoin_0(C1,C2,(C1,C3)):- var(C1),!,sanitize_true(C2,C3).
conjoin_0(C1,C2,(C3,C2)):- var(C2),!,sanitize_true(C1,C3).
conjoin_0(clean_escape(_),_,true):- trace,!.
conjoin_0((clean_escape(_),_),_,true):- trace.
conjoin_0((C1,clean_escape(_)),_,C3):- trace,visit_lit(C1,C3).
conjoin_0(C1,(clean_escape(_),_),C3):- trace,visit_lit(C1,C3).
conjoin_0(symbol_value(ReplEnv, Var, Value),symbol_value(ReplEnv, Var, Value),
           symbol_value(ReplEnv, Var, Value)).
conjoin_0((C1,C1O),C2,(C1,AAB)):-!, 
  conjoin_0(C1O,C2,AAB).
conjoin_0(C1,C2,(C11,C21)):- !,visit_lit(C1,C11),!,visit_lit(C2,C21).

visit_lit(C1,C1).


mize_body(_Ctx,_,C1,C1):- \+ compound(C1),!.
mize_body(Ctx,F, :-(C1), :-(C1O)):-!,mize_body(Ctx,F,C1,C1O).


mize_body(Ctx,F,(C1,C2),CodeJoined):-!,mize_body(Ctx,F,C1,C1O),mize_body(Ctx,F,C2,C2O),conjoin_0(C1O,C2O,CodeJoined).
%mize_body(Ctx,_,(C1 -> C2 ; _),C2O):- mize_body(Ctx,->,C1,C1O),always_true(C1O),mize_body(Ctx,';',C2,C2O),!.
mize_body(_Ctx,_,(C1 -> C2 ; _),C2):- fail, lisp_compiler_option(safe(elim_always_trues),true), always_true(C1),!.
mize_body(Ctx,_,(C1 -> C2 ; CodeC),(C1O -> C2O ; CodeCCO)):-!,mize_body(Ctx,'->',C1,C1O),mize_body(Ctx,';',C2,C2O),mize_body(Ctx,';',CodeC,CodeCCO).
mize_body(Ctx,_,catch(C1,E, C2),catch(C1O,E, C2O)):- !, mize_body(Ctx,->,C1,C1O),mize_body(Ctx,';',C2,C2O).
mize_body(Ctx,_,(C2 ; CodeC),( C2O ; CodeCCO)):-!,mize_body(Ctx,';',C2,C2O),mize_body(Ctx,';',CodeC,CodeCCO).
mize_body(Ctx,F,C1,CodeC):- mize_body1(Ctx,F,C1,C2),mize_body2(Ctx,F,C2,CodeC),!.

%mize_body(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body(Ctx,F),C1O,C2O),C2=..[F|C2O].

%mize_body(_Ctx,_,C1,C1):-!.

structure_applies(A,B):- copy_term_nat(A,CA),numbervars(CA,0,_),
  \+ (CA \= B),
   A=B.
structure_variant(A,B):- copy_term_nat(A,CA),copy_term(B,CB),numbervars(CA,0,_),numbervars(CB,0,_),
  \+ (CA \= CB),
   A=B.

ifthenelse(P):-structure_applies(P,( _ -> _ ; _ )).

mize_body1(_Ctx,_F,In,Out):-skip_optimize(In),!,In=Out.
mize_body1(Ctx,F,In,Out):-  
   clause(mize_body_1e(Ctx,F,In2,Out2),Body),
   structure_applies(In,In2),
   call(Body),!,
   (In \== Out2 -> mize_body1(Ctx,F,Out2,Out);Out=In),!.
mize_body1(_Ctx,_F,InOut,InOut).


mize_body_1e(_Ctx,_,C1,C1):- \+ compound(C1),!.
mize_body_1e(_Ctx,_F,(A=B),true):- A==B,!.
mize_body_1e(_Ctx,_,C1,L=[R]):- structure_applies(C1 , (L=[R, []])). % lisp_compiler_option(elim_vars,true).
mize_body_1e(Ctx,F,(C1,C2),CodeJoined):-!,mize_body1(Ctx,F,C1,C1O),mize_body1(Ctx,F,C2,C2O),conjoin_0(C1O,C2O,CodeJoined).
mize_body_1e(Ctx,_,symbol_value(_Env, Sym, Sym_Get),true):- 
  % lisp_compiler_option(safe(elim_symbolvalues_vars),true), 
 
  get_var_tracker(Ctx,Sym,Dict),
  Dict.w==1,
  Dict.p==1,
  % Dict.u<2,
  Dict.r>0,
  rw_add(Ctx,Sym,u),
  Dict.vars=[Was|_],!,
  must(Was=Sym_Get).
%mize_body1(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body1(Ctx,F),C1O,C2O),C2=..[F|C2O].
%mize_body1(Ctx,F,C1,C2):- is_list(C1),must_maplist(mize_body1(Ctx,F),C1,C2).
mize_body_1e(_Ctx,_,C1,C1):-!.


mize_body2(Ctx,F,In,Out):-  
   clause(mize_body_2e(Ctx,F,In2,Out2),Body),
   structure_applies(In,In2),
   call(Body),!,
   (In\==Out2 -> mize_body2(Ctx,F,Out2,Out);Out=In),!.
mize_body2(_Ctx,_F,InOut,InOut).

  
mize_body_2e(_Ctx,_,C1,C1):- \+ compound(C1),!.
mize_body_2e(_,_,In,ITE):- structure_applies(In,(ITE,R=V)), var(R),var(V),ifthenelse(ITE),R=V.
mize_body_2e(_Ctx,_,t_or_nil(G, R),G):- R==t.
mize_body_2e(_Ctx,_,t_or_nil(G, R),\+ G):- R==[].
mize_body_2e(_Ctx,_,(t_or_nil(G, R),(R \==[]-> B ; C)),(G->B;C)):- var(R).
mize_body_2e(_Ctx,_,(t_or_nil(G, R),(R \==[])),G):- var(R).

mize_body_2e(_Ctx,_,(PARG,A=B), PARG):- lisp_compiler_option(elim_xvars,true),compound(PARG),functor(PARG,_,Ar),arg(Ar,PARG,PP),(A==PP;B==PP),!,A=B.
mize_body_2e(_Ctx,_,G,true):- lisp_compiler_option(elim_always_trues,true), always_true(G).
mize_body_2e(_Ctx,_,Var is Ground,Var = Result):- lisp_compiler_option(elim_vars,true), var(Var),ground(Ground), Result is Ground.
mize_body_2e(_Ctx,_,Number=:=Var,Number==Var):- (number(Number),var(Var));number(Var),var(Number),!.

mize_body_2e(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body2(Ctx,F),C1O,C2O),C2=..[F|C2O].
mize_body_2e(_Ctx,_,C1,C1):-!.

mize_body3(_Ctx,_,C1,C1):- var(C1),del_attr(C1,rwstate).
mize_body3(_Ctx,_,C1=C2, true):- var(C1),var(C2),lisp_compiler_option(elim_vars,true),C1==C2,!.
mize_body3(_Ctx,_,C1,C1):- \+ compound(C1),!.

% mize_body3(_Ctx,_F,(C1,A=B),C1):- ifthenelse(C1),var(A),var(B),A=B.
mize_body3(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body3(Ctx,F),C1O,C2O),C2=..[F|C2O].
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

env_mize(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(env_mize(Ctx,F),C1O,C2O),C2=..[F|C2O].
env_mize(_Ctx,_,C1,C1):-!.

% inline_operation(_,_,_,C1,C1).
inline_operation(_Never,_Ctx,_,C1,C1):- var(C1),!.
inline_operation(_Never,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
inline_operation(Never,Ctx,FF,(:-C1),(:-C2)):-   
   inline_body(Never,Ctx,FF,C1,C2),C1\==C2,!.
inline_operation(Never,Ctx,FF,(:-C1),(:-C2)):-   
   inline_operation(Never,Ctx,FF,C1,C2).
inline_operation(Never,Ctx,F,(C1,C2),CodeJoined):-!,
  inline_operation(Never,Ctx,F,C1,C1O),
  inline_operation(Never,Ctx,F,C2,C2O),
  conjoin_0(C1O,C2O,CodeJoined).  

%inline_operation(Never,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Never),Out=Code.

inline_operation(_Never,_Ctx,_FF,(H:-C1),(H:-C1)):-  compound(C1), functor(C1,start_tabling,_),!.
inline_operation(Never,Ctx,FF,(MH:-C1),(MH:-C2)):- 
  strip_module(MH,_M,H),functor(H,F,A),atom_concat_or_rtrace(_,' tabled',F),!,
   inline_body([F/A|Never],Ctx,FF,C1,C2).


inline_operation(Never,Ctx,FF,(asserta(MH:-C1)),Wrapper):- 
   strip_module(MH,_,H),
   functor(H,F,A),
   %tabling:rename_term(H,HH),  
   %wdmsg(Wrapper),
   %always(ensure_tabled(M,H)),
   inline_body([F/A|Never],Ctx,FF,C1,C2),
   Wrapper = asserta(MH :- C2).


% asserta/ (:- / 1)
inline_operation(Never,Ctx,FF,PAB,Conjs):- PAB=..[F,C1|Rest],
    functor(PAB,F,A),functor_arg_is_body(F,A),!,
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
  
  always(prolog:'$flushed_predicate'(M:'$tabled'(_))),
  (always(prolog:call(M:'$tabled'(H))))).
 
do_conjs(F,C1,Rest,Conjs):-var(C1),!,Conjs=..[F,C1|Rest].
do_conjs(_F,unwrapped(C2),_Rest,C2):-!.
do_conjs(F,C1,Rest,C2):- !,do_conjs2(F,C1,Rest,C2).
do_conjs2(F,(C1,C2),Rest,Conjs):- !, do_conjs(F,C1,Rest,P1),do_conjs(F,C2,Rest,P2),conjoin_0(P1,P2,Conjs).
do_conjs2(F,C2,Rest,Conjs):- Conjs=..[F,C2|Rest].


%inline_body(_,_,_,C1,C1).
inline_body(_Never,_Ctx,_,C1,C1):- var(C1),!.
inline_body(_Never,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
inline_body(Never,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Never),Out=Code.
inline_body(Never,Ctx,F,(C1,C2),CodeJoined):-!,inline_body(Never,Ctx,F,C1,C1O),inline_body(Never,Ctx,F,C2,C2O),conjoin_0(C1O,C2O,CodeJoined).
inline_body(_Never,_Ctx,_,C1,Out):- 
  maybe_inline(C1), 
  term_variables(C1,MustHaveAll),
  copy_term(C1,Before),
  get_inlined(C1,Out),
  term_variables(Out,NewVars),
  C1=@=Before,
  subset(MustHaveAll,NewVars).
inline_body(_Never,_Ctx,_,C1,C1):- \+ compound(C1),!.
inline_body(Never,Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(inline_body(Never,Ctx,F),C1O,C2O),C2=..[F|C2O].
inline_body(_Never,_Ctx,_,C1,C1):-!.

list_to_disj([C1],(C1O)):-!, list_to_disj(C1,C1O).
list_to_disj([C1,C2],(C1O;C2O)):-!, list_to_disj(C1,C1O),list_to_disj(C2,C2O).
list_to_disj([C1|C2],(C1O;C2O)):-!, list_to_disj(C1,C1O),list_to_disj(C2,C2O).
list_to_disj(C1,C1).

get_inlined(P,Out):- bagof(I,clause(P,I),DisjL),list_to_disj(DisjL,Out),
  dmsg(inlined(P):-Out),!.

never_inline(P):- \+ callable(P),!.
never_inline(P):- predicate_property(P,foreign).
never_inline(P):- predicate_property(P,imported_from(system)).
never_inline(P):- predicate_property(P,number_of_clauses(N)),N==0.
never_inline(P):- compound(P),functor(P,F,A),never_inline_fa(F,A).
never_inline_fa(place_op,_).
never_inline_fa(F,_):- atom_concat_or_rtrace(_,' tabled',F).
never_inline_fa(F,_):- atom_concat_or_rtrace('cl_',_,F).
never_inline_fa(start_tabling,_).
never_inline_fa(symbol_value,_).
never_inline_fa(set_symbol_value,_).
never_inline_fa(get_opv,_).
never_inline_fa(member,_).
never_inline_fa(as_rest,_).
never_inline_fa(t_or_nil,_).

always_inline(P):- never_inline(P),!,fail.
always_inline(P):- compound(P),functor(P,F,A),always_inline_fa(F,A).
%always_inline(P):- clause(P,B)->(B==true;B=t_or_nil(_,_)).

always_inline_fa(F,1):- atom_concat_or_rtrace('addr_tagbody_',M,F),atom_contains(M,'_addr_enter_').
always_inline_fa(F,_):- atom_concat_or_rtrace(_,'expand1',F).

maybe_inline(C1):- always_inline(C1),
  predicate_property(C1,interpreted),
  predicate_property(C1,number_of_clauses(1)),
  \+ clause_has_cuts(C1),
  lisp_compiler_option(safe(inline),true),
  !.

maybe_inline(C1):- \+ never_inline(C1), 
  predicate_property(C1,interpreted),
  predicate_property(C1,number_of_clauses(1)),
  \+ clause_has_cuts(C1),
  lisp_compiler_option(inline,true),
  !.


clause_has_cuts(P):- clause(P,I),contains_var(!,I).

:- fixup_exports.

