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
:- include('header').

body_cleanup(Ctx,CodeIn,CodeOut):- quietly(body_cleanup_full(Ctx,CodeIn,CodeOut)).

body_cleanup_keep_debug_vars(Ctx,CodeIn,CodeOut):- quietly(body_cleanup_full(Ctx,CodeIn,CodeOut)),!.

body_cleanup_no_optimize(Ctx,CodeSt,CodeIn):-!, 
 always((   
  %show_ctx_info(Ctx),
   sanitize_true(Ctx,CodeSt,CodeIn),
   %del_attrs_of(CodeIn,freeze),   
   ! % inline_body([],Ctx,',',CodeIn,Code5a)
   )),!.

body_cleanup_keep_debug_vars_fuller(Ctx,CodeSt,Code5a):-!, 
 always((   
  %show_ctx_info(Ctx),
   sanitize_true(Ctx,CodeSt,CodeIn),
   del_attrs_of(CodeIn,freeze),
   (inline_body([],Ctx,',',CodeIn,Code5a)),
   del_attrs_of(Code5a,dif)
   )),!.

%body_cleanup_full(Ctx,CodeSt,CodeOutNow):- body_cleanup_keep_debug_vars_fuller(Ctx,CodeSt,CodeOutNow),!.

body_cleanup_full(_Ctx,CodeSt,CodeOutNow):- var(CodeSt),!,CodeOutNow=CodeSt.
%body_cleanup_full(Ctx,:- CodeSt,:- CodeOutNow):-!,body_cleanup_full(Ctx,CodeSt,CodeOutNow).
body_cleanup_full(Ctx,CodeSt,CodeOutNow):- 
 always((   
  %show_ctx_info(Ctx),
   %properly_protect(Ctx,CodeSt,_),
   sanitize_true(Ctx,CodeSt,CodeIn),
   del_attrs_of(CodeIn,freeze),
   inline_operation([],Ctx,',',CodeIn,Code0),
   body_cleanup_keep_debug_vars1(Ctx,Code0,Code2),
   body_cleanup_keep_debug_vars1(Ctx,Code2,Code3),   
   env_mize(Ctx,',',Code3,Code4),
   %inline_operation([],Ctx,',',Code4,Code5ab),
   inline_body([],Ctx,',',Code4,Code5a),   
   fast_get_sets(Ctx,',',Code5a,Code5),
   
   % 
   
   body_cleanup_keep_debug_vars1(Ctx,Code5,CodeOut),
   add_type_checks_maybe(Ctx,CodeOut,CodeOutNow),
   del_attrs_of(CodeOutNow,preserved_var),
   del_attrs_of(CodeOutNow,dif))).

add_type_checks_maybe(_,IO,IO).


body_cleanup_keep_debug_vars1(Ctx,Code0,CodeOutOut):-
 must_det_l((oper_mize(Code0,Ctx,',',Code0,Code1), mize_body(Ctx,',',Code1,CodeOut),
   sanitize_true(Ctx,CodeOut,CodeOutOut1),
   fast_get_sets(Ctx,'',CodeOutOut1,CodeOutOut))).


fast_get_sets(_Ctx,_,Code5,Code5):- \+ compound(Code5),!.
fast_get_sets(Ctx,F,(C1,C2,C4),C5):- conjoinment(Ctx,C1,C2,C3),!,fast_get_sets(Ctx,F,(C3,C4),C5).
fast_get_sets(Ctx,F,(C1,C2),Joined):- conjoinment(Ctx,C1,C2,C3),C3\==(C1,C2),!,fast_get_sets(Ctx,F,C3,Joined).
fast_get_sets(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(fast_get_sets(Ctx,F),C1O,C2O),C2=..[F|C2O].
fast_get_sets(_Ctx,_,Code5,Code5).

non_compound_code(NC):- \+ callable(NC),!.
non_compound_code(NC):- notrace(non_compound_code1(NC)),!.
non_compound_code(NC):- is_self_evaluating_object(NC),!.
non_compound_code1(NC):- \+ compound(NC).
non_compound_code1(NC):- is_list(NC).
non_compound_code1(_=_).
non_compound_code1(NC):- is_dict(NC).

skip_optimize(NC):- non_compound_code(NC),!.
skip_optimize(NC):-notrace(skip_optimize0(NC)).
skip_optimize0([_|_]):-!.
skip_optimize0(_:P):-!,skip_optimize(P).
skip_optimize0(P):- functor(P,F,_),atom_concat_or_rtrace('$',_,F).
skip_optimize0(retractall(_)).
skip_optimize0(retract(_)).
skip_optimize0(erase(_)).


always_true(G):- \+ ground(G),fail.
always_true(true).
always_true(t\==[]).
always_true([]\==t).


functor_arg_is_body(F,_):- atom_concat_or_rtrace(assert,_,F).
functor_arg_is_body(((:-)),1).

%oper_mize(_Whole,_Ctx,_,Code,Code):-!.
oper_mize(_Whole,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
%oper_mize(_Whole,Ctx,F,(:-C1),(:-C2)):-!, oper_mize(C1,Ctx,F,C1,C2).

oper_mize(_Whole,_Ctx,_F,(C1,C2),U_x_Param=CondResult):- 
   %wam_cl_option(elim_vars,true),
   C1= (U_x_Param=S1) , 
   C2= (CondResult=S2),
   var(S1),
   S1==S2,!.                            

oper_mize(_Whole,Ctx,F,(C1,C2),Joined):-!,
   oper_mize(C1,Ctx,F,C1,C1O),    
   oper_mize(C2,Ctx,F,C2,C2O),
   conjoin_0(Ctx,C1O,C2O,Joined).

oper_mize(_Whole,Ctx,F,[C1|C2],Joined):-!,oper_mize(C1,Ctx,F,C1,C1O),oper_mize(C2,Ctx,F,C2,C2O),([C1O|C2O] = Joined).

oper_mize(W,_Ctx,_,Var1 = Var2, true):- 
  wam_cl_option(elim_vars,true),
  var(Var1),var(Var2),  occurrences_of_var(Var1,W,N)-> N==2.

%oper_mize(_Whole,Ctx,_,C1=C2, true):- var(C1),var(C2),maybe_keep,C1=C2,!.

oper_mize(_Whole,Ctx,FF,PAB,PABO):- PAB=..[F,C1|Rest],functor(PAB,F,A),functor_arg_is_body(F,A),
    oper_mize(C1,Ctx,FF,C1,C2),PABO=..[F,C2|Rest].

oper_mize(_Whole,Ctx,FF,(H:-C1),(H:-C2)):- nonvar(H),!,functor(H,F,A), body_mize([F/A],(H:-C1),Ctx,FF,C1,C2).
oper_mize(W,Ctx,F,always(C1),always(C2)):-!,oper_mize(W,Ctx,F,(C1),(C2)).
oper_mize(W,Ctx,F,call(C1),call(C2)):-!,oper_mize(W,Ctx,F,(C1),(C2)).
oper_mize(W,Ctx,F,C1,C2):- body_mize([],W,Ctx,F,C1,C2).

body_mize(_Skip,_Whole,_Ctx,_,Code,Out):- skip_optimize(Code),Out=Code.
body_mize(Skip,_Whole,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Skip),Out=Code.
%body_mize(_Skip,W,_Ctx,_,Var = Ground, true):- var(Var),ground(Ground), trace, occurrences_of_var(Var,W,N)-> N==2.
body_mize(_Skip,_Whole,_Ctx,_,C1,C1):-!.
body_mize(Skip,W,Ctx,F,(C1,C2),Joined):-!,
   body_mize(Skip,W,Ctx,F,C1,C1O),
   body_mize(Skip,W,Ctx,F,C2,C2O),conjoin_0(Ctx,C1O,C2O,Joined).
%body_mize(Skip,_Whole,_Ctx,_,C1,Out):- maybe_optimize(C1), get_optimized(C1,Out).
body_mize(__Skip,_Whole,_Ctx,_,C1,C1):- non_compound_code(C1),!.
body_mize(Skip,W,Ctx,F,call(C1),call(C2)):-!, oper_mize(Skip,W,Ctx,F,C1,C2).
body_mize(Skip,W,Ctx,F,(C1,C2),Joined):-!,
   oper_mize(Skip,W,Ctx,F,C1,C1O),
   oper_mize(Skip,W,Ctx,F,C2,C2O),
   conjoin_0(Ctx,C1O,C2O,Joined).
body_mize(Skip,W,Ctx,F,(C1;C2),(C1O;C2O)):-!,
   oper_mize(Skip,W,Ctx,F,C1,C1O),
   oper_mize(Skip,W,Ctx,F,C2,C2O).

/*body_mize(Skip,W,Ctx,F,(P1->C1;C2),(P1O->C1O;C2O)):-!,
   oper_mize(Skip,W,Ctx,F,P1,P1O),
   oper_mize(Skip,P1->C1,Ctx,F,C1,C1O),
   oper_mize(Skip,P1->C2,Ctx,F,C2,C2O).
*/
body_mize(Skip,W,Ctx,_F,C1,C2):- 
  compound_name_arguments(C1,F,C1ARGS),
  must_maplist(body_mize(Skip,W,Ctx,F),C1ARGS,C2O),
  C2=..[F|C2O].
body_mize(_Skip,_Whole,_Ctx,_,C1,C1):-!.


maybe_optimize(_).


properly_protect(_, C1,C2):- non_compound_code(C1),!,C2=C1.
properly_protect(_, X=Y,X=Y):-!.
properly_protect(_Ctx,P,P):- predicate_property(P,foreign),!,stay_all_different(P).
properly_protect(_Ctx,P,P):- \+ predicate_property(P,imported_from(system)),!,stay_all_different(P).
properly_protect(Ctx,(C1:-C2),(C1O:-C2O)):-!,properly_protect(Ctx,C1,C1O),properly_protect(Ctx,C2,C2O).
properly_protect(Ctx,(C1,C2),Joined):-!,properly_protect(Ctx,C1,C1O),properly_protect(Ctx,C2,C2O),conjoin_0(Ctx,C1O,C2O,Joined).
properly_protect(Ctx,(C1 -> C2 ; CodeC),(C1O -> C2O ; CodeCCO)):-!,properly_protect(Ctx,C1,C1O),properly_protect(Ctx,C2,C2O),properly_protect(Ctx,CodeC,CodeCCO).
properly_protect(Ctx,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(properly_protect(Ctx),C1O,C2O),C2=..[F|C2O].

del_attr_rev2(Name,Var):- del_attr(Var,Name).
del_attrs_of(CodeIn,Name):- term_variables(CodeIn,AttVars),maplist(del_attr_rev2(Name),AttVars).

sanitize_true(_, C1,C2):- non_compound_code(C1),!,C2=C1.
sanitize_true(_,f_clos_pf_set_slot_value(A,B,C,D),set_opv(A,B,C)):-C=D.
sanitize_true(_,cl_slot_value(A,B,C),get_opv(A,B,C)).
sanitize_true(Ctx,(C1,C2),Joined):-!,sanitize_true(Ctx,C1,C1O),sanitize_true(Ctx,C2,C2O),conjoin_0(Ctx,C1O,C2O,Joined).
sanitize_true(Ctx,(C2 ; CodeC),( C2O ; CodeCCO)):-!,sanitize_true(Ctx,C2,C2O),sanitize_true(Ctx,CodeC,CodeCCO).
sanitize_true(Ctx,(C2 -> CodeC),( C2O -> CodeCCO)):-!,sanitize_true(Ctx,C2,C2O),sanitize_true(Ctx,CodeC,CodeCCO).
sanitize_true(Ctx,(C2 :- CodeC),( C2 :- CodeCCO)):-!,sanitize_true(Ctx,CodeC,CodeCCO).
sanitize_true(Ctx,( :- CodeC),( :- CodeCCO)):-!,sanitize_true(Ctx,CodeC,CodeCCO).
sanitize_true(_Ctx,C1,C1):-!.
%sanitize_true(Ctx,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(sanitize_true(Ctx),C1O,C2O),C2=..[F|C2O].

keeper(C1):- var(C1),!.
keeper(!).
discard(C2):- C2==true.

conjoin_1(Ctx,C1,C2,C3):- discard(C2),!,visit_lit(Ctx,C1,C3).
conjoin_1(Ctx,C1,C2,C3):- discard(C1),!,visit_lit(Ctx,C2,C3).
conjoin_1(_,C1,C2,C2):- C1==C2,C2==reset_mv,!.
conjoin_1(_,clean_escape(_),_,true):- trace,!.
%conjoin_1(Ctx,C1,C2,C3):- conjoinment(Ctx,C1,C2,C3),!.

conjoin_0(Ctx,C1,C2,(C1,C3)):- keeper(C1),!,visit_lit(Ctx,C2,C3).
conjoin_0(Ctx,C1,C2,(C3,C2)):- keeper(C2),!,visit_lit(Ctx,C1,C3).
%conjoin_0(Ctx,C1,clean_escape(_),C1):- trace,!.
conjoin_0(_,(clean_escape(_),_),_,true):- trace.
conjoin_0(Ctx,(C1,clean_escape(_)),_,C3):- trace,visit_lit(Ctx,C1,C3).
conjoin_0(Ctx,C1,(clean_escape(_),_),C3):- trace,visit_lit(Ctx,C1,C3).
conjoin_0(Ctx,C1,C2,C4):-      conjoin_1(Ctx,C1,C2,C3),!,visit_lit(Ctx,C3,C4).
conjoin_0(Ctx,C1,(C2,C2a),C3):-conjoin_1(Ctx,C1,C2,C12),!,conjoin_0(Ctx,C12,C2a,C3).
conjoin_0(Ctx,(C1,C1O),C2,OUT):-!,conjoin_0(Ctx,C1O,C2,AAB),conjoin_0(Ctx,C1,AAB,OUT).
conjoin_0(Ctx,C1,C2,(C11,C21)):- !,visit_lit(Ctx,C1,C11),!,visit_lit(Ctx,C2,C21).

visit_lit(_,C1,C1):-!.
visit_lit(_,C1,C1):-keeper(C1),!.
visit_lit(Ctx,C1,C2):-sanitize_true(Ctx,C1,C2). 


mize_body(_Ctx,_,C1,C1):- non_compound_code(C1),!.
mize_body(Ctx,F, :-(C1), :-(C1O)):-!,mize_body(Ctx,F,C1,C1O).

mize_body(Ctx,F,(C1,C2),CodeJoined):-!,mize_body(Ctx,F,C1,C1O),mize_body(Ctx,F,C2,C2O),conjoin_0(Ctx,C1O,C2O,CodeJoined).
%mize_body(Ctx,_,(C1 -> C2 ; _),C2O):- mize_body(Ctx,->,C1,C1O),always_true(C1O),mize_body(Ctx,';',C2,C2O),!.
mize_body(_Ctx,_,(C1 -> C2 ; _),C2):- fail, wam_cl_option(safe(elim_always_trues),true), always_true(C1),!.
mize_body(Ctx,_,(C1 -> C2 ; CodeC),(C1O -> C2O ; CodeCCO)):-!,mize_body(Ctx,'->',C1,C1O),mize_body(Ctx,';',C2,C2O),mize_body(Ctx,';',CodeC,CodeCCO).
mize_body(Ctx,_,(C2 ; CodeC),( C2O ; CodeCCO)):-!,mize_body(Ctx,';',C2,C2O),mize_body(Ctx,';',CodeC,CodeCCO).
mize_body(Ctx,F,C1,CodeC):- mize_body1(Ctx,F,C1,C2),mize_body2(Ctx,F,C2,CodeC),!.
mize_body(Ctx,_,catch(C1,E, C2),catch(C1O,E, C2O)):- !, mize_body(Ctx,->,C1,C1O),mize_body(Ctx,';',C2,C2O).

%mize_body(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body(Ctx,F),C1O,C2O),C2=..[F|C2O].

%mize_body(_Ctx,_,C1,C1):-!.

structure_applies(A,B):- copy_term_nat(A,CA),numbervars(CA,0,_),
  \+ (CA \= B),
   A=B.
structure_variant(A,B):- copy_term_nat(A,CA),copy_term(B,CB),numbervars(CA,0,_),numbervars(CB,0,_),
  \+ (CA \= CB),
   A=B.

conjoinment0(C1,C2,C2):- C1==C2,C2==reset_mv,!.
conjoinment0(A,B,_):- ( ( \+ compound(A)) ; \+ compound(B)), !,fail.
conjoinment0(get_var(ReplEnv1, Var1, Value1),get_var(ReplEnv2, Var2, Value2),
           get_var(ReplEnv1, Var1, Value1)):- Var1==Var2,ReplEnv2==ReplEnv1,Value1=Value2.

conjoinment0(get_var(ReplEnv1, Var1, Value1),get_var(ReplEnv2, Var2, Value2),
           (get_var(ReplEnv2, Var2, Value2),get_var(ReplEnv1, Var1, Value1))):- 
               Var1 @> Var2,!.
               %fail,
               %Value1=Value2,!.


                                    %get_var(LETENV318, u_l, Nreverse_Param),
                                    %U_l=[CAR496|Nreverse_Param],
                                    %set_var(LETENV318, setq, u_l, U_l)


conjoinment1(A,B,_):- ( ( \+ compound(A)) ; \+ compound(B)), !,fail.
conjoinment1(C1,(AEQB,C2),(C3,AEQB)):- move_down(AEQB),conjoinment0(C1,C2,C3).
conjoinment1(C1,C2,C3):- conjoinment0(C1,C2,C3).

conjoinment(_Ctx,C1,C2,C3):-conjoinment1(C1,C2,C3).

move_down(AEB):-var(AEB),!,fail.
move_down(A=B):- nop(var(A)),
  is_list(B).

ifthenelse(P):-structure_applies(P,( _ -> _ ; _ )).

structure_applies_here(In,In2,Body):- var(In2),In=In2,!,call(Body).
structure_applies_here(In,In2,Body):- structure_applies(In,In2),call(Body).

mize_body1(_Ctx,_F,In,Out):-skip_optimize(In),!,In=Out.
mize_body1(Ctx,F,In,Out):-  
   clause(mize_body_1e(Ctx,F,In2,Out2),Body),
   structure_applies_here(In,In2,Body),!,
   (In \== Out2 -> mize_body1(Ctx,F,Out2,Out);Out=In),!.
mize_body1(_Ctx,_F,InOut,InOut).

idiom_replace(set_var(E, OP, N, V),set_var(E, N, V)):- var(V), atom(N),atom(OP),memberchk(OP,[psetq,setf,setq]).
idiom_replace(set_place(E, OP, N, V),set_var(E, N, V)):- var(V), atom(N),atom(OP),memberchk(OP,[psetq,setf,setq]).
idiom_replace(set_var(E, OP, [PLACE, N], V),set_place(E, OP, [PLACE, N], V)):- var(V), atom(N),atom(OP),memberchk(OP,[setf]).

no_block_exists(G):- \+ has_block_exists(G).
has_block_exists(G):- sub_term(E,G),sub_block_exit(E).

sub_block_exit(E):- E==fail,!.
sub_block_exit(E):- \+ compound(E),!,fail.
sub_block_exit(C):-functor(C,F,A),sub_block_exit_f_a(F,A).
sub_block_exit_f_a(Addr,_):- atom(Addr), atom_contains(Addr,'addr_').
sub_block_exit_f_a(throw,1).
sub_block_exit_f_a(catch,3).


mize_body_1e(_Ctx,_,C1,C1):- non_compound_code(C1),!.

mize_body_1e(_Ctx,_,C1,C2):- idiom_replace(C1,C2).
mize_body_1e(_Ctx,_F,(A=B),true):- A==B,!.
mize_body_1e(_Ctx,_F,(A==B),true):- A==B,!.
mize_body_1e(_Ctx,_,cl_list(G, R),R=G).
mize_body_1e(_Ctx,_,C1,L=[R]):- structure_applies(C1 , (L=[R, []])). % wam_cl_option(elim_vars,true).

mize_body_1e(Ctx,F,(C1,C2,C4),C5):- conjoinment(Ctx,C1,C2,C3),!,mize_body_1e(Ctx,F,(C3,C4),C5).
mize_body_1e(Ctx,F,(C1,C2),Joined):- conjoinment(Ctx,C1,C2,C3),!,mize_body_1e(Ctx,F,C3,Joined).

mize_body_1e(Ctx,F,(C1,C2),CodeJoined):-!,mize_body1(Ctx,F,C1,C1O),mize_body1(Ctx,F,C2,C2O),conjoin_0(Ctx,C1O,C2O,CodeJoined).
mize_body_1e(Ctx,_,get_var(Env, Sym, Sym_Get),OUT):- 
  nop(OUT = 'O'(get_var(Env, Sym, Sym_Get),true)),
  OUT = true,
  % wam_cl_option(safe(elim_symbolvalues_vars),true),  
  get_var_tracker(Ctx,Sym,Dict),
  (Dict.w=1 ; ( Dict.p=1,Dict.w=0)),
  % ((Dict.p==1-> ))
  % Dict.u<2,
  %Dict.r>=0,
   wdmsg(mize_body_1e:- Dict),
  % rw_add(Ctx,Sym,u),
  Dict.vars=[Was|_],
  Was\==Sym_Get,
  (call(Was=Sym_Get)),!.
%mize_body1(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body1(Ctx,F),C1O,C2O),C2=..[F|C2O].
%mize_body1(Ctx,F,C1,C2):- is_list(C1),must_maplist(mize_body1(Ctx,F),C1,C2).

mize_body_1e(_Ctx,_,append([], R, W),R= W):- ignore(R= W).
mize_body_1e(_Ctx,_,Env=[],true):- Env=[],!.
mize_body_1e(_Ctx,_,C1,C1):-!.

'O'(Old,New):- New,nop(Old).
% 'O'(Old,New):- nop(New),Old.

mize_body2(Ctx,F,In,Out):-  
   clause(mize_body_2e(Ctx,F,In2,Out2),Body),
   structure_applies_here(In,In2,Body),!,   
   (In\==Out2 -> mize_body2(Ctx,F,Out2,Out);Out=In),!.
mize_body2(_Ctx,_F,InOut,InOut).

  
mize_body_2e(_Ctx,_,C1,C1):- non_compound_code(C1),!.
mize_body_2e(_,_,In,ITE):- structure_applies(In,(ITE,R=V)), var(R),var(V),ifthenelse(ITE),R=V.
%mize_body_2e(_Ctx,_,(S1=V,R=S2,B),(R=V,B)):- trace, var(S1),S1==S2.
mize_body_2e(_Ctx,_,(S1=V,R=S2),(R=V)):- var(S1),S1==S2,(var(R);var(V)),S2='$error_this_was_eliminated'.
mize_body_2e(_Ctx,_,t_or_nil(G, R),G):- R==t.
mize_body_2e(_Ctx,_,t_or_nil(G, R),\+ G):- R==[].
mize_body_2e(_Ctx,_,(t_or_nil(G, R),(R \==[]-> B ; C)),(G->B;C)):- var(R).
mize_body_2e(_Ctx,_,(t_or_nil(G, R),(R \==[])),G):- var(R).
mize_body_2e(_Ctx,_,catch(G,block_exit(Label,Result),true),G):- nonvar(Label),var(Result),Label\==[],no_block_exists(G).

mize_body_2e(_Ctx,_,(PARG,A=B), PARG):- wam_cl_option(elim_xvars,true),compound(PARG),functor(PARG,_,Ar),arg(Ar,PARG,PP),(A==PP;B==PP),!,A=B.
mize_body_2e(_Ctx,_,G,true):- wam_cl_option(elim_always_trues,true), always_true(G).
mize_body_2e(_Ctx,_,Var is Ground,Var = Result):- wam_cl_option(elim_vars,true), var(Var),ground(Ground), Result is Ground.
% mize_body_2e(_Ctx,_,Number=:=Var,Number==Var):- (number(Number),var(Var));number(Var),var(Number),!.

mize_body_2e(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body2(Ctx,F),C1O,C2O),C2=..[F|C2O].
mize_body_2e(_Ctx,_,C1,C1):-!.

mize_body3(_Ctx,_,C1,C1):- var(C1),del_attr(C1,rwstate).
mize_body3(_Ctx,_,C1=C2, true):- var(C1),var(C2),wam_cl_option(elim_vars,true),C1==C2,!.
mize_body3(_Ctx,_,C1,C1):- non_compound_code(C1),!.

%mize_body3(_Ctx,_F,(C1,A=B),C1):- ifthenelse(C1),var(A),var(B),A=B.
mize_body3(Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),must_maplist(mize_body3(Ctx,F),C1O,C2O),C2=..[F|C2O].
mize_body3(_Ctx,_,C1,C1):-!.


%env_mize(_Ctx,_,C1,C1):-!.
env_mize(_Ctx,_,C1,C1):- non_compound_code(C1),!.
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
  conjoin_0(Ctx,C1O,C2O,CodeJoined).  

%inline_operation(Never,_Ctx,_,Code,Out):- functor(Code,F,N),member(F/N,Never),Out=Code.

inline_operation(_Never,_Ctx,_FF,(H:-C1),(H:-C1)):-  compound(C1), functor(C1,start_tabling,_),!.
inline_operation(Never,Ctx,FF,(MH:-C1),(MH:-C2)):- 
  strip_module(MH,_M,H),functor(H,F,A),atom_concat_or_rtrace(_,' tabled',F),!,
   inline_body([F/A|Never],Ctx,FF,C1,C2).


inline_operation(Never,Ctx,FF,A,Wrapper):-
    is_assert_op(A,Where,MH:-C1), 
    strip_module(MH,_,H),
    functor(H,F,A),
    inline_body([F/A|Never],Ctx,FF,C1,C2),
    Wrapper = assert_lsp(Where,(MH :- C2)).

% assert_lsp/ (:- / 1)
inline_operation(Never,Ctx,FF,PAB,Conjs):- PAB=..[F,C1|Rest],
    functor(PAB,F,A),functor_arg_is_body(F,A),!,
    inline_operation(Never,Ctx,FF,C1,C2),
    do_conjs(F,C2,Rest,Conjs).

%inline_operation(_Never,_Ctx,_,C1,C1):-!.
inline_operation(Never,Ctx,FF,(:-C1),(:-C2)):-!,
   inline_body(Never,Ctx,FF,C1,C2),!.
inline_operation(_Never,_Ctx,_,C1,C1):-!.



progress_g(G):- copy_term(G,GG),del_attrs_of(GG,dif),GG.

ensure_tabled(M,H):-
 M:(
  (use_module(library(tabling))),
  (multifile('$tabled'/1)),
  %(dynamic('$tabled'/1)),assert_lsp(M:'$tabled'(H)),
  (multifile('$table_mode'/3)),
  (multifile('$table_update'/4)),
  
  always(prolog:'$flushed_predicate'(M:'$tabled'(_))),
  (always(prolog:call(M:'$tabled'(H))))).
 
do_conjs(F,C1,Rest,Conjs):-var(C1),!,Conjs=..[F,C1|Rest].
do_conjs(_F,unwrapped(C2),_Rest,C2):-!.
do_conjs(F,C1,Rest,C2):- !,do_conjs2(F,C1,Rest,C2).
do_conjs2(F,(C1,C2),Rest,Conjs):- !, do_conjs(F,C1,Rest,P1),do_conjs(F,C2,Rest,P2),conjoin_0(_Ctx,P1,P2,Conjs).
do_conjs2(F,C2,Rest,Conjs):- Conjs=..[F,C2|Rest].

stay_all_different(Out):- term_variables(Out,Vars),
  stay_all_different_vars(Vars).

stay_all_different_vars([]).
stay_all_different_vars([X|Vars]):- maplist(dif(X),Vars),stay_all_different_vars(Vars).

%inline_body(_,_,_,C1,C1).
inline_body(_Never,_Ctx,_,C1,C1):- var(C1),!.
inline_body(Never,Ctx,FT,(:-Body),(:-Out)):- !, inline_body(Never,Ctx,FT,Body,Out).
inline_body(Never,Ctx,FT,(A,B),(AA,BB)):-!,inline_body(Never,Ctx,FT,A,AA),inline_body(Never,Ctx,FT,B,BB).
inline_body(Never,Ctx,FT,(A;B),(AA;BB)):-!,inline_body(Never,Ctx,FT,A,AA),inline_body(Never,Ctx,FT,B,BB).
inline_body(Never,Ctx,FT,(A->B),(AA->BB)):-!,inline_body(Never,Ctx,FT,A,AA),inline_body(Never,Ctx,FT,B,BB).
inline_body(_Never,_Ctx,_,Code,Out):- \+ \+  skip_optimize(Code),!,Out=Code.
%inline_body(Never,_Ctx,_,Code,Out):- compound(Code),functor(Code,F,N),member(F/N,Never),!,Out=Code.
inline_body(Never,Ctx,FT,(M:Code:-Body),(M:Code:-Out)):- compound(Code),functor(Code,F,A),!,inline_body([F/A|Never],Ctx,FT,Body,Out).

inline_body(_Never,_Ctx,_,In,Out):- stay_all_different(In),simple_inline(In,Out),!.

inline_body(Never,Ctx,F,C1,Out):- 
  maybe_inline(C1),
  stay_all_different(C1),  
  get_inlined(C1,MID),functor(C1,F,A),
  progress_g(dbginfo(inlined(C1):-MID)),!,
  sanitize_true(Ctx,MID,MID2),
  inline_body([F/A|Never],Ctx,F,MID2,Out).
inline_body(_Never,_Ctx,_,C1,C1):- non_compound_code(C1),!.
inline_body(Never,Ctx,_F,C1,C2):- compound_name_arguments(C1,F,C1O),
  must_maplist(inline_body(Never,Ctx,F),C1O,C2O),!,C2=..[F|C2O].
inline_body(_Never,_Ctx,_F,C1,C1):-!.

simple_inline(In,_Out):- \+ compound(In),!,fail.
simple_inline(cl_list(A,B),B=A).
simple_inline(f_clos_pf_set_slot_value(A,B,C,D),set_opv(A,B,C)):-C=D.
simple_inline(cl_cdr(I,O),(I==[]->O=[];I=[_|O])):- wam_cl_option(debug,0).
simple_inline(cl_car(I,O),(I==[]->O=[];I=[O|_])):- wam_cl_option(debug,0).
list_to_disj([C1],(C1O)):-!, list_to_disj(C1,C1O).
list_to_disj([C1,C2],(C1O;C2O)):-!, list_to_disj(C1,C1O),list_to_disj(C2,C2O).
list_to_disj([C1|C2],(C1O;C2O)):-!, list_to_disj(C1,C1O),list_to_disj(C2,C2O).
list_to_disj(C1,C1).

get_inlined(P,Out):- bagof(I,clause_interface(P,I),DisjL),list_to_disj(DisjL,Out),!.

never_inline(P):- \+ callable(P),!.
never_inline(P):- predicate_property(P,foreign).
never_inline(P):- predicate_property(P,imported_from(system)).
never_inline(P):- predicate_property(P,number_of_clauses(N)),N==0.
never_inline(P):- compound(P),functor(P,F,A),never_inline_fa(F,A).
never_inline_fa(set_place,_).
never_inline_fa(F,_):- atom_concat_or_rtrace(_,' tabled',F).
never_inline_fa(start_tabling,_).
never_inline_fa(get_var,_).
never_inline_fa(f_sys_set_symbol_value,_).
never_inline_fa(get_opv,_).
never_inline_fa(member,_).
never_inline_fa(as_rest,_).
%never_inline_fa(t_or_nil,_).

always_inline(P):- never_inline(P),!,fail.
always_inline(P):- \+ callable(P),!,fail.
always_inline(P):- predicate_property(P,foreign),!,fail.
always_inline(P):- predicate_property(P,imported_from(system)),!,fail.
always_inline(P):- clause_interface(P,B),B=t_or_nil(_,_),!.
always_inline(P):- clause_interface(P,B),B=is(_,_),!.
always_inline(P):- compound(P),functor(P,F,A),always_inline_fa(F,A).

always_inline_fa(F,1):- atom_concat_or_rtrace('addr_tagbody_',M,F),atom_contains(M,'_addr_enter_').
always_inline_fa(F,1):- atom_concat_or_rtrace('addr_tagbody_',_,F), functor(P,F,1), \+ clause_calls_self(P).
always_inline_fa(F,_):- atom_concat_or_rtrace(_,'expand1',F).
always_inline_fa(F,_):- atom_concat_or_rtrace('cl_c',M,F),atom_concat_or_rtrace(_,'ar',M).
always_inline_fa(F,_):- atom_concat_or_rtrace('cl_c',M,F),atom_concat_or_rtrace(_,'dr',M).

maybe_inline(C1):- never_inline(C1),!,fail.

maybe_inline(C1):- always_inline(C1),
  predicate_property(C1,interpreted),
  predicate_property(C1,number_of_clauses(1)),
  \+ clause_has_cuts(C1),
  wam_cl_option(safe(inline),true),
  !.

maybe_inline(C1):- 
 \+ (functor(C1,F,_),atom_concat_or_rtrace('cl_',_,F)),
  predicate_property(C1,interpreted),
  predicate_property(C1,number_of_clauses(1)),
  \+ clause_has_cuts(C1),
  wam_cl_option(inline,true),
  !.

clause_has_cuts(P):- clause_interface(P,I),contains_var(!,I).
clause_calls_self(P):- clause_interface(P,I),functor(P,F,A),functor(C,F,A),contains_term(E,I),compound(E),E=C.

clause_interface(P,I):-clause(P,I).
clause_interface(P,I):- wl:pass_clause(_,P,I).

clause_interface(P,I,R):-clause(P,I,R).
clause_interface(P,I,R):- wl:pass_clause(R,P,I).












mize_prolog_code(In,Out):-skip_optimize(In),!,In=Out.
mize_prolog_code(In,Out):-  
   clause(mize_prolog_code1(In2,Out2),Body),
   structure_applies_here(In,In2,Body),!,
   (In \== Out2 -> mize_prolog_code(Out2,Out);Out=In),!.
mize_prolog_code(InOut,InOut).

mize_prolog_code1(maplist(_,[]),true).
mize_prolog_code1(maplist(P,[X]),call(P,X)).
mize_prolog_code1(call(F,A),Out):- atom(F),Out=..[F,A].

:- fixup_exports.



