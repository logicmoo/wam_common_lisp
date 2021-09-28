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
 * The program is a *HUGE* common-lisp compiler/interpreter. 
 *
 *******************************************************************/
:- module(dasm, []).

%:- meta_predicate(maplist_not_tail(1,*)).

%f_disassemble(Function, Code):- string(Function),downcase_atom(Function,DC),!,f_disassemble(DC, Code).
f_disassemble(function(Symbol),Options, Code):- !, f_disassemble(Symbol,Options, Code).
f_disassemble([quote,Symbol], Options,Code):- !, f_disassemble(Symbol, Options,Code).
f_disassemble(StringL,Options,Code):- \+ string(StringL),is_stringp(StringL),to_prolog_string_if_needed(StringL,String),!,f_disassemble(String,Options,Code).
f_disassemble(Function,_Options, Prolog):- 
  writeln('#| DISASSEMBLY FOR':Function),
   make_holder(Holder),
   print_related_clauses(Holder,_Module,Function),
  nb_holder_value(Holder,ListOut),
  Prolog = '$OBJ'(claz_prolog,ListOut),
  nop(ListOut==[]-> xlisting(Function) ; true),
  writeln('|#').


clauses_related(M,Obj,H,B,PrintKeyRef):- nonvar(Obj), get_opv(Obj,symbol_function,Obj2),clauses_related(M,Obj2,H,B,PrintKeyRef).
%clauses_related(M,Obj,H,B,PrintKeyRef):- nonvar(Obj), get_opv(Obj2,symbol_function,Obj),clauses_related(M,Obj2,H,B,PrintKeyRef).
clauses_related(_,P,H,B,PrintKeyRef):-
   H= wl:lambda_def(_DefType,H1,H2,_Args,_Body),
   clause_interface(H,B,PrintKeyRef),
  (related_functor(P,H1);related_functor(P,H2)).
clauses_related(_,P,H,B,PrintKeyRef):-
   H= wl:arglist_info(H1,H2,_,_),
   clause_interface(H,B,PrintKeyRef),
  (related_functor(P,H1);related_functor(P,H2)).
clauses_related(_,P,H,B,PrintKeyRef):-
   H= wl:init_args(_,H1),
   clause_interface(H,B,PrintKeyRef),
  (related_functor(P,H1)).
clauses_related(Module,P,Module:H,B,PrintKeyRef):- 
  current_module(Module),
  current_predicate(_,Module:H),
  \+ predicate_property(Module:H,imported_from(_)),
  \+ predicate_property(Module:H,foreign),  
  clause_interface(Module:H,B,PrintKeyRef),
  related_functor(P,H).


related_functor(P,Q):- to_related_functor(P,PP),to_related_functor(Q,QQ),QQ=PP,!.
%to_related_functor(P,_):- \+ callable(P),!,fail.
to_related_functor(P,PP):- string(P),atom_string(A,P),!,to_related_functor(A,PP).
to_related_functor(P,PP):- \+ compound(P),!,to_related_functor_each(P,PP).
to_related_functor(P,PP):- compound_name_arguments(P,F,[A,B,C|_]),!,
  (to_related_functor_each(F,PP);to_related_functor(A,PP);to_related_functor(B,PP);to_related_functor(C,PP)).
to_related_functor(P,PP):- compound_name_arguments(P,F,[]),!,(to_related_functor_each(F,PP)).
to_related_functor(P,PP):- compound_name_arguments(P,F,[A]),!,(to_related_functor_each(F,PP);to_related_functor(A,PP)).
to_related_functor(P,PP):- compound_name_arguments(P,F,[A,B]),!,(to_related_functor_each(F,PP);to_related_functor(A,PP);to_related_functor(B,PP)).



to_related_functor_each(P,_):- \+ atom(P),!,fail.
to_related_functor_each(P,PP):- to_related_functor_each1(P,PP).
to_related_functor_each(P,PP):- to_related_functor_each1(P,PPP),to_related_functor_each0(PPP,PP).

to_related_functor_each1(P,P).
to_related_functor_each1(P,PP):- downcase_atom(P,PP),PP\==P.

to_related_functor_each0(P,PP):-atom_concat('f_',PP,P).
to_related_functor_each0(P,PP):-atom_concat('u_',PP,P).
to_related_functor_each0(P,PP):-atom_concat('mf_',PP,P).

print_related_clauses(ExceptFor,_OModule,P):-
 ignore((
   no_repeats(clauses_related(_Module,P,H,B,PrintKeyRef)),
   PC = (H :- B),   
   nb_holder_value(ExceptFor,Printed),
   \+ member(PrintKeyRef,Printed),
   nb_holder_append(ExceptFor,PrintKeyRef),
   once(print_clause_plain(PC)),
   fail)).

print_clause_plain(I):-
  (current_prolog_flag(color_term, Was);Was=[]),!,
  make_pretty(I,O),
    setup_call_cleanup(set_prolog_flag(color_term, false),
     (nl,lcolormsg1((O))),
     set_prolog_flag(color_term, Was)).


lcolormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt9(Msg)).

% print_clause_plain(C):- portray_clause_w_vars(O).



make_pretty(I,O):- !,call_each(must_or_rtrace,(shrink_lisp_strings(I,M), pretty_numbervars(M,O))).
%make_pretty(I,O):- is_user_output,!,shrink_lisp_strings(I,O), pretty1(O),pretty2(O),pretty3(O).
%make_pretty(I,O):- I=O, pretty1(O),pretty2(O),pretty3(O).

%maplist_not_tail(_,ArgS):- var(ArgS),!.
%maplist_not_tail(G,[X|ArgS]):-call(G,X),maplist_not_tail(G,ArgS).




/*
may_debug_var(_,_,V):- nonvar(V),!.
may_debug_var(_,_,V):- variable_name(V,_),!.
may_debug_var(L,_,_):- upcase_atom(L,L),!.
may_debug_var(L,R,V):- atom(L),atom_concat('f_',LL,L),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- atomic_list_concat([_A1,A2,A3|AS],'_',L),atomic_list_concat([A2,A3|AS],'_',LL),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- debug_var([L,R],V).

may_debug_var(_,V):- nonvar(V),!.
may_debug_var(_,V):- variable_name(V,_),!.
may_debug_var(R,V):- debug_var(R,V).

pretty1(H):- \+ compound(H),!.
pretty1(as_rest(Name, Rest, _)):- may_debug_var(Name,Rest).
pretty1(get_var(Env, Name, Val)):- may_debug_var('GEnv',Env),may_debug_var(Name,Val).
pretty1(deflexical(Env,_Op, Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).
pretty1(set_var(Env,Name, Val)):- may_debug_var('SEnv',Env),may_debug_var(Name,Val).

pretty1(f_slot_value(_Env, Name, Val)):- may_debug_var(slot,Name,Val).
%pretty1(get_kw(ReplEnv, RestNKeys, test, test, f_eql, true, True)
pretty1(Env=[List|_]):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist(pretty1,List).
pretty1(Env=List):- compound(List),var(Env),List=[H|_],compound(H),H=bv(_,_), may_debug_var('Env',Env),
  maplist_not_tail(pretty1,List).
pretty1(P):- P=..[_,_|List],append(_,[Name, Val|_],List),atom(Name),var(Val),may_debug_var(Name,Val).
pretty1(debug_var(R,V)):- may_debug_var(R,V).
pretty1(bv(R,V)):- may_debug_var(R,V).
pretty1(H):-H=..[_|ARGS],must_maplist_det(pretty1,ARGS).


pretty2(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
%pretty2([H|T]):-!,maplist_not_tail(pretty2,[H|T]).
pretty2(H):-  
 always((functor(H,F,A),
   H=..[F,P1|ARGS],   
   (A>1 -> may_debug_var(F,'_Param',P1) ; true),
   must_maplist_det(pretty2,[P1|ARGS]))),!. 

pretty3(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty3(H):-pretty4(H),pretty5(H).

pretty4(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
%pretty4([H|T]):-!,maplist_not_tail(pretty4,[H|T]).
pretty4(H):-  
 ignore(((functor(H,F,_),
  wl:init_args(N,F),integer(N),
   A is N + 1,   
   arg(A,H,R),may_debug_var('KeysNRest',R)))),
   H=..[F,P1|ARGS],  
   must_maplist_det(pretty4,[P1|ARGS]),!. 

pretty5(H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty5([H | B]):- pretty5(H),pretty5(B),may_debug_var('CAR',H),may_debug_var('CDR',B).
pretty5(H):-  
 always((functor(H,F,A),
   H=..[F,P1|ARGS],   
   arg(A,H,R),may_debug_var(F,'_Ret',R),   
   nop(may_debug_var(F,'_Param',P1)),
   must_maplist_det(pretty5,[P1|ARGS]))),!. 

*/
:- fixup_exports.

      
end_of_file.