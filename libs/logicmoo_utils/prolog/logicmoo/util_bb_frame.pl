% ===================================================================
% File 'logicmoo_util_ctx_frame.pl'
% Purpose: An Implementation in SWI-Prolog of Unwindable context frames
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_ctx_frame.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
% ===================================================================
:- module(logicmoo_util_bb_frame, [all_different_bindings/1]).

:- use_module(pretty_clauses).

nb_current_no_nil(N,V):- nb_current(N,V),V\==[].

was_named_graph(NG,Name,Info2):- compound(NG),compound_name_arguments(NG,named_graph,[Name,Info1]), nonvar(Info1), var(Info2), Info1=Info2.

push_frame(Info,Frame):- atom(Frame),must(nb_current_no_nil(Frame,CG)),!,push_frame(Info,CG).
%push_frame(named_graph(Name,Info), Frame):- var(Frame),!,named_graph(Name,Info)=Frame. 
%push_frame(named_graph(Name,Info), Frame):- named_graph(Name,Info)=Frame,!.
push_frame(Info, _Frame):- var(Info),!.
push_frame(Info, Frame):- var(Frame), nb_current_no_nil(named_graph,F),
  compound_name_arguments(Frame,named_graph,[F,[]]), !,
  push_frame(Info, Frame).

push_frame(Info, Frame):- var(Frame), !, gensym(frame, F), compound_name_arguments(Frame, named_graph,[anonymous(F),[]]), push_frame(Info, Frame).


push_frame(Cmpd1, Cmpd2):-was_named_graph(Cmpd1,Name1, Info), was_named_graph(Cmpd2, Name2, Frame), Name1==Name2, !,push_frame(Info, Frame).
push_frame(Cmpd, Frame):- was_named_graph(Cmpd, Name, Info2), was_named_graph(Info2, Name, Info), !, 
  compound_name_arguments(NewArg,named_graph,[Name,Info]),
  push_frame(NewArg, Frame).
push_frame(Cmpd, Frame):- was_named_graph(Cmpd,_Name, Info2), was_named_graph(Info2, Name, Info), !, 
  compound_name_arguments(NewArg,named_graph,[Name,Info]),
  push_frame(NewArg, Frame).
push_frame(Cmpd,_Frame):- was_named_graph(Cmpd,_Name, Info2), Info2 ==[].
/*
push_frame(named_graph(Name,[H|List]), Frame):- fail, nonvar(H),!,
 push_frame(named_graph(Name,H), Frame),
 push_frame(named_graph(Name,List), Frame).
*/
push_frame(Cmpd, Frame):- was_named_graph(Cmpd, anonymous(_),Info), !, push_frame(Info, Frame).
push_frame(Cmpd, Frame):- was_named_graph(Cmpd, Name, Info),
   compound_sub_term(Sub, Frame), 
   was_named_graph(Sub, Name, SubFrame), !, 
   push_frame(Info, SubFrame).


push_frame(Info, call(Frame)):- !,call(Frame,Info),!.
push_frame(Info, cg(Frame)):- !, push_frame(Info, Frame),!.
push_frame(Info, _Frame):- Info==[],!.
push_frame([I1|I2], Frame):- !, push_frame(I1, Frame), push_frame(I2, Frame).
push_frame('&'(I1,I2), Frame):- !, push_frame(I1, Frame), push_frame(I2, Frame).

push_frame(Info, Frame):- do_eval_or_same(Info, BetterInfo), Info\=@=BetterInfo, push_frame(BetterInfo, Frame).

push_frame(Info, Frame):- member(Sub, Frame), Sub==Info, !.
push_frame(Info, Frame):- Frame = [H|T],!, setarg(2, Frame, [H|T]), setarg(1, Frame, Info).
push_frame(Info, Frame):- compound(Frame), functor(Frame,_,A),arg(A,Frame,E),
  (E == [] -> setarg(A,Frame,[Info]) ; push_frame(Info, E)).

get_frame(Frame, Frame):- \+ (Frame= cg(_)),!.
get_frame(cg(Frame), Frame):-!.


compound_sub_term(X, X).
compound_sub_term(X, Term) :- 
    compound(Term), 
    \+ functor(Term,preconds,_),
    arg(_, Term, Arg), 
    compound(Arg),
    compound_sub_term(X, Arg).

%  LocalContexts
%   They hold name-values in
%     -- assoc/1 lists
%     -- open tailed lists
%     -- frame/1 contains one or more of the above

% v/3s 
%  = v(Value,Setter,KeyDestructor)

% frame/3s
%  = frame(Named,Destructor,Ctx)

% well i played with a couple few differnt environment impls.. they have their pros cons.. one impl.. 
% that was unique is that an array of "binding pairs" live in an arraylist.. to be "in" an environment 
% it meant that you held an "index" into the arry list that as you went backwards you'd find your bindings.. each symbol had a java ftInt field "lastBindingIndex" 
% .. that was a "hint" to where you could fastforward the backwards search .. end named binding context also had a "index" to when you leave a named block.. 
% you could quickly reset the top of an index.
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_ctx_frame.pl

do_eval_or_same(G, GG):- \+ compound(G), !, GG=G.
do_eval_or_same([G1|G2], [GG1|GG2]):- !, do_eval_or_same(G1, GG1), do_eval_or_same(G2, GG2).
do_eval_or_same({O}, {O}):- !.
do_eval_or_same(G, GG):- compound_name_arguments(G, HT, [F|GL]), atom(F), member(HT, [t, h]), !,
 compound_name_arguments(GM, F, GL), !, do_eval_or_same(GM, GG).

do_eval_or_same(textString(P, G), textString(P, GG)):- ground(G), must(to_string_lc(G, GG)), !.
/*
do_eval_or_same(PEG, PEGG):- xnotrace((compound_name_arguments(PEG, F, Args), downcase_atom(F, D), (atom_concat(_, 'text', D);atom_concat(_, 'string', D)),
  append(Left, [G], Args))), ground(G), \+ string(G), !, must(to_string_lc(G, GG)), !,
  append(Left, [GG], NewArgs), compound_name_arguments(PEGG, F, NewArgs).
*/
do_eval_or_same(iza(P, G), Out):- !, do_eval_or_same(isa(P, G), Out). 
do_eval_or_same(isa(P, G), isa(P, GG)):- ground(G), !, must(asCol(G, GG)), !.

do_eval_or_same(xfn(P, G), GG):- !, must( call(P, G, GG)), !.
do_eval_or_same(G, GG):- compound_name_arguments(G, F, GL), F\==percept_props, !,
 maplist(do_eval_or_same, GL, GGL), !, compound_name_arguments(GG, F, GGL).
do_eval_or_same(G, G).


get_frame_vars(Frame,FVs):-
  get_frame(Frame,List), 
  setof(Var,(sub_term(Var,List),compound(Var),functor(Var,frame_var,_)),FVs),!.
get_frame_vars(Frame,FVs):-
  get_frame(Frame,List), 
  setof(frame_var(Var, RealVar),frame_var(Var,List,RealVar),FVs),!.
get_frame_vars(_Frame,[]).



merge_simular_graph_vars(CG,PCG):- 
  get_frame_vars(CG,FV),
  get_frame_vars(PCG,PFV),
  combine_gvars(FV,FV),
  combine_gvars(PFV,PFV),
  combine_gvars(PFV,FV),
  combine_gvars(FV,PFV),!.

combine_gvars([],_):-!.
combine_gvars(_,[]):-!.
combine_gvars([S|S1],S2):- ignore(member(S,S2)),
  combine_gvars(S1,S2).


merge_simular_vars([],[]):-!.
merge_simular_vars([One|Rest],List):-  member(One,Rest),merge_simular_vars(Rest,List),!.
merge_simular_vars([One|Rest],[One|List]):-  merge_simular_vars(Rest,List),!.

resolve_frame_constants(CG0,CG):-
 get_frame_vars(CG0,FVs),
 merge_simular_vars(FVs,SFVs),
 resolve_frame_constants(SFVs,CG0,CG1),
 must(correct_frame_preds(CG1,CG)).

event_frame_pred('agnt').
event_frame_pred('inst').

correct_frame_preds([H|CG1],CG):- !, 
  correct_frame_preds(H,H1),!,
  correct_frame_preds(CG1,CG2),
  flatten([H1,CG2],CG).

correct_frame_preds(FrameP,FramePO):- compound(FrameP),
  compound_name_arguments(FrameP,F,[A,B|C]),
  %downcase_atom(F,DC),
  =(F,DC),
  compound_name_arguments(FramePO,DC,[A,B|C]), !,  
  ignore((event_frame_pred(DC) -> debug_var('_Event',A), nop(debug_var('Doer',B)))).
correct_frame_preds(CG,CG).

resolve_frame_constants([],IO,IO):-!.
resolve_frame_constants([DoConst|More],Props,Out):- !, 
  resolve_frame_constants(DoConst,Props,Mid),
  resolve_frame_constants(More,Mid,Out).
resolve_frame_constants(frame_var(Var, RealVar),Props,Out):-
  downcase_atom(Var,VarD),
  upcase_atom(Var,VarU),
  % sUbst(Props,frame_var(Var, RealVar),[],Mid),
  sUbst_each(Props,[

  ?(RealVar)=RealVar,?(Var)=RealVar,?(VarD)=RealVar,?(VarU)=RealVar,
  *(RealVar)=RealVar,*(Var)=RealVar,*(VarD)=RealVar,*(VarU)=RealVar,
      Var=RealVar,VarU=RealVar,VarD=RealVar],Out),!.
resolve_frame_constants(_,Mid,Mid).


frame_var(_, Frame, _):- \+ compound(Frame), !, fail.
frame_var(Name, cg(Frame), Var):- !, frame_var(Name, Frame, Var).
frame_var(Name, Frame, Var):- nonvar(Var), !, frame_var(Name, Frame, NewVar), !, NewVar=Var.
frame_var(Name, Frame, Var):- compound(Name), !, arg(_, Name, E), frame_var(E, Frame, Var), !.
frame_var(Name, [Frame1|Frame2], Var):- !, (frame_var(Name, Frame1, Var);frame_var(Name, Frame2, Var)).
frame_var(Name, frame_var(Prop, Var),Var):- !, same_name(Name, Prop).
frame_var(Name, cg_name(Var, Prop),Var):- !, same_name(Name, Prop).
frame_var(Name, Prop = Var, Var):- !, same_name(Name, Prop).
frame_var(Name, f(Pred, 1, [Var]), Var):- !, same_name(Name, Pred).
frame_var(Name, f(_, _, [Prop|List]), Var):- !, same_name(Name, Prop), last(List, Var).
frame_var(Name, Frame, Var):- fail, compound_name_arity(Frame, Pred, Arity), Arity > 0, compound_name_arguments(Frame, Pred, List),
  frame_var(Name, f(Pred, Arity, List), Var).
frame_var(Name, Frame, Var):- arg(_, Frame, E), frame_var(Name, E, Var), !.

asCol(A, A):- var(A), !.
asCol(A, 'TypeFn'(A)):- \+ callable(A), !.
asCol(A, S):- format(atom(S), '~w', [A]).

to_upcase_name(V, V):- var(V), !.
to_upcase_name('$VAR'(T), N):- !, to_upcase_name(T, N).
to_upcase_name('?'(T), N):- !, to_upcase_name(T, N).
to_upcase_name('*'(T), N):- !, to_upcase_name(T, N).
to_upcase_name(T, N):- compound(T), !, compound_name_arity(T, A, _), !, to_upcase_name(A, N).
to_upcase_name(T, N):- format(atom(A), '~w', [T]), upcase_atom(A, N).

to_downcase_name(V, N):- var(V), !, N = V.
to_downcase_name('$VAR'(T), N):- !, to_downcase_name(T, N).
to_downcase_name('?'(T), N):- !, to_downcase_name(T, N).
to_downcase_name('*'(T), N):- !, to_downcase_name(T, N).
to_downcase_name(T, N):- compound(T), !, compound_name_arity(T, A, _), !, to_downcase_name(A, N).
to_downcase_name(T, N):- format(atom(A), '~w', [T]), downcase_atom(A, N).

same_name(T1, T2):- var(T1),!,ground(T2), to_downcase_name(T1,T2).
same_name(T1, T2):- T1 = T2,!.
same_name(T1, T2):- ground(T1), ground(T2), to_upcase_name(T1, N1), to_upcase_name(T2, N2), !, N1==N2.



%frame_to_asserts(List, cmdFrame(Frame)):- is_list(List), sort(List, ListR), list_to_conjuncts('&', ListR, Frame), !.
%frame_to_asserts(Frame, cmdFrame(Frame)).
frame_to_asserts(Frame, Asserts):- get_frame(Frame, Asserts),!.

frame_defaults([], _Frame):-!.
frame_defaults([FrameArg| FrameArgS], Frame):-
   ignore((
     member(var(NewArg), FrameArg), var(NewArg),
     member(default(D), FrameArg),
     debug_var(D, NewArg),
    % D=NewArg,
   !)),
   frame_defaults(FrameArgS, Frame).

subst_into_list([], []).
subst_into_list(+(AB), [optional(true)|AABB]):- !, subst_into_list(AB, AABB), !.
subst_into_list(A+B, AABB):-!, subst_into_list(A, AA), subst_into_list(B, BB), append(AA, BB, AABB).
subst_into_list([A|B], AABB):-!, subst_into_list(A, AA), subst_into_list(B, BB), append(AA, BB, AABB).
subst_into_list(A, [A]):-!.

fix_frame_args([], []).
fix_frame_args([LastArg, []], BetterFrameArgS):- !, fix_frame_args([LastArg], BetterFrameArgS).
fix_frame_args([FrameArg| FrameArgS], [[slot(Slot)|FrameArgL]|BetterFrameArgS]):-
  subst_into_list(FrameArg, FrameArgL),
  ignore(member(var(NewArg), FrameArgL)),
  ignore((member(default(Name), FrameArgL), functor(Name, F, _), debug_var(F, NewArg), debug_var(F, Slot))),
  fix_frame_args(FrameArgS, BetterFrameArgS).

compute_frame_slots([], []).
compute_frame_slots([FrameArg| FrameArgS], [FrameSlot|FrameSlotS]):-
  frame_arg_to_slot(FrameArg, FrameSlot),
  compute_frame_slots(FrameArgS, FrameSlotS).
compute_frame_slots([_FrameArg| FrameArgS], FrameSlotS):-
  compute_frame_slots(FrameArgS, FrameSlotS).

frame_arg_to_slot(FrameArg, Name=NewArg):-
   % \+ member(optional(true), FrameArg),
   (member(var(NewArg), FrameArg);member(slot(NewArg), FrameArg)), !,
   (member(pred(Name), FrameArg);member(prep(Name), FrameArg);member(default(Name), FrameArg)), !.

frmprint(Frame) :- get_frame(Frame,GFrame),frmprint0(GFrame).
frmprint0(Frame) :- \+ is_list(Frame),!,frmprint_e(Frame).
frmprint0(I) :-
    catch(make_pretty(I, Frame), _, I=Frame),
    guess_pretty(Frame),
    predsort(frcmp, Frame, FrameA),
    reverse(FrameA, FrameO),
    frmprint_e(FrameO).
frmprint_e(I) :- 
 pretty_clauses:((
  catch(make_pretty(I, Frame), _, I=Frame),
    guess_pretty(Frame),
 with_output_to(atom(A),print_tree_nl(Frame)), format('~N~w~n', [A]))).

sortDeref(P, PP):- \+ compound(P), !, P=PP.
%sortDeref(isa(X, Y), visa(X, Y)):-!.
sortDeref(~(P), PP):-!, sortDeref(P, PP).
sortDeref(P, PP):- arg(1, P, PP), compound(PP).
sortDeref(P, PP):- safe_functor(P, F, N), wrapper_funct_sortin(F), arg(N, P, E), !, sortDeref(E, PP).
sortDeref(P, P).


all_different_bindings([]):- !.
all_different_bindings([_]):- !.
all_different_bindings([X, Y]):- !, dif(X, Y).
all_different_bindings([X, Y, Z]):- !, dif(X, Y), dif(X, Z), dif(Z, Y).
all_different_bindings([X|Text]):- maplist(dif(X), Text), all_different_bindings(Text).

wrapper_funct_sortin(F):- arg(_, v(~, post, pre), F).
wrapper_funct_correction(F):- arg(_, v(~, post, normally, pre), F).

correct_normals(Nil, Nil):- Nil==[], !.
correct_normals(EOL, []):- EOL==end_of_list, !.
correct_normals(UNormals, Normals):- \+ compound(UNormals), !, [UNormals]=Normals.
correct_normals(~(PreU), Normals):- compound(PreU), PreU=pre(U), !, correct_normals(pre(~(U)), Normals).
correct_normals((U, UU), Normals):- !, correct_normals(U, UC), correct_normals(UU, UUC), !, append(UC, UUC, Normals).
correct_normals([U|UU], Normals):- !, correct_normals(U, UC), correct_normals(UU, UUC), !, append(UC, UUC, Normals).
correct_normals(P, Normals):- P=..[F, A1, A2|List], wrapper_funct_correction(F),
  P1=..[F, A1], P2=..[F, A2|List], !,
  correct_normals([P1|P2], Normals).
correct_normals(Normal, [Normal]).


frcmp(Cmp, P1, P2):- (\+ compound(P1) ; \+ compound(P2)), !, compare(Cmp, P1, P2).
frcmp(Cmp, P1, P2):- N=1, (arg(N, P1, A);arg(N, P2, A)), is_list(A), !, compare(Cmp, P1, P2).
frcmp(Cmp, P2, P1):- sortDeref(P1, PP1)->P1\=@=PP1, !, frcmp(Cmp, P2, PP1).
frcmp(Cmp, P1, P2):- sortDeref(P1, PP1)->P1\=@=PP1, !, frcmp(Cmp, PP1, P2).
frcmp(Cmp, P1, P2):- N=1, arg(N, P1, F1), arg(N, P2, F2), F1==F2, !, compare(Cmp, P1, P2).
frcmp(Cmp, P1, P2):- safe_functor(P1, F1, _), safe_functor(P2, F2, _), F1\==F2, compare(Cmp, F1, F2), Cmp \= (=), !.
frcmp(Cmp, P1, P2):- arg(N, P1, F1), arg(N, P2, F2), frcmp(Cmp, F1, F2), Cmp \= (=), !.
frcmp(Cmp, P1, P2):- compare(Cmp, P1, P2).
%reframed_call( Pred, Doer, [give, Object, to, Recipient], give(Doer, Object, Recipient), _Mem):- !.



sUbst_each(A, [NV|List], D) :-
    (   NV=..[_, N, V]
    ->  true
    ;   NV=..[N, V]
    ),
    !,
    sUbst(A, N, V, M),
    sUbst_each(M, List, D).
sUbst_each(A, _, A).




sUbst(A, B, C, D) :-
    notrace(nd_sUbst(A, B, C, D0)),
    on_x_debug(D=D0), !.




nd_sUbst(Var, VarS, SUB, SUB) :-
    Var==VarS,
    !.
nd_sUbst(Var, _, _, Var) :-
    (\+ compound(Var); Var = '$VAR'(_)),
    !.

nd_sUbst([H|P], X, Sk, [H1|P1]) :- !, 
    nd_sUbst(H, X, Sk, H1), 
    nd_sUbst(P, X, Sk, P1).

nd_sUbst(P, X, Sk, P1) :-
    compound_name_arguments(P, Fc, Args),
    nd_sUbst2(X, Sk, Fc, 0, [Fc|Args], [RFc|RArgs]),
    compound_name_arguments(P1, RFc, RArgs).

nd_sUbst2(_, _, _, _, [], []):-!.
nd_sUbst2(X, Sk, Fc, N, [A|Args], [R|RArgs]):-
  subst_arg(X, Sk, Fc, N, A, R),
  N1 is N + 1,
  nd_sUbst2(X, Sk, Fc, N1, Args, RArgs).

subst_arg(X, Sk, Fc, N, A, R):- \+ skipped_replace(Fc,N), nd_sUbst(A, X, Sk, R).
subst_arg(_,  _,  _, _, A, A).

skipped_replace('$VAR',_).
skipped_replace('frame_var',1).
skipped_replace('cg_name',2).
skipped_replace('cg_values',2).

:- fixup_exports.


