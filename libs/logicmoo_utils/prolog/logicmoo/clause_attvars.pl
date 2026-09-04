/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

:- module(clause_attvars,
   [
      attr_bind/2,attr_bind/1,
      split_attrs/3,
      clause_attv/3,
      variant_i/2,av_comp/2,
      unify_bodies/2,          
      clausify_attributes/2,
      clausify_attributes4/4
    ]).

:- set_module(class(library)).

:- module_transparent
      attr_bind/2,attr_bind/1,
      %clausify_attributes/2,
      clausify_attributes4/4,
      variant_i/2,av_comp/2,
      unify_bodies/2.

:- create_prolog_flag(assert_attvars,false,[keep(true)]).

split_attrs(B,true,B0):-var(B),!,B0=call(B).
split_attrs(call(C),A,B):-!,split_attrs(C,A,B).
split_attrs(M:B,ATTRS,BODY):- is_visible_module(M),!, split_attrs(B,ATTRS,BODY).
split_attrs(B,true,B0):-ground(B),!,B0=B.

/*
split_attrs(B,ATTRS,BODY):- \+ sanity((simple_var(ATTRS),simple_var(BODY))),
    dtrace,dumpST,dtrace(split_attrs(B,ATTRS,BODY)).
*/
split_attrs(M:attr_bind(G,Call),M:attr_bind(G),Call):- !.
split_attrs(attr_bind(G,Call),attr_bind(G),Call):- !.
split_attrs(true,true,true):-!.
split_attrs(_:true,true,true):-!.
split_attrs(M:A,M:ATTRS,M:BODY):- !,split_attrs(A,ATTRS,BODY).
split_attrs(attr_bind(G),attr_bind(G),true):- !.
split_attrs((A,B),ATTRS,BODY):- !,
  split_attrs(A,AA,AAA),
  split_attrs(B,BB,BBB),!,
  conjoin(AA,BB,ATTRS),
  conjoin(AAA,BBB,BODY).

split_attrs(AB,true,AB).

:- meta_predicate attr_bind(+).
:- module_transparent attr_bind/1.
attr_bind(Attribs):- dont_make_cyclic(catch(maplist(call,Attribs),error(uninstantiation_error(_),_),fail)).

:- meta_predicate attr_bind(+,0).
:- module_transparent attr_bind/2.
attr_bind(Attribs,Call):- attr_bind(Attribs),Call.


clause_attv(H,B,R):- nonvar(R),!, 
  dont_make_cyclic((must(system:clause(H0,BC,R)),
    must(split_attrs(BC,AV,B0)),!,
    must((catch(AV,error(uninstantiation_error(_),_),fail),!,unify_bodies(B0,B),H=H0)))).

clause_attv(M:H0,B0,Ref):- !,
 quietly(copy_term(H0:B0, H:B, Attribs)),
 dont_make_cyclic((    
    (M:clause(H,BC,Ref),
       split_attrs(BC,AV,BB), unify_bodies(B,BB) , AV , unify_bodies(H0,H),unify_bodies(B0,B),
        attr_bind(Attribs)))).

clause_attv(H0,B0,Ref):-
 quietly(copy_term(H0:B0, H:B, Attribs)),
 dont_make_cyclic((    
    (clause(H,BC,Ref),
       split_attrs(BC,AV,BB), unify_bodies(B,BB) , AV , unify_bodies(H0,H),unify_bodies(B0,B),
        attr_bind(Attribs)))).

unify_bodies(B1,B2):-strip_module(B1,M1,BB1),strip_module(B2,M2,BB2),(B2\==BB2;B1\==BB1),!,M1=M2,unify_bodies(BB1,BB2).
unify_bodies(B1,B2):- (\+ compound(B1);\+ compound(B2)),!,B1=B2.
unify_bodies(B1,B2):- B1=..[F|BB1],B2=..[F|BB2],context_module(M),maplist(M:unify_bodies,BB1,BB2).

/*

clause_attv(MH,B,Ref):- 
 dont_make_cyclic((
   % must(modulize_head(MH,M:H)),
   system:clause(MH,BMC,Ref),
    ((compound(BMC),BMC = attr_bind(Attribs,BOUT)) -> (attr_bind(Attribs)) ; BMC=BOUT))),
 dont_make_cyclic((BOUT=B)).
*/
/*
clause_attv(MH,B,Ref):- !,
 no_repeats(Ref,(must(modulize_head(MH,M:H)),system:clause(M:H,BMC,Ref))),
   ((compound(BMC),BMC = attr_bind(Attribs,BM)) -> true ; (BMC=BM -> Attribs=[])),
 BM = B,
 once(attr_bind(Attribs)).

  */
/*
clause_attv(H0,BIn,Ref):- 
    copy_term_nat(H0:BIn,H:B0),
    system:clause(H,BC,Ref),
  (must(quietly(split_attrs(BC,AV,B))) -> ( B=B0 -> AV -> H=H0 -> BIn=B)).
*/
% clause_attv(H00,B000,Ref):- unnumbervars((H00:B000),(H:B0)), split_attrs(B0,_A,B),!,clause_i(H,B,Ref), (clause_i(HH,BB,Ref),HH=@=H,BB=@=B,A).
% clause_attv(H,B,Ref):- system:clause(H,AB,Ref), (must(split_attrs(AB,A,B0)->A),B=B0).

clausify_attributes(Data,THIS):- notrace(clausify_attributes0(Data,THIS)).
clausify_attributes0(V,V):- \+ current_prolog_flag(assert_attvars,true),!.

clausify_attributes0(Data,THIS):- attvar(Data), clausify_attributes_helper(Data,THIS).
clausify_attributes0(V,V):- \+ compound(V),!.
%clausify_attributes(:-(V),:-(V)):-!.
clausify_attributes0(M:Data,M:THIS):- !,clausify_attributes(Data,THIS).
clausify_attributes0([H|T],[HH|TT]):- !,clausify_attributes(H,HH),clausify_attributes(T,TT).
%clausify_attributes((H,T),(HH,TT)):- !,clausify_attributes(H,HH),clausify_attributes(T,TT).
clausify_attributes0(Data,THIS):- clausify_attributes_helper(Data,THIS).

clausify_attributes_helper(Data,THIS):-  term_attvars(Data,Vars),Vars=[_|_],maplist(del_attr_type(vn),Vars),!,copy_term(Data,DataC,Attribs),expand_to_hb(DataC,H,B),clausify_attributes4(H,B,Attribs,THIS),!.
clausify_attributes_helper(Data,Data).


clausify_attributes4(H,B,[],(H:-B)):-!.
clausify_attributes4(H,B,Extra,(H:-attr_bind(Extra,B))).

variant_i(A,B):- A=@=B,!.
variant_i(A,B):- copy_term_nat(A:B,AA:BB), \+(AA=@=BB),!,fail.
variant_i(A,B):- term_variables(A,AV),AV\==[], 
   term_variables(B,BV),
   (maplist(av_comp,AV,BV)->!;(dtrace,maplist(av_comp,AV,BV))).

% % % OFF :- system:use_module(library(gvar_lib)).

av_comp(A,B):-get_attrs(A,AA),get_attrs(B,BB),AA=@=BB,!.
av_comp(A,B):-get_attrs(A,attr(_,_,AB1)),!,AB1\==[],get_attrs(B,attr(_,_,AB2)),!,AB1==AB2.
av_comp(_A,_B):-!.

:- fixup_exports.



