:- module(gapi,
[
          nb_current_value/2, % +Key, ?Value
          nb_set_value/2,  % +Key, +Value
          b_set_value/2,  % +Key, +Value
          b_get_value/2,  % +Key, ?Value
          nb_link_value/2, % +Key, +Value
          tracker_reset/0,
          get_current_tracker/1,
          nb_current_value/3, % +Key, ?Value
          nb_set_value/3,  % +Key, +Value
          b_set_value/3,  % +Key, +Value
          b_get_value/3,  % +Key, ?Value
          nb_link_value/3, % +Key, +Value
          tracker_reset/1,
          get_current_tracker/2,
          show_name_values/0,
          push_tracker_frame/0
 ]).
/** <module> gapi - Global Variable Strorage

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- system:use_module(library(rbtrees)).
:- system:use_module(library(assoc)).
:- system:use_module(library(nb_rbtrees)).
%:- system:use_module(library(rbtrees)).


show_name_values:- 
 ignore(forall(nb_current_value(gvar(_),N,V),show_name_value(N,V))),
 format('~N~n',[]).
 % ignore(forall(nb_current_value(0,N,V),show_name_value(N,V))).

show_name_value(N,_):- atomic(N),format('~N~n',[]),fail.
show_name_value(N,V):-  attvar(V),!, show_name_value_0(N,V).
show_name_value(N,V):-  \+ compound(V), !, show_name_value_0(N,V).
show_name_value(N,Tree):- is_ootree(Tree),
    ignore(((oo_empty(Tree), show_name_value_0(N,is_ootree_empty(Tree))))),
    forall(nb_current_value(Tree,NS,V),show_name_value('-'(N,NS),V)),!.

show_name_value(N,List):- fail,is_list(List), (( \+ \+ ((member(M,List),  (compound(M);attvar(M)))))),!,
  forall(nth1(I,List,V),show_name_value('::'(N,I),V)),!. 
show_name_value(N,V):-show_name_value_0(N,V).

% show_name_value_0('::'(N,I),V):- format( '~N ~p == ~q.~n', [N , V] ).
show_name_value_0(N,V):- simplify_printing(V,VV), format( '~N ~p == ~p.~n', [N , VV] ).
%   fmt9( gapi(N) :- V ).


% simplify_printing(VV,V):- compound(V),compound_name_arguments(V,N,VAs),
simplify_printing(V,V).


:- meta_predicate fail_if_undefined(0).
fail_if_undefined(G):-catch(G,existence_error(_,_),fail).

% gvar_oov
% gvar_v
% oov

nb_current_value(N,V):- quietly(nonvar(N)->once(((context_default(Ctx), nb_current_value(Ctx,N,V))));(((context_default(Ctx), nb_current_value(Ctx,N,V))))).

nb_current_value(Attvar,N,V):- attvar(Attvar),!,just_attrs(Attvar,N),get_attr(Attvar,N,V).
nb_current_value(gvar(Type),N,V):- (nonvar(Type)->!;true), nb_current(N,V0),V0\==[],get_value_value(Type,V0,V).
nb_current_value(Tracker,N,V):- is_ootree(Tracker),!,oo_in(N,_,Tracker),oo_lookup(N,V0,Tracker),
  must(get_value_value(oov,V0,V)).
nb_current_value(Var,N,V):- nonvar(Var),Var=[E|List], !, member(Ctx,[E|List]),nb_current_value(Ctx,N,V),!.
nb_current_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),nb_current_value(Tracker,N,V).

just_attrs(_,N):- atom(N),!.
just_attrs(AttVar,N):-var(N),!,get_attrs(AttVar,Atts),atts_names(Atts,N).
atts_names(att(N0,_,Atts),N):- N=N0; atts_names(Atts,N).

nb_link_value(N,V):- quietly((context_default(Ctx), nb_link_value(Ctx,N,V))),!.

nb_link_value(Tracker,N,V):- is_ootree(Tracker),!,nb_oo_link(Tracker,N,V).
nb_link_value([E|List],N,V):- nonvar(E),!, member(Ctx,[E|List]),nb_link_value(Ctx,N,V),!.
nb_link_value(gvar(v),N,V):- !, nb_linkval(N,V).
nb_link_value(gvar(_),N,V):- !, nb_current(N,Was),!,(((compound(Was),Was=oov(_)))->nb_setarg(1,Was,V);nb_linkval(N,V)).
nb_link_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),!,nb_oo_link(Tracker,N,V).

dupe_same(X,Y):- duplicate_term(X,Y),X=Y.

/*
nb_oo_link(Tracker,N,V):- 
  (nb_oo_get_node(Tracker,N,Node)
    -> (dupe_same(V,D),nb_oo_node_value(Node,RBV),nb_linkarg(1,RBV,D))
    ;  nb_oo_insert(Tracker,N,oov(V))).
*/

nb_set_value(N,V):- quietly((context_default(Ctx), nb_set_value(Ctx,N,V))),!.

nb_set_value(Attvar,N,V):- attvar(Attvar),!,nb_put_attr(Attvar,N,V).
nb_set_value(Attvar,N,V):- is_dict(Attvar),!,nb_put_attr(Attvar,N,V).
nb_set_value(gvar(v),N,V):- !, nb_setval(N,V).
nb_set_value(gvar(_),N,D):- !, nb_current(N,Was),!,(((compound(Was),Was=oov(_)))->(duplicate_term(D,V),nb_setarg(1,Was,V));nb_setval(N,D)).
nb_set_value([E|List],N,V):- !, member(Ctx,[E|List]),nb_set_value(Ctx,N,V),!.
nb_set_value(Ctx,N,D):- get_current_tracker(Ctx,Tracker),!,mcopy_term(D,V),nb_oo_link(Tracker,N,V),!.
mcopy_term(X,Y):- duplicate_term(X,Y).

b_set_value(N,V):- quietly((context_default(Ctx), b_set_value(Ctx,N,V))),!.

b_set_value(Attvar,N,V):- attvar(Attvar),!,put_attr(Attvar,N,V).
b_set_value(gvar(v),N,V):- !, b_setval(N,V).
b_set_value(gvar(_),N,V):- !, nb_current(N,Was),!,(((compound(Was),Was=oov(_)))->setarg(1,Was,V);b_setval(N,V)).
b_set_value([E|List],N,V):- !, member(Ctx,[E|List]),b_set_value(Ctx,N,V),!.
b_set_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),
  (oo_lookup(N,RBV,Tracker)-> setarg(1,RBV,V) ; (RBV=oov([]),nb_oo_insert(Tracker,N,RBV),setarg(1,RBV,V))).


b_get_value(N,V):- quietly((context_default(Ctx), b_get_value(Ctx,N,V))),!.
b_get_value(Attvar,N,V):- attvar(Attvar),!,get_attr(Attvar,N,V).
b_get_value(gvar(Type),N,V):- !,b_getval(N,V0),get_value_value(Type,V0,V).
b_get_value([E|List],N,V):- !, member(Ctx,[E|List]),b_get_value(Ctx,N,V),!.
b_get_value(Ctx,N,V):- get_current_tracker(Ctx,Tracker),oo_lookup(N,RBV,Tracker),!,maybe_deref(RBV,V).


maybe_deref(RBV,O):- (compound(RBV),RBV=oov(O))->true;O=RBV.

nb_get_value(N,V):- context_default(Ctx), nb_get_value(Ctx,N,V),!.

:- export(nb_get_value/3).
nb_get_value(Ctx,N,V):- b_get_value(Ctx,N,V).

reset_oo_tree(Tracker):- oo_empty(X),compound(X),arg(1,X,L),arg(2,X,R),nb_setarg(1,Tracker,L),nb_setarg(2,Tracker,R).

tracker_reset(Ctx):-get_current_tracker(Ctx,Tracker),reset_oo_tree(Tracker).
tracker_reset:- oo_empty(Tracker),nb_setval('$tracker',[Tracker]).
push_tracker_frame :- oo_empty(Tracker),nb_current('$tracker',Trackers),b_setval('$tracker',[Tracker|Trackers]).
get_nth_tracker(Ctx,Tracker):- nb_current('$tracker',Trackers)->nth_tracker(Ctx,Trackers,Tracker);(tracker_reset,nb_current('$tracker',Trackers),nth_tracker(Ctx,Trackers,Tracker)).


context_default(0).
context_default(gvar(v)).

get_current_tracker(Tracker):- context_default(Ctx), get_current_tracker(Ctx,Tracker),!.

get_current_tracker(Ctx,Ctx):- nonvar(Ctx),is_ootree(Ctx),!.
get_current_tracker(Ctx,Tracker):- compound(Ctx),!,get_named_tracker(Ctx,Tracker),!.
get_current_tracker(Ctx,Tracker):- get_nth_tracker(Ctx,Tracker),!.

nth_tracker(Ctx,Trackers,Tracker):- (integer(Ctx);var(Ctx)),nth0(Ctx,Trackers,Tracker).

set_named_tracker(#(Ctx), 
         Tracker):- (var(Tracker)->oo_empty(Tracker);true),nb_linkval(Ctx,Tracker).
set_named_tracker(?(Ctx),Tracker):- (var(Tracker)->oo_empty(Tracker);true),
  get_nth_tracker(0,TrackerTracker),
  nb_link_value(TrackerTracker,Ctx,Tracker).

get_named_tracker(#(Ctx),Tracker):- nb_current(Ctx,Tracker)->assertion(is_ootree(Tracker));set_named_tracker(#(Ctx),Tracker).
get_named_tracker(?(Ctx),Tracker):- get_nth_tracker(0,TrackerTracker),
     (nb_get_value(TrackerTracker,Ctx,Tracker) -> assertion(is_ootree(Tracker)) ; 
        (oo_empty(Tracker),nb_link_value(TrackerTracker,Ctx,Tracker))).


get_value_value(v,V0,V):- \+ compound(V0),!,V0=V.
get_value_value(oov,oov(V0),V):- !,V0=V.
get_value_value(v,V0,V):- !,V0=V.
get_value_value(_,V0,V):- !,V0=V.

set_value_value(v,V0,V):- V0=V.
set_value_value(oov,V,oov(V0)):- !,V0=V.
set_value_value(_,V,V0):- !,V0=V.



gv_nop(_).


b_get_oo_value(Tree,N,V):-  oo_lookup(N,RBV,Tree),!,arg(1,RBV,V).
nb_get_oo_value(Ctx,N,V):- b_get_value(Ctx,N,V).
reset_oo(Tree):- oo_empty(X),functor(Tree,_,A), forall(between(1,A,N),(arg(N,X,R),nb_setarg(N,Tree,R))).
oo_put_value(Tree,N,V):- % as_ref(Tree0,Tree),
  (oo_lookup(N,RBV,Tree)-> setarg(1,RBV,V) ; (RBV=oov([]),nb_oo_insert(Tree,N,RBV),setarg(1,RBV,V))).

nb_oo_link(Tree,N,V):- 
  (nb_oo_get_node(Tree,N,Node)
    -> (dupe_same(oov(V),D),nb_oo_set_node_value(Node,D))
    ;  nb_oo_insert(Tree,N,oov(V))).


is_ootree(Tree):- compound(Tree),is_rbtree(Tree),!.
is_ootree(Tree):- is_dict(Tree),!.
is_ootree(Tree):- fail_if_undefined(is_oo(Tree)).


% oo_new(Type,Tree):- var(Type),oo_default(Tree),!,oo_holder_type(Tree,Type).
oo_new(rbtree,Tree) :- !, rb_new(Tree).
oo_new(_,Dict) :- fail_if_undefined(new_oo(_,_,Dict)),!.

oo_empty(Tree):- is_rbtree(Tree),!,rb_empty(Tree).
oo_empty(Tree):- is_dict(Tree),!,Tree=_{}.
oo_empty(Tree):- rb_new(Tree).

nb_oo_insert(Tree, Key, Value):- is_rbtree(Tree),!,nb_rb_insert(Tree,Key,Value).
nb_oo_insert(Tree, Key, Value):- fail_if_undefined(oo_set(Tree,Key,Value)).

nb_oo_set_node_value(node(Tree, Key), Value):- !, fail_if_undefined(oo_set(Tree,Key,Value)).
nb_oo_set_node_value(Node, Value):- nb_rb_set_node_value(Node, Value).
nb_oo_node_value(node(Tree, Key), Value):- !, fail_if_undefined(oo_get_attr(Tree,Key,Value)).
nb_oo_node_value(Node, Value):- nb_rb_node_value(Node, Value).

nb_oo_get_node(Tree, Key, Node):- is_rbtree(Tree),!,nb_rb_get_node(Tree, Key, Node). 
nb_oo_get_node(Tree, Key, node(Tree, Key)).


oo_in(Key, Value, Tree):- is_rbtree(Tree),!,rb_in(Key, Value, Tree).
oo_in(Key, Value, Tree):- is_dict(Tree),!,get_dict(Key,Tree,Value).
oo_in(Key, Value, Tree):- fail_if_undefined(oo_get_attr(Tree,Key,Value)).

oo_lookup(Key, Value, Tree):- is_rbtree(Tree),!,rb_lookup(Key, Value, Tree).
oo_lookup(Key, Value, Tree):- fail_if_undefined(oo_get_attr(Tree,Key,Value)).

init_tracker:- (( \+ nb_current('$tracker',_)) -> true; (oo_empty(Tracker),nb_setval('$tracker',[Tracker]))).


nb_put_attr(X,Name,AttVal):- get_attrs(X,Atts)->(assertion(Atts\==[]),nb_att3_put_attr(Atts,Name,AttVal));
  put_attrs(X,att(Name,AttVal,[])).
   
nb_del_attr(X,Name):- get_attrs(X,Atts),nb_att3_del_attr(Atts,Name).
nb_put_attrs(X,att(N,V,R)):- get_attrs(X,Atts),assertion((atom(N),Atts\==[])),nb_set_att3(Atts,N,V,R).
nb_del_attrs(X):- get_attrs(X,Atts),(Atts==[]->true;nb_set_att3(Atts,'$dead','$dead',[])).

nb_set_att3(Atts,N,V,R):- nb_setarg(1,Atts,N),nb_setarg(2,Atts,V),nb_setarg(3,Atts,R).
nb_att3_put_attr(Atts,Name,AttVal):- 
  Atts = att(OldName,_,Rest),
  (OldName == Name -> nb_setarg(2,Atts,AttVal) ;
  Rest==[] -> nb_setarg(3,Atts,att(Name,AttVal,[]));
  nb_att3_put_attr(Rest,Name,AttVal)).

nb_att3_del_attr(Atts,Name):- 
  Atts = att(OldName,_,Rest),
  (OldName == Name -> 
    (Rest=att(N,V,R) -> nb_set_att3(Atts,N,V,R) ;
         nb_set_att3(Atts,'$dead','$dead',[])) ;     
  Rest==[] -> true ; 
  nb_att3_del_attr(Rest,Name)).

nb_att3_get_attr(atts(Name0,Value0,Atts),Name,Value):- Name==Name0 -> Value=Value0; nb_att3_get_attr(Atts,Name,Value).

:- fixup_exports.

:- include(gvar_fixup_exports).

:- system:reexport(gvar_globals_api).
:- if(exists_source(library(dictoo_lib))).
:- system:reexport(library(dictoo_lib)).
:- endif.
:- init_tracker.

