:- if(current_predicate(lmcode:combine_logicmoo_utils/0)).
:- module(logicmoo_util_bb_gvar,
    [  % when the predciates are not being moved from file to file the exports will be moved here
      ]).  

:- else.

:- endif.

:- fixup_exports.

end_of_file.

% % % OFF :- system:use_module(library(rbtrees)).
% % % % OFF :- system:use_module(library(nb_rbtrees)).

p_e(P,ENV):-functor(P,ENV,_),!. % ,functor(ENV,F,A).
p_e(_P,test123).

pe_get(P,ENV,Q):-p_e(P,ENV), nb_getval(ENV,Q),!.
pe_set(P,ENV,Q):-p_e(P,ENV), nb_setval(ENV,Q),!.

pe_get_key(P,_,Q):-p_e(P,Q).

:- dynamic(in_bb/2).

gvar_update_value(Before,After):-p_e(Before,BB),nb_current(BB,Before),!,nb_setval(BB,After).
gvar_update_value(Before,After):-retract(Before),assert(After),!.
gvar_update_value(Before,After):-env_retract(Before),env_assert(After).

gvar_value(BB,OP,Value):-must(gvar_value0(BB,OP,Value)).
gvar_value0(BB,call,Value):-!,nb_current(BB,Value),!.
gvar_value0(BB,assert,Value):-!,nb_setval(BB,Value).
gvar_value0(BB,asserta,Value):-!,nb_setval(BB,Value),!.
gvar_value0(BB,retract,Value):-nb_getval(BB,Value),!,nb_setval(BB,[]).
gvar_value0(BB,retractall,_):-nb_setval(BB,[]).

gvar_list(BB,call,Value):-!,must(nb_current(BB,List)),!,dmsg(member(Value,List)),!,member(Value,List).
gvar_list(BB,OP,Value):-must(gvar_list0(BB,OP,Value)).

gvar_list0(BB,assert,Value):-!,must(nb_current(BB,List)), (List=[] ->  nb_setval(BB,[Value]); append_el_via_setarg(List,Value)).
gvar_list0(BB,asserta,Value):-!,must(nb_current(BB,List)),nb_setval(BB,[Value|List]).
gvar_list0(BB,retract,Value):-!,must(nb_current(BB,List)), ( List=[Value|Rest]-> nb_setval(BB,Rest); remove_el_via_setarg(List,Value) ).
gvar_list0(BB,retractall,_F/_A):- !,nb_setval(BB,[]).
gvar_list0(BB,retractall,Value):- args_all_vars(Value)-> nb_setval(BB,[]) ;  
  ((   must(nb_current(BB,List)) , gvar_remove_all_list_matches(BB,List,Value) )).


args_all_vars(Value):- compound(Value), \+ ((arg(_,Value,Nv),nonvar(Nv))).

gvar_remove_all_list_matches(BB,List,Value) :-
  ( List ==[] -> true ; 
    ((List \= [Value|Rest] ->  gvar_remove_all_list_matches(BB, Rest,Value) ;
       ((nb_setval(BB,Rest),gvar_remove_all_list_matches(BB,Rest,Value)))))).

remove_el_via_setarg(List,Value):- [_|T] = List, [_,Was|_] = List,(Was=Value -> nb_setarg(2,List,T) ;  remove_el_via_setarg(Was|T,Value)).
append_el_via_setarg(List,Value):- List = [_|T], (T==[] -> setarg(2,List,[Value]) ; append_el_via_setarg(T,Value)).



bnb_current(BB,LIST):- trace_or_throw(bnb_current(BB,LIST)),!,fail.
bnb_current(BB,LIST):-nb_current(BB,LIST),!.
bnb_current(BB,LIST):-nb_setval(BB,[]),nb_current(BB,LIST),!.

bb_lookup(BB,P):-bnb_current(BB,LIST),!,member(P,LIST).
bb_lookup(BB,P):-in_bb(BB,P).

bb_add(BB,P):-bnb_current(BB,LIST),!,nb_setval(BB,[P|LIST]).
bb_add(BB,P):-asserta(in_bb(BB,P)).

bb_rem(BB,P):-bnb_current(BB,LIST),!,remove_el(LIST,P,NLIST),nb_setval(BB,NLIST).
bb_rem(BB,P):-retract(in_bb(BB,P)).

bb_op(ENV,call,P):-pe_get_key(P,ENV,BB),bb_lookup(BB,P).
bb_op(ENV,assert,P):-pe_get_key(P,ENV,BB),bb_add(BB,P).
bb_op(ENV,asserta,P):-pe_get_key(P,ENV,BB),bb_add(BB,P).
bb_op(ENV,retract,P):-pe_get_key(P,ENV,BB),bb_rem(BB,P).
bb_op(ENV,retractall,P):-pe_get_key(P,ENV,BB),forall(bb_lookup(BB,P),bb_rem(BB,P)).

bb_op_rb(ENV,call,P):-pe_get(P,ENV,BB),rb_lookup(P,P,BB).
bb_op_rb(ENV,assert,P):-pe_get(P,ENV,BB),nb_rb_insert(P,P,BB).
bb_op_rb(ENV,asserta,P):-pe_get(P,ENV,BB),nb_rb_insert(P,P,BB).
bb_op_rb(ENV,retract,P):-pe_get(P,ENV,BB),nb_rb_get_node(P,P,BB).
bb_op_rb(ENV,retractall,P):-rb_new(BB),pe_set(P,ENV,BB).

bb_op_qu(ENV,call,P):-pe_get(P,ENV,Q),inside_queue(Q,P).
bb_op_qu(ENV,assert,P):-pe_get(P,ENV,Q),push_slow_queue(Q,P).
bb_op_qu(ENV,asserta,P):-pe_get(P,ENV,Q),push_fast_queue(Q,P).
bb_op_qu(ENV,retract,P):-pe_get(P,ENV,Q),pop_queue(Q,P).
bb_op_qu(ENV,retractall,P):-make_queue(Q),pe_set(P,ENV,Q).

% FIFO queue

make_queue(Q) :- nb_setval(Q, fast_slow(QU-QU, L-L)).

push_fast_queue(Q,E) :-
        b_getval(Q, fast_slow(H-[E|T], L)),
        b_setval(Q, fast_slow(H-T, L)).

push_slow_queue(Q,E) :-
        b_getval(Q, fast_slow(L, H-[E|T])),
        b_setval(Q, fast_slow(L, H-T)).

pop_queue(Q,E) :-
        b_getval(Q, fast_slow(H-T, I-U)),
        (   nonvar(H) ->
            H = [E|NH],
            b_setval(Q, fast_slow(NH-T, I-U))
        ;   nonvar(I) ->
            I = [E|NI],
            b_setval(Q, fast_slow(H-T, NI-U))
        ;   false
        ).

inside_queue(Q,E) :- %  trace_or_throw(inside_queue(Q,E)),!,fail.
        b_getval(Q, fast_slow(H-_T, I-_U)),(   nonvar(H) , member(E,H)  ;   nonvar(I) , member(E,I)).



%:- export(pred_info/2).
pred_info(H,Props):- get_functor(H,F,_),findall(PP,call_u(mpred_isa(F,PP)),Props).


:- fixup_exports.
