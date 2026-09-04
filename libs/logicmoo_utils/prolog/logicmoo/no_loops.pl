% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_loop_check.pl
:- module(loop_check,
          [ is_loop_checked/1,
            lco_goal_expansion/2,            
            cyclic_break/1,

            loop_check_early/2,loop_check_term/3,
            loop_check_term/3,no_loop_check_term/3,
            
            loop_check/1,loop_check/2,no_loop_check/1,no_loop_check/2,
            current_loop_checker/1,
            push_loop_checker/0,
            pop_loop_checker/0,
            transitive/3,
            transitive_except/4,
            transitive_lc/3,
          is_parent_goal/1,
          is_parent_goal/2,
            lc_tcall/1
          ]).

/** <module> Utility LOGICMOO LOOP CHECK
This module prevents infinite loops.
 
@author Douglas R. Miles
@license LGPL 
*/
:- set_module(class(library)).

% :- autoload(library(apply),[maplist/2, maplist/3]).
:- system:use_module(library(lists)).
:- system:use_module(library(apply)).
:- system:use_module(library(yall)).
:- system:use_module(library(threadutil)).
:- system:use_module(library(debug)).

:- module_transparent((is_loop_checked/1,
            lco_goal_expansion/2,            
            cyclic_break/1,

            loop_check_early/2,loop_check_term/3,
            loop_check_term/3,no_loop_check_term/3,loop_check_term_frame/5,
            
            loop_check/1,loop_check/2,no_loop_check/1,no_loop_check/2,
            current_loop_checker/1,
            push_loop_checker/0,
            pop_loop_checker/0,
            transitive/3,
            transitive_except/4,
            transitive_lc/3,
            lc_tcall/1)).

:- set_module(class(library)).  
% % % OFF :- system:use_module(library(apply)).

% WAS OFF  :- system:use_module(library(tabling)).
% % % OFF :- system:use_module(library(logicmoo/each_call)).% WAS OFF  :- system:use_module(library(logicmoo_startup)).


:- meta_predicate  
        lc_tcall(0),

        loop_check(0), loop_check(0, 0),
        no_loop_check(0), no_loop_check(0, 0),
        
        loop_check_early(0, 0), loop_check_term(0, ?, 0),

        % loop_check_term(0, ?, 0),no_loop_check_term(0, ?, 0),
        
        %transitive(2, +, -),
        transitive_except(+, 2, +, -),
        transitive_lc(2, +, -).
        
/* memoize_on(+,+,0), memoize_on(+,+,+,0), */


:- module_transparent
        can_fail/1,
        get_where/1,
        get_where0/1,
        is_loop_checked/1,
        lco_goal_expansion/2.
        
  



%% transitive( :PRED2X, +A, -B) is nondet.
%
% Transitive.
%
transitive(XY,A,A):- XY=[],!.
transitive(XY,A,B):- XY= [X|Y], !, transitive(X,A,M),!, transitive(Y,M,B).
transitive(X,A,B):- once(on_x_debug(call(X,A,R)) -> ( R\=@=A -> transitive_lc(X,R,B) ; B=R); B=A),!.


non_transitive(XY,A,A):- XY=[],!.
non_transitive(XY,A,B):- XY= [X|Y], !, non_transitive(X,A,M),!, non_transitive(Y,M,B).
non_transitive(X,A,B):- once(on_x_debug(call(X,A,B))),!.
non_transitive(_,A,A).




%% transitive_lc( :PRED2X, +A, -B) is nondet.
%
% Transitive Not Loop Checked.
%
transitive_lc(XY,A,A):- XY=[],!.
transitive_lc(XY,A,B):- XY= [X|Y],!,transitive_except([],X,A,M),transitive_lc(Y,M,B).
transitive_lc(X,A,B):-transitive_except([],X,A,B).




%% transitive_except( +NotIn, :PRED2X, +A, -B) is nondet.
%
% Transitive Except.
%
transitive_except(NotIn,X,A,B):- memberchk_same_two(A,NotIn)-> (B=A,!) ;
  ((once(on_x_debug(call(X,A,R)) -> ( R\=@=A -> transitive_except([A|NotIn],X,R,B) ; B=R); B=A))),!.




%% memberchk_same_two( ?X, :TermY0) is nondet.
%
% Memberchk Same Two.
%
memberchk_same_two(X, [Y0|Ys]) :- is_list(Ys),!,C=..[v,Y0|Ys],!, arg(_,C,Y), ( X =@= Y ->  (var(X) -> X==Y ; true)),!.
memberchk_same_two(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),memberchk_same_two(X, Ys) )).


%% cyclic_break( ?Cyclic) is nondet.
%
% Cyclic Break.
%
cyclic_break(Cyclic):- notrace(cyclic_term(Cyclic))->(writeq(cyclic_break(Cyclic)),nl,prolog);notrace(true).


% ===================================================================
% Loop checking
% ===================================================================
:- thread_local lmcache:ilc/2.
:- thread_local lmcache:ilc/3.

% = :- meta_predicate(lc_tcall(0)).
% lc_tcall(C0):-reduce_make_key(C0,C),!,table(C),!,query(C).
% lc_tcall(C0):-query(C).



%% lc_tcall( :GoalC) is nondet.
%
% Call Tabled
%
:- meta_predicate(lc_tcall(0)).
%:- table(lc_tcall/1).
lc_tcall(G):- loop_check(call(G)).



%% loop_check_early( :Call, :LoopCaught) is nondet.
%
% Loop Check Early.
%
loop_check_early(Call, LoopCaught):- loop_check(Call, LoopCaught).



%% loop_check( :Call) is nondet.
%
% Loop Check.
%
loop_check(Call):- loop_check(Call, fail).



%% loop_check( :Call, :OnLoopCaught) is nondet.
%
% Loop Check.
%
loop_check(Call, LoopCaught):- 
  loop_check_term(Call,Call,LoopCaught).



%% no_loop_check( :Call) is nondet.
%
% No Loop Check.
%
no_loop_check(Call):- no_loop_check(Call, fail).



%% no_loop_check( :Call, :LoopCaught) is nondet.
%
% No Loop Check.
%
no_loop_check(Call, LoopCaught):- no_loop_check_term(Call,Call,LoopCaught).


%% no_loop_check_term( :Call, +Key, :LoopCaught) is nondet.
%
% Pushes a new Loop checking frame so all previous checks are suspended
%
% no_loop_check_term(Call,_Key,_LoopCaught):-!,Call.
no_loop_check_term(Call,Key,LoopCaught):- 
   trusted_redo_call_cleanup(push_loop_checker,
                     loop_check_term(Call,Key,LoopCaught),
                     pop_loop_checker).

:- thread_initialization(nb_setval('$loop_checker',1)).
:- initialization(nb_setval('$loop_checker',1),restore).
current_loop_checker(LC):- ((nb_current('$loop_checker',LC),number(LC))->true;LC=0).
push_loop_checker :- current_loop_checker(LC),LC2 is LC+1,nb_setval('$loop_checker',LC2).
pop_loop_checker :- current_loop_checker(LC),LC2 is LC-1,nb_setval('$loop_checker',LC2).


%% is_loop_checked( ?Call) is nondet.
%
% If Is A Loop Checked.
%
is_loop_checked(Key):- 
  prolog_current_frame(Frame),
  notrace(make_frame_key(Key,Frame,KeyS,GoaL,SearchFrame)),
  loop_check_term_frame(fail,KeyS,GoaL,SearchFrame,true).


make_frame_key(Key,Frame,Part1,Part2,Parent1):-
  prolog_frame_attribute(Frame,parent,Parent1),
  make_key(Key,Part1,Part2).

:- '$hide'(make_frame_key/5).

make_key(key(Part1),Part1,Part2):-!,current_loop_checker(Part2).
make_key(key(Key,GoaLs),Part1,Part2):-!,current_loop_checker(LC),make_key5(Key,GoaLs,LC,Part1,Part2).
make_key(Key,Key,Part2):- ground(Key),!,current_loop_checker(Part2).
make_key(Key,Part1,Part2):- copy_term(Key,KeyS,GoaLs),current_loop_checker(LC),make_key5(KeyS,GoaLs,LC,Part1,Part2).

make_key5(Part1,[],LC,Part1,LC):-!,numbervars(Part1,242,_,[attvar(error)]).
make_key5(Part1,GoaLs,LC,Part1,[LC|GoaLs]):-numbervars(Part1+GoaLs,242,_,[attvar(error)]).


     
% :- meta_predicate(loop_check_term_frame(+,+,+,+,:)).
loop_check_term_frame(Call,KeyS,GoaL,SearchFrame,LoopCaught):- 
 % set_prolog_flag(debug,true),
 set_prolog_flag(last_call_optimisation,false),
 % set_prolog_flag(gc,false),
 !, 
 (prolog_frame_attribute(SearchFrame, parent_goal, loop_check_term_frame(_,KeyS,GoaL,_,_)) 
   -> LoopCaught ;  Call).


%   (loop_check_term_frame_grovel(Call,KeyS,GoaL,SearchFrame,LoopCaught),true)),true.

/*
loop_check_term_frame(Call,KeyS,GoaL,SearchFrame,LoopCaught):- 
 % set_prolog_flag(debug,true),
 set_prolog_flag(last_call_optimisation,false),
 % set_prolog_flag(gc,false),
 !,
   (prolog_frame_attribute(SearchFrame, parent_goal, loop_check_term_frame(_,KeyS,GoaL,_,_))
    -> (LoopCaught,true)
    ;  (loop_check_term_frame_grovel(Call,KeyS,GoaL,SearchFrame,LoopCaught),true)),true.

loop_check_term_frame_grovel(Call,KeyS,GoaL,SearchFrame,LoopCaught):-  !,
  (  notrace(parent_frame_goal_0(SearchFrame, loop_check_term_frame_grovel(_,KeyS,GoaL,_,_)))
    -> (LoopCaught,true)
    ;  (Call,true)).
*/

is_parent_goal(G):- prolog_current_frame(F),is_parent_goal(F,G).
% The user must ensure the checked parent goal is not removed from the stack due 
% to last-call optimisation 
is_parent_goal(F,G):- nonvar(G),prolog_frame_attribute(F,parent_goal, G).
%and be aware of the slow operation on deeply nested calls.
is_parent_goal(F,G):- prolog_frame_attribute(F,parent,P),parent_frame_goal(P,G).

parent_frame_goal(F,V):- parent_frame_goal_0(F,V0),contains_goalf(V0,V).
parent_frame_goal_0(F,V):- prolog_frame_attribute(F,goal,V);
   (prolog_frame_attribute(F,parent,P),parent_frame_goal_0(P,V)).

contains_goalf(V0,V):- nonvar(V),same_goalf(V0,V),!.
contains_goalf(V0,_):- \+ compound(V0),!,fail.
contains_goalf(V0,V):- var(V),same_goalf(V0,V).
contains_goalf(_:V0,V):- !, contains_goalf(V0,V).
contains_goalf('$execute_directive_3'(V0),V):-!, same_goalf(V0,V).
contains_goalf('<meta-call>'(V0),V):-!, same_goalf(V0,V).
contains_goalf(catch(V0,_,_),V):- same_goalf(V0,V).
contains_goalf(catch(_,_,V0),V):- same_goalf(V0,V).
same_goalf(V,V).


%% loop_check_term( :Call, +Key, :LoopCaught) is nondet.
%
% Loop Check Term 50% of the time
%

%loop_check_term(Call,_Key,_LoopCaught):- zotrace((current_prolog_flag(unsafe_speedups , true) , 1 is random(2))),!,call(Call).
% loop_check_term(Call,_Key,_LoopCaught):-!,Call.

loop_check_term(Call,Key,LoopCaught):- 
   prolog_current_frame(Frame),
   notrace(make_frame_key(Key,Frame,KeyS,GoaL,SearchFrame)),
   loop_check_term_frame(Call,KeyS,GoaL,SearchFrame,LoopCaught).


%% get_where( :TermB) is nondet.
%
% Get Where.
%
get_where(B:L):-get_where0(F:L),file_base_name(F,B).


%% get_where0( :GoalF) is nondet.
%
% Get Where Primary Helper.
%
get_where0(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_where0(F:L):-source_location(F,L),!.
get_where0(A:0):-current_input(S),stream_property(S,alias(A)),!.
get_where0(M:0):-source_context_module(M),!.
get_where0(baseKB:0):-!.




%% lco_goal_expansion( :TermB, :TermA) is nondet.
%
% Lco Call Expansion.
%

lco_goal_expansion(V,VV):- \+ compound(V),!,V=VV.
lco_goal_expansion(loop_check(G),O):-lco_goal_expansion(loop_check(G,fail),O),!.
lco_goal_expansion(no_loop_check(G),O):-lco_goal_expansion(no_loop_check(G,fail),O),!.
lco_goal_expansion(loop_check(G,LoopCaught),loop_check_term(G,info(G,W),LoopCaught)):- get_where(W),!.
lco_goal_expansion(no_loop_check(G,LoopCaught),no_loop_check_term(G,info(G,W),LoopCaught)):- get_where(W),!.
lco_goal_expansion(B,A):- 
  compound_name_arguments(B,F,ARGS),
  F \== (meta_predicate),
  maplist(lco_goal_expansion,ARGS,AARGS),
  compound_name_arguments(A,F,AARGS),!.
lco_goal_expansion(A,A).

:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- endif.

:- multifile system:goal_expansion/4.
:- dynamic system:goal_expansion/4.
% system:goal_expansion(LC,Pos,LCO,Pos):- notrace((compound(LC),lco_goal_expansion(LC,LCO),LC\=@=LCO)).


end_of_file.

Old stuff


/*

make_key(Key0,M,lmcache:ilc(M,Key,GoaL)):- var(Key0),!,copy_term(Key0,Key,GoaL),numbervars(Key+GoaL,242,_,[attvar(error)]).
make_key(key(Key),M,lmcache:ilc(M,Key)):- !.
make_key(nat(Key0),M,lmcache:ilc(M,Key)):- !, copy_term_nat(Key0,Key),numbervars(Key,242,_,[attvar(error)]).
make_key(Key,M,lmcache:ilc(M,Key)):- ground(Key),!.
make_key(Key0,M,lmcache:ilc(M,Key,GoaL)):- copy_term(Key0,Key,GoaL),numbervars(Key+GoaL,242,_,[attvar(error)]).

old_loop_check_term(Call,Key0,LoopCaught):- strip_module(Call,M,_),
   quietly(make_key(Key0,M,Key)),!,
   (Key -> ((LoopCaught)) ; M:locally_each(Key,Call)).


loop_check_term(Call,Key,LoopCaught):- 
  current_loop_checker(Trie) ,
  (trie_lookup(Trie, Key, Value),Value==1) -> LoopCaught ;
    each_call_cleanup(trie_insert(Trie, Key, 1),Call,trie_insert(Trie, Key, 0)).
*/

