% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_loop_check.pl
end_of_file.

:- module(old_loop_check,
          [ is_loop_checked/1,
            lco_goal_expansion/2,            
            cyclic_break/1,

            make_key/2,reduce_make_key/2,
            loop_check_early/2,loop_check_term/3,
            loop_check_term_key/3,no_loop_check_term_key/3,
            
            loop_check/1,loop_check/2,no_loop_check/1,no_loop_check/2,
            
            transitive/3,
            transitive_except/4,
            transitive_lc/3
          ]).


:- meta_predicate  
        call_t(0),

        loop_check(0), loop_check(0, 0),
        no_loop_check(0), no_loop_check(0, 0),
        
        loop_check_early(0, 0), loop_check_term(0, ?, 0),

        loop_check_term_key(0, ?, 0),no_loop_check_term_key(0, ?, 0),
        
        transitive(2, +, -),
        transitive_except(+, 2, +, -),
        transitive_lc(2, +, -).
        
/* memoize_on(+,+,0), memoize_on(+,+,+,0), */


:- module_transparent
        can_fail/1,
        get_where/1,
        get_where0/1,
        is_loop_checked/1,
        lco_goal_expansion/2.
        
:- set_module(class(library)).    

%= 	 	 

%% transitive( :PRED2X, +A, -B) is semidet.
%
% Transitive.
%
transitive(X,A,B):- once(on_x_debug(call(X,A,R)) -> ( R\=@=A -> transitive_lc(X,R,B) ; B=R); B=A),!.


%= 	 	 

%% transitive_lc( :PRED2X, +A, -B) is semidet.
%
% Transitive Not Loop Checked.
%
transitive_lc(X,A,B):-transitive_except([],X,A,B).


%= 	 	 

%% transitive_except( +NotIn, :PRED2X, +A, -B) is semidet.
%
% Transitive Except.
%
transitive_except(NotIn,X,A,B):- memberchk_same_two(A,NotIn)-> (B=A,!) ;((once(on_x_debug(call(X,A,R)) -> ( R\=@=A -> transitive_except([A|NotIn],X,R,B) ; B=R); B=A))),!.


%= 	 	 

%% memberchk_same_two( ?X, :TermY0) is semidet.
%
% Memberchk Same Two.
%
memberchk_same_two(X, [Y0|Ys]) :- is_list(Ys),!,C=..[v,Y0|Ys],!, arg(_,C,Y), ( X =@= Y ->  (var(X) -> X==Y ; true)),!.
memberchk_same_two(X, [Y|Ys]) :- (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),memberchk_same_two(X, Ys) )).


%% cyclic_break( ?Cyclic) is semidet.
%
% Cyclic Break.
%
cyclic_break(Cyclic):-cyclic_term(Cyclic)->(writeq(cyclic_break(Cyclic)),nl,prolog);true.


% ===================================================================
% Loop checking
% ===================================================================
:- thread_local lmcache:ilc/1.

% = :- meta_predicate(call_t(0)).
% call_t(C0):-reduce_make_key(C0,C),!,table(C),!,query(C).
% call_t(C0):-query(C).

%= 	 	 

%% call_t( :GoalC) is semidet.
%
% Call True Stucture.
%
call_t(C):- call(C).

:- meta_predicate reduce_make_key(+,-).

%= 	 	 

%% reduce_make_key( +O, -O) is semidet.
%
% Reduce Make Key.
%
reduce_make_key(call(C),O):-!,reduce_make_key(C,O).
reduce_make_key(call_u(C),O):-!,reduce_make_key(C,O).
reduce_make_key(must(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats_old(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_repeats_old(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(lc_tcall(C),O):-!,reduce_make_key(C,O).
reduce_make_key(no_loop_check(C),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check(C),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check(C,_),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check_term_key(C,_,_),O):-!,reduce_make_key(C,O).
reduce_make_key(loop_check_term(C,_,_),O):-!,reduce_make_key(C,O).
reduce_make_key(fact_loop_checked(_,C),O):-!,reduce_make_key(C,O).
reduce_make_key(V+C,V+O):-!,reduce_make_key(C,O).
reduce_make_key(M:C,O):-atom(M),!,reduce_make_key(C,O).
reduce_make_key(O,O).



%= 	 	 

%% cc_key( ?CC, ?Key) is semidet.
%
% Cc Key.
%
cc_key(CC,Key):- cyclic_term(CC),!,dtrace,copy_term_nat(CC,CKey),numbervars(CKey,0,_,[attvars(error)]),format(atom(Key),'~w',[CKey]),!.
cc_key(CC,O):- copy_term_nat(CC,Key),numbervars(Key,0,_,[attvars(error)]),!,Key=O.


%= 	 	 

%% make_key( ?CC, ?KeyO) is semidet.
%
% Make Key.
%
make_key(M:CC,KeyO):- atom(M),!,((ground(CC)->Key=CC ; cc_key(CC,Key))),!,KeyO=Key.
make_key(CC,KeyO):- ((ground(CC)->Key=CC ; cc_key(CC,Key))),!,KeyO=Key.
:- '$set_predicate_attribute'(make_key(_,_), hide_childs, 1).
:- '$set_predicate_attribute'(make_key(_,_), trace, 1).


%= 	 	 

%% is_loop_checked( ?Call) is semidet.
%
% If Is A Loop Checked.
%
is_loop_checked(Call):-  make_key(Call,Key),!,(lmcache:ilc(Key);lmcache:ilc(Key+_)).

%= 	 	 

%% loop_check_early( :GoalCall, :GoalTODO) is semidet.
%
% Loop Check Early.
%
loop_check_early(Call, TODO):- loop_check_term_key(Call,Call, TODO).

%= 	 	 

%% loop_check( :GoalCall) is semidet.
%
% Loop Check.
%
loop_check(Call):- loop_check(Call, fail).

:- export(loop_check/2).

%= 	 	 

%% loop_check( :GoalCall, :GoalTODO) is semidet.
%
% Loop Check.
%
loop_check(Call, TODO):- !,loop_check_early(Call, TODO).
loop_check(Call, TODO):- parent_goal(ParentCall,1)->(loop_check_term_key(Call,Call+ParentCall, TODO));loop_check_early(Call, TODO).

%= 	 	 

%% loop_check_term_key( :GoalCall, ?KeyIn, :GoalTODO) is semidet.
%
% Loop Check Term Key.
%
loop_check_term_key(Call,_,_):- current_prolog_flag(unsafe_speedups , true) , 1 is random(2),!,call(Call).
loop_check_term_key(Call,KeyIn,TODO):- quietly(make_key(KeyIn,Key)) -> loop_check_term(Call,Key,TODO).



%= 	 	 

%% no_loop_check( :GoalCall) is semidet.
%
% No Loop Check.
%
no_loop_check(Call):- no_loop_check(Call, fail).

%= 	 	 

%% no_loop_check( :GoalCall, :GoalTODO) is semidet.
%
% No Loop Check.
%

no_loop_check(Call, TODO):- no_loop_check_term_key(Call,Call,TODO).
%no_loop_check(Call, TODO):- parent_goal(Term,2)->no_loop_check_term_key(Call,Term+Call, TODO).

%= 	 	 

%% no_loop_check_term_key( :GoalCall, ?KeyIn, :GoalTODO) is semidet.
%
% No Loop Check Term Key.
%
no_loop_check_term_key(Call,KeyIn,TODO):- make_key(KeyIn,Key) -> locally_hide(lmcache:ilc(_),loop_check_term(Call,Key,TODO)).

%= 	 	 

%% loop_check_term( :GoalCall, ?Key, :GoalTODO) is semidet.
%
% Loop Check Term 50% of the time
%
% loop_check_term(Call,_Key,_TODO):- 1 is random(2) ,!,call(Call).

loop_check_term(Call,_Key,_TODO):-current_prolog_flag(unsafe_speedups , true) , 1 is random(2),!, 
  call(Call).
loop_check_term(Call,Key,TODO):- quietly(TT = lmcache:ilc(Key)),
 ( quietly( \+(TT)) -> locally(TT, Call);  call(TODO)).

   % ((can_fail(TODO)->retract_can_table;true),call(TODO)).


%= 	 	 

%% can_fail( ?G) is semidet.
%
% Can Fail.
%
can_fail(G):-not(G=true),not(G=must(_)).

% get_where(When)

%= 	 	 

%% get_where( :TermB) is semidet.
%
% Get Where.
%
get_where(B:L):-get_where0(F:L),file_base_name(F,B).

%= 	 	 

%% get_where0( :GoalF) is semidet.
%
% Get Where Primary Helper.
%
get_where0(F:L):-source_location(file,F),current_input(S),line_position(S,L),!.
get_where0(F:L):-source_location(F,L),!.
get_where0(A:0):-current_input(S),stream_property(S,alias(A)),!.
get_where0(M:0):-source_context_module(M),!.
get_where0(baseKB:0):-!.


%= 	 	 

%% lco_goal_expansion( :TermB, :TermA) is semidet.
%
% Lco Goal Expansion.
%

lco_goal_expansion(V,V):- \+ compound(V),!.
lco_goal_expansion(loop_check(G),O):-!,lco_goal_expansion(loop_check(G,fail),O).
lco_goal_expansion(no_loop_check(G),O):-!,lco_goal_expansion(no_loop_check(G,fail),O).
lco_goal_expansion(loop_check(G,TODO),loop_check_term_key(G,G:W,TODO)):- get_where(W).
lco_goal_expansion(no_loop_check(G,TODO),no_loop_check_term_key(G,G:W,TODO)):- get_where(W).
lco_goal_expansion(B,A):- 
  compound_name_arguments(B,F,ARGS),
  maplist(lco_goal_expansion,ARGS,AARGS),
  compound_name_arguments(A,F,AARGS).


:- dynamic system:goal_expansion/2.
:- multifile system:goal_expansion/2.
system:goal_expansion(LC,Pos,LCO,Pos):- compound(LC),lco_goal_expansion(LC,LCO)->LC\=@=LCO.

