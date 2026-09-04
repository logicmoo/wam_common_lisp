%   Package: visualize_tree
%   Author : Helmut Simonis, Mats Carlsson
%   Updated: 18 March 2010
%   Purpose: labeling with support for visualization for SICStus Prolog
%   Development version, in flux!

:- module(visualize_tree, [
	root/1,
	solution/1,
	try/4, % questionable
	number_variables/3,
	number_variables/4,
	name_variables/4,
	name_variables/5,
	extract_array/4,
	extract_array/5,
	create_visualization/2,
	add_visualizer/3,
	draw_visualization/1,
	draw_visualization/2,
	close_visualization/1,
	label_visualization/3
	]).


/** <module> Utility LOGICMOO_CPVIZ
This module includes utilities for visualizing prolog data. 

- @author Douglas R. Miles
- @license LGPL 
*/
:- use_module(library(types)).
:- use_module(library(timeout)).
:- use_module(library(lists)).
:- use_module(library(clpfd)).
:- use_module(structures).
:- use_module(visualization).

root(Handle) :-
	get_struct(visualization(tree_stream:Stream), Handle),
        set_node_cnt(0), % we are in tree search
        format(Stream,'<root id=\"~d\"/>\n',[0]),
        draw_visualization(Handle).

solution(Handle) :-
        draw_visualization(Handle),
	get_struct(visualization(tree_stream:Stream), Handle),
        current_node_cnt(Id),
        format(Stream,'<succ id=\"~d\"/>\n',[Id]).

try(Handle,Name,Size,Value) :-
        new_node_cnt(Handle,Id,Parent,Stream),
        format(Stream,'<try id=\"~d\" parent=\"~d\" name=\"~w\" size=\"~d\" value=\"~d\" />\n',
               [Id,Parent,Name,Size,Value]).

failure(Handle,Name,Size,Value) :-
        new_node_cnt(Handle,Id,Parent,Stream),
        format(Stream,'<fail id=\"~d\" parent=\"~d\" name=\"~w\" size=\"~d\" value=\"~d\" />\n',
               [Id,Parent,Name,Size,Value]).

number_variables(Handle,L,Pairs) :-
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:2), Handle),
        (   foreach(X,L),
	    foreach(t(X,J),Pairs),
	    count(J,1,_)
	do  true
        ).

number_variables(Handle,L,Group,Pairs) :-
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
        (   foreach(X,L),
	    foreach(t(X,J,group(Group,J)),Pairs),
	    count(J,1,_),
	    param(Group)
	do  true
        ).

name_variables(Handle,L,K,P) :-
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
        (   foreach(X,L),
	    foreach(Y,K),
	    count(J,1,_),
	    foreach(t(X,Y,J),P)
	do  true
        ).

name_variables(Handle,L,K,Group,P) :-
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
        (   foreach(X,L),
	    foreach(Y,K),
	    count(J,1,_),
	    foreach(t(X,Y,group(Group,J)),P),
	    param(Group)
	do  true
        ).

extract_array(Handle,col,Matrix,List) :- !,
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
	transpose(Matrix, Transpose),
	(   foreach(Col,Transpose),
	    count(J,1,_),
	    fromto(1,K1,K3,_),
	    fromto(List,L1,L3,[])
	do  (   foreach(V,Col),
		count(I,1,_),
		count(K2,K1,K3),
		fromto(L1,[t(V,K2,I-J)|L2],L2,L3),
		param(J)
	    do  true
	    )
	).
extract_array(Handle,row,Matrix,List) :- !,
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
	(   foreach(Row,Matrix),
	    count(I,1,_),
	    fromto(1,K1,K3,_),
	    fromto(List,L1,L3,[])
	do  (   foreach(V,Row),
		count(J,1,_),
		count(K2,K1,K3),
		fromto(L1,[t(V,K2,I-J)|L2],L2,L3),
		param(I)
	    do  true
	    )
	).

extract_array(Handle,col,Group,Matrix,List) :- !,
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
	transpose(Matrix, Transpose),
	(   foreach(Col,Transpose),
	    count(J,1,_),
	    fromto(1,K1,K3,_),
	    fromto(List,L1,L3,[]),
	    param(Group)
	do  (   foreach(V,Col),
		count(I,1,_),
		count(K2,K1,K3),
		fromto(L1,[t(V,K2,group(Group,I-J))|L2],L2,L3),
		param(J,Group)
	    do  true
	    )
	).
extract_array(Handle,row,Group,Matrix,List) :- !,
	get_struct(visualization(var_arg:1,name_arg:2,focus_arg:3), Handle),
	(   foreach(Row,Matrix),
	    count(I,1,_),
	    fromto(1,K1,K3,_),
	    fromto(List,L1,L3,[]),
	    param(Group)
	do  (   foreach(V,Row),
		count(J,1,_),
		count(K2,K1,K3),
		fromto(L1,[t(V,K2,group(Group,I-J))|L2],L2,L3),
		param(I,Group)
	    do  true
	    )
	).

% NOTE: Terms are compound terms containing variables as well as annotations
% for visualization
label_visualization(Options, Terms, Handle) :-
	Goal = label_visualization(Options,Terms,Handle),
	prolog:get_module(Options, Options2, _),
	must_be(Options2, proper_list(callable), Goal, 1),
	clpfd:labeling_options(Options2, opt(leftmost,all,step(up,Handle),_,33554431,any),
			       opt(Sel,Sol,Enum,K,DU,TimeOut),
			       Terms, Goal),
	labeling(Sol, Terms, Sel, Enum, K, DU, TimeOut, Handle).

labeling(all, Terms, Sel, Enum, K, DU, TO, Handle) :-
	nobb_labeling_top(TO, Terms, param(Sel,Enum), 0, K, nobb(DU), Handle).
labeling(minimize(Value,Goal), Terms, Sel, Enum, K, DU, TO, Handle) :-
	(   foreach(_,Terms),
	    foreach(0,Zeros)
	do  true
	),
	clpfd:'$fd_minint_maxint'(_Minint, Maxint),
	asserta(clpfd:incumbent(Maxint,Zeros), Ref),
	call_cleanup(bb_labeling_top(TO, Terms, Sel, Enum, 0, K,
				     bb(minimize(Value),Ref,Goal,DU), Handle),
	             erase(Ref)).
labeling(maximize(Value,Goal), Terms, Sel, Enum, K, DU, TO, Handle) :-
	(   foreach(_,Terms),
	    foreach(0,Zeros)
	do  true
	),
	clpfd:'$fd_minint_maxint'(Minint, _Maxint),
	asserta(clfpd:incumbent(Minint,Zeros), Ref),
	call_cleanup(bb_labeling_top(TO, Terms, Sel, Enum, 0, K,
				     bb(maximize(Value),Ref,Goal,DU), Handle),
	             erase(Ref)).

bb_labeling_top(any, Terms, Sel, Enum, I, K, BB, Handle) :-
	BB = bb(MinMax,Ref,_,_), 
	bb_labeling(Terms, Sel, Enum, I, K, BB, Handle),
	bb_labeling_2(MinMax, Ref, Terms, Handle).
bb_labeling_top(time_out(Time,Flag), Terms, Sel, Enum, I, K, BB, Handle) :-
	BB = bb(MinMax,Ref,_,_), 
	time_out(clpfd:bb_labeling(Terms, Sel, Enum, I, K, BB, Handle), Time, Flag),
	bb_labeling_2(MinMax, Ref, Terms, Handle).

bb_labeling(Terms, Sel, Enum, I, K, BB, Handle) :-
	BB = bb(MinMax,Ref,Goal,_),
	nobb_labeling(Terms, param(Sel,Enum), I, K, BB, Handle),
	arg(1, MinMax, Value),
	get_struct(visualization(var_arg:VarArg), Handle),
	(   foreach(Term,Terms),
	    foreach(Var,Variables),
	    param(VarArg)
	do  arg(VarArg, Term, Var)
	),
	(   var(Value) -> illarg(var, Goal, 1)
        ;   clpfd:'$fd_update_incumbent'(Ref, Value, Variables)
        ),
	fail.
bb_labeling(_, _, _, _, _, _, _).

bb_labeling_2(minimize(ValueV), Ref, Terms, Handle) :-
	clause(clpfd:incumbent(Value,Tuple), true, Ref),
	clpfd:'$fd_minint_maxint'(_Minint, Maxint),
	Value < Maxint,
	get_struct(visualization(var_arg:VarArg), Handle),
	(   foreach(Term,Terms),
	    foreach(Var,Variables),
	    param(VarArg)
	do  arg(VarArg, Term, Var)
	),
	clpfd:fd_unify([ValueV|Variables], [Value|Tuple]).
bb_labeling_2(maximize(ValueV), Ref, Terms, Handle) :-
	clause(clpfd:incumbent(Value,Tuple), true, Ref),
	clpfd:'$fd_minint_maxint'(Minint, _Maxint),
	Value > Minint,
	get_struct(visualization(var_arg:VarArg), Handle),
	(   foreach(Term,Terms),
	    foreach(Var,Variables),
	    param(VarArg)
	do  arg(VarArg, Term, Var)
	),
	clpfd:fd_unify([ValueV|Variables], [Value|Tuple]).

nobb_labeling_top(any, L, Param, I, K, BB, Handle) :-
	nobb_labeling(L, Param, I, K, BB, Handle).
nobb_labeling_top(time_out(Time,Flag), L, Param, I, K, BB, Handle) :-
	time_out(clpfd:nobb_labeling(L, Param, I, K, BB, Handle), Time, Flag).

nobb_labeling([], _, K, K, _, _).
nobb_labeling(LL, Param, I, K, BB, Handle) :-
	LL = [T|_],
	get_struct(visualization(var_arg:VarArg), Handle),
	arg(VarArg, T, X),
	var(X), !,
	Param = param(Selector,Enum),
	delete(Selector, LL, T1, L1, VarArg),
	labeling_cont(Enum, T1, L1, LL, R, BB, BB1, Handle),
	J is I+1,
	nobb_labeling(R, Param, J, K, BB1, Handle).
nobb_labeling([_|L], Param, I, K, BB, Handle) :-
	nobb_labeling(L, Param, I, K, BB, Handle).

labeling_cont(value(Enum), T, L, LL, LL, BB, BB1, Handle) :-
	get_struct(visualization(var_arg:VarArg), Handle),
	arg(VarArg, T, X),
	call(Enum, X, L, BB, BB1, V), % API change !!!
	try_or_fail(T, V, Handle).
labeling_cont(enum(Arg), T, L, _LL, L, BB, BB1, Handle) :-
	get_struct(visualization(var_arg:VarArg), Handle),
	arg(VarArg, T, X),
	clpfd:get_fd_domain(X, Dom),
	Dom = dom(Set,_Min,_Max,_Size),
	clpfd:indomain(Arg, V, Set, BB, BB1),
	try_or_fail(T, V, Handle).
labeling_cont(step(Arg), T, L, LL, R, BB, BB1, Handle) :-
	labeling_cont(enum(Arg), T, L, LL, R, BB, BB1, Handle).
labeling_cont(bisect(Arg), T, L, LL, R, BB, BB1, Handle) :-
	labeling_cont(enum(Arg), T, L, LL, R, BB, BB1, Handle).
% NOT IMPLEMENTED: step-search, dichotomic search
% labeling_cont(step(Arg), X, L, LL, R, BB, BB1, Handle) :-
% 	clpfd:get_fd_domain(X, Dom),
% 	Dom = dom(_Set,Min,Max,_Size),
% 	clfpd:labeling_step(Arg, Min, Max, V, L, LL, R, BB, BB1),
% 	try_or_fail(X, V, Handle).
% labeling_cont(bisect(Arg), X, _L, LL, LL, BB, BB1, Handle) :-
% 	clpfd:get_fd_domain(X, Dom),
% 	Dom = dom(_Set,Min,Max,_Size),
% 	clpfd:labeling_bisect(Arg, Min, Max, V, BB, BB1),
% 	try_or_fail(X, V, Handle).

try_or_fail(Term, V, Handle) :-
	get_struct(visualization(var_arg:VarArg,
				 name_arg:NameArg,
				 focus_arg:FocusArg), Handle),
        arg(VarArg, Term, X),
	arg(NameArg, Term, Name),
	arg(FocusArg, Term, Focus),
	fd_size(X, Size),
        (   X = V
	->  try(Handle, Name, Size, V),
	    focus_option(Focus, FocusOption),
	    draw_visualization(Handle, FocusOption)
	;   failure(Handle, Name, Size, V),
	    fail_option(Focus, V, FailOption),
	    draw_visualization(Handle, FailOption),
	    fail
	).

focus_option(group(Group,V),[focus(Group,V)]) :- !.
focus_option(V,[focus(1,V)]).

fail_option(group(Group,V),Value,[failed(Group,V,Value)]) :- !.
fail_option(V,Value,[failed(1,V,Value)]).

delete(leftmost, [X|L], X, L, _VarArg).
delete(min, LL, T1, LL, VarArg) :-
	LL = [T|L],
	arg(VarArg, T, X),
	fd_min(X, Rank),
	deletemin(L, Rank, T, T1, VarArg).
delete(max, LL, T1, LL, VarArg) :-
	LL = [T|L],
	arg(VarArg, T, X),
	fd_max(X, Rank),
	deletemax(L, Rank, T, T1, VarArg).
delete(ff, LL, T1, LL, VarArg) :-
	LL = [T|L],
	arg(VarArg, T, X),
	clpfd:fd_rank(X, Rank),
	deleteff(L, Rank, T, T1, VarArg).
delete(ffc, LL, T1, LL, VarArg) :-
	LL = [T|L],
	arg(VarArg, T, X),
	clpfd:fd_rankc(X, Rank),
	deleteffc(L, Rank, T, T1, VarArg).
delete(variable(Sel), LL, T1, L1, VarArg) :-
	call(Sel, LL, T1, L1, VarArg). % API change !!!

% deletexx(+Vars, +Rank, +Best, -Var, +VarArg).

deletemin([], _, T, T, _VarArg).
deletemin([T0|L0], Rank1, T1, T, VarArg) :-
	arg(VarArg, T0, X),
	(   integer(X)
	->  deletemin(L0, Rank1, T1, T, VarArg)
	;   fd_min(X, Rank0),
	    (   Rank0 < Rank1
	    ->  deletemin(L0, Rank0, T0, T, VarArg)
	    ;   deletemin(L0, Rank1, T1, T, VarArg)
	    )
	).

deletemax([], _, T, T, _VarArg).
deletemax([T0|L0], Rank1, T1, T, VarArg) :-
	arg(VarArg, T0, X),
	(   integer(X)
	->  deletemax(L0, Rank1, T1, T, VarArg)
	;   fd_max(X, Rank0),
	    (   Rank0 > Rank1
	    ->  deletemax(L0, Rank0, T0, T, VarArg)
	    ;   deletemax(L0, Rank1, T1, T, VarArg)
	    )
	).

deleteff(_ , 2, T, T, _VarArg) :- !.
deleteff([], _, T, T, _VarArg).
deleteff([T0|L0], Rank1, T1, T, VarArg) :-
	arg(VarArg, T0, X),
	(   integer(X)
	->  deleteff(L0, Rank1, T1, T, VarArg)
	;   clpfd:fd_rank(X, Rank0),
	    (   Rank0 @< Rank1
	    ->  deleteff(L0, Rank0, T0, T, VarArg)
	    ;   deleteff(L0, Rank1, T1, T, VarArg)
	    )
	).

deleteffc([], _, T, T, _).
deleteffc([T0|L0], Rank1, T1, T, _VarArg) :-
	arg(VarArg, T0, X),
	(   integer(X)
	->  deleteffc(L0, Rank1, T1, T, VarArg)
	;   clpfd:fd_rankc(X, Rank0),
	    (   Rank0 @< Rank1
	    ->  deleteffc(L0, Rank0, T0, T, VarArg)
	    ;   deleteffc(L0, Rank1, T1, T, VarArg)
	    )
	).

