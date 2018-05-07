/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (builtin_lisp_functions.pl)
 *
 * (c) Neil Smith, 2001
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(s3q,[]).

:- set_module(class(library)).

:- include('../wam_cl/header.pl').

%:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).

is_listp(Obj):- compound(Obj)-> Obj=[_|_] ; Obj == [].
is_endp(Obj):- Obj == [].

% GROVELED f_listp(Obj,RetVal):- t_or_nil(is_listp(Obj),RetVal).
% GROVELED f_endp(Obj,RetVal):- t_or_nil(is_endp(Obj), RetVal).

f_sys_memq(E,L,R):- t_or_nil((member(Q,L),Q==E),R).


range_1(X,Keys,XR,Start1):-
  key_value(Keys,kw_start,Start1,0),
  key_value(Keys,kw_end,End1,[]),
  range_subseq(X,Start1,End1,XR).   

range_1_and_2(X,Y,[],X,Y,0):-!.
range_1_and_2(X,Y,Keys,XR,YR,Start1):-
   key_value(Keys,kw_start1,Start1,0),key_value(Keys,kw_end1,End1,[]),
   key_value(Keys,kw_start2,Start2,0),key_value(Keys,kw_end2,End2,[]),
   range_subseq(X,Start1,End1,XR),
   range_subseq(Y,Start2,End2,YR).

range_1_and_2_len(X,Y,[],X,Y,-1):-!.
range_1_and_2_len(X,Y,Keys,XR,YR,Length):-
   key_value(Keys,start1,Start1,0),key_value(Keys,end1,End1,9999999999999),
   key_value(Keys,start2,Start2,0),key_value(Keys,end2,End2,9999999999999),
   subseqence_from(X,Start1,XR),
   subseqence_from(Y,Start2,YR),
   Length is min(End1-Start1,End2-Start2).



range_subseq(X,0,[],X):-!.
range_subseq(X,N,[],XR):- !, subseqence_from(X,N,XR).
range_subseq(X,0,N,XR):-!, subseqence_until(X,N,XR).
range_subseq(X,Start,End,XR):-!, subseqence_from(X,Start,XM),NewEnd is End - Start,subseqence_until(XM,NewEnd,XR).

subseqence_from(X,0,X):-!.
subseqence_from([_|X],N,XR):- N2 is N -1, subseqence_from(X,N2,XR).

subseqence_until(_,0,[]):-!.
subseqence_until([X|XX],N,[X|XR]):-  N2 is N -1,subseqence_until(XX,N2,XR).

% Like append/3 by enumerates in reverse
append_r([H|T], L, [H|R]) :-
    append_r(T, L, R).
append_r([], L, L).


% #'ADJOIN
wl:init_args(2, adjoin).
f_adjoin(Item,List,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(f_eql,Keys,EqlPred),
 key_value(Keys,kw_from_end,FromEnd1,[]),
 range_1(List,Keys,RList,_Start1),
 ((FromEnd1==[]->append(_,[V|_Rest],RList); append_r(_,[V|_Rest],RList))),
 call_as_ident(Ident,V,Id),apply_as_pred(EqlPred,Item,Id),!,
  RetVal = List.
f_adjoin(Item,List,_,[Item|List]).



% #'MEMBER
wl:init_args(2,member).
f_member(Item,List,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(f_eql,Keys,EqlPred),
 key_value(Keys,kw_from_end,FromEnd1,[]),
 range_1(List,Keys,RList,_Start1),
 ((FromEnd1==[]->append(_,[V|Rest],RList); append_r(_,[V|Rest],RList))),
 call_as_ident(Ident,V,Id),apply_as_pred(EqlPred,Item,Id),!,
  RetVal = [V|Rest].
f_member(_,_,_,[]).

% #'MEMBER-IF
wl:init_args(2,member_if).
f_member_if(E,Seq,Keys,Result):- f_member(E,Seq,[kw_test,f_funcall|Keys],Result).  

% #'MEMBER-IF-NOT
wl:init_args(2,member_if_not).
f_member_if_not(E,Seq,Keys,Result):- f_member(E,Seq,[kw_test_not,f_funcall|Keys],Result).

wl:init_args(2,find).
f_find(E,Seq,Keys,Result):-
   f_member(E,Seq,Keys,MemberResult),
   f_car(MemberResult,Result).

wl:init_args(2,find_if).
f_find_if(E,Seq,Keys,Result):- f_find(E,Seq,[kw_test,f_funcall|Keys],Result).  

wl:init_args(2,find_if_not).
f_find_if_not(E,Seq,Keys,Result):- f_find(E,Seq,[kw_test_not,f_funcall|Keys],Result).


% #'SEARCH  - http://www.lispworks.com/documentation/HyperSpec/Body/f_search.htm
wl:init_args(2,search).
f_search(X,Y,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(f_eql,Keys,EqlPred),
 range_1_and_2(X,Y,Keys,XR0,YR0,OffsetReturn),
 xform_with_ident(YR0,Ident,[YV|YRest]), 
 xform_with_ident(XR0,Ident,XR),
 length(YR0,Len),length([XV|XRest],Len), 
 key_value(Keys,kw_from_end,FromEnd1,[]),
 append(XRest,_,Rest),
 ((FromEnd1==[]->append(LeftOffset,[XV|Rest],XR); append_r(LeftOffset,[XV|Rest],XR))),
 apply_as_pred(EqlPred,YV,XV),
 maplist(apply_as_pred(EqlPred),XRest,YRest),!,
 length(LeftOffset,N),
 RetVal is N + OffsetReturn. 
f_search(_,_,_,[]).




% #'POSITION   - http://www.lispworks.com/documentation/HyperSpec/Body/f_pos_p.htm
wl:init_args(2,position).
f_position(Item,List,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(f_eql,Keys,EqlPred),
 key_value(Keys,kw_from_end,FromEnd1,[]),
 range_1(List,Keys,RList,OffsetReturn),
 ((FromEnd1==[]->append(LeftOffset,[V|_Rest],RList); append_r(LeftOffset,[V|_Rest],RList))),
 call_as_ident(Ident,V,Id),apply_as_pred(EqlPred,Item,Id),!,
 length(LeftOffset,N),
 RetVal is N + OffsetReturn. 
f_position(_,_,_,[]).

% #'POSITION-IF
wl:init_args(2,position_if).
f_position_if(E,Seq,Keys,Result):- f_position(E,Seq,[kw_test,f_funcall|Keys],Result).  

% #'POSITION-IF-NOT
wl:init_args(2,position_if_not).
f_position_if_not(E,Seq,Keys,Result):- f_position(E,Seq,[kw_test_not,f_funcall|Keys],Result).


% #'REVERSE
f_reverse(Xs, Ys) :-
    lists:reverse(Xs, [], Ys, Ys).

% #'NREVERSE
f_nreverse(Xs, Ys) :-
    lists:reverse(Xs, [], Ys, Ys).



% #'replace
(wl:init_args(2,replace)).
f_replace(X,Y,Keys,XR):-
   range_1_and_2_len(X,Y,Keys,XR,YR,Count),
   replace_each(Count,XR,YR).

replace_each(0,_XR,_YR):-!.
replace_each(_,[],_):-!.
replace_each(_,_,[]):-!.
replace_each(Count,XR,[Y|YR]):- nb_setarg(1,XR,Y),arg(2,XR,XT),Count2 is Count-1,replace_each(Count2,XT,YR).
   

wl:init_args(1,mapcar).
f_mapcar(P, [[H|T]], [RH|RT]) :- !, f_apply(P, [H], RH),f_mapcar(P, [T], RT).
f_mapcar(P, [[H|T],[H2|T2]], [RH|RT]) :- !, f_apply(P, [H,H2], RH),f_mapcar(P, [T,T2], RT).
f_mapcar(P, [[H|T],[H2|T2],[H3|T3]], [RH|RT]) :- !, f_apply(P, [H,H2,H3], RH),f_mapcar(P, [T,T2,T3], RT).
f_mapcar(_, [[]|_], []).


(wl:init_args(0,nconc)).
f_nconc([L1,L2],Ret):- !, append(L1,L2,Ret).
f_nconc([L1],L1):-!.
f_nconc([L1,L2|Lists],Ret):- !,f_nconc([L2|Lists],LL2), append(L1,LL2,Ret).
f_nconc(X,X):-!.

f_copy_list(List,List):- \+ compound(List),!.
f_copy_list([M|List],[M|Copy]):-f_copy_list(List,Copy).


wl:type_checked(f_length(claz_cons,integer)).
f_length(Sequence,Len):- always(length(Sequence,Len)).

f_list_length(Sequence,Len):- always(length(Sequence,Len)).


f_remove('$ARRAY'([S],Type,A),B,'$ARRAY'([Sm1],Type,C)):-pl_remove(-1,is_equal,A,B,C,Did),(number(S)->Sm1 is S-Did ; Sm1=S).
f_remove(A,B,C):- pl_remove(-1,is_equal,A,B,C,_Did).

pl_remove(_Tst,0, X, _, X, 0).
pl_remove(_Tst,_,[], _, [],0).
pl_remove(Tst,May,[Elem|Tail], Del, Result,Done2) :-
    ( call(Tst,Elem,Del) ->  (May2 is May-1, pl_remove(Tst,May2,Tail, Del, Result,Done),Done2 is Done+1)
    ; ( Result = [Elem|Rest],pl_remove(Tst,May,Tail,Del,Rest,Done2))).

%f_subst('$ARRAY'([S],Type,A),B,C,'$ARRAY'([Sm1],Type,R)):-pl_subst(C,B,A,R),(number(S)->Sm1 is S-Did ; Sm1=S).
f_subst(A,B,C,R):-pl_subst(C,B,A,R).

pl_subst( Var, VarS,SUB,SUB ) :- Var==VarS,!.
pl_subst([H|T],B,A,[HH|TT]):- !,pl_subst(H,B,A,HH),pl_subst(T,B,A,TT).
pl_subst( Var, _,_,Var ).


% wl:type_checked(f_subseq(sequence(T,E),number,sequence(T,E))).
f_subseq(Seq,Offset,Result):- 
  always(coerce_to(Seq, sequence(Was,Info), Mid)),
  always(pl_subseq(Mid,Offset,MOut)),
  always(coerce_to(MOut, object(Was,Info),Result)).

f_subseq(Seq,Start,End,Result):- 
  always(coerce_to(Seq, sequence(Was,Info), Mid)),
  range_subseq(Mid,Start,End,MOut),
  always(coerce_to(MOut, object(Was,Info),Result)).

pl_subseq([], Skip, []):- Skip =:=0 -> true ; throw('should not be greater than :END').
pl_subseq([Head|Tail], Skip, [Head|Cmpl]) :- Skip<1,!,
	pl_subseq(Tail, Skip, Cmpl).
pl_subseq([_|Tail], Skip, Cmpl) :- Skip2 is Skip-1,
	pl_subseq(Tail, Skip2, Cmpl).



nth_index([Index],List,RetVal):- !, f_nth(Index,List,RetVal). 
nth_index([],List,List):-!.
nth_index([Index|Indexes],List,RetVal):- f_nth(Index,List,IndexedVal),nth_index(Indexes,IndexedVal,RetVal).
nth_index(Index,List,RetVal):- !, f_nth(Index,List,RetVal). 

set_nth_index([],_List,Value,Value):-!.
set_nth_index([Index],List,Value,RetVal):- !, f_set_nth(Index,List,Value,RetVal). 
set_nth_index([Index|Indexes],List,Value,RetVal):- f_nth(Index,List,IndexedVal),
   set_nth_index(Indexes,IndexedVal,Value,RetVal).
set_nth_index(Index,List,Value,RetVal):- !, f_set_nth(Index,List,Value,RetVal). 


set_nth(0, List, Value):- !, nb_setarg(1,List,Value).
set_nth(N, [_|List], Value):- M is N-1,!,set_nth(M, List, Value).
set_nth(N, Vector, Value):-   M is N+1,nb_setarg(M, Vector, Value).

f_set_nth(Index,Obj,Value,RetVal):- get_adata(Obj,List),set_nth(Index,List,Value), Value=RetVal.

f_nth(Index,Obj,RetVal):- get_adata(Obj,List),data_nth0(Index,List,RetVal),!.

data_nth0(Index,List,RetVal):- nth0(Index,List,RetVal),!.
data_nth0(N, Vector, Value):-   M is N+1,arg(M, Vector, Value).


:- fixup_exports.

      
end_of_file.


