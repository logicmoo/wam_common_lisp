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

:- include('header').


%module(_,_).

:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).



f_sys_memq(E,L,R):- t_or_nil((member(Q,L),Q==E),R).


xform_with_ident([],_Ident,[]).
xform_with_ident([Y0|YR0],Ident,[Y|YR]):-
   call_as_ident(Ident,Y0,Y),
   xform_with_ident(YR0,Ident,YR).

call_as_ident(Pred,X,Result):- function(Pred,X,Result).

apply_as_pred(EqlPred,X,Y,Z):-call(EqlPred,X,Y,Z,R)->R\==[].
apply_as_pred(EqlPred,X,Y):-call(EqlPred,X,Y,R)->R\==[].
apply_as_pred(EqlPred,X):-call(EqlPred,X,R)->R\==[].

% Maybe move to funcall 
function(cl_funcall,Pred,Y,Result):- !, function(Pred,Y,Result).
function(Pred,X,Y,Result):- trace,wdmsg(function(Pred,X,Y,Result)).
function(X,function(X)).
% used by call_as_ident/3
function([],X,X):-!.
function(Pred,X,Result):- call(Pred,X,Result),!.


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


% Maybe move to arglists
% key_value(Keys,Name,Value,Default).
key_value(Keys,Name,Value):- is_dict(Keys),!,Keys.Name=Value,!.
key_value(Keys,Name,Value):- get_plist_value(Keys,Name,Value).
key_value(Keys,Name,Value,_Default):- key_value(Keys,Name,Value),!.
key_value(_Keys,_Name,Default,Default).


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

% #'MEMBER
wl:init_args(2, cl_member).
cl_member(Item,List,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(Keys,EqlPred),
 key_value(Keys,kw_from_end,FromEnd1,[]),
 range_1(List,Keys,RList,_Start1),
 ((FromEnd1==[]->append(_,[V|Rest],RList); append_r(_,[V|Rest],RList))),
 call_as_ident(Ident,V,Id),apply_as_pred(EqlPred,Item,Id),!,
  RetVal = [V|Rest].
cl_member(_,_,_,[]).

% #'MEMBER-IF
wl:init_args(2, cl_member_if).
cl_member_if(E,Seq,Keys,Result):- cl_member(E,Seq,[kw_test,cl_funcall|Keys],Result).  

% #'MEMBER-IF-NOT
wl:init_args(2, cl_member_if_not).
cl_member_if_not(E,Seq,Keys,Result):- cl_member(E,Seq,[kw_test_not,cl_funcall|Keys],Result).

wl:init_args(2, cl_find).
cl_find(E,Seq,Keys,Result):-
   cl_member(E,Seq,Keys,MemberResult),
   cl_car(MemberResult,Result).

wl:init_args(2, cl_find_if).
cl_find_if(E,Seq,Keys,Result):- cl_find(E,Seq,[kw_test,cl_funcall|Keys],Result).  

wl:init_args(2, cl_find_if_not).
cl_find_if_not(E,Seq,Keys,Result):- cl_find(E,Seq,[kw_test_not,cl_funcall|Keys],Result).


% #'SEARCH  - http://www.lispworks.com/documentation/HyperSpec/Body/f_search.htm
wl:init_args(2, cl_search).
cl_search(X,Y,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(Keys,EqlPred),
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
cl_search(_,_,_,[]).




% #'POSITION   - http://www.lispworks.com/documentation/HyperSpec/Body/f_pos_p.htm
wl:init_args(2, cl_position).
cl_position(Item,List,Keys,RetVal):-
 get_identity_pred(Keys,kw_key,Ident),
 get_test_pred(Keys,EqlPred),
 key_value(Keys,kw_from_end,FromEnd1,[]),
 range_1(List,Keys,RList,OffsetReturn),
 ((FromEnd1==[]->append(LeftOffset,[V|_Rest],RList); append_r(LeftOffset,[V|_Rest],RList))),
 call_as_ident(Ident,V,Id),apply_as_pred(EqlPred,Item,Id),!,
 length(LeftOffset,N),
 RetVal is N + OffsetReturn. 
cl_position(_,_,_,[]).

% #'POSITION-IF
wl:init_args(2, cl_position_if).
cl_position_if(E,Seq,Keys,Result):- cl_position(E,Seq,[kw_test,cl_funcall|Keys],Result).  

% #'POSITION-IF-NOT
wl:init_args(2, cl_position_if_not).
cl_position_if_not(E,Seq,Keys,Result):- cl_position(E,Seq,[kw_test_not,cl_funcall|Keys],Result).


% #'REVERSE
cl_reverse(Xs, Ys) :-
    lists:reverse(Xs, [], Ys, Ys).

% #'NREVERSE
cl_nreverse(Xs, Ys) :-
    lists:reverse(Xs, [], Ys, Ys).



% #'replace
(wl:init_args(2,cl_replace)).
cl_replace(X,Y,Keys,XR):-
   range_1_and_2_len(X,Y,Keys,XR,YR,Count),
   replace_each(Count,XR,YR).

replace_each(0,_XR,_YR):-!.
replace_each(_,[],_):-!.
replace_each(_,_,[]):-!.
replace_each(Count,XR,[Y|YR]):- nb_setarg(1,XR,Y),arg(2,XR,XT),Count2 is Count-1,replace_each(Count2,XT,YR).
   

wl:init_args(1,cl_mapcar).
cl_mapcar(P, [[H|T]], [RH|RT]) :- !, cl_apply(P, [H], RH),cl_mapcar(P, [T], RT).
cl_mapcar(P, [[H|T],[H2|T2]], [RH|RT]) :- !, cl_apply(P, [H,H2], RH),cl_mapcar(P, [T,T2], RT).
cl_mapcar(P, [[H|T],[H2|T2],[H3|T3]], [RH|RT]) :- !, cl_apply(P, [H,H2,H3], RH),cl_mapcar(P, [T,T2,T3], RT).
cl_mapcar(_, [[]|_], []).


(wl:init_args(0,cl_nconc)).
cl_nconc([L1,L2],Ret):- !, append(L1,L2,Ret).
cl_nconc([L1],L1):-!.
cl_nconc([L1,L2|Lists],Ret):- !,cl_nconc([L2|Lists],LL2), append(L1,LL2,Ret).
cl_nconc(X,X):-!.

cl_copy_list(List,List):- \+ compound(List),!.
cl_copy_list([M|List],[M|Copy]):-cl_copy_list(List,Copy).


wl:type_checked(cl_length(claz_cons,integer)).
cl_length(Sequence,Len):- always(length(Sequence,Len)).



cl_remove('$ARRAY'([S],Type,A),B,'$ARRAY'([Sm1],Type,C)):-pl_remove(-1,is_equal,A,B,C,Did),(number(S)->Sm1 is S-Did ; Sm1=S).
cl_remove(A,B,C):- pl_remove(-1,is_equal,A,B,C,_Did).

pl_remove(_Tst,0, X, _, X, 0).
pl_remove(_Tst,_,[], _, [],0).
pl_remove(Tst,May,[Elem|Tail], Del, Result,Done2) :-
    ( call(Tst,Elem,Del) ->  (May2 is May-1, pl_remove(Tst,May2,Tail, Del, Result,Done),Done2 is Done+1)
    ; ( Result = [Elem|Rest],pl_remove(Tst,May,Tail,Del,Rest,Done2))).

%cl_subst('$ARRAY'([S],Type,A),B,C,'$ARRAY'([Sm1],Type,R)):-pl_subst(C,B,A,R),(number(S)->Sm1 is S-Did ; Sm1=S).
cl_subst(A,B,C,R):-pl_subst(C,B,A,R).

pl_subst( Var, VarS,SUB,SUB ) :- Var==VarS,!.
pl_subst([H|T],B,A,[HH|TT]):- !,pl_subst(H,B,A,HH),pl_subst(T,B,A,TT).
pl_subst( Var, _,_,Var ).

:- fixup_exports.

      
end_of_file.


