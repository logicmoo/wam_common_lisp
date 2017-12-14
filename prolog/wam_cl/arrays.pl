/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(aray, []).
:- set_module(class(library)).
:- include('header').

% make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset

e1_as_list(List,List):- is_list(List),!.
e1_as_list(H,[H]):-!.

cl_array_dimensions(Obj,RetVal):- always(get_opv(Obj,dims,RetVal)).
cl_array_dimension(Obj,Axis,RetVal):- get_opv(Obj,dims,List),nth0(Axis,List,RetVal).

cl_nth(Axis,List,RetVal):- nth0(Axis,List,RetVal).
cl_elt(List,Axis,RetVal):- nth0(Axis,List,RetVal).

cl_nthcdr(_,[],[]):-!.
cl_nthcdr(0,List,List).
cl_nthcdr(Index,[_|List],RetVal):- Next is Index-1,cl_nthcdr(Next,List,RetVal).

cl_set_nthcdr(_,[],[]):-!.
cl_set_nthcdr(0,List,Tail):- nb_setarg(2,List,Tail).
cl_set_nthcdr(Index,[_|List],Tail):- Next is Index-1,cl_set_nthcdr(Next,List,Tail).

wl:init_args(1,cl_aref).
cl_aref(Obj,Indexes,RetVal):-!, get_array_data(Obj,List),!,nth_index(Indexes,List,RetVal).
%cl_aref(List,Index,RetVal):- cl_nthcdr(List,Index,RetVal).

nth_index([Index],List,RetVal):- !, cl_nth(Index,List,RetVal). 
nth_index([],List,List):-!.
nth_index([Index|Indexes],List,RetVal):- cl_nth(Index,List,IndexedVal),nth_index(Indexes,IndexedVal,RetVal).

get_array_data(List,List):- is_list(List).
get_array_data(Obj,List):- get_opv(Obj,data,List),!.
get_array_data(Obj,List):- get_opv(Obj,array,Obj2),!,get_array_data(Obj2,List).


wl:init_args(0,cl_make_array).
cl_make_array(Dims,RetVal):- e1_as_list(Dims,DimsL),
 create_instance(claz_array,[dims=DimsL],RetVal).
cl_make_array(Dims,Keys,RetVal):- e1_as_list(Dims,DimsL),
 create_instance(claz_array,[dims=DimsL|Keys],RetVal).

wl:init_args(0,cl_vector).
cl_vector(Elements,RetVal):-
 length(Elements,Size),
 create_struct(claz_array,[dims=[Size],data=Elements],RetVal).

cl_vectorp(Obj,RetVal):- t_or_nil(is_vectorp(Obj),RetVal).
cl_arrayp(Obj,RetVal):- t_or_nil(is_arrayp(Obj),RetVal).
cl_listp(Obj,RetVal):- t_or_nil(is_listp(Obj),RetVal).
cl_endp(Obj,RetVal):- t_or_nil(is_endp(Obj), RetVal).


is_vectorp(Obj):- get_opv(Obj,dims,List),List=[_].
is_arrayp(Obj):- get_opv(Obj,classof,claz_array).
is_listp(Obj):- is_list(Obj).
is_endp(Obj):- Obj == [].

cl_adjustable_array_p(Obj,RetVal):-
  t_or_nil(get_opv(Obj,adjustable,t),RetVal).

% wl:type_checked(cl_subseq(sequence(T,E),number,sequence(T,E))).
cl_subseq(Seq,Offset,Result):- 
  always(coerce_to(Seq, sequence(Was,Info), Mid)),
  always(pl_subseq(Mid,Offset,MOut)),
  always(coerce_to(MOut, object(Was,Info),Result)).

pl_subseq([], Skip, []):- Skip =:=0 -> true ; throw('should not be greater than :END').
pl_subseq([Head|Tail], Skip, [Head|Cmpl]) :- Skip<1,!,
	pl_subseq(Tail, Skip, Cmpl).
pl_subseq([_|Tail], Skip, Cmpl) :- Skip2 is Skip-1,
	pl_subseq(Tail, Skip2, Cmpl).


:- fixup_exports.



