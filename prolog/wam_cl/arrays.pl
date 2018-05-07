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

:- include('./header').

% make-array dimensions &key element-type initial-element initial-contents adjustable fill-pointer displaced-to displaced-index-offset


f_adjustable_array_p(Obj,RetVal):-
  t_or_nil(get_opv(Obj,adjustable,t),RetVal).


f_array_dimensions(Obj,RetVal):- always(get_opv(Obj,dims,RetVal)).
f_array_dimension(Obj,Axis,RetVal):- get_opv(Obj,dims,Elements),nth0(Axis,Elements,RetVal).


wl:init_args(1,aref).
f_aref(Obj,Indexes,RetVal):- get_adata(Obj,Elements),!,nth_index(Indexes,Elements,RetVal).
f_aset(Obj,Indexes,Value,RetVal):- get_adata(Obj,Elements),!,set_nth_index(Indexes,Elements,Value,RetVal).
%f_aref(Elements,Index,RetVal):- f_nthcdr(Elements,Index,RetVal).


get_adata('$ARRAY'(_,_,Elements),Elements):- !,nonvar(Elements).
get_adata('$OBJ'(_,Value),Elements):- !,nonvar(Value),get_adata(Value,Elements).
get_adata(Elements,Elements):- is_list(Elements),!.
get_adata(Obj,Elements):- get_opv(Obj,data,Elements),!.
get_adata(Obj,Elements):- get_opv(Obj,array,Array),!,get_adata(Array,Elements).
get_adata(Obj,Elements):- get_opv(Obj,dims,Indexes),!,
   create_array_data(Indexes,Data),
   set_opv(Obj,data,Data),get_opv(Obj,data,Elements).
get_adata(Obj,Elements):- get_opv(Obj,value,Data),!,get_adata(Data,Elements).

create_array_data([Len|Indexes],Obj):-
   create_array_data(Indexes,Elements),!,
   init_list_with_copy_data(Len,Elements,Obj).
create_array_data([],[]).
create_array_data(Len,Elements):-
  init_list_with_copy_data(Len,[],Elements).

init_list_with_copy_data(0,_,[]):-!.
init_list_with_copy_data(Len,Elements,[CData|Obj]):-
  copy_term(Elements,CData),
  LenM is Len-1,
  init_list_with_copy_data(LenM,Elements,Obj).


% svref simple-vector index => element
% (setf (svref simple-vector index) new-element)

wl:init_args(0,make_array).
f_make_array(Dims,RetVal):- listify(Dims,DimsL),
 create_object(claz_array,[dims=DimsL],RetVal).
f_make_array(Dims,Keys,RetVal):- listify(Dims,DimsL),
 create_object(claz_array,[dims=DimsL|Keys],RetVal).

wl:init_args(0,vector).
f_vector(Size,RetVal):-
 create_array_data([Size],Elements),
 create_struct(claz_array,[dims=[Size],data=Elements],RetVal).

% GROVELED f_vectorp(Obj,RetVal):- t_or_nil(is_vectorp(Obj),RetVal).
% GROVELED f_arrayp(Obj,RetVal):- t_or_nil(is_arrayp(Obj),RetVal).


is_vectorp(Obj):- get_opv(Obj,dims,Dims),Dims=[_].
is_arrayp(Obj):- get_opv(Obj,type_of,array).

:- fixup_exports.



