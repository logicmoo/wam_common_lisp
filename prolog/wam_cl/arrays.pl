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
f_array_dimension(Obj,Axis,RetVal):- get_opv(Obj,dims,List),nth0(Axis,List,RetVal).


wl:init_args(1,aref).
f_aref(Obj,Indexes,RetVal):-!, get_array_data(Obj,List),!,nth_index(Indexes,List,RetVal).
%f_aref(List,Index,RetVal):- f_nthcdr(List,Index,RetVal).


get_array_data(List,List):- is_list(List).
get_array_data(Obj,List):- get_opv(Obj,data,List),!.
get_array_data(Obj,List):- get_opv(Obj,array,Obj2),!,get_array_data(Obj2,List).




wl:init_args(0,make_array).
f_make_array(Dims,RetVal):- listify(Dims,DimsL),
 create_object(claz_array,[dims=DimsL],RetVal).
f_make_array(Dims,Keys,RetVal):- listify(Dims,DimsL),
 create_object(claz_array,[dims=DimsL|Keys],RetVal).

wl:init_args(0,vector).
f_vector(Elements,RetVal):-
 length(Elements,Size),
 create_struct(claz_array,[dims=[Size],data=Elements],RetVal).

% GROVELED f_vectorp(Obj,RetVal):- t_or_nil(is_vectorp(Obj),RetVal).
% GROVELED f_arrayp(Obj,RetVal):- t_or_nil(is_arrayp(Obj),RetVal).


is_vectorp(Obj):- get_opv(Obj,dims,List),List=[_].
is_arrayp(Obj):- get_opv(Obj,type_of,array).

:- fixup_exports.



