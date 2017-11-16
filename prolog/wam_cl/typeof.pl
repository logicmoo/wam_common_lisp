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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(typeof, []).
:- set_module(class(library)).
:- include('header.pro').

cl_type_of(O,T):- type_i(O,T),!.
cl_type_of(O,T):- type_ges(O,T),!.
cl_type_of(_Obj,t).

cl_class_of(Obj,Class):- get_opv(Obj,classof,Class),!.
cl_class_of(Obj,'$OBJ'(claz_builtin_class,Type)):- type_i(Obj,Type).
cl_class_of(function(OP),Class):- get_opv(OP,function,Obj),!,cl_class_of(Obj,Class).
cl_class_of(Obj,'$OBJ'(claz_class,Type)):- type_ges(Obj,Type).
cl_class_of(_,'$OBJ'(claz_builtin_class,t)).

type_i(Var,sys_locative):-var(Var).
type_i([],null):-!.
type_i([_|_],cons):-!.
type_i('$OBJ'(Type,_Data),Type).
type_i(Dict,Type):- is_dict(Dict,Type).
type_i(Str,string):- string(Str).
type_i(t,boolean).
type_i(Obj,Type):- number(Obj),!,number_type_of(Obj,Type).
type_i('$CHAR'(_),character).
type_i(Obj,Type):- get_opv(Obj,typeof,Type).
type_i(function(OP),Class):- get_opv(OP,function,Obj),!,cl_type_of(Obj,Class).


type_or_class_nameof(Obj,Name):- cl_class_of(Obj,Type),type_named(Type,Name),atom(Name).

type_named('$OBJ'(_,Type),Type):- atom(Type),!.
type_named(Type,Type):- atomic(Type).


type_ges(function(_),function).
type_ges(Obj,Type):- compound(Obj),functor(Obj,Type,_).
type_ges(Atom,Type):- atom(Atom),atomic_list_concat([Prefix|Rest],'_',Atom),prefix_to_typeof(Prefix,Rest,Atom,Type),!.



% prefix_to_typeof(Prefix,Rest,Atom,Type).
prefix_to_typeof(pkg,[_],_,package).
prefix_to_typeof(f,[_,_|_],_,function).
prefix_to_typeof(kw,[_|_],_,keyword).
prefix_to_typeof(cl,[_|_],_,function).
prefix_to_typeof(_,symbol).

float_bits(_,32).
int_bits(_,24).

number_type_of(0,bit).
number_type_of(1,bit).
number_type_of(Obj,Type):- float(Obj),float_bits(Obj,Bits), (Bits =< 32 -> Type=single_float ; Type=double_float).
number_type_of(Obj,Type):- integer(Obj),int_bits(Obj,Bits),(Bits =< 24 -> Type=fixnum ; Type=bignum).
number_type_of(_Obj,number).


:- fixup_exports.

