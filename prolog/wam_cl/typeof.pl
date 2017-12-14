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
:- module(typeof, []).
:- set_module(class(library)).
:- include('header').

cl_class_of(Obj,Class):- i_class(Obj,Class),!.

:- use_module(library('dialect/sicstus/arrays')).
% :- use_module(library('dialect/sicstus')).
% Numbers, pathnames, and arrays are examples of self-evaluating objects.
is_self_evaluating_object(X):- var(X),!.
is_self_evaluating_object(X):- atomic(X),!,is_self_evaluationing_const(X).
is_self_evaluating_object('$OBJ'(_,_)):-!.
is_self_evaluating_object('#\\'(_)):-!.
is_self_evaluating_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.
is_self_evaluating_object(P):- compound_name_arity(P,F,_),atom_concat_or_rtrace('$',_,F),!.

is_self_evaluationing_const(X):- atomic(X),is_self_evaluationing_const0(X),!.
is_self_evaluationing_const0(X):- (X==t;X==[];number(X);is_keywordp(X);string(X);(blob(X,T),T\==text)),!.
is_self_evaluationing_const0(X):- is_functionp(X),!.

i_class(Var,Class):-attvar(Var),get_attr(Var,classof,Class).
i_class(Var,claz_locative):-var(Var).
% compounds
i_class(function(OP),Class):- get_opv(OP,function,Obj),cl_class_of(Obj,Class).
i_class([_|_],claz_cons):-!.
i_class('$OBJ'(Type,_Data),Type).
i_class('#\\'(_),claz_character).
i_class('$COMPLEX'(_,_),claz_complex).
i_class('$NUMBER'(Type,_),Type).
% atomics
i_class([],claz_null):-!.
i_class(Str,claz_string):- is_stringp(Str).
i_class(t,claz_symbol).
i_class(Dict,Type):- is_dict(Dict,Type).
i_class(Number,claz_integer):- integer(Number).
i_class(Number,claz_float):- float(Number).
i_class(Atom,Kind):- atom(Atom),atomic_list_concat([Type,_Name],'_znst_',Atom),atom_concat_or_rtrace('claz_',Type,Kind).
i_class(Obj,Type):- get_opv_i(Obj,classof,Type).
i_class(function(_),claz_function).


i_type(Var,sys_locative):-var(Var).
i_type([],null):-!.
i_type([_|_],cons):-!.
i_type(Obj,Type):- get_opv_i(Obj,dims,List),(List=[N] -> Type = [simple_vector,N]; Type = [array,List]).
i_type(Obj,Type):- get_opv_i(Obj,typeof,Type).
i_type(Obj,Type):- get_opv_i(Obj,classof,Class),claz_to_symbol(Class,Type).
i_type(Dict,Type):- is_dict(Dict,Type).
i_type(Str,string):- is_stringp(Str).
i_type(t,boolean).
i_type('#\\'(_),character).
i_type(Obj,Type):- number(Obj),!,number_type_of(Obj,Type).
i_type('$OBJ'(Type,_Data),Type).
i_type(Atom,Type):- atom(Atom),atomic_list_concat([Type,_Name],'_znst_',Atom).
i_type(function(OP),Class):- get_opv(OP,function,Obj),cl_type_of(Obj,Class).

type_ges(function(_),function).
type_ges(Obj,Type):- compound(Obj),functor(Obj,Type,_).
type_ges(Atom,Type):- atom(Atom),atomic_list_concat([Prefix|Rest],'_',Atom),prefix_to_typeof(Prefix,Rest,Atom,Type),!.


type_or_class_nameof(Obj,Name):- quietly((cl_class_of(Obj,Type),type_named(Type,Name),atom(Name))).

type_named('$OBJ'(_,Type),Type):- atom(Type),!.
type_named(Type,Type):- atomic(Type).


cl_typep(Obj,Type,Result):- t_or_nil(is_typep(Obj,Type),Result),push_values([Result,t],_).

is_subtypep(SubType,Type):- find_class(SubType,SubClass),find_class(Type,Class),is_subclass(SubClass,Class).
is_subclass(Class,Class).
is_subclass(SubClass,Class):- get_struct_opv(SubClass,include,Class);(get_struct_opv(SubClass,super_priority,Classes),memberchk(Class,Classes)).
is_typep(Obj,Type):- i_type(Obj,SubType),is_subtypep(SubType,Type),!.

cl_type_of(O,T):- i_type(O,T),!.
cl_type_of(O,T):- type_ges(O,T),!.
cl_type_of(_Obj,t).



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

