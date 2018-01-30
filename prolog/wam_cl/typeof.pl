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
:- module(typ30f, []).

:- include('header').

f_class_of(Obj,Class):- quietly(i_class(Obj,Class0)),!,Class=Class0.
f_class_of(_,claz_t).

:- user:use_module(library('dialect/sicstus/arrays'),[is_array/1]).
% :- use_module(library('dialect/sicstus')).
% Numbers, pathnames, and arrays are examples of self-evaluating objects.
is_self_evaluating_object(X):- var(X),!.
is_self_evaluating_object(X):- is_pathnamep(X),!.
is_self_evaluating_object(X):- atomic(X),!,is_self_evaluationing_const(X).
is_self_evaluating_object('$OBJ'(_,_)):-!.
is_self_evaluating_object('#\\'(_)):-!.
is_self_evaluating_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.
is_self_evaluating_object(P):- compound_name_arity(P,F,_),atom_concat_or_rtrace('$',_,F),!.

is_self_evaluationing_const(X):- atomic(X),is_self_evaluationing_const0(X),!.
is_self_evaluationing_const0(X):- (X==t;X==[];number(X);is_keywordp(X);string(X);(blob(X,T),T\==text)),!.
is_self_evaluationing_const0(X):- is_packagep(X),!.
is_self_evaluationing_const0(X):- is_functionp(X),!.
is_self_evaluationing_const0(X):- is_classp(X),!.

i_class(Atom,Class):- atom(Atom),!,a_class(Atom,Class).
i_class(Obj,Type):- compound(Obj),!,c_class(Obj,Type),!.
i_class(Var,Class):-attvar(Var),get_attr(Var,type_of,Class).
i_class(Var,claz_locative):-var(Var).
% atomics
i_class([],claz_null):-!.
i_class(Number,claz_integer):- integer(Number).
i_class(Number,claz_float):- float(Number).
i_class(Str,claz_string):- is_stringp(Str).
% atom
a_class(t,claz_symbol).
a_class(symbol,claz_symbol).
a_class(Obj,Class):- b_type(Obj,Type),always_find_class(Type,Class).
% compounds
c_class(function(OP),Class):- atom(OP),get_opv(OP,symbol_function,Obj),f_class_of(Obj,Class).
c_class(function(_),claz_function).
c_class([_|_],claz_cons):-!.
c_class('$OBJ'(Type,_Data),Type).
c_class('#\\'(_),claz_character).
c_class('$COMPLEX'(_,_),claz_complex).
c_class('$NUMBER'(Type,_),Type).
c_class(Dict,Class):- is_dict(Dict,Class).
c_class(Str,claz_string):- is_stringp(Str).

always_find_class(keyword,claz_symbol).
always_find_class(boolean,claz_symbol).
always_find_class(Type,Class):- find_class(Type,Class),!.
always_find_class(Type,Class):- atom_concat_or_rtrace('claz_',Type,Class).

i_type(Var,sys_locative):-var(Var).
i_type(t,boolean).
i_type([],null).
i_type(Obj,Type):- atom(Obj),!,a_type(Obj,Type),!.
i_type([_|_],cons):-!.
i_type(Dict,Type):- is_dict(Dict,Type).
i_type(Str,string):- is_stringp(Str).
i_type('#\\'(_),character).
i_type(Obj,Type):- number(Obj),!,number_type_of(Obj,Type).
i_type(Obj,Type):- compound(Obj),i_class(Obj,Class),claz_to_symbol(Class,Type).
i_type('$NUMBER'(Type,_),Type).
i_type(function(OP),Class):- get_opv(OP,symbol_function,Obj),f_type_of(Obj,Class).

% atomics
%a_type(Obj,Type):- get_opv_iiii(Obj,classof,Class),claz_to_symbol(Class,Type),!.
a_type(Obj,Type):- b_type(Obj,Type),!.

b_type(Obj,Type):- nonvar(Type),b_type(Obj,Type0),!,Type=Type0.
b_type(Obj,_):- \+ atom(Obj),!,fail.
b_type(Obj,keyword):- package_external_symbols(pkg_kw,_,Obj).
b_type(Obj,symbol):- package_external_symbols(_,_,Obj).
b_type(Obj,symbol):- package_internal_symbols(_,_,Obj).
b_type(Obj,Type):- get_opv_iiii(Obj,type_of,Type),!.
b_type(Obj,Type):- get_opv_iiii(Obj,dims,List),(List=[N] -> Type = [simple_vector,N]; Type = [array,List]),!.
b_type(Atom,Type):- atomic_list_concat([Type,_Name],'_znst_',Atom),!.
b_type(Atom,Type):- atomic_list_concat([Prefix|Rest],'_',Atom),prefix_to_typeof(Prefix,Rest,Atom,Type),!.

type_ges(function(_),function).
type_ges(Obj,Type):- compound(Obj),functor(Obj,Type,_).
type_ges(Atom,Type):- atom(Atom),atomic_list_concat([Prefix|Rest],'_',Atom),prefix_to_typeof(Prefix,Rest,Atom,Type),!.


type_or_class_nameof(Obj,Type):- quietly(f_type_of(Obj,Type)),!.

type_named('$OBJ'(_,Type),Type):- atom(Type),!.
type_named(Type,Type):- atomic(Type).


f_typep(Obj,Type,OptEnv,Result):- t_or_nil(is_typep(Obj,OptEnv,Type),Result),f_values_list([Result,t],_).
is_typep(Obj,Type,OptEnv):- i_type(Obj,SubType),is_subtypep(SubType,Type,OptEnv),!.

f_subtypep(SubType,Type,OptEnv,Result):- t_or_nil(is_subtypep(SubType,Type,OptEnv),Result).
is_subtypep(SubType,Type,OptEnv):- 
  OptErrorOptEnv = [[]|OptEnv],
  f_find_class(SubType,OptErrorOptEnv,SubClass),f_find_class(Type,OptErrorOptEnv,Class),is_subclass(SubClass,Class).

is_subclass(SubClass,Class):- SubClass=Class; get_super_class(SubClass,Class).


f_type_of(O,T):- i_type(O,T),!.
f_type_of(O,T):- type_ges(O,T),!.
f_type_of(_Obj,t).



% prefix_to_typeof(Prefix,Rest,Atom,Type).
prefix_to_typeof(pkg,[_|_],_,package).
prefix_to_typeof(f,[_|_],_,function).
prefix_to_typeof(sf,[_|_],_,sys_special_operator).
prefix_to_typeof(mf,[_|_],_,sys_macro).
prefix_to_typeof(kw,[_|_],_,keyword).
prefix_to_typeof(claz,[_|_],_,standard_class).
%prefix_to_typeof(sys,[_|_],_,symbol).
prefix_to_typeof(_,symbol).

float_bits(_,32).
int_bits(_,24).

number_type_of(0,bit).
number_type_of(1,bit).
number_type_of(Obj,Type):- float(Obj),float_bits(Obj,Bits), (Bits =< 32 -> Type=single_float ; Type=double_float).
number_type_of(Obj,Type):- integer(Obj),int_bits(Obj,Bits),(Bits =< 24 -> Type=fixnum ; Type=bignum).
number_type_of(_Obj,number).


:- fixup_exports.

