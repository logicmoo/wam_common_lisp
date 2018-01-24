/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(soops, []).
:- set_module(class(library)).
:- include('header').

:- multifile(soops:struct_opv/3).
:- discontiguous soops:struct_opv/3.
:- dynamic((soops:struct_opv/3)).
:- multifile(soops:struct_opv/4).
:- discontiguous soops:struct_opv/4.
:- dynamic((soops:struct_opv/4)).

:- multifile(xlisting_config:xlisting_always/1).
:- dynamic(xlisting_config:xlisting_always/1).

xlisting_config:xlisting_always(G):- G=soops:_, current_predicate(_,G),
  predicate_property(G,dynamic),
  \+ predicate_property(G,imported_from(_)).

new_cl_fixnum(Init,Obj):-
  create_object(claz_fixnum,[Init],Obj),!.

% standard object
f_make_instance([Name|Slots],Obj):- always(create_object(Name,Slots,Obj)).

create_struct(X,Y):-create_object(X,Y).
create_struct(X,Y,Z):-create_object(X,Y,Z).
/*
create_struct1(Kind,[Value],Value):- data_record(Kind,[_]),!.
create_struct1(Kind,ARGS,Obj):-create_object(Kind,ARGS,Obj),!.
create_struct1(_Type,Value,Value).
*/

create_object([Name|Slots],Obj):- !,create_object(Name,Slots,Obj).
create_object(TypeARGS,Obj):- compound(TypeARGS),!,compound_name_arguments(TypeARGS,Kind,ARGS),
  create_object(Kind,ARGS,Obj),!.
create_object(Kind,Obj):- create_object(Kind,[],Obj),!.
create_object(Kind,Attrs,Obj):- new_unnamed_opv(Kind,Attrs,Obj).

new_unnamed_opv(Kind,Attrs,Obj):-  
  gensym('znst_',ZName),
  new_partly_named_opv(Kind,ZName,Attrs,Obj).

new_named_opv(SKind,Name,Attrs,Obj):-
  new_partly_named_opv(SKind,Name,Attrs,Obj).
/*
new_named_opv(SKind,Name,Attrs,Obj):-
  always((
  (var(Kind)->claz_to_symbol(SKind,Kind);true),
  (var(Obj)->Obj=Name;true),
  add_opv_new(Obj,type_of,Kind))),
  new_init_instance_pt2(SKind,Name,Attrs,Obj,Kind).
*/

new_partly_named_opv(SKind,Name,Attrs,Obj):-
  new_partly_named_opv_pt1(SKind,Name,Attrs,Obj,Kind),
  new_init_instance_pt2(SKind,Name,Attrs,Obj,Kind).

new_partly_named_opv_pt1(SKind,Name,_Attrs,Obj,Kind):-
  always((
  (var(Kind)->find_class(SKind,Kind);true),
  (var(Obj)->
    (instance_prefix(Kind,Pre),!,atomic_list_concat([Pre,Name],'_',PName),
           claz_to_symbol(Kind,Type),
                prologcase_name(PName,Obj),to_prolog_string_anyways(Name,SName),
                add_opv_new(Obj,type_of,Type),
                set_opv(Obj,debug_name,SName));add_opv_new(Obj,type_of,Type)))).

new_init_instance_pt2(_SKind,_Name,Attrs,Obj,Kind):-
  always((     
   init_instance_slots(Kind,1,Obj,Attrs),
   call_init_slot_props(Kind,Obj))).  

call_init_slot_props(Kind,Obj):- get_opv_iiii(Obj,sys_initialized,Kind),!.
call_init_slot_props(Kind,Obj):- add_opv_new_iiii(Obj,sys_initialized,Kind),
 always((
  forall(get_kind_supers(Kind,Sup),call_init_slot_props(Sup,Obj)),
  ensure_opv_type_inited(Kind),
   forall(get_struct_opv(Kind,kw_initform,Value,ZLOT),
      (  get_opv_iii(Kind,Obj,ZLOT,_)-> true ; 
         (f_eval(Value,Result),set_opv(Obj,ZLOT,Result)))))).


when_must(True,Then):- True->always(Then);true.

init_instance_slots(Kind,_Ord,Obj,PProps):-always(init_instance_kv(Kind,Obj,PProps)),!.
%init_instance_slots(Kind,Ord,Obj,PProps):-init_slot_props_iv(Kind,Ord,Obj,PProps),!.


init_slot_props_iv(_,_N,_Obj,[]):-!.
init_slot_props_iv(Kind,N,Obj,[Value|Props]):-
  add_i_opv(Kind,Obj,N,Value), N2 is N + 1,
  init_instance_kv(Kind,N2,Obj,Props).

add_i_opv(Kind,Obj,N,Value):- 
  always((get_struct_opv(Kind,kw_offset,N,ZLOT),
  get_struct_opv(Kind,kw_name,Key,ZLOT),
   add_opv(Obj,Key,Value))).

f_sys_pf_set_slot_value(Obj,Key,Value,Value):- get_object_slot_name(Obj,Key,ZLOT),!, set_opv(Obj,ZLOT,Value).
f_slot_exists_p(Obj,Slot,Value):- t_or_nil(get_opv(Obj,Slot,_),Value).

f_slot_value(Obj,Slot,Value):- always(get_opv(Obj,Slot,Value)).

f_class_slot_value(Kind,Obj,Slot,Value):- get_kind_object_slot_value(Kind,Obj,Slot,Value).
f_sys_set_class_slot_value(Kind,Obj,Slot,Value,Value):- set_kind_object_slot_value(Kind,Obj,Slot,Value).

get_kind_object_slot_value(Kind,Obj,Key,Value):- 
  always(( (nonvar(Kind),get_kind_or_supers_slot_name(Kind,Key,SlotName)) 
     ->get_opv(Obj,SlotName,Value);
       (get_object_slot_name(Obj,Key,SlotName), get_opv(Obj,SlotName,Value)))),!.

set_kind_object_slot_value(Kind,Obj,Key,Value):- 
  always(( (nonvar(Kind),get_kind_or_supers_slot_name(Kind,Key,SlotName)) 
     ->set_opv(Obj,SlotName,Value);
       (get_object_slot_name(Obj,Key,SlotName), set_opv(Obj,SlotName,Value)))),!.


get_object_slot_name(Obj,Key,SlotName):- 
    type_or_class_nameof(Obj,Kind),!,
    get_kind_or_supers_slot_name(Kind,Key,SlotName).
    
get_kind_or_supers_slot_name(Kind,Key,SlotName):- 
   ((ground(Kind+Key) -> once(get_kind_or_supers_slot_name_now(Kind,Key,SlotName)); get_kind_or_supers_slot_name_now(Kind,Key,SlotName)) *-> true;
    get_kind_slot_name(Kind,Key,SlotName)).

get_kind_or_supers_slot_name_now(Kind,Key,SlotName):-
  no_repeats(SlotName,always(((get_kind_supers(Kind,Sup),get_kind_slot_name(Sup,Key,SlotName))))),!.

get_kind_supers(Kind,Sup):- find_class(Kind,KSup), get_kind_supers3(KSup,[],Sup).      

get_kind_supers3(Kind,ExceptFor,_Sup):- member(Kind,ExceptFor),!,fail.
get_kind_supers3(Kind,_,Kind).
get_kind_supers3(Kind,ExceptFor,Sup):- 
   get_super_class(Kind,Sup),
   get_kind_supers3(Sup,[Kind|ExceptFor],SupSup),
   SupSup \== claz_null,
   SupSup \== claz_t.

get_super_class(Kind,Sup):- get_struct_opv(Kind,type,Type),find_class(Type,Sup)->Sup\==Kind.
get_super_class(Kind,Sup):- get_struct_opv(Kind,kw_include,Sup).
get_super_class(Kind,Sup):- get_struct_opv(Kind,sys_class_precedence_list,List),!,e_member(Sup,List).


get_kind_slot_name(Kind,Key,SlotName):- nonvar(Key),find_class(Kind,KSup),!,get_slot_name0(KSup,Key,SlotName),!.
get_slot_name0(Kind,Key,SlotName):- builtin_slot(Kind,Key),!,Key=SlotName.

get_slot_name0(Kind,Key,SlotName):- sys_hash_table_index_vector==Key,!,wdmsg(get_slot_name0(Kind,Key,SlotName)),break.
%get_slot_name0(claz_u_mammal, kw_legs, u_mammal_legs):-!.
%get_slot_name0(claz_u_mammal, kw_comes_from, u_mammal_comes_from):-!.
%get_slot_name0(claz_u_aardvark, kw_legs, u_mammal_legs):-!.
%get_slot_name0(claz_u_aardvark, kw_comes_from, u_mammal_comes_from):-!.
get_slot_name0(Kind,SlotName,ZLOT):- get_struct_opv_i(Kind,_,_,SlotName),ZLOT=SlotName.
get_slot_name0(Kind,SlotName,ZLOT):- get_struct_opv_i(Kind,_,OneOf,ZLOT),notrace(e_member(SlotName,OneOf)),!.
get_slot_name0(Kind,SlotName,ZLOT):- get_struct_opv_i(Super,_,OneOf,ZLOT),notrace(e_member(SlotName,OneOf)),
   wdmsg(always(f_subtypep(Kind,Super))).
get_slot_name0(claz_symbol,value,symbol_value).

/*
get_slot_name0(Kind,SlotName,ZLOT):-
   (get_struct_opv(Kind,_,_,ZLOT),ZLOT=SlotName);   
   (get_struct_opv(Kind,kw_accessor,SlotName,ZLOT));
   (get_struct_opv(Kind,kw_name,SlotName,ZLOT));
   (get_struct_opv(Kind,kw_initarg,SlotName,ZLOT));
   (get_struct_opv(Kind,sys_initargs,OneOf,ZLOT),e_member(SlotName,OneOf));
   (get_struct_opv(Kind,sys_readers,OneOf,ZLOT),e_member(SlotName,OneOf)).
*/

e_member(E,L):- is_list(L),!,member(E,L).
e_member(E,E).

slot_is_vertical_items(type).
slot_is_vertical_items(sys_initialized).
slot_is_vertical_items(kw_include).

% completed
init_instance_kv(_,_,[]).
% special list if items
init_instance_kv(Kind,Obj,[[Key|LList]|Props]):- slot_is_vertical_items(Key),!,
 always((((LList=[List],is_list(List));LList=List),  
  get_kind_slot_name(Kind,Key,SlotName),
  maplist(add_opv_new(Obj,SlotName),List))),
  init_instance_kv(Kind,Obj,Props).
% tuple
init_instance_kv(Kind,Obj,[KV|Props]):- compound(KV),get_kv(KV,Key,Value),!,
  show_call_trace(set_kind_object_slot_value(Kind,Obj,Key,Value)),
  init_instance_kv(Kind,Obj,Props).
% plist
init_instance_kv(Kind,Obj,[Key,Value|Props]):-  
  nop(always(is_keywordp(Key))),
  set_kind_object_slot_value(Kind,Obj,Key,Value),!,
  init_instance_kv(Kind,Obj,Props).

/*
init_instance_kv(Kind,Obj,[[Key|List]|Props]):- is_keywordp(Key),
   slot_is_vertical_items(Key),
   get_kind_slot_name(Kind,Key,SlotName),
   maplist(add_opv_new(Obj,SlotName),List),
  (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
   init_instance_kv(Kind,Obj,Props).

init_instance_kv(Kind,Obj,[KV|Props]):- compound(KV),get_kv(KV,Key,Value),
 (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
  get_kind_slot_name(Kind,Key,SlotName),
  add_opv_new(Obj,SlotName,Value),!,  
  init_instance_kv(Kind,Obj,Props).

init_instance_kv(Kind,Obj,[Value|Props]):-
  type_slot_number(Kind,Key),
  get_kind_slot_name(Kind,Key,SlotName),
  add_opv_new(Obj,SlotName,Value),!,
  Ord2 is Ord+1,
  init_instance_kv(Kind,Obj,Props).
*/

type_slot_number(Kind,Key,Ordinal):-
   get_kind_slot_name(Kind,Key,ZLOT),   
   get_struct_opv(Kind,kw_offset,Ordinal,ZLOT).


/*
:- defstruct([obr, [':print-function', 'print-ob']],
             "OB representation structure",
             [obnames, []],
             [slots, []],
             [literal, []],
             Kind).
*/

(wl:init_args(0,defstruct)).
(wl:init_args(0,make_instance)).
(wl:init_args(0,defclass)).


foc_class(Name,Kind):- find_class(Name,Kind),Kind\==[],!.
foc_class(Name,Kind):- % to_prolog_string_anyways(Name,SName),
   new_named_opv(claz_structure_class,Name,[],Kind),!.

find_class(Name,Claz):- atom(Name),atom_concat_or_rtrace('claz_',_,Name),!,Claz=Name.
find_class(Name,Claz):- soops:struct_opv(Claz,class_name,Name),!.
find_class(Name,Claz):- atom(Name),atom_concat_or_rtrace('claz_',Name,Claz).
%find_class(Name,Claz):- get_struct_opv(Claz,name,Name),!.


f_find_class(Name,Claz):- always(is_symbolp(Name)),
  once((find_class(Name,Claz),claz_to_symbol(Claz,Sym),Name==Sym,always(is_classp(Claz)))).
f_find_class(_,[]).
  
is_classp(C):-f_type_of(C,T),is_class_classp(T),!.
is_classp(C):-atom(C),atom_concat('claz_',_,C).


is_class_classp(standard_class).
is_class_classp(built_in_class).
is_class_classp(structure_class).
is_class_classp(class).

sf_defstruct(_ReplEnv,[[Name,KeyWords]|Slots],Name):- !, always(define_struct(Name,KeyWords,Slots,_Kind)).
sf_defstruct(_ReplEnv,[[Name|KeyWords]|Slots],Name):- !, always(define_struct(Name,KeyWords,Slots,_Kind)).
sf_defstruct(_ReplEnv,[Name|Slots],Name):- always(define_struct(Name,[],Slots,_Kind)).

sf_defclass(_ReplEnv,[Name,Supers,Slots|KwInfo],Kind):- !, always(define_class(Name,[[kw_include,Supers]|KwInfo],Slots,Kind)).


define_class(Name,KeyWords,SlotsIn,Kind):- 
  (var(Kind) -> (( new_named_opv(claz_standard_class,Name,[],Kind)));true),
   define_kind(defclass,Name,KeyWords,SlotsIn,Kind),
   ensure_metaobject(Kind,_),
   ensure_prototype(Kind,_).


is_prop_class_alloc(Type,SlotName,Where):- % \+ not_shareble_prop(SlotName),
 freeze(Type,find_class(Type,Kind)),
 get_struct_opv_i(Kind,kw_allocation,kw_class,ZLOT),
  get_kind_slot_name(Kind,SlotName,ZLOT0),ZLOT0=ZLOT,
  ensure_metaobject(Kind,Where).

% @TODO Store INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and SHARED-INITIALIZE  Hooks
ensure_prototype(Kind,Instance):- get_struct_opv(Kind,prototype,Instance),!.
ensure_prototype(Kind,Instance):- make_proto_instance(Kind,Instance),assert_struct_opv(Kind,prototype,Instance).

make_proto_instance(Kind,Obj):-
  new_partly_named_opv_pt1(_SKind,znst_prototypical_0,[],Obj,Kind).

ensure_metaobject(Kind,Instance):- get_struct_opv(Kind,metaobject,Instance),!.
ensure_metaobject(Kind,Instance):- make_metaobject_instance(Kind,Instance),assert_struct_opv(Kind,metaobject,Instance).

make_metaobject_instance(Kind,Obj):-
  new_partly_named_opv_pt1(Kind,znst_metaobject_0,[],Obj,_Kind).



define_struct(Name,KeyWords,SlotsIn,Kind):- 
  (var(Kind) -> (( new_named_opv(claz_structure_class,Name,[],Kind)));true),
   define_kind(defstruct,Name,KeyWords,SlotsIn,Kind).

is_structure_classp(T):- get_opv(T,type_of,TT),!,TT=structure_class.


define_kind(DefType,Name,KeyWords,SlotsIn,Kind):- 
 always((
  (DefType == defstruct-> KindKind = structure_class ; KindKind = standard_class ),
  assert_struct_opv(Kind,class_name,Name),
  % assert_struct_opv(Kind,type,Name),  
  % add doc for string
  maybe_get_docs('class',Name,SlotsIn,Slots,Code),
  always(Code),  
  %add_class_keywords(Kind,KeyWords),
  init_instance_kv(KindKind,Kind,KeyWords),
  get_struct_offset(Kind,Offset),
  NOffset is Offset +1,  
  add_class_slots(DefType,Kind,NOffset,Slots),  
  generate_missing_claz_functions(KindKind,Kind))).

get_struct_offset(Kind,W):- get_struct_opv(Kind,initial_offset,W).
get_struct_offset(_,0).

generate_missing_claz_functions(_KindKind,Kind):-
  always(( claz_to_symbol(Kind,Name),
  to_prolog_string_anyways(Name,SName),
 % define keyword defaults now
 show_call_trace(make_default_constructor(Kind,Code)),
 always(Code),
 show_call_trace(maybe_add_kw_function(Kind,SName,"-P",kw_predicate, [obj],( eq('type-of'(obj),quote(Name))))),
 % make accessors
 struct_opv_else(Kind,kw_conc_name,ConcatName,(string_concat(SName,"-",ConcatName),
    assert_struct_opv(Kind,kw_conc_name,ConcatName))),
 forall(get_struct_opv(Kind,kw_name,SlotName,ZLOT),
   (intern_slot_name('',ConcatName,SlotName,ConcSlotName),
    add_slot_accessor_functions(Kind,ConcSlotName,ZLOT))),
 forall(get_struct_opv(Kind,kw_reader,Accessor,ZLOT),
    add_slot_accessor_functions(Kind,Accessor,ZLOT)),
 forall(get_struct_opv(Kind,kw_accessor,Accessor,ZLOT),
    add_slot_accessor_functions(Kind,Accessor,ZLOT)))).

% % % % % % 
always_ignore(G):- always(ignore(G)).


/*
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))
*/
make_default_constructor(Kind,Code):- 
 always((
 get_struct_opv(Kind,class_name,Name),to_prolog_string_anyways(Name,SName),
 atom_concat_or_rtrace("MAKE-",SName,FnName),
 force_symbol_package(Name,Package),
 (package_not_for_slots(Package)->f_intern(FnName,FnSym);f_intern(FnName,Package,FnSym)),
 foc_operator(_,_,kw_function,FnSym,3,Function), 
 Head=..[Function,List,Obj],
 Body=..[f_make_instance,[Kind|List],Obj],
 Code = (assert_lsp(FnSym,wl:init_args(0,FnSym)),
         assert_lsp(FnSym,wl:init_args(0,Function)),
         %set_opv(Function,type_of,compiled_function),
         set_opv(FnSym,symbol_function,Function),
         assert_lsp(Name,(user:Head:-Body))))).
 



maybe_add_get_set_functions(Kind,ZLOT):- 
  get_struct_opv(Kind,kw_accessor,Accessor,ZLOT),!,
   add_slot_accessor_functions(Kind,Accessor,ZLOT),
   add_slot_accessor_functions(Kind,ZLOT,ZLOT).
maybe_add_get_set_functions(Kind,ZLOT):- 
  Accessor = ZLOT,
  assert_struct_opv4(Kind,kw_accessor,Accessor,ZLOT),
   add_slot_accessor_functions(Kind,Accessor,ZLOT).

add_slot_accessor_functions(Kind,Accessor,ZLOT):-
  add_slot_getter_function(Kind,Accessor,ZLOT),
   (\+ get_struct_opv(Kind,read_only,t,ZLOT) ->
   add_slot_setter_function(Kind,Accessor,ZLOT) ; true).


add_slot_getter_function(Kind,Accessor,ZLOT):-
  maybe_add_function(Accessor,[object],['class-slot-value',Kind,object,[quote,ZLOT]],Added1), 
 (Added1\==[]-> push_struct_opv(Kind,sys_readers,Added1,ZLOT) ; true).

add_slot_setter_function(Kind,Accessor,ZLOT):-
  SETTER = [setf,Accessor],
  maybe_add_function(SETTER,[object,value],['set-class-slot-value',Kind,object,[quote,ZLOT],value],Added1), 
 (Added1\==[]-> push_struct_opv(Kind,sys_writers,Added1,ZLOT) ; true).


member_element_list(kw_writer,sys_writers).
member_element_list(kw_reader,sys_readers).

f_clos_class_direct_superclasses(C,SL):-findall(S,struct_opv(C,kw_include,S),SL).

f_clos_class_precedence_list(C,SL):- struct_opv(C,super_priority,SL),!.
f_clos_class_precedence_list(C,SL):- f_clos_class_direct_superclasses(C,List1),maplist(f_clos_class_direct_superclasses,List1,List2),
   append(List1,List2,List3),list_to_set(List3,SL).
   

% catch accidental unification that destroys metaclasses
classof:attr_unify_hook(A,B):- trace,wdmsg(classof:attr_unify_hook(A,B)),lisp_dump_break. %  break.


maybe_add_kw_function(Kind,L,R,Key,ArgList,LispBody):- 
  (get_struct_opv(Kind,Key, FnName) -> true; atom_concat_or_rtrace(L,R,FnName)),
   maybe_add_function(FnName,ArgList,LispBody,_).

maybe_add_function(FnName,ArgList,LispBody,R):-   
   ((atom(FnName),reader_intern_symbols(FnName,Sym),is_implemented(Sym))->R=Sym;
     ((R=Result,as_sexp(LispBody,SLispBody),
       reader_intern_symbols([defun,FnName,ArgList,[progn,SLispBody]],LispInterned),
         ((lisp_compile(Result,LispInterned,PrologCode),
             cmpout(PrologCode),
             always(PrologCode)))))).


struct_opv_else(Kind,Key,Value,Else):-
   (get_struct_opv(Kind,Key,Value)->true;
     (Else,assert_struct_opv(Kind,Key,Value))).
  


add_class_keywords(_Struct,[]):-!.
add_class_keywords(Kind,[[Key,Value]|KeyWords]):- !,
   assert_struct_kw(Kind, Key, Value),
   add_class_keywords(Kind,KeyWords).
add_class_keywords(Kind,[[Key|Value]|KeyWords]):- !, 
   assert_struct_kw(Kind, Key, Value),
   add_class_keywords(Kind,KeyWords).
add_class_keywords(Kind,[Key|KeyWords]):-  Key = kw_named,
   assert_struct_kw(Kind, Key, t),
   add_class_keywords(Kind,KeyWords).
add_class_keywords(Kind,[Key,Value|KeyWords]):-    
   assert_struct_kw(Kind, Key, Value),
   add_class_keywords(Kind,KeyWords).

assert_struct_kw(Kind, Key, Value):- 
  ignore(( \+ is_keywordp(Key) , dbginfo(warn(assert_struct_kw(Kind, Key, Value))))),
  assert_struct_opv(Kind, Key, Value).
  
assert_struct_opv(Obj, Key, Value):-
  show_call_trace(assertz_new(soops:struct_opv(Obj, Key, Value))).

%assert_struct_opv4(_Obj, sys_initargs, _Value, _Info):- trace,fail.

assert_struct_opv4(Obj, Key, Value,Info):-
  show_call_trace(assertz_new(soops:struct_opv(Obj, Key, Value,Info))).

update_struct_opv4(Obj, Key, Value,Info):-
  ignore(retract(soops:struct_opv(Obj, Key, _,Info))),
  show_call_trace(assertz_new(soops:struct_opv(Obj, Key, Value,Info))).

get_struct_opv(Obj, Key, Value):- soops:struct_opv(Obj , Key, Value).
get_struct_opv(Kind, Key, Value, Slot):- soops:struct_opv(Kind, Key, Value, Slot).

get_struct_opv_i(Kind, Key, Value, Slot):- soops:struct_opv(Kind, Key, Value, Slot).

push_struct_opv(Kind,Prop,Item,ZLOT):-
   (get_struct_opv(Kind,Prop,List,ZLOT) -> 
    (member(Item,List) -> true ; update_struct_opv4(Kind,Prop,[Item|List],ZLOT));
   assert_struct_opv4(Kind,Prop,[Item],ZLOT)).
     


force_symbol_package(Symbol,Package):- atom(Symbol),f_symbol_package(Symbol,Package),!.
force_symbol_package(_Symbol,[]).

gen_slot_name(Prefix,Type,Key,ZLOT):-
 always((
  Type=..[Kind|Params],
  claz_to_symbol(Kind,Sym),
  intern_slot_name(Prefix,Sym,Key,SlotInfo2),
  ZLOT=..[SlotInfo2|Params])).

intern_slot_name(Prefix,Type,Key,SlotInfo2):-   
  notrace((to_prolog_string_anyways(Type,ClassName),force_symbol_package(Type,Pack1),
  to_prolog_string_anyways(Key,KeyName),force_symbol_package(Key,Pack2),
  choose_package(Pack1,Pack2,Package),
  %(Package\==Pack2-> KeyNameUsed = Key ; KeyNameUsed=KeyName),
  (atom_concat(_,'-',ClassName)->ClassNameDash=ClassName;atom_concat(ClassName,'-',ClassNameDash)),
  atomics_to_string([Prefix,ClassNameDash,KeyName],String))),
  string_upper(String,StringUC),
  f_intern(StringUC,Package,SlotInfo2),!.
  

package_not_for_slots([]).
package_not_for_slots(pkg_cl).
package_not_for_slots(pkg_kw).

choose_package(_,Pack2,Pack2):- \+ package_not_for_slots(Pack2),!.
choose_package(Pack1,_,Pack1):- \+ package_not_for_slots(Pack1),!.
choose_package(_,_,pkg_sys).

/*
%o_p_v(hash_table_znst_12,
["SYS", [
        ['$S'([logical_pathname,kw_host,"SYS",kw_device,kw_unspecific,kw_directory,[kw_relative],kw_name,kw_wild,kw_type,"LISP",kw_version,[]]),"*.lisp"],
        ['$S'([logical_pathname,kw_host,"SYS",kw_device,kw_unspecific,kw_directory,[kw_relative],kw_name,kw_wild,kw_type,[],kw_version,[]]),"*"],
        ['$S'([logical_pathname,kw_host,"SYS",kw_device,kw_unspecific,kw_directory,[kw_absolute],kw_name,kw_wild,kw_type,[],kw_version,[]]),"\/ *"]
        ]).
%o_p_v(sys_xx_logical_pathname_translations_xx,symbol_value,['#S',['HASH-TABLE','TEST','EQUALP',
% ['SYS',[['#S',['LOGICAL-PATHNAME','HOST','SYS','DEVICE','UNSPECIFIC','DIRECTORY',['RELATIVE'],'NAME','WILD','TYPE','LISP','VERSION',[]]],'*.lisp'],
         [['#S',['LOGICAL-PATHNAME','HOST','SYS','DEVICE','UNSPECIFIC','DIRECTORY',['RELATIVE'],'NAME','WILD','TYPE',[],'VERSION',[]]],*],
         [['#S',['LOGICAL-PATHNAME','HOST','SYS','DEVICE','UNSPECIFIC','DIRECTORY',['ABSOLUTE'],'NAME','WILD','TYPE',[],'VERSION',[]]],'\/ *']]]]).
*/

un_kw(Prop,Prop). 

% not part of #'equalp
personal_props(debug_name).
personal_props(ref).
personal_props(sys_initialized).


wl:init_args(x,sys_get_iprops).
wl:interned_eval('`sys:get-iprops').
f_sys_get_iprops(Obj,Result):- nonvar(Obj),findall([Prop|Value],get_opv_i(Obj,Prop,Value),ResultL),list_to_set(ResultL,Result).
wl:init_args(x,sys_get_opv).
wl:interned_eval('`sys:get-opv').
f_sys_get_opv(Obj,Prop,Value):- get_opv(Obj,Prop,Value).
wl:init_args(x,sys_set_opv).
wl:interned_eval('`sys:set-opv').
f_sys_set_opv(Obj,Prop,Value,R):- set_opv(Obj,Prop,Value),R=Obj.

f_sys_to_pvs(X,[float|XX]):- notrace(catch(XX is (1.0 * X),_,fail)),!.
f_sys_to_pvs(X,XX):- findall([P|V],((get_opv_i(X,P,V),\+ personal_props(P))),List),
  List\==[],sort(List,XX),!.
f_sys_to_pvs(X,[str|XX]):- format(string(S),'~w',[X]),string_upper(S,XX),!.




/*
get_opv(Obj,Prop,RealValue):- get_opv(Obj,Prop,Value),
  ensure_awakened(Value,RealValue),
  (Value==RealValue->true;set_opv(Obj,Prop,RealValue)).


ensure_awakened(Value,RealValue):- \+ atom(Value),!,Value=RealValue.
ensure_awakened(Value,RealValue):- !, Value=RealValue.
ensure_awakened(Value,RealValue):- notrace(nb_current(Value,RealValue)),!.
ensure_awakened(Value,RealValue):- soops:o_p_v(Value,sys_initialized,_),
  f_sys_get_iprops(Value,KeyProps), KeyProps\==[],!,
   always((forall(member([K|V],KeyProps),set_opv(Value,K,V)),
   trace,nb_current(Value,RealValue))).
ensure_awakened(Value,RealValue):- Value=RealValue.

*/

is_refp(Value):-  atom(Value),notrace(nb_current(Value,_)),!.
is_objp(Value):-  compound(Value),functor(Value,'$OBJ',2).
%is_immediate(Value):-  \+ is_refp(Value), \+ is_objp(Value).


ref:attr_unify_hook(Same,Var):- get_attr(Var,ref,SameO)->Same==SameO;var(Var).

get_opv(Obj,Prop,Values):- no_repeats((Obj-Prop),get_opv_i(Obj,Prop,Values)).

get_kind_ref(KindObj,Kind,Obj):- var(KindObj),!,Obj=KindObj,Kind=_.
get_kind_ref(KindObj,Kind,Obj):- compound_deref(KindObj,Real),!,get_kind_ref(Real,Kind,Obj).
get_kind_ref('$OBJ'(Kind,_,Obj),Kind,Obj):- !.
get_kind_ref('$OBJ'(Kind,Obj),Kind,Obj):- !.
get_kind_ref(Obj,Kind,Obj):-  type_or_class_nameof(Obj,Kind),!.

compound_deref(C,_):- \+ compound(C),!,fail.
compound_deref('$OBJ'(claz_reference,B),B):- atom(B).


%get_opv_i(Obj,Prop,Value):- get_opv_iiii(Obj,Prop,Value).
get_opv_i(Obj,Prop,Value):- attvar(Obj),!,nonvar(Prop),get_attr(Obj,Prop,Value).
get_opv_i(Sym,Prop,Value):- atom(Sym),is_keywordp(Sym),!,get_type_default(keyword,Sym,Prop,Value).
get_opv_i(KindObj,Prop,Value):- get_kind_ref(KindObj,Kind,Obj),get_opv_ii(Kind,Obj,Prop,Value).

get_opv_ii(_Kind,Obj,Prop,Value):- quietly(get_opv_iiii(Obj,Prop,Value)).
get_opv_ii(Kind,Obj,Prop,Type):- type_of == Prop,!,(i_type(Obj,Type)->true;Kind=Type).
get_opv_ii(symbol,Obj,Prop,Value):- nonvar(Obj),wl:quietly((symbol_has_prop_getter(Obj,Prop,Getter),call(Getter,Obj,Prop,Value))).

get_opv_ii(Kind,Obj,Prop,Values):-
   kind_attribute_pred(Kind,Prop,Pred),
   modulize(call(Pred,Obj,Value),OPred),
   predicate_property(OPred,dynamic),
   findall(Value,OPred,Values).
get_opv_ii(_Kind,Obj,Prop,Value):- get_opv_iiii(Obj,Prop,Value).
get_opv_ii(_Kind,_,Prop,_):- var(Prop),!,fail.
get_opv_ii(_Kind,_,Prop,_):- not_shareble_prop(Prop),!,fail.
get_opv_ii(Kind,Obj,Prop,Value):- nonvar(Kind),get_kind_or_supers_slot_name(Kind,Prop,Where)->Prop\==Where,!, get_opv_iiii(Obj,Where,Value).
get_opv_ii(Kind,Obj,Prop,Value):- notrace(is_prop_class_alloc(Kind,Prop,Where))->Where\==Obj,!,get_opv_iiii(Where,Prop,Value).


get_opv_iii(symbol,Obj,Prop,Value):- nonvar(Obj),wl:symbol_has_prop_getter(Obj,Prop,Getter),call(Getter,Obj,Prop,Value).
get_opv_iii(_Kind,Obj,Prop,Value):- get_opv_iiii(Obj,Prop,Value).

get_opv_iiii(Obj,Prop,Value):- current_prolog_flag(wamcl_gvars,true),(atom(Obj);var(Obj)),nb_current(Obj,Ref),nb_current_value(Ref,Prop,Value).
get_opv_iiii(Obj,Prop,Value):- soops:o_p_v(Obj,Prop,Value).
get_opv_iiii(Obj,Prop,Value):- soops:struct_opv(Obj,Prop,Value).

not_shareble_prop(Prop):-notrace((nonvar(Prop),not_shareble_prop0(Prop))).
not_shareble_prop0(type_of).
not_shareble_prop0(symbol_value).
not_shareble_prop0(conc_name).

                                                                         

get_type_default(keyword,Name,symbol_name,Out):- atom(Name), string_concat(kw_,Str,Name),string_upper(Str,Out).
get_type_default(keyword,_,symbol_package,pkg_keyword).
get_type_default(keyword,_,defined_as,defconstant).
get_type_default(keyword,_,type_of,keyword).
get_type_default(Kind,Obj,Prop,Value):- is_prop_class_alloc(Kind,Prop,Where),Obj\==Where,get_opv_iii(Kind,Where,Prop,Value).

%get_opv_pred(Obj,Prop,Value):- get_obj_pred(Obj,Prop,Pred), call(Pred,Obj,Value).
%get_opv_pred(Obj,Prop,Value):- fail,fail,fail,fail,fail,fail,fail, get_obj_prefix(Obj,Prefix),atom_concat_or_rtrace(Prefix,DashKey,Prop),atom_concat_or_rtrace('_',Key,DashKey),!,get_opv_i(Kind,Obj,Key,Value).
  

set_ref_object(Ref,Object):- quietly(nb_setval(Ref,Object)),!.
release_ref_object(Ref):- dbginfo(release_ref_object(Ref)),quietly(nb_setval(Ref,[])),!.
has_ref_object(Ref,Object):- nb_current(Ref,Object),Object\==[].
get_ref_object(Ref,Object):- has_ref_object(Ref,Object),!.
get_ref_object(Ref,Object):- always(atom(Ref)), 
   %oo_empty(Object0),
   %put_attr(Object0,type_of,ref),
   nb_put_attr(Object0,ref,Ref),
   always(nb_setval(Ref,Object0)),
   always(nb_current(Ref,Object)),!.

/*
set_ref_object(Ref,Object):- quietly(nb_set_value(?(Ref),pointer,Object)),!.
release_ref_object(Ref):- dbginfo(release_ref_object(Ref)),quietly(nb_set_value(?(Ref),pointer,[])),!.
has_ref_object(Ref,Object):- nb_current_value(?(Ref),pointer,Object),Object\=[],!.
get_ref_object(Ref,Object):- nb_current_value(?(Ref),pointer,Object),Object\=[],!.
get_ref_object(Ref,Object):- 
   oo_empty(Object0),
   oo_put_attr(Object0,type_of,ref),
   oo_put_attr(Object0,ref,Ref),
   always(nb_set_value(?(Ref),pointer,Object0)),!,
   always(nb_current_value(?(Ref),pointer,Object)),!.
*/


type_prop_prefix(Class,Prefix):- get_opv(Class,conc_name,Prefix),!.
type_prop_prefix(Class,Prefix):- claz_to_symbol(Class,Prefix),!.

%get_obj_prefix(Obj,Kind):- f_type_of(Obj,Kind),!.


instance_prefix(I,Obj):- instance_prefix0(I,Obj),!.
instance_prefix(I,Obj):- instance_prefix1(I,Obj), \+ instance_prefix0(I,Obj).

instance_prefix0(claz_structure_class, claz_).
instance_prefix0(claz_structure_object, claz_).
instance_prefix0(claz_standard_class, claz_).
instance_prefix0(claz_package, pkg_).
instance_prefix0(claz_keyword, kw_).

instance_prefix1(Kind, Prefix):- claz_to_symbol(Kind, Prefix).

f_class_name(C,S):- claz_to_symbol(C,S).

claz_to_symbol(C,S):- claz_to_symbol0(C,S)*->true;claz_to_symbol1(C,S).

claz_to_symbol0(C,S):- get_struct_opv(C,class_name,S).
%claz_to_symbol0(C,S):- get_struct_opv(C,name,S), \+ string(S).
claz_to_symbol0(claz_symbol,symbol).
claz_to_symbol0(claz_package,package).
claz_to_symbol0(claz_number,number).
claz_to_symbol0(C,S):- get_struct_opv(C,type,S).


claz_to_symbol1(Class,Sym):-atom(Class),atom_concat_or_rtrace('claz_',Sym,Class).
claz_to_symbol1(Class,Sym):-Class=Sym.




builtin_slot(Kind,Prop):-notrace((nonvar(Prop),builtin_slot0(Kind,Prop))).
builtin_slot0(_,type_of).
builtin_slot0(_,sys_initialized).
builtin_slot0(_,debug_name).
builtin_slot0(claz_package,_).
builtin_slot0(claz_standard_class,_).
builtin_slot0(claz_t,_).
/*
builtin_slot0(claz_symbol,symbol_name).
builtin_slot0(claz_symbol,symbol_package).
builtin_slot0(claz_symbol,symbol_value).
builtin_slot0(claz_symbol,symbol_function).
*/
builtin_slot0(_,symbol_name).
builtin_slot0(_,symbol_package).
builtin_slot0(_,symbol_value).
builtin_slot0(_,symbol_function).
builtin_slot0(_,symbol_macro).
builtin_slot0(_,symbol_plist).
%builtin_slot0(_,compile_as).







add_opv_maybe(Obj,Prop,_):- get_opv_i(Obj,Prop,_),!.
add_opv_maybe(Obj,Prop,Value):- add_opv_new(Obj,Prop,Value),!.

update_opv(Obj,Prop,Value):- set_opv(Obj,Prop,Value).
set_opv(Obj,Prop,Value):- 
  retractall(soops:o_p_v(Obj,Prop,_)),
  /*delete_opvalues(Obj,Prop),*/ 
   add_opv_new(Obj,Prop,Value).


add_opv(Obj,Prop,Value):- add_opv_new(Obj,Prop,Value),!.
add_opv_new_iii(Obj,Prop,Value):- add_opv_new(Obj,Prop,Value),!.

add_opv_new(Obj,Prop,V):- ( \+ atomic(V)),is_stringp(V),to_prolog_string_if_needed(V,V0),!,show_call_trace(add_opv_new(Obj,Prop,V0)).
%is_obj_alloc(Obj,Prop,Where):- type_or_class_nameof(Obj,Kind), (is_prop_class_alloc(Kind,Prop,Where)*->true;Where=Obj). 
%add_opv_new(Obj,u_x,V):- notrace(wdmsg(add_opv_new(Obj,u_x,V))), break.
add_opv_new(Obj,Prop,V):- notrace(is_list(Obj)), wdmsg(add_opv_new(Obj,Prop,V)), break.
add_opv_new(Obj,Prop,Value):- always(type_or_class_nameof(Obj,Kind)),!, add_opv_new_ii(Kind,Obj,Prop,Value),!.  


add_opv_new_ii(Kind,Obj,Prop,Value):- 
   get_opv_ii(Kind,Obj,Prop,OldValue),Value==OldValue,!.
add_opv_new_ii(Kind,Obj,Prop,Value):- (is_prop_class_alloc(Kind,Prop,Where) -> Obj\==Where), !,add_opv_new(Where,Prop,Value).
add_opv_new_ii(claz_symbol,Obj,Prop,Value):- nonvar(Obj), forall(wl:symbol_has_prop_setter(Obj,Prop,Setter),once(call(Setter,Obj,Prop,Value))),fail.
add_opv_new_ii(Kind,Obj,Prop,Val):- 
   once((kind_attribute_pred(Kind,Prop,Pred),
   modulize(call(Pred,Obj,Val),OPred),
   predicate_property(OPred,dynamic),
   show_call_trace(assert_lsp(OPred)))),fail.
add_opv_new_ii(Kind,Obj,Prop,Value):-  builtin_slot(Kind,Prop),!,add_opv_new_iiii(Obj,Prop,Value).
add_opv_new_ii(Kind, Obj,Prop,Value):- get_kind_or_supers_slot_name(Kind,Prop,Where) -> Where\==Prop,!, add_opv_new(Obj,Where,Value).
add_opv_new_ii(_Kind,Obj,Prop,Value):- add_opv_new_iiii(Obj,Prop,Value).
%add_opv_new_i(Obj,Prop,Value):- Prop==value, nonvar(Obj),nb_setval(Obj,Value).


% u_daft_point_znst_1,u_daft_point_znst_2,u_daft_point_z


add_opv_new_iiii(Obj,Prop,Value):- assertion(ground(o_p_v(Obj,Prop,Value))),fail.
add_opv_new_iiii(Ref,u_daft_point_z,_Value):- Ref\==u_daft_point_znst_metaobject_0,!,break.
% add_opv_new_iiii(Obj,Prop,Value):- get_opv_iiii(Obj,Prop,OldValue),Value==OldValue,!.
add_opv_new_iiii(Ref,Prop,Value):-current_prolog_flag(wamcl_gvars,true),!, always(get_ref_object(Ref,Object)),!,   
   %show_call_trace
   (always(nb_put_attr(Object,Prop,Value))).
add_opv_new_iiii(Obj,Prop,Value):- % show_call_trace
   ((atom(Obj),(atom_concat(sys_,_,Obj);atom_concat(os_,_,Obj)))->true;wdmsg(assert_lsp(o_p_v(Obj,Prop,Value)))),
   assert_lsp(Obj,soops:o_p_v(Obj,Prop,Value)).

%delete_opvalues(Obj,Key):- Key == value, nb_delete(Obj),fail.
delete_opvalues(Obj,Prop):- 
 always(\+ is_list(Obj);Obj==[]),
   type_or_class_nameof(Obj,Kind),
   ignore(forall(retract(soops:o_p_v(Obj,Prop,_)),true)),
   ignore((     
   kind_attribute_pred(Kind,Prop,Pred),
   modulize(call(Pred,Obj,_),OPred),predicate_property(OPred,dynamic),   
   forall(clause(OPred,true,Ref),erase(Ref)))).

%get_obj_prefix(Obj,Prefix):- quietly(((type_or_class_nameof(Obj,Class),!,type_prop_prefix(Class,Prefix)))).


delete_obj(Obj):- 
   obj_properties(Obj,Props),!,
   maplist(delete_opvalues(Obj),Props).
delete_obj(Obj):- 
   always(\+ is_list(Obj);Obj==[]),
   ignore(forall(retract(soops:o_p_v(Obj,_,_)),true)).


obj_properties(Obj,Props):- 
   findall(Prop,get_opv_i(Obj,Prop,_),Props).

modulize(call(Pred,Obj,Val),OPred):- IPred=..[Pred,Obj,Val],!,modulize(IPred,OPred).
modulize(O:Pred,O:Pred):-!.
modulize(Pred,M:Pred):-predicate_property(Pred,imported_from(M)),!.
modulize(Pred,M:Pred):-predicate_property(Pred,module(M)),!.
modulize(Pred,Pred).


wl:symbol_has_prop_set_get(sys_xx_global_env_var_xx,claz_environment, set_global_env, global_env).
wl:symbol_has_prop_set_get(sys_xx_env_var_xx,claz_environment, set_current_env, current_env).

wl:symbol_has_prop_getter(Sym,symbol_value,prolog_direct(Getter/1)):- wl:symbol_has_prop_set_get(Sym,_,_Setter,Getter).
wl:symbol_has_prop_setter(Sym,symbol_value,prolog_direct(Setter/1)):- wl:symbol_has_prop_set_get(Sym,_,Setter,_Getter).
%wl:symbol_has_prop_getter(sys_xx_stderr_xx,symbol_value,prolog_direct(set_error/1)).
%wl:symbol_has_prop_setter(sys_xx_stderr_xx,symbol_value,prolog_direct(current_error/1)).

prolog_direct(Pred/1,_Obj,_Prop,Value):- call(Pred,Value).
prolog_direct(Pred/2,Obj,_Prop,Value):- call(Pred,Obj,Value).
prolog_direct(Pred/3,Obj,Prop,Value):- call(Pred,Obj,Prop,Value).
   

:- dynamic(cache:is_kind_innited/1).

ensure_opv_type_inited(Kind):- cache:is_kind_innited(Kind),!.
ensure_opv_type_inited(Kind):- 
  asserta(cache:is_kind_innited(Kind)),!,
  get_deftype(Kind,DefType),
  findall(Slot,soops:struct_opv(Kind,kw_name,Slot,_),Slots),add_class_slots(DefType,Kind,1,Slots).

get_deftype(Kind,DefType):- (is_structure_classp(Kind) -> DefType=defstruct; DefType=defclass).

add_class_slots(DefType,Kind,N,[Slot|Slots]):- !, 
 always((add_slot_def(DefType,N,Kind,Slot),N1 is N + 1,
  add_class_slots(DefType,Kind,N1,Slots))).
add_class_slots(_DefType,_Type,_N,[]).

list_oddp(Keys):- always(length(Keys,Len)), is_oddp(Len).

add_slot_def(_DefType,N,Kind,Prop):- atom(Prop),!,add_slot_def_props(N,Kind,Prop,[]).

add_slot_def(defstruct,N,Kind,[Prop,Default|Keys]):-  
   add_slot_def_props(N,Kind,Prop,[kw_initform,Default|Keys]).

add_slot_def(_Defclass,N,Kind,[Prop,Default|Keys]):-  \+ list_oddp(Keys),
   add_slot_def_props(N,Kind,Prop,[Default|Keys]).
add_slot_def(_DefType,N,Kind,[Prop|Keys]):- add_slot_def_props(N,Kind,Prop,Keys).

add_slot_def_props(N,Kind,SlotSym,MoreInfo):-
   always((gen_slot_name('',Kind,SlotSym,ZLOT),
     assert_struct_opv4(Kind,kw_name,SlotSym,ZLOT), 
   to_prolog_string_anyways(SlotSym,SName),

   create_keyword(SName,KW),assert_struct_opv4(Kind,sys_initargs,[KW],ZLOT),

      struct_opv_else(Kind,kw_conc_name,ConcatName,
        (get_struct_opv(Kind,class_name,KName),to_prolog_string_anyways(KName,KSName),string_concat(KSName,"-",ConcatName),
              assert_struct_opv(Kind,kw_conc_name,ConcatName))),
      
       
      %atom_concat_or_rtrace(ConcatName,SName,GetterName),
      %ignore((nonvar(N),(assert_struct_opv4(Kind,kw_offset,N,ZLOT)))),
      %f_intern(GetterName,Getter),

   %claz_to_symbol(Kind,ClassSymbol),f_symbol_package(ClassSymbol,Package),trace,intern_symbol(SName,Package,Name,_),
   %assert_struct_opv4(Kind,name,Name,ZLOT),
   ignore((nonvar(N),(assert_struct_opv4(Kind,kw_offset,N,ZLOT)))),
   ignore((kind_attribute_pred(Kind,SlotSym,Pred),assert_struct_opv4(Kind,kw_accessor_predicate,Pred,ZLOT))),
   add_slot_more_info(SlotSym,Kind,ZLOT,MoreInfo))).

is_slot_name(KW):- \+ is_list(KW).

add_slot_more_info(_SlotKW,_Kind,_SlotInfo,[]):-!.
add_slot_more_info(_SlotKW,_Kind,_SlotInfo,[[]]):-!.
add_slot_more_info(SlotName,Kind,ZLOT,[KW,Value|MoreInfo]):- is_slot_name(KW),
   assert_slot_prop(SlotName,Kind,KW,Value,ZLOT),!,
   add_slot_more_info(SlotName,Kind,ZLOT,MoreInfo).

add_slot_more_info(SlotName,Kind,ZLOT,[[Default,KW,Value]]):- is_slot_name(KW),
   assert_slot_prop(SlotName,Kind,kw_initform,Default,ZLOT),!,
   assert_slot_prop(SlotName,Kind,KW,Value,ZLOT),!.
   

add_slot_more_info(SlotName,Kind,ZLOT,[[KW,Value]|MoreInfo]):- is_slot_name(KW),
   assert_slot_prop(SlotName,Kind,KW,Value,ZLOT),!,
   add_slot_more_info(SlotName,Kind,ZLOT,MoreInfo).

add_slot_more_info(SlotName,Kind,ZLOT,[[Value]]):-
   assert_slot_prop(SlotName,Kind,kw_initform,Value,ZLOT),!.

assert_slot_prop(_SlotName,Kind,Prop,Value,ZLOT):-
  assert_struct_opv4(Kind,Prop,Value,ZLOT).



prop_to_name(X,S):-string(X),!,X=S.
prop_to_name(Prop,Upper):- to_prolog_string_if_needed(Prop,F),!,prop_to_name(F,Upper).
prop_to_name(Prop,Upper):- to_prolog_string_anyways(Prop,Upper),!.
prop_to_name(Prop,Upper):- claz_to_symbol(Prop,Key),
 atomic_list_concat(List,'_',Key),atomic_list_concat(List,'-',Lower),string_upper(Lower,Upper).

get_opv_else(Obj,Prop,Value,Else):- get_opv(Obj,Prop,Value)*->true;Else.


:- dynamic(wl:type_attribute_pred_dyn/3).

%decl_mapped_opv(_,_):-!.
decl_mapped_opv(Kind,Maps):- is_list(Maps),!,maplist(decl_mapped_opv(Kind),Maps).
decl_mapped_opv(Kind,Prop=Pred):-
  assertz(wl:interned_eval(call(assert_lsp(wl:type_attribute_pred_dyn(Kind,Prop,Pred))))),
  nop(modulize(call(Pred,Obj,Val),OPred)),
  nop(assertz(wl:interned_eval(call(forall(OPred,add_opv_new(Obj,Prop,Val)))))),
  nop(assert_lsp((OPred:- (is_kind(Obj,Kind),(fail->!;true),get_opv(Obj,Prop,Val))))).

is_kind(O,_K):- nonvar(O).

kind_attribute_pred(Kind,Prop,Pred):- wl:type_attribute_pred_dyn(Kind,Prop,Pred).


%type_attribute_pred0(Kind,Prop,Pred):- .
%type_attribute_pred0(Kind,Prop,Pred):- nonvar(Prop),gen_slot_name('',Kind,Prop,Pred),functor(Pred,F,A),AA is A +2,current_predicate(F/AA).






:- discontiguous soops:struct_opv/3.
:- discontiguous soops:struct_opv/4.
:- dynamic((soops:struct_opv/3)).
:- dynamic((soops:struct_opv/4)).
:- multifile((soops:struct_opv/3)).
:- multifile((soops:struct_opv/4)).
:- soops:ensure_loaded('ci.data').
cleanup_mop:-  
 ignore((get_struct_opv(X,kw_include,claz_object),get_struct_opv(X,kw_include,Y),Y\==claz_object,show_call_trace(retract(soops:struct_opv(X,kw_include,claz_object))),fail)),
 ignore((get_struct_opv(X,kw_include,claz_t),get_struct_opv(X,kw_include,Y),Y\==claz_t,show_call_trace(retract(soops:struct_opv(X,kw_include,claz_t))),fail)).

save_mop:- cleanup_mop,tell('ci3.data'),
 forall(member(Assert,[struct_opv(_,P,_),struct_opv(_,P,_,_)]),
   forall(soops:Assert,
      ignore((P\==slot1,P\==has_slots,format('~q.~n',[Assert]))))), told.
:- style_check(-discontiguous).

make_soops_old:- cleanup_mop,tell('si2.data'),
   forall(member(Assert,[o_p_v(_,_,_)]),
     forall(clause(soops:Assert,true),
        ignore((P\==slot1,P\==has_slots,format('~q.~n',[Assert]))))), told.

cleanup_opv0:-
  doall(retract(soops:o_p_v(Obj,compile_as,kw_function))),
  doall((
   get_opv_iiii(Obj,compile_as,kw_special),
   get_opv_iiii(Obj,function,Was),
   atom_concat('m',Was,WillBe),
   assert_if_new(soops:o_p_v(Obj,symbol_function,WillBe)),
   retract(soops:o_p_v(Obj,function,Was)),
   retract(soops:o_p_v(Obj,compile_as,kw_special)),
   doall(retract(soops:o_p_v(Obj,compile_as,kw_special))))).

cleanup_opv:-
  doall((
   get_opv_iiii(Was,type_of,macro),
   atom_concat('m',Was,WillBe),
   assert_if_new(soops:o_p_v(WillBe,type_of,macro)),
   doall(retract(soops:o_p_v(Was,type_of,macro))))).



save_syms:-
  make_soops_old,
  save_soops,
  save_pi.
  
save_soops:-   
  tell('si3.data'),
     forall(get_opv_iiii(Obj,Prop,Value),
       once(write_o_p_v(Obj,Prop,Value))),
     told.

write_o_p_v(_,_,Value):- var(Value).
write_o_p_v(_,extra_info_proclaimed,[]).
write_o_p_v(_,extra_info_deftype,[]).
write_o_p_v(_,extra_info,[]).
write_o_p_v(Obj,doc_deftype,[String,Def]):-write_o_p_v(Obj,doc_deftype,String),write_o_p_v(Obj,result_deftype,Def).
write_o_p_v(_,ref,_).
write_o_p_v(Obj,extra_info(_),List):-!,maplist(write_o_p_t(Obj),List).
write_o_p_v(Obj,result_type(ecl2),WAS):- get_opv_iiii(Obj,result_type(sbcl),WAS).
write_o_p_v(Obj,lambda_list(ecl2),WAS):- get_opv_iiii(Obj,lambda_list(sbcl),WAS).
write_o_p_v(Obj,result_type(ecl2),number):- write_o_p_v(Obj,result_type(sbcl),sys_irrational).

write_o_p_v(Obj,lambda_list(sbcl),WAS):- write_o_p_v(Obj,lambda_list,WAS).
write_o_p_v(Obj,result_type(sbcl),WAS):- write_o_p_v(Obj,result_type,WAS).

write_o_p_v(Obj,Prop,Value):- format('~q.~n',[o_p_v(Obj,Prop,Value)]).
write_o_p_t(Obj,Prop):- format('~q.~n',[o_p_v(Obj,Prop,t)]).

:- multifile o_p_v/3.
:- dynamic o_p_v/3.
:- multifile c_p_v/3.
:- dynamic c_p_v/3.

load_si:-
  open('si.data',read,Stream),
  repeat,
    read_term(Stream,Value,[]),
    (Value==end_of_file->!;
      (load_si_value(Value),fail)).
load_si_value(Value):- assert_lsp(Value).

process_si:- 
   ensure_loaded(packages),
   doall((
    clause(soops:o_p_v(X,Y,Z),true,Ref),
    process_si(soops:o_p_v(X,Y,Z)),
    erase(Ref))).
   
%process_si(soops:o_p_v(X,Y,Z)):- Y==symbol_value, show_call_trace(nb_setval(X,Z)).
process_si(soops:o_p_v(X,Y,Z)):- X\==[], set_opv(X,Y,Z).

:- if(true).
:- soops:ensure_loaded('si.data').
:- else.
:- load_si.
:- endif.

%:- include('si2.data').

:- fixup_exports.




