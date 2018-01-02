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
cl_make_instance([Name|Slots],Obj):- always(create_object(Name,Slots,Obj)).

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
  new_partly_named_opv(SKind,Name,Attrs,Obj),
  set_opv(Obj,name,Name).


new_partly_named_opv(SKind,Name,Attrs,Obj):-
  new_partly_named_opv_pt1(SKind,Name,Attrs,Obj,Kind),
  new_partly_named_opv_pt2(SKind,Name,Attrs,Obj,Kind).

new_partly_named_opv_pt1(SKind,Name,_Attrs,Obj,Kind):-
  always((
  (var(Kind)->find_class(SKind,Kind);true),
  (var(Obj)->
    (instance_prefix(Kind,Pre),!,atomic_list_concat([Pre,Name],'_',PName),
                prologcase_name(PName,Obj),to_prolog_string_anyways(Name,SName),
                add_opv_new(Obj,classof,Kind),
                set_opv(Obj,sname,SName));add_opv_new(Obj,classof,Kind)))).

new_partly_named_opv_pt2(_SKind,_Name,Attrs,Obj,Kind):-
  always((     
   init_instance_slots(Kind,1,Obj,Attrs),
   call_init_slot_props(Kind,Obj))).  

call_init_slot_props(Kind,Obj):- get_opv_else(Obj,instance,Kinds,Kinds=[])->
  (member(Kind,Kinds)->!;(set_opv(Obj,instance,[Kind|Kinds]),fail)).
call_init_slot_props(Kind,Obj):-
 always((
  forall(get_kind_supers(Kind,Sup),call_init_slot_props(Sup,Obj)),
  ensure_opv_type_inited(Kind),
  forall(get_struct_opv(Kind,kw_initarg,From,SlotInfo),
    (get_struct_opv(Kind,slotfullname,Into,SlotInfo),
     (get_opv_iii(Obj,Into,_)-> true ; 
        (get_opv_else(Obj,From,Value,Value=[]),set_opv(Obj,Into,Value))))),
   forall(get_struct_opv(Kind,kw_initform,Value,SlotInfo),
     (get_struct_opv(Kind,slotfullname,Into,SlotInfo),
      ( % get_opv_iii(Obj,Name,_)-> true ; 
         (cl_eval(Value,Result),set_opv(Obj,Into,Result))))))).


when_must(True,Then):- True->always(Then);true.

init_instance_slots(Kind,_Ord,Obj,PProps):-always(init_instance_kv(Kind,Obj,PProps)),!.
%init_instance_slots(Kind,Ord,Obj,PProps):-init_slot_props_iv(Kind,Ord,Obj,PProps),!.


init_slot_props_iv(_,_N,_Obj,[]):-!.
init_slot_props_iv(Kind,N,Obj,[Value|Props]):-
  add_i_opv(Kind,Obj,N,Value), N2 is N + 1,
  init_instance_kv(Kind,N2,Obj,Props).

add_i_opv(Kind,Obj,N,Value):- get_struct_opv(Kind,ordinal,N,SlotInfo),
  get_struct_opv(Kind,slotname,Key,SlotInfo),
   add_opv(Obj,Key,Value).

f_clos_pf_set_slot_value(Obj,Key,Value,Value):- set_slot(Obj,Key,Value).
set_slot(Obj,Key,Value):- get_object_slot_name(Obj,Key,SlotName), set_opv(Obj,SlotName,Value).


get_object_slot_name(Obj,Key,SlotName):- 
  type_or_class_nameof(Obj,Kind),
    get_kind_slot(Kind,Key,SlotName).
    

get_kind_slot(Kind,Key,SlotName):- 
  no_repeats(get_kind_supers(Kind,Sup)),
  get_slot_name(Sup,Key,SlotName).

get_kind_supers(Kind,Sup):- get_kind_supers2(Kind,[],Sup).
get_kind_supers2(Kind,ExceptFor,Sup):-find_class(Kind,KSup),!,get_kind_supers3(KSup,ExceptFor,Sup).
get_kind_supers3(Kind,ExceptFor,_Sup):- member(Kind,ExceptFor),!,fail.
get_kind_supers3(Kind,_,Kind).
get_kind_supers3(Kind,ExceptFor,Sup):-get_struct_opv(Kind,type,E),get_kind_supers2(E,[Kind|ExceptFor],Sup).
get_kind_supers3(Kind,ExceptFor,Sup):-get_struct_opv(Kind,kw_include,List),member(E,List),get_kind_supers2(E,[Kind|ExceptFor],Sup).

get_slot_name(Kind,Key,SlotName):- quietly(get_slot_name0(Kind,Key,SlotName)).
get_slot_name0(Kind,Key,SlotName):-
   (get_struct_opv(Kind,slotfullname,Key,_)*-> Key=SlotName;
   (((get_struct_opv(Kind,slotname,Key,SlotInfo), get_struct_opv(Kind,slotfullname,SlotName,SlotInfo))) *-> true;
   (get_struct_opv(Kind,keyword,Key,SlotInfo),get_struct_opv(Kind,slotfullname,SlotName,SlotInfo)))).

is_zlot_key(SlotInfo,SlotName):-
   get_struct_opv(_Kind,slotfullname,SlotName,SlotInfo);
   get_struct_opv(_Kind,slotname,SlotName,SlotInfo);
   get_struct_opv(_Kind,keyword,SlotName,SlotInfo).


slot_is_nonlist(kw_type).
slot_is_nonlist(kw_instance).

init_instance_kv(_,_,[]).
init_instance_kv(Kind,Obj,[Key,Value|Props]):-  always(is_keywordp(Key)),
  add_opv_new(Obj,Key,Value),!,  
  init_instance_kv(Kind,Obj,Props).
/*
init_instance_kv(Kind,Obj,[[Key|List]|Props]):- is_keywordp(Key),
   slot_is_nonlist(Key),
   get_slot_name(Kind,Key,SlotName),
   maplist(add_opv_new(Obj,SlotName),List),
  (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
   init_instance_kv(Kind,Obj,Props).

init_instance_kv(Kind,Obj,[KV|Props]):- compound(KV),get_kv(KV,Key,Value),
 (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
  get_slot_name(Kind,Key,SlotName),
  add_opv_new(Obj,SlotName,Value),!,  
  init_instance_kv(Kind,Obj,Props).

init_instance_kv(Kind,Obj,[Value|Props]):-
  type_slot_number(Kind,Key),
  get_slot_name(Kind,Key,SlotName),
  add_opv_new(Obj,SlotName,Value),!,
  Ord2 is Ord+1,
  init_instance_kv(Kind,Obj,Props).
*/

type_slot_number(Kind,Key,Ordinal):-
   get_struct_opv(Kind,slotname,Key,SlotInfo),   
   get_struct_opv(Kind,ordinal,Ordinal,SlotInfo).


/*
name_value_default(m(_,array_of(Kind),Name),Name-mut([],array_of(Kind))).
name_value_default(m(_,prolog_array_list(Kind),Name),Name-mut([],array_of(Kind))).
name_value_default(m(_,Kind,Name),Name-Def):-value_default(Kind,Def).
name_value_default(m(_,Kind,Name),Name-mut(@(null),Kind)).
name_value_default(N-Value,N-Value).
*/

value_default(claz_prolog_concurrent_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_prolog_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_list,[]).
value_default(integer,0).
value_default(claz_object,mut([],claz_object)).

%value_default(claz_simple_string, @(null)).
%value_default(claz_string, @(null)).
%value_default(prolog_array_list(_),[]).
%value_default(array_of(_),[]).


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


find_or_create_class(Name,Kind):- find_class(Name,Kind),Kind\==[],!.
find_or_create_class(Name,Kind):- 
   %to_prolog_string_anyways(Name,SName),
   new_named_opv(claz_structure_object,Name,[],Kind),!.

find_class(Name,Claz):- (var(Name) -> lisp_dump_break ; true) , 
  get_struct_opv(Claz,symbolname,Name),!.
%find_class(Name,Claz):- get_struct_opv(Claz,name,Name),!.
find_class(Name,Claz):-
  to_prolog_string_anyways(Name,StringC)->string_upper(StringC,NameS),
  get_struct_opv(Claz,kw_namestring,NameS).
find_class(Name,Claz):- atom(Name),atom_concat_or_rtrace('claz_',_,Name),!,Claz=Name.


cl_find_class(Name,Claz):- always(is_symbolp(Name)),once((find_class(Name,Claz),claz_to_symbol(Claz,Sym),Name==Sym)).
cl_find_class(_,[]).
  

cl_slot_exists_p(Obj,Slot,Value):- t_or_nil(get_opv(Obj,Slot,_),Value).

cl_slot_value(Obj,Slot,Value):- always(get_opv(Obj,Slot,Value)).

cl_defstruct([[Name,KeyWords]|Slots],Name):- !, always(define_struct(Name,KeyWords,Slots,_Kind)).
cl_defstruct([[Name|KeyWords]|Slots],Name):- !, always(define_struct(Name,KeyWords,Slots,_Kind)).
cl_defstruct([Name|Slots],Name):- always(define_struct(Name,[],Slots,_Kind)).

cl_defclass([Name,Supers,Slots|KwInfo],Kind):- !, always(define_class(Name,[[kw_include|Supers]|KwInfo],Slots,Kind)).


define_class(Name,KeyWords,SlotsIn,Kind):- 
  (var(Kind) -> (( new_named_opv(claz_standard_class,Name,[],Kind)));true),
   define_kind(defclass,Name,KeyWords,SlotsIn,Kind),
   ensure_metaobject(Kind,_),
   ensure_prototype(Kind,_).


is_prop_class_alloc(Kind,SlotName,Where):- \+ not_shareble_prop(SlotName),
  get_struct_opv(Kind,kw_allocation,kw_class,ZLOT),
  is_zlot_key(ZLOT,SlotName),
  ensure_metaobject(Kind,Where).

% @TODO Store INITIALIZE-INSTANCE, REINITIALIZE-INSTANCE, and SHARED-INITIALIZE  Hooks
ensure_prototype(Kind,Instance):- get_struct_opv(Kind,prototype,Instance),!.
ensure_prototype(Kind,Instance):- make_proto_instance(Kind,Instance),assert_struct_opv(Kind,prototype,Instance).

make_proto_instance(Kind,Obj):-
  new_partly_named_opv_pt1(_SKind,zinst_prototypical_0,[],Obj,Kind).

ensure_metaobject(Kind,Instance):- get_struct_opv(Kind,metaobject,Instance),!.
ensure_metaobject(Kind,Instance):- make_metaobject_instance(Kind,Instance),assert_struct_opv(Kind,metaobject,Instance).

make_metaobject_instance(Kind,Obj):-
  new_partly_named_opv_pt1(_SKind,zinst_metaobject_0,[],Obj,Kind).



define_struct(Name,KeyWords,SlotsIn,Kind):- 
  (var(Kind) -> (( new_named_opv(claz_structure_object,Name,[],Kind)));true),
   define_kind(defstruct,Name,KeyWords,SlotsIn,Kind).

is_structure_class(T):- get_opv(T,classof,claz_structure_class);get_opv(T,classof,claz_structure_object).


define_kind(DefType,Name,KeyWords,SlotsIn,Kind):- 
 always((
  assert_struct_opv(Kind,symbolname,Name),
  assert_struct_opv(Kind,type,Name),  
  % add doc for string
  maybe_get_docs('class',Name,SlotsIn,Slots,Code),
  always(Code),  
  add_class_keywords(Kind,KeyWords),
  get_struct_offset(Kind,Offset),
  NOffset is Offset +1,  
  add_class_slots(DefType,Kind,NOffset,Slots),  
  generate_missing_struct_functions(Kind))).

get_struct_offset(Kind,W):- get_struct_opv(Kind,initial_offset,W).
get_struct_offset(_,0).

generate_missing_struct_functions(Kind):-
  always(( get_struct_opv(Kind,symbolname,Name),
  to_prolog_string_anyways(Name,SName),
 % define keyword defaults now
 make_default_constructor(Kind,Code),
 always(Code),
 maybe_add_kw_function(Kind,SName,"-P",kw_predicate, [obj],( eq('class-of'(obj),quote(Kind)))),
 % make accessors
 struct_opv_else(Kind,kw_conc_name,ConcatName,(string_concat(SName,"-",ConcatName),
    assert_struct_opv(Kind,kw_conc_name,ConcatName))),
 forall(get_struct_opv(Kind,slotfullname,_,ZLOT),
   maybe_add_get_set_functions(Kind,ZLOT)))),
   forall(get_struct_opv(Kind,kw_reader,ReaderSymbol,ZLOT),
     always((get_struct_opv(Kind,slotfullname,GetterSym,ZLOT),
      maybe_add_slot_accessor_function(ReaderSymbol,GetterSym)))),
 forall(get_struct_opv(Kind,kw_accessor,ReaderSymbol,ZLOT),
   always((get_struct_opv(Kind,slotfullname,GetterSym,ZLOT),
     maybe_add_slot_accessor_function(ReaderSymbol,GetterSym)))).

maybe_add_slot_accessor_function(ReaderSymbol,GetterSym):-
  foc_operator(_Ctx,_Env,ReaderSymbol,_, ProposedName),
  Head=..[ProposedName,Obj,Value],
  Body= get_opv_ii(Obj,GetterSym,Value),
  PrologCode =
    (assert_lsp(ReaderSymbol, (Head:-Body)),
     set_opv(ReaderSymbol,function,ProposedName),
     set_opv(ReaderSymbol,compiled_as,kw_function),
     set_opv(ReaderSymbol,lambda_list,[object])),
  cmpout(PrologCode),!.
maybe_add_slot_accessor_function(ReaderSymbol,GetterSym):-
  show_call_debug(maybe_add_function(ReaderSymbol,[obj],['slot-value',obj,[quote,GetterSym]],_Added1)).

  %Added\==[]-> assert_struct_opv4(Kind,getter_fn,Added,ZLOT) ; true )

/*
(defclass animal ()
  ((legs :reader leg-count :initarg :legs)
   (comes-from :reader comes-from :initarg :comes-from)))
*/
make_default_constructor(Kind,Code):- 
 always((
 get_struct_opv(Kind,symbolname,Name),to_prolog_string_anyways(Name,SName),
 atom_concat_or_rtrace("MAKE-",SName,FnName),
 reader_intern_symbols(FnName,Sym),
 foc_operator(_,_,Sym,3,Function), 
 Head=..[Function,List,Obj],
 Body=..[cl_make_instance,[Kind|List],Obj],
 Code = (assert_lsp(wl:init_args(0,Function)),
         set_opv(Function,classof,claz_compiled_function),
         set_opv(Sym,compile_as,kw_function),
         set_opv(Sym,function,Function),
         assert_lsp((user:Head:-Body))))).
 

maybe_add_get_set_functions(Kind,ZLOT):- 
   (\+ get_struct_opv(Kind,read_only,t,ZLOT) ->
     maybe_add_set_function(Kind,ZLOT) ; true),
   maybe_add_get_function(Kind,ZLOT).

maybe_add_get_function(Kind,ZLOT):- get_struct_opv(Kind,getter_fn,_Getter,ZLOT),!.
maybe_add_get_function(Kind,ZLOT):-   
  always(( get_struct_opv(Kind,slotfullname,GetterSym,ZLOT), maybe_add_slot_accessor_function(GetterSym,GetterSym))).

maybe_add_set_function(Kind,ZLOT):- get_struct_opv(Kind,setter_fn,_Setter,ZLOT),!.
maybe_add_set_function(Kind,ZLOT):-   
  always((
  get_struct_opv(Kind,slotfullname,SetterSym,ZLOT),
  to_prolog_string_anyways(SetterSym,SetterSymName),
  atomic_list_concat(['SETF',SetterSymName],'-',Setter),cl_intern(Setter,SetterFn),
  maybe_add_function(SetterFn,[obj,val],['clos_pf_set_slot_value',obj,[quote,SetterSym],val],Added),
  ( Added\==[]-> assert_struct_opv4(Kind,setter_fn,Added,ZLOT) ; true ))).
/*
f_u_setf_point_z(A, B, G) :-
        D=[bv(u_obj, A), bv(u_val, B)|C],
        global_env(C),
        catch(( ( get_var(D, u_obj, E),
                  get_var(D, u_val, F),
                  set_opv(E, u_z, F)
                ),
                F=G
              ),
              block_exit(u_setf_point_z, G),
              true).
*/

f_clos_class_direct_superclasses(C,SL):-findall(S,struct_opv(C,include,S),SL).

f_clos_class_precedence_list(C,SL):- struct_opv(C,super_priority,SL),!.
f_clos_class_precedence_list(C,SL):- f_clos_class_direct_superclasses(C,List1),maplist(f_clos_class_direct_superclasses,List1,List2),
   append(List1,List2,List3),list_to_set(List3,SL).
   

% catch accidental unification that destroys metaclasses
classof:attr_unify_hook(A,B):- trace,wdmsg(classof:attr_unify_hook(A,B)),lisp_dump_break. %  break.


maybe_add_kw_function(Kind,L,R,Key,ArgList,LispBody):- 
   (get_struct_opv(Kind,Key, FnName) -> true;
     atom_concat_or_rtrace(L,R,FnName)),
   maybe_add_function(FnName,ArgList,LispBody,_).

maybe_add_function(FnName,ArgList,LispBody,R):-
   reader_intern_symbols(FnName,Sym),
   (is_fboundp(Sym)->R=Sym;
     (R=Result,as_sexp(LispBody,SLispBody),
       reader_intern_symbols(pkg_user,[defun,FnName,ArgList,[progn,SLispBody]],LispInterned),
         lisp_compile(Result,LispInterned,PrologCode),always(PrologCode))).


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

%assert_struct_opv4(_Obj, keyword, _Value, _Info):- trace,fail.

assert_struct_opv4(Obj, Key, Value,Info):-
  show_call_trace(assertz_new(soops:struct_opv(Obj, Key, Value,Info))).

get_struct_opv(Obj, Key, String):- soops:struct_opv(Obj , Key, String).


get_struct_opv(Obj, Key, Value, Info):- soops:struct_opv(Obj, Key, Value, Info).


get_szlot(Prefix,Type,Key,SlotInfo):-
  Type=..[Kind|Params],
  claz_to_symbol(Kind,Sym),
  to_prolog_string_anyways(Sym,ClassName),
  atomic_list_concat([ClassName,Key],'_',SlotInfo0),
  atom_concat_or_rtrace(Prefix,SlotInfo0,SlotInfo1),
  prologcase_name(SlotInfo1,SlotInfo2),
  SlotInfo=..[SlotInfo2|Params].
   
/*
%o_p_v(hash_table_znst_12,
["SYS", [
        ['$S'([logical_pathname,kw_host,"SYS",kw_device,kw_unspecific,kw_directory,[kw_relative],kw_name,kw_wild,kw_type,"LISP",kw_version,[]]),"*.lisp"],
        ['$S'([logical_pathname,kw_host,"SYS",kw_device,kw_unspecific,kw_directory,[kw_relative],kw_name,kw_wild,kw_type,[],kw_version,[]]),"*"],
        ['$S'([logical_pathname,kw_host,"SYS",kw_device,kw_unspecific,kw_directory,[kw_absolute],kw_name,kw_wild,kw_type,[],kw_version,[]]),"\/ *"]
        ]).
%o_p_v(sys_xx_logical_pathname_translations_xx,value,['#S',['HASH-TABLE','TEST','EQUALP',
% ['SYS',[['#S',['LOGICAL-PATHNAME','HOST','SYS','DEVICE','UNSPECIFIC','DIRECTORY',['RELATIVE'],'NAME','WILD','TYPE','LISP','VERSION',[]]],'*.lisp'],
         [['#S',['LOGICAL-PATHNAME','HOST','SYS','DEVICE','UNSPECIFIC','DIRECTORY',['RELATIVE'],'NAME','WILD','TYPE',[],'VERSION',[]]],*],
         [['#S',['LOGICAL-PATHNAME','HOST','SYS','DEVICE','UNSPECIFIC','DIRECTORY',['ABSOLUTE'],'NAME','WILD','TYPE',[],'VERSION',[]]],'\/ *']]]]).
*/
un_kw(Prop,Prop):-!. 
/*
 =(Key,Prop):- notrace(un_kw1(Key,Prop)).


un_kw1(Prop,Prop):- var(Prop),!.
un_kw1(Key,Prop):- \+ atomic(Key),!,Key=Prop.
un_kw1([],[]):-!.
un_kw1(Key,Key):-!.
%un_kw1(Key,Prop):-string(Key),Key=Prop,!.
un_kw1(Key,Prop):- soops:struct_opv(K, accessor, Key, Slot),
  (soops:struct_opv(K, keyword, KW, Slot);
    soops:struct_opv(K, slotname, KW, Slot)),!,=(KW,Prop).

un_kw1(Key,Prop):- \+ atomic_list_concat([_,_|_],'_',Key),!,Prop=Key.
un_kw1(Key,Prop):- Prop\==name,to_prolog_string_anyways(Key,Str),prologcase_name(Str,Prop),!.
un_kw1(Key,Prop):- atom_concat_or_rtrace('kw_',Prop,Key),lisp_dump_break,!.
un_kw1(Key,Prop):- atom_concat_or_rtrace(':',Prop,Key),lisp_dump_break,!.
un_kw1(Prop,Prop).
*/

(wl:init_args(x,f_sys_get_iprops)).
wl:interned_eval('`sys:get-iprops').
f_sys_get_iprops(Obj,Result):- findall([Prop|Value],get_opv(Obj,Prop,Value),Result).
(wl:init_args(x,f_sys_get_opv)).
wl:interned_eval('`sys:get-opv').
f_sys_get_opv(Obj,Prop,Value):- get_opv(Obj,Prop,Value).
wl:interned_eval('`sys:set-opv').
f_sys_set_opv(Obj,Prop,Value,R):- set_opv(Obj,Prop,Value),R=Obj.

f_u_to_pvs(X,[float|XX]):- notrace(catch(XX is (1.0 * X),_,fail)),!.
f_u_to_pvs(X,XX):- findall([P|V],((get_opv_ii(X,P,V),\+ personal_props(P))),List),
  List\==[],sort(List,XX),!.
f_u_to_pvs(X,[str|XX]):- format(string(S),'~w',[X]),string_upper(S,XX),!.

   personal_props(sname).
   personal_props(ref).
   personal_props(classof).
   personal_props(instance).



add_opv_maybe(Obj,Prop,_):- get_opv_i(Obj,Prop,_),!.
add_opv_maybe(Obj,Prop,Value):- add_opv_new(Obj,Prop,Value),!.

/*
get_opv(Obj,Prop,RealValue):- get_opv(Obj,Prop,Value),
  ensure_awakened(Value,RealValue),
  (Value==RealValue->true;set_opv(Obj,Prop,RealValue)).


ensure_awakened(Value,RealValue):- \+ atom(Value),!,Value=RealValue.
ensure_awakened(Value,RealValue):- !, Value=RealValue.
ensure_awakened(Value,RealValue):- notrace(nb_current(Value,RealValue)),!.
ensure_awakened(Value,RealValue):- soops:o_p_v(Value,instance,_),
  f_sys_get_iprops(Value,KeyProps), KeyProps\==[],!,
   always((forall(member([K|V],KeyProps),set_opv(Value,K,V)),
   trace,nb_current(Value,RealValue))).
ensure_awakened(Value,RealValue):- Value=RealValue.

*/

is_refp(Value):-  atom(Value),notrace(nb_current(Value,_)),!.
is_objp(Value):-  compound(Value),functor(Value,'$OBJ',2).
%is_immediate(Value):-  \+ is_refp(Value), \+ is_objp(Value).


ref:attr_unify_hook(Same,Var):- get_attr(Var,ref,SameO)->Same==SameO;var(Var).
    

get_opv(Obj,Prop,Value):- no_repeats((Obj-Prop),get_opv_i(Obj,Prop,Value)).

get_opv_i(Obj,Prop,Value):- attvar(Obj),!,nonvar(Prop),get_attr(Obj,Prop,Value).
get_opv_i(Obj,Prop,Value):- compound_deref(Obj,Real),!,get_opv_ii(Real,Prop,Value).
get_opv_i(Obj,_,_):- \+ atom(Obj), \+ var(Obj),!,fail.
get_opv_i(Obj,Prop,Value):- get_opv_ii(Obj,Prop,Value).

get_opv_ii(Obj,Prop,Value):- get_opv_iii(Obj,Prop,Value) *-> true; get_opv_fallback(Obj,Prop,Value).

get_opv_iii(Obj,Prop,Value):- nonvar(Obj),wl:symbol_has_prop_getter(Obj,Prop,Getter),call(Getter,Obj,Prop,Value).
get_opv_iii(Obj,Prop,Value):- get_opv_iiii(Obj,Prop,Value).
get_opv_iii(Obj,Prop,Value):- soops:struct_opv(Obj,Prop,Value).

get_opv_iiii(Obj,Prop,Value):- (atom(Obj);var(Obj)),nb_current(Obj,Ref),nb_current_value(Ref,Prop,Value).
get_opv_iiii(Obj,Prop,Value):- soops:o_p_v(Obj,Prop,Value).

not_shareble_prop(Prop):-notrace((nonvar(Prop),not_shareble_prop0(Prop))).
not_shareble_prop0(classof).
not_shareble_prop0(typeof).
not_shareble_prop0(value).
not_shareble_prop0(conc_name).

get_opv_fallback(Sym,Prop,Value):- nonvar(Sym),is_keywordp(Sym),!,get_type_default(keyword,Sym,Prop,Value).
get_opv_fallback(Obj,Prop,Symbol):- typeof == Prop,!,get_opv_iii(Obj,classof,Value),!,claz_to_symbol(Value,Symbol).
get_opv_fallback(_,Prop,_):- (var(Prop) ; not_shareble_prop(Prop)),!,fail.
get_opv_fallback(Obj,Prop,Value):- get_opv_pred(Obj,Prop,Value),!.
get_opv_fallback(Obj,Prop,Value):- notrace(type_or_class_nameof(Obj,Kind)),
    ((notrace(is_prop_class_alloc(Kind,Prop,Where)->Where\==Obj))-> 
      get_opv_iii(Where,Prop,Value);
     (get_kind_slot(Kind,Prop,Where) -> (Where\==Prop, get_opv(Obj,Where,Value)))).

                                                                         

get_type_default(keyword,Name,name,Out):- atom(Name), string_concat(kw_,Str,Name),string_upper(Str,Out).
get_type_default(keyword,_,package,pkg_keyword).
get_type_default(keyword,_,classof,clz_keyword).
get_type_default(keyword,_,defined_as,defconstant).
get_type_default(keyword,_,typeof,keyword).
get_type_default(Type,_,Prop,Value):- is_prop_class_alloc(Type,Prop,Where),!,get_opv_iii(Where,Prop,Value).


compound_deref(C,_):- \+ compound(C),!,fail.
compound_deref('$OBJ'(claz_reference,B),B):- atom(B).

get_opv_pred(Obj,Prop,Value):- get_obj_pred(Obj,Prop,Pred), call(Pred,Obj,Value).
get_opv_pred(Obj,Prop,Value):- fail,fail,fail,fail,fail,fail,fail, get_obj_prefix(Obj,Prefix),atom_concat_or_rtrace(Prefix,DashKey,Prop),atom_concat_or_rtrace('_',Key,DashKey),!,get_opv_i(Obj,Key,Value).
  

set_ref_object(Ref,Object):- quietly(nb_setval(Ref,Object)),!.
release_ref_object(Ref):- dbginfo(release_ref_object(Ref)),quietly(nb_setval(Ref,[])),!.
has_ref_object(Ref,Object):- nb_current(Ref,Object),Object\==[].
get_ref_object(Ref,Object):- has_ref_object(Ref,Object).
get_ref_object(Ref,Object):- always(atom(Ref)), 
   %oo_empty(Object0),
   %put_attr(Object0,classof,claz_ref),
   nb_put_attr(Object0,ref,Ref),
   always(nb_setval(Ref,Object0)),!,
   always(b_getval(Ref,Object)),!.

/*
set_ref_object(Ref,Object):- quietly(nb_set_value(?(Ref),pointer,Object)),!.
release_ref_object(Ref):- dbginfo(release_ref_object(Ref)),quietly(nb_set_value(?(Ref),pointer,[])),!.
has_ref_object(Ref,Object):- nb_current_value(?(Ref),pointer,Object),Object\=[],!.
get_ref_object(Ref,Object):- nb_current_value(?(Ref),pointer,Object),Object\=[],!.
get_ref_object(Ref,Object):- 
   oo_empty(Object0),
   oo_put_attr(Object0,classof,claz_ref),
   oo_put_attr(Object0,ref,Ref),
   always(nb_set_value(?(Ref),pointer,Object0)),!,
   always(nb_current_value(?(Ref),pointer,Object)),!.
*/


get_obj_prefix(Obj,Prefix):- quietly(((type_or_class_nameof(Obj,Class),!,type_prop_prefix(Class,Prefix)))).

type_prop_prefix(Class,Prefix):- get_opv(Class,conc_name,Prefix),!.
type_prop_prefix(Class,Prefix):- claz_to_symbol(Class,Prefix),!.

%type_or_class_nameof(Obj,Kind):- get_opv_iii(Obj,classof,Class),!,claz_to_symbol(Class,Kind).
%type_or_class_nameof(Obj,Kind):- type_or_class_nameof(Obj,Class),claz_to_symbol(Class,Kind).

%type_or_class_nameof(Obj,Class):- type_or_class_nameof(Obj,Class).

get_obj_pred(Obj,Prop,Pred):- type_or_class_nameof(Obj,Kind),kind_attribute_pred(Kind,Prop,Pred).

%get_obj_prefix(Obj,Kind):- cl_type_of(Obj,Kind),!.


instance_prefix(I,Obj):- instance_prefix0(I,Obj),!.
instance_prefix(I,Obj):- instance_prefix1(I,Obj), \+ instance_prefix0(I,Obj).

instance_prefix0(claz_structure_class, claz_).
instance_prefix0(claz_structure_object, claz_).
instance_prefix0(claz_standard_class, claz_).
instance_prefix0(claz_package, pkg_).
instance_prefix0(claz_keyword, kw_).

instance_prefix1(Kind, Prefix):- claz_to_symbol(Kind, Prefix).

cl_class_name(C,S):- claz_to_symbol(C,S).

claz_to_symbol(C,S):- claz_to_symbol0(C,S)*->true;claz_to_symbol1(C,S).

claz_to_symbol0(C,S):- get_struct_opv(C,symbolname,S).
%claz_to_symbol0(C,S):- get_struct_opv(C,name,S), \+ string(S).
claz_to_symbol0(C,S):- get_struct_opv(C,type,S).
claz_to_symbol0(claz_symbol,symbol).
claz_to_symbol0(claz_package,package).
claz_to_symbol0(claz_number,number).


claz_to_symbol1(Class,Sym):-atom(Class),atom_concat_or_rtrace('claz_',Sym,Class).
claz_to_symbol1(Class,Sym):-Class=Sym.


add_opv(Obj,Prop,Value):- add_opv_new(Obj,Prop,Value),!.


% add_opv_pred(MPred,Obj,Key,Value):- strip_module(MPred,M,Pred),Assertion=.. [Pred,Obj,Key,Value], ( \+ M:Assertion -> assertz(M:Assertion) ; true).

% add_opv_new(Sym,value,SValue):- atom(SValue),(atom_contains(SValue,'(');atom_contains(SValue,' ')),(as_sexp(SValue,Value)->SValue\==Value),!,set_opv(Sym,value,Value).
% add_opv_new(Obj,Prop,Value):-  add_opv_new(Obj,Prop,Value).


builtin_slot(Prop):-notrace((nonvar(Prop),builtin_slot0(Prop))).
builtin_slot0(classof).
builtin_slot0(typeof).
builtin_slot0(function).
builtin_slot0(value).
builtin_slot0(compile_as).


add_opv_new(Obj,Prop,V):- ( \+ atomic(V)),is_stringp(V),to_prolog_string_if_needed(V,V0),!,show_call_trace(add_opv_new(Obj,Prop,V0)).
%is_obj_alloc(Obj,Prop,Where):- type_or_class_nameof(Obj,Kind), (is_prop_class_alloc(Kind,Prop,Where)*->true;Where=Obj). 
add_opv_new(Obj,u_x,V):- notrace(wdmsg(add_opv_new(Obj,u_x,V))), break.
add_opv_new(Obj,Prop,V):- notrace((\+ (\+ is_list(Obj);Obj==[]), wdmsg(add_opv_new(Obj,Prop,V)))), break.
add_opv_new(Obj,Prop,Value):- builtin_slot(Prop),!,add_opv_new_iii(Obj,Prop,Value).
add_opv_new(Obj,Prop,Value):- always(type_or_class_nameof(Obj,Kind)),!, add_opv_new_ii(Kind,Obj,Prop,Value),!.  


add_opv_new_ii(Kind,Obj,Prop,Value):- is_prop_class_alloc(Kind,Prop,Where)->Obj\==Where,!,add_opv_new(Where,Prop,Value).
add_opv_new_ii(_Kind,Obj,Prop,Value):- nonvar(Obj), forall(wl:symbol_has_prop_setter(Obj,Prop,Setter),once(call(Setter,Obj,Prop,Value))),fail.
add_opv_new_ii(Kind, Obj,Prop,Value):- get_kind_slot(Kind,Prop,Where) -> Where\==Prop,!, add_opv_new(Obj,Where,Value).
add_opv_new_ii(_Kind,Obj,Prop,Value):- add_opv_new_iii(Obj,Prop,Value).
%add_opv_new_i(Obj,Prop,Value):- Prop==value, nonvar(Obj),nb_setval(Obj,Value).

add_opv_new_iii(Ref,Prop,Value):- always(get_ref_object(Ref,Object)),!,
   retractall(soops:o_p_v(Ref,Prop,_)),
   %show_call_trace
   (always(nb_put_attr(Object,Prop,Value))).
/*
add_opv_new_ii(Obj,Prop,Val):-  fail,
   get_obj_pred(Obj,Prop,Pred),
   modulize(call(Pred,Obj,Val),OPred),
   predicate_property(OPred,dynamic),
   assert_lsp(OPred),!.
   */
%add_opv_new_i(Obj,Prop,Value):-show_call_trace(assert_lsp(soops:o_p_v(Obj,Prop,Value))).

%delete_opvalues(Obj,Key):- Key == value, nb_delete(Obj),fail.
delete_opvalues(Obj,Prop):- 
 always(\+ is_list(Obj);Obj==[]),
   ignore(forall(retract(soops:o_p_v(Obj,Prop,_)),true)),
   ignore((
     get_obj_prefix(Obj,Kind),
   kind_attribute_pred(Kind,Prop,Pred),
   modulize(call(Pred,Obj,_),OPred),predicate_property(OPred,dynamic),   
   forall(clause(OPred,true,Ref),erase(Ref)))).

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

wl:symbol_has_prop_getter(Sym,value,prolog_direct(Getter/1)):- wl:symbol_has_prop_set_get(Sym,_,_Setter,Getter).
wl:symbol_has_prop_setter(Sym,value,prolog_direct(Setter/1)):- wl:symbol_has_prop_set_get(Sym,_,Setter,_Getter).
%wl:symbol_has_prop_getter(sys_xx_stderr_xx,value,prolog_direct(set_error/1)).
%wl:symbol_has_prop_setter(sys_xx_stderr_xx,value,prolog_direct(current_error/1)).

prolog_direct(Pred/1,_Obj,_Prop,Value):- call(Pred,Value).
prolog_direct(Pred/2,Obj,_Prop,Value):- call(Pred,Obj,Value).
prolog_direct(Pred/3,Obj,Prop,Value):- call(Pred,Obj,Prop,Value).
   
update_opv(Obj,Prop,Value):- set_opv(Obj,Prop,Value).
set_opv(Obj,Prop,Value):- /*delete_opvalues(Obj,Prop),*/ add_opv_new(Obj,Prop,Value).



:- dynamic(cache:is_kind_innited/1).

ensure_opv_type_inited(Kind):- cache:is_kind_innited(Kind),!.
ensure_opv_type_inited(Kind):- 
  asserta(cache:is_kind_innited(Kind)),!,
  get_deftype(Kind,DefType),
  findall(Slot,soops:struct_opv(Kind,slotname,Slot,_),Slots),add_class_slots(DefType,Kind,1,Slots).

get_deftype(Kind,DefType):- (is_structure_class(Kind) -> DefType=defstruct; DefType=defclass).

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
   always((get_szlot('zlot_',Kind,SlotSym,SlotInfo),
     assert_struct_opv4(Kind,slotname,SlotSym,SlotInfo), 
   to_prolog_string_anyways(SlotSym,SName),

   create_keyword(SName,KW),assert_struct_opv4(Kind,keyword,KW,SlotInfo),

      struct_opv_else(Kind,kw_conc_name,ConcatName,
        (get_struct_opv(Kind,symbolname,KName),to_prolog_string_anyways(KName,KSName),string_concat(KSName,"-",ConcatName),
              assert_struct_opv(Kind,kw_conc_name,ConcatName))),
      
       
      atom_concat_or_rtrace(ConcatName,SName,GetterName),cl_intern(GetterName,Getter),

   assert_struct_opv4(Kind,slotfullname,Getter,SlotInfo),
   %claz_to_symbol(Kind,ClassSymbol),cl_symbol_package(ClassSymbol,Package),trace,intern_symbol(SName,Package,Name,_),
   %assert_struct_opv4(Kind,name,Name,SlotInfo),
   ignore((nonvar(N),(assert_struct_opv4(Kind,ordinal,N,SlotInfo)))),
   ignore((kind_attribute_pred(Kind,SlotSym,Pred),assert_struct_opv4(Kind,accessor_predicate,Pred,SlotInfo))),
   add_slot_more_info(SlotSym,Kind,SlotInfo,MoreInfo))).

is_slot_name(KW):- \+ is_list(KW).

add_slot_more_info(_SlotKW,_Kind,_SlotInfo,[]):-!.
add_slot_more_info(_SlotKW,_Kind,_SlotInfo,[[]]):-!.
add_slot_more_info(SlotName,Kind,SlotInfo,[KW,Value|MoreInfo]):- is_slot_name(KW),
   assert_slot_prop(SlotName,Kind,KW,Value,SlotInfo),!,
   add_slot_more_info(SlotName,Kind,SlotInfo,MoreInfo).

add_slot_more_info(SlotName,Kind,SlotInfo,[[Default,KW,Value]]):- is_slot_name(KW),
   assert_slot_prop(SlotName,Kind,kw_initform,Default,SlotInfo),!,
   assert_slot_prop(SlotName,Kind,KW,Value,SlotInfo),!.
   

add_slot_more_info(SlotName,Kind,SlotInfo,[[KW,Value]|MoreInfo]):- is_slot_name(KW),
   assert_slot_prop(SlotName,Kind,KW,Value,SlotInfo),!,
   add_slot_more_info(SlotName,Kind,SlotInfo,MoreInfo).

add_slot_more_info(SlotName,Kind,SlotInfo,[[Value]]):-
   assert_slot_prop(SlotName,Kind,kw_initarg,Value,SlotInfo),!.

assert_slot_prop(_SlotName,Kind,Prop,Value,SlotInfo):-
   /*(Prop == kw_accessor -> (assert_struct_opv4(Kind,slotfullname,SlotName,SlotInfo),
     show_call_trace(maybe_add_function(Value,[obj],['slot-value',obj,[quote,SlotName]],_Added))) ; true),
   (Prop == kw_reader -> (assert_struct_opv4(Kind,slotfullname,SlotName,SlotInfo),
     show_call_trace(maybe_add_function(Value,[obj],['slot-value',obj,[quote,SlotName]],_Added2))) ; true),*/
  assert_struct_opv4(Kind,Prop,Value,SlotInfo).



prop_to_name(X,S):-string(X),!,X=S.
prop_to_name(Prop,Upper):- to_prolog_string_if_needed(Prop,F),!,prop_to_name(F,Upper).
prop_to_name(Prop,Upper):- to_prolog_string_anyways(Prop,Upper),!.
prop_to_name(Prop,Upper):- claz_to_symbol(Prop,Key),
 atomic_list_concat(List,'_',Key),atomic_list_concat(List,'-',Lower),string_upper(Lower,Upper).

get_opv_else(Obj,Prop,Value,Else):- get_opv(Obj,Prop,Value)*->true;Else.


:- dynamic(type_attribute_pred_dyn/3).

decl_mapped_opv(_,_):-!.
decl_mapped_opv(Kind,Maps):- is_list(Maps),!,maplist(decl_mapped_opv(Kind),Maps).
decl_mapped_opv(Kind,KW=Pred):-  =(KW,Prop),
  show_call_trace(assert_lsp(type_attribute_pred_dyn(Kind,Prop,Pred))),
  modulize(call(Pred,Obj,Val),OPred),
  forall(OPred,add_opv_new(Obj,Prop,Val)),
  nop(assert_lsp((OPred:- (is_kind(Obj,Kind),(fail->!;true),get_opv(Obj,Prop,Val))))).

is_kind(O,_K):- nonvar(O).

kind_attribute_pred(Kind,Prop,Pred):- type_attribute_pred0(Kind,Prop,Pred).


type_attribute_pred0(Kind,Prop,Pred):- type_attribute_pred_dyn(Kind,Prop,Pred).
%type_attribute_pred0(Kind,Prop,Pred):- nonvar(Prop),get_szlot('',Kind,Prop,Pred),functor(Pred,F,A),AA is A +2,current_predicate(F/AA).






:- discontiguous soops:struct_opv/4.
:- dynamic((soops:struct_opv/4)).
:- soops:ensure_loaded('ci.data').
cleanup_mop:-  
 ignore((get_struct_opv(X,include,claz_object),get_struct_opv(X,include,Y),Y\==claz_object,show_call_trace(retract(soops:struct_opv(X,include,claz_object))),fail)),
 ignore((get_struct_opv(X,include,claz_t),get_struct_opv(X,include,Y),Y\==claz_t,show_call_trace(retract(soops:struct_opv(X,include,claz_t))),fail)).

save_mop:- cleanup_mop,tell('ci3.data'),
 forall(member(Assert,[get_struct_opv(_,P,_),get_struct_opv(_,P,_,_),get_struct_opv(_,P,_,_,_)]),
   forall(soops:Assert,
      ignore((P\==slot1,P\==has_slots,format('~q.~n',[Assert]))))), told.
:- style_check(-discontiguous).

make_soops:- cleanup_mop,tell('si2.data'),
   forall(member(Assert,[o_p_v(_,_,_)]),
     forall(clause(soops:Assert,true),
        ignore((P\==slot1,P\==has_slots,format('~q.~n',[Assert]))))), told.

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
   ensure_loaded(package),
   doall((
    clause(soops:o_p_v(X,Y,Z),true,Ref),
    process_si(soops:o_p_v(X,Y,Z)),
    erase(Ref))).
   
%process_si(soops:o_p_v(X,Y,Z)):- Y==value, show_call_trace(nb_setval(X,Z)).
process_si(soops:o_p_v(X,Y,Z)):- X\==[], set_opv(X,Y,Z).

:- if(true).
:- soops:ensure_loaded('si.data').
:- else.
:- load_si.
:- endif.

%:- include('si2.data').

:- fixup_exports.




