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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(soops, []).
:- set_module(class(library)).
:- include('header.pro').


new_cl_fixnum(Init,Obj):-
  create_struct(claz_fixnum,[Init],Obj),!.


show_call_trace(G):- G *-> wdmsg(G); (wdmsg(warn(failed(show_call_trace(G)))),fail).

create_struct1(Kind,[Value],Value):- data_record(Kind,[_]),!.
create_struct1(Kind,ARGS,Obj):-create_struct(Kind,ARGS,Obj),!.
create_struct1(_Type,Value,Value).

find_kind(Name,Kind):- atom(Name),atom_concat('claz_',_,Name),!,Kind=Name.
find_kind(Name,Kind):- cl_string(Name,SName),
  new_named_opv(claz_structure_class,SName,[],Kind),!.

create_struct([Name|Slots],Obj):- !,create_struct(Name,Slots,Obj).
create_struct(TypeARGS,Obj):- compound(TypeARGS),!,compound_name_arguments(TypeARGS,Kind,ARGS),
  create_struct(Kind,ARGS,Obj),!.
create_struct(Kind,Obj):- create_struct(Kind,[],Obj),!.
create_struct(Kind,Attrs,Obj):- 
  gensym('znst_',Name),  
  new_named_opv(Kind,Name,Attrs,Obj).

new_named_opv(SKind,Name,Attrs,Obj):- 
  find_kind(SKind,Kind),
  instance_prefix(Kind,Pre),!,
  cl_string(Name,SName),
  atomic_list_concat([Pre,SName],'_',PName),
  prologcase_name(PName,Obj),
  set_opv(Obj,name,SName),  
  add_opv(Obj,classof,Kind),
  ensure_opv_type_inited(Kind),
  construct_opv(Obj,Kind),
  init_slot_props(Kind,1,Obj,Attrs).
  %maplist(add_missing_opv(Obj,Kind),Attrs).


init_slot_props(_,_,_,[]).
init_slot_props(Kind,Ord,Obj,[Key,Value|Props]):- is_keywordp(Key),
  (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
  add_kw_opv(Obj,Key,Value),  
  init_slot_props(Kind,Ord2,Obj,Props).
init_slot_props(Kind,Ord,Obj,[[Key|List]|Props]):- 
   is_keywordp(Key),!,maplist(add_kw_opv(Obj,Key),List),
  (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
   init_slot_props(Kind,Ord2,Obj,Props).
init_slot_props(Kind,Ord,Obj,[KV|Props]):- get_kv(KV,Key,Value),
  (type_slot_number(Kind,Key,SOrd)->Ord2 is SOrd+1;Ord2 is Ord+1),
  add_kw_opv(Obj,Key,Value),  
  init_slot_props(Kind,Ord2,Obj,Props).
init_slot_props(Kind,Ord,Obj,[Value|Props]):-
  must_or_rtrace(type_slot_number(Kind,Key,Ord)),
  add_kw_opv(Obj,Key,Value),
  Ord2 is Ord+1,
  init_slot_props(Kind,Ord2,Obj,Props).


type_slot_number(Kind,Key,Ordinal):-
   soops:struct_opv(SlotInfo,name,Key),
   soops:struct_opv(SlotInfo,memberof,Kind),
   soops:struct_opv(SlotInfo,ordinal,Ordinal).


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

return_arg_is_first(cl_defstruct).


cl_find_class(Name,Claz):-
  cl_string(Name,StringC),
  string_upper(StringC,NameS),
  struct_opv(Claz,classname,NameS).
  

cl_defstruct(Kind,[[Name|KeyWords]|Slots]):- define_struct(Name,KeyWords,Slots,Kind).
cl_defstruct(Kind,[Name|Slots]):- define_struct(Name,[],Slots,Kind).
  

define_struct(Name,KeyWords,SlotsIn,Kind):-  
  % add doc for string
  maybe_get_docs('class',Name,SlotsIn,Slots,Code),
  call(Code),
  cl_string(Name,SName),  
  new_named_opv(claz_structure_class,SName,[],Kind),
  set_opv(Kind,name,SName),
  add_opv_keywords(Kind,KeyWords),
  add_opv_slots(Kind,1,Slots).

  
   

assert_struct_opv(Obj, KW, String):-
  un_kw(KW,Key),
  show_call_trace(assertz_new(soops:struct_opv(Obj, Key, String))).

assert_struct_opv(Obj, KW, String,Info):-
  un_kw(KW,Key),
  show_call_trace(assertz_new(soops:struct_opv(Obj, Key, String,Info))).

add_opv_keywords(_Struct,[]):-!.
add_opv_keywords(Obj,[kw_include,Class|KeyWords]):- 
   (assert_struct_opv(Obj, subtypep, Class)),
   add_opv_keywords(Obj,KeyWords).
add_opv_keywords(Obj,[kw_conc_name,String|KeyWords]):- 
   (assert_struct_opv(Obj, kw_conc_name, String)),
   add_opv_keywords(Obj,KeyWords).
add_opv_keywords(Obj,[Key,Value|KeyWords]):-atom(Key),
   maplist(add_opv(Obj,Key,Value)),
   add_opv_keywords(Obj,KeyWords).
add_opv_keywords(Obj,[Key,Value|KeyWords]):- % atom(Key),
   maplist(add_opv(Obj,Key,Value)),
   add_opv_keywords(Obj,KeyWords).

:- discontiguous soops:struct_opv/3.
:- discontiguous soops:struct_opv/4.
:- discontiguous soops:struct_opv/5.
:- dynamic((soops:struct_opv/3)).
:- dynamic((soops:struct_opv/4)).
:- dynamic((soops:struct_opv/5)).
:- include('ci.pro').


get_szlot(Prefix,Type,Key,SlotInfo):-
  Type=..[Kind|Params],
  claz_to_kind(Kind,UType),
  un_kw(Key,UKey),  
  atomic_list_concat([UType,UKey],'_',SlotInfo0),
  atom_concat(Prefix,SlotInfo0,SlotInfo1),
  SlotInfo=..[SlotInfo1|Params].
   
  

save_mop:- cleanup_mop,tell('ci.pro'),
 forall(member(Assert,[struct_opv(_,P,_),struct_opv(_,P,_,_),struct_opv(_,P,_,_,_)]),
   forall(soops:Assert,
      ignore((P\==slot1,P\==has_slots,format('~q.~n',[Assert]))))), told.
:- style_check(-discontiguous).

make_soops:- cleanup_mop,tell('si2.pro'),
 forall(member(Assert,[o_p_v(_,_,_)]),
   forall(clause(soops:Assert,true),
      ignore((P\==slot1,P\==has_slots,format('~q.~n',[Assert]))))), told.

:- multifile soops:o_p_v/3.
:- dynamic soops:o_p_v/3.

soops:o_p_v(Symbol,defined_as,defconstant):- is_keywordp(Symbol).
soops:o_p_v(Symbol,typeof,keyword):- is_keywordp(Symbol).
:- include('si.pro').
soops:o_p_v(Symbol,typeof,Kind):- soops:o_p_v(Symbol,classof,Class),
  \+ clause(soops:o_p_v(Symbol,typeof,_AnyType),true),
  claz_to_kind(Class,Kind).
%:- include('si2.pro').
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


un_kw(Key,Prop):- atom_concat(kw_,Prop,Key),!.
un_kw(Key,Prop):- atom_concat(':',Prop,Key),!.
un_kw(Prop,Prop).

add_kw_opv(Obj,Key,V):- un_kw(Key,Prop),add_opv(Obj,Prop,V).

f_u_get_opv(Obj,Result):- findall([Prop|Value],get_opv(Obj,Prop,Value),Result).
f_u_get_opv(Obj,Prop,Value):- get_opv(Obj,Prop,Value).
	
add_opv_maybe(Obj,Prop,_):- get_opv_i(Obj,Prop,_),!.
add_opv_maybe(Obj,Prop,Value):- add_opv(Obj,Prop,Value),!.

get_opv(Obj,_,_):- string(Obj),!,fail.
get_opv(Obj,Prop,Value):- no_repeats(Obj-Prop,get_opv_i(Obj,Prop,Value)).

get_opv_i(Obj, value, Value):- Obj==quote, throw(get_opv_i(quote, value, Value)).
get_opv_i(Obj,Prop,Value):- nonvar(Obj), has_prop_value_getter(Obj,Prop,Getter),!,call(Getter,Obj,Prop,Value).
get_opv_i(Obj,Prop,Value):- soops:o_p_v(Obj,Prop,Value).
get_opv_i(Obj,Prop,Value):- nonvar(Obj),
  notrace((Prop\==classof,Prop\==typeof,Prop\==value,Prop\==conc_name)),
  get_opv_ii(Obj,Prop,Value).

get_opv_ii(Obj,Prop,Value):-
  get_obj_pred(Obj,Prop,Pred),
  call(Pred,Obj,Value).
get_opv_ii(Obj,Prop,Value):- get_obj_prefix(Obj,Prefix),atom_concat(Prefix,DashKey,Prop),atom_concat('_',Key,DashKey),!,
  get_opv_i(Obj,Key,Value).
  

get_obj_prefix(Obj,Prefix):- quietly(((type_or_class_nameof(Obj,Class),!,type_prop_prefix(Class,Prefix)))).

type_prop_prefix(Class,Prefix):- get_opv(Class,conc_name,Prefix),trace,!.
type_prop_prefix(Class,Prefix):- claz_to_kind(Class,Prefix),!.

get_o_kind(Obj,Kind):- get_opv_i(Obj,classof,Class),!,claz_to_kind(Class,Kind).
get_o_kind(Obj,Kind):- type_or_class_nameof(Obj,Class),claz_to_kind(Class,Kind).

get_obj_pred(Obj,Prop,Pred):- get_o_kind(Obj,Kind),kind_attribute_pred(Kind,Prop,Pred).

%get_obj_prefix(Obj,Kind):- cl_type_of(Obj,Kind),!.


instance_prefix(I,Obj):- instance_prefix0(I,Obj).
instance_prefix(I,Obj):- instance_prefix1(I,Obj), \+ instance_prefix0(I,Obj).

instance_prefix0(claz_structure_class, claz_).
instance_prefix0(claz_package, pkg_).
instance_prefix0(claz_keyword, kw_).

instance_prefix1(Kind, Prefix):- claz_to_kind(Kind, Prefix).

claz_to_kind(claz_symbol,symbol).
claz_to_kind(claz_package,package).
claz_to_kind(claz_number,number).
claz_to_kind(Class,Kind):- nonvar(Class),atom_concat('claz_',Kind,Class),!.
claz_to_kind(Kind,Kind).




add_opv(Symbol,value,SValue):- atom(SValue),
 (atom_contains(SValue,'(');atom_contains(SValue,' ')),
  (as_sexp(SValue,Value)->SValue\==Value),!,set_opv(Symbol,value,Value).
add_opv(Obj,Prop,Value):-  add_opv_new(Obj,Prop,Value).


% add_opv_pred(MPred,Obj,Key,Value):- strip_module(MPred,M,Pred),Assertion=.. [Pred,Obj,Key,Value], ( \+ M:Assertion -> assert(M:Assertion) ; true).

add_opv_new(Obj,Prop,V):- 
   get_obj_pred(Obj,Prop,Pred),
   modulize(call(Pred,Obj,V),OPred),predicate_property(OPred,dynamic),
   assert_if_new(OPred),!.
add_opv_new(Obj,Prop,Value):- show_call_trace(assert_if_new(soops:o_p_v(Obj,Prop,Value))).
delete_opvalues(Obj,Prop):- 
   ignore(forall(retract(soops:o_p_v(Obj,Prop,_)),true)),
   ignore((
     get_obj_prefix(Obj,Kind),
   kind_attribute_pred(Kind,Prop,Pred),
   modulize(call(Pred,Obj,_),OPred),predicate_property(OPred,dynamic),   
   forall(clause(OPred,true,Ref),erase(Ref)))).

modulize(call(Pred,Obj,Val),OPred):- IPred=..[Pred,Obj,Val],!,modulize(IPred,OPred).
modulize(O:Pred,O:Pred):-!.
modulize(Pred,M:Pred):-predicate_property(Pred,imported_from(M)),!.
modulize(Pred,M:Pred):-predicate_property(Pred,module(M)),!.
modulize(Pred,Pred).

:- dynamic(symbol_set_get/3).
:- multifile(symbol_set_get/3).
:- dynamic(has_prop_value_setter/3).
:- multifile(has_prop_value_setter/3).
:- dynamic(has_prop_value_getter/3).
:- multifile(has_prop_value_getter/3).

symbol_set_get(sys_xx_stdin_xx,claz_prolog_output_stream,set_input,current_input).

has_prop_value_setter(Symbol,value,prolog_direct(Setter/1)):- symbol_set_get(Symbol,Setter,_Getter).
has_prop_value_setter(sys_xx_stdout_xx,value,prolog_direct(set_output/1)).

has_prop_value_getter(Symbol,value,prolog_direct(Getter/1)):- symbol_set_get(Symbol,_Setter,Getter).
has_prop_value_getter(sys_xx_stdout_xx,value,prolog_direct(current_output/1)).
%has_prop_value_setter(sys_xx_stderr_xx,value,prolog_direct(set_error/1)).
%has_prop_value_getter(sys_xx_stderr_xx,value,prolog_direct(current_error/1)).

prolog_direct(Pred/1,_Obj,_Prop,Value):- call(Pred,Value).
prolog_direct(Pred/2,Obj,_Prop,Value):- call(Pred,Obj,Value).
prolog_direct(Pred/3,Obj,Prop,Value):- call(Pred,Obj,Prop,Value).
   
update_opv(Obj,Prop,Value):- set_opv(Obj,Prop,Value).

set_opv(Obj,Prop,Value):- has_prop_value_setter(Obj,Prop,Setter),!,call(Setter,Obj,Prop,Value).
set_opv(Obj,Prop,Value):- delete_opvalues(Obj,Prop),add_opv(Obj,Prop,Value).

:- dynamic(is_obj_type/1).

ensure_opv_type_inited(Kind):- is_obj_type(Kind),!.
ensure_opv_type_inited(Kind):- 
  asserta(is_obj_type(Kind)),!,
  findall(Slot,soops:struct_opv(Kind,has_slot,Slot),Slots),add_opv_slots(Kind,1,Slots).

add_opv_slots(Kind,N,[Slot|Slots]):- !, add_slot_def(N,Kind,Slot),N1 is N + 1,add_opv_slots(Kind,N1,Slots).
add_opv_slots(_Type,_N,[]).

add_slot_def(Kind,SLOT):- add_slot_def(_,Kind,SLOT).

add_slot_def(N,Kind,Prop):- atom(Prop),!,
   add_slot_def(N,Kind,[Prop,[]]).
add_slot_def(N,Kind,[Prop,Default|MoreInfo]):-
   atom_string(Key,Prop),
   prop_to_name(Prop,Upper),
   get_szlot('zlot_',Kind,Key,SlotInfo),
   (assert_struct_opv(Kind,slot,Key,SlotInfo)), 
   (assert_struct_opv(Kind,name,Upper,SlotInfo)),
   (assert_struct_opv(Kind,default_value,Default,SlotInfo)),
   ignore((nonvar(N),(assert_struct_opv(Kind,ordinal,N,SlotInfo)))),
   ignore((kind_attribute_pred(Kind,Key,Pred),(assert_struct_opv(Kind,predicate,Pred,SlotInfo)))),
   add_slot_more_info(Kind,SlotInfo,MoreInfo).

add_slot_more_info(_Kind,_SlotInfo,[]):-!.
add_slot_more_info(Kind,SlotInfo,[kw_type,DataType|MoreInfo]):-
   assert_struct_opv(Kind,returns,DataType,SlotInfo),
   add_slot_more_info(Kind,SlotInfo,MoreInfo).
add_slot_more_info(Kind,SlotInfo,[kw_read_only,X|MoreInfo]):-
   assert_struct_opv(Kind,readonly,X,SlotInfo),
   add_slot_more_info(Kind,SlotInfo,MoreInfo).

prop_to_name(X,S):-string(X),!,X=S.
prop_to_name(Prop,Upper):- compound(Prop),!,functor(Prop,F,_),prop_to_name(F,Upper).
prop_to_name(Prop,Upper):- cl_string(Prop,Upper),!.
prop_to_name(Prop,Upper):- claz_to_kind(Prop,Key),
 atomic_list_concat(List,'_',Key),atomic_list_concat(List,'-',Lower),string_upper(Lower,Upper).

get_opv_else(Obj,Prop,Value,Else):- get_opv(Obj,Prop,Value)*->true;Else.


:- dynamic(type_attribute_pred_dyn/3).

decl_mapped_opv(Kind,Maps):- is_list(Maps),!,maplist(decl_mapped_opv(Kind),Maps).
decl_mapped_opv(Kind,KW=Pred):- un_kw(KW,Prop),
  show_call_trace(assert_if_new(type_attribute_pred_dyn(Kind,Prop,Pred))),
  modulize(call(Pred,Obj,Val),OPred),
  forall(OPred,add_opv_new(Obj,Prop,Val)).


kind_attribute_pred(Kind,Key,Pred):- (atom(Key)->un_kw(Key,Prop);Prop=Key),type_attribute_pred0(Kind,Prop,Pred).


type_attribute_pred0(Kind,Prop,Pred):- type_attribute_pred_dyn(Kind,Prop,Pred).
type_attribute_pred0(Kind,Prop,Pred):- nonvar(Prop),
    get_szlot('',Kind,Prop,Pred),functor(Pred,F,A),AA is A +2,current_predicate(F/AA).


construct_opv(Obj,Kind):- get_opv(Obj,instance,Kind),!.
construct_opv(Obj,Kind):-
  add_opv(Obj,instance,Kind),
  forall(soops:struct_opv(Kind,subtypep,Super),construct_opv(Obj,Super)).  

/*
add_missing_opv(Obj,Kind,KV):- get_kv(KV,Key,Value), add_missing_opv(Obj,Kind,Key,Value). 

add_missing_opv(Obj,Kind,Key,Value):- 
  get_szlot(Kind,Key,SlotInfo),
  soops:struct_opv(SlotInfo,returnType,DataType),
  create_struct(DataType,Value,VObj),!,
  update_opv(Obj,Key,VObj).
add_missing_opv(Obj,Kind,Key,Value):- 
  cl_type_of(Value,DataType),
  atom_string(Key,SKey),string_lower(SKey,Prop),
  add_slot_def(Kind,slot(DataType,Prop)),
  update_opv(Obj,Key,Value).
*/  


/*

  // Packages.
  public static final Package PACKAGE_CL =
    Packages.createPackage("COMMON-LISP", 2048); // EH 10-10-2010: Actual number = 1014
  public static final Package PACKAGE_CL_USER =
    Packages.createPackage("COMMON-LISP-USER", 1024);
  public static final Package PACKAGE_KEYWORD =
    Packages.createPackage("KEYWORD", 1024);
  public static final Package PACKAGE_SYS =
    Packages.createPackage("SYSTEM", 2048); // EH 10-10-2010: Actual number = 1216
  public static final Package PACKAGE_MOP =
    Packages.createPackage("MOP", 512); // EH 10-10-2010: Actual number = 277
  public static final Package PACKAGE_TPL =
    Packages.createPackage("TOP-LEVEL", 128); // EH 10-10-2010: Actual number = 6
  public static final Package PACKAGE_EXT =
    Packages.createPackage("EXTENSIONS", 256); // EH 10-10-2010: Actual number = 131
  public static final Package PACKAGE_JVM =
    Packages.createPackage("JVM", 2048); // EH 10-10-2010: Actual number = 1518
  public static final Package PACKAGE_LOOP =
    Packages.createPackage("LOOP", 512); // EH 10-10-2010: Actual number = 305
  public static final Package PACKAGE_PROF =
    Packages.createPackage("PROFILER");
  public static final Package PACKAGE_JAVA =
    Packages.createPackage("JAVA");
  public static final Package PACKAGE_LISP =
    Packages.createPackage("LISP");
  public static final Package PACKAGE_THREADS =
    Packages.createPackage("THREADS");
  public static final Package PACKAGE_FORMAT =
    Packages.createPackage("FORMAT");
  public static final Package PACKAGE_XP =
    Packages.createPackage("XP");
  public static final Package PACKAGE_PRECOMPILER =
    Packages.createPackage("PRECOMPILER");
  public static final Package PACKAGE_SEQUENCE =
    Packages.createPackage("SEQUENCE", 128); // EH 10-10-2010: Actual number 62


  @DocString(name="nil")
  public static final Symbol NIL = Nil.NIL;

  // We need NIL before we can call usePackage().
  static
  {
    PACKAGE_CL.addNickname("CL");
    PACKAGE_CL_USER.addNickname("CL-USER");
    PACKAGE_CL_USER.usePackage(PACKAGE_CL);
    PACKAGE_CL_USER.usePackage(PACKAGE_EXT);
    PACKAGE_CL_USER.usePackage(PACKAGE_JAVA);
    PACKAGE_SYS.addNickname("SYS");
    PACKAGE_SYS.usePackage(PACKAGE_CL);
    PACKAGE_SYS.usePackage(PACKAGE_EXT);
    PACKAGE_MOP.usePackage(PACKAGE_CL);
    PACKAGE_MOP.usePackage(PACKAGE_EXT);
    PACKAGE_MOP.usePackage(PACKAGE_SYS);
    PACKAGE_TPL.addNickname("TPL");
    PACKAGE_TPL.usePackage(PACKAGE_CL);
    PACKAGE_TPL.usePackage(PACKAGE_EXT);
    PACKAGE_EXT.addNickname("EXT");
    PACKAGE_EXT.usePackage(PACKAGE_CL);
    PACKAGE_EXT.usePackage(PACKAGE_THREADS);
    PACKAGE_JVM.usePackage(PACKAGE_CL);
    PACKAGE_JVM.usePackage(PACKAGE_EXT);
    PACKAGE_JVM.usePackage(PACKAGE_SYS);
    PACKAGE_LOOP.usePackage(PACKAGE_CL);
    PACKAGE_PROF.addNickname("PROF");
    PACKAGE_PROF.usePackage(PACKAGE_CL);
    PACKAGE_PROF.usePackage(PACKAGE_EXT);
    PACKAGE_JAVA.usePackage(PACKAGE_CL);
    PACKAGE_JAVA.usePackage(PACKAGE_EXT);
    PACKAGE_LISP.usePackage(PACKAGE_CL);
    PACKAGE_LISP.usePackage(PACKAGE_EXT);
    PACKAGE_LISP.usePackage(PACKAGE_SYS);
    PACKAGE_THREADS.usePackage(PACKAGE_CL);
    PACKAGE_THREADS.usePackage(PACKAGE_EXT);
    PACKAGE_THREADS.usePackage(PACKAGE_SYS);
    PACKAGE_FORMAT.usePackage(PACKAGE_CL);
    PACKAGE_FORMAT.usePackage(PACKAGE_EXT);
    PACKAGE_XP.usePackage(PACKAGE_CL);
    PACKAGE_PRECOMPILER.addNickname("PRE");
    PACKAGE_PRECOMPILER.usePackage(PACKAGE_CL);
    PACKAGE_PRECOMPILER.usePackage(PACKAGE_EXT);
    PACKAGE_PRECOMPILER.usePackage(PACKAGE_SYS);
    PACKAGE_SEQUENCE.usePackage(PACKAGE_CL);
  }

*/

:- fixup_exports.


