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


new_cl_fixnum(Obj,R):-
  create_struct(clz_fixnum,[Obj],R),!.

create_struct(Type,R):-create_struct(Type,[],R),!.
create_struct(TypeARGS,R):- compound_name_arguments(TypeARGS,Type,ARGS), create_struct(Type,ARGS,R),!.
create_struct(Type,ARGS,R):-
   data_record(Type,PARGS),
   parse_data_record_args3(PARGS,ARGS,KVs),
   %append(KVs,['type-info'-data_record(Type,PARGS),extended2-'$mutable'([],_)],Make),
   Make = KVs,
   dict_create(R,Type,Make).

create_struct1(Type,[Value],Value):- data_record(Type,[_]),!.
create_struct1(Type,ARGS,R):-create_struct(Type,ARGS,R),!.
create_struct1(_Type,Value,Value).

parse_data_record_args3(PARGS,[],KVs):- make_defaults(PARGS,KVs),!.
parse_data_record_args3([m(ro, integer, Name)], [Obj], [Name-Obj]).
parse_data_record_args3(PARGS,ARGS,KVs):-
  must_det_l(( apply:partition(=(m(ro,_,_)), PARGS,ReadOnly,ReadWrite),
   parse_data_record_args5(ReadOnly,ARGS,Part1,ExtraRO,ExtraArgs),
   parse_data_record_args5(ReadWrite,ExtraArgs,Part2,ExtraRW,ExtraExtraArgs),
   make_defaults(ExtraRO,Part3),
   make_defaults(ExtraRW,Part4),
   make_defaults(ExtraRW,Part4),
   append(Part1,Part2,Part12),
   append(Part3,Part4,Part34),   
   (ExtraExtraArgs == [] ->  append(Part12,Part34,KVs) ; 
   append(Part12,[extended1-ExtraExtraArgs|Part34],KVs)))).
parse_data_record_args5(REST,[],[],REST,[]).
parse_data_record_args5([],REST,[],[],REST).
parse_data_record_args5([m(_,Type,Name)|PARGS],[Obj|ARGS],[Name-Value|KVs],O1,O2):-
   create_struct1(Type,Obj,Value),!,
   parse_data_record_args5(PARGS,ARGS,KVs,O1,O2).

name_value_default(m(_,array_of(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,prolog_array_list(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,Type,Name),Name-Def):-value_default(Type,Def).
name_value_default(m(_,Type,Name),Name-mut(@(null),Type)).
name_value_default(N-Value,N-Value).

value_default(prolog_concurrent_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(prolog_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_list,[]).
value_default(integer,0).
value_default(claz_object,mut([],claz_object)).

%value_default(claz_simple_string, @(null)).
%value_default(claz_string, @(null)).
%value_default(prolog_array_list(_),[]).
%value_default(array_of(_),[]).

make_defaults([],[]).
make_defaults([Default|From],[Value|Values]):- name_value_default(Default,Value),
  make_defaults(From,Values).

data_record(Name,[Name],[],[InitArg]):- 
  data_record(Name,[InitArg]),
  m_arg3(InitArg,Name).

data_record(Name,InitArgs,Extras,Args):-
  data_record(Name,Args),Args\=[_],
  must_det_l((
  apply:partition(=(m(ro,_,_)), Args,ReadOnly,ReadWrite),
  maplist(m_arg3,ReadOnly,InitArgs),
  maplist(m_arg3,ReadWrite,Extras))).

m_arg3(m(_,_,Name),Name):-!.

/*
:- defstruct([obr, [':print-function', 'print-ob']],
             "OB representation structure",
             [obnames, []],
             [slots, []],
             [literal, []],
             Defstruct_Ret).
*/

return_arg_is_first(cl_defstruct).

cl_defstruct(Defstruct_Ret,[[Name|KeyWords]|Slots]):- define_struct(Name,KeyWords,Slots,Defstruct_Ret).
cl_defstruct(Defstruct_Ret,[Name|Slots]):- define_struct(Name,[],Slots,Defstruct_Ret).

define_struct(Name,KeyWords,SlotsIn,Struct):-  
  % add doc for string
  maybe_get_docs('class',Name,SlotsIn,Slots,Code),
  call(Code),
  cl_string(Name,SName),  
  new_named_opv(claz_structure_class,SName,[],Struct),
  update_opv(Struct,name,SName),
  add_opv_keywords(Struct,KeyWords),
  maplist(add_opv(Struct,struct_slot),Slots).

add_opv_keywords(_Struct,[]):-!.

add_opv_keywords(Struct,[kw_include,Class|KeyWords]):- 
   assert_if_new(struct_opv(Struct, subtypep, Class)),
   add_opv_keywords(Struct,KeyWords).
add_opv_keywords(Struct,[kw_conc_name,String|KeyWords]):- 
   assert_if_new(struct_opv(Struct, kw_conc_name, String)),
   add_opv_keywords(Struct,KeyWords).
add_opv_keywords(Struct,[Key,Value|KeyWords]):-atom(Key),
   maplist(add_opv(Struct,Key,Value)),
   add_opv_keywords(Struct,KeyWords).
add_opv_keywords(Struct,[Key,Value|KeyWords]):- % atom(Key),
   maplist(add_opv(Struct,Key,Value)),
   add_opv_keywords(Struct,KeyWords).

type_prefix(claz_structure_class, claz_).
type_prefix(Prefix, Prefix):- atom_concat(claz_,_,Prefix),!.
type_prefix(Type, Prefix):- atom_concat(claz_,Type,Prefix).

:- dynamic((struct_opv/3)).
:- include('ci.pro').


cleanup_mop:- 
  forall(struct_opv(Obj, subtypep, Obj),retract(struct_opv(Obj, subtypep, Obj))),
  forall((struct_opv(Obj, has_slot, slot(NT,Name)),NT\==claz_t,
     clause(struct_opv(Obj, has_slot, slot(Other,Name)),true,R),Other==claz_t),
  erase(R)).

save_mop:- cleanup_mop,tell('ci.pro'),P= struct_opv(_,_,_),
   forall(P,format('~q.~n',[P])), told.

:- style_check(-discontiguous).

:- multifile soops:o_p_v/3.
:- dynamic soops:o_p_v/3.

soop_to_typeof(claz_symbol,symbol).
soop_to_typeof(claz_package,package).
soop_to_typeof(claz_number,numer).

soops:o_p_v(Symbol,kw_deftype,defconstant):- soops:o_p_v(Symbol,package,pkg_kw).
soops:o_p_v(Symbol,typeof,keyword):- soops:o_p_v(Symbol,package,pkg_kw).
:- include('si.pro').
soops:o_p_v(Symbol,typeof,Type):- soops:o_p_v(Symbol,classof,Class),
  \+ clause(soops:o_p_v(Symbol,typeof,_AnyType),true),
  soop_to_typeof(Class,Type).
%:- include('si2.pro').


f_u_get_opv(O,Result):- findall([P|Value],get_opv(O,P,Value),Result).
f_u_get_opv(O,P,Value):- get_opv(O,P,Value).
	
add_opv_maybe(O,P,_):- soops:o_p_v(O,P,_),!.
add_opv_maybe(O,P,Value):- add_opv(O,P,Value),!.

add_opv_pred(MPred,O,P,Value):- strip_module(MPred,M,Pred),Prop=.. [Pred,O,P,Value], 
   ( \+ M:Prop -> assert(M:Prop) ; true).

add_opv(Symbol,value,SValue):- atom(SValue),
 (atom_contains(SValue,'(');atom_contains(SValue,' ')),
  (as_sexp(SValue,Value)->SValue\==Value),!,add_opv(Symbol,value,Value).

add_opv(O,P,Value):- ( \+ soops:o_p_v(O,P,_) -> assert(soops:o_p_v(O,P,Value)) ; true).

get_opv(O,_,_):- string(O),!,fail.
get_opv(O,P,Value):- no_repeats(O-P,soops:o_p_v(O,P,Value)).

update_opv(O,P,Value):- ignore(retract(soops:o_p_v(O,P,_))),assert(soops:o_p_v(O,P,Value)).

:- dynamic(accessor_info/6).

ensure_opv_type(Type):-accessor_info(Type,_,_,_,_,_),!.
ensure_opv_type(Type):- findall(Slot,struct_opv(Type,has_slot,Slot),Slots),add_opv_slots(Type,1,Slots).

add_opv_slots(Type,N,[Slot|Slots]):- !, add_opv_slot(N,Type,Slot),N1 is N + 1,add_opv_slots(Type,N1,Slots).
add_opv_slots(_Type,_N,[]).
add_opv_slot(N,Type,slot(DataType,Prop)):- 
   atom_string(Key,Prop),
   get_opv_else(Type,kw_conc_name,Concat,Concat=Type),
   atomic_list_concat([Type,Prop],'_',Accessor),!,
   assert_if_new(accessor_info(Type,Key,N,Prop,Accessor,DataType)).

get_opv_else(Obj,Prop,Value,Else):- get_opv(Obj,Prop,Value)*->true;Else.


new_named_opv(Type,Name,Attrs,RObj):- type_prefix(Type,Pre),
  cl_string(Name,SName),
  atomic_list_concat([Pre,SName],'_inst_',PName),
  prologcase_name(PName,RObj),
  add_opv(RObj,classof,Type),
  add_opv(RObj,name,SName),
  ensure_opv_type(Type),
  construct_opv(RObj,Type),
  maplist(add_opv_kv(RObj,Type),Attrs).
new_opv(Type,RObj):-new_opv(Type,[],RObj).
new_opv(Type,Attrs,RObj):- type_prefix(Type,Pre),atom_concat(Pre,'_inst_',Stem),
  gensym(Stem,RObj),
  add_opv(RObj,classof,Type),
  ensure_opv_type(Type),
  construct_opv(RObj,Type),
  maplist(add_opv_kv(RObj,Type),Attrs).


construct_opv(RObj,Type):- get_opv(RObj,instance,Type),!.
construct_opv(RObj,Type):-
  add_opv(RObj,instance,Type),
  forall(struct_opv(Type,subtypep,Super),construct_opv(RObj,Super)).  


add_opv_kv(RObj,Type,KV):- get_kv(KV,Key,Value), add_type_opv(RObj,Type,Key,Value). 

add_type_opv(RObj,Type,Key,Value):- 
  accessor_info(Type,Key,_Accessor,DataType),!,
  new_opv(DataType,Value,VObj),
  update_opv(RObj,Key,VObj).
add_type_opv(RObj,Type,Key,Value):- 
  cl_type_of(Value,DataType),
  atom_string(Key,SKey),string_lower(SKey,Prop),
  add_opv_slot(Type,slot(DataType,Prop)),
  update_opv(RObj,Key,Value).
  


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


