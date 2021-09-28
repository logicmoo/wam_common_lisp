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
:- module(clstructs, []).
:- set_module(class(library)).
:- include('./header').

% :- expects_dialect(pfc).

to_claz(C1,C1):- var(C1),!.
to_claz(S0,S1):- is_stringp(S0),!,to_prolog_string(S0,S1),!.
to_claz(S0,S1):- atom(S0),to_clz_or_str_a(S0,S1),!.
to_claz(C1,C1):- \+ compound(C1),!.
to_claz([H|T],[HH|TT]):- to_claz(H,HH),to_claz(T,TT).
to_claz(C1,C2):- compound_name_arguments(C1,F,C1O),!,must_maplist(to_claz,C1O,C2O),C2=..[F|C2O].
to_claz(Some,Some).

to_clz_or_str_a(Name,Claz):- atom_concat_or_rtrace('claz_',_,Name),!,Claz=Name.
to_clz_or_str_a(Name,Claz):- atom_concat_or_rtrace('claz_',Name,Claz),!.





%;;(ADD-CI "BUILT-IN-CLASS" T "class_symbol" T)
%;;(ADD-CI "BUILT-IN-CLASS" T "method" (SETF CLASS-NAME) :NAME (SETF CLASS-NAME))
sf_sys_add_ci(_ReplEnv,_Str,Claz0,S0,LspValue,t):-  
  to_claz(Claz0,Claz),add_ci_p2(Claz,S0,LspValue).
sf_sys_add_ci(_ReplEnv,_Str,Claz0,S0,LspObj,LspKey,LspValue,t):-
  to_claz(Claz0,Claz),add_ci_p2(Claz,S0,LspObj,LspKey,LspValue).

notify_assert(G):- clause(G,_),!.
notify_assert(G):- assertz(G),dmsg(G).

is_class_prop(class_name).
is_class_prop(type_of).
is_class_prop(type).
is_class_prop(kw_include).

add_ci_p2(Claz,kw_subclass,S0):- to_claz(S0,S1),!,add_ci_p2(S1,kw_include,Claz).
add_ci_p2(Claz,Prop,S1):- is_class_prop(Prop),!,notify_assert(soops:struct_opv(Claz,Prop,S1)).
add_ci_p2(Claz,Prop,S0):- to_claz(S0,S1),!,notify_assert(soops:struct_opv(Claz,Prop,S1)).


get_zlot(Claz,SlotName,ZLOT):- always(gen_slot_name('',Claz,SlotName,ZLOT)).

add_ci_p2(Claz,kw_slot,  SlotName,SlotProp,SlotPropValue):- !,
    always((get_zlot(Claz,SlotName,ZLOT),
    notify_assert(soops:struct_opv(Claz,sys_name,SlotName,ZLOT)),
    notify_assert(soops:struct_opv(Claz,SlotProp,SlotPropValue,ZLOT)))).
add_ci_p2(Claz,kw_method,SlotName,SlotProp,SlotPropValue):- notify_assert(added_method(Claz,SlotName,SlotProp,SlotPropValue)).
add_ci_p2(Claz,KW,       SlotName,SlotProp,SlotPropValue):- trace, notify_assert(added_ci(Claz,KW,SlotName,SlotProp,SlotPropValue)).

/*
sf_sys_add_ci2(_ReplEnv,list, type_of, built_in_class).
sf_sys_add_ci2(_ReplEnv,logical_pathname, class_name, logical_pathname).
sf_sys_add_ci2(_ReplEnv,logical_pathname, sys_class_precedence_list, [pathname, t]).
sf_sys_add_ci2(_ReplEnv,logical_pathname, type_of, built_in_class).
sf_sys_add_ci2(_ReplEnv,claz_, class_name, _).
sf_sys_add_ci2(_ReplEnv,claz_, sys_class_precedence_list, claz_).
sf_sys_add_ci2(_ReplEnv,claz_, type_of, _).
sf_sys_add_ci2(_ReplEnv,number, "subclass", complex).
*/

ds_create_struct(Type,ARGS,R):-
   data_record(Type,PARGS),
   parse_data_record_args3(PARGS,ARGS,KVs),
   %append(KVs,['type-info'-data_record(Type,PARGS),extended2-'$mutable'([],_)],Make),
   Make = KVs,
   dict_create(R,Type,Make).

create_struct1(Type,[Value],Value):- data_record(Type,[_]),!.
create_struct1(Type,ARGS,R):-create_struct(Type,ARGS,R),!.
create_struct1(_Type,Value,Value).

parse_data_record_args3(PARGS,[],KVs):- make_defaults(PARGS,KVs),!.
parse_data_record_args3([m(ro, integer, Name)], [X], [Name-X]).
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
parse_data_record_args5([m(_,Type,Name)|PARGS],[X|ARGS],[Name-Value|KVs],O1,O2):-
   create_struct1(Type,X,Value),!,
   parse_data_record_args5(PARGS,ARGS,KVs,O1,O2).

name_value_default(m(_,array_of(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,claz_sys_array_list(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,Type,Name),Name-Def):-value_default(Type,Def).
name_value_default(m(_,Type,Name),Name-mut(@(null),Type)).
name_value_default(N-V,N-V).

value_default(claz_claz_sys_concurrent_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_claz_sys_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_list,[]).
value_default(integer,0).
value_default(claz_object,mut([],claz_object)).

%value_default(claz_simple_string, @(null)).
%value_default(claz_string, @(null)).
%value_default(claz_sys_array_list(_),[]).
%value_default(array_of(_),[]).


%value_default(cl_simple_string, @(null)).
%value_default(cl_string, @(null)).
%value_default(claz_sys_array_list(_),[]).
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


:- dynamic(system_subclazz/2).



system_subclazz(claz_simple_array__t,claz_array).
system_subclazz(claz_simple_array(unsigned_byte16),claz_array).
system_subclazz(claz_simple_array(unsigned_byte32),claz_array).
system_subclazz(claz_simple_array(unsigned_byte8),claz_array).
system_subclazz(claz_simple_array(_Unsigned_byte8),claz_array).
system_subclazz(claz_complex_array,claz_array).
system_subclazz(claz_complex_array(unsigned_byte32),claz_array).
system_subclazz(claz_complex_array(unsigned_byte8),claz_array).
system_subclazz(claz_complex_array(_Unsigned_byte8),claz_array).
system_subclazz(claz_complex_bit_vector,claz_bit_vector).
system_subclazz(claz_complex_string,claz_string).
system_subclazz(claz_complex_vector,claz_vector).
system_subclazz(claz_complex_vector(unsigned_byte32),claz_vector).
system_subclazz(claz_complex_vector(unsigned_byte8),claz_vector).
system_subclazz(claz_complex_vector(_Unsigned_byte8),claz_vector).
system_subclazz(claz_zero_rank_array,claz_array).
system_subclazz(claz_nil_vector,claz_string).
system_subclazz(claz_string,claz_vector).
system_subclazz(claz_simple_vector,claz_vector).
system_subclazz(claz_basic_vector(unsigned_byte16),claz_vector).
system_subclazz(claz_basic_vector(unsigned_byte32),claz_vector).
system_subclazz(claz_basic_vector(unsigned_byte8),claz_vector).
system_subclazz(claz_basic_vector(_Unsigned_byte8),claz_vector).
system_subclazz(claz_bit_vector,claz_vector).
system_subclazz(claz_vector,claz_array).
system_subclazz(claz_array,claz_object).
system_subclazz(claz_case_frob_stream,claz_stream).
system_subclazz(claz_control_transfer,claz_sys_runtime_exception).
system_subclazz(claz_dispatch_macro_function,claz_function).
system_subclazz(claz_function,claz_operator).
system_subclazz(claz_lisp_class,claz_standard_object).
system_subclazz(claz_operator,claz_object).
system_subclazz(claz_reader_macro_function,claz_function).
system_subclazz(claz_stack_frame,claz_object).
system_subclazz(claz_arithmetic_error,claz_lisp_error).
system_subclazz(claz_autoload,claz_function).
system_subclazz(claz_built_in_class,claz_lisp_class).
system_subclazz(claz_cell_error,claz_lisp_error).
system_subclazz(claz_closure,claz_function).
system_subclazz(claz_compiled_closure,claz_closure).
system_subclazz(claz_compiled_primitive,claz_primitive).
system_subclazz(claz_condition,claz_standard_object).
system_subclazz(claz_fasl_class_loader,claz_ffi_class_loader).
system_subclazz(claz_funcallable_standard_class,claz_standard_class).
system_subclazz(claz_funcallable_standard_object,claz_standard_object).
system_subclazz(claz_hash_table,claz_object).
system_subclazz(claz_integrity_error,claz_sys_error).
system_subclazz(claz_ffi_class_loader,claz_sys_url_class_loader).
system_subclazz(claz_ffi_exception,claz_lisp_error).
system_subclazz(claz_ffi_stack_frame,claz_stack_frame).
system_subclazz(claz_layout,claz_object).
system_subclazz(claz_lisp_error,claz_serious_condition).
system_subclazz(claz_lisp_integer,claz_object).
system_subclazz(claz_lisp_stack_frame,claz_stack_frame).
system_subclazz(claz_memory_class_loader,claz_ffi_class_loader).
system_subclazz(claz_pathname,claz_object).
system_subclazz(claz_primitive,claz_function).
system_subclazz(claz_print_not_readable,claz_lisp_error).
system_subclazz(claz_processing_terminated,claz_sys_error).
system_subclazz(claz_program_error,claz_lisp_error).
system_subclazz(claz_readtable,claz_object).
system_subclazz(claz_serious_condition,claz_condition).
system_subclazz(claz_simple_condition,claz_condition).
system_subclazz(claz_slime_input_stream,claz_stream).
system_subclazz(claz_slot_class,claz_lisp_class).
system_subclazz(claz_special_operator,claz_operator).
system_subclazz(claz_standard_class,claz_slot_class).
system_subclazz(claz_standard_object,claz_object).
system_subclazz(claz_storage_condition,claz_serious_condition).
system_subclazz(claz_stream,claz_structure_object).
system_subclazz(claz_stream_error,claz_lisp_error).
system_subclazz(claz_structure_class,claz_slot_class).
system_subclazz(claz_structure_object,claz_object).
system_subclazz(claz_symbol,claz_object).
system_subclazz(claz_thread_destroyed,claz_sys_error).
system_subclazz(claz_two_way_stream,claz_stream).
system_subclazz(claz_type_error,claz_lisp_error).
system_subclazz(claz_decoding_reader,claz_sys_pushback_reader).
system_subclazz(claz_racf_malformed_input_exception ,claz_sys_malformed_input_exception).
system_subclazz(claz_racf_unmappable_character_exception ,claz_sys_unmappable_character_exception).
system_subclazz(claz_warning,claz_condition).
system_subclazz(claz_weak_hash_table,claz_object).
system_subclazz(claz_weak_reference,claz_object).
system_subclazz(claz_autoload_generalized_reference,claz_autoload).
system_subclazz(claz_autoload_macro,claz_autoload).
system_subclazz(claz_bignum,claz_lisp_integer).
system_subclazz(claz_broadcast_stream,claz_stream).
system_subclazz(claz_byte_array_input_stream,claz_stream).
system_subclazz(claz_byte_array_output_stream,claz_stream).
system_subclazz(claz_capitalize_first_stream,claz_case_frob_stream).
system_subclazz(claz_capitalize_stream,claz_case_frob_stream).
system_subclazz(claz_complex,claz_object).
system_subclazz(claz_concatenated_stream,claz_stream).
system_subclazz(claz_cons,claz_object).
system_subclazz(claz_control_error,claz_lisp_error).
system_subclazz(claz_division_by_zero,claz_arithmetic_error).
%system_subclazz(cldolist,claz_special_operator).
%system_subclazz(cldotimes,claz_special_operator).
system_subclazz(claz_double_float,claz_object).
system_subclazz(claz_downcase_stream,claz_case_frob_stream).
system_subclazz(claz_echo_stream,claz_stream).
system_subclazz(claz_e_m_f_cache,claz_object).
system_subclazz(claz_end_of_file,claz_stream_error).
system_subclazz(claz_environment,claz_object).
system_subclazz(claz_fasl_readtable,claz_readtable).
system_subclazz(claz_file_error,claz_lisp_error).
system_subclazz(claz_file_stream,claz_stream).
system_subclazz(claz_fill_pointer_output_stream,claz_stream).
system_subclazz(claz_fixnum,claz_lisp_integer).
system_subclazz(claz_floating_point_inexact,claz_arithmetic_error).
system_subclazz(claz_floating_point_invalid_operation,claz_arithmetic_error).
system_subclazz(claz_floating_point_overflow,claz_arithmetic_error).
system_subclazz(claz_floating_point_underflow,claz_arithmetic_error).
system_subclazz(claz_go,claz_control_transfer).
system_subclazz(claz_illegal_monitor_state,claz_program_error).
system_subclazz(claz_jar_stream,claz_stream).
system_subclazz(claz_ffi_object,claz_object).
system_subclazz(claz_character,claz_object).
system_subclazz(claz_lisp_thread,claz_object).
system_subclazz(claz_logical_pathname,claz_pathname).
system_subclazz(claz_macro_object,claz_function).
system_subclazz(claz_nil,claz_symbol).
system_subclazz(claz_package,claz_object).
system_subclazz(claz_package_error,claz_lisp_error).
system_subclazz(claz_parse_error,claz_lisp_error).
system_subclazz(claz_random_state,claz_object).
system_subclazz(claz_ratio,claz_object).
system_subclazz(claz_reader_error,claz_stream_error).
system_subclazz(claz_return,claz_control_transfer).
system_subclazz(claz_seekable_string_writer,claz_sys_writer).
system_subclazz(claz_simple_bit_vector,claz_bit_vector).
system_subclazz(claz_simple_error,claz_lisp_error).
system_subclazz(claz_simple_string,claz_string).
system_subclazz(claz_simple_type_error,claz_type_error).
system_subclazz(claz_simple_warning,claz_warning).
system_subclazz(claz_single_float,claz_object).
system_subclazz(claz_slime_output_stream,claz_stream).
system_subclazz(claz_slot_definition,claz_standard_object).
system_subclazz(claz_slot_definition_class,claz_standard_class).
system_subclazz(claz_socket_stream,claz_two_way_stream).
system_subclazz(claz_string_input_stream,claz_stream).
system_subclazz(claz_string_output_stream,claz_stream).
system_subclazz(claz_style_warning,claz_warning).
system_subclazz(claz_symbol_macro,claz_object).
system_subclazz(claz_synonym_stream,claz_stream).
system_subclazz(claz_throw,claz_control_transfer).
system_subclazz(claz_unbound_slot,claz_cell_error).
system_subclazz(claz_unbound_variable,claz_cell_error).
system_subclazz(claz_undefined_function,claz_cell_error).
system_subclazz(claz_upcase_stream,claz_case_frob_stream).
system_subclazz(claz_url_stream,claz_stream).
system_subclazz(claz_wrong_number_of_arguments_exception,claz_program_error).




data_record(claz_complex_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,claz_boolean,is_displaced),
  m(rw,array_of(Kind),elements),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(claz_complex_vector,[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,claz_boolean,is_displaced),
  m(rw,claz_list,elements),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(claz_bignum,[
  m(ro,claz_sys_big_integer,value)]).

data_record(claz_complex_bit_vector,[
  m(rw,integer,fill_pointer),
  m(rw,claz_boolean,is_displaced),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(claz_simple_array(unsigned_byte16),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,array_of(integer),data)]).

data_record(claz_package,[
  m(rw,claz_string,name),
  m(rw,claz_list,property_list),
  m(rw,claz_hash_table(claz_string,claz_symbol),internal_symbols),
  m(rw,claz_hash_table(claz_string,claz_symbol),external_symbols),
  m(rw,claz_hash_table(claz_string,claz_symbol),shadowing_symbols),
  m(rw,claz_sys_array_list(claz_string),nicknames),
  m(rw,claz_sys_array_list(claz_package),use_list),
  m(rw,claz_sys_array_list(claz_package),used_by_list),
  m(rw,claz_hash_table(claz_string,claz_package),local_nicknames)]).

data_record(claz_condition,[
  m(rw,claz_string,message)]).


data_record(claz_random_state,[
  m(rw,claz_sys_random,random)]).

data_record(claz_pathname,[
  m(rw,claz_object,host),
  m(rw,claz_object,device),
  m(rw,claz_object,directory),
  m(rw,claz_object,name),
  m(rw,claz_object,type),
  m(rw,claz_object,version),
  m(rw,claz_string,namestring)]).

data_record(claz_hash_table,[
  m(ro,integer,rehash_size),
  m(ro,number,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(claz_hash_table_entry),buckets),
  m(rw,integer,count),
  m(ro,function,test),
  m(ro,j_reentrant_lock,lock)]).

data_record(claz_zero_rank_array,[
  m(ro,claz_object,element_type),
  m(ro,claz_boolean,adjustable),
  m(rw,claz_object,data)]).

data_record(claz_slot_class,[
  m(rw,claz_list,direct_slot_definitions),
  m(rw,claz_list,slot_definitions),
  m(rw,claz_list,direct_default_initargs),
  m(rw,claz_list,default_initargs)]).


data_record(claz_synonym_stream,[
  m(ro,claz_symbol,stream_name)]).


data_record(claz_socket_stream,[
  m(ro,claz_sys_socket,socket)]).

data_record(claz_slime_input_stream,[
  m(rw,claz_string,s),
  m(rw,integer,length),
  m(ro,claz_function,f),
  m(ro,claz_stream,ostream)]).

data_record(claz_slime_output_stream,[
  m(ro,claz_sys_string_writer,string_writer),
  m(ro,claz_function,f)]).

data_record(claz_nil_vector,[
  m(rw,integer,capacity)]).

data_record(claz_string_input_stream,[
  m(ro,claz_sys_string_reader,string_reader),
  m(ro,integer,start),
  m(ro,claz_string,sub_string)]).

data_record(claz_byte_array_output_stream,[
  m(ro,claz_sys_byte_array_output_stream,byte_array_output_stream)]).

data_record(claz_stream_error,[
  m(ro,claz_sys_throwable,cause)]).

data_record(claz_broadcast_stream,[
  m(ro,array_of(claz_stream),streams)]).

data_record(claz_standard_object,[
  m(rw,claz_layout,layout),
  m(rw,claz_list,slots)]).

data_record(claz_macro_object,[
  m(ro,claz_object,name),
  m(ro,claz_object,expander)]).

data_record(claz_complex_string,[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,claz_boolean,is_displaced),
  m(rw,array_of(char_code),chars),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(claz_function,[
  m(rw,claz_list,property_list),
  m(rw,integer,call_count),
  m(rw,integer,hot_count),
  m(ro,claz_object,loaded_from)]).

data_record(claz_string_output_stream,[
  m(ro,claz_seekable_string_writer,string_writer)]).

data_record(claz_character,[
  m(ro,char_code,value),
  m(rw,claz_string,name)]).

data_record(claz_weak_reference,[
  m(rw,claz_sys_weak_reference(claz_object),ref)]).

data_record(claz_emf_cache___eql_specialization,[
  m(rw,claz_object,eql_to)]).

data_record(claz_simple_array(Kind),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,array_of(Kind),data)]).


data_record(claz_symbol,[
  m(ro,claz_simple_string,name),
  m(rw,integer,hash),
  m(rw,integer,special_index),
  m(rw,claz_object,pkg),
  m(rw,claz_object,value),
  m(rw,claz_object,function),
  m(rw,claz_list,property_list),
  m(rw,bitmask,flags)]).

data_record(claz_operator,[
  m(rw,claz_object,lambda_name),
  m(rw,claz_object,lambda_list)]).

data_record(claz_function_binding,[
  m(rw,claz_object,name),
  m(rw,claz_object,value),
  m(ro,claz_function_binding,next)]).

data_record(claz_capitalize_stream,[
  m(rw,claz_boolean,in_word)]).

data_record(claz_basic_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,array_of(Kind),elements)]).
data_record(claz_simple_vector,[
  m(rw,integer,capacity),
  m(rw,claz_list,data)]).

data_record(claz_structure_object,[
  m(ro,claz_structure_class,structure_class),
  m(ro,claz_list,slots)]).

data_record(claz_interpreter,[
  m(ro,claz_boolean,jlisp),
  m(ro,claz_sys_input_stream,input_stream),
  m(ro,claz_sys_output_stream,output_stream)]).

data_record(claz_capitalize_first_stream,[
  m(rw,claz_boolean,virgin)]).

data_record(claz_ffi_exception,[
  m(ro,claz_sys_throwable,throwable)]).

data_record(claz_file_stream,[
  m(ro,claz_random_access_character_file,racf),
  m(ro,claz_pathname,pathname),
  m(ro,integer,bytes_per_unit)]).

data_record(claz_seekable_string_writer,[
  m(ro,claz_sys_string_buffer,string_buffer),
  m(rw,integer,offset)]).

data_record(claz_bit_vector,[
  m(rw,integer,capacity),
  m(rw,array_of(long),bits)]).

data_record(claz_double_float,[
  m(ro,double,value)]).

data_record(claz_simple_string,[
  m(rw,integer,capacity),
  m(rw,array_of(char_code),chars)]).

data_record(claz_concatenated_stream,[
  m(rw,claz_object,streams)]).

data_record(claz_simple_array(unsigned_byte32),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,claz_list,data)]).

data_record(claz_case_frob_stream,[
  m(ro,claz_stream,target)]).

data_record(claz_complex,[
  m(ro,claz_object,realpart),
  m(ro,claz_object,imagpart)]).


data_record(claz_char_hash_map(T),[
  m(ro,array_of(T),constants_by_char_code),
  m(ro,T,null_value),
  m(ro,claz_hash_table(claz_sys_character,T),backing)]).

data_record(claz_throw,[
  m(ro,claz_object,tag),
  m(ro,claz_object,result),
  m(ro,claz_list,values)]).

data_record(claz_simple_array__t,[
  m(ro,array_of(integer),dimv),
  m(ro,claz_object,element_type),
  m(ro,integer,total_size),
  m(ro,claz_list,data)]).

data_record(claz_ffi_object,[
  m(ro,claz_sys_object,obj),
  m(ro,claz_sys_class,intended_class)]).

data_record(claz_funcallable_standard_object,[
  m(rw,claz_object,function),
  m(rw,claz_emf_cache,cache),
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

data_record(claz_url_stream,[
  m(ro,claz_pathname,pathname),
  m(ro,claz_sys_input_stream,input),
  m(ro,claz_sys_reader,reader),
  m(ro,integer,bytes_per_unit)]).

data_record(claz_lisp_stack_frame,[
  m(ro,claz_object,operator),
  m(ro,claz_list,args)]).

data_record(claz_echo_stream,[
  m(ro,claz_stream,in),
  m(ro,claz_stream,out),
  m(rw,integer,unread_char)]).

data_record(claz_readtable,[
  m(ro,claz_char_hash_map(claz_sys_byte),syntax),
  m(ro,claz_char_hash_map(claz_object),reader_macro_functions),
  m(ro,claz_char_hash_map(claz_readtable___dispatch_table),dispatch_tables),
  m(rw,claz_object,readtable_case)]).

data_record(claz_fill_pointer_output_stream,[
  m(rw,claz_complex_string,string_buffer)]).


data_record(claz_lisp_thread,[
  m(rw,claz_object,thread_value),
  m(ro,claz_sys_thread,ffi_thread),
  m(rw,claz_boolean,destroyed),
  m(ro,claz_object,name),
  m(rw,claz_list,thread_values),
  m(rw,claz_boolean,thread_interrupted),
  m(rw,claz_object,pending),
  m(rw,claz_symbol,wrapper),
  m(rw,array_of(claz_special_binding),specials),
  m(rw,claz_special_bindings_mark,saved_specials),
  m(rw,claz_object,catch_tags),
  m(rw,claz_lisp_thread___stack_segment,top_stack_segment),
  m(rw,array_of(claz_sys_object),stack),
  m(rw,integer,stack_ptr),
  m(rw,claz_lisp_thread___stack_segment,spare_stack_segment)]).

data_record(claz_byte_array_input_stream,[
  m(ro,claz_sys_byte_array_input_stream,byte_array_input_stream)]).


data_record(claz_closure,[
  m(ro,claz_object,body),
  m(ro,claz_object,execution_body),
  m(ro,claz_environment,environment),
  m(ro,array_of(claz_symbol),free_specials),
  m(ro,claz_argument_list_processor,arglist)]).

data_record(claz_racf_unmappable_character_exception,[
  m(ro,integer,position),
  m(ro,char_code,character_value),
  m(ro,claz_string,charset_name)]).

data_record(claz_racf_malformed_input_exception,[
  m(ro,integer,position),
  m(ro,char_code,character),
  m(ro,claz_string,charset_name)]).

data_record(claz_decoding_reader,[
  m(rw,claz_sys_byte_buffer,bbuf),
  m(rw,claz_sys_pushback_input_stream,stream),
  m(rw,claz_sys_charset_decoder,cd),
  m(rw,claz_sys_charset_encoder,ce)]).

data_record(claz_random_access_character_file,[
  m(rw,claz_random_access_writer,writer),
  m(rw,claz_random_access_reader,reader),
  m(rw,claz_random_access_input_stream,input_stream),
  m(rw,claz_random_access_output_stream,output_stream),
  m(rw,claz_sys_file_channel,fcn),
  m(rw,claz_sys_charset,cset),
  m(rw,claz_sys_charset_encoder,cenc),
  m(rw,claz_sys_charset_decoder,cdec),
  m(rw,claz_sys_byte_buffer,bbuf),
  m(rw,claz_boolean,bbuf_is_dirty),
  m(rw,claz_boolean,bbuf_is_readable),
  m(rw,long,bbufpos),
  m(rw,claz_sys_char_buffer,single_char_buf),
  m(rw,claz_sys_byte_buffer,short_byte_buf)]).


data_record(claz_single_float,[
  m(ro,float,value)]).

data_record(claz_compiled_closure,[
  m(rw,array_of(claz_closure_binding),ctx)]).

data_record(claz_special_operator,[
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

data_record(claz_layout,[
  m(ro,claz_object,lisp_class),
  m(ro,claz_hash_table(claz_object,claz_object),slot_table),
  m(ro,claz_list,slot_names),
  m(ro,claz_object,shared_slots),
  m(rw,claz_boolean,invalid)]).

data_record(claz_binding,[
  m(ro,claz_object,bound_symbol),
  m(rw,claz_environment,env),
  m(rw,claz_object,value),
  m(rw,claz_boolean,specialp),
  m(ro,claz_binding,next)]).

data_record(claz_fixnum,[
  m(ro,integer,value)]).

data_record(claz_ratio,[
  m(rw,claz_sys_big_integer,numerator),
  m(rw,claz_sys_big_integer,denominator)]).

data_record(claz_shell_command,[
  m(rw,claz_sys_thread,thread),
  m(ro,claz_string,command),
  m(ro,claz_string,directory),
  m(ro,claz_stream,output_stream),
  m(ro,claz_sys_string_buffer,output),
  m(rw,integer,exit_value)]).

data_record(claz_stream,[
  m(rw,claz_object,element_type),
  m(rw,claz_boolean,is_input_stream),
  m(rw,claz_boolean,is_output_stream),
  m(rw,claz_boolean,is_character_stream),
  m(rw,claz_boolean,is_binary_stream),
  m(rw,claz_boolean,past_end),
  m(rw,claz_boolean,interactive),
  m(rw,claz_boolean,open),
  m(rw,claz_sys_pushback_reader,reader),
  m(rw,integer,offset),
  m(rw,integer,line_number),
  m(rw,claz_sys_writer,writer),
  m(rw,integer,char_pos),
  m(rw,claz_stream___eol_style,eol_style),
  m(rw,char_code,eol_char),
  m(rw,claz_object,external_format),
  m(rw,claz_string,encoding),
  m(rw,char_code,last_char),
  m(rw,claz_sys_input_stream,in),
  m(rw,claz_sys_output_stream,out)]).

data_record(claz_environment,[
  m(rw,claz_binding,vars),
  m(rw,claz_function_binding,last_function_binding),
  m(rw,claz_binding,blocks),
  m(rw,claz_binding,tags),
  m(rw,claz_boolean,inactive)]).

data_record(claz_complex_array(Kind),[
  m(ro,array_of(integer),dimv),
  m(rw,integer,total_size),
  m(rw,array_of(Kind),data),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(claz_complex_array,[
  m(ro,array_of(integer),dimv),
  m(ro,claz_object,element_type),
  m(rw,integer,total_size),
  m(rw,claz_list,data),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(claz_cons,[
  m(rw,claz_object,car),
  m(rw,claz_object,cdr)]).


data_record(claz_lisp_class,[
  m(ro,integer,sxhash),
  m(rw,claz_object,name),
  m(rw,claz_list,property_list),
  m(rw,claz_layout,class_layout),
  m(rw,claz_list,direct_superclasses),
  m(rw,claz_list,direct_subclasses),
  m(rw,claz_list,class_precedence_list),
  m(rw,claz_list,direct_methods),
  m(rw,claz_list,documentation),
  m(rw,claz_boolean,finalized)]).

data_record(claz_autoload_generalized_reference,[
  m(rw,claz_symbol,indicator)]).

data_record(claz_two_way_stream,[
  m(ro,claz_stream,in),
  m(ro,claz_stream,out)]).



claz_object_to_OBJECT(Atom,OUT):-atom(Atom),atom_concat('claz_',_,Atom),upcase_atom(Atom,UPPER),UPPER\==Atom,
  atomic_list_concat(List,'_',UPPER),atomic_list_concat(List,'-',OUT),!.
claz_OBJECT_to_object(Atom,OUT):-
  atom(Atom),downcase_atom(Atom,DOWN),DOWN\==Atom,
  atomic_list_concat(List,'-',DOWN),atomic_list_concat(List,'_',CL), atom_concat('claz_',CL,OUT),!.

% system_subclazz(C1,C2)==>(recognised_clazz(C1),recognised_clazz(C2)).  
recognised_clazz(AA):- data_record(C1,Lst),(AA=C1-> true ; (member(m(_,C2,_),Lst),C2==AA)).
recognised_clazz(AA):- system_subclazz(C1,C2),(AA=C1;AA=C2).
maybe_xform_recognised_clazz([A|B],AA):-is_list(B),!,maplist(maybe_xform_recognised_clazz,[A|B],AA).
maybe_xform_recognised_clazz(A,AA):- (claz_OBJECT_to_object(A,AA),recognised_clazz(AA))->true;A=AA.

/*
term_expansion(mop_direct(A,P,B),mop_direct(AA,P,BB)):- 
  maybe_xform_recognised_clazz(A,AA),
  (szlot\=P -> maybe_xform_recognised_clazz(B,BB) ; B=BB),
  ((A\==AA) ; (B\==BB)).
*/

:- fixup_exports.



end_of_file.

data_record(claz_symbol_macro,[
  m(rw,claz_object,expansion)]).

data_record(cla__non_constant_init_form,[
  m(rw,claz_object,form)]).

data_record(claz_sys_weak_hash_entry_weak_key_and_value,[
  m(rw,claz_sys_weak_reference(claz_object),key),
  m(rw,claz_sys_weak_reference(claz_object),value),
  m(ro,claz_weak_hash_table,this__0)]).

data_record(claz_closure_binding,[
  m(rw,claz_object,value)]).

data_record(claz_processing_terminated,[
  m(rw,integer,status)]).

data_record(claz_sys_repl_console,[
  m(rw,claz_sys_string_buffer,input_buffer),
  m(rw,claz_sys_reader,reader),
  m(rw,claz_sys_writer,writer),
  m(rw,claz_boolean,disposed),
  m(ro,claz_sys_thread,repl_thread),
  m(ro,claz_object,debugger_hook)]).


true.

?- listing(sf_sys_add_ci2).

true.

?- lisitng(added_ci2).
Correct to: "listing(added_ci2)"? yes
:- dynamic added_ci2/5.





data_record(claz_ffi_object__1,[
  m(ro,claz_list,val__acc),
  m(ro,claz_ffi_object,this__0)]).

data_record(claz_hash_table_entry,[
  m(rw,claz_object,key),
  m(rw,integer,hash),
  m(rw,claz_object,value),
  m(rw,claz_hash_table_entry,next)]).

data_record(claz_profiler__1__1,[
  m(rw,claz_sys_thread,thread),
  m(ro,claz_lisp_thread,val__thread),
  m(ro,claz_profiler__1,this__0)]).


data_record(claz_lisp_thread___stack_marker,[
  m(ro,integer,num_args)]).

data_record(claz_primitives__pf_finalize__1,[
  m(rw,claz_sys_thread,thread),
  m(ro,claz_object,val__fun),
  m(ro,claz_primitives__pf_finalize,this__0)]).

data_record(cla__environment_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special)]).


data_record(claz_claz_sys_proxy___lisp_handler,[
  m(rw,claz_sys_map,table)]).

data_record(claz_autoload,[
  m(ro,claz_string,file_name),
  m(ro,claz_string,class_name),
  m(ro,claz_symbol,function_symbol)]).

data_record(claz_char_hash_map__1,[
  m(ro,claz_sys_iterator(claz_sys_character),car_it),
  m(rw,integer,char_num),
  m(ro,claz_char_hash_map,this__0)]).

data_record(claz_string_functions___string_indices_and_chars,[
  m(rw,claz_string,string1),
  m(rw,claz_boolean,convert_case),
  m(rw,array_of(char_code),array1),
  m(rw,array_of(char_code),array2),
  m(rw,integer,start1),
  m(rw,integer,end1),
  m(rw,integer,start2),
  m(rw,integer,end2)]).


data_record(claz_ffi_object__2,[
  m(ro,claz_list,val__acc),
  m(ro,claz_object,val__fn)]).

data_record(claz_shell_command___reader_thread,[
  m(rw,array_of(char_code),buf),
  m(ro,claz_sys_input_stream,input_stream),
  m(ro,claz_sys_buffered_reader,reader),                                  
  m(ro,claz_shell_command,this__0),
  m(rw,claz_boolean,done)]).

data_record(cla__rest_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special)]).

data_record(claz_ffi_stack_frame,[
  m(ro,claz_sys_stack_trace_element,ffi_frame)]).

data_record(cla__arg_list,[
  m(ro,claz_list,args),
  m(rw,integer,args_consumed),
  m(ro,integer,len),
  m(ro,claz_environment,env)]).

data_record(claz_claz_sys_proxy___entry,[
  m(rw,claz_sys_class,iface),
  m(rw,claz_sys_map,lisp_defined_methods)]).

data_record(claz_interpreter___unhandled_condition,[
  m(rw,claz_object,condition)]).









data_record(cla__slow_matcher,[
  m(ro,claz_argument_list_processor,this__0)]).

data_record(cla__aux_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special),
  m(rw,cla__init_form,initform)]).

data_record(claz_sys_weak_hash_entry,[
  m(rw,claz_object,key),
  m(rw,integer,hash),
  m(rw,claz_object,value),
  m(rw,claz_sys_weak_hash_entry,next),
  m(rw,integer,slot),
  m(ro,claz_weak_hash_table,this__0)]).

data_record(claz_illegal_monitor_state,[
  m(rw,claz_string,message)]).

data_record(cla__required_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special)]).

data_record(claz_emf_cache,[
  m(rw,claz_hash_table(claz_emf_cache___cache_entry,claz_object),cache),
  m(rw,index_of(claz_emf_cache___eql_specialization),eql_specializations)]).

data_record(claz_sys_weak_hash_entry_weak_key_or_value,[
  m(ro,claz_weak_hash_table,this__0)]).

data_record(cla__constant_init_form,[
  m(rw,claz_object,value)]).

data_record(claz_lisp_thread___stack_segment,[
  m(ro,array_of(claz_sys_object),stack),
  m(ro,claz_lisp_thread___stack_segment,next),
  m(rw,integer,stack_ptr)]).



data_record(cla__fast_matcher,[
  m(ro,claz_argument_list_processor,this__0)]).

data_record(claz_go,[
  m(ro,claz_object,tagbody),
  m(ro,claz_object,tag)]).

data_record(cla__optional_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special),
  m(rw,claz_symbol,supplied_var),
  m(rw,claz_boolean,supplied_special),
  m(rw,cla__init_form,init_form)]).

data_record(ffi_script_engine,[
  m(rw,claz_interpreter,interpreter),
  m(rw,claz_function,eval_script),
  m(rw,claz_function,eval_function),
  m(rw,claz_function,compile_script),
  m(rw,claz_function,eval_compiled_script)]).

data_record(ffi_script_engine___abclaz_compiled_script,[
  m(rw,claz_object,function),
  m(ro,ffi_script_engine,this__0)]).

data_record(claz_return,[
  m(ro,claz_object,tag),
  m(ro,claz_object,block),
  m(ro,claz_object,result)]).

data_record(claz_claz_sys_handler___entry,[
  m(rw,claz_function,handler),
  m(rw,claz_object,data),
  m(rw,integer,count),
  m(rw,claz_sys_map(claz_string,claz_claz_sys_handler___entry),entry_table),
  m(rw,claz_string,event)]).

data_record(cla__keyword_param,[
  m(rw,claz_symbol,keyword)]).

data_record(claz_weak_hash_table,[
  m(ro,claz_object,rehash_size),
  m(ro,claz_object,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(claz_sys_weak_hash_entry),buckets),
  m(rw,integer,count),
  m(ro,claz_weak_hash_table___test,test),
  m(ro,j_reentrant_lock,lock),
  m(rw,claz_sys_weak_hash_entry,bucket_type),
  m(ro,claz_object,weakness),
  m(rw,claz_sys_reference_queue(claz_object),queue),
  m(rw,claz_sys_map(claz_sys_reference,claz_sys_weak_hash_entry),entry_lookup)]).

data_record(claz_wrong_number_of_arguments_exception,[
  m(rw,claz_operator,operator),
  m(rw,integer,expected_min_args),
  m(rw,integer,expected_max_args),
  m(rw,claz_object,actual_args),
  m(rw,claz_string,message)]).


data_record(claz_zip_cache___entry,[
  m(rw,long,last_modified),
  m(rw,j_zip_file,file)]).

data_record(clzip___directories,[
  m(rw,j_zip_output_stream,out)]).

data_record(claz_sys_weak_hash_entry_weak_key,[
  m(rw,claz_sys_weak_reference(claz_object),key),
  m(ro,claz_weak_hash_table,this__0)]).

data_record(claz_emf_cache___cache_entry,[
  m(ro,claz_list,array)]).

data_record(claz_special_bindings_mark,[
  m(rw,integer,idx),
  m(rw,claz_special_binding,binding),
  m(rw,claz_special_bindings_mark,next)]).

data_record(claz_readtable___dispatch_table,[
  m(ro,claz_char_hash_map(claz_object),functions)]).

data_record(claz_ffi_class_loader_pf_get_default_classloader,[
  m(ro,claz_object,default_class_loader)]).

data_record(claz_argument_list_processor,[
  m(rw,array_of(cla__param),required_parameters),
  m(rw,array_of(cla__param),optional_parameters),
  m(rw,array_of(cla__keyword_param),keyword_parameters),
  m(rw,array_of(cla__param),aux_vars),
  m(rw,array_of(cla__param),positional_parameters),
  m(rw,claz_symbol,rest_var),
  m(rw,cla__param,rest_param),
  m(rw,claz_symbol,env_var),
  m(rw,cla__param,env_param),
  m(rw,integer,arity),
  m(rw,integer,min_args),
  m(rw,integer,max_args),
  m(rw,array_of(claz_symbol),variables),
  m(rw,array_of(claz_boolean),specials),
  m(rw,claz_boolean,and_key),
  m(rw,claz_boolean,allow_other_keys),
  m(ro,cla__argument_matcher,matcher),
  m(rw,claz_boolean,matcher_needs_env),
  m(rw,claz_operator,function)]).

data_record(claz_fasl_class_loader,[
  m(ro,claz_string,base_name),
  m(ro,claz_ffi_object,boxed_this)]).


data_record(claz_random_access_output_stream,[
  m(rw,array_of(unsigned_byte8),write_buf),
  m(ro,claz_random_access_character_file,this__0)]).

data_record(claz_finalizer___finalizing_weak_reference,[
  m(rw,claz_sys_linked_list(claz_sys_runnable),finalizers)]).

data_record(claz_random_access_writer,[
  m(ro,claz_random_access_character_file,this__0)]).

data_record(claz_random_access_reader,[
  m(rw,array_of(char_code),read_buf),
  m(ro,claz_random_access_character_file,this__0)]).


data_record(claz_fill_pointer_output_stream___writer,[
  m(ro,claz_fill_pointer_output_stream,this__0)]).
data_record(claz_memory_class_loader,[
  m(ro,claz_hash_table(claz_string,claz_ffi_object),hashtable),
  m(ro,claz_ffi_object,boxed_this),
  m(ro,claz_string,internal_name_prefix)]).


data_record(claz_jar_stream,[
  m(ro,claz_pathname,pathname),
  m(ro,claz_sys_input_stream,input),
  m(ro,claz_sys_reader,reader),
  m(ro,integer,bytes_per_unit)]).


data_record(claz_sys_weak_hash_entry_weak_value,[
  m(rw,claz_sys_weak_reference(claz_object),value),
  m(ro,claz_weak_hash_table,this__0)]).

data_record(claz_claz_sys_proxy_lisp_invocation_handler,[
  m(rw,claz_function,function)]).

data_record(claz_runtime_class,[
  m(rw,claz_sys_map(claz_string,claz_function),methods)]).

data_record(claz_random_access_input_stream,[
  m(rw,array_of(unsigned_byte8),read_buf),
  m(ro,claz_random_access_character_file,this__0)]).


data_record(claz_stack_frame,[
  m(rw,claz_stack_frame,next),
  m(rw,claz_environment,env)]).


data_record(claz_special_binding,[
  m(ro,integer,idx),
  m(rw,claz_object,value)]).


