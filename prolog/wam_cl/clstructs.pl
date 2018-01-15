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
:- include(header).

% :- use_module(library(pfc)).


to_prolog_string_if_string(LStrKey0,LStrKey):- is_stringp(LStrKey0),!,to_prolog_string(LStrKey0,LStrKey),!.
to_prolog_string_if_string(O,O).

%;;(ADD-CI "BUILT-IN-CLASS" T "class_symbol" T)
%;;(ADD-CI "BUILT-IN-CLASS" T "method" (SETF CLASS-NAME) :NAME (SETF CLASS-NAME))
sf_sys_add_ci(_Str,Claz,LStrKey0,LspValue,RetVal):-  
  to_prolog_string_if_string(LStrKey0,LStrKey),
  assertz(sf_sys_add_ci2(Claz,LStrKey,LspValue,RetVal)).
sf_sys_add_ci(_Str,Claz,LStrKey0,LspObj,LspKey,LspValue,RetVal):-
  to_prolog_string_if_string(LStrKey0,LStrKey),
  assertz(sf_sys_add_ci2(Claz,LStrKey,LspObj,LspKey,LspValue,RetVal)).

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

data_record(cla__rest_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special)]).

data_record(claz_ffi_stack_frame,[
  m(ro,claz_sys_stack_trace_element,ffi_frame)]).

data_record(claz_bignum,[
  m(ro,claz_sys_big_integer,value)]).

data_record(claz_complex_bit_vector,[
  m(rw,integer,fill_pointer),
  m(rw,claz_boolean,is_displaced),
  m(rw,claz_array,array),
  m(rw,integer,displacement)]).

data_record(cla__slow_matcher,[
  m(ro,claz_argument_list_processor,this__0)]).

data_record(cla__aux_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special),
  m(rw,cla__init_form,initform)]).

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

data_record(claz_condition,[
  m(rw,claz_string,message)]).

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
  m(rw,array_of(claz_hash_table___hash_entry),buckets),
  m(rw,integer,count),
  m(ro,claz_hash_table___comparator,comparator),
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



data_record(claz_sys_weak_hash_entry_weak_key_or_value,[
  m(ro,claz_weak_hash_table,this__0)]).

data_record(claz_synonym_stream,[
  m(ro,claz_symbol,stream_name)]).

data_record(cla__constant_init_form,[
  m(rw,claz_object,value)]).

data_record(claz_lisp_thread___stack_segment,[
  m(ro,array_of(claz_sys_object),stack),
  m(ro,claz_lisp_thread___stack_segment,next),
  m(rw,integer,stack_ptr)]).

data_record(claz_socket_stream,[
  m(ro,claz_sys_socket,socket)]).

data_record(claz_memory_class_loader,[
  m(ro,claz_hash_table(claz_string,claz_ffi_object),hashtable),
  m(ro,claz_ffi_object,boxed_this),
  m(ro,claz_string,internal_name_prefix)]).

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

data_record(cla__fast_matcher,[
  m(ro,claz_argument_list_processor,this__0)]).

data_record(claz_go,[
  m(ro,claz_object,tagbody),
  m(ro,claz_object,tag)]).

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

data_record(claz_string_functions___string_indices_and_chars,[
  m(rw,claz_string,string1),
  m(rw,claz_boolean,convert_case),
  m(rw,array_of(char_code),array1),
  m(rw,array_of(char_code),array2),
  m(rw,integer,start1),
  m(rw,integer,end1),
  m(rw,integer,start2),
  m(rw,integer,end2)]).

data_record(claz_symbol,[
  m(ro,claz_simple_string,name),
  m(rw,integer,hash),
  m(rw,integer,special_index),
  m(rw,claz_object,pkg),
  m(rw,claz_object,value),
  m(rw,claz_object,function),
  m(rw,claz_list,property_list),
  m(rw,bitmask,flags)]).

data_record(claz_claz_sys_handler___entry,[
  m(rw,claz_function,handler),
  m(rw,claz_object,data),
  m(rw,integer,count),
  m(rw,claz_sys_map(claz_string,claz_claz_sys_handler___entry),entry_table),
  m(rw,claz_string,event)]).

data_record(claz_operator,[
  m(rw,claz_object,lambda_name),
  m(rw,claz_object,lambda_list)]).

data_record(claz_function_binding,[
  m(rw,claz_object,name),
  m(rw,claz_object,value),
  m(ro,claz_function_binding,next)]).

data_record(claz_capitalize_stream,[
  m(rw,claz_boolean,in_word)]).

data_record(claz_special_binding,[
  m(ro,integer,idx),
  m(rw,claz_object,value)]).

data_record(claz_basic_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,array_of(Kind),elements)]).
data_record(claz_simple_vector,[
  m(rw,integer,capacity),
  m(rw,claz_list,data)]).

data_record(cla__optional_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special),
  m(rw,claz_symbol,supplied_var),
  m(rw,claz_boolean,supplied_special),
  m(rw,cla__init_form,init_form)]).

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

data_record(claz_ffi_object__2,[
  m(ro,claz_list,val__acc),
  m(ro,claz_object,val__fn)]).

data_record(claz_shell_command___reader_thread,[
  m(rw,array_of(char_code),buf),
  m(ro,claz_sys_input_stream,input_stream),
  m(ro,claz_sys_buffered_reader,reader),                                  
  m(ro,claz_shell_command,this__0),
  m(rw,claz_boolean,done)]).

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

data_record(cla__keyword_param,[
  m(rw,claz_symbol,keyword)]).

data_record(claz_weak_hash_table,[
  m(ro,claz_object,rehash_size),
  m(ro,claz_object,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(claz_sys_weak_hash_entry),buckets),
  m(rw,integer,count),
  m(ro,claz_weak_hash_table___comparator,comparator),
  m(ro,j_reentrant_lock,lock),
  m(rw,claz_sys_weak_hash_entry,bucket_type),
  m(ro,claz_object,weakness),
  m(rw,claz_sys_reference_queue(claz_object),queue),
  m(rw,claz_sys_map(claz_sys_reference,claz_sys_weak_hash_entry),entry_lookup)]).

data_record(claz_case_frob_stream,[
  m(ro,claz_stream,target)]).

data_record(claz_complex,[
  m(ro,claz_object,realpart),
  m(ro,claz_object,imagpart)]).

data_record(claz_fill_pointer_output_stream___writer,[
  m(ro,claz_fill_pointer_output_stream,this__0)]).

data_record(claz_jar_stream,[
  m(ro,claz_pathname,pathname),
  m(ro,claz_sys_input_stream,input),
  m(ro,claz_sys_reader,reader),
  m(ro,integer,bytes_per_unit)]).

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

data_record(claz_primitives__pf_finalize__1,[
  m(rw,claz_sys_thread,thread),
  m(ro,claz_object,val__fun),
  m(ro,claz_primitives__pf_finalize,this__0)]).

data_record(cla__environment_param,[
  m(rw,claz_symbol,var),
  m(rw,claz_boolean,special)]).

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

data_record(claz_sys_weak_hash_entry_weak_value,[
  m(rw,claz_sys_weak_reference(claz_object),value),
  m(ro,claz_weak_hash_table,this__0)]).

data_record(claz_claz_sys_proxy_lisp_invocation_handler,[
  m(rw,claz_function,function)]).

data_record(claz_runtime_class,[
  m(rw,claz_sys_map(claz_string,claz_function),methods)]).

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

data_record(claz_zip_cache___entry,[
  m(rw,long,last_modified),
  m(rw,j_zip_file,file)]).

data_record(clzip___directories,[
  m(rw,j_zip_output_stream,out)]).

data_record(claz_wrong_number_of_arguments_exception,[
  m(rw,claz_operator,operator),
  m(rw,integer,expected_min_args),
  m(rw,integer,expected_max_args),
  m(rw,claz_object,actual_args),
  m(rw,claz_string,message)]).

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

data_record(claz_finalizer___finalizing_weak_reference,[
  m(rw,claz_sys_linked_list(claz_sys_runnable),finalizers)]).

data_record(claz_random_access_writer,[
  m(ro,claz_random_access_character_file,this__0)]).

data_record(claz_random_access_reader,[
  m(rw,array_of(char_code),read_buf),
  m(ro,claz_random_access_character_file,this__0)]).

data_record(claz_racf_malformed_input_exception,[
  m(ro,integer,position),
  m(ro,char_code,character),
  m(ro,claz_string,charset_name)]).

data_record(claz_random_access_output_stream,[
  m(rw,array_of(unsigned_byte8),write_buf),
  m(ro,claz_random_access_character_file,this__0)]).

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

data_record(claz_random_access_input_stream,[
  m(rw,array_of(unsigned_byte8),read_buf),
  m(ro,claz_random_access_character_file,this__0)]).


data_record(claz_ffi_object__1,[
  m(ro,claz_list,val__acc),
  m(ro,claz_ffi_object,this__0)]).

data_record(claz_stack_frame,[
  m(rw,claz_stack_frame,next),
  m(rw,claz_environment,env)]).

data_record(claz_single_float,[
  m(ro,float,value)]).

data_record(claz_compiled_closure,[
  m(rw,array_of(claz_closure_binding),ctx)]).

data_record(claz_special_operator,[
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

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

data_record(claz_hash_table___hash_entry,[
  m(rw,claz_object,key),
  m(rw,integer,hash),
  m(rw,claz_object,value),
  m(rw,claz_hash_table___hash_entry,next)]).

data_record(claz_lisp_thread___stack_marker,[
  m(ro,integer,num_args)]).


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

data_record(claz_profiler__1__1,[
  m(rw,claz_sys_thread,thread),
  m(ro,claz_lisp_thread,val__thread),
  m(ro,claz_profiler__1,this__0)]).

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

true.

?- listing(sf_sys_add_ci2).
:- dynamic sf_sys_add_ci2/4.

sf_sys_add_ci2(array, class_name, array).
sf_sys_add_ci2(array, sys_class_precedence_list, [t]).
sf_sys_add_ci2(array, "subclass", simple_array).
sf_sys_add_ci2(array, "subclass", vector).
sf_sys_add_ci2(array, type_of, built_in_class).
sf_sys_add_ci2(base_string, class_name, base_string).
sf_sys_add_ci2(base_string, sys_class_precedence_list, [string, vector, array, sequence, t]).
sf_sys_add_ci2(base_string, sys_class_precedence_list, [string, vector, array, sequence, t]).
sf_sys_add_ci2(base_string, "subclass", simple_base_string).
sf_sys_add_ci2(base_string, type_of, built_in_class).
sf_sys_add_ci2(bignum, class_name, bignum).
sf_sys_add_ci2(bignum, sys_class_precedence_list, [integer, rational, real, number, t]).
sf_sys_add_ci2(bignum, type_of, built_in_class).
sf_sys_add_ci2(bit_vector, class_name, bit_vector).
sf_sys_add_ci2(bit_vector, sys_class_precedence_list, [vector, array, sequence, t]).
sf_sys_add_ci2(bit_vector, "subclass", simple_bit_vector).
sf_sys_add_ci2(bit_vector, type_of, built_in_class).
sf_sys_add_ci2(broadcast_stream, class_name, broadcast_stream).
sf_sys_add_ci2(broadcast_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(broadcast_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(broadcast_stream, type_of, built_in_class).
sf_sys_add_ci2(character, class_name, character).
sf_sys_add_ci2(character, sys_class_precedence_list, [t]).
sf_sys_add_ci2(character, type_of, built_in_class).
sf_sys_add_ci2(complex, class_name, complex).
sf_sys_add_ci2(complex, sys_class_precedence_list, [number, t]).
sf_sys_add_ci2(complex, type_of, built_in_class).
sf_sys_add_ci2(concatenated_stream, class_name, concatenated_stream).
sf_sys_add_ci2(concatenated_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(concatenated_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(concatenated_stream, type_of, built_in_class).
sf_sys_add_ci2(cons, class_name, cons).
sf_sys_add_ci2(cons, sys_class_precedence_list, [list, sequence, t]).
sf_sys_add_ci2(cons, type_of, built_in_class).
sf_sys_add_ci2(double_float, class_name, double_float).
sf_sys_add_ci2(double_float, sys_class_precedence_list, [float, real, number, t]).
sf_sys_add_ci2(double_float, type_of, built_in_class).
sf_sys_add_ci2(echo_stream, class_name, echo_stream).
sf_sys_add_ci2(echo_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(echo_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(echo_stream, type_of, built_in_class).
sf_sys_add_ci2(sys_ansi_stream, "subclass", broadcast_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", concatenated_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", echo_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", file_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", string_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", synonym_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", two_way_stream).
sf_sys_add_ci2(file_stream, class_name, file_stream).
sf_sys_add_ci2(file_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(file_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(file_stream, type_of, built_in_class).
sf_sys_add_ci2(fixnum, class_name, fixnum).
sf_sys_add_ci2(fixnum, sys_class_precedence_list, [integer, rational, real, number, t]).
sf_sys_add_ci2(fixnum, type_of, built_in_class).
sf_sys_add_ci2(float, class_name, float).
sf_sys_add_ci2(float, sys_class_precedence_list, [real, number, t]).
sf_sys_add_ci2(float, "subclass", double_float).
sf_sys_add_ci2(float, "subclass", single_float).
sf_sys_add_ci2(float, type_of, built_in_class).
sf_sys_add_ci2(function, class_name, function).
sf_sys_add_ci2(function, sys_class_precedence_list, [t]).
sf_sys_add_ci2(function, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(function, type_of, built_in_class).
sf_sys_add_ci2(hash_table, class_name, hash_table).
sf_sys_add_ci2(hash_table, sys_class_precedence_list, [t]).
sf_sys_add_ci2(hash_table, type_of, built_in_class).
sf_sys_add_ci2(integer, class_name, integer).
sf_sys_add_ci2(integer, sys_class_precedence_list, [rational, real, number, t]).
sf_sys_add_ci2(integer, "subclass", bignum).
sf_sys_add_ci2(integer, "subclass", fixnum).
sf_sys_add_ci2(integer, type_of, built_in_class).
sf_sys_add_ci2(keyword, class_name, keyword).
sf_sys_add_ci2(keyword, sys_class_precedence_list, [symbol, t]).
sf_sys_add_ci2(keyword, type_of, built_in_class).
sf_sys_add_ci2(list, class_name, list).
sf_sys_add_ci2(list, sys_class_precedence_list, [sequence, t]).
sf_sys_add_ci2(list, "subclass", cons).
sf_sys_add_ci2(list, "subclass", null).
sf_sys_add_ci2(list, type_of, built_in_class).
sf_sys_add_ci2(logical_pathname, class_name, logical_pathname).
sf_sys_add_ci2(logical_pathname, sys_class_precedence_list, [pathname, t]).
sf_sys_add_ci2(logical_pathname, type_of, built_in_class).
sf_sys_add_ci2(null, class_name, null).
sf_sys_add_ci2(null, sys_class_precedence_list, [symbol, list, sequence, t]).
sf_sys_add_ci2(null, type_of, built_in_class).
sf_sys_add_ci2(number, class_name, number).
sf_sys_add_ci2(number, sys_class_precedence_list, [t]).
sf_sys_add_ci2(number, "subclass", complex).
sf_sys_add_ci2(number, "subclass", real).
sf_sys_add_ci2(number, type_of, built_in_class).
sf_sys_add_ci2(package, class_name, package).
sf_sys_add_ci2(package, sys_class_precedence_list, [t]).
sf_sys_add_ci2(package, type_of, built_in_class).
sf_sys_add_ci2(pathname, class_name, pathname).
sf_sys_add_ci2(pathname, sys_class_precedence_list, [t]).
sf_sys_add_ci2(pathname, "subclass", logical_pathname).
sf_sys_add_ci2(pathname, type_of, built_in_class).
sf_sys_add_ci2(random_state, class_name, random_state).
sf_sys_add_ci2(random_state, sys_class_precedence_list, [t]).
sf_sys_add_ci2(random_state, type_of, built_in_class).
sf_sys_add_ci2(ratio, class_name, ratio).
sf_sys_add_ci2(ratio, sys_class_precedence_list, [rational, real, number, t]).
sf_sys_add_ci2(ratio, type_of, built_in_class).
sf_sys_add_ci2(rational, class_name, rational).
sf_sys_add_ci2(rational, sys_class_precedence_list, [real, number, t]).
sf_sys_add_ci2(rational, "subclass", integer).
sf_sys_add_ci2(rational, "subclass", ratio).
sf_sys_add_ci2(rational, type_of, built_in_class).
sf_sys_add_ci2(readtable, class_name, readtable).
sf_sys_add_ci2(readtable, sys_class_precedence_list, [t]).
sf_sys_add_ci2(readtable, type_of, built_in_class).
sf_sys_add_ci2(real, class_name, real).
sf_sys_add_ci2(real, sys_class_precedence_list, [number, t]).
sf_sys_add_ci2(real, "subclass", float).
sf_sys_add_ci2(real, "subclass", rational).
sf_sys_add_ci2(real, type_of, built_in_class).
sf_sys_add_ci2(sequence, class_name, sequence).
sf_sys_add_ci2(sequence, sys_class_precedence_list, [t]).
sf_sys_add_ci2(sequence, "subclass", list).
sf_sys_add_ci2(sequence, "subclass", vector).
sf_sys_add_ci2(sequence, type_of, built_in_class).
sf_sys_add_ci2(sys_weak_pointer, class_name, sys_weak_pointer).
sf_sys_add_ci2(sys_weak_pointer, sys_class_precedence_list, [t]).
sf_sys_add_ci2(sys_weak_pointer, type_of, built_in_class).
sf_sys_add_ci2(simple_array, class_name, simple_array).
sf_sys_add_ci2(simple_array, sys_class_precedence_list, [array, t]).
sf_sys_add_ci2(simple_array, "subclass", simple_bit_vector).
sf_sys_add_ci2(simple_array, "subclass", simple_string).
sf_sys_add_ci2(simple_array, "subclass", simple_vector).
sf_sys_add_ci2(simple_array, type_of, built_in_class).
sf_sys_add_ci2(simple_base_string, class_name, simple_base_string).
sf_sys_add_ci2(simple_base_string, sys_class_precedence_list, [base_string, simple_string, string, vector, simple_array, array, sequence, t]).
sf_sys_add_ci2(simple_base_string, type_of, built_in_class).
sf_sys_add_ci2(simple_bit_vector, class_name, simple_bit_vector).
sf_sys_add_ci2(simple_bit_vector, sys_class_precedence_list, [bit_vector, vector, simple_array, array, sequence, t]).
sf_sys_add_ci2(simple_bit_vector, type_of, built_in_class).
sf_sys_add_ci2(simple_string, class_name, simple_string).
sf_sys_add_ci2(simple_string, sys_class_precedence_list, [string, vector, simple_array, array, sequence, t]).
sf_sys_add_ci2(simple_string, "subclass", simple_base_string).
sf_sys_add_ci2(simple_string, type_of, built_in_class).
sf_sys_add_ci2(simple_vector, class_name, simple_vector).
sf_sys_add_ci2(simple_vector, sys_class_precedence_list, [vector, simple_array, array, sequence, t]).
sf_sys_add_ci2(simple_vector, type_of, built_in_class).
sf_sys_add_ci2(single_float, class_name, single_float).
sf_sys_add_ci2(single_float, sys_class_precedence_list, [float, real, number, t]).
sf_sys_add_ci2(single_float, type_of, built_in_class).
sf_sys_add_ci2(stream, class_name, stream).
sf_sys_add_ci2(stream, sys_class_precedence_list, [t]).
sf_sys_add_ci2(stream, "subclass", broadcast_stream).
sf_sys_add_ci2(stream, "subclass", concatenated_stream).
sf_sys_add_ci2(stream, "subclass", echo_stream).
sf_sys_add_ci2(stream, "subclass", file_stream).
sf_sys_add_ci2(stream, "subclass", string_stream).
sf_sys_add_ci2(stream, "subclass", synonym_stream).
sf_sys_add_ci2(stream, "subclass", two_way_stream).
sf_sys_add_ci2(stream, type_of, built_in_class).
sf_sys_add_ci2(string, class_name, string).
sf_sys_add_ci2(string, sys_class_precedence_list, [vector, array, sequence, t]).
sf_sys_add_ci2(string, "subclass", base_string).
sf_sys_add_ci2(string, "subclass", simple_string).
sf_sys_add_ci2(string, type_of, built_in_class).
sf_sys_add_ci2(string_stream, class_name, string_stream).
sf_sys_add_ci2(string_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(string_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(string_stream, type_of, built_in_class).
sf_sys_add_ci2(symbol, class_name, symbol).
sf_sys_add_ci2(symbol, sys_class_precedence_list, [t]).
sf_sys_add_ci2(symbol, "subclass", keyword).
sf_sys_add_ci2(symbol, "subclass", null).
sf_sys_add_ci2(symbol, type_of, built_in_class).
sf_sys_add_ci2(synonym_stream, class_name, synonym_stream).
sf_sys_add_ci2(synonym_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(synonym_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(synonym_stream, type_of, built_in_class).
sf_sys_add_ci2(t, class_name, t).
sf_sys_add_ci2(t, "subclass", array).
sf_sys_add_ci2(t, "subclass", character).
sf_sys_add_ci2(t, "subclass", function).
sf_sys_add_ci2(t, "subclass", hash_table).
sf_sys_add_ci2(t, "subclass", number).
sf_sys_add_ci2(t, "subclass", package).
sf_sys_add_ci2(t, "subclass", pathname).
sf_sys_add_ci2(t, "subclass", random_state).
sf_sys_add_ci2(t, "subclass", readtable).
sf_sys_add_ci2(t, "subclass", sequence).
sf_sys_add_ci2(t, "subclass", sys_code_block).
sf_sys_add_ci2(t, "subclass", sys_foreign_data).
sf_sys_add_ci2(t, "subclass", sys_frame).
sf_sys_add_ci2(t, "subclass", sys_simd_pack).
sf_sys_add_ci2(t, "subclass", sys_system_area_pointer).
sf_sys_add_ci2(t, "subclass", sys_weak_pointer).
sf_sys_add_ci2(t, "subclass", standard_object).
sf_sys_add_ci2(t, "subclass", stream).
sf_sys_add_ci2(t, "subclass", structure_object).
sf_sys_add_ci2(t, "subclass", symbol).
sf_sys_add_ci2(t, type_of, built_in_class).
sf_sys_add_ci2(two_way_stream, class_name, two_way_stream).
sf_sys_add_ci2(two_way_stream, sys_class_precedence_list, [sys_ansi_stream, stream, t]).
sf_sys_add_ci2(two_way_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(two_way_stream, type_of, built_in_class).
sf_sys_add_ci2(vector, class_name, vector).
sf_sys_add_ci2(vector, sys_class_precedence_list, [array, sequence, t]).
sf_sys_add_ci2(vector, "subclass", base_string).
sf_sys_add_ci2(vector, "subclass", bit_vector).
sf_sys_add_ci2(vector, "subclass", simple_vector).
sf_sys_add_ci2(vector, "subclass", string).
sf_sys_add_ci2(vector, type_of, built_in_class).
sf_sys_add_ci2(arithmetic_error, class_name, arithmetic_error).
sf_sys_add_ci2(arithmetic_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(arithmetic_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(arithmetic_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(arithmetic_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(arithmetic_error, "subclass", division_by_zero).
sf_sys_add_ci2(arithmetic_error, "subclass", floating_point_inexact).
sf_sys_add_ci2(arithmetic_error, "subclass", floating_point_invalid_operation).
sf_sys_add_ci2(arithmetic_error, "subclass", floating_point_overflow).
sf_sys_add_ci2(arithmetic_error, "subclass", floating_point_underflow).
sf_sys_add_ci2(arithmetic_error, "subclass", sys_simple_arithmetic_error).
sf_sys_add_ci2(arithmetic_error, type_of, sys_condition_class).
sf_sys_add_ci2(arithmetic_error, type_of, standard_class).
sf_sys_add_ci2(cell_error, class_name, cell_error).
sf_sys_add_ci2(cell_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(cell_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(cell_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(cell_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(cell_error, "subclass", sys_simple_cell_error).
sf_sys_add_ci2(cell_error, "subclass", unbound_slot).
sf_sys_add_ci2(cell_error, "subclass", unbound_variable).
sf_sys_add_ci2(cell_error, "subclass", undefined_function).
sf_sys_add_ci2(cell_error, type_of, sys_condition_class).
sf_sys_add_ci2(cell_error, type_of, standard_class).
sf_sys_add_ci2(condition, class_name, condition).
sf_sys_add_ci2(condition, sys_class_precedence_list, [sys_slot_object, t]).
sf_sys_add_ci2(condition, "subclass", serious_condition).
sf_sys_add_ci2(condition, "subclass", sys_compiler_note).
sf_sys_add_ci2(condition, "subclass", sys_step_condition).
sf_sys_add_ci2(condition, "subclass", sys_system_condition).
sf_sys_add_ci2(condition, "subclass", simple_condition).
sf_sys_add_ci2(condition, "subclass", warning).
sf_sys_add_ci2(condition, type_of, sys_condition_class).
sf_sys_add_ci2(control_error, class_name, control_error).
sf_sys_add_ci2(control_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(control_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(control_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(control_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(control_error, "subclass", sys_abort_failure).
sf_sys_add_ci2(control_error, "subclass", sys_simple_control_error).
sf_sys_add_ci2(control_error, type_of, sys_condition_class).
sf_sys_add_ci2(control_error, type_of, standard_class).
sf_sys_add_ci2(division_by_zero, class_name, division_by_zero).
sf_sys_add_ci2(division_by_zero, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(division_by_zero, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(division_by_zero, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(division_by_zero, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(division_by_zero, "subclass", sys_simple_division_by_zero).
sf_sys_add_ci2(division_by_zero, type_of, sys_condition_class).
sf_sys_add_ci2(division_by_zero, type_of, standard_class).
sf_sys_add_ci2(end_of_file, class_name, end_of_file).
sf_sys_add_ci2(end_of_file, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(end_of_file, sys_class_precedence_list, [stream_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(end_of_file, sys_class_precedence_list, [stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(end_of_file, sys_class_precedence_list, [stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(end_of_file, "subclass", sys_simple_end_of_file).
sf_sys_add_ci2(end_of_file, type_of, sys_condition_class).
sf_sys_add_ci2(end_of_file, type_of, standard_class).
sf_sys_add_ci2(error, class_name, error).
sf_sys_add_ci2(error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(error, sys_class_precedence_list, [serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(error, sys_class_precedence_list, [serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(error, sys_class_precedence_list, [serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(error, "subclass", arithmetic_error).
sf_sys_add_ci2(error, "subclass", cell_error).
sf_sys_add_ci2(error, "subclass", control_error).
sf_sys_add_ci2(error, "subclass", file_error).
sf_sys_add_ci2(error, "subclass", package_error).
sf_sys_add_ci2(error, "subclass", parse_error).
sf_sys_add_ci2(error, "subclass", print_not_readable).
sf_sys_add_ci2(error, "subclass", program_error).
sf_sys_add_ci2(error, "subclass", sys_cpl_protocol_violation).
sf_sys_add_ci2(error, "subclass", sys_instance_structure_protocol_error).
sf_sys_add_ci2(error, "subclass", sys_invalid_superclass).
sf_sys_add_ci2(error, "subclass", sys_missing_load_form).
sf_sys_add_ci2(error, "subclass", sys_new_value_specialization).
sf_sys_add_ci2(error, "subclass", sys_no_primary_method).
sf_sys_add_ci2(error, "subclass", sys_obsolete_structure).
sf_sys_add_ci2(error, "subclass", sys_slotd_initialization_error).
sf_sys_add_ci2(error, "subclass", sys_breakpoint_error).
sf_sys_add_ci2(error, "subclass", sys_compiler_error).
sf_sys_add_ci2(error, "subclass", sys_defconstant_uneql).
sf_sys_add_ci2(error, "subclass", sys_deprecation_error).
sf_sys_add_ci2(error, "subclass", sys_invalid_fasl).
sf_sys_add_ci2(error, "subclass", sys_memory_fault_error).
sf_sys_add_ci2(error, "subclass", simple_error).
sf_sys_add_ci2(error, "subclass", stream_error).
sf_sys_add_ci2(error, "subclass", type_error).
sf_sys_add_ci2(error, type_of, sys_condition_class).
sf_sys_add_ci2(error, type_of, standard_class).
sf_sys_add_ci2(sys_os_error, "subclass", sys_simple_os_error).
sf_sys_add_ci2(sys_source_program_error, "subclass", sys_simple_source_program_error).
sf_sys_add_ci2(file_error, class_name, file_error).
sf_sys_add_ci2(file_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(file_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(file_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(file_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(file_error, "subclass", sys_simple_file_error).
sf_sys_add_ci2(file_error, type_of, sys_condition_class).
sf_sys_add_ci2(file_error, type_of, standard_class).
sf_sys_add_ci2(floating_point_inexact, class_name, floating_point_inexact).
sf_sys_add_ci2(floating_point_inexact, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(floating_point_inexact, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(floating_point_inexact, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_inexact, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_inexact, type_of, sys_condition_class).
sf_sys_add_ci2(floating_point_inexact, type_of, standard_class).
sf_sys_add_ci2(floating_point_invalid_operation, class_name, floating_point_invalid_operation).
sf_sys_add_ci2(floating_point_invalid_operation, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(floating_point_invalid_operation, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(floating_point_invalid_operation, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_invalid_operation, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_invalid_operation, type_of, sys_condition_class).
sf_sys_add_ci2(floating_point_invalid_operation, type_of, standard_class).
sf_sys_add_ci2(floating_point_overflow, class_name, floating_point_overflow).
sf_sys_add_ci2(floating_point_overflow, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(floating_point_overflow, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(floating_point_overflow, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_overflow, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_overflow, "subclass", sys_simple_floating_point_overflow).
sf_sys_add_ci2(floating_point_overflow, type_of, sys_condition_class).
sf_sys_add_ci2(floating_point_overflow, type_of, standard_class).
sf_sys_add_ci2(floating_point_underflow, class_name, floating_point_underflow).
sf_sys_add_ci2(floating_point_underflow, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(floating_point_underflow, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(floating_point_underflow, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_underflow, sys_class_precedence_list, [arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(floating_point_underflow, "subclass", sys_simple_floating_point_underflow).
sf_sys_add_ci2(floating_point_underflow, type_of, sys_condition_class).
sf_sys_add_ci2(floating_point_underflow, type_of, standard_class).
sf_sys_add_ci2(package_error, class_name, package_error).
sf_sys_add_ci2(package_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(package_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(package_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(package_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(package_error, "subclass", sys_simple_package_error).
sf_sys_add_ci2(package_error, "subclass", sys_name_conflict).
sf_sys_add_ci2(package_error, "subclass", sys_package_lock_violation).
sf_sys_add_ci2(package_error, type_of, sys_condition_class).
sf_sys_add_ci2(package_error, type_of, standard_class).
sf_sys_add_ci2(parse_error, class_name, parse_error).
sf_sys_add_ci2(parse_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(parse_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(parse_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(parse_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(parse_error, "subclass", reader_error).
sf_sys_add_ci2(parse_error, "subclass", sys_simple_parse_error).
sf_sys_add_ci2(parse_error, type_of, sys_condition_class).
sf_sys_add_ci2(parse_error, type_of, standard_class).
sf_sys_add_ci2(print_not_readable, class_name, print_not_readable).
sf_sys_add_ci2(print_not_readable, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(print_not_readable, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(print_not_readable, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(print_not_readable, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(print_not_readable, "subclass", sys_simple_print_not_readable).
sf_sys_add_ci2(print_not_readable, type_of, sys_condition_class).
sf_sys_add_ci2(print_not_readable, type_of, standard_class).
sf_sys_add_ci2(program_error, class_name, program_error).
sf_sys_add_ci2(program_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(program_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(program_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(program_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(program_error, "subclass", sys_argument_list_dotted).
sf_sys_add_ci2(program_error, "subclass", sys_initarg_error).
sf_sys_add_ci2(program_error, "subclass", sys_keyword_error).
sf_sys_add_ci2(program_error, "subclass", sys_simple_program_error).
sf_sys_add_ci2(program_error, type_of, sys_condition_class).
sf_sys_add_ci2(program_error, type_of, standard_class).
sf_sys_add_ci2(reader_error, class_name, reader_error).
sf_sys_add_ci2(reader_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(reader_error, sys_class_precedence_list, [parse_error, stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(reader_error, sys_class_precedence_list, [parse_error, stream_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(reader_error, sys_class_precedence_list, [parse_error, stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(reader_error, sys_class_precedence_list, [parse_error, stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(reader_error, "subclass", sys_simple_reader_error).
sf_sys_add_ci2(reader_error, type_of, sys_condition_class).
sf_sys_add_ci2(reader_error, type_of, standard_class).
sf_sys_add_ci2(serious_condition, class_name, serious_condition).
sf_sys_add_ci2(serious_condition, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(serious_condition, sys_class_precedence_list, [condition, sys_slot_object, t]).
sf_sys_add_ci2(serious_condition, sys_class_precedence_list, [condition, standard_object, t]).
sf_sys_add_ci2(serious_condition, "subclass", error).
sf_sys_add_ci2(serious_condition, "subclass", sys_interrupt_condition).
sf_sys_add_ci2(serious_condition, "subclass", sys_simple_serious_condition).
sf_sys_add_ci2(serious_condition, "subclass", sys_interactive_interrupt).
sf_sys_add_ci2(serious_condition, "subclass", sys_timeout).
sf_sys_add_ci2(serious_condition, "subclass", storage_condition).
sf_sys_add_ci2(serious_condition, type_of, sys_condition_class).
sf_sys_add_ci2(serious_condition, type_of, standard_class).
sf_sys_add_ci2(sys_abort_failure, class_name, sys_abort_failure).
sf_sys_add_ci2(sys_abort_failure, sys_class_precedence_list, [control_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_abort_failure, type_of, standard_class).
sf_sys_add_ci2(sys_argument_list_dotted, class_name, sys_argument_list_dotted).
sf_sys_add_ci2(sys_argument_list_dotted, sys_class_precedence_list, [program_error, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_argument_list_dotted, "subclass", sys_simple_argument_list_dotted).
sf_sys_add_ci2(sys_argument_list_dotted, type_of, standard_class).
sf_sys_add_ci2(sys_case_failure, class_name, sys_case_failure).
sf_sys_add_ci2(sys_case_failure, sys_class_precedence_list, [type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_case_failure, type_of, standard_class).
sf_sys_add_ci2(sys_charset_type_error, class_name, sys_charset_type_error).
sf_sys_add_ci2(sys_charset_type_error, sys_class_precedence_list, [type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_charset_type_error, type_of, standard_class).
sf_sys_add_ci2(sys_compiler_unsupported_feature_error, class_name, sys_compiler_unsupported_feature_error).
sf_sys_add_ci2(sys_compiler_unsupported_feature_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(sys_compiler_unsupported_feature_error, sys_class_precedence_list, [sys_compiler_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_compiler_unsupported_feature_error, type_of, standard_class).
sf_sys_add_ci2(sys_cpl_protocol_violation, class_name, sys_cpl_protocol_violation).
sf_sys_add_ci2(sys_cpl_protocol_violation, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_cpl_protocol_violation, type_of, sys_condition_class).
sf_sys_add_ci2(sys_find_method_length_mismatch, class_name, sys_find_method_length_mismatch).
sf_sys_add_ci2(sys_find_method_length_mismatch, sys_class_precedence_list, [sys_reference_condition, simple_error, simple_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_find_method_length_mismatch, type_of, sys_condition_class).
sf_sys_add_ci2(sys_initarg_error, class_name, sys_initarg_error).
sf_sys_add_ci2(sys_initarg_error, sys_class_precedence_list, [sys_reference_condition, program_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_initarg_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_instance_structure_protocol_error, class_name, sys_instance_structure_protocol_error).
sf_sys_add_ci2(sys_instance_structure_protocol_error, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_instance_structure_protocol_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_interrupt_condition, class_name, sys_interrupt_condition).
sf_sys_add_ci2(sys_interrupt_condition, sys_class_precedence_list, [serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_interrupt_condition, "subclass", sys_simple_interrupt_condition).
sf_sys_add_ci2(sys_interrupt_condition, type_of, standard_class).
sf_sys_add_ci2(sys_invalid_superclass, class_name, sys_invalid_superclass).
sf_sys_add_ci2(sys_invalid_superclass, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_invalid_superclass, type_of, sys_condition_class).
sf_sys_add_ci2(sys_keyword_error, class_name, sys_keyword_error).
sf_sys_add_ci2(sys_keyword_error, sys_class_precedence_list, [program_error, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_keyword_error, "subclass", sys_simple_keyword_error).
sf_sys_add_ci2(sys_keyword_error, type_of, standard_class).
sf_sys_add_ci2(sys_long_method_combination_error, class_name, sys_long_method_combination_error).
sf_sys_add_ci2(sys_long_method_combination_error, sys_class_precedence_list, [sys_reference_condition, simple_error, simple_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_long_method_combination_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_metaobject_initialization_violation, class_name, sys_metaobject_initialization_violation).
sf_sys_add_ci2(sys_metaobject_initialization_violation, sys_class_precedence_list, [sys_reference_condition, simple_error, simple_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_metaobject_initialization_violation, type_of, sys_condition_class).
sf_sys_add_ci2(sys_missing_load_form, class_name, sys_missing_load_form).
sf_sys_add_ci2(sys_missing_load_form, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_missing_load_form, "subclass", sys_simple_missing_load_form).
sf_sys_add_ci2(sys_missing_load_form, type_of, standard_class).
sf_sys_add_ci2(sys_new_value_specialization, class_name, sys_new_value_specialization).
sf_sys_add_ci2(sys_new_value_specialization, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_new_value_specialization, type_of, sys_condition_class).
sf_sys_add_ci2(sys_no_primary_method, class_name, sys_no_primary_method).
sf_sys_add_ci2(sys_no_primary_method, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_no_primary_method, type_of, sys_condition_class).
sf_sys_add_ci2(sys_obsolete_structure, class_name, sys_obsolete_structure).
sf_sys_add_ci2(sys_obsolete_structure, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_obsolete_structure, type_of, sys_condition_class).
sf_sys_add_ci2(sys_print_object_stream_specializer, class_name, sys_print_object_stream_specializer).
sf_sys_add_ci2(sys_print_object_stream_specializer, sys_class_precedence_list, [sys_reference_condition, simple_warning, simple_condition, warning, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_print_object_stream_specializer, type_of, sys_condition_class).
sf_sys_add_ci2(sys_simple_argument_list_dotted, class_name, sys_simple_argument_list_dotted).
sf_sys_add_ci2(sys_simple_argument_list_dotted, sys_class_precedence_list, [simple_error, simple_condition, sys_argument_list_dotted, program_error, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_argument_list_dotted, type_of, standard_class).
sf_sys_add_ci2(sys_simple_arithmetic_error, class_name, sys_simple_arithmetic_error).
sf_sys_add_ci2(sys_simple_arithmetic_error, sys_class_precedence_list, [simple_error, simple_condition, arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_arithmetic_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_cell_error, class_name, sys_simple_cell_error).
sf_sys_add_ci2(sys_simple_cell_error, sys_class_precedence_list, [simple_error, simple_condition, cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_cell_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_clos_warning, class_name, sys_simple_clos_warning).
sf_sys_add_ci2(sys_simple_clos_warning, sys_class_precedence_list, [simple_condition, sys_clos_warning, warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_clos_warning, type_of, standard_class).
sf_sys_add_ci2(sys_simple_control_error, class_name, sys_simple_control_error).
sf_sys_add_ci2(sys_simple_control_error, sys_class_precedence_list, [simple_error, simple_condition, control_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_control_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_division_by_zero, class_name, sys_simple_division_by_zero).
sf_sys_add_ci2(sys_simple_division_by_zero, sys_class_precedence_list, [simple_error, simple_condition, division_by_zero, arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_division_by_zero, type_of, standard_class).
sf_sys_add_ci2(sys_simple_end_of_file, class_name, sys_simple_end_of_file).
sf_sys_add_ci2(sys_simple_end_of_file, sys_class_precedence_list, [simple_error, simple_condition, end_of_file, stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_end_of_file, type_of, standard_class).
sf_sys_add_ci2(sys_simple_file_error, class_name, sys_simple_file_error).
sf_sys_add_ci2(sys_simple_file_error, sys_class_precedence_list, [simple_error, simple_condition, file_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_file_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_floating_point_overflow, class_name, sys_simple_floating_point_overflow).
sf_sys_add_ci2(sys_simple_floating_point_overflow, sys_class_precedence_list, [simple_error, simple_condition, floating_point_overflow, arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_floating_point_overflow, type_of, standard_class).
sf_sys_add_ci2(sys_simple_floating_point_underflow, class_name, sys_simple_floating_point_underflow).
sf_sys_add_ci2(sys_simple_floating_point_underflow, sys_class_precedence_list, [simple_error, simple_condition, floating_point_underflow, arithmetic_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_floating_point_underflow, type_of, standard_class).
sf_sys_add_ci2(sys_simple_gf_already_called_warning, class_name, sys_simple_gf_already_called_warning).
sf_sys_add_ci2(sys_simple_gf_already_called_warning, sys_class_precedence_list, [simple_condition, sys_gf_already_called_warning, sys_clos_warning, warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_gf_already_called_warning, type_of, standard_class).
sf_sys_add_ci2(sys_simple_gf_replacing_method_warning, class_name, sys_simple_gf_replacing_method_warning).
sf_sys_add_ci2(sys_simple_gf_replacing_method_warning, sys_class_precedence_list, [simple_condition, sys_gf_replacing_method_warning, sys_clos_warning, warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_gf_replacing_method_warning, type_of, standard_class).
sf_sys_add_ci2(sys_simple_interrupt_condition, class_name, sys_simple_interrupt_condition).
sf_sys_add_ci2(sys_simple_interrupt_condition, sys_class_precedence_list, [simple_condition, sys_interrupt_condition, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_interrupt_condition, type_of, standard_class).
sf_sys_add_ci2(sys_simple_keyword_error, class_name, sys_simple_keyword_error).
sf_sys_add_ci2(sys_simple_keyword_error, sys_class_precedence_list, [simple_error, simple_condition, sys_keyword_error, program_error, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_keyword_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_missing_load_form, class_name, sys_simple_missing_load_form).
sf_sys_add_ci2(sys_simple_missing_load_form, sys_class_precedence_list, [simple_error, simple_condition, sys_missing_load_form, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_missing_load_form, type_of, standard_class).
sf_sys_add_ci2(sys_simple_os_error, class_name, sys_simple_os_error).
sf_sys_add_ci2(sys_simple_os_error, sys_class_precedence_list, [simple_error, simple_condition, sys_os_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_os_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_package_error, class_name, sys_simple_package_error).
sf_sys_add_ci2(sys_simple_package_error, sys_class_precedence_list, [simple_error, simple_condition, package_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_package_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_parse_error, class_name, sys_simple_parse_error).
sf_sys_add_ci2(sys_simple_parse_error, sys_class_precedence_list, [simple_error, simple_condition, parse_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_parse_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_print_not_readable, class_name, sys_simple_print_not_readable).
sf_sys_add_ci2(sys_simple_print_not_readable, sys_class_precedence_list, [simple_error, simple_condition, print_not_readable, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_print_not_readable, type_of, standard_class).
sf_sys_add_ci2(sys_simple_program_error, class_name, sys_simple_program_error).
sf_sys_add_ci2(sys_simple_program_error, sys_class_precedence_list, [simple_error, simple_condition, program_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_program_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_reader_error, class_name, sys_simple_reader_error).
sf_sys_add_ci2(sys_simple_reader_error, sys_class_precedence_list, [simple_error, simple_condition, reader_error, parse_error, stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_reader_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_serious_condition, class_name, sys_simple_serious_condition).
sf_sys_add_ci2(sys_simple_serious_condition, sys_class_precedence_list, [simple_condition, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_serious_condition, type_of, standard_class).
sf_sys_add_ci2(sys_simple_source_program_error, class_name, sys_simple_source_program_error).
sf_sys_add_ci2(sys_simple_source_program_error, sys_class_precedence_list, [simple_error, simple_condition, sys_source_program_error, program_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_source_program_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_storage_condition, class_name, sys_simple_storage_condition).
sf_sys_add_ci2(sys_simple_storage_condition, sys_class_precedence_list, [simple_condition, storage_condition, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_storage_condition, type_of, standard_class).
sf_sys_add_ci2(sys_simple_stream_error, class_name, sys_simple_stream_error).
sf_sys_add_ci2(sys_simple_stream_error, sys_class_precedence_list, [simple_error, simple_condition, stream_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_stream_error, type_of, standard_class).
sf_sys_add_ci2(sys_simple_style_warning, class_name, sys_simple_style_warning).
sf_sys_add_ci2(sys_simple_style_warning, sys_class_precedence_list, [simple_condition, style_warning, warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_style_warning, sys_class_precedence_list, [style_warning, warning, simple_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_style_warning, type_of, standard_class).
sf_sys_add_ci2(sys_simple_unbound_slot, class_name, sys_simple_unbound_slot).
sf_sys_add_ci2(sys_simple_unbound_slot, sys_class_precedence_list, [simple_error, simple_condition, unbound_slot, cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_unbound_slot, type_of, standard_class).
sf_sys_add_ci2(sys_simple_unbound_variable, class_name, sys_simple_unbound_variable).
sf_sys_add_ci2(sys_simple_unbound_variable, sys_class_precedence_list, [simple_error, simple_condition, unbound_variable, cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_unbound_variable, type_of, standard_class).
sf_sys_add_ci2(sys_simple_undefined_function, class_name, sys_simple_undefined_function).
sf_sys_add_ci2(sys_simple_undefined_function, sys_class_precedence_list, [simple_error, simple_condition, undefined_function, cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_simple_undefined_function, type_of, standard_class).
sf_sys_add_ci2(sys_slot_object, "subclass", condition).
sf_sys_add_ci2(sys_slotd_initialization_error, class_name, sys_slotd_initialization_error).
sf_sys_add_ci2(sys_slotd_initialization_error, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_slotd_initialization_error, "subclass", sys_slotd_initialization_type_error).
sf_sys_add_ci2(sys_slotd_initialization_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_slotd_initialization_type_error, class_name, sys_slotd_initialization_type_error).
sf_sys_add_ci2(sys_slotd_initialization_type_error, sys_class_precedence_list, [sys_slotd_initialization_error, sys_reference_condition, type_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_slotd_initialization_type_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_unset_funcallable_instance_function, class_name, sys_unset_funcallable_instance_function).
sf_sys_add_ci2(sys_unset_funcallable_instance_function, sys_class_precedence_list, [sys_reference_condition, simple_error, simple_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_unset_funcallable_instance_function, type_of, sys_condition_class).
sf_sys_add_ci2(sys_breakpoint_error, class_name, sys_breakpoint_error).
sf_sys_add_ci2(sys_breakpoint_error, sys_class_precedence_list, [sys_system_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_breakpoint_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_clos_warning, class_name, sys_clos_warning).
sf_sys_add_ci2(sys_clos_warning, sys_class_precedence_list, [warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_clos_warning, "subclass", sys_simple_clos_warning).
sf_sys_add_ci2(sys_clos_warning, "subclass", sys_gf_already_called_warning).
sf_sys_add_ci2(sys_clos_warning, "subclass", sys_gf_replacing_method_warning).
sf_sys_add_ci2(sys_clos_warning, type_of, standard_class).
sf_sys_add_ci2(sys_compiler_error, class_name, sys_compiler_error).
sf_sys_add_ci2(sys_compiler_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(sys_compiler_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_compiler_error, "subclass", sys_compiler_unsupported_feature_error).
sf_sys_add_ci2(sys_compiler_error, "subclass", sys_internal_compiler_error).
sf_sys_add_ci2(sys_compiler_error, type_of, standard_class).
sf_sys_add_ci2(sys_compiler_note, class_name, sys_compiler_note).
sf_sys_add_ci2(sys_compiler_note, sys_class_precedence_list, [condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_compiler_note, type_of, sys_condition_class).
sf_sys_add_ci2(sys_deadline_timeout, class_name, sys_deadline_timeout).
sf_sys_add_ci2(sys_deadline_timeout, sys_class_precedence_list, [sys_timeout, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_deadline_timeout, type_of, sys_condition_class).
sf_sys_add_ci2(sys_defconstant_uneql, class_name, sys_defconstant_uneql).
sf_sys_add_ci2(sys_defconstant_uneql, sys_class_precedence_list, [sys_reference_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_defconstant_uneql, type_of, sys_condition_class).
sf_sys_add_ci2(sys_format_error, class_name, sys_format_error).
sf_sys_add_ci2(sys_format_error, sys_class_precedence_list, [simple_error, simple_condition, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_format_error, type_of, standard_class).
sf_sys_add_ci2(sys_gf_already_called_warning, class_name, sys_gf_already_called_warning).
sf_sys_add_ci2(sys_gf_already_called_warning, sys_class_precedence_list, [sys_clos_warning, warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_gf_already_called_warning, "subclass", sys_simple_gf_already_called_warning).
sf_sys_add_ci2(sys_gf_already_called_warning, type_of, standard_class).
sf_sys_add_ci2(sys_gf_replacing_method_warning, class_name, sys_gf_replacing_method_warning).
sf_sys_add_ci2(sys_gf_replacing_method_warning, sys_class_precedence_list, [sys_clos_warning, warning, condition, standard_object, t]).
sf_sys_add_ci2(sys_gf_replacing_method_warning, "subclass", sys_simple_gf_replacing_method_warning).
sf_sys_add_ci2(sys_gf_replacing_method_warning, type_of, standard_class).
sf_sys_add_ci2(sys_implicit_generic_function_warning, class_name, sys_implicit_generic_function_warning).
sf_sys_add_ci2(sys_implicit_generic_function_warning, sys_class_precedence_list, [style_warning, warning, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_implicit_generic_function_warning, type_of, sys_condition_class).
sf_sys_add_ci2(sys_interactive_interrupt, class_name, sys_interactive_interrupt).
sf_sys_add_ci2(sys_interactive_interrupt, sys_class_precedence_list, [sys_system_condition, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_interactive_interrupt, type_of, sys_condition_class).
sf_sys_add_ci2(sys_internal_compiler_error, class_name, sys_internal_compiler_error).
sf_sys_add_ci2(sys_internal_compiler_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(sys_internal_compiler_error, sys_class_precedence_list, [sys_compiler_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_internal_compiler_error, type_of, standard_class).
sf_sys_add_ci2(sys_invalid_fasl, class_name, sys_invalid_fasl).
sf_sys_add_ci2(sys_invalid_fasl, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_invalid_fasl, type_of, sys_condition_class).
sf_sys_add_ci2(sys_io_timeout, class_name, sys_io_timeout).
sf_sys_add_ci2(sys_io_timeout, sys_class_precedence_list, [stream_error, error, sys_timeout, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_io_timeout, type_of, sys_condition_class).
sf_sys_add_ci2(sys_memory_fault_error, class_name, sys_memory_fault_error).
sf_sys_add_ci2(sys_memory_fault_error, sys_class_precedence_list, [sys_system_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_memory_fault_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_method_call_error, class_name, sys_method_call_error).
sf_sys_add_ci2(sys_method_call_error, sys_class_precedence_list, [simple_error, simple_condition, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_method_call_error, "subclass", sys_method_call_type_error).
sf_sys_add_ci2(sys_method_call_error, type_of, standard_class).
sf_sys_add_ci2(sys_method_call_type_error, class_name, sys_method_call_type_error).
sf_sys_add_ci2(sys_method_call_type_error, sys_class_precedence_list, [simple_type_error, sys_method_call_error, simple_error, simple_condition, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(sys_method_call_type_error, type_of, standard_class).
sf_sys_add_ci2(sys_name_conflict, class_name, sys_name_conflict).
sf_sys_add_ci2(sys_name_conflict, sys_class_precedence_list, [sys_reference_condition, package_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_name_conflict, type_of, sys_condition_class).
sf_sys_add_ci2(sys_package_lock_violation, class_name, sys_package_lock_violation).
sf_sys_add_ci2(sys_package_lock_violation, sys_class_precedence_list, [package_error, error, serious_condition, sys_reference_condition, simple_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_package_lock_violation, "subclass", sys_package_locked_error).
sf_sys_add_ci2(sys_package_lock_violation, "subclass", sys_symbol_package_locked_error).
sf_sys_add_ci2(sys_package_lock_violation, type_of, sys_condition_class).
sf_sys_add_ci2(sys_package_locked_error, class_name, sys_package_locked_error).
sf_sys_add_ci2(sys_package_locked_error, sys_class_precedence_list, [sys_package_lock_violation, package_error, error, serious_condition, sys_reference_condition, simple_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_package_locked_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_cpl_protocol_violation).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_find_method_length_mismatch).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_initarg_error).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_instance_structure_protocol_error).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_invalid_superclass).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_long_method_combination_error).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_metaobject_initialization_violation).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_new_value_specialization).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_no_primary_method).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_print_object_stream_specializer).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_slotd_initialization_error).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_unset_funcallable_instance_function).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_defconstant_uneql).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_name_conflict).
sf_sys_add_ci2(sys_reference_condition, "subclass", sys_package_lock_violation).
sf_sys_add_ci2(sys_step_condition, class_name, sys_step_condition).
sf_sys_add_ci2(sys_step_condition, sys_class_precedence_list, [condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_step_condition, "subclass", sys_step_finished_condition).
sf_sys_add_ci2(sys_step_condition, "subclass", sys_step_form_condition).
sf_sys_add_ci2(sys_step_condition, type_of, sys_condition_class).
sf_sys_add_ci2(sys_step_finished_condition, class_name, sys_step_finished_condition).
sf_sys_add_ci2(sys_step_finished_condition, sys_class_precedence_list, [sys_step_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_step_finished_condition, type_of, sys_condition_class).
sf_sys_add_ci2(sys_step_form_condition, class_name, sys_step_form_condition).
sf_sys_add_ci2(sys_step_form_condition, sys_class_precedence_list, [sys_step_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_step_form_condition, type_of, sys_condition_class).
sf_sys_add_ci2(sys_symbol_package_locked_error, class_name, sys_symbol_package_locked_error).
sf_sys_add_ci2(sys_symbol_package_locked_error, sys_class_precedence_list, [sys_package_lock_violation, package_error, error, serious_condition, sys_reference_condition, simple_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_symbol_package_locked_error, type_of, sys_condition_class).
sf_sys_add_ci2(sys_system_condition, class_name, sys_system_condition).
sf_sys_add_ci2(sys_system_condition, sys_class_precedence_list, [condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_system_condition, "subclass", sys_breakpoint_error).
sf_sys_add_ci2(sys_system_condition, "subclass", sys_interactive_interrupt).
sf_sys_add_ci2(sys_system_condition, "subclass", sys_memory_fault_error).
sf_sys_add_ci2(sys_system_condition, type_of, sys_condition_class).
sf_sys_add_ci2(sys_timeout, class_name, sys_timeout).
sf_sys_add_ci2(sys_timeout, sys_class_precedence_list, [serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(sys_timeout, "subclass", sys_deadline_timeout).
sf_sys_add_ci2(sys_timeout, "subclass", sys_io_timeout).
sf_sys_add_ci2(sys_timeout, type_of, sys_condition_class).
sf_sys_add_ci2(simple_condition, class_name, simple_condition).
sf_sys_add_ci2(simple_condition, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(simple_condition, sys_class_precedence_list, [condition, sys_slot_object, t]).
sf_sys_add_ci2(simple_condition, sys_class_precedence_list, [condition, standard_object, t]).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_clos_warning).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_gf_already_called_warning).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_gf_replacing_method_warning).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_interrupt_condition).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_serious_condition).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_storage_condition).
sf_sys_add_ci2(simple_condition, "subclass", sys_simple_style_warning).
sf_sys_add_ci2(simple_condition, "subclass", sys_package_lock_violation).
sf_sys_add_ci2(simple_condition, "subclass", simple_error).
sf_sys_add_ci2(simple_condition, "subclass", simple_type_error).
sf_sys_add_ci2(simple_condition, "subclass", simple_warning).
sf_sys_add_ci2(simple_condition, type_of, sys_condition_class).
sf_sys_add_ci2(simple_condition, type_of, standard_class).
sf_sys_add_ci2(simple_error, class_name, simple_error).
sf_sys_add_ci2(simple_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(simple_error, sys_class_precedence_list, [simple_condition, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(simple_error, sys_class_precedence_list, [simple_condition, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(simple_error, sys_class_precedence_list, [simple_condition, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(simple_error, "subclass", sys_find_method_length_mismatch).
sf_sys_add_ci2(simple_error, "subclass", sys_long_method_combination_error).
sf_sys_add_ci2(simple_error, "subclass", sys_metaobject_initialization_violation).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_argument_list_dotted).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_arithmetic_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_cell_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_control_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_division_by_zero).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_end_of_file).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_file_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_floating_point_overflow).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_floating_point_underflow).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_keyword_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_missing_load_form).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_os_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_package_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_parse_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_print_not_readable).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_program_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_reader_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_source_program_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_stream_error).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_unbound_slot).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_unbound_variable).
sf_sys_add_ci2(simple_error, "subclass", sys_simple_undefined_function).
sf_sys_add_ci2(simple_error, "subclass", sys_unset_funcallable_instance_function).
sf_sys_add_ci2(simple_error, "subclass", sys_format_error).
sf_sys_add_ci2(simple_error, "subclass", sys_method_call_error).
sf_sys_add_ci2(simple_error, type_of, sys_condition_class).
sf_sys_add_ci2(simple_error, type_of, standard_class).
sf_sys_add_ci2(simple_type_error, class_name, simple_type_error).
sf_sys_add_ci2(simple_type_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(simple_type_error, sys_class_precedence_list, [simple_condition, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(simple_type_error, sys_class_precedence_list, [simple_condition, type_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(simple_type_error, sys_class_precedence_list, [simple_condition, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(simple_type_error, sys_class_precedence_list, [simple_condition, type_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(simple_type_error, "subclass", sys_method_call_type_error).
sf_sys_add_ci2(simple_type_error, type_of, sys_condition_class).
sf_sys_add_ci2(simple_type_error, type_of, standard_class).
sf_sys_add_ci2(simple_warning, class_name, simple_warning).
sf_sys_add_ci2(simple_warning, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(simple_warning, sys_class_precedence_list, [simple_condition, warning, condition, sys_slot_object, t]).
sf_sys_add_ci2(simple_warning, sys_class_precedence_list, [simple_condition, warning, condition, standard_object, t]).
sf_sys_add_ci2(simple_warning, sys_class_precedence_list, [simple_condition, warning, condition, standard_object, t]).
sf_sys_add_ci2(simple_warning, "subclass", sys_print_object_stream_specializer).
sf_sys_add_ci2(simple_warning, type_of, sys_condition_class).
sf_sys_add_ci2(simple_warning, type_of, standard_class).
sf_sys_add_ci2(storage_condition, class_name, storage_condition).
sf_sys_add_ci2(storage_condition, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(storage_condition, sys_class_precedence_list, [serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(storage_condition, sys_class_precedence_list, [serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(storage_condition, sys_class_precedence_list, [serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(storage_condition, "subclass", sys_simple_storage_condition).
sf_sys_add_ci2(storage_condition, type_of, sys_condition_class).
sf_sys_add_ci2(storage_condition, type_of, standard_class).
sf_sys_add_ci2(stream_error, class_name, stream_error).
sf_sys_add_ci2(stream_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(stream_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(stream_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(stream_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(stream_error, "subclass", end_of_file).
sf_sys_add_ci2(stream_error, "subclass", reader_error).
sf_sys_add_ci2(stream_error, "subclass", sys_simple_stream_error).
sf_sys_add_ci2(stream_error, "subclass", sys_io_timeout).
sf_sys_add_ci2(stream_error, type_of, sys_condition_class).
sf_sys_add_ci2(stream_error, type_of, standard_class).
sf_sys_add_ci2(style_warning, class_name, style_warning).
sf_sys_add_ci2(style_warning, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(style_warning, sys_class_precedence_list, [warning, condition, sys_slot_object, t]).
sf_sys_add_ci2(style_warning, sys_class_precedence_list, [warning, condition, standard_object, t]).
sf_sys_add_ci2(style_warning, sys_class_precedence_list, [warning, condition, standard_object, t]).
sf_sys_add_ci2(style_warning, "subclass", sys_simple_style_warning).
sf_sys_add_ci2(style_warning, "subclass", sys_early_deprecation_warning).
sf_sys_add_ci2(style_warning, "subclass", sys_implicit_generic_function_warning).
sf_sys_add_ci2(style_warning, type_of, sys_condition_class).
sf_sys_add_ci2(style_warning, type_of, standard_class).
sf_sys_add_ci2(type_error, class_name, type_error).
sf_sys_add_ci2(type_error, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(type_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(type_error, sys_class_precedence_list, [error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(type_error, sys_class_precedence_list, [error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(type_error, "subclass", sys_argument_list_dotted).
sf_sys_add_ci2(type_error, "subclass", sys_case_failure).
sf_sys_add_ci2(type_error, "subclass", sys_charset_type_error).
sf_sys_add_ci2(type_error, "subclass", sys_keyword_error).
sf_sys_add_ci2(type_error, "subclass", sys_slotd_initialization_type_error).
sf_sys_add_ci2(type_error, "subclass", simple_type_error).
sf_sys_add_ci2(type_error, type_of, sys_condition_class).
sf_sys_add_ci2(type_error, type_of, standard_class).
sf_sys_add_ci2(unbound_slot, class_name, unbound_slot).
sf_sys_add_ci2(unbound_slot, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(unbound_slot, sys_class_precedence_list, [cell_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(unbound_slot, sys_class_precedence_list, [cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(unbound_slot, sys_class_precedence_list, [cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(unbound_slot, "subclass", sys_simple_unbound_slot).
sf_sys_add_ci2(unbound_slot, type_of, sys_condition_class).
sf_sys_add_ci2(unbound_slot, type_of, standard_class).
sf_sys_add_ci2(unbound_variable, class_name, unbound_variable).
sf_sys_add_ci2(unbound_variable, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(unbound_variable, sys_class_precedence_list, [cell_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(unbound_variable, sys_class_precedence_list, [cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(unbound_variable, sys_class_precedence_list, [cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(unbound_variable, "subclass", sys_simple_unbound_variable).
sf_sys_add_ci2(unbound_variable, type_of, sys_condition_class).
sf_sys_add_ci2(unbound_variable, type_of, standard_class).
sf_sys_add_ci2(undefined_function, class_name, undefined_function).
sf_sys_add_ci2(undefined_function, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(undefined_function, sys_class_precedence_list, [cell_error, error, serious_condition, condition, sys_slot_object, t]).
sf_sys_add_ci2(undefined_function, sys_class_precedence_list, [cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(undefined_function, sys_class_precedence_list, [cell_error, error, serious_condition, condition, standard_object, t]).
sf_sys_add_ci2(undefined_function, "subclass", sys_simple_undefined_function).
sf_sys_add_ci2(undefined_function, type_of, sys_condition_class).
sf_sys_add_ci2(undefined_function, type_of, standard_class).
sf_sys_add_ci2(warning, class_name, warning).
sf_sys_add_ci2(warning, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(warning, sys_class_precedence_list, [condition, sys_slot_object, t]).
sf_sys_add_ci2(warning, sys_class_precedence_list, [condition, standard_object, t]).
sf_sys_add_ci2(warning, "subclass", sys_clos_warning).
sf_sys_add_ci2(warning, "subclass", sys_final_deprecation_warning).
sf_sys_add_ci2(warning, "subclass", sys_late_deprecation_warning).
sf_sys_add_ci2(warning, "subclass", simple_warning).
sf_sys_add_ci2(warning, "subclass", style_warning).
sf_sys_add_ci2(warning, type_of, sys_condition_class).
sf_sys_add_ci2(warning, type_of, standard_class).
sf_sys_add_ci2(function, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(function, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(generic_function, class_name, generic_function).
sf_sys_add_ci2(generic_function, sys_class_precedence_list, [sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_metaobject, sys_funcallable_standard_object, function, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(generic_function, sys_class_precedence_list, [sys_metaobject, sys_funcallable_standard_object, standard_object, function, t]).
sf_sys_add_ci2(generic_function, sys_class_precedence_list, [sys_metaobject, sys_funcallable_standard_object, standard_object, function, t]).
sf_sys_add_ci2(generic_function, "subclass", standard_generic_function).
sf_sys_add_ci2(generic_function, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(generic_function, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(sys_funcallable_standard_object, class_name, sys_funcallable_standard_object).
sf_sys_add_ci2(sys_funcallable_standard_object, sys_class_precedence_list, [function, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_funcallable_standard_object, "subclass", generic_function).
sf_sys_add_ci2(sys_funcallable_standard_object, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(sys_metaobject, "subclass", generic_function).
sf_sys_add_ci2(sys_definition_source_mixin, "subclass", generic_function).
sf_sys_add_ci2(sys_dependent_update_mixin, "subclass", generic_function).
sf_sys_add_ci2(sys_funcallable_standard_object, class_name, sys_funcallable_standard_object).
sf_sys_add_ci2(sys_funcallable_standard_object, sys_class_precedence_list, [function, standard_object, t]).
sf_sys_add_ci2(sys_funcallable_standard_object, sys_class_precedence_list, [standard_object, function, t]).
sf_sys_add_ci2(sys_funcallable_standard_object, "subclass", generic_function).
sf_sys_add_ci2(sys_funcallable_standard_object, "subclass", generic_function).
sf_sys_add_ci2(sys_funcallable_standard_object, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(generic_function, class_name, generic_function).
sf_sys_add_ci2(generic_function, sys_class_precedence_list, [sys_metaobject, sys_funcallable_standard_object, function, standard_object, t]).
sf_sys_add_ci2(generic_function, "subclass", standard_generic_function).
sf_sys_add_ci2(generic_function, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(sys_metaobject, "subclass", generic_function).
sf_sys_add_ci2(sys_metaobject, "subclass", generic_function).
sf_sys_add_ci2(standard_generic_function, class_name, standard_generic_function).
sf_sys_add_ci2(standard_generic_function, sys_class_precedence_list, [generic_function, sys_metaobject, sys_funcallable_standard_object, function, standard_object, t]).
sf_sys_add_ci2(standard_generic_function, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(standard_generic_function, class_name, standard_generic_function).
sf_sys_add_ci2(standard_generic_function, sys_class_precedence_list, [generic_function, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_metaobject, sys_funcallable_standard_object, function, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(standard_generic_function, sys_class_precedence_list, [generic_function, sys_metaobject, sys_funcallable_standard_object, standard_object, function, t]).
sf_sys_add_ci2(standard_generic_function, sys_class_precedence_list, [generic_function, sys_metaobject, sys_funcallable_standard_object, standard_object, function, t]).
sf_sys_add_ci2(standard_generic_function, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(standard_generic_function, type_of, sys_funcallable_standard_class).
sf_sys_add_ci2(standard_object, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(standard_object, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(sys_slot_object, class_name, sys_slot_object).
sf_sys_add_ci2(sys_slot_object, sys_class_precedence_list, [t]).
sf_sys_add_ci2(sys_slot_object, "subclass", condition).
sf_sys_add_ci2(sys_slot_object, "subclass", standard_object).
sf_sys_add_ci2(sys_slot_object, "subclass", structure_object).
sf_sys_add_ci2(sys_slot_object, type_of, sys_slot_class).
sf_sys_add_ci2(t, "subclass", sys_slot_object).
sf_sys_add_ci2(built_in_class, class_name, built_in_class).
sf_sys_add_ci2(built_in_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(built_in_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(built_in_class, sys_class_precedence_list, [class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(built_in_class, sys_class_precedence_list, [sys_system_class, sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(built_in_class, type_of, standard_class).
sf_sys_add_ci2(class, class_name, class).
sf_sys_add_ci2(class, sys_class_precedence_list, [sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(class, sys_class_precedence_list, [sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(class, "subclass", built_in_class).
sf_sys_add_ci2(class, "subclass", sys_pcl_class).
sf_sys_add_ci2(class, "subclass", sys_std_class).
sf_sys_add_ci2(class, "subclass", sys_forward_referenced_class).
sf_sys_add_ci2(class, "subclass", sys_funcallable_standard_class).
sf_sys_add_ci2(class, "subclass", standard_class).
sf_sys_add_ci2(class, "subclass", structure_class).
sf_sys_add_ci2(class, type_of, standard_class).
sf_sys_add_ci2(condition, class_name, condition).
sf_sys_add_ci2(condition, "default_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(condition, "direct_initargs", [[kw_format_arguments, [], '$ARRAY'([*], claz_base_character, [#\('F'), #\('I'), #\('L'), #\('T'), #\('R'), #\('A'), #\('C'), #\('E')])]]).
sf_sys_add_ci2(condition, sys_class_precedence_list, [standard_object, t]).
sf_sys_add_ci2(condition, "subclass", serious_condition).
sf_sys_add_ci2(condition, "subclass", simple_condition).
sf_sys_add_ci2(condition, "subclass", warning).
sf_sys_add_ci2(condition, type_of, standard_class).
sf_sys_add_ci2(function, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(method, class_name, method).
sf_sys_add_ci2(method, sys_class_precedence_list, [sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(method, sys_class_precedence_list, [sys_metaobject, standard_object, t]).
sf_sys_add_ci2(method, "subclass", standard_method).
sf_sys_add_ci2(method, type_of, standard_class).
sf_sys_add_ci2(method_combination, class_name, method_combination).
sf_sys_add_ci2(method_combination, sys_class_precedence_list, [sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(method_combination, sys_class_precedence_list, [sys_metaobject, standard_object, t]).
sf_sys_add_ci2(method_combination, "subclass", sys_long_method_combination).
sf_sys_add_ci2(method_combination, "subclass", sys_short_method_combination).
sf_sys_add_ci2(method_combination, "subclass", sys_standard_method_combination).
sf_sys_add_ci2(method_combination, type_of, standard_class).
sf_sys_add_ci2(sys_direct_slot_definition, class_name, sys_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_direct_slot_definition, "subclass", sys_standard_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, "subclass", sys_condition_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, "subclass", sys_structure_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_effective_slot_definition, class_name, sys_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_effective_slot_definition, "subclass", sys_standard_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, "subclass", sys_condition_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, "subclass", sys_structure_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_eql_specializer, class_name, sys_eql_specializer).
sf_sys_add_ci2(sys_eql_specializer, sys_class_precedence_list, [sys_standard_specializer, sys_exact_class_specializer, sys_specializer_with_object, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_eql_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_forward_referenced_class, class_name, sys_forward_referenced_class).
sf_sys_add_ci2(sys_forward_referenced_class, sys_class_precedence_list, [sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_forward_referenced_class, type_of, standard_class).
sf_sys_add_ci2(sys_funcallable_standard_class, class_name, sys_funcallable_standard_class).
sf_sys_add_ci2(sys_funcallable_standard_class, sys_class_precedence_list, [sys_std_class, sys_slot_class, sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_funcallable_standard_class, type_of, standard_class).
sf_sys_add_ci2(sys_metaobject, class_name, sys_metaobject).
sf_sys_add_ci2(sys_metaobject, sys_class_precedence_list, [standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_metaobject, "subclass", generic_function).
sf_sys_add_ci2(sys_metaobject, "subclass", method).
sf_sys_add_ci2(sys_metaobject, "subclass", method_combination).
sf_sys_add_ci2(sys_metaobject, "subclass", sys_slot_definition).
sf_sys_add_ci2(sys_metaobject, "subclass", sys_specializer).
sf_sys_add_ci2(sys_metaobject, type_of, standard_class).
sf_sys_add_ci2(sys_slot_definition, class_name, sys_slot_definition).
sf_sys_add_ci2(sys_slot_definition, sys_class_precedence_list, [sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_direct_slot_definition).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_effective_slot_definition).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_standard_slot_definition).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_condition_slot_definition).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_structure_slot_definition).
sf_sys_add_ci2(sys_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_specializer, class_name, sys_specializer).
sf_sys_add_ci2(sys_specializer, sys_class_precedence_list, [sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_specializer, "subclass", sys_exact_class_specializer).
sf_sys_add_ci2(sys_specializer, "subclass", sys_specializer_with_object).
sf_sys_add_ci2(sys_specializer, "subclass", sys_standard_specializer).
sf_sys_add_ci2(sys_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_standard_accessor_method, class_name, sys_standard_accessor_method).
sf_sys_add_ci2(sys_standard_accessor_method, sys_class_precedence_list, [sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_accessor_method, "subclass", sys_standard_reader_method).
sf_sys_add_ci2(sys_standard_accessor_method, "subclass", sys_standard_writer_method).
sf_sys_add_ci2(sys_standard_accessor_method, "subclass", sys_standard_boundp_method).
sf_sys_add_ci2(sys_standard_accessor_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_direct_slot_definition, class_name, sys_standard_direct_slot_definition).
sf_sys_add_ci2(sys_standard_direct_slot_definition, sys_class_precedence_list, [sys_standard_slot_definition, sys_direct_slot_definition, sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_standard_effective_slot_definition, class_name, sys_standard_effective_slot_definition).
sf_sys_add_ci2(sys_standard_effective_slot_definition, sys_class_precedence_list, [sys_standard_slot_definition, sys_effective_slot_definition, sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_standard_reader_method, class_name, sys_standard_reader_method).
sf_sys_add_ci2(sys_standard_reader_method, sys_class_precedence_list, [sys_standard_accessor_method, sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_reader_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_slot_definition, class_name, sys_standard_slot_definition).
sf_sys_add_ci2(sys_standard_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_slot_definition, "subclass", sys_standard_direct_slot_definition).
sf_sys_add_ci2(sys_standard_slot_definition, "subclass", sys_standard_effective_slot_definition).
sf_sys_add_ci2(sys_standard_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_standard_writer_method, class_name, sys_standard_writer_method).
sf_sys_add_ci2(sys_standard_writer_method, sys_class_precedence_list, [sys_standard_accessor_method, sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_writer_method, type_of, standard_class).
sf_sys_add_ci2(sys_accessor_method, class_name, sys_accessor_method).
sf_sys_add_ci2(sys_accessor_method, sys_class_precedence_list, [standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_accessor_method, "subclass", sys_standard_accessor_method).
sf_sys_add_ci2(sys_accessor_method, "subclass", sys_global_boundp_method).
sf_sys_add_ci2(sys_accessor_method, "subclass", sys_global_reader_method).
sf_sys_add_ci2(sys_accessor_method, "subclass", sys_global_writer_method).
sf_sys_add_ci2(sys_accessor_method, type_of, standard_class).
sf_sys_add_ci2(sys_class_eq_specializer, class_name, sys_class_eq_specializer).
sf_sys_add_ci2(sys_class_eq_specializer, sys_class_precedence_list, [sys_standard_specializer, sys_exact_class_specializer, sys_specializer_with_object, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_class_eq_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_class_prototype_specializer, class_name, sys_class_prototype_specializer).
sf_sys_add_ci2(sys_class_prototype_specializer, sys_class_precedence_list, [sys_standard_specializer, sys_specializer_with_object, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_class_prototype_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_condition_class, class_name, sys_condition_class).
sf_sys_add_ci2(sys_condition_class, sys_class_precedence_list, [sys_slot_class, sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_condition_class, type_of, standard_class).
sf_sys_add_ci2(sys_condition_direct_slot_definition, class_name, sys_condition_direct_slot_definition).
sf_sys_add_ci2(sys_condition_direct_slot_definition, sys_class_precedence_list, [sys_condition_slot_definition, sys_direct_slot_definition, sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_condition_direct_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_condition_effective_slot_definition, class_name, sys_condition_effective_slot_definition).
sf_sys_add_ci2(sys_condition_effective_slot_definition, sys_class_precedence_list, [sys_condition_slot_definition, sys_effective_slot_definition, sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_condition_effective_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_condition_slot_definition, class_name, sys_condition_slot_definition).
sf_sys_add_ci2(sys_condition_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_condition_slot_definition, "subclass", sys_condition_direct_slot_definition).
sf_sys_add_ci2(sys_condition_slot_definition, "subclass", sys_condition_effective_slot_definition).
sf_sys_add_ci2(sys_condition_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_definition_source_mixin, class_name, sys_definition_source_mixin).
sf_sys_add_ci2(sys_definition_source_mixin, sys_class_precedence_list, [standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_definition_source_mixin, "subclass", class).
sf_sys_add_ci2(sys_definition_source_mixin, "subclass", generic_function).
sf_sys_add_ci2(sys_definition_source_mixin, "subclass", sys_standard_method_combination).
sf_sys_add_ci2(sys_definition_source_mixin, "subclass", standard_method).
sf_sys_add_ci2(sys_definition_source_mixin, type_of, standard_class).
sf_sys_add_ci2(sys_dependent_update_mixin, class_name, sys_dependent_update_mixin).
sf_sys_add_ci2(sys_dependent_update_mixin, sys_class_precedence_list, [sys_plist_mixin, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_dependent_update_mixin, "subclass", class).
sf_sys_add_ci2(sys_dependent_update_mixin, "subclass", generic_function).
sf_sys_add_ci2(sys_dependent_update_mixin, type_of, standard_class).
sf_sys_add_ci2(sys_exact_class_specializer, class_name, sys_exact_class_specializer).
sf_sys_add_ci2(sys_exact_class_specializer, sys_class_precedence_list, [sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_exact_class_specializer, "subclass", sys_eql_specializer).
sf_sys_add_ci2(sys_exact_class_specializer, "subclass", sys_class_eq_specializer).
sf_sys_add_ci2(sys_exact_class_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_global_boundp_method, class_name, sys_global_boundp_method).
sf_sys_add_ci2(sys_global_boundp_method, sys_class_precedence_list, [sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_global_boundp_method, type_of, standard_class).
sf_sys_add_ci2(sys_global_reader_method, class_name, sys_global_reader_method).
sf_sys_add_ci2(sys_global_reader_method, sys_class_precedence_list, [sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_global_reader_method, type_of, standard_class).
sf_sys_add_ci2(sys_global_writer_method, class_name, sys_global_writer_method).
sf_sys_add_ci2(sys_global_writer_method, sys_class_precedence_list, [sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_global_writer_method, type_of, standard_class).
sf_sys_add_ci2(sys_initargs_updater, class_name, sys_initargs_updater).
sf_sys_add_ci2(sys_initargs_updater, sys_class_precedence_list, [standard_object, t]).
sf_sys_add_ci2(sys_initargs_updater, type_of, standard_class).
sf_sys_add_ci2(sys_long_method_combination, class_name, sys_long_method_combination).
sf_sys_add_ci2(sys_long_method_combination, sys_class_precedence_list, [method_combination, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_long_method_combination, sys_class_precedence_list, [sys_standard_method_combination, sys_definition_source_mixin, method_combination, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_long_method_combination, type_of, standard_class).
sf_sys_add_ci2(sys_misdesigned_forward_referenced_class, class_name, sys_misdesigned_forward_referenced_class).
sf_sys_add_ci2(sys_misdesigned_forward_referenced_class, sys_class_precedence_list, [sys_forward_referenced_class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_misdesigned_forward_referenced_class, type_of, standard_class).
sf_sys_add_ci2(sys_pcl_class, class_name, sys_pcl_class).
sf_sys_add_ci2(sys_pcl_class, sys_class_precedence_list, [class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_pcl_class, "subclass", sys_forward_referenced_class).
sf_sys_add_ci2(sys_pcl_class, "subclass", sys_slot_class).
sf_sys_add_ci2(sys_pcl_class, "subclass", sys_system_class).
sf_sys_add_ci2(sys_pcl_class, type_of, standard_class).
sf_sys_add_ci2(sys_plist_mixin, class_name, sys_plist_mixin).
sf_sys_add_ci2(sys_plist_mixin, sys_class_precedence_list, [standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_plist_mixin, "subclass", sys_dependent_update_mixin).
sf_sys_add_ci2(sys_plist_mixin, "subclass", standard_method).
sf_sys_add_ci2(sys_plist_mixin, type_of, standard_class).
sf_sys_add_ci2(sys_potential_class, class_name, sys_potential_class).
sf_sys_add_ci2(sys_potential_class, sys_class_precedence_list, [sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_potential_class, "subclass", sys_misdesigned_forward_referenced_class).
sf_sys_add_ci2(sys_potential_class, "subclass", class).
sf_sys_add_ci2(sys_potential_class, type_of, standard_class).
sf_sys_add_ci2(sys_semi_standard_class, class_name, sys_semi_standard_class).
sf_sys_add_ci2(sys_semi_standard_class, sys_class_precedence_list, [sys_slotted_class, class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_semi_standard_class, "subclass", sys_funcallable_standard_class).
sf_sys_add_ci2(sys_semi_standard_class, "subclass", standard_class).
sf_sys_add_ci2(sys_semi_standard_class, type_of, standard_class).
sf_sys_add_ci2(sys_short_method_combination, class_name, sys_short_method_combination).
sf_sys_add_ci2(sys_short_method_combination, sys_class_precedence_list, [method_combination, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_short_method_combination, sys_class_precedence_list, [sys_standard_method_combination, sys_definition_source_mixin, method_combination, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_short_method_combination, type_of, standard_class).
sf_sys_add_ci2(sys_slot_class, class_name, sys_slot_class).
sf_sys_add_ci2(sys_slot_class, sys_class_precedence_list, [sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_slot_class, "subclass", sys_condition_class).
sf_sys_add_ci2(sys_slot_class, "subclass", sys_std_class).
sf_sys_add_ci2(sys_slot_class, "subclass", structure_class).
sf_sys_add_ci2(sys_slot_class, type_of, standard_class).
sf_sys_add_ci2(sys_slot_object, "subclass", standard_object).
sf_sys_add_ci2(sys_slotted_class, class_name, sys_slotted_class).
sf_sys_add_ci2(sys_slotted_class, sys_class_precedence_list, [class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_slotted_class, "subclass", sys_semi_standard_class).
sf_sys_add_ci2(sys_slotted_class, "subclass", structure_class).
sf_sys_add_ci2(sys_slotted_class, type_of, standard_class).
sf_sys_add_ci2(sys_specializer_with_object, class_name, sys_specializer_with_object).
sf_sys_add_ci2(sys_specializer_with_object, sys_class_precedence_list, [sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_specializer_with_object, "subclass", sys_eql_specializer).
sf_sys_add_ci2(sys_specializer_with_object, "subclass", sys_class_eq_specializer).
sf_sys_add_ci2(sys_specializer_with_object, "subclass", sys_class_prototype_specializer).
sf_sys_add_ci2(sys_specializer_with_object, type_of, standard_class).
sf_sys_add_ci2(sys_standard_boundp_method, class_name, sys_standard_boundp_method).
sf_sys_add_ci2(sys_standard_boundp_method, sys_class_precedence_list, [sys_standard_accessor_method, sys_accessor_method, standard_method, sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_boundp_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_method_combination, class_name, sys_standard_method_combination).
sf_sys_add_ci2(sys_standard_method_combination, sys_class_precedence_list, [sys_definition_source_mixin, method_combination, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_method_combination, "subclass", sys_long_method_combination).
sf_sys_add_ci2(sys_standard_method_combination, "subclass", sys_short_method_combination).
sf_sys_add_ci2(sys_standard_method_combination, type_of, standard_class).
sf_sys_add_ci2(sys_standard_specializer, class_name, sys_standard_specializer).
sf_sys_add_ci2(sys_standard_specializer, sys_class_precedence_list, [sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_standard_specializer, "subclass", class).
sf_sys_add_ci2(sys_standard_specializer, "subclass", sys_eql_specializer).
sf_sys_add_ci2(sys_standard_specializer, "subclass", sys_class_eq_specializer).
sf_sys_add_ci2(sys_standard_specializer, "subclass", sys_class_prototype_specializer).
sf_sys_add_ci2(sys_standard_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_std_class, class_name, sys_std_class).
sf_sys_add_ci2(sys_std_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_std_class, sys_class_precedence_list, [sys_slot_class, sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_std_class, "subclass", sys_funcallable_standard_class).
sf_sys_add_ci2(sys_std_class, "subclass", sys_funcallable_standard_class).
sf_sys_add_ci2(sys_std_class, "subclass", standard_class).
sf_sys_add_ci2(sys_std_class, type_of, standard_class).
sf_sys_add_ci2(sys_structure_direct_slot_definition, class_name, sys_structure_direct_slot_definition).
sf_sys_add_ci2(sys_structure_direct_slot_definition, sys_class_precedence_list, [sys_structure_slot_definition, sys_direct_slot_definition, sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_structure_direct_slot_definition, sys_class_precedence_list, [sys_direct_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_structure_direct_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_structure_effective_slot_definition, class_name, sys_structure_effective_slot_definition).
sf_sys_add_ci2(sys_structure_effective_slot_definition, sys_class_precedence_list, [sys_structure_slot_definition, sys_effective_slot_definition, sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_structure_effective_slot_definition, sys_class_precedence_list, [sys_effective_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_structure_effective_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_structure_slot_definition, class_name, sys_structure_slot_definition).
sf_sys_add_ci2(sys_structure_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_structure_slot_definition, "subclass", sys_structure_direct_slot_definition).
sf_sys_add_ci2(sys_structure_slot_definition, "subclass", sys_structure_effective_slot_definition).
sf_sys_add_ci2(sys_structure_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_super_class, class_name, sys_super_class).
sf_sys_add_ci2(sys_super_class, sys_class_precedence_list, [sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_super_class, "subclass", sys_potential_class).
sf_sys_add_ci2(sys_super_class, "subclass", sys_forward_referenced_class).
sf_sys_add_ci2(sys_super_class, type_of, standard_class).
sf_sys_add_ci2(class, class_name, class).
sf_sys_add_ci2(class, sys_class_precedence_list, [sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(class, "subclass", built_in_class).
sf_sys_add_ci2(class, "subclass", sys_slotted_class).
sf_sys_add_ci2(class, type_of, standard_class).
sf_sys_add_ci2(sys_direct_slot_definition, class_name, sys_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_direct_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_direct_slot_definition, "subclass", sys_structure_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, "subclass", sys_standard_direct_slot_definition).
sf_sys_add_ci2(sys_direct_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_effective_slot_definition, class_name, sys_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_effective_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_effective_slot_definition, "subclass", sys_structure_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, "subclass", sys_standard_effective_slot_definition).
sf_sys_add_ci2(sys_effective_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_eql_specializer, class_name, sys_eql_specializer).
sf_sys_add_ci2(sys_eql_specializer, sys_class_precedence_list, [sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_eql_specializer, sys_class_precedence_list, [sys_specializer, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_eql_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_forward_referenced_class, class_name, sys_forward_referenced_class).
sf_sys_add_ci2(sys_forward_referenced_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_forward_referenced_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_forward_referenced_class, sys_class_precedence_list, [sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_forward_referenced_class, "subclass", sys_misdesigned_forward_referenced_class).
sf_sys_add_ci2(sys_forward_referenced_class, type_of, standard_class).
sf_sys_add_ci2(sys_funcallable_standard_class, class_name, sys_funcallable_standard_class).
sf_sys_add_ci2(sys_funcallable_standard_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_funcallable_standard_class, sys_class_precedence_list, [sys_semi_standard_class, sys_slotted_class, class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_funcallable_standard_class, sys_class_precedence_list, [sys_std_class, class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_funcallable_standard_class, type_of, standard_class).
sf_sys_add_ci2(sys_funcallable_standard_object, class_name, sys_funcallable_standard_object).
sf_sys_add_ci2(sys_funcallable_standard_object, sys_class_precedence_list, [standard_object, function, t]).
sf_sys_add_ci2(sys_funcallable_standard_object, "subclass", generic_function).
sf_sys_add_ci2(sys_funcallable_standard_object, type_of, standard_class).
sf_sys_add_ci2(sys_metaobject, class_name, sys_metaobject).
sf_sys_add_ci2(sys_metaobject, sys_class_precedence_list, [standard_object, t]).
sf_sys_add_ci2(sys_metaobject, "subclass", generic_function).
sf_sys_add_ci2(sys_metaobject, "subclass", method).
sf_sys_add_ci2(sys_metaobject, "subclass", method_combination).
sf_sys_add_ci2(sys_metaobject, "subclass", sys_super_class).
sf_sys_add_ci2(sys_metaobject, "subclass", generic_function).
sf_sys_add_ci2(sys_metaobject, "subclass", method).
sf_sys_add_ci2(sys_metaobject, "subclass", method_combination).
sf_sys_add_ci2(sys_metaobject, "subclass", sys_slot_definition).
sf_sys_add_ci2(sys_metaobject, "subclass", sys_specializer).
sf_sys_add_ci2(sys_metaobject, type_of, standard_class).
sf_sys_add_ci2(method, class_name, method).
sf_sys_add_ci2(method, sys_class_precedence_list, [sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(method, "subclass", standard_method).
sf_sys_add_ci2(method, type_of, standard_class).
sf_sys_add_ci2(method_combination, class_name, method_combination).
sf_sys_add_ci2(method_combination, sys_class_precedence_list, [sys_metaobject, standard_object, t]).
sf_sys_add_ci2(method_combination, type_of, standard_class).
sf_sys_add_ci2(sys_slot_definition, class_name, sys_slot_definition).
sf_sys_add_ci2(sys_slot_definition, sys_class_precedence_list, [sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_direct_slot_definition).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_effective_slot_definition).
sf_sys_add_ci2(sys_slot_definition, "subclass", sys_standard_slot_definition).
sf_sys_add_ci2(sys_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_specializer, class_name, sys_specializer).
sf_sys_add_ci2(sys_specializer, sys_class_precedence_list, [sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_specializer, sys_class_precedence_list, [sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_specializer, "subclass", class).
sf_sys_add_ci2(sys_specializer, "subclass", sys_potential_class).
sf_sys_add_ci2(sys_specializer, "subclass", sys_eql_specializer).
sf_sys_add_ci2(sys_specializer, type_of, standard_class).
sf_sys_add_ci2(sys_standard_accessor_method, class_name, sys_standard_accessor_method).
sf_sys_add_ci2(sys_standard_accessor_method, sys_class_precedence_list, [standard_method, method, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_accessor_method, sys_class_precedence_list, [standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_accessor_method, sys_class_precedence_list, [standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_accessor_method, "subclass", sys_standard_reader_method).
sf_sys_add_ci2(sys_standard_accessor_method, "subclass", sys_standard_writer_method).
sf_sys_add_ci2(sys_standard_accessor_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_direct_slot_definition, class_name, sys_standard_direct_slot_definition).
sf_sys_add_ci2(sys_standard_direct_slot_definition, sys_class_precedence_list, [sys_direct_slot_definition, sys_standard_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, sys_class_precedence_list, [sys_standard_slot_definition, sys_direct_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, sys_class_precedence_list, [sys_standard_slot_definition, sys_direct_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_standard_effective_slot_definition, class_name, sys_standard_effective_slot_definition).
sf_sys_add_ci2(sys_standard_effective_slot_definition, sys_class_precedence_list, [sys_effective_slot_definition, sys_standard_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, sys_class_precedence_list, [sys_standard_slot_definition, sys_effective_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, sys_class_precedence_list, [sys_standard_slot_definition, sys_effective_slot_definition, sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, type_of, standard_class).
sf_sys_add_ci2(standard_method, class_name, standard_method).
sf_sys_add_ci2(standard_method, sys_class_precedence_list, [method, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(standard_method, "subclass", sys_standard_accessor_method).
sf_sys_add_ci2(standard_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_optimized_reader_method, class_name, sys_standard_optimized_reader_method).
sf_sys_add_ci2(sys_standard_optimized_reader_method, sys_class_precedence_list, [sys_standard_reader_method, sys_standard_accessor_method, standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_optimized_writer_method, class_name, sys_standard_optimized_writer_method).
sf_sys_add_ci2(sys_standard_optimized_writer_method, sys_class_precedence_list, [sys_standard_writer_method, sys_standard_accessor_method, standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_reader_method, class_name, sys_standard_reader_method).
sf_sys_add_ci2(sys_standard_reader_method, sys_class_precedence_list, [sys_standard_accessor_method, standard_method, method, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_reader_method, sys_class_precedence_list, [sys_standard_accessor_method, standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_reader_method, sys_class_precedence_list, [sys_standard_accessor_method, standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_reader_method, "subclass", sys_standard_optimized_reader_method).
sf_sys_add_ci2(sys_standard_reader_method, type_of, standard_class).
sf_sys_add_ci2(sys_standard_slot_definition, class_name, sys_standard_slot_definition).
sf_sys_add_ci2(sys_standard_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_slot_definition, sys_class_precedence_list, [sys_slot_definition, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_slot_definition, "subclass", sys_standard_direct_slot_definition).
sf_sys_add_ci2(sys_standard_slot_definition, "subclass", sys_standard_effective_slot_definition).
sf_sys_add_ci2(sys_standard_slot_definition, type_of, standard_class).
sf_sys_add_ci2(sys_standard_stablehash, class_name, sys_standard_stablehash).
sf_sys_add_ci2(sys_standard_stablehash, sys_class_precedence_list, [standard_object, t]).
sf_sys_add_ci2(sys_standard_stablehash, "subclass", sys_super_class).
sf_sys_add_ci2(sys_standard_stablehash, "subclass", method).
sf_sys_add_ci2(sys_standard_stablehash, "subclass", sys_specializer).
sf_sys_add_ci2(sys_standard_stablehash, type_of, standard_class).
sf_sys_add_ci2(sys_standard_writer_method, class_name, sys_standard_writer_method).
sf_sys_add_ci2(sys_standard_writer_method, sys_class_precedence_list, [sys_standard_accessor_method, standard_method, method, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_writer_method, sys_class_precedence_list, [sys_standard_accessor_method, standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_writer_method, sys_class_precedence_list, [sys_standard_accessor_method, standard_method, method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(sys_standard_writer_method, "subclass", sys_standard_optimized_writer_method).
sf_sys_add_ci2(sys_standard_writer_method, type_of, standard_class).
sf_sys_add_ci2(structure_class, class_name, structure_class).
sf_sys_add_ci2(structure_class, sys_class_precedence_list, [sys_slotted_class, class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(structure_class, type_of, standard_class).
sf_sys_add_ci2(sys_system_class, class_name, sys_system_class).
sf_sys_add_ci2(sys_system_class, sys_class_precedence_list, [sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_system_class, "subclass", built_in_class).
sf_sys_add_ci2(sys_system_class, type_of, standard_class).
sf_sys_add_ci2(standard_class, class_name, standard_class).
sf_sys_add_ci2(standard_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(standard_class, sys_class_precedence_list, [sys_semi_standard_class, sys_slotted_class, class, sys_potential_class, sys_specializer, sys_super_class, sys_standard_stablehash, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(standard_class, sys_class_precedence_list, [sys_std_class, class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(standard_class, sys_class_precedence_list, [sys_std_class, sys_slot_class, sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(standard_class, type_of, standard_class).
sf_sys_add_ci2(standard_method, class_name, standard_method).
sf_sys_add_ci2(standard_method, sys_class_precedence_list, [method, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(standard_method, sys_class_precedence_list, [sys_plist_mixin, sys_definition_source_mixin, method, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(standard_method, "subclass", sys_accessor_method).
sf_sys_add_ci2(standard_method, "subclass", sys_standard_accessor_method).
sf_sys_add_ci2(standard_method, type_of, standard_class).
sf_sys_add_ci2(standard_object, class_name, standard_object).
sf_sys_add_ci2(standard_object, sys_class_precedence_list, [sys_slot_object, t]).
sf_sys_add_ci2(standard_object, sys_class_precedence_list, [t]).
sf_sys_add_ci2(standard_object, "subclass", condition).
sf_sys_add_ci2(standard_object, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(standard_object, "subclass", sys_metaobject).
sf_sys_add_ci2(standard_object, "subclass", sys_definition_source_mixin).
sf_sys_add_ci2(standard_object, "subclass", sys_initargs_updater).
sf_sys_add_ci2(standard_object, "subclass", sys_plist_mixin).
sf_sys_add_ci2(standard_object, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(standard_object, "subclass", sys_metaobject).
sf_sys_add_ci2(standard_object, "subclass", sys_standard_stablehash).
sf_sys_add_ci2(standard_object, type_of, standard_class).
sf_sys_add_ci2(structure_class, class_name, structure_class).
sf_sys_add_ci2(structure_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(structure_class, sys_class_precedence_list, [class, sys_specializer, sys_metaobject, standard_object, t]).
sf_sys_add_ci2(structure_class, sys_class_precedence_list, [sys_slot_class, sys_pcl_class, class, sys_dependent_update_mixin, sys_plist_mixin, sys_definition_source_mixin, sys_standard_specializer, sys_specializer, sys_metaobject, standard_object, sys_slot_object, t]).
sf_sys_add_ci2(structure_class, type_of, standard_class).
sf_sys_add_ci2(t, "subclass", standard_object).
sf_sys_add_ci2(broadcast_stream, class_name, broadcast_stream).
sf_sys_add_ci2(broadcast_stream, sys_class_precedence_list, [sys_ansi_stream, stream, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(broadcast_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(broadcast_stream, type_of, structure_class).
sf_sys_add_ci2(concatenated_stream, class_name, concatenated_stream).
sf_sys_add_ci2(concatenated_stream, sys_class_precedence_list, [sys_ansi_stream, stream, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(concatenated_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(concatenated_stream, type_of, structure_class).
sf_sys_add_ci2(echo_stream, class_name, echo_stream).
sf_sys_add_ci2(echo_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(echo_stream, sys_class_precedence_list, [two_way_stream, sys_ansi_stream, stream, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(echo_stream, type_of, structure_class).
sf_sys_add_ci2(file_stream, class_name, file_stream).
sf_sys_add_ci2(file_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(file_stream, "subclass", sys_fd_stream).
sf_sys_add_ci2(file_stream, type_of, structure_class).
sf_sys_add_ci2(function, "subclass", sys_pf_method_function).
sf_sys_add_ci2(function, "subclass", sys_ctor).
sf_sys_add_ci2(function, "subclass", sys_standard_funcallable_instance).
sf_sys_add_ci2(hash_table, class_name, hash_table).
sf_sys_add_ci2(hash_table, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(hash_table, type_of, structure_class).
sf_sys_add_ci2(logical_pathname, class_name, logical_pathname).
sf_sys_add_ci2(logical_pathname, sys_class_precedence_list, [pathname, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(logical_pathname, type_of, structure_class).
sf_sys_add_ci2(package, class_name, package).
sf_sys_add_ci2(package, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(package, type_of, structure_class).
sf_sys_add_ci2(pathname, class_name, pathname).
sf_sys_add_ci2(pathname, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(pathname, "subclass", logical_pathname).
sf_sys_add_ci2(pathname, type_of, structure_class).
sf_sys_add_ci2(readtable, class_name, readtable).
sf_sys_add_ci2(readtable, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(readtable, type_of, structure_class).
sf_sys_add_ci2(restart, class_name, restart).
sf_sys_add_ci2(restart, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(restart, type_of, structure_class).
sf_sys_add_ci2(sys_ansi_stream, "subclass", broadcast_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", concatenated_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", sys_fd_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", synonym_stream).
sf_sys_add_ci2(sys_ansi_stream, "subclass", two_way_stream).
sf_sys_add_ci2(sys_pf_method_function, class_name, sys_pf_method_function).
sf_sys_add_ci2(sys_pf_method_function, sys_class_precedence_list, function(t)).
sf_sys_add_ci2(sys_pf_method_function, type_of, structure_class).
sf_sys_add_ci2(sys_accessor_dfun_info, class_name, sys_accessor_dfun_info).
sf_sys_add_ci2(sys_accessor_dfun_info, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_accessor_dfun_info, "subclass", sys_n_n).
sf_sys_add_ci2(sys_accessor_dfun_info, "subclass", sys_one_index_dfun_info).
sf_sys_add_ci2(sys_accessor_dfun_info, type_of, structure_class).
sf_sys_add_ci2(sys_anode, class_name, sys_anode).
sf_sys_add_ci2(sys_anode, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_anode, type_of, structure_class).
sf_sys_add_ci2(sys_arg_info, class_name, sys_arg_info).
sf_sys_add_ci2(sys_arg_info, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_arg_info, type_of, structure_class).
sf_sys_add_ci2(sys_block_end, class_name, sys_block_end).
sf_sys_add_ci2(sys_block_end, sys_class_precedence_list, [sys_queued_op, structure_object, t]).
sf_sys_add_ci2(sys_block_end, type_of, structure_class).
sf_sys_add_ci2(sys_block_start, class_name, sys_block_start).
sf_sys_add_ci2(sys_block_start, sys_class_precedence_list, [sys_section_start, sys_queued_op, structure_object, t]).
sf_sys_add_ci2(sys_block_start, type_of, structure_class).
sf_sys_add_ci2(sys_c_source_point, class_name, sys_c_source_point).
sf_sys_add_ci2(sys_c_source_point, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_c_source_point, type_of, structure_class).
sf_sys_add_ci2(sys_caching, class_name, sys_caching).
sf_sys_add_ci2(sys_caching, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_caching, type_of, structure_class).
sf_sys_add_ci2(sys_checking, class_name, sys_checking).
sf_sys_add_ci2(sys_checking, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_checking, type_of, structure_class).
sf_sys_add_ci2(sys_class_precedence_description, class_name, sys_class_precedence_description).
sf_sys_add_ci2(sys_class_precedence_description, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_class_precedence_description, type_of, structure_class).
sf_sys_add_ci2(sys_const, class_name, sys_const).
sf_sys_add_ci2(sys_const, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_const, type_of, structure_class).
sf_sys_add_ci2(sys_constant_fast_method_call, class_name, sys_constant_fast_method_call).
sf_sys_add_ci2(sys_constant_fast_method_call, sys_class_precedence_list, [sys_fast_method_call, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_constant_fast_method_call, type_of, structure_class).
sf_sys_add_ci2(sys_constant_method_call, class_name, sys_constant_method_call).
sf_sys_add_ci2(sys_constant_method_call, sys_class_precedence_list, [sys_method_call, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_constant_method_call, type_of, structure_class).
sf_sys_add_ci2(sys_constant_value, class_name, sys_constant_value).
sf_sys_add_ci2(sys_constant_value, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_constant_value, type_of, structure_class).
sf_sys_add_ci2(sys_control_string_directive, class_name, sys_control_string_directive).
sf_sys_add_ci2(sys_control_string_directive, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_control_string_directive, type_of, structure_class).
sf_sys_add_ci2(sys_ctor, class_name, sys_ctor).
sf_sys_add_ci2(sys_ctor, sys_class_precedence_list, function(t)).
sf_sys_add_ci2(sys_ctor, type_of, structure_class).
sf_sys_add_ci2(sys_dead_beef_structure_object, class_name, sys_dead_beef_structure_object).
sf_sys_add_ci2(sys_dead_beef_structure_object, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_dead_beef_structure_object, type_of, structure_class).
sf_sys_add_ci2(sys_default_method_only, class_name, sys_default_method_only).
sf_sys_add_ci2(sys_default_method_only, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_default_method_only, type_of, structure_class).
sf_sys_add_ci2(sys_dfun_info, class_name, sys_dfun_info).
sf_sys_add_ci2(sys_dfun_info, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_accessor_dfun_info).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_caching).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_checking).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_constant_value).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_default_method_only).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_dispatch).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_initial).
sf_sys_add_ci2(sys_dfun_info, "subclass", sys_no_methods).
sf_sys_add_ci2(sys_dfun_info, type_of, structure_class).
sf_sys_add_ci2(sys_dispatch, class_name, sys_dispatch).
sf_sys_add_ci2(sys_dispatch, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_dispatch, type_of, structure_class).
sf_sys_add_ci2(sys_fast_instance_boundp, class_name, sys_fast_instance_boundp).
sf_sys_add_ci2(sys_fast_instance_boundp, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_fast_instance_boundp, type_of, structure_class).
sf_sys_add_ci2(sys_fast_method_call, class_name, sys_fast_method_call).
sf_sys_add_ci2(sys_fast_method_call, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_fast_method_call, "subclass", sys_constant_fast_method_call).
sf_sys_add_ci2(sys_fast_method_call, type_of, structure_class).
sf_sys_add_ci2(sys_indentation, class_name, sys_indentation).
sf_sys_add_ci2(sys_indentation, sys_class_precedence_list, [sys_queued_op, structure_object, t]).
sf_sys_add_ci2(sys_indentation, type_of, structure_class).
sf_sys_add_ci2(sys_initial, class_name, sys_initial).
sf_sys_add_ci2(sys_initial, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_initial, type_of, structure_class).
sf_sys_add_ci2(sys_input_character, class_name, sys_input_character).
sf_sys_add_ci2(sys_input_character, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_input_character, type_of, structure_class).
sf_sys_add_ci2(sys_logical_block, class_name, sys_logical_block).
sf_sys_add_ci2(sys_logical_block, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_logical_block, type_of, structure_class).
sf_sys_add_ci2(sys_loop_initialization, class_name, sys_loop_initialization).
sf_sys_add_ci2(sys_loop_initialization, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_loop_initialization, type_of, structure_class).
sf_sys_add_ci2(sys_method_call, class_name, sys_method_call).
sf_sys_add_ci2(sys_method_call, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_method_call, "subclass", sys_constant_method_call).
sf_sys_add_ci2(sys_method_call, type_of, structure_class).
sf_sys_add_ci2(sys_n_n, class_name, sys_n_n).
sf_sys_add_ci2(sys_n_n, sys_class_precedence_list, [sys_accessor_dfun_info, sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_n_n, type_of, structure_class).
sf_sys_add_ci2(sys_newline, class_name, sys_newline).
sf_sys_add_ci2(sys_newline, sys_class_precedence_list, [sys_section_start, sys_queued_op, structure_object, t]).
sf_sys_add_ci2(sys_newline, type_of, structure_class).
sf_sys_add_ci2(sys_no_methods, class_name, sys_no_methods).
sf_sys_add_ci2(sys_no_methods, sys_class_precedence_list, [sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_no_methods, type_of, structure_class).
sf_sys_add_ci2(sys_one_class, class_name, sys_one_class).
sf_sys_add_ci2(sys_one_class, sys_class_precedence_list, [sys_one_index_dfun_info, sys_accessor_dfun_info, sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_one_class, "subclass", sys_two_class).
sf_sys_add_ci2(sys_one_class, type_of, structure_class).
sf_sys_add_ci2(sys_one_index, class_name, sys_one_index).
sf_sys_add_ci2(sys_one_index, sys_class_precedence_list, [sys_one_index_dfun_info, sys_accessor_dfun_info, sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_one_index, type_of, structure_class).
sf_sys_add_ci2(sys_one_index_dfun_info, class_name, sys_one_index_dfun_info).
sf_sys_add_ci2(sys_one_index_dfun_info, sys_class_precedence_list, [sys_accessor_dfun_info, sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_one_index_dfun_info, "subclass", sys_one_class).
sf_sys_add_ci2(sys_one_index_dfun_info, "subclass", sys_one_index).
sf_sys_add_ci2(sys_one_index_dfun_info, type_of, structure_class).
sf_sys_add_ci2(sys_pprint_dispatch_entry, class_name, sys_pprint_dispatch_entry).
sf_sys_add_ci2(sys_pprint_dispatch_entry, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, type_of, structure_class).
sf_sys_add_ci2(sys_pprint_dispatch_table, class_name, sys_pprint_dispatch_table).
sf_sys_add_ci2(sys_pprint_dispatch_table, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_pprint_dispatch_table, type_of, structure_class).
sf_sys_add_ci2(sys_pv_table, class_name, sys_pv_table).
sf_sys_add_ci2(sys_pv_table, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_pv_table, type_of, structure_class).
sf_sys_add_ci2(sys_queued_op, class_name, sys_queued_op).
sf_sys_add_ci2(sys_queued_op, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_queued_op, "subclass", sys_block_end).
sf_sys_add_ci2(sys_queued_op, "subclass", sys_indentation).
sf_sys_add_ci2(sys_queued_op, "subclass", sys_section_start).
sf_sys_add_ci2(sys_queued_op, "subclass", sys_tab).
sf_sys_add_ci2(sys_queued_op, type_of, structure_class).
sf_sys_add_ci2(sys_section_start, class_name, sys_section_start).
sf_sys_add_ci2(sys_section_start, sys_class_precedence_list, [sys_queued_op, structure_object, t]).
sf_sys_add_ci2(sys_section_start, "subclass", sys_block_start).
sf_sys_add_ci2(sys_section_start, "subclass", sys_newline).
sf_sys_add_ci2(sys_section_start, type_of, structure_class).
sf_sys_add_ci2(sys_slot_info, class_name, sys_slot_info).
sf_sys_add_ci2(sys_slot_info, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_slot_info, type_of, structure_class).
sf_sys_add_ci2(sys_slot_object, "subclass", structure_object).
sf_sys_add_ci2(sys_standard_funcallable_instance, class_name, sys_standard_funcallable_instance).
sf_sys_add_ci2(sys_standard_funcallable_instance, sys_class_precedence_list, function(t)).
sf_sys_add_ci2(sys_standard_funcallable_instance, type_of, structure_class).
sf_sys_add_ci2(sys_system_stream, class_name, sys_system_stream).
sf_sys_add_ci2(sys_system_stream, sys_class_precedence_list, [stream, structure_object, t]).
sf_sys_add_ci2(sys_system_stream, "subclass", broadcast_stream).
sf_sys_add_ci2(sys_system_stream, "subclass", concatenated_stream).
sf_sys_add_ci2(sys_system_stream, "subclass", echo_stream).
sf_sys_add_ci2(sys_system_stream, "subclass", file_stream).
sf_sys_add_ci2(sys_system_stream, "subclass", string_stream).
sf_sys_add_ci2(sys_system_stream, "subclass", synonym_stream).
sf_sys_add_ci2(sys_system_stream, "subclass", two_way_stream).
sf_sys_add_ci2(sys_system_stream, type_of, structure_class).
sf_sys_add_ci2(sys_tab, class_name, sys_tab).
sf_sys_add_ci2(sys_tab, sys_class_precedence_list, [sys_queued_op, structure_object, t]).
sf_sys_add_ci2(sys_tab, type_of, structure_class).
sf_sys_add_ci2(sys_two_class, class_name, sys_two_class).
sf_sys_add_ci2(sys_two_class, sys_class_precedence_list, [sys_one_class, sys_one_index_dfun_info, sys_accessor_dfun_info, sys_dfun_info, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_two_class, type_of, structure_class).
sf_sys_add_ci2(sys_fd_stream, class_name, sys_fd_stream).
sf_sys_add_ci2(sys_fd_stream, sys_class_precedence_list, [file_stream, sys_ansi_stream, stream, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_fd_stream, type_of, structure_class).
sf_sys_add_ci2(sys_structure_c33_object, class_name, sys_structure_c33_object).
sf_sys_add_ci2(sys_structure_c33_object, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_structure_c33_object, type_of, structure_class).
sf_sys_add_ci2(sys_structure_stablehash, class_name, sys_structure_stablehash).
sf_sys_add_ci2(sys_structure_stablehash, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(sys_structure_stablehash, type_of, structure_class).
sf_sys_add_ci2(sys_timer, class_name, sys_timer).
sf_sys_add_ci2(sys_timer, sys_class_precedence_list, [structure_object, sys_slot_object, t]).
sf_sys_add_ci2(sys_timer, type_of, structure_class).
sf_sys_add_ci2(stream, class_name, stream).
sf_sys_add_ci2(stream, sys_class_precedence_list, [structure_object, t]).
sf_sys_add_ci2(stream, "subclass", sys_system_stream).
sf_sys_add_ci2(stream, type_of, structure_class).
sf_sys_add_ci2(string_stream, class_name, string_stream).
sf_sys_add_ci2(string_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(string_stream, type_of, structure_class).
sf_sys_add_ci2(structure_object, class_name, structure_object).
sf_sys_add_ci2(structure_object, sys_class_precedence_list, [sys_slot_object, t]).
sf_sys_add_ci2(structure_object, sys_class_precedence_list, [t]).
sf_sys_add_ci2(structure_object, "subclass", hash_table).
sf_sys_add_ci2(structure_object, "subclass", package).
sf_sys_add_ci2(structure_object, "subclass", pathname).
sf_sys_add_ci2(structure_object, "subclass", random_state).
sf_sys_add_ci2(structure_object, "subclass", readtable).
sf_sys_add_ci2(structure_object, "subclass", restart).
sf_sys_add_ci2(structure_object, "subclass", sys_anode).
sf_sys_add_ci2(structure_object, "subclass", sys_arg_info).
sf_sys_add_ci2(structure_object, "subclass", sys_c_source_point).
sf_sys_add_ci2(structure_object, "subclass", sys_cache).
sf_sys_add_ci2(structure_object, "subclass", sys_class_precedence_description).
sf_sys_add_ci2(structure_object, "subclass", sys_const).
sf_sys_add_ci2(structure_object, "subclass", sys_control_string_directive).
sf_sys_add_ci2(structure_object, "subclass", sys_dead_beef_structure_object).
sf_sys_add_ci2(structure_object, "subclass", sys_dfun_info).
sf_sys_add_ci2(structure_object, "subclass", sys_fast_instance_boundp).
sf_sys_add_ci2(structure_object, "subclass", sys_fast_method_call).
sf_sys_add_ci2(structure_object, "subclass", sys_fgen).
sf_sys_add_ci2(structure_object, "subclass", sys_fnode).
sf_sys_add_ci2(structure_object, "subclass", sys_input_character).
sf_sys_add_ci2(structure_object, "subclass", sys_inspection).
sf_sys_add_ci2(structure_object, "subclass", sys_logical_block).
sf_sys_add_ci2(structure_object, "subclass", sys_loop_initialization).
sf_sys_add_ci2(structure_object, "subclass", sys_method_call).
sf_sys_add_ci2(structure_object, "subclass", sys_pprint_dispatch_entry).
sf_sys_add_ci2(structure_object, "subclass", sys_pprint_dispatch_table).
sf_sys_add_ci2(structure_object, "subclass", sys_pv_table).
sf_sys_add_ci2(structure_object, "subclass", sys_queued_op).
sf_sys_add_ci2(structure_object, "subclass", sys_slot_info).
sf_sys_add_ci2(structure_object, "subclass", sys_var).
sf_sys_add_ci2(structure_object, "subclass", sys_structure_c33_object).
sf_sys_add_ci2(structure_object, "subclass", sys_structure_stablehash).
sf_sys_add_ci2(structure_object, "subclass", sys_timer).
sf_sys_add_ci2(structure_object, "subclass", stream).
sf_sys_add_ci2(structure_object, type_of, structure_class).
sf_sys_add_ci2(structure_object, type_of, structure_class).
sf_sys_add_ci2(synonym_stream, class_name, synonym_stream).
sf_sys_add_ci2(synonym_stream, sys_class_precedence_list, [sys_ansi_stream, stream, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(synonym_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(synonym_stream, type_of, structure_class).
sf_sys_add_ci2(t, "subclass", structure_object).
sf_sys_add_ci2(two_way_stream, class_name, two_way_stream).
sf_sys_add_ci2(two_way_stream, sys_class_precedence_list, [sys_ansi_stream, stream, structure_object, sys_slot_object, t]).
sf_sys_add_ci2(two_way_stream, sys_class_precedence_list, [sys_system_stream, stream, structure_object, t]).
sf_sys_add_ci2(two_way_stream, "subclass", echo_stream).
sf_sys_add_ci2(two_way_stream, type_of, structure_class).
sf_sys_add_ci2(file_stream, class_name, file_stream).
sf_sys_add_ci2(file_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(file_stream, "subclass", sys_fd_stream).
sf_sys_add_ci2(file_stream, type_of, sys_system_class).
sf_sys_add_ci2(function, class_name, function).
sf_sys_add_ci2(function, sys_class_precedence_list, [t]).
sf_sys_add_ci2(function, "subclass", sys_funcallable_standard_object).
sf_sys_add_ci2(function, "subclass", sys_pf_method_function).
sf_sys_add_ci2(function, "subclass", sys_ctor).
sf_sys_add_ci2(function, "subclass", sys_standard_funcallable_instance).
sf_sys_add_ci2(function, type_of, sys_system_class).
sf_sys_add_ci2(sequence, class_name, sequence).
sf_sys_add_ci2(sequence, sys_class_precedence_list, [t]).
sf_sys_add_ci2(sequence, "subclass", list).
sf_sys_add_ci2(sequence, "subclass", vector).
sf_sys_add_ci2(sequence, type_of, sys_system_class).
sf_sys_add_ci2(stream, class_name, stream).
sf_sys_add_ci2(stream, sys_class_precedence_list, [t]).
sf_sys_add_ci2(stream, "subclass", file_stream).
sf_sys_add_ci2(stream, "subclass", string_stream).
sf_sys_add_ci2(stream, type_of, sys_system_class).
sf_sys_add_ci2(string_stream, class_name, string_stream).
sf_sys_add_ci2(string_stream, sys_class_precedence_list, [stream, t]).
sf_sys_add_ci2(string_stream, type_of, sys_system_class).
sf_sys_add_ci2(t, class_name, t).
sf_sys_add_ci2(t, "subclass", array).
sf_sys_add_ci2(t, "subclass", character).
sf_sys_add_ci2(t, "subclass", function).
sf_sys_add_ci2(t, "subclass", number).
sf_sys_add_ci2(t, "subclass", sequence).
sf_sys_add_ci2(t, "subclass", sys_slot_object).
sf_sys_add_ci2(t, "subclass", sys_simd_pack).
sf_sys_add_ci2(t, "subclass", sys_system_area_pointer).
sf_sys_add_ci2(t, "subclass", sys_weak_pointer).
sf_sys_add_ci2(t, "subclass", stream).
sf_sys_add_ci2(t, "subclass", symbol).
sf_sys_add_ci2(t, type_of, sys_system_class).

:- dynamic sf_sys_add_ci2/6.

sf_sys_add_ci2(function, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(function, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(list, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(list, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(null, "method", sys_ensure_class_using_class, kw_name, sys_ensure_class_using_class).
sf_sys_add_ci2(null, "method", sys_ensure_generic_function_using_class, kw_name, sys_ensure_generic_function_using_class).
sf_sys_add_ci2(package, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(package, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(symbol, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(symbol, "method", [setf, sys_slot_value_using_class], kw_name, [setf, sys_slot_value_using_class]).
sf_sys_add_ci2(symbol, "method", change_class, kw_name, change_class).
sf_sys_add_ci2(symbol, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(symbol, "method", make_instance, kw_name, make_instance).
sf_sys_add_ci2(symbol, "method", make_instances_obsolete, kw_name, make_instances_obsolete).
sf_sys_add_ci2(symbol, "method", sys_find_method_combination, kw_name, sys_find_method_combination).
sf_sys_add_ci2(symbol, "method", sys_slot_boundp_using_class, kw_name, sys_slot_boundp_using_class).
sf_sys_add_ci2(symbol, "method", sys_slot_makunbound_using_class, kw_name, sys_slot_makunbound_using_class).
sf_sys_add_ci2(symbol, "method", sys_slot_value_using_class, kw_name, sys_slot_value_using_class).
sf_sys_add_ci2(t, "method", [setf, class_name], kw_name, [setf, class_name]).
sf_sys_add_ci2(t, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(t, "method", [setf, sys_class_layout], kw_name, [setf, sys_class_layout]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_allocation_class], kw_name, [setf, sys_slot_definition_allocation_class]).
sf_sys_add_ci2(t, "method", [setf, sys_class_default_initargs], kw_name, [setf, sys_class_default_initargs]).
sf_sys_add_ci2(t, "method", [setf, sys_class_direct_default_initargs], kw_name, [setf, sys_class_direct_default_initargs]).
sf_sys_add_ci2(t, "method", [setf, sys_class_direct_methods], kw_name, [setf, sys_class_direct_methods]).
sf_sys_add_ci2(t, "method", [setf, sys_class_direct_slots], kw_name, [setf, sys_class_direct_slots]).
sf_sys_add_ci2(t, "method", [setf, sys_class_direct_subclasses], kw_name, [setf, sys_class_direct_subclasses]).
sf_sys_add_ci2(t, "method", [setf, sys_class_direct_superclasses], kw_name, [setf, sys_class_direct_superclasses]).
sf_sys_add_ci2(t, "method", [setf, sys_class_finalized_p], kw_name, [setf, sys_class_finalized_p]).
sf_sys_add_ci2(t, "method", [setf, sys_class_precedence_list], kw_name, [setf, sys_class_precedence_list]).
sf_sys_add_ci2(t, "method", [setf, sys_class_slots], kw_name, [setf, sys_class_slots]).
sf_sys_add_ci2(t, "method", [setf, sys_generic_function_name], kw_name, [setf, sys_generic_function_name]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_allocation], kw_name, [setf, sys_slot_definition_allocation]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_documentation], kw_name, [setf, sys_slot_definition_documentation]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_initargs], kw_name, [setf, sys_slot_definition_initargs]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_initform], kw_name, [setf, sys_slot_definition_initform]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_initfunction], kw_name, [setf, sys_slot_definition_initfunction]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_location], kw_name, [setf, sys_slot_definition_location]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_name], kw_name, [setf, sys_slot_definition_name]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_readers], kw_name, [setf, sys_slot_definition_readers]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_type], kw_name, [setf, sys_slot_definition_type]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_definition_writers], kw_name, [setf, sys_slot_definition_writers]).
sf_sys_add_ci2(t, "method", [setf, sys_slot_value_using_class], kw_name, [setf, sys_slot_value_using_class]).
sf_sys_add_ci2(t, "method", compute_applicable_methods, kw_name, compute_applicable_methods).
sf_sys_add_ci2(t, "method", describe_object, kw_name, describe_object).
sf_sys_add_ci2(t, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(t, "method", find_method, kw_name, find_method).
sf_sys_add_ci2(t, "method", make_load_form, kw_name, make_load_form).
sf_sys_add_ci2(t, "method", no_applicable_method, kw_name, no_applicable_method).
sf_sys_add_ci2(t, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(t, "method", shared_initialize, kw_name, shared_initialize).
sf_sys_add_ci2(t, "method", sys_method_more_specific_p, kw_name, sys_method_more_specific_p).
sf_sys_add_ci2(t, "method", sys_slot_exists_p_using_class, kw_name, sys_slot_exists_p_using_class).
sf_sys_add_ci2(t, "method", sys_add_dependent, kw_name, sys_add_dependent).
sf_sys_add_ci2(t, "method", sys_class_prototype, kw_name, sys_class_prototype).
sf_sys_add_ci2(t, "method", sys_compute_applicable_methods_using_classes, kw_name, sys_compute_applicable_methods_using_classes).
sf_sys_add_ci2(t, "method", sys_compute_effective_method, kw_name, sys_compute_effective_method).
sf_sys_add_ci2(t, "method", sys_compute_effective_slot_definition, kw_name, sys_compute_effective_slot_definition).
sf_sys_add_ci2(t, "method", sys_ensure_class_using_class, kw_name, sys_ensure_class_using_class).
sf_sys_add_ci2(t, "method", sys_ensure_generic_function_using_class, kw_name, sys_ensure_generic_function_using_class).
sf_sys_add_ci2(t, "method", sys_find_method_combination, kw_name, sys_find_method_combination).
sf_sys_add_ci2(t, "method", sys_make_method_lambda, kw_name, sys_make_method_lambda).
sf_sys_add_ci2(t, "method", sys_map_dependents, kw_name, sys_map_dependents).
sf_sys_add_ci2(t, "method", sys_remove_dependent, kw_name, sys_remove_dependent).
sf_sys_add_ci2(t, "method", sys_slot_boundp_using_class, kw_name, sys_slot_boundp_using_class).
sf_sys_add_ci2(t, "method", sys_slot_makunbound_using_class, kw_name, sys_slot_makunbound_using_class).
sf_sys_add_ci2(t, "method", sys_slot_value_using_class, kw_name, sys_slot_value_using_class).
sf_sys_add_ci2(t, "method", slot_missing, kw_name, slot_missing).
sf_sys_add_ci2(t, "method", slot_unbound, kw_name, slot_unbound).
sf_sys_add_ci2(t, "method", update_instance_for_redefined_class, kw_name, update_instance_for_redefined_class).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operands, kw_initargs, [kw_operands]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operands, kw_readers, [arithmetic_error_operands]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operation, kw_initargs, [kw_operation]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operation, kw_initform, [quote, []]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operation, kw_readers, [arithmetic_error_operation]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operands, kw_initargs, [kw_operands]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operands, kw_initform, [quote, []]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operands, kw_readers, [arithmetic_error_operands]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operation, kw_initargs, [kw_operation]).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operation, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(arithmetic_error, "slot", sys_operation, kw_readers, [arithmetic_error_operation]).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_readers, [cell_error_name]).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_readers, [cell_error_name]).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(cell_error, "slot", sys_name, kw_readers, [cell_error_name]).
sf_sys_add_ci2(end_of_file, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\('U'), #\(n), #\(e), #\(x), #\(p), #\(e), #\(c), #\(t), #\(e), #\(d), #\(' '), #\(e), #\(n), #\(d), #\(' '), #\(o), #\(f), #\(' '), #\(f), #\(i), #\(l), #\(e), #\(' '), #\(o), #\(n), #\(' '), #\(~), #\('S'), #\('.')]), [stream_error_stream, condition]]]).
sf_sys_add_ci2(file_error, "slot", pathname, kw_initargs, [kw_pathname]).
sf_sys_add_ci2(file_error, "slot", pathname, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(file_error, "slot", pathname, kw_readers, [file_error_pathname]).
sf_sys_add_ci2(file_error, "slot", pathname, kw_initargs, [kw_pathname]).
sf_sys_add_ci2(file_error, "slot", pathname, kw_readers, [file_error_pathname]).
sf_sys_add_ci2(file_error, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\('F'), #\(i), #\(l), #\(e), #\(s), #\(y), #\(s), #\(t), #\(e), #\(m), #\(' '), #\(e), #\(r), #\(r), #\(o), #\(r), #\(' '), #\(w), #\(i), #\(t), #\(h), #\(' '), #\(p), #\(a), #\(t), #\(h), #\(n), #\(a), #\(m), #\(e), #\(' '), #\(~), #\('S'), #\('.'), #\(~), #\('%'), #\('E'), #\(i), #\(t), #\(h), #\(e), #\(r), #\(' '), #\('1'), #\(')'), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(f), #\(i), #\(l), #\(e), #\(' '), #\(d), #\(o), #\(e), #\(s), #\(' '), #\(n), #\(o), #\(t), #\(' '), #\(e), #\(x), #\(i), #\(s), #\(t), #\(','), #\(' '), #\(o), #\(r), #\(' '), #\('2'), #\(')'), #\(' '), #\(w), #\(e), #\(' '), #\(a), #\(r), #\(e), #\(' '), #\(n), #\(o), #\(t), #\(' '), #\(a), #\(l), #\(l), #\(o), #\(w), #\(e), #\(d), #\(' '), #\(t), #\(o), #\(' '), #\(a), #\(c), #\(c), #\(e), #\(s), #\(s), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(f), #\(i), #\(l), #\(e), #\(','), #\(' '), #\(o), #\(r), #\(' '), #\('3'), #\(')'), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(p), #\(a), #\(t), #\(h), #\(n), #\(a), #\(m), #\(e), #\(' '), #\(p), #\(o), #\(i), #\(n), #\(t), #\(s), #\(' '), #\(t), #\(o), #\(' '), #\(a), #\(' '), #\(b), #\(r), #\(o), #\(k), #\(e), #\(n), #\(' '), #\(s), #\(y), #\(m), #\(b), #\(o), #\(l), #\(i), #\(c), #\(' '), #\(l), #\(i), #\(n), #\(k), #\('.')]), [file_error_pathname, condition]]]).
sf_sys_add_ci2(package_error, "slot", package, kw_initargs, [kw_package]).
sf_sys_add_ci2(package_error, "slot", package, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(package_error, "slot", package, kw_readers, [package_error_package]).
sf_sys_add_ci2(package_error, "slot", package, kw_initargs, [kw_package]).
sf_sys_add_ci2(package_error, "slot", package, kw_readers, [package_error_package]).
sf_sys_add_ci2(print_not_readable, "slot", sys_object, kw_initargs, [kw_object]).
sf_sys_add_ci2(print_not_readable, "slot", sys_object, kw_readers, [print_not_readable_object]).
sf_sys_add_ci2(print_not_readable, "slot", sys_object, kw_initargs, [kw_object]).
sf_sys_add_ci2(print_not_readable, "slot", sys_object, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(print_not_readable, "slot", sys_object, kw_readers, [print_not_readable_object]).
sf_sys_add_ci2(print_not_readable, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\('C'), #\(a), #\(n), #\(n), #\(o), #\(t), #\(' '), #\(p), #\(r), #\(i), #\(n), #\(t), #\(' '), #\(o), #\(b), #\(j), #\(e), #\(c), #\(t), #\(' '), #\(~), #\('A'), #\(' '), #\(r), #\(e), #\(a), #\(d), #\(a), #\(b), #\(l), #\(y), #\('.')]), [print_not_readable_object, condition]]]).
sf_sys_add_ci2(sys_abort_failure, "slot", sys_report_function, kw_initform, '$ARRAY'([*], claz_base_character, [#\('A'), #\(b), #\(o), #\(r), #\(t), #\(' '), #\(f), #\(a), #\(i), #\(l), #\(e), #\(d), #\('.')])).
sf_sys_add_ci2(sys_case_failure, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_case_failure, "slot", sys_name, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_case_failure, "slot", sys_name, kw_readers, [sys_case_failure_name]).
sf_sys_add_ci2(sys_case_failure, "slot", sys_possibilities, kw_initargs, [kw_possibilities]).
sf_sys_add_ci2(sys_case_failure, "slot", sys_possibilities, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_case_failure, "slot", sys_possibilities, kw_readers, [sys_case_failure_possibilities]).
sf_sys_add_ci2(sys_case_failure, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\(~), #\('S'), #\(' '), #\(f), #\(e), #\(l), #\(l), #\(' '), #\(t), #\(h), #\(r), #\(o), #\(u), #\(g), #\(h), #\(' '), #\(~), #\('S'), #\(' '), #\(e), #\(x), #\(p), #\(r), #\(e), #\(s), #\(s), #\(i), #\(o), #\(n), #\('.'), #\(~), #\('%'), #\('W'), #\(a), #\(n), #\(t), #\(e), #\(d), #\(' '), #\(o), #\(n), #\(e), #\(' '), #\(o), #\(f), #\(' '), #\(~), #\(:), #\('S'), #\('.')]), [type_error_datum, condition], [sys_case_failure_name, condition], [sys_case_failure_possibilities, condition]]]).
sf_sys_add_ci2(sys_cpl_protocol_violation, "slot", class, kw_initargs, [kw_class]).
sf_sys_add_ci2(sys_cpl_protocol_violation, "slot", class, kw_readers, [sys_cpl_protocol_violation_class]).
sf_sys_add_ci2(sys_cpl_protocol_violation, "slot", sys_cpl, kw_initargs, [kw_cpl]).
sf_sys_add_ci2(sys_cpl_protocol_violation, "slot", sys_cpl, kw_readers, [sys_cpl_protocol_violation_cpl]).
sf_sys_add_ci2(sys_initarg_error, "slot", class, kw_initargs, [kw_class]).
sf_sys_add_ci2(sys_initarg_error, "slot", class, kw_readers, [sys_initarg_error_class]).
sf_sys_add_ci2(sys_initarg_error, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_initarg_error, "slot", sys_initargs, kw_readers, [sys_initarg_error_initargs]).
sf_sys_add_ci2(sys_instance_structure_protocol_error, "slot", sys_fun, kw_initargs, [kw_fun]).
sf_sys_add_ci2(sys_instance_structure_protocol_error, "slot", sys_fun, kw_readers, [sys_instance_structure_protocol_error_fun]).
sf_sys_add_ci2(sys_instance_structure_protocol_error, "slot", sys_slotd, kw_initargs, [kw_slotd]).
sf_sys_add_ci2(sys_instance_structure_protocol_error, "slot", sys_slotd, kw_readers, [sys_instance_structure_protocol_error_slotd]).
sf_sys_add_ci2(sys_invalid_superclass, "slot", class, kw_initargs, [kw_class]).
sf_sys_add_ci2(sys_invalid_superclass, "slot", class, kw_readers, [sys_invalid_superclass_class]).
sf_sys_add_ci2(sys_invalid_superclass, "slot", sys_superclass, kw_initargs, [kw_superclass]).
sf_sys_add_ci2(sys_invalid_superclass, "slot", sys_superclass, kw_readers, [sys_invalid_superclass_superclass]).
sf_sys_add_ci2(sys_missing_load_form, "slot", sys_object, kw_initargs, [kw_object]).
sf_sys_add_ci2(sys_missing_load_form, "slot", sys_object, kw_readers, [sys_missing_load_form_object]).
sf_sys_add_ci2(sys_new_value_specialization, "slot", sys_pf_method, kw_initargs, [kw_method]).
sf_sys_add_ci2(sys_new_value_specialization, "slot", sys_pf_method, kw_readers, [sys_new_value_specialization_method]).
sf_sys_add_ci2(sys_no_primary_method, "slot", generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_no_primary_method, "slot", generic_function, kw_readers, [sys_no_primary_method_generic_function]).
sf_sys_add_ci2(sys_no_primary_method, "slot", sys_args, kw_initargs, [kw_args]).
sf_sys_add_ci2(sys_no_primary_method, "slot", sys_args, kw_readers, [sys_no_primary_method_args]).
sf_sys_add_ci2(sys_obsolete_structure, "slot", sys_datum, kw_initargs, [kw_datum]).
sf_sys_add_ci2(sys_obsolete_structure, "slot", sys_datum, kw_readers, [sys_obsolete_structure_datum]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_initarg, kw_initargs, [kw_initarg]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_initarg, kw_readers, [sys_slotd_initialization_error_initarg]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_kind, kw_initargs, [kw_kind]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_kind, kw_readers, [sys_slotd_initialization_error_kind]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_value, kw_initargs, [kw_value]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_value, kw_initform, [quote, []]).
sf_sys_add_ci2(sys_slotd_initialization_error, "slot", sys_value, kw_readers, [sys_slotd_initialization_error_value]).
sf_sys_add_ci2(sys_slotd_initialization_type_error, "slot", sys_value, kw_initargs, [kw_datum, kw_value]).
sf_sys_add_ci2(sys_slotd_initialization_type_error, "slot", sys_value, kw_initform, [quote, []]).
sf_sys_add_ci2(sys_defconstant_uneql, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_defconstant_uneql, "slot", sys_name, kw_readers, [sys_defconstant_uneql_name]).
sf_sys_add_ci2(sys_defconstant_uneql, "slot", sys_new_value, kw_initargs, [kw_new_value]).
sf_sys_add_ci2(sys_defconstant_uneql, "slot", sys_new_value, kw_readers, [sys_defconstant_uneql_new_value]).
sf_sys_add_ci2(sys_defconstant_uneql, "slot", sys_old_value, kw_initargs, [kw_old_value]).
sf_sys_add_ci2(sys_defconstant_uneql, "slot", sys_old_value, kw_readers, [sys_defconstant_uneql_old_value]).
sf_sys_add_ci2(sys_format_error, "slot", sys_control_string, kw_initargs, [kw_control_string]).
sf_sys_add_ci2(sys_format_error, "slot", sys_control_string, kw_initform, sys_xx_default_format_error_control_string_xx).
sf_sys_add_ci2(sys_format_error, "slot", sys_control_string, kw_readers, [sys_format_error_control_string]).
sf_sys_add_ci2(sys_format_error, "slot", sys_format_arguments, kw_initargs, [kw_arguments]).
sf_sys_add_ci2(sys_format_error, "slot", sys_format_arguments, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_format_error, "slot", sys_format_control, kw_initargs, [kw_complaint]).
sf_sys_add_ci2(sys_format_error, "slot", sys_format_control, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_format_error, "slot", sys_offset, kw_initargs, [kw_offset]).
sf_sys_add_ci2(sys_format_error, "slot", sys_offset, kw_initform, sys_xx_default_format_error_offset_xx).
sf_sys_add_ci2(sys_format_error, "slot", sys_offset, kw_readers, [sys_format_error_offset]).
sf_sys_add_ci2(sys_format_error, "slot", sys_print_banner, kw_initargs, [kw_print_banner]).
sf_sys_add_ci2(sys_format_error, "slot", sys_print_banner, kw_initform, t).
sf_sys_add_ci2(sys_format_error, "slot", sys_print_banner, kw_readers, [sys_format_error_print_banner]).
sf_sys_add_ci2(sys_format_error, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\(~), #\(:), #\('['), #\(~), #\(;), #\('E'), #\(r), #\(r), #\(o), #\(r), #\(' '), #\(i), #\(n), #\(' '), #\(f), #\(o), #\(r), #\(m), #\(a), #\(t), #\(:), #\(' '), #\(~), #\(']'), #\(~), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(' '), #\(~), #\(?), #\(~), #\(@), #\('['), #\(~), #\('%'), #\(' '), #\(' '), #\(~), #\('A'), #\(~), #\('%'), #\(' '), #\(' '), #\(~), #\('V'), #\(@), #\('T'), #\(^), #\(~), #\(']')]), [sys_format_error_print_banner, condition], [simple_condition_format_control, condition], [simple_condition_format_arguments, condition], [sys_format_error_control_string, condition], [sys_format_error_offset, condition]]]).
sf_sys_add_ci2(sys_implicit_generic_function_warning, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_implicit_generic_function_warning, "slot", sys_name, kw_readers, [sys_implicit_generic_function_name]).
sf_sys_add_ci2(sys_invalid_fasl, "slot", sys_expected, kw_initargs, [kw_expected]).
sf_sys_add_ci2(sys_invalid_fasl, "slot", sys_expected, kw_readers, [sys_invalid_fasl_expected]).
sf_sys_add_ci2(sys_invalid_fasl, "slot", stream, kw_initargs, [kw_stream]).
sf_sys_add_ci2(sys_invalid_fasl, "slot", stream, kw_readers, [sys_invalid_fasl_stream]).
sf_sys_add_ci2(sys_io_timeout, "slot", sys_direction, kw_initargs, [kw_direction]).
sf_sys_add_ci2(sys_io_timeout, "slot", sys_direction, kw_readers, [sys_io_timeout_direction]).
sf_sys_add_ci2(sys_method_call_error, "slot", sys_args, kw_initargs, [kw_argument_list]).
sf_sys_add_ci2(sys_method_call_error, "slot", sys_args, kw_readers, [sys_method_call_error_argument_list]).
sf_sys_add_ci2(sys_method_call_error, "slot", sys_gf, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_method_call_error, "slot", sys_gf, kw_readers, [sys_method_call_error_generic_function]).
sf_sys_add_ci2(sys_method_call_error, "slot", method, kw_initargs, [kw_method]).
sf_sys_add_ci2(sys_method_call_error, "slot", method, kw_readers, [sys_method_call_error_method]).
sf_sys_add_ci2(sys_name_conflict, "slot", function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_name_conflict, "slot", function, kw_readers, [sys_name_conflict_function]).
sf_sys_add_ci2(sys_name_conflict, "slot", sys_datum, kw_initargs, [kw_datum]).
sf_sys_add_ci2(sys_name_conflict, "slot", sys_datum, kw_readers, [sys_name_conflict_datum]).
sf_sys_add_ci2(sys_name_conflict, "slot", sys_symbols, kw_initargs, [kw_symbols]).
sf_sys_add_ci2(sys_name_conflict, "slot", sys_symbols, kw_readers, [sys_name_conflict_symbols]).
sf_sys_add_ci2(sys_package_lock_violation, "slot", sys_current_package, kw_initform, [quote, xx_package_xx]).
sf_sys_add_ci2(sys_package_lock_violation, "slot", sys_current_package, kw_readers, [sys_package_lock_violation_in_package]).
sf_sys_add_ci2(sys_step_condition, "slot", sys_form, kw_initargs, [kw_form]).
sf_sys_add_ci2(sys_step_condition, "slot", sys_form, kw_readers, [sys_step_condition_form]).
sf_sys_add_ci2(sys_step_form_condition, "slot", sys_args, kw_initargs, [kw_args]).
sf_sys_add_ci2(sys_step_form_condition, "slot", sys_args, kw_readers, [sys_step_condition_args]).
sf_sys_add_ci2(sys_symbol_package_locked_error, "slot", symbol, kw_initargs, [kw_symbol]).
sf_sys_add_ci2(sys_symbol_package_locked_error, "slot", symbol, kw_readers, [sys_package_locked_error_symbol]).
sf_sys_add_ci2(sys_system_condition, "slot", sys_address, kw_initargs, [kw_address]).
sf_sys_add_ci2(sys_system_condition, "slot", sys_address, kw_initform, [quote, []]).
sf_sys_add_ci2(sys_system_condition, "slot", sys_address, kw_readers, [sys_system_condition_address]).
sf_sys_add_ci2(sys_system_condition, "slot", sys_context, kw_initargs, [kw_context]).
sf_sys_add_ci2(sys_system_condition, "slot", sys_context, kw_initform, [quote, []]).
sf_sys_add_ci2(sys_system_condition, "slot", sys_context, kw_readers, [sys_system_condition_context]).
sf_sys_add_ci2(sys_timeout, "slot", sys_seconds, kw_initargs, [kw_seconds]).
sf_sys_add_ci2(sys_timeout, "slot", sys_seconds, kw_initform, [quote, []]).
sf_sys_add_ci2(sys_timeout, "slot", sys_seconds, kw_readers, [sys_timeout_seconds]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_arguments, kw_initargs, [kw_format_arguments]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_arguments, kw_initform, [quote, []]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_arguments, kw_readers, [simple_condition_format_arguments]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_initargs, [kw_format_control]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_initform, [quote, []]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_readers, [simple_condition_format_control]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_arguments, kw_initargs, [kw_format_arguments]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_arguments, kw_readers, [simple_condition_format_arguments]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_arguments, kw_writers, [[setf, simple_condition_format_arguments]]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_initargs, [kw_format_control]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_initform, '$ARRAY'([*], claz_base_character, [])).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_readers, [sys_simple_condition_format_string, simple_condition_format_control]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_readers, [simple_condition_format_control]).
sf_sys_add_ci2(simple_condition, "slot", sys_format_control, kw_writers, [[setf, simple_condition_format_control]]).
sf_sys_add_ci2(simple_condition, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\(~), #\(?)]), [simple_condition_format_control, condition], [simple_condition_format_arguments, condition]]]).
sf_sys_add_ci2(stream_error, "slot", stream, kw_initargs, [kw_stream]).
sf_sys_add_ci2(stream_error, "slot", stream, kw_readers, [stream_error_stream]).
sf_sys_add_ci2(stream_error, "slot", stream, kw_initargs, [kw_stream]).
sf_sys_add_ci2(stream_error, "slot", stream, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(stream_error, "slot", stream, kw_readers, [stream_error_stream]).
sf_sys_add_ci2(type_error, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(type_error, "slot", sys_datum, kw_initargs, [kw_datum]).
sf_sys_add_ci2(type_error, "slot", sys_datum, kw_readers, [type_error_datum]).
sf_sys_add_ci2(type_error, "slot", sys_expected_type, kw_initargs, [kw_expected_type]).
sf_sys_add_ci2(type_error, "slot", sys_expected_type, kw_readers, [type_error_expected_type]).
sf_sys_add_ci2(type_error, "slot", sys_datum, kw_initargs, [kw_datum]).
sf_sys_add_ci2(type_error, "slot", sys_datum, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(type_error, "slot", sys_datum, kw_readers, [type_error_datum]).
sf_sys_add_ci2(type_error, "slot", sys_expected_type, kw_initargs, [kw_expected_type]).
sf_sys_add_ci2(type_error, "slot", sys_expected_type, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(type_error, "slot", sys_expected_type, kw_readers, [type_error_expected_type]).
sf_sys_add_ci2(type_error, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\(~), #\('S'), #\(' '), #\(i), #\(s), #\(' '), #\(n), #\(o), #\(t), #\(' '), #\(o), #\(f), #\(' '), #\(t), #\(y), #\(p), #\(e), #\(' '), #\(~), #\('S'), #\('.')]), [type_error_datum, condition], [type_error_expected_type, condition]]]).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_initargs, [kw_instance]).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_readers, [unbound_slot_instance]).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_initargs, [kw_instance]).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_readers, [unbound_slot_instance]).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_initargs, [kw_instance]).
sf_sys_add_ci2(unbound_slot, "slot", sys_instance, kw_readers, [unbound_slot_instance]).
sf_sys_add_ci2(unbound_slot, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\('T'), #\(h), #\(e), #\(' '), #\(s), #\(l), #\(o), #\(t), #\(' '), #\(~), #\('S'), #\(' '), #\(i), #\(n), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(o), #\(b), #\(j), #\(e), #\(c), #\(t), #\(' '), #\(~), #\('S'), #\(' '), #\(i), #\(s), #\(' '), #\(u), #\(n), #\(b), #\(o), #\(u), #\(n), #\(d), #\('.')]), [cell_error_name, condition], [unbound_slot_instance, condition]]]).
sf_sys_add_ci2(unbound_variable, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(unbound_variable, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\('T'), #\(h), #\(e), #\(' '), #\(v), #\(a), #\(r), #\(i), #\(a), #\(b), #\(l), #\(e), #\(' '), #\(~), #\('S'), #\(' '), #\(i), #\(s), #\(' '), #\(u), #\(n), #\(b), #\(o), #\(u), #\(n), #\(d), #\('.')]), [cell_error_name, condition]]]).
sf_sys_add_ci2(undefined_function, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(undefined_function, "slot", sys_report_function, kw_initform, [lambda, [condition, stream], [format, stream, '$ARRAY'([*], claz_base_character, [#\('T'), #\(h), #\(e), #\(' '), #\(f), #\(u), #\(n), #\(c), #\(t), #\(i), #\(o), #\(n), #\(' '), #\(~), #\('S'), #\(' '), #\(i), #\(s), #\(' '), #\(u), #\(n), #\(d), #\(e), #\(f), #\(i), #\(n), #\(e), #\(d), #\('.')]), [cell_error_name, condition]]]).
sf_sys_add_ci2(generic_function, "method", [setf, sys_generic_function_name], kw_name, [setf, sys_generic_function_name]).
sf_sys_add_ci2(generic_function, "method", add_method, kw_name, add_method).
sf_sys_add_ci2(generic_function, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(generic_function, "method", remove_method, kw_name, remove_method).
sf_sys_add_ci2(generic_function, "method", shared_initialize, kw_name, shared_initialize).
sf_sys_add_ci2(generic_function, "method", sys_ensure_generic_function_using_class, kw_name, sys_ensure_generic_function_using_class).
sf_sys_add_ci2(generic_function, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(generic_function, "slot", sys_encapsulations, kw_readers, [sys_generic_function_encapsulations]).
sf_sys_add_ci2(generic_function, "slot", sys_encapsulations, kw_writers, [[setf, sys_generic_function_encapsulations]]).
sf_sys_add_ci2(generic_function, "slot", sys_initial_methods, kw_readers, [sys_generic_function_initial_methods]).
sf_sys_add_ci2(generic_function, "slot", sys_initial_methods, kw_writers, [[setf, sys_generic_function_initial_methods]]).
sf_sys_add_ci2(sys_funcallable_standard_object, "slot", sys_name, kw_readers, [sys_funcallable_name]).
sf_sys_add_ci2(sys_funcallable_standard_object, "slot", sys_name, kw_writers, [[setf, sys_funcallable_name]]).
sf_sys_add_ci2(generic_function, "slot", sys_listeners, kw_readers, [sys_gf_listeners]).
sf_sys_add_ci2(generic_function, "slot", sys_listeners, kw_type, list).
sf_sys_add_ci2(generic_function, "slot", sys_listeners, kw_writers, [[setf, sys_gf_listeners]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_argorder, kw_readers, [sys_std_gf_argorder]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_argorder, kw_type, list).
sf_sys_add_ci2(standard_generic_function, "slot", sys_argorder, kw_writers, [[setf, sys_std_gf_argorder]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declspecs, kw_readers, [sys_std_gf_declspecs]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declspecs, kw_type, list).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declspecs, kw_writers, [[setf, sys_std_gf_declspecs]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_default_method_class, kw_readers, [sys_std_gf_default_method_class]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_default_method_class, kw_type, class).
sf_sys_add_ci2(standard_generic_function, "slot", sys_default_method_class, kw_writers, [[setf, sys_std_gf_default_method_class]]).
sf_sys_add_ci2(standard_generic_function, "slot", documentation, kw_readers, [sys_std_gf_documentation]).
sf_sys_add_ci2(standard_generic_function, "slot", documentation, kw_type, [or, null, string]).
sf_sys_add_ci2(standard_generic_function, "slot", documentation, kw_writers, [[setf, sys_std_gf_documentation]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_effective_method_cache, kw_readers, [sys_std_gf_effective_method_cache]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_effective_method_cache, kw_type, list).
sf_sys_add_ci2(standard_generic_function, "slot", sys_effective_method_cache, kw_writers, [[setf, sys_std_gf_effective_method_cache]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_initialized, kw_readers, [sys_std_gf_initialized]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_initialized, kw_type, boolean).
sf_sys_add_ci2(standard_generic_function, "slot", sys_initialized, kw_writers, [[setf, sys_std_gf_initialized]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_readers, [sys_std_gf_lambda_list]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_type, list).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_writers, [[setf, sys_std_gf_lambda_list]]).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_readers, [sys_std_gf_method_combination]).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_type, method_combination).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_writers, [[setf, sys_std_gf_method_combination]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_readers, [sys_std_gf_methods]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_type, list).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_writers, [[setf, sys_std_gf_methods]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_signature, kw_readers, [sys_std_gf_signature]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_signature, kw_type, [simple_vector, 6]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_signature, kw_writers, [[setf, sys_std_gf_signature]]).
sf_sys_add_ci2(standard_generic_function, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(standard_generic_function, "method", add_method, kw_name, add_method).
sf_sys_add_ci2(standard_generic_function, "method", compute_applicable_methods, kw_name, compute_applicable_methods).
sf_sys_add_ci2(standard_generic_function, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(standard_generic_function, "method", find_method, kw_name, find_method).
sf_sys_add_ci2(standard_generic_function, "method", reinitialize_instance, kw_name, reinitialize_instance).
sf_sys_add_ci2(standard_generic_function, "method", remove_method, kw_name, remove_method).
sf_sys_add_ci2(standard_generic_function, "method", shared_initialize, kw_name, shared_initialize).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_optional_arguments, kw_name, sys_generic_function_optional_arguments).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_required_arguments, kw_name, sys_generic_function_required_arguments).
sf_sys_add_ci2(standard_generic_function, "method", sys_method_more_specific_p, kw_name, sys_method_more_specific_p).
sf_sys_add_ci2(standard_generic_function, "method", sys_add_dependent, kw_name, sys_add_dependent).
sf_sys_add_ci2(standard_generic_function, "method", sys_compute_applicable_methods_using_classes, kw_name, sys_compute_applicable_methods_using_classes).
sf_sys_add_ci2(standard_generic_function, "method", sys_compute_discriminating_function, kw_name, sys_compute_discriminating_function).
sf_sys_add_ci2(standard_generic_function, "method", sys_compute_effective_method, kw_name, sys_compute_effective_method).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_argument_precedence_order, kw_name, sys_generic_function_argument_precedence_order).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_declarations, kw_name, sys_generic_function_declarations).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_lambda_list, kw_name, sys_generic_function_lambda_list).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_method_class, kw_name, sys_generic_function_method_class).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_method_combination, kw_name, sys_generic_function_method_combination).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_methods, kw_name, sys_generic_function_methods).
sf_sys_add_ci2(standard_generic_function, "method", sys_generic_function_name, kw_name, sys_generic_function_name).
sf_sys_add_ci2(standard_generic_function, "method", sys_make_method_lambda, kw_name, sys_make_method_lambda).
sf_sys_add_ci2(standard_generic_function, "method", sys_map_dependents, kw_name, sys_map_dependents).
sf_sys_add_ci2(standard_generic_function, "method", sys_remove_dependent, kw_name, sys_remove_dependent).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_initargs, [kw_method_combination]).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_initform, [sys_find_method_combination, [sys_class_prototype, [find_class, [quote, standard_generic_function]]], [quote, standard], []]).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_readers, [sys_generic_function_method_combination]).
sf_sys_add_ci2(standard_generic_function, "slot", method_combination, kw_writers, [[setf, sys_generic_function_method_combination]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_lock, kw_initform, [sys_make_mutex, kw_name, '$ARRAY'([*], claz_base_character, [#\('G'), #\('F'), #\(' '), #\(l), #\(o), #\(c), #\(k)])]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_lock, kw_readers, [sys_gf_lock]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_method_combination, kw_initargs, [kw_method_combination]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_method_combination, kw_initform, sys_c43_the_standard_method_combination_c43).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_method_combination, kw_readers, [sys_generic_function_method_combination]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_pf_method_combination, kw_writers, [[setf, sys_generic_function_method_combination]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_a_p_o_function, kw_readers, [sys_generic_function_a_p_o_function]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_a_p_o_function, kw_writers, [[setf, sys_generic_function_a_p_o_function]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_arg_info, kw_initform, [sys_make_arg_info]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_arg_info, kw_readers, [sys_gf_arg_info]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_argument_precedence_order, kw_initargs, [kw_argument_precedence_order]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_argument_precedence_order, kw_readers, [sys_generic_function_argument_precedence_order]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_argument_precedence_order, kw_writers, [[setf, sys_generic_function_argument_precedence_order]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declarations, kw_initargs, [kw_declarations]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declarations, kw_initargs, [kw_declare, kw_declarations]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declarations, kw_readers, [sys_generic_function_declarations]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declarations, kw_readers, [sys_generic_function_declarations]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declarations, kw_writers, [[setf, sys_generic_function_declarations]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_declarations, kw_writers, [[setf, sys_generic_function_declarations]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_dependents, kw_readers, [sys_generic_function_dependents]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_dependents, kw_writers, [[setf, sys_generic_function_dependents]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_dfun_state, kw_readers, [sys_gf_dfun_state]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_dfun_state, kw_writers, [[setf, sys_gf_dfun_state]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_info_needs_update, kw_readers, [sys_gf_info_needs_update]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_info_needs_update, kw_writers, [[setf, sys_gf_info_needs_update]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_initial_methods, kw_initargs, [kw_initial_methods]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_readers, [sys_generic_function_lambda_list]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_lambda_list, kw_writers, [[setf, sys_generic_function_lambda_list]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_method_class, kw_initargs, [kw_method_class]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_method_class, kw_initform, [find_class, [quote, standard_method]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_method_class, kw_initform, sys_c43_the_standard_method_class_c43).
sf_sys_add_ci2(standard_generic_function, "slot", sys_method_class, kw_readers, [sys_generic_function_method_class]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_method_class, kw_writers, [[setf, sys_generic_function_method_class]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_initargs, [kw_methods]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_readers, [sys_generic_function_methods]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_readers, [sys_generic_function_methods]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_type, list).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_writers, [[setf, sys_generic_function_methods]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_methods, kw_writers, [[setf, sys_generic_function_methods]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_name, kw_readers, [sys_generic_function_name]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_name, kw_readers, [sys_generic_function_name]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_optional_args, kw_initargs, [kw_optional_args]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_required_args, kw_initargs, [kw_required_args]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_spec_list, kw_readers, [sys_generic_function_spec_list]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_spec_list, kw_writers, [[setf, sys_generic_function_spec_list]]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_generic_function, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(built_in_class, "method", [setf, class_name], kw_name, [setf, class_name]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_layout], kw_name, [setf, sys_class_layout]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_default_initargs], kw_name, [setf, sys_class_default_initargs]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_direct_default_initargs], kw_name, [setf, sys_class_direct_default_initargs]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_direct_methods], kw_name, [setf, sys_class_direct_methods]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_direct_slots], kw_name, [setf, sys_class_direct_slots]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_direct_subclasses], kw_name, [setf, sys_class_direct_subclasses]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_direct_superclasses], kw_name, [setf, sys_class_direct_superclasses]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_finalized_p], kw_name, [setf, sys_class_finalized_p]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_precedence_list], kw_name, [setf, sys_class_precedence_list]).
sf_sys_add_ci2(built_in_class, "method", [setf, sys_class_slots], kw_name, [setf, sys_class_slots]).
sf_sys_add_ci2(built_in_class, "method", allocate_instance, kw_name, allocate_instance).
sf_sys_add_ci2(built_in_class, "method", class_name, kw_name, class_name).
sf_sys_add_ci2(built_in_class, "method", sys_class_layout, kw_name, sys_class_layout).
sf_sys_add_ci2(built_in_class, "method", sys_class_default_initargs, kw_name, sys_class_default_initargs).
sf_sys_add_ci2(built_in_class, "method", sys_class_direct_default_initargs, kw_name, sys_class_direct_default_initargs).
sf_sys_add_ci2(built_in_class, "method", sys_class_direct_methods, kw_name, sys_class_direct_methods).
sf_sys_add_ci2(built_in_class, "method", sys_class_direct_slots, kw_name, sys_class_direct_slots).
sf_sys_add_ci2(built_in_class, "method", sys_class_direct_subclasses, kw_name, sys_class_direct_subclasses).
sf_sys_add_ci2(built_in_class, "method", sys_class_direct_superclasses, kw_name, sys_class_direct_superclasses).
sf_sys_add_ci2(built_in_class, "method", sys_class_finalized_p, kw_name, sys_class_finalized_p).
sf_sys_add_ci2(built_in_class, "method", sys_class_precedence_list, kw_name, sys_class_precedence_list).
sf_sys_add_ci2(built_in_class, "method", sys_class_slots, kw_name, sys_class_slots).
sf_sys_add_ci2(built_in_class, "method", sys_ensure_class_using_class, kw_name, sys_ensure_class_using_class).
sf_sys_add_ci2(built_in_class, "method", sys_validate_superclass, kw_documentation, '$ARRAY'([*], claz_base_character, [#\('T'), #\(h), #\(i), #\(s), #\(' '), #\(g), #\(e), #\(n), #\(e), #\(r), #\(i), #\(c), #\(' '), #\(f), #\(u), #\(n), #\(c), #\(t), #\(i), #\(o), #\(n), #\(' '), #\(i), #\(s), #\(' '), #\(c), #\(a), #\(l), #\(l), #\(e), #\(d), #\(' '), #\(t), #\(o), #\(' '), #\(d), #\(e), #\(t), #\(e), #\(r), #\(m), #\(i), #\(n), #\(e), #\(' '), #\(w), #\(h), #\(e), #\(t), #\(h), #\(e), #\(r), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(' '), #\(s), #\(u), #\(p), #\(e), #\(r), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(i), #\(s), #\(' '), #\(s), #\(u), #\(i), #\(t), #\(a), #\(b), #\(l), #\(e), #\(' '), #\(f), #\(o), #\(r), #\(' '), #\(u), #\(s), #\(e), #\(' '), #\(a), #\(s), #\(' '), #\(a), #\(' '), #\(s), #\(u), #\(p), #\(e), #\(r), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(o), #\(f), #\(' '), #\(c), #\(l), #\(a), #\(s), #\(s), #\('.')])).
sf_sys_add_ci2(built_in_class, "method", sys_validate_superclass, kw_name, sys_validate_superclass).
sf_sys_add_ci2(built_in_class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(built_in_class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(built_in_class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(built_in_class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(built_in_class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(built_in_class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(built_in_class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(built_in_class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(built_in_class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(built_in_class, "slot", sys_forward, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(built_in_class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(built_in_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(built_in_class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(built_in_class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(built_in_class, "slot", sys_optimize_slot_access, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(built_in_class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(built_in_class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(built_in_class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(built_in_class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(built_in_class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(built_in_class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(built_in_class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(built_in_class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(built_in_class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(built_in_class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(built_in_class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(built_in_class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(built_in_class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(built_in_class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(class, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(class, "method", allocate_instance, kw_name, allocate_instance).
sf_sys_add_ci2(class, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(class, "method", make_instance, kw_name, make_instance).
sf_sys_add_ci2(class, "method", make_load_form, kw_name, make_load_form).
sf_sys_add_ci2(class, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(class, "method", shared_initialize, kw_name, shared_initialize).
sf_sys_add_ci2(class, "method", sys_add_direct_method, kw_name, sys_add_direct_method).
sf_sys_add_ci2(class, "method", sys_add_direct_subclass, kw_name, sys_add_direct_subclass).
sf_sys_add_ci2(class, "method", sys_direct_slot_definition_class, kw_name, sys_direct_slot_definition_class).
sf_sys_add_ci2(class, "method", sys_effective_slot_definition_class, kw_name, sys_effective_slot_definition_class).
sf_sys_add_ci2(class, "method", sys_ensure_class_using_class, kw_name, sys_ensure_class_using_class).
sf_sys_add_ci2(class, "method", sys_remove_direct_method, kw_name, sys_remove_direct_method).
sf_sys_add_ci2(class, "method", sys_remove_direct_subclass, kw_name, sys_remove_direct_subclass).
sf_sys_add_ci2(class, "method", sys_specializer_direct_generic_functions, kw_name, sys_specializer_direct_generic_functions).
sf_sys_add_ci2(class, "method", sys_specializer_direct_methods, kw_name, sys_specializer_direct_methods).
sf_sys_add_ci2(class, "method", sys_validate_superclass, kw_documentation, '$ARRAY'([*], claz_base_character, [#\('T'), #\(h), #\(i), #\(s), #\(' '), #\(g), #\(e), #\(n), #\(e), #\(r), #\(i), #\(c), #\(' '), #\(f), #\(u), #\(n), #\(c), #\(t), #\(i), #\(o), #\(n), #\(' '), #\(i), #\(s), #\(' '), #\(c), #\(a), #\(l), #\(l), #\(e), #\(d), #\(' '), #\(t), #\(o), #\(' '), #\(d), #\(e), #\(t), #\(e), #\(r), #\(m), #\(i), #\(n), #\(e), #\(' '), #\(w), #\(h), #\(e), #\(t), #\(h), #\(e), #\(r), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(' '), #\(s), #\(u), #\(p), #\(e), #\(r), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(i), #\(s), #\(' '), #\(s), #\(u), #\(i), #\(t), #\(a), #\(b), #\(l), #\(e), #\(' '), #\(f), #\(o), #\(r), #\(' '), #\(u), #\(s), #\(e), #\(' '), #\(a), #\(s), #\(' '), #\(a), #\(' '), #\(s), #\(u), #\(p), #\(e), #\(r), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(o), #\(f), #\(' '), #\(c), #\(l), #\(a), #\(s), #\(s), #\('.')])).
sf_sys_add_ci2(class, "method", sys_validate_superclass, kw_name, sys_validate_superclass).
sf_sys_add_ci2(class, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(class, "slot", sys_class_eq_specializer, kw_readers, [sys_class_eq_specializer]).
sf_sys_add_ci2(class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(class, "slot", sys_direct_methods, kw_initform, [cons, [], []]).
sf_sys_add_ci2(class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(class, "slot", sys_finalized_p, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(class, "slot", sys_name, kw_readers, [class_name]).
sf_sys_add_ci2(class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(class, "slot", sys_safe_p, kw_initargs, [sys_safe_p]).
sf_sys_add_ci2(class, "slot", sys_safe_p, kw_readers, [sys_safe_p]).
sf_sys_add_ci2(class, "slot", sys_safe_p, kw_writers, [[setf, sys_safe_p]]).
sf_sys_add_ci2(class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(condition, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(condition, "slot", sys_format_arguments, kw_initargs, [kw_format_arguments]).
sf_sys_add_ci2(condition, "slot", sys_format_arguments, kw_readers, [simple_condition_format_arguments]).
sf_sys_add_ci2(condition, "slot", sys_format_control, kw_initargs, [kw_format_control]).
sf_sys_add_ci2(condition, "slot", sys_format_control, kw_readers, [simple_condition_format_control]).
sf_sys_add_ci2(condition, "slot", sys_report_function, kw_allocation, kw_class).
sf_sys_add_ci2(condition, "slot", sys_report_function, kw_location, kw_class).
sf_sys_add_ci2(method, "method", add_method, kw_name, add_method).
sf_sys_add_ci2(method, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(method, "method", remove_method, kw_name, remove_method).
sf_sys_add_ci2(method, "method", sys_add_direct_method, kw_name, sys_add_direct_method).
sf_sys_add_ci2(method, "method", sys_remove_direct_method, kw_name, sys_remove_direct_method).
sf_sys_add_ci2(method_combination, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(method_combination, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(method_combination, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(method_combination, "slot", sys_compiler, kw_initargs, [kw_compiler]).
sf_sys_add_ci2(method_combination, "slot", sys_compiler, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(method_combination, "slot", sys_compiler, kw_readers, [sys_method_combination_compiler]).
sf_sys_add_ci2(method_combination, "slot", sys_compiler, kw_writers, [[setf, sys_method_combination_compiler]]).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_readers, [sys_method_combination_name]).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_writers, [[setf, sys_method_combination_name]]).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_initargs, [kw_options]).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_readers, [sys_method_combination_options]).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_writers, [[setf, sys_method_combination_options]]).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_accessor_flags, kw_initform, 0).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_info, kw_readers, [sys_slot_definition_info]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_info, kw_writers, [[setf, sys_slot_definition_info]]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_direct_methods, kw_initform, [cons, [], []]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_object, kw_initargs, [kw_object]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_object, kw_readers, [sys_eql_specializer_object, sys_specializer_object]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_class, kw_initargs, [kw_class]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_class, kw_readers, [sys_slot_definition_class]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_class, kw_writers, [[setf, sys_slot_definition_class]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_documentation, kw_readers, [sys_pf_slot_definition_documentation]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_type, kw_initform, t).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_pf_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_source, kw_initargs, [sys_source]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_source, kw_readers, [sys_definition_source]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_source, kw_writers, [[setf, sys_definition_source]]).
sf_sys_add_ci2(sys_specializer, "slot", sys_pf_type, kw_readers, [sys_specializer_type]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_pf_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_pf_slot_definition, kw_readers, [sys_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation_class, kw_initargs, [kw_allocation_class]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation_class, kw_readers, [sys_slot_definition_allocation_class]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation_class, kw_writers, [[setf, sys_slot_definition_allocation_class]]).
sf_sys_add_ci2(sys_accessor_method, "slot", sys_slot_name, kw_initargs, [kw_slot_name]).
sf_sys_add_ci2(sys_accessor_method, "slot", sys_slot_name, kw_readers, [sys_accessor_method_slot_name]).
sf_sys_add_ci2(sys_class_eq_specializer, "slot", sys_object, kw_initargs, [kw_class]).
sf_sys_add_ci2(sys_class_eq_specializer, "slot", sys_object, kw_readers, [sys_specializer_object, sys_specializer_class]).
sf_sys_add_ci2(sys_class_prototype_specializer, "slot", sys_object, kw_initargs, [kw_class]).
sf_sys_add_ci2(sys_class_prototype_specializer, "slot", sys_object, kw_readers, [sys_specializer_object, sys_specializer_class]).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation_class, kw_initargs, [kw_allocation_class]).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation_class, kw_readers, [sys_slot_definition_allocation_class]).
sf_sys_add_ci2(sys_condition_slot_definition, "slot", sys_allocation_class, kw_writers, [[setf, sys_slot_definition_allocation_class]]).
sf_sys_add_ci2(sys_definition_source_mixin, "slot", sys_source, kw_initargs, [kw_definition_source]).
sf_sys_add_ci2(sys_definition_source_mixin, "slot", sys_source, kw_readers, [sys_definition_source]).
sf_sys_add_ci2(sys_long_method_combination, "slot", function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_long_method_combination, "slot", function, kw_readers, [sys_long_method_combination_function]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_args_lambda_list, kw_initargs, [kw_args_lambda_list]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_args_lambda_list, kw_readers, [sys_long_method_combination_args_lambda_list]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_arguments, kw_initargs, [kw_arguments]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_declarations, kw_initargs, [kw_declarations]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_forms, kw_initargs, [kw_forms]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_generic_function_symbol, kw_initargs, [kw_generic_function_symbol]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(sys_long_method_combination, "slot", sys_method_group_specs, kw_initargs, [kw_method_group_specs]).
sf_sys_add_ci2(sys_pcl_class, "slot", sys_pf_class_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(sys_pcl_class, "slot", sys_can_precede_list, kw_readers, [sys_class_can_precede_list]).
sf_sys_add_ci2(sys_pcl_class, "slot", sys_cpl_available_p, kw_readers, [sys_cpl_available_p]).
sf_sys_add_ci2(sys_pcl_class, "slot", sys_incompatible_superclass_list, kw_readers, [sys_class_incompatible_superclass_list]).
sf_sys_add_ci2(sys_pcl_class, "slot", sys_incompatible_superclass_list, kw_writers, [[setf, sys_class_incompatible_superclass_list]]).
sf_sys_add_ci2(sys_pcl_class, "slot", sys_wrapper, kw_readers, [sys_class_wrapper]).
sf_sys_add_ci2(sys_plist_mixin, "slot", sys_plist, kw_initargs, [sys_plist]).
sf_sys_add_ci2(sys_plist_mixin, "slot", sys_plist, kw_readers, [sys_object_plist]).
sf_sys_add_ci2(sys_plist_mixin, "slot", sys_plist, kw_writers, [[setf, sys_object_plist]]).
sf_sys_add_ci2(sys_semi_standard_class, "slot", sys_current_version, kw_type, simple_vector).
sf_sys_add_ci2(sys_semi_standard_class, "slot", sys_direct_instance_specializers, kw_type, [or, hash_table, sys_weak_list, null]).
sf_sys_add_ci2(sys_semi_standard_class, "slot", sys_finalized_direct_subclasses, kw_type, [or, hash_table, sys_weak_list, null]).
sf_sys_add_ci2(sys_semi_standard_class, "slot", sys_fixed_slot_locations, kw_initargs, [kw_fixed_slot_locations]).
sf_sys_add_ci2(sys_semi_standard_class, "slot", sys_funcallablep, kw_type, boolean).
sf_sys_add_ci2(sys_semi_standard_class, "slot", sys_instantiated, kw_type, boolean).
sf_sys_add_ci2(sys_short_method_combination, "slot", sys_identity_with_one_argument, kw_initargs, [kw_identity_with_one_argument]).
sf_sys_add_ci2(sys_short_method_combination, "slot", sys_identity_with_one_argument, kw_readers, [sys_short_combination_identity_with_one_argument]).
sf_sys_add_ci2(sys_short_method_combination, "slot", sys_operator, kw_initargs, [kw_operator]).
sf_sys_add_ci2(sys_short_method_combination, "slot", sys_operator, kw_readers, [sys_short_combination_operator]).
sf_sys_add_ci2(sys_slot_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(sys_slot_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(sys_slotted_class, "slot", sys_direct_accessors, kw_type, list).
sf_sys_add_ci2(sys_slotted_class, "slot", sys_instance_size, kw_type, [integer, 1, *]).
sf_sys_add_ci2(sys_slotted_class, "slot", sys_subclass_of_stablehash_p, kw_type, boolean).
sf_sys_add_ci2(sys_slotted_class, "slot", sys_valid_initargs_from_slots, kw_type, list).
sf_sys_add_ci2(sys_standard_method_combination, "slot", sys_options, kw_initargs, [kw_options]).
sf_sys_add_ci2(sys_standard_method_combination, "slot", sys_options, kw_readers, [sys_method_combination_options]).
sf_sys_add_ci2(sys_standard_method_combination, "slot", sys_type_name, kw_initargs, [kw_type_name]).
sf_sys_add_ci2(sys_standard_method_combination, "slot", sys_type_name, kw_readers, [sys_method_combination_type_name]).
sf_sys_add_ci2(sys_std_class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(sys_std_class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(sys_std_class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(sys_std_class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(sys_std_class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(sys_std_class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_forward, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(sys_std_class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_std_class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(sys_std_class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_optimize_slot_access, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(sys_std_class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(sys_std_class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(sys_std_class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(sys_std_class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(sys_std_class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(sys_std_class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(sys_std_class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_std_class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_std_class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(sys_std_class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(sys_structure_effective_slot_definition, "slot", sys_efm_sbuc, kw_type, function).
sf_sys_add_ci2(sys_structure_effective_slot_definition, "slot", sys_efm_smuc, kw_type, function).
sf_sys_add_ci2(sys_structure_effective_slot_definition, "slot", sys_efm_ssvuc, kw_type, function).
sf_sys_add_ci2(sys_structure_effective_slot_definition, "slot", sys_efm_svuc, kw_type, function).
sf_sys_add_ci2(sys_structure_effective_slot_definition, "slot", sys_readonly, kw_initargs, [sys_readonly]).
sf_sys_add_ci2(sys_structure_effective_slot_definition, "slot", sys_readonly, kw_type, boolean).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_defstruct_accessor_symbol, kw_initargs, [kw_defstruct_accessor_symbol]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_defstruct_accessor_symbol, kw_readers, [sys_slot_definition_defstruct_accessor_symbol]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_defstruct_accessor_symbol, kw_writers, [[setf, sys_slot_definition_defstruct_accessor_symbol]]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_internal_reader_function, kw_initargs, [kw_internal_reader_function]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_internal_reader_function, kw_readers, [sys_slot_definition_internal_reader_function]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_internal_reader_function, kw_writers, [[setf, sys_slot_definition_internal_reader_function]]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_internal_writer_function, kw_initargs, [kw_internal_writer_function]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_internal_writer_function, kw_readers, [sys_slot_definition_internal_writer_function]).
sf_sys_add_ci2(sys_structure_slot_definition, "slot", sys_internal_writer_function, kw_writers, [[setf, sys_slot_definition_internal_writer_function]]).
sf_sys_add_ci2(sys_super_class, "slot", sys_classname, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_super_class, "slot", sys_classname, kw_type, symbol).
sf_sys_add_ci2(sys_super_class, "slot", sys_direct_subclasses, kw_type, [or, hash_table, sys_weak_list, null]).
sf_sys_add_ci2(class, "slot", sys_all_superclasses, kw_type, hash_table).
sf_sys_add_ci2(class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(class, "slot", sys_direct_default_initargs, kw_type, list).
sf_sys_add_ci2(class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(class, "slot", sys_direct_slots, kw_type, list).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(class, "slot", sys_direct_superclasses, kw_type, list).
sf_sys_add_ci2(class, "slot", documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(class, "slot", documentation, kw_type, [or, string, null]).
sf_sys_add_ci2(class, "slot", sys_initialized, kw_type, [integer, 0, 6]).
sf_sys_add_ci2(class, "slot", sys_listeners, kw_type, list).
sf_sys_add_ci2(class, "slot", sys_precedence_list, kw_type, list).
sf_sys_add_ci2(class, "slot", sys_slot_location_table, kw_type, hash_table).
sf_sys_add_ci2(class, "slot", sys_slots, kw_type, list).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_declared_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_declared_type, kw_initform, t).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_declared_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_declared_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initform, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_type, list).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_type, list).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_docstring, kw_readers, [sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_direct_slot_definition, "slot", sys_docstring, kw_writers, [[setf, sys_slot_definition_documentation]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_declared_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_declared_type, kw_initform, t).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_declared_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_declared_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_efm_sbuc, kw_type, function).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_efm_smuc, kw_type, function).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_efm_ssvuc, kw_type, function).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_efm_svuc, kw_type, function).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initform, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_location, kw_initargs, [sys_location]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_location, kw_type, [or, null, integer, cons]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_docstring, kw_readers, [sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_effective_slot_definition, "slot", sys_docstring, kw_writers, [[setf, sys_slot_definition_documentation]]).
sf_sys_add_ci2(sys_eql_specializer, "method", sys_add_direct_method, kw_name, sys_add_direct_method).
sf_sys_add_ci2(sys_eql_specializer, "method", sys_remove_direct_method, kw_name, sys_remove_direct_method).
sf_sys_add_ci2(sys_eql_specializer, "method", sys_specializer_direct_generic_functions, kw_name, sys_specializer_direct_generic_functions).
sf_sys_add_ci2(sys_eql_specializer, "method", sys_specializer_direct_methods, kw_name, sys_specializer_direct_methods).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_flag, kw_initform, t).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_object, kw_initargs, [kw_object]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_object, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_object, kw_readers, [sys_eql_specializer_object]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_object, kw_writers, [[setf, sys_eql_specializer_object]]).
sf_sys_add_ci2(sys_eql_specializer, "slot", sys_singleton, kw_initargs, [sys_singleton]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, class_name], kw_name, [setf, class_name]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_layout], kw_name, [setf, sys_class_layout]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_default_initargs], kw_name, [setf, sys_class_default_initargs]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_direct_default_initargs], kw_name, [setf, sys_class_direct_default_initargs]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_direct_methods], kw_name, [setf, sys_class_direct_methods]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_direct_slots], kw_name, [setf, sys_class_direct_slots]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_direct_subclasses], kw_name, [setf, sys_class_direct_subclasses]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_direct_superclasses], kw_name, [setf, sys_class_direct_superclasses]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_finalized_p], kw_name, [setf, sys_class_finalized_p]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_precedence_list], kw_name, [setf, sys_class_precedence_list]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", [setf, sys_class_slots], kw_name, [setf, sys_class_slots]).
sf_sys_add_ci2(sys_forward_referenced_class, "method", class_name, kw_name, class_name).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_layout, kw_name, sys_class_layout).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_default_initargs, kw_name, sys_class_default_initargs).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_direct_default_initargs, kw_name, sys_class_direct_default_initargs).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_direct_methods, kw_name, sys_class_direct_methods).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_direct_slots, kw_name, sys_class_direct_slots).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_direct_subclasses, kw_name, sys_class_direct_subclasses).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_direct_superclasses, kw_name, sys_class_direct_superclasses).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_finalized_p, kw_name, sys_class_finalized_p).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_precedence_list, kw_name, sys_class_precedence_list).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_class_slots, kw_name, sys_class_slots).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_ensure_class_using_class, kw_name, sys_ensure_class_using_class).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_validate_superclass, kw_documentation, '$ARRAY'([*], claz_base_character, [#\('T'), #\(h), #\(i), #\(s), #\(' '), #\(g), #\(e), #\(n), #\(e), #\(r), #\(i), #\(c), #\(' '), #\(f), #\(u), #\(n), #\(c), #\(t), #\(i), #\(o), #\(n), #\(' '), #\(i), #\(s), #\(' '), #\(c), #\(a), #\(l), #\(l), #\(e), #\(d), #\(' '), #\(t), #\(o), #\(' '), #\(d), #\(e), #\(t), #\(e), #\(r), #\(m), #\(i), #\(n), #\(e), #\(' '), #\(w), #\(h), #\(e), #\(t), #\(h), #\(e), #\(r), #\(' '), #\(t), #\(h), #\(e), #\(' '), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(' '), #\(s), #\(u), #\(p), #\(e), #\(r), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(i), #\(s), #\(' '), #\(s), #\(u), #\(i), #\(t), #\(a), #\(b), #\(l), #\(e), #\(' '), #\(f), #\(o), #\(r), #\(' '), #\(u), #\(s), #\(e), #\(' '), #\(a), #\(s), #\(' '), #\(a), #\(' '), #\(s), #\(u), #\(p), #\(e), #\(r), #\(c), #\(l), #\(a), #\(s), #\(s), #\(' '), #\(o), #\(f), #\(' '), #\(c), #\(l), #\(a), #\(s), #\(s), #\('.')])).
sf_sys_add_ci2(sys_forward_referenced_class, "method", sys_validate_superclass, kw_name, sys_validate_superclass).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(sys_forward_referenced_class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, class_name], kw_name, [setf, class_name]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_layout], kw_name, [setf, sys_class_layout]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_default_initargs], kw_name, [setf, sys_class_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_direct_default_initargs], kw_name, [setf, sys_class_direct_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_direct_methods], kw_name, [setf, sys_class_direct_methods]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_direct_slots], kw_name, [setf, sys_class_direct_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_direct_subclasses], kw_name, [setf, sys_class_direct_subclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_direct_superclasses], kw_name, [setf, sys_class_direct_superclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_finalized_p], kw_name, [setf, sys_class_finalized_p]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_precedence_list], kw_name, [setf, sys_class_precedence_list]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_class_slots], kw_name, [setf, sys_class_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", [setf, sys_slot_value_using_class], kw_name, [setf, sys_slot_value_using_class]).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", allocate_instance, kw_name, allocate_instance).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", class_name, kw_name, class_name).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", initialize_instance, kw_name, initialize_instance).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", make_instance, kw_name, make_instance).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", make_instances_obsolete, kw_name, make_instances_obsolete).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", reinitialize_instance, kw_name, reinitialize_instance).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_layout, kw_name, sys_class_layout).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_slot_exists_p_using_class, kw_name, sys_slot_exists_p_using_class).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_add_dependent, kw_name, sys_add_dependent).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_default_initargs, kw_name, sys_class_default_initargs).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_direct_default_initargs, kw_name, sys_class_direct_default_initargs).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_direct_methods, kw_name, sys_class_direct_methods).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_direct_slots, kw_name, sys_class_direct_slots).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_direct_subclasses, kw_name, sys_class_direct_subclasses).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_direct_superclasses, kw_name, sys_class_direct_superclasses).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_finalized_p, kw_name, sys_class_finalized_p).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_precedence_list, kw_name, sys_class_precedence_list).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_prototype, kw_name, sys_class_prototype).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_class_slots, kw_name, sys_class_slots).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_compute_class_precedence_list, kw_name, sys_compute_class_precedence_list).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_compute_default_initargs, kw_name, sys_compute_default_initargs).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_compute_effective_slot_definition, kw_name, sys_compute_effective_slot_definition).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_compute_slots, kw_name, sys_compute_slots).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_finalize_inheritance, kw_name, sys_finalize_inheritance).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_map_dependents, kw_name, sys_map_dependents).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_reader_method_class, kw_name, sys_reader_method_class).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_remove_dependent, kw_name, sys_remove_dependent).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_slot_boundp_using_class, kw_name, sys_slot_boundp_using_class).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_slot_makunbound_using_class, kw_name, sys_slot_makunbound_using_class).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_slot_value_using_class, kw_name, sys_slot_value_using_class).
sf_sys_add_ci2(sys_funcallable_standard_class, "method", sys_writer_method_class, kw_name, sys_writer_method_class).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_pf_documentation, kw_readers, [sys_class_documentation]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_forward, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_optimize_slot_access, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_default_initargs, kw_initargs, [kw_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_methods, kw_initargs, [kw_direct_methods]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_methods, kw_readers, [sys_class_direct_methods]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_subclasses, kw_initargs, [kw_direct_subclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_finalized_p, kw_initargs, [kw_finalized_p]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_finalized_p, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_layout, kw_initargs, [kw_layout]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_layout, kw_readers, [sys_class_layout]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_name, kw_readers, [class_name]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_precedence_list, kw_initargs, [kw_precedence_list]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slots, kw_initargs, [kw_slots]).
sf_sys_add_ci2(sys_funcallable_standard_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(method, "slot", sys_from_defgeneric, kw_readers, [sys_method_from_defgeneric]).
sf_sys_add_ci2(method, "slot", sys_from_defgeneric, kw_type, boolean).
sf_sys_add_ci2(method, "slot", sys_from_defgeneric, kw_writers, [[setf, sys_method_from_defgeneric]]).
sf_sys_add_ci2(method_combination, "slot", sys_arguments_lambda_list, kw_readers, [sys_method_combination_arguments_lambda_list]).
sf_sys_add_ci2(method_combination, "slot", sys_arguments_lambda_list, kw_type, list).
sf_sys_add_ci2(method_combination, "slot", sys_arguments_lambda_list, kw_writers, [[setf, sys_method_combination_arguments_lambda_list]]).
sf_sys_add_ci2(method_combination, "slot", sys_call_next_method_allowed, kw_readers, [sys_method_combination_call_next_method_allowed]).
sf_sys_add_ci2(method_combination, "slot", sys_call_next_method_allowed, kw_type, function).
sf_sys_add_ci2(method_combination, "slot", sys_call_next_method_allowed, kw_writers, [[setf, sys_method_combination_call_next_method_allowed]]).
sf_sys_add_ci2(method_combination, "slot", sys_check_method_qualifiers, kw_readers, [sys_method_combination_check_method_qualifiers]).
sf_sys_add_ci2(method_combination, "slot", sys_check_method_qualifiers, kw_type, function).
sf_sys_add_ci2(method_combination, "slot", sys_check_method_qualifiers, kw_writers, [[setf, sys_method_combination_check_method_qualifiers]]).
sf_sys_add_ci2(method_combination, "slot", sys_check_options, kw_readers, [sys_method_combination_check_options]).
sf_sys_add_ci2(method_combination, "slot", sys_check_options, kw_type, function).
sf_sys_add_ci2(method_combination, "slot", sys_check_options, kw_writers, [[setf, sys_method_combination_check_options]]).
sf_sys_add_ci2(method_combination, "slot", sys_declarations, kw_readers, [sys_method_combination_declarations]).
sf_sys_add_ci2(method_combination, "slot", sys_declarations, kw_type, list).
sf_sys_add_ci2(method_combination, "slot", sys_declarations, kw_writers, [[setf, sys_method_combination_declarations]]).
sf_sys_add_ci2(method_combination, "slot", sys_expander, kw_readers, [sys_method_combination_expander]).
sf_sys_add_ci2(method_combination, "slot", sys_expander, kw_type, function).
sf_sys_add_ci2(method_combination, "slot", sys_expander, kw_writers, [[setf, sys_method_combination_expander]]).
sf_sys_add_ci2(method_combination, "slot", sys_identity_with_one_argument, kw_readers, [sys_method_combination_identity_with_one_argument]).
sf_sys_add_ci2(method_combination, "slot", sys_identity_with_one_argument, kw_type, boolean).
sf_sys_add_ci2(method_combination, "slot", sys_identity_with_one_argument, kw_writers, [[setf, sys_method_combination_identity_with_one_argument]]).
sf_sys_add_ci2(method_combination, "slot", sys_long_expander, kw_readers, [sys_method_combination_long_expander]).
sf_sys_add_ci2(method_combination, "slot", sys_long_expander, kw_type, function).
sf_sys_add_ci2(method_combination, "slot", sys_long_expander, kw_writers, [[setf, sys_method_combination_long_expander]]).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_readers, [sys_method_combination_name]).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_type, symbol).
sf_sys_add_ci2(method_combination, "slot", sys_name, kw_writers, [[setf, sys_method_combination_name]]).
sf_sys_add_ci2(method_combination, "slot", sys_operator, kw_readers, [sys_method_combination_operator]).
sf_sys_add_ci2(method_combination, "slot", sys_operator, kw_type, symbol).
sf_sys_add_ci2(method_combination, "slot", sys_operator, kw_writers, [[setf, sys_method_combination_operator]]).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_readers, [sys_method_combination_options]).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_type, list).
sf_sys_add_ci2(method_combination, "slot", sys_options, kw_writers, [[setf, sys_method_combination_options]]).
sf_sys_add_ci2(method_combination, "slot", sys_qualifiers, kw_readers, [sys_method_combination_qualifiers]).
sf_sys_add_ci2(method_combination, "slot", sys_qualifiers, kw_type, list).
sf_sys_add_ci2(method_combination, "slot", sys_qualifiers, kw_writers, [[setf, sys_method_combination_qualifiers]]).
sf_sys_add_ci2(method_combination, "slot", documentation, kw_readers, [sys_method_combination_documentation]).
sf_sys_add_ci2(method_combination, "slot", documentation, kw_type, [or, null, string]).
sf_sys_add_ci2(method_combination, "slot", documentation, kw_writers, [[setf, sys_method_combination_documentation]]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_allocation_class], kw_name, [setf, sys_slot_definition_allocation_class]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_allocation], kw_name, [setf, sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_documentation], kw_name, [setf, sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_initargs], kw_name, [setf, sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_initform], kw_name, [setf, sys_slot_definition_initform]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_initfunction], kw_name, [setf, sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_location], kw_name, [setf, sys_slot_definition_location]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_name], kw_name, [setf, sys_slot_definition_name]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_readers], kw_name, [setf, sys_slot_definition_readers]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_type], kw_name, [setf, sys_slot_definition_type]).
sf_sys_add_ci2(sys_slot_definition, "method", [setf, sys_slot_definition_writers], kw_name, [setf, sys_slot_definition_writers]).
sf_sys_add_ci2(sys_slot_definition, "method", shared_initialize, kw_name, shared_initialize).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_allocation_class, kw_name, sys_slot_definition_allocation_class).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_allocation, kw_name, sys_slot_definition_allocation).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_documentation, kw_name, sys_slot_definition_documentation).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_initargs, kw_name, sys_slot_definition_initargs).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_initform, kw_name, sys_slot_definition_initform).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_initfunction, kw_name, sys_slot_definition_initfunction).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_location, kw_name, sys_slot_definition_location).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_name, kw_name, sys_slot_definition_name).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_readers, kw_name, sys_slot_definition_readers).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_type, kw_name, sys_slot_definition_type).
sf_sys_add_ci2(sys_slot_definition, "method", sys_slot_definition_writers, kw_name, sys_slot_definition_writers).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_allocation, kw_type, symbol).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_declared_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_declared_type, kw_initform, t).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_declared_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_declared_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_inheritable_doc, kw_initargs, [sys_inheritable_doc]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_inheritable_doc, kw_type, cons).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_inheritable_initer, kw_initargs, [sys_inheritable_initer]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_inheritable_initer, kw_type, cons).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_type, list).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_type, symbol).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_slot_definition, "slot", type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_docstring, kw_readers, [sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_slot_definition, "slot", sys_docstring, kw_writers, [[setf, sys_slot_definition_documentation]]).
sf_sys_add_ci2(sys_specializer, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(sys_specializer, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(sys_specializer, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(sys_specializer, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(sys_specializer, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(sys_specializer, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(sys_standard_accessor_method, "method", sys_accessor_method_slot_definition, kw_name, sys_accessor_method_slot_definition).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_pf_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_keywords, kw_readers, [sys_method_keywords]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_keywords, kw_writers, [[setf, sys_method_keywords]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_method_lambda_list]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_plist, kw_initargs, [kw_plist]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_plist, kw_readers, [sys_method_plist]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_plist, kw_writers, [[setf, sys_method_plist]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_qualifiers, kw_initargs, [kw_qualifiers]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_qualifiers, kw_readers, [method_qualifiers]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_qualifiers, kw_writers, [[setf, method_qualifiers]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_slot_definition, kw_readers, [sys_pf_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_slot_definition, kw_type, sys_direct_slot_definition).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_slot_definition, kw_writers, [[setf, sys_pf_accessor_method_slot_definition]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_specializers, kw_initargs, [kw_specializers]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_specializers, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_specializers, kw_writers, [[setf, sys_method_specializers]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_function, kw_writers, [[setf, sys_method_function]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_the_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_slot_definition, kw_readers, [sys_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_accessor_method, "slot", sys_slot_definition, kw_writers, [[setf, sys_accessor_method_slot_definition]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "method", sys_reader_method_class, kw_name, sys_reader_method_class).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "method", sys_writer_method_class, kw_name, sys_writer_method_class).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_pf_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_allocation_class, kw_initargs, [kw_allocation_class]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_declared_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_declared_type, kw_initform, t).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_declared_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_declared_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initform, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_docstring, kw_readers, [sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_docstring, kw_writers, [[setf, sys_slot_definition_documentation]]).
sf_sys_add_ci2(sys_standard_direct_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "method", [setf, sys_slot_value_using_class], kw_name, [setf, sys_slot_value_using_class]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "method", sys_slot_boundp_using_class, kw_name, sys_slot_boundp_using_class).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "method", sys_slot_makunbound_using_class, kw_name, sys_slot_makunbound_using_class).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "method", sys_slot_value_using_class, kw_name, sys_slot_value_using_class).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_pf_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_allocation_class, kw_initargs, [kw_allocation_class]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_declared_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_declared_type, kw_initform, t).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_declared_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_declared_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initform, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_docstring, kw_readers, [sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_docstring, kw_writers, [[setf, sys_slot_definition_documentation]]).
sf_sys_add_ci2(sys_standard_effective_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(standard_method, "slot", documentation, kw_readers, [sys_std_method_documentation]).
sf_sys_add_ci2(standard_method, "slot", documentation, kw_type, [or, string, null]).
sf_sys_add_ci2(standard_method, "slot", documentation, kw_writers, [[setf, sys_std_method_documentation]]).
sf_sys_add_ci2(standard_method, "slot", sys_fast_function, kw_readers, [sys_std_method_fast_function]).
sf_sys_add_ci2(standard_method, "slot", sys_fast_function, kw_type, [or, null, function]).
sf_sys_add_ci2(standard_method, "slot", sys_fast_function, kw_writers, [[setf, sys_std_method_fast_function]]).
sf_sys_add_ci2(standard_method, "slot", function, kw_readers, [sys_std_method_function]).
sf_sys_add_ci2(standard_method, "slot", function, kw_type, [or, null, function]).
sf_sys_add_ci2(standard_method, "slot", function, kw_writers, [[setf, sys_std_method_function]]).
sf_sys_add_ci2(standard_method, "slot", sys_gf, kw_readers, [sys_std_method_generic_function]).
sf_sys_add_ci2(standard_method, "slot", sys_gf, kw_type, [or, null, generic_function]).
sf_sys_add_ci2(standard_method, "slot", sys_gf, kw_writers, [[setf, sys_std_method_generic_function]]).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_readers, [sys_std_method_lambda_list]).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_type, list).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_std_method_lambda_list]]).
sf_sys_add_ci2(standard_method, "slot", sys_qualifiers, kw_readers, [sys_std_method_qualifiers]).
sf_sys_add_ci2(standard_method, "slot", sys_qualifiers, kw_type, list).
sf_sys_add_ci2(standard_method, "slot", sys_qualifiers, kw_writers, [[setf, sys_std_method_qualifiers]]).
sf_sys_add_ci2(standard_method, "slot", sys_signature, kw_readers, [sys_std_method_signature]).
sf_sys_add_ci2(standard_method, "slot", sys_signature, kw_type, [simple_vector, 6]).
sf_sys_add_ci2(standard_method, "slot", sys_signature, kw_writers, [[setf, sys_std_method_signature]]).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_readers, [sys_std_method_specializers]).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_type, list).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_writers, [[setf, sys_std_method_specializers]]).
sf_sys_add_ci2(standard_method, "slot", sys_wants_next_method_p, kw_readers, [sys_std_method_wants_next_method_p]).
sf_sys_add_ci2(standard_method, "slot", sys_wants_next_method_p, kw_type, boolean).
sf_sys_add_ci2(standard_method, "slot", sys_wants_next_method_p, kw_writers, [[setf, sys_std_method_wants_next_method_p]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_keywords, kw_readers, [sys_method_keywords]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_keywords, kw_writers, [[setf, sys_method_keywords]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_method_lambda_list]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_plist, kw_initargs, [kw_plist]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_plist, kw_readers, [sys_method_plist]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_plist, kw_writers, [[setf, sys_method_plist]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_qualifiers, kw_initargs, [kw_qualifiers]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_qualifiers, kw_readers, [method_qualifiers]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_qualifiers, kw_writers, [[setf, method_qualifiers]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_specializers, kw_initargs, [kw_specializers]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_specializers, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_specializers, kw_writers, [[setf, sys_method_specializers]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_function, kw_writers, [[setf, sys_method_function]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_the_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_slot_definition, kw_readers, [sys_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_optimized_reader_method, "slot", sys_slot_definition, kw_writers, [[setf, sys_accessor_method_slot_definition]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_keywords, kw_readers, [sys_method_keywords]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_keywords, kw_writers, [[setf, sys_method_keywords]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_method_lambda_list]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_plist, kw_initargs, [kw_plist]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_plist, kw_readers, [sys_method_plist]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_plist, kw_writers, [[setf, sys_method_plist]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_qualifiers, kw_initargs, [kw_qualifiers]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_qualifiers, kw_readers, [method_qualifiers]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_qualifiers, kw_writers, [[setf, method_qualifiers]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_specializers, kw_initargs, [kw_specializers]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_specializers, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_specializers, kw_writers, [[setf, sys_method_specializers]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_function, kw_writers, [[setf, sys_method_function]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_the_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_slot_definition, kw_readers, [sys_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_optimized_writer_method, "slot", sys_slot_definition, kw_writers, [[setf, sys_accessor_method_slot_definition]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_keywords, kw_readers, [sys_method_keywords]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_keywords, kw_writers, [[setf, sys_method_keywords]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_method_lambda_list]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_plist, kw_initargs, [kw_plist]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_plist, kw_readers, [sys_method_plist]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_plist, kw_writers, [[setf, sys_method_plist]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_qualifiers, kw_initargs, [kw_qualifiers]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_qualifiers, kw_readers, [method_qualifiers]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_qualifiers, kw_writers, [[setf, method_qualifiers]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_specializers, kw_initargs, [kw_specializers]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_specializers, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_specializers, kw_writers, [[setf, sys_method_specializers]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_function, kw_writers, [[setf, sys_method_function]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_the_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_slot_definition, kw_readers, [sys_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_reader_method, "slot", sys_slot_definition, kw_writers, [[setf, sys_accessor_method_slot_definition]]).
sf_sys_add_ci2(sys_standard_slot_definition, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(sys_standard_slot_definition, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_location, kw_readers, [sys_slot_definition_location]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_location, kw_writers, [[setf, sys_slot_definition_location]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_pf_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_initargs, [kw_allocation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_initform, kw_instance).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_readers, [sys_slot_definition_allocation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation, kw_writers, [[setf, sys_slot_definition_allocation]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_allocation_class, kw_initargs, [kw_allocation_class]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_declared_type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_declared_type, kw_initform, t).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_declared_type, kw_readers, [sys_slot_definition_type]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_declared_type, kw_writers, [[setf, sys_slot_definition_type]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initargs, kw_initargs, [kw_initargs]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initargs, kw_readers, [sys_slot_definition_initargs]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initargs, kw_writers, [[setf, sys_slot_definition_initargs]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initform, kw_initargs, [kw_initform]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initform, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initform, kw_readers, [sys_slot_definition_initform]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initform, kw_writers, [[setf, sys_slot_definition_initform]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initfunction, kw_initargs, [kw_initfunction]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initfunction, kw_readers, [sys_slot_definition_initfunction]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_initfunction, kw_writers, [[setf, sys_slot_definition_initfunction]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_location, kw_initargs, [kw_location]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_name, kw_readers, [sys_slot_definition_name]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_name, kw_writers, [[setf, sys_slot_definition_name]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_readers, kw_initargs, [kw_readers]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_readers, kw_readers, [sys_slot_definition_readers]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_readers, kw_writers, [[setf, sys_slot_definition_readers]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_writers, kw_initargs, [kw_writers]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_writers, kw_readers, [sys_slot_definition_writers]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_writers, kw_writers, [[setf, sys_slot_definition_writers]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_docstring, kw_readers, [sys_slot_definition_documentation]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_docstring, kw_writers, [[setf, sys_slot_definition_documentation]]).
sf_sys_add_ci2(sys_standard_slot_definition, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_keywords, kw_readers, [sys_method_keywords]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_keywords, kw_writers, [[setf, sys_method_keywords]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_method_lambda_list]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_plist, kw_initargs, [kw_plist]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_plist, kw_readers, [sys_method_plist]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_plist, kw_writers, [[setf, sys_method_plist]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_qualifiers, kw_initargs, [kw_qualifiers]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_qualifiers, kw_readers, [method_qualifiers]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_qualifiers, kw_writers, [[setf, method_qualifiers]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_specializers, kw_initargs, [kw_specializers]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_specializers, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_specializers, kw_writers, [[setf, sys_method_specializers]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_function, kw_writers, [[setf, sys_method_function]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_the_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_slot_definition, kw_initargs, [kw_slot_definition]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_slot_definition, kw_readers, [sys_accessor_method_slot_definition]).
sf_sys_add_ci2(sys_standard_writer_method, "slot", sys_slot_definition, kw_writers, [[setf, sys_accessor_method_slot_definition]]).
sf_sys_add_ci2(structure_class, "slot", sys_boa_constructors, kw_type, list).
sf_sys_add_ci2(structure_class, "slot", sys_copier, kw_type, symbol).
sf_sys_add_ci2(structure_class, "slot", sys_kconstructor, kw_type, symbol).
sf_sys_add_ci2(structure_class, "slot", sys_names, kw_type, cons).
sf_sys_add_ci2(structure_class, "slot", sys_predicate, kw_type, symbol).
sf_sys_add_ci2(standard_class, "method", [setf, class_name], kw_name, [setf, class_name]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_layout], kw_name, [setf, sys_class_layout]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_default_initargs], kw_name, [setf, sys_class_default_initargs]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_direct_default_initargs], kw_name, [setf, sys_class_direct_default_initargs]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_direct_methods], kw_name, [setf, sys_class_direct_methods]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_direct_slots], kw_name, [setf, sys_class_direct_slots]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_direct_subclasses], kw_name, [setf, sys_class_direct_subclasses]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_direct_superclasses], kw_name, [setf, sys_class_direct_superclasses]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_finalized_p], kw_name, [setf, sys_class_finalized_p]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_precedence_list], kw_name, [setf, sys_class_precedence_list]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_class_slots], kw_name, [setf, sys_class_slots]).
sf_sys_add_ci2(standard_class, "method", [setf, sys_slot_value_using_class], kw_name, [setf, sys_slot_value_using_class]).
sf_sys_add_ci2(standard_class, "method", allocate_instance, kw_name, allocate_instance).
sf_sys_add_ci2(standard_class, "method", change_class, kw_name, change_class).
sf_sys_add_ci2(standard_class, "method", class_name, kw_name, class_name).
sf_sys_add_ci2(standard_class, "method", initialize_instance, kw_name, initialize_instance).
sf_sys_add_ci2(standard_class, "method", make_instance, kw_name, make_instance).
sf_sys_add_ci2(standard_class, "method", make_instances_obsolete, kw_name, make_instances_obsolete).
sf_sys_add_ci2(standard_class, "method", reinitialize_instance, kw_name, reinitialize_instance).
sf_sys_add_ci2(standard_class, "method", sys_class_layout, kw_name, sys_class_layout).
sf_sys_add_ci2(standard_class, "method", sys_slot_exists_p_using_class, kw_name, sys_slot_exists_p_using_class).
sf_sys_add_ci2(standard_class, "method", sys_add_dependent, kw_name, sys_add_dependent).
sf_sys_add_ci2(standard_class, "method", sys_class_default_initargs, kw_name, sys_class_default_initargs).
sf_sys_add_ci2(standard_class, "method", sys_class_direct_default_initargs, kw_name, sys_class_direct_default_initargs).
sf_sys_add_ci2(standard_class, "method", sys_class_direct_methods, kw_name, sys_class_direct_methods).
sf_sys_add_ci2(standard_class, "method", sys_class_direct_slots, kw_name, sys_class_direct_slots).
sf_sys_add_ci2(standard_class, "method", sys_class_direct_subclasses, kw_name, sys_class_direct_subclasses).
sf_sys_add_ci2(standard_class, "method", sys_class_direct_superclasses, kw_name, sys_class_direct_superclasses).
sf_sys_add_ci2(standard_class, "method", sys_class_finalized_p, kw_name, sys_class_finalized_p).
sf_sys_add_ci2(standard_class, "method", sys_class_precedence_list, kw_name, sys_class_precedence_list).
sf_sys_add_ci2(standard_class, "method", sys_class_prototype, kw_name, sys_class_prototype).
sf_sys_add_ci2(standard_class, "method", sys_class_slots, kw_name, sys_class_slots).
sf_sys_add_ci2(standard_class, "method", sys_compute_class_precedence_list, kw_name, sys_compute_class_precedence_list).
sf_sys_add_ci2(standard_class, "method", sys_compute_default_initargs, kw_name, sys_compute_default_initargs).
sf_sys_add_ci2(standard_class, "method", sys_compute_effective_slot_definition, kw_name, sys_compute_effective_slot_definition).
sf_sys_add_ci2(standard_class, "method", sys_compute_slots, kw_name, sys_compute_slots).
sf_sys_add_ci2(standard_class, "method", sys_finalize_inheritance, kw_name, sys_finalize_inheritance).
sf_sys_add_ci2(standard_class, "method", sys_map_dependents, kw_name, sys_map_dependents).
sf_sys_add_ci2(standard_class, "method", sys_reader_method_class, kw_name, sys_reader_method_class).
sf_sys_add_ci2(standard_class, "method", sys_remove_dependent, kw_name, sys_remove_dependent).
sf_sys_add_ci2(standard_class, "method", sys_slot_boundp_using_class, kw_name, sys_slot_boundp_using_class).
sf_sys_add_ci2(standard_class, "method", sys_slot_makunbound_using_class, kw_name, sys_slot_makunbound_using_class).
sf_sys_add_ci2(standard_class, "method", sys_slot_value_using_class, kw_name, sys_slot_value_using_class).
sf_sys_add_ci2(standard_class, "method", sys_writer_method_class, kw_name, sys_writer_method_class).
sf_sys_add_ci2(standard_class, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_class, "slot", sys_pf_documentation, kw_readers, [sys_class_documentation]).
sf_sys_add_ci2(standard_class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(standard_class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(standard_class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(standard_class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(standard_class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(standard_class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(standard_class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(standard_class, "slot", sys_forward, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(standard_class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(standard_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(standard_class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(standard_class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(standard_class, "slot", sys_optimize_slot_access, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(standard_class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(standard_class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(standard_class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(standard_class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(standard_class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(standard_class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(standard_class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(standard_class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(standard_class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(standard_class, "slot", sys_default_initargs, kw_initargs, [kw_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_methods, kw_initargs, [kw_direct_methods]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_methods, kw_readers, [sys_class_direct_methods]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_subclasses, kw_initargs, [kw_direct_subclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(standard_class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_class, "slot", sys_finalized_p, kw_initargs, [kw_finalized_p]).
sf_sys_add_ci2(standard_class, "slot", sys_finalized_p, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(standard_class, "slot", sys_layout, kw_initargs, [kw_layout]).
sf_sys_add_ci2(standard_class, "slot", sys_layout, kw_readers, [sys_class_layout]).
sf_sys_add_ci2(standard_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(standard_class, "slot", sys_name, kw_readers, [class_name]).
sf_sys_add_ci2(standard_class, "slot", sys_precedence_list, kw_initargs, [kw_precedence_list]).
sf_sys_add_ci2(standard_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(standard_class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(standard_class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(standard_class, "slot", sys_slots, kw_initargs, [kw_slots]).
sf_sys_add_ci2(standard_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(standard_method, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(standard_method, "method", add_method, kw_name, add_method).
sf_sys_add_ci2(standard_method, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(standard_method, "method", function_keywords, kw_name, function_keywords).
sf_sys_add_ci2(standard_method, "method", method_qualifiers, kw_name, method_qualifiers).
sf_sys_add_ci2(standard_method, "method", remove_method, kw_name, remove_method).
sf_sys_add_ci2(standard_method, "method", sys_make_method_lambda, kw_name, sys_make_method_lambda).
sf_sys_add_ci2(standard_method, "method", sys_method_function, kw_name, sys_method_function).
sf_sys_add_ci2(standard_method, "method", sys_method_generic_function, kw_name, sys_method_generic_function).
sf_sys_add_ci2(standard_method, "method", sys_method_lambda_list, kw_name, sys_method_lambda_list).
sf_sys_add_ci2(standard_method, "method", sys_method_specializers, kw_name, sys_method_specializers).
sf_sys_add_ci2(standard_method, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_method, "slot", sys_pf_documentation, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_method, "slot", sys_pf_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(standard_method, "slot", sys_pf_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(standard_method, "slot", sys_pf_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(standard_method, "slot", sys_pf_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(standard_method, "slot", sys_pf_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(standard_method, "slot", sys_fast_function, kw_initargs, [kw_fast_function]).
sf_sys_add_ci2(standard_method, "slot", sys_keywords, kw_initargs, [kw_keywords]).
sf_sys_add_ci2(standard_method, "slot", sys_keywords, kw_readers, [sys_method_keywords]).
sf_sys_add_ci2(standard_method, "slot", sys_keywords, kw_writers, [[setf, sys_method_keywords]]).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_initargs, [kw_lambda_list]).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_readers, [sys_method_lambda_list]).
sf_sys_add_ci2(standard_method, "slot", sys_lambda_list, kw_writers, [[setf, sys_method_lambda_list]]).
sf_sys_add_ci2(standard_method, "slot", sys_other_keywords_p, kw_initargs, [kw_other_keywords_p]).
sf_sys_add_ci2(standard_method, "slot", sys_plist, kw_initargs, [kw_plist]).
sf_sys_add_ci2(standard_method, "slot", sys_plist, kw_readers, [sys_method_plist]).
sf_sys_add_ci2(standard_method, "slot", sys_plist, kw_writers, [[setf, sys_method_plist]]).
sf_sys_add_ci2(standard_method, "slot", sys_qualifiers, kw_initargs, [kw_qualifiers]).
sf_sys_add_ci2(standard_method, "slot", sys_qualifiers, kw_readers, [method_qualifiers]).
sf_sys_add_ci2(standard_method, "slot", sys_qualifiers, kw_writers, [[setf, method_qualifiers]]).
sf_sys_add_ci2(standard_method, "slot", sys_simple_next_method_call, kw_initargs, [sys_simple_next_method_call]).
sf_sys_add_ci2(standard_method, "slot", sys_simple_next_method_call, kw_readers, [sys_simple_next_method_call_p]).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_initargs, [kw_specializers]).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_readers, [sys_method_specializers]).
sf_sys_add_ci2(standard_method, "slot", sys_specializers, kw_writers, [[setf, sys_method_specializers]]).
sf_sys_add_ci2(standard_method, "slot", sys_the_function, kw_initargs, [kw_function]).
sf_sys_add_ci2(standard_method, "slot", sys_the_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(standard_method, "slot", sys_the_function, kw_readers, [sys_method_function]).
sf_sys_add_ci2(standard_method, "slot", sys_the_function, kw_writers, [[setf, sys_method_function]]).
sf_sys_add_ci2(standard_method, "slot", sys_the_generic_function, kw_initargs, [kw_generic_function]).
sf_sys_add_ci2(standard_method, "slot", sys_the_generic_function, kw_readers, [sys_method_generic_function]).
sf_sys_add_ci2(standard_method, "slot", sys_the_generic_function, kw_writers, [[setf, sys_method_generic_function]]).
sf_sys_add_ci2(standard_method, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(standard_object, "method", change_class, kw_name, change_class).
sf_sys_add_ci2(standard_object, "method", initialize_instance, kw_name, initialize_instance).
sf_sys_add_ci2(standard_object, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(standard_object, "method", reinitialize_instance, kw_name, reinitialize_instance).
sf_sys_add_ci2(standard_object, "method", shared_initialize, kw_name, shared_initialize).
sf_sys_add_ci2(standard_object, "method", update_instance_for_different_class, kw_name, update_instance_for_different_class).
sf_sys_add_ci2(standard_object, "method", update_instance_for_redefined_class, kw_name, update_instance_for_redefined_class).
sf_sys_add_ci2(structure_class, "method", [setf, class_name], kw_name, [setf, class_name]).
sf_sys_add_ci2(structure_class, "method", [setf, documentation], kw_name, [setf, documentation]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_layout], kw_name, [setf, sys_class_layout]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_default_initargs], kw_name, [setf, sys_class_default_initargs]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_direct_default_initargs], kw_name, [setf, sys_class_direct_default_initargs]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_direct_methods], kw_name, [setf, sys_class_direct_methods]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_direct_slots], kw_name, [setf, sys_class_direct_slots]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_direct_subclasses], kw_name, [setf, sys_class_direct_subclasses]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_direct_superclasses], kw_name, [setf, sys_class_direct_superclasses]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_finalized_p], kw_name, [setf, sys_class_finalized_p]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_precedence_list], kw_name, [setf, sys_class_precedence_list]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_class_slots], kw_name, [setf, sys_class_slots]).
sf_sys_add_ci2(structure_class, "method", [setf, sys_slot_value_using_class], kw_name, [setf, sys_slot_value_using_class]).
sf_sys_add_ci2(structure_class, "method", allocate_instance, kw_name, allocate_instance).
sf_sys_add_ci2(structure_class, "method", class_name, kw_name, class_name).
sf_sys_add_ci2(structure_class, "method", documentation, kw_name, documentation).
sf_sys_add_ci2(structure_class, "method", sys_class_layout, kw_name, sys_class_layout).
sf_sys_add_ci2(structure_class, "method", sys_slot_exists_p_using_class, kw_name, sys_slot_exists_p_using_class).
sf_sys_add_ci2(structure_class, "method", sys_class_default_initargs, kw_name, sys_class_default_initargs).
sf_sys_add_ci2(structure_class, "method", sys_class_direct_default_initargs, kw_name, sys_class_direct_default_initargs).
sf_sys_add_ci2(structure_class, "method", sys_class_direct_methods, kw_name, sys_class_direct_methods).
sf_sys_add_ci2(structure_class, "method", sys_class_direct_slots, kw_name, sys_class_direct_slots).
sf_sys_add_ci2(structure_class, "method", sys_class_direct_subclasses, kw_name, sys_class_direct_subclasses).
sf_sys_add_ci2(structure_class, "method", sys_class_direct_superclasses, kw_name, sys_class_direct_superclasses).
sf_sys_add_ci2(structure_class, "method", sys_class_finalized_p, kw_name, sys_class_finalized_p).
sf_sys_add_ci2(structure_class, "method", sys_class_precedence_list, kw_name, sys_class_precedence_list).
sf_sys_add_ci2(structure_class, "method", sys_class_prototype, kw_name, sys_class_prototype).
sf_sys_add_ci2(structure_class, "method", sys_class_slots, kw_name, sys_class_slots).
sf_sys_add_ci2(structure_class, "method", sys_slot_boundp_using_class, kw_name, sys_slot_boundp_using_class).
sf_sys_add_ci2(structure_class, "method", sys_slot_makunbound_using_class, kw_name, sys_slot_makunbound_using_class).
sf_sys_add_ci2(structure_class, "method", sys_slot_value_using_class, kw_name, sys_slot_value_using_class).
sf_sys_add_ci2(structure_class, "slot", documentation, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_constructors, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_copier, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_default_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_default_initargs, kw_readers, [sys_class_default_initargs]).
sf_sys_add_ci2(structure_class, "slot", sys_default_initargs, kw_writers, [[setf, sys_class_default_initargs]]).
sf_sys_add_ci2(structure_class, "slot", sys_defstruct_constructor, kw_readers, [sys_class_defstruct_constructor]).
sf_sys_add_ci2(structure_class, "slot", sys_defstruct_constructor, kw_writers, [[setf, sys_class_defstruct_constructor]]).
sf_sys_add_ci2(structure_class, "slot", sys_defstruct_form, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_defstruct_form, kw_readers, [sys_class_defstruct_form]).
sf_sys_add_ci2(structure_class, "slot", sys_defstruct_form, kw_writers, [[setf, sys_class_defstruct_form]]).
sf_sys_add_ci2(structure_class, "slot", sys_dependents, kw_readers, [sys_class_dependents]).
sf_sys_add_ci2(structure_class, "slot", sys_dependents, kw_writers, [[setf, sys_class_dependents]]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_default_initargs, kw_initargs, [kw_direct_default_initargs]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_default_initargs, kw_readers, [sys_class_direct_default_initargs]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_default_initargs, kw_writers, [[setf, sys_class_direct_default_initargs]]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_generic_functions, kw_readers, [sys_specializer_direct_generic_functions]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_generic_functions, kw_writers, [[setf, sys_specializer_direct_generic_functions]]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_methods, kw_readers, [sys_specializer_direct_methods]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_methods, kw_writers, [[setf, sys_specializer_direct_methods]]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_slots, kw_initargs, [kw_direct_slots]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_direct_slots, kw_readers, [sys_class_direct_slots]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_slots, kw_writers, [[setf, sys_class_direct_slots]]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_subclasses, kw_readers, [sys_class_direct_subclasses]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_subclasses, kw_writers, [[setf, sys_class_direct_subclasses]]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_superclasses, kw_initargs, [kw_direct_superclasses]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_superclasses, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_direct_superclasses, kw_readers, [sys_class_direct_superclasses]).
sf_sys_add_ci2(structure_class, "slot", sys_direct_superclasses, kw_writers, [[setf, sys_class_direct_superclasses]]).
sf_sys_add_ci2(structure_class, "slot", sys_finalized, kw_readers, [sys_class_finalized_p]).
sf_sys_add_ci2(structure_class, "slot", sys_finalized, kw_writers, [[setf, sys_class_finalized_p]]).
sf_sys_add_ci2(structure_class, "slot", sys_flag, kw_readers, [sys_eql_specializer_flag]).
sf_sys_add_ci2(structure_class, "slot", sys_flag, kw_writers, [[setf, sys_eql_specializer_flag]]).
sf_sys_add_ci2(structure_class, "slot", sys_from_defclass_p, kw_initargs, [kw_from_defclass_p]).
sf_sys_add_ci2(structure_class, "slot", sys_initial_offset, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_location_table, kw_readers, [sys_class_location_table]).
sf_sys_add_ci2(structure_class, "slot", sys_location_table, kw_writers, [[setf, sys_class_location_table]]).
sf_sys_add_ci2(structure_class, "slot", sys_name, kw_initargs, [kw_name]).
sf_sys_add_ci2(structure_class, "slot", sys_name, kw_readers, [sys_class_id]).
sf_sys_add_ci2(structure_class, "slot", sys_name, kw_writers, [[setf, sys_class_id]]).
sf_sys_add_ci2(structure_class, "slot", sys_precedence_list, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_precedence_list, kw_readers, [sys_class_precedence_list]).
sf_sys_add_ci2(structure_class, "slot", sys_precedence_list, kw_writers, [[setf, sys_class_precedence_list]]).
sf_sys_add_ci2(structure_class, "slot", sys_predicate, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_print_function, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_sealedp, kw_initargs, [kw_sealedp]).
sf_sys_add_ci2(structure_class, "slot", sys_sealedp, kw_readers, [sys_class_sealedp]).
sf_sys_add_ci2(structure_class, "slot", sys_sealedp, kw_writers, [[setf, sys_class_sealedp]]).
sf_sys_add_ci2(structure_class, "slot", sys_size, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_size, kw_readers, [sys_class_size]).
sf_sys_add_ci2(structure_class, "slot", sys_size, kw_writers, [[setf, sys_class_size]]).
sf_sys_add_ci2(structure_class, "slot", sys_slot_descriptions, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_slots, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_slots, kw_readers, [sys_class_slots]).
sf_sys_add_ci2(structure_class, "slot", sys_slots, kw_writers, [[setf, sys_class_slots]]).
sf_sys_add_ci2(structure_class, "slot", sys_valid_initargs, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_valid_initargs, kw_readers, [sys_class_valid_initargs]).
sf_sys_add_ci2(structure_class, "slot", sys_valid_initargs, kw_writers, [[setf, sys_class_valid_initargs]]).
sf_sys_add_ci2(structure_class, "slot", sys_docstring, kw_initargs, [kw_documentation]).
sf_sys_add_ci2(structure_class, "slot", sys_slot_table, kw_initform, sys_c43_initform_unsupplied_c43).
sf_sys_add_ci2(structure_class, "slot", sys_slot_table, kw_readers, [sys_slot_table]).
sf_sys_add_ci2(structure_class, "slot", sys_slot_table, kw_writers, [[setf, sys_slot_table]]).
sf_sys_add_ci2(broadcast_stream, "slot", sys_bout, kw_initform, function(sys_broadcast_bout)).
sf_sys_add_ci2(broadcast_stream, "slot", sys_bout, kw_type, function).
sf_sys_add_ci2(broadcast_stream, "slot", sys_misc, kw_initform, function(sys_broadcast_misc)).
sf_sys_add_ci2(broadcast_stream, "slot", sys_misc, kw_type, function).
sf_sys_add_ci2(broadcast_stream, "slot", sys_out, kw_initform, function(sys_broadcast_out)).
sf_sys_add_ci2(broadcast_stream, "slot", sys_out, kw_type, function).
sf_sys_add_ci2(broadcast_stream, "slot", sys_sout, kw_initform, function(sys_broadcast_sout)).
sf_sys_add_ci2(broadcast_stream, "slot", sys_sout, kw_type, function).
sf_sys_add_ci2(broadcast_stream, "slot", sys_streams, kw_type, list).
sf_sys_add_ci2(concatenated_stream, "slot", sys_bin, kw_initform, function(sys_concatenated_bin)).
sf_sys_add_ci2(concatenated_stream, "slot", sys_bin, kw_type, function).
sf_sys_add_ci2(concatenated_stream, "slot", sys_in, kw_initform, function(sys_concatenated_in)).
sf_sys_add_ci2(concatenated_stream, "slot", sys_in, kw_type, function).
sf_sys_add_ci2(concatenated_stream, "slot", sys_misc, kw_initform, function(sys_concatenated_misc)).
sf_sys_add_ci2(concatenated_stream, "slot", sys_misc, kw_type, function).
sf_sys_add_ci2(concatenated_stream, "slot", sys_n_bin, kw_initform, function(sys_concatenated_n_bin)).
sf_sys_add_ci2(concatenated_stream, "slot", sys_n_bin, kw_type, function).
sf_sys_add_ci2(concatenated_stream, "slot", sys_streams, kw_type, list).
sf_sys_add_ci2(echo_stream, "slot", sys_bin, kw_initform, function(sys_echo_bin)).
sf_sys_add_ci2(echo_stream, "slot", sys_bin, kw_type, function).
sf_sys_add_ci2(echo_stream, "slot", sys_in, kw_initform, function(sys_echo_in)).
sf_sys_add_ci2(echo_stream, "slot", sys_in, kw_type, function).
sf_sys_add_ci2(echo_stream, "slot", sys_misc, kw_initform, function(sys_echo_misc)).
sf_sys_add_ci2(echo_stream, "slot", sys_misc, kw_type, function).
sf_sys_add_ci2(echo_stream, "slot", sys_n_bin, kw_initform, function(sys_echo_n_bin)).
sf_sys_add_ci2(echo_stream, "slot", sys_n_bin, kw_type, function).
sf_sys_add_ci2(echo_stream, "slot", sys_unread_stuff, kw_type, boolean).
sf_sys_add_ci2(hash_table, "slot", sys_cache, kw_type, [or, null, sys_index]).
sf_sys_add_ci2(hash_table, "slot", sys_hash_fun, kw_type, function).
sf_sys_add_ci2(hash_table, "slot", sys_hash_vector, kw_type, [or, null, [simple_array, sys_word, [*]]]).
sf_sys_add_ci2(hash_table, "slot", sys_index_vector, kw_type, [simple_array, sys_word, [*]]).
sf_sys_add_ci2(hash_table, "slot", sys_lock, kw_initform, [sys_make_mutex, kw_name, '$ARRAY'([*], claz_base_character, [#\(h), #\(a), #\(s), #\(h), #\(-), #\(t), #\(a), #\(b), #\(l), #\(e), #\(' '), #\(l), #\(o), #\(c), #\(k)])]).
sf_sys_add_ci2(hash_table, "slot", sys_lock, kw_type, sys_mutex).
sf_sys_add_ci2(hash_table, "slot", sys_needs_rehash_p, kw_type, [member, [], t]).
sf_sys_add_ci2(hash_table, "slot", sys_next_free_kv, kw_initform, 0).
sf_sys_add_ci2(hash_table, "slot", sys_next_free_kv, kw_type, sys_index).
sf_sys_add_ci2(hash_table, "slot", sys_next_vector, kw_type, [simple_array, sys_word, [*]]).
sf_sys_add_ci2(hash_table, "slot", sys_next_weak_hash_table, kw_type, null).
sf_sys_add_ci2(hash_table, "slot", sys_number_entries, kw_initform, 0).
sf_sys_add_ci2(hash_table, "slot", sys_number_entries, kw_type, sys_index).
sf_sys_add_ci2(hash_table, "slot", sys_rehash_size, kw_type, [or, sys_index, [single_float, [1.0]]]).
sf_sys_add_ci2(hash_table, "slot", sys_rehash_threshold, kw_type, [single_float, [0.0], 1.0]).
sf_sys_add_ci2(hash_table, "slot", sys_rehash_trigger, kw_type, sys_index).
sf_sys_add_ci2(hash_table, "slot", sys_synchronized_p, kw_type, [member, [], t]).
sf_sys_add_ci2(hash_table, "slot", sys_table, kw_type, simple_vector).
sf_sys_add_ci2(hash_table, "slot", sys_test, kw_type, symbol).
sf_sys_add_ci2(hash_table, "slot", sys_test_fun, kw_type, function).
sf_sys_add_ci2(hash_table, "slot", sys_weakness, kw_type, [member, [], kw_key, kw_value, kw_key_or_value, kw_key_and_value]).
sf_sys_add_ci2(package, "slot", sys_pf_implementation_packages, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_pf_local_nicknames, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_pf_locally_nicknamed_by, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_pf_name, kw_type, [or, simple_string, null]).
sf_sys_add_ci2(package, "slot", sys_pf_nicknames, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_pf_shadowing_symbols, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_pf_use_list, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_pf_used_by_list, kw_type, list).
sf_sys_add_ci2(package, "slot", sys_doc_string, kw_type, [or, simple_string, null]).
sf_sys_add_ci2(package, "slot", sys_external_symbols, kw_type, sys_package_hashtable).
sf_sys_add_ci2(package, "slot", sys_internal_symbols, kw_type, sys_package_hashtable).
sf_sys_add_ci2(package, "slot", sys_lock, kw_type, boolean).
sf_sys_add_ci2(package, "slot", sys_mru_table_index, kw_initform, 0).
sf_sys_add_ci2(package, "slot", sys_mru_table_index, kw_type, sys_index).
sf_sys_add_ci2(package, "slot", sys_source_location, kw_type, [or, null, sys_definition_source_location]).
sf_sys_add_ci2(package, "slot", sys_tables, kw_initform, '$OBJ'(claz_vector, [])).
sf_sys_add_ci2(package, "slot", sys_tables, kw_type, simple_vector).
sf_sys_add_ci2(pathname, "slot", directory, kw_type, list).
sf_sys_add_ci2(pathname, "slot", sys_device, kw_type, [or, simple_string, sys_pathname_component_tokens]).
sf_sys_add_ci2(pathname, "slot", sys_name, kw_type, [or, simple_string, sys_pattern, sys_pathname_component_tokens]).
sf_sys_add_ci2(pathname, "slot", sys_version, kw_type, [or, integer, sys_pathname_component_tokens, [member, kw_newest]]).
sf_sys_add_ci2(pathname, "slot", sys_host, kw_type, [or, sys_host, null]).
sf_sys_add_ci2(pathname, "slot", type, kw_type, [or, simple_string, sys_pattern, sys_pathname_component_tokens]).
sf_sys_add_ci2(readtable, "slot", sys_pf_readtable_case, kw_initform, kw_upcase).
sf_sys_add_ci2(readtable, "slot", sys_pf_readtable_case, kw_type, [member, kw_upcase, kw_downcase, kw_preserve, kw_invert]).
sf_sys_add_ci2(readtable, "slot", sys_pf_readtable_normalization, kw_initform, t).
sf_sys_add_ci2(readtable, "slot", sys_pf_readtable_normalization, kw_type, boolean).
sf_sys_add_ci2(readtable, "slot", sys_character_attribute_array, kw_initform, [make_array, sys_base_char_code_limit, kw_element_type, [quote, [unsigned_byte, 8]], kw_initial_element, sys_c43_char_attr_constituent_c43]).
sf_sys_add_ci2(readtable, "slot", sys_character_attribute_array, kw_type, sys_attribute_table).
sf_sys_add_ci2(readtable, "slot", sys_character_attribute_hash_table, kw_initform, [make_hash_table]).
sf_sys_add_ci2(readtable, "slot", sys_character_attribute_hash_table, kw_type, hash_table).
sf_sys_add_ci2(readtable, "slot", sys_character_macro_array, kw_initform, [make_array, sys_base_char_code_limit, kw_initial_element, []]).
sf_sys_add_ci2(readtable, "slot", sys_character_macro_array, kw_type, [simple_vector, 128]).
sf_sys_add_ci2(readtable, "slot", sys_character_macro_hash_table, kw_initform, [make_hash_table]).
sf_sys_add_ci2(readtable, "slot", sys_character_macro_hash_table, kw_type, hash_table).
sf_sys_add_ci2(restart, "slot", function, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(restart, "slot", function, kw_type, function).
sf_sys_add_ci2(restart, "slot", sys_associated_conditions, kw_initform, [quote, []]).
sf_sys_add_ci2(restart, "slot", sys_associated_conditions, kw_type, list).
sf_sys_add_ci2(restart, "slot", sys_interactive_function, kw_type, [or, null, function]).
sf_sys_add_ci2(restart, "slot", sys_name, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(restart, "slot", sys_name, kw_type, symbol).
sf_sys_add_ci2(restart, "slot", sys_report_function, kw_type, [or, null, function]).
sf_sys_add_ci2(restart, "slot", sys_test_function, kw_initform, [lambda, [cond], [declare, [ignore, cond]], t]).
sf_sys_add_ci2(restart, "slot", sys_test_function, kw_type, function).
sf_sys_add_ci2(sys_anode, "slot", sys_code, kw_initargs, [kw_code]).
sf_sys_add_ci2(sys_anode, "slot", sys_code, kw_readers, [sys_anode_code]).
sf_sys_add_ci2(sys_anode, "slot", sys_code, kw_writers, [[setf, sys_anode_code]]).
sf_sys_add_ci2(sys_anode, "slot", sys_seclass, kw_initargs, [kw_seclass]).
sf_sys_add_ci2(sys_anode, "slot", sys_seclass, kw_readers, [sys_anode_seclass]).
sf_sys_add_ci2(sys_anode, "slot", sys_seclass, kw_writers, [[setf, sys_anode_seclass]]).
sf_sys_add_ci2(sys_anode, "slot", type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_anode, "slot", type, kw_readers, [sys_anode_type]).
sf_sys_add_ci2(sys_anode, "slot", type, kw_writers, [[setf, sys_anode_type]]).
sf_sys_add_ci2(sys_arg_info, "slot", sys_arg_info_lambda_list, kw_initform, kw_no_lambda_list).
sf_sys_add_ci2(sys_arg_info, "slot", sys_gf_info_c_a_m_emf_std_p, kw_initform, t).
sf_sys_add_ci2(sys_block_end, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_block_end, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_block_end, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_block_end, "slot", sys_suffix, kw_initargs, [kw_suffix]).
sf_sys_add_ci2(sys_block_end, "slot", sys_suffix, kw_type, [or, null, string]).
sf_sys_add_ci2(sys_block_start, "slot", sys_block_end, kw_initargs, [kw_block_end]).
sf_sys_add_ci2(sys_block_start, "slot", sys_block_end, kw_type, [or, null, sys_block_end]).
sf_sys_add_ci2(sys_block_start, "slot", sys_depth, kw_initargs, [kw_depth]).
sf_sys_add_ci2(sys_block_start, "slot", sys_depth, kw_initform, 0).
sf_sys_add_ci2(sys_block_start, "slot", sys_depth, kw_type, sys_index).
sf_sys_add_ci2(sys_block_start, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_block_start, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_block_start, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_block_start, "slot", sys_prefix, kw_initargs, [kw_prefix]).
sf_sys_add_ci2(sys_block_start, "slot", sys_prefix, kw_type, [or, null, string]).
sf_sys_add_ci2(sys_block_start, "slot", sys_section_end, kw_initargs, [kw_section_end]).
sf_sys_add_ci2(sys_block_start, "slot", sys_section_end, kw_type, [or, null, sys_newline, sys_block_end]).
sf_sys_add_ci2(sys_block_start, "slot", sys_suffix, kw_initargs, [kw_suffix]).
sf_sys_add_ci2(sys_block_start, "slot", sys_suffix, kw_type, [or, null, string]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_file, kw_initargs, [kw_file]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_file, kw_readers, [sys_c_source_point_file]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_file, kw_writers, [[setf, sys_c_source_point_file]]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_lineno1, kw_initargs, [kw_lineno1]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_lineno1, kw_readers, [sys_c_source_point_lineno1]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_lineno1, kw_writers, [[setf, sys_c_source_point_lineno1]]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_lineno2, kw_initargs, [kw_lineno2]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_lineno2, kw_readers, [sys_c_source_point_lineno2]).
sf_sys_add_ci2(sys_c_source_point, "slot", sys_lineno2, kw_writers, [[setf, sys_c_source_point_lineno2]]).
sf_sys_add_ci2(sys_class_precedence_description, "slot", sys_cpd_count, kw_initform, 0).
sf_sys_add_ci2(sys_const, "slot", sys_form, kw_initargs, [kw_form]).
sf_sys_add_ci2(sys_const, "slot", sys_form, kw_readers, [sys_const_form]).
sf_sys_add_ci2(sys_const, "slot", sys_form, kw_writers, [[setf, sys_const_form]]).
sf_sys_add_ci2(sys_const, "slot", sys_horizon, kw_initargs, [kw_horizon]).
sf_sys_add_ci2(sys_const, "slot", sys_horizon, kw_readers, [sys_const_horizon]).
sf_sys_add_ci2(sys_const, "slot", sys_horizon, kw_writers, [[setf, sys_const_horizon]]).
sf_sys_add_ci2(sys_const, "slot", sys_ltv_form, kw_initargs, [kw_ltv_form]).
sf_sys_add_ci2(sys_const, "slot", sys_ltv_form, kw_readers, [sys_const_ltv_form]).
sf_sys_add_ci2(sys_const, "slot", sys_ltv_form, kw_writers, [[setf, sys_const_ltv_form]]).
sf_sys_add_ci2(sys_const, "slot", sys_value, kw_initargs, [kw_value]).
sf_sys_add_ci2(sys_const, "slot", sys_value, kw_readers, [sys_const_value]).
sf_sys_add_ci2(sys_const, "slot", sys_value, kw_writers, [[setf, sys_const_value]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_atsign_p, kw_initargs, [kw_atsign_p]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_atsign_p, kw_readers, [sys_csd_atsign_p]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_atsign_p, kw_type, symbol).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_atsign_p, kw_writers, [[setf, sys_csd_atsign_p]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_clause_chain, kw_initargs, [kw_clause_chain]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_clause_chain, kw_readers, [sys_csd_clause_chain]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_clause_chain, kw_writers, [[setf, sys_csd_clause_chain]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_colon_p, kw_initargs, [kw_colon_p]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_colon_p, kw_readers, [sys_csd_colon_p]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_colon_p, kw_type, symbol).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_colon_p, kw_writers, [[setf, sys_csd_colon_p]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_cs_index, kw_initargs, [kw_cs_index]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_cs_index, kw_readers, [sys_csd_cs_index]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_cs_index, kw_type, fixnum).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_cs_index, kw_writers, [[setf, sys_csd_cs_index]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_data, kw_initargs, [kw_data]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_data, kw_readers, [sys_csd_data]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_data, kw_writers, [[setf, sys_csd_data]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_parm_list, kw_initargs, [kw_parm_list]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_parm_list, kw_readers, [sys_csd_parm_list]).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_parm_list, kw_type, list).
sf_sys_add_ci2(sys_control_string_directive, "slot", sys_parm_list, kw_writers, [[setf, sys_csd_parm_list]]).
sf_sys_add_ci2(sys_control_string_directive, "slot", type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_control_string_directive, "slot", type, kw_readers, [sys_csd_type]).
sf_sys_add_ci2(sys_control_string_directive, "slot", type, kw_type, fixnum).
sf_sys_add_ci2(sys_control_string_directive, "slot", type, kw_writers, [[setf, sys_csd_type]]).
sf_sys_add_ci2(sys_fast_instance_boundp, "slot", sys_index, kw_initform, 0).
sf_sys_add_ci2(sys_fast_instance_boundp, "slot", sys_index, kw_type, fixnum).
sf_sys_add_ci2(sys_fast_method_call, "slot", function, kw_initform, function(identity)).
sf_sys_add_ci2(sys_fast_method_call, "slot", function, kw_type, function).
sf_sys_add_ci2(sys_indentation, "slot", sys_amount, kw_initargs, [kw_amount]).
sf_sys_add_ci2(sys_indentation, "slot", sys_amount, kw_initform, 0).
sf_sys_add_ci2(sys_indentation, "slot", sys_amount, kw_type, fixnum).
sf_sys_add_ci2(sys_indentation, "slot", sys_kind, kw_initargs, [kw_kind]).
sf_sys_add_ci2(sys_indentation, "slot", sys_kind, kw_initform, [sys_required_argument]).
sf_sys_add_ci2(sys_indentation, "slot", sys_kind, kw_type, [member, kw_block, kw_current]).
sf_sys_add_ci2(sys_indentation, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_indentation, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_indentation, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_input_character, "slot", char, kw_initargs, [kw_char]).
sf_sys_add_ci2(sys_input_character, "slot", char, kw_readers, [sys_input_character_char]).
sf_sys_add_ci2(sys_input_character, "slot", char, kw_type, [or, null, character]).
sf_sys_add_ci2(sys_input_character, "slot", char, kw_writers, [[setf, sys_input_character_char]]).
sf_sys_add_ci2(sys_input_character, "slot", sys_bits, kw_initargs, [kw_bits]).
sf_sys_add_ci2(sys_input_character, "slot", sys_bits, kw_readers, [sys_input_character_bits]).
sf_sys_add_ci2(sys_input_character, "slot", sys_bits, kw_type, [integer, 0, 15]).
sf_sys_add_ci2(sys_input_character, "slot", sys_bits, kw_writers, [[setf, sys_input_character_bits]]).
sf_sys_add_ci2(sys_input_character, "slot", sys_font, kw_initargs, [kw_font]).
sf_sys_add_ci2(sys_input_character, "slot", sys_font, kw_readers, [sys_input_character_font]).
sf_sys_add_ci2(sys_input_character, "slot", sys_font, kw_type, [integer, 0, 15]).
sf_sys_add_ci2(sys_input_character, "slot", sys_font, kw_writers, [[setf, sys_input_character_font]]).
sf_sys_add_ci2(sys_input_character, "slot", sys_key, kw_initargs, [kw_key]).
sf_sys_add_ci2(sys_input_character, "slot", sys_key, kw_readers, [sys_input_character_key]).
sf_sys_add_ci2(sys_input_character, "slot", sys_key, kw_type, [or, null, character, symbol]).
sf_sys_add_ci2(sys_input_character, "slot", sys_key, kw_writers, [[setf, sys_input_character_key]]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_per_line_prefix_end, kw_initargs, [kw_per_line_prefix_end]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_per_line_prefix_end, kw_initform, 0).
sf_sys_add_ci2(sys_logical_block, "slot", sys_per_line_prefix_end, kw_type, sys_index).
sf_sys_add_ci2(sys_logical_block, "slot", sys_prefix_length, kw_initargs, [kw_prefix_length]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_prefix_length, kw_initform, 0).
sf_sys_add_ci2(sys_logical_block, "slot", sys_prefix_length, kw_type, sys_index).
sf_sys_add_ci2(sys_logical_block, "slot", sys_section_column, kw_initargs, [kw_section_column]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_section_column, kw_initform, 0).
sf_sys_add_ci2(sys_logical_block, "slot", sys_section_column, kw_type, sys_column).
sf_sys_add_ci2(sys_logical_block, "slot", sys_section_start_line, kw_initargs, [kw_section_start_line]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_section_start_line, kw_initform, 0).
sf_sys_add_ci2(sys_logical_block, "slot", sys_section_start_line, kw_type, sys_index).
sf_sys_add_ci2(sys_logical_block, "slot", sys_start_column, kw_initargs, [kw_start_column]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_start_column, kw_initform, 0).
sf_sys_add_ci2(sys_logical_block, "slot", sys_start_column, kw_type, sys_column).
sf_sys_add_ci2(sys_logical_block, "slot", sys_suffix_length, kw_initargs, [kw_suffix_length]).
sf_sys_add_ci2(sys_logical_block, "slot", sys_suffix_length, kw_initform, 0).
sf_sys_add_ci2(sys_logical_block, "slot", sys_suffix_length, kw_type, sys_index).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_bindings, kw_initargs, [kw_bindings]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_bindings, kw_readers, [sys_li_bindings]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_bindings, kw_writers, [[setf, sys_li_bindings]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_declspecs, kw_initargs, [kw_declspecs]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_declspecs, kw_readers, [sys_li_declspecs]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_declspecs, kw_writers, [[setf, sys_li_declspecs]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_depends_preceding, kw_initargs, [kw_depends_preceding]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_depends_preceding, kw_readers, [sys_li_depends_preceding]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_depends_preceding, kw_writers, [[setf, sys_li_depends_preceding]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_endtest_forms, kw_initargs, [kw_endtest_forms]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_endtest_forms, kw_readers, [sys_li_endtest_forms]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_endtest_forms, kw_writers, [[setf, sys_li_endtest_forms]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_everytime, kw_initargs, [kw_everytime]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_everytime, kw_readers, [sys_li_everytime]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_everytime, kw_writers, [[setf, sys_li_everytime]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_later_depend, kw_initargs, [kw_later_depend]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_later_depend, kw_readers, [sys_li_later_depend]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_later_depend, kw_writers, [[setf, sys_li_later_depend]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_preamble, kw_initargs, [kw_preamble]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_preamble, kw_readers, [sys_li_preamble]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_preamble, kw_writers, [[setf, sys_li_preamble]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_requires_stepbefore, kw_initargs, [kw_requires_stepbefore]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_requires_stepbefore, kw_readers, [sys_li_requires_stepbefore]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_requires_stepbefore, kw_writers, [[setf, sys_li_requires_stepbefore]]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_specform, kw_initargs, [kw_specform]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_specform, kw_readers, [sys_li_specform]).
sf_sys_add_ci2(sys_loop_initialization, "slot", sys_specform, kw_writers, [[setf, sys_li_specform]]).
sf_sys_add_ci2(sys_method_call, "slot", function, kw_initform, function(identity)).
sf_sys_add_ci2(sys_method_call, "slot", function, kw_type, function).
sf_sys_add_ci2(sys_newline, "slot", sys_depth, kw_initargs, [kw_depth]).
sf_sys_add_ci2(sys_newline, "slot", sys_depth, kw_initform, 0).
sf_sys_add_ci2(sys_newline, "slot", sys_depth, kw_type, sys_index).
sf_sys_add_ci2(sys_newline, "slot", sys_kind, kw_initargs, [kw_kind]).
sf_sys_add_ci2(sys_newline, "slot", sys_kind, kw_initform, [sys_required_argument]).
sf_sys_add_ci2(sys_newline, "slot", sys_kind, kw_type, [member, kw_linear, kw_fill, kw_miser, kw_literal, kw_mandatory]).
sf_sys_add_ci2(sys_newline, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_newline, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_newline, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_newline, "slot", sys_section_end, kw_initargs, [kw_section_end]).
sf_sys_add_ci2(sys_newline, "slot", sys_section_end, kw_type, [or, null, sys_newline, sys_block_end]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", function, kw_initargs, [kw_function]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", function, kw_initform, [sys_required_argument]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", function, kw_type, [or, function, symbol]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", sys_initial_p, kw_initargs, [kw_initial_p]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", sys_initial_p, kw_initform, [not, [boundp, [quote, sys_xx_initial_pprint_dispatch_xx]]]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", sys_initial_p, kw_type, [member, t, []]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", sys_priority, kw_initargs, [kw_priority]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", sys_priority, kw_initform, 0).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", sys_priority, kw_type, real).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", type, kw_initargs, [kw_type]).
sf_sys_add_ci2(sys_pprint_dispatch_entry, "slot", type, kw_initform, [sys_required_argument]).
sf_sys_add_ci2(sys_pprint_dispatch_table, "slot", sys_cons_entries, kw_initargs, [kw_cons_entries]).
sf_sys_add_ci2(sys_pprint_dispatch_table, "slot", sys_cons_entries, kw_initform, [make_hash_table, kw_test, function(eql)]).
sf_sys_add_ci2(sys_pprint_dispatch_table, "slot", sys_entries, kw_initargs, [kw_entries]).
sf_sys_add_ci2(sys_pprint_dispatch_table, "slot", sys_entries, kw_type, list).
sf_sys_add_ci2(sys_pprint_dispatch_table, "slot", sys_read_only_p, kw_initargs, [kw_read_only_p]).
sf_sys_add_ci2(sys_pv_table, "slot", sys_cache, kw_type, [or, sys_cache, null]).
sf_sys_add_ci2(sys_pv_table, "slot", sys_pv_size, kw_initform, 0).
sf_sys_add_ci2(sys_pv_table, "slot", sys_pv_size, kw_type, fixnum).
sf_sys_add_ci2(sys_pv_table, "slot", sys_slot_name_lists, kw_type, list).
sf_sys_add_ci2(sys_queued_op, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_queued_op, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_queued_op, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_section_start, "slot", sys_depth, kw_initargs, [kw_depth]).
sf_sys_add_ci2(sys_section_start, "slot", sys_depth, kw_initform, 0).
sf_sys_add_ci2(sys_section_start, "slot", sys_depth, kw_type, sys_index).
sf_sys_add_ci2(sys_section_start, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_section_start, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_section_start, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_section_start, "slot", sys_section_end, kw_initargs, [kw_section_end]).
sf_sys_add_ci2(sys_section_start, "slot", sys_section_end, kw_type, [or, null, sys_newline, sys_block_end]).
sf_sys_add_ci2(sys_slot_info, "slot", boundp, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(sys_slot_info, "slot", boundp, kw_type, function).
sf_sys_add_ci2(sys_slot_info, "slot", sys_reader, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(sys_slot_info, "slot", sys_reader, kw_type, function).
sf_sys_add_ci2(sys_slot_info, "slot", sys_typecheck, kw_type, [or, null, function]).
sf_sys_add_ci2(sys_slot_info, "slot", sys_writer, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(sys_slot_info, "slot", sys_writer, kw_type, function).
sf_sys_add_ci2(sys_tab, "slot", sys_colinc, kw_initargs, [kw_colinc]).
sf_sys_add_ci2(sys_tab, "slot", sys_colinc, kw_initform, 0).
sf_sys_add_ci2(sys_tab, "slot", sys_colinc, kw_type, sys_column).
sf_sys_add_ci2(sys_tab, "slot", sys_colnum, kw_initargs, [kw_colnum]).
sf_sys_add_ci2(sys_tab, "slot", sys_colnum, kw_initform, 0).
sf_sys_add_ci2(sys_tab, "slot", sys_colnum, kw_type, sys_column).
sf_sys_add_ci2(sys_tab, "slot", sys_posn, kw_initargs, [kw_posn]).
sf_sys_add_ci2(sys_tab, "slot", sys_posn, kw_initform, 0).
sf_sys_add_ci2(sys_tab, "slot", sys_posn, kw_type, sys_posn).
sf_sys_add_ci2(sys_tab, "slot", sys_relativep, kw_initargs, [kw_relativep]).
sf_sys_add_ci2(sys_tab, "slot", sys_relativep, kw_type, [member, t, []]).
sf_sys_add_ci2(sys_tab, "slot", sys_sectionp, kw_initargs, [kw_sectionp]).
sf_sys_add_ci2(sys_tab, "slot", sys_sectionp, kw_type, [member, t, []]).
sf_sys_add_ci2(sys_fd_stream, "slot", listen, kw_type, [member, [], t, kw_eof]).
sf_sys_add_ci2(sys_fd_stream, "slot", pathname, kw_type, [or, pathname, null]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_bivalent_p, kw_type, boolean).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_buffering, kw_initform, kw_full).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_buffering, kw_type, [member, kw_full, kw_line, kw_none]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_char_size, kw_initform, 1).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_char_size, kw_type, [or, fixnum, function]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_element_size, kw_initform, 1).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_element_size, kw_type, sys_index).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_element_type, kw_initform, [quote, base_char]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_eof_forced_p, kw_type, [member, t, []]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_external_format, kw_initform, kw_latin_1).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_fd, kw_initform, -1).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_fd, kw_type, fixnum).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_fd_type, kw_initform, kw_unknown).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_fd_type, kw_type, keyword).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_ibuf, kw_type, [or, sys_buffer, null]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_instead, kw_initform, [make_array, 0, kw_element_type, [quote, character], kw_adjustable, t, kw_fill_pointer, t]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_instead, kw_type, [array, character, [*]]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_misc, kw_initform, function(sys_fd_stream_misc_routine)).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_misc, kw_type, function).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_obuf, kw_type, [or, sys_buffer, null]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_original, kw_type, [or, simple_string, null]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_output_bytes, kw_initform, function(sys_ill_out)).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_output_bytes, kw_type, function).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_output_column, kw_type, [or, unsigned_byte, null]).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_serve_events, kw_type, boolean).
sf_sys_add_ci2(sys_fd_stream, "slot", sys_timeout, kw_type, [or, single_float, null]).
sf_sys_add_ci2(sys_structure_stablehash, "slot", sys_hashcode, kw_initargs, [kw_hashcode]).
sf_sys_add_ci2(sys_structure_stablehash, "slot", sys_hashcode, kw_readers, [sys_structure_stablehash_hashcode]).
sf_sys_add_ci2(sys_structure_stablehash, "slot", sys_hashcode, kw_writers, [[setf, sys_structure_stablehash_hashcode]]).
sf_sys_add_ci2(sys_timer, "slot", sys_cancel_function, kw_type, [or, null, function]).
sf_sys_add_ci2(sys_timer, "slot", sys_catch_up, kw_type, boolean).
sf_sys_add_ci2(sys_timer, "slot", sys_expire_time, kw_initform, 1).
sf_sys_add_ci2(sys_timer, "slot", sys_expire_time, kw_type, [or, null, real]).
sf_sys_add_ci2(sys_timer, "slot", sys_interrupt_function, kw_type, [or, null, function]).
sf_sys_add_ci2(sys_timer, "slot", sys_repeat_interval, kw_type, [or, null, [real, 0]]).
sf_sys_add_ci2(sys_timer, "slot", sys_thread, kw_type, [or, sys_thread, boolean]).
sf_sys_add_ci2(structure_object, "method", print_object, kw_name, print_object).
sf_sys_add_ci2(synonym_stream, "slot", sys_bin, kw_initform, function(sys_synonym_bin)).
sf_sys_add_ci2(synonym_stream, "slot", sys_bin, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", sys_bout, kw_initform, function(sys_synonym_bout)).
sf_sys_add_ci2(synonym_stream, "slot", sys_bout, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", sys_in, kw_initform, function(sys_synonym_in)).
sf_sys_add_ci2(synonym_stream, "slot", sys_in, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", sys_misc, kw_initform, function(sys_synonym_misc)).
sf_sys_add_ci2(synonym_stream, "slot", sys_misc, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", sys_n_bin, kw_initform, function(sys_synonym_n_bin)).
sf_sys_add_ci2(synonym_stream, "slot", sys_n_bin, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", sys_out, kw_initform, function(sys_synonym_out)).
sf_sys_add_ci2(synonym_stream, "slot", sys_out, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", sys_sout, kw_initform, function(sys_synonym_sout)).
sf_sys_add_ci2(synonym_stream, "slot", sys_sout, kw_type, function).
sf_sys_add_ci2(synonym_stream, "slot", symbol, kw_type, symbol).
sf_sys_add_ci2(two_way_stream, "slot", sys_bin, kw_initform, function(sys_two_way_bin)).
sf_sys_add_ci2(two_way_stream, "slot", sys_bin, kw_type, function).
sf_sys_add_ci2(two_way_stream, "slot", sys_bout, kw_initform, function(sys_two_way_bout)).
sf_sys_add_ci2(two_way_stream, "slot", sys_bout, kw_type, function).
sf_sys_add_ci2(two_way_stream, "slot", sys_in, kw_initform, function(sys_two_way_in)).
sf_sys_add_ci2(two_way_stream, "slot", sys_in, kw_type, function).
sf_sys_add_ci2(two_way_stream, "slot", sys_input_stream, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(two_way_stream, "slot", sys_input_stream, kw_type, stream).
sf_sys_add_ci2(two_way_stream, "slot", sys_misc, kw_initform, function(sys_two_way_misc)).
sf_sys_add_ci2(two_way_stream, "slot", sys_misc, kw_type, function).
sf_sys_add_ci2(two_way_stream, "slot", sys_n_bin, kw_initform, function(sys_two_way_n_bin)).
sf_sys_add_ci2(two_way_stream, "slot", sys_n_bin, kw_type, function).
sf_sys_add_ci2(two_way_stream, "slot", sys_out, kw_initform, function(sys_two_way_out)).
sf_sys_add_ci2(two_way_stream, "slot", sys_out, kw_type, function).
sf_sys_add_ci2(two_way_stream, "slot", sys_output_stream, kw_initform, [sys_missing_arg]).
sf_sys_add_ci2(two_way_stream, "slot", sys_output_stream, kw_type, stream).
sf_sys_add_ci2(two_way_stream, "slot", sys_sout, kw_initform, function(sys_two_way_sout)).
sf_sys_add_ci2(two_way_stream, "slot", sys_sout, kw_type, function).
