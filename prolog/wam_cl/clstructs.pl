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
name_value_default(m(_,prolog_array_list(Type),Name),Name-mut([],array_of(Type))).
name_value_default(m(_,Type,Name),Name-Def):-value_default(Type,Def).
name_value_default(m(_,Type,Name),Name-mut(@(null),Type)).
name_value_default(N-V,N-V).

value_default(claz_prolog_concurrent_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_prolog_hash_map(Key,Value),mut([],map(Key,Value))).
value_default(claz_list,[]).
value_default(integer,0).
value_default(claz_object,mut([],claz_object)).

%value_default(claz_simple_string, @(null)).
%value_default(claz_string, @(null)).
%value_default(prolog_array_list(_),[]).
%value_default(array_of(_),[]).


%value_default(cl_simple_string, @(null)).
%value_default(cl_string, @(null)).
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


:- dynamic(system_subclazz/2).


system_subclazz(obj_simple_array__t,obj_array).
system_subclazz(obj_simple_array(unsigned_byte16),obj_array).
system_subclazz(obj_simple_array(unsigned_byte32),obj_array).
system_subclazz(obj_simple_array(unsigned_byte8),obj_array).
system_subclazz(obj_simple_array(_Unsigned_byte8),obj_array).
system_subclazz(obj_complex_array,obj_array).
system_subclazz(obj_complex_array(unsigned_byte32),obj_array).
system_subclazz(obj_complex_array(unsigned_byte8),obj_array).
system_subclazz(obj_complex_array(_Unsigned_byte8),obj_array).
system_subclazz(obj_complex_bit_vector,obj_bit_vector).
system_subclazz(obj_complex_string,obj_string).
system_subclazz(obj_complex_vector,obj_vector).
system_subclazz(obj_complex_vector(unsigned_byte32),obj_vector).
system_subclazz(obj_complex_vector(unsigned_byte8),obj_vector).
system_subclazz(obj_complex_vector(_Unsigned_byte8),obj_vector).
system_subclazz(obj_zero_rank_array,obj_array).
system_subclazz(obj_nil_vector,obj_string).
system_subclazz(obj_string,obj_vector).
system_subclazz(obj_simple_vector,obj_vector).
system_subclazz(obj_basic_vector(unsigned_byte16),obj_vector).
system_subclazz(obj_basic_vector(unsigned_byte32),obj_vector).
system_subclazz(obj_basic_vector(unsigned_byte8),obj_vector).
system_subclazz(obj_basic_vector(_Unsigned_byte8),obj_vector).
system_subclazz(obj_bit_vector,obj_vector).
system_subclazz(obj_vector,obj_array).
system_subclazz(obj_array,obj_object).
system_subclazz(obj_case_frob_stream,obj_stream).
system_subclazz(obj_control_transfer,prolog_runtime_exception).
system_subclazz(obj_dispatch_macro_function,obj_function).
system_subclazz(obj_function,obj_operator).
system_subclazz(obj_lisp_class,obj_standard_object).
system_subclazz(obj_operator,obj_object).
system_subclazz(obj_reader_macro_function,obj_function).
system_subclazz(obj_stack_frame,obj_object).
system_subclazz(obj_arithmetic_error,obj_lisp_error).
system_subclazz(obj_autoload,obj_function).
system_subclazz(obj_built_in_class,obj_lisp_class).
system_subclazz(obj_cell_error,obj_lisp_error).
system_subclazz(obj_closure,obj_function).
system_subclazz(obj_compiled_closure,obj_closure).
system_subclazz(obj_compiled_primitive,obj_primitive).
system_subclazz(obj_condition,obj_standard_object).
system_subclazz(obj_fasl_class_loader,obj_ffi_class_loader).
system_subclazz(obj_funcallable_standard_class,obj_standard_class).
system_subclazz(obj_funcallable_standard_object,obj_standard_object).
system_subclazz(obj_hash_table,obj_object).
system_subclazz(obj_integrity_error,prolog_error).
system_subclazz(obj_ffi_class_loader,prolog_url_class_loader).
system_subclazz(obj_ffi_exception,obj_lisp_error).
system_subclazz(obj_ffi_stack_frame,obj_stack_frame).
system_subclazz(obj_layout,obj_object).
system_subclazz(obj_lisp_error,obj_serious_condition).
system_subclazz(obj_lisp_integer,obj_object).
system_subclazz(obj_lisp_stack_frame,obj_stack_frame).
system_subclazz(obj_memory_class_loader,obj_ffi_class_loader).
system_subclazz(obj_pathname,obj_object).
system_subclazz(obj_primitive,obj_function).
system_subclazz(obj_print_not_readable,obj_lisp_error).
system_subclazz(obj_processing_terminated,prolog_error).
system_subclazz(obj_program_error,obj_lisp_error).
system_subclazz(obj_readtable,obj_object).
system_subclazz(obj_serious_condition,obj_condition).
system_subclazz(obj_simple_condition,obj_condition).
system_subclazz(obj_slime_input_stream,obj_stream).
system_subclazz(obj_slot_class,obj_lisp_class).
system_subclazz(obj_special_operator,obj_operator).
system_subclazz(obj_standard_class,obj_slot_class).
system_subclazz(obj_standard_object,obj_object).
system_subclazz(obj_storage_condition,obj_serious_condition).
system_subclazz(obj_stream,obj_structure_object).
system_subclazz(obj_stream_error,obj_lisp_error).
system_subclazz(obj_structure_class,obj_slot_class).
system_subclazz(obj_structure_object,obj_object).
system_subclazz(obj_symbol,obj_object).
system_subclazz(obj_thread_destroyed,prolog_error).
system_subclazz(obj_two_way_stream,obj_stream).
system_subclazz(obj_type_error,obj_lisp_error).
system_subclazz(obj_decoding_reader,prolog_pushback_reader).
system_subclazz(obj_racf_malformed_input_exception ,prolog_malformed_input_exception).
system_subclazz(obj_racf_unmappable_character_exception ,prolog_unmappable_character_exception).
system_subclazz(obj_warning,obj_condition).
system_subclazz(obj_weak_hash_table,obj_object).
system_subclazz(obj_weak_reference,obj_object).
system_subclazz(obj_autoload_generalized_reference,obj_autoload).
system_subclazz(obj_autoload_macro,obj_autoload).
system_subclazz(obj_bignum,obj_lisp_integer).
system_subclazz(obj_broadcast_stream,obj_stream).
system_subclazz(obj_byte_array_input_stream,obj_stream).
system_subclazz(obj_byte_array_output_stream,obj_stream).
system_subclazz(obj_capitalize_first_stream,obj_case_frob_stream).
system_subclazz(obj_capitalize_stream,obj_case_frob_stream).
system_subclazz(obj_complex,obj_object).
system_subclazz(obj_concatenated_stream,obj_stream).
system_subclazz(obj_cons,obj_object).
system_subclazz(obj_control_error,obj_lisp_error).
system_subclazz(obj_division_by_zero,obj_arithmetic_error).
system_subclazz(cldolist,obj_special_operator).
system_subclazz(cldotimes,obj_special_operator).
system_subclazz(obj_double_float,obj_object).
system_subclazz(obj_downcase_stream,obj_case_frob_stream).
system_subclazz(obj_echo_stream,obj_stream).
system_subclazz(obj_e_m_f_cache,obj_object).
system_subclazz(obj_end_of_file,obj_stream_error).
system_subclazz(obj_environment,obj_object).
system_subclazz(obj_fasl_readtable,obj_readtable).
system_subclazz(obj_file_error,obj_lisp_error).
system_subclazz(obj_file_stream,obj_stream).
system_subclazz(obj_fill_pointer_output_stream,obj_stream).
system_subclazz(obj_fixnum,obj_lisp_integer).
system_subclazz(obj_floating_point_inexact,obj_arithmetic_error).
system_subclazz(obj_floating_point_invalid_operation,obj_arithmetic_error).
system_subclazz(obj_floating_point_overflow,obj_arithmetic_error).
system_subclazz(obj_floating_point_underflow,obj_arithmetic_error).
system_subclazz(obj_go,obj_control_transfer).
system_subclazz(obj_illegal_monitor_state,obj_program_error).
system_subclazz(obj_jar_stream,obj_stream).
system_subclazz(obj_ffi_object,obj_object).
system_subclazz(obj_character,obj_object).
system_subclazz(obj_lisp_thread,obj_object).
system_subclazz(obj_logical_pathname,obj_pathname).
system_subclazz(obj_macro_object,obj_function).
system_subclazz(obj_nil,obj_symbol).
system_subclazz(obj_package,obj_object).
system_subclazz(obj_package_error,obj_lisp_error).
system_subclazz(obj_parse_error,obj_lisp_error).
system_subclazz(obj_random_state,obj_object).
system_subclazz(obj_ratio,obj_object).
system_subclazz(obj_reader_error,obj_stream_error).
system_subclazz(obj_return,obj_control_transfer).
system_subclazz(obj_seekable_string_writer,prolog_writer).
system_subclazz(obj_simple_bit_vector,obj_bit_vector).
system_subclazz(obj_simple_error,obj_lisp_error).
system_subclazz(obj_simple_string,obj_string).
system_subclazz(obj_simple_type_error,obj_type_error).
system_subclazz(obj_simple_warning,obj_warning).
system_subclazz(obj_single_float,obj_object).
system_subclazz(obj_slime_output_stream,obj_stream).
system_subclazz(obj_slot_definition,obj_standard_object).
system_subclazz(obj_slot_definition_class,obj_standard_class).
system_subclazz(obj_socket_stream,obj_two_way_stream).
system_subclazz(obj_string_input_stream,obj_stream).
system_subclazz(obj_string_output_stream,obj_stream).
system_subclazz(obj_style_warning,obj_warning).
system_subclazz(obj_symbol_macro,obj_object).
system_subclazz(obj_synonym_stream,obj_stream).
system_subclazz(obj_throw,obj_control_transfer).
system_subclazz(obj_unbound_slot,obj_cell_error).
system_subclazz(obj_unbound_variable,obj_cell_error).
system_subclazz(obj_undefined_function,obj_cell_error).
system_subclazz(obj_upcase_stream,obj_case_frob_stream).
system_subclazz(obj_url_stream,obj_stream).
system_subclazz(obj_wrong_number_of_arguments_exception,obj_program_error).



data_record(obj_symbol_macro,[
  m(rw,obj_object,expansion)]).

data_record(cla__non_constant_init_form,[
  m(rw,obj_object,form)]).

data_record(prolog_weak_hash_entry_weak_key_and_value,[
  m(rw,prolog_weak_reference(obj_object),key),
  m(rw,prolog_weak_reference(obj_object),value),
  m(ro,obj_weak_hash_table,this__0)]).

data_record(obj_closure_binding,[
  m(rw,obj_object,value)]).

data_record(obj_processing_terminated,[
  m(rw,integer,status)]).

data_record(prolog_repl_console,[
  m(rw,prolog_string_buffer,input_buffer),
  m(rw,prolog_reader,reader),
  m(rw,prolog_writer,writer),
  m(rw,obj_boolean,disposed),
  m(ro,prolog_thread,repl_thread),
  m(ro,obj_object,debugger_hook)]).

data_record(obj_complex_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,obj_boolean,is_displaced),
  m(rw,array_of(Kind),elements),
  m(rw,obj_array,array),
  m(rw,integer,displacement)]).
data_record(obj_complex_vector,[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,obj_boolean,is_displaced),
  m(rw,obj_list,elements),
  m(rw,obj_array,array),
  m(rw,integer,displacement)]).

data_record(cla__rest_param,[
  m(rw,obj_symbol,var),
  m(rw,obj_boolean,special)]).

data_record(obj_ffi_stack_frame,[
  m(ro,prolog_stack_trace_element,ffi_frame)]).

data_record(obj_bignum,[
  m(ro,prolog_big_integer,value)]).

data_record(obj_complex_bit_vector,[
  m(rw,integer,fill_pointer),
  m(rw,obj_boolean,is_displaced),
  m(rw,obj_array,array),
  m(rw,integer,displacement)]).

data_record(cla__slow_matcher,[
  m(ro,obj_argument_list_processor,this__0)]).

data_record(cla__aux_param,[
  m(rw,obj_symbol,var),
  m(rw,obj_boolean,special),
  m(rw,cla__init_form,initform)]).

data_record(obj_simple_array(unsigned_byte16),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,array_of(integer),data)]).

data_record(obj_package,[
  m(rw,obj_string,name),
  m(rw,obj_list,property_list),
  m(rw,obj_hash_table(obj_string,obj_symbol),internal_symbols),
  m(rw,obj_hash_table(obj_string,obj_symbol),external_symbols),
  m(rw,obj_hash_table(obj_string,obj_symbol),shadowing_symbols),
  m(rw,prolog_array_list(obj_string),nicknames),
  m(rw,prolog_array_list(obj_package),use_list),
  m(rw,prolog_array_list(obj_package),used_by_list),
  m(rw,obj_hash_table(obj_string,obj_package),local_nicknames)]).

data_record(cla__arg_list,[
  m(ro,obj_list,args),
  m(rw,integer,args_consumed),
  m(ro,integer,len),
  m(ro,obj_environment,env)]).

data_record(obj_prolog_proxy___entry,[
  m(rw,prolog_class,iface),
  m(rw,prolog_map,lisp_defined_methods)]).

data_record(obj_interpreter___unhandled_condition,[
  m(rw,obj_object,condition)]).

data_record(obj_condition,[
  m(rw,obj_string,message)]).

data_record(prolog_weak_hash_entry,[
  m(rw,obj_object,key),
  m(rw,integer,hash),
  m(rw,obj_object,value),
  m(rw,prolog_weak_hash_entry,next),
  m(rw,integer,slot),
  m(ro,obj_weak_hash_table,this__0)]).

data_record(obj_illegal_monitor_state,[
  m(rw,obj_string,message)]).

data_record(cla__required_param,[
  m(rw,obj_symbol,var),
  m(rw,obj_boolean,special)]).

data_record(obj_emf_cache,[
  m(rw,obj_hash_table(obj_emf_cache___cache_entry,obj_object),cache),
  m(rw,index_of(obj_emf_cache___eql_specialization),eql_specializations)]).

data_record(obj_random_state,[
  m(rw,prolog_random,random)]).

data_record(obj_pathname,[
  m(rw,obj_object,host),
  m(rw,obj_object,device),
  m(rw,obj_object,directory),
  m(rw,obj_object,name),
  m(rw,obj_object,type),
  m(rw,obj_object,version),
  m(rw,obj_string,namestring)]).

data_record(obj_hash_table,[
  m(ro,integer,rehash_size),
  m(ro,number,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(obj_hash_table___hash_entry),buckets),
  m(rw,integer,count),
  m(ro,obj_hash_table___comparator,comparator),
  m(ro,j_reentrant_lock,lock)]).

data_record(obj_zero_rank_array,[
  m(ro,obj_object,element_type),
  m(ro,obj_boolean,adjustable),
  m(rw,obj_object,data)]).

data_record(obj_slot_class,[
  m(rw,obj_list,direct_slot_definitions),
  m(rw,obj_list,slot_definitions),
  m(rw,obj_list,direct_default_initargs),
  m(rw,obj_list,default_initargs)]).



data_record(prolog_weak_hash_entry_weak_key_or_value,[
  m(ro,obj_weak_hash_table,this__0)]).

data_record(obj_synonym_stream,[
  m(ro,obj_symbol,stream_name)]).

data_record(cla__constant_init_form,[
  m(rw,obj_object,value)]).

data_record(obj_lisp_thread___stack_segment,[
  m(ro,array_of(prolog_object),stack),
  m(ro,obj_lisp_thread___stack_segment,next),
  m(rw,integer,stack_ptr)]).

data_record(obj_socket_stream,[
  m(ro,prolog_socket,socket)]).

data_record(obj_memory_class_loader,[
  m(ro,obj_hash_table(obj_string,obj_ffi_object),hashtable),
  m(ro,obj_ffi_object,boxed_this),
  m(ro,obj_string,internal_name_prefix)]).

data_record(obj_slime_input_stream,[
  m(rw,obj_string,s),
  m(rw,integer,length),
  m(ro,obj_function,f),
  m(ro,obj_stream,ostream)]).

data_record(obj_slime_output_stream,[
  m(ro,prolog_string_writer,string_writer),
  m(ro,obj_function,f)]).

data_record(obj_nil_vector,[
  m(rw,integer,capacity)]).

data_record(obj_string_input_stream,[
  m(ro,prolog_string_reader,string_reader),
  m(ro,integer,start),
  m(ro,obj_string,sub_string)]).

data_record(obj_byte_array_output_stream,[
  m(ro,prolog_byte_array_output_stream,byte_array_output_stream)]).

data_record(obj_stream_error,[
  m(ro,prolog_throwable,cause)]).

data_record(cla__fast_matcher,[
  m(ro,obj_argument_list_processor,this__0)]).

data_record(obj_go,[
  m(ro,obj_object,tagbody),
  m(ro,obj_object,tag)]).

data_record(obj_broadcast_stream,[
  m(ro,array_of(obj_stream),streams)]).

data_record(obj_standard_object,[
  m(rw,obj_layout,layout),
  m(rw,obj_list,slots)]).

data_record(obj_macro_object,[
  m(ro,obj_object,name),
  m(ro,obj_object,expander)]).

data_record(obj_complex_string,[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,obj_boolean,is_displaced),
  m(rw,array_of(char_code),chars),
  m(rw,obj_array,array),
  m(rw,integer,displacement)]).

data_record(obj_function,[
  m(rw,obj_list,property_list),
  m(rw,integer,call_count),
  m(rw,integer,hot_count),
  m(ro,obj_object,loaded_from)]).

data_record(ffi_script_engine,[
  m(rw,obj_interpreter,interpreter),
  m(rw,obj_function,eval_script),
  m(rw,obj_function,eval_function),
  m(rw,obj_function,compile_script),
  m(rw,obj_function,eval_compiled_script)]).

data_record(ffi_script_engine___abobj_compiled_script,[
  m(rw,obj_object,function),
  m(ro,ffi_script_engine,this__0)]).

data_record(obj_return,[
  m(ro,obj_object,tag),
  m(ro,obj_object,block),
  m(ro,obj_object,result)]).

data_record(obj_string_output_stream,[
  m(ro,obj_seekable_string_writer,string_writer)]).

data_record(obj_character,[
  m(ro,char_code,value),
  m(rw,obj_string,name)]).

data_record(obj_weak_reference,[
  m(rw,prolog_weak_reference(obj_object),ref)]).

data_record(obj_emf_cache___eql_specialization,[
  m(rw,obj_object,eql_to)]).

data_record(obj_simple_array(Kind),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,array_of(Kind),data)]).

data_record(obj_string_functions___string_indices_and_chars,[
  m(rw,obj_string,string1),
  m(rw,obj_boolean,convert_case),
  m(rw,array_of(char_code),array1),
  m(rw,array_of(char_code),array2),
  m(rw,integer,start1),
  m(rw,integer,end1),
  m(rw,integer,start2),
  m(rw,integer,end2)]).

data_record(obj_symbol,[
  m(ro,obj_simple_string,name),
  m(rw,integer,hash),
  m(rw,integer,special_index),
  m(rw,obj_object,pkg),
  m(rw,obj_object,value),
  m(rw,obj_object,function),
  m(rw,obj_list,property_list),
  m(rw,bitmask,flags)]).

data_record(obj_prolog_handler___entry,[
  m(rw,obj_function,handler),
  m(rw,obj_object,data),
  m(rw,integer,count),
  m(rw,prolog_map(obj_string,obj_prolog_handler___entry),entry_table),
  m(rw,obj_string,event)]).

data_record(obj_operator,[
  m(rw,obj_object,lambda_name),
  m(rw,obj_object,lambda_list)]).

data_record(obj_function_binding,[
  m(rw,obj_object,name),
  m(rw,obj_object,value),
  m(ro,obj_function_binding,next)]).

data_record(obj_capitalize_stream,[
  m(rw,obj_boolean,in_word)]).

data_record(obj_special_binding,[
  m(ro,integer,idx),
  m(rw,obj_object,value)]).

data_record(obj_basic_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,array_of(Kind),elements)]).
data_record(obj_simple_vector,[
  m(rw,integer,capacity),
  m(rw,obj_list,data)]).

data_record(cla__optional_param,[
  m(rw,obj_symbol,var),
  m(rw,obj_boolean,special),
  m(rw,obj_symbol,supplied_var),
  m(rw,obj_boolean,supplied_special),
  m(rw,cla__init_form,init_form)]).

data_record(obj_structure_object,[
  m(ro,obj_structure_class,structure_class),
  m(ro,obj_list,slots)]).

data_record(obj_interpreter,[
  m(ro,obj_boolean,jlisp),
  m(ro,prolog_input_stream,input_stream),
  m(ro,prolog_output_stream,output_stream)]).

data_record(obj_capitalize_first_stream,[
  m(rw,obj_boolean,virgin)]).

data_record(obj_ffi_exception,[
  m(ro,prolog_throwable,throwable)]).

data_record(obj_file_stream,[
  m(ro,obj_random_access_character_file,racf),
  m(ro,obj_pathname,pathname),
  m(ro,integer,bytes_per_unit)]).

data_record(obj_seekable_string_writer,[
  m(ro,prolog_string_buffer,string_buffer),
  m(rw,integer,offset)]).

data_record(obj_bit_vector,[
  m(rw,integer,capacity),
  m(rw,array_of(long),bits)]).

data_record(obj_ffi_object__2,[
  m(ro,obj_list,val__acc),
  m(ro,obj_object,val__fn)]).

data_record(obj_shell_command___reader_thread,[
  m(rw,array_of(char_code),buf),
  m(ro,prolog_input_stream,input_stream),
  m(ro,prolog_buffered_reader,reader),                                  
  m(ro,obj_shell_command,this__0),
  m(rw,obj_boolean,done)]).

data_record(obj_double_float,[
  m(ro,double,value)]).

data_record(obj_simple_string,[
  m(rw,integer,capacity),
  m(rw,array_of(char_code),chars)]).

data_record(obj_concatenated_stream,[
  m(rw,obj_object,streams)]).

data_record(obj_simple_array(unsigned_byte32),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,obj_list,data)]).

data_record(cla__keyword_param,[
  m(rw,obj_symbol,keyword)]).

data_record(obj_weak_hash_table,[
  m(ro,obj_object,rehash_size),
  m(ro,obj_object,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(prolog_weak_hash_entry),buckets),
  m(rw,integer,count),
  m(ro,obj_weak_hash_table___comparator,comparator),
  m(ro,j_reentrant_lock,lock),
  m(rw,prolog_weak_hash_entry,bucket_type),
  m(ro,obj_object,weakness),
  m(rw,prolog_reference_queue(obj_object),queue),
  m(rw,prolog_map(prolog_reference,prolog_weak_hash_entry),entry_lookup)]).

data_record(obj_case_frob_stream,[
  m(ro,obj_stream,target)]).

data_record(obj_complex,[
  m(ro,obj_object,realpart),
  m(ro,obj_object,imagpart)]).

data_record(obj_fill_pointer_output_stream___writer,[
  m(ro,obj_fill_pointer_output_stream,this__0)]).

data_record(obj_jar_stream,[
  m(ro,obj_pathname,pathname),
  m(ro,prolog_input_stream,input),
  m(ro,prolog_reader,reader),
  m(ro,integer,bytes_per_unit)]).

data_record(obj_char_hash_map(T),[
  m(ro,array_of(T),constants_by_char_code),
  m(ro,T,null_value),
  m(ro,obj_hash_table(prolog_character,T),backing)]).

data_record(obj_throw,[
  m(ro,obj_object,tag),
  m(ro,obj_object,result),
  m(ro,obj_list,values)]).

data_record(obj_simple_array__t,[
  m(ro,array_of(integer),dimv),
  m(ro,obj_object,element_type),
  m(ro,integer,total_size),
  m(ro,obj_list,data)]).

data_record(obj_primitives__pf_finalize__1,[
  m(rw,prolog_thread,thread),
  m(ro,obj_object,val__fun),
  m(ro,obj_primitives__pf_finalize,this__0)]).

data_record(cla__environment_param,[
  m(rw,obj_symbol,var),
  m(rw,obj_boolean,special)]).

data_record(obj_ffi_object,[
  m(ro,prolog_object,obj),
  m(ro,prolog_class,intended_class)]).

data_record(obj_funcallable_standard_object,[
  m(rw,obj_object,function),
  m(rw,obj_emf_cache,cache),
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

data_record(obj_url_stream,[
  m(ro,obj_pathname,pathname),
  m(ro,prolog_input_stream,input),
  m(ro,prolog_reader,reader),
  m(ro,integer,bytes_per_unit)]).

data_record(obj_lisp_stack_frame,[
  m(ro,obj_object,operator),
  m(ro,obj_list,args)]).

data_record(obj_echo_stream,[
  m(ro,obj_stream,in),
  m(ro,obj_stream,out),
  m(rw,integer,unread_char)]).

data_record(obj_readtable,[
  m(ro,obj_char_hash_map(prolog_byte),syntax),
  m(ro,obj_char_hash_map(obj_object),reader_macro_functions),
  m(ro,obj_char_hash_map(obj_readtable___dispatch_table),dispatch_tables),
  m(rw,obj_object,readtable_case)]).

data_record(obj_fill_pointer_output_stream,[
  m(rw,obj_complex_string,string_buffer)]).

data_record(prolog_weak_hash_entry_weak_value,[
  m(rw,prolog_weak_reference(obj_object),value),
  m(ro,obj_weak_hash_table,this__0)]).

data_record(obj_prolog_proxy_lisp_invocation_handler,[
  m(rw,obj_function,function)]).

data_record(obj_runtime_class,[
  m(rw,prolog_map(obj_string,obj_function),methods)]).

data_record(obj_lisp_thread,[
  m(rw,obj_object,thread_value),
  m(ro,prolog_thread,ffi_thread),
  m(rw,obj_boolean,destroyed),
  m(ro,obj_object,name),
  m(rw,obj_list,thread_values),
  m(rw,obj_boolean,thread_interrupted),
  m(rw,obj_object,pending),
  m(rw,obj_symbol,wrapper),
  m(rw,array_of(obj_special_binding),specials),
  m(rw,obj_special_bindings_mark,saved_specials),
  m(rw,obj_object,catch_tags),
  m(rw,obj_lisp_thread___stack_segment,top_stack_segment),
  m(rw,array_of(prolog_object),stack),
  m(rw,integer,stack_ptr),
  m(rw,obj_lisp_thread___stack_segment,spare_stack_segment)]).

data_record(obj_byte_array_input_stream,[
  m(ro,prolog_byte_array_input_stream,byte_array_input_stream)]).

data_record(obj_zip_cache___entry,[
  m(rw,long,last_modified),
  m(rw,j_zip_file,file)]).

data_record(clzip___directories,[
  m(rw,j_zip_output_stream,out)]).

data_record(obj_wrong_number_of_arguments_exception,[
  m(rw,obj_operator,operator),
  m(rw,integer,expected_min_args),
  m(rw,integer,expected_max_args),
  m(rw,obj_object,actual_args),
  m(rw,obj_string,message)]).

data_record(prolog_weak_hash_entry_weak_key,[
  m(rw,prolog_weak_reference(obj_object),key),
  m(ro,obj_weak_hash_table,this__0)]).

data_record(obj_emf_cache___cache_entry,[
  m(ro,obj_list,array)]).

data_record(obj_special_bindings_mark,[
  m(rw,integer,idx),
  m(rw,obj_special_binding,binding),
  m(rw,obj_special_bindings_mark,next)]).

data_record(obj_readtable___dispatch_table,[
  m(ro,obj_char_hash_map(obj_object),functions)]).

data_record(obj_ffi_class_loader_pf_get_default_classloader,[
  m(ro,obj_object,default_class_loader)]).

data_record(obj_argument_list_processor,[
  m(rw,array_of(cla__param),required_parameters),
  m(rw,array_of(cla__param),optional_parameters),
  m(rw,array_of(cla__keyword_param),keyword_parameters),
  m(rw,array_of(cla__param),aux_vars),
  m(rw,array_of(cla__param),positional_parameters),
  m(rw,obj_symbol,rest_var),
  m(rw,cla__param,rest_param),
  m(rw,obj_symbol,env_var),
  m(rw,cla__param,env_param),
  m(rw,integer,arity),
  m(rw,integer,min_args),
  m(rw,integer,max_args),
  m(rw,array_of(obj_symbol),variables),
  m(rw,array_of(obj_boolean),specials),
  m(rw,obj_boolean,and_key),
  m(rw,obj_boolean,allow_other_keys),
  m(ro,cla__argument_matcher,matcher),
  m(rw,obj_boolean,matcher_needs_env),
  m(rw,obj_operator,function)]).

data_record(obj_fasl_class_loader,[
  m(ro,obj_string,base_name),
  m(ro,obj_ffi_object,boxed_this)]).

data_record(obj_closure,[
  m(ro,obj_object,body),
  m(ro,obj_object,execution_body),
  m(ro,obj_environment,environment),
  m(ro,array_of(obj_symbol),free_specials),
  m(ro,obj_argument_list_processor,arglist)]).

data_record(obj_racf_unmappable_character_exception,[
  m(ro,integer,position),
  m(ro,char_code,character_value),
  m(ro,obj_string,charset_name)]).

data_record(obj_finalizer___finalizing_weak_reference,[
  m(rw,prolog_linked_list(prolog_runnable),finalizers)]).

data_record(obj_random_access_writer,[
  m(ro,obj_random_access_character_file,this__0)]).

data_record(obj_random_access_reader,[
  m(rw,array_of(char_code),read_buf),
  m(ro,obj_random_access_character_file,this__0)]).

data_record(obj_racf_malformed_input_exception,[
  m(ro,integer,position),
  m(ro,char_code,character),
  m(ro,obj_string,charset_name)]).

data_record(obj_random_access_output_stream,[
  m(rw,array_of(unsigned_byte8),write_buf),
  m(ro,obj_random_access_character_file,this__0)]).

data_record(obj_decoding_reader,[
  m(rw,prolog_byte_buffer,bbuf),
  m(rw,prolog_pushback_input_stream,stream),
  m(rw,prolog_charset_decoder,cd),
  m(rw,prolog_charset_encoder,ce)]).

data_record(obj_random_access_character_file,[
  m(rw,obj_random_access_writer,writer),
  m(rw,obj_random_access_reader,reader),
  m(rw,obj_random_access_input_stream,input_stream),
  m(rw,obj_random_access_output_stream,output_stream),
  m(rw,prolog_file_channel,fcn),
  m(rw,prolog_charset,cset),
  m(rw,prolog_charset_encoder,cenc),
  m(rw,prolog_charset_decoder,cdec),
  m(rw,prolog_byte_buffer,bbuf),
  m(rw,obj_boolean,bbuf_is_dirty),
  m(rw,obj_boolean,bbuf_is_readable),
  m(rw,long,bbufpos),
  m(rw,prolog_char_buffer,single_char_buf),
  m(rw,prolog_byte_buffer,short_byte_buf)]).

data_record(obj_random_access_input_stream,[
  m(rw,array_of(unsigned_byte8),read_buf),
  m(ro,obj_random_access_character_file,this__0)]).


data_record(obj_ffi_object__1,[
  m(ro,obj_list,val__acc),
  m(ro,obj_ffi_object,this__0)]).

data_record(obj_stack_frame,[
  m(rw,obj_stack_frame,next),
  m(rw,obj_environment,env)]).

data_record(obj_single_float,[
  m(ro,float,value)]).

data_record(obj_compiled_closure,[
  m(rw,array_of(obj_closure_binding),ctx)]).

data_record(obj_special_operator,[
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

data_record(obj_prolog_proxy___lisp_handler,[
  m(rw,prolog_map,table)]).

data_record(obj_autoload,[
  m(ro,obj_string,file_name),
  m(ro,obj_string,class_name),
  m(ro,obj_symbol,function_symbol)]).

data_record(obj_char_hash_map__1,[
  m(ro,prolog_iterator(prolog_character),car_it),
  m(rw,integer,char_num),
  m(ro,obj_char_hash_map,this__0)]).

data_record(obj_layout,[
  m(ro,obj_object,lisp_class),
  m(ro,obj_hash_table(obj_object,obj_object),slot_table),
  m(ro,obj_list,slot_names),
  m(ro,obj_object,shared_slots),
  m(rw,obj_boolean,invalid)]).

data_record(obj_binding,[
  m(ro,obj_object,bound_symbol),
  m(rw,obj_environment,env),
  m(rw,obj_object,value),
  m(rw,obj_boolean,specialp),
  m(ro,obj_binding,next)]).

data_record(obj_fixnum,[
  m(ro,integer,value)]).

data_record(obj_ratio,[
  m(rw,prolog_big_integer,numerator),
  m(rw,prolog_big_integer,denominator)]).

data_record(obj_shell_command,[
  m(rw,prolog_thread,thread),
  m(ro,obj_string,command),
  m(ro,obj_string,directory),
  m(ro,obj_stream,output_stream),
  m(ro,prolog_string_buffer,output),
  m(rw,integer,exit_value)]).

data_record(obj_stream,[
  m(rw,obj_object,element_type),
  m(rw,obj_boolean,is_input_stream),
  m(rw,obj_boolean,is_output_stream),
  m(rw,obj_boolean,is_character_stream),
  m(rw,obj_boolean,is_binary_stream),
  m(rw,obj_boolean,past_end),
  m(rw,obj_boolean,interactive),
  m(rw,obj_boolean,open),
  m(rw,prolog_pushback_reader,reader),
  m(rw,integer,offset),
  m(rw,integer,line_number),
  m(rw,prolog_writer,writer),
  m(rw,integer,char_pos),
  m(rw,obj_stream___eol_style,eol_style),
  m(rw,char_code,eol_char),
  m(rw,obj_object,external_format),
  m(rw,obj_string,encoding),
  m(rw,char_code,last_char),
  m(rw,prolog_input_stream,in),
  m(rw,prolog_output_stream,out)]).

data_record(obj_environment,[
  m(rw,obj_binding,vars),
  m(rw,obj_function_binding,last_function_binding),
  m(rw,obj_binding,blocks),
  m(rw,obj_binding,tags),
  m(rw,obj_boolean,inactive)]).

data_record(obj_hash_table___hash_entry,[
  m(rw,obj_object,key),
  m(rw,integer,hash),
  m(rw,obj_object,value),
  m(rw,obj_hash_table___hash_entry,next)]).

data_record(obj_lisp_thread___stack_marker,[
  m(ro,integer,num_args)]).


data_record(obj_complex_array(Kind),[
  m(ro,array_of(integer),dimv),
  m(rw,integer,total_size),
  m(rw,array_of(Kind),data),
  m(rw,obj_array,array),
  m(rw,integer,displacement)]).

data_record(obj_complex_array,[
  m(ro,array_of(integer),dimv),
  m(ro,obj_object,element_type),
  m(rw,integer,total_size),
  m(rw,obj_list,data),
  m(rw,obj_array,array),
  m(rw,integer,displacement)]).

data_record(obj_profiler__1__1,[
  m(rw,prolog_thread,thread),
  m(ro,obj_lisp_thread,val__thread),
  m(ro,obj_profiler__1,this__0)]).

data_record(obj_cons,[
  m(rw,obj_object,car),
  m(rw,obj_object,cdr)]).


data_record(obj_lisp_class,[
  m(ro,integer,sxhash),
  m(rw,obj_object,name),
  m(rw,obj_list,property_list),
  m(rw,obj_layout,class_layout),
  m(rw,obj_list,direct_superclasses),
  m(rw,obj_list,direct_subclasses),
  m(rw,obj_list,class_precedence_list),
  m(rw,obj_list,direct_methods),
  m(rw,obj_list,documentation),
  m(rw,obj_boolean,finalized)]).

data_record(obj_autoload_generalized_reference,[
  m(rw,obj_symbol,indicator)]).

data_record(obj_two_way_stream,[
  m(ro,obj_stream,in),
  m(ro,obj_stream,out)]).



obj_object_to_OBJECT(Atom,OUT):-atom(Atom),atom_concat('obj_',_,Atom),upcase_atom(Atom,UPPER),UPPER\==Atom,
  atomic_list_concat(List,'_',UPPER),atomic_list_concat(List,'-',OUT),!.
obj_OBJECT_to_object(Atom,OUT):-
  atom(Atom),downcase_atom(Atom,DOWN),DOWN\==Atom,
  atomic_list_concat(List,'-',DOWN),atomic_list_concat(List,'_',CL), atom_concat('obj_',CL,OUT),!.

% system_subclazz(C1,C2)==>(recognised_clazz(C1),recognised_clazz(C2)).  
recognised_clazz(AA):- data_record(C1,Lst),(AA=C1-> true ; (member(m(_,C2,_),Lst),C2==AA)).
recognised_clazz(AA):- system_subclazz(C1,C2),(AA=C1;AA=C2).
maybe_xform_recognised_clazz([A|B],AA):-is_list(B),!,maplist(maybe_xform_recognised_clazz,[A|B],AA).
maybe_xform_recognised_clazz(A,AA):- (obj_OBJECT_to_object(A,AA),recognised_clazz(AA))->true;A=AA.

/*
term_expansion(mop_direct(A,P,B),mop_direct(AA,P,BB)):- 
  maybe_xform_recognised_clazz(A,AA),
  (szlot\=P -> maybe_xform_recognised_clazz(B,BB) ; B=BB),
  ((A\==AA) ; (B\==BB)).
*/

:- fixup_exports.



