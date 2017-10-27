
% :- use_module(library(pfc)).

:- dynamic(system_subclazz/2).


system_subclazz(cl_simple_array__t,cl_array).
system_subclazz(cl_simple_array(unsigned_byte16),cl_array).
system_subclazz(cl_simple_array(unsigned_byte32),cl_array).
system_subclazz(cl_simple_array(unsigned_byte8),cl_array).
system_subclazz(cl_simple_array(_Unsigned_byte8),cl_array).
system_subclazz(cl_complex_array,cl_array).
system_subclazz(cl_complex_array(unsigned_byte32),cl_array).
system_subclazz(cl_complex_array(unsigned_byte8),cl_array).
system_subclazz(cl_complex_array(_Unsigned_byte8),cl_array).
system_subclazz(cl_complex_bit_vector,cl_bit_vector).
system_subclazz(cl_complex_string,cl_string).
system_subclazz(cl_complex_vector,cl_vector).
system_subclazz(cl_complex_vector(unsigned_byte32),cl_vector).
system_subclazz(cl_complex_vector(unsigned_byte8),cl_vector).
system_subclazz(cl_complex_vector(_Unsigned_byte8),cl_vector).
system_subclazz(cl_zero_rank_array,cl_array).
system_subclazz(cl_nil_vector,cl_string).
system_subclazz(cl_string,cl_vector).
system_subclazz(cl_simple_vector,cl_vector).
system_subclazz(cl_basic_vector(unsigned_byte16),cl_vector).
system_subclazz(cl_basic_vector(unsigned_byte32),cl_vector).
system_subclazz(cl_basic_vector(unsigned_byte8),cl_vector).
system_subclazz(cl_basic_vector(_Unsigned_byte8),cl_vector).
system_subclazz(cl_bit_vector,cl_vector).
system_subclazz(cl_vector,cl_array).
system_subclazz(cl_array,cl_object).
system_subclazz(cl_case_frob_stream,cl_stream).
system_subclazz(cl_control_transfer,prolog_runtime_exception).
system_subclazz(cl_dispatch_macro_function,cl_function).
system_subclazz(cl_function,cl_operator).
system_subclazz(cl_lisp_class,cl_standard_object).
system_subclazz(cl_operator,cl_object).
system_subclazz(cl_reader_macro_function,cl_function).
system_subclazz(cl_stack_frame,cl_object).
system_subclazz(cl_arithmetic_error,cl_lisp_error).
system_subclazz(cl_autoload,cl_function).
system_subclazz(cl_built_in_class,cl_lisp_class).
system_subclazz(cl_cell_error,cl_lisp_error).
system_subclazz(cl_closure,cl_function).
system_subclazz(cl_compiled_closure,cl_closure).
system_subclazz(cl_compiled_primitive,cl_primitive).
system_subclazz(cl_condition,cl_standard_object).
system_subclazz(cl_fasl_class_loader,cl_ffi_class_loader).
system_subclazz(cl_funcallable_standard_class,cl_standard_class).
system_subclazz(cl_funcallable_standard_object,cl_standard_object).
system_subclazz(cl_hash_table,cl_object).
system_subclazz(cl_integrity_error,prolog_error).
system_subclazz(cl_ffi_class_loader,prolog_url_class_loader).
system_subclazz(cl_ffi_exception,cl_lisp_error).
system_subclazz(cl_ffi_stack_frame,cl_stack_frame).
system_subclazz(cl_layout,cl_object).
system_subclazz(cl_lisp_error,cl_serious_condition).
system_subclazz(cl_lisp_integer,cl_object).
system_subclazz(cl_lisp_stack_frame,cl_stack_frame).
system_subclazz(cl_memory_class_loader,cl_ffi_class_loader).
system_subclazz(cl_pathname,cl_object).
system_subclazz(cl_primitive,cl_function).
system_subclazz(cl_print_not_readable,cl_lisp_error).
system_subclazz(cl_processing_terminated,prolog_error).
system_subclazz(cl_program_error,cl_lisp_error).
system_subclazz(cl_readtable,cl_object).
system_subclazz(cl_serious_condition,cl_condition).
system_subclazz(cl_simple_condition,cl_condition).
system_subclazz(cl_slime_input_stream,cl_stream).
system_subclazz(cl_slot_class,cl_lisp_class).
system_subclazz(cl_special_operator,cl_operator).
system_subclazz(cl_standard_class,cl_slot_class).
system_subclazz(cl_standard_object,cl_object).
system_subclazz(cl_storage_condition,cl_serious_condition).
system_subclazz(cl_stream,cl_structure_object).
system_subclazz(cl_stream_error,cl_lisp_error).
system_subclazz(cl_structure_class,cl_slot_class).
system_subclazz(cl_structure_object,cl_object).
system_subclazz(cl_symbol,cl_object).
system_subclazz(cl_thread_destroyed,prolog_error).
system_subclazz(cl_two_way_stream,cl_stream).
system_subclazz(cl_type_error,cl_lisp_error).
system_subclazz(cl_decoding_reader,prolog_pushback_reader).
system_subclazz(cl_racf_malformed_input_exception ,prolog_malformed_input_exception).
system_subclazz(cl_racf_unmappable_character_exception ,prolog_unmappable_character_exception).
system_subclazz(cl_warning,cl_condition).
system_subclazz(cl_weak_hash_table,cl_object).
system_subclazz(cl_weak_reference,cl_object).
system_subclazz(cl_autoload_generalized_reference,cl_autoload).
system_subclazz(cl_autoload_macro,cl_autoload).
system_subclazz(cl_bignum,cl_lisp_integer).
system_subclazz(cl_broadcast_stream,cl_stream).
system_subclazz(cl_byte_array_input_stream,cl_stream).
system_subclazz(cl_byte_array_output_stream,cl_stream).
system_subclazz(cl_capitalize_first_stream,cl_case_frob_stream).
system_subclazz(cl_capitalize_stream,cl_case_frob_stream).
system_subclazz(cl_complex,cl_object).
system_subclazz(cl_concatenated_stream,cl_stream).
system_subclazz(cl_cons,cl_object).
system_subclazz(cl_control_error,cl_lisp_error).
system_subclazz(cl_division_by_zero,cl_arithmetic_error).
system_subclazz(cldolist,cl_special_operator).
system_subclazz(cldotimes,cl_special_operator).
system_subclazz(cl_double_float,cl_object).
system_subclazz(cl_downcase_stream,cl_case_frob_stream).
system_subclazz(cl_echo_stream,cl_stream).
system_subclazz(cl_e_m_f_cache,cl_object).
system_subclazz(cl_end_of_file,cl_stream_error).
system_subclazz(cl_environment,cl_object).
system_subclazz(cl_fasl_readtable,cl_readtable).
system_subclazz(cl_file_error,cl_lisp_error).
system_subclazz(cl_file_stream,cl_stream).
system_subclazz(cl_fill_pointer_output_stream,cl_stream).
system_subclazz(cl_fixnum,cl_lisp_integer).
system_subclazz(cl_floating_point_inexact,cl_arithmetic_error).
system_subclazz(cl_floating_point_invalid_operation,cl_arithmetic_error).
system_subclazz(cl_floating_point_overflow,cl_arithmetic_error).
system_subclazz(cl_floating_point_underflow,cl_arithmetic_error).
system_subclazz(cl_go,cl_control_transfer).
system_subclazz(cl_illegal_monitor_state,cl_program_error).
system_subclazz(cl_jar_stream,cl_stream).
system_subclazz(cl_ffi_object,cl_object).
system_subclazz(cl_character,cl_object).
system_subclazz(cl_lisp_thread,cl_object).
system_subclazz(cl_logical_pathname,cl_pathname).
system_subclazz(cl_macro_object,cl_function).
system_subclazz(cl_nil,cl_symbol).
system_subclazz(cl_package,cl_object).
system_subclazz(cl_package_error,cl_lisp_error).
system_subclazz(cl_parse_error,cl_lisp_error).
system_subclazz(cl_random_state,cl_object).
system_subclazz(cl_ratio,cl_object).
system_subclazz(cl_reader_error,cl_stream_error).
system_subclazz(cl_return,cl_control_transfer).
system_subclazz(cl_seekable_string_writer,prolog_writer).
system_subclazz(cl_simple_bit_vector,cl_bit_vector).
system_subclazz(cl_simple_error,cl_lisp_error).
system_subclazz(cl_simple_string,cl_string).
system_subclazz(cl_simple_type_error,cl_type_error).
system_subclazz(cl_simple_warning,cl_warning).
system_subclazz(cl_single_float,cl_object).
system_subclazz(cl_slime_output_stream,cl_stream).
system_subclazz(cl_slot_definition,cl_standard_object).
system_subclazz(cl_slot_definition_class,cl_standard_class).
system_subclazz(cl_socket_stream,cl_two_way_stream).
system_subclazz(cl_string_input_stream,cl_stream).
system_subclazz(cl_string_output_stream,cl_stream).
system_subclazz(cl_style_warning,cl_warning).
system_subclazz(cl_symbol_macro,cl_object).
system_subclazz(cl_synonym_stream,cl_stream).
system_subclazz(cl_throw,cl_control_transfer).
system_subclazz(cl_unbound_slot,cl_cell_error).
system_subclazz(cl_unbound_variable,cl_cell_error).
system_subclazz(cl_undefined_function,cl_cell_error).
system_subclazz(cl_upcase_stream,cl_case_frob_stream).
system_subclazz(cl_url_stream,cl_stream).
system_subclazz(cl_wrong_number_of_arguments_exception,cl_program_error).



data_record(cl_symbol_macro,[
  m(rw,cl_object,expansion)]).

data_record(cla__non_constant_init_form,[
  m(rw,cl_object,form)]).

data_record(prolog_weak_hash_entry_weak_key_and_value,[
  m(rw,prolog_weak_reference(cl_object),key),
  m(rw,prolog_weak_reference(cl_object),value),
  m(ro,cl_weak_hash_table,this__0)]).

data_record(cl_closure_binding,[
  m(rw,cl_object,value)]).

data_record(cl_processing_terminated,[
  m(rw,integer,status)]).

data_record(prolog_repl_console,[
  m(rw,prolog_string_buffer,input_buffer),
  m(rw,prolog_reader,reader),
  m(rw,prolog_writer,writer),
  m(rw,cl_boolean,disposed),
  m(ro,prolog_thread,repl_thread),
  m(ro,cl_object,debugger_hook)]).

data_record(cl_complex_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,cl_boolean,is_displaced),
  m(rw,array_of(Kind),elements),
  m(rw,cl_array,array),
  m(rw,integer,displacement)]).
data_record(cl_complex_vector,[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,cl_boolean,is_displaced),
  m(rw,cl_list,elements),
  m(rw,cl_array,array),
  m(rw,integer,displacement)]).

data_record(cla__rest_param,[
  m(rw,cl_symbol,var),
  m(rw,cl_boolean,special)]).

data_record(cl_ffi_stack_frame,[
  m(ro,prolog_stack_trace_element,ffi_frame)]).

data_record(cl_bignum,[
  m(ro,prolog_big_integer,value)]).

data_record(cl_complex_bit_vector,[
  m(rw,integer,fill_pointer),
  m(rw,cl_boolean,is_displaced),
  m(rw,cl_array,array),
  m(rw,integer,displacement)]).

data_record(cla__slow_matcher,[
  m(ro,cl_argument_list_processor,this__0)]).

data_record(cla__aux_param,[
  m(rw,cl_symbol,var),
  m(rw,cl_boolean,special),
  m(rw,cla__init_form,initform)]).

data_record(cl_simple_array(unsigned_byte16),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,array_of(integer),data)]).

data_record(cl_package,[
  m(rw,cl_string,name),
  m(rw,cl_list,property_list),
  m(rw,cl_hash_table(cl_string,cl_symbol),internal_symbols),
  m(rw,cl_hash_table(cl_string,cl_symbol),external_symbols),
  m(rw,cl_hash_table(cl_string,cl_symbol),shadowing_symbols),
  m(rw,prolog_array_list(cl_string),nicknames),
  m(rw,prolog_array_list(cl_package),use_list),
  m(rw,prolog_array_list(cl_package),used_by_list),
  m(rw,cl_hash_table(cl_string,cl_package),local_nicknames)]).

data_record(cla__arg_list,[
  m(ro,cl_list,args),
  m(rw,integer,args_consumed),
  m(ro,integer,len),
  m(ro,cl_environment,env)]).

data_record(cl_prolog_proxy___entry,[
  m(rw,prolog_class,iface),
  m(rw,prolog_map,lisp_defined_methods)]).

data_record(cl_interpreter___unhandled_condition,[
  m(rw,cl_object,condition)]).

data_record(cl_condition,[
  m(rw,cl_string,message)]).

data_record(prolog_weak_hash_entry,[
  m(rw,cl_object,key),
  m(rw,integer,hash),
  m(rw,cl_object,value),
  m(rw,prolog_weak_hash_entry,next),
  m(rw,integer,slot),
  m(ro,cl_weak_hash_table,this__0)]).

data_record(cl_illegal_monitor_state,[
  m(rw,cl_string,message)]).

data_record(cla__required_param,[
  m(rw,cl_symbol,var),
  m(rw,cl_boolean,special)]).

data_record(cl_emf_cache,[
  m(rw,cl_hash_table(cl_emf_cache___cache_entry,cl_object),cache),
  m(rw,index_of(cl_emf_cache___eql_specialization),eql_specializations)]).

data_record(cl_random_state,[
  m(rw,prolog_random,random)]).

data_record(cl_pathname,[
  m(rw,cl_object,host),
  m(rw,cl_object,device),
  m(rw,cl_object,directory),
  m(rw,cl_object,name),
  m(rw,cl_object,type),
  m(rw,cl_object,version),
  m(rw,cl_string,namestring)]).

data_record(cl_hash_table,[
  m(ro,integer,rehash_size),
  m(ro,number,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(cl_hash_table___hash_entry),buckets),
  m(rw,integer,count),
  m(ro,cl_hash_table___comparator,comparator),
  m(ro,j_reentrant_lock,lock)]).

data_record(cl_zero_rank_array,[
  m(ro,cl_object,element_type),
  m(ro,cl_boolean,adjustable),
  m(rw,cl_object,data)]).

data_record(cl_slot_class,[
  m(rw,cl_list,direct_slot_definitions),
  m(rw,cl_list,slot_definitions),
  m(rw,cl_list,direct_default_initargs),
  m(rw,cl_list,default_initargs)]).



data_record(prolog_weak_hash_entry_weak_key_or_value,[
  m(ro,cl_weak_hash_table,this__0)]).

data_record(cl_synonym_stream,[
  m(ro,cl_symbol,stream_name)]).

data_record(cla__constant_init_form,[
  m(rw,cl_object,value)]).

data_record(cl_lisp_thread___stack_segment,[
  m(ro,array_of(prolog_object),stack),
  m(ro,cl_lisp_thread___stack_segment,next),
  m(rw,integer,stack_ptr)]).

data_record(cl_socket_stream,[
  m(ro,prolog_socket,socket)]).

data_record(cl_memory_class_loader,[
  m(ro,cl_hash_table(cl_string,cl_ffi_object),hashtable),
  m(ro,cl_ffi_object,boxed_this),
  m(ro,cl_string,internal_name_prefix)]).

data_record(cl_slime_input_stream,[
  m(rw,cl_string,s),
  m(rw,integer,length),
  m(ro,cl_function,f),
  m(ro,cl_stream,ostream)]).

data_record(cl_slime_output_stream,[
  m(ro,prolog_string_writer,string_writer),
  m(ro,cl_function,f)]).

data_record(cl_nil_vector,[
  m(rw,integer,capacity)]).

data_record(cl_string_input_stream,[
  m(ro,prolog_string_reader,string_reader),
  m(ro,integer,start),
  m(ro,cl_string,sub_string)]).

data_record(cl_byte_array_output_stream,[
  m(ro,prolog_byte_array_output_stream,byte_array_output_stream)]).

data_record(cl_stream_error,[
  m(ro,prolog_throwable,cause)]).

data_record(cla__fast_matcher,[
  m(ro,cl_argument_list_processor,this__0)]).

data_record(cl_go,[
  m(ro,cl_object,tagbody),
  m(ro,cl_object,tag)]).

data_record(cl_broadcast_stream,[
  m(ro,array_of(cl_stream),streams)]).

data_record(cl_standard_object,[
  m(rw,cl_layout,layout),
  m(rw,cl_list,slots)]).

data_record(cl_macro_object,[
  m(ro,cl_object,name),
  m(ro,cl_object,expander)]).

data_record(cl_complex_string,[
  m(rw,integer,capacity),
  m(rw,integer,fill_pointer),
  m(rw,cl_boolean,is_displaced),
  m(rw,array_of(char_code),chars),
  m(rw,cl_array,array),
  m(rw,integer,displacement)]).

data_record(cl_function,[
  m(rw,cl_list,property_list),
  m(rw,integer,call_count),
  m(rw,integer,hot_count),
  m(ro,cl_object,loaded_from)]).

data_record(ffi_script_engine,[
  m(rw,cl_interpreter,interpreter),
  m(rw,cl_function,eval_script),
  m(rw,cl_function,eval_function),
  m(rw,cl_function,compile_script),
  m(rw,cl_function,eval_compiled_script)]).

data_record(ffi_script_engine___abcl_compiled_script,[
  m(rw,cl_object,function),
  m(ro,ffi_script_engine,this__0)]).

data_record(cl_return,[
  m(ro,cl_object,tag),
  m(ro,cl_object,block),
  m(ro,cl_object,result)]).

data_record(cl_string_output_stream,[
  m(ro,cl_seekable_string_writer,string_writer)]).

data_record(cl_character,[
  m(ro,char_code,value),
  m(rw,cl_string,name)]).

data_record(cl_weak_reference,[
  m(rw,prolog_weak_reference(cl_object),ref)]).

data_record(cl_emf_cache___eql_specialization,[
  m(rw,cl_object,eql_to)]).

data_record(cl_simple_array(Kind),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,array_of(Kind),data)]).

data_record(cl_string_functions___string_indices_and_chars,[
  m(rw,cl_string,string1),
  m(rw,cl_boolean,convert_case),
  m(rw,array_of(char_code),array1),
  m(rw,array_of(char_code),array2),
  m(rw,integer,start1),
  m(rw,integer,end1),
  m(rw,integer,start2),
  m(rw,integer,end2)]).

data_record(cl_symbol,[
  m(ro,cl_simple_string,name),
  m(rw,integer,hash),
  m(rw,integer,special_index),
  m(rw,cl_object,pkg),
  m(rw,cl_object,value),
  m(rw,cl_object,function),
  m(rw,cl_list,property_list),
  m(rw,bitmask,flags)]).

data_record(cl_prolog_handler___entry,[
  m(rw,cl_function,handler),
  m(rw,cl_object,data),
  m(rw,integer,count),
  m(rw,prolog_map(cl_string,cl_prolog_handler___entry),entry_table),
  m(rw,cl_string,event)]).

data_record(cl_operator,[
  m(rw,cl_object,lambda_name),
  m(rw,cl_object,lambda_list)]).

data_record(cl_function_binding,[
  m(rw,cl_object,name),
  m(rw,cl_object,value),
  m(ro,cl_function_binding,next)]).

data_record(cl_capitalize_stream,[
  m(rw,cl_boolean,in_word)]).

data_record(cl_special_binding,[
  m(ro,integer,idx),
  m(rw,cl_object,value)]).

data_record(cl_basic_vector(Kind),[
  m(rw,integer,capacity),
  m(rw,array_of(Kind),elements)]).
data_record(cl_simple_vector,[
  m(rw,integer,capacity),
  m(rw,cl_list,data)]).

data_record(cla__optional_param,[
  m(rw,cl_symbol,var),
  m(rw,cl_boolean,special),
  m(rw,cl_symbol,supplied_var),
  m(rw,cl_boolean,supplied_special),
  m(rw,cla__init_form,init_form)]).

data_record(cl_structure_object,[
  m(ro,cl_structure_class,structure_class),
  m(ro,cl_list,slots)]).

data_record(cl_interpreter,[
  m(ro,cl_boolean,jlisp),
  m(ro,prolog_input_stream,input_stream),
  m(ro,prolog_output_stream,output_stream)]).

data_record(cl_capitalize_first_stream,[
  m(rw,cl_boolean,virgin)]).

data_record(cl_ffi_exception,[
  m(ro,prolog_throwable,throwable)]).

data_record(cl_file_stream,[
  m(ro,cl_random_access_character_file,racf),
  m(ro,cl_pathname,pathname),
  m(ro,integer,bytes_per_unit)]).

data_record(cl_seekable_string_writer,[
  m(ro,prolog_string_buffer,string_buffer),
  m(rw,integer,offset)]).

data_record(cl_bit_vector,[
  m(rw,integer,capacity),
  m(rw,array_of(long),bits)]).

data_record(cl_ffi_object__2,[
  m(ro,cl_list,val__acc),
  m(ro,cl_object,val__fn)]).

data_record(cl_shell_command___reader_thread,[
  m(rw,array_of(char_code),buf),
  m(ro,prolog_input_stream,input_stream),
  m(ro,prolog_buffered_reader,reader),                                  
  m(ro,cl_shell_command,this__0),
  m(rw,cl_boolean,done)]).

data_record(cl_double_float,[
  m(ro,double,value)]).

data_record(cl_simple_string,[
  m(rw,integer,capacity),
  m(rw,array_of(char_code),chars)]).

data_record(cl_concatenated_stream,[
  m(rw,cl_object,streams)]).

data_record(cl_simple_array(unsigned_byte32),[
  m(ro,array_of(integer),dimv),
  m(ro,integer,total_size),
  m(ro,cl_list,data)]).

data_record(cla__keyword_param,[
  m(rw,cl_symbol,keyword)]).

data_record(cl_weak_hash_table,[
  m(ro,cl_object,rehash_size),
  m(ro,cl_object,rehash_threshold),
  m(rw,integer,threshold),
  m(rw,array_of(prolog_weak_hash_entry),buckets),
  m(rw,integer,count),
  m(ro,cl_weak_hash_table___comparator,comparator),
  m(ro,j_reentrant_lock,lock),
  m(rw,prolog_weak_hash_entry,bucket_type),
  m(ro,cl_object,weakness),
  m(rw,prolog_reference_queue(cl_object),queue),
  m(rw,prolog_map(prolog_reference,prolog_weak_hash_entry),entry_lookup)]).

data_record(cl_case_frob_stream,[
  m(ro,cl_stream,target)]).

data_record(cl_complex,[
  m(ro,cl_object,realpart),
  m(ro,cl_object,imagpart)]).

data_record(cl_fill_pointer_output_stream___writer,[
  m(ro,cl_fill_pointer_output_stream,this__0)]).

data_record(cl_jar_stream,[
  m(ro,cl_pathname,pathname),
  m(ro,prolog_input_stream,input),
  m(ro,prolog_reader,reader),
  m(ro,integer,bytes_per_unit)]).

data_record(cl_char_hash_map(T),[
  m(ro,array_of(T),constants_by_char_code),
  m(ro,T,null_value),
  m(ro,cl_hash_table(prolog_character,T),backing)]).

data_record(cl_throw,[
  m(ro,cl_object,tag),
  m(ro,cl_object,result),
  m(ro,cl_list,values)]).

data_record(cl_simple_array__t,[
  m(ro,array_of(integer),dimv),
  m(ro,cl_object,element_type),
  m(ro,integer,total_size),
  m(ro,cl_list,data)]).

data_record(cl_primitives__pf_finalize__1,[
  m(rw,prolog_thread,thread),
  m(ro,cl_object,val__fun),
  m(ro,cl_primitives__pf_finalize,this__0)]).

data_record(cla__environment_param,[
  m(rw,cl_symbol,var),
  m(rw,cl_boolean,special)]).

data_record(cl_ffi_object,[
  m(ro,prolog_object,obj),
  m(ro,prolog_class,intended_class)]).

data_record(cl_funcallable_standard_object,[
  m(rw,cl_object,function),
  m(rw,cl_emf_cache,cache),
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

data_record(cl_url_stream,[
  m(ro,cl_pathname,pathname),
  m(ro,prolog_input_stream,input),
  m(ro,prolog_reader,reader),
  m(ro,integer,bytes_per_unit)]).

data_record(cl_lisp_stack_frame,[
  m(ro,cl_object,operator),
  m(ro,cl_list,args)]).

data_record(cl_echo_stream,[
  m(ro,cl_stream,in),
  m(ro,cl_stream,out),
  m(rw,integer,unread_char)]).

data_record(cl_readtable,[
  m(ro,cl_char_hash_map(prolog_byte),syntax),
  m(ro,cl_char_hash_map(cl_object),reader_macro_functions),
  m(ro,cl_char_hash_map(cl_readtable___dispatch_table),dispatch_tables),
  m(rw,cl_object,readtable_case)]).

data_record(cl_fill_pointer_output_stream,[
  m(rw,cl_complex_string,string_buffer)]).

data_record(prolog_weak_hash_entry_weak_value,[
  m(rw,prolog_weak_reference(cl_object),value),
  m(ro,cl_weak_hash_table,this__0)]).

data_record(cl_prolog_proxy_lisp_invocation_handler,[
  m(rw,cl_function,function)]).

data_record(cl_runtime_class,[
  m(rw,prolog_map(cl_string,cl_function),methods)]).

data_record(cl_lisp_thread,[
  m(rw,cl_object,thread_value),
  m(ro,prolog_thread,ffi_thread),
  m(rw,cl_boolean,destroyed),
  m(ro,cl_object,name),
  m(rw,cl_list,thread_values),
  m(rw,cl_boolean,thread_interrupted),
  m(rw,cl_object,pending),
  m(rw,cl_symbol,wrapper),
  m(rw,array_of(cl_special_binding),specials),
  m(rw,cl_special_bindings_mark,saved_specials),
  m(rw,cl_object,catch_tags),
  m(rw,cl_lisp_thread___stack_segment,top_stack_segment),
  m(rw,array_of(prolog_object),stack),
  m(rw,integer,stack_ptr),
  m(rw,cl_lisp_thread___stack_segment,spare_stack_segment)]).

data_record(cl_byte_array_input_stream,[
  m(ro,prolog_byte_array_input_stream,byte_array_input_stream)]).

data_record(cl_zip_cache___entry,[
  m(rw,long,last_modified),
  m(rw,j_zip_file,file)]).

data_record(clzip___directories,[
  m(rw,j_zip_output_stream,out)]).

data_record(cl_wrong_number_of_arguments_exception,[
  m(rw,cl_operator,operator),
  m(rw,integer,expected_min_args),
  m(rw,integer,expected_max_args),
  m(rw,cl_object,actual_args),
  m(rw,cl_string,message)]).

data_record(prolog_weak_hash_entry_weak_key,[
  m(rw,prolog_weak_reference(cl_object),key),
  m(ro,cl_weak_hash_table,this__0)]).

data_record(cl_emf_cache___cache_entry,[
  m(ro,cl_list,array)]).

data_record(cl_special_bindings_mark,[
  m(rw,integer,idx),
  m(rw,cl_special_binding,binding),
  m(rw,cl_special_bindings_mark,next)]).

data_record(cl_readtable___dispatch_table,[
  m(ro,cl_char_hash_map(cl_object),functions)]).

data_record(cl_ffi_class_loader_pf_get_default_classloader,[
  m(ro,cl_object,default_class_loader)]).

data_record(cl_argument_list_processor,[
  m(rw,array_of(cla__param),required_parameters),
  m(rw,array_of(cla__param),optional_parameters),
  m(rw,array_of(cla__keyword_param),keyword_parameters),
  m(rw,array_of(cla__param),aux_vars),
  m(rw,array_of(cla__param),positional_parameters),
  m(rw,cl_symbol,rest_var),
  m(rw,cla__param,rest_param),
  m(rw,cl_symbol,env_var),
  m(rw,cla__param,env_param),
  m(rw,integer,arity),
  m(rw,integer,min_args),
  m(rw,integer,max_args),
  m(rw,array_of(cl_symbol),variables),
  m(rw,array_of(cl_boolean),specials),
  m(rw,cl_boolean,and_key),
  m(rw,cl_boolean,allow_other_keys),
  m(ro,cla__argument_matcher,matcher),
  m(rw,cl_boolean,matcher_needs_env),
  m(rw,cl_operator,function)]).

data_record(cl_fasl_class_loader,[
  m(ro,cl_string,base_name),
  m(ro,cl_ffi_object,boxed_this)]).

data_record(cl_closure,[
  m(ro,cl_object,body),
  m(ro,cl_object,execution_body),
  m(ro,cl_environment,environment),
  m(ro,array_of(cl_symbol),free_specials),
  m(ro,cl_argument_list_processor,arglist)]).

data_record(cl_racf_unmappable_character_exception,[
  m(ro,integer,position),
  m(ro,char_code,character_value),
  m(ro,cl_string,charset_name)]).

data_record(cl_finalizer___finalizing_weak_reference,[
  m(rw,prolog_linked_list(prolog_runnable),finalizers)]).

data_record(cl_random_access_writer,[
  m(ro,cl_random_access_character_file,this__0)]).

data_record(cl_random_access_reader,[
  m(rw,array_of(char_code),read_buf),
  m(ro,cl_random_access_character_file,this__0)]).

data_record(cl_racf_malformed_input_exception,[
  m(ro,integer,position),
  m(ro,char_code,character),
  m(ro,cl_string,charset_name)]).

data_record(cl_random_access_output_stream,[
  m(rw,array_of(unsigned_byte8),write_buf),
  m(ro,cl_random_access_character_file,this__0)]).

data_record(cl_decoding_reader,[
  m(rw,prolog_byte_buffer,bbuf),
  m(rw,prolog_pushback_input_stream,stream),
  m(rw,prolog_charset_decoder,cd),
  m(rw,prolog_charset_encoder,ce)]).

data_record(cl_random_access_character_file,[
  m(rw,cl_random_access_writer,writer),
  m(rw,cl_random_access_reader,reader),
  m(rw,cl_random_access_input_stream,input_stream),
  m(rw,cl_random_access_output_stream,output_stream),
  m(rw,prolog_file_channel,fcn),
  m(rw,prolog_charset,cset),
  m(rw,prolog_charset_encoder,cenc),
  m(rw,prolog_charset_decoder,cdec),
  m(rw,prolog_byte_buffer,bbuf),
  m(rw,cl_boolean,bbuf_is_dirty),
  m(rw,cl_boolean,bbuf_is_readable),
  m(rw,long,bbufpos),
  m(rw,prolog_char_buffer,single_char_buf),
  m(rw,prolog_byte_buffer,short_byte_buf)]).

data_record(cl_random_access_input_stream,[
  m(rw,array_of(unsigned_byte8),read_buf),
  m(ro,cl_random_access_character_file,this__0)]).


data_record(cl_ffi_object__1,[
  m(ro,cl_list,val__acc),
  m(ro,cl_ffi_object,this__0)]).

data_record(cl_stack_frame,[
  m(rw,cl_stack_frame,next),
  m(rw,cl_environment,env)]).

data_record(cl_single_float,[
  m(ro,float,value)]).

data_record(cl_compiled_closure,[
  m(rw,array_of(cl_closure_binding),ctx)]).

data_record(cl_special_operator,[
  m(rw,integer,call_count),
  m(rw,integer,hot_count)]).

data_record(cl_prolog_proxy___lisp_handler,[
  m(rw,prolog_map,table)]).

data_record(cl_autoload,[
  m(ro,cl_string,file_name),
  m(ro,cl_string,class_name),
  m(ro,cl_symbol,function_symbol)]).

data_record(cl_char_hash_map__1,[
  m(ro,prolog_iterator(prolog_character),car_it),
  m(rw,integer,char_num),
  m(ro,cl_char_hash_map,this__0)]).

data_record(cl_layout,[
  m(ro,cl_object,lisp_class),
  m(ro,cl_hash_table(cl_object,cl_object),slot_table),
  m(ro,cl_list,slot_names),
  m(ro,cl_object,shared_slots),
  m(rw,cl_boolean,invalid)]).

data_record(cl_binding,[
  m(ro,cl_object,bound_symbol),
  m(rw,cl_environment,env),
  m(rw,cl_object,value),
  m(rw,cl_boolean,specialp),
  m(ro,cl_binding,next)]).

data_record(cl_fixnum,[
  m(ro,integer,value)]).

data_record(cl_ratio,[
  m(rw,prolog_big_integer,numerator),
  m(rw,prolog_big_integer,denominator)]).

data_record(cl_shell_command,[
  m(rw,prolog_thread,thread),
  m(ro,cl_string,command),
  m(ro,cl_string,directory),
  m(ro,cl_stream,output_stream),
  m(ro,prolog_string_buffer,output),
  m(rw,integer,exit_value)]).

data_record(cl_stream,[
  m(rw,cl_object,element_type),
  m(rw,cl_boolean,is_input_stream),
  m(rw,cl_boolean,is_output_stream),
  m(rw,cl_boolean,is_character_stream),
  m(rw,cl_boolean,is_binary_stream),
  m(rw,cl_boolean,past_end),
  m(rw,cl_boolean,interactive),
  m(rw,cl_boolean,open),
  m(rw,prolog_pushback_reader,reader),
  m(rw,integer,offset),
  m(rw,integer,line_number),
  m(rw,prolog_writer,writer),
  m(rw,integer,char_pos),
  m(rw,cl_stream___eol_style,eol_style),
  m(rw,char_code,eol_char),
  m(rw,cl_object,external_format),
  m(rw,cl_string,encoding),
  m(rw,char_code,last_char),
  m(rw,prolog_input_stream,in),
  m(rw,prolog_output_stream,out)]).

data_record(cl_environment,[
  m(rw,cl_binding,vars),
  m(rw,cl_function_binding,last_function_binding),
  m(rw,cl_binding,blocks),
  m(rw,cl_binding,tags),
  m(rw,cl_boolean,inactive)]).

data_record(cl_hash_table___hash_entry,[
  m(rw,cl_object,key),
  m(rw,integer,hash),
  m(rw,cl_object,value),
  m(rw,cl_hash_table___hash_entry,next)]).

data_record(cl_lisp_thread___stack_marker,[
  m(ro,integer,num_args)]).


data_record(cl_complex_array(Kind),[
  m(ro,array_of(integer),dimv),
  m(rw,integer,total_size),
  m(rw,array_of(Kind),data),
  m(rw,cl_array,array),
  m(rw,integer,displacement)]).

data_record(cl_complex_array,[
  m(ro,array_of(integer),dimv),
  m(ro,cl_object,element_type),
  m(rw,integer,total_size),
  m(rw,cl_list,data),
  m(rw,cl_array,array),
  m(rw,integer,displacement)]).

data_record(cl_profiler__1__1,[
  m(rw,prolog_thread,thread),
  m(ro,cl_lisp_thread,val__thread),
  m(ro,cl_profiler__1,this__0)]).

data_record(cl_cons,[
  m(rw,cl_object,car),
  m(rw,cl_object,cdr)]).


data_record(cl_lisp_class,[
  m(ro,integer,sxhash),
  m(rw,cl_object,name),
  m(rw,cl_list,property_list),
  m(rw,cl_layout,class_layout),
  m(rw,cl_list,direct_superclasses),
  m(rw,cl_list,direct_subclasses),
  m(rw,cl_list,class_precedence_list),
  m(rw,cl_list,direct_methods),
  m(rw,cl_list,documentation),
  m(rw,cl_boolean,finalized)]).

data_record(cl_autoload_generalized_reference,[
  m(rw,cl_symbol,indicator)]).

data_record(cl_two_way_stream,[
  m(ro,cl_stream,in),
  m(ro,cl_stream,out)]).



cl_object_to_OBJECT(Atom,OUT):-atom(Atom),atom_concat('cl_',_,Atom),upcase_atom(Atom,UPPER),UPPER\==Atom,
  atomic_list_concat(List,'_',UPPER),atomic_list_concat(List,'-',OUT),!.
cl_OBJECT_to_object(Atom,OUT):-
  atom(Atom),downcase_atom(Atom,DOWN),DOWN\==Atom,
  atomic_list_concat(List,'-',DOWN),atomic_list_concat(List,'_',CL), atom_concat('cl_',CL,OUT),!.

% system_subclazz(C1,C2)==>(recognised_clazz(C1),recognised_clazz(C2)).  
recognised_clazz(AA):- data_record(C1,Lst),(AA=C1-> true ; (member(m(_,C2,_),Lst),C2==AA)).
recognised_clazz(AA):- system_subclazz(C1,C2),(AA=C1;AA=C2).
maybe_xform_recognised_clazz([A|B],AA):-is_list(B),!,maplist(maybe_xform_recognised_clazz,[A|B],AA).
maybe_xform_recognised_clazz(A,AA):- (cl_OBJECT_to_object(A,AA),recognised_clazz(AA))->true;A=AA.

/*
term_expansion(mop_direct(A,P,B),mop_direct(AA,P,BB)):- 
  maybe_xform_recognised_clazz(A,AA),
  (szlot\=P -> maybe_xform_recognised_clazz(B,BB) ; B=BB),
  ((A\==AA) ; (B\==BB)).
*/

mop_direct('%METHOD-FUNCTION', supers, [t]).
mop_direct('%METHOD-FUNCTION', szlot, 'FAST-FUNCTION').
mop_direct('%METHOD-FUNCTION', szlot, 'NAME').
mop_direct('ABORT-FAILURE', supers, [cl_control_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ABSTRACT-LEXENV', submop, 'LEXENV').
mop_direct('ABSTRACT-LEXENV', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ACCESSOR-DFUN-INFO', submop, 'N-N').
mop_direct('ACCESSOR-DFUN-INFO', submop, 'ONE-INDEX-DFUN-INFO').
mop_direct('ACCESSOR-DFUN-INFO', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ACCESSOR-DFUN-INFO', szlot, 'ACCESSOR-TYPE').
mop_direct('ACCESSOR-METHOD', submop, 'GLOBAL-BOUNDP-METHOD').
mop_direct('ACCESSOR-METHOD', submop, 'GLOBAL-READER-METHOD').
mop_direct('ACCESSOR-METHOD', submop, 'GLOBAL-WRITER-METHOD').
mop_direct('ACCESSOR-METHOD', submop, 'STANDARD-ACCESSOR-METHOD').
mop_direct('ACCESSOR-METHOD', supers, ['STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('ACCESSOR-METHOD', szlot, 'SLOT-NAME').
mop_direct('ALIEN-ALIEN-VALUE-TYPE', submop, 'ALIEN-MEM-BLOCK-TYPE').
mop_direct('ALIEN-ALIEN-VALUE-TYPE', submop, 'ALIEN-POINTER-TYPE').
mop_direct('ALIEN-ALIEN-VALUE-TYPE', supers, ['ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-ALIEN-VALUE-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-ARRAY-TYPE', supers, ['ALIEN-MEM-BLOCK-TYPE', 'ALIEN-ALIEN-VALUE-TYPE', 'ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-ARRAY-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-ARRAY-TYPE', szlot, 'DIMENSIONS').
mop_direct('ALIEN-ARRAY-TYPE', szlot, 'ELEMENT-TYPE').
mop_direct('ALIEN-BOOLEAN-TYPE', supers, ['ALIEN-INTEGER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-BOOLEAN-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-C-STRING-TYPE', supers, ['ALIEN-POINTER-TYPE', 'ALIEN-ALIEN-VALUE-TYPE', 'ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-C-STRING-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-C-STRING-TYPE', szlot, 'ELEMENT-TYPE').
mop_direct('ALIEN-C-STRING-TYPE', szlot, 'EXTERNAL-FORMAT').
mop_direct('ALIEN-C-STRING-TYPE', szlot, 'NOT-NULL').
mop_direct('ALIEN-DOUBLE-FLOAT-TYPE', supers, ['ALIEN-FLOAT-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-DOUBLE-FLOAT-TYPE', szlot, 'BITS').
mop_direct('ALIEN-DOUBLE-FLOAT-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-ENUM-TYPE', supers, ['ALIEN-INTEGER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-ENUM-TYPE', szlot, 'BITS').
mop_direct('ALIEN-ENUM-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-ENUM-TYPE', szlot, 'FROM').
mop_direct('ALIEN-ENUM-TYPE', szlot, 'KIND').
mop_direct('ALIEN-ENUM-TYPE', szlot, 'NAME').
mop_direct('ALIEN-ENUM-TYPE', szlot, 'OFFSET').
mop_direct('ALIEN-ENUM-TYPE', szlot, 'TO').
mop_direct('ALIEN-FLOAT-TYPE', submop, 'ALIEN-DOUBLE-FLOAT-TYPE').
mop_direct('ALIEN-FLOAT-TYPE', submop, 'ALIEN-SINGLE-FLOAT-TYPE').
mop_direct('ALIEN-FLOAT-TYPE', supers, ['ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-FLOAT-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-FLOAT-TYPE', szlot, 'TYPE').
mop_direct('ALIEN-FUN-TYPE', supers, ['ALIEN-MEM-BLOCK-TYPE', 'ALIEN-ALIEN-VALUE-TYPE', 'ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-FUN-TYPE', szlot, 'ARG-TYPES').
mop_direct('ALIEN-FUN-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-FUN-TYPE', szlot, 'CONVENTION').
mop_direct('ALIEN-FUN-TYPE', szlot, 'RESULT-TYPE').
mop_direct('ALIEN-FUN-TYPE', szlot, 'STUB').
mop_direct('ALIEN-INTEGER-TYPE', submop, 'ALIEN-BOOLEAN-TYPE').
mop_direct('ALIEN-INTEGER-TYPE', submop, 'ALIEN-ENUM-TYPE').
mop_direct('ALIEN-INTEGER-TYPE', supers, ['ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-INTEGER-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-INTEGER-TYPE', szlot, 'SIGNED').
mop_direct('ALIEN-MEM-BLOCK-TYPE', submop, 'ALIEN-ARRAY-TYPE').
mop_direct('ALIEN-MEM-BLOCK-TYPE', submop, 'ALIEN-FUN-TYPE').
mop_direct('ALIEN-MEM-BLOCK-TYPE', submop, 'ALIEN-RECORD-TYPE').
mop_direct('ALIEN-MEM-BLOCK-TYPE', supers, ['ALIEN-ALIEN-VALUE-TYPE', 'ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-MEM-BLOCK-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-POINTER-TYPE', submop, 'ALIEN-C-STRING-TYPE').
mop_direct('ALIEN-POINTER-TYPE', supers, ['ALIEN-ALIEN-VALUE-TYPE', 'ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-POINTER-TYPE', szlot, 'BITS').
mop_direct('ALIEN-POINTER-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-POINTER-TYPE', szlot, 'TO').
mop_direct('ALIEN-RECORD-FIELD', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-RECORD-FIELD', szlot, 'BITS').
mop_direct('ALIEN-RECORD-FIELD', szlot, 'NAME').
mop_direct('ALIEN-RECORD-FIELD', szlot, 'OFFSET').
mop_direct('ALIEN-RECORD-FIELD', szlot, 'TYPE').
mop_direct('ALIEN-RECORD-TYPE', supers, ['ALIEN-MEM-BLOCK-TYPE', 'ALIEN-ALIEN-VALUE-TYPE', 'ALIEN-SYSTEM-AREA-POINTER-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-RECORD-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-RECORD-TYPE', szlot, 'FIELDS').
mop_direct('ALIEN-RECORD-TYPE', szlot, 'KIND').
mop_direct('ALIEN-RECORD-TYPE', szlot, 'NAME').
mop_direct('ALIEN-SINGLE-FLOAT-TYPE', supers, ['ALIEN-FLOAT-TYPE', 'ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-SINGLE-FLOAT-TYPE', szlot, 'BITS').
mop_direct('ALIEN-SINGLE-FLOAT-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-STACK-EXHAUSTED', supers, [cl_storage_condition, cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-SYSTEM-AREA-POINTER-TYPE', submop, 'ALIEN-ALIEN-VALUE-TYPE').
mop_direct('ALIEN-SYSTEM-AREA-POINTER-TYPE', supers, ['ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-SYSTEM-AREA-POINTER-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-TYPE', submop, 'ALIEN-FLOAT-TYPE').
mop_direct('ALIEN-TYPE', submop, 'ALIEN-INTEGER-TYPE').
mop_direct('ALIEN-TYPE', submop, 'ALIEN-SYSTEM-AREA-POINTER-TYPE').
mop_direct('ALIEN-TYPE', submop, 'ALIEN-VALUES-TYPE').
mop_direct('ALIEN-TYPE', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-TYPE', szlot, 'ALIGNMENT').
mop_direct('ALIEN-TYPE', szlot, 'BITS').
mop_direct('ALIEN-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-TYPE-CLASS', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-TYPE-CLASS', szlot, 'ALIEN-REP').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'ARG-TN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'DEFSTRUCT-NAME').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'DEPORT-ALLOC-GEN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'DEPORT-GEN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'DEPORT-PIN-P').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'DEPOSIT-GEN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'EXTRACT-GEN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'INCLUDE').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'LISP-REP').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'NAME').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'NATURALIZE-GEN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'RESULT-TN').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'SUBTYPEP').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'TYPE=').
mop_direct('ALIEN-TYPE-CLASS', szlot, 'UNPARSE').
mop_direct('ALIEN-TYPE-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-TYPE-TYPE', szlot, 'ALIEN-TYPE').
mop_direct('ALIEN-TYPE-TYPE', szlot, 'CLASS-INFO').
mop_direct('ALIEN-VALUE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-VALUE', szlot, 'SAP').
mop_direct('ALIEN-VALUE', szlot, 'TYPE').
mop_direct('ALIEN-VALUES-TYPE', supers, ['ALIEN-TYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIEN-VALUES-TYPE', szlot, 'CLASS').
mop_direct('ALIEN-VALUES-TYPE', szlot, 'VALUES').
mop_direct('ALIGNMENT-NOTE', supers, ['ANNOTATION', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ALIGNMENT-NOTE', szlot, 'BITS').
mop_direct('ALIGNMENT-NOTE', szlot, 'PATTERN').
mop_direct('ALIGNMENT-NOTE', szlot, 'SIZE').
mop_direct('AMBIGUOUS-VAR-NAME', supers, ['DEBUG-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('AMBIGUOUS-VAR-NAME', szlot, 'FRAME').
mop_direct('AMBIGUOUS-VAR-NAME', szlot, 'NAME').
mop_direct('ANNOTATION', submop, 'ALIGNMENT-NOTE').
mop_direct('ANNOTATION', submop, 'BACK-PATCH').
mop_direct('ANNOTATION', submop, 'CHOOSER').
mop_direct('ANNOTATION', submop, 'FILLER').
mop_direct('ANNOTATION', submop, 'LABEL').
mop_direct('ANNOTATION', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ANNOTATION', szlot, 'INDEX').
mop_direct('ANNOTATION', szlot, 'POSN').
mop_direct('ANODE', supers, [cl_structure_object, t]).
mop_direct('ANODE', szlot, 'CODE').
mop_direct('ANODE', szlot, 'SECLASS').
mop_direct('ANODE', szlot, 'TYPE').
mop_direct('ANSI-STREAM', submop, cl_broadcast_stream).
mop_direct('ANSI-STREAM', submop, cl_case_frob_stream).
mop_direct('ANSI-STREAM', submop, cl_concatenated_stream).
mop_direct('ANSI-STREAM', submop, 'FD-STREAM').
mop_direct('ANSI-STREAM', submop, cl_fill_pointer_output_stream).
mop_direct('ANSI-STREAM', submop, 'PRETTY-STREAM').
mop_direct('ANSI-STREAM', submop, cl_string_input_stream).
mop_direct('ANSI-STREAM', submop, cl_string_output_stream).
mop_direct('ANSI-STREAM', submop, cl_synonym_stream).
mop_direct('ANSI-STREAM', submop, cl_two_way_stream).
mop_direct('ANSI-STREAM', supers, [cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ANSI-STREAM', szlot, 'BIN').
mop_direct('ANSI-STREAM', szlot, 'BOUT').
mop_direct('ANSI-STREAM', szlot, 'CIN-BUFFER').
mop_direct('ANSI-STREAM', szlot, 'IN').
mop_direct('ANSI-STREAM', szlot, 'IN-BUFFER').
mop_direct('ANSI-STREAM', szlot, 'IN-INDEX').
mop_direct('ANSI-STREAM', szlot, 'INPUT-CHAR-POS').
mop_direct('ANSI-STREAM', szlot, 'MISC').
mop_direct('ANSI-STREAM', szlot, 'N-BIN').
mop_direct('ANSI-STREAM', szlot, 'OUT').
mop_direct('ANSI-STREAM', szlot, 'SOUT').
mop_direct('APPROXIMATE-FUN-TYPE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('APPROXIMATE-FUN-TYPE', szlot, 'KEYS').
mop_direct('APPROXIMATE-FUN-TYPE', szlot, 'MAX-ARGS').
mop_direct('APPROXIMATE-FUN-TYPE', szlot, 'MIN-ARGS').
mop_direct('APPROXIMATE-FUN-TYPE', szlot, 'TYPES').
mop_direct('APPROXIMATE-KEY-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('APPROXIMATE-KEY-INFO', szlot, 'ALLOWP').
mop_direct('APPROXIMATE-KEY-INFO', szlot, 'NAME').
mop_direct('APPROXIMATE-KEY-INFO', szlot, 'POSITION').
mop_direct('APPROXIMATE-KEY-INFO', szlot, 'TYPES').
mop_direct('ARG', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARG', szlot, 'FIELDS').
mop_direct('ARG', szlot, 'NAME').
mop_direct('ARG', szlot, 'POSITION').
mop_direct('ARG', szlot, 'PREFILTER').
mop_direct('ARG', szlot, 'PRINTER').
mop_direct('ARG', szlot, 'SIGN-EXTEND-P').
mop_direct('ARG', szlot, 'USE-LABEL').
mop_direct('ARG', szlot, 'VALUE').
mop_direct('ARG-COUNT-ERROR', submop, 'ARG-COUNT-PROGRAM-ERROR').
mop_direct('ARG-COUNT-ERROR', supers, ['DEFMACRO-LAMBDA-LIST-BIND-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ARG-COUNT-ERROR', szlot, 'ARGS').
mop_direct('ARG-COUNT-ERROR', szlot, 'LAMBDA-LIST').
mop_direct('ARG-COUNT-ERROR', szlot, 'MAXIMUM').
mop_direct('ARG-COUNT-ERROR', szlot, 'MINIMUM').
mop_direct('ARG-COUNT-PROGRAM-ERROR', supers, ['ARG-COUNT-ERROR', 'DEFMACRO-LAMBDA-LIST-BIND-ERROR', cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ARG-FORM-KIND', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARG-FORM-KIND', szlot, 'CHECKER').
mop_direct('ARG-FORM-KIND', szlot, 'NAMES').
mop_direct('ARG-FORM-KIND', szlot, 'PRODUCER').
mop_direct('ARG-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARG-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARG-INFO', szlot, 'ARG-INFO-KEY/REST-P').
mop_direct('ARG-INFO', szlot, 'ARG-INFO-KEYS').
mop_direct('ARG-INFO', szlot, 'ARG-INFO-LAMBDA-LIST').
mop_direct('ARG-INFO', szlot, 'ARG-INFO-METATYPES').
mop_direct('ARG-INFO', szlot, 'ARG-INFO-NUMBER-OPTIONAL').
mop_direct('ARG-INFO', szlot, 'ARG-INFO-PRECEDENCE').
mop_direct('ARG-INFO', szlot, 'DEFAULT').
mop_direct('ARG-INFO', szlot, 'GF-INFO-C-A-M-EMF-STD-P').
mop_direct('ARG-INFO', szlot, 'GF-INFO-FAST-MF-P').
mop_direct('ARG-INFO', szlot, 'GF-INFO-SIMPLE-ACCESSOR-TYPE').
mop_direct('ARG-INFO', szlot, 'GF-INFO-STATIC-C-A-M-EMF').
mop_direct('ARG-INFO', szlot, 'GF-PRECOMPUTE-DFUN-AND-EMF-P').
mop_direct('ARG-INFO', szlot, 'KEY').
mop_direct('ARG-INFO', szlot, 'KIND').
mop_direct('ARG-INFO', szlot, 'SPECIALP').
mop_direct('ARG-INFO', szlot, 'SUPPLIED-P').
mop_direct('ARG-INFO', szlot, 'SUPPLIED-USED-P').
mop_direct('ARG-STATE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARG-STATE', szlot, 'REGISTER-ARGS').
mop_direct('ARG-STATE', szlot, 'STACK-FRAME-SIZE').
mop_direct('ARG-STATE', szlot, 'XMM-ARGS').
mop_direct('ARGS-TYPE', submop, 'FUN-TYPE').
mop_direct('ARGS-TYPE', submop, 'VALUES-TYPE').
mop_direct('ARGS-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARGS-TYPE', szlot, 'ALLOWP').
mop_direct('ARGS-TYPE', szlot, 'KEYP').
mop_direct('ARGS-TYPE', szlot, 'KEYWORDS').
mop_direct('ARGS-TYPE', szlot, 'OPTIONAL').
mop_direct('ARGS-TYPE', szlot, 'REQUIRED').
mop_direct('ARGS-TYPE', szlot, 'REST').
mop_direct('ARGUMENT-LIST-DOTTED', submop, 'SIMPLE-ARGUMENT-LIST-DOTTED').
mop_direct('ARGUMENT-LIST-DOTTED', supers, [cl_program_error, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('ARGUMENTS-OUT-OF-DOMAIN-ERROR', supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_arithmetic_error, submop, 'ARGUMENTS-OUT-OF-DOMAIN-ERROR').
mop_direct(cl_arithmetic_error, submop, cl_division_by_zero).
mop_direct(cl_arithmetic_error, submop, 'FLOATING-POINT-EXCEPTION').
mop_direct(cl_arithmetic_error, submop, cl_floating_point_inexact).
mop_direct(cl_arithmetic_error, submop, cl_floating_point_invalid_operation).
mop_direct(cl_arithmetic_error, submop, cl_floating_point_overflow).
mop_direct(cl_arithmetic_error, submop, cl_floating_point_underflow).
mop_direct(cl_arithmetic_error, submop, 'SIMPLE-ARITHMETIC-ERROR').
mop_direct(cl_arithmetic_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_arithmetic_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_arithmetic_error, szlot, '$OPERANDS').
mop_direct(cl_arithmetic_error, szlot, '$OPERATION').
mop_direct(cl_arithmetic_error, szlot, 'OPERANDS').
mop_direct(cl_arithmetic_error, szlot, 'OPERATION').
mop_direct(cl_array, submop, 'SIMPLE-ARRAY').
mop_direct(cl_array, submop, cl_vector).
mop_direct(cl_array, supers, [t]).
mop_direct('ARRAY-INITIAL-ELEMENT-MISMATCH', supers, ['REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ARRAY-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ARRAY-TYPE', szlot, 'CLASS-INFO').
mop_direct('ARRAY-TYPE', szlot, 'COMPLEXP').
mop_direct('ARRAY-TYPE', szlot, 'DIMENSIONS').
mop_direct('ARRAY-TYPE', szlot, 'ELEMENT-TYPE').
mop_direct('ARRAY-TYPE', szlot, 'SPECIALIZED-ELEMENT-TYPE').
mop_direct('ASTERISKS-AROUND-CONSTANT-VARIABLE-NAME', supers, ['DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME', cl_style_warning, cl_warning, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ASTERISKS-AROUND-LEXICAL-VARIABLE-NAME', supers, ['DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME', cl_style_warning, cl_warning, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('BACK-PATCH', supers, ['ANNOTATION', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BACK-PATCH', szlot, 'FUN').
mop_direct('BACK-PATCH', szlot, 'SIZE').
mop_direct('BASE-STRING', submop, 'SIMPLE-BASE-STRING').
mop_direct('BASE-STRING', supers, [cl_string, cl_vector, cl_array, 'SEQUENCE', t]).
mop_direct('BASIC-COMBINATION', submop, 'COMBINATION').
mop_direct('BASIC-COMBINATION', submop, 'MV-COMBINATION').
mop_direct('BASIC-COMBINATION', supers, ['VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BASIC-COMBINATION', szlot, 'ARGS').
mop_direct('BASIC-COMBINATION', szlot, 'FUN').
mop_direct('BASIC-COMBINATION', szlot, 'FUN-INFO').
mop_direct('BASIC-COMBINATION', szlot, 'INFO').
mop_direct('BASIC-COMBINATION', szlot, 'KIND').
mop_direct('BASIC-COMBINATION', szlot, 'STEP-INFO').
mop_direct('BASIC-COMBINATION', szlot, 'TYPE-VALIDATED-FOR-LEAF').
mop_direct('BASIC-VAR', submop, 'GLOBAL-VAR').
mop_direct('BASIC-VAR', submop, 'LAMBDA-VAR').
mop_direct('BASIC-VAR', supers, ['LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BASIC-VAR', szlot, 'SETS').
mop_direct(cl_bignum, supers, ['INTEGER', 'RATIONAL', 'REAL', 'NUMBER', t]).
mop_direct('BIND', supers, ['NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BIND', szlot, 'LAMBDA').
mop_direct('BINDING-STACK-EXHAUSTED', supers, [cl_storage_condition, cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_bit_vector, submop, cl_simple_bit_vector).
mop_direct(cl_bit_vector, supers, [cl_vector, cl_array, 'SEQUENCE', t]).
mop_direct('BLOCK-ANNOTATION', submop, 'IR2-BLOCK').
mop_direct('BLOCK-ANNOTATION', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BLOCK-ANNOTATION', szlot, 'BLOCK').
mop_direct('BLOCK-ANNOTATION', szlot, 'NEXT').
mop_direct('BLOCK-ANNOTATION', szlot, 'PREV').
mop_direct('BLOCK-END', supers, ['QUEUED-OP', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BLOCK-END', szlot, 'SUFFIX').
mop_direct('BLOCK-START', supers, ['SECTION-START', 'QUEUED-OP', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BLOCK-START', szlot, 'BLOCK-END').
mop_direct('BLOCK-START', szlot, 'PREFIX').
mop_direct('BLOCK-START', szlot, 'SUFFIX').
mop_direct('BOGUS-DEBUG-FUN', supers, ['DEBUG-FUN', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BOGUS-DEBUG-FUN', szlot, '%NAME').
mop_direct('BOOTSTRAP-PACKAGE-NOT-FOUND', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('BOOTSTRAP-PACKAGE-NOT-FOUND', szlot, 'NAME').
mop_direct('BOUNDING-INDICES-BAD-ERROR', supers, ['REFERENCE-CONDITION', cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('BOUNDING-INDICES-BAD-ERROR', szlot, 'OBJECT').
mop_direct('BREAKPOINT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BREAKPOINT', szlot, '%INFO').
mop_direct('BREAKPOINT', szlot, 'COOKIE-FUN').
mop_direct('BREAKPOINT', szlot, 'HOOK-FUN').
mop_direct('BREAKPOINT', szlot, 'INTERNAL-DATA').
mop_direct('BREAKPOINT', szlot, 'KIND').
mop_direct('BREAKPOINT', szlot, 'START-HELPER').
mop_direct('BREAKPOINT', szlot, 'STATUS').
mop_direct('BREAKPOINT', szlot, 'UNKNOWN-RETURN-PARTNER').
mop_direct('BREAKPOINT', szlot, 'WHAT').
mop_direct('BREAKPOINT-DATA', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BREAKPOINT-DATA', szlot, 'BREAKPOINTS').
mop_direct('BREAKPOINT-DATA', szlot, 'COMPONENT').
mop_direct('BREAKPOINT-DATA', szlot, 'INSTRUCTION').
mop_direct('BREAKPOINT-DATA', szlot, 'OFFSET').
mop_direct('BREAKPOINT-ERROR', supers, ['SYSTEM-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_broadcast_stream, supers, ['ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_broadcast_stream, supers, [cl_stream, t]).
mop_direct(cl_broadcast_stream, szlot, 'BOUT').
mop_direct(cl_broadcast_stream, szlot, 'MISC').
mop_direct(cl_broadcast_stream, szlot, 'OUT').
mop_direct(cl_broadcast_stream, szlot, 'SOUT').
mop_direct(cl_broadcast_stream, szlot, 'STREAMS').
mop_direct('BUFFER', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BUFFER', szlot, 'HEAD').
mop_direct('BUFFER', szlot, 'LENGTH').
mop_direct('BUFFER', szlot, 'SAP').
mop_direct('BUFFER', szlot, 'TAIL').
mop_direct('BUG', supers, [cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_built_in_class, supers, ['CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct(cl_built_in_class, supers, ['SYSTEM-CLASS', 'PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_built_in_class, szlot, '$PROTOTYPE').
mop_direct('BUILT-IN-CLASSOID', supers, ['CLASSOID', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('BUILT-IN-CLASSOID', szlot, 'TRANSLATION').
mop_direct('C-SOURCE-POINT', supers, [cl_structure_object, t]).
mop_direct('C-SOURCE-POINT', szlot, 'FILE').
mop_direct('C-SOURCE-POINT', szlot, 'LINENO1').
mop_direct('C-SOURCE-POINT', szlot, 'LINENO2').
mop_direct('C-STRING-DECODING-ERROR', supers, ['CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('C-STRING-ENCODING-ERROR', supers, ['CHARACTER-ENCODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CACHE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CACHE', szlot, 'DEPTH').
mop_direct('CACHE', szlot, 'KEY-COUNT').
mop_direct('CACHE', szlot, 'LIMIT').
mop_direct('CACHE', szlot, 'LINE-SIZE').
mop_direct('CACHE', szlot, 'MASK').
mop_direct('CACHE', szlot, 'VALUE').
mop_direct('CACHE', szlot, 'VECTOR').
mop_direct('CACHED-FUN', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CACHED-FUN', szlot, 'CONSTRAINT').
mop_direct('CACHED-FUN', szlot, 'FUNSTATE').
mop_direct('CACHED-FUN', szlot, 'NAME').
mop_direct('CACHING', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CALLBACK-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CALLBACK-INFO', szlot, 'FUNCTION').
mop_direct('CALLBACK-INFO', szlot, 'INDEX').
mop_direct('CALLBACK-INFO', szlot, 'SPECIFIER').
mop_direct('CALLBACK-INFO', szlot, 'WRAPPER').
mop_direct('CASE-FAILURE', supers, [cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CASE-FAILURE', szlot, 'NAME').
mop_direct('CASE-FAILURE', szlot, 'POSSIBILITIES').
mop_direct(cl_case_frob_stream, supers, ['ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_case_frob_stream, szlot, 'MISC').
mop_direct(cl_case_frob_stream, szlot, 'TARGET').
mop_direct('CAST', supers, ['VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CAST', szlot, '%TYPE-CHECK').
mop_direct('CAST', szlot, 'ASSERTED-TYPE').
mop_direct('CAST', szlot, 'TYPE-TO-CHECK').
mop_direct('CAST', szlot, 'VALUE').
mop_direct('CAST', szlot, 'VESTIGIAL-EXIT-ENTRY-LEXENV').
mop_direct('CAST', szlot, 'VESTIGIAL-EXIT-LEXENV').
mop_direct('CBLOCK', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CBLOCK', szlot, 'COMPONENT').
mop_direct('CBLOCK', szlot, 'DOMINATORS').
mop_direct('CBLOCK', szlot, 'FLAG').
mop_direct('CBLOCK', szlot, 'FLAGS').
mop_direct('CBLOCK', szlot, 'GEN').
mop_direct('CBLOCK', szlot, 'IN').
mop_direct('CBLOCK', szlot, 'INFO').
mop_direct('CBLOCK', szlot, 'KILL').
mop_direct('CBLOCK', szlot, 'LAST').
mop_direct('CBLOCK', szlot, 'LOOP').
mop_direct('CBLOCK', szlot, 'LOOP-NEXT').
mop_direct('CBLOCK', szlot, 'NEXT').
mop_direct('CBLOCK', szlot, 'OUT').
mop_direct('CBLOCK', szlot, 'PHYSENV-CACHE').
mop_direct('CBLOCK', szlot, 'PRED').
mop_direct('CBLOCK', szlot, 'PREV').
mop_direct('CBLOCK', szlot, 'START').
mop_direct('CBLOCK', szlot, 'SUCC').
mop_direct('CBLOCK', szlot, 'XREFS').
mop_direct(cl_cell_error, submop, 'SIMPLE-CELL-ERROR').
mop_direct(cl_cell_error, submop, 'SYMBOL-VALUE-IN-THREAD-ERROR').
mop_direct(cl_cell_error, submop, cl_unbound_slot).
mop_direct(cl_cell_error, submop, cl_unbound_variable).
mop_direct(cl_cell_error, submop, 'UNDEFINED-ALIEN-ERROR').
mop_direct(cl_cell_error, submop, cl_undefined_function).
mop_direct(cl_cell_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_cell_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_cell_error, szlot, '$NAME').
mop_direct(cl_cell_error, szlot, 'NAME').
mop_direct(cl_character, supers, [t]).
mop_direct('CHARACTER-CODING-ERROR', submop, 'CHARACTER-DECODING-ERROR').
mop_direct('CHARACTER-CODING-ERROR', submop, 'CHARACTER-ENCODING-ERROR').
mop_direct('CHARACTER-CODING-ERROR', supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-CODING-ERROR', szlot, 'EXTERNAL-FORMAT').
mop_direct('CHARACTER-DECODING-ERROR', submop, 'C-STRING-DECODING-ERROR').
mop_direct('CHARACTER-DECODING-ERROR', submop, 'OCTET-DECODING-ERROR').
mop_direct('CHARACTER-DECODING-ERROR', submop, 'STREAM-DECODING-ERROR').
mop_direct('CHARACTER-DECODING-ERROR', supers, ['CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-DECODING-ERROR', szlot, 'OCTETS').
mop_direct('CHARACTER-DECODING-ERROR-IN-COMMENT', submop, 'CHARACTER-DECODING-ERROR-IN-DISPATCH-MACRO-CHAR-COMMENT').
mop_direct('CHARACTER-DECODING-ERROR-IN-COMMENT', submop, 'CHARACTER-DECODING-ERROR-IN-MACRO-CHAR-COMMENT').
mop_direct('CHARACTER-DECODING-ERROR-IN-COMMENT', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-DECODING-ERROR-IN-COMMENT', szlot, 'POSITION').
mop_direct('CHARACTER-DECODING-ERROR-IN-COMMENT', szlot, 'STREAM').
mop_direct('CHARACTER-DECODING-ERROR-IN-DISPATCH-MACRO-CHAR-COMMENT', supers, ['CHARACTER-DECODING-ERROR-IN-COMMENT', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-DECODING-ERROR-IN-DISPATCH-MACRO-CHAR-COMMENT', szlot, 'DISP-CHAR').
mop_direct('CHARACTER-DECODING-ERROR-IN-DISPATCH-MACRO-CHAR-COMMENT', szlot, 'SUB-CHAR').
mop_direct('CHARACTER-DECODING-ERROR-IN-MACRO-CHAR-COMMENT', supers, ['CHARACTER-DECODING-ERROR-IN-COMMENT', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-DECODING-ERROR-IN-MACRO-CHAR-COMMENT', szlot, 'CHAR').
mop_direct('CHARACTER-ENCODING-ERROR', submop, 'C-STRING-ENCODING-ERROR').
mop_direct('CHARACTER-ENCODING-ERROR', submop, 'OCTETS-ENCODING-ERROR').
mop_direct('CHARACTER-ENCODING-ERROR', submop, 'STREAM-ENCODING-ERROR').
mop_direct('CHARACTER-ENCODING-ERROR', supers, ['CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-ENCODING-ERROR', szlot, 'CODE').
mop_direct('CHARACTER-OUT-OF-RANGE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-SET-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CHARACTER-SET-TYPE', szlot, 'CLASS-INFO').
mop_direct('CHARACTER-SET-TYPE', szlot, 'PAIRS').
mop_direct('CHARACTER-STRING', submop, 'SIMPLE-CHARACTER-STRING').
mop_direct('CHARACTER-STRING', supers, [cl_string, cl_vector, cl_array, 'SEQUENCE', t]).
mop_direct('CHARSET-TYPE-ERROR', submop, 'SIMPLE-CHARSET-TYPE-ERROR').
mop_direct('CHARSET-TYPE-ERROR', supers, [cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('CHECKING', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CHECKING', szlot, 'FUNCTION').
mop_direct('CHOOSER', supers, ['ANNOTATION', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CHOOSER', szlot, 'ALIGNMENT').
mop_direct('CHOOSER', szlot, 'MAYBE-SHRINK').
mop_direct('CHOOSER', szlot, 'SIZE').
mop_direct('CHOOSER', szlot, 'WORST-CASE-FUN').
mop_direct('CIF', supers, ['NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CIF', szlot, 'ALTERNATIVE').
mop_direct('CIF', szlot, 'ALTERNATIVE-CONSTRAINTS').
mop_direct('CIF', szlot, 'CONSEQUENT').
mop_direct('CIF', szlot, 'CONSEQUENT-CONSTRAINTS').
mop_direct('CIF', szlot, 'TEST').
mop_direct('CIRCULARITY', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CIRCULARITY', szlot, 'ENCLOSING-OBJECT').
mop_direct('CIRCULARITY', szlot, 'INDEX').
mop_direct('CIRCULARITY', szlot, 'OBJECT').
mop_direct('CIRCULARITY', szlot, 'TYPE').
mop_direct('CIRCULARITY', szlot, 'VALUE').
mop_direct('CLAMBDA', supers, ['FUNCTIONAL', 'LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CLAMBDA', szlot, 'ALLOW-INSTRUMENTING').
mop_direct('CLAMBDA', szlot, 'BIND').
mop_direct('CLAMBDA', szlot, 'CALL-LEXENV').
mop_direct('CLAMBDA', szlot, 'CALLS-OR-CLOSES').
mop_direct('CLAMBDA', szlot, 'CHILDREN').
mop_direct('CLAMBDA', szlot, 'ENTRIES').
mop_direct('CLAMBDA', szlot, 'HOME').
mop_direct('CLAMBDA', szlot, 'LETS').
mop_direct('CLAMBDA', szlot, 'OPTIONAL-DISPATCH').
mop_direct('CLAMBDA', szlot, 'PARENT').
mop_direct('CLAMBDA', szlot, 'PHYSENV').
mop_direct('CLAMBDA', szlot, 'RETURN').
mop_direct('CLAMBDA', szlot, 'SYSTEM-LAMBDA-P').
mop_direct('CLAMBDA', szlot, 'TAIL-SET').
mop_direct('CLAMBDA', szlot, 'VARS').
mop_direct('CLASS', submop, cl_built_in_class).
mop_direct('CLASS', submop, 'PCL-CLASS').
mop_direct('CLASS', submop, 'SLOTTED-CLASS').
mop_direct('CLASS', supers, ['DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CLASS', supers, ['POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('CLASS', szlot, '$ALL-SUPERCLASSES').
mop_direct('CLASS', szlot, '$DEFAULT-INITARGS').
mop_direct('CLASS', szlot, '$DIRECT-DEFAULT-INITARGS').
mop_direct('CLASS', szlot, '$DIRECT-SLOTS').
mop_direct('CLASS', szlot, '$DIRECT-SUPERCLASSES').
mop_direct('CLASS', szlot, '$DOCUMENTATION').
mop_direct('CLASS', szlot, '$INITIALIZED').
mop_direct('CLASS', szlot, '$LISTENERS').
mop_direct('CLASS', szlot, '$PRECEDENCE-LIST').
mop_direct('CLASS', szlot, '$SLOT-LOCATION-TABLE').
mop_direct('CLASS', szlot, '$SLOTS').
mop_direct('CLASS', szlot, '%DOCUMENTATION').
mop_direct('CLASS', szlot, 'CLASS-EQ-SPECIALIZER').
mop_direct('CLASS', szlot, 'DIRECT-METHODS').
mop_direct('CLASS', szlot, 'DIRECT-SUBCLASSES').
mop_direct('CLASS', szlot, 'DIRECT-SUPERCLASSES').
mop_direct('CLASS', szlot, 'FINALIZED-P').
mop_direct('CLASS', szlot, 'NAME').
mop_direct('CLASS', szlot, 'SAFE-P').
mop_direct('CLASS-EQ-SPECIALIZER', supers, ['STANDARD-SPECIALIZER', 'EXACT-CLASS-SPECIALIZER', 'SPECIALIZER-WITH-OBJECT', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CLASS-EQ-SPECIALIZER', szlot, 'OBJECT').
mop_direct('CLASS-PRECEDENCE-DESCRIPTION', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CLASS-PRECEDENCE-DESCRIPTION', szlot, 'CPD-AFTER').
mop_direct('CLASS-PRECEDENCE-DESCRIPTION', szlot, 'CPD-CLASS').
mop_direct('CLASS-PRECEDENCE-DESCRIPTION', szlot, 'CPD-COUNT').
mop_direct('CLASS-PRECEDENCE-DESCRIPTION', szlot, 'CPD-SUPERS').
mop_direct('CLASS-PROTOTYPE-SPECIALIZER', supers, ['STANDARD-SPECIALIZER', 'SPECIALIZER-WITH-OBJECT', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CLASS-PROTOTYPE-SPECIALIZER', szlot, 'OBJECT').
mop_direct('CLASSOID', submop, 'BUILT-IN-CLASSOID').
mop_direct('CLASSOID', submop, 'CONDITION-CLASSOID').
mop_direct('CLASSOID', submop, 'STANDARD-CLASSOID').
mop_direct('CLASSOID', submop, 'STATIC-CLASSOID').
mop_direct('CLASSOID', submop, 'STRUCTURE-CLASSOID').
mop_direct('CLASSOID', submop, 'UNDEFINED-CLASSOID').
mop_direct('CLASSOID', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CLASSOID', szlot, 'CLASS-INFO').
mop_direct('CLASSOID', szlot, 'DIRECT-SUPERCLASSES').
mop_direct('CLASSOID', szlot, 'LAYOUT').
mop_direct('CLASSOID', szlot, 'NAME').
mop_direct('CLASSOID', szlot, 'PCL-CLASS').
mop_direct('CLASSOID', szlot, 'STATE').
mop_direct('CLASSOID', szlot, 'SUBCLASSES').
mop_direct('CLASSOID-CELL', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CLASSOID-CELL', szlot, 'CLASSOID').
mop_direct('CLASSOID-CELL', szlot, 'NAME').
mop_direct('CLASSOID-CELL', szlot, 'PCL-CLASS').
mop_direct('CLEANUP', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CLEANUP', szlot, 'INFO').
mop_direct('CLEANUP', szlot, 'KIND').
mop_direct('CLEANUP', szlot, 'MESS-UP').
mop_direct('CLOOP', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CLOOP', szlot, 'BLOCKS').
mop_direct('CLOOP', szlot, 'DEPTH').
mop_direct('CLOOP', szlot, 'EXITS').
mop_direct('CLOOP', szlot, 'HEAD').
mop_direct('CLOOP', szlot, 'INFERIORS').
mop_direct('CLOOP', szlot, 'INFO').
mop_direct('CLOOP', szlot, 'KIND').
mop_direct('CLOOP', szlot, 'SUPERIOR').
mop_direct('CLOOP', szlot, 'TAIL').
mop_direct('CLOS-WARNING', submop, 'GF-ALREADY-CALLED-WARNING').
mop_direct('CLOS-WARNING', submop, 'GF-REPLACING-METHOD-WARNING').
mop_direct('CLOS-WARNING', submop, 'SIMPLE-CLOS-WARNING').
mop_direct('CLOS-WARNING', supers, [cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('CLOSED-STREAM-ERROR', supers, [cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CODE-COMPONENT', supers, [t]).
mop_direct('CODE-DELETION-NOTE', supers, ['SIMPLE-COMPILER-NOTE', cl_simple_condition, 'COMPILER-NOTE', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CODE-LOCATION', submop, 'COMPILED-CODE-LOCATION').
mop_direct('CODE-LOCATION', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CODE-LOCATION', szlot, '%DEBUG-BLOCK').
mop_direct('CODE-LOCATION', szlot, '%FORM-NUMBER').
mop_direct('CODE-LOCATION', szlot, '%TLF-OFFSET').
mop_direct('CODE-LOCATION', szlot, '%UNKNOWN-P').
mop_direct('CODE-LOCATION', szlot, 'DEBUG-FUN').
mop_direct('COMBINATION', supers, ['BASIC-COMBINATION', 'VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMMA', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMMA', szlot, 'EXPR').
mop_direct('COMMA', szlot, 'KIND').
mop_direct('COMPILED-CODE-LOCATION', supers, ['CODE-LOCATION', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-CODE-LOCATION', szlot, '%LIVE-SET').
mop_direct('COMPILED-CODE-LOCATION', szlot, 'KIND').
mop_direct('COMPILED-CODE-LOCATION', szlot, 'PC').
mop_direct('COMPILED-CODE-LOCATION', szlot, 'STEP-INFO').
mop_direct('COMPILED-DEBUG-BLOCK', supers, ['DEBUG-BLOCK', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-DEBUG-BLOCK', szlot, 'CODE-LOCATIONS').
mop_direct('COMPILED-DEBUG-FUN', supers, ['DEBUG-FUN', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-DEBUG-FUN', supers, ['DEBUG-FUN', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-DEBUG-FUN', szlot, 'ARGUMENTS').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'BLOCKS').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'BSP-SAVE').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'CLOSURE-SAVE').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'COMPILER-DEBUG-FUN').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'COMPONENT').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'ELSEWHERE-PC').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'END-STARTER').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'FORM-NUMBER').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'KIND').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'NAME').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'RETURNS').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'START-PC').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'TLF-NUMBER').
mop_direct('COMPILED-DEBUG-FUN', szlot, 'VARS').
mop_direct('COMPILED-DEBUG-INFO', supers, ['DEBUG-INFO', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-DEBUG-INFO', szlot, 'FUN-MAP').
mop_direct('COMPILED-DEBUG-VAR', supers, ['DEBUG-VAR', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-DEBUG-VAR', szlot, 'INDIRECT-SC-OFFSET').
mop_direct('COMPILED-DEBUG-VAR', szlot, 'INFO').
mop_direct('COMPILED-DEBUG-VAR', szlot, 'SAVE-SC-OFFSET').
mop_direct('COMPILED-DEBUG-VAR', szlot, 'SC-OFFSET').
mop_direct('COMPILED-FRAME', supers, ['FRAME', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-FRAME', szlot, 'ESCAPED').
mop_direct('COMPILED-PROGRAM-ERROR', supers, [cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('COMPILED-PROGRAM-ERROR', szlot, 'MESSAGE').
mop_direct('COMPILED-PROGRAM-ERROR', szlot, 'SOURCE').
mop_direct('COMPILER-ENVIRONMENT-TOO-COMPLEX-ERROR', supers, [cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('COMPILER-ERROR', supers, ['ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('COMPILER-ERROR-CONTEXT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'CONTEXT').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'ENCLOSING-SOURCE').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'FILE-NAME').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'FILE-POSITION').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'LEXENV').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'ORIGINAL-SOURCE').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'ORIGINAL-SOURCE-PATH').
mop_direct('COMPILER-ERROR-CONTEXT', szlot, 'SOURCE').
mop_direct('COMPILER-MACRO-APPLICATION-MISSED-WARNING', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('COMPILER-MACRO-APPLICATION-MISSED-WARNING', szlot, 'COUNT').
mop_direct('COMPILER-MACRO-APPLICATION-MISSED-WARNING', szlot, 'FUNCTION').
mop_direct('COMPILER-MACRO-KEYWORD-PROBLEM', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('COMPILER-MACRO-KEYWORD-PROBLEM', szlot, 'ARGUMENT').
mop_direct('COMPILER-NOTE', submop, 'SIMPLE-COMPILER-NOTE').
mop_direct('COMPILER-NOTE', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_complex, submop, 'COMPLEX-DOUBLE-FLOAT').
mop_direct(cl_complex, submop, 'COMPLEX-SINGLE-FLOAT').
mop_direct(cl_complex, supers, ['NUMBER', t]).
mop_direct('COMPLEX-DOUBLE-FLOAT', supers, [cl_complex, 'NUMBER', t]).
mop_direct('COMPLEX-SINGLE-FLOAT', supers, [cl_complex, 'NUMBER', t]).
mop_direct('COMPONENT', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPONENT', szlot, 'DELETE-BLOCKS').
mop_direct('COMPONENT', szlot, 'DX-LVARS').
mop_direct('COMPONENT', szlot, 'FAILED-OPTIMIZATIONS').
mop_direct('COMPONENT', szlot, 'HEAD').
mop_direct('COMPONENT', szlot, 'INFO').
mop_direct('COMPONENT', szlot, 'INLINE-EXPANSIONS').
mop_direct('COMPONENT', szlot, 'KIND').
mop_direct('COMPONENT', szlot, 'LAMBDAS').
mop_direct('COMPONENT', szlot, 'LAST-BLOCK').
mop_direct('COMPONENT', szlot, 'NAME').
mop_direct('COMPONENT', szlot, 'NEW-FUNCTIONALS').
mop_direct('COMPONENT', szlot, 'NLX-INFO-GENERATED-P').
mop_direct('COMPONENT', szlot, 'OUTER-LOOP').
mop_direct('COMPONENT', szlot, 'REANALYZE').
mop_direct('COMPONENT', szlot, 'REANALYZE-FUNCTIONALS').
mop_direct('COMPONENT', szlot, 'REOPTIMIZE').
mop_direct('COMPONENT', szlot, 'SSET-NUMBER').
mop_direct('COMPONENT', szlot, 'TAIL').
mop_direct('COMPOUND-TYPE', submop, 'INTERSECTION-TYPE').
mop_direct('COMPOUND-TYPE', submop, 'UNION-TYPE').
mop_direct('COMPOUND-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COMPOUND-TYPE', szlot, 'ENUMERABLE').
mop_direct('COMPOUND-TYPE', szlot, 'TYPES').
mop_direct(cl_concatenated_stream, supers, ['ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_concatenated_stream, supers, [cl_stream, t]).
mop_direct(cl_concatenated_stream, szlot, 'BIN').
mop_direct(cl_concatenated_stream, szlot, 'IN').
mop_direct(cl_concatenated_stream, szlot, 'MISC').
mop_direct(cl_concatenated_stream, szlot, 'N-BIN').
mop_direct(cl_concatenated_stream, szlot, 'STREAMS').
mop_direct(cl_condition, submop, 'BOOTSTRAP-PACKAGE-NOT-FOUND').
mop_direct(cl_condition, submop, 'COMPILER-MACRO-KEYWORD-PROBLEM').
mop_direct(cl_condition, submop, 'COMPILER-NOTE').
mop_direct(cl_condition, submop, 'ENCAPSULATED-CONDITION').
mop_direct(cl_condition, submop, 'PARSE-DEPRECATED-TYPE').
mop_direct(cl_condition, submop, 'PARSE-UNKNOWN-TYPE').
mop_direct(cl_condition, submop, 'PROCLAMATION-MISMATCH').
mop_direct(cl_condition, submop, 'REFERENCE-CONDITION').
mop_direct(cl_condition, submop, cl_serious_condition).
mop_direct(cl_condition, submop, cl_simple_condition).
mop_direct(cl_condition, submop, 'STEP-CONDITION').
mop_direct(cl_condition, submop, 'SYSTEM-CONDITION').
mop_direct(cl_condition, submop, cl_warning).
mop_direct(cl_condition, supers, ['SLOT-OBJECT', t]).
mop_direct(cl_condition, supers, [cl_standard_object, t]).
mop_direct('CONDITION-CLASS', supers, [cl_slot_class, 'PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CONDITION-CLASSOID', supers, ['CLASSOID', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONDITION-CLASSOID', szlot, 'CLASS-SLOTS').
mop_direct('CONDITION-CLASSOID', szlot, 'CPL').
mop_direct('CONDITION-CLASSOID', szlot, 'DIRECT-DEFAULT-INITARGS').
mop_direct('CONDITION-CLASSOID', szlot, 'HAIRY-SLOTS').
mop_direct('CONDITION-CLASSOID', szlot, 'REPORT').
mop_direct('CONDITION-CLASSOID', szlot, 'SLOTS').
mop_direct('CONDITION-DIRECT-SLOT-DEFINITION', supers, ['CONDITION-SLOT-DEFINITION', 'DIRECT-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CONDITION-EFFECTIVE-SLOT-DEFINITION', supers, ['CONDITION-SLOT-DEFINITION', 'EFFECTIVE-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CONDITION-SLOT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONDITION-SLOT', szlot, 'ALLOCATION').
mop_direct('CONDITION-SLOT', szlot, 'CELL').
mop_direct('CONDITION-SLOT', szlot, 'DOCUMENTATION').
mop_direct('CONDITION-SLOT', szlot, 'INITARGS').
mop_direct('CONDITION-SLOT', szlot, 'INITFORM').
mop_direct('CONDITION-SLOT', szlot, 'INITFORM-P').
mop_direct('CONDITION-SLOT', szlot, 'INITFUNCTION').
mop_direct('CONDITION-SLOT', szlot, 'NAME').
mop_direct('CONDITION-SLOT', szlot, 'READERS').
mop_direct('CONDITION-SLOT', szlot, 'WRITERS').
mop_direct('CONDITION-SLOT-DEFINITION', submop, 'CONDITION-DIRECT-SLOT-DEFINITION').
mop_direct('CONDITION-SLOT-DEFINITION', submop, 'CONDITION-EFFECTIVE-SLOT-DEFINITION').
mop_direct('CONDITION-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('CONDITION-SLOT-DEFINITION', szlot, 'ALLOCATION').
mop_direct('CONDITION-SLOT-DEFINITION', szlot, 'ALLOCATION-CLASS').
mop_direct(cl_cons, supers, [cl_list, 'SEQUENCE', t]).
mop_direct('CONS-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONS-TYPE', szlot, 'CAR-TYPE').
mop_direct('CONS-TYPE', szlot, 'CDR-TYPE').
mop_direct('CONS-TYPE', szlot, 'CLASS-INFO').
mop_direct('CONSET', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSET', szlot, 'MAX').
mop_direct('CONSET', szlot, 'MIN').
mop_direct('CONSET', szlot, 'VECTOR').
mop_direct('CONST', supers, [cl_structure_object, t]).
mop_direct('CONST', szlot, 'FORM').
mop_direct('CONST', szlot, 'HORIZON').
mop_direct('CONST', szlot, 'LTV-FORM').
mop_direct('CONST', szlot, 'VALUE').
mop_direct('CONSTANT', supers, ['LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSTANT', szlot, 'BOXED-TN').
mop_direct('CONSTANT', szlot, 'VALUE').
mop_direct('CONSTANT-FAST-METHOD-CALL', supers, ['FAST-METHOD-CALL', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSTANT-FAST-METHOD-CALL', szlot, 'VALUE').
mop_direct('CONSTANT-METHOD-CALL', supers, ['METHOD-CALL', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSTANT-METHOD-CALL', szlot, 'VALUE').
mop_direct('CONSTANT-MODIFIED', supers, ['REFERENCE-CONDITION', cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CONSTANT-MODIFIED', szlot, 'FUN-NAME').
mop_direct('CONSTANT-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSTANT-TYPE', szlot, 'CLASS-INFO').
mop_direct('CONSTANT-TYPE', szlot, 'TYPE').
mop_direct('CONSTANT-VALUE', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSTRAINT', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CONSTRAINT', szlot, 'KIND').
mop_direct('CONSTRAINT', szlot, 'NOT-P').
mop_direct('CONSTRAINT', szlot, 'X').
mop_direct('CONSTRAINT', szlot, 'Y').
mop_direct(cl_control_error, submop, 'ABORT-FAILURE').
mop_direct(cl_control_error, submop, 'SIMPLE-CONTROL-ERROR').
mop_direct(cl_control_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_control_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('CONTROL-STACK-EXHAUSTED', supers, [cl_storage_condition, cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CONTROL-STRING-DIRECTIVE', supers, [cl_structure_object, t]).
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'ATSIGN-P').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'CLAUSE-CHAIN').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'COLON-P').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'CS-INDEX').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'DATA').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'PARM-LIST').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'TYPE').
mop_direct('CONTROL-STRING-DIRECTIVE', szlot, 'V-OR-#-P').
mop_direct('CORE-OBJECT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CORE-OBJECT', szlot, 'DEBUG-INFO').
mop_direct('CORE-OBJECT', szlot, 'ENTRY-TABLE').
mop_direct('CORE-OBJECT', szlot, 'PATCH-TABLE').
mop_direct('COUNTER', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('COUNTER', szlot, 'OVERFLOW').
mop_direct('COUNTER', szlot, 'WORD').
mop_direct('CPL-PROTOCOL-VIOLATION', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('CPL-PROTOCOL-VIOLATION', szlot, 'CLASS').
mop_direct('CPL-PROTOCOL-VIOLATION', szlot, 'CPL').
mop_direct('CRETURN', supers, ['NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CRETURN', szlot, 'LAMBDA').
mop_direct('CRETURN', szlot, 'RESULT').
mop_direct('CRETURN', szlot, 'RESULT-TYPE').
mop_direct('CSET', supers, ['VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CSET', szlot, 'DERIVED-TYPE').
mop_direct('CSET', szlot, 'VALUE').
mop_direct('CSET', szlot, 'VAR').
mop_direct('CTOR', supers, [t]).
mop_direct('CTOR', szlot, 'CLASS').
mop_direct('CTOR', szlot, 'CLASS-OR-NAME').
mop_direct('CTOR', szlot, 'FUNCTION-NAME').
mop_direct('CTOR', szlot, 'INITARGS').
mop_direct('CTOR', szlot, 'SAFE-P').
mop_direct('CTOR', szlot, 'STATE').
mop_direct('CTRAN', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CTRAN', szlot, 'BLOCK').
mop_direct('CTRAN', szlot, 'KIND').
mop_direct('CTRAN', szlot, 'NEXT').
mop_direct('CTRAN', szlot, 'USE').
mop_direct('CTYPE', submop, 'ALIEN-TYPE-TYPE').
mop_direct('CTYPE', submop, 'ARGS-TYPE').
mop_direct('CTYPE', submop, 'ARRAY-TYPE').
mop_direct('CTYPE', submop, 'CHARACTER-SET-TYPE').
mop_direct('CTYPE', submop, 'CLASSOID').
mop_direct('CTYPE', submop, 'COMPOUND-TYPE').
mop_direct('CTYPE', submop, 'CONS-TYPE').
mop_direct('CTYPE', submop, 'CONSTANT-TYPE').
mop_direct('CTYPE', submop, 'HAIRY-TYPE').
mop_direct('CTYPE', submop, 'MEMBER-TYPE').
mop_direct('CTYPE', submop, 'NAMED-TYPE').
mop_direct('CTYPE', submop, 'NEGATION-TYPE').
mop_direct('CTYPE', submop, 'NUMERIC-TYPE').
mop_direct('CTYPE', submop, 'SIMD-PACK-TYPE').
mop_direct('CTYPE', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('CTYPE', szlot, 'CLASS-INFO').
mop_direct('CTYPE', szlot, 'HASH-VALUE').
mop_direct('DEAD-BEEF-STRUCTURE-OBJECT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEADLINE-TIMEOUT', supers, ['TIMEOUT', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-BLOCK', submop, 'COMPILED-DEBUG-BLOCK').
mop_direct('DEBUG-BLOCK', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-BLOCK', szlot, 'ELSEWHERE-P').
mop_direct('DEBUG-CONDITION', submop, 'AMBIGUOUS-VAR-NAME').
mop_direct('DEBUG-CONDITION', submop, 'INVALID-VALUE').
mop_direct('DEBUG-CONDITION', submop, 'LAMBDA-LIST-UNAVAILABLE').
mop_direct('DEBUG-CONDITION', submop, 'NO-DEBUG-BLOCKS').
mop_direct('DEBUG-CONDITION', submop, 'NO-DEBUG-FUN-RETURNS').
mop_direct('DEBUG-CONDITION', submop, 'NO-DEBUG-VARS').
mop_direct('DEBUG-CONDITION', supers, [cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-ERROR', submop, 'FRAME-FUN-MISMATCH').
mop_direct('DEBUG-ERROR', submop, 'INVALID-CONTROL-STACK-POINTER').
mop_direct('DEBUG-ERROR', submop, 'UNHANDLED-DEBUG-CONDITION').
mop_direct('DEBUG-ERROR', submop, 'UNKNOWN-CODE-LOCATION').
mop_direct('DEBUG-ERROR', submop, 'UNKNOWN-DEBUG-VAR').
mop_direct('DEBUG-ERROR', supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-FUN', submop, 'BOGUS-DEBUG-FUN').
mop_direct('DEBUG-FUN', submop, 'COMPILED-DEBUG-FUN').
mop_direct('DEBUG-FUN', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-FUN', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-FUN', szlot, '%DEBUG-VARS').
mop_direct('DEBUG-FUN', szlot, '%FUNCTION').
mop_direct('DEBUG-FUN', szlot, '%LAMBDA-LIST').
mop_direct('DEBUG-FUN', szlot, 'BLOCKS').
mop_direct('DEBUG-INFO', submop, 'COMPILED-DEBUG-INFO').
mop_direct('DEBUG-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-INFO', szlot, 'NAME').
mop_direct('DEBUG-INFO', szlot, 'SOURCE').
mop_direct('DEBUG-NAME-MARKER', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-SOURCE', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-SOURCE', szlot, 'COMPILED').
mop_direct('DEBUG-SOURCE', szlot, 'CREATED').
mop_direct('DEBUG-SOURCE', szlot, 'FORM').
mop_direct('DEBUG-SOURCE', szlot, 'FUNCTION').
mop_direct('DEBUG-SOURCE', szlot, 'NAMESTRING').
mop_direct('DEBUG-SOURCE', szlot, 'PLIST').
mop_direct('DEBUG-SOURCE', szlot, 'SOURCE-ROOT').
mop_direct('DEBUG-SOURCE', szlot, 'START-POSITIONS').
mop_direct('DEBUG-VAR', submop, 'COMPILED-DEBUG-VAR').
mop_direct('DEBUG-VAR', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEBUG-VAR', szlot, 'ALIVE-P').
mop_direct('DEBUG-VAR', szlot, 'ID').
mop_direct('DEBUG-VAR', szlot, 'SYMBOL').
mop_direct('DECLARATION-TYPE-CONFLICT-ERROR', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEFAULT-METHOD-ONLY', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEFCONSTANT-UNEQL', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEFCONSTANT-UNEQL', szlot, 'NAME').
mop_direct('DEFCONSTANT-UNEQL', szlot, 'NEW-VALUE').
mop_direct('DEFCONSTANT-UNEQL', szlot, 'OLD-VALUE').
mop_direct('DEFINED-FUN', supers, ['GLOBAL-VAR', 'BASIC-VAR', 'LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEFINED-FUN', szlot, 'FUNCTIONALS').
mop_direct('DEFINED-FUN', szlot, 'INLINE-EXPANSION').
mop_direct('DEFINED-FUN', szlot, 'INLINEP').
mop_direct('DEFINED-FUN', szlot, 'KIND').
mop_direct('DEFINED-FUN', szlot, 'WHERE-FROM').
mop_direct('DEFINITION-SOURCE-LOCATION', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEFINITION-SOURCE-LOCATION', szlot, 'FORM-NUMBER').
mop_direct('DEFINITION-SOURCE-LOCATION', szlot, 'NAMESTRING').
mop_direct('DEFINITION-SOURCE-LOCATION', szlot, 'PLIST').
mop_direct('DEFINITION-SOURCE-LOCATION', szlot, 'TOPLEVEL-FORM-NUMBER').
mop_direct('DEFINITION-SOURCE-MIXIN', submop, 'CLASS').
mop_direct('DEFINITION-SOURCE-MIXIN', submop, 'GENERIC-FUNCTION').
mop_direct('DEFINITION-SOURCE-MIXIN', submop, 'STANDARD-METHOD').
mop_direct('DEFINITION-SOURCE-MIXIN', submop, 'STANDARD-METHOD-COMBINATION').
mop_direct('DEFINITION-SOURCE-MIXIN', supers, [cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('DEFINITION-SOURCE-MIXIN', szlot, 'SOURCE').
mop_direct('DEFMACRO-LAMBDA-LIST-BIND-ERROR', submop, 'ARG-COUNT-ERROR').
mop_direct('DEFMACRO-LAMBDA-LIST-BIND-ERROR', submop, 'DEFMACRO-LAMBDA-LIST-BROKEN-KEY-LIST-ERROR').
mop_direct('DEFMACRO-LAMBDA-LIST-BIND-ERROR', supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEFMACRO-LAMBDA-LIST-BIND-ERROR', szlot, 'KIND').
mop_direct('DEFMACRO-LAMBDA-LIST-BIND-ERROR', szlot, 'NAME').
mop_direct('DEFMACRO-LAMBDA-LIST-BROKEN-KEY-LIST-ERROR', supers, ['DEFMACRO-LAMBDA-LIST-BIND-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEFMACRO-LAMBDA-LIST-BROKEN-KEY-LIST-ERROR', szlot, 'INFO').
mop_direct('DEFMACRO-LAMBDA-LIST-BROKEN-KEY-LIST-ERROR', szlot, 'PROBLEM').
mop_direct('DEFSTRUCT-DESCRIPTION', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'ALTERNATE-METACLASS').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'CONC-NAME').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'CONSTRUCTORS').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'COPIER-NAME').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'DOC').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'ELEMENT-TYPE').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'INCLUDE').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'INHERITED-ACCESSOR-ALIST').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'LENGTH').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'NAME').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'NAMED').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'NULL-LEXENV-P').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'OFFSET').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'PREDICATE-NAME').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'PRINT-OPTION').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'PRINTER-FNAME').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'PURE').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'SLOTS').
mop_direct('DEFSTRUCT-DESCRIPTION', szlot, 'TYPE').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'ACCESSOR-NAME').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'DEFAULT').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'INDEX').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'NAME').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'RAW-TYPE').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'READ-ONLY').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'SAFE-P').
mop_direct('DEFSTRUCT-SLOT-DESCRIPTION', szlot, 'TYPE').
mop_direct('DEPENDENT-UPDATE-MIXIN', submop, 'CLASS').
mop_direct('DEPENDENT-UPDATE-MIXIN', submop, 'GENERIC-FUNCTION').
mop_direct('DEPENDENT-UPDATE-MIXIN', supers, ['PLIST-MIXIN', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('DEPRECATED-EVAL-WHEN-SITUATIONS', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEPRECATED-EVAL-WHEN-SITUATIONS', szlot, 'SITUATIONS').
mop_direct('DEPRECATION-CONDITION', submop, 'DEPRECATION-ERROR').
mop_direct('DEPRECATION-CONDITION', submop, 'EARLY-DEPRECATION-WARNING').
mop_direct('DEPRECATION-CONDITION', submop, 'FINAL-DEPRECATION-WARNING').
mop_direct('DEPRECATION-CONDITION', submop, 'LATE-DEPRECATION-WARNING').
mop_direct('DEPRECATION-CONDITION', supers, ['REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEPRECATION-CONDITION', szlot, 'NAME').
mop_direct('DEPRECATION-CONDITION', szlot, 'NAMESPACE').
mop_direct('DEPRECATION-CONDITION', szlot, 'REPLACEMENTS').
mop_direct('DEPRECATION-CONDITION', szlot, 'RUNTIME-ERROR').
mop_direct('DEPRECATION-CONDITION', szlot, 'SOFTWARE').
mop_direct('DEPRECATION-CONDITION', szlot, 'VERSION').
mop_direct('DEPRECATION-ERROR', supers, ['ERROR', cl_serious_condition, 'DEPRECATION-CONDITION', 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DEPRECATION-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DEPRECATION-INFO', szlot, 'REPLACEMENTS').
mop_direct('DEPRECATION-INFO', szlot, 'SOFTWARE').
mop_direct('DEPRECATION-INFO', szlot, 'STATE').
mop_direct('DEPRECATION-INFO', szlot, 'VERSION').
mop_direct('DESCRIBE-STREAM', supers, ['FILL-STREAM', 'FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', 'FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('DFUN-INFO', submop, 'ACCESSOR-DFUN-INFO').
mop_direct('DFUN-INFO', submop, 'CACHING').
mop_direct('DFUN-INFO', submop, 'CHECKING').
mop_direct('DFUN-INFO', submop, 'CONSTANT-VALUE').
mop_direct('DFUN-INFO', submop, 'DEFAULT-METHOD-ONLY').
mop_direct('DFUN-INFO', submop, 'DISPATCH').
mop_direct('DFUN-INFO', submop, 'INITIAL').
mop_direct('DFUN-INFO', submop, 'NO-METHODS').
mop_direct('DFUN-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DFUN-INFO', szlot, 'CACHE').
mop_direct('DIRECT-SLOT-DEFINITION', submop, 'CONDITION-DIRECT-SLOT-DEFINITION').
mop_direct('DIRECT-SLOT-DEFINITION', submop, 'STANDARD-DIRECT-SLOT-DEFINITION').
mop_direct('DIRECT-SLOT-DEFINITION', submop, 'STRUCTURE-DIRECT-SLOT-DEFINITION').
mop_direct('DIRECT-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('DIRECT-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('DIRECT-SLOT-DEFINITION', szlot, '$READERS').
mop_direct('DIRECT-SLOT-DEFINITION', szlot, '$WRITERS').
mop_direct('DIRECT-SLOT-DEFINITION', szlot, 'READERS').
mop_direct('DIRECT-SLOT-DEFINITION', szlot, 'WRITERS').
mop_direct('DISASSEM-STATE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('DISASSEM-STATE', szlot, 'ADDR-PRINT-LEN').
mop_direct('DISASSEM-STATE', szlot, 'ALIGNMENT').
mop_direct('DISASSEM-STATE', szlot, 'ARGUMENT-COLUMN').
mop_direct('DISASSEM-STATE', szlot, 'BYTE-ORDER').
mop_direct('DISASSEM-STATE', szlot, 'CUR-LABELS').
mop_direct('DISASSEM-STATE', szlot, 'CUR-OFFS').
mop_direct('DISASSEM-STATE', szlot, 'CUR-OFFS-HOOKS').
mop_direct('DISASSEM-STATE', szlot, 'CURRENT-VALID-LOCATIONS').
mop_direct('DISASSEM-STATE', szlot, 'FILTERED-VALUES').
mop_direct('DISASSEM-STATE', szlot, 'FUN-HOOKS').
mop_direct('DISASSEM-STATE', szlot, 'INST-PROPERTIES').
mop_direct('DISASSEM-STATE', szlot, 'LABEL-HASH').
mop_direct('DISASSEM-STATE', szlot, 'LABELS').
mop_direct('DISASSEM-STATE', szlot, 'NEXT-OFFS').
mop_direct('DISASSEM-STATE', szlot, 'NOTES').
mop_direct('DISASSEM-STATE', szlot, 'OUTPUT-STATE').
mop_direct('DISASSEM-STATE', szlot, 'PROPERTIES').
mop_direct('DISASSEM-STATE', szlot, 'SEGMENT').
mop_direct('DISASSEM-STATE', szlot, 'SEGMENT-SAP').
mop_direct('DISPATCH', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_division_by_zero, submop, 'SIMPLE-DIVISION-BY-ZERO').
mop_direct(cl_division_by_zero, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_division_by_zero, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_double_float, supers, ['FLOAT', 'REAL', 'NUMBER', t]).
mop_direct('DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME', submop, 'ASTERISKS-AROUND-CONSTANT-VARIABLE-NAME').
mop_direct('DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME', submop, 'ASTERISKS-AROUND-LEXICAL-VARIABLE-NAME').
mop_direct('DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME', supers, [cl_style_warning, cl_warning, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DUPLICATE-CASE-KEY-WARNING', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DUPLICATE-CASE-KEY-WARNING', szlot, 'CASE-KIND').
mop_direct('DUPLICATE-CASE-KEY-WARNING', szlot, 'KEY').
mop_direct('DUPLICATE-CASE-KEY-WARNING', szlot, 'OCCURRENCES').
mop_direct('DUPLICATE-DEFINITION', supers, ['REFERENCE-CONDITION', cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('DUPLICATE-DEFINITION', szlot, 'NAME').
mop_direct('EA', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('EA', szlot, 'BASE').
mop_direct('EA', szlot, 'DISP').
mop_direct('EA', szlot, 'INDEX').
mop_direct('EA', szlot, 'SCALE').
mop_direct('EA', szlot, 'SIZE').
mop_direct('EARLY-DEPRECATION-WARNING', supers, [cl_style_warning, cl_warning, 'DEPRECATION-CONDITION', 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_echo_stream, supers, [cl_stream, t]).
mop_direct(cl_echo_stream, supers, [cl_two_way_stream, 'ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_echo_stream, szlot, 'BIN').
mop_direct(cl_echo_stream, szlot, 'IN').
mop_direct(cl_echo_stream, szlot, 'MISC').
mop_direct(cl_echo_stream, szlot, 'N-BIN').
mop_direct(cl_echo_stream, szlot, 'UNREAD-STUFF').
mop_direct('EFFECTIVE-SLOT-DEFINITION', submop, 'CONDITION-EFFECTIVE-SLOT-DEFINITION').
mop_direct('EFFECTIVE-SLOT-DEFINITION', submop, 'STANDARD-EFFECTIVE-SLOT-DEFINITION').
mop_direct('EFFECTIVE-SLOT-DEFINITION', submop, 'STRUCTURE-EFFECTIVE-SLOT-DEFINITION').
mop_direct('EFFECTIVE-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('EFFECTIVE-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SBUC').
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SMUC').
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SSVUC').
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SVUC').
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, '$LOCATION').
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, 'ACCESSOR-FLAGS').
mop_direct('EFFECTIVE-SLOT-DEFINITION', szlot, 'INFO').
mop_direct('ENCAPSULATED-CONDITION', submop, 'COMPILER-ERROR').
mop_direct('ENCAPSULATED-CONDITION', submop, 'EVAL-ERROR').
mop_direct('ENCAPSULATED-CONDITION', submop, 'FATAL-COMPILER-ERROR').
mop_direct('ENCAPSULATED-CONDITION', submop, 'INPUT-ERROR-IN-COMPILE-FILE').
mop_direct('ENCAPSULATED-CONDITION', submop, 'INTERPRETED-PROGRAM-ERROR').
mop_direct('ENCAPSULATED-CONDITION', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ENCAPSULATED-CONDITION', szlot, 'CONDITION').
mop_direct('ENCAPSULATION-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ENCAPSULATION-INFO', szlot, 'DEFINITION').
mop_direct('ENCAPSULATION-INFO', szlot, 'TYPE').
mop_direct(cl_end_of_file, submop, 'READER-EOF-ERROR').
mop_direct(cl_end_of_file, submop, 'SIMPLE-END-OF-FILE').
mop_direct(cl_end_of_file, supers, [cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_end_of_file, supers, [cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('END-OF-INPUT-IN-CHARACTER', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ENTRY', supers, ['NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ENTRY', szlot, 'CLEANUP').
mop_direct('ENTRY', szlot, 'EXITS').
mop_direct('ENTRY-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ENTRY-INFO', szlot, 'ARGUMENTS').
mop_direct('ENTRY-INFO', szlot, 'CLOSURE-TN').
mop_direct('ENTRY-INFO', szlot, 'INFO').
mop_direct('ENTRY-INFO', szlot, 'NAME').
mop_direct('ENTRY-INFO', szlot, 'OFFSET').
mop_direct('ENTRY-INFO', szlot, 'TYPE').
mop_direct('ENV', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ENV', szlot, 'BLOCKS').
mop_direct('ENV', szlot, 'DECLARATIONS').
mop_direct('ENV', szlot, 'EXPANDERS').
mop_direct('ENV', szlot, 'FUNS').
mop_direct('ENV', szlot, 'NATIVE-LEXENV').
mop_direct('ENV', szlot, 'PARENT').
mop_direct('ENV', szlot, 'SYMBOL-EXPANSIONS').
mop_direct('ENV', szlot, 'TAGS').
mop_direct('ENV', szlot, 'VARS').
mop_direct('EQL-SPECIALIZER', supers, ['SPECIALIZER', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('EQL-SPECIALIZER', supers, ['STANDARD-SPECIALIZER', 'EXACT-CLASS-SPECIALIZER', 'SPECIALIZER-WITH-OBJECT', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('EQL-SPECIALIZER', szlot, '$SINGLETON').
mop_direct('EQL-SPECIALIZER', szlot, 'DIRECT-METHODS').
mop_direct('EQL-SPECIALIZER', szlot, 'OBJECT').
mop_direct('ERROR', submop, cl_arithmetic_error).
mop_direct('ERROR', submop, 'BREAKPOINT-ERROR').
mop_direct('ERROR', submop, cl_cell_error).
mop_direct('ERROR', submop, 'CHARACTER-CODING-ERROR').
mop_direct('ERROR', submop, cl_control_error).
mop_direct('ERROR', submop, 'CPL-PROTOCOL-VIOLATION').
mop_direct('ERROR', submop, 'DEBUG-ERROR').
mop_direct('ERROR', submop, 'DEFCONSTANT-UNEQL').
mop_direct('ERROR', submop, 'DEFMACRO-LAMBDA-LIST-BIND-ERROR').
mop_direct('ERROR', submop, 'DEPRECATION-ERROR').
mop_direct('ERROR', submop, cl_file_error).
mop_direct('ERROR', submop, 'FORMAT-ERROR').
mop_direct('ERROR', submop, 'FTYPE-PROCLAMATION-MISMATCH-ERROR').
mop_direct('ERROR', submop, 'INSTANCE-STRUCTURE-PROTOCOL-ERROR').
mop_direct('ERROR', submop, 'INVALID-FASL').
mop_direct('ERROR', submop, 'INVALID-SUPERCLASS').
mop_direct('ERROR', submop, 'MEMORY-FAULT-ERROR').
mop_direct('ERROR', submop, 'MISSING-LOAD-FORM').
mop_direct('ERROR', submop, 'NEW-VALUE-SPECIALIZATION').
mop_direct('ERROR', submop, 'NO-PRIMARY-METHOD').
mop_direct('ERROR', submop, 'OBSOLETE-STRUCTURE').
mop_direct('ERROR', submop, 'OS-ERROR').
mop_direct('ERROR', submop, cl_package_error).
mop_direct('ERROR', submop, cl_parse_error).
mop_direct('ERROR', submop, cl_print_not_readable).
mop_direct('ERROR', submop, cl_program_error).
mop_direct('ERROR', submop, 'SAVE-ERROR').
mop_direct('ERROR', submop, cl_simple_error).
mop_direct('ERROR', submop, 'SLOTD-INITIALIZATION-ERROR').
mop_direct('ERROR', submop, 'STANDARD-PPRINT-DISPATCH-TABLE-MODIFIED-ERROR').
mop_direct('ERROR', submop, 'STANDARD-READTABLE-MODIFIED-ERROR').
mop_direct('ERROR', submop, cl_stream_error).
mop_direct('ERROR', submop, 'THREAD-ERROR').
mop_direct('ERROR', submop, cl_type_error).
mop_direct('ERROR', supers, [cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('ERROR', supers, [cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('EVAL-ERROR', supers, ['ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('EVENT-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('EVENT-INFO', szlot, 'ACTION').
mop_direct('EVENT-INFO', szlot, 'COUNT').
mop_direct('EVENT-INFO', szlot, 'DESCRIPTION').
mop_direct('EVENT-INFO', szlot, 'LEVEL').
mop_direct('EVENT-INFO', szlot, 'NAME').
mop_direct('EVENT-INFO', szlot, 'VAR').
mop_direct('EXACT-CLASS-SPECIALIZER', submop, 'CLASS-EQ-SPECIALIZER').
mop_direct('EXACT-CLASS-SPECIALIZER', submop, 'EQL-SPECIALIZER').
mop_direct('EXACT-CLASS-SPECIALIZER', supers, ['SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('EXIT', supers, ['VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('EXIT', szlot, 'ENTRY').
mop_direct('EXIT', szlot, 'NLX-INFO').
mop_direct('EXIT', szlot, 'VALUE').
mop_direct('EXTENSION-FAILURE', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('EXTERNAL-FORMAT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('EXTERNAL-FORMAT', szlot, 'BYTES-FOR-CHAR-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'DEFAULT-REPLACEMENT-CHARACTER').
mop_direct('EXTERNAL-FORMAT', szlot, 'NAMES').
mop_direct('EXTERNAL-FORMAT', szlot, 'OCTETS-TO-STRING-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'READ-C-STRING-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'READ-CHAR-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'READ-N-CHARS-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'RESYNC-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'STRING-TO-OCTETS-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'WRITE-C-STRING-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'WRITE-CHAR-FULL-BUFFERED-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'WRITE-CHAR-LINE-BUFFERED-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'WRITE-CHAR-NONE-BUFFERED-FUN').
mop_direct('EXTERNAL-FORMAT', szlot, 'WRITE-N-BYTES-FUN').
mop_direct('FASL-HEADER-MISSING', supers, ['INVALID-FASL', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FASL-HEADER-MISSING', szlot, 'FHSSS').
mop_direct('FASL-INPUT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FASL-INPUT', szlot, 'DEPRECATED-STUFF').
mop_direct('FASL-INPUT', szlot, 'SKIP-UNTIL').
mop_direct('FASL-INPUT', szlot, 'STACK').
mop_direct('FASL-INPUT', szlot, 'STREAM').
mop_direct('FASL-INPUT', szlot, 'TABLE').
mop_direct('FASL-OUTPUT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FASL-OUTPUT', szlot, 'CIRCULARITY-TABLE').
mop_direct('FASL-OUTPUT', szlot, 'DEBUG-INFO').
mop_direct('FASL-OUTPUT', szlot, 'ENTRY-TABLE').
mop_direct('FASL-OUTPUT', szlot, 'EQ-TABLE').
mop_direct('FASL-OUTPUT', szlot, 'EQUAL-TABLE').
mop_direct('FASL-OUTPUT', szlot, 'PACKAGES').
mop_direct('FASL-OUTPUT', szlot, 'PATCH-TABLE').
mop_direct('FASL-OUTPUT', szlot, 'STREAM').
mop_direct('FASL-OUTPUT', szlot, 'STRING=-TABLE').
mop_direct('FASL-OUTPUT', szlot, 'TABLE-FREE').
mop_direct('FASL-OUTPUT', szlot, 'VALID-STRUCTURES').
mop_direct('FAST-INSTANCE-BOUNDP', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FAST-INSTANCE-BOUNDP', szlot, 'INDEX').
mop_direct('FAST-METHOD-CALL', submop, 'CONSTANT-FAST-METHOD-CALL').
mop_direct('FAST-METHOD-CALL', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FAST-METHOD-CALL', szlot, 'ARG-INFO').
mop_direct('FAST-METHOD-CALL', szlot, 'FUNCTION').
mop_direct('FAST-METHOD-CALL', szlot, 'NEXT-METHOD-CALL').
mop_direct('FAST-METHOD-CALL', szlot, 'PV').
mop_direct('FATAL-COMPILER-ERROR', supers, ['ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FD-STREAM', submop, 'FORM-TRACKING-STREAM').
mop_direct('FD-STREAM', supers, [cl_file_stream, 'ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FD-STREAM', szlot, 'BIVALENT-P').
mop_direct('FD-STREAM', szlot, 'BUFFERING').
mop_direct('FD-STREAM', szlot, 'CHAR-SIZE').
mop_direct('FD-STREAM', szlot, 'DELETE-ORIGINAL').
mop_direct('FD-STREAM', szlot, 'DUAL-CHANNEL-P').
mop_direct('FD-STREAM', szlot, 'ELEMENT-SIZE').
mop_direct('FD-STREAM', szlot, 'ELEMENT-TYPE').
mop_direct('FD-STREAM', szlot, 'EOF-FORCED-P').
mop_direct('FD-STREAM', szlot, 'EXTERNAL-FORMAT').
mop_direct('FD-STREAM', szlot, 'FD').
mop_direct('FD-STREAM', szlot, 'FD-TYPE').
mop_direct('FD-STREAM', szlot, 'FILE').
mop_direct('FD-STREAM', szlot, 'HANDLER').
mop_direct('FD-STREAM', szlot, 'IBUF').
mop_direct('FD-STREAM', szlot, 'INSTEAD').
mop_direct('FD-STREAM', szlot, 'LISTEN').
mop_direct('FD-STREAM', szlot, 'MISC').
mop_direct('FD-STREAM', szlot, 'NAME').
mop_direct('FD-STREAM', szlot, 'OBUF').
mop_direct('FD-STREAM', szlot, 'ORIGINAL').
mop_direct('FD-STREAM', szlot, 'OUTPUT-BYTES').
mop_direct('FD-STREAM', szlot, 'OUTPUT-COLUMN').
mop_direct('FD-STREAM', szlot, 'OUTPUT-QUEUE').
mop_direct('FD-STREAM', szlot, 'PATHNAME').
mop_direct('FD-STREAM', szlot, 'SERVE-EVENTS').
mop_direct('FD-STREAM', szlot, 'TIMEOUT').
mop_direct('FDEFN', supers, [t]).
mop_direct('FFI-MODULE', supers, [cl_structure_object, t]).
mop_direct('FFI-MODULE', szlot, 'C-NAME').
mop_direct('FFI-MODULE', szlot, 'CONSTANT-TABLE').
mop_direct('FFI-MODULE', szlot, 'FINI').
mop_direct('FFI-MODULE', szlot, 'FUNCTION-LIST').
mop_direct('FFI-MODULE', szlot, 'INIT-ALWAYS').
mop_direct('FFI-MODULE', szlot, 'INIT-ONCE').
mop_direct('FFI-MODULE', szlot, 'NAME').
mop_direct('FFI-MODULE', szlot, 'OBJECT-TABLE').
mop_direct('FFI-MODULE', szlot, 'TYPE-TABLE').
mop_direct('FFI-MODULE', szlot, 'VARIABLE-LIST').
mop_direct('FGEN', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FGEN', szlot, 'GENERATOR').
mop_direct('FGEN', szlot, 'GENERATOR-LAMBDA').
mop_direct('FGEN', szlot, 'GENSYMS').
mop_direct('FGEN', szlot, 'SYSTEM').
mop_direct(cl_file_error, submop, 'SIMPLE-FILE-ERROR').
mop_direct(cl_file_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_file_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_file_error, szlot, '$PATHNAME').
mop_direct(cl_file_error, szlot, 'PATHNAME').
mop_direct('FILE-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FILE-INFO', supers, [cl_structure_object, t]).
mop_direct('FILE-INFO', szlot, 'ATIME').
mop_direct('FILE-INFO', szlot, 'ATTRIBUTES').
mop_direct('FILE-INFO', szlot, 'CTIME').
mop_direct('FILE-INFO', szlot, 'EXTERNAL-FORMAT').
mop_direct('FILE-INFO', szlot, 'FORMS').
mop_direct('FILE-INFO', szlot, 'NAME').
mop_direct('FILE-INFO', szlot, 'NAME-SHORT').
mop_direct('FILE-INFO', szlot, 'POSITIONS').
mop_direct('FILE-INFO', szlot, 'SIZE').
mop_direct('FILE-INFO', szlot, 'SOURCE-ROOT').
mop_direct('FILE-INFO', szlot, 'STYLE-WARNING-TRACKER').
mop_direct('FILE-INFO', szlot, 'SUBFORMS').
mop_direct('FILE-INFO', szlot, 'UNTRUENAME').
mop_direct('FILE-INFO', szlot, 'WRITE-DATE').
mop_direct('FILE-INFO', szlot, 'WTIME').
mop_direct('FILE-STAT', supers, [cl_structure_object, t]).
mop_direct('FILE-STAT', szlot, 'ATIME').
mop_direct('FILE-STAT', szlot, 'BLKSIZE').
mop_direct('FILE-STAT', szlot, 'BLOCKS').
mop_direct('FILE-STAT', szlot, 'CTIME').
mop_direct('FILE-STAT', szlot, 'DEV').
mop_direct('FILE-STAT', szlot, 'FILE').
mop_direct('FILE-STAT', szlot, 'GID').
mop_direct('FILE-STAT', szlot, 'INO').
mop_direct('FILE-STAT', szlot, 'MODE').
mop_direct('FILE-STAT', szlot, 'MTIME').
mop_direct('FILE-STAT', szlot, 'NLINK').
mop_direct('FILE-STAT', szlot, 'RDEV').
mop_direct('FILE-STAT', szlot, 'SIZE').
mop_direct('FILE-STAT', szlot, 'UID').
mop_direct(cl_file_stream, submop, 'FD-STREAM').
mop_direct(cl_file_stream, supers, [cl_stream, t]).
mop_direct(cl_fill_pointer_output_stream, supers, ['STRING-STREAM', 'ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_fill_pointer_output_stream, szlot, 'MISC').
mop_direct(cl_fill_pointer_output_stream, szlot, 'OUT').
mop_direct(cl_fill_pointer_output_stream, szlot, 'SOUT').
mop_direct(cl_fill_pointer_output_stream, szlot, 'STRING').
mop_direct('FILL-STREAM', submop, 'DESCRIBE-STREAM').
mop_direct('FILL-STREAM', supers, ['FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', 'FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FILL-STREAM', szlot, 'BUFFER').
mop_direct('FILL-STREAM', szlot, 'CURRENT-INDENT').
mop_direct('FILL-STREAM', szlot, 'INDENT-VAR').
mop_direct('FILL-STREAM', szlot, 'INSIDE-SEXP').
mop_direct('FILL-STREAM', szlot, 'PENDING-INDENT').
mop_direct('FILL-STREAM', szlot, 'PENDING-SPACE').
mop_direct('FILL-STREAM', szlot, 'SEXP-INDENT').
mop_direct('FILL-STREAM', szlot, 'TARGET-STREAM').
mop_direct('FILLER', supers, ['ANNOTATION', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FILLER', szlot, 'BYTES').
mop_direct('FINAL-DEPRECATION-WARNING', supers, [cl_warning, 'DEPRECATION-CONDITION', 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FIND-METHOD-LENGTH-MISMATCH', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FINITE-SB', supers, ['SB', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FINITE-SB', szlot, 'ALWAYS-LIVE').
mop_direct('FINITE-SB', szlot, 'ALWAYS-LIVE-COUNT').
mop_direct('FINITE-SB', szlot, 'CONFLICTS').
mop_direct('FINITE-SB', szlot, 'CURRENT-SIZE').
mop_direct('FINITE-SB', szlot, 'LAST-BLOCK-COUNT').
mop_direct('FINITE-SB', szlot, 'LAST-OFFSET').
mop_direct('FINITE-SB', szlot, 'LIVE-TNS').
mop_direct('FINITE-SB', szlot, 'SIZE-ALIGNMENT').
mop_direct('FINITE-SB', szlot, 'SIZE-INCREMENT').
mop_direct(cl_fixnum, supers, ['INTEGER', 'RATIONAL', 'REAL', 'NUMBER', t]).
mop_direct('FIXUP', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FIXUP', szlot, 'FLAVOR').
mop_direct('FIXUP', szlot, 'NAME').
mop_direct('FIXUP', szlot, 'OFFSET').
mop_direct('FIXUP-NOTE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FIXUP-NOTE', szlot, 'FIXUP').
mop_direct('FIXUP-NOTE', szlot, 'KIND').
mop_direct('FIXUP-NOTE', szlot, 'POSITION').
mop_direct('FLOAT', submop, cl_double_float).
mop_direct('FLOAT', submop, cl_single_float).
mop_direct('FLOAT', supers, ['REAL', 'NUMBER', t]).
mop_direct('FLOATING-POINT-EXCEPTION', supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FLOATING-POINT-EXCEPTION', szlot, 'FLAGS').
mop_direct(cl_floating_point_inexact, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_floating_point_inexact, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_floating_point_invalid_operation, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_floating_point_invalid_operation, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_floating_point_overflow, submop, 'SIMPLE-FLOATING-POINT-OVERFLOW').
mop_direct(cl_floating_point_overflow, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_floating_point_overflow, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_floating_point_underflow, submop, 'SIMPLE-FLOATING-POINT-UNDERFLOW').
mop_direct(cl_floating_point_underflow, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_floating_point_underflow, supers, [cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('FNODE', supers, [cl_structure_object, t]).
mop_direct('FNODE', szlot, 'ALLOW-OTHER-KEYS-FLAG').
mop_direct('FNODE', szlot, 'BLOCKS').
mop_direct('FNODE', szlot, 'BLOCKS-OFFSET').
mop_direct('FNODE', szlot, 'CODE').
mop_direct('FNODE', szlot, 'CONSTS').
mop_direct('FNODE', szlot, 'CONSTS-FORMS').
mop_direct('FNODE', szlot, 'CONSTS-LTV-FORMS').
mop_direct('FNODE', szlot, 'CONSTS-OFFSET').
mop_direct('FNODE', szlot, 'DENV').
mop_direct('FNODE', szlot, 'DOCUMENTATION').
mop_direct('FNODE', szlot, 'ENCLOSING').
mop_direct('FNODE', szlot, 'FAR-ASSIGNED-VARS').
mop_direct('FNODE', szlot, 'FAR-USED-BLOCKS').
mop_direct('FNODE', szlot, 'FAR-USED-TAGBODYS').
mop_direct('FNODE', szlot, 'FAR-USED-VARS').
mop_direct('FNODE', szlot, 'GF-P').
mop_direct('FNODE', szlot, 'IGNORABLE').
mop_direct('FNODE', szlot, 'IGNORE').
mop_direct('FNODE', szlot, 'KEYWORD-FLAG').
mop_direct('FNODE', szlot, 'KEYWORD-OFFSET').
mop_direct('FNODE', szlot, 'KEYWORDS').
mop_direct('FNODE', szlot, 'LAMBDA-LIST').
mop_direct('FNODE', szlot, 'NAME').
mop_direct('FNODE', szlot, 'OPT-NUM').
mop_direct('FNODE', szlot, 'REQ-NUM').
mop_direct('FNODE', szlot, 'REST-FLAG').
mop_direct('FNODE', szlot, 'TAGBODYS').
mop_direct('FNODE', szlot, 'TAGBODYS-OFFSET').
mop_direct('FNODE', szlot, 'TAGS').
mop_direct('FNODE', szlot, 'USED').
mop_direct('FNODE', szlot, 'VENVC').
mop_direct('FNODE', szlot, 'VENVCONST').
mop_direct('FOREIGN-THREAD', supers, ['THREAD', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FORM-TRACKING-STREAM', supers, ['FD-STREAM', cl_file_stream, 'ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FORM-TRACKING-STREAM', szlot, 'FORM-START-BYTE-POS').
mop_direct('FORM-TRACKING-STREAM', szlot, 'FORM-START-CHAR-POS').
mop_direct('FORM-TRACKING-STREAM', szlot, 'INPUT-CHAR-POS').
mop_direct('FORM-TRACKING-STREAM', szlot, 'LAST-NEWLINE').
mop_direct('FORM-TRACKING-STREAM', szlot, 'MISC').
mop_direct('FORM-TRACKING-STREAM', szlot, 'NEWLINES').
mop_direct('FORM-TRACKING-STREAM', szlot, 'OBSERVER').
mop_direct('FORMAT-ARGS-MISMATCH', submop, 'FORMAT-TOO-FEW-ARGS-WARNING').
mop_direct('FORMAT-ARGS-MISMATCH', submop, 'FORMAT-TOO-MANY-ARGS-WARNING').
mop_direct('FORMAT-ARGS-MISMATCH', supers, ['REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FORMAT-DIRECTIVE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FORMAT-DIRECTIVE', szlot, 'ATSIGNP').
mop_direct('FORMAT-DIRECTIVE', szlot, 'CHARACTER').
mop_direct('FORMAT-DIRECTIVE', szlot, 'COLONP').
mop_direct('FORMAT-DIRECTIVE', szlot, 'END').
mop_direct('FORMAT-DIRECTIVE', szlot, 'PARAMS').
mop_direct('FORMAT-DIRECTIVE', szlot, 'START').
mop_direct('FORMAT-DIRECTIVE', szlot, 'STRING').
mop_direct('FORMAT-ERROR', supers, ['ERROR', cl_serious_condition, 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FORMAT-ERROR', szlot, 'ARGS').
mop_direct('FORMAT-ERROR', szlot, 'COMPLAINT').
mop_direct('FORMAT-ERROR', szlot, 'CONTROL-STRING').
mop_direct('FORMAT-ERROR', szlot, 'OFFSET').
mop_direct('FORMAT-ERROR', szlot, 'PRINT-BANNER').
mop_direct('FORMAT-ERROR', szlot, 'SECOND-RELATIVE').
mop_direct('FORMAT-TOO-FEW-ARGS-WARNING', supers, ['FORMAT-ARGS-MISMATCH', 'REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FORMAT-TOO-MANY-ARGS-WARNING', supers, ['FORMAT-ARGS-MISMATCH', 'REFERENCE-CONDITION', 'SIMPLE-STYLE-WARNING', cl_simple_condition, cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FORWARD-REFERENCED-CLASS', submop, 'MISDESIGNED-FORWARD-REFERENCED-CLASS').
mop_direct('FORWARD-REFERENCED-CLASS', supers, ['PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('FORWARD-REFERENCED-CLASS', supers, ['SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('FRAME', submop, 'COMPILED-FRAME').
mop_direct('FRAME', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FRAME', szlot, '%CATCHES').
mop_direct('FRAME', szlot, '%DOWN').
mop_direct('FRAME', szlot, 'CODE-LOCATION').
mop_direct('FRAME', szlot, 'DEBUG-FUN').
mop_direct('FRAME', szlot, 'NUMBER').
mop_direct('FRAME', szlot, 'POINTER').
mop_direct('FRAME', szlot, 'UP').
mop_direct('FRAME-FUN-MISMATCH', supers, ['DEBUG-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FRAME-FUN-MISMATCH', szlot, 'CODE-LOCATION').
mop_direct('FRAME-FUN-MISMATCH', szlot, 'FORM').
mop_direct('FRAME-FUN-MISMATCH', szlot, 'FRAME').
mop_direct('FTYPE-PROCLAMATION-MISMATCH', submop, 'FTYPE-PROCLAMATION-MISMATCH-ERROR').
mop_direct('FTYPE-PROCLAMATION-MISMATCH', submop, 'FTYPE-PROCLAMATION-MISMATCH-WARNING').
mop_direct('FTYPE-PROCLAMATION-MISMATCH', supers, ['PROCLAMATION-MISMATCH', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FTYPE-PROCLAMATION-MISMATCH-ERROR', supers, ['ERROR', cl_serious_condition, 'FTYPE-PROCLAMATION-MISMATCH', 'PROCLAMATION-MISMATCH', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FTYPE-PROCLAMATION-MISMATCH-WARNING', supers, [cl_style_warning, cl_warning, 'FTYPE-PROCLAMATION-MISMATCH', 'PROCLAMATION-MISMATCH', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FUN-CACHE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FUN-CACHE', szlot, 'LABELLERS').
mop_direct('FUN-CACHE', szlot, 'PREFILTERS').
mop_direct('FUN-CACHE', szlot, 'PRINTERS').
mop_direct('FUN-CACHE', szlot, 'SERIAL-NUMBER').
mop_direct('FUN-END-COOKIE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FUN-END-COOKIE', szlot, 'BOGUS-LRA').
mop_direct('FUN-END-COOKIE', szlot, 'DEBUG-FUN').
mop_direct('FUN-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FUN-INFO', szlot, 'ATTRIBUTES').
mop_direct('FUN-INFO', szlot, 'CONSTRAINT-PROPAGATE').
mop_direct('FUN-INFO', szlot, 'CONSTRAINT-PROPAGATE-IF').
mop_direct('FUN-INFO', szlot, 'DERIVE-TYPE').
mop_direct('FUN-INFO', szlot, 'DESTROYED-CONSTANT-ARGS').
mop_direct('FUN-INFO', szlot, 'IR2-CONVERT').
mop_direct('FUN-INFO', szlot, 'LTN-ANNOTATE').
mop_direct('FUN-INFO', szlot, 'OPTIMIZER').
mop_direct('FUN-INFO', szlot, 'PREDICATE-TYPE').
mop_direct('FUN-INFO', szlot, 'RESULT-ARG').
mop_direct('FUN-INFO', szlot, 'STACK-ALLOCATE-RESULT').
mop_direct('FUN-INFO', szlot, 'TEMPLATES').
mop_direct('FUN-INFO', szlot, 'TRANSFORMS').
mop_direct('FUN-TYPE', supers, ['ARGS-TYPE', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FUN-TYPE', szlot, 'CLASS-INFO').
mop_direct('FUN-TYPE', szlot, 'RETURNS').
mop_direct('FUN-TYPE', szlot, 'WILD-ARGS').
mop_direct(cl_funcallable_standard_class, supers, ['SEMI-STANDARD-CLASS', 'SLOTTED-CLASS', 'CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct(cl_funcallable_standard_class, supers, ['STD-CLASS', cl_slot_class, 'PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_funcallable_standard_object, submop, 'GENERIC-FUNCTION').
mop_direct(cl_funcallable_standard_object, supers, [cl_function, cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_funcallable_standard_object, supers, [cl_function, cl_standard_object, t]).
mop_direct(cl_funcallable_standard_object, szlot, '$NAME').
mop_direct(cl_function, submop, '%METHOD-FUNCTION').
mop_direct(cl_function, submop, 'CTOR').
mop_direct(cl_function, submop, cl_funcallable_standard_object).
mop_direct(cl_function, submop, 'INTERPRETED-FUNCTION').
mop_direct(cl_function, submop, 'STANDARD-FUNCALLABLE-INSTANCE').
mop_direct(cl_function, supers, [t]).
mop_direct('FUNCTION-REDEFINITION-WARNING', submop, 'REDEFINITION-WITH-DEFMACRO').
mop_direct('FUNCTION-REDEFINITION-WARNING', submop, 'REDEFINITION-WITH-DEFUN').
mop_direct('FUNCTION-REDEFINITION-WARNING', supers, ['REDEFINITION-WARNING', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('FUNCTION-REDEFINITION-WARNING', szlot, 'NEW-FUNCTION').
mop_direct('FUNCTIONAL', submop, 'CLAMBDA').
mop_direct('FUNCTIONAL', submop, 'OPTIONAL-DISPATCH').
mop_direct('FUNCTIONAL', supers, ['LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FUNCTIONAL', szlot, '%DEBUG-NAME').
mop_direct('FUNCTIONAL', szlot, '%SOURCE-NAME').
mop_direct('FUNCTIONAL', szlot, 'ALLOCATOR').
mop_direct('FUNCTIONAL', szlot, 'ARG-DOCUMENTATION').
mop_direct('FUNCTIONAL', szlot, 'DOCUMENTATION').
mop_direct('FUNCTIONAL', szlot, 'ENTRY-FUN').
mop_direct('FUNCTIONAL', szlot, 'HAS-EXTERNAL-REFERENCES-P').
mop_direct('FUNCTIONAL', szlot, 'INLINE-EXPANDED').
mop_direct('FUNCTIONAL', szlot, 'INLINE-EXPANSION').
mop_direct('FUNCTIONAL', szlot, 'INLINEP').
mop_direct('FUNCTIONAL', szlot, 'KIND').
mop_direct('FUNCTIONAL', szlot, 'LEXENV').
mop_direct('FUNCTIONAL', szlot, 'PLIST').
mop_direct('FUNCTIONAL', szlot, 'TYPE').
mop_direct('FUNCTIONAL', szlot, 'WHERE-FROM').
mop_direct('FUNCTIONAL', szlot, 'XREF').
mop_direct('FUNDAMENTAL-BINARY-INPUT-STREAM', supers, ['FUNDAMENTAL-INPUT-STREAM', 'FUNDAMENTAL-BINARY-STREAM', 'FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-BINARY-INPUT-STREAM', supers, ['FUNDAMENTAL-INPUT-STREAM', 'FUNDAMENTAL-BINARY-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-BINARY-OUTPUT-STREAM', supers, ['FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-BINARY-STREAM', 'FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-BINARY-OUTPUT-STREAM', supers, ['FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-BINARY-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-BINARY-STREAM', submop, 'FUNDAMENTAL-BINARY-INPUT-STREAM').
mop_direct('FUNDAMENTAL-BINARY-STREAM', submop, 'FUNDAMENTAL-BINARY-OUTPUT-STREAM').
mop_direct('FUNDAMENTAL-BINARY-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-BINARY-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-CHARACTER-INPUT-STREAM', supers, ['FUNDAMENTAL-INPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-CHARACTER-INPUT-STREAM', supers, ['FUNDAMENTAL-INPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-CHARACTER-INPUT-STREAM', szlot, '$LASTCHAR').
mop_direct('FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', submop, 'FILL-STREAM').
mop_direct('FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', submop, 'HTML-STREAM-OUT').
mop_direct('FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', supers, ['FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', supers, ['FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-CHARACTER-STREAM', submop, 'FUNDAMENTAL-CHARACTER-INPUT-STREAM').
mop_direct('FUNDAMENTAL-CHARACTER-STREAM', submop, 'FUNDAMENTAL-CHARACTER-OUTPUT-STREAM').
mop_direct('FUNDAMENTAL-CHARACTER-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-CHARACTER-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-INPUT-STREAM', submop, 'FUNDAMENTAL-BINARY-INPUT-STREAM').
mop_direct('FUNDAMENTAL-INPUT-STREAM', submop, 'FUNDAMENTAL-CHARACTER-INPUT-STREAM').
mop_direct('FUNDAMENTAL-INPUT-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-INPUT-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-OUTPUT-STREAM', submop, 'FUNDAMENTAL-BINARY-OUTPUT-STREAM').
mop_direct('FUNDAMENTAL-OUTPUT-STREAM', submop, 'FUNDAMENTAL-CHARACTER-OUTPUT-STREAM').
mop_direct('FUNDAMENTAL-OUTPUT-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-OUTPUT-STREAM', supers, ['FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-STREAM', submop, 'FUNDAMENTAL-BINARY-STREAM').
mop_direct('FUNDAMENTAL-STREAM', submop, 'FUNDAMENTAL-CHARACTER-STREAM').
mop_direct('FUNDAMENTAL-STREAM', submop, 'FUNDAMENTAL-INPUT-STREAM').
mop_direct('FUNDAMENTAL-STREAM', submop, 'FUNDAMENTAL-OUTPUT-STREAM').
mop_direct('FUNDAMENTAL-STREAM', supers, [cl_standard_object, 'SLOT-OBJECT', cl_stream, t]).
mop_direct('FUNDAMENTAL-STREAM', supers, [cl_stream, cl_standard_object, t]).
mop_direct('FUNDAMENTAL-STREAM', szlot, '$FASL').
mop_direct('FUNDAMENTAL-STREAM', szlot, '$OPEN').
mop_direct('FUNDAMENTAL-STREAM', szlot, '$PENL').
mop_direct('FUNDAMENTAL-STREAM', szlot, 'OPEN-P').
mop_direct('FUNSTATE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('FUNSTATE', szlot, 'ARG-TEMPS').
mop_direct('FUNSTATE', szlot, 'ARGS').
mop_direct('GENERIC-FUNCTION', submop, 'STANDARD-GENERIC-FUNCTION').
mop_direct('GENERIC-FUNCTION', supers, ['DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METAOBJECT', cl_funcallable_standard_object, cl_function, cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('GENERIC-FUNCTION', supers, ['METAOBJECT', cl_funcallable_standard_object, cl_function, cl_standard_object, t]).
mop_direct('GENERIC-FUNCTION', szlot, '$LISTENERS').
mop_direct('GENERIC-FUNCTION', szlot, '%DOCUMENTATION').
mop_direct('GENERIC-FUNCTION', szlot, 'ENCAPSULATIONS').
mop_direct('GENERIC-FUNCTION', szlot, 'INITIAL-METHODS').
mop_direct('GENERIC-FUNCTION-LAMBDA-LIST-ERROR', supers, ['REFERENCE-CONDITION', 'SIMPLE-PROGRAM-ERROR', cl_simple_condition, cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('GENERIC-STREAM-CONTROLLER', supers, [cl_standard_object, t]).
mop_direct('GF-ALREADY-CALLED-WARNING', submop, 'SIMPLE-GF-ALREADY-CALLED-WARNING').
mop_direct('GF-ALREADY-CALLED-WARNING', supers, ['CLOS-WARNING', cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('GF-REPLACING-METHOD-WARNING', submop, 'SIMPLE-GF-REPLACING-METHOD-WARNING').
mop_direct('GF-REPLACING-METHOD-WARNING', supers, ['CLOS-WARNING', cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('GLOBAL-BOUNDP-METHOD', supers, ['ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('GLOBAL-CONFLICTS', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('GLOBAL-CONFLICTS', szlot, 'BLOCK').
mop_direct('GLOBAL-CONFLICTS', szlot, 'CONFLICTS').
mop_direct('GLOBAL-CONFLICTS', szlot, 'KIND').
mop_direct('GLOBAL-CONFLICTS', szlot, 'NEXT-BLOCKWISE').
mop_direct('GLOBAL-CONFLICTS', szlot, 'NEXT-TNWISE').
mop_direct('GLOBAL-CONFLICTS', szlot, 'NUMBER').
mop_direct('GLOBAL-CONFLICTS', szlot, 'TN').
mop_direct('GLOBAL-READER-METHOD', supers, ['ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('GLOBAL-VAR', submop, 'DEFINED-FUN').
mop_direct('GLOBAL-VAR', supers, ['BASIC-VAR', 'LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('GLOBAL-VAR', szlot, 'KIND').
mop_direct('GLOBAL-WRITER-METHOD', supers, ['ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('GROUP-INFO', supers, [cl_structure_object, t]).
mop_direct('GROUP-INFO', szlot, 'GID').
mop_direct('GROUP-INFO', szlot, 'MEMBERS').
mop_direct('GROUP-INFO', szlot, 'NAME').
mop_direct('HAIRY-TYPE', submop, 'UNKNOWN-TYPE').
mop_direct('HAIRY-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('HAIRY-TYPE', szlot, 'CLASS-INFO').
mop_direct('HAIRY-TYPE', szlot, 'SPECIFIER').
mop_direct('HANDLER', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('HANDLER', szlot, 'ACTIVE').
mop_direct('HANDLER', szlot, 'BOGUS').
mop_direct('HANDLER', szlot, 'DESCRIPTOR').
mop_direct('HANDLER', szlot, 'DIRECTION').
mop_direct('HANDLER', szlot, 'FUNCTION').
mop_direct(cl_hash_table, supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_hash_table, supers, [t]).
mop_direct(cl_hash_table, szlot, 'CACHE').
mop_direct(cl_hash_table, szlot, 'HASH-FUN').
mop_direct(cl_hash_table, szlot, 'HASH-VECTOR').
mop_direct(cl_hash_table, szlot, 'INDEX-VECTOR').
mop_direct(cl_hash_table, szlot, 'LOCK').
mop_direct(cl_hash_table, szlot, 'NEEDS-REHASH-P').
mop_direct(cl_hash_table, szlot, 'NEXT-FREE-KV').
mop_direct(cl_hash_table, szlot, 'NEXT-VECTOR').
mop_direct(cl_hash_table, szlot, 'NEXT-WEAK-HASH-TABLE').
mop_direct(cl_hash_table, szlot, 'NUMBER-ENTRIES').
mop_direct(cl_hash_table, szlot, 'REHASH-SIZE').
mop_direct(cl_hash_table, szlot, 'REHASH-THRESHOLD').
mop_direct(cl_hash_table, szlot, 'REHASH-TRIGGER').
mop_direct(cl_hash_table, szlot, 'SYNCHRONIZED-P').
mop_direct(cl_hash_table, szlot, 'TABLE').
mop_direct(cl_hash_table, szlot, 'TEST').
mop_direct(cl_hash_table, szlot, 'TEST-FUN').
mop_direct(cl_hash_table, szlot, 'WEAKNESS').
mop_direct('HEAP-ALIEN-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('HEAP-ALIEN-INFO', szlot, 'ALIEN-NAME').
mop_direct('HEAP-ALIEN-INFO', szlot, 'DATAP').
mop_direct('HEAP-ALIEN-INFO', szlot, 'TYPE').
mop_direct('HEAP-EXHAUSTED-ERROR', supers, [cl_storage_condition, cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('HOST', submop, 'LOGICAL-HOST').
mop_direct('HOST', submop, 'UNIX-HOST').
mop_direct('HOST', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('HOST', szlot, 'CUSTOMARY-CASE').
mop_direct('HOST', szlot, 'PARSE').
mop_direct('HOST', szlot, 'PARSE-NATIVE').
mop_direct('HOST', szlot, 'SIMPLIFY-NAMESTRING').
mop_direct('HOST', szlot, 'UNPARSE').
mop_direct('HOST', szlot, 'UNPARSE-DIRECTORY').
mop_direct('HOST', szlot, 'UNPARSE-DIRECTORY-SEPARATOR').
mop_direct('HOST', szlot, 'UNPARSE-ENOUGH').
mop_direct('HOST', szlot, 'UNPARSE-FILE').
mop_direct('HOST', szlot, 'UNPARSE-HOST').
mop_direct('HOST', szlot, 'UNPARSE-NATIVE').
mop_direct('HOSTENT', supers, [cl_structure_object, t]).
mop_direct('HOSTENT', szlot, 'ADDR-LIST').
mop_direct('HOSTENT', szlot, 'ADDRTYPE').
mop_direct('HOSTENT', szlot, 'ALIASES').
mop_direct('HOSTENT', szlot, 'NAME').
mop_direct('HTML-STREAM-OUT', supers, ['FUNDAMENTAL-CHARACTER-OUTPUT-STREAM', 'FUNDAMENTAL-OUTPUT-STREAM', 'FUNDAMENTAL-CHARACTER-STREAM', 'FUNDAMENTAL-STREAM', cl_stream, cl_standard_object, t]).
mop_direct('HTML-STREAM-OUT', szlot, 'TARGET-STREAM').
mop_direct('HUFFMAN-NODE', submop, 'HUFFMAN-PAIR').
mop_direct('HUFFMAN-NODE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('HUFFMAN-NODE', szlot, 'KEY').
mop_direct('HUFFMAN-NODE', szlot, 'WEIGHT').
mop_direct('HUFFMAN-PAIR', supers, ['HUFFMAN-NODE', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('HUFFMAN-PAIR', szlot, 'LEFT').
mop_direct('HUFFMAN-PAIR', szlot, 'RIGHT').
mop_direct('IMPLICIT-GENERIC-FUNCTION-WARNING', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('IMPLICIT-GENERIC-FUNCTION-WARNING', szlot, 'NAME').
mop_direct('INDENTATION', supers, ['QUEUED-OP', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INDENTATION', szlot, 'AMOUNT').
mop_direct('INDENTATION', szlot, 'KIND').
mop_direct('INDEX-TOO-LARGE-ERROR', supers, [cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INFO-HASHTABLE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INFO-HASHTABLE', szlot, 'COMPARATOR').
mop_direct('INFO-HASHTABLE', szlot, 'COUNT').
mop_direct('INFO-HASHTABLE', szlot, 'HASH-FUNCTION').
mop_direct('INFO-HASHTABLE', szlot, 'MUTEX').
mop_direct('INFO-HASHTABLE', szlot, 'STORAGE').
mop_direct('INITARG-ERROR', supers, ['REFERENCE-CONDITION', cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INITARG-ERROR', szlot, 'CLASS').
mop_direct('INITARG-ERROR', szlot, 'INITARGS').
mop_direct('INITIAL', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INLINING-DEPENDENCY-FAILURE', supers, ['SIMPLE-STYLE-WARNING', cl_simple_condition, cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INPUT-CHARACTER', supers, [cl_structure_object, t]).
mop_direct('INPUT-CHARACTER', szlot, 'BITS').
mop_direct('INPUT-CHARACTER', szlot, 'CHAR').
mop_direct('INPUT-CHARACTER', szlot, 'FONT').
mop_direct('INPUT-CHARACTER', szlot, 'KEY').
mop_direct('INPUT-ERROR-IN-COMPILE-FILE', submop, 'INPUT-ERROR-IN-LOAD').
mop_direct('INPUT-ERROR-IN-COMPILE-FILE', supers, [cl_reader_error, cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, 'ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INPUT-ERROR-IN-COMPILE-FILE', szlot, 'INVOKER').
mop_direct('INPUT-ERROR-IN-COMPILE-FILE', szlot, 'LINE/COL').
mop_direct('INPUT-ERROR-IN-COMPILE-FILE', szlot, 'POSITION').
mop_direct('INPUT-ERROR-IN-LOAD', supers, ['INPUT-ERROR-IN-COMPILE-FILE', cl_reader_error, cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, 'ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INSPECTION', supers, [cl_structure_object, t]).
mop_direct('INSPECTION', szlot, 'BLURB').
mop_direct('INSPECTION', szlot, 'ID').
mop_direct('INSPECTION', szlot, 'NTH-SLOT').
mop_direct('INSPECTION', szlot, 'NUM-SLOTS').
mop_direct('INSPECTION', szlot, 'POS').
mop_direct('INSPECTION', szlot, 'SELF').
mop_direct('INSPECTION', szlot, 'SET-SLOT').
mop_direct('INSPECTION', szlot, 'TITLE').
mop_direct('INSPECTION', szlot, 'UP').
mop_direct('INST-SPACE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INST-SPACE', szlot, 'CHOICES').
mop_direct('INST-SPACE', szlot, 'VALID-MASK').
mop_direct('INST-SPACE-CHOICE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INST-SPACE-CHOICE', szlot, 'COMMON-ID').
mop_direct('INST-SPACE-CHOICE', szlot, 'SUBSPACE').
mop_direct('INSTANCE-STRUCTURE-PROTOCOL-ERROR', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INSTANCE-STRUCTURE-PROTOCOL-ERROR', szlot, 'FUN').
mop_direct('INSTANCE-STRUCTURE-PROTOCOL-ERROR', szlot, 'SLOTD').
mop_direct('INSTRUCTION', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INSTRUCTION', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INSTRUCTION', szlot, 'ATTRIBUTES').
mop_direct('INSTRUCTION', szlot, 'CONTROL').
mop_direct('INSTRUCTION', szlot, 'DELAY').
mop_direct('INSTRUCTION', szlot, 'DEPTH').
mop_direct('INSTRUCTION', szlot, 'EMITTER').
mop_direct('INSTRUCTION', szlot, 'FORMAT-NAME').
mop_direct('INSTRUCTION', szlot, 'ID').
mop_direct('INSTRUCTION', szlot, 'LABELLER').
mop_direct('INSTRUCTION', szlot, 'LENGTH').
mop_direct('INSTRUCTION', szlot, 'MASK').
mop_direct('INSTRUCTION', szlot, 'NAME').
mop_direct('INSTRUCTION', szlot, 'PREFILTER').
mop_direct('INSTRUCTION', szlot, 'PRINT-NAME').
mop_direct('INSTRUCTION', szlot, 'PRINTER').
mop_direct('INSTRUCTION', szlot, 'READ-DEPENDENCIES').
mop_direct('INSTRUCTION', szlot, 'READ-DEPENDENTS').
mop_direct('INSTRUCTION', szlot, 'SPECIALIZERS').
mop_direct('INSTRUCTION', szlot, 'WRITE-DEPENDENCIES').
mop_direct('INSTRUCTION', szlot, 'WRITE-DEPENDENTS').
mop_direct('INSTRUCTION-FORMAT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INSTRUCTION-FORMAT', szlot, 'ARGS').
mop_direct('INSTRUCTION-FORMAT', szlot, 'DEFAULT-PRINTER').
mop_direct('INSTRUCTION-FORMAT', szlot, 'LENGTH').
mop_direct('INSTRUCTION-FORMAT', szlot, 'NAME').
mop_direct('INTEGER', submop, cl_bignum).
mop_direct('INTEGER', submop, cl_fixnum).
mop_direct('INTEGER', supers, ['RATIONAL', 'REAL', 'NUMBER', t]).
mop_direct('INTERACTIVE-INTERRUPT', supers, ['SYSTEM-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INTERFERENCE-GRAPH', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INTERFERENCE-GRAPH', szlot, 'PRECOLORED-VERTICES').
mop_direct('INTERFERENCE-GRAPH', szlot, 'TN-VERTEX').
mop_direct('INTERFERENCE-GRAPH', szlot, 'TN-VERTEX-MAPPING').
mop_direct('INTERFERENCE-GRAPH', szlot, 'VERTICES').
mop_direct('INTERPRETED-FUNCTION', supers, [t]).
mop_direct('INTERPRETED-FUNCTION', szlot, 'BODY').
mop_direct('INTERPRETED-FUNCTION', szlot, 'DEBUG-LAMBDA-LIST').
mop_direct('INTERPRETED-FUNCTION', szlot, 'DEBUG-NAME').
mop_direct('INTERPRETED-FUNCTION', szlot, 'DECLARATIONS').
mop_direct('INTERPRETED-FUNCTION', szlot, 'DOCUMENTATION').
mop_direct('INTERPRETED-FUNCTION', szlot, 'ENV').
mop_direct('INTERPRETED-FUNCTION', szlot, 'LAMBDA-LIST').
mop_direct('INTERPRETED-FUNCTION', szlot, 'NAME').
mop_direct('INTERPRETED-FUNCTION', szlot, 'SOURCE-LOCATION').
mop_direct('INTERPRETED-PROGRAM-ERROR', supers, [cl_program_error, 'ERROR', cl_serious_condition, 'ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INTERPRETED-PROGRAM-ERROR', supers, [cl_program_error, 'ERROR', cl_serious_condition, cl_simple_condition, 'ENCAPSULATED-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INTERPRETED-PROGRAM-ERROR', szlot, 'FORM').
mop_direct('INTERPRETER-ENVIRONMENT-TOO-COMPLEX-ERROR', supers, [cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INTERRUPT-CONDITION', submop, 'SIMPLE-INTERRUPT-CONDITION').
mop_direct('INTERRUPT-CONDITION', supers, [cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('INTERRUPT-THREAD-ERROR', supers, ['THREAD-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INTERSECTION-TYPE', supers, ['COMPOUND-TYPE', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INTERSECTION-TYPE', szlot, 'CLASS-INFO').
mop_direct('INTERVAL', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('INTERVAL', szlot, 'HIGH').
mop_direct('INTERVAL', szlot, 'LOW').
mop_direct('INVALID-ARRAY-ERROR', supers, ['REFERENCE-CONDITION', cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-ARRAY-INDEX-ERROR', supers, [cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-ARRAY-INDEX-ERROR', szlot, 'ARRAY').
mop_direct('INVALID-ARRAY-INDEX-ERROR', szlot, 'AXIS').
mop_direct('INVALID-CONTROL-STACK-POINTER', supers, ['DEBUG-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-EUC-JP-CONTINUATION-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-EUC-JP-STARTER-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-FASL', submop, 'FASL-HEADER-MISSING').
mop_direct('INVALID-FASL', submop, 'INVALID-FASL-FEATURES').
mop_direct('INVALID-FASL', submop, 'INVALID-FASL-HEADER').
mop_direct('INVALID-FASL', submop, 'INVALID-FASL-IMPLEMENTATION').
mop_direct('INVALID-FASL', submop, 'INVALID-FASL-VERSION').
mop_direct('INVALID-FASL', supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-FASL', szlot, 'EXPECTED').
mop_direct('INVALID-FASL', szlot, 'STREAM').
mop_direct('INVALID-FASL-FEATURES', supers, ['INVALID-FASL', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-FASL-FEATURES', szlot, 'FEATURES').
mop_direct('INVALID-FASL-FEATURES', szlot, 'POTENTIAL-FEATURES').
mop_direct('INVALID-FASL-HEADER', supers, ['INVALID-FASL', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-FASL-HEADER', szlot, 'BYTE').
mop_direct('INVALID-FASL-HEADER', szlot, 'BYTE-NR').
mop_direct('INVALID-FASL-IMPLEMENTATION', supers, ['INVALID-FASL', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-FASL-IMPLEMENTATION', szlot, 'IMPLEMENTATION').
mop_direct('INVALID-FASL-VERSION', supers, ['INVALID-FASL', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-FASL-VERSION', szlot, 'VERSION').
mop_direct('INVALID-GBK-CONTINUATION-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-GBK-STARTER-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-METHOD-INITARG', supers, ['SIMPLE-PROGRAM-ERROR', cl_simple_condition, cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-METHOD-INITARG', szlot, 'METHOD').
mop_direct('INVALID-SHIFT_JIS-CONTINUATION-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-SHIFT_JIS-STARTER-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-SUPERCLASS', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-SUPERCLASS', szlot, 'CLASS').
mop_direct('INVALID-SUPERCLASS', szlot, 'SUPERCLASS').
mop_direct('INVALID-UTF8-CONTINUATION-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-UTF8-STARTER-BYTE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-VALUE', supers, ['DEBUG-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('INVALID-VALUE', szlot, 'DEBUG-VAR').
mop_direct('INVALID-VALUE', szlot, 'FRAME').
mop_direct('IO-TIMEOUT', supers, [cl_stream_error, 'ERROR', 'TIMEOUT', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('IO-TIMEOUT', szlot, 'DIRECTION').
mop_direct('IR2-BLOCK', supers, ['BLOCK-ANNOTATION', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('IR2-BLOCK', szlot, '%LABEL').
mop_direct('IR2-BLOCK', szlot, '%TRAMPOLINE-LABEL').
mop_direct('IR2-BLOCK', szlot, 'DROPPED-THRU-TO').
mop_direct('IR2-BLOCK', szlot, 'END-STACK').
mop_direct('IR2-BLOCK', szlot, 'GLOBAL-TNS').
mop_direct('IR2-BLOCK', szlot, 'LAST-VOP').
mop_direct('IR2-BLOCK', szlot, 'LIVE-IN').
mop_direct('IR2-BLOCK', szlot, 'LIVE-OUT').
mop_direct('IR2-BLOCK', szlot, 'LOCAL-TN-COUNT').
mop_direct('IR2-BLOCK', szlot, 'LOCAL-TNS').
mop_direct('IR2-BLOCK', szlot, 'LOCATIONS').
mop_direct('IR2-BLOCK', szlot, 'NUMBER').
mop_direct('IR2-BLOCK', szlot, 'POPPED').
mop_direct('IR2-BLOCK', szlot, 'PUSHED').
mop_direct('IR2-BLOCK', szlot, 'START-STACK').
mop_direct('IR2-BLOCK', szlot, 'START-VOP').
mop_direct('IR2-BLOCK', szlot, 'WRITTEN').
mop_direct('IR2-COMPONENT', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('IR2-COMPONENT', szlot, 'ALIAS-TNS').
mop_direct('IR2-COMPONENT', szlot, 'COMPONENT-TNS').
mop_direct('IR2-COMPONENT', szlot, 'CONSTANT-TNS').
mop_direct('IR2-COMPONENT', szlot, 'CONSTANTS').
mop_direct('IR2-COMPONENT', szlot, 'ENTRIES').
mop_direct('IR2-COMPONENT', szlot, 'FORMAT').
mop_direct('IR2-COMPONENT', szlot, 'GLOBAL-TN-COUNTER').
mop_direct('IR2-COMPONENT', szlot, 'NFP').
mop_direct('IR2-COMPONENT', szlot, 'NORMAL-TNS').
mop_direct('IR2-COMPONENT', szlot, 'RESTRICTED-TNS').
mop_direct('IR2-COMPONENT', szlot, 'SPECIFIED-SAVE-TNS').
mop_direct('IR2-COMPONENT', szlot, 'SPILLED-TNS').
mop_direct('IR2-COMPONENT', szlot, 'SPILLED-VOPS').
mop_direct('IR2-COMPONENT', szlot, 'VALUES-RECEIVERS').
mop_direct('IR2-COMPONENT', szlot, 'WIRED-TNS').
mop_direct('IR2-LVAR', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('IR2-LVAR', szlot, 'KIND').
mop_direct('IR2-LVAR', szlot, 'LOCS').
mop_direct('IR2-LVAR', szlot, 'PRIMITIVE-TYPE').
mop_direct('IR2-LVAR', szlot, 'STACK-POINTER').
mop_direct('IR2-NLX-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('IR2-NLX-INFO', szlot, 'DYNAMIC-STATE').
mop_direct('IR2-NLX-INFO', szlot, 'HOME').
mop_direct('IR2-NLX-INFO', szlot, 'SAVE-SP').
mop_direct('IR2-NLX-INFO', szlot, 'TARGET').
mop_direct('IR2-PHYSENV', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('IR2-PHYSENV', szlot, 'BSP-SAVE-TN').
mop_direct('IR2-PHYSENV', szlot, 'CLOSURE').
mop_direct('IR2-PHYSENV', szlot, 'CLOSURE-SAVE-TN').
mop_direct('IR2-PHYSENV', szlot, 'DEBUG-LIVE-TNS').
mop_direct('IR2-PHYSENV', szlot, 'ELSEWHERE-START').
mop_direct('IR2-PHYSENV', szlot, 'ENVIRONMENT-START').
mop_direct('IR2-PHYSENV', szlot, 'LIVE-TNS').
mop_direct('IR2-PHYSENV', szlot, 'NUMBER-STACK-P').
mop_direct('IR2-PHYSENV', szlot, 'OLD-FP').
mop_direct('IR2-PHYSENV', szlot, 'RETURN-PC').
mop_direct('IR2-PHYSENV', szlot, 'RETURN-PC-PASS').
mop_direct('JOIN-THREAD-ERROR', supers, ['THREAD-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('JOIN-THREAD-ERROR', szlot, 'PROBLEM').
mop_direct('KEY-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('KEY-INFO', szlot, 'NAME').
mop_direct('KEY-INFO', szlot, 'TYPE').
mop_direct('KEYWORD-ERROR', submop, 'SIMPLE-KEYWORD-ERROR').
mop_direct('KEYWORD-ERROR', supers, [cl_program_error, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('LABEL', supers, ['ANNOTATION', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LAMBDA-LIST-UNAVAILABLE', supers, ['DEBUG-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('LAMBDA-LIST-UNAVAILABLE', szlot, 'DEBUG-FUN').
mop_direct('LAMBDA-VAR', supers, ['BASIC-VAR', 'LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LAMBDA-VAR', szlot, 'ARG-INFO').
mop_direct('LAMBDA-VAR', szlot, 'CONSTRAINTS').
mop_direct('LAMBDA-VAR', szlot, 'CTYPE-CONSTRAINTS').
mop_direct('LAMBDA-VAR', szlot, 'EQ-CONSTRAINTS').
mop_direct('LAMBDA-VAR', szlot, 'EQL-VAR-CONSTRAINTS').
mop_direct('LAMBDA-VAR', szlot, 'FLAGS').
mop_direct('LAMBDA-VAR', szlot, 'FOP-VALUE').
mop_direct('LAMBDA-VAR', szlot, 'HOME').
mop_direct('LAMBDA-VAR', szlot, 'INHERITABLE-CONSTRAINTS').
mop_direct('LAMBDA-VAR', szlot, 'LAST-INITIAL-TYPE').
mop_direct('LAMBDA-VAR', szlot, 'PRIVATE-CONSTRAINTS').
mop_direct('LAMBDA-VAR', szlot, 'SPECVAR').
mop_direct('LATE-DEPRECATION-WARNING', supers, [cl_warning, 'DEPRECATION-CONDITION', 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_layout, supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_layout, szlot, '%FOR-STD-CLASS-B').
mop_direct(cl_layout, szlot, 'CLASSOID').
mop_direct(cl_layout, szlot, 'CLOS-HASH').
mop_direct(cl_layout, szlot, 'DEPTHOID').
mop_direct(cl_layout, szlot, 'EQUALP-TESTS').
mop_direct(cl_layout, szlot, 'INFO').
mop_direct(cl_layout, szlot, 'INHERITS').
mop_direct(cl_layout, szlot, 'INVALID').
mop_direct(cl_layout, szlot, 'LENGTH').
mop_direct(cl_layout, szlot, 'PURE').
mop_direct(cl_layout, szlot, 'SLOT-LIST').
mop_direct(cl_layout, szlot, 'SLOT-TABLE').
mop_direct(cl_layout, szlot, 'SOURCE-LOCATION').
mop_direct(cl_layout, szlot, 'UNTAGGED-BITMAP').
mop_direct('LAYOUT-INVALID', supers, [cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('LEAF', submop, 'BASIC-VAR').
mop_direct('LEAF', submop, 'CONSTANT').
mop_direct('LEAF', submop, 'FUNCTIONAL').
mop_direct('LEAF', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LEAF', szlot, '%SOURCE-NAME').
mop_direct('LEAF', szlot, 'DEFINED-TYPE').
mop_direct('LEAF', szlot, 'EVER-USED').
mop_direct('LEAF', szlot, 'EXTENT').
mop_direct('LEAF', szlot, 'INFO').
mop_direct('LEAF', szlot, 'NUMBER').
mop_direct('LEAF', szlot, 'REFS').
mop_direct('LEAF', szlot, 'TYPE').
mop_direct('LEAF', szlot, 'WHERE-FROM').
mop_direct('LEXENV', supers, ['ABSTRACT-LEXENV', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LEXENV', szlot, '%POLICY').
mop_direct('LEXENV', szlot, 'BLOCKS').
mop_direct('LEXENV', szlot, 'CLEANUP').
mop_direct('LEXENV', szlot, 'DISABLED-PACKAGE-LOCKS').
mop_direct('LEXENV', szlot, 'FUNS').
mop_direct('LEXENV', szlot, 'HANDLED-CONDITIONS').
mop_direct('LEXENV', szlot, 'LAMBDA').
mop_direct('LEXENV', szlot, 'PARENT').
mop_direct('LEXENV', szlot, 'TAGS').
mop_direct('LEXENV', szlot, 'TYPE-RESTRICTIONS').
mop_direct('LEXENV', szlot, 'USER-DATA').
mop_direct('LEXENV', szlot, 'VARS').
mop_direct('LEXICAL-ENVIRONMENT-TOO-COMPLEX', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('LEXICAL-ENVIRONMENT-TOO-COMPLEX', szlot, 'FORM').
mop_direct('LEXICAL-ENVIRONMENT-TOO-COMPLEX', szlot, 'LEXENV').
mop_direct(cl_list, submop, cl_cons).
mop_direct(cl_list, submop, 'NULL').
mop_direct(cl_list, supers, ['SEQUENCE', t]).
mop_direct('LOCAL-ALIEN-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOCAL-ALIEN-INFO', szlot, 'FORCE-TO-MEMORY-P').
mop_direct('LOCAL-ALIEN-INFO', szlot, 'TYPE').
mop_direct('LOCAL-ARGUMENT-MISMATCH', supers, ['REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('LOCALE-CONV', supers, [cl_structure_object, t]).
mop_direct('LOCALE-CONV', szlot, 'CURRENCY_SYMBOL').
mop_direct('LOCALE-CONV', szlot, 'DECIMAL_POINT').
mop_direct('LOCALE-CONV', szlot, 'FRAC_DIGITS').
mop_direct('LOCALE-CONV', szlot, 'GROUPING').
mop_direct('LOCALE-CONV', szlot, 'INT_CURR_SYMBOL').
mop_direct('LOCALE-CONV', szlot, 'INT_FRAC_DIGITS').
mop_direct('LOCALE-CONV', szlot, 'INT_N_CS_PRECEDES').
mop_direct('LOCALE-CONV', szlot, 'INT_N_SEP_BY_SPACE').
mop_direct('LOCALE-CONV', szlot, 'INT_N_SIGN_POSN').
mop_direct('LOCALE-CONV', szlot, 'INT_P_CS_PRECEDES').
mop_direct('LOCALE-CONV', szlot, 'INT_P_SEP_BY_SPACE').
mop_direct('LOCALE-CONV', szlot, 'INT_P_SIGN_POSN').
mop_direct('LOCALE-CONV', szlot, 'MON_DECIMAL_POINT').
mop_direct('LOCALE-CONV', szlot, 'MON_GROUPING').
mop_direct('LOCALE-CONV', szlot, 'MON_THOUSANDS_SEP').
mop_direct('LOCALE-CONV', szlot, 'N_CS_PRECEDES').
mop_direct('LOCALE-CONV', szlot, 'N_SEP_BY_SPACE').
mop_direct('LOCALE-CONV', szlot, 'N_SIGN_POSN').
mop_direct('LOCALE-CONV', szlot, 'NEGATIVE_SIGN').
mop_direct('LOCALE-CONV', szlot, 'P_CS_PRECEDES').
mop_direct('LOCALE-CONV', szlot, 'P_SEP_BY_SPACE').
mop_direct('LOCALE-CONV', szlot, 'P_SIGN_POSN').
mop_direct('LOCALE-CONV', szlot, 'POSITIVE_SIGN').
mop_direct('LOCALE-CONV', szlot, 'THOUSANDS_SEP').
mop_direct('LOCATION-GROUP', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOCATION-GROUP', szlot, 'LOCATIONS').
mop_direct('LOCATION-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOCATION-INFO', szlot, 'KIND').
mop_direct('LOCATION-INFO', szlot, 'LABEL').
mop_direct('LOCATION-INFO', szlot, 'VOP').
mop_direct('LOGICAL-BLOCK', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOGICAL-BLOCK', szlot, 'PER-LINE-PREFIX-END').
mop_direct('LOGICAL-BLOCK', szlot, 'PREFIX-LENGTH').
mop_direct('LOGICAL-BLOCK', szlot, 'SECTION-COLUMN').
mop_direct('LOGICAL-BLOCK', szlot, 'SECTION-START-LINE').
mop_direct('LOGICAL-BLOCK', szlot, 'START-COLUMN').
mop_direct('LOGICAL-BLOCK', szlot, 'SUFFIX-LENGTH').
mop_direct('LOGICAL-HOST', supers, ['HOST', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOGICAL-HOST', szlot, 'CANON-TRANSLS').
mop_direct('LOGICAL-HOST', szlot, 'CUSTOMARY-CASE').
mop_direct('LOGICAL-HOST', szlot, 'NAME').
mop_direct('LOGICAL-HOST', szlot, 'PARSE').
mop_direct('LOGICAL-HOST', szlot, 'PARSE-NATIVE').
mop_direct('LOGICAL-HOST', szlot, 'SIMPLIFY-NAMESTRING').
mop_direct('LOGICAL-HOST', szlot, 'TRANSLATIONS').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE-DIRECTORY').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE-DIRECTORY-SEPARATOR').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE-ENOUGH').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE-FILE').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE-HOST').
mop_direct('LOGICAL-HOST', szlot, 'UNPARSE-NATIVE').
mop_direct(cl_logical_pathname, supers, [cl_pathname, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_logical_pathname, supers, [cl_pathname, t]).
mop_direct('LONG-METHOD-COMBINATION', supers, ['STANDARD-METHOD-COMBINATION', 'DEFINITION-SOURCE-MIXIN', 'METHOD-COMBINATION', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('LONG-METHOD-COMBINATION', szlot, 'ARGS-LAMBDA-LIST').
mop_direct('LONG-METHOD-COMBINATION', szlot, 'FUNCTION').
mop_direct('LONG-METHOD-COMBINATION-ERROR', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('LOOP-COLLECTOR', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOOP-COLLECTOR', szlot, 'CLASS').
mop_direct('LOOP-COLLECTOR', szlot, 'DATA').
mop_direct('LOOP-COLLECTOR', szlot, 'DTYPE').
mop_direct('LOOP-COLLECTOR', szlot, 'HISTORY').
mop_direct('LOOP-COLLECTOR', szlot, 'NAME').
mop_direct('LOOP-COLLECTOR', szlot, 'SPECIFIED-TYPE').
mop_direct('LOOP-COLLECTOR', szlot, 'TEMPVARS').
mop_direct('LOOP-INITIALIZATION', supers, [cl_structure_object, t]).
mop_direct('LOOP-INITIALIZATION', szlot, 'BINDINGS').
mop_direct('LOOP-INITIALIZATION', szlot, 'DECLSPECS').
mop_direct('LOOP-INITIALIZATION', szlot, 'DEPENDS-PRECEDING').
mop_direct('LOOP-INITIALIZATION', szlot, 'ENDTEST-FORMS').
mop_direct('LOOP-INITIALIZATION', szlot, 'EVERYTIME').
mop_direct('LOOP-INITIALIZATION', szlot, 'LATER-DEPEND').
mop_direct('LOOP-INITIALIZATION', szlot, 'PREAMBLE').
mop_direct('LOOP-INITIALIZATION', szlot, 'REQUIRES-STEPBEFORE').
mop_direct('LOOP-INITIALIZATION', szlot, 'SPECFORM').
mop_direct('LOOP-MINIMAX', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOOP-MINIMAX', szlot, 'ANSWER-VARIABLE').
mop_direct('LOOP-MINIMAX', szlot, 'FLAG-VARIABLE').
mop_direct('LOOP-MINIMAX', szlot, 'INFINITY-DATA').
mop_direct('LOOP-MINIMAX', szlot, 'OPERATIONS').
mop_direct('LOOP-MINIMAX', szlot, 'TEMP-VARIABLE').
mop_direct('LOOP-MINIMAX', szlot, 'TYPE').
mop_direct('LOOP-PATH', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOOP-PATH', szlot, 'FUNCTION').
mop_direct('LOOP-PATH', szlot, 'INCLUSIVE-PERMITTED').
mop_direct('LOOP-PATH', szlot, 'NAMES').
mop_direct('LOOP-PATH', szlot, 'PREPOSITION-GROUPS').
mop_direct('LOOP-PATH', szlot, 'USER-DATA').
mop_direct('LOOP-UNIVERSE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LOOP-UNIVERSE', szlot, 'FOR-KEYWORDS').
mop_direct('LOOP-UNIVERSE', szlot, 'ITERATION-KEYWORDS').
mop_direct('LOOP-UNIVERSE', szlot, 'KEYWORDS').
mop_direct('LOOP-UNIVERSE', szlot, 'PATH-KEYWORDS').
mop_direct('LOOP-UNIVERSE', szlot, 'TYPE-KEYWORDS').
mop_direct('LOOP-UNIVERSE', szlot, 'TYPE-SYMBOLS').
mop_direct('LRA', supers, [t]).
mop_direct('LVAR', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('LVAR', szlot, '%DERIVED-TYPE').
mop_direct('LVAR', szlot, '%EXTERNALLY-CHECKABLE-TYPE').
mop_direct('LVAR', szlot, 'DEST').
mop_direct('LVAR', szlot, 'DYNAMIC-EXTENT').
mop_direct('LVAR', szlot, 'INFO').
mop_direct('LVAR', szlot, 'REOPTIMIZE').
mop_direct('LVAR', szlot, 'USES').
mop_direct('MACROEXPAND-HOOK-TYPE-ERROR', supers, [cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('MALFORMED-ASCII', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('MALFORMED-EUC-JP', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('MALFORMED-GBK', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('MALFORMED-SHIFT_JIS', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('MATCH', supers, [cl_structure_object, t]).
mop_direct('MATCH', szlot, 'END').
mop_direct('MATCH', szlot, 'START').
mop_direct('MEMBER-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('MEMBER-TYPE', szlot, 'CLASS-INFO').
mop_direct('MEMBER-TYPE', szlot, 'FP-ZEROES').
mop_direct('MEMBER-TYPE', szlot, 'XSET').
mop_direct('MEMORY-FAULT-ERROR', supers, ['SYSTEM-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('META-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('META-INFO', szlot, 'CATEGORY').
mop_direct('META-INFO', szlot, 'DEFAULT').
mop_direct('META-INFO', szlot, 'KIND').
mop_direct('META-INFO', szlot, 'NUMBER').
mop_direct('META-INFO', szlot, 'TYPE-CHECKER').
mop_direct('META-INFO', szlot, 'TYPE-SPEC').
mop_direct('META-INFO', szlot, 'VALIDATE-FUNCTION').
mop_direct('METAOBJECT', submop, 'GENERIC-FUNCTION').
mop_direct('METAOBJECT', submop, 'METHOD').
mop_direct('METAOBJECT', submop, 'METHOD-COMBINATION').
mop_direct('METAOBJECT', submop, cl_slot_definition).
mop_direct('METAOBJECT', submop, 'SPECIALIZER').
mop_direct('METAOBJECT', submop, 'SUPER-CLASS').
mop_direct('METAOBJECT', supers, [cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('METAOBJECT', supers, [cl_standard_object, t]).
mop_direct('METAOBJECT-INITIALIZATION-VIOLATION', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('METHOD', submop, 'STANDARD-METHOD').
mop_direct('METHOD', supers, ['METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('METHOD', supers, ['STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('METHOD', szlot, '$FROM-DEFGENERIC').
mop_direct('METHOD-CALL', submop, 'CONSTANT-METHOD-CALL').
mop_direct('METHOD-CALL', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('METHOD-CALL', szlot, 'CALL-METHOD-ARGS').
mop_direct('METHOD-CALL', szlot, 'FUNCTION').
mop_direct('METHOD-CALL-ERROR', submop, 'METHOD-CALL-TYPE-ERROR').
mop_direct('METHOD-CALL-ERROR', supers, [cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('METHOD-CALL-ERROR', szlot, '$ARGS').
mop_direct('METHOD-CALL-ERROR', szlot, '$GF').
mop_direct('METHOD-CALL-ERROR', szlot, '$METHOD').
mop_direct('METHOD-CALL-TYPE-ERROR', supers, [cl_simple_type_error, 'METHOD-CALL-ERROR', cl_simple_error, cl_simple_condition, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('METHOD-COMBINATION', submop, 'STANDARD-METHOD-COMBINATION').
mop_direct('METHOD-COMBINATION', supers, ['METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('METHOD-COMBINATION', supers, ['METAOBJECT', cl_standard_object, t]).
mop_direct('METHOD-COMBINATION', szlot, '%DOCUMENTATION').
mop_direct('METHOD-COMBINATION', szlot, 'ARGUMENTS-LAMBDA-LIST').
mop_direct('METHOD-COMBINATION', szlot, 'CALL-NEXT-METHOD-ALLOWED').
mop_direct('METHOD-COMBINATION', szlot, 'CHECK-METHOD-QUALIFIERS').
mop_direct('METHOD-COMBINATION', szlot, 'CHECK-OPTIONS').
mop_direct('METHOD-COMBINATION', szlot, 'DECLARATIONS').
mop_direct('METHOD-COMBINATION', szlot, 'DOCUMENTATION').
mop_direct('METHOD-COMBINATION', szlot, 'EXPANDER').
mop_direct('METHOD-COMBINATION', szlot, 'IDENTITY-WITH-ONE-ARGUMENT').
mop_direct('METHOD-COMBINATION', szlot, 'LONG-EXPANDER').
mop_direct('METHOD-COMBINATION', szlot, 'NAME').
mop_direct('METHOD-COMBINATION', szlot, 'OPERATOR').
mop_direct('METHOD-COMBINATION', szlot, 'OPTIONS').
mop_direct('METHOD-COMBINATION', szlot, 'QUALIFIERS').
mop_direct('MISDESIGNED-FORWARD-REFERENCED-CLASS', supers, ['FORWARD-REFERENCED-CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('MISSING-LOAD-FORM', submop, 'SIMPLE-MISSING-LOAD-FORM').
mop_direct('MISSING-LOAD-FORM', supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('MISSING-LOAD-FORM', szlot, '$OBJECT').
mop_direct('MODULAR-CLASS', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('MODULAR-CLASS', szlot, 'FUNS').
mop_direct('MODULAR-CLASS', szlot, 'VERSIONS').
mop_direct('MODULAR-CLASS', szlot, 'WIDTHS').
mop_direct('MODULAR-FUN-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('MODULAR-FUN-INFO', szlot, 'LAMBDA-LIST').
mop_direct('MODULAR-FUN-INFO', szlot, 'NAME').
mop_direct('MODULAR-FUN-INFO', szlot, 'PROTOTYPE').
mop_direct('MODULAR-FUN-INFO', szlot, 'SIGNEDP').
mop_direct('MODULAR-FUN-INFO', szlot, 'WIDTH').
mop_direct('MUTEX', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('MUTEX', szlot, '%OWNER').
mop_direct('MUTEX', szlot, 'NAME').
mop_direct('MUTEX', szlot, 'STATE').
mop_direct('MV-COMBINATION', supers, ['BASIC-COMBINATION', 'VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('N-N', supers, ['ACCESSOR-DFUN-INFO', 'DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NAME-CONFLICT', supers, ['REFERENCE-CONDITION', cl_package_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NAME-CONFLICT', szlot, 'DATUM').
mop_direct('NAME-CONFLICT', szlot, 'FUNCTION').
mop_direct('NAME-CONFLICT', szlot, 'SYMBOLS').
mop_direct('NAMED-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NAMED-TYPE', szlot, 'CLASS-INFO').
mop_direct('NAMED-TYPE', szlot, 'NAME').
mop_direct('NAMESTRING-PARSE-ERROR', supers, [cl_parse_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NAMESTRING-PARSE-ERROR', szlot, 'ARGS').
mop_direct('NAMESTRING-PARSE-ERROR', szlot, 'COMPLAINT').
mop_direct('NAMESTRING-PARSE-ERROR', szlot, 'NAMESTRING').
mop_direct('NAMESTRING-PARSE-ERROR', szlot, 'OFFSET').
mop_direct('NEGATION-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NEGATION-TYPE', szlot, 'CLASS-INFO').
mop_direct('NEGATION-TYPE', szlot, 'TYPE').
mop_direct('NEW-VALUE-SPECIALIZATION', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NEW-VALUE-SPECIALIZATION', szlot, '%METHOD').
mop_direct('NEWLINE', supers, ['SECTION-START', 'QUEUED-OP', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NEWLINE', szlot, 'KIND').
mop_direct('NIL-ARRAY-ACCESSED-ERROR', supers, ['REFERENCE-CONDITION', cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NLX-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NLX-INFO', szlot, 'BLOCK').
mop_direct('NLX-INFO', szlot, 'CLEANUP').
mop_direct('NLX-INFO', szlot, 'INFO').
mop_direct('NLX-INFO', szlot, 'SAFE-P').
mop_direct('NLX-INFO', szlot, 'TARGET').
mop_direct('NO-DEBUG-BLOCKS', supers, ['DEBUG-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NO-DEBUG-BLOCKS', szlot, 'DEBUG-FUN').
mop_direct('NO-DEBUG-FUN-RETURNS', supers, ['DEBUG-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NO-DEBUG-FUN-RETURNS', szlot, 'DEBUG-FUN').
mop_direct('NO-DEBUG-VARS', supers, ['DEBUG-CONDITION', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NO-DEBUG-VARS', szlot, 'DEBUG-FUN').
mop_direct('NO-METHODS', supers, ['DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NO-PRIMARY-METHOD', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('NO-PRIMARY-METHOD', szlot, 'ARGS').
mop_direct('NO-PRIMARY-METHOD', szlot, 'GENERIC-FUNCTION').
mop_direct('NODE', submop, 'BIND').
mop_direct('NODE', submop, 'CIF').
mop_direct('NODE', submop, 'CRETURN').
mop_direct('NODE', submop, 'ENTRY').
mop_direct('NODE', submop, 'VALUED-NODE').
mop_direct('NODE', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NODE', szlot, 'LEXENV').
mop_direct('NODE', szlot, 'NEXT').
mop_direct('NODE', szlot, 'NUMBER').
mop_direct('NODE', szlot, 'PREV').
mop_direct('NODE', szlot, 'REOPTIMIZE').
mop_direct('NODE', szlot, 'SOURCE-PATH').
mop_direct('NODE', szlot, 'TAIL-P').
mop_direct('NULL', supers, [cl_symbol, cl_list, 'SEQUENCE', t]).
mop_direct('NUMBER', submop, cl_complex).
mop_direct('NUMBER', submop, 'REAL').
mop_direct('NUMBER', supers, [t]).
mop_direct('NUMERIC-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('NUMERIC-TYPE', szlot, 'CLASS').
mop_direct('NUMERIC-TYPE', szlot, 'CLASS-INFO').
mop_direct('NUMERIC-TYPE', szlot, 'COMPLEXP').
mop_direct('NUMERIC-TYPE', szlot, 'ENUMERABLE').
mop_direct('NUMERIC-TYPE', szlot, 'FORMAT').
mop_direct('NUMERIC-TYPE', szlot, 'HIGH').
mop_direct('NUMERIC-TYPE', szlot, 'LOW').
mop_direct('OBSOLETE-STRUCTURE', supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('OBSOLETE-STRUCTURE', szlot, 'DATUM').
mop_direct('OCTET-DECODING-ERROR', submop, 'CHARACTER-OUT-OF-RANGE').
mop_direct('OCTET-DECODING-ERROR', submop, 'END-OF-INPUT-IN-CHARACTER').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-EUC-JP-CONTINUATION-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-EUC-JP-STARTER-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-GBK-CONTINUATION-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-GBK-STARTER-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-SHIFT_JIS-CONTINUATION-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-SHIFT_JIS-STARTER-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-UTF8-CONTINUATION-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'INVALID-UTF8-STARTER-BYTE').
mop_direct('OCTET-DECODING-ERROR', submop, 'MALFORMED-ASCII').
mop_direct('OCTET-DECODING-ERROR', submop, 'MALFORMED-EUC-JP').
mop_direct('OCTET-DECODING-ERROR', submop, 'MALFORMED-GBK').
mop_direct('OCTET-DECODING-ERROR', submop, 'MALFORMED-SHIFT_JIS').
mop_direct('OCTET-DECODING-ERROR', submop, 'OVERLONG-UTF8-SEQUENCE').
mop_direct('OCTET-DECODING-ERROR', supers, ['CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('OCTET-DECODING-ERROR', szlot, 'ARRAY').
mop_direct('OCTET-DECODING-ERROR', szlot, 'END').
mop_direct('OCTET-DECODING-ERROR', szlot, 'EXTERNAL-FORMAT').
mop_direct('OCTET-DECODING-ERROR', szlot, 'POSITION').
mop_direct('OCTET-DECODING-ERROR', szlot, 'START').
mop_direct('OCTETS-ENCODING-ERROR', supers, ['CHARACTER-ENCODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('OCTETS-ENCODING-ERROR', szlot, 'EXTERNAL-FORMAT').
mop_direct('OCTETS-ENCODING-ERROR', szlot, 'POSITION').
mop_direct('OCTETS-ENCODING-ERROR', szlot, 'STRING').
mop_direct('OFFS-HOOK', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('OFFS-HOOK', szlot, 'BEFORE-ADDRESS').
mop_direct('OFFS-HOOK', szlot, 'FUN').
mop_direct('OFFS-HOOK', szlot, 'OFFSET').
mop_direct('ONE-CLASS', submop, 'TWO-CLASS').
mop_direct('ONE-CLASS', supers, ['ONE-INDEX-DFUN-INFO', 'ACCESSOR-DFUN-INFO', 'DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ONE-CLASS', szlot, 'WRAPPER0').
mop_direct('ONE-INDEX', supers, ['ONE-INDEX-DFUN-INFO', 'ACCESSOR-DFUN-INFO', 'DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ONE-INDEX-DFUN-INFO', submop, 'ONE-CLASS').
mop_direct('ONE-INDEX-DFUN-INFO', submop, 'ONE-INDEX').
mop_direct('ONE-INDEX-DFUN-INFO', supers, ['ACCESSOR-DFUN-INFO', 'DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ONE-INDEX-DFUN-INFO', szlot, 'INDEX').
mop_direct('OPERAND-PARSE', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('OPERAND-PARSE', szlot, 'BORN').
mop_direct('OPERAND-PARSE', szlot, 'DIES').
mop_direct('OPERAND-PARSE', szlot, 'KIND').
mop_direct('OPERAND-PARSE', szlot, 'LOAD').
mop_direct('OPERAND-PARSE', szlot, 'LOAD-TN').
mop_direct('OPERAND-PARSE', szlot, 'NAME').
mop_direct('OPERAND-PARSE', szlot, 'OFFSET').
mop_direct('OPERAND-PARSE', szlot, 'SC').
mop_direct('OPERAND-PARSE', szlot, 'SCS').
mop_direct('OPERAND-PARSE', szlot, 'TARGET').
mop_direct('OPERAND-PARSE', szlot, 'TEMP').
mop_direct('OPTIONAL-DISPATCH', supers, ['FUNCTIONAL', 'LEAF', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('OPTIONAL-DISPATCH', szlot, 'ALLOWP').
mop_direct('OPTIONAL-DISPATCH', szlot, 'ARGLIST').
mop_direct('OPTIONAL-DISPATCH', szlot, 'ENTRY-POINTS').
mop_direct('OPTIONAL-DISPATCH', szlot, 'KEYP').
mop_direct('OPTIONAL-DISPATCH', szlot, 'MAIN-ENTRY').
mop_direct('OPTIONAL-DISPATCH', szlot, 'MAX-ARGS').
mop_direct('OPTIONAL-DISPATCH', szlot, 'MIN-ARGS').
mop_direct('OPTIONAL-DISPATCH', szlot, 'MORE-ENTRY').
mop_direct('ORDERED-SET', supers, ['SSET', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ORDERED-SET', szlot, 'MEMBERS').
mop_direct('OS-ERROR', submop, 'SIMPLE-OS-ERROR').
mop_direct('OS-ERROR', supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('OVERHEAD', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('OVERHEAD', szlot, 'CALL').
mop_direct('OVERHEAD', szlot, 'INTERNAL').
mop_direct('OVERHEAD', szlot, 'TOTAL').
mop_direct('OVERLONG-UTF8-SEQUENCE', supers, ['OCTET-DECODING-ERROR', 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_package, supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_package, supers, [t]).
mop_direct(cl_package, szlot, '%IMPLEMENTATION-PACKAGES').
mop_direct(cl_package, szlot, '%LOCAL-NICKNAMES').
mop_direct(cl_package, szlot, '%LOCALLY-NICKNAMED-BY').
mop_direct(cl_package, szlot, '%NAME').
mop_direct(cl_package, szlot, '%NICKNAMES').
mop_direct(cl_package, szlot, '%SHADOWING-SYMBOLS').
mop_direct(cl_package, szlot, '%USE-LIST').
mop_direct(cl_package, szlot, '%USED-BY-LIST').
mop_direct(cl_package, szlot, 'DOC-STRING').
mop_direct(cl_package, szlot, 'EXTERNAL-SYMBOLS').
mop_direct(cl_package, szlot, 'INTERNAL-SYMBOLS').
mop_direct(cl_package, szlot, 'LOCK').
mop_direct(cl_package, szlot, 'MRU-TABLE-INDEX').
mop_direct(cl_package, szlot, 'SOURCE-LOCATION').
mop_direct(cl_package, szlot, 'TABLES').
mop_direct('PACKAGE-AT-VARIANCE', supers, ['REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PACKAGE-AT-VARIANCE-ERROR', supers, ['REFERENCE-CONDITION', cl_simple_condition, cl_package_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_package_error, submop, 'NAME-CONFLICT').
mop_direct(cl_package_error, submop, 'PACKAGE-AT-VARIANCE-ERROR').
mop_direct(cl_package_error, submop, 'PACKAGE-LOCK-VIOLATION').
mop_direct(cl_package_error, submop, 'SIMPLE-PACKAGE-ERROR').
mop_direct(cl_package_error, submop, 'SIMPLE-READER-PACKAGE-ERROR').
mop_direct(cl_package_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_package_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_package_error, szlot, '$PACKAGE').
mop_direct(cl_package_error, szlot, 'PACKAGE').
mop_direct('PACKAGE-HASHTABLE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PACKAGE-HASHTABLE', szlot, 'CELLS').
mop_direct('PACKAGE-HASHTABLE', szlot, 'DELETED').
mop_direct('PACKAGE-HASHTABLE', szlot, 'FREE').
mop_direct('PACKAGE-HASHTABLE', szlot, 'SIZE').
mop_direct('PACKAGE-LOCK-VIOLATION', submop, 'PACKAGE-LOCKED-ERROR').
mop_direct('PACKAGE-LOCK-VIOLATION', submop, 'SYMBOL-PACKAGE-LOCKED-ERROR').
mop_direct('PACKAGE-LOCK-VIOLATION', supers, [cl_package_error, 'ERROR', cl_serious_condition, 'REFERENCE-CONDITION', cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PACKAGE-LOCK-VIOLATION', szlot, 'CURRENT-PACKAGE').
mop_direct('PACKAGE-LOCKED-ERROR', supers, ['PACKAGE-LOCK-VIOLATION', cl_package_error, 'ERROR', cl_serious_condition, 'REFERENCE-CONDITION', cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PARSE-DEPRECATED-TYPE', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PARSE-DEPRECATED-TYPE', szlot, 'SPECIFIER').
mop_direct(cl_parse_error, submop, 'NAMESTRING-PARSE-ERROR').
mop_direct(cl_parse_error, submop, cl_reader_error).
mop_direct(cl_parse_error, submop, 'SIMPLE-PARSE-ERROR').
mop_direct(cl_parse_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_parse_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('PARSE-UNKNOWN-TYPE', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PARSE-UNKNOWN-TYPE', szlot, 'SPECIFIER').
mop_direct(cl_pathname, submop, cl_logical_pathname).
mop_direct(cl_pathname, supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_pathname, supers, [t]).
mop_direct(cl_pathname, szlot, 'DEVICE').
mop_direct(cl_pathname, szlot, 'DIRECTORY').
mop_direct(cl_pathname, szlot, 'HOST').
mop_direct(cl_pathname, szlot, 'NAME').
mop_direct(cl_pathname, szlot, 'TYPE').
mop_direct(cl_pathname, szlot, 'VERSION').
mop_direct('PATTERN', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PATTERN', szlot, 'PIECES').
mop_direct('PCL-CLASS', submop, 'FORWARD-REFERENCED-CLASS').
mop_direct('PCL-CLASS', submop, cl_slot_class).
mop_direct('PCL-CLASS', submop, 'SYSTEM-CLASS').
mop_direct('PCL-CLASS', supers, ['CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('PCL-CLASS', szlot, '%CLASS-PRECEDENCE-LIST').
mop_direct('PCL-CLASS', szlot, 'CAN-PRECEDE-LIST').
mop_direct('PCL-CLASS', szlot, 'CPL-AVAILABLE-P').
mop_direct('PCL-CLASS', szlot, 'INCOMPATIBLE-SUPERCLASS-LIST').
mop_direct('PCL-CLASS', szlot, 'PROTOTYPE').
mop_direct('PCL-CLASS', szlot, 'WRAPPER').
mop_direct('PHYSENV', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PHYSENV', szlot, 'CLOSURE').
mop_direct('PHYSENV', szlot, 'INFO').
mop_direct('PHYSENV', szlot, 'LAMBDA').
mop_direct('PHYSENV', szlot, 'NLX-INFO').
mop_direct('PLIST-MIXIN', submop, 'DEPENDENT-UPDATE-MIXIN').
mop_direct('PLIST-MIXIN', submop, 'STANDARD-METHOD').
mop_direct('PLIST-MIXIN', supers, [cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('PLIST-MIXIN', szlot, 'PLIST').
mop_direct('POLICY', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('POLICY', szlot, 'DEPENDENT-QUALITIES').
mop_direct('POLICY', szlot, 'PRESENCE-BITS').
mop_direct('POLICY', szlot, 'PRIMARY-QUALITIES').
mop_direct('POLICY-DEPENDENT-QUALITY', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('POLICY-DEPENDENT-QUALITY', szlot, 'EXPRESSION').
mop_direct('POLICY-DEPENDENT-QUALITY', szlot, 'GETTER').
mop_direct('POLICY-DEPENDENT-QUALITY', szlot, 'NAME').
mop_direct('POLICY-DEPENDENT-QUALITY', szlot, 'VALUES-DOCUMENTATION').
mop_direct('POLLFDS', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('POLLFDS', szlot, 'FDS').
mop_direct('POLLFDS', szlot, 'LIST').
mop_direct('POLLFDS', szlot, 'MAP').
mop_direct('POLLFDS', szlot, 'N-FDS').
mop_direct('POTENTIAL-CLASS', submop, 'CLASS').
mop_direct('POTENTIAL-CLASS', submop, 'MISDESIGNED-FORWARD-REFERENCED-CLASS').
mop_direct('POTENTIAL-CLASS', supers, ['SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('PPRINT-DISPATCH-ENTRY', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PPRINT-DISPATCH-ENTRY', szlot, 'FUN').
mop_direct('PPRINT-DISPATCH-ENTRY', szlot, 'INITIAL-P').
mop_direct('PPRINT-DISPATCH-ENTRY', szlot, 'PRIORITY').
mop_direct('PPRINT-DISPATCH-ENTRY', szlot, 'TEST-FN').
mop_direct('PPRINT-DISPATCH-ENTRY', szlot, 'TYPE').
mop_direct('PPRINT-DISPATCH-TABLE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PPRINT-DISPATCH-TABLE', szlot, 'CONS-ENTRIES').
mop_direct('PPRINT-DISPATCH-TABLE', szlot, 'ENTRIES').
mop_direct('PRETTY-STREAM', supers, ['ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PRETTY-STREAM', szlot, 'BLOCKS').
mop_direct('PRETTY-STREAM', szlot, 'BUFFER').
mop_direct('PRETTY-STREAM', szlot, 'BUFFER-FILL-POINTER').
mop_direct('PRETTY-STREAM', szlot, 'BUFFER-OFFSET').
mop_direct('PRETTY-STREAM', szlot, 'BUFFER-START-COLUMN').
mop_direct('PRETTY-STREAM', szlot, 'CHAR-OUT-ONESHOT-HOOK').
mop_direct('PRETTY-STREAM', szlot, 'LINE-LENGTH').
mop_direct('PRETTY-STREAM', szlot, 'LINE-NUMBER').
mop_direct('PRETTY-STREAM', szlot, 'PENDING-BLOCKS').
mop_direct('PRETTY-STREAM', szlot, 'PREFIX').
mop_direct('PRETTY-STREAM', szlot, 'PRINT-LINES').
mop_direct('PRETTY-STREAM', szlot, 'QUEUE-HEAD').
mop_direct('PRETTY-STREAM', szlot, 'QUEUE-TAIL').
mop_direct('PRETTY-STREAM', szlot, 'SUFFIX').
mop_direct('PRETTY-STREAM', szlot, 'TARGET').
mop_direct('PRIM-OBJECT-SLOT', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PRIM-OBJECT-SLOT', szlot, 'DOCS').
mop_direct('PRIM-OBJECT-SLOT', szlot, 'NAME').
mop_direct('PRIM-OBJECT-SLOT', szlot, 'OFFSET').
mop_direct('PRIM-OBJECT-SLOT', szlot, 'OPTIONS').
mop_direct('PRIM-OBJECT-SLOT', szlot, 'REST-P').
mop_direct('PRIM-OBJECT-SLOT', szlot, 'SPECIAL').
mop_direct('PRIMITIVE-OBJECT', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PRIMITIVE-OBJECT', szlot, 'LOWTAG').
mop_direct('PRIMITIVE-OBJECT', szlot, 'NAME').
mop_direct('PRIMITIVE-OBJECT', szlot, 'OPTIONS').
mop_direct('PRIMITIVE-OBJECT', szlot, 'SIZE').
mop_direct('PRIMITIVE-OBJECT', szlot, 'SLOTS').
mop_direct('PRIMITIVE-OBJECT', szlot, 'VARIABLE-LENGTH-P').
mop_direct('PRIMITIVE-OBJECT', szlot, 'WIDETAG').
mop_direct('PRIMITIVE-TYPE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PRIMITIVE-TYPE', szlot, 'CHECK').
mop_direct('PRIMITIVE-TYPE', szlot, 'NAME').
mop_direct('PRIMITIVE-TYPE', szlot, 'SCS').
mop_direct('PRIMITIVE-TYPE', szlot, 'SPECIFIER').
mop_direct(cl_print_not_readable, submop, 'SIMPLE-PRINT-NOT-READABLE').
mop_direct(cl_print_not_readable, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_print_not_readable, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_print_not_readable, szlot, '$OBJECT').
mop_direct(cl_print_not_readable, szlot, 'OBJECT').
mop_direct('PRINT-OBJECT-STREAM-SPECIALIZER', supers, ['REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PRIORITY-QUEUE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PRIORITY-QUEUE', szlot, 'CONTENTS').
mop_direct('PRIORITY-QUEUE', szlot, 'KEYFUN').
mop_direct('PROCESS', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PROCESS', szlot, '%EXIT-CODE').
mop_direct('PROCESS', szlot, '%STATUS').
mop_direct('PROCESS', szlot, 'COOKIE').
mop_direct('PROCESS', szlot, 'CORE-DUMPED').
mop_direct('PROCESS', szlot, 'ERROR').
mop_direct('PROCESS', szlot, 'INPUT').
mop_direct('PROCESS', szlot, 'OUTPUT').
mop_direct('PROCESS', szlot, 'PID').
mop_direct('PROCESS', szlot, 'PLIST').
mop_direct('PROCESS', szlot, 'PTY').
mop_direct('PROCESS', szlot, 'STATUS-HOOK').
mop_direct('PROCLAMATION-MISMATCH', submop, 'FTYPE-PROCLAMATION-MISMATCH').
mop_direct('PROCLAMATION-MISMATCH', submop, 'TYPE-PROCLAMATION-MISMATCH').
mop_direct('PROCLAMATION-MISMATCH', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PROCLAMATION-MISMATCH', szlot, 'DESCRIPTION').
mop_direct('PROCLAMATION-MISMATCH', szlot, 'KIND').
mop_direct('PROCLAMATION-MISMATCH', szlot, 'NAME').
mop_direct('PROCLAMATION-MISMATCH', szlot, 'NEW').
mop_direct('PROCLAMATION-MISMATCH', szlot, 'OLD').
mop_direct('PROFILE-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PROFILE-INFO', szlot, 'CLEAR-STATS-FUN').
mop_direct('PROFILE-INFO', szlot, 'ENCAPSULATED-FUN').
mop_direct('PROFILE-INFO', szlot, 'ENCAPSULATION-FUN').
mop_direct('PROFILE-INFO', szlot, 'NAME').
mop_direct('PROFILE-INFO', szlot, 'READ-STATS-FUN').
mop_direct(cl_program_error, submop, 'ARG-COUNT-PROGRAM-ERROR').
mop_direct(cl_program_error, submop, 'ARGUMENT-LIST-DOTTED').
mop_direct(cl_program_error, submop, 'COMPILED-PROGRAM-ERROR').
mop_direct(cl_program_error, submop, 'INITARG-ERROR').
mop_direct(cl_program_error, submop, 'INTERPRETED-PROGRAM-ERROR').
mop_direct(cl_program_error, submop, 'KEYWORD-ERROR').
mop_direct(cl_program_error, submop, 'SIMPLE-PROGRAM-ERROR').
mop_direct(cl_program_error, submop, 'SOURCE-PROGRAM-ERROR').
mop_direct(cl_program_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_program_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('PROTOCOL-UNIMPLEMENTED', supers, [cl_type_error, 'ERROR', cl_serious_condition, 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('PV-TABLE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('PV-TABLE', szlot, 'CACHE').
mop_direct('PV-TABLE', szlot, 'PV-SIZE').
mop_direct('PV-TABLE', szlot, 'SLOT-NAME-LISTS').
mop_direct('QUEUED-OP', submop, 'BLOCK-END').
mop_direct('QUEUED-OP', submop, 'INDENTATION').
mop_direct('QUEUED-OP', submop, 'SECTION-START').
mop_direct('QUEUED-OP', submop, 'TAB').
mop_direct('QUEUED-OP', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('QUEUED-OP', szlot, 'POSN').
mop_direct('RANDOM-CLASS', supers, [t]).
mop_direct(cl_random_state, supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_random_state, supers, [t]).
mop_direct(cl_random_state, szlot, 'STATE').
mop_direct(cl_ratio, supers, ['RATIONAL', 'REAL', 'NUMBER', t]).
mop_direct('RATIONAL', submop, 'INTEGER').
mop_direct('RATIONAL', submop, cl_ratio).
mop_direct('RATIONAL', supers, ['REAL', 'NUMBER', t]).
mop_direct('RAW-SLOT-DATA', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('RAW-SLOT-DATA', szlot, 'ACCESSOR-NAME').
mop_direct('RAW-SLOT-DATA', szlot, 'ALIGNMENT').
mop_direct('RAW-SLOT-DATA', szlot, 'COMPARER').
mop_direct('RAW-SLOT-DATA', szlot, 'INIT-VOP').
mop_direct('RAW-SLOT-DATA', szlot, 'N-WORDS').
mop_direct('RAW-SLOT-DATA', szlot, 'RAW-TYPE').
mop_direct('READER-EOF-ERROR', supers, [cl_end_of_file, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('READER-EOF-ERROR', szlot, 'CONTEXT').
mop_direct(cl_reader_error, submop, 'INPUT-ERROR-IN-COMPILE-FILE').
mop_direct(cl_reader_error, submop, 'SIMPLE-READER-ERROR').
mop_direct(cl_reader_error, supers, [cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_reader_error, supers, [cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('READER-IMPOSSIBLE-NUMBER-ERROR', supers, ['SIMPLE-READER-ERROR', cl_reader_error, cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('READER-IMPOSSIBLE-NUMBER-ERROR', szlot, 'ERROR').
mop_direct(cl_readtable, supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_readtable, supers, [t]).
mop_direct(cl_readtable, szlot, '%READTABLE-CASE').
mop_direct(cl_readtable, szlot, '%READTABLE-NORMALIZATION').
mop_direct(cl_readtable, szlot, 'CHARACTER-ATTRIBUTE-ARRAY').
mop_direct(cl_readtable, szlot, 'CHARACTER-ATTRIBUTE-HASH-TABLE').
mop_direct(cl_readtable, szlot, 'CHARACTER-MACRO-ARRAY').
mop_direct(cl_readtable, szlot, 'CHARACTER-MACRO-HASH-TABLE').
mop_direct('REAL', submop, 'FLOAT').
mop_direct('REAL', submop, 'RATIONAL').
mop_direct('REAL', supers, ['NUMBER', t]).
mop_direct('REDEFINITION-WARNING', submop, 'FUNCTION-REDEFINITION-WARNING').
mop_direct('REDEFINITION-WARNING', submop, 'REDEFINITION-WITH-DEFGENERIC').
mop_direct('REDEFINITION-WARNING', submop, 'REDEFINITION-WITH-DEFMETHOD').
mop_direct('REDEFINITION-WARNING', submop, 'REDEFINITION-WITH-DEFTRANSFORM').
mop_direct('REDEFINITION-WARNING', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REDEFINITION-WARNING', szlot, 'NAME').
mop_direct('REDEFINITION-WARNING', szlot, 'NEW-LOCATION').
mop_direct('REDEFINITION-WITH-DEFGENERIC', supers, ['REDEFINITION-WARNING', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REDEFINITION-WITH-DEFMACRO', supers, ['FUNCTION-REDEFINITION-WARNING', 'REDEFINITION-WARNING', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REDEFINITION-WITH-DEFMETHOD', supers, ['REDEFINITION-WARNING', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REDEFINITION-WITH-DEFMETHOD', szlot, 'NEW-LOCATION').
mop_direct('REDEFINITION-WITH-DEFMETHOD', szlot, 'OLD-METHOD').
mop_direct('REDEFINITION-WITH-DEFMETHOD', szlot, 'QUALIFIERS').
mop_direct('REDEFINITION-WITH-DEFMETHOD', szlot, 'SPECIALIZERS').
mop_direct('REDEFINITION-WITH-DEFTRANSFORM', supers, ['REDEFINITION-WARNING', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REDEFINITION-WITH-DEFTRANSFORM', szlot, 'TRANSFORM').
mop_direct('REDEFINITION-WITH-DEFUN', supers, ['FUNCTION-REDEFINITION-WARNING', 'REDEFINITION-WARNING', cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REF', supers, ['VALUED-NODE', 'NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('REF', szlot, '%SOURCE-NAME').
mop_direct('REF', szlot, 'LEAF').
mop_direct('REF', szlot, 'REOPTIMIZE').
mop_direct('REFERENCE-CONDITION', submop, 'ARGUMENTS-OUT-OF-DOMAIN-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'ARRAY-INITIAL-ELEMENT-MISMATCH').
mop_direct('REFERENCE-CONDITION', submop, 'BOUNDING-INDICES-BAD-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'CONSTANT-MODIFIED').
mop_direct('REFERENCE-CONDITION', submop, 'CPL-PROTOCOL-VIOLATION').
mop_direct('REFERENCE-CONDITION', submop, 'DECLARATION-TYPE-CONFLICT-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'DEFCONSTANT-UNEQL').
mop_direct('REFERENCE-CONDITION', submop, 'DEPRECATION-CONDITION').
mop_direct('REFERENCE-CONDITION', submop, 'DUPLICATE-DEFINITION').
mop_direct('REFERENCE-CONDITION', submop, 'EXTENSION-FAILURE').
mop_direct('REFERENCE-CONDITION', submop, 'FIND-METHOD-LENGTH-MISMATCH').
mop_direct('REFERENCE-CONDITION', submop, 'FORMAT-ARGS-MISMATCH').
mop_direct('REFERENCE-CONDITION', submop, 'FORMAT-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'GENERIC-FUNCTION-LAMBDA-LIST-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'INITARG-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'INSTANCE-STRUCTURE-PROTOCOL-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'INVALID-ARRAY-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'INVALID-SUPERCLASS').
mop_direct('REFERENCE-CONDITION', submop, 'LOCAL-ARGUMENT-MISMATCH').
mop_direct('REFERENCE-CONDITION', submop, 'LONG-METHOD-COMBINATION-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'METAOBJECT-INITIALIZATION-VIOLATION').
mop_direct('REFERENCE-CONDITION', submop, 'NAME-CONFLICT').
mop_direct('REFERENCE-CONDITION', submop, 'NEW-VALUE-SPECIALIZATION').
mop_direct('REFERENCE-CONDITION', submop, 'NIL-ARRAY-ACCESSED-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'NO-PRIMARY-METHOD').
mop_direct('REFERENCE-CONDITION', submop, 'PACKAGE-AT-VARIANCE').
mop_direct('REFERENCE-CONDITION', submop, 'PACKAGE-AT-VARIANCE-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'PACKAGE-LOCK-VIOLATION').
mop_direct('REFERENCE-CONDITION', submop, 'PRINT-OBJECT-STREAM-SPECIALIZER').
mop_direct('REFERENCE-CONDITION', submop, 'PROTOCOL-UNIMPLEMENTED').
mop_direct('REFERENCE-CONDITION', submop, 'SAVE-CONDITION').
mop_direct('REFERENCE-CONDITION', submop, 'SIMPLE-REFERENCE-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'SIMPLE-REFERENCE-WARNING').
mop_direct('REFERENCE-CONDITION', submop, 'SLOTD-INITIALIZATION-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'SPECIALIZED-LAMBDA-LIST-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'STANDARD-PPRINT-DISPATCH-TABLE-MODIFIED-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'STANDARD-READTABLE-MODIFIED-ERROR').
mop_direct('REFERENCE-CONDITION', submop, 'STRUCTURE-INITARG-NOT-KEYWORD').
mop_direct('REFERENCE-CONDITION', submop, 'TYPE-STYLE-WARNING').
mop_direct('REFERENCE-CONDITION', submop, 'TYPE-WARNING').
mop_direct('REFERENCE-CONDITION', submop, 'UNSET-FUNCALLABLE-INSTANCE-FUNCTION').
mop_direct('REFERENCE-CONDITION', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('REFERENCE-CONDITION', szlot, 'REFERENCES').
mop_direct('REG-SPEC', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('REG-SPEC', szlot, 'KIND').
mop_direct('REG-SPEC', szlot, 'NAME').
mop_direct('REG-SPEC', szlot, 'OFFSET').
mop_direct('REG-SPEC', szlot, 'SCS').
mop_direct('REG-SPEC', szlot, 'TEMP').
mop_direct('RESTART', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('RESTART', supers, [cl_structure_object, t]).
mop_direct('RESTART', szlot, 'ASSOCIATED-CONDITIONS').
mop_direct('RESTART', szlot, 'FUNCTION').
mop_direct('RESTART', szlot, 'INTERACTIVE').
mop_direct('RESTART', szlot, 'INTERACTIVE-FUNCTION').
mop_direct('RESTART', szlot, 'INVOKE-FUNCTION').
mop_direct('RESTART', szlot, 'INVOKE-TAG').
mop_direct('RESTART', szlot, 'MEANINGFULP').
mop_direct('RESTART', szlot, 'NAME').
mop_direct('RESTART', szlot, 'REPORT').
mop_direct('RESTART', szlot, 'REPORT-FUNCTION').
mop_direct('RESTART', szlot, 'TEST').
mop_direct('RESTART', szlot, 'TEST-FUNCTION').
mop_direct('RESULT-STATE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('RESULT-STATE', szlot, 'NUM-RESULTS').
mop_direct('RETURN-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('RETURN-INFO', szlot, 'COUNT').
mop_direct('RETURN-INFO', szlot, 'KIND').
mop_direct('RETURN-INFO', szlot, 'LOCATIONS').
mop_direct('RETURN-INFO', szlot, 'TYPES').
mop_direct('RLIMIT', supers, [cl_structure_object, t]).
mop_direct('RLIMIT', szlot, 'CUR').
mop_direct('RLIMIT', szlot, 'MAX').
mop_direct('ROOM-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('ROOM-INFO', szlot, 'KIND').
mop_direct('ROOM-INFO', szlot, 'NAME').
mop_direct('SAME-FILE-REDEFINITION-WARNING', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SAME-FILE-REDEFINITION-WARNING', szlot, 'NAME').
mop_direct('SAVE-CONDITION', submop, 'SAVE-ERROR').
mop_direct('SAVE-CONDITION', supers, ['REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SAVE-ERROR', submop, 'SAVE-WITH-MULTIPLE-THREADS-ERROR').
mop_direct('SAVE-ERROR', supers, ['ERROR', cl_serious_condition, 'SAVE-CONDITION', 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SAVE-WITH-MULTIPLE-THREADS-ERROR', supers, ['SAVE-ERROR', 'ERROR', cl_serious_condition, 'SAVE-CONDITION', 'REFERENCE-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SAVE-WITH-MULTIPLE-THREADS-ERROR', szlot, 'INTERACTIVE-THREAD').
mop_direct('SAVE-WITH-MULTIPLE-THREADS-ERROR', szlot, 'OTHER-THREADS').
mop_direct('SB', submop, 'FINITE-SB').
mop_direct('SB', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SB', szlot, 'KIND').
mop_direct('SB', szlot, 'NAME').
mop_direct('SB', szlot, 'SIZE').
mop_direct('SC', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SC', szlot, 'ALIGNMENT').
mop_direct('SC', szlot, 'ALTERNATE-SCS').
mop_direct('SC', szlot, 'CONSTANT-SCS').
mop_direct('SC', szlot, 'ELEMENT-SIZE').
mop_direct('SC', szlot, 'LOAD-COSTS').
mop_direct('SC', szlot, 'LOCATIONS').
mop_direct('SC', szlot, 'MOVE-ARG-VOPS').
mop_direct('SC', szlot, 'MOVE-COSTS').
mop_direct('SC', szlot, 'MOVE-FUNS').
mop_direct('SC', szlot, 'MOVE-VOPS').
mop_direct('SC', szlot, 'NAME').
mop_direct('SC', szlot, 'NUMBER').
mop_direct('SC', szlot, 'NUMBER-STACK-P').
mop_direct('SC', szlot, 'RESERVE-LOCATIONS').
mop_direct('SC', szlot, 'SAVE-P').
mop_direct('SC', szlot, 'SB').
mop_direct('SECTION-START', submop, 'BLOCK-START').
mop_direct('SECTION-START', submop, 'NEWLINE').
mop_direct('SECTION-START', supers, ['QUEUED-OP', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SECTION-START', szlot, 'DEPTH').
mop_direct('SECTION-START', szlot, 'SECTION-END').
mop_direct('SEGMENT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SEGMENT', szlot, '%CURRENT-INDEX').
mop_direct('SEGMENT', szlot, 'ALIGNMENT').
mop_direct('SEGMENT', szlot, 'ANNOTATIONS').
mop_direct('SEGMENT', szlot, 'BRANCH-COUNTDOWN').
mop_direct('SEGMENT', szlot, 'BUFFER').
mop_direct('SEGMENT', szlot, 'CODE').
mop_direct('SEGMENT', szlot, 'CURRENT-POSN').
mop_direct('SEGMENT', szlot, 'DELAYED').
mop_direct('SEGMENT', szlot, 'EMITTABLE-INSTS-QUEUE').
mop_direct('SEGMENT', szlot, 'EMITTABLE-INSTS-SSET').
mop_direct('SEGMENT', szlot, 'FINAL-INDEX').
mop_direct('SEGMENT', szlot, 'FINAL-POSN').
mop_direct('SEGMENT', szlot, 'HOOKS').
mop_direct('SEGMENT', szlot, 'INST-HOOK').
mop_direct('SEGMENT', szlot, 'INST-NUMBER').
mop_direct('SEGMENT', szlot, 'LAST-ANNOTATION').
mop_direct('SEGMENT', szlot, 'LENGTH').
mop_direct('SEGMENT', szlot, 'OPCODES-LENGTH').
mop_direct('SEGMENT', szlot, 'POSTITS').
mop_direct('SEGMENT', szlot, 'QUEUED-BRANCHES').
mop_direct('SEGMENT', szlot, 'READERS').
mop_direct('SEGMENT', szlot, 'RUN-SCHEDULER').
mop_direct('SEGMENT', szlot, 'SAP-MAKER').
mop_direct('SEGMENT', szlot, 'STORAGE-INFO').
mop_direct('SEGMENT', szlot, 'SYNC-POSN').
mop_direct('SEGMENT', szlot, 'TYPE').
mop_direct('SEGMENT', szlot, 'UNBOXED-DATA-RANGE').
mop_direct('SEGMENT', szlot, 'VIRTUAL-LOCATION').
mop_direct('SEGMENT', szlot, 'WRITERS').
mop_direct('SEMAPHORE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SEMAPHORE', szlot, '%COUNT').
mop_direct('SEMAPHORE', szlot, 'MUTEX').
mop_direct('SEMAPHORE', szlot, 'NAME').
mop_direct('SEMAPHORE', szlot, 'QUEUE').
mop_direct('SEMAPHORE', szlot, 'WAITCOUNT').
mop_direct('SEMAPHORE-NOTIFICATION', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SEMAPHORE-NOTIFICATION', szlot, '%STATUS').
mop_direct('SEMI-STANDARD-CLASS', submop, cl_funcallable_standard_class).
mop_direct('SEMI-STANDARD-CLASS', submop, cl_standard_class).
mop_direct('SEMI-STANDARD-CLASS', supers, ['SLOTTED-CLASS', 'CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('SEMI-STANDARD-CLASS', szlot, '$CURRENT-VERSION').
mop_direct('SEMI-STANDARD-CLASS', szlot, '$DIRECT-INSTANCE-SPECIALIZERS').
mop_direct('SEMI-STANDARD-CLASS', szlot, '$FINALIZED-DIRECT-SUBCLASSES').
mop_direct('SEMI-STANDARD-CLASS', szlot, '$FIXED-SLOT-LOCATIONS').
mop_direct('SEMI-STANDARD-CLASS', szlot, '$FUNCALLABLEP').
mop_direct('SEMI-STANDARD-CLASS', szlot, '$INSTANTIATED').
mop_direct('SEMI-STANDARD-CLASS', szlot, '$PROTOTYPE').
mop_direct('SEQUENCE', submop, cl_list).
mop_direct('SEQUENCE', submop, cl_vector).
mop_direct('SEQUENCE', supers, [t]).
mop_direct(cl_serious_condition, submop, 'DEBUG-CONDITION').
mop_direct(cl_serious_condition, submop, 'ERROR').
mop_direct(cl_serious_condition, submop, 'INTERACTIVE-INTERRUPT').
mop_direct(cl_serious_condition, submop, 'INTERRUPT-CONDITION').
mop_direct(cl_serious_condition, submop, 'SIMPLE-SERIOUS-CONDITION').
mop_direct(cl_serious_condition, submop, cl_storage_condition).
mop_direct(cl_serious_condition, submop, 'TIMEOUT').
mop_direct(cl_serious_condition, supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_serious_condition, supers, [cl_condition, cl_standard_object, t]).
mop_direct('SERVICE', supers, [cl_structure_object, t]).
mop_direct('SERVICE', szlot, 'ALIASES').
mop_direct('SERVICE', szlot, 'NAME').
mop_direct('SERVICE', szlot, 'PORT').
mop_direct('SERVICE', szlot, 'PROTO').
mop_direct('SESSION', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SESSION', szlot, 'INTERACTIVE-THREADS').
mop_direct('SESSION', szlot, 'INTERACTIVE-THREADS-QUEUE').
mop_direct('SESSION', szlot, 'LOCK').
mop_direct('SESSION', szlot, 'THREADS').
mop_direct('SHARED-OBJECT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SHARED-OBJECT', szlot, 'DONT-SAVE').
mop_direct('SHARED-OBJECT', szlot, 'HANDLE').
mop_direct('SHARED-OBJECT', szlot, 'NAMESTRING').
mop_direct('SHARED-OBJECT', szlot, 'PATHNAME').
mop_direct('SHORT-METHOD-COMBINATION', supers, ['STANDARD-METHOD-COMBINATION', 'DEFINITION-SOURCE-MIXIN', 'METHOD-COMBINATION', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('SHORT-METHOD-COMBINATION', szlot, 'IDENTITY-WITH-ONE-ARGUMENT').
mop_direct('SHORT-METHOD-COMBINATION', szlot, 'OPERATOR').
mop_direct('SIMD-PACK', supers, [t]).
mop_direct('SIMD-PACK-TYPE', supers, ['CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SIMD-PACK-TYPE', szlot, 'CLASS-INFO').
mop_direct('SIMD-PACK-TYPE', szlot, 'ELEMENT-TYPE').
mop_direct('SIMPLE-ARGUMENT-LIST-DOTTED', supers, [cl_simple_error, cl_simple_condition, 'ARGUMENT-LIST-DOTTED', cl_program_error, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-ARITHMETIC-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-DOUBLE-FLOAT').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-FIXNUM').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-SIGNED-BYTE-16').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-SIGNED-BYTE-32').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-SIGNED-BYTE-64').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-SIGNED-BYTE-8').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-SINGLE-FLOAT').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-15').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-16').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-2').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-31').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-32').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-4').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-63').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-64').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-7').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-8').
mop_direct('SIMPLE-ARRAY', submop, 'SIMPLE-ARRAY-UNSIGNED-FIXNUM').
mop_direct('SIMPLE-ARRAY', submop, cl_simple_bit_vector).
mop_direct('SIMPLE-ARRAY', submop, cl_simple_string).
mop_direct('SIMPLE-ARRAY', submop, cl_simple_vector).
mop_direct('SIMPLE-ARRAY', supers, [cl_array, t]).
mop_direct('SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-DOUBLE-FLOAT', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-FIXNUM', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-NIL', supers, ['VECTOR-NIL', cl_simple_string, cl_string, cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-SIGNED-BYTE-16', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-SIGNED-BYTE-32', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-SIGNED-BYTE-64', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-SIGNED-BYTE-8', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-SINGLE-FLOAT', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-15', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-16', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-2', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-31', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-32', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-4', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-63', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-64', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-7', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-BYTE-8', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-ARRAY-UNSIGNED-FIXNUM', supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-BASE-STRING', supers, ['BASE-STRING', cl_simple_string, cl_string, cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct(cl_simple_bit_vector, supers, [cl_bit_vector, cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-CELL-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-CHARACTER-STRING', supers, ['CHARACTER-STRING', cl_simple_string, cl_string, cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-CHARSET-TYPE-ERROR', supers, [cl_simple_error, cl_simple_condition, 'CHARSET-TYPE-ERROR', cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-CLOS-WARNING', supers, [cl_simple_condition, 'CLOS-WARNING', cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-COMPILER-NOTE', submop, 'CODE-DELETION-NOTE').
mop_direct('SIMPLE-COMPILER-NOTE', supers, [cl_simple_condition, 'COMPILER-NOTE', cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_simple_condition, submop, 'DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME').
mop_direct(cl_simple_condition, submop, 'INTERPRETED-PROGRAM-ERROR').
mop_direct(cl_simple_condition, submop, 'PACKAGE-AT-VARIANCE-ERROR').
mop_direct(cl_simple_condition, submop, 'PACKAGE-LOCK-VIOLATION').
mop_direct(cl_simple_condition, submop, 'SIMPLE-CLOS-WARNING').
mop_direct(cl_simple_condition, submop, 'SIMPLE-COMPILER-NOTE').
mop_direct(cl_simple_condition, submop, 'SIMPLE-CONTROL-ERROR').
mop_direct(cl_simple_condition, submop, cl_simple_error).
mop_direct(cl_simple_condition, submop, 'SIMPLE-FILE-ERROR').
mop_direct(cl_simple_condition, submop, 'SIMPLE-GF-ALREADY-CALLED-WARNING').
mop_direct(cl_simple_condition, submop, 'SIMPLE-GF-REPLACING-METHOD-WARNING').
mop_direct(cl_simple_condition, submop, 'SIMPLE-INTERRUPT-CONDITION').
mop_direct(cl_simple_condition, submop, 'SIMPLE-PACKAGE-ERROR').
mop_direct(cl_simple_condition, submop, 'SIMPLE-PARSE-ERROR').
mop_direct(cl_simple_condition, submop, 'SIMPLE-PROGRAM-ERROR').
mop_direct(cl_simple_condition, submop, 'SIMPLE-READER-ERROR').
mop_direct(cl_simple_condition, submop, 'SIMPLE-SERIOUS-CONDITION').
mop_direct(cl_simple_condition, submop, 'SIMPLE-STORAGE-CONDITION').
mop_direct(cl_simple_condition, submop, 'SIMPLE-STREAM-ERROR').
mop_direct(cl_simple_condition, submop, 'SIMPLE-STYLE-WARNING').
mop_direct(cl_simple_condition, submop, 'SIMPLE-THREAD-ERROR').
mop_direct(cl_simple_condition, submop, cl_simple_type_error).
mop_direct(cl_simple_condition, submop, cl_simple_warning).
mop_direct(cl_simple_condition, supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_simple_condition, supers, [cl_condition, cl_standard_object, t]).
mop_direct(cl_simple_condition, szlot, '$FORMAT-ARGUMENTS').
mop_direct(cl_simple_condition, szlot, '$FORMAT-CONTROL').
mop_direct(cl_simple_condition, szlot, 'FORMAT-ARGUMENTS').
mop_direct(cl_simple_condition, szlot, 'FORMAT-CONTROL').
mop_direct('SIMPLE-CONTROL-ERROR', supers, [cl_simple_condition, cl_control_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-CONTROL-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_control_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-DIVISION-BY-ZERO', supers, [cl_simple_error, cl_simple_condition, cl_division_by_zero, cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-END-OF-FILE', supers, [cl_simple_error, cl_simple_condition, cl_end_of_file, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_simple_error, submop, 'BUG').
mop_direct(cl_simple_error, submop, 'COMPILER-ENVIRONMENT-TOO-COMPLEX-ERROR').
mop_direct(cl_simple_error, submop, 'DECLARATION-TYPE-CONFLICT-ERROR').
mop_direct(cl_simple_error, submop, 'EXTENSION-FAILURE').
mop_direct(cl_simple_error, submop, 'FIND-METHOD-LENGTH-MISMATCH').
mop_direct(cl_simple_error, submop, 'INTERPRETER-ENVIRONMENT-TOO-COMPLEX-ERROR').
mop_direct(cl_simple_error, submop, 'LONG-METHOD-COMBINATION-ERROR').
mop_direct(cl_simple_error, submop, 'METAOBJECT-INITIALIZATION-VIOLATION').
mop_direct(cl_simple_error, submop, 'METHOD-CALL-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-ARGUMENT-LIST-DOTTED').
mop_direct(cl_simple_error, submop, 'SIMPLE-ARITHMETIC-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-CELL-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-CHARSET-TYPE-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-CONTROL-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-DIVISION-BY-ZERO').
mop_direct(cl_simple_error, submop, 'SIMPLE-END-OF-FILE').
mop_direct(cl_simple_error, submop, 'SIMPLE-FILE-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-FLOATING-POINT-OVERFLOW').
mop_direct(cl_simple_error, submop, 'SIMPLE-FLOATING-POINT-UNDERFLOW').
mop_direct(cl_simple_error, submop, 'SIMPLE-KEYWORD-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-MISSING-LOAD-FORM').
mop_direct(cl_simple_error, submop, 'SIMPLE-OS-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-PACKAGE-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-PARSE-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-PRINT-NOT-READABLE').
mop_direct(cl_simple_error, submop, 'SIMPLE-PROGRAM-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-READER-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-REFERENCE-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-SOURCE-PROGRAM-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-STREAM-ERROR').
mop_direct(cl_simple_error, submop, 'SIMPLE-UNBOUND-SLOT').
mop_direct(cl_simple_error, submop, 'SIMPLE-UNBOUND-VARIABLE').
mop_direct(cl_simple_error, submop, 'SIMPLE-UNDEFINED-FUNCTION').
mop_direct(cl_simple_error, submop, 'UNSET-FUNCALLABLE-INSTANCE-FUNCTION').
mop_direct(cl_simple_error, submop, 'UNSUPPORTED-OPERATOR').
mop_direct(cl_simple_error, supers, [cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_simple_error, supers, [cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-FILE-ERROR', supers, [cl_simple_condition, cl_file_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-FILE-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_file_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-FLOATING-POINT-OVERFLOW', supers, [cl_simple_error, cl_simple_condition, cl_floating_point_overflow, cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-FLOATING-POINT-UNDERFLOW', supers, [cl_simple_error, cl_simple_condition, cl_floating_point_underflow, cl_arithmetic_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-GF-ALREADY-CALLED-WARNING', supers, [cl_simple_condition, 'GF-ALREADY-CALLED-WARNING', 'CLOS-WARNING', cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-GF-REPLACING-METHOD-WARNING', supers, [cl_simple_condition, 'GF-REPLACING-METHOD-WARNING', 'CLOS-WARNING', cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-INTERRUPT-CONDITION', supers, [cl_simple_condition, 'INTERRUPT-CONDITION', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-KEYWORD-ERROR', supers, [cl_simple_error, cl_simple_condition, 'KEYWORD-ERROR', cl_program_error, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-MISSING-LOAD-FORM', supers, [cl_simple_error, cl_simple_condition, 'MISSING-LOAD-FORM', 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-OS-ERROR', supers, [cl_simple_error, cl_simple_condition, 'OS-ERROR', 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-PACKAGE-ERROR', supers, [cl_simple_condition, cl_package_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-PACKAGE-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_package_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-PARSE-ERROR', supers, [cl_simple_condition, cl_parse_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-PARSE-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_parse_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-PRINT-NOT-READABLE', supers, [cl_simple_error, cl_simple_condition, cl_print_not_readable, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-PROGRAM-ERROR', submop, 'GENERIC-FUNCTION-LAMBDA-LIST-ERROR').
mop_direct('SIMPLE-PROGRAM-ERROR', submop, 'INVALID-METHOD-INITARG').
mop_direct('SIMPLE-PROGRAM-ERROR', submop, 'SPECIALIZED-LAMBDA-LIST-ERROR').
mop_direct('SIMPLE-PROGRAM-ERROR', supers, [cl_simple_condition, cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-PROGRAM-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_program_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-READER-ERROR', submop, 'READER-IMPOSSIBLE-NUMBER-ERROR').
mop_direct('SIMPLE-READER-ERROR', submop, 'SIMPLE-READER-PACKAGE-ERROR').
mop_direct('SIMPLE-READER-ERROR', supers, [cl_reader_error, cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-READER-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_reader_error, cl_parse_error, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-READER-PACKAGE-ERROR', supers, ['SIMPLE-READER-ERROR', cl_reader_error, cl_parse_error, cl_stream_error, cl_simple_condition, cl_package_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-REFERENCE-ERROR', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-REFERENCE-WARNING', supers, ['REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-SERIOUS-CONDITION', supers, [cl_simple_condition, cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-SOURCE-PROGRAM-ERROR', supers, [cl_simple_error, cl_simple_condition, 'SOURCE-PROGRAM-ERROR', cl_program_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-STORAGE-CONDITION', supers, [cl_simple_condition, cl_storage_condition, cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-STORAGE-CONDITION', supers, [cl_storage_condition, cl_serious_condition, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-STREAM-ERROR', supers, [cl_simple_condition, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-STREAM-ERROR', supers, [cl_simple_error, cl_simple_condition, cl_stream_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_simple_string, submop, 'SIMPLE-ARRAY-NIL').
mop_direct(cl_simple_string, submop, 'SIMPLE-BASE-STRING').
mop_direct(cl_simple_string, submop, 'SIMPLE-CHARACTER-STRING').
mop_direct(cl_simple_string, supers, [cl_string, cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct('SIMPLE-STYLE-WARNING', submop, 'FORMAT-TOO-MANY-ARGS-WARNING').
mop_direct('SIMPLE-STYLE-WARNING', submop, 'INLINING-DEPENDENCY-FAILURE').
mop_direct('SIMPLE-STYLE-WARNING', submop, 'STRUCTURE-INITARG-NOT-KEYWORD').
mop_direct('SIMPLE-STYLE-WARNING', submop, 'TYPE-STYLE-WARNING').
mop_direct('SIMPLE-STYLE-WARNING', supers, [cl_simple_condition, cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SIMPLE-STYLE-WARNING', supers, [cl_simple_condition, cl_style_warning, cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-THREAD-ERROR', supers, ['THREAD-ERROR', 'ERROR', cl_serious_condition, cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_simple_type_error, submop, 'METHOD-CALL-TYPE-ERROR').
mop_direct(cl_simple_type_error, supers, [cl_simple_condition, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_simple_type_error, supers, [cl_simple_condition, cl_type_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-UNBOUND-SLOT', supers, [cl_simple_error, cl_simple_condition, cl_unbound_slot, cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-UNBOUND-VARIABLE', supers, [cl_simple_error, cl_simple_condition, cl_unbound_variable, cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SIMPLE-UNDEFINED-FUNCTION', supers, [cl_simple_error, cl_simple_condition, cl_undefined_function, cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_simple_vector, supers, [cl_vector, 'SIMPLE-ARRAY', cl_array, 'SEQUENCE', t]).
mop_direct(cl_simple_warning, submop, 'ARRAY-INITIAL-ELEMENT-MISMATCH').
mop_direct(cl_simple_warning, submop, 'FORMAT-TOO-FEW-ARGS-WARNING').
mop_direct(cl_simple_warning, submop, 'LOCAL-ARGUMENT-MISMATCH').
mop_direct(cl_simple_warning, submop, 'PACKAGE-AT-VARIANCE').
mop_direct(cl_simple_warning, submop, 'PRINT-OBJECT-STREAM-SPECIALIZER').
mop_direct(cl_simple_warning, submop, 'SIMPLE-REFERENCE-WARNING').
mop_direct(cl_simple_warning, submop, 'TYPE-WARNING').
mop_direct(cl_simple_warning, supers, [cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_simple_warning, supers, [cl_simple_condition, cl_warning, cl_condition, cl_standard_object, t]).
mop_direct(cl_single_float, supers, ['FLOAT', 'REAL', 'NUMBER', t]).
mop_direct(cl_slot_class, submop, 'CONDITION-CLASS').
mop_direct(cl_slot_class, submop, 'STD-CLASS').
mop_direct(cl_slot_class, submop, cl_structure_class).
mop_direct(cl_slot_class, supers, ['PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_slot_class, szlot, 'DIRECT-SLOTS').
mop_direct(cl_slot_class, szlot, 'SLOTS').
mop_direct(cl_slot_definition, submop, 'CONDITION-SLOT-DEFINITION').
mop_direct(cl_slot_definition, submop, 'DIRECT-SLOT-DEFINITION').
mop_direct(cl_slot_definition, submop, 'EFFECTIVE-SLOT-DEFINITION').
mop_direct(cl_slot_definition, submop, 'STANDARD-SLOT-DEFINITION').
mop_direct(cl_slot_definition, submop, 'STRUCTURE-SLOT-DEFINITION').
mop_direct(cl_slot_definition, supers, ['METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_slot_definition, supers, ['METAOBJECT', cl_standard_object, t]).
mop_direct(cl_slot_definition, szlot, '$ALLOCATION').
mop_direct(cl_slot_definition, szlot, '$INHERITABLE-DOC').
mop_direct(cl_slot_definition, szlot, '$INHERITABLE-INITER').
mop_direct(cl_slot_definition, szlot, '$INITARGS').
mop_direct(cl_slot_definition, szlot, '$NAME').
mop_direct(cl_slot_definition, szlot, '$TYPE').
mop_direct(cl_slot_definition, szlot, '%CLASS').
mop_direct(cl_slot_definition, szlot, '%DOCUMENTATION').
mop_direct(cl_slot_definition, szlot, '%TYPE').
mop_direct(cl_slot_definition, szlot, 'INITARGS').
mop_direct(cl_slot_definition, szlot, 'INITFORM').
mop_direct(cl_slot_definition, szlot, 'INITFUNCTION').
mop_direct(cl_slot_definition, szlot, 'NAME').
mop_direct(cl_slot_definition, szlot, 'SOURCE').
mop_direct('SLOT-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SLOT-INFO', szlot, 'BOUNDP').
mop_direct('SLOT-INFO', szlot, 'READER').
mop_direct('SLOT-INFO', szlot, 'TYPECHECK').
mop_direct('SLOT-INFO', szlot, 'WRITER').
mop_direct('SLOT-OBJECT', submop, cl_condition).
mop_direct('SLOT-OBJECT', submop, cl_standard_object).
mop_direct('SLOT-OBJECT', submop, cl_structure_object).
mop_direct('SLOT-OBJECT', supers, [t]).
mop_direct('SLOTD-INITIALIZATION-ERROR', submop, 'SLOTD-INITIALIZATION-TYPE-ERROR').
mop_direct('SLOTD-INITIALIZATION-ERROR', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SLOTD-INITIALIZATION-ERROR', szlot, 'INITARG').
mop_direct('SLOTD-INITIALIZATION-ERROR', szlot, 'KIND').
mop_direct('SLOTD-INITIALIZATION-ERROR', szlot, 'VALUE').
mop_direct('SLOTD-INITIALIZATION-TYPE-ERROR', supers, ['SLOTD-INITIALIZATION-ERROR', 'REFERENCE-CONDITION', cl_type_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SLOTD-INITIALIZATION-TYPE-ERROR', szlot, 'VALUE').
mop_direct('SLOTTED-CLASS', submop, 'SEMI-STANDARD-CLASS').
mop_direct('SLOTTED-CLASS', submop, cl_structure_class).
mop_direct('SLOTTED-CLASS', supers, ['CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('SLOTTED-CLASS', szlot, '$DIRECT-ACCESSORS').
mop_direct('SLOTTED-CLASS', szlot, '$GENERIC-ACCESSORS').
mop_direct('SLOTTED-CLASS', szlot, '$INSTANCE-SIZE').
mop_direct('SLOTTED-CLASS', szlot, '$SUBCLASS-OF-STABLEHASH-P').
mop_direct('SLOTTED-CLASS', szlot, '$VALID-INITARGS-FROM-SLOTS').
mop_direct('SOURCE-FORM-CACHE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SOURCE-FORM-CACHE', szlot, 'DEBUG-SOURCE').
mop_direct('SOURCE-FORM-CACHE', szlot, 'LAST-FORM-RETRIEVED').
mop_direct('SOURCE-FORM-CACHE', szlot, 'LAST-LOCATION-RETRIEVED').
mop_direct('SOURCE-FORM-CACHE', szlot, 'TOPLEVEL-FORM-INDEX').
mop_direct('SOURCE-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SOURCE-INFO', szlot, 'FILE-INFO').
mop_direct('SOURCE-INFO', szlot, 'LAST-DEFN-SOURCE-LOC').
mop_direct('SOURCE-INFO', szlot, 'PARENT').
mop_direct('SOURCE-INFO', szlot, 'START-REAL-TIME').
mop_direct('SOURCE-INFO', szlot, 'START-TIME').
mop_direct('SOURCE-INFO', szlot, 'STREAM').
mop_direct('SOURCE-PROGRAM-ERROR', submop, 'SIMPLE-SOURCE-PROGRAM-ERROR').
mop_direct('SOURCE-PROGRAM-ERROR', supers, [cl_program_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('SOURCE-PROGRAM-ERROR', szlot, '$DETAIL').
mop_direct('SOURCE-PROGRAM-ERROR', szlot, '$FORM').
mop_direct('SPECIAL-FORM-FUNCTION', supers, [cl_undefined_function, cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'COMPLEX-TYPECODE').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'CTYPE').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'FIXNUM-P').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'IMPORTANCE').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'INITIAL-ELEMENT-DEFAULT').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'N-BITS').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'N-PAD-ELEMENTS').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'PRIMITIVE-TYPE-NAME').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'SPECIFIER').
mop_direct('SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES', szlot, 'TYPECODE').
mop_direct('SPECIALIZED-LAMBDA-LIST-ERROR', supers, ['REFERENCE-CONDITION', 'SIMPLE-PROGRAM-ERROR', cl_simple_condition, cl_program_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SPECIALIZER', submop, 'EQL-SPECIALIZER').
mop_direct('SPECIALIZER', submop, 'EXACT-CLASS-SPECIALIZER').
mop_direct('SPECIALIZER', submop, 'POTENTIAL-CLASS').
mop_direct('SPECIALIZER', submop, 'SPECIALIZER-WITH-OBJECT').
mop_direct('SPECIALIZER', submop, 'STANDARD-SPECIALIZER').
mop_direct('SPECIALIZER', supers, ['METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('SPECIALIZER', supers, ['STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('SPECIALIZER', szlot, '$DIRECT-METHODS').
mop_direct('SPECIALIZER', szlot, '%TYPE').
mop_direct('SPECIALIZER-WITH-OBJECT', submop, 'CLASS-EQ-SPECIALIZER').
mop_direct('SPECIALIZER-WITH-OBJECT', submop, 'CLASS-PROTOTYPE-SPECIALIZER').
mop_direct('SPECIALIZER-WITH-OBJECT', submop, 'EQL-SPECIALIZER').
mop_direct('SPECIALIZER-WITH-OBJECT', supers, ['SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('SSET', submop, 'ORDERED-SET').
mop_direct('SSET', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SSET', szlot, 'COUNT').
mop_direct('SSET', szlot, 'FREE').
mop_direct('SSET', szlot, 'VECTOR').
mop_direct('SSET-ELEMENT', submop, 'CBLOCK').
mop_direct('SSET-ELEMENT', submop, 'CONSTRAINT').
mop_direct('SSET-ELEMENT', submop, 'INSTRUCTION').
mop_direct('SSET-ELEMENT', submop, 'LEAF').
mop_direct('SSET-ELEMENT', submop, 'NODE').
mop_direct('SSET-ELEMENT', submop, 'TN').
mop_direct('SSET-ELEMENT', submop, 'VERTEX').
mop_direct('SSET-ELEMENT', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('SSET-ELEMENT', szlot, 'NUMBER').
mop_direct('STANDARD-ACCESSOR-METHOD', submop, 'STANDARD-BOUNDP-METHOD').
mop_direct('STANDARD-ACCESSOR-METHOD', submop, 'STANDARD-READER-METHOD').
mop_direct('STANDARD-ACCESSOR-METHOD', submop, 'STANDARD-WRITER-METHOD').
mop_direct('STANDARD-ACCESSOR-METHOD', supers, ['ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-ACCESSOR-METHOD', supers, ['STANDARD-METHOD', 'METHOD', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('STANDARD-ACCESSOR-METHOD', szlot, '$SLOT-DEFINITION').
mop_direct('STANDARD-ACCESSOR-METHOD', szlot, '%SLOT-DEFINITION').
mop_direct('STANDARD-BOUNDP-METHOD', supers, ['STANDARD-ACCESSOR-METHOD', 'ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_standard_class, supers, ['SEMI-STANDARD-CLASS', 'SLOTTED-CLASS', 'CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct(cl_standard_class, supers, ['STD-CLASS', cl_slot_class, 'PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-CLASSOID', supers, ['CLASSOID', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-DIRECT-SLOT-DEFINITION', supers, ['DIRECT-SLOT-DEFINITION', 'STANDARD-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('STANDARD-DIRECT-SLOT-DEFINITION', supers, ['STANDARD-SLOT-DEFINITION', 'DIRECT-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-EFFECTIVE-SLOT-DEFINITION', supers, ['EFFECTIVE-SLOT-DEFINITION', 'STANDARD-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('STANDARD-EFFECTIVE-SLOT-DEFINITION', supers, ['STANDARD-SLOT-DEFINITION', 'EFFECTIVE-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-EFFECTIVE-SLOT-DEFINITION', szlot, 'LOCATION').
mop_direct('STANDARD-FUNCALLABLE-INSTANCE', supers, [t]).
mop_direct('STANDARD-FUNCALLABLE-INSTANCE', szlot, 'CLOS-SLOTS').
mop_direct('STANDARD-FUNCALLABLE-INSTANCE', szlot, 'HASH-CODE').
mop_direct('STANDARD-GENERIC-FUNCTION', supers, ['GENERIC-FUNCTION', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METAOBJECT', cl_funcallable_standard_object, cl_function, cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-GENERIC-FUNCTION', supers, ['GENERIC-FUNCTION', 'METAOBJECT', cl_funcallable_standard_object, cl_function, cl_standard_object, t]).
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$ARGORDER').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$DECLSPECS').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$DEFAULT-METHOD-CLASS').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$DOCUMENTATION').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$EFFECTIVE-METHOD-CACHE').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$INITIALIZED').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$LAMBDA-LIST').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$METHOD-COMBINATION').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$METHODS').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '$SIGNATURE').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '%LOCK').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, '%METHOD-COMBINATION').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'ARG-INFO').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'DECLARATIONS').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'DFUN-STATE').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'INFO-NEEDS-UPDATE').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'METHOD-CLASS').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'METHODS').
mop_direct('STANDARD-GENERIC-FUNCTION', szlot, 'NAME').
mop_direct('STANDARD-METHOD', submop, 'ACCESSOR-METHOD').
mop_direct('STANDARD-METHOD', submop, 'STANDARD-ACCESSOR-METHOD').
mop_direct('STANDARD-METHOD', supers, ['METHOD', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('STANDARD-METHOD', supers, ['PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-METHOD', szlot, '$DOCUMENTATION').
mop_direct('STANDARD-METHOD', szlot, '$FAST-FUNCTION').
mop_direct('STANDARD-METHOD', szlot, '$FUNCTION').
mop_direct('STANDARD-METHOD', szlot, '$GF').
mop_direct('STANDARD-METHOD', szlot, '$LAMBDA-LIST').
mop_direct('STANDARD-METHOD', szlot, '$QUALIFIERS').
mop_direct('STANDARD-METHOD', szlot, '$SIGNATURE').
mop_direct('STANDARD-METHOD', szlot, '$SPECIALIZERS').
mop_direct('STANDARD-METHOD', szlot, '$WANTS-NEXT-METHOD-P').
mop_direct('STANDARD-METHOD', szlot, '%DOCUMENTATION').
mop_direct('STANDARD-METHOD', szlot, '%FUNCTION').
mop_direct('STANDARD-METHOD', szlot, '%GENERIC-FUNCTION').
mop_direct('STANDARD-METHOD', szlot, 'LAMBDA-LIST').
mop_direct('STANDARD-METHOD', szlot, 'QUALIFIERS').
mop_direct('STANDARD-METHOD', szlot, 'SIMPLE-NEXT-METHOD-CALL').
mop_direct('STANDARD-METHOD', szlot, 'SPECIALIZERS').
mop_direct('STANDARD-METHOD-COMBINATION', submop, 'LONG-METHOD-COMBINATION').
mop_direct('STANDARD-METHOD-COMBINATION', submop, 'SHORT-METHOD-COMBINATION').
mop_direct('STANDARD-METHOD-COMBINATION', supers, ['DEFINITION-SOURCE-MIXIN', 'METHOD-COMBINATION', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-METHOD-COMBINATION', szlot, 'OPTIONS').
mop_direct('STANDARD-METHOD-COMBINATION', szlot, 'TYPE-NAME').
mop_direct(cl_standard_object, submop, cl_condition).
mop_direct(cl_standard_object, submop, 'DEFINITION-SOURCE-MIXIN').
mop_direct(cl_standard_object, submop, cl_funcallable_standard_object).
mop_direct(cl_standard_object, submop, 'FUNDAMENTAL-STREAM').
mop_direct(cl_standard_object, submop, 'GENERIC-STREAM-CONTROLLER').
mop_direct(cl_standard_object, submop, 'METAOBJECT').
mop_direct(cl_standard_object, submop, 'PLIST-MIXIN').
mop_direct(cl_standard_object, submop, 'STANDARD-STABLEHASH').
mop_direct(cl_standard_object, supers, ['SLOT-OBJECT', t]).
mop_direct(cl_standard_object, supers, [t]).
mop_direct('STANDARD-PPRINT-DISPATCH-TABLE-MODIFIED-ERROR', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-PPRINT-DISPATCH-TABLE-MODIFIED-ERROR', szlot, 'OPERATION').
mop_direct('STANDARD-READER-METHOD', supers, ['STANDARD-ACCESSOR-METHOD', 'ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-READER-METHOD', supers, ['STANDARD-ACCESSOR-METHOD', 'STANDARD-METHOD', 'METHOD', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('STANDARD-READTABLE-MODIFIED-ERROR', supers, ['REFERENCE-CONDITION', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-READTABLE-MODIFIED-ERROR', szlot, 'OPERATION').
mop_direct('STANDARD-SLOT-DEFINITION', submop, 'STANDARD-DIRECT-SLOT-DEFINITION').
mop_direct('STANDARD-SLOT-DEFINITION', submop, 'STANDARD-EFFECTIVE-SLOT-DEFINITION').
mop_direct('STANDARD-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('STANDARD-SLOT-DEFINITION', szlot, 'ALLOCATION').
mop_direct('STANDARD-SLOT-DEFINITION', szlot, 'ALLOCATION-CLASS').
mop_direct('STANDARD-SPECIALIZER', submop, 'CLASS').
mop_direct('STANDARD-SPECIALIZER', submop, 'CLASS-EQ-SPECIALIZER').
mop_direct('STANDARD-SPECIALIZER', submop, 'CLASS-PROTOTYPE-SPECIALIZER').
mop_direct('STANDARD-SPECIALIZER', submop, 'EQL-SPECIALIZER').
mop_direct('STANDARD-SPECIALIZER', supers, ['SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-STABLEHASH', submop, 'METHOD').
mop_direct('STANDARD-STABLEHASH', submop, 'SPECIALIZER').
mop_direct('STANDARD-STABLEHASH', submop, 'SUPER-CLASS').
mop_direct('STANDARD-STABLEHASH', supers, [cl_standard_object, t]).
mop_direct('STANDARD-STABLEHASH', szlot, '$HASHCODE').
mop_direct('STANDARD-WRITER-METHOD', supers, ['STANDARD-ACCESSOR-METHOD', 'ACCESSOR-METHOD', 'STANDARD-METHOD', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'METHOD', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STANDARD-WRITER-METHOD', supers, ['STANDARD-ACCESSOR-METHOD', 'STANDARD-METHOD', 'METHOD', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('STAT-VFS', supers, [cl_structure_object, t]).
mop_direct('STAT-VFS', szlot, 'BAVAIL').
mop_direct('STAT-VFS', szlot, 'BFREE').
mop_direct('STAT-VFS', szlot, 'BLOCKS').
mop_direct('STAT-VFS', szlot, 'BSIZE').
mop_direct('STAT-VFS', szlot, 'FAVAIL').
mop_direct('STAT-VFS', szlot, 'FFREE').
mop_direct('STAT-VFS', szlot, 'FILE').
mop_direct('STAT-VFS', szlot, 'FILES').
mop_direct('STAT-VFS', szlot, 'FLAG').
mop_direct('STAT-VFS', szlot, 'FRSIZE').
mop_direct('STAT-VFS', szlot, 'FS-TYPE').
mop_direct('STAT-VFS', szlot, 'FSID').
mop_direct('STAT-VFS', szlot, 'NAMEMAX').
mop_direct('STAT-VFS', szlot, 'VOL-NAME').
mop_direct('STATIC-CLASSOID', supers, ['CLASSOID', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('STD-CLASS', submop, cl_funcallable_standard_class).
mop_direct('STD-CLASS', submop, cl_standard_class).
mop_direct('STD-CLASS', supers, [cl_slot_class, 'PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STEP-CONDITION', submop, 'STEP-FINISHED-CONDITION').
mop_direct('STEP-CONDITION', submop, 'STEP-FORM-CONDITION').
mop_direct('STEP-CONDITION', submop, 'STEP-RESULT-CONDITION').
mop_direct('STEP-CONDITION', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STEP-CONDITION', szlot, 'FORM').
mop_direct('STEP-FINISHED-CONDITION', supers, ['STEP-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STEP-FORM-CONDITION', supers, ['STEP-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STEP-FORM-CONDITION', szlot, 'ARGS').
mop_direct('STEP-RESULT-CONDITION', submop, 'STEP-VALUES-CONDITION').
mop_direct('STEP-RESULT-CONDITION', supers, ['STEP-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STEP-RESULT-CONDITION', szlot, 'RESULT').
mop_direct('STEP-VALUES-CONDITION', supers, ['STEP-RESULT-CONDITION', 'STEP-CONDITION', cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_storage_condition, submop, 'ALIEN-STACK-EXHAUSTED').
mop_direct(cl_storage_condition, submop, 'BINDING-STACK-EXHAUSTED').
mop_direct(cl_storage_condition, submop, 'CONTROL-STACK-EXHAUSTED').
mop_direct(cl_storage_condition, submop, 'HEAP-EXHAUSTED-ERROR').
mop_direct(cl_storage_condition, submop, 'SIMPLE-STORAGE-CONDITION').
mop_direct(cl_storage_condition, supers, [cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_storage_condition, supers, [cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('STORAGE-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('STORAGE-INFO', szlot, 'DEBUG-VARS').
mop_direct('STORAGE-INFO', szlot, 'GROUPS').
mop_direct(cl_stream, submop, 'ANSI-STREAM').
mop_direct(cl_stream, submop, cl_broadcast_stream).
mop_direct(cl_stream, submop, cl_concatenated_stream).
mop_direct(cl_stream, submop, cl_echo_stream).
mop_direct(cl_stream, submop, cl_file_stream).
mop_direct(cl_stream, submop, 'FUNDAMENTAL-STREAM').
mop_direct(cl_stream, submop, 'STRING-STREAM').
mop_direct(cl_stream, submop, cl_synonym_stream).
mop_direct(cl_stream, submop, cl_two_way_stream).
mop_direct(cl_stream, supers, [t]).
mop_direct('STREAM-DECODING-ERROR', supers, [cl_stream_error, 'CHARACTER-DECODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('STREAM-ENCODING-ERROR', supers, [cl_stream_error, 'CHARACTER-ENCODING-ERROR', 'CHARACTER-CODING-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_stream_error, submop, 'CLOSED-STREAM-ERROR').
mop_direct(cl_stream_error, submop, cl_end_of_file).
mop_direct(cl_stream_error, submop, 'IO-TIMEOUT').
mop_direct(cl_stream_error, submop, cl_reader_error).
mop_direct(cl_stream_error, submop, 'SIMPLE-STREAM-ERROR').
mop_direct(cl_stream_error, submop, 'STREAM-DECODING-ERROR').
mop_direct(cl_stream_error, submop, 'STREAM-ENCODING-ERROR').
mop_direct(cl_stream_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_stream_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_stream_error, szlot, '$STREAM').
mop_direct(cl_stream_error, szlot, 'STREAM').
mop_direct(cl_string, submop, 'BASE-STRING').
mop_direct(cl_string, submop, 'CHARACTER-STRING').
mop_direct(cl_string, submop, cl_simple_string).
mop_direct(cl_string, submop, 'VECTOR-NIL').
mop_direct(cl_string, supers, [cl_vector, cl_array, 'SEQUENCE', t]).
mop_direct(cl_string_input_stream, supers, ['STRING-STREAM', 'ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_string_input_stream, szlot, 'CURRENT').
mop_direct(cl_string_input_stream, szlot, 'END').
mop_direct(cl_string_input_stream, szlot, 'IN').
mop_direct(cl_string_input_stream, szlot, 'MISC').
mop_direct(cl_string_input_stream, szlot, 'STRING').
mop_direct(cl_string_output_stream, supers, ['STRING-STREAM', 'ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_string_output_stream, szlot, 'BUFFER').
mop_direct(cl_string_output_stream, szlot, 'ELEMENT-TYPE').
mop_direct(cl_string_output_stream, szlot, 'INDEX').
mop_direct(cl_string_output_stream, szlot, 'INDEX-CACHE').
mop_direct(cl_string_output_stream, szlot, 'MISC').
mop_direct(cl_string_output_stream, szlot, 'NEXT').
mop_direct(cl_string_output_stream, szlot, 'OUT').
mop_direct(cl_string_output_stream, szlot, 'POINTER').
mop_direct(cl_string_output_stream, szlot, 'PREV').
mop_direct(cl_string_output_stream, szlot, 'SOUT').
mop_direct('STRING-STREAM', submop, cl_fill_pointer_output_stream).
mop_direct('STRING-STREAM', submop, cl_string_input_stream).
mop_direct('STRING-STREAM', submop, cl_string_output_stream).
mop_direct('STRING-STREAM', supers, [cl_stream, t]).
mop_direct('STRUCTURE_bang_OBJECT', submop, 'ALIEN-RECORD-FIELD').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'ALIEN-TYPE').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'ANNOTATION').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'ARG-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'BLOCK-ANNOTATION').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'CLASSOID-CELL').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'CLEANUP').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'CLOOP').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'COMPONENT').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'CTRAN').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'CTYPE').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEBUG-FUN').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEBUG-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEBUG-NAME-MARKER').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEBUG-SOURCE').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEFINITION-SOURCE-LOCATION').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEFSTRUCT-DESCRIPTION').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'DEFSTRUCT-SLOT-DESCRIPTION').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'GLOBAL-CONFLICTS').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'HEAP-ALIEN-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'INTERFERENCE-GRAPH').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'IR2-COMPONENT').
mop_direct('STRUCTURE_bang_OBJECT', submop, cl_layout).
mop_direct('STRUCTURE_bang_OBJECT', submop, 'LOCAL-ALIEN-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'LVAR').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'MUTEX').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'NLX-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'OPERAND-PARSE').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'PATTERN').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'PHYSENV').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'PRIM-OBJECT-SLOT').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'PRIMITIVE-OBJECT').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'ROOM-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'SB').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'SC').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'SSET-ELEMENT').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'TAIL-SET').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'TEMPLATE').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'TN-REF').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'TRACE-INFO').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'TYPE-CLASS').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'UNDEFINED-WARNING').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'VOP').
mop_direct('STRUCTURE_bang_OBJECT', submop, 'VOP-PARSE').
mop_direct('STRUCTURE_bang_OBJECT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_structure_class, supers, [cl_slot_class, 'PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct(cl_structure_class, supers, ['SLOTTED-CLASS', 'CLASS', 'POTENTIAL-CLASS', 'SPECIALIZER', 'SUPER-CLASS', 'STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct(cl_structure_class, szlot, '$BOA-CONSTRUCTORS').
mop_direct(cl_structure_class, szlot, '$COPIER').
mop_direct(cl_structure_class, szlot, '$KCONSTRUCTOR').
mop_direct(cl_structure_class, szlot, '$NAMES').
mop_direct(cl_structure_class, szlot, '$PREDICATE').
mop_direct(cl_structure_class, szlot, '$PROTOTYPE').
mop_direct(cl_structure_class, szlot, 'DEFSTRUCT-CONSTRUCTOR').
mop_direct(cl_structure_class, szlot, 'DEFSTRUCT-FORM').
mop_direct(cl_structure_class, szlot, 'FROM-DEFCLASS-P').
mop_direct('STRUCTURE-CLASSOID', supers, ['CLASSOID', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('STRUCTURE-DIRECT-SLOT-DEFINITION', supers, ['DIRECT-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('STRUCTURE-DIRECT-SLOT-DEFINITION', supers, ['STRUCTURE-SLOT-DEFINITION', 'DIRECT-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', supers, ['EFFECTIVE-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, t]).
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', supers, ['STRUCTURE-SLOT-DEFINITION', 'EFFECTIVE-SLOT-DEFINITION', cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SBUC').
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SMUC').
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SSVUC').
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', szlot, '$EFM-SVUC').
mop_direct('STRUCTURE-EFFECTIVE-SLOT-DEFINITION', szlot, '$READONLY').
mop_direct('STRUCTURE-INITARG-NOT-KEYWORD', supers, ['REFERENCE-CONDITION', 'SIMPLE-STYLE-WARNING', cl_simple_condition, cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_structure_object, submop, 'ABSTRACT-LEXENV').
mop_direct(cl_structure_object, submop, 'ALIEN-TYPE-CLASS').
mop_direct(cl_structure_object, submop, 'ALIEN-VALUE').
mop_direct(cl_structure_object, submop, 'ANODE').
mop_direct(cl_structure_object, submop, 'ANSI-STREAM').
mop_direct(cl_structure_object, submop, 'APPROXIMATE-FUN-TYPE').
mop_direct(cl_structure_object, submop, 'APPROXIMATE-KEY-INFO').
mop_direct(cl_structure_object, submop, 'ARG').
mop_direct(cl_structure_object, submop, 'ARG-FORM-KIND').
mop_direct(cl_structure_object, submop, 'ARG-INFO').
mop_direct(cl_structure_object, submop, 'ARG-STATE').
mop_direct(cl_structure_object, submop, 'BREAKPOINT').
mop_direct(cl_structure_object, submop, 'BREAKPOINT-DATA').
mop_direct(cl_structure_object, submop, 'BUFFER').
mop_direct(cl_structure_object, submop, 'C-SOURCE-POINT').
mop_direct(cl_structure_object, submop, 'CACHE').
mop_direct(cl_structure_object, submop, 'CACHED-FUN').
mop_direct(cl_structure_object, submop, 'CALLBACK-INFO').
mop_direct(cl_structure_object, submop, 'CIRCULARITY').
mop_direct(cl_structure_object, submop, 'CLASS-PRECEDENCE-DESCRIPTION').
mop_direct(cl_structure_object, submop, 'CODE-LOCATION').
mop_direct(cl_structure_object, submop, 'COMMA').
mop_direct(cl_structure_object, submop, 'COMPILER-ERROR-CONTEXT').
mop_direct(cl_structure_object, submop, 'CONDITION-SLOT').
mop_direct(cl_structure_object, submop, 'CONSET').
mop_direct(cl_structure_object, submop, 'CONST').
mop_direct(cl_structure_object, submop, 'CONTROL-STRING-DIRECTIVE').
mop_direct(cl_structure_object, submop, 'CORE-OBJECT').
mop_direct(cl_structure_object, submop, 'COUNTER').
mop_direct(cl_structure_object, submop, 'DEAD-BEEF-STRUCTURE-OBJECT').
mop_direct(cl_structure_object, submop, 'DEBUG-BLOCK').
mop_direct(cl_structure_object, submop, 'DEBUG-FUN').
mop_direct(cl_structure_object, submop, 'DEBUG-VAR').
mop_direct(cl_structure_object, submop, 'DEPRECATION-INFO').
mop_direct(cl_structure_object, submop, 'DFUN-INFO').
mop_direct(cl_structure_object, submop, 'DISASSEM-STATE').
mop_direct(cl_structure_object, submop, 'EA').
mop_direct(cl_structure_object, submop, 'ENCAPSULATION-INFO').
mop_direct(cl_structure_object, submop, 'ENTRY-INFO').
mop_direct(cl_structure_object, submop, 'ENV').
mop_direct(cl_structure_object, submop, 'EVENT-INFO').
mop_direct(cl_structure_object, submop, 'EXTERNAL-FORMAT').
mop_direct(cl_structure_object, submop, 'FASL-INPUT').
mop_direct(cl_structure_object, submop, 'FASL-OUTPUT').
mop_direct(cl_structure_object, submop, 'FAST-INSTANCE-BOUNDP').
mop_direct(cl_structure_object, submop, 'FAST-METHOD-CALL').
mop_direct(cl_structure_object, submop, 'FFI-MODULE').
mop_direct(cl_structure_object, submop, 'FGEN').
mop_direct(cl_structure_object, submop, 'FILE-INFO').
mop_direct(cl_structure_object, submop, 'FILE-STAT').
mop_direct(cl_structure_object, submop, 'FIXUP').
mop_direct(cl_structure_object, submop, 'FIXUP-NOTE').
mop_direct(cl_structure_object, submop, 'FNODE').
mop_direct(cl_structure_object, submop, 'FORMAT-DIRECTIVE').
mop_direct(cl_structure_object, submop, 'FRAME').
mop_direct(cl_structure_object, submop, 'FUN-CACHE').
mop_direct(cl_structure_object, submop, 'FUN-END-COOKIE').
mop_direct(cl_structure_object, submop, 'FUN-INFO').
mop_direct(cl_structure_object, submop, 'FUNSTATE').
mop_direct(cl_structure_object, submop, 'GROUP-INFO').
mop_direct(cl_structure_object, submop, 'HANDLER').
mop_direct(cl_structure_object, submop, cl_hash_table).
mop_direct(cl_structure_object, submop, 'HOST').
mop_direct(cl_structure_object, submop, 'HOSTENT').
mop_direct(cl_structure_object, submop, 'HUFFMAN-NODE').
mop_direct(cl_structure_object, submop, 'INFO-HASHTABLE').
mop_direct(cl_structure_object, submop, 'INPUT-CHARACTER').
mop_direct(cl_structure_object, submop, 'INSPECTION').
mop_direct(cl_structure_object, submop, 'INST-SPACE').
mop_direct(cl_structure_object, submop, 'INST-SPACE-CHOICE').
mop_direct(cl_structure_object, submop, 'INSTRUCTION').
mop_direct(cl_structure_object, submop, 'INSTRUCTION-FORMAT').
mop_direct(cl_structure_object, submop, 'INTERVAL').
mop_direct(cl_structure_object, submop, 'IR2-LVAR').
mop_direct(cl_structure_object, submop, 'IR2-NLX-INFO').
mop_direct(cl_structure_object, submop, 'IR2-PHYSENV').
mop_direct(cl_structure_object, submop, 'KEY-INFO').
mop_direct(cl_structure_object, submop, 'LOCALE-CONV').
mop_direct(cl_structure_object, submop, 'LOCATION-GROUP').
mop_direct(cl_structure_object, submop, 'LOCATION-INFO').
mop_direct(cl_structure_object, submop, 'LOGICAL-BLOCK').
mop_direct(cl_structure_object, submop, 'LOOP-COLLECTOR').
mop_direct(cl_structure_object, submop, 'LOOP-INITIALIZATION').
mop_direct(cl_structure_object, submop, 'LOOP-MINIMAX').
mop_direct(cl_structure_object, submop, 'LOOP-PATH').
mop_direct(cl_structure_object, submop, 'LOOP-UNIVERSE').
mop_direct(cl_structure_object, submop, 'MATCH').
mop_direct(cl_structure_object, submop, 'META-INFO').
mop_direct(cl_structure_object, submop, 'METHOD-CALL').
mop_direct(cl_structure_object, submop, 'MODULAR-CLASS').
mop_direct(cl_structure_object, submop, 'MODULAR-FUN-INFO').
mop_direct(cl_structure_object, submop, 'OFFS-HOOK').
mop_direct(cl_structure_object, submop, 'OVERHEAD').
mop_direct(cl_structure_object, submop, cl_package).
mop_direct(cl_structure_object, submop, 'PACKAGE-HASHTABLE').
mop_direct(cl_structure_object, submop, cl_pathname).
mop_direct(cl_structure_object, submop, 'POLICY').
mop_direct(cl_structure_object, submop, 'POLICY-DEPENDENT-QUALITY').
mop_direct(cl_structure_object, submop, 'POLLFDS').
mop_direct(cl_structure_object, submop, 'PPRINT-DISPATCH-ENTRY').
mop_direct(cl_structure_object, submop, 'PPRINT-DISPATCH-TABLE').
mop_direct(cl_structure_object, submop, 'PRIMITIVE-TYPE').
mop_direct(cl_structure_object, submop, 'PRIORITY-QUEUE').
mop_direct(cl_structure_object, submop, 'PROCESS').
mop_direct(cl_structure_object, submop, 'PROFILE-INFO').
mop_direct(cl_structure_object, submop, 'PV-TABLE').
mop_direct(cl_structure_object, submop, 'QUEUED-OP').
mop_direct(cl_structure_object, submop, cl_random_state).
mop_direct(cl_structure_object, submop, 'RAW-SLOT-DATA').
mop_direct(cl_structure_object, submop, cl_readtable).
mop_direct(cl_structure_object, submop, 'REG-SPEC').
mop_direct(cl_structure_object, submop, 'RESTART').
mop_direct(cl_structure_object, submop, 'RESULT-STATE').
mop_direct(cl_structure_object, submop, 'RETURN-INFO').
mop_direct(cl_structure_object, submop, 'RLIMIT').
mop_direct(cl_structure_object, submop, 'SEGMENT').
mop_direct(cl_structure_object, submop, 'SEMAPHORE').
mop_direct(cl_structure_object, submop, 'SEMAPHORE-NOTIFICATION').
mop_direct(cl_structure_object, submop, 'SERVICE').
mop_direct(cl_structure_object, submop, 'SESSION').
mop_direct(cl_structure_object, submop, 'SHARED-OBJECT').
mop_direct(cl_structure_object, submop, 'SLOT-INFO').
mop_direct(cl_structure_object, submop, 'SOURCE-FORM-CACHE').
mop_direct(cl_structure_object, submop, 'SOURCE-INFO').
mop_direct(cl_structure_object, submop, 'SPECIALIZED-ARRAY-ELEMENT-TYPE-PROPERTIES').
mop_direct(cl_structure_object, submop, 'SSET').
mop_direct(cl_structure_object, submop, 'STAT-VFS').
mop_direct(cl_structure_object, submop, 'STORAGE-INFO').
mop_direct(cl_structure_object, submop, 'STRUCTURE_bang_OBJECT').
mop_direct(cl_structure_object, submop, 'STRUCTURE-STABLEHASH').
mop_direct(cl_structure_object, submop, 'THREAD').
mop_direct(cl_structure_object, submop, 'TIME-INFO').
mop_direct(cl_structure_object, submop, 'TIMER').
mop_direct(cl_structure_object, submop, 'TOKEN-BUF').
mop_direct(cl_structure_object, submop, 'TRANSFORM').
mop_direct(cl_structure_object, submop, 'UNAME').
mop_direct(cl_structure_object, submop, 'UNDEFINED-PACKAGE').
mop_direct(cl_structure_object, submop, 'UNPRINTABLE-OBJECT').
mop_direct(cl_structure_object, submop, 'USAGE').
mop_direct(cl_structure_object, submop, 'USER-INFO').
mop_direct(cl_structure_object, submop, 'UTMPX').
mop_direct(cl_structure_object, submop, 'VALSRC').
mop_direct(cl_structure_object, submop, 'VAR').
mop_direct(cl_structure_object, submop, 'WAITQUEUE').
mop_direct(cl_structure_object, submop, 'XSET').
mop_direct(cl_structure_object, supers, ['SLOT-OBJECT', t]).
mop_direct(cl_structure_object, supers, [t]).
mop_direct('STRUCTURE-SLOT-DEFINITION', submop, 'STRUCTURE-DIRECT-SLOT-DEFINITION').
mop_direct('STRUCTURE-SLOT-DEFINITION', submop, 'STRUCTURE-EFFECTIVE-SLOT-DEFINITION').
mop_direct('STRUCTURE-SLOT-DEFINITION', supers, [cl_slot_definition, 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('STRUCTURE-SLOT-DEFINITION', szlot, 'DEFSTRUCT-ACCESSOR-SYMBOL').
mop_direct('STRUCTURE-SLOT-DEFINITION', szlot, 'INTERNAL-READER-FUNCTION').
mop_direct('STRUCTURE-SLOT-DEFINITION', szlot, 'INTERNAL-WRITER-FUNCTION').
mop_direct('STRUCTURE-STABLEHASH', supers, [cl_structure_object, t]).
mop_direct('STRUCTURE-STABLEHASH', szlot, 'HASHCODE').
mop_direct(cl_style_warning, submop, 'CHARACTER-DECODING-ERROR-IN-COMMENT').
mop_direct(cl_style_warning, submop, 'COMPILER-MACRO-APPLICATION-MISSED-WARNING').
mop_direct(cl_style_warning, submop, 'DEPRECATED-EVAL-WHEN-SITUATIONS').
mop_direct(cl_style_warning, submop, 'DUBIOUS-ASTERISKS-AROUND-VARIABLE-NAME').
mop_direct(cl_style_warning, submop, 'DUPLICATE-CASE-KEY-WARNING').
mop_direct(cl_style_warning, submop, 'EARLY-DEPRECATION-WARNING').
mop_direct(cl_style_warning, submop, 'FTYPE-PROCLAMATION-MISMATCH-WARNING').
mop_direct(cl_style_warning, submop, 'IMPLICIT-GENERIC-FUNCTION-WARNING').
mop_direct(cl_style_warning, submop, 'LEXICAL-ENVIRONMENT-TOO-COMPLEX').
mop_direct(cl_style_warning, submop, 'REDEFINITION-WARNING').
mop_direct(cl_style_warning, submop, 'SAME-FILE-REDEFINITION-WARNING').
mop_direct(cl_style_warning, submop, 'SIMPLE-STYLE-WARNING').
mop_direct(cl_style_warning, submop, 'TYPE-PROCLAMATION-MISMATCH-WARNING').
mop_direct(cl_style_warning, submop, 'UNDEFINED-ALIEN-STYLE-WARNING').
mop_direct(cl_style_warning, supers, [cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_style_warning, supers, [cl_warning, cl_condition, cl_standard_object, t]).
mop_direct('SUPER-CLASS', submop, 'FORWARD-REFERENCED-CLASS').
mop_direct('SUPER-CLASS', submop, 'POTENTIAL-CLASS').
mop_direct('SUPER-CLASS', supers, ['STANDARD-STABLEHASH', 'METAOBJECT', cl_standard_object, t]).
mop_direct('SUPER-CLASS', szlot, '$CLASSNAME').
mop_direct('SUPER-CLASS', szlot, '$DIRECT-SUBCLASSES').
mop_direct(cl_symbol, submop, 'NULL').
mop_direct(cl_symbol, supers, [t]).
mop_direct('SYMBOL-PACKAGE-LOCKED-ERROR', supers, ['PACKAGE-LOCK-VIOLATION', cl_package_error, 'ERROR', cl_serious_condition, 'REFERENCE-CONDITION', cl_simple_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SYMBOL-PACKAGE-LOCKED-ERROR', szlot, 'SYMBOL').
mop_direct('SYMBOL-VALUE-IN-THREAD-ERROR', supers, [cl_cell_error, 'THREAD-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SYMBOL-VALUE-IN-THREAD-ERROR', szlot, 'INFO').
mop_direct(cl_synonym_stream, supers, ['ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_synonym_stream, supers, [cl_stream, t]).
mop_direct(cl_synonym_stream, szlot, 'BIN').
mop_direct(cl_synonym_stream, szlot, 'BOUT').
mop_direct(cl_synonym_stream, szlot, 'IN').
mop_direct(cl_synonym_stream, szlot, 'MISC').
mop_direct(cl_synonym_stream, szlot, 'N-BIN').
mop_direct(cl_synonym_stream, szlot, 'OUT').
mop_direct(cl_synonym_stream, szlot, 'SOUT').
mop_direct(cl_synonym_stream, szlot, 'SYMBOL').
mop_direct('SYSTEM-AREA-POINTER', supers, [t]).
mop_direct('SYSTEM-CLASS', submop, cl_built_in_class).
mop_direct('SYSTEM-CLASS', supers, ['PCL-CLASS', 'CLASS', 'DEPENDENT-UPDATE-MIXIN', 'PLIST-MIXIN', 'DEFINITION-SOURCE-MIXIN', 'STANDARD-SPECIALIZER', 'SPECIALIZER', 'METAOBJECT', cl_standard_object, 'SLOT-OBJECT', t]).
mop_direct('SYSTEM-CONDITION', submop, 'BREAKPOINT-ERROR').
mop_direct('SYSTEM-CONDITION', submop, 'INTERACTIVE-INTERRUPT').
mop_direct('SYSTEM-CONDITION', submop, 'MEMORY-FAULT-ERROR').
mop_direct('SYSTEM-CONDITION', supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct('SYSTEM-CONDITION', szlot, 'ADDRESS').
mop_direct('SYSTEM-CONDITION', szlot, 'CONTEXT').
mop_direct('TAB', supers, ['QUEUED-OP', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TAB', szlot, 'COLINC').
mop_direct('TAB', szlot, 'COLNUM').
mop_direct('TAB', szlot, 'RELATIVEP').
mop_direct('TAB', szlot, 'SECTIONP').
mop_direct('TAIL-SET', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TAIL-SET', szlot, 'FUNS').
mop_direct('TAIL-SET', szlot, 'INFO').
mop_direct('TAIL-SET', szlot, 'TYPE').
mop_direct('TEMPLATE', submop, 'VOP-INFO').
mop_direct('TEMPLATE', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TEMPLATE', szlot, 'ARG-TYPES').
mop_direct('TEMPLATE', szlot, 'COST').
mop_direct('TEMPLATE', szlot, 'GUARD').
mop_direct('TEMPLATE', szlot, 'INFO-ARG-COUNT').
mop_direct('TEMPLATE', szlot, 'LTN-POLICY').
mop_direct('TEMPLATE', szlot, 'MORE-ARGS-TYPE').
mop_direct('TEMPLATE', szlot, 'MORE-RESULTS-TYPE').
mop_direct('TEMPLATE', szlot, 'NAME').
mop_direct('TEMPLATE', szlot, 'NOTE').
mop_direct('TEMPLATE', szlot, 'RESULT-TYPES').
mop_direct('TEMPLATE', szlot, 'TYPE').
mop_direct('THREAD', submop, 'FOREIGN-THREAD').
mop_direct('THREAD', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('THREAD', szlot, '%ALIVE-P').
mop_direct('THREAD', szlot, '%EPHEMERAL-P').
mop_direct('THREAD', szlot, 'INTERRUPTIONS').
mop_direct('THREAD', szlot, 'INTERRUPTIONS-LOCK').
mop_direct('THREAD', szlot, 'NAME').
mop_direct('THREAD', szlot, 'OS-THREAD').
mop_direct('THREAD', szlot, 'RESULT').
mop_direct('THREAD', szlot, 'RESULT-LOCK').
mop_direct('THREAD', szlot, 'WAITING-FOR').
mop_direct('THREAD-DEADLOCK', supers, ['THREAD-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('THREAD-DEADLOCK', szlot, 'CYCLE').
mop_direct('THREAD-ERROR', submop, 'INTERRUPT-THREAD-ERROR').
mop_direct('THREAD-ERROR', submop, 'JOIN-THREAD-ERROR').
mop_direct('THREAD-ERROR', submop, 'SIMPLE-THREAD-ERROR').
mop_direct('THREAD-ERROR', submop, 'SYMBOL-VALUE-IN-THREAD-ERROR').
mop_direct('THREAD-ERROR', submop, 'THREAD-DEADLOCK').
mop_direct('THREAD-ERROR', supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('THREAD-ERROR', szlot, 'THREAD').
mop_direct('TIME-INFO', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TIME-INFO', szlot, 'CALLS').
mop_direct('TIME-INFO', szlot, 'CONSING').
mop_direct('TIME-INFO', szlot, 'GC-RUN-TIME').
mop_direct('TIME-INFO', szlot, 'NAME').
mop_direct('TIME-INFO', szlot, 'SECONDS').
mop_direct('TIMEOUT', submop, 'DEADLINE-TIMEOUT').
mop_direct('TIMEOUT', submop, 'IO-TIMEOUT').
mop_direct('TIMEOUT', supers, [cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('TIMEOUT', szlot, 'SECONDS').
mop_direct('TIMER', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TIMER', szlot, 'CANCEL-FUNCTION').
mop_direct('TIMER', szlot, 'CATCH-UP').
mop_direct('TIMER', szlot, 'EXPIRE-TIME').
mop_direct('TIMER', szlot, 'FUNCTION').
mop_direct('TIMER', szlot, 'INTERRUPT-FUNCTION').
mop_direct('TIMER', szlot, 'NAME').
mop_direct('TIMER', szlot, 'REPEAT-INTERVAL').
mop_direct('TIMER', szlot, 'THREAD').
mop_direct('TN', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TN', szlot, 'COST').
mop_direct('TN', szlot, 'CURRENT-CONFLICT').
mop_direct('TN', szlot, 'GLOBAL-CONFLICTS').
mop_direct('TN', szlot, 'KIND').
mop_direct('TN', szlot, 'LEAF').
mop_direct('TN', szlot, 'LOCAL').
mop_direct('TN', szlot, 'LOCAL-CONFLICTS').
mop_direct('TN', szlot, 'LOCAL-NUMBER').
mop_direct('TN', szlot, 'LOOP-DEPTH').
mop_direct('TN', szlot, 'NEXT').
mop_direct('TN', szlot, 'NEXT*').
mop_direct('TN', szlot, 'OFFSET').
mop_direct('TN', szlot, 'PHYSENV').
mop_direct('TN', szlot, 'PRIMITIVE-TYPE').
mop_direct('TN', szlot, 'READS').
mop_direct('TN', szlot, 'SAVE-TN').
mop_direct('TN', szlot, 'SC').
mop_direct('TN', szlot, 'WRITES').
mop_direct('TN-REF', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TN-REF', szlot, 'ACROSS').
mop_direct('TN-REF', szlot, 'LOAD-TN').
mop_direct('TN-REF', szlot, 'NEXT').
mop_direct('TN-REF', szlot, 'NEXT-REF').
mop_direct('TN-REF', szlot, 'TARGET').
mop_direct('TN-REF', szlot, 'TN').
mop_direct('TN-REF', szlot, 'VOP').
mop_direct('TN-REF', szlot, 'WRITE-P').
mop_direct('TOKEN-BUF', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TOKEN-BUF', szlot, 'ADJUSTABLE-STRING').
mop_direct('TOKEN-BUF', szlot, 'CURSOR').
mop_direct('TOKEN-BUF', szlot, 'ESCAPES').
mop_direct('TOKEN-BUF', szlot, 'FILL-PTR').
mop_direct('TOKEN-BUF', szlot, 'INITIAL-STRING').
mop_direct('TOKEN-BUF', szlot, 'NEXT').
mop_direct('TOKEN-BUF', szlot, 'ONLY-BASE-CHARS').
mop_direct('TOKEN-BUF', szlot, 'STRING').
mop_direct('TRACE-INFO', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TRACE-INFO', szlot, 'BREAK').
mop_direct('TRACE-INFO', szlot, 'BREAK-AFTER').
mop_direct('TRACE-INFO', szlot, 'CONDITION').
mop_direct('TRACE-INFO', szlot, 'CONDITION-AFTER').
mop_direct('TRACE-INFO', szlot, 'ENCAPSULATED').
mop_direct('TRACE-INFO', szlot, 'END-BREAKPOINT').
mop_direct('TRACE-INFO', szlot, 'METHODS').
mop_direct('TRACE-INFO', szlot, 'NAMED').
mop_direct('TRACE-INFO', szlot, 'PRINT').
mop_direct('TRACE-INFO', szlot, 'PRINT-AFTER').
mop_direct('TRACE-INFO', szlot, 'START-BREAKPOINT').
mop_direct('TRACE-INFO', szlot, 'UNTRACED').
mop_direct('TRACE-INFO', szlot, 'WHAT').
mop_direct('TRACE-INFO', szlot, 'WHEREIN').
mop_direct('TRANSFORM', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TRANSFORM', szlot, 'FUNCTION').
mop_direct('TRANSFORM', szlot, 'IMPORTANT').
mop_direct('TRANSFORM', szlot, 'NOTE').
mop_direct('TRANSFORM', szlot, 'TYPE').
mop_direct('TWO-CLASS', supers, ['ONE-CLASS', 'ONE-INDEX-DFUN-INFO', 'ACCESSOR-DFUN-INFO', 'DFUN-INFO', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TWO-CLASS', szlot, 'WRAPPER1').
mop_direct(cl_two_way_stream, submop, cl_echo_stream).
mop_direct(cl_two_way_stream, supers, ['ANSI-STREAM', cl_stream, cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_two_way_stream, supers, [cl_stream, t]).
mop_direct(cl_two_way_stream, szlot, 'BIN').
mop_direct(cl_two_way_stream, szlot, 'BOUT').
mop_direct(cl_two_way_stream, szlot, 'IN').
mop_direct(cl_two_way_stream, szlot, 'INPUT-STREAM').
mop_direct(cl_two_way_stream, szlot, 'MISC').
mop_direct(cl_two_way_stream, szlot, 'N-BIN').
mop_direct(cl_two_way_stream, szlot, 'OUT').
mop_direct(cl_two_way_stream, szlot, 'OUTPUT-STREAM').
mop_direct(cl_two_way_stream, szlot, 'SOUT').
mop_direct('TYPE-CLASS', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('TYPE-CLASS', szlot, 'COMPLEX-=').
mop_direct('TYPE-CLASS', szlot, 'COMPLEX-INTERSECTION2').
mop_direct('TYPE-CLASS', szlot, 'COMPLEX-SUBTYPEP-ARG1').
mop_direct('TYPE-CLASS', szlot, 'COMPLEX-SUBTYPEP-ARG2').
mop_direct('TYPE-CLASS', szlot, 'COMPLEX-UNION2').
mop_direct('TYPE-CLASS', szlot, 'ENUMERABLE-P').
mop_direct('TYPE-CLASS', szlot, 'MIGHT-CONTAIN-OTHER-TYPES-P').
mop_direct('TYPE-CLASS', szlot, 'NAME').
mop_direct('TYPE-CLASS', szlot, 'NEGATE').
mop_direct('TYPE-CLASS', szlot, 'SIMPLE-=').
mop_direct('TYPE-CLASS', szlot, 'SIMPLE-INTERSECTION2').
mop_direct('TYPE-CLASS', szlot, 'SIMPLE-SUBTYPEP').
mop_direct('TYPE-CLASS', szlot, 'SIMPLE-UNION2').
mop_direct('TYPE-CLASS', szlot, 'SINGLETON-P').
mop_direct('TYPE-CLASS', szlot, 'UNPARSE').
mop_direct(cl_type_error, submop, 'ARGUMENT-LIST-DOTTED').
mop_direct(cl_type_error, submop, 'BOUNDING-INDICES-BAD-ERROR').
mop_direct(cl_type_error, submop, 'CASE-FAILURE').
mop_direct(cl_type_error, submop, 'CHARSET-TYPE-ERROR').
mop_direct(cl_type_error, submop, 'INDEX-TOO-LARGE-ERROR').
mop_direct(cl_type_error, submop, 'INVALID-ARRAY-ERROR').
mop_direct(cl_type_error, submop, 'INVALID-ARRAY-INDEX-ERROR').
mop_direct(cl_type_error, submop, 'KEYWORD-ERROR').
mop_direct(cl_type_error, submop, 'LAYOUT-INVALID').
mop_direct(cl_type_error, submop, 'MACROEXPAND-HOOK-TYPE-ERROR').
mop_direct(cl_type_error, submop, 'NIL-ARRAY-ACCESSED-ERROR').
mop_direct(cl_type_error, submop, 'PROTOCOL-UNIMPLEMENTED').
mop_direct(cl_type_error, submop, cl_simple_type_error).
mop_direct(cl_type_error, submop, 'SLOTD-INITIALIZATION-TYPE-ERROR').
mop_direct(cl_type_error, supers, ['ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_type_error, supers, ['ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_type_error, szlot, '$DATUM').
mop_direct(cl_type_error, szlot, '$EXPECTED-TYPE').
mop_direct(cl_type_error, szlot, 'DATUM').
mop_direct(cl_type_error, szlot, 'EXPECTED-TYPE').
mop_direct('TYPE-PROCLAMATION-MISMATCH', submop, 'TYPE-PROCLAMATION-MISMATCH-WARNING').
mop_direct('TYPE-PROCLAMATION-MISMATCH', supers, ['PROCLAMATION-MISMATCH', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('TYPE-PROCLAMATION-MISMATCH-WARNING', supers, [cl_style_warning, cl_warning, 'TYPE-PROCLAMATION-MISMATCH', 'PROCLAMATION-MISMATCH', cl_condition, 'SLOT-OBJECT', t]).
mop_direct('TYPE-STYLE-WARNING', supers, ['REFERENCE-CONDITION', 'SIMPLE-STYLE-WARNING', cl_simple_condition, cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('TYPE-WARNING', supers, ['REFERENCE-CONDITION', cl_simple_warning, cl_simple_condition, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNAME', supers, [cl_structure_object, t]).
mop_direct('UNAME', szlot, 'MACHINE').
mop_direct('UNAME', szlot, 'NODENAME').
mop_direct('UNAME', szlot, 'RELEASE').
mop_direct('UNAME', szlot, 'SYSNAME').
mop_direct('UNAME', szlot, 'VERSION').
mop_direct(cl_unbound_slot, submop, 'SIMPLE-UNBOUND-SLOT').
mop_direct(cl_unbound_slot, supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_unbound_slot, supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct(cl_unbound_slot, szlot, '$INSTANCE').
mop_direct(cl_unbound_slot, szlot, 'INSTANCE').
mop_direct(cl_unbound_variable, submop, 'SIMPLE-UNBOUND-VARIABLE').
mop_direct(cl_unbound_variable, supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_unbound_variable, supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('UNDEFINED-ALIEN-ERROR', submop, 'UNDEFINED-ALIEN-FUNCTION-ERROR').
mop_direct('UNDEFINED-ALIEN-ERROR', submop, 'UNDEFINED-ALIEN-VARIABLE-ERROR').
mop_direct('UNDEFINED-ALIEN-ERROR', supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNDEFINED-ALIEN-FUNCTION-ERROR', supers, ['UNDEFINED-ALIEN-ERROR', cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNDEFINED-ALIEN-STYLE-WARNING', supers, [cl_style_warning, cl_warning, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNDEFINED-ALIEN-STYLE-WARNING', szlot, 'SYMBOL').
mop_direct('UNDEFINED-ALIEN-VARIABLE-ERROR', supers, ['UNDEFINED-ALIEN-ERROR', cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNDEFINED-CLASSOID', supers, ['CLASSOID', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct(cl_undefined_function, submop, 'SIMPLE-UNDEFINED-FUNCTION').
mop_direct(cl_undefined_function, submop, 'SPECIAL-FORM-FUNCTION').
mop_direct(cl_undefined_function, supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_undefined_function, supers, [cl_cell_error, 'ERROR', cl_serious_condition, cl_condition, cl_standard_object, t]).
mop_direct('UNDEFINED-PACKAGE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('UNDEFINED-PACKAGE', szlot, 'ERROR').
mop_direct('UNDEFINED-WARNING', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('UNDEFINED-WARNING', szlot, 'COUNT').
mop_direct('UNDEFINED-WARNING', szlot, 'KIND').
mop_direct('UNDEFINED-WARNING', szlot, 'NAME').
mop_direct('UNDEFINED-WARNING', szlot, 'WARNINGS').
mop_direct('UNHANDLED-DEBUG-CONDITION', supers, ['DEBUG-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNHANDLED-DEBUG-CONDITION', szlot, 'CONDITION').
mop_direct('UNION-TYPE', supers, ['COMPOUND-TYPE', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('UNION-TYPE', szlot, 'CLASS-INFO').
mop_direct('UNIX-HOST', supers, ['HOST', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('UNIX-HOST', szlot, 'CUSTOMARY-CASE').
mop_direct('UNIX-HOST', szlot, 'PARSE').
mop_direct('UNIX-HOST', szlot, 'PARSE-NATIVE').
mop_direct('UNIX-HOST', szlot, 'SIMPLIFY-NAMESTRING').
mop_direct('UNIX-HOST', szlot, 'UNPARSE').
mop_direct('UNIX-HOST', szlot, 'UNPARSE-DIRECTORY').
mop_direct('UNIX-HOST', szlot, 'UNPARSE-DIRECTORY-SEPARATOR').
mop_direct('UNIX-HOST', szlot, 'UNPARSE-ENOUGH').
mop_direct('UNIX-HOST', szlot, 'UNPARSE-FILE').
mop_direct('UNIX-HOST', szlot, 'UNPARSE-HOST').
mop_direct('UNIX-HOST', szlot, 'UNPARSE-NATIVE').
mop_direct('UNKNOWN-CODE-LOCATION', supers, ['DEBUG-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNKNOWN-CODE-LOCATION', szlot, 'CODE-LOCATION').
mop_direct('UNKNOWN-DEBUG-VAR', supers, ['DEBUG-ERROR', 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNKNOWN-DEBUG-VAR', szlot, 'DEBUG-FUN').
mop_direct('UNKNOWN-DEBUG-VAR', szlot, 'DEBUG-VAR').
mop_direct('UNKNOWN-TYPE', supers, ['HAIRY-TYPE', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('UNPRINTABLE-OBJECT', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('UNPRINTABLE-OBJECT', szlot, 'STRING').
mop_direct('UNSET-FUNCALLABLE-INSTANCE-FUNCTION', supers, ['REFERENCE-CONDITION', cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('UNSUPPORTED-OPERATOR', supers, [cl_simple_error, cl_simple_condition, 'ERROR', cl_serious_condition, cl_condition, 'SLOT-OBJECT', t]).
mop_direct('USAGE', supers, [cl_structure_object, t]).
mop_direct('USAGE', szlot, 'BLOCKS-INPUT').
mop_direct('USAGE', szlot, 'BLOCKS-OUTPUT').
mop_direct('USAGE', szlot, 'CONTEXT-SWITCHES-INVOLUNTARY').
mop_direct('USAGE', szlot, 'CONTEXT-SWITCHES-VOLUNTARY').
mop_direct('USAGE', szlot, 'DATA-MEMORY').
mop_direct('USAGE', szlot, 'MAJOR-PAGE-FAULTS').
mop_direct('USAGE', szlot, 'MAX-RSS').
mop_direct('USAGE', szlot, 'MESSAGES-RECEIVED').
mop_direct('USAGE', szlot, 'MESSAGES-SENT').
mop_direct('USAGE', szlot, 'MINOR-PAGE-FAULTS').
mop_direct('USAGE', szlot, 'NUM-SWAPS').
mop_direct('USAGE', szlot, 'SHARED-MEMORY').
mop_direct('USAGE', szlot, 'SIGNALS').
mop_direct('USAGE', szlot, 'STACK-MEMORY').
mop_direct('USAGE', szlot, 'SYSTEM-TIME').
mop_direct('USAGE', szlot, 'USER-TIME').
mop_direct('USER-INFO', supers, [cl_structure_object, t]).
mop_direct('USER-INFO', szlot, 'FULL-NAME').
mop_direct('USER-INFO', szlot, 'GID').
mop_direct('USER-INFO', szlot, 'HOME-DIR').
mop_direct('USER-INFO', szlot, 'LOGIN-ID').
mop_direct('USER-INFO', szlot, 'PASSWD').
mop_direct('USER-INFO', szlot, 'SHELL').
mop_direct('USER-INFO', szlot, 'UID').
mop_direct('UTMPX', supers, [cl_structure_object, t]).
mop_direct('UTMPX', szlot, 'HOST').
mop_direct('UTMPX', szlot, 'ID').
mop_direct('UTMPX', szlot, 'LINE').
mop_direct('UTMPX', szlot, 'PID').
mop_direct('UTMPX', szlot, 'TV').
mop_direct('UTMPX', szlot, 'TYPE').
mop_direct('UTMPX', szlot, 'USER').
mop_direct('VALSRC', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VALSRC', szlot, 'SOURCE').
mop_direct('VALSRC', szlot, 'VALUE').
mop_direct('VALUED-NODE', submop, 'BASIC-COMBINATION').
mop_direct('VALUED-NODE', submop, 'CAST').
mop_direct('VALUED-NODE', submop, 'CSET').
mop_direct('VALUED-NODE', submop, 'EXIT').
mop_direct('VALUED-NODE', submop, 'REF').
mop_direct('VALUED-NODE', supers, ['NODE', 'SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VALUED-NODE', szlot, 'DERIVED-TYPE').
mop_direct('VALUED-NODE', szlot, 'LVAR').
mop_direct('VALUES-TYPE', supers, ['ARGS-TYPE', 'CTYPE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VALUES-TYPE', szlot, 'CLASS-INFO').
mop_direct('VAR', supers, [cl_structure_object, t]).
mop_direct('VAR', szlot, 'ASSIGNEDP').
mop_direct('VAR', szlot, 'CLOSUREP').
mop_direct('VAR', szlot, 'CONSTANT').
mop_direct('VAR', szlot, 'CONSTANTP').
mop_direct('VAR', szlot, 'FNODE').
mop_direct('VAR', szlot, 'FOR-VALUE-USEDP').
mop_direct('VAR', szlot, 'MODIFIED-LIST').
mop_direct('VAR', szlot, 'NAME').
mop_direct('VAR', szlot, 'REALLY-USEDP').
mop_direct('VAR', szlot, 'REPLACEABLE-LIST').
mop_direct('VAR', szlot, 'SPECIALP').
mop_direct('VAR', szlot, 'STACKZ').
mop_direct('VAR', szlot, 'USEDP').
mop_direct('VAR', szlot, 'VENVC').
mop_direct(cl_vector, submop, cl_bit_vector).
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-COMPLEX-DOUBLE-FLOAT').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-COMPLEX-SINGLE-FLOAT').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-DOUBLE-FLOAT').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-FIXNUM').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-SIGNED-BYTE-16').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-SIGNED-BYTE-32').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-SIGNED-BYTE-64').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-SIGNED-BYTE-8').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-SINGLE-FLOAT').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-15').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-16').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-2').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-31').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-32').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-4').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-63').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-64').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-7').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-BYTE-8').
mop_direct(cl_vector, submop, 'SIMPLE-ARRAY-UNSIGNED-FIXNUM').
mop_direct(cl_vector, submop, cl_simple_vector).
mop_direct(cl_vector, submop, cl_string).
mop_direct(cl_vector, supers, [cl_array, 'SEQUENCE', t]).
mop_direct('VECTOR-NIL', submop, 'SIMPLE-ARRAY-NIL').
mop_direct('VECTOR-NIL', supers, [cl_string, cl_vector, cl_array, 'SEQUENCE', t]).
mop_direct('VERTEX', supers, ['SSET-ELEMENT', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VERTEX', szlot, 'COLOR').
mop_direct('VERTEX', szlot, 'INCIDENCE').
mop_direct('VERTEX', szlot, 'INITIAL-DOMAIN').
mop_direct('VERTEX', szlot, 'INITIAL-DOMAIN-SIZE').
mop_direct('VERTEX', szlot, 'INVISIBLE').
mop_direct('VERTEX', szlot, 'PACK-TYPE').
mop_direct('VERTEX', szlot, 'SPILL-COST').
mop_direct('VERTEX', szlot, 'TN').
mop_direct('VOP', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VOP', szlot, 'ARGS').
mop_direct('VOP', szlot, 'BLOCK').
mop_direct('VOP', szlot, 'CODEGEN-INFO').
mop_direct('VOP', szlot, 'INFO').
mop_direct('VOP', szlot, 'NEXT').
mop_direct('VOP', szlot, 'NODE').
mop_direct('VOP', szlot, 'PREV').
mop_direct('VOP', szlot, 'REFS').
mop_direct('VOP', szlot, 'RESULTS').
mop_direct('VOP', szlot, 'SAVE-SET').
mop_direct('VOP', szlot, 'TEMPS').
mop_direct('VOP-INFO', supers, ['TEMPLATE', 'STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VOP-INFO', szlot, 'AFFECTED').
mop_direct('VOP-INFO', szlot, 'ARG-COSTS').
mop_direct('VOP-INFO', szlot, 'ARG-LOAD-SCS').
mop_direct('VOP-INFO', szlot, 'EFFECTS').
mop_direct('VOP-INFO', szlot, 'GENERATOR-FUNCTION').
mop_direct('VOP-INFO', szlot, 'MORE-ARG-COSTS').
mop_direct('VOP-INFO', szlot, 'MORE-RESULT-COSTS').
mop_direct('VOP-INFO', szlot, 'MOVE-ARGS').
mop_direct('VOP-INFO', szlot, 'NUM-ARGS').
mop_direct('VOP-INFO', szlot, 'NUM-RESULTS').
mop_direct('VOP-INFO', szlot, 'REF-ORDERING').
mop_direct('VOP-INFO', szlot, 'RESULT-COSTS').
mop_direct('VOP-INFO', szlot, 'RESULT-LOAD-SCS').
mop_direct('VOP-INFO', szlot, 'SAVE-P').
mop_direct('VOP-INFO', szlot, 'TARGETS').
mop_direct('VOP-INFO', szlot, 'TEMPS').
mop_direct('VOP-INFO', szlot, 'VARIANT').
mop_direct('VOP-PARSE', supers, ['STRUCTURE_bang_OBJECT', cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('VOP-PARSE', szlot, 'AFFECTED').
mop_direct('VOP-PARSE', szlot, 'ARG-TYPES').
mop_direct('VOP-PARSE', szlot, 'ARGS').
mop_direct('VOP-PARSE', szlot, 'BODY').
mop_direct('VOP-PARSE', szlot, 'CONDITIONAL-P').
mop_direct('VOP-PARSE', szlot, 'COST').
mop_direct('VOP-PARSE', szlot, 'EFFECTS').
mop_direct('VOP-PARSE', szlot, 'GUARD').
mop_direct('VOP-PARSE', szlot, 'IGNORES').
mop_direct('VOP-PARSE', szlot, 'INFO-ARGS').
mop_direct('VOP-PARSE', szlot, 'INHERITS').
mop_direct('VOP-PARSE', szlot, 'LTN-POLICY').
mop_direct('VOP-PARSE', szlot, 'MORE-ARGS').
mop_direct('VOP-PARSE', szlot, 'MORE-RESULTS').
mop_direct('VOP-PARSE', szlot, 'MOVE-ARGS').
mop_direct('VOP-PARSE', szlot, 'NAME').
mop_direct('VOP-PARSE', szlot, 'NODE-VAR').
mop_direct('VOP-PARSE', szlot, 'NOTE').
mop_direct('VOP-PARSE', szlot, 'OPERANDS').
mop_direct('VOP-PARSE', szlot, 'RESULT-TYPES').
mop_direct('VOP-PARSE', szlot, 'RESULTS').
mop_direct('VOP-PARSE', szlot, 'SAVE-P').
mop_direct('VOP-PARSE', szlot, 'TEMPS').
mop_direct('VOP-PARSE', szlot, 'TRANSLATE').
mop_direct('VOP-PARSE', szlot, 'VARIANT').
mop_direct('VOP-PARSE', szlot, 'VARIANT-VARS').
mop_direct('VOP-PARSE', szlot, 'VOP-VAR').
mop_direct('WAITQUEUE', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('WAITQUEUE', szlot, 'NAME').
mop_direct('WAITQUEUE', szlot, 'TOKEN').
mop_direct(cl_warning, submop, 'CLOS-WARNING').
mop_direct(cl_warning, submop, 'CONSTANT-MODIFIED').
mop_direct(cl_warning, submop, 'DUPLICATE-DEFINITION').
mop_direct(cl_warning, submop, 'FINAL-DEPRECATION-WARNING').
mop_direct(cl_warning, submop, 'LATE-DEPRECATION-WARNING').
mop_direct(cl_warning, submop, cl_simple_warning).
mop_direct(cl_warning, submop, cl_style_warning).
mop_direct(cl_warning, supers, [cl_condition, 'SLOT-OBJECT', t]).
mop_direct(cl_warning, supers, [cl_condition, cl_standard_object, t]).
mop_direct('WEAK-POINTER', supers, [t]).
mop_direct('XSET', supers, [cl_structure_object, 'SLOT-OBJECT', t]).
mop_direct('XSET', szlot, 'DATA').
mop_direct('XSET', szlot, 'LIST-SIZE').
mop_direct(t, precedance, cl_nil).
mop_direct(t, supers, cl_nil).
mop_direct(t, submop, cl_array).
mop_direct(t, submop, cl_character).
mop_direct(t, submop, 'CODE-COMPONENT').
mop_direct(t, submop, 'FDEFN').
mop_direct(t, submop, cl_function).
mop_direct(t, submop, cl_hash_table).
mop_direct(t, submop, 'LRA').
mop_direct(t, submop, 'NUMBER').
mop_direct(t, submop, cl_package).
mop_direct(t, submop, cl_pathname).
mop_direct(t, submop, 'RANDOM-CLASS').
mop_direct(t, submop, cl_random_state).
mop_direct(t, submop, cl_readtable).
mop_direct(t, submop, 'SEQUENCE').
mop_direct(t, submop, 'SIMD-PACK').
mop_direct(t, submop, 'SLOT-OBJECT').
mop_direct(t, submop, cl_standard_object).
mop_direct(t, submop, cl_stream).
mop_direct(t, submop, cl_structure_object).
mop_direct(t, submop, cl_symbol).
mop_direct(t, submop, 'SYSTEM-AREA-POINTER').
mop_direct(t, submop, 'WEAK-POINTER').
