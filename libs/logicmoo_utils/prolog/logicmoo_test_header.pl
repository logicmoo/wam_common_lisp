:- if((prolog_load_context(source,File),prolog_load_context(file,File));current_prolog_flag(xref,true)).

:- else.

%test_completed% This file is mostly all inside if/endifs so it doesnt interfere with `module/2`
:- if((set_stream(current_output,tty(true)))).  :- endif.

%:- dumpST.

:- if((
 %set_prolog_flag(debug, true),
 %set_prolog_flag(gc, false),
 %set_prolog_flag(runtime_speed,0), % 0 = dont care
 set_prolog_flag(runtime_speed, 0), % 1 = default
 set_prolog_flag(runtime_debug, 3), % 2 = important but dont sacrifice other features for it
 set_prolog_flag(runtime_safety, 3),  % 3 = very important
 set_prolog_flag(unsafe_speedups, false),
 create_prolog_flag(logicmoo_message_hook,junit,[type(term),keep(false)]),
 %mpred_trace_exec,
 true)).
:- endif.


:- if( \+ current_module(logicmoo_clif)).

:- if( \+ getenv('keep_going','-k')).
% Load Editline/Readline
:- if( \+ current_module(prolog_history)).
:- if((set_stream(current_input,tty(true)))).  :- endif.
:- if(( %ignore(exists_source(library(editline))->use_module(library(editline))
       %;(exists_source(library(readline)),use_module(library(readline)))),
   '$toplevel':(  setup_colors,
                  setup_history,
                  setup_readline))). :- endif.
:- endif.
:- endif.

% Load SWI Utils
:- if(( \+ exists_source(library(logicmoo_utils)),
   prolog_load_context(directory,X),absolute_file_name('../../',O,[relative_to(X),file_type(directory)]), attach_packs(O))).
:- endif.
:- if(use_module(library(logicmoo_utils))). :-endif.

% Load PFC
:- if(set_prolog_flag(pfc_version,v(2,0,0))). :- endif.
:- if(ignore((exists_source(library(pfc_lib)),ensure_loaded(library(pfc_lib))))). :-endif.

% Load CLIF
:- if(ignore((exists_source(library(logicmoo_clif)),ensure_loaded(library(logicmoo_clif))))). :-endif.

:- endif. % \+ current_module(logicmoo_clif)

:- if(assert_if_new((clifops:clif_op_decls((
 op(1199,fx,('==>')), op(1190,xfx,('::::')), op(1180,xfx,('==>')), op(1170,xfx,('<==>')), op(1160,xfx,('<-')),
 op(1150,xfx,('=>')), op(1140,xfx,('<=')), op(1130,xfx,'<=>'),
 op(1120,xfx,'<->'),
 op(600,yfx,('&')), op(600,yfx,('v')),op(350,xfx,('xor')), op(300,fx,('-')),
 op(300,fx,('~'))))))).   :- endif.

:- if((prolog_load_context(source,S),format(user_error,'~N~q,~n',[running(S)]))). :- endif.
:- if(( \+ current_prolog_flag(test_module,_),set_prolog_flag(test_module,baseKB),assert(baseKB:this_is_baseKB))). :- endif.
:- if(( \+ current_prolog_flag(test_typein_module,_), set_prolog_flag(test_typein_module,baseKB))). :- endif.

:- if(current_prolog_flag(loaded_test_header,_)).
:- wdmsg(reload_of_test_header).
:- mpred_reset.
:- else.
:- if(( \+ current_prolog_flag(loaded_test_header,_),set_prolog_flag(loaded_test_header,loaded))).  :- endif.

:- if(prolog_load_context(module,user)).
:- if(( \+ current_prolog_flag(test_module,user), \+ current_prolog_flag(test_module,baseKB))).
% writes a temp header file and include/1s it
:- if(( tmp_file(swi, Dir), make_directory(Dir),working_directory(OLD,Dir),asserta(t_l:old_pwd(OLD,Dir)),current_prolog_flag(test_module,Module),open('module_header.pl',write,OS),
  format(OS,'\n:- module(~q,[test_header_include/0]).\n test_header_include. ',[Module]),close(OS))). :- endif.
:- include('module_header.pl').
:- retract(t_l:old_pwd(OLD,Delete)),working_directory(_,OLD),delete_directory_and_contents(Delete).
:- endif.
:- endif. % prolog_load_context(module,user)
:- endif. % current_prolog_flag(loaded_test_header,_)

%:- if((current_prolog_flag(test_module,Module), '$set_source_module'(Module))). :- endif.

:- if((prolog_load_context(source,File),!,
   ignore((((sub_atom(File,_,_,_,'.pfc')
   -> (sanity(is_pfc_file),set_prolog_flag(is_pfc_file_dialect,true))
   ; nop((sanity( \+ is_pfc_file),set_prolog_flag(is_pfc_file_dialect,false))))))))).
:- if((current_prolog_flag(test_module,Module), clifops:clif_op_decls(OPS), call(Module:OPS))). :- endif.
:- endif.


% :- if(('$current_source_module'(W), '$set_typein_module'(W))). :- endif.
%:- if((current_prolog_flag(test_typein_module,Module), '$set_typein_module'(Module), module(Module))). :- endif.

:- if(current_prolog_flag(is_pfc_file_dialect,true)).
:- if((current_prolog_flag(test_typein_module,Module), clifops:clif_op_decls(OPS), call(Module:OPS))). :- endif.
:- expects_dialect(pfc).
:- else.
:- if((dmsg(this_test_might_need(:- expects_dialect(pfc))))).  :- endif.
:- endif.

:- if((dmsg(this_test_might_need(:- use_module(library(logicmoo_plarkc)))))).  :- endif.

:- if((ensure_loaded(library(logicmoo_test)))).
:- if(at_halt(system:test_completed)). :- endif.
:- endif.


%:- if(false).
:- if((prolog_load_context(source,Src),set_prolog_flag(test_src,Src))). :- endif.
:- if((prolog_load_context(source,Src),add_test_info(testsuite,file,Src))). :- endif.
%:- endif.

:- if((prolog_load_context(source,File), sub_atom(File,_,_,_,'.plt'),!, user:use_module(library(plunit)))).
:- if(at_halt(logicmoo_test:run_junit_tests_at_halt)). :- endif.
:- else.
:- if((prolog_load_context(source,F),echo_source_file_no_catchup(F))).  :- endif.
:- endif.

:- endif.

