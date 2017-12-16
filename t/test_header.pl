
:- use_module(library(wamcl)).

:- if(current_prolog_flag(test_header,_)).

:- wdmsg(reload_of_test_header).

:- mpred_reset.

:- else.

% runtype: default = pfc
:- if(current_prolog_flag(runtime_testing_module,_)->true;
  set_prolog_flag(runtime_testing_module,test_header)).
:- endif.

:- if(( \+ current_prolog_flag(test_header,_),set_prolog_flag(test_header,loaded))).


:- if((prolog_load_context(module,user), \+ current_module(pfc_lib))).

:- thread_local(t_l:each_file_term/1).

:- if( exists_source(library(logicmoo_clif)) -> true ;
 ((dynamic(user:file_search_path/2),multifile(user:file_search_path/2),
  absolute_file_name('../../prolog',Dir),asserta(user:file_search_path(library,Dir))))).
:- endif.

% runtype: default = pfc
:- if(current_prolog_flag(runtime_testing_module,_)->true;
  set_prolog_flag(runtime_testing_module,test_header)).
:- endif.

:- if(( \+ current_prolog_flag(test_header,_),set_prolog_flag(test_header,loaded))).




:- if((prolog_load_context(module,user), \+ current_module(pfc_lib))).
:- module(header_sane,[test_header_include/0]).
:- endif.

test_header_include.

:- endif.
%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).
:- set_prolog_flag(logicmoo_message_hook,break).


:- endif.



%:- set_prolog_flag(debug, true).
%:- set_prolog_flag(gc, false).

:- '$current_source_module'(W), '$set_typein_module'(W).

:- set_prolog_flag(nonet,true).
:- set_prolog_flag(run_network,false).
:- set_prolog_flag(load_network,false).



%:- set_prolog_flag(runtime_speed,0). % 0 = dont care
:- set_prolog_flag(runtime_speed, 0). % 1 = default
:- set_prolog_flag(runtime_debug, 3). % 2 = important but dont sacrifice other features for it
:- set_prolog_flag(runtime_safety, 3).  % 3 = very important
:- set_prolog_flag(unsafe_speedups, false).

% :- dynamic(ttExpressionType/1).

:- set_prolog_flag(runtime_debug, 0). 




dir_from(Rel,Y):-
    ((getenv('LOGICMOO_WS',Dir);
     prolog_load_context(directory,Dir);
     '~/logicmoo_workspace'=Dir;
     '/home/dmiles/logicmoo_workspace/'=Dir)),
    absolute_file_name(Rel,Y,[relative_to(Dir),file_type(directory),file_errors(fail)]),
    exists_directory(Y),!.
add_pack_path(Rel):-
   dir_from(Rel,Y),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- add_pack_path(packs_sys).
:- add_pack_path(packs_usr).
:- add_pack_path(packs_web).
:- add_pack_path(packs_xtra).
:- initialization(attach_packs,now).
:- if(exists_source(library(editline))).
:- use_module(library(editline)).
:- else.
:- if(exists_source(library(readline))).
:- use_module(library(readline)).
:- endif.
:- endif.

:-  '$toplevel':setup_history.
:- set_prolog_flag(os_argv,[swipl, '-f', '/dev/null','--nonet','--unsafe','--']).


:- ensure_loaded(library(script_files)).
:- if(( \+ current_module(pfc_lib) )).
:- use_module(library(pfc)).
:- prolog_load_context(source,File),(atom_contains(File,'.pfc')-> sanity(is_pfc_file) ; must_not_be_pfc_file).
:- endif.

:- ensure_loaded(library(pfc_test)).


:- kb_global(baseKB:ttExpressionType/1).

:- sanity((defaultAssertMt(Mt1),fileAssertMt(Mt2),source_module(Mt3))),sanity((Mt1==Mt2,Mt2==Mt3)).

% logicmoo_clif should maybe load from logicmoo_user
%:- use_module(library(logicmoo_user)).
:- use_module(library(script_files)).
:- set_prolog_flag(runtime_debug, 3). 


:- fav_debug.
:- set_prolog_flag(gc, true).

:- endif.


:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- fixup_exports.

end_of_file.


:-
 op(1199,fx,('==>')), 
 op(1190,xfx,('::::')),
 op(1180,xfx,('==>')),
 op(1170,xfx,'<==>'),  
 op(1160,xfx,('<-')),
 op(1150,xfx,'=>'),
 op(1140,xfx,'<='),
 op(1130,xfx,'<=>'), 
 op(600,yfx,'&'), 
 op(600,yfx,'v'),
 op(350,xfx,'xor'),
 op(300,fx,'~'),
 op(300,fx,'-').

:- sanity((defaultAssertMt(Mt1),fileAssertMt(Mt2),source_module(Mt3))),sanity((Mt1==Mt2,Mt1==Mt3)).



%:- ensure_loaded(library('logicmoo_clif')).
%:- ensure_loaded(library('logicmoo/common_logic/common_logic_loader.pl')).
:- assert(t_l:each_file_term(must_kif_process_after_rename)).

:- prolog_load_context(source,File),((atom_contains(File,'.pfc');atom_contains(File,'.clif'))-> sanity(is_pfc_file) ; must_not_be_pfc_file).


:- if(is_pfc_file).

%:- mpred_trace_exec.

:- else. % is_pfc_file


%:- mpred_trace_exec.

:- endif. % is_pfc_file

:- endif. % current_prolog_flag(test_header,_).

%:- cls.

:- '$current_source_module'(M),install_retry_undefined(M,kbi_define).

% :- set_prolog_IO(user_input,user_output,user_error).


:- if((prolog_load_context(source,File),(atom_contains(File,'.clif')))).

:- use_module(library(logicmoo_clif)).

:-assert(t_l:each_file_term(must_kif_process_after_rename)).

% install_constant_renamer_until_eof:-  
  %call_on_eof(show_missing_renames), 
%  set_prolog_flag_until_eof(do_renames,term_expansion).

:- set_prolog_flag(runtime_debug, 0). 
:- use_module(library(logicmoo_clif)).
:- set_prolog_flag(runtime_debug, 3). 

:- set_prolog_flag(do_renames,term_expansion).
:- ((prolog_load_context(source,File), atom_contains(File,'.clif')) ->
   (current_stream(File, read, Stream),with_lisp_translation(Stream,must_kif_process_after_rename)); true).

%:- call(call,((asserta(((system:goal_expansion(Here,Loc,_,_):- dmsg(s_goal_expansion(Here,Loc)),trace,fail))),
%   asserta(((system:term_expansion(Here,Loc,_,_):- dmsg(s_term_expansion(Here,Loc)),trace,fail)))))).

:- else.       % end clif file


:- endif.


