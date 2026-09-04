% :-module(xlisting_web,[ensure_sigma/0,search4term/0]).
%:- if(( ( \+ ((current_prolog_flag(logicmoo_include,Call),Call))) )). 
:- module(xlisting_web_server,
          [ register_logicmoo_browser/0,
            swish_reply_config_root/1
          ]).

:- set_module(class(library)).
/** <module> xlisting_web_server
% Provides /logicmoo runtime preds browsing
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/

:- public(swish_reply_config_root/1).
:- export(swish_reply_config_root/1).
swish_reply_config_root(Request):-
  (current_predicate(swish_config:swish_reply_config/1)
   -> call(call,swish_config:swish_reply_config(Request)); 
      swish_reply_config_root).

swish_reply_config_root:-
  current_predicate(swish_config:json_config/2),!,
  call(call,swish_config:
  (json_config(JSON, []),
	 reply_json(JSON))).
swish_reply_config_root:- 
  user:file_search_path(xlisting_web,Here),
  atom_concat(Here,'/swish_config.json',ConfigFile),
  exists_file(ConfigFile),!,
  format('Content-type: application/json; charset=UTF-8~n~n'),
  read_file_to_string(ConfigFile,Config,[]),
  write(current_output, Config).
swish_reply_config_root:- 
  http_json:reply_json(_{}).





:- http_handler('/swish_config.json', swish_reply_config_root,[]).

/*

:- dynamic user:library_directory/1.
:- multifile user:library_directory/1.
hide_xpce_library_directory:- fail,
  user:library_directory(X),
  atom(X),
  atom_concat(_,'xpce/prolog/lib/',X),!,
  retract((user:library_directory(X))),
  assert((user:library_directory(X):- \+ current_prolog_flag(hide_xpce_library_directory,true))).
hide_xpce_library_directory.

%:- hide_xpce_library_directory.
*/
%:- set_prolog_flag(hide_xpce_library_directory,true).


/*
:- system:use_module(library(http/thread_httpd)).
:- system:use_module(thread_httpd:library(http/http_dispatch)).
:- system:use_module(library(http/http_dispatch)).

*/
%:- ensure_loaded(library(logicmoo_swilib)).
:- system:use_module(swi(library/http/html_head)).

:- system:use_module(library(http/http_path)).
:- system:use_module(library(http/http_log)).
:- system:use_module(library(http/http_client)).
:- system:use_module(library(http/http_server_files)).
:- system:use_module(library(http/http_parameters)).

:- system:use_module(library(uri)).
:- system:use_module(library(http/http_openid)).
:- system:use_module(library(http/http_host)).
:- use_module(library(http/html_write)).
:- system:use_module(library(http/http_error)).


:- system:use_module(library(predicate_streams)).
%:- system:use_module(library(logicmoo/with_no_x)).
:- system:use_module(library(logicmoo/each_call)).
%:- use_module(library(logicmoo/butterfly_console)).


:- if(exists_source(cliopatria('applications/help/load'))).
:- system:use_module(cliopatria('applications/help/load')).
% Load ClioPatria itself.  Better keep this line.
:- system:use_module(cliopatria(cliopatria)).
:- else.
cp_menu:cp_menu(X,X).
:- endif.


:- thread_initialization(nb_setval(pldoc_options,[ prefer(manual) ])).


%% ensure_sigma( ?ARG1) is det.
%
% Ensure Webserver.
%
ensure_sigma(Port) :- format(atom(A),'httpd@~w_1',[Port]),thread_property(_,alias(A)),!.
ensure_sigma(Port) :- on_x_debug(catch((http_server(http_dispatch,[ port(Port), workers(16) ])),E,wdmsg(E))).


%% ensure_sigma is det.
%
% Ensure Webserver.
%
ensure_sigma:- ensure_sigma(3020).


:- if( \+ exists_source(library(logicmoo_utils_all))).
:- dynamic user:file_search_path/2.
:- multifile user:file_search_path/2.
:- prolog_load_context(directory,Dir),
   DirFor = mpred_online,
   (( \+ user:file_search_path(DirFor,Dir)) ->asserta(user:file_search_path(DirFor,Dir));true),
   absolute_file_name('../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- initialization(attach_packs,now).
% [Required] Load the Logicmoo Library Utils
:- endif.
 



handler_logicmoo_cyclone2a(X):- xlisting_web:handler_logicmoo_cyclone2(X).
handler_logicmoo_cyclone3a(X):- xlisting_web:handler_logicmoo_cyclone3(X).

%% user:file_search_path( ?ARG1, ?ARG2) is det.
%
% Hook To [user:file_search_path/2] For Module Mpred_www.
% File Search Path.
%
:- prolog_load_context(directory,Here),atom_concat(Here,'/pixmaps',NewDir),asserta_new((user:file_search_path(pixmapx,NewDir))).
:- prolog_load_context(directory,Here),asserta_new((user:file_search_path(xlisting_web,Here))).
%user:file_search_path(pixmapx, logicmoo('mpred_online/pixmapx')).

%user:file_search_path(pixmapx,NewDir):- user:file_search_path(xlisting_web,Here), atom_concat(Here,'/pixmaps',NewDir).

register_logicmoo_browser:- 
  %http_handler('/lm_xref/', handler_logicmoo_cyclone0, [prefix]), % chunked
  %http_handler('/lm_xref_nc/', handler_logicmoo_cyclone1, [prefix,chunked]),
  http_handler(('/swish/lm_xref'), handler_logicmoo_cyclone2a, [prefix,priority(50)]), % chunked
  call(call,http_handler(('/swish/lm_xref/swish_config.json'), swish_reply_config_root,[priority(200)])),
  http_handler(('/swish/lm_xref/slowcode'), handler_logicmoo_slowcode, [prefix,chunked,priority(100)]), % chunked
  http_handler(('/swish/lm_xref/pixmapx'), http_server_files:serve_files_in_directory(pixmapx), [prefix,priority(100)]),
  http_handler(('/swish/lm_xref_nc'), handler_logicmoo_cyclone3a, [prefix,chunked]),

  nop(doc_collect(true)).

:- fixup_module_exports_into(baseKB).
:- fixup_module_exports_into(system).

:- register_logicmoo_browser.



% :- thread_property(_,alias('http@3020'))->true; http_server(http_dispatch, [port(3020)]).



end_of_file.

