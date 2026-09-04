/* Part of LogicMOO Base Logicmoo Utils
% ===================================================================
    File:          'logicmoo_startup.pl'
    Previous File: 'logicmoo_util_startuo.pl'
    Purpose:       To load the logicmoo libraries inits as needed
    Contact:       $Author: logicmoo@gmail.com ;
    Version:       'logicmoo_util_startuo.pl' 1.0.0
    Revision:      $Revision: 1.2 $
    Revised At:    $Date: 2017/06/02 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/logicmoo_utils/edit/master/prolog/logicmoo_startup.pl
    Copyleft:      1999-2021, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

% We save the name of the module loading this module
:- module(logicmoo_startup,[]).
:- export(define_into_module/1).
:- meta_predicate(define_into_module(:)).
define_into_module(S:List):- define_into_module(S:system,List).

/** <module> Utility LOGICMOO_STARTUP
This module manages logicmoo startup (adding history and tests, etc).
@author Douglas R. Miles
@license LGPL
*/

:- dynamic   user:file_search_path/2.
:- multifile user:file_search_path/2.


%:- expects_dialect(swi).
%:- discontiguous '$exported_op'/3.
%:- autoload(library(lists),[member/2,append/3]).
:- autoload(library(debug),[debug/3]).

:- module_transparent(now_and_later/1).
:- module_transparent(now_and_later/2).
:- module_transparent(now_and_later/4).

:- export(now_and_later/1).
:- export(now_and_later/2).
now_and_later(MGoal):- strip_module(MGoal,M,Goal), now_and_later(c,M:Goal).
now_and_later(MC,MGoal):- strip_module(MGoal,M,Goal), '$current_typein_module'(TIM), '$current_source_module'(SM),
  now_and_later(MC,TIM,SM,M:Goal).

now_and_later(n,TIM,SM,MGoal):- strip_module(MGoal,M,Goal), !, sys:call_now(c,TIM,SM,M:Goal).
now_and_later(MC,TIM,SM,MGoal):- strip_module(MGoal,M,Goal),
  sys:call_now(c,TIM,SM,M:Goal),
  initialization(sys:call_now(MC,TIM,SM,M:Goal),restore).

:- meta_predicate(in_lm_ws(:)).
:- export(in_lm_ws/1).
in_lm_ws(MGoal):- getenv('LOGICMOO_WS',WS),!,
   strip_module(MGoal,M,Goal),
   working_directory(X,X),
   setup_call_cleanup(cd(WS),(cd(prologmud_server),call(M:Goal)),cd(X)).
in_lm_ws(MGoal):- strip_module(MGoal,M,Goal), call(M:Goal).

sys:s_t_s(TIM,SM):- '$set_typein_module'(TIM),'$set_source_module'(SM).

:- module_transparent(sys:with_typein_and_source/3).
sys:with_typein_and_source(TIM,SM,MGoal):-
  strip_module(MGoal,M,Goal),
  '$current_typein_module'(WasTIM), '$current_source_module'(WasSM),
  setup_call_cleanup(s_t_s(TIM,SM),M:Goal, s_t_s(WasTIM,WasSM)).


:- module_transparent(sys:call_now/4).
%sys:call_now(n,_TIM,_SM,_MGoal):-!.
%sys:call_now(m,_TIM,_SM,_MGoal):-!.
sys:call_now(_,TIM,SM,MGoal):-
  strip_module(MGoal,M,Goal),
   maybe_writeln(sys:call_now(TIM,SM,M:Goal)),
  sys:with_typein_and_source(TIM,SM,M:Goal).

:- module_transparent(call_with_typein_and_source/3).
call_with_typein_and_source(TIM,SM,MGoal):-
  strip_module(MGoal,M,Goal),
  sys:with_typein_and_source(TIM,SM,M:Goal).


dont_wl(X):- var(X),!,fail.
dont_wl(X):- strip_module(X,_,all_source_file_predicates_are_exported),!.
dont_wl(X):- compound(X),compound_name_arity(X,F,_),(dont_wl(F);(arg(_,X,E),dont_wl(E))).


:- export(maybe_writeln/1).
maybe_writeln(_):- !.
maybe_writeln(X):- dont_wl(X),!.
maybe_writeln(X):- writeln(X).

:- if( \+ current_predicate(add_absolute_search_folder/2)).

name_to_files(Spec, Files) :-
    name_to_files(Spec, Files, true).
name_to_files(Spec, Files, Exists) :-
    name_to_files_(Spec, Files, Exists),
    (   Files==[]
    ->  print_message(warning, format('No match: ~w', [Spec])),
        fail
    ;   true
    ).


:- meta_predicate(call_safely(:)).
call_safely(MGoal):- strip_module(MGoal,M,Goal),!,call_safely(M,Goal).

call_safely(M,[H|T]):- !, maplist(call_safely(M),[H|T]).
call_safely(M,(Goal,!,Goal2)):- !, call_safely(M,Goal),!,call_safely(M,Goal2).
call_safely(M,(Goal,!)):- !, call_safely(M,Goal),!.
call_safely(M,(Goal,Goal2)):-!, call_safely(M,Goal),call_safely(M,Goal2).
call_safely(_,M:Goal):- !, call_safely(M,Goal).
call_safely(M,Goal):- ignore(must_or_rtrace(in_lm_ws(M:Goal))),
  nop(check_memory(Goal)).

logicmoo_compiling_mud_server:- (current_prolog_flag(logicmoo_compiling,mud_server);compiling).

:- meta_predicate(only_runtime(:)).
only_runtime(MGoal):- strip_module(MGoal,M,Goal), (logicmoo_compiling_mud_server -> true; call_safely(M:Goal)).

%    working_directory(Dir,Dir);prolog_load_context(directory,Dir)

spec_to_files(Spec,Files):-
    findall(File,
            (   absolute_file_name(Spec,File,[ access(exist),file_type(directory),file_errors(fail),solutions(all)])
            ;   absolute_file_name(Spec,File,[ access(exist),file_errors(fail),solutions(all)])), Files).

local_file_name_to_atom_(Spec, File) :-
    atomic(Spec),
    !,
    atom_string(File, Spec).
local_file_name_to_atom_(Spec, File) :-
    segments_local_(Spec, L, []),
    atomic_list_concat(L, /, File).

segments_local_(Var, A, B) :-
    var(Var),
    !,
    instantiation_error(Var),
    B=A.
segments_local_(A/B, C, D) :-
    !,
    E=C,
    segments_local_(A, E, F),
    segments_local_(B, F, D).
segments_local_(A, B, C) :-
    must_be(atomic, A),
    D=B,
    D=[A|C].
%:- autoload(library(shell),[file_name_to_atom/2]).

name_to_files_(Spec, Files, _) :-
 % prolog_load_context(directory,Dir),
    compound(Spec),
    compound_name_arity(Spec, _Alias, 1), !,
    spec_to_files(Spec,Files).
name_to_files_(Spec, Files, Exists) :-
    %use_module(library(shell)),
    %shell:file_name_to_atom(Spec, S1),
    local_file_name_to_atom_(Spec, S1),
    expand_file_name(S1, Files0),
    (   Exists==true,
        Files0==[S1],
        \+ access_file(S1, exist)
    ->  print_message(warning,format('"~w" does not exist', [S1])),
        fail
    ;   Files=Files0
    ).


with_abs_paths(Pred1, Path):- is_list(Path),!, maplist(with_abs_paths(Pred1),Path).
with_abs_paths(Pred1, Path):-
 ( \+ atom(Path); \+ is_absolute_file_name(Path); \+ exists_file_or_dir(Path)), !,
 wdmsg(resolve(with_abs_paths(Pred1,Path))),
 (must((forall((
     (name_to_files(Path, MatchesL)*-> member(Matches,MatchesL) ; Path = Matches),
    spec_to_files(Matches,AbsPath)),
    with_abs_paths(Pred1,AbsPath))))).

with_abs_paths(Pred1, Path):-
  wdmsg(with_abs_paths(Pred1,Path)),
  call(Pred1,Path).

/*
with_abs_paths(Pred1, Path):-
atom(Path), is_absolute_file_name(Path), show_failure(exists_file_or_dir(Path))
 ->
  (wdmsg(with_abs_paths(Pred1,Path)),
 call(Pred1,Path))
  ;
  (must((forall((
     (name_to_files(Path, MatchesL)*-> member(Matches,MatchesL) ; Path = Matches),
    spec_to_files(Matches,AbsPath)),
    with_abs_paths(Pred1,AbsPath)))))).
*/

ain_file_search_path(Name,Path):-
 (user:file_search_path(Name,Path) -> true ; asserta(user:file_search_path(Name,Path))).

add_absolute_search_folder(Name,Path):- with_abs_paths(ain_file_search_path(Name), Path).

:- endif.

:- if(false).
:- endif.

:- module_transparent(enotrace/1).
enotrace(G):- call(G),!.


%:- system:use_module(library(logicmoo_utils_all)).
:- create_prolog_flag(dmsg_level,[never,error,warning,info,filter,always],[type(term),keep(true)]).


:- if( \+ current_predicate(each_call_cleanup/3)).
% :- system:use_module(library(each_call_cleanup)).
:- endif.

% ==============================================
% Add Pack Directories
% ==============================================
:- multifile(user:file_search_path/2).
:-   dynamic(user:file_search_path/2).

:- system:use_module(library(prolog_pack)).

dir_from(Rel,Y):-
    ((getenv('LOGICMOO_WS',Dir);
      working_directory(Dir,Dir);
      prolog_load_context(directory,Dir);
      getenv('CD',Dir);
      %'w:/opt/logicmoo_workspace/'=Dir;
      %'~/logicmoo_workspace'=Dir;
      %'/opt/logicmoo_workspace/'=Dir;
      fail)),
    absolute_file_name(Rel,Y,[relative_to(Dir),file_type(directory),file_errors(fail)]),
    exists_directory(Y),!.

:- export(add_pack_path/1).
add_pack_path(packs_xtra):-pack_property(logicmoo_nlu,_),!.
add_pack_path(packs_sys):-pack_property(logicmoo_base,_),!.
add_pack_path(Rel):- ( \+ atom(Rel) ; \+ is_absolute_file_name(Rel)),
   dir_from(Rel,Y), Y\==Rel, add_pack_path(Y), !.
add_pack_path(Y):- (\+ atom(Y) ; \+ exists_directory(Y)),!.
add_pack_path(Y):- attach_packs(Y),!.
add_pack_path(Y):-  \+ user:file_search_path(pack,Y) ->asserta(user:file_search_path(pack,Y));true.



:- if( \+ exists_source(library(logicmoo_common))).
:- add_pack_path('../..').
:- endif.
:- if( \+ exists_source(library(logicmoo_webui))).
:- add_pack_path('../../../packs_web').
:- endif.
:- if( \+ exists_source(library(aleph))).
:- add_pack_path('../../../packs_lib').
:- endif.

%:- if( \+ exists_source(library(logicmoo_hyhtn))).
%:- add_pack_path(packs_xtra).
%:- endif.

%:- ignore(add_pack_path(packs_usr)).
%:- add_pack_path(packs_web).
%:- add_pack_path(packs_xtra).
%:- add_pack_path(packs_lib).

%=======================================
% Utils
%=======================================

/*
:- if(gethostname(gitlab)).

:- set_prolog_flag(runtime_debug,3).
:- set_prolog_flag(runtime_safety,3).
:- set_prolog_flag(runtime_speed,0).

:- else.

:- set_prolog_flag(runtime_debug,1).
:- set_prolog_flag(runtime_safety,1).
:- set_prolog_flag(runtime_speed,1).

:- endif.

:- set_prolog_flag(lm_no_autoload,false).
:- set_prolog_flag(lm_pfc_lean,false).


*/


/*
:- set_prolog_flag(stack_limit, 32 000 000 000).
:- set_prolog_stack(global, limit(32*10**9)).
:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(trail, limit(32*10**9)).
*/


%:- setenv('DISPLAY', '').
% :- system:use_module(library(plunit)).


% ==============================================
% Enable History
% ==============================================
:- if( \+ getenv('keep_going','-k')).
:- if(\+ current_predicate(setup_hist0/0)).
:- if(current_prolog_flag(windows, false)).

:- if(exists_source(library(editline))).
:- system:use_module(library(editline)).
:- else.
:- if(exists_source(library(readline))).
 :- system:use_module(library(readline)).
:- else.
 :- if(exists_source(library(editline))).
  :- system:use_module(library(editline)).
 :- endif.
:- endif.
setup_hist0:-  '$toplevel':setup_history.
:- initialization(setup_hist0, now).
:- endif.
:- endif.
:- endif.
:- endif. % keep_going

:- if(false).

:- system:use_module(library(aggregate)).
%:- system:use_module(library(ansi_term)).
:- system:use_module(library(apply)).
:- system:use_module(library(apply_macros)).
:- system:use_module(library(archive)).
:- system:use_module(library(arithmetic)).
:- system:use_module(library(assoc)).
:- system:use_module(library(atom)).
:- system:use_module(library(backcomp)).
:- system:use_module(library(base32)).
:- system:use_module(library(base64)).
:- system:use_module(library(bdb)).
:- system:use_module(library(broadcast)).
:- system:use_module(library(c14n2)).
%:- system:use_module(library(cgi)).
:- system:use_module(library(charsio)).
:- system:use_module(library(check)).
:- system:use_module(library(check_installation)).
:- system:use_module(library(checklast)).
:- system:use_module(library(checkselect)).
/*
:- system:use_module(library(chr)).
:- system:use_module(library(clp/bounds)).
:- system:use_module(library(clp/clp_distinct)).
:- system:use_module(library(clp/clp_events)).
:- system:use_module(library(clp/clpb)).
:- system:use_module(library(clp/clpfd)).
:- system:use_module(library(clp/clpq)).
:- system:use_module(library(clp/clpr)).
:- system:use_module(library(clp/inclpr)).
:- system:use_module(library(clp/simplex)).
*/
:- system:use_module(library(codesio)).
:- user:use_module(library(coinduction)).
:- system:use_module(library(console_input)).
:- user:use_module(library(cql/cql)).
:- system:use_module(library(crypt)).
:- system:use_module(library(crypto)).
:- system:use_module(library(csv)).
:- system:use_module(library(ctypes)).
:- system:use_module(library(date)).
:- system:use_module(library(dcg/basics)).
:- system:use_module(library(dcg/high_order)).
:- system:use_module(library(debug)).
:- system:use_module(library(dialect)).
/*
:- system:use_module(library(dialect/bim)).
:- system:use_module(library(dialect/commons)).
:- system:use_module(library(dialect/eclipse/test_util_iso)).
:- system:use_module(library(dialect/hprolog)).
:- system:use_module(library(dialect/hprolog/format)).
:- system:use_module(library(dialect/iso/iso_predicates)).
:- system:use_module(library(dialect/sicstus)).
:- system:use_module(library(dialect/sicstus4)).
:- system:use_module(library(dialect/swi/syspred_options)).
:- system:use_module(library(dialect/xsb)).
:- system:use_module(library(dialect/yap)).
*/
%:- set_prolog_flag(autoload,false).
%:- set_prolog_flag(autoload,user_or_explicit).
%:- set_prolog_flag(autoload,true).
%:- ensure_loaded(library(debuggery/wemsg)).
:- system:use_module(library(sandbox)).
:- system:use_module(library(hashtable)).
:- system:use_module(library(dicts)).
:- system:use_module(library(dif)).
:- system:use_module(library(doc_files)).
:- system:use_module(library(doc_http)).
:- system:use_module(library(doc_latex)).
:- system:use_module(library(double_metaphone)).
:- system:use_module(library(edinburgh)).
:- system:use_module(library(edit)).
:- system:use_module(library(editline)).
:- system:use_module(library(error)).
:- system:use_module(library(explain)).
:- system:use_module(library(fastrw)).
:- system:use_module(library(files)).
:- system:use_module(library(filesex)).
:- system:use_module(library(gensym)).
:- system:use_module(library(git)).
:- system:use_module(library(hash_stream)).
:- system:use_module(library(hashtable)).
:- system:use_module(library(heaps)).
:- system:use_module(library(help)).
:- system:use_module(library(hotfix)).
:- system:use_module(library(http/ax)).
:- system:use_module(library(http/dcg_basics)).
:- system:use_module(library(http/html_head)).
:- system:use_module(library(http/html_quasiquotations)).
:- user:use_module(library(http/html_write)).
:- system:use_module(library(http/http_authenticate)).
:- system:use_module(library(http/http_client)).
:- system:use_module(library(http/http_cookie)).
:- system:use_module(library(http/http_cors)).
:- system:use_module(library(http/http_digest)).
:- system:use_module(library(http/http_dirindex)).
:- system:use_module(library(http/http_dispatch)).
:- system:use_module(library(http/http_dyn_workers)).
:- system:use_module(library(http/http_error)).
:- system:use_module(library(http/http_exception)).
:- system:use_module(library(http/http_files)).
:- system:use_module(library(http/http_header)).
:- system:use_module(library(http/http_hook)).
:- system:use_module(library(http/http_host)).
:- system:use_module(library(http/http_json)).
:- system:use_module(library(http/http_load)).
:- system:use_module(library(http/http_log)).
:- system:use_module(library(http/http_multipart_plugin)).
:- system:use_module(library(http/http_open)).
:- system:use_module(library(http/http_openid)).
:- system:use_module(library(http/http_parameters)).
:- system:use_module(library(http/http_path)).
:- system:use_module(library(http/http_proxy)).
:- system:use_module(library(http/http_pwp)).
:- system:use_module(library(http/http_redis_plugin)).
:- user:use_module(library(http/http_server)).
:- system:use_module(library(http/http_server_files)).
:- system:use_module(library(http/http_session)).
:- system:use_module(library(http/http_sgml_plugin)).
:- system:use_module(library(http/http_ssl_plugin)).
:- system:use_module(library(http/http_stream)).
:- system:use_module(library(http/http_unix_daemon)).
:- system:use_module(library(http/http_wrapper)).
:- system:use_module(library(http/hub)).
:- system:use_module(library(increval)).
:- system:use_module(library(intercept)).
:- system:use_module(library(iostream)).
:- system:use_module(library(iri_scheme/file)).
:- system:use_module(library(iso_639)).
:- system:use_module(library(isub)).
:- system:use_module(library(jpl)).
:- user:use_module(library(latex2html/latex2html)).
:- system:use_module(library(lazy_lists)).
:- system:use_module(library(listing)).
%:- system:use_module(library(lists)).
:- system:use_module(library(lynx/format)).
:- system:use_module(library(lynx/html_style)).
:- system:use_module(library(lynx/html_text)).
:- system:use_module(library(lynx/pldoc_style)).
:- system:use_module(library(main)).
:- system:use_module(library(make)).
:- system:use_module(library(mallocinfo)).
:- system:use_module(library(md5)).
:- system:use_module(library(memfile)).
:- system:use_module(library(modules)).
:- system:use_module(library(mqi)).
:- system:use_module(library(nb_rbtrees)).
:- system:use_module(library(nb_set)).
:- system:use_module(library(obfuscate)).
:- system:use_module(library(occurs)).
:- system:use_module(library(odbc)).
:- system:use_module(library(operators)).
:- system:use_module(library(option)).
:- system:use_module(library(optparse)).
:- system:use_module(library(ordsets)).
:- system:use_module(library(oset)).
:- system:use_module(library(pairs)).
:- system:use_module(library(paxos)).
:- system:use_module(library(pcre)).
:- system:use_module(library(pdt_console)).
:- system:use_module(library(pengines)).
:- system:use_module(library(pengines_io)).
:- system:use_module(library(pengines_sandbox)).
:- user:use_module(library(persistency)).
:- system:use_module(library(pio)).
:- system:use_module(library(pldoc)).
:- system:use_module(library(pldoc/doc_access)).
:- system:use_module(library(pldoc/doc_colour)).
:- system:use_module(library(pldoc/doc_htmlsrc)).
:- system:use_module(library(pldoc/doc_index)).
:- system:use_module(library(pldoc/doc_library)).
:- system:use_module(library(pldoc/doc_modes)).
:- system:use_module(library(pldoc/doc_pack)).
:- system:use_module(library(pldoc/doc_process)).
:- system:use_module(library(pldoc/doc_register)).
:- system:use_module(library(pldoc/doc_search)).
:- system:use_module(library(pldoc/doc_util)).
:- system:use_module(library(pldoc/doc_wiki)).
:- system:use_module(library(pldoc/doc_words)).
:- include(library(pldoc/hooks)).
:- system:use_module(library(pldoc/man_index)).
:- system:use_module(library(plunit)).
:- system:use_module(library(porter_stem)).
:- system:use_module(library(portray_text)).
:- system:use_module(library(pprint)).
:- system:use_module(library(predicate_options)).
:- system:use_module(library(process)).
:- system:use_module(library(prolog_autoload)).
:- system:use_module(library(prolog_breakpoints)).
:- system:use_module(library(prolog_clause)).
:- system:use_module(library(prolog_code)).
:- system:use_module(library(prolog_codewalk)).
:- system:use_module(library(prolog_colour)).
:- system:use_module(library(prolog_config)).
:- system:use_module(library(prolog_debug)).
:- system:use_module(library(prolog_deps)).
:- system:use_module(library(prolog_format)).
:- system:use_module(library(prolog_history)).
:- system:use_module(library(prolog_install)).
:- system:use_module(library(prolog_jiti)).
:- system:use_module(library(prolog_metainference)).
:- system:use_module(library(prolog_pack)).
:- system:use_module(library(prolog_server)).
:- system:use_module(library(prolog_source)).
:- system:use_module(library(prolog_stack)).
:- system:use_module(library(prolog_stream)).
:- system:use_module(library(prolog_trace)).
:- system:use_module(library(prolog_wrap)).
:- system:use_module(library(prolog_xref)).
:- system:use_module(library(protobufs)).
:- system:use_module(library(protobufs/protoc_gen_prolog_pb/google/protobuf/compiler/plugin_pb)).
:- system:use_module(library(protobufs/protoc_gen_prolog_pb/google/protobuf/descriptor_pb)).
:- system:use_module(library(pure_input)).
:- system:use_module(library(pwp)).
:- system:use_module(library(qpforeign)).
:- system:use_module(library(qsave)).
:- system:use_module(library(quasi_quotations)).
:- system:use_module(library(quintus)).
:- system:use_module(library(random)).
:- system:use_module(library(rbtrees)).
:- system:use_module(library(rdf)).
:- system:use_module(library(rdf_diagram)).
:- system:use_module(library(rdf_parser)).
:- system:use_module(library(rdf_triple)).
:- system:use_module(library(rdf_write)).
:- system:use_module(library(readline)).
:- system:use_module(library(readln)).
:- system:use_module(library(readutil)).
:- user:use_module(library(record)).
:- system:use_module(library(redis)).
:- system:use_module(library(redis_streams)).
:- user:use_module(library(rewrite_term)).
:- system:use_module(library(rlimit)).
:- system:use_module(library(saml)).
:- system:use_module(library(sandbox)).
:- user:use_module(library(semweb/rdf11)).
:- if(false).
/*
:- system:use_module(library(semweb/rdf11_containers)).
:- system:use_module(library(semweb/rdf_cache)).
:- system:use_module(library(semweb/rdf_compare)).
:- system:use_module(library(semweb/rdf_db)).
:- system:use_module(library(semweb/rdf_edit)).
:- system:use_module(library(semweb/rdf_http_plugin)).
:- system:use_module(library(semweb/rdf_library)).
:- system:use_module(library(semweb/rdf_litindex)).
:- system:use_module(library(semweb/rdf_ntriples)).
:- system:use_module(library(semweb/rdf_persistency)).
:- system:use_module(library(semweb/rdf_portray)).
:- system:use_module(library(semweb/rdf_prefixes)).
:- system:use_module(library(semweb/rdf_sandbox)).
:- system:use_module(library(semweb/rdf_turtle)).
:- system:use_module(library(semweb/rdf_turtle_write)).
:- system:use_module(library(semweb/rdf_zlib_plugin)).
*/
:- system:use_module(library(semweb/rdfa)).
:- system:use_module(library(semweb/rdfs)).
:- system:use_module(library(semweb/sparql_client)).
:- system:use_module(library(semweb/turtle)).
:- system:use_module(library(settings)).
:- system:use_module(library(sgml)).
:- system:use_module(library(sgml_write)).
:- system:use_module(library(sha)).
:- system:use_module(library(shell)).
:- system:use_module(library(shlib)).
:- system:use_module(library(snowball)).
:- system:use_module(library(socket)).
:- system:use_module(library(solution_sequences)).
:- system:use_module(library(sort)).
:- system:use_module(library(ssl)).
:- system:use_module(library(statistics)).
%:- abolish(system:time,1).
:- user:use_module(library(jpl)).
:- user:use_module(library(pldoc/doc_process)).
:- user:use_module(library(pprint)).
:- system:use_module(library(stomp)).
:- system:use_module(library(streaminfo)).
:- system:use_module(library(streampool)).
:- system:use_module(library(strings)).
:- system:use_module(library(syslog)).
:- system:use_module(library(system)).
:- system:use_module(library(table)).
:- system:use_module(library(table_util)).
%:- system:use_module(library(tables)).
%:- system:use_module(library(tabling)).
:- system:use_module(library(term_to_json)).
:- system:use_module(library(terms)).
:- system:use_module(library(test_cover)).
:- system:use_module(library(test_wizard)).
%:- system:use_module(library(theme/auto)).
%:- system:use_module(library(theme/dark)).
:- system:use_module(library(thread)).
:- system:use_module(library(thread_pool)).
:- system:use_module(library(threadutil)).
:- system:use_module(library(time)).
:- system:use_module(library(tipc/tipc)).
:- system:use_module(library(tipc/tipc_broadcast)).
:- system:use_module(library(tipc/tipc_linda)).
:- system:use_module(library(tipc/tipc_paxos)).
:- system:use_module(library(tty)).
:- system:use_module(library(udp_broadcast)).
:- system:use_module(library(ugraphs)).
:- system:use_module(library(uid)).
%:- system:use_module(library(unicode)).
%:- system:use_module(library(unicode/blocks)).
%:- system:use_module(library(unicode/unicode_data)).
:- system:use_module(library(unix)).
:- system:use_module(library(uri)).
:- system:use_module(library(url)).
:- system:use_module(library(utf8)).
:- system:use_module(library(uuid)).
%w:- system:use_module(library(varnumbers)).
:- system:use_module(library(vm)).
:- endif.
%:- user:use_module(library(wfs)).
:- system:use_module(library(wfs),[call_residual_program/2,call_delays/2,delays_residual_program/2,answer_residual/2]).
:- system:use_module(library(when)).
%w :- system:use_module(library(win_menu)).
:- system:use_module(library(writef)).
:- system:use_module(library(www_browser)).
:- system:use_module(library(xmldsig)).
:- system:use_module(library(xmlenc)).
%:- system:use_module(library(xpath)).
:- system:use_module(library(xsdp_types)).
:- system:use_module(library(yaml)).
:- system:use_module(library(zip)).
:- system:use_module(library(zlib)).


%:- system:use_module(library(yall)).

:- endif.
% :- predicate_inheritance:kb_global(plunit:loading_unit/4).


%=

%% is_startup_script is semidet.
%
% If Startup Script.
%
is_startup_script:- prolog_load_context(source, File),is_startup_script(File).


:-export(is_startup_script/1).

is_startup_script(Name):- var(Name),!,absolute_startup_script(Path),directory_file_path(_,Name,Path).
is_startup_script(Name):- absolute_file_name(Name,File,[file_type(prolog),access(read),file_errors(fail)]),Name\==File,!,is_startup_script(File).
is_startup_script(Name):- exists_source(Name),absolute_startup_script(Path),same_file(Name,Path),!.
is_startup_script(Name):- absolute_startup_script(Path),directory_file_path(_,Named,Path),atom_concat(Name,_,Named),!.

absolute_startup_script(AFile):- short_startup_script(File),
   absolute_file_name(File,AFile,[file_type(prolog),access(read),file_errors(fail)]).

:-export(short_startup_script/1).

script_type('-f').
script_type('-l').
script_type('-s').
script_type(A):-atom(A), \+ atom_concat('-',_,A).

short_startup_script(File):- current_prolog_flag(associated_file,File).
short_startup_script(File):- sub_argv(Type,File),exists_source(File),script_type(Type).

sub_argv(X,Y):-app_argv(List),
  (append(ListL,[--|_],List) ->
    append(_,[X,Y|_],ListL) ;
    append(_,[X,Y|_],List)).


:- dynamic(lmconf:saved_app_argv/1).
app_argv(Atom):- \+ atom(Atom),!,current_app_argv(Atom).
app_argv(Atom):- app_argv_off(Atom),!,fail.
app_argv(Atom):- app_argv1(Atom),!.
app_argv(Atom):- atom_concat(Pos,'=yes',Atom),!,app_argv1(Pos).
app_argv(Atom):- \+ is_argv_neg(Atom), app_argv1('--all'), atom_concat('--',_Stem,Atom).

app_argv_ok(Atom):- app_argv1(Atom),!.
app_argv_ok(Atom):- \+ app_argv_off(Atom).

is_argv_neg(Neg):- atom_concat('--no',_,Neg).

app_argv_off(Neg):- is_argv_neg(Neg),!,fail.
app_argv_off(Atom):- atom_concat('--',Pos,Atom), atom_concat('--no',Pos,Neg),app_argv1(Neg),!.
app_argv_off(Pos):-  atom_concat('--no',Pos,Neg),app_argv1(Neg),!.
app_argv_off(Pos):- atom_concat(Pos,'=no',Neg),app_argv1(Neg),!.

app_argv1(Atom):- current_app_argv(List),member(Atom,List).
app_argv1(Atom):- lmconf:saved_app_argv(Atom),\+ is_list(Atom).

current_app_argv(List):- lmconf:saved_app_argv(List).
current_app_argv(List):- current_prolog_flag(argv,List),List\==[].
current_app_argv(List):- current_prolog_flag(os_argv,List).

shell_format(Fmt,Args):-format(string(S),Fmt,Args),shell(S),!.
start_tty_redirect(PORT):-
  PORT100 is PORT + 100,
  shell_format('lsof -t -i:~w | xargs --no-run-if-empty kill -9',[PORT100]),
  % shell_format('nohup node app.js -p ~w -c rlwrap -a -A -r -c -N -r --file=completion_~w --history-filename=history_~w -s 1000 telnet localhost ~w &',[PORT100,PORT,PORT,PORT]),
  shell_format('nohup ttyd -r 100 -p ~w rlwrap -a -A -r -c -N -r --file=completion_~w --history-filename=history_~w -s 1000 telnet localhost ~w &',[PORT100,PORT,PORT,PORT]),
  !.


erase_clause(H,B):-
  BH=B+H,BHC=BC+HC,
  copy_term(BH,BHC),
  clause(HC,BC,Ref),
  BH=@=BHC,
  erase(Ref).


%% maybe_notrace(:Goal) is nondet.
%
% When not tracing, try to run Goal.
%   if Goal has a problem (like fails)
%         run inside rtrace/1 (the non-interactive debugger).
% If tracing, try to run Goal inside of quietly(Goal)
%   if Goal has a problem (like fails)
%         trace interactively.
%
% @NOTE quietly/1 is the nondet version of notrace/1.

:- meta_predicate(at_current_Y(+, :)).
at_current_Y(_S,Goal):- maybe_notrace(Goal),!.

:- meta_predicate(maybe_notrace(:)).
maybe_notrace(Goal):- tracing
 -> (debug,maybe_one(quietly(Goal), ftrace(Goal)))
 ; maybe_one(enotrace(Goal),ftrace(Goal)).

:- meta_predicate(maybe_one(:,:)).
maybe_one(Goal,Else):- catch(call(Goal),E,(dumpST,writeln(E),fail))*-> true ; (wdmsg(maybe_one(Goal,Else)),Else).
/*maybe_one(Goal,Else):-
  (catch(Goal,E1,(wdmsg(error_maybe_zotrace(E1,Goal)),Else))
   -> !
   ; (( wdmsg(failed_maybe_zotrace(Goal)),
     ignore(catch(Else,E2,(wdmsg(else_error_maybe_zotrace(E2, Else, goal(Goal))),(nonvar(E1)->throw(E1);throw(E2)))))))).
*/

%=======================================
% DURING/AFTER BOOT HOOKS
%=======================================

:- multifile(lmconf:at_restore_goal/2).
:- dynamic(lmconf:at_restore_goal/2).


%% before_boot(:Goal) is semidet.
%
% Run a +Goal just before entering/returning to prolog/0 or main goal.
%
%  much like to initialization(Goal,[restore]).  but *also* happens in non-compiled programs
%
%
%  swipl -l some_startup_file   - run before the banner would be displayed
%
%  ./some_qaved_program         - run at 'restore' time
%
%  ?-  use_module(has_several_hookers). -  run all just before returning to toplevel
%
%  swipl -s some_startup_file   - run immediately unless is module
%


:- meta_predicate(at_phase(:, +)).
at_phase(Goal, When):- is_list(When), !, maplist(at_phase(Goal), When).
at_phase(MGoal, When):- strip_module(MGoal,M,Goal),
  (\+ compound(Goal); \+ functor(Goal,at_current_Y,_)),
  % add_history(MGoal),
  source_location(S,L),!,at_phase(at_current_Y(cuz(S:L),M:Goal), When).
at_phase(Goal, When):- When == now, !, ignore(try_pending_init(When,Goal)).
at_phase(Goal, When):-
   current_prolog_flag(current_phase, Current),
   (When == Current ; before_phase(When, Current)), !,
   system:assertz(lmconf:at_restore_goal(When,Goal)),
   ignore(try_pending_init(When,Goal)).

at_phase(Goal, When):- system:assertz(lmconf:at_restore_goal(When,Goal)).

before_phase(P1,P2):- number_phase(N1,P1), number_phase(N2,P2), N1 < N2.

:- set_prolog_flag(current_phase, load).

%% loadtime_boot(:Goal) is semidet.
%
% Run a +Goal as soon as possible
%
%  much like initialization(Goal,[now]).  but *also* happens in compiled systems
%
%
%  swipl -l some_startup_file   - like initialization(Goal,[now])
%
%  ./some_qaved_program         - like initialization(Goal,[restore])
%
%  ?-  use_module(has_several_hookers). -  like initialization(Goal,[now])
%
%  swipl -s some_startup_file   - like initialization(Goal,[now])
%
:- meta_predicate(loadtime_boot(:)).
loadtime_boot(G):- at_phase(G, [now,load]).

:- meta_predicate(before_boot(:)).
before_boot(G):- at_phase(G, [now,before_boot]).

:- meta_predicate(during_boot(:)).
during_boot(G):- at_phase(G, [during_boot]).

:- meta_predicate(after_boot(:)).
after_boot(G):- at_phase(G, [after_boot]).

:- meta_predicate(runtime_boot(:)).
runtime_boot(Goal):- at_phase(Goal, [runtime]).

% doesnt run if --nonet
:- meta_predicate(during_net_boot(:)).
during_net_boot(M:Goal):- after_boot(whenever_flag_permits(run_network,M:Goal)).

% --nonet
:- meta_predicate(after_net_boot(:)).
after_net_boot(M:Goal):- runtime_boot(whenever_flag_permits(run_network,M:Goal)).

:- meta_predicate(test_runtime_boot(:)).
test_runtime_boot(M:Goal):- nop(after_boot(M:sanity(M:Goal))).

%% call_last_is_var( :GoalMCall) is semidet.
%
% Call Last If Is A Variable.
%
:- meta_predicate(call_last_is_var(0)).
call_last_is_var(MCall):- strip_module(MCall,M,Call),
   must((compound(Call),functor(Call,_,A))),
   arg(A,Call,Last),nonvar(Last),Call=..FArgs,
   append(Left,[Last],FArgs),
   append(Left,[IsVar],NFArgs),NewCall=..NFArgs,!,
    ( M:NewCall*->IsVar=Last;fail).


:- meta_predicate(if_script(:)).

%% if_script( :Goal) is semidet.
%
% If this is a Startup Script call Goal
%
if_script(Call):- is_startup_script->Call;wemsg(\+ is_startup_script(Call)).

is_startup_file(File):- is_startup_script(File).

%=======================================
%= CALL BOOT HOOKS
%=======================================
:- dynamic(lmcache:called_startup_goal/2).
:- volatile(lmcache:called_startup_goal/2).


number_phase(1,load). % before compile
number_phase(2,before_boot).  % before booting/compile  compiled sits at 2 (and resumes from 2)
number_phase(3,during_boot).
number_phase(4,after_boot).  % after booting
number_phase(5,runtime). % when running


init_why(Phase, Why):-
  %wemsg("%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%%"),
  %wemsg("%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%%"),
  nop(wemsg(init_why(Phase, Why))),
  set_prolog_flag(current_phase, Phase),
  %wemsg("%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%%"),
  %wemsg("%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%~%%%%"),!,
  run_pending_inits(Phase).

:- module_transparent(run_pending_inits/0).
run_pending_inits:-
  current_prolog_flag(current_phase, Phase),
  run_pending_inits(Phase).

:- module_transparent(run_pending_inits/1).
run_pending_inits(Until):-
 number_phase(UntilN,Until),
 forall(
  (number_phase(WhenN,When),
   lmconf:at_restore_goal(When,Goal),
   WhenN=<UntilN),
   try_pending_init(When,Goal)).


has_ran_once(When,Goal):- lmcache:called_startup_goal(When,GoalW), GoalW =@= Goal,!.
has_ran_once(Goal):- has_ran_once(_,Goal).

:- meta_predicate try_pending_init(+,:).
try_pending_init(_,Goal):- has_ran_once(_, Goal),!.
try_pending_init(When,Goal):-
    assertz(lmcache:called_startup_goal(When,Goal)),
    ( \+ \+ maybe_notrace(Goal)
      -> true ;
       erase_clause(lmcache:called_startup_goal(When,Goal),true)).


:- if(app_argv('--nonet')).
:- set_prolog_flag(run_network,false).
:- else.
:- set_prolog_flag(run_network,true).
:- endif.



%= Register a hook after welcome
:- multifile prolog:message//1.
% prolog:message(welcome) -->  {init_why(welcome),fail}.

%= Register a hook after our so-called startup file (Should be last file in list)
:- multifile(system:'$init_goal'/3).
:- dynamic(system:'$init_goal'/3).
:- module_transparent(system:'$init_goal'/3).
:- forall(absolute_startup_script(F),
    (assertz(system:'$init_goal'(F,logicmoo_startup:init_why(before_boot,after(F)),F:9999)))).

:- if(false).
:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).
user:expand_answer(_Bindings, _ExpandedBindings):- run_pending_inits,fail.
:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
user:expand_query(_Goal, _Expanded, _Bindings, _ExpandedBindings):-  run_pending_inits,fail.
:- endif.

%:- system:use_module(logicmoo_utils_all).
%:- fixup_exports.

:- if( app_argv1('--upgrade') ).
:- whenever_flag_permits(run_network,logicmoo_update).
:- endif.


%:- system:use_module(library(logicmoo/each_call)).
%:- system:use_module(library(logicmoo_startup)).

:- meta_predicate if_debugging(*,0).


fav_module:-
  '$current_typein_module'(Module),prolog_load_context(module,SourceModule),
  ((SourceModule==Module) -> true ;
  ((SourceModule==user -> '$set_source_module'(Module) ; true),
  (Module==user -> '$set_typein_module'(SourceModule) ; true))).



fav_debug9:-
  fav_module,
  fav_debug,
   set_prolog_flag(access_level,system),
   set_prolog_flag(write_attributes,ignore),
   set_prolog_flag(fileerrors,true),
   %set_prolog_flag(gc,false),
   %set_prolog_flag(occurs_check,true),
 % set_prolog_flag(retry_undefined, none),
   !.

fav_debug:-
 set_prolog_flag(backtrace, true),
 set_prolog_flag(backtrace_goal_depth, 2000),
 set_prolog_flag(backtrace_show_lines, true),
 %set_prolog_flag(debug,true),
 set_prolog_flag(debug_on_error,true),
 set_prolog_flag(debugger_show_context,true),
 %set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(10), attributes(write)]),
 set_prolog_flag(report_error,true),
 set_prolog_flag(runtime_debug, 3), % 2 = important but dont sacrifice other features for it
 set_prolog_flag(runtime_safety, 3),  % 3 = very important
 set_prolog_flag(runtime_speed, 0), % 1 = default
 set_prolog_flag(runtime_speed, 1), % 0 = dont care
 set_prolog_flag(unsafe_speedups, false),
  set_prolog_flag(verbose_autoload,true),
  set_prolog_flag(verbose_load,full),
 !.

%setup_hist:-  '$toplevel':setup_history.
%:- setup_hist.

:- dynamic(goal_main_interval/2).
:- meta_predicate(do_each_main_interval(:, +)).
:- export(do_each_main_interval/2).
do_each_main_interval(Goal, Interval):-
 term_to_atom(Goal, Name),
 retractall(goal_main_interval(Name,_)),
 asserta(goal_main_interval(Name,Interval)),
 (((thread_property(T,alias(Name)),
     thread_property(T,status(running))))
    -> true ;
    thread_create(do_each_main_interval(Goal, Name, Interval),_ID,
       [detached(true),alias(Name)])).

do_each_main_interval(Goal, Name, Interval):-
  repeat,
  thread_signal(main,call(Goal)),
  (goal_main_interval(Name,DInterval)->true;DInterval=Interval),
  sleep(DInterval),
  fail.


bt:- ds, dumpST9.
ds:-
 ensure_loaded(library(prolog_stack)),
 prolog_stack:export(prolog_stack:get_prolog_backtrace_lc/3),
 use_module(library(prolog_stack),[print_prolog_backtrace/2,get_prolog_backtrace_lc/3]),
  notrace(prolog_stack:call(call,get_prolog_backtrace_lc,8000, Stack, [goal_depth(600)])),
  stream_property(S,file_no(1)), prolog_stack:print_prolog_backtrace(S, Stack),
  ignore((current_output(Out), \+ stream_property(Out,file_no(1)), print_prolog_backtrace(Out, Stack))),!.
:- system:import(ds).
:- system:import(bt).


:- meta_predicate(whenever_flag_permits(+,:)).
whenever_flag_permits(Flag,G):- (current_prolog_flag(Flag, false) -> true ; G).
whenever(Flag, G):- whenever_flag_permits(Flag,G).


% startup_file0(File):- sub_argv(['-g',Opt]),atom_to_term(Opt,LoadsFile,_),is_loads_file(LoadsFile,File).
is_loads_file(ensure_loaded(SFile),File):- strip_module(File,_,SFile).
is_loads_file([File],SFile):- strip_module(File,_,SFile).
is_loads_file(consult(SFile),File):- strip_module(File,_,SFile).
is_loads_file(use_module(SFile),File):- strip_module(File,_,SFile).
is_loads_file(_:SFile,File):-!,is_loads_file(SFile,File).

%=======================================
% Load only if exists
%=======================================


%% if_file_exists( ?M) is semidet.
%
% If File Exists.
%
:- meta_predicate(if_file_exists(:)).
if_file_exists(M:Call):- arg(1,Call,MMFile),strip_module(MMFile,_,File),
 (exists_source(File)-> must(M:Call);nop(wemsg(warning,not_installing(M,Call)))),!.



% sets up and restore the subsystems

:- module_transparent(load_library_system/1).
load_library_system(M:File):- !, load_library_system(M,File).
load_library_system(File):- context_module(M),load_library_system(M,File).
:- export(load_library_system/1).
:- system:import(load_library_system/1).

:- module_transparent(load_library_system/2).
load_library_system(user,File):-!, before_boot(gripe_time(40,(if_file_exists(ensure_loaded(system:File))))).
load_library_system(M,File):- before_boot(gripe_time(40,(if_file_exists(ensure_loaded(M:File))))).
:- export(load_library_system/2).
:- system:import(load_library_system/2).


:- meta_predicate iff_defined(*).
:- meta_predicate iff_defined(:,0).
:- module_transparent((iff_defined/1,iff_defined/2)).

%% iff_defined( ?G) is semidet.
%
% If Defined.
%
iff_defined(Goal):- iff_defined(Goal,(rtrace,(print_message(warning,warn_undefined(Goal))),!,fail)).

%% iff_defined( ?Goal, :GoalElse) is semidet.
%
% If Defined Else.
%
iff_defined(Goal,Else):- current_predicate(_,Goal)*->Goal;Else.
% iff_defined(M:Goal,Else):- !, current_predicate(_,OM:Goal),!,OM:Goal;Else.
%iff_defined(Goal,  Else):- current_predicate(_,OM:Goal)->OM:Goal;Else.



:- module_transparent((add_history/1,qsave_lm/1,ignore_not_not/1,load_library_system/1,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2,
          all_source_file_predicates_are_exported/4)).

:- meta_predicate(ignore_not_not(0)).




normally(G):- locally(set_prolog_flag(runtime_debug,0),locally(set_prolog_flag(bugger,false),G)).


shared_vars(Left,Right,SVG):-quietly(( term_variables(Left,Vs1),term_variables(Right,Vs2),intersect_eq0(Vs2,Vs1,SVG))).


  member_eq0(X, [Y|Ys]) :- X==Y;member_eq0(X,Ys).

 intersect_eq0([], _, []).
 intersect_eq0([X|Xs], Ys, L) :-
         (   member_eq0(X, Ys)
         ->  L = [X|T],
             intersect_eq0(Xs, Ys, T)
         ;   intersect_eq0(Xs, Ys, L)
         ).


% ======================================================
% Add Extra file_search_paths
% ======================================================
:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).


add_file_search_path_safe(Name,Path):-  absolute_directory(Path,Dir),!,
   is_absolute_file_name(Dir), (( \+ user:file_search_path(Name,Dir)) ->asserta(user:file_search_path(Name,Dir));true).
add_file_search_path_safe(Name,Path):- writeln('user_error',skip(add_file_search_path_safe(Name,Path))),!.

absolute_directory(Dir,Dir):- atom(Dir),is_absolute_file_name(Dir),exists_directory(Dir),!.
absolute_directory(Dir,ABS):- absolute_file_name(Dir,ABS,[file_type(directory),solutions(all),expand(true),case_sensitive(false),access(read),file_errors(fail)]),exists_directory(ABS),!.
absolute_directory(Dir,ABS):- absolute_file_name(library(Dir),ABS,[file_type(directory),solutions(all),case_sensitive(false),expand(true),access(read),file_errors(fail)]),exists_directory(ABS),!.

%~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% DEFAULT PROLOG FLAGS
%~%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
 % :- set_prolog_flag(subclause_expansion,default).
 % :- set_prolog_flag(subclause_expansion,false).
:- set_prolog_flag(dialect_pfc,default).
:- set_prolog_flag(qcompile,part).
:- set_prolog_flag(do_renames,never).
:- if( \+ current_module(prolog_stack)).
:- user:use_module(library(prolog_stack)).
 prolog_stack:stack_guard(none).
:- endif.


/*
system:logicmoo_user_stacks:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),
  set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

:- rtrace,during_boot(system:logicmoo_user_stacks).
:- Six = 6, set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)),set_prolog_stack(trail, limit(Six*10**9)).

:- set_prolog_stack(global, limit(32*10**9)).
:- set_prolog_stack(local, limit(32*10**9)).
:- set_prolog_stack(trail, limit(32*10**9)).
:- module_transparent( (set_prolog_stack_gb)/1).
:- before_boot(set_prolog_stack_gb(16)).


*/

%% set_prolog_stack_gb( +Six) is semidet.
%
% Set Prolog Stack Gb.
%
set_prolog_stack_gb(Six):-set_prolog_stack(global, limit(Six*10**9)),set_prolog_stack(local, limit(Six*10**9)), set_prolog_stack(trail, limit(Six*10**9)).

%:- set_prolog_stack_gb(16).


if_debugging(Topic,Goal):- debugging(Topic)->call(Goal);true.

%% all_source_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(all_source_file_predicates_are_exported/0).
all_source_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
all_source_file_predicates_are_exported:-
 prolog_load_context(source,S), prolog_load_context(module,LC),
 all_source_file_predicates_are_exported(S,LC),!.
all_source_file_predicates_are_exported:-
 prolog_load_context(module,LC),'$current_typein_module'(TIM),
 forall((LC\==user,module_property(LC,file(S))),all_source_file_predicates_are_exported(S,LC)),
 forall((TIM\==LC,TIM\==user,module_property(TIM,file(S))),all_source_file_predicates_are_exported(S,TIM)).


:- dynamic(lmconfig:never_export_named/3).
:- public(lmconfig:never_export_named/3).

lmconfig:never_export_named(_,attr_unify_hook,2).
lmconfig:never_export_named(_,attribute_goals,3).
lmconfig:never_export_named(_,project_attributes,2).
lmconfig:never_export_named(_,attr_portray_hook,2).

%lmconfig:never_export_named(system,_,_).
%lmconfig:never_export_named(user,_,_).
/*
lmconfig:never_export_named(_,'/',2).
lmconfig:never_export_named(_,'>>',2).
lmconfig:never_export_named(_,'==>',2).
lmconfig:never_export_named(_,'=>',2).
*/
lmconfig:never_export_named(_,Symbol,N):- upcase_atom(Symbol,UC),Symbol==UC, N=<3,!.
lmconfig:never_export_named(_,isa,2).
lmconfig:never_export_named(_,F,_):- atom_concat('$',_,F) ; atom_concat('__aux',_,F).

lmconfig:never_reexport_named(_,goal_expansion,_).
lmconfig:never_reexport_named(_,term_expansion,_).
lmconfig:never_reexport_named(_,predicate_options,_).

% lmconfig:never_export_named(_M,F,A):- current_predicate(user:F/A).

% :- module_transparent(all_source_file_predicates_are_exported/2).
maybe_export([],_,_,_):-!.
maybe_export([LC|LCT],M,F,A):- maybe_export(LC,M,F,A),!,maybe_export(LCT,M,F,A).
maybe_export(false,_,_,_):-!.
maybe_export(true, M,F,A):- !, maybe_export(system, M,F,A).
maybe_export(_, M,F,A):- lmconfig:never_export_named(M,F,A).
maybe_export(LC,M,_,_):- \+ (atom(LC); \+ atom(M)), !.
maybe_export(LC,M,F,A):- LC==M, !, M:export(M:F/A).
maybe_export(LC,_,F,A):- current_predicate(LC:F/A),!.
maybe_export(LC,M,F,A):-
   LC:import(M:F/A),
   (lmconfig:never_reexport_named(LC,F,A)-> true ; LC:export(M:F/A)).

:- set_prolog_flag(logicmoo_import_to_system, baseKB).

:- module_transparent(all_source_file_predicates_are_exported/2).
all_source_file_predicates_are_exported(S,LC):-
 (ignore(source_location(S,_);prolog_load_context(source,S))),
  ignore(prolog_load_context(module,LC)),

 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
   \+ atom_concat(_,'__aux_',F),
  %(module_property(M,exports(List))-> \+ member(F/A,List); true),
  % M:public(M:F/A),
  now_and_later(all_source_file_predicates_are_exported(LC,M,F,A))))).

:- module_transparent(all_source_file_predicates_are_exported/4).
all_source_file_predicates_are_exported(LC,M,F,A):-
  enotrace(catch(maybe_export(M,M,F,A),_,fail)), maybe_export(LC,M,F,A),
 (current_prolog_flag(logicmoo_import_to_system, BaseKB)-> maybe_export(BaseKB,M,F,A) ; true),
  maybe_export(system,M,F,A).

:- export(con_x_fail/1).
:- meta_predicate(con_x_fail(:)).
con_x_fail((G1,G2)):-!, con_x_fail(G1),con_x_fail(G2).
con_x_fail(M:(G1,G2)):-!, con_x_fail(M:G1),con_x_fail(M:G2).
con_x_fail(G):-catch(G,_,fail).

:- meta_predicate(sexport(:)).
sexport(M:F/A):- M:export(M:F/A),system:import(M:F/A).

%% all_source_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
:- module_transparent(all_source_file_predicates_are_transparent/0).
all_source_file_predicates_are_transparent:- current_prolog_flag(xref,true),!.
all_source_file_predicates_are_transparent:-
 source_location(S,_), prolog_load_context(module,LC),
 all_source_file_predicates_are_transparent(S,LC),!.
all_source_file_predicates_are_transparent:-
 prolog_load_context(module,LC),'$current_typein_module'(TIM),
 forall((LC\==user,module_property(LC,file(S))),all_source_file_predicates_are_transparent(S,LC)),
 forall((TIM\==LC,TIM\==user,module_property(TIM,file(S))),all_source_file_predicates_are_transparent(S,TIM)).

:- module_transparent(all_source_file_predicates_are_transparent/2).
all_source_file_predicates_are_transparent(S,_LC):-
 forall(source_file(M:H,S),
 (functor(H,F,A),
   now_and_later(ensure_transparent(M,F,A)))).

ensure_transparent(M,F,A):- functor(H,F,A),
  ignore(((\+ predicate_property(M:H,transparent), \+ lmconfig:never_export_named(M,F,A), module_transparent(M:F/A),
  \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A])))).

dont_mess_with(baseKB:isa/2).

:- export(fixup_exports/0).

var_non_attvar(V):- var(V),\+ attvar(V).


getenv_or(Name,ValueO,Default):-
   (getenv(Name,RV)->Value=RV;Value=Default),
   (number(Default) -> ( \+ number(Value) -> atom_number(Value,ValueO); Value=ValueO);
      Value=ValueO).


/*
:- dynamic(baseKB:logicmoo_utils_separate/0).
:- retractall(baseKB:logicmoo_utils_separate).
:- set_prolog_flag(generate_debug_info, true).


% :- abox:defaultTBoxMt(_)->true;('$current_typein_module'(M),asserta(abox:defaultTBoxMt(M))).


:- dynamic(baseKB:mpred_is_impl_file/1).
:- multifile(baseKB:mpred_is_impl_file/1).
% :- volatile(baseKB:mpred_is_impl_file/1).


*/



qsave_lm:-!.
qsave_lm:-  is_startup_script(X),!,atom_concat(X,'.o',F),!,qsave_lm(F).
qsave_lm:- qsave_lm(qsaved_lm),!.
qsave_lm(_):- !.
qsave_lm(LM):- \+ access_file(LM,write),!,debug(logicmoo,'~N% NO FILE WRITE ~p~n',[qsave_lm(LM)]).
qsave_lm(_):- predicate_property(kb7166:assertion_content(_,_,_),number_of_clauses(N)),N>0,!.
qsave_lm(LM):-qsave_lm0(LM),!.
qsave_lm0(LM):- statistics(globallimit,G),statistics(locallimit,L),statistics(traillimit,T),
  X = qsave_program(LM,[toplevel(logicmoo_toplevel),
   goal(logicmoo_goal),op(save),
       stand_alone(false),
       class(development),
       autoload(false),
       % foreign(no_save),
       global(G),trail(T),local(L)]),
   wemsg(X),
   call(X).


run_prologmud :- ensure_loaded(library(prologmud_sample_games/run_mud_server)),init_why(runtime,run_prologmud).
init_logicmoo :- ensure_loaded(library(logicmoo_repl)),init_why(during_booting,init_logicmoo).


% invert_varname(NV):-  ignore(((NV=(N=V), V='$VAR'(N)))).

:- system:use_module(library(prolog_history)).

add_history(_):- prolog_load_context(reloading, true),!.
add_history(O):- add_history1(O).
:- export(add_history/1).
:- export(add_history1/1).
add_history1(O):- is_list(O), member(E,O), compound(E), !, maplist(add_history,O).
%add_history(O):- !, wdmsg(not_add_history(O)),!.
add_history1(O):- ignore_not_not((nonvar(O),make_historical(O,A),add_history0(A))),!.

ignore_not_not(G):- ignore((catch((( \+ \+ (ignore(once(G))))),_,fail))),!.

ensure_period_at_end(A,A):- string_concat(_,'.',A),!.
ensure_period_at_end(A,O):- string_concat(A,'.',O).

make_historical(O,PO):-
  make_historical0(O,A),
  string_trim1(A,A2),
  replace_in_string(['\n'=' ','   '=' ','    '=' '],A2,A3),
  ensure_period_at_end(A3,PO),!.

make_historical0(M:O,A):- (M==user),!, make_historical0(O,A).
make_historical0(whenever_flag_permits(_,O),A):-!,make_historical0(O,A).
make_historical0(add_history(O),A):-!,make_historical(O,A).
make_historical0(O,A):- string(O),A=O.
make_historical0(O,A):- ground(O),
  without_color(format(string(A), '~W', [O, [fullstop(true),portrayed(true),quoted(true),numbervars(true)]])),!.
make_historical0(O,A):-
    prolog_load_context(variable_names, Bindings),
    without_color(format(string(A), '~W', [O, [fullstop(true),portray(true),quoted(true),variable_names(Bindings)]])).

%:- multifile prolog:history/2.
:- nb_setval('$without_color',[]).
without_color(G):- locally_tl(print_mode(plain),with_b_setval('$without_color',true,G)).
with_color(G):- with_b_setval('$without_color',false,G).
with_b_setval(Name,Value,Goal):-
 (nb_current(Name,Was)->true;Was=[]),
 (Was == [] -> New=Value ; New=Value),
  scce_orig(
    b_setval(Name,New),
    Goal,
    b_setval(Name,Was)).


default_history_file(File):-
   catch(prolog_history:dir_history_file('.', File), E,
            (print_message(warning, E),fail)),!.

:- set_prolog_flag(history, 5000).
:- dynamic(histtmp:history_data/1).

string_trim1(X,Y):- string_concat(M,"\\040",X),!,string_trim1(M,Y).
string_trim1(X,Y):- string_concat(M,"\040",X),!,string_trim1(M,Y).
string_trim1(X,Y):- string_concat(XM,".\\040.",X),string_concat(XM,".",M),!,string_trim1(M,Y).
string_trim1(X,Y):- string_concat(XM,". .",X),string_concat(XM,".",M),!,string_trim1(M,Y).
string_trim1(X,Y):- string_concat(M," ",X),!,string_trim1(M,Y).
string_trim1(X,Y):- string_concat(M,"\n",X),!,string_trim1(M,Y).
string_trim1(X,Y):- current_predicate(string_trim/2),!,call(call,string_trim,X,Y).
string_trim1(X,X).

carelessly(G):- ignore(notrace(catch(G,E,((wemsg(E)),!,fail)))).
add_history0(_):- notrace(app_argv('--no-history')),!.
add_history0(S):-
  forall(clause('$history':'$history'(_,W),true,_Ref),carelessly(add_history00(W))),
  carelessly(add_history00(S)),!.

add_history00(A):- make_historical(A,S),!,add_history01(S).
add_history01(A):- histtmp:history_data(A),!.
add_history01(A):- assert(histtmp:history_data(A)),
   default_history_file(File),
   carelessly(setup_call_cleanup(open(File,append,Out),format(Out,'~w~n',[A]),close(Out))),
   ((fail,current_prolog_flag(readline,editline)) -> User_input = libedit_input; User_input = user_input),
   prolog:history(User_input, load(File)).

get_hist_next_num(N1):- clause('$history':'$history'(N,_),true,_Ref), !, N1 is N + 1.
get_hist_next_num(1).

add_history02(A0):-
   make_historical(A0,A),
   carelessly(prolog_history:prolog_history(enable)),
   current_input(S),
   get_hist_next_num(N),
   asserta('$history':'$history'(N,A)),
   (current_prolog_flag(readline,editline) -> User_input = libedit_input; User_input = user_input),
   carelessly(prolog:history(S,add(A))),
      ignore((
         stream_property(UI,file_no(0)),
         ( \+ same_streams(S,UI)),
            carelessly(prolog:history(User_input,add(A))))),!,
   nop(carelessly((default_history_file(File),prolog:history(User_input, save(File))))).

add_history2(X):- make_historical(X,H),assertz_new(histtmp:history_data(H)).

load_history_from_file(File):- \+ exists_file(File),!.
load_history_from_file(File):-
 setup_call_cleanup(
  open(File,read,In,[encoding(utf8)]),
   (repeat, read_line_to_string(In,String), (String=end_of_file -> ! ; (add_history2(String), fail))),
   close(In)).

load_history:-
 carelessly(prolog_history:prolog_history(enable)),
 default_history_file(File),doug_only(show_call(always,load_history_from_file(File))),!,
 %(current_prolog_flag(readline,editline) -> User_input = libedit_input; User_input = user_input),
 %prolog:history(User_input, load(File)),
 doug_only(listing('$history':'$history'/2)),
 doug_only(listing(histtmp:history_data/1)),
 !.

doug_only(G):- nop(G).


nb_linkval_current(N,V):-duplicate_term(V,VV),V=VV,nb_linkval(N,VV),nb_current(N,V).

extend_varnames(ExpandedBindings):-
    prolog_load_context(variable_names,Vs),
    append(Vs,ExpandedBindings,NewVs),
    append(NewVs,[],NewVs),
    nb_linkval_current('$variable_names',NewVs).

:- if(false).
:- user:multifile(expand_answer/2).
:- user:dynamic(expand_answer/2).
user:expand_answer(Bindings, ExpandedBindings):-
    nb_linkval_current('$expand_answer',Bindings),
    toplevel_variables:expand_answer(Bindings, ExpandedBindings),
    nb_linkval_current('$expand_answer',ExpandedBindings).


:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
user:expand_query(Goal, _Expanded, Bindings, _ExpandedBindings):-        fail,
   ignore_not_not((once(( nb_linkval_current('$expand_query',Goal-Bindings),
    append(Bindings,[],Bindings),
    % ignore_not_not(nortrace),ignore_not_not(notrace),
    format(atom(A), '~W', [Goal, [fullstop(true),portray(true),quoted(true),variable_names(Bindings)]]),
    add_history0(A))))),
   fail.
:- endif.

:- module_transparent(fixup_exports/0).

fixup_exports:-
   all_source_file_predicates_are_exported,
   all_source_file_predicates_are_transparent,
   fixup_module_exports_now.

:- module_transparent(fixup_exports_system/0).
fixup_exports_system:-   (prolog_load_context(source,SF)-> reexport(SF) ; true).

:- module_transparent(fixup_module_exports_now/0).
fixup_module_exports_now:-
  (prolog_load_context(reloading,true)->true;fixup_module_exports_into(system)).

:- module_transparent(loading_from_module/1).
loading_from_module(From):- \+ source_location(_,_), !, '$current_typein_module'(From).
loading_from_module(From):- source_location(Src,_),module_property(From,file(Src)),!.
loading_from_module(From):- source_location(_,_),prolog_load_context(source,Src),module_property(From,file(Src)),!.
loading_from_module(From):- prolog_load_context(module,From),!.
loading_from_module(From):- '$current_source_module'(From),!.
loading_from_module(From):- strip_module(_,From,_).

:- module_transparent(fixup_module_exports_into/1).
fixup_module_exports_into(Into):-
  loading_from_module(From),
  now_and_later(fixup_module_exports_into_from(Into,From)).

:- module_transparent(fixup_module_exports_into_from/2).
fixup_module_exports_into_from(_Into,From):- system == From, !.
fixup_module_exports_into_from(Into,From):-
  nop(format('~N% ?- ~q. ~n',[fixup_module_exports_into_from(Into,From)])),
  forall((predicate_property(From:P,defined), \+ predicate_property(From:P,imported_from(_)),functor(P,F,A)),
    From:define_into_module(From:Into,F/A)).

:- meta_predicate(define_into_module(:,+)).
define_into_module(_:From:Into,FA):- define_into_module_now_and_later(From,Into,FA).
define_into_module(From:Into,FA):- define_into_module_now_and_later(From,Into,FA).

define_into_module_now_and_later(A,B,C):- now_and_later(define_into_module_now(A,B,C)).


define_into_module_now(From,Into,SMFA):-
 current_prolog_flag(access_level,Was),
   setup_call_cleanup(set_prolog_flag(access_level,system),
   define_into_module_now1(From,Into,SMFA),
   set_prolog_flag(access_level,Was)).

define_into_module_now1(_,Into,SM:FA):-!, define_into_module_now1(SM,Into,FA).
define_into_module_now1(From,Into,[A]):-!, define_into_module_now1(From,Into,A).
define_into_module_now1(From,Into,[A|B]):-!, define_into_module_now1(From,Into,A),define_into_module_now1(From,Into,B).
define_into_module_now1(From,Into,(A,B)):-!, define_into_module_now1(From,Into,A),define_into_module_now1(From,Into,B).

define_into_module_now1(From,[Into],FA):-!,define_into_module_now1(From,Into,FA).
define_into_module_now1(From,[Into,N],FA):-!,define_into_module_now1(From,Into,FA),define_into_module_now1(From,N,FA).
define_into_module_now1(From,Into,op(P,XFY,F)):-!,notrace(catch(From:op(P,XFY,F),_,true)),notrace(catch(Into:op(P,XFY,F),_,true)).
define_into_module_now1(From,Into,F//A):- number(A), !, A2 is A+2,define_into_module_now1(From,Into,F/A2).
define_into_module_now1(_,Into,F/A):- lmconfig:never_reexport_named(Into,F,A),!.
define_into_module_now1(_,Into,F/A):- lmconfig:never_export_named(Into,F,A),!.
%ignore((\+ current_predicate(Into:F/A), functor(P,F,A), Into:assert(((Into:P):- P)))),
define_into_module_now1(From,Into,F/A):-
 %From:dynamic(F/A),
 ignore(notrace(catch(From:export(From:F/A),_,true))),
 ignore(notrace(catch(From:module_transparent(From:F/A),_,true))),
 (((Into == From -> true ; ignore(notrace(catch('@'(import(From:F/A),Into),_,true)))))),
 !.
define_into_module_now1(From,Into,FA):- %format(user_error,'~N ~q ~n',[define_into_module_now(From,Into,FA)]),
  format(user_error,'~N ~q ~n',[define_into_module_now(From,Into,FA)]).

:- redefine_system_predicate(system:nop/1).
:- abolish(system:nop/1),asserta(system:nop(_)).
%! nop( :Goal) is det.
%
%  Comments out code without losing syntax
%

%:- fixup_module_exports_into(system).

:- define_into_module(
         [maybe_notrace/1,
          if_file_exists/1,
          absolute_startup_script/1,
          before_boot/1,
          during_boot/1,
          after_boot/1,
          sexport/1,
          has_ran_once/1,
          has_ran_once/2,
          app_argv/1,
          app_argv1/1,
          app_argv_ok/1,
          app_argv_off/1,
          qsave_bin/1,
          qsave_lm/1,
          qsave_lm/0,
          is_startup_file/1,
          add_absolute_search_folder/2,
          pack_upgrade_soft/1,
          is_startup_script/1,
          init_why/2,
          define_into_module/2,
          now_and_later/1,
          only_runtime/1,
          logicmoo_compiling_mud_server/0,
          call_safely/1,
          whenever/2,
          fixup_module_exports_now/0,
          whenever_flag_permits/2,
          during_net_boot/1,
          runtime_boot/1,
          test_runtime_boot/1,
          after_net_boot/1,
          fixup_exports_system/0,
          fixup_module_exports_into_from/2,
          fixup_module_exports_into/1,
          all_source_file_predicates_are_transparent/0,
          all_source_file_predicates_are_transparent/2,
          all_source_file_predicates_are_exported/0,
          all_source_file_predicates_are_exported/2,
          all_source_file_predicates_are_exported/4,
          run_pending_inits/0]).


:- fixup_exports.

:- system:use_module(library(debuggery/bugger)).

%:- system:reexport(library(debuggery/bugger)).

%:- logicmoo_startup:use_module(library(option),[options/3]).

logicmoo_base_port(Base):- getenv_or('LOGICMOO_BASE_PORT',Base,4000),!.
logicmoo_base_port(Base):- app_argv1(One),\+ is_list(One),
   (atom(One)-> (atomic_list_concat([_,Atom],'port=',One),atom_number(Atom,Base20))),!,Base is Base20 -20,
   setenv('LOGICMOO_BASE_PORT',Base).
:- export(logicmoo_base_port/1).
:- system:import(logicmoo_base_port/1).

% ==============================================
% Easier to trace while access_level system
% ==============================================
:- '$hide'('$toplevel':restore_debug/0).
:- '$hide'('$toplevel':save_debug/0).
%:- '$hide'('$toplevel':residue_vars/2).
:- '$hide'('system':deterministic/1).
:- '$hide'(toplevel_call/2).
:- '$hide'('$toplevel':'$query_loop'/0).

% ==============================================
% System metapredicates
% ==============================================
/*
:- meta_predicate '$syspreds':bit(2,?,?).
:- meta_predicate '$bags':findnsols_loop(*,*,0,*,*).
%:- meta_predicate '$bags':findall_loop(*,0,*,*).
:- meta_predicate '$attvar':unfreeze(0).
:- meta_predicate '$attvar':run_crv(0,*,*,*).
:- meta_predicate '$expand':expand_term_list(4,*,*,*,*).
%:- meta_predicate '$parms':cached_library_directory(*,0,*).
%:- meta_predicate '$toplevel':residue_vars(0,-).
:- meta_predicate '$toplevel':toplevel_call(0).
:- meta_predicate '$toplevel':run_initialize(0,*).
% :- meta_predicate '$toplevel':run_init_goal(0,*).
% :- meta_predicate '$attvar':uhook(*,0,*,*).
% :- meta_predicate '$attvar':uhook(*,0,*).
%:- meta_predicate '$toplevel':'$execute_goal2'(0,*).
*/




% :- initialization(attach_packs,now).

% ==============================================
% Updates for Packs
% ==============================================
:- export(logicmoo_update/0).

teamspoon_pack(Pack):-
  call((user:use_module(library(prolog_pack)))),
  pack_property(Pack,home(Home)),once(sub_string(Home, _, _, _, 'github.com/logicmoo')).

logicmoo_update:-  call((user:use_module(library(prolog_pack)))),
    forall(teamspoon_pack(Pack),wemsg(warning,maybe_pack_upgrade(Pack))).
/*
pack_upgrade_wrong:- call((user:use_module(library(prolog_pack)),use_module(library(predicate_streams)),
  call(call,
   with_input_from_predicate(({}/[X]>>(repeat,X='YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY')))),
    forall(call(call,prolog_pack:current_pack,Pack),maybe_pack_upgrade(Pack)))).
*/
maybe_pack_upgrade(Pack):- pack_property(Pack, directory(PackDir)),\+ access_file(PackDir,write),!.
maybe_pack_upgrade(Pack):- pack_upgrade_soft(Pack).


%!  pack_upgrade_soft(+Pack) is semidet.
%
%   Try to upgrade the package Pack.
%
%   @tbd    Update dependencies when updating a pack from git?

which_pack(Pack,Pack):- pack_property(Pack,_),!.
which_pack(Dir,Pack):-  prolog_pack:pack_info(Pack, _, directory(Dir)).
which_pack(Pack,Pack):- prolog_pack:pack_info(Pack, _, _).
which_pack(Lib,Pack):-  pack_property(Pack,library(Lib)).
which_pack(Home,Pack):- pack_property(Pack,home(Home)).


update_packs:- !.
update_packs:-
   use_module(library(prolog_pack)),
   (pack_property(prologmud_samples,version(Version));
    pack_property(pfc,version(Version))),!,
   use_module(library(git)),
   forall(
   (pack_property(Pack,version(Version)), pack_property(Pack,directory(Dir)),
      directory_file_path(Dir, '.git', GitDir),
      %(exists_file(GitDir);exists_directory(GitDir)),
       access_file(GitDir,read),
       access_file(GitDir,write)),
     ( print_message(informational, pack(git_fetch(Dir))),
     git([fetch], [ directory(Dir) ]),
     git_describe(V0, [ directory(Dir) ]),
     git_describe(V1, [ directory(Dir), commit('origin/master') ]),
     (   V0 == V1
     ->  print_message(informational, pack(up_to_date(Pack)))
     ;   true,
         git([merge, 'origin/master'], [ directory(Dir) ]),
         pack_rebuild(Pack)
     ))),
   initialization(attach_packs,now).

% :- update_packs.
:- system:use_module(library(prolog_pack)).
:- export(pack_upgrade_soft/1).
/*
pack_upgrade_soft(Which) :- which_pack(Which,Pack)-> Which\==Pack,!, pack_upgrade_soft(Pack).
pack_upgrade_soft(Pack) :-
  prolog_pack:(
    (pack_info(Pack, _, directory(Dir));pack_property(Pack, directory(Dir))),
    directory_file_path(Dir, '.git', GitDir),
    exists_directory(GitDir),
    !,
    print_message(informational, pack(git_fetch(Dir))),
    git([fetch], [ directory(Dir) ]),
    git_describe(V0, [ directory(Dir) ]),
    git_describe(V1, [ directory(Dir), commit('origin/master') ]),
    (   V0 == V1
    ->  print_message(informational, pack(up_to_date(Pack)))
    ;   nop(confirm(upgrade(Pack, V0, V1), yes, [])),
        git([merge, 'origin/master'], [ directory(Dir) ]),
        pack_rebuild(Pack)
    )).
pack_upgrade_soft(Pack) :-
 prolog_pack:(
    once(pack_info(Pack, _, version(VersionAtom))),
    atom_version(VersionAtom, Version),
    pack_info(Pack, _, download(URL)),
    (   wildcard_pattern(URL)
    ->  true
    ;   github_url(URL, _User, _Repo)
    ),
    !,
    available_download_versions(URL, [Latest-LatestURL|_Versions]),
    (   Latest @> Version
    ->  nop(confirm(upgrade(Pack, Version, Latest), yes, [])),
        pack_install(Pack,
                     [ url(LatestURL),
                       upgrade(true),
                       interactive(false),
                       pack(Pack)
                     ])
    ;   print_message(informational, pack(up_to_date(Pack)))
    )).
*/

pack_upgrade_soft(Pack) :-
    print_message(warning, pack(no_upgrade_info(Pack))).


:- export(pack_upgrade_soft/0).
pack_upgrade_soft :- pack_upgrade_soft(pfc), pack_upgrade_soft(logicmoo_utils), pack_upgrade_soft(dictoo),
  forall(pack_property(X,version(_)),pack_upgrade_soft(X)).
:- system:import(pack_upgrade_soft/0).


fix_deps(Pack-_Why):-
  pack_install(Pack,[interactive(false)]),
  %pack_info(Pack),
  !.

correct_unsatisfied_dependencies:-
  call(call,prolog_pack:unsatisfied_dependencies(List)),reverse(List,R),maplist(fix_deps,R).
correct_unsatisfied_dependencies:-!.

ensure_this_pack_installed_correctly:-
  % pack_upgrade(logicmoo_utils),
  % pack_install('https://github.com/logicmoo/predicate_streams.git',[silent(true),git(true),interactive(false)]),
  pack_install(predicate_streams,[interactive(false)]),
  pack_install(gvar_syntax,[interactive(false)]),
  pack_install(dictoo,[interactive(false)]),
  pack_list_installed,
  correct_unsatisfied_dependencies,
  !.

ensure_this_pack_installed:- exists_source(library(debuggery/first)),!.
ensure_this_pack_installed:-
  prolog_load_context(directory,Here),
  absolute_file_name('../../',PackDir,[relative_to(Here),file_type(directory)]),
  attach_packs(PackDir),
  exists_source(library(debuggery/first)),!,
  ensure_this_pack_installed_correctly.

:- ensure_this_pack_installed.

ensure_logicmoo_pack_install(X):- pack_property(X,version(_)),!.
ensure_logicmoo_pack_install(X):- atomic_list_concat(['https://github.com/logicmoo/',X,'.git'],URL),pack_install(URL,[interactive(false)]).
install_logicmoo:-
  use_module(library(prolog_pack)),
  ensure_this_pack_installed,
  maplist(ensure_logicmoo_pack_install,[
    body_reordering,lps_corner,predicate_streams,eggdrop,pfc,logicmoo_ec,gvar_syntax,logicmoo_base,
    dictoo,logicmoo_webui,logicmoo_utils,prologmud_samples,instant_prolog_docs,
    logicmoo_cg,prologmud,wam_common_lisp,narsese,
    multimodal_dcg,logicmoo_nlu]),
  pack_list_installed,
  correct_unsatisfied_dependencies,
  pack_list_installed,
  !.

% :- pack_list_installed.



:-system:use_module(library(filesex)).
:-system:use_module(library(qsave)).

qsave_bin(_):- current_prolog_flag(logicmoo_compiling,mud_server),!.
qsave_bin(_):- current_prolog_flag(logicmoo_compiling,done),!.
qsave_bin(_):- current_prolog_flag(os_argv,List), \+ member('-t',List), !.
qsave_bin(Clif):- current_prolog_flag(logicmoo_compiling,Clif),!.

qsave_bin(Clif):-
  (current_prolog_flag(logicmoo_compiling,Was);Was=false),
  setup_call_cleanup(
    set_prolog_flag(logicmoo_compiling,Clif),
    qsave_bin_now(Clif),
    set_prolog_flag(logicmoo_compiling,Was)).

qsave_bin_now(Clif):-
  atom_concat('lmoo-',Clif,Lmoo),
  directory_file_path('bin',Lmoo,Bin),
  getenv('LOGICMOO_WS',Dir),
  directory_file_path(Dir,Bin,Path),
  writeln(qsave_bin(Clif)=Path),
  current_prolog_flag(stack_limit,Stack_limit),
  qsave_program(Path,
    [ class(development),
      % verbose(true),
      stack_limit(Stack_limit),
      toplevel(prolog),
      goal(true),
      undefined(ignore),
      op(save),
      % map('logicmoo_server.map'),
      foreign(no_save),
      autoload(true),
      stand_alone(false)]).


%:- system:use_module(library(logicmoo/each_call)).

%:- system:use_module(library(debuggery/wemsg)).
%:- system:use_module(library(must_sanity)).

% ( GFE = Girl-Friend Experience )


%=======================================

%=======================================
:- user:use_module(library(http/term_html)).
:- system:use_module(library(http/http_session)).
:- system:use_module(library(apply)).
:- system:use_module(library(dcg/basics)).
:- system:use_module(library(debug)).
:- system:use_module(library(error)).
:- system:use_module(library(filesex)).
:- system:use_module(library(http/html_head)).
:- user:use_module(library(http/html_write)).
:- system:use_module(library(http/http_dispatch)).
:- system:use_module(library(http/http_path)).
:- system:use_module(library(http/http_wrapper)).
:- system:use_module(library(http/jquery)).
:- system:use_module(library(http/js_grammar)).
:- system:use_module(library(http/js_write)).
:- system:use_module(library(http/mimepack)).
:- system:use_module(library(http/mimetype)).
:- system:use_module(library(http/thread_httpd)).
:- system:use_module(library(http/websocket)).
:- system:use_module(library(http/yadis)).
:- user:use_module(library(http/json)).
:- user:use_module(library(http/json_convert)).
:- system:use_module(library(listing)).
%:- system:use_module(library(lists)).
:- system:use_module(library(occurs)).
:- system:use_module(library(option)).
:- system:use_module(library(pairs)).
:- system:use_module(library(prolog_source)).
:- system:use_module(library(prolog_stack)).
:- system:use_module(library(prolog_xref)).
:- system:use_module(library(pure_input)).     % syntax_error//1
:- system:use_module(library(readutil)).
:- system:use_module(library(solution_sequences)).
:- system:use_module(library(time)).
:- system:use_module(library(uri)).

:- user:use_module(library(pldoc)).
:- user:use_module(library(pldoc/doc_html)).
/*
:- system:use_module(library(pldoc/doc_index)).
:- system:use_module(library(pldoc/doc_man)).
:- system:use_module(library(pldoc/doc_modes)).
:- system:use_module(library(pldoc/doc_process)).
:- system:use_module(library(pldoc/doc_search)).
:- system:use_module(library(pldoc/doc_util)).
:- system:use_module(library(pldoc/doc_wiki)).
*/
%:- system:use_module(library(debuggery/ucatch)).

%:- system:reexport(library(debuggery/ucatch)).
%:- system:reexport(library(debuggery/first)).
:- system:reexport(library(logicmoo/util_strings)).
%:- system:reexport(library(debuggery/wemsg)).
:- system:reexport(library(debuggery/rtrace)).
%:- system:reexport(library(debuggery/dumpst)).
:- system:reexport(library(debuggery/frames)).
:- system:reexport(library(logicmoo/predicate_inheritance)).

:- system:reexport(library(xlisting)).

:- system:reexport(library(logicmoo/call_from_module)).
:- system:reexport(library(hook_database)).
%:- system:reexport(library(must_sanity)).
:- system:reexport(library(logicmoo/filesystem)).

:- system:reexport(library(logicmoo/misc_terms)).
:- system:reexport(library(logicmoo/lockable_vars)).
%:- system:reexport(library(logicmoo/portray_vars)).
:- system:reexport(library(logicmoo/util_varnames)).

:- system:reexport(library(logicmoo/each_call)).
:- system:reexport(library(logicmoo/redo_locally)).
:- system:reexport(library(logicmoo/no_loops)).
:- system:reexport(library(logicmoo/no_repeats)).
:- system:reexport(library(logicmoo/subclause_expansion)).


:- system:reexport(library(file_scope)).
:- system:reexport(library(logicmoo/clause_attvars)).
:- system:reexport(library(logicmoo/with_no_x)).
:- system:reexport(library(logicmoo/filestreams)).
:- system:reexport(library(logicmoo/filesystem)).
:- system:reexport(library(logicmoo/call_reorder)).
:- system:reexport(library(logicmoo/nb_set_term)).
:- system:reexport(library(logicmoo/pretty_clauses)).

:- system:reexport(library(logicmoo/dcg_meta)).
:- system:reexport(library(logicmoo/util_bb_frame)).

/*
:- system:reexport(library(logicmoo/virtualize_source)).
:- system:reexport(library(script_files)).

%:- user:reexport(library(logicmoo/retry_undefined)).




*/
:- export(same_streams/2).
wemsg(D):- format(user_error,"~N; ~q.~n",[D]).
wemsg(W,D):- format(user_error,"~N; ~q.~n",[W=D]).
%=======================================
%= REGISTER FOR INIT EVENTS
%=======================================

% These are mainly so we can later understand the restore phasing
/*
:- initialization(nop(wemsg(init_phase(program))),program).
:- initialization((wemsg(init_phase(after_load))),after_load).
:- initialization(nop(wemsg(init_phase(restore))),restore).
:- initialization((wemsg(init_phase(restore_state))),restore_state).
:- initialization(nop(wemsg(init_phase(prepare_state))),prepare_state).
*/

%= Register a hook after restore
:- initialization(nop(init_why(during_boot,restore)),restore).
%= Register a hook
:- initialization(init_why(after_boot,program),program).
%= Register a hook
%:- initialization(init_why(runtime,main),main).

:- now_and_later(ignore(load_history)).
:- fixup_exports.


:- if(false).
:- multifile(user:term_expansion/2).
:- dynamic(user:term_expansion/2).

% basically only run if is in 'user'
user:term_expansion(EOF,_):- EOF == end_of_file, prolog_load_context(source,File),prolog_load_context(file,File),
  prolog_load_context(module,SourceModule), '$current_typein_module'(TypeIn),
  wemsg(info,File : '?='(SourceModule , TypeIn)),
  SourceModule == TypeIn,
  run_pending_inits, fail.

:- endif.

