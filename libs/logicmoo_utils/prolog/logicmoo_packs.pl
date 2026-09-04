%#!/usr/bin/swipl 

:- module(logicmoo_packages,[ rescan_pack_autoload_packages/0 ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PACK LOADER
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- use_module(library(prolog_pack)).

:- if( \+ prolog_pack:current_pack(logicmoo_base)).

:- attach_packs('/opt/logicmoo_workspace/packs_sys',[duplicate(keep)]).

:- multifile(user:file_search_path/2).
:-   dynamic(user:file_search_path/2).
:- prolog_load_context(directory,Dir),
   absolute_file_name('../../',Y,[relative_to(Dir),file_type(directory)]),
   (( \+ user:file_search_path(pack,Y)) ->asserta(user:file_search_path(pack,Y));true).
:- initialization(attach_packs,now).
:- pack_list_installed.
:- endif.


         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- dmsg("AUTOLOAD PACKAGES").
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
pack_autoload_packages(NeedExistingIndex):- 
 forall(user:expand_file_search_path(library(''),Dir),
  ignore(( (\+ NeedExistingIndex ; absolute_file_name('INDEX',_Absolute,[relative_to(Dir),access(read),file_type(prolog),file_errors(fail)]))->
   make_library_index(Dir, ['*.pl']) -> 
  (user:library_directory(Dir) -> true ; (asserta(user:library_directory(Dir)) , reload_library_index))))).

:- before_boot(pack_autoload_packages(true)).
*/


rescan_pack_autoload_packages:- \+ access_file('.',write),dmsg("READONLY PACKAGES"),!.
rescan_pack_autoload_packages:- \+ app_argv('--all'),!.
rescan_pack_autoload_packages:- dmsg("AUTOLOADING PACKAGES..."),
 forall('$pack':pack(Pack, _),
  forall(((pack_property(Pack, directory(PackDir)),prolog_pack:pack_info_term(PackDir,autoload(true)))),
  (access_file(PackDir,write) -> prolog_pack:post_install_autoload(PackDir, [autoload(true)]) ; true))),
 dmsg(".. AUTOLOADING COMPLETE"),!.

:- before_boot(rescan_pack_autoload_packages).
:- during_boot(rescan_pack_autoload_packages).


%:- reload_library_index.
%:- autoload([verbose(true)]).
:- reload_library_index.


