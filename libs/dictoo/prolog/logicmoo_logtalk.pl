:- module(logicmoo_logtalk,[ensure_LOGTALKUSER/0,load_logtalk/0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% LOAD LOGTALK
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logtalk_home(LTH):- getenv('LOGTALKHOME',LTH),!.
logtalk_home(LTH):- absolute_directory(pack(logtalk), Directory0),
  atom_concat(Directory0,'/logtalk-*',Directory1),
  expand_file_name(Directory1,[LTH]),!.

skip_logtalk:- app_argv('--nologtalk'),!.
skip_logtalk:- \+ app_argv1('--logtalk'),!.
skip_logtalk:- \+ logtalk_home(_), !.
skip_logtalk:- logtalk_home(LTH), \+ exists_directory(LTH),!.

had_LOGTALKUSER :- getenv('LOGTALKUSER', _Location),getenv('LOGTALKHOME', _Location2).

ensure_LOGTALKUSER:- had_LOGTALKUSER,!.
ensure_LOGTALKUSER:- skip_logtalk, !, dmsg("Skipping logtalk").
% ensure_LOGTALKUSER:- user:use_module(library(logtalk)).
ensure_LOGTALKUSER:- logtalk_home(LTH),
   setenv('LOGTALKHOME', LTH),
   setenv('LOGTALKUSER', LTH),!.

:- multifile(logtalk:'$lgt_current_engine_'/4).
:- volatile(logtalk:'$lgt_current_engine_'/4).
load_logtalk(system):- load_logtalk('/usr/share/logtalk').
load_logtalk(LTH):- atom_concat(LTH,'/integration/logtalk_swi',Init),
  exists_source(Init),!,logtalk:ensure_loaded(Init),!,listing(logtalk:'$lgt_default_flag'/2).
load_logtalk(LTH):-  dmsg("Skipping logtalk="+LTH).

load_logtalk:- current_predicate(logtalk:'$lgt_default_flag'/2).
load_logtalk:- skip_logtalk, !, dmsg("Skipping logtalk").
load_logtalk:- logtalk_home(LTH), \+ exists_directory(LTH),!,dmsg("Skipping logtalk").
load_logtalk:- had_LOGTALKUSER,!,
   dmsg("Installing logtalk"),
   load_logtalk(system).
load_logtalk:- ensure_LOGTALKUSER,
   logtalk_home(LTH),
   dmsg("Logtalk installed"=LTH),
   load_logtalk(LTH).

:- dmsg("Loading logtalk").
:- before_boot(ensure_LOGTALKUSER).
:- before_boot(load_logtalk).

% :- if( (( \+ prolog_load_context(reload,true) ))).

:- module_transparent(logtalk:'::'/1).
:- logtalk:export(logtalk:'::'/1).
:- user:import(logtalk:'::'/1).

:- module_transparent(logtalk:'::'/2).
:- logtalk:export(logtalk:'::'/2).
:- user:import(logtalk:'::'/2).
:- baseKB:import(logtalk:'::'/2).

%user:'::'(X,Y):- logtalk:'::'(X,Y).
%user:'::'(X):- logtalk:'::'(X).

:-op(200,fy,user:'--').
:-op(600,fy,user:'::').
:-op(600,xfy,user:'::').
:-op(200,fy,user:'++').
:-op(600,fy,user:'^^').


% :- endif.
:- fixup_exports.


