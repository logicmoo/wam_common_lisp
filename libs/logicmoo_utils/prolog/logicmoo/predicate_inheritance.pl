/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- module(predicate_inheritance,
          [
          %  /*ex*/predicate_property/2,
check_mfa/4,
make_as_dynamic_now/4,
with_no_retry_undefined/1,
user_exception_undefined_predicate/5,
undefined_predicate_exception/4,
%skip_mfa/4,
export_everywhere/3,
create_predicate_inheritance/4,
now_inherit_above/4,
decl_as/2,
decl_kb_global/3,
never_move/2,

decl_kb_shared/3,
decl_kb_local/3,
decl_kb_type/4,
predicate_m_f_a_decl/4,
do_import/4,
(kb_local)/1,
(kb_global)/1,
(kb_local)/1,
%make_as_dynamic/4,
(kb_shared)/1
]).

:- set_module(class(library)).
% % % OFF :- system:reexport(library(must_sanity)).
% % % OFF :- system:reexport(library(logicmoo/no_loops)).

:- create_prolog_flag(retry_undefined, none,[type(term),keep(true)]).

:- use_module(library(logicmoo/no_loops),[is_parent_goal/1,is_parent_goal/2, lco_goal_expansion/2]).

:- meta_predicate decl_as(*,+).
:- meta_predicate decl_as_rev(+,*).


:- if( \+ current_op(_,_,(kb_global))).

:- current_prolog_flag(access_level,Was),
   set_prolog_flag(access_level,system),
   op(1150,fx,(kb_global)),
   op(1150,fx,(kb_global)),
   op(1150,fx,(kb_local)),
   set_prolog_flag(access_level,Was).

:- endif.


:- module_transparent((
check_mfa/4, 
%skip_mfa/4,
create_predicate_inheritance/4,
now_inherit_above/4,
decl_as/2,
decl_az/2,
decl_as/4,
decl_az/4,
do_import/4,
(kb_local)/1,
(kb_global)/1,
(kb_shared)/1,
make_as_dynamic/4
          )).

:- export((
check_mfa/4, 
%skip_mfa/4,
create_predicate_inheritance/4,
now_inherit_above/4,
decl_as/2,
decl_az/2,
decl_as/4,
decl_az/4,
do_import/4,
(kb_local)/1,
(kb_global)/1,
(kb_shared)/1,
make_as_dynamic/4
          )).

/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- export((	% uses_predicate/3,
         uses_undefined_hook/0,
         install_retry_undefined/2
         % uses_predicate/5,
         %     retry_undefined/3,
         
         
         
         
)).

:- thread_local(ru_l:was_prolog_flag/1).
:- current_prolog_flag(retry_undefined,Was)->asserta(ru_l:was_prolog_flag(retry_undefined,Was));asserta(ru_l:was_prolog_flag(retry_undefined,none)).
% WAS OFF  :- system:use_module(library(loop_check)).% WAS OFF  :- system:use_module(library(bugger)).
% % % OFF :- system:use_module(library(hook_database)).


:- module_transparent((	
   uses_predicate/2,
   uses_undefined_hook/0,				
   uses_predicate/5,
   chk_retry_undefined/3,
   
   install_retry_undefined/2,
   
   
   get_retry_undefined_hook/2)).

:- dynamic(ru:retry_undefined_hook/2).

get_retry_undefined_hook(X,Y):- ru:retry_undefined_hook(X,Y).
get_retry_undefined_hook(_M,Was):- current_prolog_flag(retry_undefined, Was), Was\==module.

install_retry_undefined(Module,Setting):- asserta((ru:retry_undefined_hook(Module,Was):-!,Was=Setting)).

:- install_retry_undefined('$toplevel',error).
:- install_retry_undefined('user',error).

uses_undefined_hook.



 :- meta_predicate uses_predicate(*,*,*,*,*,*).
 :- meta_predicate uses_predicate(*,*,*,*,*,*).
 :- meta_predicate uses_predicate(1,*,*,*,*,*).


dumpST_dbreak:- dumpST,break.

% baseKBOnly mark_mark/3 must be findable from every module (dispite the fact that baseKB is not imported)
% :- dynamic baseKB:mpred_prop/4.

% hybrid_support (like '$spft'/4) must be defined directly in every module and then aggregated thru genlMts (thus to baseKB)
/*
is_parent_goal(G):- prolog_current_frame(F),is_parent_goal(F,G).
% The user must ensure the checked parent goal is not removed from the stack due 
% to last-call optimisation 
is_parent_goal(F,G):- nonvar(G),prolog_frame_attribute(F,parent_goal, G).
%and be aware of the slow operation on deeply nested calls.
is_parent_goal(F,G):- prolog_frame_attribute(F,parent,P),parent_frame_goal(P,G).

parent_frame_goal(F,V):- parent_frame_goal_0(F,V0),contains_goalf(V0,V).
parent_frame_goal_0(F,V):- prolog_frame_attribute(F,goal,V);
   (prolog_frame_attribute(F,parent,P),parent_frame_goal_0(P,V)).

contains_goalf(V0,V):- nonvar(V),same_goalf(V0,V),!.
contains_goalf(V0,_):- \+ compound(V0),!,fail.
contains_goalf(V0,V):- var(V),same_goalf(V0,V).
contains_goalf(_:V0,V):- !, contains_goalf(V0,V).
contains_goalf('$execute_directive_3'(V0),V):-!, same_goalf(V0,V).
contains_goalf('<meta-call>'(V0),V):-!, same_goalf(V0,V).
contains_goalf(catch(V0,_,_),V):- same_goalf(V0,V).
contains_goalf(catch(_,_,V0),V):- same_goalf(V0,V).
same_goalf(V,V).
*/

is_in_define:- prolog_current_frame(F), 
 (is_parent_goal(F,'$define_predicate'(_));
  is_parent_goal(F,_:'assert_u'(_));
  is_parent_goal(check:check);
  is_parent_goal(F,'$syspreds':property_predicate(_,_))),!.

% make sure we ignore calls to predicate_property/2  (or thus '$define_predicate'/1)
uses_predicate(_DEF,_,_,_,_,Error):- is_in_define,!,is_in_define, error = Error.

uses_predicate(_Was,_CM,_M,_F,_A,Error):- is_parent_goal(check:check),!,Error=error.
uses_predicate(_Was,_CM,_M,_F,_A,Error):- show_success(is_parent_goal('$define_predicate')),!,Error=error.

uses_predicate(_Was,_CM,'$toplevel',_F,_A,Error):- !,Error=error.

uses_predicate(_DEF,_, _, ~, 1, error) :- !.
uses_predicate(_DEF,_,CallerMt,'$pldoc',4,retry):- make_as_dynamic(uses_predicate,CallerMt,'$pldoc',4),!.
uses_predicate(_DEF,User, User, module, 2, error):-!.
uses_predicate(_DEF,_,_, (:-), _, error) :- !, fail. 
uses_predicate(_DEF,_,_, (/), _, error) :- !. 
uses_predicate(_DEF,_,_, ( '//' ), _, error) :- !. 
uses_predicate(_DEF,_,_, F, _, error) :- atom_concat('__',_,F),!.
uses_predicate(_DEF,_,_, F, _, error) :- atom_concat('$',_,F),!.

uses_predicate(_DEF,_,_, (:), _, error) :- !. % ,dumpST_dbreak.
% uses_predicate(_DEF,_,_, '[|]', _, error) :- !,dumpST_dbreak.
% uses_predicate(_DEF,_,_, '>>',  4, error) :- !,dumpST_dbreak.
% uses_predicate(_DEF,_,M, inherit_above,_,retry):- M:use_module(library(virtualize_source)).

% makes sure we ignore calls to predicate_property/2  (or thus '$define_predicate'/1)
% uses_predicate(_DEF,_,M,F,A,R):- prolog_current_frame(FR), functor(P,F,A),(prolog_frame_attribute(FR,parent_goal,predicate_property(M:P,_))),!,R=error.
uses_predicate(_DEF,_,Module,Name,Arity,Action) :-
      current_prolog_flag(autoload, true),
	Module:'$autoload'(Module, Name, Arity), !,
	Action = retry.


uses_predicate(E,_,_,_,_,Error):- E=error, !,Error=error.
uses_predicate(fail,_,_,_,_,_):- !,fail.
uses_predicate(break,_,_,_,_,Error):- !,dumpST_dbreak,Error=error.

uses_predicate(Was,CM,M,F,A,Action):-  uses_predicate2(Was,CM,M,F,A,Action).

uses_predicate2(_DEF,_,System, _,_, error):- module_property(System,class(system)),!.
uses_predicate2(_DEF,_,System, _,_, error):- module_property(System,class(library)),!.

uses_predicate2(Setting,SM,M,F,A,Act):- Setting\== (kb_shared), SM\==user, M\==baseKB,
     dmsg(uses_predicate(Setting,SM,M,F,A,Act)),fail.

uses_predicate2(kb_shared,System, M, F,A, retry):-   
   show_failure(uses_undefined_hook(M)),
   create_predicate_inheritance(kb_shared(M:F/A),M,F,A),
   nop(System:import(M:F/A)),!.

% uses_predicate(true,_, M,F,A, Retry):-  chk_retry_undefined(M,F,A),!,Retry=retry.

uses_predicate2(_,_, M,F,A, Retry):-  chk_retry_undefined(M,F,A),!,Retry=retry.

uses_predicate2(DEF,_, M, F,A, Retry):-  call(DEF, M:F/A),!,Retry=retry.

:- if(\+ current_predicate(autoload_library_index/4)).
in_autoload_library_index(F,A,_PredMt,File):- '$in_library'(F,A,File).
:- else.
in_autoload_library_index(F,A,PredMt,File):- autoload_library_index(F,A,PredMt,File).
:- endif.

:- module_transparent(with_no_retry_undefined/1).
/*with_no_retry_undefined(Goal):- setup_call_cleanup(current_prolog_flag(retry_undefined, Was),
                                     locally(set_prolog_flag(runtime_debug,0),
                                      (set_prolog_flag(retry_undefined, none),Goal)),
                                     set_prolog_flag(retry_undefined, Was)),!.
*/
with_no_retry_undefined(Goal):- locally(set_prolog_flag(retry_undefined, none),
                                     locally(set_prolog_flag(runtime_debug,0),Goal)).


% Every module has it''s own
chk_retry_undefined(CallerMt,'$pldoc',4):- multifile(CallerMt:'$pldoc'/4),discontiguous(CallerMt:'$pldoc'/4),
 dynamic(CallerMt:'$pldoc'/4),!.
% System-like Autoloads (TODO: confirm these can be removed)
chk_retry_undefined(CallerMt,debug,1):- CallerMt:use_module(CallerMt:library(debug)),!.
chk_retry_undefined(CallerMt,debugging,1):- CallerMt:use_module(CallerMt:library(debug)),!.
chk_retry_undefined(CallerMt,member,2):- CallerMt:use_module(CallerMt:library(lists)),!.
chk_retry_undefined(CallerMt,directory_file_path,3):- CallerMt:use_module(CallerMt:library(filesex)),!.
% 3 very special Mts
% Module defines the type
% chk_retry_undefined(baseKB,F,A):- make_as_dynamic(chk_retry_undefined(baseKB),baseKB,F,A),!.
chk_retry_undefined(lmcache,F,A):- volatile(lmcache:F/A),make_as_dynamic(chk_retry_undefined(lmcache),lmcache,F,A),!.
chk_retry_undefined(t_l,F,A):- thread_local(t_l:F/A),!,make_as_dynamic(chk_retry_undefined(t_l),t_l,F,A),!.


:- if(false).

% adult-like Mt
retry_undefined_falsed_out(Mt, F, A):-  clause_b(mtCycLBroad(Mt)), clause_b(hybrid_support(F,A)),
   make_as_dynamic(mtCycLBroad(Mt),Mt,F,A).

% child-like Mt
retry_undefined_falsed_out(CallerMt,F,A):- clause_b(mtGlobal(CallerMt)), clause_b(hybrid_support(F,A)),
   % find_and_call(baseKB:mtGlobal(CallerMt)),
   create_predicate_inheritance(retry_undefined_falsed_out(CallerMt:F/A),CallerMt,F,A).

% import built-ins ?
retry_undefined_falsed_out(CallerMt,F,A):- current_predicate(system:F/A), current_module(M),M\=system,
  current_predicate(M:F/A),functor(P,F,A),predicate_property(M:P,defined),\+predicate_property(M:P,imported_from(_)),
  CallerMt:import(M:F/A).

% our autoloader hacks
retry_undefined_falsed_out(CallerMt,F,A):-
   in_autoload_library_index(F,A,_PredMt,File),
   use_module(CallerMt:File),!.

% Autoloads importing the entire other module
retry_undefined_falsed_out(CallerMt,F,A):- fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,
       in_autoload_library_index(F,A,PredMt,File),
       asserta(lmcache:how_registered_pred(PredMt:use_module(CallerMt:File),CallerMt,F,A)),
       use_module(system:File),!.
       % system:add_import_module(CallerMt,system,start).


retry_undefined_falsed_out(CallerMt,F,A):- fail,
       in_autoload_library_index(F,A,_,File),
       load_files(CallerMt:File,[if(true),imports([F/A]),register(false),silent(false)]),!.

% Autoloads importing the entire other module
retry_undefined_falsed_out(CallerMt,F,A):- fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,
       in_autoload_library_index(F,A,PredMt,File),
       asserta(lmcache:how_registered_pred(PredMt:use_module(CallerMt:File),CallerMt,F,A)),
       use_module(CallerMt:File),!.

/*
chk_retry_undefined(CallerMt,F,A):-
      in_autoload_library_index(F,A,PredMt,File),
      ((current_module(PredMt),current_predicate(PredMt:F/A))
       -> add_import_module(CallerMt,PredMt,start) ;
       (PredMt:ensure_loaded(PredMt:File),add_import_module(CallerMt,PredMt,start))),!.
*/

retry_undefined_falsed_out(CallerMt,F,A):- fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,fail,
   functor(P,F,A),find_module(P,M),show_call(CallerMt:import(M:F/A)),!.



%chk_retry_undefined(PredMt:must/1) % UNDO % :- add_import_module(PredMt,logicmoo_util_catch,start),!.
%chk_retry_undefined(PredMt:debugm/2) % UNDO % :- add_import_module(PredMt,logicmoo_util_dmsg,start),!.

:- endif.



%uses_undefined_hook(CM):- (clause_b(genlMt(CM,_));clause_b(mtHybrid(CM))).
uses_undefined_hook(CM):- nonvar(CM),clause(mtNoInheritance(CM),true),!,fail.
uses_undefined_hook(CM):- clause_b(genlMt(CM,_)),!.
% uses_undefined_hook(CM):- is_pfc_module(CM),!.
uses_undefined_hook(baseKB).
%uses_undefined_hook(user).




%% create_predicate_inheritance(+Reason,+ChildDefMt,+F,+A) is semidet.
%
% Ensure inherit_above/2 stub is present in ChildDefMt.
%

%create_predicate_inheritance(Reason,CallerMt,F,A):- clause_b((baseKB:mpred_prop(CallerMt,F,A,inherits_above))),!.
%create_predicate_inheritance(Reason,CallerMt,F,A):- assert_if_new((baseKB:mpred_prop(CallerMt,F,A,inherits_above))),fail.

create_predicate_inheritance(_Reason,CallerMt,F,A):- lmcache:already_decl(kb_global,M,F,A),!,CallerMt:import(M:F/A).
%create_predicate_inheritance(Reason,_,F,A):- lmcache:already_decl(kb_shared,_,F,A),!.
%create_predicate_inheritance(Reason,M,F,A):- show_success(lmcache:already_decl(kb_local,M,F,A)),!.
%create_predicate_inheritance(Reason,M,F,A):- show_success(lmcache:already_decl(kb_shared,M,F,A)),!.

create_predicate_inheritance(Reason,CallerMt,F,A):- now_inherit_above(Reason,CallerMt,F,A),!.



% TODO unsuspect the next line (nothing needs to see above baseKB)

%% now_inherit_above(+Reason,+ChildDefMt,+F,+A) is semidet.
%
% Ensure now_inherit_above/2 stub is present in ChildDefMt.
%
now_inherit_above(_Reason,Nonvar,F,A):- var(Nonvar)-> break ; (sanity(ground(now_inheritance(Nonvar,F,A))),fail).
now_inherit_above(Reason,baseKB,F,A):- !, make_as_dynamic(Reason,baseKB,F,A).
/*
now_inherit_above(_Reason,baseKB,F,A):- !,
  make_as_dynamic(now_inherit_above(_Reason,baseKB,F,A),baseKB,F,A), 
     ignore((( \+ (defaultAssertMt(CallerMt),CallerMt\==baseKB,now_inherit_above(_Reason,CallerMt,F,A) )))).
*/
now_inherit_above(Reason,abox,F,A):-  
       !, must(call(call,defaultAssertMt(CallerMt))),
       sanity(CallerMt\=abox),!,
       now_inherit_above(Reason,CallerMt,F,A).

now_inherit_above(Reason,CallerMt,F,A):- fail, clause_b(mtProlog(CallerMt)),
   sanity(\+ clause_b(mtHybrid(CallerMt))),!,
   wdmsg(warn(create_predicate_istAbove_mtProlog(Reason,CallerMt,F,A))),dtrace.

now_inherit_above(_Reason,CallerMt,F,A):- 
  lmcache:already_decl(kb_global,M,F,A),do_import(CallerMt,M,F,A),!.

now_inherit_above(Reason,CallerMt,F,A):-
   make_as_dynamic(Reason,CallerMt,F,A),
   functor(Goal,F,A),
   CallerMt:import(inherit_above/2),
   CallerMt:import(do_ihherit_above/2),
   predicate_inheritance:get_inherit_above_clause(CallerMt,Goal,Head,Body),
   CallerMt:assertz_new(Head:-Body).

% get_inherit_above_clause(CallerMt,Goal,Head,Body)

system:get_inherit_above_clause(From,Goal,IAHead,IABody):- 
   (nonvar(Goal)->(strip_module(Goal,_,Call), functor(Call,F,A),functor(Head,F,A)) ; Goal=Head),
   IAHead = From:Head,   
   IABody = (zwc,inherit_above(From,Head)),
   nop(wdmsg(IABody)).

%awc:-true.
%zwc:-true.

:- module_transparent(system:inherit_above/2).
:- export(system:inherit_above/2).
system:inherit_above(Mt,Query):- 
   % \+ context_module(baseKB), 
   Mt\=baseKB, 
   Query\=do_inherit_above(_,_),
   do_inherit_above(Mt,Query).

:- export(never_move/2).
:- public(never_move/2).
never_move('$spft',_).
never_move(mpred_prop,_).
never_move(meta_argtypes,_).
never_move('$pt',_).
never_move('$bt',_).
never_move(tc,_).
never_move(proven_tru,_).
never_move(is_pfc_file,_):- dumpST,break.

%never_move(_,_).

:- module_transparent(system:do_inherit_above/2).
:- export(system:do_inherit_above/2).
:- thread_local(t_l:exact_kb/1).
system:do_inherit_above(Mt,_):- t_l:exact_kb(Mt),!,fail.
%system:do_inherit_above(user,G):- !, fail, baseKB:call(G).

system:do_inherit_above(_Mt,_QueryIn):- !, fail.

system:do_inherit_above(Mt,QueryIn):- 
   functor(QueryIn,F,A), ( \+ predicate_inheritance:never_move(F,A) ),
   predicate_property(QueryIn,number_of_clauses(N)),
   Mt:nth_clause(QueryIn,N,Ref),clause(_,Body,Ref),
   predicate_inheritance:get_inherit_above_clause(Mt,QueryIn,IAHead,IABody),
   Body\=IABody,
   once((Mt:clause(IAHead,IABody,Kill),
   erase(Kill),% functor(QueryIn,F,A),
   dmsg(moving_to_last_clause(Mt,IAHead):-IABody), % inherit_above(Mt,Query)
   Mt:assertz(IAHead:-IABody))),fail.
   

  % TODO   no_repeats(MtAbove,(clause(Mt:genlMt(Mt,MtAbove),true);clause(baseKB:genlMt(Mt,MtAbove),true))),

system:do_inherit_above(Mt,Query):- 
   clause(genlMt(Mt,MtAbove),true),
   MtAbove \= Mt,
   do_call_inherited(MtAbove,Query).

:- module_transparent(system:do_call_inherited/2).
:- export(system:do_call_inherited/2).
system:do_call_inherited(MtAbove,Query):- 
   \+ current_prolog_flag(retry_undefined,none),
   % use_inheritance(MtAbove),
   \+ current_predicate(_,MtAbove:Query),
   functor(Query,F,A) -> create_predicate_inheritance(do_call_inherited(MtAbove,Query),MtAbove,F,A) -> fail.

system:do_call_inherited(_Mt,_QueryIn):- !, fail.
system:do_call_inherited(MtAbove,Query):- !, on_x_debug(MtAbove:Query).
system:do_call_inherited(MtAbove,Query):- on_x_debug(call(call,ireq(MtAbove:Query))).
  

export_everywhere(M,F,A):- now_and_later(n,export_everywhere_now(M,F,A)).
export_everywhere_now(system,F,A):- !, system:export(system:F/A).
export_everywhere_now(user,F,A):- !,user:export(user:F/A),system:import(user:F/A),baseKB:import(user:F/A).
export_everywhere_now(baseKB,F,A):- !, baseKB:export(baseKB:F/A),system:import(baseKB:F/A),user:import(baseKB:F/A).
export_everywhere_now(M,F,A):- M:export(M:F/A),system:import(M:F/A),user:import(M:F/A),baseKB:import(M:F/A).

%make_as_dynamic(M,F,A):- make_as_dynamic(make_as_dynamic,M,F,A).

make_as_dynamic(Reason,M,F,A):- now_and_later(n,make_as_dynamic_now(Reason,M,F,A)).

make_as_dynamic_now(Reason,M,F,A):- Reason= kb_global(_),!,make_as_dynamic_really(Reason,M,F,A),export_everywhere(M,F,A).
make_as_dynamic_now(Reason,M,F,A):- Reason= kb_local(_),!,make_as_dynamic_really(Reason,M,F,A),!. 
make_as_dynamic_now(Reason,M,F,A):- Reason= decl_kb_type(_,_),!,make_as_dynamic_really(Reason,M,F,A),!. 
make_as_dynamic_now(Reason,M,F,A):- F== is_pfc_file, break, make_as_dynamic_really(Reason,M,F,A).
make_as_dynamic_now(Reason,M,F,A):- dmsg(make_as_dynamic(Reason,M,F,A)),!,make_as_dynamic_really(Reason,M,F,A),!. 

:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
%user:message_hook(import_private(Module, Private),_,_):- Module==system,!, nop(dmsg(import_private(Module, Private))).
%user:message_hook(import_private(Module, Private),_,_):- current_prolog_flag(runtime_message_hook, true), dmsg(import_private(Module, Private)).

make_as_dynamic_really(Reason,M,F,A):- functor(PI,F,A), make_as_dynamic_really(Reason,M,PI,F,A).


make_as_dynamic_really(Reason,M,PI,F,A):- 
  ignore((is_static_predicate(M:PI),really_remake_as_dynamic(Reason,M,PI,F,A))),
  ignore((is_static_predicate(M:PI),really_remake_as_dynamic_no_props(Reason,M,PI,F,A))),
  make_as_dynamic_really_two(Reason,M,PI,F,A).


make_as_dynamic_really_two(Reason,M,PI,F,A):- 
  notrace(catch((public(M:F/A),fail),_,true)),
  predicate_property(M:PI,imported_from(OM)),!,
  make_as_dynamic_really(Reason,OM,PI,F,A).

make_as_dynamic_really_two(Reason,M,PI,F,A):-
  notrace(catch((public(M:F/A),fail),_,true)),
  really_remake_as_dynamic_no_props(Reason,M,PI,F,A),
  make_as_dynamic_really_two(Reason,M,PI,F,A).

  
make_as_dynamic_really_two(Reason,M,PI,F,A):-
 must_det_l((   
   (is_static_predicate(M:PI) -> true ; (predicate_property(M:PI,dynamic) -> true ; must(M:dynamic(M:F/A)))),      
   public(M:F/A),
   nop(on_f_throw( (M:F/A)\== (baseKB:loaded_external_kbs/1))),   
   nop((is_static_predicate(M:PI) -> true ; (ignore(source_location(S,L)),assertz_if_new(( M:PI :- (fail,infoF(make_as_dynamic_really(Reason,S,L)))))))),
   M:module_transparent(M:F/A),
   M:discontiguous(M:F/A),
   M:multifile(M:F/A))).


do_inherit(_SM,_M,_F,_A).


/*
 * Copy all clauses whose head unifies Arg3 from module Arg1 to 
 * module Arg2 without deleting the original clauses.
 */   
copy_module_predicate(InpMod, OutMod, Head) :-
   copy_predicate_clauses(InpMod:Head, OutMod:Head).  % SWI-PL

copy_module_predicate_no_props(InpMod, OutMod, Head) :-
   copy_predicate_clauses_no_props(InpMod:Head, OutMod:Head).  % SWI-PL

copy_predicate_clauses_no_props(From, To) :-
        copy_predicate_clauses_too_head(From, MF:FromHead),
        copy_predicate_clauses_too_head(To, MT:ToHead),
        FromHead =.. [_|Args],
        ToHead =.. [_|Args],
        forall(clause(MF:FromHead, Body),
               assertz(MT:ToHead, Body)).

copy_predicate_clauses_too_head(From, M:Head) :-
        strip_module(From, M, Name/Arity),
        functor(Head, Name, Arity).



really_remake_as_dynamic_no_props(Reason,M,PI,F,A):- 
  predicate_property(M:PI,imported_from(OM)),
  dmsg(warn(really_remake_as_dynamic_no_props(OM:PI,for(M,Reason)))),
  really_remake_as_dynamic_no_props(Reason,OM,PI,F,A).

really_remake_as_dynamic_no_props(Reason,M,PI,F,A):- 
  dmsg(warn(really_remake_as_dynamic_no_props2(M:PI,bc(Reason)))),
  % must((predicate_property(M:PI,module(Was)),Was=M)),
  ignore(delete_import_module(make_as_dynamic,system)),
  ignore(delete_import_module(make_as_dynamic,user)),
  ignore(delete_import_module(make_as_dynamic,M)),
  dynamic(make_as_dynamic:F/A),
  copy_module_predicate_no_props(M,make_as_dynamic,PI),
  abolish(M:F/A),
  copy_module_predicate_no_props(make_as_dynamic,M,PI),
  abolish(make_as_dynamic:F/A).



really_remake_as_dynamic(Reason,M,PI,F,A):- 
  predicate_property(M:PI,imported_from(OM)),
  dmsg(warn(really_remake_as_dynamic(OM:PI,for(M,Reason)))),
  really_remake_as_dynamic(Reason,OM,PI,F,A).
really_remake_as_dynamic(Reason,M,PI,F,A):- 
  dmsg(warn(really_remake_as_dynamic2(M:PI,bc(Reason)))),
  % must((predicate_property(M:PI,module(Was)),Was=M)),
  ignore(delete_import_module(make_as_dynamic,system)),
  ignore(delete_import_module(make_as_dynamic,user)),
  ignore(delete_import_module(make_as_dynamic,M)),
  dynamic(make_as_dynamic:F/A),
  copy_module_predicate(M,make_as_dynamic,PI),
  abolish(M:F/A),
  copy_module_predicate(make_as_dynamic,M,PI),
  abolish(make_as_dynamic:F/A).


% TODO uncomment these out!
%do_import(system,M,F,A):-throw(unexpected(do_import(system,M,F,A))).
%do_import(user,M,F,A):-throw(unexpected(do_import(user,M,F,A))).
do_import(TM,M,F,A):- 
   must((TM:import(M:F/A),TM:export(TM:F/A))),!.
   % must((TM:module_transparent(M:F/A))). % in case this has clauses th

%% decl_as(Types, TermM) is semidet.
%
% Declare as Types.
%
decl_as(Types,Goal):- now_and_later(n,decl_az(Types,Goal)).

decl_az(Types,Var):-var(Var),!,trace_or_throw(var_decl_shared(Types,Var)).
decl_az(Types,M:FA):- if_defined(defaultAssertMt(M),fail),!,decl_az(Types,FA),!.
decl_az(Types,abox:FA):-!,decl_az(Types,FA),!.
decl_az(Types,MM:G1):- (MM= (_:M)), !,decl_az(Types,M:G1),!.

decl_az(Types,(G1,G2)):-!,decl_az(Types,G1),!,decl_az(Types,G2),!.
decl_az(Types,[G1]):-!,decl_az(Types,G1),!.
decl_az(Types,[G1|G2]):-!,decl_az(Types,G1),!,decl_az(Types,G2),!.
decl_az(Types,M:(G1,G2)):-!,decl_az(Types,M:G1),!,decl_az(Types,M:G2),!.
decl_az(Types,M:[G1]):-!,decl_az(Types,M:G1),!.
decl_az(Types,M:[G1|G2]):-!,decl_az(Types,M:G1),!,decl_az(Types,M:G2),!.
decl_az(Types,M:F):-atom(F),!,decl_az(Types,M,F,_).
decl_az(Types,F):-atom(F),!,decl_az(Types,_,F,_).
decl_az(Types,M:'//'(F,Am2)):-!,A is Am2+2, decl_az(Types,M,F,A).
decl_az(Types,M:F/A):-!,decl_az(Types,M,F,A).
decl_az(Types,'//'(F,Am2)):-!,A is Am2+2, decl_az(Types,_,F,A).
decl_az(Types,F/A):-!,decl_az(Types,_,F,A).
decl_az(Types,M:Goal):-compound(Goal),!,functor(Goal,F,A),decl_az(Types,M,F,A).
decl_az(Types,Goal):-compound(Goal),!,functor(Goal,F,A),decl_az(Types,_,F,A).
decl_az(Types,Goal):-trace_or_throw(bad_decl_as(Types,Goal)).

decl_as(Types,M,F,A):- now_and_later(n,decl_az(Types,M,F,A)).
decl_az(Types,M,F,A):- var(M),if_defined(defaultAssertMt(M),M=baseKB),!,decl_az(Types,M,F,A).
decl_az(Types,M,F,A):- var(A),!,forall(between(1,12,A),decl_az(Types,M,F,A)).
decl_az(M:Types,M,F,A):-!, decl_az(Types,M,F,A).
decl_az(Types,M,F,A):-!, decl_as_rev(M:F/A,Types).

decl_as_rev(MFA,(G1,G2)):-!,decl_as_rev(MFA,G1),!,decl_as_rev(MFA,G2),!.
decl_as_rev(MFA,[G1]):-!,decl_as_rev(MFA,G1),!.
decl_as_rev(MFA,[G1|G2]):-!,decl_as_rev(MFA,G1),!,decl_as_rev(MFA,G2),!.
decl_as_rev(MFA,M:(G1,G2)):-!,decl_as_rev(MFA,M:G1),!,decl_as_rev(MFA,M:G2),!.
decl_as_rev(MFA,M:[G1]):-!,decl_as_rev(MFA,M:G1),!.
decl_as_rev(MFA,M:[G1|G2]):-!,decl_as_rev(MFA,M:G1),!,decl_as_rev(MFA,M:G2),!.
                                                    
decl_as_rev(M:F/A,_OM:Pred):- check_mfa(Pred,M,F,A),
  must(call(Pred,M,F,A)),!.


decl_as_rev(M:F/A,Pred):- check_mfa(Pred,M,F,A),
  must(call(Pred,M,F,A)).











% skip_mfa(Why,M, genlMt, 2):- baseKB\=M,dumpST,dmsg(skip_mfa(Why,M, genlMt, 2)),!,break.
check_mfa(_Why,M,F,A):-sanity(atom(F)),sanity(integer(A)),sanity(current_module(M)->true;dmsg(new_module(M))).



% kb_global(SPEC):- SPEC=(_:_), !, decl_as(decl_kb_global,SPEC), context_module(M),!,( \+ mtHybrid(M) -> M:import(SPEC); true).
kb_global(SPEC):- must(decl_as(decl_kb_global,SPEC)),!.



:- multifile(lmcache:already_decl/4).
:- dynamic(lmcache:already_decl/4).                                     

predicate_m_f_a_decl(M,F,A,Other):- lmcache:already_decl(Other,M,F,A).

pred_decl_kb_mfa_type(M,F,A,Other):- lmcache:already_decl(Other,M,F,A).


%:- dynamic(rdf_rewrite:decl_kb_global/3).
%:- multifile(rdf_rewrite:decl_kb_global/3).
%:- import(rdf_rewrite:decl_kb_global/3).


% TODO comment this out!
decl_kb_global(M,'==>',A):- !, dmsg(warn(skip(decl_kb_global(M,'==>',A)))).

decl_kb_global(M,F,A):- check_mfa(kb_global,M,F,A),!,
  (lmcache:already_decl(kb_global,M,F,A)->true;
  (asserta(lmcache:already_decl(kb_global,M,F,A)),do_decl_kb_global(M,F,A))),!.
decl_kb_global(M,F,A):- trace_or_throw(bad_kb_global(M,F,A)).

do_decl_kb_global(M,prologSingleValued,0):- trace_or_throw(do_decl_kb_global(M,prologSingleValued,0)).

do_decl_kb_global(M,F,A):-functor(PI,F,A),do_decl_kb_global_1(M,F,A,PI).

%:- rdf_rewrite:import(decl_kb_global/3).

%do_decl_kb_global_1(M,F,A,PI):- M\=baseKB,M\=elmt,M\=rdf_rewrite,\+ clause(baseKB:using_pfc(user,M,pfc_mod),true),dumpST,break,(trace_or_throw(do_decl_kb_global_m(M,F,A,PI))).
%do_decl_kb_global_1(M,F,A,PI):- if_defined(mpred_database_term(F,A,_),F = ~),dmsg(trace_or_throw(do_decl_kb_global_1(M,F,A,PI))).
do_decl_kb_global_1(M,F,A,PI):- lmcache:already_decl(Other,M,F,A), Other \== (kb_global), dmsg(warn(trace_or_throw(already_decl(Other,M,F,A,PI)))),!.

do_decl_kb_global_1(M,F,A,PI):- \+ predicate_property(M:PI,imported_from(_)), predicate_property(M:PI,defined),!,do_decl_kb_global_2(M,F,A,PI).
% not possible do_decl_kb_global_1(M,F,A,PI):- predicate_property(M:PI,imported_from(M)),!,do_decl_kb_global_2(M,F,A,PI).

do_decl_kb_global_1(M,F,A,PI):- predicate_property(M:PI,imported_from(R)),R\==M,!,
   show_failure(pfc(inherited_shared(R)),do_import(M,R,F,A)),
   do_decl_kb_global_2(R,F,A,PI),
   nop(do_import(system,R,F,A)),!.

do_decl_kb_global_1(M,F,A,PI):- current_predicate(F,R:PI), 
   \+ predicate_property(R:PI,inherited_from(_)),
   R\==M,
   dmsg(pfc(shared_found_peer(R,M:F/A))),
   do_import(M,R,F,A),
   do_decl_kb_global_2(R,F,A,PI),
   nop(do_import(system,R,F,A)),!.

do_decl_kb_global_1(M,F,A,PI):- do_decl_kb_global_2(M,F,A,PI),!.
  

do_decl_kb_global_2(M,F,A,_PI):- 
   nop(dmsg((do_decl_kb_global(M,F,A)))),
 ((
   make_as_dynamic(kb_global(M:F/A),M,F,A),
    M:export(M:F/A),
    do_import(baseKB,M,F,A),
    do_import(pfc_toplevel,M,F,A),   
    do_import(pfc_mod,M,F,A),   
    do_import(pfc_lib,M,F,A),   
    do_import(mpred_type_isa,M,F,A),

   do_import(system,M,F,A),   
% TODO BEGIN comment these out!
   do_import(user,M,F,A),
   %do_import(header_sane,M,F,A),      
   %'$current_source_module'(SM),do_import(SM,M,F,A),   
   %'$current_typein_module'(TM),do_import(TM,M,F,A),
% TODO END comment these out!
   nop(decl_wrapped(M,F,A,ereq)))).

   % on_f_throw( (M:F/A)\== (lmcache:loaded_external_kbs/1)),
   % (find_and_call(mtHybrid(M))->ain(baseKB:prologHybrid(F));true),


% kb_local(SPEC):- !,kb_global(SPEC),!.



kb_local(SPEC):-  decl_as(decl_kb_local,SPEC),!.
kb_shared(SPEC):- decl_as(decl_kb_shared,SPEC),!.

decl_kb_shared(M,F,A):- lmcache:already_decl(kb_global,R,F,A), nop(dmsg(warn(kb_local(already_decl(kb_global,R->M,F,A))))),!.
decl_kb_shared(R,F,A):- lmcache:already_decl(kb_global,M,F,A),!,do_import(M,R,F,A).
decl_kb_shared(M,F,A):- decl_kb_type(kb_shared,M,F,A).

decl_kb_local(M,F,A):- lmcache:already_decl(kb_global,R,F,A), nop(dmsg(warn(kb_local(already_decl(kb_global,R->M,F,A))))),!.
decl_kb_local(R,F,A):- lmcache:already_decl(kb_global,M,F,A),!,do_import(M,R,F,A).
decl_kb_local(M,F,A):- decl_kb_type(kb_local,M,F,A).

decl_kb_type(Type,M,'==>',A):- A==1, !, nop(dmsg(skip(decl_kb_type(Type,M,'==>',A)))).

decl_kb_type(Type,M,F,A):- lmcache:already_decl(kb_global,R,F,A),M==R, 
   nop(dmsg(warn(trace_or_throw(already_decl(kb_global(Type),R->M,F,A))))),!.
decl_kb_type(Type,M,F,A):- lmcache:already_decl(kb_global,R,F,A), 
   dmsg(warn(trace_or_throw(already_decl(kb_global(Type),R->M,F,A)))),!.


decl_kb_type(Type,M,F,A):- check_mfa(Type,M,F,A),!,
  (lmcache:already_decl(Type,M,F,A)->true;
    (asserta(lmcache:already_decl(Type,M,F,A)),do_decl_kb_type(Type,Type,M,F,A))),!.
decl_kb_type(Type,M,F,A):- trace_or_throw(bad_decl_kb_type(Type,M,F,A)).

do_decl_kb_type(Type,Type,M,prologSingleValued,0):- 
  trace_or_throw(do_decl_kb_type(Type,Type,M,prologSingleValued,0)).

do_decl_kb_type(Type,Type,M,F,A):-functor(PI,F,A), do_decl_kb_type_1(Type,M,F,A,PI),!.

do_decl_kb_type_1(Type,M,F,A,_):- lmcache:already_decl(Other,M,F,A),Other\=(Type),!. % ,dmsg(lmcache:already_decl(Other,M,F,A)).

do_decl_kb_type_1(Type,M,F,A,PI):-
  predicate_property(M:PI,inherited_from(R)),R\==M,!,
  do_decl_kb_type_2(Type,R,F,A,PI),
  show_call(pfc(inherited_local(R)),do_import(M,R,F,A)).

do_decl_kb_type_1(Type,M,F,A,PI):- 
  % \+ predicate_property(M:PI,inherited_from(_)), 
  predicate_property(M:PI,defined),
  do_decl_kb_type_2(Type,M,F,A,PI).
% not possible do_decl_kb_type_1(Type,M,F,A,PI):- predicate_property(M:PI,inherited_from(M)),!,do_decl_kb_type_2(Type,M,F,A,PI).

do_decl_kb_type_1(_Type,M,F,A,PI):- fail,
   findall(R,(current_predicate(F,R:PI), 
   \+ predicate_property(R:PI,inherited_from(_)),
   R\==M),Rs),Rs\==[],Rs\==[baseKB],
   dmsg(pfc(local_found_peer(Rs,M:F/A))),fail,
   !,
   show_call(pfc(found_peer(R)),do_import(M,R,F,A)).

do_decl_kb_type_1(Type,M,F,A,PI):- do_decl_kb_type_2(Type,M,F,A,PI),!.
  

do_decl_kb_type_2(Type,M,F,A,_PI):- 
 nop(dmsg((do_decl_kb_type(Type,M,F,A)))),
 ((
  Why = decl_kb_type(Type,M:F/A),
  make_as_dynamic(Why,M,F,A),
  ain(baseKB:mpred_prop(M,F,A,Type)),
  create_predicate_inheritance(Why,M,F,A),
  nop(decl_wrapped(M,F,A,ereq)))).

expand_globals(In,_):- notrace(( \+ callable(In);In\=(':'(_,_)))),!,fail.
expand_globals((P:-B),(PO:-B)):-!,expand_globals(P,PO).
expand_globals(P,Out):-
   (compound(P)->functor(P,F,A);F=P,A=0),
   expand_already_decl(P,F,A,Out).

expand_already_decl(P,F,A,Out):- lmcache:already_decl(kb_global,M,F,A),Out=':'(M,P),!.
expand_already_decl(P,F,A,Out):- lmcache:already_decl(kb_shared,M,F,A),Out=':'(M,P),!.
                                        

%:- fixup_exports.

:- multifile(lmcache:was_retry_undefined/2).
:- dynamic(lmcache:was_retry_undefined/2).

:- multifile(prolog:make_hook/2).
:- dynamic(prolog:make_hook/2).

prolog:make_hook(before, C):- current_prolog_flag(retry_undefined, WAS),
  asserta(lmcache:was_retry_undefined(WAS,C)),set_prolog_flag(retry_undefined, none),fail.
prolog:make_hook(after, C):- retract(lmcache:was_retry_undefined(WAS,C)),
  set_prolog_flag(retry_undefined, WAS),fail.

%:- meta_predicate( ex_predicate_property(:,?)).
% ex_predicate_property(P,Prop):- with_no_retry_undefined(predicate_property(P,Prop)).

%:- kb_shared(system:rtArgsVerbatum/1).
%:- kb_shared(system:prologBuiltin/1).
%baseKB:module(_X,_Y).



undefined_predicate_exception(F/A,  CM,_,ActionO):- !, user_exception_undefined_predicate(CM,CM,F,A, ActionO).
undefined_predicate_exception(M:F/A,CM,_, ActionO):-   user_exception_undefined_predicate(CM, M,F,A, ActionO).

user_exception_undefined_predicate(CM,M,F,A,ActionO):- 
   \+ prolog_load_context(reloading,true),
  current_prolog_flag(retry_undefined, Was), Was \== none,
  get_retry_undefined_hook(M,Setting), !, Setting \== error,     
   CM:
  setup_call_cleanup(set_prolog_flag(retry_undefined, none),
                      (trace,uses_predicate(Setting,CM,M,F,A,ActionO),trace, ActionO \== error),
                      set_prolog_flag(retry_undefined, Was)),!.

:- multifile(user:exception/3).
:- module_transparent(user:exception/3).
:- dynamic(user:exception/3).

% user_exception_undefined_predicate
/*
user:exception(undefined_predicate, MFA, Action):-  notrace,nodebug, 
  current_prolog_flag(retry_undefined,Was), Was\==none,
  strip_module(MFA, CM, FA),      trace, 
  undefined_predicate_exception(MFA,CM,FA, Action),trace, !, Action\==error.
*/

:- retract( ru_l:was_prolog_flag(retry_undefined,Was)), set_prolog_flag(retry_undefined, Was).


:- set_prolog_flag(predicate_inheritance,false).
/*
:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- module_transparent(system:goal_expansion/4).
system:goal_expansion(In,P,Out,PO):- notrace( current_prolog_flag(predicate_inheritance,true)), expand_globals(In,Out),P=PO.

:- multifile(system:term_expansion/4).
:- dynamic(system:term_expansion/4).
:- module_transparent(system:term_expansion/4).
system:term_expansion(In,P,Out,PO):- notrace( current_prolog_flag(predicate_inheritance,true)), 
   % checks if asserting
   notrace((nb_current('$term', Term),In == Term ; (Term=(Head:-_),In == Head))),
   expand_globals(In,Out),P=PO.
*/
