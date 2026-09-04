/* Part of LogicMOO Base logicmoo_util_bb_env
% Provides a prolog database *env*
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles logicmoo@gmail.com ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2021/07/11 21:57:28 $
% License: LGPL
% ===================================================================
*/

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_structs.pl
:- module(virtualize_source,
   [%cnas/3,
    nb_current_or_nil/2,
    safe_virtualize/3,          
    %skipped_dirs/1,
    check_how_virtualize_file/2,
    could_safe_virtualize/0,
    decl_wrapped/4,
    get_how_virtualize_file/2,
    same_terms/2,          
    sd_goal_expansion/4,
    set_how_virtualize_file/1,
    set_how_virtualize_file/2,
    set_how_virtualize_file/3,
    swc/0,
    is_file_virtualize_allowed/0,
    virtualize_code/3,
    virtualize_code_each/4,
    virtualize_code_fa/5,
    virtualize_ereq/2,
    virtualize_source/3,
    virtualize_source_file/0,
    vwc/0]).
/** <module> Utility LOGICMOO VIRTUALIZE SOURCE
Source code transformation - Uses Hook Database and Hook Hybrid to rewrite source code to better interact with hybrid database. 

- @author Douglas R. Miles
- @license LGPL 
*/

:- autoload(library(apply),[maplist/2]).
:- autoload(library(lists),[member/2,append/3]).
:- autoload(library(occurs),[sub_term/2]).

:- define_into_module(
   [nb_current_or_nil/2,
    safe_virtualize/3,          
    %skipped_dirs/1,
    check_how_virtualize_file/2,
    could_safe_virtualize/0,
    decl_wrapped/4,
    get_how_virtualize_file/2,
    same_terms/2,          
    sd_goal_expansion/4,
    set_how_virtualize_file/1,
    set_how_virtualize_file/2,
    set_how_virtualize_file/3,
    swc/0,
    is_file_virtualize_allowed/0,
    virtualize_code/3,
    virtualize_code_each/4,
    virtualize_code_fa/5,
    virtualize_ereq/2,
    virtualize_source/3,
    virtualize_source_file/0,
    vwc/0]).

:- module_transparent((
%cnas/3,
nb_current_or_nil/2,
safe_virtualize/3,
same_terms/2,          
%decl_wrapped/4,
sd_goal_expansion/4,
%skipped_dirs/1,
swc/0,
virtualize_code/3,
virtualize_code_each/4,
virtualize_code_fa/5,
virtualize_ereq/2,
virtualize_source/3,
vwc/0
          )).

:- set_module(class(library)).% WAS OFF  :- system:reexport(library(must_sanity)).% WAS OFF  :- system:reexport(library(loop_check)).
:- module_transparent((virtualize_ereq_source/0)).% WAS OFF  :- system:use_module(predicate_inheritance).% WAS OFF  :- system:use_module(retry_undefined).



:- meta_predicate map_compound_args(3,*,*,*).
:- meta_predicate map_compound_args(2,*,*).

:- module_transparent(declared_to_wrap/3).


:- thread_local(t_l:disable_px/0).

% % % OFF :- system:reexport(library(hook_database)).

nb_current_or_nil(N,V):- quietly((nb_current(N,V)->true;V=[])).

/*
:- multifile(baseKB:col_as_isa/1).
:- multifile(baseKB:col_as_unary/1).
:- multifile(baseKB:col_as_static/1).
:- dynamic(baseKB:col_as_isa/1).
:- dynamic(baseKB:col_as_unary/1).
:- dynamic(baseKB:col_as_static/1).
*/


:- dynamic(baseKB:how_virtualize_file/3).
% stream_position_data
get_source_location(File,Pos):-  (current_source_location(File,Pos); (prolog_load_context(file,File),prolog_load_context(term_position,Pos))),!.
get_source_location(_File,0).

%guess_file_language(File, Lang):- file_content_type(File,Lang).

get_current_clause(MI):- prolog_load_context(term,Was), Was \== [], !, Was = MI.
get_current_clause(MI):- 
  arg(_,v('$term','$term_user','$term_exp_skip','$orig_term''$source_term','$goal_term','$query_term'),Var),
  nb_current(Var,Was),Was\==[], !, Was = MI.
get_current_clause(_).



virtualize_alias(pfc,heads).
virtualize_alias(full,heads).

virtualize_alias(part,bodies).
virtualize_alias(plmoo,bodies).
virtualize_alias(true,bodies).

virtualize_alias(none,false).
virtualize_alias(prolog,false).


is_pfcname(F) :- atom(F), \+ \+ (((atom_concat(_,'.pfc.pl',F);atom_concat(_,'_pfc.pl',F);atom_concat(_,'.clif',F);atom_concat(_,'.pfc',F)))).

virtualize_source_file :- set_how_virtualize_file(bodies).
virtualize_source_file(How) :- set_how_virtualize_file(How).

set_how_virtualize_file(How):-  get_source_location(File,Pos), set_how_virtualize_file(How,File,Pos).
set_how_virtualize_file(How,F):- set_how_virtualize_file(How,F,0).
set_how_virtualize_file(How,F1,Pos):- virtualize_alias(How,NewHow),How\==NewHow, !, set_how_virtualize_file(NewHow,F1,Pos).
set_how_virtualize_file(How,F1,Pos):-
  resolve_file_pathname(F1,F),
  forall((clause(baseKB:how_virtualize_file(_PrevHow,F,PrevPos),true,Ref),
    PrevPos>=Pos),erase(Ref)),
  asserta_new(baseKB:how_virtualize_file(How,F,Pos)).


resolve_file_pathname(F1,F):- absolute_file_name(F1,F,[access(read),file_errors(fail)]),!.
resolve_file_pathname(F1,F):- absolute_file_name(F1,F,[file_type(prolog),access(read),file_errors(fail)]),!.
resolve_file_pathname(F1,F):- absolute_file_name(F1,F,[file_type(directory),access(read),file_errors(fail)]),!.
resolve_file_pathname(F,F):-!.

get_how_virtualize_file(Lang):- must(get_source_location(File,Pos)),get_how_virtualize_file(Lang, File, Pos).
get_how_virtualize_file(Lang, File):- get_source_location(File,Pos),!,get_how_virtualize_file(Lang, File, Pos).
get_how_virtualize_file(Lang, File):- get_how_virtualize_file(Lang, File, 0).
get_how_virtualize_file(Lang, File, Pos):- 
  findall(sfl(Pos,Lang),baseKB:how_virtualize_file(Lang, File, Pos),List),
  sort(List,Sort),member(sfl(Pos2,Lang),Sort),Pos>=Pos2,!.
get_how_virtualize_file(Lang, File, _Pos):- baseKB:how_virtualize_file(Lang, File, 0),!.
get_how_virtualize_file(Lang, File, _):- guess_file_language(File, Lang),set_how_virtualize_file(Lang, File, 0).


  

guess_file_language(File, heads):- is_pfcname(File), !.
guess_file_language(File, How):- baseKB:how_virtualize_dir(Was,Stem), atom_concat(Stem,_,File),!, 
    set_how_virtualize_file(Was, File, 0), !, How = Was.
guess_file_language(File, false):- atom_concat(_,'.pl',File).
guess_file_language(File, body):- atom_concat(_,'.plmoo',File).
guess_file_language(_File, false).



:- dynamic(baseKB:how_virtualize_file/3).


set_how_virtualize_dir(How,F1):- 
  resolve_file_pathname(F1,F),
   retractall(baseKB:how_virtualize_dir(_,F)),
   asserta_new(baseKB:how_virtualize_dir(How,F)).

:- dynamic(baseKB:how_virtualize_dir/2).
baseKB:how_virtualize_dir(false,'/opt/logicmoo_workspace/packs_xtra/logicmoo_nlu/ext/').
:- expand_file_search_path(swi(''),M),(set_how_virtualize_dir(false,M)).
:- expand_file_search_path(pack(logicmoo_util),M),(set_how_virtualize_dir(false,M)).


srcfilew(File):- prolog_load_context(file,File)-> true; source_location(File,_W).


could_safe_virtualize:- srcfilew(File),could_safe_virtualize(File).
could_safe_virtualize(File):- is_file_virtualize_allowed(File),!.                           
could_safe_virtualize(File):- prolog_load_context(module,M), \+ clause_b(mtHybrid(M)),   
     \+ ((current_prolog_flag(dialect_pfc,fwc); is_pfcname(File))).


% check_how_virtualize_file(heads,File):- prolog_load_context(file,File),t_l:current_lang(pfc).
% check_how_virtualize_file(heads,File):- prolog_load_context(source,File),t_l:current_lang(pfc),source_location(SFile,_W), \+ check_how_virtualize_file(false,SFile),!.

% file late late joiners
:- if( \+ prolog_load_context(reload,true)).

:- source_location(File, Line)-> (((set_how_virtualize_file(false,File, Line)))).

:- doall((module_property(M,file(File)),
          \+ baseKB:how_virtualize_file(_,File, _),
          module_property(M,class(CT)),
          memberchk(CT,[library,system]),
          set_how_virtualize_file(false,File,0))).
%:- doall((source_file(File),(set_how_virtualize_file(false,File)))).
%base_kb_dynamic(F,A):- ain(mpred_prop(M,F,A,prologHybrid)),kb_shared(F/A).
%:- doall((virtualize_ereq(F,A),base_kb_dynamic(F,A))).
:- endif.



% if_defined(G,Else) = if G is defined then call G.. else call Else
%ignore_mpreds_in_file:- if_defined(t_l:disable_px,fail),!.
ignore_mpreds_in_file:- prolog_load_context(file,F),check_how_virtualize_file(false,F),!.
ignore_mpreds_in_file:- prolog_load_context(source,F), \+ prolog_load_context(file,F), check_how_virtualize_file(false,F),!.

is_file_virtualize_allowed(F):- check_how_virtualize_file(bodies,F).

is_file_virtualize_allowed:- fail,
  prolog_load_context(source,S), (is_file_virtualize_allowed(S)-> true ; 
   (prolog_load_context(file,F),F\==S, is_file_virtualize_allowed(F))).





check_how_virtualize_file(How,F):- get_how_virtualize_file(How2,F), How2 == How,!.
check_how_virtualize_file(bodies,F):- atom_concat(_,'.plv',F).
check_how_virtualize_file(bodies,F):- is_pfcname(F),!,fail.
check_how_virtualize_file(bodies,F):- \+ get_how_virtualize_file(_,F),!, is_file_virtualize_allowed.
check_how_virtualize_file(bodies,F):- get_how_virtualize_file(TF,F), !, TF \== false.
check_how_virtualize_file(heads,F):- get_how_virtualize_file(How,F),!, How==heads.
check_how_virtualize_file(heads,F):- \+ get_how_virtualize_file(_,F),!, is_pfcname(F), !, set_how_virtualize_file(heads,F),!.
check_how_virtualize_file(false,F):- is_pfcname(F),set_how_virtualize_file(heads,F),!, fail.
check_how_virtualize_file(false,F):- get_how_virtualize_file(heads,F),!,fail.
check_how_virtualize_file(How,F):- get_how_virtualize_file(How2,F),!, How2 == How.

 

%check_how_virtualize_file(false,File):- module_property(M,file(File)),module_property(M,class(library)),(set_how_virtualize_file(false,File)),!.
% check_how_virtualize_file(false,File):- check_how_virtualize_file(false,File),!.
% check_how_virtualize_file(false,File):- (set_how_virtualize_file(heads,File)),!,fail.


%:- asserta((check_how_virtualize_file(false,M):- skipped_dirs(M))).

%skipped_dirs(Dir):-skipped_dirs0(M),exists_directory(M),absolute_file_name(M,Dir).
%skipped_dirs0(M):-expand_file_search_path(pack(logicmoo_util),M).
%skipped_dirs0(M):-expand_file_search_path(pack('swish/..'),M).
%skipped_dirs0(M):-expand_file_search_path(pack('wam_common_lisp/..'),M).
% skipped_dirs(M):-expand_file_search_path(pack(pfc),M),nonvar(M).








































use_file_filter_cached(Module:Check):- 
 Check =..[Include,F],
 (Module:Check *-> true ;
  ((call(Module:Include,Base), 
   (Base=F -> true ;
     (atom(F),atom(Base),    
      (atom_concat(Base,_,F);atom_concat(_,Base,F)),
       asserta(Module:Check)))))).

:- (set_how_virtualize_file(false,'.data')).
%% declared_to_wrap(M, ?Functor, ?Arity, ?Wrapper) is semidet.
%
% Virtualizer Shared Code.
%

get_virtualizer_mode(ge,F,A,HowIn):- suggest_m(M), declared_to_wrap(M,F,A,HowOut),!,must(HowIn=HowOut),HowOut\==never.

/*
:- dynamic baseKB:t/2.
:- multifile baseKB:t/2.
:- public baseKB:t/2.
:- module_transparent baseKB:t/2.
:- dynamic baseKB:t/1.
:- multifile baseKB:t/1.
:- public baseKB:t/1.
:- module_transparent baseKB:t/1.
*/

:- multifile(baseKB:safe_wrap/4).
:- module_transparent(baseKB:safe_wrap/4).
:- dynamic(baseKB:safe_wrap/4).

declared_to_wrap(_M,O,_,_):- bad_functor_check(O),!,trace_or_throw(bad_functor_check(O)),fail.
declared_to_wrap(_M,mtHybrid,1,clause_b).
declared_to_wrap(_M,F,A,on_x_debug):- integer(A),virtualize_safety(F,A).
declared_to_wrap(M,F,A,HowIn):- clause_b(safe_wrap(M,F,A,HowIn)),!.
declared_to_wrap(_,F,A,HowIn):- clause_b(safe_wrap(_,F,A,HowIn)),!.
declared_to_wrap(_M,F,A,dbreq):- virtualize_dbreq(F,A), virtualize_dbreq_source.
declared_to_wrap(M,F,A,ereq):- clause_b(mpred_prop(M,F,A,prologHybrid)),!.
declared_to_wrap(M,F,A,ereq):- virtualize_m_ereq(M,F,A), virtualize_ereq_source.
declared_to_wrap(M,F,A,_):- prolog_load_context(module,M),never_virtualize(M:F/A),!,fail.
declared_to_wrap(M,F,A,_):-      clause_b(mpred_prop(M,F,A,prologBuiltin)),!,fail.
declared_to_wrap(M,F,A,call_u):- clause_b(mpred_prop(M,F,A,_)),!.

declared_to_wrap(M,F,A,ereq):- atom(F),integer(A),
   functor(Goal,F,A),
   % member(M,[baseKB,lmcache,lmconf]),
   baseKB = M,
   predicate_property(M:Goal,defined),
   \+ predicate_property(M:Goal,static),!.
   % \+ predicate_property(M:Goal,imported_from(_)),!.




is_dynamic_module(user).
is_dynamic_module(baseKB).
is_dynamic_module(lmcache).
is_dynamic_module(lmconf).
is_dynamic_module(tlbugger).
is_dynamic_module(t_l).
is_dynamic_module(prolog).
is_dynamic_module(eggdrop).
is_dynamic_module(M):- clause_b(mtHybrid(M)).

is_static_module(system).
is_static_module(file_scope).
is_static_module(pfc_lib).
is_static_module(M):- is_dynamic_module(M),!,fail.
is_static_module(M):- module_property(M,class(development)),!,fail.
is_static_module(M):- module_property(M,class(library)),!.
is_static_module(M):- module_property(M,class(system)),!.

% virtualize_dbreq_source :- prolog_load_context(module,M), (atom_concat('common_logic_',_,F);atom_concat('logicmoo_',_,F);atom_concat('mpred_',_,F)),!.
virtualize_dbreq_source :- prolog_load_context(source,F), 
  (atom_concat('common_logic_',_,F);atom_concat('logicmoo_',_,F);atom_concat('mpred_',_,F)),!.
virtualize_dbreq_source :- prolog_load_context(module,M), \+ is_static_module(M).
% virtualize_dbreq_source.

virtualize_ereq_source :- prolog_load_context(module,M), member(M,['pfc_lib','mpred_expansion']),!,fail.
virtualize_ereq_source.

bad_functor_check(O):-var(O).
bad_functor_check(':'):- !,dumpST,dtrace.
%bad_functor_check(/):- !,dumpST,dtrace.
%bad_functor_check(//):- !,dumpST,dtrace.


% Preds that we''d like to know a little more than "instanciation exception"s
virtualize_safety(O,_):- bad_functor_check(O),!,fail.

virtualize_safety((=..),2).
virtualize_safety(functor,3).
virtualize_safety(arg,3).
virtualize_safety(is,2).
/*

*/

% Preds that we assume indicate we''d already passed over it


never_virtualize(O):- bad_functor_check(O),!,fail.
never_virtualize(_:','/2):-!,fail.
never_virtualize(_:F/_):- !, never_virtualize_atom(F),!.
never_virtualize(thread_util:_/A):-integer(A). % prevents query
never_virtualize(M:F/A):- clause_b(mpred_prop(M,F,A,prologBuiltin)),!.
never_virtualize(_M:F/A):- current_predicate(pfc_lib:F/A),!.
never_virtualize(M:F/A):- functor(P,F,A),source_file(M:P,_SF), 
   \+ predicate_property(M:P,meta_predicate(_)), 
   \+ predicate_property(M:P,transparent),  
  % dmsg(never_virtualize('@'(F/A,M),SF)),
  ain(baseKB:mpred_prop(M,F,A,prologBuiltin)). 
never_virtualize(M:F/A):- functor(P,F,A),source_file(M:P,SF),
   \+ predicate_property(M:P,meta_predicate(_)), 
   \+ predicate_property(M:P,transparent), !,
  dmsg(never_virtualize(M:F/A,SF)),
  aina(baseKB:mpred_prop(M,F,A,prologBuiltin)).
never_virtualize(_:F/_):- never_virtualize_atom(F),!.
never_virtualize(_:FA):- !,never_virtualize(FA),!.

never_virtualize_atom(Atom):- \+ atom(Atom),!,fail.
never_virtualize_atom(F):- functor(C,F,1),predicate_property(system:C,static), 
     \+ predicate_property(system:C,transparent).
never_virtualize_atom(ereq).
never_virtualize_atom(dbreq).
never_virtualize_atom(call_u).
never_virtualize_atom(on_x_debug).
never_virtualize_atom(clause_u).
never_virtualize_atom(lookup_u).
never_virtualize_atom(clause_b).
never_virtualize_atom(('.')).
never_virtualize_atom(('[]')).
never_virtualize_atom(('[|]')).
never_virtualize_atom(add).
never_virtualize_atom(dmsg).
never_virtualize_atom(member).
never_virtualize_atom(fully_expand).
never_virtualize_atom(wdmsg).
never_virtualize_atom(trace_or_throw).


never_virtualize_atom(padd).
never_virtualize_atom(del).
never_virtualize_atom(ain_expanded).
never_virtualize_atom(meta_predicate).
never_virtualize_atom(dynamic).
never_virtualize_atom(clr).
never_virtualize_atom(ain).
never_virtualize_atom(props).
never_virtualize_atom(=).
never_virtualize_atom(==).
never_virtualize_atom(iprop).
never_virtualize_atom(aina).
never_virtualize_atom(decl_as).
never_virtualize_atom(ainz).
never_virtualize_atom((':-')).
never_virtualize_atom(F):- suggest_m(M), clause_b(mpred_prop(M,F,_,pfcBuiltin)). % @todo not yet created
%never_virtualize_atom(F):- show_success(plz_never_virtualize(F)).

never_virtualize_atom(Atom):- never_virtualize2(Atom).
never_virtualize_atom(Atom):- atom(Atom),!,atom_concat('mpred_',_,Atom). % mpred_* are pfc builtins


never_virtualize2((/)).
never_virtualize2((//)).
never_virtualize2(call).
never_virtualize2(fix_mp).
never_virtualize2(apply).

plz_never_virtualize(on_x_debug).


% operations to transactionalize
virtualize_dbreq(O,_):- bad_functor_check(O),!,fail.
virtualize_dbreq(abolish,1).
virtualize_dbreq(abolish,2).
virtualize_dbreq(assert,1).
virtualize_dbreq(assert,2).
virtualize_dbreq(asserta,1).
virtualize_dbreq(asserta,2).
virtualize_dbreq(assertz,1).
virtualize_dbreq(assertz,2).
virtualize_dbreq(nth_clause,3).
virtualize_dbreq(clause,2).
virtualize_dbreq(clause,3).
virtualize_dbreq(retract,1).
virtualize_dbreq(listing,1).
virtualize_dbreq(clause_property,2).
virtualize_dbreq(retractall,1).
virtualize_dbreq(recorda,_).
virtualize_dbreq(recordz,_).
virtualize_dbreq(recorded,_).
virtualize_dbreq(erase,1).




virtualize_m_ereq(_M,F,A):- virtualize_ereq(F,A).
virtualize_ereq(O,_):- bad_functor_check(O),!,fail.

%virtualize_ereq(lmcache:loaded_external_kbs,1).

%virtualize_ereq(COL,A):- clause_b(col_as_isa(COL)),sanity(A==1).
%virtualize_ereq(COL,A):- clause_b(col_as_unary(COL)),sanity(A==1).

virtualize_ereq(t,_).
virtualize_ereq(t,2).
virtualize_ereq(t,3).

virtualize_ereq(functorDeclares,1).

virtualize_ereq(mtCore,1).
virtualize_ereq(mtProlog,1).
virtualize_ereq(mtHybrid,1).
virtualize_ereq(mtExact,1).
virtualize_ereq(mtGlobal,1).
virtualize_ereq(arity,2).


virtualize_ereq(lambda,5).

virtualize_ereq(mpred_f,_).
virtualize_ereq(mpred_f,4).
virtualize_ereq(mpred_f,5).
virtualize_ereq(mpred_f,6).
virtualize_ereq(mpred_f,7).
virtualize_ereq(props,2).




virtualize_ereq(mpred_prop,4).

virtualize_ereq(pfcControlled,1).
virtualize_ereq(pfcRHS,1).
virtualize_ereq(predicateConventionMt,2).
virtualize_ereq(prologBuiltin,1).
virtualize_ereq(prologDynamic,1).
virtualize_ereq(prologHybrid,1).
virtualize_ereq(functorIsMacro,1).
virtualize_ereq(prologSideEffects,1).

virtualize_ereq(singleValuedInArg,2).
virtualize_ereq(singleValuedInArgAX,3).
virtualize_ereq(support_hilog,2).
virtualize_ereq(rtNotForUnboundPredicates,1).

virtualize_ereq(ttExpressionType,1).
virtualize_ereq(ttRelationType,1).



virtualize_ereq('$spft',4).
virtualize_ereq(==>,_).
virtualize_ereq(<==>,_).
virtualize_ereq((<--),2).


virtualize_ereq(F,A):-virtualize_ereq_plz_move_dmiles(F,A).

% TODO BEGIN These need to be assigned the correct files

virtualize_ereq_plz_move_dmiles(call_OnEachLoad,1).

virtualize_ereq_plz_move_dmiles(prologKIF,1).
virtualize_ereq_plz_move_dmiles(prologPTTP,1).

virtualize_ereq_plz_move_dmiles(use_ideep_swi,0).
virtualize_ereq_plz_move_dmiles(meta_argtypes,1).
virtualize_ereq_plz_move_dmiles(coerce_hook,_).
virtualize_ereq_plz_move_dmiles(baseKB:agent_text_command,_).
virtualize_ereq_plz_move_dmiles(baseKB:agent_command,_).
virtualize_ereq_plz_move_dmiles(isa,2).
virtualize_ereq_plz_move_dmiles(genls,2).
virtualize_ereq_plz_move_dmiles(nameString,2).
virtualize_ereq_plz_move_dmiles(argIsa,3).
virtualize_ereq_plz_move_dmiles(argQuotedIsa,3).
virtualize_ereq_plz_move_dmiles(cyckb_t_e2c,3).
virtualize_ereq_plz_move_dmiles(cyckb_t_e2c,4).
virtualize_ereq_plz_move_dmiles(cyckb_t_e2c,_).
virtualize_ereq_plz_move_dmiles(completeExtentEnumerable,1).
virtualize_ereq_plz_move_dmiles(completelyAssertedCollection,1).
virtualize_ereq_plz_move_dmiles(constrain_args_pttp,2).
virtualize_ereq_plz_move_dmiles(cycPlus2,2).
virtualize_ereq_plz_move_dmiles(cycPred,2).
virtualize_ereq_plz_move_dmiles(decided_not_was_isa,2).
virtualize_ereq_plz_move_dmiles(mudKeyword,2).
virtualize_ereq_plz_move_dmiles(resultIsa,2).
virtualize_ereq_plz_move_dmiles(tCol,1).
virtualize_ereq_plz_move_dmiles(tPred,1).
virtualize_ereq_plz_move_dmiles(tRelation,1).
virtualize_ereq_plz_move_dmiles(tAgent,1).
virtualize_ereq_plz_move_dmiles(tCol,1).
virtualize_ereq_plz_move_dmiles(ttTemporalType,1).

% END These need to be assigned the correct files

%% virtualize_code(X, :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.f$
%

%virtualize_code(X,Goal,_):- functor(Goal,F,_),arg(_,v(call_u,call,(/),(',')),F),!,fail.
%virtualize_code(X,M:Goal,(call_u(genlMt(abox,GMt)),with_umt(GMt,Goal))):- M==tbox.
/*
virtualize_args_as(Goal,Args):- sanity((arg(1,Goal,Var),var(Var))), predicate_property(Goal,meta_predicate(Args)).
virtualize_args_as(Goal,_):-predicate_property(Goal,built_in),!,fail.
virtualize_args_as(Goal,Goal):-predicate_property(Goal,transparent),!.
virtualize_args_as(Which,Args):- descend_ge(Which),Args=Which.
*/



virtualize_args_as(Which,Args):- descend_ge(Which),Args=Which.
virtualize_args_as(Goal,Args):- sanity((arg(1,Goal,Var),var(Var))), predicate_property(Goal,meta_predicate(Args)).
%virtualize_args_as(Goal,Goal):-predicate_property(Goal,transparent),!.
% virtualize_args_as(Goal,_):-predicate_property(Goal,built_in),!,fail.

descend_ge(':-'((:),0)).
descend_ge(':-'((-),0)).
descend_ge(( :- 0)).
descend_ge('{}'(0)).
descend_ge('must'(0)).
descend_ge('quietly'(0)).
descend_ge('sanity'(0)).
descend_ge('->'(0,0)).
descend_ge(';'(0,0)).
descend_ge('==>'(-,-)).
descend_ge('==>'(-)).
descend_ge('<--'(-,-)).
descend_ge(z(if)).
descend_ge(z(_)):-!,fail.
descend_ge(Which):-functor(Which,F,_),!,descend_ge(z(F)),!.

:- nb_linkval('$xform_arity',xform_arity(_C,_F,_A)).

xform_arity(C,F,A):-var(C),!,sanity(var(F)),must(var(A)), nb_getval('$xform_arity',xform_arity(C,F,A)),!.
xform_arity(C,F,A):-atom(C),!,C=F,ignore(clause_b(arity(F,A))).
xform_arity(F/A,F,A):-atom(F),!.
xform_arity(F//Am2,F,A):- integer(Am2),!, A is Am2+2.
xform_arity(C,F,A):- compound(C), functor(C,F,A).

xform(_,_):-!,fail.
xform(Var,Out):- \+compound(Var),!,Out=Var.
xform(Nonvar,Out):- \+ current_prolog_flag(subclause_expansion,true),!,Nonvar=Out.
%xform(isa(C,P),mpred_prop(M,F,A,P)):-nonvar(P),!,is_reltype(P),xform_arity(C,F,A).
%xform(isa(C,P),(ttRelationType(P),mpred_prop(M,F,A,P))):-nonvar(C),xform_arity(C,F,A),is_reltype(P),!.
% xform(mpred_isa(C,P),mpred_prop(M,F,A,P)):- xform_arity(C,F,A),!.
xform(hybrid_support(F,A),mpred_prop(_M,F,A,prologHybrid)):-!.
% xform(arity(F,A),mpred_prop(M,F,A,arity)):-!.
xform(mpred_prop(M,F,A,P),mpred_prop(M,F,A,P)):-!.


xform(PC,mpred_prop(M,F,A,P)):- current_assertion_module(M), PC=..[P,C],is_reltype(P),!,xform_arity(C,F,A).
xform(PFA,mpred_prop(M,F,A,P)):- if_defined(defaultAssertMt(M),fail),PFA=..[P,F,A],is_reltype(P),!.
xform(In,PART):- map_compound_args(xform,In,PART),!.

%:-multifile(baseKB:ttRelationType/1).
%:-dynamic(baseKB:ttRelationType/1).
is_reltype(Var):-var(Var),!,fail.
is_reltype(pfcControlled).
is_reltype(prologHybrid).
is_reltype(prologBuiltin).
is_reltype(P):-clause_b(ttRelationType(P)).


cannot_descend_expansion(_,In):- \+ compound(In),!.
cannot_descend_expansion(ge,In):- strip_module(In,M,FA),functor(FA,F,A),!,never_virtualize(M:F/A).


virtualize_code(_,In,Out):- \+ compound(In),!,In=Out.
virtualize_code(_,functor(P,F,A),cfunctor(P,F,A)):-!.
virtualize_code(_,(SWC,REST),(SWC,REST)):- (swc==SWC /* ;cwc==SWC */),!. % never goal expand
virtualize_code(X,(VWC,In),(Out)):- vwc==VWC,!,virtualize_code(X,In,Out).
virtualize_code(_,P=..In,cnas(P,H,T)):- nonvar(In),In=[H|T],!.
virtualize_code(_,P=..In,on_x_debug(P=..In)):-!.
virtualize_code(_,functor(P,F,A),on_x_debug(functor(P,F,A))):-!.
% virtualize_code(X,(G1:-G2),(G1:-O2)):- !,virtualize_code(X,G2,O2),!.
virtualize_code(X,(G1,G2),(O1,O2)):- !,virtualize_code(X,G1,O1),!,virtualize_code(X,G2,O2),!.
virtualize_code(X,\+ G1,call_u(\+ G1)):- virtualize_code(X,G1,O1), \+ same_terms(G1,O1),!.
virtualize_code(X, \+ (G1), \+ (O1)):- !, virtualize_code(X,G1,O1),!.
virtualize_code(X,must(G1),must(O1)):- !, virtualize_code(X,G1,O1),!.
virtualize_code(X,sanity(G1),sanity(O1)):- !, virtualize_code(X,G1,O1),!.
virtualize_code(X,setof(In,G1,Out),setof(In,O1,Out)):- virtualize_code(X,G1,O1),!.
virtualize_code(X,catch(G1,E,G2),catch(O1,E,O2)):- !,virtualize_code(X,G1,O1),!,virtualize_code(X,G2,O2),!.
virtualize_code(_,(G1 \= G2),(G1 \= G2)):-!.
virtualize_code(_,(G1 == G2),(G1 == G2)):-!.
virtualize_code(_,(G1 \== G2),(G1 \== G2)):-!.
virtualize_code(_,(G1 = G2),(G1 = G2)):-!.
virtualize_code(X,(G1;G2),(O1;O2)):- !,virtualize_code(X,G1,O1),!,virtualize_code(X,G2,O2),!.
virtualize_code(X,(G1->G2),(O1->O2)):- !,virtualize_code(X,G1,O1),!,virtualize_code(X,G2,O2),!.
virtualize_code(ge,M:In,ereq(In)):- M==abox,!.
virtualize_code(ge,M:In,M:In):- M==dif,!.
virtualize_code(ge,M:mtProlog(C),M:mtProlog(C)):- M==baseKB,!.

virtualize_code(_,M:In,M:PART):- \+ compound(In),!,In=PART.

/*
virtualize_code(ge,M:In,M:In):- atom(M),callable(In),(predicate_property(M:In,volatile);predicate_property(M:In,thread_local)),!.
virtualize_code(X,M:In,M:Out):- atom(M),
  '$current_source_module'(SM),atom(SM),'$set_source_module'(M),
  must(call_cleanup(virtualize_code(X,In,Out),'$set_source_module'(SM))).

virtualize_code(X,M:In,M:Out):- !, must(virtualize_code(X,In,Out)),!.
*/

virtualize_code(X,M:In,PART):- !, ((functor(In,F,A),virtualize_code_fa(X,M:In,F,A,PART))->true;(M:In=PART)),!.
virtualize_code(X,In,PART):- !, ((functor(In,F,A),virtualize_code_fa(X,In,F,A,PART))->true;In=PART),!.
%virtualize_code(X,In,PART):- must(map_compound_args(virtualize_code(X),In,PART)),!.
% virtualize_code(ge,In,In).
% virtualize_code(_,In,In).
% virtualize_code(X,In,PART):- wdmsg(bad_virtualize_code(X,In,PART)), dtrace.

virtualize_code_fa(X,M:In,F,A,M:PART):-!,virtualize_code_fa(X,In,F,A,PART).
virtualize_code_fa(X,In,_,_,In):- cannot_descend_expansion(X,In),!. % ,fail. % wdmsg(cannot_descend_expansion(X,In))
virtualize_code_fa(X,In,F,A,PART):- get_virtualizer_mode(X,F,A,How),!,must(safe_virtualize(In,How,PART)).
virtualize_code_fa(X,In,F,A,PART):- X==ge, functor(ArgModes,F,A),
  Args=ArgModes,
  virtualize_args_as(Args,ArgModes),!, 
  map_compound_args(virtualize_code_each(X),ArgModes,In,PART),!.

% virtualize_code(X,In,Out):- compound(In), virtualize_special_outside(X,In),!,Out=ereq(In).

virtualize_special_outside(X,In):- functor(In,F,A),get_virtualizer_mode(X,F,A,_How),!.
virtualize_special_outside(X,In):- arg(_,In,Arg), \+cannot_descend_expansion(X,Arg),virtualize_special_outside(X,In).

virtualize_code_each(X,Arg,In,Out):- var(Arg),!,virtualize_code_each(X,(+),In,Out).
virtualize_code_each(X,Arg,In,Out):- (integer(Arg); Arg == +; Arg == * ) -> virtualize_code(X,In,Out),!.
virtualize_code_each(X,-,In,Out):- current_predicate(mpred_expansion_file/0), if_defined(fully_expand_head(X,In,Out)),!.
virtualize_code_each(_,_,In,Out):- must(Out=In).



map_compound_args(Pred,In,Out):- must(( compound(In), In=..[F|InL],maplist(Pred,InL,OutL),Out=..[F|OutL])).

map_compound_args(Pred,Args,In,Out):- must(( compound(Args), compound(In), Args=..[_|ArgsL],In=..[F|InL],maplist(Pred,ArgsL,InL,OutL),Out=..[F|OutL])).


%virtualize_source(X,In,Out):- (ground(In);true;current_prolog_flag(unsafe_speedups,true)),!,virtualize_code(X,In,Out).
%virtualize_source(X,In,Out):- ground(In),!,virtualize_code(X,In,Out).
%virtualize_source(X,In,Out):- callable(In),term_variables(In,List),with_vars_locked(throw,List,virtualize_code(X,In,Out)).
virtualize_source(X,In,Out):- virtualize_code(X,In,Out),!.
  


%% safe_virtualize( Term, +How, -Wrapped) is semidet.
%
% Safely Paying Attention To Corner Cases Wrap.
%

safe_virtualize(Goal,How,Out):- must(safe_virtualize_0(Goal,How,call(MHow,MGoal))),!, 
   safe_univ(Out,[MHow,MGoal]).

safe_virtualize_0(M:Goal,M:How,call(How,M:Goal)).
safe_virtualize_0(M:Goal,How,call(How,M:Goal)).
safe_virtualize_0(Goal,baseKB:How,call(How,Goal)).
safe_virtualize_0(Goal,M:How,call(How,M:Goal)).
safe_virtualize_0(Goal,How,call(How,Goal)).




is_this_file_virtualize_allowed:- prolog_load_context(file,F), is_this_file_virtualize_allowed(F).
is_this_file_virtualize_allowed(F):- get_how_virtualize_file(How, F), !, How \== false.
is_this_file_virtualize_allowed(F):- prolog_load_context(source,S),  S\==F, !, is_this_source_virtualize_allowed(S).
is_this_source_virtualize_allowed(S):- 
             \+ baseKB:how_virtualize_file(heads, S, _), 
             \+ baseKB:how_virtualize_file(false, S, _),
                nop(baseKB:how_virtualize_file(bodies, S)).


% Skip Virtualizing
swc.

% Virtualize
vwc :- throw('Code was missed by virtualizer!').

% always goal expand (and remove it so it wont throw)
:- module_transparent(sd_goal_expansion/4).
sd_goal_expansion(_Head, _, (VWC,In),Out):- vwc==VWC, !, must((callable(In),virtualize_source(ge,In,Out))).
sd_goal_expansion(_Head, In,  _In0  ,Out):- compound(In),
  notrace(is_this_file_virtualize_allowed),
  virtualize_source(ge,In,Out).

:- module_transparent(virtualized_goal_expansion/3).
virtualized_goal_expansion(Head, In, Out):-
  strip_module(In,_,In0),compound(In0),
  (sd_goal_expansion(Head,In,In0,Out)-> 
    nop((( \+ same_terms(In,Out), \+ same_terms(In0,Out)) -> 
      ((
        dmsg( virtualized_goal_expansion(Head,In,_)),
        dmsg( be4 :- In),
        dmsg( out :- Out)))))).


decl_wrapped(M,F,A,How):-
 assert_if_new(rdf_rewrite:arity(F,A)), % TODO puts this in Local Mt
 assert_if_new(baseKB:safe_wrap(M,F,A,How)).
 % once((M==baseKB->true;assert_if_new(baseKB:predicateConventionMt(F,M)))).
:- export(decl_wrapped/4).

%= 	 	 

%% same_terms( ?A, :TermB) is semidet.
%
% Same Terms.
%
same_terms(A,B):-same_term(A,B),!.
same_terms(A,B):-A==B,!.
same_terms(A,B):-A=@=B,!.
same_terms(A,B):-A=@=B,!,A=B.
same_terms(A,B):- \+ \+ A = B,!,fail.
same_terms(A,B):- ( \+ compound(A) ; \+ compound(B)),!,fail.
same_terms(M:A,B):-atom(M),!,same_terms(A,B).
same_terms(A,M:B):-atom(M),!,same_terms(A,B).
same_terms(In,on_x_debug(Out)):- !, same_terms(In,Out).
same_terms(on_x_debug(Out),In):- !, same_terms(In,Out).
same_terms(In,dbreq(Out)):- !, same_terms(In,Out).
same_terms(A=..[P|AA],cnas(B,P,BB)):-!,same_terms(A,B),same_terms(AA,BB).
same_terms((A,AA),(B,BB)):-!,same_terms(A,B),same_terms(AA,BB).
same_terms([A|AA],[B|BB]):-!,same_terms(A,B),same_terms(AA,BB).
same_terms((A;AA),(B;BB)):-!,same_terms(A,B),same_terms(AA,BB).
same_terms((A:-AA),(B:-BB)):-!,same_terms(A,B),same_terms(AA,BB).
same_terms(AAA,BBB):-  AAA=..[F|AA],BBB=..[F|BB],!,same_terms(AA,BB).



:- fixup_exports.

:- if(false).

:- multifile(system:file_body_expansion/2).
:-   dynamic(system:file_body_expansion/2).
% % % OFF :- system:use_module(system:library(subclause_expansion)).
system:file_body_expansion(Head,In,Out):- compound(In), 
  is_file_virtualize_allowed,   
  virtualized_goal_expansion(Head,In,Out).
  
:- else.

:- multifile(system:goal_expansion/4).
:- dynamic(system:goal_expansion/4).
:- module_transparent(system:goal_expansion/4).

system:goal_expansion(In,P,Out,PO):-
     notrace((compound(In), nonvar(P))),
     notrace((get_current_clause(Head :- FileTerm),In == FileTerm)),
     virtualized_goal_expansion(Head,In,Out) -> PO=P.

:- endif.

