:- module(lockable_vars,[
   lock_vars/1,
   unlock_vars/1,
   with_vars_unlocked/1,
   with_vars_relocked/1,
   with_vars_locked/1,
   with_vars_locked/2,
   with_some_vars_locked/2,
   with_vars_locked/3,
   with_vars_locked_old/2,
   with_vars_locked_old/3,
   with_vars_locked_trusted/3,
   with_quiet_vars_lock/1,
   with_vars_lock_else/2,
   skip_varlocks/0]).

/** <module> Utility LOGICMOO LOCKABLE VARS
This module prevents bugs due accidental tampering, allows one to write code that keeps free variables from being accidently tampered with unification one way.
 
@author Douglas R. Miles
@license LGPL 
*/ 
:- set_module(class(library)).

%:- use_module(util_varnames,[get_var_name/2]).


%% lock_vars( :TermVar) is semidet.
%
% Lock Variables.
%

lock_vars(Term):-lock_vars(lockable_vars:just_fail,Term).

just_fail(_):- notrace( \+ skip_varlocks),!.
just_fail(_).
%skip_varlocks:- !.
skip_varlocks:- current_prolog_flag(skip_varlocks , TF),!,TF==true.
skip_varlocks:- current_prolog_flag(unsafe_speedups , true) ,!.

:- set_prolog_flag(skip_varlocks,false).

:- meta_predicate(lock_vars(1,+)).
lock_vars(_Notify,Var):- notrace(skip_varlocks; Var==[]),!.
%lock_vars(Notify,[Var]):- !, put_attr(Var,vl,when_rest(Notify,1,Var,vv(Var))).
lock_vars(Notify,[Var|Vars]):- !, PVs=..[vv|[Var|Vars]], 
 lock_these_vars_now(Notify,1,[Var|Vars],PVs),!.

lock_vars(Notify,Term):- term_variables(Term,Vs),lock_vars(Notify,Vs).

lock_these_vars_now(Notify,N0,[Var|Vars],PVs):-!,
  ignore((var(Var),
    put_attr(Var,vl,when_rest(Notify,N0,Var,PVs)))), 
   N is N0+1,
   lock_these_vars_now(Notify,N,Vars,PVs).
lock_these_vars_now(_,_,[],_).

vl:attr_unify_hook(InSLock,Value):-
  current_prolog_flag(skip_varlocks,true)-> true;
  with_vars_unlocked(vl_attr_unify_hook(InSLock,Value)).

vl_attr_unify_hook(InSLock,Value):- compound(InSLock), InSLock = slock(InLock,Else,Sorted),!,
  check_slock(InLock,Else,InSLock,Sorted,Value).
vl_attr_unify_hook(A,B):- vlauh(A,B),!.

vlauh(when_rest(Notify,N,_Var,VVs),VarValue):- 
    arg(NN,VVs,Was),Was==VarValue,
    NN\==N,!,
    dmsg(collide_locked_var(Notify,VarValue)),
    call(Notify,VarValue).

%vlauh(when_rest(_,_,Var,_),VarValue):- unify_name_based0(Var, VarValue).
%vlauh(_,VarValue):- locking_verbatum_var(VarValue),!,variable_name_or_ref(VarValue,_),!.

vlauh(_,_):- current_prolog_flag(skip_varlocks,true),!.

vlauh(_,_):- on_x_fail(( \+ thread_self_main)),!,fail.
%vlauh(_,_):- thread_self_main,!.
vlauh(when_rest(Notify,N,Var,VVs),VarValue):- 
  \+ (var(VarValue);locking_verbatum_var(VarValue)),!,               
  dmsg(error_locked_var1(when_rest(Notify,N,Var,VVs),VarValue)),
  (current_prolog_flag(debugg,true)->((dumpST,
  dmsg(error_locked_var2(when_rest(Notify,N,Var,VVs),VarValue))));true),
  (current_prolog_flag(debugg,true)->break;true),
  call(Notify,VarValue),!.

vlauh(when_rest(Notify,N,Var,VVs),VarValue):- var(VarValue),!,
      (get_attr(VarValue,vl,when_rest(Notify,N,Var,VVs))
      -> fail ; 
         put_attr(VarValue,vl,when_rest(Notify,N,Var,VVs))).

vlauh(_,VarValue):- locking_verbatum_var(VarValue),!,variable_name_or_ref(VarValue,_),!.


% move to logicmoo_utils_common.pl? 
locking_verbatum_var(Var):-var(Var),!,fail.
locking_verbatum_var('$VAR'(_)).
locking_verbatum_var('aVar'(_)).
locking_verbatum_var('aVar'(_,_)).

:- thread_local(t_l:varname_lock/1).

unify_name_based0(Var1, Var2):- \+ atom(Var1),variable_name_or_ref(Var1,Name),!,unify_name_based0(Name, Var2).
unify_name_based0(Name1, Var):- if_defined(t_l:var_locked(What),fail),!,((get_var_name(Var,Name2),Name1==Name2)->true;call(What,Var)).
unify_name_based0(_Form, _OtherValue):- local_override(no_kif_var_coroutines,G),!,call(G).
unify_name_based0(Name1, Var):-  get_var_name(Var,Name2),!,Name1=Name2,!.
unify_name_based0(Name1, Var):- get_attr(Var, vn, Name2),!,combine_varnames(Name1,Name2,Name),(Name2==Name->true;put_attr(Var,vn,Name)).
unify_name_based0(Name1, Var):- var(Var),!,put_attr(Var, vn, Name1).
unify_name_based0(_, Var):- nonvar(Var),!.
%unify_name_based0(_, Var):- cyclic_term(Var),!,fail.
%unify_name_based0(_, Var):- cyclic_term(Var),!.
%unify_name_based0(_, Var):- cyclic_break(Var),!,fail.
unify_name_based0(_Form, _OtherValue):-!.

combine_varnames(Name1,Name2,Name1):-Name1==Name2,!.
combine_varnames(Name1,Name2,Name):-
 ((atom_concat(_,Name1,Name2);atom_concat(Name1,_,Name2)) -> Name=Name2 ; (
   ((atom_concat(Name2,_,Name1);atom_concat(_,Name2,Name1)) -> Name=Name1 ; (
   (atomic_list_concat([Name2,'_',Name1],Name)))))).


%% unlock_vars( :TermOrVar) is semidet.
%
% Unlock Variables.
%

%unlock_vars(_Var):- notrace(skip_varlocks),!.
unlock_vars(Term):- must(quietly((term_attvars(Term,Vs),maplist(delete_vl,Vs)))).

delete_vl( Var):- var(Var),!, del_attr(Var,vl).
delete_vl( Term):- term_attvars(Term,Vs),maplist(delete_vl,Vs).

% % % OFF :- system:use_module(library(logicmoo/each_call)).

:- meta_predicate(with_vars_locked_old(1,:)).
with_vars_locked_old(Notify,Goal):- term_variables(Goal,Vs),with_vars_locked_old(Notify,Vs,Goal).

:- meta_predicate(with_vars_locked_old(1,?,:)).
with_vars_locked_old(_Notify,_Vs,Goal):- notrace(skip_varlocks),!,Goal.
with_vars_locked_old(Notify,Vs0,Goal):- term_variables(Vs0,Vs),with_vars_locked_trusted(Notify,Vs,Goal).

:- meta_predicate(with_vars_locked_trusted(1,?,:)).
with_vars_locked_trusted(_Notify,_Vs,Goal):- notrace(skip_varlocks),!,Goal.
with_vars_locked_trusted(Notify,Vs,Goal):- set_prolog_flag(access_level,system),
 trusted_redo_call_cleanup(
   lock_vars(Notify,Vs),
      (nop(trace),Goal),
     maplist(delete_vl,Vs)).


:- thread_local(t_l:on_vars_lock_failed/1).

:- meta_predicate(with_vars_locked(:)).
with_vars_locked(Goal):- with_vars_locked(Goal,Goal).

:- meta_predicate(with_some_vars_locked(+,:)).
with_some_vars_locked(_Vars,Goal):-!, Goal.
with_some_vars_locked(Vars,Goal):-
  with_vars_locked(Vars,Goal) *-> true ; Goal.

:- meta_predicate(with_vars_locked(+,:)).
with_vars_locked(Vars,Goal):- 
   term_variables(Vars,Vs),sort(Vs,Sorted),!,   
   with_vars_slocked(lookup_how,Sorted,Goal).
   %set_prolog_flag(access_level,system), 

:- meta_predicate(with_vars_unlocked(:)).
with_vars_unlocked(Goal):- 
  locally(current_prolog_flag(skip_varlocks,true), Goal).

:- meta_predicate(with_vars_relocked(0)).
with_vars_relocked(Goal):- 
  locally(current_prolog_flag(skip_varlocks,false), Goal).

:- meta_predicate(with_vars_locked(1,+,:)).
with_vars_locked(Else,Vars,Goal):- 
   term_variables(Vars,Vs),sort(Vs,Sorted),!,
   with_vars_slocked(Else,Sorted,Goal).
   %set_prolog_flag(access_level,system), 

vl1:attr_unify_hook(InLock,Value):- with_vars_unlocked(vl1_attr_unify_hook(InLock,Value)).
  
vl1_attr_unify_hook(InLock,Value):- 
  (var(Value)
    -> put_attr(Value,vl1,InLock)
    ; (InLock=vlock(YesNo,Else),
       (YesNo==no->true;Else))).

with_vars_slocked(_Else,Sorted,Goal):- notrace(skip_varlocks;Sorted==[]),!,call(Goal).
with_vars_slocked(Else,[Var],Goal):- fail,!,
 InLock = vlock(no,call(Else,Var)),
 setup_call_cleanup(
   put_attr(Var,vl1,InLock),
   call_in_lock(InLock,Goal),
   del_attr(Var,vl1)).

with_vars_slocked(Else,Sorted,Goal):-
   InLock = slock(no,Else,Sorted),
   setup_call_cleanup(
      maplist(lock_as_sorted(InLock),Sorted),
      call_in_lock(InLock,Goal),
      maplist(unlock_as_sorted(InLock),Sorted)).

call_in_lock(InLock,Goal):- 
  %set_prolog_flag(access_level,system),
   trusted_redo_call_cleanup(
    nb_setarg(1,InLock,yes),
    Goal,
    nb_setarg(1,InLock,no)).

lock_as_sorted(InLock,Var):- put_attr(Var,vl,InLock).
unlock_as_sorted(InLock,Var):-  ignore((attvar(Var),get_attr(Var,vl,InLockW),InLock==InLockW,del_attr(Var,vl))).

:- meta_predicate(check_slock(+,:,+,+,+)).
check_slock(no,_Else,InSLock,_Sorted,Value):- !,
  ((var(Value), \+ get_attr(Value,vl,_)) -> put_attr(Value,vl,InSLock);  true).
check_slock(yes,Else,InSLock,Sorted,Value):- 
  (test_slock(yes,Else,InSLock,Sorted,Value)-> true ; 
    failed_slock(yes,Else,InSLock,Sorted,Value)),!.

:- meta_predicate(failed_slock(+,:,+,+,+)).
failed_slock(yes,_:Else,_InSLock,_Sorted,_Value):- builtin_slock(Else),!,fail.
failed_slock(yes,_:lookup_how,InSLock,Sorted,Value):-!,
  (t_l:on_vars_lock_failed(Else)-> failed_slock(yes,Else,InSLock,Sorted,Value);fail).
failed_slock(yes,Else,InSLock,_Sorted,_Value):- trace,call(Else,InSLock),fail.

:- meta_predicate(builtin_slock(*)).
builtin_slock(just_fail):- !.
builtin_slock(fail):- !.
builtin_slock(trace):- !,trace.
builtin_slock(dbreak):- !,dbreak.

:- meta_predicate(with_quiet_vars_lock(:)).
with_quiet_vars_lock(M:G):- with_vars_lock_else(M:just_fail,M:G).

:- meta_predicate(with_vars_lock_else(1,:)).
with_vars_lock_else(Else,M:G):- 
  locally(t_l:on_vars_lock_failed(M:Else),M:G).


test_slock(yes,_Else,InSLock,Sorted,Value):-
  var(Value), 
  sort(Sorted,SortedW1),Sorted==SortedW1,
  (get_attr(Value,vl,slock(_VYN,_VElse,VSorted)) 
      -> VSorted \== Sorted ; put_attr(Value,vl,InSLock)).

  

/*vl:attr_unify_hook(InSLock,Value):- assertion(InSLock = slock(InLock,Else,Sorted)),
  InSLock = slock(InLock,Else,Sorted),
  check_slock(InLock,Else,InSLock,Sorted,Value).
*/
