/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles logicmoo@gmail.com ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2021/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
:- if((prolog_load_context(source,F),prolog_load_context(file,F))).
:- module(must_sanity,[]).
:- endif.
:- use_module(logicmoo_startup).
:- define_into_module(          
   [
      must/1, % Goal must succeed at least once once
      must_once/1, % Goal must succeed at most once
      must_det/1, % Goal must succeed determistically
      sanity/1,  % like assertion but adds trace control
      %nop/1, % syntactic comment
      scce_orig/3,
      must_or_rtrace/1
    ]).
%:- endif.
/** <module> Utility LOGICMOO_MUST_SANITY
This module includes predicate utilities that allows program to detect unwanted failures. 
@author Douglas R. Miles
@license LGPL
*/

%:- discontiguous '$exported_op'/3.
:- meta_predicate
        must(:),
        must_once(:),
        must_det(:),
        nop(*),
        sanity(:),
        %must_or_rtrace_mep(M,E,*),
        scce_orig(:,:,:).

:- set_module(class(library)).
% % % OFF :- system:use_module(library(logicmoo_utils_all)).

:- system:reexport(library(debug),[debug/3]).
:- system:reexport(library(logicmoo_common)).
 
% TODO Make a speed,safety,debug Triangle instead of these flags
:- create_prolog_flag(runtime_must,debug,[type(term)]).


%! must(:Goal) is nondet.
%
% Goal must succeed at least once once
%
% Wrap must/1 over parts of your code you do not trust
% If your code fails.. it will rewind to your entry block (at the scope of this declaration) and invoke rtrace/1 .
% If there are 50 steps to your code, it will save you from pushing `creep` 50 times.  
% Instead it turns off the leash to allow you to trace with your eyeballs instead of your fingers.
%
%% must( :Goal) is semidet.
%
% Must Be Successfull.
%

must(MGoal):- (call(MGoal)*->true;must_0(MGoal)).
must_0(MGoal):- quietly(get_must(MGoal,Must))-> call(Must).



:- meta_predicate(deterministic_tf(:,-)).
deterministic_tf(G, F) :-
   G,
   deterministic(F),
   otherwise. /* prevent tail recursion */

:- meta_predicate(was_cut(+)).
was_cut(Cut):- nonvar(Cut), strip_module(Cut,_,(!)).

:- meta_predicate(mor_event(:)).

handle_mor_event(e(M,E,Err,G)):- !, call_cleanup(handle_mor_event(e(Err,G)),wdmsg(mor_e(M,E,Err,G))).
handle_mor_event(f(M,E,G)):- !, call_cleanup(handle_mor_event(f(G)),wdmsg(mor_f(M,E,G))).
handle_mor_event(e(E,_)):- !, handle_mor_event(E). 
handle_mor_event(e(Err,G)):- 
   wdmsg(mor_e(Err,G)), dumpST,!, 
   wdmsg(mor_e(Err,G)), ignore(rtrace(G)),
   throw(Err).

handle_mor_event(f(G)):- notrace(t_l:rtracing),!,wdmsg(warn(f0(G))),G.
handle_mor_event(f(G)):- 
   wdmsg(f1(G)), dumpST,!,
   wdmsg(f2(G)), rtrace(G),!,
   wdmsg(failed_must_or_rtrace(i3,G)), dtrace(G).

mor_event(E):- handle_mor_event(E).
mor_event(E):- throw(E).

:- meta_predicate(must_or_rtrace_mep(+,0,:)).
must_or_rtrace_mep(M,E,(G1,Cut)):- was_cut(Cut),!,must_or_rtrace_mep(M,E,G1),!.
must_or_rtrace_mep(M,E,(G1,Cut,G2)):- was_cut(Cut),!,must_or_rtrace_mep(M,E,G1),!,must_or_rtrace_mep(M,G1,G2).
must_or_rtrace_mep(M,E,(G1,G2)):- !, (G1*->G2;throw(f(M,E,G1))).
must_or_rtrace_mep(M,E,P):- predicate_property(P,number_of_clauses(_)),!,
   findall(B,clause(P,B),Bs),!,(Bs==[]->throw(f(M,E,P));(mor_list_to_disj(fail,Bs,ORs),(ORs*->throw(f(M,E,P))))).
must_or_rtrace_mep(M,E,G):- catch(G,Er,throw(e(M,E,Er,G)))*->true;throw(f(M,E,G)).

mor_list_to_disj(_,[X],X):-!.
mor_list_to_disj(L,[A|B],(A;BB)):- mor_list_to_disj(L,B,BB).
mor_list_to_disj(End,[],End):-!.

:- meta_predicate(must_or_rtrace(:)).

must_or_rtrace(G):- tracing,!,call(G).
must_or_rtrace((G1,Cut)):- was_cut(Cut),!,must_or_rtrace(G1),!.
must_or_rtrace((G1,Cut,G2)):- was_cut(Cut),!,must_or_rtrace(G1),!,must_or_rtrace(G2).
must_or_rtrace((G1,G2)):- !,( catch(G1,Ex,mor_event(e(Ex,G1)))*->must_or_rtrace(G2);mor_event(f(G1))).
must_or_rtrace(G):- catch(G,Ex,mor_event(e(Ex,G)))*-> true; mor_event(f(G)).

%:- export(notrace/1).
%:- meta_predicate(notrace(:)).
%notrace(G):- call(G).
:- redefine_system_predicate(system:notrace/1).
:- '$hide'(system:notrace/1).

%must_or_rtrace_mep(M,E,G):- get_must_l(G,Must),!,call(Must).
%must_or_rtrace_mep(M,E,G):- catch(G,Err,(dmsg(error_must_or_rtrace(Err)->G),ignore(rtrace(G)),throw(Err))) *->true; ftrace(G).

%% get_must( ?Goal, ?CGoal) is semidet.
%
% Get Must Be Successfull.
%

get_must(Goal,CGoal):- (skipWrapper ; tlbugger:skipMust),!,CGoal = Goal.
%get_must(M:Goal,M:CGoal):- must_be(nonvar,Goal),!,get_must(Goal,CGoal).
get_must(quietly(Goal),quietly(CGoal)):- current_prolog_flag(runtime_safety,3), !, get_must(Goal,CGoal).
get_must(quietly(Goal),CGoal):- !,get_must((quietly(Goal)*->true;Goal),CGoal).
get_must(Goal,CGoal):- keep_going,!,CGoal=must_keep_going(Goal).
get_must(Goal,CGoal):- hide_non_user_console,!,get_must_type(rtrace,Goal,CGoal).
get_must(Goal,CGoal):- current_prolog_flag(runtime_must,How), How \== none, !, get_must_type(How,Goal,CGoal).
get_must(Goal,CGoal):- current_prolog_flag(runtime_debug,2), !, 
   (CGoal = (on_x_debug(Goal) *-> true; debugCallWhy(failed(on_f_debug(Goal)),Goal))).
get_must(Goal,CGoal):- get_must_l(Goal,CGoal).

get_must_l(Goal,CGoal):-
   (CGoal = (catchv(Goal,E,
     ignore_each(((dumpST_error(must_ERROR(E,Goal)), %set_prolog_flag(debug_on_error,true),
         rtrace(Goal),nortrace,dtrace(Goal),badfood(Goal)))))
         *-> true ; (dumpST,ignore_each(((dtrace(must_failed_F__A__I__L_(Goal),Goal),badfood(Goal))))))).


get_must_type(speed,Goal,Goal).
get_must_type(warning,Goal,show_failure(Goal)).
get_must_type(fail,Goal,Goal).
get_must_type(rtrace,Goal,on_f_rtrace(Goal)).
get_must_type(keep_going,Goal,must_keep_going(Goal)).
get_must_type(retry,Goal,must_retry(Goal)).
get_must_type(How,Goal,CGoal):- 
     (How == assertion -> CGoal = (Goal*->true;call(prolog_debug:assertion_failed(fail, must(Goal))));
     (How == error ; true ) 
       -> CGoal = (Goal*-> true; (rtrace(Goal),throw(failed_must(Goal))))).

must_retry(Call):- 
   (repeat, (catchv(Call,E,(dmsg(E:Call),fail)) *-> true ; 
      catch((ignore(rtrace(Call)),leash(+all),visible(+all),
        repeat,wdmsg(failed(Call)),trace,Call,fail),'$aborted',true))).

must_keep_going(Goal):- 
 locally(set_prolog_flag(debug_on_error,false),
  ((catch(Goal,E,
      notrace(((dumpST_error(sHOW_MUST_go_on_xI__xI__xI__xI__xI_(E,Goal)),ignore(rtrace(Goal)),badfood(Goal)))))
            *-> true ;
              notrace(dumpST_error(sHOW_MUST_go_on_failed_F__A__I__L_(Goal))),ignore(rtrace(Goal)),badfood(Goal)))).


:- '$hide'(get_must/2).


%! sanity(:Goal) is det.
%
% Optional Sanity Checking.
%
% like assertion/1 but adds trace control
%

sanity(_):- notrace(current_prolog_flag(runtime_safety,0)),!.
sanity(_):-!.
sanity(Goal):- \+ ( nb_current('$inprint_message', Messages), Messages\==[] ),
   \+ tracing,
   \+ current_prolog_flag(runtime_safety,3),
   \+ current_prolog_flag(runtime_debug,0),
   (current_prolog_flag(runtime_speed,S),S>1),
   !, (1 is random(10)-> must(Goal) ; true).
sanity(Goal):- (current_prolog_flag(debug,true)->quietly(Goal);nop(Goal)),!.
sanity(Goal):- keep_going,!,dmsg(failed_sanity(Goal)=keep_going),fail.
sanity(_):- dumpST,break,fail.
sanity(Goal):- ignore(setup_call_cleanup(wdmsg(begin_FAIL_in(Goal)),rtrace(Goal),wdmsg(end_FAIL_in(Goal)))),!,dtrace(assertion(Goal)).

%! must_once(:Goal) is det.
%
% Goal must succeed at most once
%
must_once(Goal):- must(Goal),!.


%! must_det(:Goal) is det.
%
% Goal must succeed determistically
%

% must_det(Goal):- current_prolog_flag(runtime_safety,0),!,must_once(Goal).
must_det(Goal):- \+ current_prolog_flag(runtime_safety,3),!,must_once(Goal).
must_det(Goal):- must_once(Goal),!.
/*
must_det(Goal):- must_once((Goal,deterministic(YN))),(YN==true->true;dmsg(warn(nondet_exit(Goal)))),!.
must_det(Goal):- must_once((Goal,deterministic(YN))),(YN==true->true;throw(nondet_exit(Goal))).
*/

:- redefine_system_predicate(system:nop/1).
:- abolish(system:nop/1),asserta(system:nop(_)).
%! nop( :Goal) is det.
%
%  Comments out code without losing syntax
%



/*
scce_orig(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 

:- abolish(system:scce_orig,3).


[debug]  ?- scce_orig( (writeln(a),trace,start_rtrace,rtrace) , (writeln(b),member(X,[1,2,3]),writeln(c)), writeln(d)).
a
b
c
d
X = 1 ;
a
c
d
X = 2 ;
a
c
d
X = 3.


*/
%:- meta_predicate(mquietly(?)).
:- module_transparent(mquietly/1).
:- export(mquietly/1).
%:- system:import(mquietly/1).
mquietly(Var):- var(Var),!,trace_or_throw(var_mquietly(Var)).
%mquietly((G1,G2)):- !, call(G1),mquietly(G2).
%mquietly((G1;G2)):- !, call(G1);mquietly(G2).
%mquietly(M:(G1,G2)):- !, call(M:G1),mquietly(M:G2).
%mquietly(M:(G1;G2)):- !, call(M:G1);mquietly(M:G2).
mquietly(G):- call(G).

:- '$hide'(mquietly/1).
%:- '$hide'(mquietly/2).

mquietly_if(false,_):- !.
mquietly_if(_,G):- mquietly(G).


scce_orig(Setup,Goal,Cleanup):- 
   HdnCleanup = mquietly_if(true,Cleanup),   
   setup_call_cleanup(Setup, 
     ((Goal,deterministic(DET)),
        (notrace(DET == true) 
          -> ! 
          ;((Cleanup,notrace(nb_setarg(1,HdnCleanup,false)))
             ;(Setup,notrace(nb_setarg(1,HdnCleanup, true)),notrace(fail))))),
        HdnCleanup).


scce_orig1(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, notrace(deterministic(DET))),
       '$sig_atomic'(Cleanup),
         (notrace(DET == true) -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 

scce_orig0(Setup0,Goal,Cleanup0):-
  notrace((Cleanup = notrace('$sig_atomic'(Cleanup0)),Setup = notrace('$sig_atomic'(Setup0)))),
   \+ \+ Setup, !,
   (catch(Goal, E,(Cleanup,throw(E)))
      *-> (tracing->(deterministic(DET));deterministic(DET)); (Cleanup,!,fail)),
     Cleanup,
     (notrace(DET == true) -> ! ; (true;(Setup,fail))).

'my_set_predicate_attribute'(M:F/A,B,C):- functor(P,F,A),'my_set_predicate_attribute'(M:P,B,C),!.
'my_set_predicate_attribute'(F/A,B,C):- functor(P,F,A),'my_set_predicate_attribute'(P,B,C),!.

'my_set_predicate_attribute'(A,B,C):-
  current_prolog_flag(access_level,system),!,
  'my_set_predicate_attribute2'(A,B,C).
'my_set_predicate_attribute'(A,B,C):- 
  current_prolog_flag(access_level,Was),
  setup_call_cleanup(set_prolog_flag(access_level,system),
    'my_set_predicate_attribute2'(A,B,C),set_prolog_flag(access_level,Was)).
  
'my_set_predicate_attribute2'(A,B,C):- 
  redefine_system_predicate(A), '$set_predicate_attribute'(A,B,C),!.


%:- '$hide'(scce_orig/3).
%:- 'my_set_predicate_attribute'(scce_orig(_,_,_), hide_childs, true).

%:- 'my_set_predicate_attribute'(notrace/1, hide_childs, true).

%:- '$hide'(system:setup_call_catcher_cleanup/4).
%:- 'my_set_predicate_attribute'(system:setup_call_catcher_cleanup/4, hide_childs, false).

%:- redefine_system_predicate(call_cleanup(_,_)).
%:- '$hide'(system:call_cleanup/2).
%:- 'my_set_predicate_attribute'(call_cleanup/2, hide_childs, false).


scce_orig2(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup), 
   catch( 
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))), 
      E, 
      ('$sig_atomic'(Cleanup),throw(E))). 



% % % OFF :- system:reexport(library('debuggery/first')).
% % % OFF :- system:reexport(library('debuggery/ucatch')).
% % % OFF :- system:reexport(library('debuggery/dmsg')).
% % % OFF :- system:reexport(library('debuggery/rtrace')).
% % % OFF :- system:reexport(library('debuggery/bugger')).
% % % OFF :- system:reexport(library('debuggery/dumpst')).
% % % OFF :- system:reexport(library('debuggery/frames')).



:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

 


