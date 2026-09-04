% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_no_repeats.pl
:- module(no_repeats,
          [ /*memberchk_pred/3,
            memberchk_pred_rev/3,
            memberchk_same/2,
            memberchk_same0/2,
            memberchk_same1/2,
            memberchk_same2/2,
            memberchk_same3/2,
            memberchk_cmp/3,
            memb_r/2,*/
            must_not_repeat/1,
            no_repeats/1,
            no_repeats/2,
/*
            no_repeats_av/0,
            no_repeats_findall5/5,
            no_repeats_findall_r/5,
            no_repeats_old/1,         
            no_repeats_old/2,
            no_repeats_cmp/3,
            no_repeats_save/2,
            no_repeats_save/4,
            no_repeats_u/2,
            
            succeeds_n_times/2,
            nr_test/2,
            */
            subtract_eq/3,
            no_repeats_var/1
%            loop_check_nr/1
          ]).

/** <module> Utility LOGICMOO NO REPEATS
	No repeats allows each indiv answer returned by prolog to be unique. Removes duplicate answers. 
- @author Douglas R. Miles
- @license LGPL 
*/
:- meta_predicate
        memberchk_pred(2, ?, ?),
        memberchk_pred_rev(2, ?, ?),
        must_not_repeat(0),
        no_repeats(0),
        no_repeats(+, 0),
        no_repeats_findall5(+, 0, -, -, -),
        no_repeats_findall_r(+, 0, -, -, -),
        no_repeats_old(0),
        no_repeats_old(+, 0),
        no_repeats_cmp(2, +, 0),
        no_repeats_save(+, 0),
        no_repeats_save(+, 0, -, -),
        no_repeats_u(+, 0),
        loop_check_nr(0),
        succeeds_n_times(0, -).
:- module_transparent
        memberchk_same/2,
        no_repeats_av/0,
        no_repeats_cmp/3,
        subtract_eq/3,
        nr_test/2.

      
:- set_module(class(library)).

%:- prolog_load_context(directory,Dir),add_file_search_path_safe(library,Dir).
% % % OFF :- system:use_module(library(logicmoo/misc_terms)).

%% loop_check_nr( ?CL) is semidet.
%
% Loop Check Nr.
%
loop_check_nr(CL):- loop_check(no_repeats(CL)).


% ===================================================================

:- thread_local  tlbugger:attributedVars/0.

%  tlbugger:attributedVars.

:- export(must_not_repeat/1).
% = :- meta_predicate(must_not_repeat(0)).



%% must_not_repeat( :GoalC) is semidet.
%
% Must Be Successfull Not Repeat.
%
must_not_repeat(C):-call(C).

% ===================================================
%
% no_repeats(:Call)
%
% Like call/1 but ony succeeds only unique variabes
%
% logicmoo_mud:  ?- no_repeats(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.
% ===================================================







%% no_repeats_av is semidet.
%
% No Repeats Attributed Variables.
%
no_repeats_av:-tlbugger:attributedVars.

:- export(no_repeats/1).
:- meta_predicate no_repeats(0).

% no_repeats(Call):- tlbugger:old_no_repeats,!, no_repeats_old(Call).
%no_repeats(Call):- no_repeats_av,!,no_repeats_av(Call).



%% no_repeats( :Goal) is semidet.
%
% No Repeats.
%
no_repeats(Call):- no_repeats_old(Call).


:- export(no_repeats/2).
:- meta_predicate no_repeats(+,0).
%no_repeats(Vs,Call):- tlbugger:old_no_repeats,!,no_repeats_old(Vs,Call).
%no_repeats(Vs,Call):- no_repeats_av,!,no_repeats_av(Vs,Call).



%% no_repeats( +Vs, :Goal) is semidet.
%
% No Repeats.
%
no_repeats(Vs,Call):- no_repeats_old(Vs,Call).

/*
no_repeats_dif(Vs,Call):- dif(Vs,_), get_attr(Vs,dif,vardif(CONS,_)),!,
  Call,copy_term_nat(Vs,C),nb_setarg(2, CONS, [_-C|CONS]).

*/

% ===================================================
%
% no_repeats_old([+Vars,]:Call)
%
% Like call/1 but ony succeeds on unique free variabes
%
% logicmoo_mud:  ?- no_repeats( X , member(X-Y,[3-2,1-4,1-5,2-1])).
% X = 3, Y = 2 ;
% X = 1, Y = 4 ;
% X = 2, Y = 1.
% ===================================================
:- export(no_repeats_old/1).
:- meta_predicate no_repeats_old(0).



%% no_repeats_old( :Goal) is semidet.
%
% No Repeats Old.
%
no_repeats_old(Call):- no_repeats_old(Call,Call).


% % % % OFF :- system:use_module(rec_lambda).




%% memberchk_same( ?X, :TermY0) is semidet.
%
% Memberchk Same.
%
memberchk_same(X, List) :- is_list(List),!, \+ atomic(List), C=..[v|List],(var(X)-> (arg(_,C,YY),X==YY) ; (arg(_,C,YY),X =@= YY)),!.
memberchk_same(X, Ys) :-  nonvar(Ys), var(X)->memberchk_same0(X, Ys);memberchk_same1(X,Ys).
memberchk_same0(X, [Y|Ys]) :-  X==Y  ; (nonvar(Ys),memberchk_same0(X, Ys)).
memberchk_same1(X, [Y|Ys]) :-  X =@= Y ; (nonvar(Ys),memberchk_same1(X, Ys)).

memberchk_same2(X, List) :- Hold=hold(List), !,
        repeat, (arg(1,Hold,[Y0|Y0s]) ->
          ( X==Y0-> true; (nb_setarg(1,Hold,Y0s),fail)) ; (! , fail)).

memberchk_same3(X, List) :- Hold=hold(List), !,
        repeat, (arg(1,Hold,[Y0|Y0s]) ->
          ( X=@=Y0-> true; (nb_setarg(1,Hold,Y0s),fail)) ; (! , fail)).

memb_r(X, List) :- Hold=hold(List), !, trace_or_throw(broken_memb_r(X, List)),
         repeat,
          ((arg(1,Hold,[Y|Ys]),nb_setarg(1,Hold,Ys)) -> X=Y ; (! , fail)).








%% memberchk_pred( :PRED2Pred, ?X, ?Y0) is semidet.
%
% Memberchk Predicate.
%
memberchk_pred(Pred, X, [Y0|Ys]) :- is_list(Ys),C=..[v,Y0|Ys],!, arg(_,C,Y), call(Pred,X,Y),!.
memberchk_pred(Pred, X, [Y|Ys]) :- (   call(Pred,X,Y) -> true ;   (nonvar(Ys),memberchk_pred(Pred, X, Ys) )).



%% memberchk_pred_rev( :PRED2Pred, ?X, ?Y0) is semidet.
%
% Memberchk Predicate Rev.
%
memberchk_pred_rev(Pred, X, [Y0|Ys]) :- is_list(Ys),C=..[v,Y0|Ys],!, arg(_,C,Y), call(Pred,Y,X),!.
memberchk_pred_rev(Pred, X, [Y|Ys]) :- (   call(Pred,Y,X) -> true ;   (nonvar(Ys),memberchk_pred_rev(Pred,X, Ys) )).

:- export(no_repeats_old/2).
:- meta_predicate no_repeats_old(+,0).



%% no_repeats_old( +Vs, :Goal) is semidet.
%
% No Repeats Old.
%
no_repeats_old(Vs,Call):- ground(Vs),!,Call,!.
no_repeats_old(Vs,Call):- CONS = [_], (Call), 
   quietly(( \+ memberchk_same(Vs,CONS), copy_term(Vs,CVs), CONS=[_|T], nb_setarg(2, CONS, [CVs|T]))).

% mcs_t2(A,B) :- call(lambda(X, [Y|Ys], (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),reenter_lambda(X, Ys) ))),A,B).
% mcs_t(A,B) :- call(lambda(X, [Y|Ys], (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),reenter_lambda(X, Ys) ))),A,B).

no_repeats_cmp(_,Vs,Call):- ground(Vs),!,Call,!.
no_repeats_cmp(Cmp,Vs,Call):- CONS = [zzzZZZZzzzzZZZ], (Call), 
   quietly(( \+ memberchk_cmp(Cmp,Vs,CONS), copy_term(Vs,CVs), CONS=[_|T], nb_setarg(2, CONS, [CVs|T]))).
:- export(no_repeats_cmp/3).
memberchk_cmp(Cmp,Vs,CONS):-
   member(XY,CONS),call(Cmp,Vs,XY),!.

/*
:- meta_predicate no_repeats_t(?,0).
no_repeats_t(Vs,Call):- CONS = [_], (Call), (( \+ call(lambda(X, [Y|Ys], (   X =@= Y ->  (var(X) -> X==Y ; true) ;   (nonvar(Ys),reenter_lambda(X, Ys) ))),Vs,CONS), copy_term(Vs,CVs), CONS=[_|T], nb_linkarg(2, CONS, [CVs|T]))).
*/

%

:- export(no_repeats_u/2).
:- meta_predicate no_repeats_u(+,0).



%% no_repeats_u( +Vs, :Goal) is semidet.
%
% No Repeats For User Code.
%
no_repeats_u(Vs,Call):- CONS = [_], (Call), /*quietly*/((  CONS=[_|T],
    \+ memberchk_pred_rev(subsumes_term,Vs,T), copy_term(Vs,CVs), nb_setarg(2, CONS, [CVs|T]))).



% for dont-care vars
/*
:- export(no_repeats_dc/2).
:- meta_predicate no_repeats_dc(+,0).
no_repeats_dc(Vs,Call):- term_variables(Call,CV),term_variables(Vs,VsL),subtract_eq(CV,VsL,NewVs),no_repeats(NewVs,Call).
*/

%% subtract_eq(+Set, +Delete, -Result) is det.
% Delete all elements in Delete from Set. Deletion is based on unification using ==/2. The complexity is |Delete|*|Set|.




%% subtract_eq( :TermE, ?Delete, ?Result) is semidet.
%
% Subtract Using (==/2) (or =@=/2) ).
%
subtract_eq([],_,[]) :- !.
subtract_eq([E|Set], Delete, Result) :-
   subtract_eq(Set, Delete, Mid),
   (identical_memberchk(E,Delete)-> Result = Mid ; Result = [E|Mid]).
   
% ===================================================
%
%  no_repeats_av/1 - Filter repeats using coroutining
%
% Same as no_repeats(:Call) (so same as call/1 but fitered)
%
% (everytime we see new value.. we add it to was/2 in an attributed variable that we have a refernce in a compound)
% Cehcked via ?- was(AVar,Foo), get_attrs(AVar,ATTRS1), get_attrs(AVar,ATTRS2), ATTRS1==ATTRS2.
%
%  So the variable binding gerts rejected several frames below your code? ( are we nipping away futile bindings?)
%
% however ..
%     does that mess with anything in code that we are calling?
%  Could some peice of code been required to see some binding to make a side effect come about?
%
% logicmoo_mud:  ?- no_repeats_av(member(X,[3,1,1,1,3,2])).
% X = 3 ;
% X = 1 ;
% X = 2.
%
% attributed variable verson of getting filtered bindings
% ===================================================
/*
:- export(no_repeats_av/1).
% = :- meta_predicate(no_repeats_av(0)).
:- export(no_repeats_av/2).
% = :- meta_predicate(no_repeats_av(+,0)).
:- export(no_repeats_av_l/2).
% = :- meta_predicate(no_repeats_av_l(+,0)).

no_repeats_av(AVar,Call):- var(AVar),!,
   Call):-term_variables(Call,Vs),!,no_repeats_av_l(Vs,Call).

no_repeats_av_l([],Call):-!,Call,!.
no_repeats_av_l([AVar],Call):-!,
   no_repeats_av(AVar,Call).
no_repeats_av_l([AVar|List],Call):-   each_call_cleanup(
       (was(AVar,iNEVER),asserta(tlbugger:cannot_save_table,Ref),get_attr(AVar,waz,varwaz(CONS,_))),
        (Call,copy_term_nat(AVar,C),nb_linkarg(2, CONS, [_-C|CONS])),
        (del_attr(AVar,was),erase_safe(tlbugger:cannot_save_table,Ref))).
no_repeats_av(List,Call):- is_list(List),!,no_repeats_av_l(List,Call).
no_repeats_av(Term,Call):-term_variables(Term,List),!,no_repeats_av_l(List,Call).



*/

% =========================================================================
:- meta_predicate succeeds_n_times(0, -).
% =========================================================================




%% succeeds_n_times( :Goal, -Times) is semidet.
%
% Succeeds N Times.
%
succeeds_n_times(Goal, Times) :-
        Counter = counter(0),
        (   Goal,
            arg(1, Counter, N0),
            N is N0 + 1,
            nb_setarg(1, Counter, N),
            fail
        ;   arg(1, Counter, Times)
        ).



:- export(no_repeats_findall5/5).
:- meta_predicate no_repeats_findall5(+,0,-,-,-).



%% no_repeats_findall5( +Vs, :Goal, -ExitDET, -USE, -NEW) is semidet.
%
% No Repeats Findall5.
%
no_repeats_findall5(Vs,Call,ExitDET,USE,NEW):-
   (((HOLDER = fa([]),
   Call,arg(1,HOLDER,CONS),
   ((
   (( \+ memberchk_same(Vs,CONS),
   copy_term(Vs,CVs),
   append(CONS,[CVs],NEW),
    nb_setarg(1, HOLDER, NEW)))
      ->
       USE=true;
       ((USE=(false),CONS=NEW))
       )),
   deterministic(ExitDET),true))
    *-> true;
     (NEW=[],ExitDET=true,USE=(false))).

:- export(no_repeats_save/4).
:- meta_predicate no_repeats_save(+,0,-,-).



%% no_repeats_save( +Vs, :Goal, -Saved, -USE) is semidet.
%
% No Repeats Save.
%
no_repeats_save(Vs,Call,Saved,USE):-
 SavedHolder = saved(_),
  no_repeats_findall5(Vs,Call,ExitDET,USE,NEW),
  ( ExitDET==true -> (nb_linkarg(1,SavedHolder,NEW),!) ; true),
  arg(1,SavedHolder,Saved).

:- export(no_repeats_save/2).
:- meta_predicate no_repeats_save(+,0).



%% no_repeats_save( +Vs, :Goal) is semidet.
%
% No Repeats Save.
%
no_repeats_save(Vs,Call):-
  call_cleanup(
   (( no_repeats_save(Vs,Call,SavedList,USE),
      (USE==true -> true ; fail))),
   (is_list(SavedList) -> writeln(saving(SavedList)) ; writeln(givingup_on(Call)))).


:- export(no_repeats_findall_r/5).
:- meta_predicate no_repeats_findall_r(+,0,-,-,-).



%% no_repeats_findall_r( +Vs, :Goal, -CONS, -ExitDET, -List) is semidet.
%
% No Repeats Findall R.
%
no_repeats_findall_r(Vs,Call,CONS,ExitDET,List):-
   CONS = [ExitDET],
   (Call,once(( \+ memberchk_same(Vs,CONS), copy_term(Vs,CVs), CONS=[_|T],List=[CVs|T], nb_linkarg(2, CONS, List)))),
   deterministic(ExitDET).

%% no_repeats_var(+Var) is det.
%
% Attribute a varaible to never be bound to the same value twice
%
%  ==
%  ?- no_repeats_var(X),member(X,[1,2,3,3,3,1,2,3]).
%  X = 1;
%  X = 2;
%  X = 3;
%  No.
%  ==
no_repeats_var(Var):- nonvar(Var) ->true; 
 (get_attr(Var,nr,_)->true;put_attr(Var,nr,old_vals(Var,same_forms,[]))).
no_repeats_var(Cmp,Var):- nonvar(Var) ->true; (get_attr(Var,nr,_)->true;put_attr(Var,nr,old_vals(Var,Cmp,[]))).

nr:attr_unify_hook(AttValue,VarValue):- 
  AttValue=old_vals(_Var,Cmp,Waz),
  \+ memberchk_pred(Cmp,VarValue,Waz),nb_setarg(3,AttValue,[VarValue|Waz]).
/*
nr:attr_unify_hook(AttValue,VarValue):- 
  AttValue=old_vals(_Var,Cmp,Waz),
  \+ (member(E,Waz), call(Cmp,VarValue,E)),
  nb_setarg(3,AttValue,[VarValue|Waz]).
*/

same_forms(F1,F2):- get_attrs(F1,A1),!,get_attrs(F2,A2),A1=@=A2.
same_forms(F1,F2):- get_attrs(F2,A1),!,fail,get_attrs(F1,A2),A1=@=A2.
same_forms(F1,F2):- var(F1),!,F2==F1.
same_forms(F1,F2):- var(F2),!,fail,F2==F1.
same_forms(F1,F2):- F1=@=F2.
% WAS OFF  :- system:use_module(library(logicmoo_startup)).

:- if(current_predicate(fixup_exports/0)).
:- fixup_exports.
:- endif.

%% nr_test( :PRED-1VALUE1, :PRED1VALUE2) is semidet.
%
% Term.
%
nr_test((-7), 3).
nr_test(2,10).
nr_test((-8), 5).
nr_test((-8), 2).
nr_test(42,11).
nr_test(42,14).
nr_test(1,3).
nr_test(77,2).
nr_test(80,10).
nr_test(80,0).
nr_test((-3),12).
nr_test((-4), 14).
nr_test((-4), 0).
nr_test(45,0).
nr_test(45,9).
nr_test((-1),1).


