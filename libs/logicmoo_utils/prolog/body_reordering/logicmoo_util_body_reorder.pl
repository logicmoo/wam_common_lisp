/* Part of LogicMOO Base Logicmoo Path Setups
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
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_body_reorder.pl
:- module(logicmoo_util_body_reorder,
          [ call_body_reorder/3,
            call_body_reorder_compare/4,
            enable_body_reorder/0,
            timeOfFirstAnswer/2,
            timeOfFirstAnswer_0/2,
            timeOfFirstAnswer_1/2,
            reorderBody/2,
            reorderBody/3,
            reorderBody/4,
            guess_reorder/3,
            callClause/1,
            make_reordering_key/4,
            do_body_reorder/4,
            fasterClause/4,
            disable_body_reorder/0]).


:- module_transparent((
            call_body_reorder/3,
            call_body_reorder_compare/4,
            enable_body_reorder/0,
            timeOfFirstAnswer/2,
            timeOfFirstAnswer_0/2,
            timeOfFirstAnswer_1/2,
            reorderBody/2,
            reorderBody/3,
            reorderBody/4,
            guess_reorder/3,
            callClause/1,
            reorder/0,
            make_reordering_key/4,
            do_body_reorder/4,
            fasterClause/4,
            disable_body_reorder/0)).

:- thread_local(t_l:noreorder/0).

%:- ensure_loaded(logicmoo_util_body_file_scope).
:-meta_predicate(call_body_reorder(+,+,+)).

:-volatile(lmcache:reordering/3).
:-dynamic(lmcache:reordering/3).

t_l:noreorder.
reorder.

reorder_if_var(Var,A,B):- nonvar(Var)-> (call(A),call(B));(call(B),call(A)).

can_reorderBody(_ ,true):-!,fail.
can_reorderBody(_ ,Body):-member(M,[did,t_l:noreorder,!,call_body_reorder,call_body_reorder_compare,var,nonvar]),contains_f(M,Body),!,fail.
can_reorderBody(_ ,Body):-member(M,[reorder]),contains_f(M,Body),!.
% can_reorderBody(Head, _):- reorder_term_expansion,compound(Head),functor(Head, _ ,A),A > 0.

:-meta_predicate(do_body_reorder(+,?,+,-)).

do_body_reorder(_,_,_,_):-!,fail.
do_body_reorder(_,_,H,H):- \+ compound(H),!.
do_body_reorder(Head,Vars,(A;B),(AA;BB)):- !,do_body_reorder(Head,Vars,A,AA),do_body_reorder(Head,Vars,B,BB).
do_body_reorder(Head,_,Body,Body):- \+ can_reorderBody(Head,Body),!.
do_body_reorder(Head,Vars,H,HO):-do_body_reorder_e(Head,Vars,H,HO)->H\=@=HO,!.
do_body_reorder(Head,[],Body,call_body_reorder(Code,Head,BodyLOut)):- fail,
   conj_to_list_reorder(Body,BodyLOut)->BodyLOut=[_,_|_],
   gensym(reorderCode,Code),!.
do_body_reorder(_,_,Body,Body).

do_body_reorder_e(_,_,H,H):- \+ compound(H),!.
do_body_reorder_e(Head,_,Body,call_body_reorder(Code,Head,BodyLOut)):- 
   Body=..[reorderBody|BodyLOut],!,
   gensym(reorderCode,Code).
do_body_reorder_e(Head,Vars,H,HO):- 
  H=..HL, must_maplist(do_body_reorder_e(Head,Vars),HL,HOL),HO=..HOL.

conj_to_list_reorder((A),[A]):- \+ compound(A),!.
conj_to_list_reorder((A,B),AB):-!,conj_to_list_reorder(A,AL),conj_to_list_reorder(B,BL),append(AL,BL,AB).
conj_to_list_reorder((A),[A]).

make_reordering_key(Head,C1,C2,Key):-
    term_variables_of_ex(Head,HeadVars),
    term_variables_of_ex(C1,C1V),
    term_variables_of_ex(C2,C2V),
    make_reordering_vkey(HeadVars,C1V,C2V,Key),!.


term_variables_of_ex((C2^_),C2V):-!,term_variables(C2,C2V).
term_variables_of_ex(C2,C2V):-term_variables(C2,C2V).

shared_len(VA,VB,N3):-ord_subtract(VA,VB,Rest),length(Rest,N3).

pairify_key(VA,VB,(UA^N3^UB)):- length(VA,N1),length(VB,N2),shared_len(VA,VB,N3),UA is N1-N3,UB is N2-N3.
% make_reordering_vkey(HeadVars,C1V,C2V,Key)
make_reordering_vkey([],[],[],nonvars).
make_reordering_vkey([],[],_ ,h0_a0).
make_reordering_vkey([],_ ,[],h0_b0).
make_reordering_vkey(_ ,[],[],a0_b0).

make_reordering_vkey([],VA,VB,h0(Key)):-pairify_key(VA,VB,Key).
make_reordering_vkey(VH,[],VB,a0(Key)):-pairify_key(VH,VB,Key).
make_reordering_vkey(VH,VA,[],b0(Key)):-pairify_key(VH,VA,Key).
make_reordering_vkey(VH,VA,VB,open(Key1,Key2,Key3)):-pairify_key(VH,VA,Key3),pairify_key(VH,VB,Key1),pairify_key(VA,VB,Key2).

reorderBody(C1,C2):- call_body_reorder(anon,2,[C1,C2]).
reorderBody(C1,C2,C3):- call_body_reorder(anon,3,[C1,C2,C3]).
reorderBody(C1,C2,C3,C4):- call_body_reorder(anon,4,[C1,C2,C3,C4]).

lookup_reorder(Key,In,Out):- lmcache:reordering(Key,In,Out),!.
save_reorder(Key,In,Out):- call(asserta,lmcache:reordering(Key,In,Out)),!.

call_body_reorder_key(_Code,_Head,Key,C1,C2):- notrace(lookup_reorder(Key,(C1,C2),Found)),!,ereq(Found).
call_body_reorder_key(_Code,_Head,Key,C1,C2):- notrace(guess_reorder(C1,C2,Reordered)),!,
   save_reorder(Key,(C1,C2),Reordered),!,ereq(Reordered).


call_body_reorder_compare(Code,Head,C1,C2):- notrace(make_reordering_key(Head,C1,C2,Key)),!,
   call_body_reorder_key(Code,Head,Code-Key,C1,C2).


make_body_reorderer(Code,Head,[C2,C1],call_body_reorder_compare(Code,Head,C1,C2)):-!.
make_body_reorderer(Code,Head,[C3|C12],OUT):- make_body_reorderer(Code,Head,C12,OC12),make_body_reorderer(Code,Head,[OC12,C3],OUT).

call_body_reorder(C,CC):-call_body_reorder(C,C,CC).

:-meta_predicate(call_body_reorder(+,+,+)).
call_body_reorder(_Code,_Head,[A]):- !,callClause(A).
call_body_reorder(Code,Head,[A|B]):- !,callClause(A),call_body_reorder(Code,Head,B).
call_body_reorder(Code,Head,[C1,C2]):- !, call_body_reorder_compare(Code,Head,C1,C2).
call_body_reorder(Code,Head,[C1,C2,C3]):- call_body_reorder_compare(Code,Head,call_body_reorder_compare(Code,Head,C1,C2),C3).

call_body_reorder(Code,Head,[C1,C2,C3,C4]):- 
   call_body_reorder_compare(Code,Head,call_body_reorder_compare(Code,Head,call_body_reorder_compare(Code,Head,C1,C2),C3),C4).

% convert 
% call_body_reorder(Code,Head,[C1,C2,C3,C4]):- 
% to...
%   call_body_reorder_compare(Code,Head,call_body_reorder_compare(Code,Head,call_body_reorder_compare(Code,Head,C1,C2),C3),C4).
call_body_reorder(Code,Head,List):- reverse(List,Rev),
   make_body_reorderer(Code,Head,Rev,OUT),!,ereq(OUT).

call_body_reorder(_Code,Head, List):- member(Var^Call,List), ground(Var), !,delete_eq(List,Var^Call,ListO),!,callClause(Call),call_body_reorder(Head,ListO).
call_body_reorder(_Code,Head,[A|List]):- !,callClause(A),call_body_reorder(Head,List).
call_body_reorder(_Code,Head,C):- dmsg(warn(callClause((Head:-C)))),dtrace,callClause(C).

% 999 = error, 888 = fail,  T < 888 actual time
timeOfFirstAnswer(C1,Time):- catch((timeOfFirstAnswer_0(C1,Time)*->true;Time=888),Time=999,true).

timeOfFirstAnswer_0(C1,TimeVar):-
   TimeVar1=e(_), 
   timeOfFirstAnswer_1(C1,TimeVar1),
   arg(1,TimeVar1,TimeVar).

timeOfFirstAnswer_1(C1,TimeVar1):-  
   statistics(cputime,Start),
   (ereq(C1)*->(ground(TimeVar1)->true;
     (statistics(cputime,End),Time is End - Start,
        nb_setarg(1,TimeVar1,Time)))).
  

:-meta_predicate(guess_reorder(0,0,-)).
guess_reorder(C1,C2,Reordered):-
   ( Try1 = ( \+ ( \+ ((C1,C2))))),
   ( Try2 = ( \+ ( \+ ((C2,C1))))),
   fasterClause(Try1,Try2,T1,T2),!,
   (T1<888 ; T2<888),
   ( (T1 > T2 ) -> 
       Reordered = (C2,C1); 
       Reordered = (C1,C2)).

:-meta_predicate(fasterClause(0,0,+,+)).
fasterClause(C1,C2,T1,T2):-  
  timeOfFirstAnswer(C1,T1),
  catch(catch(call_with_time_limit(T1,
     timeOfFirstAnswer_0(C2,T2)),
          time_limit_exceeded,T2 is T1+1),_,T2=999).


:-meta_predicate(callClause(+)) .
callClause(_^C):-!,callClause(C).
callClause((C0->C1;C2)):-!,(callClause(C0)->callClause(C1);callClause(C2)).
callClause((C0*->C1;C2)):-!,(callClause(C0)*->callClause(C1);callClause(C2)).
callClause((C0->C1)):-!,(callClause(C0)->callClause(C1)).
callClause((C0*->C1)):-!,(callClause(C0)*->callClause(C1)).
callClause((C1;C2)):-!,callClause(C1);callClause(C2).
callClause((C1,C2)):-!,callClause(C1),callClause(C2).
%callClause(C):-debugOnError(ereq(C)).
callClause([L|Ist]):- !, dmsg(callClause([L|Ist])),!,call_body_reorder(_ ,[L|Ist]).
callClause(C):- on_x_debug(ereq(C)).

enable_body_reorder:- enable_in_file(do_body_reorder).
disable_body_reorder:- disable_in_file(do_body_reorder).
:- disable_body_reorder.

:- fixup_exports.

:- if(true).
% some tests

:- endif.
