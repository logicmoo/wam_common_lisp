/*  Logicmoo Path Setups

   $Id$

    Logicmoo Util Library Path Setups

    File:         'logicmoo_util_terms.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_terms.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/
/** <module> Common Util MISC_TERMS
This module includes random predicate utilities that manipulate terms for substitution, decomposes, recomposes, composes, etc.
@author Douglas R. Miles
@license free or GNU 2
*/
:- module(logicmoo_util_terms,
        [
append_term/3,
append_term0/3,
at_start/1,
remember_at_start/2,
expire_tabled_list/1,
call_n_times/2,
call_no_cuts/1,
conjoin/3,
conjoin_op/4,
conjuncts_to_list/2,
delete_eq/3,
disjuncts_to_list/2,
doall/1,
dynamic_load_pl/1,
each_subterm/2,
each_subterm/3,
flatten_dedupe/2,
flatten_set/2,
wom_functor/3,
functor_h/2,
functor_h/3,
get_functor/2,
get_functor/3,
identical_memberchk/2,
in_thread_and_join/1,
in_thread_and_join/2,
is_proof/1,
is_true/1,
is_src_true/1,
lastMember2/2,
pred_subst/3,
list_retain/3,
list_to_conjuncts/2,
list_to_conjuncts/3,
list_to_set_safe/2,
load_assert/3,
load_dirrective/2,
load_term/2,
load_term2/2,
logicmoo_library_file_loaded/0,
make_list/3,
%makeArgIndexes/1,
%makeArgIndexes/2,
maptree/3,
nd_predsubst/3,
nd_predsubst1/4,
nd_predsubst2/3,
nd_subst/4,
nd_subst1/5,
nd_subst2/4,
pred_delete/4,
pred_juncts_to_list2/3,
pred_juncts_to_list/3,
pred_subst/5,
pred_term_parts/3,
pred_term_parts_l/3,
predsubst/3,
proccess_status/3,
read_each_term/3,
remove_dupes/2,
remove_dupes/3,
subst/4,
term_parts/2,
term_parts_l/2,
throw_if_true_else_fail/2,
univ_safe/2,
univ_term/2,
weak_nd_subst/4,
weak_nd_subst1/5,
weak_nd_subst2/4,
wsubst/4,
append_termlist/3,            
append_term0/3,            
apply_term/3,
atom_concat_safe/3,
compound_name_args_safe/3,
compound_name_arity_safe/3
          ]).

:- set_module(class(library)).
:- autoload(library(memfile),[memory_file_to_atom/2,atom_to_memory_file/2,open_memory_file/3,open_memory_file/4,free_memory_file/1]).

:-op(700,xfx,('univ_safe')).
:- system:use_module(library(logicmoo_startup)).


:- meta_predicate
        at_start(0),
        call_n_times(+, 0),
        call_no_cuts(0),
        doall(0),
        each_subterm(?, 2, ?),
        functor_h(?, ?),
        functor_h(?, ?, ?),
        get_functor(?, ?),
        get_functor(?, ?, ?),
        in_thread_and_join(0),
        in_thread_and_join(0, +),
        list_retain(?, 1, ?),
        load_dirrective(0, ?),
        maptree(2, +, ?),
        nd_predsubst(?, 2, ?),
        nd_predsubst1(2, ?, ?, ?),
        nd_predsubst2(2, ?, ?),
        pred_delete(2, ?, ?, ?),
        pred_subst(2, ?, ?, ?, ?),
        pred_term_parts(1, ?, ?),
        pred_term_parts_l(1, ?, ?),
        predsubst(?, 2, ?),
        throw_if_true_else_fail(0, ?).
:- module_transparent
        conjoin/3,
        conjoin_op/4,
        conjuncts_to_list/2,
        delete_eq/3,
        disjuncts_to_list/2,
        dynamic_load_pl/1,
        each_subterm/2,
        flatten_dedupe/2,
        flatten_set/2,       
        is_proof/1,
        is_src_true/1,
        is_true/1,
        lastMember2/2,
        list_to_conjuncts/2,
        list_to_conjuncts/3,
        list_to_set_safe/2,
        load_assert/3,
        load_term/2,
        load_term2/2,
        logicmoo_library_file_loaded/0,
        makeArgIndexes/1,
        makeArgIndexes/2,
        make_list/3,
        nd_subst/4,
        nd_subst1/5,
        nd_subst2/4,
        pred1_juncts_to_list/2,
        pred1_juncts_to_list/3,
        proccess_status/3,
        read_each_term/3,
        remove_dupes/2,
        remove_dupes/3,
        subst/4,
        term_parts/2,
        term_parts_l/2,
        univ_safe/2,
        univ_term/2,
        weak_nd_subst/4,
        weak_nd_subst1/5,
        weak_nd_subst2/4,
        wsubst/4.



         %upcase_atom_safe/2,
         %get_module_of/2,
          % concat_atom_safe/3,
           
           %exists_file_safe/1,
           %exists_directory_safe/1,
           %eraseall/2,
           %time_file_safe/2,


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- set_module(class(library)).
% % % OFF :- system:use_module(library(apply)).
:- dynamic(double_quotes_was_lib/1).
:- multifile(double_quotes_was_lib/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_lib(WAS)).
:- retract(double_quotes_was_lib(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_lib(WAS)).
:- set_prolog_flag(double_quotes,string).


expire_tabled_list(_).

%= 	 	 

%% lastMember2( ?E, :TermList) is semidet.
%
% Last Member Extended Helper.
%
lastMember2(_E,List):-var(List),!,fail.
lastMember2(E,[H|List]):-lastMember2(E,List);E=H.


%= 	 	 

is_true(B):- is_src_true(B).

%% is_src_true( ?B) is semidet.
%
% If Is A True.
%
:- module_transparent is_src_true/1.
is_src_true(B):-var(B),!,fail.
is_src_true(true):-!.
is_src_true({B}):- !, is_src_true(B).
is_src_true(props(_,NIL)):- NIL==[].
is_src_true(B):-is_proof(B).


%= 	 	 

%% is_proof( ?P) is semidet.
%
% If Is A Proof.
%
is_proof(P):-compound(P),functor(P,ftProofFn,_).


:- export(structureless/1).
structureless(A) :- \+ compound(A),!.
structureless(A) :- compound_name_arity(A,_,0),!.
% structureless('$VAR'(_)).


sub_compound(_,LF):- \+ compound(LF),!,fail.
sub_compound(X,X).
sub_compound(X, Term) :-
    (is_list(Term) ->
     (member(E,Term), sub_compound(X,E)) ; 
     (arg(_, Term, Arg), sub_compound(X, Arg))).



% What would be a good way to describe the manipulations?
% Function: maptree func expr many
% Try applying Lisp function func to various sub-expressions of expr.
% Initially, call func with expr itself as an argument.
% If this returns an expression which is not equal to expr, apply func again
% until eventually it does return expr with no changes. Then, if expr is a function call,
% recursively apply func to each of the arguments. This keeps going until no
% changes occur anywhere in the expression; this final expression is returned by maptree.
% Note that, unlike simplification rules, func functions may not make destructive changes to expr.
% If a third argument many is provided, it is an integer which says how many times func may be applied;
% the default, as described above, is infinitely many times.
% = :- meta_predicate(maptree(2,+,-)).
:- export(maptree/3).

%= 	 	 

%% maptree( :PRED2Pred, +I, -O) is semidet.
%
% Maptree.
%
maptree(Pred,I,O):- call(Pred,I,O),!.
maptree(_ ,I,O):- structureless(I),!, must(I=O).
maptree(Pred,[F|IL],LIST):- is_list([F|IL]), (maplist(maptree(Pred),[F|IL],LIST)),!.
maptree(Pred,I,O):- I=..[F|IL], 
 (maplist(maptree(Pred),[F|IL],[FO|OL])),
   (atom(FO)-> O=..[FO|OL] ; must((nop(maptree(Pred,I,O)),O=..[F,FO|OL]))).

:- export(disjuncts_to_list/2).

%= 	 	 

%% disjuncts_to_list( ?Var, ?Var) is semidet.
%
% Disjuncts Converted To List.
%
disjuncts_to_list(Var,[Var]):-is_ftVar(Var),!.
disjuncts_to_list(true,[]).
disjuncts_to_list([],[]).
disjuncts_to_list('v'(A,B),ABL):-!,
  disjuncts_to_list(A,AL),
  disjuncts_to_list(B,BL),
  append(AL,BL,ABL).
disjuncts_to_list([A|B],ABL):-!,
  disjuncts_to_list(A,AL),
  disjuncts_to_list(B,BL),
  append(AL,BL,ABL).
disjuncts_to_list((A;B),ABL):-!,
  disjuncts_to_list(A,AL),
  disjuncts_to_list(B,BL),
  append(AL,BL,ABL).
disjuncts_to_list(Lit,[Lit]).

:- export(conjuncts_to_list/2).

%= 	 	 

%% conjuncts_to_list( ?Var, ?Var) is semidet.
%
% Conjuncts Converted To List.
%
conjuncts_to_list(Var,[Var]):-is_ftVar(Var),!.
conjuncts_to_list(true,[]):-!.
conjuncts_to_list([],[]):-!.
conjuncts_to_list('&'(A,B),ABL):-!,
  conjuncts_to_list(A,AL),
  conjuncts_to_list(B,BL),
  append(AL,BL,ABL).
conjuncts_to_list([A|B],ABL):-!,
  conjuncts_to_list(A,AL),
  conjuncts_to_list(B,BL),
  append(AL,BL,ABL).
conjuncts_to_list((A,B),ABL):-!,
  conjuncts_to_list(A,AL),
  conjuncts_to_list(B,BL),
  append(AL,BL,ABL).
conjuncts_to_list(Lit,[Lit]).



pred_juncts_to_list(F,AB,ABL):- pred1_juncts_to_list(==(F),AB,ABL),!.

:- export(pred1_juncts_to_list/3).

%= 	 	 

%% pred1_juncts_to_list( ?F, ?AB, ?ABL) is semidet.
%
% Predicate Juncts Converted To List.
%
pred1_juncts_to_list(_,Var,[Var]):-is_ftVar(Var),!.
pred1_juncts_to_list(_,true,[]).
pred1_juncts_to_list(_,[],[]).
pred1_juncts_to_list(Pred1,AB,ABL):-AB=..[F,A,B],
  call(Pred1,F),!,
  pred1_juncts_to_list(Pred1,A,AL),
  pred1_juncts_to_list(Pred1,B,BL),
  append(AL,BL,ABL).
pred1_juncts_to_list(Pred1,AB,AL):-AB=..[F,A],
  call(Pred1,F),!,
  pred1_juncts_to_list(Pred1,A,AL).

pred1_juncts_to_list(Pred1,AB,ABL):-AB=..[F,A|ABB],
  call(Pred1,F),
  pred1_juncts_to_list(Pred1,A,AL),
  B=..[F|ABB],  
  pred1_juncts_to_list(Pred1,B,BL),
  append(AL,BL,ABL),!.
pred1_juncts_to_list(_Pred1,Lit,[Lit]).

%= 	 	 

%% pred1_juncts_to_list( ?A, ?ABL) is semidet.
%
% Predicate Juncts Converted To List.
%
pred_juncts_to_list2(_Pred1,I,I):- (is_ftVar(I);I==[]),!.
pred_juncts_to_list2(Pred1,[A|B],ABL):-!,
  pred_juncts_to_list2(Pred1,A,AL),
  pred_juncts_to_list2(Pred1,B,BL),
  append(AL,BL,ABL).
pred_juncts_to_list2(_Pred1,Lit,[Lit]).


:- export(list_to_conjuncts/2).

%= 	 	 

%% list_to_conjuncts( ?I, ?O) is semidet.
%
% List Converted To Conjuncts.
%
list_to_conjuncts(I,O):-list_to_conjuncts((','),I,O).

:- export(list_to_conjuncts/3).

%= 	 	 

%% list_to_conjuncts( ?VALUE1, ?H, ?H) is semidet.
%
% List Converted To Conjuncts.
%
list_to_conjuncts(_,V,V):- var(V),!.
list_to_conjuncts(_,[],true):-!.
list_to_conjuncts(_,V,V):-not(compound(V)),!.
list_to_conjuncts(OP,[H],HH):-list_to_conjuncts(OP,H,HH),!.

list_to_conjuncts(OP,[H|T],Body):- current_op(_,yfx,OP),!,
    list_to_conjuncts_yfx(OP,H,T,Body).

list_to_conjuncts(OP,[H|T],Body):-!,
    list_to_conjuncts(OP,H,HH),
    list_to_conjuncts(OP,T,TT),
    conjoin_op(OP,HH,TT,Body),!.
list_to_conjuncts(_,H,H).


list_to_conjuncts_yfx(_,H,[],H):-!.
list_to_conjuncts_yfx(OP,Ac,[H|T],Body):- !,
   conjoin_op(OP,Ac,H,MBody),
   list_to_conjuncts_yfx(OP,MBody,T,Body).
list_to_conjuncts_yfx(OP,Ac,T,Body):-
   conjoin_op(OP,Ac,T,Body).




%= 	 	 

%% conjoin( ?A, ?B, ?C) is semidet.
%
% Conjoin.
%
conjoin(A,B,C):-A==B,!,C=A.
conjoin(True,C,C):-True==true,!.
conjoin(C,True,C):-True==true,!.
conjoin(A,B,(A,B)):- (var(A);var(B)),!.
conjoin((A1,A),B,(A1,C)):-!,conjoin(A,B,C).
conjoin(A1,(A,B),(A1,C)):-!,conjoin(A,B,C).
conjoin(A,B,(A,B)).

%= conjoin_op(OP,+Conjunct1,+Conjunct2,?Conjunction).
%= arg4 is a simplified expression representing the conjunction of
%= args 2 and 3.


%= 	 	 

%% conjoin_op( ?VALUE1, ?TRUE, ?X, ?X) is semidet.
%
% Conjoin Oper..
%
conjoin_op(_,TRUE,X,X) :- TRUE==true, !.
conjoin_op(_,X,X,TRUE) :- TRUE==true, !.
conjoin_op(_,X,Y,Z) :- X==Y,Z=X,!.
conjoin_op(OP,C1,C2,C):- current_op(_,yfx,OP),functor(OP,C2,2),C =..[OP,C2,C1],!.
conjoin_op(OP,C1,C2,C):- C =..[OP,C1,C2].

% =================================================================================
% Utils
% =================================================================================



%= 	 	 

%% read_each_term( ?S, ?CMD, ?Vs) is semidet.
%
% Read Each Term.
%
read_each_term(S,CMD,Vs):- atom_string(W,S),atom_to_memory_file(W,MF),
 call_cleanup((open_memory_file(MF,read,Stream,[free_on_close(true)]),
      findall(CMD-Vs,(
       repeat,
       catch((clpfd:read_term(Stream,CMD,[double_quotes(string),variable_names(Vs)])),_,CMD=end_of_file),
       (CMD==end_of_file->!;true)),Results)),
  free_memory_file(MF)),!,
  ((member(CMD-Vs,Results),CMD\==end_of_file)*->true;catch((clpfd:read_term_from_atom(W,CMD,[double_quotes(string),variable_names(Vs)])),_,fail)).




:- export(each_subterm/2).

%= 	 	 

%% each_subterm( ?B, ?A) is semidet.
%
% Each Subterm.
%
each_subterm(B, A):- (compound(B), arg(_, B, C), each_subterm(C, A));A=B.

:- export(each_subterm/3).

%= 	 	 

%% each_subterm( ?A, :PRED2Pred, ?B) is semidet.
%
% Each Subterm.
%
each_subterm(A,Pred,B):- call( Pred,A,B).
each_subterm(A,Pred,O):-
   compound(A),
   once(  A=[H|T] ;  A=..[H|T] ),
   (each_subterm(H,Pred,O);
     each_subterm(T,Pred,O)).



:- dynamic(argNumsTracked/3).
%:- dynamic(argNFound/3).
% :-was_indexed(argNFound(1,1,1)).


/*
%= 	 	                         

%% makeArgIndexes( ?CateSig) is semidet.
%
% Make Argument Indexes.
%
makeArgIndexes(CateSig):-functor_catch(CateSig,F,_),makeArgIndexes(CateSig,F),!.

%= 	 	 

%% makeArgIndexes( ?CateSig, ?F) is semidet.
%
% Make Argument Indexes.
%
makeArgIndexes(CateSig,F):- 
  ignore((argNumsTracked(F,Atom,Number),arg(Number,CateSig,Arg), nonvar(Arg),
     % Number<10, nonvar(Arg),atom_number(Atom,Number),
     ( \+ argNFound(F,Atom,Arg),assert(argNFound(F,Atom,Arg))))).


*/

%= 	 	 

%% flatten_dedupe( ?Percepts0, ?Percepts) is semidet.
%
% Flatten Dedupe.
%
flatten_dedupe(Percepts0,Percepts):-
   flatten([Percepts0],Percepts1),remove_dupes(Percepts1,Percepts).

% :- ensure_loaded(logicmoo_util_bugger).


%= 	 	 

%% proccess_status( ?VALUE1, ?Det, ?Goal2) is semidet.
%
% Proccess Status.
%
proccess_status(_,exited(called(Det,Goal)),called(Goal2)):- Det = true,!,must_det(Goal=Goal2).
proccess_status(ID,exited(called(Det,Goal)),called(Goal2)):- dmsg(nondet_proccess_status(ID,exited(called(Det,Goal)),called(Goal2))),!,must_det(Goal=Goal2).
proccess_status(ID,true,Want):- dmsg(proccess_status(ID,true,Want)),!.
proccess_status(ID,false,Want):- dmsg(failed_proccess_status(ID,false,Want)),!,fail.
proccess_status(ID,exception(Status),Want):- dmsg(exception(Status, ID,false,Want)),!,throw(Status).
proccess_status(ID,exited(Other),Want):-dmsg(wierd_proccess_status(ID,exited(Other),Want)),!.

:- meta_predicate in_thread_and_join(0).

%= 	 	 

%% in_thread_and_join( :Goal) is semidet.
%
% In Thread And Join.
%
in_thread_and_join(Goal):-thread_create((Goal,deterministic(Det),thread_exit(called(Det,Goal))),ID,[detatched(false)]),thread_join(ID,Status),show_call(why,proccess_status(ID,Status,called(Goal))).
:- meta_predicate in_thread_and_join(0,+).

%= 	 	 

%% in_thread_and_join( :Goal, +Status) is semidet.
%
% In Thread And Join.
%
in_thread_and_join(Goal,Status):-thread_create(Goal,ID,[]),thread_join(ID,Status).


% ===================================================================
% Substitution based on Pred
% ===================================================================

% Usage: predsubst(+Fml,+Pred,?FmlSk)


%= 	 	 

%% predsubst( ?A, :PRED2Pred, ?D) is semidet.
%
% Predsubst.
%
predsubst(A,Pred, D):-
      catchv(quietly(nd_predsubst(A,Pred,D)),_,fail),!.
predsubst(A,_B,A).


%= 	 	 

%% nd_predsubst( ?Var, :PRED2Pred, ?SUB) is semidet.
%
% Nd Predsubst.
%
nd_predsubst(  Var, Pred,SUB ) :- call(Pred,Var,SUB).
nd_predsubst(  Var, _,Var ) :- var(Var),!.
nd_predsubst(  P, Pred, P1 ) :- functor_catch(P,_,N),nd_predsubst1( Pred, P, N, P1 ).


%= 	 	 

%% nd_predsubst1( :PRED2Pred, ?P, ?N, ?P1) is semidet.
%
% Nd Predsubst Secondary Helper.
%
nd_predsubst1( _,  P, 0, P  ).
nd_predsubst1( Pred,  P, N, P1 ) :- N > 0, compound_name_arguments(P,F,Args),
            nd_predsubst2( Pred,  Args, ArgS ),
            nd_predsubst2( Pred, [F], [FS] ),
            univ_term(P1 , [FS|ArgS]).



%= 	 	 

%% nd_predsubst2( :PRED2Pred, ?L, ?L) is semidet.
%
% Nd Predsubst Extended Helper.
%
nd_predsubst2( _, [], [] ).
nd_predsubst2( Pred, [A|As], [Sk|AS] ) :- call(Pred,A,Sk), !, nd_predsubst2( Pred, As, AS).
nd_predsubst2( Pred, [A|As], [A|AS]  ) :- var(A), !, nd_predsubst2( Pred, As, AS).
nd_predsubst2( Pred, [A|As], [Ap|AS] ) :- nd_predsubst( A,Pred,Ap ),nd_predsubst2( Pred, As, AS).
nd_predsubst2( _, L, L ).

compound_name_arity_safe(P,F,A):- compound(P) -> compound_name_arity(P,F,A) ; functor(P,F,A).


compound_name_args_safe(P,F,A):- 
  ( compound(P)
   -> (compound(F) ->apply_term(F,A,P);compound_name_arguments(P,F,A))
   ; ( atom(F)
       -> (call(call, ( =.. ),P,[F|A]))
        ;  (apply_term(F,A,P)))),!.

%% append_term0( ?T, ?I, ?HEAD) is semidet.
%
% Append Term.
%
append_term(M:I,P,M:O):- !, append_term0(I,P,O).
append_term(I,P,O):- append_term0(I,P,O).
append_term0(F,I,HEAD):-atom(F),!,HEAD=..[F,I],!.
append_term0(F,I,HEAD):-atom(F),!,HEAD=..[F,I],!.
append_term0(Call,E,CallE):-var(Call),!, must(compound(CallE)),CallE=..ListE,append(List,[E],ListE),Call=..List.
append_term0(Call,E,CallE):-must(compound(Call)), Call=..List, append(List,[E],ListE), CallE=..ListE.


apply_term(T,LST,HEAD):- atom(T),!,HEAD=..[T|LST],!.
apply_term(T,LST,HEAD):- HEAD=..[t,T|LST],!.
apply_term(T,LST,HEAD):- HEAD==T,!,LST=[].
apply_term(T,LST,HEAD):- (LST==[] -> HEAD= T ; (HEAD= T -> LST=[] )).


%% append_termlist( ?Call, ?EList, ?CallE) is semidet.
%
% Append Termlist.
%
append_termlist(M:Call,EList,Out):- nonvar(M),!,append_termlist(Call,EList,CallE),M:CallE=Out.
append_termlist(Call,EList,CallE):- var(Call),must(is_list(EList)),!,must((append([t,Call],EList,ListE), CallE=..ListE)).
append_termlist(Call,EList,CallE):-must(is_list(EList)),!,must((Call=..LeftSide, append(LeftSide,EList,ListE), CallE=..ListE)).


/*
compound_name_args_safe(P,F,A):- var(P),
   compound_name_arguments(P,F,A),!.
compound_name_args_safe(P,F,A):- atom(F),call(call,=..,P,[F|A]),!.
compound_name_args_safe(B,P,ARGS):- ARGS==[],!,B=P.
compound_name_args_safe(B,P,ARGS):- nonvar(ARGS), \+ is_list(ARGS),!,B=[P|ARGS].
*/

% ===================================================================
% Substitution based on +PRED
% ===================================================================
/*
% Usage: subst(+Pred,+Fml,+X,+Sk,?FmlSk)

pred_subst(Pred,A,B,C,D):-  catchv(quietly(nd_pred_subst(Pred,A,B,C,D)),E,(dumpST,dmsg(E:nd_pred_subst(Pred,A,B,C,D)),fail)),!.
pred_subst(_,A,_B,_C,A).

nd_pred_subst(Pred,  Var, VarS,SUB,SUB ) :- call(Pred, Var,VarS),!.
nd_pred_subst(_Pred,  Var, _,_,Var ) :- var(Var),!.

nd_pred_subst(Pred,  P, X,Sk, P1 ) :- compound_name_arity_safe(P,_,N),nd_pred_subst1(Pred, X, Sk, P, N, P1 ).

nd_pred_subst1(_Pred, _,  _, P, 0, P  ).
nd_pred_subst1(Pred, X, Sk, P, N, P1 ) :- N > 0, univ_term(P , [F|Args]),
            nd_pred_subst2(Pred, X, Sk, Args, ArgS ),
            nd_pred_subst2(Pred, X, Sk, [F], [FS] ),
            univ_term(P1 , [FS|ArgS]).

nd_pred_subst2(_, _,  _, [], [] ).
nd_pred_subst2(Pred, X, Sk, [A|As], [Sk|AS] ) :- call(Pred, X , A), !, nd_pred_subst2(Pred, X, Sk, As, AS).
nd_pred_subst2(Pred, X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_pred_subst2(Pred, X, Sk, As, AS).
nd_pred_subst2(Pred, X, Sk, [A|As], [Ap|AS] ) :- nd_pred_subst(Pred, A,X,Sk,Ap ),nd_pred_subst2(Pred, X, Sk, As, AS).
nd_pred_subst2(_, _X, _Sk, L, L ).

*/

% -- CODEBLOCK
% Usage: pred_subst(+Pred,+Fml,+X,+Sk,?FmlSk)
:- export(pred_subst/5).


%= 	 	 

%% pred_subst( :PRED2Pred, ?P, ?X, ?Sk, ?P1) is semidet.
%
% Predicate Subst.
%
pred_subst( Pred, P,       X,Sk,       P1    ) :- call(Pred,P,X),!,must( Sk=P1),!.
pred_subst(_Pred, P,       _,_ ,       P1    ) :- is_ftVar(P),!, must(P1=P),!.
pred_subst( Pred,[P|Args], X,Sk,    [P1|ArgS]) :- !, pred_subst(Pred,P,X,Sk,P1),!, must(pred_subst( Pred, Args,X, Sk, ArgS )),!.
pred_subst( Pred, P,       X,Sk,       P1    ) :- compound(P),!, compound_name_arguments(P,F,Args), pred_subst( Pred, [F|Args],X, Sk, [Fs|ArgS] ),!, compound_name_arguments(P1,Fs,ArgS),!.
pred_subst(_Pred ,P,       _, _,       P     ).

:- meta_predicate(pred_subst(2,+,-)).
pred_subst( Pred, P,   X    ) :- call(Pred,P,X),!.
pred_subst(_Pred, P,   P1    ) :- is_ftVar(P),!, must(P1=P),!.
pred_subst( Pred,[P|Args], [P1|ArgS]) :- !, pred_subst(Pred,P,P1),!, must(pred_subst( Pred, Args, ArgS )),!.
pred_subst( Pred, P,   P1    ) :- compound(P),!, compound_name_arguments(P,F,Args), pred_subst( Pred, [F|Args], [Fs|ArgS] ),!, compound_name_arguments(P1,Fs,ArgS),!.
pred_subst(_Pred ,P,   P     ).


% dcgPredicate(M,F,A,P).


%= 	 	 

%% univ_safe( ?P, ?L) is semidet.
%
% Univ Safely Paying Attention To Corner Cases.
%

'univ_safe'(P,[L|IST]):- compound(P) -> compound_name_arguments(P,L,IST) ; P=..[L|IST].
%univ_safe(P,[L|L1]):- nonvar(P), must((var(L);atom(L))),!,on_x_debug(( P=..[L|L1] )).
%univ_safe(P,L):- must_det(is_list(L)),on_x_debug((P=..L)).

:- op(700,xfx,prolog:('univ_safe')).

safe_functor(P,F,A):- compound(P) -> compound_name_arity(P,F,A) ; functor(P,F,A).
:- export(safe_functor/3).

m_functor(M:P, M:F, A):- !, safe_functor(P, F, A).
m_functor(P, F, A):- safe_functor(P, F, A).

wom_functor(MP, F, A):- strip_module(MP,_,P),safe_functor(P, F, A).

% ===================================================================
% Substitution based on ==
% ===================================================================

% Usage: subst(+Fml,+X,+Sk,?FmlSk)

% :- mpred_trace_nochilds(subst/4).


%= 	 	 

%% subst( ?A, ?B, ?C, ?D) is semidet.
%
% Subst.
%
% subst(A,B,C,D):-  quietly((catchv(quietly(nd_subst(A,B,C,D)),E,(dumpST,dmsg(E:nd_subst(A,B,C,D)),fail)))),!.
subst(A,B,C,D):-  (must(nd_subst(A,B,C,D0))),on_x_debug(D=D0),!.
subst(A,_B,_C,A).

:- export(subst/4).

subst_each(A,[NV|List],D):-
  (NV=..[_,N,V]->true;NV=..[N,V]),!,
  subst(A,N,V,M),
  subst_each(M,List,D).
subst_each(A,_,A).

:- export(subst_each/3).

%= 	 	 

%% nd_subst( ?Var, ?VarS, ?SUB, ?SUB) is semidet.
%
% Nd Subst.
%
nd_subst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
% nd_subst(  Var, _,_,Var ) :- var(Var),!.
nd_subst(  Var, _,_,Var ) :- \+ compound(Var),!.
nd_subst(  P, X,Sk, P1 ) :- 
  compound_name_arity(P,_,N),
  nd_subst1( X, Sk, P, N, P1 ),!.


%= 	 	 

%% nd_subst1( ?X, ?Sk, ?P, ?N, ?P1) is semidet.
%
% Nd Subst Secondary Helper.
%
nd_subst1( _,  _, P, 0, P  ).
nd_subst1( X, Sk, P, N, P1 ) :- N > 0, univ_term(P , [F|Args]),
            nd_subst2( X, Sk, Args, ArgS ),
            nd_subst2( X, Sk, [F], [FS] ),
            univ_term(P1 , [FS|ArgS]).


%= 	 	 

%% nd_subst2( ?X, ?Sk, ?L, ?L) is semidet.
%
% Nd Subst Extended Helper.
%
nd_subst2( _,  _, [], [] ).
nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- X == A, !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_subst2( X, Sk, As, AS).
nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- nd_subst( A,X,Sk,Ap ),!,nd_subst2( X, Sk, As, AS).
nd_subst2( _X, _Sk, L, L ).


%= 	 	 

%% univ_term( ?P1, :TermFS) is semidet.
%
% Univ Term.
%
univ_term(P1,[FS|ArgS]):- compound(FS),!,append_termlist(FS,ArgS,P1).
univ_term(P1,[FS|ArgS]):- univ_safe(P1 , [FS|ArgS]).



%= 	 	 

%% wsubst( ?A, ?B, ?C, ?D) is semidet.
%
% Wsubst.
%
wsubst(A,B,C,D):-
      catchv(quietly(weak_nd_subst(A,B,C,D)),_,fail),!.
wsubst(A,_B,_C,A).


%= 	 	 

%% weak_nd_subst( ?Var, ?VarS, ?SUB, ?SUB) is semidet.
%
% Weak Nd Subst.
%
weak_nd_subst(  Var, VarS,SUB,SUB ) :- nonvar(Var),Var=VarS,!.
weak_nd_subst(        P, X,Sk,        P1 ) :- functor_catch(P,_,N),weak_nd_subst1( X, Sk, P, N, P1 ).


%= 	 	 

%% weak_nd_subst1( ?X, ?Sk, ?P, ?N, ?P1) is semidet.
%
% Weak Nd Subst Secondary Helper.
%
weak_nd_subst1( _,  _, P, 0, P  ).

weak_nd_subst1( X, Sk, P, N, P1 ) :- N > 0, 
  compound_name_arguments(P,F,Args),
  weak_nd_subst2( X, Sk, Args, ArgS ),
            weak_nd_subst2( X, Sk, [F], [FS] ),
            univ_term(P1 , [FS|ArgS]).


%= 	 	 

%% weak_nd_subst2( ?X, ?Sk, ?L, ?L) is semidet.
%
% Weak Nd Subst Extended Helper.
%
weak_nd_subst2( _,  _, [], [] ).
weak_nd_subst2( X, Sk, [A|As], [Sk|AS] ) :- nonvar(A), X = A, !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [A|AS]  ) :- var(A), !, weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( X, Sk, [A|As], [Ap|AS] ) :- weak_nd_subst( A,X,Sk,Ap ),weak_nd_subst2( X, Sk, As, AS).
weak_nd_subst2( _X, _Sk, L, L ).



%= 	 	 

%% make_list( ?E, ?VALUE2, :TermE) is semidet.
%
% Make List.
%
make_list(E,1,[E]):-!.
make_list(E,N,[E|List]):- M1 is N - 1, make_list(E,M1,List),!.

:- export(flatten_set/2).

%= 	 	 

%% flatten_set( ?L, ?S) is semidet.
%
% Flatten Set.
%
flatten_set(L,S):-flatten([L],F),list_to_set(F,S),!.
%flatten_set(Percepts0,Percepts):- flatten([Percepts0],Percepts1),remove_dupes(Percepts1,Percepts).


%= 	 	 

%% remove_dupes( ?In, ?Out) is semidet.
%
% Remove Dupes.
%
remove_dupes(In,Out):-remove_dupes(In,Out,[]).


%= 	 	 

%% remove_dupes( :TermI, :TermOut, ?Shown) is semidet.
%
% Remove Dupes.
%
remove_dupes([],[],_):-!.
remove_dupes([I|In],Out,Shown):-member(I,Shown),!,remove_dupes(In,Out,Shown).
remove_dupes([I|In],[I|Out],Shown):-remove_dupes(In,Out,[I|Shown]).

% = :- meta_predicate(functor_h(?,?)).

%= 	 	 

%% functor_h( ?Obj, ?F) is nondet.
%
% Functor Head.
%
functor_h(Obj,F):- functor_h(Obj,F,_).

%= 	 	 

%% get_functor( ?Obj, ?FO) is det.
%
% Get Functor.
%
get_functor(Obj,FO):- must(functor_h(Obj,F,_)),!,FO=F.

%= 	 	 

%% get_functor( ?Obj, ?FO, ?AO) is semidet.
%
% Get Functor.
%
get_functor(Obj,FO,AO):- must(functor_h(Obj,F,A)),!,FO=F,AO=A.

% = :- meta_predicate(functor_h(?,?,?)).

%= 	 	 

%% functor_h( ?Obj, ?F, ?A) is semidet.
%
% Functor Head.
%
functor_h(Obj,F,A):- var(Obj),!,trace_or_throw(var_functor_h(Obj,F,A)).
functor_h('$VAR'(Obj),F,A):- !, trace_or_throw(var_functor_h('$VAR'(Obj),F,A)).
% functor_h(Obj,F,A):- var(Obj),!,(number(A)->functor(Obj,F,A);((current_predicate(F/A);throw(var_functor_h(Obj,F,A))))).

functor_h(M:Obj,MF,A):-(callable(Obj);atom(M)),!,functor_h(Obj,F,A),(MF=F;MF=M:F).
functor_h(F//A,
 F,Ap2):-number(A),!,Ap2 is A+2,( atom(F) ->  true ; current_predicate(F/Ap2)).
functor_h(F/A,F,A):-number(A),!,( atom(F) ->  true ; current_predicate(F/A)).
functor_h(Obj,F,A):-atom(F),strip_module(Obj,_M,P),functor(P,F,A).

functor_h([L|Ist],F,A):- is_list([L|Ist]),!,var(F),L=F,length(Ist,A).
functor_h(':-'(Obj,_),F,A):- nonvar(Obj), !,functor_h(Obj,F,A).
functor_h(M:_,F,A):- atom(M),!, ( M=F -> current_predicate(F/A) ; current_predicate(M:F/A)).
functor_h(':-'(Obj),F,A):- nonvar(Obj), !,functor_h(Obj,F,A).

functor_h(Obj,F,0):- string(Obj),!,maybe_notrace(atom_string(F,Obj)).

functor_h(Obj,Obj,0):- \+ compound(Obj),!.
functor_h(Obj,F,0):- compound_name_arity(Obj,F,0),!.
functor_h(Obj,F,A):-var(F),!,strip_module(Obj,_M,P),functor(P,F,A).
functor_h(Obj,F,A):-functor(Obj,F,A).



:- meta_predicate call_n_times(+,0).

%= 	 	 

%% call_n_times( +N, :Goal) is semidet.
%
% Call N Times.
%
call_n_times(0,_Goal):-!.
call_n_times(1,Goal):-!,Goal.
call_n_times(N,Goal):-doall((between(2,N,_),once(Goal))),Goal.


:- meta_predicate at_start(0).
:- meta_predicate remember_at_start(+,0).

:- volatile(lmcache:at_started/1).
:- volatile(lmcache:needs_started/2).
:- dynamic(lmcache:at_started/1).
:- dynamic(lmcache:needs_started/2).

%= 	 	 

%% at_start( :Goal) is semidet.
%
% When Start.
%
at_start(Goal):-
	copy_term(Goal,Named),
	numbervars(Named,0,_,[attvar(bind),singletons(true)]),	
        initialization(remember_at_start(Named,Goal)),
        initialization(remember_at_start(Named,Goal),restore).

remember_at_start(Named,_):- lmcache:at_started(Named),!.
remember_at_start(Named,Goal):- compiling,!,call(assert,lmcache:needs_started(Named,Goal)).
remember_at_start(Named,Goal):-
	 catchv(
		 (call(assert,lmcache:at_started(Named)),on_f_debug((Goal))),
		 E,
		 (retractall(lmcache:at_started(Named)),call(assert,lmcache:needs_started(Named,Goal)),trace_or_throw(E))).

run_at_start:- forall(lmcache:needs_started(Named,Goal),remember_at_start(Named,Goal)).


:- initialization(run_at_start).
:- initialization(run_at_start,restore).

:- export(list_to_set_safe/2).

%= 	 	 

%% list_to_set_safe( :TermA, ?A) is semidet.
%
% List Converted To Set Safely Paying Attention To Corner Cases.
%
list_to_set_safe(A,A):-(var(A);atomic(A)),!.
list_to_set_safe([A|AA],BB):- ( \+ ( \+ (lastMember2(A,AA))) -> list_to_set_safe(AA,BB) ; (list_to_set_safe(AA,NB),BB=[A|NB])),!.





%= 	 	 

%% term_parts( ?A, :TermA) is semidet.
%
% Term Parts.
%
term_parts(A,[A]):- not(compound(A)),!.
term_parts([A|L],TERMS):-!,term_parts_l([A|L],TERMS).
term_parts(Comp,[P/A|TERMS]):- functor_catch(Comp,P,A), Comp=..[P|List],term_parts_l(List,TERMS).


%= 	 	 

%% term_parts_l( :TermVar, ?Var) is semidet.
%
% Term Parts (list Version).
%
term_parts_l(Var,[open(Var),Var]):-var(Var),!.
term_parts_l([],[]):-!.
term_parts_l([A|L],TERMS):-!,term_parts(A,AP),term_parts_l(L,LP),append(AP,LP,TERMS).
term_parts_l(Term,[open(Term)|TERMS]):-term_parts(Term,TERMS),!.


%= 	 	 

%% pred_term_parts( :PRED1Pred, ?Comp, ?TERMS) is semidet.
%
% Predicate Term Parts.
%
pred_term_parts(Pred,A,[A]):- call(Pred,A),!.
pred_term_parts(_Pred,A,[]):-not(compound(A)),!.
pred_term_parts(Pred,[A|L],TERMS):-!,pred_term_parts_l(Pred,[A|L],TERMS),!.
pred_term_parts(Pred,Comp,TERMS):-Comp=..[P,A|List],pred_term_parts_l(Pred,[P,A|List],TERMS),!.
pred_term_parts(_,_Term,[]).


%= 	 	 

%% pred_term_parts_l( :PRED1Pred, ?Term, ?TERMS) is semidet.
%
% Predicate Term Parts (list Version).
%
pred_term_parts_l(_,NV,[]):-NV==[],!.
pred_term_parts_l(Pred,[A|L],TERMS):-!,pred_term_parts(Pred,A,AP),pred_term_parts_l(Pred,L,LP),append(AP,LP,TERMS),!.
pred_term_parts_l(Pred,Term,TERMS):-pred_term_parts(Pred,Term,TERMS),!.
pred_term_parts_l(_,_Term,[]).


%= 	 	 

%% throw_if_true_else_fail( :GoalT, ?E) is semidet.
%
% Throw If True Else Fail.
%
throw_if_true_else_fail(T,E):- once(quietly(T)),trace_or_throw(throw_if_true_else_fail(E:T)).


%= 	 	 

%% list_retain( ?PL, :PRED1Pred, ?Result) is semidet.
%
% List Retain.
%
list_retain(PL,Pred,Result):- throw_if_true_else_fail(not(is_list(PL)),list_retain(PL,Pred,Result)).
list_retain([],_Pred,[]):-!.
list_retain([R|List],Pred,[R|Retained]):- call(Pred,R),!, list_retain(List,Pred,Retained).
list_retain([_|List],Pred,Retained):- list_retain(List,Pred,Retained).

:- export(identical_memberchk/2).

%= 	 	 

%% identical_memberchk( ?X, :TermY) is semidet.
%
% Identical Member.
%
identical_memberchk(_,Var):-var(Var),!,fail.
identical_memberchk(X,[Y|_])  :-
	X == Y,
	!.
identical_memberchk(X,[_|L]) :-
	identical_memberchk(X,L).

:- export(delete_eq/3).
:- export(pred_delete/4).

%= 	 	 

%% delete_eq( ?A, ?B, ?C) is semidet.
%
% Delete Using (==/2) (or =@=/2) ).
%
delete_eq(A,B,C):-pred_delete(==,A,B,C).

%= 	 	 

%% pred_delete( :PRED2Pred, ?A, ?B, ?D) is semidet.
%
% Predicate Delete.
%
pred_delete(_,[], _, []).
pred_delete(Pred,[A|C], B, D) :-
        (    call(Pred,A,B)
        ->  pred_delete(Pred,C, B, D)
        ;   D=[A|E],
            pred_delete(Pred,C, B, E)
        ).



:- export(doall/1).
:- meta_predicate doall(0).

%= 	 	 

%% doall( :GoalC) is semidet.
%
% Doall.
%
doall(M:C):-!, M:ignore(M:(C,fail)).
doall(C):-ignore((C,fail)).

% =================================================================================
% Loader Utils
% =================================================================================



%= 	 	 

%% dynamic_load_pl( ?PLNAME) is semidet.
%
% Dynamic Load Pl.
%
dynamic_load_pl(PLNAME):-ensure_loaded(PLNAME),!.

dynamic_load_pl(PLNAME):- % unload_file(PLNAME),
   open(PLNAME, read, In, []),
   repeat,
   line_count(In,Lineno),
   % double_quotes(_DQBool)
   Options = [variables(_Vars),variable_names(_VarNames),singletons(_Singletons),comment(_Comment)],
   catchv((read_term(In,Term,[syntax_errors(error)|Options])),E,(dmsg(E),fail)),
   load_term(Term,[line_count(Lineno),file(PLNAME),stream(In)|Options]),
   Term==end_of_file,
   close(In).


%= 	 	 

%% load_term( ?E, ?Options) is semidet.
%
% Load Term.
%
load_term(E,_Options):- E == end_of_file, !.
load_term(Term,Options):-catchv(load_term2(Term,Options),E,(dmsg(error(load_term(Term,Options,E))),throw_safe(E))).


%= 	 	 

%% load_term2( :TermFact, ?Options) is semidet.
%
% Load Term Extended Helper.
%
load_term2(':-'(Term),Options):-!,load_dirrective(Term,Options),!.
load_term2(:-(H,B),Options):-!,load_assert(H,B,Options).
load_term2(Fact,Options):-!,load_assert(Fact,true,Options).


%= 	 	 

%% load_assert( ?H, ?B, ?Options) is semidet.
%
% Load Assert.
%
load_assert(H,B,_Options):-assert((H:-B)),!.


%= 	 	 

%% load_dirrective( :GoalCALL, ?Options) is semidet.
%
% Load Dirrective.
%
load_dirrective(include(PLNAME),_Options):- (atom_concat_safe(Key,'.pl',PLNAME) ; Key=PLNAME),!, dynamic_load_pl(Key).
load_dirrective(CALL,_Options):- CALL=..[module,M,_Preds],!,module(M),call(CALL).
load_dirrective(Term,_Options):-!,Term.

%% atom_concat_safe( ?L, ?R, ?A) is semidet.
%
% Atom Concat Safely Paying Attention To Corner Cases.
%
atom_concat_safe(L,R,A):- ((atom(A),(atom(L);atom(R))) ; ((atom(L),atom(R)))), !, atom_concat(L,R,A),!.

% =====================================================================================================================
:- export((call_no_cuts/1)).
% =====================================================================================================================
:- meta_predicate call_no_cuts(0).
:- module_transparent call_no_cuts/1.

%= 	 	 

%% call_no_cuts( :GoalCALL) is semidet.
%
% Call No Cuts.
%
call_no_cuts(ereq(A)):-!,call_no_cuts(A).
call_no_cuts(call_u(A)):-!,call_no_cuts(A).
call_no_cuts((A,B)):-!,(call_no_cuts(A),call_no_cuts(B)).
call_no_cuts((A;B)):-!,(call_no_cuts(A);call_no_cuts(B)).
call_no_cuts((A->B)):-!,(call_no_cuts(A)->call_no_cuts(B)).
call_no_cuts((A*->B;C)):-!,(call_no_cuts(A)->call_no_cuts(B);call_no_cuts(C)).
call_no_cuts((A->B;C)):-!,(call_no_cuts(A)->call_no_cuts(B);call_no_cuts(C)).
call_no_cuts(M:CALL):-atom(M),!,functor(CALL,F,A),functor(C,F,A),must(once( \+ ( \+ (clause(M:C,_))))),!,clause(M:CALL,TEST),M:on_x_debug(TEST).
call_no_cuts(CALL):-functor(CALL,F,A),functor(C,F,A),must(once( \+ ( \+ (clause(C,_))))),!,clause(CALL,TEST),on_x_debug(TEST).


% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_lib(WAS)).

%:- module_predicates_are_exported.
% :- all_module_predicates_are_transparent(logicmoo_util_terms).


%= 	 	 

%% logicmoo_library_file_loaded is semidet.
%
% Logicmoo Library File Loaded.
%
logicmoo_library_file_loaded.

:- fixup_exports.
