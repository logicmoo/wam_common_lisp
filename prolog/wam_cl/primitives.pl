/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (builtin_lisp_functions.pl)
 *
 * (c) Neil Smith, 2001
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * This program provides some built-in functionality for the 
 * Lisp compiler.  It requires that the file lisp_compiler.pl has 
 * already been successfully compiled.
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(eq4l1y,[]).

:- meta_predicate t_or_nil(0,*).

:- include('./header').

%module(_,_).

%:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).

legal_for_pred_props(Here):- Here \= (:-), \+ functor(Here, (':'), _).

wl:init_args(x,Here):- legal_for_pred_props(Here), functor(P,Here,3),predicate_property(P,imported_from(eq4l1y)).
wl:init_args(x,Here):- legal_for_pred_props(Here), functor(P,Here,2),predicate_property(P,imported_from(eq4l1y)).

t_or_nil(G,Ret):- G->Ret=t;Ret=[].

f_not(Obj,Ret):- t_or_nil(Obj == [] , Ret).

f_eq(A,B,Ret):- t_or_nil( is_eq(A,B) , Ret).
f_eql(A,B,Ret):- t_or_nil( is_eql(A,B) , Ret).
f_equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).
f_equalp(A,B,Ret):- t_or_nil( is_equalp(A,B) , Ret).
equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).


is_eql(X,Y):- is_eq(X,Y)->true;((f_type_of(X,T),f_type_of(Y,T)),
  (T==character -> X=Y ; notrace(catch(X=:=Y,_,fail)))).
is_eq(X,Y):- same_term(X,Y).
% is_eq(X,Y):- X==Y, (\+ compound(X)-> true ; \+ \+ ((gensym(cookie,Cook),setarg(1,X,Cook),X==Y))).
is_equal(X,Y):- (X=@=Y->true;is_eql(X,Y)).
is_equalp(X,Y):- is_equal(X,Y)->true;((f_sys_to_pvs(X,XX),f_sys_to_pvs(Y,YY), XX=@=YY)-> true ; ( \+ X\=Y)).




f_sys_quit(ExitCode,Ret):- trace,t_or_nil(halt(ExitCode),Ret).



is_special_var_c(_,_):-!,fail.
sym_arg_val_envc(N,A,B,_) :- is_special_var_c(N,B) -> true ; A = B.


%   every(List, Pred)
%   suceeds when Pred(Elem) succeeds for each Elem in the List.

every([], _Pred):-!.
every([Head|Tail], Pred) :-
        lpa_apply(Pred, [Head]),
        every(Tail, Pred).


show_special:-
		setof(Var=Value, get_opv_iiii(Var,symbol_value, Value), SVs)
	->	writef('Variable \tValue\n\n'),
		every(SVs, [(Var2 = Value2)]^(writef('%t :\t%t\n',[Var2, Value2])))
	;	writef('No special variables\n').


:- fixup_exports.

end_of_file.

