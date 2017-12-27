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

:- set_module(class(library)).

:- include('header').

%module(_,_).

:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).


t_or_nil(G,Ret):- G->Ret=t;Ret=[].

cl_not(Obj,Ret):- t_or_nil(Obj == [] , Ret).

cl_eq(A,B,Ret):- t_or_nil( is_eq(A,B) , Ret).
cl_eql(A,B,Ret):- t_or_nil( is_eql(A,B) , Ret).
cl_equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).
cl_equalp(A,B,Ret):- t_or_nil( is_equalp(A,B) , Ret).
equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).


is_eql(X,Y):- is_eq(X,Y)->true;cl_type_of(X,T),cl_type_of(Y,T), notrace(catch(X=:=Y,_,fail)).
is_eq(X,Y):- same_term(X,Y).
% is_eq(X,Y):- X==Y, (\+ compound(X)-> true ; \+ \+ ((gensym(cookie,Cook),setarg(1,X,Cook),X==Y))).
is_equal(X,Y):- (X=@=Y->true;is_eql(X,Y)).
is_equalp(X,Y):- is_equal(X,Y)->true;((f_u_to_pvs(X,XX),f_u_to_pvs(Y,YY), XX=@=YY)-> true ; ( \+ X\=Y)).




f_ext_quit(ExitCode,Ret):- trace,t_or_nil(halt(ExitCode),Ret).



is_special_var_c(_,_):-!,fail.
sym_arg_val_envc(N,A,B,_) :- is_special_var_c(N,B) -> true ; A = B.




show_special:-
		setof(Package:Var=Type:Value, symp:symbol_info(Var, Package, Type, Value), SVs)
	->	writef('Variable \tValue\n\n'),
		every(SVs, [(Var2 = Value2)]^(writef('%t :\t%t\n',[Var2, Value2])))
	;	writef('No special variables\n').


:- fixup_exports.

end_of_file.

