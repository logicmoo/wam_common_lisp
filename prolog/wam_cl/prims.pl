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
:- module(prims,[]).

:- set_module(class(library)).

:- include('header.pro').

prims:cl_exact.

%module(_,_).

:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).

:- use_module(library('dialect/sicstus/arrays')).
% :- use_module(library('dialect/sicstus')).

% Numbers, pathnames, and arrays are examples of self-evaluating objects.
is_self_evaluationing_object(X):- var(X),!.
is_self_evaluationing_object(X):- atomic(X),!,is_self_evaluationing_const(X).
is_self_evaluationing_object('$OBJ'(_,_)):-!.
is_self_evaluationing_object('$CHAR'(_)):-!.

is_self_evaluationing_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.

is_self_evaluationing_const(X):- atomic(X),is_self_evaluationing_const0(X),!.
is_self_evaluationing_const0(X):- (X==t;X==[];number(X);is_keywordp(X);string(X);(blob(X,T),T\==text)),!.
is_self_evaluationing_const0(X):- is_functionp(X).

is_functionp(X):- \+ atom(X),!,fail.
is_functionp(X):- atom_concat_or_rtrace('f_',_,X),!.
is_functionp(X):- atom_concat_or_rtrace('cl_',_,X),!.

%:- dynamic(op_replacement/2).
user:op_replacement(first,cl_car).
cl_car(List, Result):- 
  (List = [Result|_] -> true;
  (List==[] -> Result=[];
  (	error(first_not_cons, ErrNo, _),
		throw(ErrNo)))).

user:op_replacement(rest,cl_cdr).
cl_cdr(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).


user:op_replacement(setcar,cl_rplaca).
cl_rplaca(Cons,Obj,Cons):- nb_setarg(1,Cons,Obj).

user:op_replacement(setcdr,cl_rplacd).
cl_rplacd(Cons,Obj,Cons):- nb_setarg(2,Cons,Obj).


wl:declared(cl_cons,inline(cons)).
cl_cons(Item,
 List, Result):-
	Result = [Item|List].

cl_append(A,B,R):- append(A,B,R),!.

wl:declared(cl_error,lambda(['&rest',r])).
cl_error(Args,Res):- cl_format([t|Args],Res),throw(cl_error(Args,Res)).

wl:declared(cl_list,lambda(['&rest',r])).
wl:declared(cl_list,inline(list)).
wl:declared(cl_list,uses_rest).
cl_list(List,List).

cl_plus(Num1, Num2, Result):-Result is Num1 + Num2.

cl_minus(Num1, Num2, Result):-
	Result is Num1 - Num2.
cl_times(Num1, Num2, Result):-
	Result is Num1 * Num2.
cl_divide(Num1, Num2, Result):-
	Result is Num1 / Num2.


cl_lisp_not(Boolean, Result):-
		Boolean = []
	->	Result = t
	;	Result = [].


/*
Wrongness
cl_or(Bool1, Bool2, Result):-
		once( (Bool1 \= [] ; Bool2 \= []))
	->	Result = t
	;	Result = [].

cl_and(Bool1, Bool2, Result):-
		(Bool1 \= [] , Bool2 \= [])
	->	Result = t
	;	Result = [].

*/


cl_mapcar(P, [H|T], [RH|RT]) :- !, cl_apply(P, [H], RH),cl_mapcar(P, T, RT).
cl_mapcar(_, [], []).


cl_apply(closure(Environment,ClosureResult,FormalArgs,Body), Arguments, Result):-!,
  closure(Environment,ClosureResult,FormalArgs,Body,Arguments,Result).
cl_apply(function(FunctionName), Arguments, Result):-!,cl_apply((FunctionName), Arguments, Result).
cl_apply((FunctionName), Arguments, Result):-!,
  lisp_compiled_eval([FunctionName|Arguments],Result).


lisp_call(Function, Result):-
	apply(Function, [Result]).


% negation can be over existence,  future possiblity or past existence, we say there exists some truth in which  


t_or_nil(G,Ret):- G->Ret=t;Ret=[].

cl_not(Obj,Ret):- t_or_nil(Obj == [] , Ret).
cl_null(Obj,Ret):- t_or_nil(Obj == [] , Ret).

f_c61(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret). 
=(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret).

cl_eq(A,B,Ret):- t_or_nil( is_eq(A,B) , Ret).
cl_eql(A,B,Ret):- t_or_nil( is_eql(A,B) , Ret).
cl_equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).
cl_equalp(A,B,Ret):- t_or_nil( is_equalp(A,B) , Ret).


is_eql(X,Y):- same_term(X,Y)->true;cl_type_of(X,T),cl_type_of(Y,T), notrace(catch(X=:=Y,_,fail)).
is_eq(X,Y):- same_term(X,Y).
% is_eq(X,Y):- X==Y, (\+ compound(X)-> true ; \+ \+ ((gensym(cookie,Cook),setarg(1,X,Cook),X==Y))).
is_equal(X,Y):- (X=@=Y->true;is_eql(X,Y)).
is_equalp(X,Y):- is_equal(X,Y)->true;((f_u_to_pvs(X,XX),f_u_to_pvs(Y,YY), XX=@=YY)-> true ; ( \+ X\=Y)).

f_u_to_pvs(X,[float|XX]):- notrace(catch(XX is (1.0 * X),_,fail)),!.
f_u_to_pvs(X,XX):- findall([P|V],(get_opv(X,P,V);get_struct_opv(X,P,V)),List),List\==[],sort(List,XX),!.
f_u_to_pvs(X,[str|XX]):- format(string(S),'~w',[X]),string_upper(S,XX),!.

make_character('$CHAR'(Int),'$CHAR'(Int)):-!.
make_character(Int,'$CHAR'(Int)).

f_ext_quit(ExitCode,Ret):- trace,t_or_nil(halt(ExitCode),Ret).

as_list(Str,List):-string(Str),atom_chars(Str,Chars),maplist(make_character,Chars,List).

cl_subseq(Seq,Offset,Result):- as_list(Seq,List), length(Left,Offset),append(Left,Result,List).

cl_sqrt(X,Y):- \+ integer(X)-> (Y is sqrt(X)) ; (IY is sqrt(X), RY is floor(IY),(RY=:=IY -> Y=RY ; Y=IY)).

f_u_c43(N1,N2,Ret):- Ret is (N1 + N2).
+(N1,N2,Ret):- Ret is (N1 + N2).

f_u_c45(N1,N2,Ret):- Ret is (N1 + N2).
-(N1,N2,Ret):- Ret is (N1 - N2).

f_u_c42(N1,N2,Ret):- Ret is (N1 + N2).
*(N1,N2,Ret):- Ret is (N1 * N2).

f_u_c47(N1,N2,Ret):- Ret is (N1 + N2).
'/'(N1,N2,Ret):- Ret is (N1 / N2).

<(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
>(N1,N2,Ret):- t_or_nil(>(N1,N2),Ret). 

'1+'(N,Ret):- Ret is N + 1.
'1-'(N,Ret):- Ret is N - 1.

is_special_var_c(_,_):-!,fail.
sym_arg_val_envc(N,A,B,_) :- is_special_var_c(N,B) -> true ; A = B.




show_special:-
		setof(Package:Var=Type:Value, symp:symbol_info(Var, Package, Type, Value), SVs)
	->	writef('Variable \tValue\n\n'),
		every(SVs, [(Var2 = Value2)]^(writef('%t :\t%t\n',[Var2, Value2])))
	;	writef('No special variables\n').



make_accessor(cadr).

make_accessor(cdar).

make_accessor(cddr).

make_accessor(caaar).

make_accessor(caadr).

make_accessor(cadar).

make_accessor(caddr).

make_accessor(cdaar).

make_accessor(cdadr).

make_accessor(cddar).

make_accessor(cdddr).

make_accessor(caaaar).

make_accessor(caaadr).

make_accessor(caadar).

make_accessor(caaddr).

make_accessor(cadaar).

make_accessor(cadadr).

make_accessor(caddar).

make_accessor(cadddr).

make_accessor(cdaaar).

make_accessor(cdaadr).

make_accessor(cdadar).

make_accessor(cdaddr).

make_accessor(cddaar).

make_accessor(cddadr).

make_accessor(cdddar).

make_accessor(cddddr).

/*
(caar x)        (car (car x))                    
(cadr x)        (car (cdr x))                    
(cdar x)        (cdr (car x))                    
(cddr x)        (cdr (cdr x))                    
(caaar x)       (car (car (car x)))              
(caadr x)       (car (car (cdr x)))              
(cadar x)       (car (cdr (car x)))              
(caddr x)       (car (cdr (cdr x)))              
(cdaar x)       (cdr (car (car x)))              
(cdadr x)       (cdr (car (cdr x)))              
(cddar x)       (cdr (cdr (car x)))              
(cdddr x)       (cdr (cdr (cdr x)))              
(caaaar x)      (car (car (car (car x))))        
(caaadr x)      (car (car (car (cdr x))))        
(caadar x)      (car (car (cdr (car x))))        
(caaddr x)      (car (car (cdr (cdr x))))        
(cadaar x)      (car (cdr (car (car x))))        
(cadadr x)      (car (cdr (car (cdr x))))        
(caddar x)      (car (cdr (cdr (car x))))        
(cadddr x)      (car (cdr (cdr (cdr x))))        
(cdaaar x)      (cdr (car (car (car x))))        
(cdaadr x)      (cdr (car (car (cdr x))))        
(cdadar x)      (cdr (car (cdr (car x))))        
(cdaddr x)      (cdr (car (cdr (cdr x))))        
(cddaar x)      (cdr (cdr (car (car x))))        
(cddadr x)      (cdr (cdr (car (cdr x))))        
(cdddar x)      (cdr (cdr (cdr (car x))))        
(cddddr x)      (cdr (cdr (cdr (cdr x))))  


symbol_info(Sym,P,function,O),symbol_info(Sym,P,function_type,FT),symbol_info(Sym,P,name,Name),
  format('~N% ~w (~w ~w)~n~q(A,Result):- ...\n\n',[Name,FT,P, O]),nl,fail.

*/      



:- fixup_exports.

