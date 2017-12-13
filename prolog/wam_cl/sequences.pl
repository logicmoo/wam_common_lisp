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

:- include('header').

prims:cl_exact.

%module(_,_).

:- ensure_loaded((utils_writef)).
:- ensure_loaded(library(lists)).

:- use_module(library('dialect/sicstus/arrays')).
% :- use_module(library('dialect/sicstus')).

% Numbers, pathnames, and arrays are examples of self-evaluating objects.
is_self_evaluating_object(X):- var(X),!.
is_self_evaluating_object(X):- atomic(X),!,is_self_evaluationing_const(X).
is_self_evaluating_object('$OBJ'(_,_)):-!.
is_self_evaluating_object('#\\'(_)):-!.
is_self_evaluating_object(X):- (is_dict(X);is_array(X);is_rbtree(X)),!.
is_self_evaluating_object(P):- compound_name_arity(P,F,_),atom_concat_or_rtrace('$',_,F),!.

is_self_evaluationing_const(X):- atomic(X),is_self_evaluationing_const0(X),!.
is_self_evaluationing_const0(X):- (X==t;X==[];number(X);is_keywordp(X);string(X);(blob(X,T),T\==text)),!.
is_self_evaluationing_const0(X):- is_functionp(X),!.

is_functionp(X):- \+ atom(X),!,fail.
is_functionp(X):- atom_concat_or_rtrace('f_',_,X),!.
is_functionp(X):- atom_concat_or_rtrace('cl_',_,X),!.
is_consp(Obj):- nonvar(Obj),Obj=[_|_].

%:- dynamic(op_replacement/2).
wl:op_replacement(first,cl_car).
cl_car(List, Result):- 
  (List = [Result|_] -> true;
  (List==[] -> Result=[];
  (	error(first_not_cons, ErrNo, _),
		throw(ErrNo)))).

wl:op_replacement(rest,cl_cdr).
cl_cdr(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).


wl:op_replacement(setcar,cl_rplaca).
cl_rplaca(Cons,Obj,Cons):- nb_setarg(1,Cons,Obj).
f_sys_set_car(A,B,C):-cl_rplaca(A,B,C).

wl:op_replacement(setcdr,cl_rplacd).
cl_rplacd(Cons,Obj,Cons):- nb_setarg(2,Cons,Obj).
f_sys_set_cdr(A,B,C):-cl_rplacd(A,B,C).


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


% assoc item alist
cl_assoc(Key,List,KV):- member(KV,List),KV=[Key|_],!.
cl_assoc(_Key,_List,[]).

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
cl_atom(Obj,Ret):-  t_or_nil( Obj\=[_|_] , Ret).
cl_consp(Obj,RetVal):- t_or_nil(is_consp(Obj),RetVal).
cl_functionp(Obj,RetVal):- t_or_nil(is_functionp(Obj),RetVal).

:-assertz(wl:arg_lambda_type(rest_only,cl_nconc)).
cl_nconc([L1,L2],Ret):- !, append(L1,L2,Ret).
cl_nconc([L1],L1):-!.
cl_nconc([L1,L2|Lists],Ret):- !,cl_nconc([L2|Lists],LL2), append(L1,LL2,Ret).
cl_nconc(X,X):-!.

cl_copy_list(List,List):- \+ compound(List),!.
cl_copy_list([M|List],[M|Copy]):-cl_copy_list(List,Copy).



:-assertz(wl:arg_lambda_type(req(1),cl_last)).
cl_last(List,[],Tail):-  !, cl_last_1(List,Tail).
cl_last(List,[N],Ret):- 
  (N=1 -> cl_last_1(List,Ret);
  (N=0 -> cl_last_0(List,Ret);
 (( length([R|RightM1],N),
  [R|RightM1]=Right,
  (append(_,Right,List)->Ret=Right;
  (append(_,List,Right)->Ret=List;
  ((cl_last_1(List,R1),   
    append(RightM1,R1,Ret),
      append(_,Ret,List))->true;Ret=List))))))).

cl_last_0([_|List],R):- !, cl_last_0(List,R).
cl_last_0(A,A).
cl_last_1([A],[A]):-!.
cl_last_1([_,H|List],R):- !, cl_last_1([H|List],R).
cl_last_1([A|R],[A|R]):-!.
cl_last_1(_,[]).

/*
 (last nil) =>  NIL
 (last '(1 2 3)) =>  (3)
 (last '(1 2 . 3)) =>  (2 . 3)
 (setq x (list 'a 'b 'c 'd)) =>  (A B C D)
 (last x) =>  (D)
 (rplacd (last x) (list 'e 'f)) x =>  (A B C D E F)
 (last x) =>  (F)

 (last '(a b c))   =>  (C)

 (last '(a b c) 0) =>  ()
 (last '(a b c) 1) =>  (C)
 (last '(a b c) 2) =>  (B C)
 (last '(a b c) 3) =>  (A B C)
 (last '(a b c) 4) =>  (A B C)

 (last '(a . b) 0) =>  B
 (last '(a . b) 1) =>  (A . B)
 (last '(a . b) 2) =>  (A . B)
*/
cl_eq(A,B,Ret):- t_or_nil( is_eq(A,B) , Ret).
cl_eql(A,B,Ret):- t_or_nil( is_eql(A,B) , Ret).
cl_equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).
cl_equalp(A,B,Ret):- t_or_nil( is_equalp(A,B) , Ret).



is_eql(X,Y):- is_eq(X,Y)->true;cl_type_of(X,T),cl_type_of(Y,T), notrace(catch(X=:=Y,_,fail)).
is_eq(X,Y):- same_term(X,Y).
% is_eq(X,Y):- X==Y, (\+ compound(X)-> true ; \+ \+ ((gensym(cookie,Cook),setarg(1,X,Cook),X==Y))).
is_equal(X,Y):- (X=@=Y->true;is_eql(X,Y)).
is_equalp(X,Y):- is_equal(X,Y)->true;((f_u_to_pvs(X,XX),f_u_to_pvs(Y,YY), XX=@=YY)-> true ; ( \+ X\=Y)).

personal_props(sname).
personal_props(ref).
personal_props(classof).
personal_props(instance).
f_u_to_pvs(X,[float|XX]):- notrace(catch(XX is (1.0 * X),_,fail)),!.
f_u_to_pvs(X,XX):- findall([P|V],((get_opv_ii(X,P,V),\+ personal_props(P))),List),
  List\==[],sort(List,XX),!.
f_u_to_pvs(X,[str|XX]):- format(string(S),'~w',[X]),string_upper(S,XX),!.



f_ext_quit(ExitCode,Ret):- trace,t_or_nil(halt(ExitCode),Ret).


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

