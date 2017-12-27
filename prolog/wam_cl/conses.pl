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
:- module(c0nz,[]).

:- set_module(class(library)).

:- include('header').


% #'CONSP
cl_consp(Obj,RetVal):- t_or_nil(is_consp(Obj),RetVal).
is_consp(Obj):- nonvar(Obj),Obj=[_|_].
% negation can be over existence,  future possiblity or past existence, we say there exists some truth in which  

% #'NULL
cl_null(Obj,Ret):- t_or_nil(Obj == [] , Ret).

% #'ATOM
cl_atom(Obj,Ret):-  t_or_nil( Obj\=[_|_] , Ret).

% #'CONS
wl:declared(cl_cons,inline(cons)).
cl_cons(Item, List, Result):- Result = [Item|List].

% #'APPEND
cl_append(A,B,R):- append(A,B,R),!.

% #'LIST
wl:declared(cl_list,inline(list)).
wl:init_args(0,cl_list).
cl_list(ListI,ListO):- ListI=ListO.


%:- dynamic(op_replacement/2).
wl:op_replacement(first,car).
f_ext_pf_car(List, Result):-cl_car(List, Result).
cl_first(List, Result):-cl_car(List, Result).
cl_car(List, Result):- 
  (List = [Result|_] -> true;
  (List==[] -> Result=[];
  (	error(first_not_cons, ErrNo, _),
		throw(ErrNo)))).

wl:op_replacement(rest,cdr).
wl:op_replacement(ext_pf_cdr,cdr).
f_ext_pf_cdr(List, Result):-cl_cdr(List, Result).
f_u_pf_cdr(List, Result):-cl_cdr(List, Result).
cl_cdr(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).

f_u_pf_cddr(A,C):-f_u_pf_cdr(A,B),f_u_pf_cdr(B,C).

wl:interned_eval(("`cl:rplaca")).
wl:op_replacement(setcar,rplaca).
wl:init_args(exact_only,cl_rplaca).
cl_rplaca(Cons,Obj,Cons):- nb_setarg(1,Cons,Obj).
f_sys_set_car(A,B,C):-cl_rplaca(A,B,C).

wl:op_replacement(setcdr,rplacd).
wl:interned_eval(("`cl:rplacd")).
wl:init_args(exact_only,cl_rplacd).
cl_rplacd(Cons,Obj,Cons):- nb_setarg(2,Cons,Obj).
f_sys_set_cdr(A,B,C):-cl_rplacd(A,B,C).


cl_copy_alist([H|T],[HH|TT]):- !, assertion(nonvar(H)),
  pl_copy_1assoc(H,HH),
  cl_copy_alist(T,TT).
cl_copy_alist(T,T).
pl_copy_1assoc([H|T],[H|T]).
pl_copy_1assoc(HT,HT).

% #'ACONS
wl: init_args(exact_only, cl_acons).
cl_acons(Key_Param, Datum_Param, Alist_Param, [[Key_Param|Datum_Param]|Alist_Param]).

wl:interned_eval((
"(defconstant +upgraded-array-element-types+ 
   '(NIL BASE-CHAR CHARACTER BIT EXT:BYTE8 EXT:INTEGER8 EXT:BYTE16 EXT:INTEGER16
 EXT:BYTE32 EXT:INTEGER32 EXT:BYTE64 EXT:INTEGER64 SINGLE-FLOAT DOUBLE-FLOAT T))")).

wl:interned_eval(("`SYS:MAKE-VECTOR")).


% #'rassoc item alist &key key test test-not => entry
wl:init_args(2,cl_rassoc).
cl_rassoc(Item,AList,Options,RetVal):-
 get_identity_pred(Options,kw_key,Ident),
  get_test_pred(cl_eql,Options,EqlPred),
  (member([K|V],AList),call(Ident,V,Id),
    (call(EqlPred,Item,Id,R)->R\==[])),!,
  RetVal = [K|V].
cl_rassoc(_,_,_,[]).

wl:init_args(2,cl_assoc).
cl_assoc(Item,AList,Options,RetVal):-
 get_identity_pred(Options,kw_key,Ident),
  get_test_pred(cl_eql,Options,EqlPred),
  (member([K|V],AList),call(Ident,K,Id),
    (call(EqlPred,Item,Id,R)->R\==[])),!,
  RetVal = [K|V].
cl_assoc(_,_,_,[]).


% assoc item alist

cl_second(List,R):- List=[_,R|_]->true;R=[].
f_ext_pf_cadr(List,R):- List=[_,R|_]->true;R=[].
cl_cadr(List,R):- List=[_,R|_]->true;R=[].


(wl:init_args(1,cl_last)).
cl_last(List,[],Tail):-  !, cl_last_1(List,Tail).
cl_last(List,[N],Ret):- 
  (N=1 -> cl_last_1(List,Ret);
  (N=0 -> cl_last_0(List,Ret);
 (( always(length([R|RightM1],N)),
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
ensure_cl_contains([
       caaar,
       caadr,
       caar,
       cadar,
       caddr,
       cdaar,
       cdadr,
       cdar,
       cddar,
       cddddr,
       cdddr,
       cddr]).
*/
cl_cddr(X, Cdr_Ret) :-
  cl_cdr(X, Cdr_Param),
  cl_cdr(Cdr_Param, Cdr_Ret).

cl_cdar(X, Cdr_Ret) :-
  cl_car(X, Cdr_Param),
  cl_cdr(Cdr_Param, Cdr_Ret).

cl_caar(X, Cdr_Ret) :-
  cl_car(X, Cdr_Param),
  cl_car(Cdr_Param, Cdr_Ret).

cl_caaar(X, Cdr_Ret) :-
  cl_car(X, Y),
  cl_car(Y, Z),
  cl_car(Z, Cdr_Ret).

cl_cdaar(X, Cdr_Ret) :-
  cl_car(X, Y),
  cl_car(Y, Z),
  cl_cdr(Z, Cdr_Ret).

cl_cadar(X, Cdr_Ret) :-
  cl_car(X, Y),
  cl_cdr(Y, Z),
  cl_car(Z, Cdr_Ret).

cl_cdadr(X, Cdr_Ret) :-
  cl_cdr(X, Y),
  cl_car(Y, Z),
  cl_cdr(Z, Cdr_Ret).

cl_caadr(X, Cdr_Ret) :-
  cl_cdr(X, Y),
  cl_car(Y, Z),
  cl_car(Z, Cdr_Ret).

cl_caddr(X, Cdr_Ret) :-
  cl_cdr(X, Y),
  cl_cdr(Y, Z),
  cl_car(Z, Cdr_Ret).

cl_cdddr(X, Cdr_Ret) :-
  cl_cdr(X, Y),
  cl_cdr(Y, Z),
  cl_cdr(Z, Cdr_Ret).

cl_cddar(X, Cdr_Ret) :-
  cl_car(X, Y),
  cl_cdr(Y, Z),
  cl_cdr(Z, Cdr_Ret).

cl_cddddr(W, Cdr_Ret) :-
  cl_cdr(W, X),
  cl_cdr(X, Y),
  cl_cdr(Y, Z),
  cl_cdr(Z, Cdr_Ret).

:- fixup_exports.

      
end_of_file.


