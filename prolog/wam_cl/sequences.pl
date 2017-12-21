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


is_consp(Obj):- nonvar(Obj),Obj=[_|_].

%:- dynamic(op_replacement/2).
wl:op_replacement(first,cl_car).
f_ext_pf_car(List, Result):-cl_car(List, Result).
cl_first(List, Result):-cl_car(List, Result).
cl_car(List, Result):- 
  (List = [Result|_] -> true;
  (List==[] -> Result=[];
  (	error(first_not_cons, ErrNo, _),
		throw(ErrNo)))).

wl:op_replacement(rest,cl_cdr).
wl:op_replacement(ext_pf_cdr,cl_cdr).
f_ext_pf_cdr(List, Result):-cl_cdr(List, Result).
f_u_pf_cdr(List, Result):-cl_cdr(List, Result).
cl_cdr(List, Result):- List==[]->Result=[];
	once( (	List = [_|Result]
	    ;	error(rest_not_cons, ErrNo, _),
		throw(ErrNo)	)).

f_u_pf_cddr(A,C):-f_u_pf_cdr(A,B),f_u_pf_cdr(B,C).

wl:op_replacement(setcar,cl_rplaca).
wl:init_args(exact_only,cl_rplaca).
cl_rplaca(Cons,Obj,Cons):- nb_setarg(1,Cons,Obj).
f_sys_set_car(A,B,C):-cl_rplaca(A,B,C).

%wl:op_replacement(setcdr,cl_rplacd).
wl:init_args(exact_only,cl_rplacd).
cl_rplacd(Cons,Obj,Cons):- nb_setarg(2,Cons,Obj).
f_sys_set_cdr(A,B,C):-cl_rplacd(A,B,C).


wl:declared(cl_cons,inline(cons)).
cl_cons(Item,
 List, Result):-
	Result = [Item|List].

cl_append(A,B,R):- append(A,B,R),!.

wl:declared(cl_error,inline(error)).
wl:init_args(0,cl_error).
cl_error(Args,Res):- cl_format([t|Args],Res),throw(cl_error(Args,Res)).

wl:declared(cl_list,inline(list)).
wl:init_args(0,cl_list).
cl_list(List,List).


cl_copy_alist([H|T],[HH|TT]):- !, assertion(nonvar(H)),
  pl_copy_1assoc(H,HH),
  cl_copy_alist(T,TT).
cl_copy_alist(T,T).
pl_copy_1assoc([H|T],[H|T]).
pl_copy_1assoc(HT,HT).

% asserting1... u
wl: init_args(exact_only, cl_acons).
cl_acons(Key_Param, Datum_Param, Alist_Param, [[Key_Param|Datum_Param]|Alist_Param]).


%rassoc item alist &key key test test-not => entry
wl:init_args(2,cl_rassoc).
cl_rassoc(Item,AList,Options,RetVal):-
 get_identity_pred(Options,kw_key,Ident),
  get_test_pred(Options,EqlPred),
  (member([K|V],AList),call(Ident,V,Id),
    (call(EqlPred,Item,Id,R)->R\==[])),!,
  RetVal = [K|V].
cl_rassoc(_,_,_,[]).

wl:init_args(2,cl_assoc).
cl_assoc(Item,AList,Options,RetVal):-
 get_identity_pred(Options,kw_key,Ident),
  get_test_pred(Options,EqlPred),
  (member([K|V],AList),call(Ident,K,Id),
    (call(EqlPred,Item,Id,R)->R\==[])),!,
  RetVal = [K|V].
cl_assoc(_,_,_,[]).

% assoc item alist
  /*
cl_assoc(Key,List,KV):- member(KV,List),KV=[Key|_],!.
cl_assoc(_Key,_List,[]).
*/

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
cl_second(List,R):- List=[_,R|_]->true;R=[].
f_ext_pf_cadr(List,R):- List=[_,R|_]->true;R=[].
cl_cadr(List,R):- List=[_,R|_]->true;R=[].




cl_reverse(Xs, Ys) :-
    lists:reverse(Xs, [], Ys, Ys).

cl_nreverse(Xs, Ys) :-
    lists:reverse(Xs, [], Ys, Ys).



% string=
(wl:init_args(2,cl_replace)).
cl_replace(X,Y,Keys,XR):-
   range_1_and_2_len(X,Y,Keys,XR,YR,Count),
   replace_each(Count,XR,YR).

replace_each(0,_XR,_YR):-!.
replace_each(_,[],_):-!.
replace_each(_,_,[]):-!.
replace_each(Count,XR,[Y|YR]):- nb_setarg(1,XR,Y),arg(2,XR,XT),Count2 is Count-1,replace_each(Count2,XT,YR).
   
range_1_and_2_len(X,Y,[],X,Y,-1):-!.
range_1_and_2_len(X,Y,Keys,XR,YR,Length):-
   key_value(Keys,start1,Start1,0),key_value(Keys,end1,End1,9999999999999),
   key_value(Keys,start2,Start2,0),key_value(Keys,end2,End2,9999999999999),
   subseqence_from(X,Start1,XR),
   subseqence_from(Y,Start2,YR),
   Length is min(End1-Start1,End2-Start2).


wl:init_args(1,cl_mapcar).
cl_mapcar(P, [[H|T]], [RH|RT]) :- !, cl_apply(P, [H], RH),cl_mapcar(P, [T], RT).
cl_mapcar(P, [[H|T],[H2|T2]], [RH|RT]) :- !, cl_apply(P, [H,H2], RH),cl_mapcar(P, [T,T2], RT).
cl_mapcar(P, [[H|T],[H2|T2],[H3|T3]], [RH|RT]) :- !, cl_apply(P, [H,H2,H3], RH),cl_mapcar(P, [T,T2,T3], RT).
cl_mapcar(_, [[]|_], []).


wl:init_args(1,cl_apply).
cl_apply(closure(Environment,ClosureResult,FormalArgs,Body), [Arguments], Result):-!,
  closure(Environment,ClosureResult,FormalArgs,Body,Arguments,Result).
cl_apply(function(FunctionName), Arguments, Result):-!,cl_apply((FunctionName), Arguments, Result).
cl_apply((FunctionName), [Arguments], Result):-!,
  lisp_compiled_eval([FunctionName|Arguments],Result).


lisp_call(Function, Result):-
	apply(Function, [Result]).


% negation can be over existence,  future possiblity or past existence, we say there exists some truth in which  


t_or_nil(G,Ret):- G->Ret=t;Ret=[].

cl_not(Obj,Ret):- t_or_nil(Obj == [] , Ret).
cl_null(Obj,Ret):- t_or_nil(Obj == [] , Ret).
cl_atom(Obj,Ret):-  t_or_nil( Obj\=[_|_] , Ret).
cl_consp(Obj,RetVal):- t_or_nil(is_consp(Obj),RetVal).

(wl:init_args(0,cl_nconc)).
cl_nconc([L1,L2],Ret):- !, append(L1,L2,Ret).
cl_nconc([L1],L1):-!.
cl_nconc([L1,L2|Lists],Ret):- !,cl_nconc([L2|Lists],LL2), append(L1,LL2,Ret).
cl_nconc(X,X):-!.

cl_copy_list(List,List):- \+ compound(List),!.
cl_copy_list([M|List],[M|Copy]):-cl_copy_list(List,Copy).


wl:type_checked(cl_length(claz_cons,integer)).
cl_length(Sequence,Len):- always(length(Sequence,Len)).


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
equal(A,B,Ret):- t_or_nil( is_equal(A,B) , Ret).


is_eql(X,Y):- is_eq(X,Y)->true;cl_type_of(X,T),cl_type_of(Y,T), notrace(catch(X=:=Y,_,fail)).
is_eq(X,Y):- same_term(X,Y).
% is_eq(X,Y):- X==Y, (\+ compound(X)-> true ; \+ \+ ((gensym(cookie,Cook),setarg(1,X,Cook),X==Y))).
is_equal(X,Y):- (X=@=Y->true;is_eql(X,Y)).
is_equalp(X,Y):- is_equal(X,Y)->true;((f_u_to_pvs(X,XX),f_u_to_pvs(Y,YY), XX=@=YY)-> true ; ( \+ X\=Y)).


cl_remove('$ARRAY'([S],Type,A),B,'$ARRAY'([Sm1],Type,C)):-pl_remove(-1,is_equal,A,B,C,Did),(number(S)->Sm1 is S-Did ; Sm1=S).
cl_remove(A,B,C):- pl_remove(-1,is_equal,A,B,C,_Did).

pl_remove(_Tst,0, X, _, X, 0).
pl_remove(_Tst,_,[], _, [],0).
pl_remove(Tst,May,[Elem|Tail], Del, Result,Done2) :-
    ( call(Tst,Elem,Del) ->  (May2 is May-1, pl_remove(Tst,May2,Tail, Del, Result,Done),Done2 is Done+1)
    ; ( Result = [Elem|Rest],pl_remove(Tst,May,Tail,Del,Rest,Done2))).

%cl_subst('$ARRAY'([S],Type,A),B,C,'$ARRAY'([Sm1],Type,R)):-pl_subst(C,B,A,R),(number(S)->Sm1 is S-Did ; Sm1=S).
cl_subst(A,B,C,R):-pl_subst(C,B,A,R).

pl_subst( Var, VarS,SUB,SUB ) :- Var==VarS,!.
pl_subst([H|T],B,A,[HH|TT]):- !,pl_subst(H,B,A,HH),pl_subst(T,B,A,TT).
pl_subst( Var, _,_,Var ).


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

./xabcl/assoc.lisp:(defun rassoc-if-not (predicate alist &key key)
./nonwamcl/rcyc/cynd/sublisp-cl.lisp:(define-caller-pattern rassoc-if-not (fn form &key pbody) :lisp)
./nonwamcl/rcyc/cycl/list-utilities.lisp:(define rassoc-if-not (predicate alist)
./nonwamcl/ccl/lib/lists.lisp:(defun rassoc-if-not (predicate alist &key key)
./nonwamcl/ccl/lib/.svn/text-base/lists.lisp.svn-base:(defun rassoc-if-not (predicate alist &key key)
./nonwamcl/sbcl/src/code/list.lisp:(defun rassoc-if-not (predicate alist &key key)
./nonwamcl/eclipse-lisp/lisp/alist.lisp:(defun RASSOC-IF-NOT (predicate a-list &key key)
./nonwamcl/com-informatimago/common-lisp/lisp/cl-definition.lisp:(declare-function RASSOC-IF-NOT ())
./nonwamcl/SICL/Code/Cons/rassoc-if-not-defun.lisp:(defun rassoc-if-not (predicate alist &key key)
./nonwamcl/SICL/Code/Cons-high/cons-high.lisp:(defun rassoc-if-not (predicate alist &key key)
./nonwamcl/t/reference/lisp_8500/init5000.lisp:  (defun rassoc-if-not (predicate alist &rest rest)
./nonwamcl/t/reference/lisp_8500/init5000lite.lisp:  (defun rassoc-if-not (predicate alist &rest rest)
./nonwamcl/t/reference/lisp_8500/init500.lisp:  (defun rassoc-if-not (predicate alist &rest rest)
./nonwamcl/t/reference/lisp_8500/core800.lisp:  (defun rassoc-if-not (predicate alist &rest rest)
./nonwamcl/slime/xref.lisp:(define-caller-pattern rassoc-if-not (fn form &key (:star form)) :lisp)
./nonwamcl/com-informatimago_common-lisp_lisp/cl-definition.lisp:(declare-function RASSOC-IF-NOT ())
./nonwamcl/dwim.hu/hu.dwim.delico/source/interpreter/common-lisp-cc.lisp:(redefun/cc rassoc-if-not (predicate alist &key key)
./nonwamcl/ecl-mirror/src/cmp/proclamations.lsp:(proclamation rassoc-if-not (function-designator association-list &key) t)
./nonwamcl/ecl-mirror/src/lsp/listlib.lsp:(defun rassoc-if-not (test alist &key key)
./xlisp500/wam-cl-init2.lisp:  (defun rassoc-if-not (predicate alist &rest rest)
./emacs-cl/cl-conses.el:(cl:defun RASSOC-IF-NOT (predicate alist &KEY KEY)
./reference/pjb-cl-definition.lisp:(declare-function RASSOC-IF-NOT ())
./reference/xref-patterns.lisp:(data-assrt :define-caller-pattern rassoc-if-not (fn form &key (:star form)) :lisp)


symbol_info(Sym,P,function,O),symbol_info(Sym,P,function_type,FT),symbol_info(Sym,P,name,Name),
  format('~N% ~w (~w ~w)~n~q(A,Result):- ...\n\n',[Name,FT,P, O]),nl,fail.

*/  
:- fixup_exports.

end_of_file.

Warning: cl_minusp/2, which is referenced by
Warning:        1-st clause of f_u_yfor/4: 1-st clause of f_u_yfor/4



Warning: cl_acons/4, which is referenced by
Warning:        1-st clause of f_u_record_in_loop_alist/3: 1-st clause of f_u_record_in_loop_alist/3
Warning: cl_char/3, which is referenced by
Warning:        1-st clause of f_u_string_head/2: 1-st clause of f_u_string_head/2
Warning: cl_fifth/2, which is referenced by
Warning:        1-st clause of f_u_yfor/4: 1-st clause of f_u_yfor/4
Warning: cl_fourth/2, which is referenced by
Warning:        1-st clause of f_u_decr/5: 1-st clause of f_u_decr/5
Warning:        1-st clause of f_u_incr/5: 1-st clause of f_u_incr/5
Warning: cl_nsublis/3, which is referenced by
Warning:        1-st clause of f_u_substitute_loop_return/3: 1-st clause of f_u_substitute_loop_return/3
Warning: cl_numberp/2, which is referenced by
Warning:        1-st clause of f_u_decr/5: 1-st clause of f_u_decr/5
Warning:        1-st clause of f_u_incr/5: 1-st clause of f_u_incr/5
Warning: cl_rassoc/3, which is referenced by
Warning:        1-st clause of f_u_add_element_to_end_of_loop_alist/3: 1-st clause of f_u_add_element_to_end_of_loop_alist/3
Warning:        1-st clause of f_u_add_element_to_loop_alist/3: 1-st clause of f_u_add_element_to_loop_alist/3
Warning:        1-st clause of f_u_fetch_new_iteration_variable/1: 1-st clause of f_u_fetch_new_iteration_variable/1
Warning:        1-st clause of f_u_fetch_old_iteration_variable/1: 1-st clause of f_u_fetch_old_iteration_variable/1
Warning:        1-st clause of f_u_iteration_variable_exists_p/1: 1-st clause of f_u_iteration_variable_exists_p/1
Warning:        1-st clause of f_u_substitute_iteration_variable/2: 1-st clause of f_u_substitute_iteration_variable/2
Warning:        1-st clause of f_u_yloop/2: 1-st clause of f_u_yloop/2
Warning: cl_remove/3, which is referenced by
Warning:        1-st clause of f_u_substitute_iteration_variable/2: 1-st clause of f_u_substitute_iteration_variable/2
Warning: cl_subst/4, which is referenced by
Warning:        1-st clause of f_u_define_and_rename_loop_locals/5: 1-st clause of f_u_define_and_rename_loop_locals/5
Warning:        1-st clause of f_u_substitute_iteration_variable/2: 1-st clause of f_u_substitute_iteration_variable/2
Warning: cl_third/2, which is referenced by
Warning:        1-st clause of f_u_yfor/4: 1-st clause of f_u_yfor/4
Warning: f_u_add_elements_to_clause/4, which is referenced by
Warning:        1-st clause of f_u_yfor/4: 1-st clause of f_u_yfor/4
Warning: f_u_result/2, which is referenced by
Warning:        1-st clause of f_u_maximize/2: 1-st clause of f_u_maximize/2
Warning: f_u_ydo/3, which is referenced by
Warning:        1-st clause of f_u_walkcdr/3: 1-st clause of f_u_walkcdr/3
Warning: f_u_yloop/4, which is referenced by
Warning:        1-st clause of f_u_walkcdr/3: 1-st clause of f_u_walkcdr/3

