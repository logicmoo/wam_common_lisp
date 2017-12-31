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
wl:init_args(0,list).
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
wl:init_args(x,rplaca).
cl_rplaca(Cons,Obj,Cons):- nb_setarg(1,Cons,Obj).
f_sys_set_car(A,B,C):-cl_rplaca(A,B,C).

wl:op_replacement(setcdr,rplacd).
wl:interned_eval(("`cl:rplacd")).
wl:init_args(x,rplacd).
cl_rplacd(Cons,Obj,Cons):- nb_setarg(2,Cons,Obj).
f_sys_set_cdr(A,B,C):-cl_rplacd(A,B,C).


cl_copy_alist([H|T],[HH|TT]):- !, assertion(nonvar(H)),
  pl_copy_1assoc(H,HH),
  cl_copy_alist(T,TT).
cl_copy_alist(T,T).
pl_copy_1assoc([H|T],[H|T]).
pl_copy_1assoc(HT,HT).

% #'ACONS
wl: init_args(x, cl_acons).
cl_acons(Key_Param, Datum_Param, Alist_Param, [[Key_Param|Datum_Param]|Alist_Param]).

wl:interned_eval((
"(defconstant +upgraded-array-element-types+ 
   '(NIL BASE-CHAR CHARACTER BIT EXT:BYTE8 EXT:INTEGER8 EXT:BYTE16 EXT:INTEGER16
 EXT:BYTE32 EXT:INTEGER32 EXT:BYTE64 EXT:INTEGER64 SINGLE-FLOAT DOUBLE-FLOAT T))")).

wl:interned_eval(("`SYS:MAKE-VECTOR")).


% #'rassoc item alist &key key test test-not => entry
wl:init_args(2,rassoc).
cl_rassoc(Item,AList,Options,RetVal):-
 get_identity_pred(Options,kw_key,Ident),
  get_test_pred(cl_eql,Options,EqlPred),
  (member([K|V],AList),call(Ident,V,Id),
    (call(EqlPred,Item,Id,R)->R\==[])),!,
  RetVal = [K|V].
cl_rassoc(_,_,_,[]).

wl:init_args(2,assoc).
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


(wl:init_args(1,last)).
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
cl_cddr(X, Ref) :-
  cl_cdr(X, Cdr_Param),
  cl_cdr(Cdr_Param, Ref).

cl_cdar(X, Ref) :-
  cl_car(X, Cdr_Param),
  cl_cdr(Cdr_Param, Ref).

cl_caar(X, Ref) :-
  cl_car(X, Cdr_Param),
  cl_car(Cdr_Param, Ref).

cl_caaar(X, Ref) :-
  cl_car(X, Y),
  cl_car(Y, Z),
  cl_car(Z, Ref).

cl_cdaar(X, Ref) :-
  cl_car(X, Y),
  cl_car(Y, Z),
  cl_cdr(Z, Ref).

cl_cadar(X, Ref) :-
  cl_car(X, Y),
  cl_cdr(Y, Z),
  cl_car(Z, Ref).

cl_cdadr(X, Ref) :-
  cl_cdr(X, Y),
  cl_car(Y, Z),
  cl_cdr(Z, Ref).

cl_caadr(X, Ref) :-
  cl_cdr(X, Y),
  cl_car(Y, Z),
  cl_car(Z, Ref).

cl_caddr(X, Ref) :-
  cl_cdr(X, Y),
  cl_cdr(Y, Z),
  cl_car(Z, Ref).

cl_cdddr(X, Ref) :-
  cl_cdr(X, Y),
  cl_cdr(Y, Z),
  cl_cdr(Z, Ref).

cl_cddar(X, Ref) :-
  cl_car(X, Y),
  cl_cdr(Y, Z),
  cl_cdr(Z, Ref).

cl_cddddr(W, Ref) :-
  cl_cdr(W, X),
  cl_cdr(X, Y),
  cl_cdr(Y, Z),
  cl_cdr(Z, Ref).

cl_caaadr(W, Ref) :-
  cl_cdr(W, X),
  cl_car(X, Y),
  cl_car(Y, Z),
  cl_car(Z, Ref).

:- fixup_exports.

      
end_of_file.


/*

Warning: cl_array_element_type/2, which is referenced by
Warning:        1-st clause of f_sys_coerce_to_vector/5: 1-st clause of f_sys_coerce_to_vector/5
Warning:        1-st clause of f_sys_seq_result1/4: 1-st clause of f_sys_seq_result1/4
Warning: cl_array_has_fill_pointer_p/2, which is referenced by
Warning:        1-st clause of cl_map_into/4: 1-st clause of cl_map_into/4
Warning: cl_caaadr/2, which is referenced by
Warning:        1-st clause of f_sys_seq_ref1/3: 1-st clause of f_sys_seq_ref1/3
Warning: cl_char_code/2, which is referenced by
Warning:        1-st clause of cl_alpha_char_p/2: 1-st clause of cl_alpha_char_p/2
Warning:        1-st clause of cl_alphanumericp/2: 1-st clause of cl_alphanumericp/2
Warning:        1-st clause of cl_char_downcase/2: 1-st clause of cl_char_downcase/2
Warning:        1-st clause of cl_char_int/2: 1-st clause of cl_char_int/2
Warning:        1-st clause of cl_char_name/2: 1-st clause of cl_char_name/2
Warning:        1-st clause of cl_char_upcase/2: 1-st clause of cl_char_upcase/2
Warning:        1-st clause of cl_digit_char_p/3: 1-st clause of cl_digit_char_p/3
Warning:        1-st clause of cl_lower_case_p/2: 1-st clause of cl_lower_case_p/2
Warning:        1-st clause of cl_name_char/2: 1-st clause of cl_name_char/2
Warning:        1-st clause of cl_standard_char_p/2: 1-st clause of cl_standard_char_p/2
Warning:        1-st clause of cl_upper_case_p/2: 1-st clause of cl_upper_case_p/2
Warning: cl_code_char/2, which is referenced by
Warning:        1-st clause of cl_char_downcase/2: 1-st clause of cl_char_downcase/2
Warning:        1-st clause of cl_char_upcase/2: 1-st clause of cl_char_upcase/2
Warning:        1-st clause of cl_digit_char/3: 1-st clause of cl_digit_char/3
Warning:        1-st clause of cl_name_char/2: 1-st clause of cl_name_char/2
Warning:        1-st clause of f_sys_integer_string/3: 1-st clause of f_sys_integer_string/3
Warning: cl_fmakunbound/2, which is referenced by
Warning:        1-st clause of f_sys_ansi_loop/2: 1-st clause of f_sys_ansi_loop/2
Warning: cl_function/2, which is referenced by
Warning:        1-st clause of cl_map/5: 1-st clause of cl_map/5
Warning: cl_invoke_debugger/2, which is referenced by
Warning:        1-st clause of cl_signal/3: 1-st clause of cl_signal/3
Warning: cl_list_xx/3, which is referenced by
Warning:        1-st clause of cl_map/5: 1-st clause of cl_map/5
Warning: cl_make_condition/3, which is referenced by
Warning:        1-st clause of f_sys_designator_condition/4: 1-st clause of f_sys_designator_condition/4
Warning: cl_make_condition/6, which is referenced by
Warning:        1-st clause of f_sys_designator_condition/4: 1-st clause of f_sys_designator_condition/4
Warning: cl_make_list/4, which is referenced by
Warning:        1-st clause of cl_make_sequence/4: 1-st clause of cl_make_sequence/4
Warning: cl_merge_pathnames/2, which is referenced by
Warning:        1-st clause of f_sys_dd/1: 1-st clause of f_sys_dd/1
Warning: cl_nsubst/8, which is referenced by
Warning:        1-st clause of cl_nsubst_if/5: 1-st clause of cl_nsubst_if/5
Warning:        1-st clause of cl_nsubst_if_not/5: 1-st clause of cl_nsubst_if_not/5
Warning: cl_parse_integer/4, which is referenced by
Warning:        1-st clause of cl_name_char/2: 1-st clause of cl_name_char/2
Warning: cl_predicate/2, which is referenced by
Warning:        1-st clause of cl_every/4: 1-st clause of cl_every/4
Warning:        1-st clause of cl_some/4: 1-st clause of cl_some/4
Warning: cl_reduce/7, which is referenced by
Warning:        1-st clause of cl_map/5: 1-st clause of cl_map/5
Warning: cl_remf/3, which is referenced by
Warning:        1-st clause of cl_remprop/3: 1-st clause of cl_remprop/3
Warning: cl_restart_case/3, which is referenced by
Warning:        1-st clause of cl_warn/3: 1-st clause of cl_warn/3
Warning: cl_restart_name/2, which is referenced by
Warning:        1-st clause of cl_find_restart/3: 1-st clause of cl_find_restart/3
Warning:        1-st clause of f_sys_designator_restart/2: 1-st clause of f_sys_designator_restart/2
Warning: cl_subst/8, which is referenced by
Warning:        1-st clause of cl_subst_if/5: 1-st clause of cl_subst_if/5
Warning:        1-st clause of cl_subst_if_not/5: 1-st clause of cl_subst_if_not/5
Warning: cl_subtypep/3, which is referenced by
Warning:        1-st clause of cl_make_sequence/4: 1-st clause of cl_make_sequence/4
Warning: cl_type_error/3, which is referenced by
Warning:        1-st clause of f_sys_pf_set_subseq/4: 1-st clause of f_sys_pf_set_subseq/4
Warning: cl_values/2, which is referenced by
Warning:        1-st clause of cl_values_list/2: 1-st clause of cl_values_list/2
Warning: cl_with_simple_restart/3, which is referenced by
Warning:        1-st clause of cl_break/2: 1-st clause of cl_break/2
Warning: f_ext_neq/3, which is referenced by
Warning:        1-st clause of f_sys_compiler_macroexpand_1/3: 1-st clause of f_sys_compiler_macroexpand_1/3
Warning: f_sys_all_car/2, which is referenced by
Warning:        1-st clause of f_sys_all_car1/2: 1-st clause of f_sys_all_car1/2
Warning: f_sys_all_cdr/2, which is referenced by
Warning:        1-st clause of f_sys_all_cdr1/2: 1-st clause of f_sys_all_cdr1/2
Warning: f_sys_c43/2, which is referenced by
Warning:        1-st clause of cl_concatenate/3: 1-st clause of cl_concatenate/3
Warning: f_sys_c60/4, which is referenced by
Warning:        1-st clause of cl_alpha_char_p/2: 1-st clause of cl_alpha_char_p/2
Warning:        1-st clause of cl_alphanumericp/2: 1-st clause of cl_alphanumericp/2
Warning:        1-st clause of cl_char_downcase/2: 1-st clause of cl_char_downcase/2
Warning:        1-st clause of cl_char_upcase/2: 1-st clause of cl_char_upcase/2
Warning:        1-st clause of cl_digit_char_p/3: 1-st clause of cl_digit_char_p/3
Warning:        1-st clause of cl_lower_case_p/2: 1-st clause of cl_lower_case_p/2
Warning:        1-st clause of cl_standard_char_p/2: 1-st clause of cl_standard_char_p/2
Warning:        1-st clause of cl_upper_case_p/2: 1-st clause of cl_upper_case_p/2
Warning: f_sys_c60_c61/2, which is referenced by
Warning:        1-st clause of cl_char_c60_c61/2: 1-st clause of cl_char_c60_c61/2
Warning: f_sys_closest_sequence_type/2, which is referenced by
Warning:        1-st clause of cl_make_sequence/4: 1-st clause of cl_make_sequence/4
Warning: f_sys_conc_string/3, which is referenced by
Warning:        1-st clause of cl_char_name/2: 1-st clause of cl_char_name/2
Warning: f_sys_do_sequences/3, which is referenced by
Warning:        1-st clause of cl_every/4: 1-st clause of cl_every/4
Warning:        1-st clause of cl_map/5: 1-st clause of cl_map/5
Warning:        1-st clause of cl_some/4: 1-st clause of cl_some/4
Warning: f_sys_elt_list/4, which is referenced by
Warning:        1-st clause of cl_every/4: 1-st clause of cl_every/4
Warning:        1-st clause of cl_map/5: 1-st clause of cl_map/5
Warning:        1-st clause of cl_some/4: 1-st clause of cl_some/4
Warning: f_sys_fdefinition_block_name/2, which is referenced by
Warning:        1-st clause of cl_define_compiler_type_macro/4: 1-st clause of cl_define_compiler_type_macro/4
Warning: f_sys_fill_array_with_elt/5, which is referenced by
Warning:        1-st clause of cl_make_sequence/4: 1-st clause of cl_make_sequence/4
Warning: f_sys_fixnump/2, which is referenced by
Warning:        1-st clause of f_sys_make_seq_iterator/3: 1-st clause of f_sys_make_seq_iterator/3
Warning:        1-st clause of f_sys_seq_iterator_next/3: 1-st clause of f_sys_seq_iterator_next/3
Warning:        1-st clause of f_sys_seq_iterator_ref/3: 1-st clause of f_sys_seq_iterator_ref/3
Warning:        1-st clause of f_sys_seq_iterator_set/4: 1-st clause of f_sys_seq_iterator_set/4
Warning: f_sys_gethash1/3, which is referenced by
Warning:        1-st clause of cl_compiler_macro_function/3: 1-st clause of cl_compiler_macro_function/3
Warning: f_sys_imakunbound/3, which is referenced by
Warning:        1-st clause of cl_makunbound/2: 1-st clause of cl_makunbound/2
Warning: f_sys_make_vector/7, which is referenced by
Warning:        1-st clause of cl_make_sequence/4: 1-st clause of cl_make_sequence/4
Warning:        1-st clause of f_sys_coerce_to_vector/5: 1-st clause of f_sys_coerce_to_vector/5
Warning: f_sys_parse_deftype_macro/10, which is referenced by
Warning:        1-st clause of cl_define_compiler_type_macro/4: 1-st clause of cl_define_compiler_type_macro/4
Warning: f_sys_pf_set_fill_pointer/3, which is referenced by
Warning:        1-st clause of cl_map_into/4: 1-st clause of cl_map_into/4
Warning: f_sys_reckless/2, which is referenced by
Warning:        1-st clause of cl_every/4: 1-st clause of cl_every/4
Warning:        1-st clause of cl_some/4: 1-st clause of cl_some/4
Warning: f_sys_recur1/2, which is referenced by
Warning:        1-st clause of cl_case/3: 1-st clause of cl_case/3
Warning: f_sys_recur11/3, which is referenced by
Warning:        1-st clause of f_sys_integer_string/3: 1-st clause of f_sys_integer_string/3
Warning: f_sys_restart_interactive_function/2, which is referenced by
Warning:        1-st clause of cl_invoke_restart_interactively/2: 1-st clause of cl_invoke_restart_interactively/2
Warning: f_sys_restartp/2, which is referenced by
Warning:        1-st clause of f_sys_designator_restart/2: 1-st clause of f_sys_designator_restart/2
Warning: f_sys_signal_type_error/3, which is referenced by
Warning:        1-st clause of f_sys_error_not_a_sequence/2: 1-st clause of f_sys_error_not_a_sequence/2
Warning: f_sys_simple_atrue.

*/
