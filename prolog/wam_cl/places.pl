/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (symbol_places.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(places, []).
:- set_module(class(library)).
:- include('header').


% get_setf_expander_get_set(_Ctx,_Env,[car,Var],[car,Var],[set_car,Var],  true):- atom(Var),!.
get_setf_expander_get_set(Ctx,Env,[OP,LVar|EXTRA],[OP,GET|EXTRA],[INVERSE,GET|EXTRA], (Code1, Body)):- 
 compile_apply(Ctx,Env,OP,[LVar|EXTRA],Result,ExpandedFunction),
 ExpandedFunction=..[_FN|ARGS],append([VAR|PARAMS],[Result],ARGS),
 show_call_trace((compile_each(Ctx,Env,PARAMS,CPARAMS,Code1),append([VAR|CPARAMS],[_VAL,Result],_SETFARGS), setf_inverse_op(OP,INVERSE))),
 must_compile_body(Ctx,Env,GET,LVar, Body), (var(GET)->put_attr(GET,preserved_var,t); true).

%get_setf_expander_get_set(Ctx,Env,LVar,GET,[sys_set_symbol_value,GET], true):- atom(LVar),lookup_symbol_macro(Ctx,Env,LVar,GET),!.
%get_setf_expander_get_set(_,_,LVar,GET,[set,GET], true):- \+ atom(LVar),atom(LVar),LVar=GET.

f_clos_pf_set_slot_value(Obj,Key,Value,Value):- set_opv(Obj,Key,Value).

lookup_symbol_macro(Ctx,Env,LVar,GET):- get_ctx_env_attribute(Ctx,Env,symbol_macro(LVar),GET).

wl:init_args(1,cl_array_row_major_index).
wl:init_args(exact_only,cl_row_major_aref).

wl:setf_inverse(slot_value,clos_pf_set_slot_value).
wl:setf_inverse(car,rplaca).
wl:setf_inverse(cdr,rplacd).

setf_inverse_op(Sym,Inverse):- wl:setf_inverse(Sym,Inverse).
setf_inverse_op(G,S):- cl_get(G,sys_setf_inverse,[],S),ground(S).
setf_inverse_op(Sym,Inverse):- 
   symbol_prefix_and_atom(Sym,FunPkg,Name),
   member(SETPRefix,['setf','set','pf_set']),
   atomic_list_concat([FunPkg,SETPRefix,Name],'_',Inverse),
   find_lisp_function(Inverse,_Arity,_Fn).
setf_inverse_op(Sym,Combined):- 
   combine_setfs([setf,Sym],Combined).
   


make_place_op(Ctx,Env,Result,incf,GET,LV,SET,Body) :- 
 always((
   value_or(LV,Value,1),!,
   must_compile_body(Ctx,Env,ValueR,Value,Part1),
   must_compile_body(Ctx,Env,Old,GET,Part2),
   Part3 = (New is Old+ ValueR),
   append(SET,[New],LispOp),
   must_compile_body(Ctx,Env,Result,LispOp,Part4),
   Body = (Part1,Part2,Part3,Part4))).



is_setf_op([setf|Accessor],Accessor):- nonvar(Accessor).


not_place_op(setq).
not_place_op(psetq).

is_parallel_op(psetf).
is_parallel_op(psetq).

is_pair_op(setq).
is_pair_op(psetq).
is_pair_op(setf).
is_pair_op(psetf).

is_only_read_op(getf).

is_place_write(P):- is_place_op(P), \+ is_only_read_op(P).

is_place_op(setf).
is_place_op(psetf).
is_place_op(getf).
is_place_op(incf).
is_place_op(decf).
/*
is_place_op(rotatef).
is_place_op(shiftf).


(defmacro rotatef (&rest args)
  `(psetf ,@(mapcan #'list
                    args
                    (append (cdr args) 
                            (list (car args))))))


not place ops but now Macros
is_place_op(push).
is_place_op(pushnew).
is_place_op(pop).
*/



pairify([],[],[]).
pairify([Var, ValueForm | Rest],[Var | Atoms],[ValueForm | Forms]):-
   pairify(Rest,Atoms,Forms).

wl:init_args(2,X):- at_least_two_args(X).


combine_setfs(Name0,Name):-atom(Name0),!,Name0=Name.
combine_setfs([setf,Name],Combined):- cl_symbol_package(Name,Pkg),pl_symbol_name(Name,Str),string_concat("SETF-",Str,SETF_STR),
  string_upper(SETF_STR,UPPER_SETF_STR),
  cl_intern(UPPER_SETF_STR,Pkg,Combined).
%combine_setfs([setf,Name],Combined):- atomic_list_concat([setf,Name],'_',Combined).


compile_setfs(_Ctx,_Env,Symbol,[Function,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- notrace(at_least_two_args(Function)),\+ is_fboundp(Function),!,P=..[Function,Symbol,A2,AMORE].

compile_setfs(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- notrace((at_least_two_args(Function),same_symbol(Function,Fun0))),\+ is_fboundp(Function),!,P=..[Function,Symbol,A2,AMORE].
:- discontiguous compile_accessors/5.

compile_accessors(Ctx,Env,Result,[SetQ, Var, ValueForm, Atom2| Rest], Body):- is_parallel_op(SetQ),!, 
   pairify([Var, ValueForm, Atom2| Rest],Atoms,Forms),
   maplist(expand_ctx_env_forms(Ctx,Env),Forms,BodyS1,Results),
   maplist(set_with_prolog_var(Ctx,Env,SetQ),Atoms,Results,BodyS2),   
   ((op_return_type(SetQ,RT),RT=name) ->  last(Atoms,Result) ; last(Results,Result)),
   append(BodyS1,BodyS2,BodyS),list_to_conjuncts(BodyS,Body).


compile_accessors(Ctx,Env,Result,[SetQ, Var, ValueForm, Atom2| Rest], Body):- is_pair_op(SetQ), 
   must_compile_body(Ctx,Env,_ResultU,[SetQ, Var, ValueForm], Body1),
   must_compile_body(Ctx,Env,Result,[SetQ, Atom2| Rest],  Body2),
   Body = (Body1 , Body2).

compile_accessors(Ctx,Env,Result,[Defvar, Var], Body):- is_def_nil(Defvar),!,
  compile_accessors(Ctx,Env,Result,[Defvar, Var , nil],Body).

/* TODO CONFIRM WE ARE SETTING SYMBOLS honoring SYMBOL-MACROs  */
compile_accessors(Ctx,Env,Result,[setf, LVar, ValuesForms], Code):- atom(LVar),
     lookup_symbol_macro(Ctx,Env,LVar,SET), % rw_add(Ctx,LVar,r),    
     must_compile_body(Ctx,Env,Result,[setf, SET, ValuesForms],Code).

compile_accessors(Ctx,Env,Result,[setf, Place, ValuesForms], (Part1, set_var(Env,Place,Result))):- atom(Place),
     assertion(is_symbolp(Place)),
     rw_add(Ctx,Place,w),
     must_compile_body(Ctx,Env,Result,ValuesForms,Part1).


compile_accessors(Ctx,Env,Result,[Getf|ValuePlace], Body):- fail, is_place_op_verbatum(Getf),     
        debug_var([Getf,'_R'],Result),
        debug_var([Getf,'_Env'],Env),
        place_extract(ValuePlace,Value,Place),
        extract_var_atom(Place,RVar),
        (is_only_read_op(Getf)->rw_add(Ctx,RVar,r);rw_add(Ctx,RVar,w)),
        Body = (set_place(Env,Getf, Place, Value, Result)).


portray(List):- notrace((nonvar(List),List=[_,_],sub_term(E,List),ground(E),E = ((environ=W)),write(environment(W)))).


compile_accessors(Ctx,Env,Result,[Setf, Place|ValuesForms], (Part0,Body)):- is_place_write(Setf),
     get_setf_expander_get_set(Ctx,Env,Place,GET,SET,Part0),
     make_place_op(Ctx,Env,Result,Setf,GET,ValuesForms,SET,Body),!.

compile_accessors(Ctx,Env,Result,[setf, Place, ValuesForms], (Part0,Part1,Part4)):- \+ atom(Place),
     get_setf_expander_get_set(Ctx,Env,Place,_,SET,Part0),     
     must_compile_body(Ctx,Env,New,ValuesForms,Part1),
     append(SET,[New],LispOp),
     must_compile_body(Ctx,Env,Result,LispOp,Part4).

compile_accessors(Ctx,Env,Result,[getf, Place], (Part0,Part4)):- 
     get_setf_expander_get_set(Ctx,Env,Place,GET,_SET,Part0),     
     must_compile_body(Ctx,Env,Result,GET,Part4).



extract_var_atom([_,RVar|_],RVar):-atomic(RVar).
extract_var_atom(Var,Var).
                  

% %  (LET ((a 0)(v (VECTOR 0 1 2 3 4 5))) (INCF (AREF (INCF a))) v)

% %  (LET ((a 0)(v (VECTOR 0 1 2 3 4 5))) (INCF (AREF (INCF a))) v)

compile_accessors(Ctx,Env,Result,[Getf, Var| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts([true|ValuesBody],BodyS),!,
        debug_var([Getf,'_R'],Result),
        debug_var([Getf,'_Env'],Env),
        extract_var_atom(Var,RVar),
        compile_place(Ctx,Env,UsedVar,Var,Code),
        (Var\==RVar -> rw_add(Ctx,RVar,r) ; (is_only_read_op(Getf)->rw_add(Ctx,RVar,r);rw_add(Ctx,RVar,w))),
        Body = (BodyS,Code,set_place(Env,Getf, UsedVar, ResultVs,Result)).

% compile_place(Ctx,Env,Result,Var,Code).
compile_place(_Ctx,_Env,[value,Var],Var,true):- \+ is_list(Var),!.
%compile_place(_Ctx,_Env,[Place,Var],[Place,Var],true):- atom(Var),!.
compile_place(Ctx,Env,[Place|VarResult],[Place|VarEval],Code):- compile_each(Ctx,Env,VarResult,VarEval,Code).
%compile_place(Ctx,Env,[Place,Var,Result],[Place,Var|Eval],Code):- must_compile_progn(Ctx,Env,Result,Eval,Code).
%compile_place(_Ctx,_Env,Var,Var,true).


at_least_two_args(define_compiler_macro).
at_least_two_args(defsetf).
at_least_two_args(deftype).
at_least_two_args(symbol_macrolet).
at_least_two_args(define_setf_expander).



wl:interned_eval_e(
"(defmacro pushnew (obj place)
  (let ((sym (gensym)))
    `(let ((,sym ,obj))
       (unless (member ,sym ,place)
         (push ,sym ,place)))))").


%(wl:init_args(2,cl_pushnew)).
%cl_pushnew(Element, Place, FnResult) :-

wl:interned_eval_e(
'(defmacro my-push (element place)
   (let ((el-sym  (gensym))
         (new-sym (gensym "NEW")))
     `(let* ((,el-sym  ,element)
             (,new-sym (cons ,el-sym ,place)))
        (setf ,place ,new-sym)))))').

cl_push(Element, Place, FnResult) :-
        global_env(ReplEnv),
        Env=[bv(u_element, Element), bv(u_place, Place)|ReplEnv],
        cl_gensym(El_sym_Init),
        cl_gensym('$ARRAY'([*], claz_base_character, "NEW"), New_sym_Init),
        LEnv=[bv(u_el_sym, El_sym_Init), bv(u_new_sym, New_sym_Init)|Env],
        get_var(LEnv, u_el_sym, El_sym_Get12),
        get_var(LEnv, u_element, Element_Get),
        get_var(LEnv, u_new_sym, New_sym_Get15),
        get_var(LEnv, u_place, Place_Get14),
        [let_xx, [[El_sym_Get12, Element_Get], [New_sym_Get15, [cons, El_sym_Get12, Place_Get14]]], [setf, Place_Get14, New_sym_Get15]]=MFResult,
        cl_eval(MFResult, FnResult).


/*
(defun setf-function-name-p (name)
  (and (consp name)
             (consp (%cdr name))
             (null (%cddr name))
             (symbolp (%cadr name))
             (eq (car name) 'setf)))
*/
% asserting... u
wl:arglist_info(f_sys_setf_function_name_p, [sys_name], [_Name_Param], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name], opt:0, req:[sys_name], rest:0, whole:0}).
% asserting... u
wl:init_args(exact_only, f_sys_setf_function_name_p).
% asserting... u
wl:lambda_def(defun, sys_setf_function_name_p, f_sys_setf_function_name_p, [sys_name], [[and, [consp, sys_name], [consp, [ext_pf_cdr, sys_name]], [null, [u_pf_cddr, sys_name]], [symbolp, [ext_pf_cadr, sys_name]], [eq, [car, sys_name], [quote, setf]]]]).
f_sys_setf_function_name_p(Name_Param, TrueResult66) :-
        (   is_consp(Name_Param)
        ->  f_ext_pf_cdr(Name_Param, PredArgResult35),
            (   is_consp(PredArgResult35)
            ->  f_u_pf_cddr(Name_Param, IFTEST40),
                (   IFTEST40==[]
                ->  f_ext_pf_cadr(Name_Param, PredArgResult53),
                    (   is_symbolp(PredArgResult53)
                    ->  cl_car(Name_Param, Is_eq_Param),
                        t_or_nil(is_eq(Is_eq_Param, setf), TrueResult),
                        TrueResult66=TrueResult
                    ;   TrueResult66=[]
                    )
                ;   TrueResult66=[]
                )
            ;   TrueResult66=[]
            )
        ;   TrueResult66=[]
        ).

value_or([Value],Value,_):- !.
value_or([],Value,Value):- !.
value_or(Value,Value,_).

wl:init_args(1,cl_get_setf_expansion).

%place_op(Env,PlOP,[Place,Obj],[],Result):- place_op(Env,PlOP,Obj,[Place],Result).

%place_op(Env,PlOP,Obj,Value,Result):- var(Env),ensure_env(Env), \+ var(Env),!, place_op(Env,PlOP,Obj,Value,Result).

to_place([value,Obj],Obj,value):-!.
to_place([symbol_value,Obj],Obj,value):-!.
to_place([slot_value,Obj,Place],Obj,Place):-!.
to_place([aref,Obj|Index],Obj,[aref|Index]):-!.
to_place([Place,Obj],Obj,Place):-!.
to_place([Place,Obj|Args],Obj,[Place|Args]):-!.
%to_place([Obj],Obj,value):-!.
to_place(Obj,Obj,value).

get_place(Env, Oper, Obj, Value,  Result):-
  always(to_place(Obj,RObj,Place)),!,
    always(place_op(Env, Oper, RObj, Place, Value,  Result)).

set_place(Env, Oper, Obj, Value,  Result):-
  always(to_place(Obj,RObj,Place)),!,
    always(place_op(Env, Oper, RObj, Place, Value,  Result)).

plistify(L,L):-L==[],!.
plistify([H|T],[H|T]):-!.
plistify(H,[H]).

place_op(Env,getf,Obj,Place,[Value],Value):-!,get_place_value(Env, Obj, Place, Value).
place_op(Env,setf,Obj,Place, [Value], Value):-!,set_place_value(Env, Obj, Place, Value).

place_op(Env,incf, Obj, Place, LV,  Result):- value_or(LV,Value,1),!,
   get_place_value(Env, Obj, Place, Old),
   Result is Old+ Value,
   set_place_value(Env, Obj, Place, Result).

place_op(Env,decf, Obj, Place, LV,  Result):- value_or(LV,Value,1),!,
   get_place_value(Env, Obj, Place, Old),
   Result is Old- Value,
   set_place_value(Env, Obj, Place, Result).

place_op(Env,pop, Obj, Place, [],  Result):- 
   get_place_value(Env, Obj, Place, Old),
   plistify(Old,OldL),
   (OldL = [Result|New]-> true ; (Old=[],New=[],Result=[])),
   set_place_value(Env, Obj, Place, New).

place_op(Env,pushnew, Obj, Place, LV,  Result):- value_or(LV,Value,[]),!,
   get_place_value(Env, Obj, Place, Old),
   plistify(Old,OldL),
   Result = [Value|OldL],
   set_place_value(Env, Obj, Place, Result).

place_extract([Value,Place],[Value],Place).
place_extract([Place],[],Place).
place_extract([Value|Place],Value,Place).

get_place_value(_,[H|_],car,H).
get_place_value(_,[_|T],cdr,T).
get_place_value(Env, Obj, value, Value):- atom(Obj),!,get_symbol_value(Env,Obj,Value).
get_place_value(_Env, Obj, Place, Value):- get_opv(Obj, Place, Value).

set_place_value(_,Cons,car,H):- is_consp(Cons),!, cl_rplaca(Cons,H,_).
set_place_value(_,Cons,cdr,T):- is_consp(Cons),!, cl_rplacd(Cons,T,_).
set_place_value(Env, Obj, value, Value):- atom(Obj),!,set_var(Env,Obj,Value).
set_place_value(_Env, Obj, Place, Value):- set_opv(Obj, Place, Value).


%with_place_value(Env,OPR,Obj,Place, Value):-!, type_or_class_nameof(Obj,Type),with_place_value(Env,OPR,Obj,Type,Place,Value).
/*
with_place_value(Env,OPR,Obj,Type,Place,Value):- 
  always(atomic_list_concat(List,'_',Place)),
  with_place_value6(Env,OPR,Place,List,Type,Obj,Value).

with_place_value6(_Env,OPR,_Place,[Type,Prop],Type,Obj, Value):- call_opv(OPR,Obj,Prop,Value),!.
with_place_value6(_Env,OPR, Place,_List,      _Type,Obj, Value):- call_opv(OPR,Obj,Place,Value),!.

call_opv(OPR,[slot_value,Obj,Place],value,Value):- !, call(OPR,Obj,Place,Value).
call_opv(OPR,[Place,Obj],value,Value):- !, call(OPR,Obj,Place,Value).
call_opv(OPR,Obj,Place,Value):- !, call(OPR,Obj,Place,Value).
*/
/*

The effect of

 (defsetf symbol-value set)
is built into the Common Lisp system. This causes the form (setf (symbol-value foo) fu) to expand into (set foo fu).

Note that

 (defsetf car rplaca)
would be incorrect because rplaca does not return its last argument.

*/
:- fixup_exports.

end_of_file.



 (get-setf-expansion '(symbol-value 't))
(#:TEMP-5499) ;
('T) ;
(#:NEW-5498) ;
(SYSTEM::SET-SYMBOL-VALUE #:TEMP-5499 #:NEW-5498) ;
(SYMBOL-VALUE #:TEMP-5499)


(get-setf-expansion (symbol-value 't))
NIL ;
NIL ;
(#:NEW-3230) ;
(SETQ T #:NEW-3230) ;

Examples:

 (defun lastguy (x) (car (last x))
  =>  LASTGUY
 (define-setf-expander lastguy (x &environment env)
   "Set the last element in a list to the given value."
   (multiple-value-bind (dummies vals newval setter getter)
       (get-setf-expansion x env)
     (let ((store (gensym)))
       (values dummies
               vals
               `(,store)
               `(progn (rplaca (last ,getter) ,store) ,store)
               `(lastguy ,getter))))) =>  LASTGUY
 (setq a (list 'a 'b 'c 'd)
       b (list 'x)
       c (list 1 2 3 (list 4 5 6))) =>  (1 2 3 (4 5 6))
 (setf (lastguy a) 3) =>  3
 (setf (lastguy b) 7) =>  7
 (setf (lastguy (lastguy c)) 'lastguy-symbol) =>  LASTGUY-SYMBOL
 a =>  (A B C 3)
 b =>  (7)
 c =>  (1 2 3 (4 5 LASTGUY-SYMBOL))
;;; Setf expander for the form (LDB bytespec int).
;;; Recall that the int form must itself be suitable for SETF.
 (define-setf-expander ldb (bytespec int &environment env)
   (multiple-value-bind (temps vals stores
                          store-form access-form)
       (get-setf-expansion int env);Get setf expansion for int.
     (let ((btemp (gensym))     ;Temp var for byte specifier.
           (store (gensym))     ;Temp var for byte to store.
           (stemp (first stores))) ;Temp var for int to store.
       (if (cdr stores) (error "Can't expand this."))
;;; Return the setf expansion for LDB as five values.
       (values (cons btemp temps)       ;Temporary variables.
               (cons bytespec vals)     ;Value forms.
               (list store)             ;Store variables.
               `(let ((,stemp (dpb ,store ,btemp ,access-form)))
                  ,store-form
                  ,store)               ;Storing form.
               `(ldb ,btemp ,access-form) ;Accessing form.
              ))))
Affected By: None.
