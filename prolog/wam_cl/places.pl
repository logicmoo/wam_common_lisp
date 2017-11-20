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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(places, []).
:- set_module(class(library)).
:- include('header.pro').
:- ensure_loaded((utils_for_swi)).

value_or([Value],Value,_):- !.
value_or([],Value,Value).

place_op(_Env,getf, Obj, Place,  Result):- 
  value_or(Place,Prop,value),
  get_opv(Obj,Prop, Result).

place_op(_Env,setf, Obj, Place,  Result):-  value_or(Place,Prop,value),
  update_opv(Obj,Prop, Result),!.
place_op(Env,setf, Place, [Result],  Result):- !,set_place_value(Env,Place, Result).

place_op(Env,incf, Var, LV,  Result):- value_or(LV,Value,1),
   get_place_value(Env,Var, Old),
   Result is Old+ Value,
   set_place_value(Env,Var, Result).
place_op(Env,decf, Var, LV,  Result):- value_or(LV,Value,1),
   get_place_value(Env,Var, Old),
   Result is Old-Value,
   set_place_value(Env,Var, Result).



get_place_value(Env, Obj, Value):- atom(Obj),!,symbol_value_or(Env,Obj,with_place_value(Env,get_opv,Obj, Value),Value).
get_place_value(Env, Obj, Value):- with_place_value(Env,get_opv,Obj, Value).

set_place_value(Env, Obj, Value):- atom(Obj),set_symbol_value(Env,Obj,Value).
set_place_value(Env, Obj, Value):- with_place_value(Env,set_opv,Obj, Value).


with_place_value(Env,OPR,[Place,Obj], Value):-!, type_or_class_nameof(Obj,Type),with_place_value(Env,OPR,Obj,Type,Place,Value).
with_place_value(Env,OPR, Obj, Value):-!, type_or_class_nameof(Obj,Type),with_place_value(Env,OPR,Obj,Type,value,Value).

with_place_value(Env,OPR,Obj,Type,Place,Value):- 
  atomic_list_concat(List,'_',Place),
  with_place_value6(Env,OPR,Place,List,Type,Obj,Value).

with_place_value6(_Env,OPR,_Place,[Type,Prop],Type,Obj, Value):- call_opv(OPR,Obj,Prop,Value),!.
with_place_value6(_Env,OPR, Place,_List,      _Type,Obj, Value):- call_opv(OPR,Obj,Place,Value),!.

call_opv(OPR,Obj,Place,Value):- call(OPR,Obj,Place,Value).

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
