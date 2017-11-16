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

set_place_value(Env,[Place,Obj], Value):-!, type_or_class_nameof(Obj,Type),set_place_value(Env,Obj,Type,Place,Value).
set_place_value(Env, Obj, Value):-!, type_or_class_nameof(Obj,Type),set_place_value(Env,Obj,Type,value,Value).


set_place_value(Env,Obj,Type,Place,Value):- 
  atomic_list_concat(List,'_',Place),
  set_place_value6(Env,Place,List,Type,Obj,Value).

set_place_value6(_Env,_Place,[Type,Prop],Type,Obj, Value):- update_opv(Obj,Prop,Value),!.
set_place_value6(_Env, Place,_List,      _Type,Obj, Value):- update_opv(Obj,Place,Value),!.

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
