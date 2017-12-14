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
:- ensure_loaded((utils_for_swi)).

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
