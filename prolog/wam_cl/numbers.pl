/*******************************************************************
 *
 * C1 Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(mth, []).
:- set_module(class(library)).
:- include('header').


grovel_math:-
  doall((((((clause(arithmetic:eval(P,_,_),_),nonvar(P)),(functor(P,F,A),always(define_cl_math(F,A)))))),fail)),
  grovel_preds(_).

grovel_preds(M):-
 %module_property(M,file(File)),
 
 doall((
  source_file(M:P,_File),
  %current_predicate(_,M:P), \+ predicate_property(M:P,imported_from(_)),
  %predicate_property(M:P,module(M)),
  functor(P,F,A),
  once(forall(clause(wl:grovel_pred(M,F,A),B),call(B))),
  fail)).

wl:grovel_pred(M,F,1):-
  atom(F),atom(M),
  atom_concat_or_rtrace('is_',R,F),atom_concat(_,'p',R),
  doall(((get_opv_iii(_Sym,function,SF),
  (atom(SF),atom_concat(Prefix,R,SF),
   \+ atomic_list_concat([_,_,_|_],'_',Prefix),
   Head=..[SF,N,RetVal],
   PBody=..[F,N],
   (assert_lsp(user:Head :- t_or_nil(M:PBody,RetVal))))),fail)).

wl:interned_eval(call(notrace(grovel_math))).


% define_cl_math(F,0):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X],FP=..[F], assertz(P:- X is FP).


/*
(defun my-max (real &rest reals) (dolist (r reals real)(when (> r real) (setq real r)))) 
==>
cl_my_max(Real, RestNKeys, FnResult) :-
        nop(global_env(ReplEnv)),
        GEnv=[[[bv(real, Real), bv(u_reals, RestNKeys)]|ReplEnv]|ReplEnv],
        get_var(GEnv, real, Real_Get),
        LEnv=[bv(real, Real_Get)|GEnv],
        get_var(LEnv, u_reals, Reals_Get),
        BV=bv(u_r, Ele),
        AEnv=[BV|LEnv],
        forall(member(Ele, Reals_Get),
               ( nb_setarg(2, BV, Ele),
                 get_var(AEnv, real, Real_Get12),
                 get_var(AEnv, u_r, R_Get),
                 (   Real_Get12>R_Get
                 ->  get_var(AEnv, u_r, R_Get18),
                     set_var(AEnv, real, R_Get18),
                     _2740=R_Get18
                 ;   _2740=[]
                 )
               )),
        get_var(LEnv, real, Real_Get24),
        Real_Get24=FnResult.
*/
wl: init_args(1,cl_max).
cl_max(Real,Reals,Out):-  
   (Reals=[R|DoList] ->
    ( R > Real -> 
       cl_max(R,DoList,Out);
        cl_max(Real,DoList,Out));
    Out=Real).

wl: init_args(1,cl_min).
cl_min(Real,Reals,Out):-  
   Reals=[R|DoList] ->
    ( R < Real -> 
       cl_min(R,DoList,Out);
        cl_min(Real,DoList,Out));
    Out=Real.

define_cl_math(max,_):-!.
define_cl_math(min,_):-!.
define_cl_math(F,1):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X,R],FP=..[F,X],
  (is_defined(CLN,2)-> true ; always(assert_lsp(P:- R is FP))).
define_cl_math(F,2):- atom_concat_or_rtrace('cl_',F,CLN), P=..[CLN,X,Y,R],FP=..[F,X,Y],
  (is_defined(CLN,3)-> true ; always(assert_lsp(P:- R is FP))).
define_cl_math(_,_).

wl:type_checked(P):- current_predicate(_,mth:P), \+ predicate_property(mth:P,imported_from(_)),
   P=..[_|List],maplist( =(number),List).

% Lisp COERCE
wl:coercion(In, number, Out):- is_numberp(In),to_prolog_number(In,Out).

to_prolog_number('$NUMBER'(_,Y),Z):- !, to_prolog_number(Y,Z).
to_prolog_number('$RATIO'(X,Y),Z):- !, to_prolog_number(X,XX),to_prolog_number(Y,YY),Z is XX/YY.
to_prolog_number('$COMPLEX'(X,Y),Z):- !, to_prolog_number(Y,YY), 0 is YY,to_prolog_number(X,Z).
to_prolog_number('$EXP'(I,_,E),N):- !, notrace(catch(N is (I * 10^E),_,fail)),!.
to_prolog_number(X,Y):- Y is X,!.

% Lisp Type Predicates

is_numberp('$NUMBER'(_,_)).
is_numberp('$RATIO'(_,_)).
is_numberp('$COMPLEX'(_,_)).
is_numberp('$EXP'(_,_,_)).
is_numberp(P):- number(P).

is_integerp(P):- integer(P).
is_bignump(P):- compound(P),arg(1,P,Type),!,Type==claz_bignum,(functor(P,'$NUMBER',_);functor(P,'$EXP',_)).

is_oddp(N):- 1 is N div 2.
is_evenp(N):- 0 is N div 2.

is_minusp(N):- N<0.
is_plusp(N):- N>0.
is_zerop(N):- N=:=0.


% Lisp Comparison Predicates

cl_c61(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret). 
'='(N1,N2,Ret):- t_or_nil( (N1=:=N2),Ret).

cl_c60_c61(N1,N2,Ret):- t_or_nil('=<'(N1,N2),Ret).
'<='(N1,N2,Ret):- t_or_nil('=<'(N1,N2),Ret).

cl_c62_c61(N1,N2,Ret):- t_or_nil('>='(N1,N2),Ret).
'>='(N1,N2,Ret):- t_or_nil('>='(N1,N2),Ret).

cl_c60(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
'<'(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 

cl_c62(N1,N2,Ret):- t_or_nil(<(N1,N2),Ret). 
'>'(N1,N2,Ret):- t_or_nil(>(N1,N2),Ret). 

% Lisp Operators/Functions
cl_sqrt(X,Y):-
    X < 0 
       -> (NX is -X , cl_sqrt(NX,NY), Y = '$COMPLEX'(0, NY))
     ;
    (\+ integer(X)
      -> (Y is sqrt(X)) 
      ;
      (IY is sqrt(X), RY is floor(IY),(RY=:=IY -> Y=RY ; Y=IY))).

cl_exp(N,Ret):- Ret is exp(N).

cl_expt(N1,N2,Ret):- Ret is (N1 ^ N2).




% asserting1... u
wl: lambda_def(defun,Sym,Cl_Sym,[u_x, c38_optional, [u_y, 1]],[[truncate,Sym, [/, u_x, u_y]]]):-
  var_or_atom(Cl_Sym),tround(Sym),atom_concat_or_rtrace('cl_',Sym,Cl_Sym).


de_ratio('$RATIO'(N,D),N,D):-!.
de_ratio(N,N,1).
re_ratio(Rem,1,Rem).
re_ratio(0,_,0).
re_ratio(Rem,Y,'$RATIO'(Rem,Y)).

wl: init_args(1,Sym):-tround(Sym).
tround(Sym):- tround0(Sym).
tround(FSym):- var_or_atom(FSym),tround0(Sym),atom_concat_or_rtrace('f',Sym,FSym).

var_or_atom(FSym):- var(FSym)->true;atom(FSym).

tround0(round).
tround0(floor).
tround0(ceiling).
tround0(truncate).

% asserting... u
cl_ceiling(X, RestNKeys, MResult):- pl_truncate(ceiling,X, RestNKeys, MResult).
cl_floor(X, RestNKeys, MResult):- pl_truncate(floor,X, RestNKeys, MResult).
cl_truncate(X, RestNKeys, MResult):- pl_truncate(truncate,X, RestNKeys, MResult).
cl_round(X, RestNKeys, MResult):- pl_truncate(round,X, RestNKeys, MResult).
pl_truncate(_Type, X, RestNKeys, MResult):- 
     nth_value(RestNKeys,1,Y,1),
     de_ratio(X,X0,Xd),
     de_ratio(Y,Y0,Yd),
     XX is X0 * Yd,
     YY is Y0 * Xd,
     DD is Yd * Xd,
     Whole  is XX div YY,
     Rement is XX mod YY,
     re_ratio(Rement,DD,RatRem),!,
     push_values([Whole,RatRem],MResult).

% asserting... u
cl_ftruncate(X, RestNKeys, MResult):- pl_ftruncate(truncate,X, RestNKeys, MResult).
cl_fceiling(X, RestNKeys, MResult):- pl_ftruncate(ceiling,X, RestNKeys, MResult).
cl_ffloor(X, RestNKeys, MResult):- pl_ftruncate(floor,X, RestNKeys, MResult).
cl_fround(X, RestNKeys, MResult):- pl_ftruncate(round,X, RestNKeys, MResult).
pl_ftruncate(_Type,X, RestNKeys, MResult):- 
     nth_value(RestNKeys,1,Y,1),
     de_ratio(X,X0,Xd),
     de_ratio(Y,Y0,Yd),
     XX is X0 * Yd,
     YY is Y0 * Xd,
     DD is Yd * Xd,
     Whole  is (XX div YY)*1.0,
     Rement is (XX mod YY)*1.0,
     re_ratio(Rement,DD,RatRem),!,
     push_values([Whole,RatRem],MResult).

/*
;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be negative then decrement the quotient and augment the remainder by
;;; the divisor.
;;;
*/
wl:interned_eval_todo(
'(defun floor (number &optional divisor)
  "Return the greatest integer not greater than number, or number/divisor.
  The second returned value is (mod number divisor)."
  (if (null divisor)(setq divisor 1))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
             (if (minusp divisor)
               (plusp number)
               (minusp number)))
      (if (called-for-mv-p)
        (values (1- tru) (+ rem divisor))
        (1- tru))
      (values tru rem))))').

%cl_truncate(X,Y):- Y is floor(X).
%cl_log(X,Y):- Y is log(X).


'1+'(N,Ret):- Ret is N + 1.
'1-'(N,Ret):- Ret is N - 1.

cl_c43(N1,N2,Ret):- Ret is (N1 + N2).
'+'(N1,N2,Ret):- Ret is (N1 + N2).

cl_c45(N1,N2,Ret):- Ret is (N1 + N2).
'-'(N1,N2,Ret):- Ret is (N1 - N2).

cl_c42(N1,N2,Ret):- Ret is (N1 + N2).
'*'(N1,N2,Ret):- Ret is (N1 * N2).

cl_c47(N1,N2,Ret):- Ret is (N1 + N2).
'/'(N1,N2,Ret):- Ret is (N1 / N2).

cl_plus(Num1, Num2, Result):-
        Result is Num1 + Num2.
cl_minus(Num1, Num2, Result):-
	Result is Num1 - Num2.
cl_times(Num1, Num2, Result):-
	Result is Num1 * Num2.
cl_divide(Num1, Num2, Result):-
	Result is Num1 / Num2.


:- fixup_exports.


% tests

end_of_file.

 (exp 0) =>  1.0
 (exp 1) =>  2.718282
 (exp (log 5)) =>  5.0 
 (expt 2 8) =>  256
 (expt 4 .5) =>  2.0
 (expt #c(0 1) 2) =>  -1
 (expt #c(2 2) 3) =>  #C(-16 16)
 (expt #c(2 2) 4) =>  -64 

  (expt -8 1/3) => #C(1.0 1.7320508)


