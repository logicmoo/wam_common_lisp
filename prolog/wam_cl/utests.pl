/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
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
:- module(tests, []).



:- include('header').

:- dynamic(tst:is_local_test/1).
:- multifile(tst:is_local_test/1).
:- discontiguous(tst:is_local_test/1).
:- dynamic(tst:is_local_test/2).
:- multifile(tst:is_local_test/2).
:- discontiguous(tst:is_local_test/2).
:- dynamic(tst:is_local_test/3).
:- multifile(tst:is_local_test/3).
:- discontiguous(tst:is_local_test/3).


tst:is_local_test(H):- tst:is_local_test(H,_V).
   
tst:is_local_test(H,V):-
  clause(tst:is_local_test(_,H,V),true,_).
   
tst:is_local_test(_,H,_):-
  clause(tst:is_local_test(H),true,_).
tst:is_local_test(_,H,V):-
  clause(tst:is_local_test(H,V),true,_).

call_test_compiled(Name,Value):- 
  always(compile_test(Name,Code,Return,Expected)),
  debug_var('Return',Return),
  debug_var('Expected',Expected),
  debug_var('OutValue',OutValue),
  ignore(catch((Code,Return=Value),goto(_,OutValue,_),Value=OutValue)),
  userout(Expected=Value).

  

compile_test(Name,Code,Return,Expected):-
   tst:is_local_test(Name,SExpression,Expected),
   as_sexp(SExpression,Expression),
   userout(:- compile_test(Name,Code,Return,Expected)),
   always(writeExpression(Expression)),
   lisp_compile(Return,Expression,Code),
   
   term_attvars(Code,AttVars),maplist(del_attr_rev2(vn),AttVars).




:- use_module(library(tabling)).
:- table fibt/2.
fibt(0, 1) :- !.
fibt(1, 1) :- !.
fibt(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fibt(N1, F1),
        fibt(N2, F2),
        F is F1+F2.

fibp(0, 1) :- !.
fibp(1, 1) :- !.
fibp(N, F) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fibp(N1, F1),
        fibp(N2, F2),
        F is F1+F2.
% SBCL 
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)
% YAP
% ?- time(fibp(38,O)).
% 4.924 CPU in 4.953 seconds ( 99% CPU)
% SWI
% ?- timel(fibp(38,O)).
% 252,983,942 inferences, 19.712 CPU in 19.949 seconds (99% CPU, 12833899 Lips)
% CLISP
% (time (fib 38))
% Run time: 53.0 sec.
% BProlog
% ?- time(fibp(38,O)).
% CPU time 75.764 seconds.

fibp2(N, F) :-
        N =< 1 
        -> F = 1 
        ;
        N1 is N-1,
        N2 is N-2,
        fibp2(N1, F1),
        fibp2(N2, F2),
        F is F1+F2.
% SBCL 
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)
% YAP
% ?- time(fibp2(38,O)).
% 3.124 CPU in 3.148 seconds ( 99% CPU)
% SWI
% ?- timel(fibp2(38,O)).
% 442,721,899 inferences, 24.558 CPU in 24.826 seconds (99% CPU, 18027611 Lips)
% CLISP
% (time (fib 38))
% 53.0 sec.


/*
 
NOTES:
 
* WAM-CL currently produces code 6 times slower than the handwritten code
 
* Handwritten Prolog is 2-3 slower than SBCL
 
* If WAM-CL becomes fast as handwritten code,
** it will be 17 times faster than CLISP
** it will be 6 times faster than ECL
 
 
 
 
(defun fib (n)
  (if (> n 1)
    (+ (fib (- n 1))
       (fib (- n 2)))
    1))
 
*/

% WAM-CL 
fibc(A, K) :- !,
        B=[[bv(n, [A|_])]],
        sym_arg_val_envc(n, A, C, B),
        >(C, 1, D),
        (   D\=[]
        ->  sym_arg_val_envc(n, A, Obj, B),
            -(Obj, 1, F),
            fibc(F, I),
            sym_arg_val_envc(n, A, G, B),
            -(G, 2, H),
            fibc(H, J),
            +(I, J, L),
            K=L
        ;   K=1
        ).
%fibc(_, _) :- '<<=='(fibc(n),if(n>1, fibc(n-1)+fibc(n-2), 1)).


% HANDWRITTEN

fibp3(N, F) :-
        N =< 1 
        -> F = 1 
        ;
        N1 is N-1,
        N2 is N-2,
        fibp3(N1, F1),
        fibp3(N2, F2),
        F is F1+F2.





% SBCL 1.3.1
% * (time (fib 38))
% 1.264000 seconds of total run time (1.264000 user, 0.000000 system)

% YAP-Prolog (Hand written)
% ?- time(fibp2(38,O)).
% 3.124 CPU in 3.148 seconds ( 99% CPU)

% YAP-Lisp (WAM-CL)
% ?- time(fibc(38,O)).
% 20.184 CPU in 20.340 seconds ( 99% CPU)

% SWI-Prolog (Hand written)
% ?- timel(fibp3(38,O)).
% 24.558 CPU in 24.826 seconds (99% CPU, 18027611 Lips)

% ECL 15.3.7
% > (time (fib 38))
% run time  : 25.516 secs (real time : 26.290 secs)

% CLISP 2.49
% (time (fib 38))
% 53.0 sec.

% SWI-Lisp (WAM-CL)
% ?- time(fibc(38,O)).
% 113.043 CPU in 114.324 seconds (99% CPU, 15665558 Lips)


sym_arg_val_envd(Var,_InValue,Value,Environment):- 
  (once((	(member(Bindings, Environment),
			member(bv(Var, Value0), Bindings),
			extract_variable_value(Value0, Value, _))
		    ;	get_var(Var, Value)
		    ;	(lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var))))).

fibd(A, K) :- !,
        B=[[bv(n, [A|_])]],
        sym_arg_val_envd(n, A, C, B),
        >(C, 1, D),
        (   D\=[]
        ->  sym_arg_val_envd(n, A, Obj, B),
            -(Obj, 1, F),
            fibd(F, I),
            sym_arg_val_envd(n, A, G, B),
            -(G, 2, H),
            fibd(H, J),
            +(I, J, L),
            K=L
        ;   K=1
        ).
%fibd(_, _) :- '<<=='(fibd(n),if(n>1, fibd(n-1)+fibd(n-2), 1)).
% YAP
% ?- time(fibd(38,O)).
% 41.608 CPU in 42.418 seconds ( 98% CPU)






wl:lambda_def(defmacro, u_is, mf_u_is, [u_eqf, u_expected, u_actual], [progn, [let, [[u_a, [gensym, '$ARRAY'([*], claz_base_character, "a")]], [u_b, [gensym, '$ARRAY'([*], claz_base_character, "b")]]], ['#BQ', [let, [[['#COMMA', u_a], ['#COMMA', u_expected]], [['#COMMA', u_b], ['#COMMA', u_actual]]], [if, [['#COMMA', u_eqf], ['#COMMA', u_a], ['#COMMA', u_b]], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, ['#COMMA', u_expected]], [quote, ['#COMMA', u_eqf]], [quote, ['#COMMA', u_actual]]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), ['#COMMA', u_a], ['#COMMA', u_b]], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]]]]).
wl:arglist_info(u_is, sf_u_is, [u_eqf, u_expected, u_actual], arginfo{all:[u_eqf, u_expected, u_actual], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[u_eqf, u_expected, u_actual], opt:0, req:[u_eqf, u_expected, u_actual], rest:0, sublists:0, whole:0}).
wl:init_args(x, u_is).

sf_u_is(Eqf_In, Expected_In, Actual_In, FnResult):- 
  mf_u_is(Eqf_In, Expected_In, Actual_In, MFResult),
  f_eval(MFResult, FnResult).

mf_u_is(Eqf_In, Expected_In, Actual_In, MFResult) :-
        nop(defmacro),
        Env=[bv(u_eqf, Eqf_In), bv(u_expected, Expected_In), bv(u_actual, Actual_In)|ReplEnv],
        global_env(ReplEnv),
        catch(( ( f_gensym('$ARRAY'([*], claz_base_character, "a"), A_Init),
                  f_gensym('$ARRAY'([*], claz_base_character, "b"), B_Init),
                  LEnv=[bv(u_a, A_Init), bv(u_b, B_Init)|Env],
                  get_var(LEnv, u_a, A_Get),
                  ( get_var(LEnv, u_actual, Actual_Get),
                    get_var(LEnv, u_b, B_Get)
                  ),
                  ( get_var(LEnv, u_a, A_Get17),
                    get_var(LEnv, u_expected, Expected_Get)
                  ),
                  get_var(LEnv, u_b, B_Get18),
                  ( get_var(LEnv, u_a, A_Get22),
                    get_var(LEnv, u_eqf, Eqf_Get20)
                  ),
                  ( get_var(LEnv, u_actual, Actual_Get21),
                    get_var(LEnv, u_expected, Expected_Get19)
                  ),
                  get_var(LEnv, u_b, B_Get23)
                ),
                [let, [[A_Get, Expected_Get], [B_Get, Actual_Get]], [if, [Eqf_Get20, A_Get17, B_Get18], [format, t, '$ARRAY'([*], claz_base_character, "OK: ~a is ~a to ~a~%"), [quote, Expected_Get19], [quote, Eqf_Get20], [quote, Actual_Get21]], [progn, [format, t, '$ARRAY'([*], claz_base_character, "FAILED: when matching ~a and ~a~%"), A_Get22, B_Get23], [sys_prolog_inline, '$ARRAY'([*], claz_base_character, "trace")]]]]=MFResult
              ),
              block_exit(u_is, MFResult),
              true).
:- set_opv(mf_u_is,type_of,sys_macro),
   set_opv(sf_u_is,type_of,sys_special_operator),
   set_opv(u_is, symbol_function, sf_u_is).

:- fixup_exports.

end_of_file.

 
parsing(Program, Forms0):- sformat(S,'(\n~s\n)\n',[Program]),str_to_expression(S,Forms0).
run666(Program, Values) :-
    quietly(parsing(Program, Forms)),
    maplist(see_and_do(eval),Forms,Values),
    last(Values,Last),
    writeExpression(Last).

see_and_do(Pred2, I,O):-
  userout(seeingFormala(I)),
  always(call(Pred2,I,O)),
  userout(result(O)).

:- set_prolog_flag(double_quotes,string).

% if_script_file_time666(_):-!.
if_script_file_time666(X):- prolog_statistics:time(user:X).

% Append:
test(0) :- if_script_file_time666(run666("
        (defun append (x y)
          (if x
              (cons (car x) (append (cdr x) y))
            y))

        (append '(a b) '(3 4 5))")).

    %@ V = [append, [a, b, 3, 4, 5]].
    

% Fibonacci, naive version:
test(1) :- if_script_file_time666(run666("
        (defun fib (n)
          (if (= 0 n)
              0
            (if (= 1 n)
                1
              (+ (fib (- n 1)) (fib (- n 2))))))
        (fib 24)")).

    %@ % 14,255,802 inferences, 3.71 CPU in 3.87 seconds (96% CPU, 3842534 Lips)
    %@ V = [fib, 46368].
    

% Fibonacci, accumulating version:
test(2) :- if_script_file_time666(run666("
        (defun fib (n)
          (if (= 0 n) 0 (fib1 0 1 1 n)))

        (defun fib1 (f1 f2 i to)
          (if (= i to)
              f2
            (fib1 f2 (+ f1 f2) (+ i 1) to)))

        (fib 250)")).

    %@ % 39,882 inferences, 0.010 CPU in 0.013 seconds (80% CPU, 3988200 Lips)
    %@ V = [fib, fib1, 7896325826131730509282738943634332893686268675876375].
    

% Fibonacci, iterative version:
test(3):- if_script_file_time666(run666("
        (defun fib (n)
          (setq f (cons 0 1))
          (setq i 0)
          (while (< i n)
            (setq f (cons (cdr f) (+ (car f) (cdr f))))
            (setq i (+ i 1)))
          (car f))

        (fib 350)")).

    %@ % 34,233 inferences, 0.010 CPU in 0.010 seconds (98% CPU, 3423300 Lips)
    %@ V = [fib, 6254449428820551641549772190170184190608177514674331726439961915653414425].
    

% Higher-order programming and eval:
test(4):- if_script_file_time666(run666("
        (defun map (f xs)
          (if xs
              (cons (eval (list f (car xs))) (map f (cdr xs)))
            ()))

        (defun plus1 (x) (+ 1 x))

        (map 'plus1 '(1 2 3))
        "
        )).

    %@ V = [map, plus1, [2, 3, 4]].
 

unused_ :- writeln('
| ?- lisp.
Welcome to WAM-CL!
This is a miniscule Lisp interpreter, written in Prolog
> (cons 1 nil)
( 1 ) 
> (defun my_second (lst) (car (cdr lst)))
MY_SECOND 
> (my_second \'(a b c))
B 
> (defun fib (n) (if (> n 1) (+ (fib (- n 1)) (fib (- n 2)))1))

> quit
Terminating WAM-CL
yes
'
).


