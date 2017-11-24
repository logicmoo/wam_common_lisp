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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(tests, []).

:- set_module(class(library)).

:- include('header.pro').

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
  must_or_rtrace(compile_test(Name,Code,Return,Expected)),
  debug_var('Return',Return),
  debug_var('Expected',Expected),
  debug_var('OutValue',OutValue),
  ignore(catch((Code,Return=Value),goto(_,OutValue,_),Value=OutValue)),
  dmsg(Expected=Value).

  

compile_test(Name,Code,Return,Expected):-
   tst:is_local_test(Name,SExpression,Expected),
   as_sexp(SExpression,Expression),
   dbmsg(compile_test(Name,Code,Return,Expected)),
   must_or_rtrace(writeExpression(Expression)),
   lisp_compile(Return,Expression,Code),
   
   term_attvars(Code,AttVars),maplist(del_attr_rev2(vn),AttVars).


:- fixup_exports.



end_of_file.

/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (tests.pl)
 *
 * (c) Neil Smith, 2001
 *
 * A few sample function definitions, mainly used by me as simple 
 * test cases for the compiler.  I'm sure you can come up with 
 * something better...
 *
 *******************************************************************/


simple(x) <<== x.


lisp_append_2(l1, l2) <<==
	cond(  [[null(l1), l2], 
		[t,	cons( first(l1),
			      lisp_append_2(rest(l1),
			                    l2))]]).
 

lisp_error(x) <<== setq(y, 5).

%:- rtrace.
lisp_let() <<==
	let([bind(x, 3), bind(y, 5)], 
		      progn(x,y)).

lisp_let1() <<==
	let([bind(x, 3), bind(y, 5)], 
			x, 		% implicit progn here
			y).


% maps 'first' over a list of lists
mapfirst(l) <<==
	mapcar(function(first), l).


<<== defvar(fred, 13).

<<== defvar(george).


reset_george(val) <<==
	setq(george, val).


make_adder(x) <<==
	function(lambda([y], plus(x, y))).


scale_list(xs, scale) <<==
	let([bind(fred, function(lambda([num], times(scale, num))))], mapcar(fred, xs)).


make_summer(total) <<== 
	function(lambda([n],
		setq(total, plus(total, n)))).


sum_with_map(xs) <<==
	let([bind(running_total, 0)],
		let([bind(summer, function(lambda([n], setq(running_total, 
							plus(running_total, n)))))],
		 mapcar(summer, xs),
		  running_total )).


 
parsing(Program, Forms0):- sformat(S,'(\n~s\n)\n',[Program]),str_to_expression(S,Forms0).
run666(Program, Values) :-
    quietly(parsing(Program, Forms)),
    maplist(see_and_do(eval),Forms,Values),
    last(Values,Last),
    writeExpression(Last).

see_and_do(Pred2, I,O):-
  dmsg(seeingFormala(I)),
  call(Pred2,I,O),
  dmsg(result(O)).

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


