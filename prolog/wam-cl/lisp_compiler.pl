/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 * (c) Neil Smith, 2001
 *
 * This program, and its associated support files, forms a compiler
 * for a subset of the language LISP.  It supports a few simple 
 * built-in procedures, listed below.  It also supports both special
 * and lexical variables, and higher-order functions and lexical
 * closures.
 *
 * This compiler was written in LPA Prolog v3.6 under MS Windows.
 * It should run under other Prologs without too much conversion needed,
 * but note the required library modules.
 *
 *
 * Special forms
 *
 * [] and nil are treated as special forms, evaluating to [], and treated as 'false'
 * t is a special form, evaluating to t, and treated as 'true'
 * if, cond
 * progn (and implicit progn in defun and let bodies)
 * quote
 * let
 * setq
 * function
 * lambda
 * defvar, defparameter (both with and without initial values)
 * 
 * Built-in procedures (defined in builtin_lisp_functions.pl)
 *
 * cons, first, rest, null
 * eq, equalp
 * plus, minus, times, divide
 * lisp_not, or, and
 * lisp_apply
 * 
 * Other procedures are defined in lisp_library.pl
 *
 *******************************************************************/

/*******************************************************************
 *
 * Example definitions:
 * second(l) <<== first(rest(l)).
 * list_3(a, b, c) <<== cons(a, cons(b, cons(c, nil))).
 * 
 * Example use:
 * ?| - lisp_call(second([a,b,c]), Result).
 * Result = b
 *
 * ?| - second([a,b,c], Result).
 * Result = b
 *
 * ?| - lisp_call(list_3(tom, dick, harry), Result).
 * Result = [tom, dick, harry]
 * 
 * ?| - list_3(tom, dick, harry, Result).
 * Result = [tom, dick, harry]
 *
 *******************************************************************/

:- ensure_loaded(lpa_to_swi).

:- style_check.

:- ensure_loaded(library(higher_order)).
:- ensure_loaded(library(list_utilities)).


% :- ensure_loaded(builtin_lisp_functions). % Lisp primitives: this directives is at the end of the file
% :- ensure_loaded(lisp_library).	% Functions defined in lisp: this directive is at the end of the file
					% allowing them to be compiled correctly


:- op(1200, xfx, <<== ).	% function definition
:- op(1200,  fx, <<== ).	% functional imperative definition

:- dynamic special_var/2.	% closure environments


% Connection to LPA's built-in error handler

'?ERROR?'(Error, Form):-
	lisp_error_description(_, Error, Description),
	!,
	write('LISP ERROR  '),
	write(Description),
	write(Form),
	nl.
'?ERROR?'(Error, Goal):-
	error_hook(Error, Goal).


lisp_error_description(unbound_atom,        100, 'No value found for atom: ').
lisp_error_description(atom_does_not_exist, 101, 'Setq: Variable does not exist: ').
lisp_error_description(first_not_cons,      102, 'First: This is not a cons cell: ').
lisp_error_description(rest_not_cons,       103, 'Rest: This is not a cons cell: ').


% The hook into the compiler

term_expansion( (FunctionHead <<== FunctionBody), 
		(Head         :-   Body) ):-
	expand_function_head(FunctionHead, Head, ArgBindings, Result),
	expand_function_body(implicit_progn(FunctionBody), Result, Body, [ArgBindings]).

term_expansion( ( <<== FunctionBody), 
		( :-   Body) ):-
	expand_function_body(implicit_progn(FunctionBody), _Result, Body, []).


expand_function_head(FunctionHead, Head, ArgBindings, Result):-
	FunctionHead =.. [FunctionName | FormalArgs],
	zip_with(FormalArgs, ActualArgs, [Arg, Val, binding(Arg, [Val|_])]^true, ArgBindings),
	append(ActualArgs, [Result], HeadArgs),
	Head =.. [FunctionName | HeadArgs].


% expand_function_body(Function, Result, Body, Environment).
% Expands a Lisp-like function body into its Prolog equivalent
expand_function_body(nil, [], true, _Environment):-
	!.
expand_function_body([], [], true, _Environment):-
	!.
expand_function_body(t,   t,  true, _Environment):-
	!.
expand_function_body(if(Test, IfTrue, IfFalse), Result, Body, Environment):-
	!,
	expand_function_body(Test,    TestResult,  TestBody,  Environment),
	expand_function_body(IfTrue,  TrueResult,  TrueBody,  Environment),
	expand_function_body(IfFalse, FalseResult, FalseBody, Environment),
	Body = (	TestBody,
			( TestResult \= []
				-> 	TrueBody,  
					Result      = TrueResult
				;  	FalseBody, 
					Result      = FalseResult	) ).

expand_function_body(cond([]), [], true, _Environment):-
	!.
expand_function_body(cond([ [Test|ResultForms] |Clauses]), Result, Body, Environment):-
	!,
	expand_function_body(Test, TestResult, TestBody, Environment),
	expand_progn(ResultForms, TestResult, ResultFormsResult, ResultFormsBody, Environment),
	expand_function_body(cond(Clauses), ClausesResult, ClausesBody, Environment),
	Body = (	TestBody,
			( TestResult \= []
				->	ResultFormsBody,
					Result      = ResultFormsResult
				;	ClausesBody,
					Result      = ClausesResult )	).

expand_function_body(progn(Forms), Result, Body, Environment):-
	!,
	expand_progn(Forms, [], Result, Body, Environment).

expand_function_body(implicit_progn(Forms), Result, Body, Environment):-
	!,
	(one (Forms = [] ; Forms = [_|_] )
	->  expand_progn(Forms, [], Result, Body, Environment)
	;   expand_function_body(Forms, Result, Body, Environment)).

expand_function_body(setq(Atom, ValueForm), Result, Body, Environment):-
	!,
	lisp_error_description(atom_does_not_exist, ErrNo, _),
	expand_function_body(ValueForm, Result, ValueBody, Environment),
	Body = (	ValueBody,
			(	member(Bindings, Environment),
				member(binding(Atom, Value0), Bindings)
			->	extract_variable_value(Value0, _, Hole),
				Hole = [Result|_]
			;	special_var(Atom, Old)
			->	one(retract(special_var(Atom, Old))),
				assert(special_var(Atom, Result))
			;	throw(ErrNo, Atom)	)	).

expand_function_body(quote(Item), Item, true, _Environment):-
	!.

expand_function_body(let(NewBindings, BodyForms), Result, Body, Environment):-
	!,
	zip_with(Variables, ValueForms, [Variable, Form, bind(Variable, Form)]^true, NewBindings),
	expand_arguments(ValueForms, ValueBody, Values, Environment),
	zip_with(Variables, Values, [Var, Val, binding(Var, [Val|_])]^true, Bindings),
	Body = ( ValueBody, BodyFormsBody ),
	expand_function_body(implicit_progn(BodyForms), Result, BodyFormsBody, 
		[Bindings|Environment]).

expand_function_body(function(lambda(LambdaArgs, LambdaBody)), Result, Body, Environment):-
	!,
	expand_function_body(implicit_progn(LambdaBody), ClosureResult, ClosureBody, ClosureEnvironment),
	Result = closure(LambdaArgs, 
			[ClosureEnvironment, ClosureResult]^ClosureBody, 
			Environment),
	Body = true.

expand_function_body(function(Function), function(Function), true, _Environment):-
	!.

expand_function_body(defvar(Var), Result, Body, Environment):-
	!,
	expand_function_body(defvar(Var, nil), Result, Body, Environment).
expand_function_body(defvar(Var, Value), Result, Body, Environment):-
	!,
	expand_function_body(Value, Result, ValueBody, Environment),
	Body = (	ValueBody,
			(	special_var(Var, _)
			->	true
			;	assert(special_var(Var, Result))	)	).
expand_function_body(defparameter(Var), Result, Body, Environment):-
	!,
	expand_function_body(defparameter(Var, nil), Result, Body, Environment).
expand_function_body(defparameter(Var, Value), Result, Body, Environment):-
	!,
	expand_function_body(Value, Result, ValueBody, Environment),
	Body = (	ValueBody,
			(	special_var(Var, _)
			->	one(retract(special_var(Var, _)))
			;	true	),
			assert(special_var(Var, Result))	).

expand_function_body(Number, Number, true, _Environment):-
	number(Number),
	!.

expand_function_body(Atom, Value, Body, Environment):-
	atom(Atom),
	!,
	lisp_error_description(unbound_atom, ErrNo, _),
	Body = (one (	member(Bindings, Environment),
			member(binding(Atom, Value0), Bindings),
			extract_variable_value(Value0, Value, _)
		    ;	special_var(Atom, Value)
		    ;	throw(ErrNo, Atom)	)	).	

% Non built-in function expands into an explicit function call
expand_function_body(Function, Result, Body, Environment):-
	!,
	Function =.. [FunctionName | FunctionArgs],
	expand_arguments(FunctionArgs, ArgBody, Args, Environment),
	append(Args, [Result], ArgsResult),
	ExpandedFunction =.. [FunctionName | ArgsResult],
	Body = (	ArgBody,
			ExpandedFunction	).

	expand_arguments([], true, [], _Environment).
	expand_arguments([Arg|Args], Body, [Result|Results], Environment):-
		expand_function_body(Arg, Result, ArgBody, Environment),
		Body = (ArgBody, ArgsBody),
		expand_arguments(Args, ArgsBody, Results, Environment).


expand_progn([], Result, Result, true, _Environment).
expand_progn([Form | Forms], _PreviousResult, Result, Body, Environment):-
	expand_function_body(Form, FormResult, FormBody, Environment),
	Body = (FormBody, FormsBody),
	expand_progn(Forms, FormResult, Result, FormsBody, Environment).


% Now Prolog can understand them, compile the additional library files

:- ensure_loaded(builtin_lisp_functions).
:- ensure_loaded(lisp_library).

:- writeln('


| ?- lisp.
Welcome to Pro-Lisp!
This is a miniscule Lisp interpreter, written in Prolog
> (cons 1 nil)
( 1 ) 
> (defun my_second (lst) (car (cdr lst)))
MY_SECOND 
> (my_second `(a b c))
B 
> quit
Terminating Pro-Lisp
yes



').
