/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 * Changes since 2001:
 *
 *  ..............................
 *
 *
 * Neil''s Notes:
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
:- module(comp, []).
:- set_module(class(library)).
:- include('header.pro').
                                         :- set_module(class(library)).
:- ensure_loaded(utils_for_swi).

:- style_check.


% :- ensure_loaded(builtin_lisp_functions). % Lisp primitives: this directives is at the end of the file
% :- ensure_loaded(lisp_library).	% Functions defined in lisp: this directive is at the end of the file
					% allowing them to be compiled correctly


:- op(1200, xfx, <<== ).	% function definition
:- op(1200,  fx, <<== ).	% functional imperative definition


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

bvof(E,L):-member(E,L).
env_memb(E,L):-member(E,L).

lisp_error_description(unbound_atom,        100, 'No value found for atom: ').
lisp_error_description(atom_does_not_exist, 101, 'SetQ: Variable does not exist: ').
lisp_error_description(first_not_cons,      102, 'Form1: This is not a cons cell: ').
lisp_error_description(rest_not_cons,       103, 'Rest: This is not a cons cell: ').

find_incoming_value(Ctx,_Ev,Atom,InValue,Value):-
      debug_var([Atom,'_In'],InValue),
      debug_var([Atom,'_Thru'],Value),
      ignore((member(bv(Atom0,[Value0|Unused]),Ctx.argbindings),
         Atom0==Atom,Value0=InValue,debug_var("__",Unused))).

% The hook into the compiler

lisp_compiler_term_expansion( (FunctionHeadP <<== FunctionBodyP),
		[(Head         :- !,  Code),
                 (Head         :- fail, <<==(FunctionHead , FunctionBody))] ):-
        must_det_l((expand_pterm_to_sterm(FunctionHeadP,FunctionHead),
        expand_pterm_to_sterm(FunctionBodyP,FunctionBody),
        make_compiled(FunctionHead,FunctionBody,Head,Code))).

lisp_compiler_term_expansion( ( <<== FunctionBodyP), ( :-   Code) ):-
        must_det_l((expand_pterm_to_sterm(FunctionBodyP,FunctionBody),
	must_compile_body(_Cx,toplevel,_Result,implicit_progn([FunctionBody]), Body),
   body_cleanup(Body,Code))).

make_compiled(FunctionHead,FunctionBody,Head,Code):-
   Ctx = ctx{head:Head,argbindings:ArgBindings},
	must_or_rtrace(expand_function_head(Ctx,Env,FunctionHead, Head, ArgBindings, Result,HeadCode)),
        debug_var("RET",Result),debug_var("Env",Env),
	must_compile_body(Ctx,Env,Result,implicit_progn([FunctionBody]),Body0),
        Body = (Env=[ArgBindings],Body0),
    body_cleanup((HeadCode,Body),Code).

lisp_compiled_eval(SExpression):- 
  as_sexp(SExpression,Expression),
  lisp_compiled_eval(Expression,Result),
  dbmsg(result(Result)).
                                
lisp_compiled_eval(SExpression,Result):-
  as_sexp(SExpression,Expression),
  dbmsg(lisp_compile(Expression)),
  lisp_compile(Result,Expression,Code),
  dbmsg(Code),
  call(Code),!.


lisp_compile(SExpression):-
  as_sexp(SExpression,Expression),
  dbmsg(lisp_compiled_eval(Expression)),
  lisp_compile(Expression,Code),!,
  dbmsg(Code).

lisp_compile(Expression,Body):-
   debug_var('_Ignored',Result),
   lisp_compile(Result,Expression,Body).

lisp_compile(Result,SExpression,Body):- 
   lisp_compile(toplevel,Result,SExpression,Body).

lisp_compile(Env,Result,Expression,Body):- 
   lisp_compile(ctx{head:lisp_compile(),argbindings:[]},Env,Result,Expression,Body).

lisp_compile(Ctx,Env,Result,SExpression,Body):- 
   as_sexp(SExpression,Expression),
   compile_forms(Ctx,Env,Result,[Expression],Body).


compile_forms(Ctx,Env,Result,FunctionBody,Code):-
   must_compile_body(Ctx,Env,Result,implicit_progn(FunctionBody), Body),!,
   body_cleanup(Body,Code).


% compile_body(Ctx,Env,Result,Function, Body).
% Expands a Lisp-like function body into its Prolog equivalent

must_compile_body(Ctx,Env,Result,Function, Body):-
  must_or_rtrace(compile_body(Ctx,Env,Result,Function, Body)).


% PROG
/*(defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/
:- dynamic(compiler_macro_left_right/3).
:- discontiguous(compiler_macro_left_right/3).
compiler_macro_left_right(prog,[Vars|TagBody], [block,[],[let,Vars,[tagbody|TagBody]]]).

% (defmacro unless (test-form &rest forms) `(if (not ,test-form) (progn ,@forms)))
compiler_macro_left_right(unless,[Test|IfFalse] , [if, Test, [], [progn|IfFalse]]).
% (defmacro when (test-form &rest forms) `(if ,test-form (progn ,@forms)))
compiler_macro_left_right( when,[Test|IfTrue]  , [if, Test, [progn|IfTrue], []]).

% IF/2
compiler_macro_left_right(if,[Test, IfTrue], [if, Test, IfTrue ,[]]).

% AND
compiler_macro_left_right(and,[], []).
compiler_macro_left_right(and,[Form1], Form1).
compiler_macro_left_right(and,[Form1,Form2], [if,Form1,Form2]).
compiler_macro_left_right(and,[Form1|Rest], [and,Form1,[and|Rest]]).
 

:- discontiguous(compile_body/5).

% VAR
compile_body(Ctx,Env,Result,Var, Code):- is_ftVar(Var), !,
  debug_var("EVAL",Var),
  compile_body(Ctx,Env,Result,[eval,Var], Code).
 
% SOURCE TRANSFORMATIONS
compile_body(Ctx,Env,Result,[M|MACROLEFT], Code):- 
  term_variables([M|MACROLEFT],VarsS),
  compiler_macro_left_right(M,MACROLEFT,MACRORIGHT),
  term_variables(MACRORIGHT,VarsE),
  VarsE==VarsS,!,
  must_compile_body(Ctx,Env,Result,MACRORIGHT, Code).

% Compiler Plugin 
compile_body(Ctx,Env,Result,InstrS,Code):-
  shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code),!.

% SELF EVALUATING OBJECTS
compile_body(_Cx,_Ev, [],[],true):- !.
compile_body(_Cx,_Ev, [],nil,true):- !.
compile_body(_Cx,_Ev,SelfEval,SelfEval,true):- notrace(is_self_evaluationing_object(SelfEval)),!.

% symbols
compile_body(Ctx,Env,Value,Atom, Body):- atom(Atom),!, 
  must_or_rtrace(compile_symbol_getter(Ctx,Env,Value, Atom, Body)).

% QUOTE 
compile_body(_Cx,_Ev,Item,[quote, Item],  true):- !.

% COMMENTS
compile_body(_Ctx,_Env,[],['$COMMENT'|_InstrS],true):-!.
compile_body(_Ctx,_Env,[],['$COMMENT0'|_InstrS],true):-!.
compile_body(_Ctx,_Env,[],['$COMMENT1'|_InstrS],true):-!.
compile_body(_Ctx,_Env,[],['$COMMENT2'|_InstrS],true):-!.


% OR
compiler_macro_left_right(or,[], []).
compiler_macro_left_right(or,[Form1], Form1).
% OR-2 needs to use body compiler below
% compiler_macro_left_right(or,[Form1,Form2,Form3|Rest], [or,Form1,[or,Form2,[or,Form3,[or|Rest]]]]).
% OR-2+
compile_body(Ctx,Env,Result,[or,Form1|Form2],Code):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1),
   must_compile_body(Ctx,Env,Result2,[or|Form2], Body2),
   debug_var("FORM1_Res",Result1),
        Code = (	Body1,
			( Result1 \== []
				-> 	Result = Result1
				;  	(Body2, Result = Result2))).

% PROG1
compile_body(Ctx,Env,Result,[prog1,Form1|FormS],Code):- !,
   must_compile_body(Ctx,Env,Result,Form1, Body1),
   must_compile_progn(Ctx,Env,_ResultS,FormS,Result,Body2),
   Code = (Body1, Body2).

% PROG2
compile_body(Ctx,Env,Result,[prog2,Form1,Form2|FormS],Code):- !,
   must_compile_body(Ctx,Env,_Result1,Form1, Body1),
   must_compile_body(Ctx,Env,Result,Form2, Body2),
   must_compile_progn(Ctx,Env,_ResultS,FormS,Result,BodyS),
   Code = (Body1, Body2, BodyS).

% DEFMACRO
compile_body(_Cx,_Ev,Name,[defmacro,Name,FormalParms|Body0], CompileBody):- !,
      maybe_get_docs(defmacro,Name,Body0,Body),
      CompileBody =(retractall(macro_lambda(Name,_,_)),
	assert(macro_lambda(Name, FormalParms, Body))).

% Use a previous DEFMACRO
compile_body(Cxt,Env,Result,[Procedure|Arguments],CompileBodyCode):-
  macro_lambda(Procedure, FormalParams, LambdaExpression),
  must_or_rtrace(bind_formal_parameters(FormalParams, Arguments, [], NewEnv,BindCode)),
  call(BindCode),
  must_or_rtrace(expand_commas([NewEnv|Env],CommaResult,LambdaExpression,Code)),
  dbmsg(macro(macroResult(CommaResult))),
  must_compile_body(Cxt,Env,Result,[progn|CommaResult], CompileBody),
  CompileBodyCode = (Code,CompileBody).

% `, Backquoted commas
compile_body(_Cx,Env,Result,['$BQ',Form], Code):-!,compile_bq(Env,Result,Form,Code).
compile_body(_Cx,Env,Result,['`',Form], Code):-!,compile_bq(Env,Result,Form,Code).


% DEFUN
compile_body(_Cx,_Ev,Name,[defun,Name0,Args|FunctionBody0], CompileBody):- 
    combine_setfs(Name0,Name),
    must_or_rtrace(maybe_get_docs(defun,Name,FunctionBody0,FunctionBody)),
    FunctionHead=[Name|Args],
    CompileBody = (% asserta((Head  :- (fail, <<==(FunctionHead , FunctionBody)))),
                   asserta((Head  :- (!,  Code)))),!,
    make_compiled(FunctionHead,FunctionBody,Head,Code).


is_when(X):- dbmsg(warn(free_pass(is_when(X)))).

combine_setfs(Name0,Name):-atom(Name0),Name0=Name.
combine_setfs(Name0,Name):-atomic_list_concat(Name0,-,Name).

% EVAL-WHEN
compile_body(Ctx,Env,Result,['eval-when',Flags|Forms], Code):- !, 
 ((member(X,Flags),is_when(X) )
  -> compile_body(Ctx,Env,Result,[progn,Forms], Code) ; Code = true).

% #+
compile_body(Ctx,Env,Result,['#+',Flag,Form], Code):- !, symbol_value(Env,'*features*',List),
   (member(Flag,List) -> compile_body(Ctx,Env,Result,Form, Code) ; Code = true).
% #-
compile_body(Ctx,Env,Result,['#+',Flag,Form], Code):- !, symbol_value(Env,'*features*',List),
( \+ member(Flag,List) -> compile_body(Ctx,Env,Result,Form, Code) ; Code = true).  


% DOLIST
compile_body(Ctx,Env,Result,['dolist',[Var,List]|FormS], Code):- !,
    compile_body(Ctx,Env,ResultL,List,ListBody),
    compile_body(Ctx,Env,Result,[progn,FormS], Body),
    Code = (ListBody,
        forall(member(X,ResultL),
              lexically_bind(Var,X,Body))).

% PROGN
compile_body(_Cx,_Ev,[],[progn],  true):- !.
compile_body(Ctx,Env,Result,[progn,Forms], Body):- !, must_compile_body(Ctx,Env,Result,Forms, Body).
compile_body(Ctx,Env,Result,[progn|Forms], Body):- !, must_compile_progn(Ctx,Env,Result,Forms, [],Body).
compile_body(Ctx,Env,Result,implicit_progn(Forms), Body):- is_list(Forms),!,must_compile_progn(Ctx,Env,Result,Forms, [],Body).
compile_body(Ctx,Env,Result,implicit_progn(Forms), Body):- !,must_compile_body(Ctx,Env,Result,Forms, Body).


% Lazy Reader
compile_body(Ctx,Env,Result, 's'(Str),  Body):-
  parse_sexpr_untyped(string(Str),Expression),!,
  must_compile_body(Ctx,Env,Result, Expression,  Body).


% IF (null ...)
compile_body(Ctx,Env,Result,[if, [null,Test], IfTrue, IfFalse], Body):-
	!,
   must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
   must_compile_body(Ctx,Env,TrueResult,IfTrue, TrueBody),
   must_compile_body(Ctx,Env,FalseResult,IfFalse, FalseBody),
        debug_var("IF",TestResult),
        Body = (	TestBody,
			( TestResult == []
				-> 	TrueBody,
					Result      = TrueResult
				;  	FalseBody,
					Result      = FalseResult	) ).

% IF-3
compile_body(Ctx,Env,Result,[if, Test, IfTrue, IfFalse], Body):-
	!,
   must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
   must_compile_body(Ctx,Env,TrueResult,IfTrue, TrueBody),
   must_compile_body(Ctx,Env,FalseResult,IfFalse, FalseBody),
        debug_var("IF",TestResult),
        Body = (	TestBody,
			( TestResult \= []
				-> 	TrueBody,
					Result      = TrueResult
				;  	FalseBody,
					Result      = FalseResult	) ).

% COND 
compile_body(_Cx,_Ev,[],[cond, []], true):- !.
compile_body(Ctx,Env,Result,[cond, [ [Test|ResultForms] |Clauses]], Body):-
	!,
	must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
	must_compile_progn(Ctx,Env,ResultFormsResult,ResultForms, TestResult, ResultFormsBody),
	must_compile_body(Ctx,Env,ClausesResult,[cond, Clauses],  ClausesBody),
	Body = (	TestBody,
			( TestResult \= []
				->	ResultFormsBody,
					Result      = ResultFormsResult
				;	ClausesBody,
					Result      = ClausesResult )	).


% CONS inine
compile_body(Ctx,Env,Result,[cons, IN1,IN2], Body):- \+ current_prolog_flag(lisp_inline,false),
	!,
        must_compile_body(Ctx,Env,MID1,IN1,  ValueBody1),
        must_compile_body(Ctx,Env,MID2,IN2,  ValueBody2),
        Body = (ValueBody1,ValueBody2,Result=[MID1|MID2]).



% (function (lambda ... ))
compile_body(Ctx,Env,Result,[function, [lambda,LambdaArgs| LambdaBody]], Body):-
	!,
	must_compile_body(Ctx,ClosureEnvironment,ClosureResult,implicit_progn(LambdaBody),  ClosureBody),
        debug_var('LArgs',LambdaArgs),
        debug_var('LResult',ClosureResult),
        debug_var('LEnv',ClosureEnvironment),
                     Result = [closure,LambdaArgs,
			[ClosureEnvironment, ClosureResult]^ClosureBody,
			Env],
	Body = true.

% (function ?? )
compile_body(_Cx,_Ev,[function|Function], [function|Function], true):- !.

% ((consure ...)..)
compile_body(Ctx,Env,ClosureResult,[[closure,LambdaArgs,
    [ClosureEnvironment, ClosureResult]^ClosureBody,
			AltEnv]|ActualParams],Code):-
	!,
	bind_formal_parameters(LambdaArgs, ActualParams, [ClosureEnvironment,AltEnv], AltEnvBindings,BindCode),
        call(BindCode),
	must_compile_body(Ctx,[AltEnvBindings|Env],ClosureResult,ClosureBody, Code).
	

% (lambda ...)
compile_body(Ctx,Env,Result,[lambda,LambdaArgs|LambdaBody], Body):-
	!,
	must_compile_body(Ctx,ClosureEnvironment,ClosureResult,implicit_progn(LambdaBody),  ClosureBody),
        debug_var('LArgs',LambdaArgs),
        debug_var('LResult',ClosureResult),
        debug_var('LEnv',ClosureEnvironment),
                     Result = [closure,LambdaArgs,
			[ClosureEnvironment, ClosureResult]^ClosureBody,
			Env],
	Body = true.





% PROGV            % ( progv ' ( a ) ` ( , ( + 1 1 ) ) a ) => 2
compile_body(Ctx,Env,Result,[progv,VarsForm,ValuesForm|FormS],Code):- !,
   must_compile_body(Ctx,Env,VarsRs,VarsForm,Body1),
   must_compile_body(Ctx,Env,ValuesRs,ValuesForm,Body2),
   must_compile_progn(Ctx,Env,Result,FormS,[],BodyS),
   Code = (Body1, Body2 , maplist(bind_dynamic_value(Env),VarsRs,ValuesRs), BodyS).

normalize_let([],[]).
normalize_let([Decl|NewBindingsIn],[Norm|NewBindings]):-
  must_or_rtrace(normalize_let1(Decl,Norm)),
  normalize_let(NewBindingsIn,NewBindings).


normalize_let1([bind, Variable, Form],[bind, Variable, Form]).
normalize_let1([Variable, Form],[bind, Variable, Form]).
normalize_let1( Variable,[bind, Variable, []]).

% LET
compile_body(Ctx,Env,Result,['let', []| BodyForms], Body):- compile_body(Ctx,Env,Result,[progn| BodyForms], Body).
compile_body(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body):-
     must_or_rtrace(normalize_let(NewBindingsIn,NewBindings)),!,        
	zip_with(Variables, ValueForms, [Variable, Form, [bind, Variable, Form]]^true, NewBindings),
	expand_arguments(Ctx,Env,'funcall',1,ValueForms, ValueBody, Values),
	zip_with(Variables, Values, [Var, Val, bv(Var, [Val|Unused])]^true,Bindings),

   must_or_rtrace((debug_var("_U",Unused),
   debug_var("LETENV",BindingsEnvironment),
   ignore((member(VarN,[Variable,Var]),atom(VarN),debug_var([VarN,'_Let'],Val))))), 

	must_compile_body(Ctx,BindingsEnvironment,Result,implicit_progn(BodyForms), BodyFormsBody),
         Body = ( ValueBody,BindingsEnvironment=[Bindings|Env], BodyFormsBody ).

% LET*
compile_body(Ctx,Env,Result,['let*', []| BodyForms], Body):- compile_body(Ctx,Env,Result,[progn| BodyForms], Body).
compile_body(Ctx,Env,Result,['let*', [Binding1|NewBindings]| BodyForms], Body):-
   compile_body(Ctx,Env,Result,['let', [Binding1], ['let*', NewBindings| BodyForms]], Body).


%   zip_with(Xs, Ys, Pred, Zs)
%   is true if Pred(X, Y, Z) is true for all X, Y, Z.

zip_with([], [], _, []).
zip_with([X|Xs], [Y|Ys], Pred, [Z|Zs]):-
	lpa_apply(Pred, [X, Y, Z]),
	zip_with(Xs, Ys, Pred, Zs).


% SETQ - PSET
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_assigns(Ctx,Env,Result,BodyForms, Body).

:- dynamic(op_replacement/2).
/*
op_replacement(+,plus).
op_replacement(-,minus).
op_replacement(*,mult).
op_replacement(<,lessThan).
op_replacement(>,greaterThan).
*/

compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body):- op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).


compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- \+ atom(FunctionName),!,
  must_compile_body(Ctx,Env,Result,[funcall,FunctionName | FunctionArgs],Body).

compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- FunctionName \==funcall,
  member(bv(Atom0,_),Ctx.argbindings),Atom0==FunctionName,!,
  must_compile_body(Ctx,Env,Result,[invoke_tl,FunctionName | FunctionArgs],Body).

% Operator
compile_body(_Ctx,_Env,Result,[FunctionName | FunctionArgs], Body):- lisp_operator(FunctionName),!,
   append(FunctionArgs, [Result], ArgsPlusResult),
   debug_var([FunctionName,'_Ret'],Result),
   ExpandedFunction =.. [ FunctionName | ArgsPlusResult],
   Body = (ExpandedFunction).

% Function
compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- symbol_info(FunctionName,_,function_type,_O),!,
      expand_arguments(Ctx,Env,FunctionName,0, FunctionArgs,ArgBody, Args),
      append(Args, [Result], ArgsPlusResult),
      debug_var([FunctionName,'_Ret'],Result),      
   ExpandedFunction =.. [ FunctionName | ArgsPlusResult],
   Body = (ArgBody,ExpandedFunction).

% Non built-in function expands into an explicit function call
compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):-
    %  (FunctionArgs==[] -> (dumpST,break ); true),
      !,
      expand_arguments(Ctx,Env,FunctionName,0, FunctionArgs,ArgBody, Args),
      append(Args, [Result], ArgsPlusResult),
      debug_var([FunctionName,'_Ret'],Result),      
      find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).
   

find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction):-
    length(ArgsPlusResult,Len),
    (some_function_or_macro(FunctionName,Len,['','cl_','pf_','sf_','mf_'],NewName) 
      -> ExpandedFunction =.. [NewName | ArgsPlusResult];
    ( ExpandedFunction =.. [ FunctionName | ArgsPlusResult])).

some_function_or_macro(FunctionName,Len,[Name|NameS],NewName):-
   atom_concat(Name,FunctionName,ProposedPName),   
   (((ProposedPName = ProposedName; prologcase_name(ProposedPName,ProposedName)),
    functor(P,ProposedName,Len),current_predicate(_,P))-> ProposedName=NewName;
   some_function_or_macro(FunctionName,Len,NameS,NewName)).

 
must_compile_progn(Ctx,Env,Result,Forms, PreviousResult, Body):-
   must_or_rtrace(compile_progn(Ctx,Env,Result,Forms, PreviousResult,Body)).

compile_progn(_Cx,_Ev,Result,[], PreviousResult,true):- PreviousResult = Result.
compile_progn(Ctx,Env,Result,[Form | Forms], _PreviousResult, Body):-  !,
	must_compile_body(Ctx,Env,FormResult, Form,FormBody),
	must_compile_progn(Ctx,Env,Result, Forms, FormResult, FormSBody),
        Body = (FormBody,FormSBody).
compile_progn(Ctx,Env,Result, Form , _PreviousResult, Body):-
	must_compile_body(Ctx,Env,Result,Form, Body).


:- set_prolog_flag(double_quotes,string).


tst:is_local_test("
(defun sum_with_map (xs)
  (let (( running_total 0))
    (let ((summer 
            (function 
               (lambda (n)
                (setq running_total (+ running_total n))))))
       (mapcar summer  xs) running_total)))
 "
  ).

tst:is_local_test("(defun accumulate (op seq &optional (init 0)) (if (null seq) init (funcall op (car seq) (accumulate op (cdr seq) init))))").


:- fixup_exports.
