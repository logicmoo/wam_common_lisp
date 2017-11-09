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

lisp_error_description(unbound_atom,        100, 'No value found for atom: ').
lisp_error_description(atom_does_not_exist, 101, 'SetQ: Variable does not exist: ').
lisp_error_description(first_not_cons,      102, 'Form1: This is not a cons cell: ').
lisp_error_description(rest_not_cons,       103, 'Rest: This is not a cons cell: ').


lisp_compiled_eval(SExpression):- 
  notrace(as_sexp(SExpression,Expression)),
  lisp_compiled_eval(Expression,Result),
  dbmsg(result(Result)).
                                
lisp_compiled_eval(SExpression,Result):-
  notrace(as_sexp(SExpression,Expression)),
  dbmsg(lisp_compile(Expression)),
  lisp_compile(Result,Expression,Code),
  dbmsg(Code),
  call(Code),!.


lisp_compile(SExpression):-
  notrace(as_sexp(SExpression,Expression)),
  dbmsg(lisp_compiled_eval(Expression)),
  lisp_compile(Expression,Code),!,
  dbmsg(Code).

lisp_compile(Expression,Body):-
   debug_var('_Ignored',Result),
   lisp_compile(Result,Expression,Body).

lisp_compile(Result,SExpression,Body):- 
   lisp_compile(toplevel,Result,SExpression,Body).

lisp_compile(Env,Result,Expression,Body):- 
   lisp_compile(ctx([]),Env,Result,Expression,Body).

lisp_compile(Ctx,Env,Result,SExpression,Body):- 
   notrace(as_sexp(SExpression,Expression)),
   compile_forms(Ctx,Env,Result,[Expression],Body).


compile_forms(Ctx,Env,Result,FunctionBody,Code):-
   must_compile_progn(Ctx,Env,Result,FunctionBody,[], Body),!,
   body_cleanup(Body,Code).


% compile_body(Ctx,Env,Result,Function, Body).
% Expands a Lisp-like function body into its Prolog equivalent


must_compile_body(Ctx,Env,Result,Function, Body):-
  must_or_rtrace(compile_body(Ctx,Env,Result,Function, Body)).

if_must_compile_body(Ctx,Env,Result,Function, Body):-
  local_override(with_forms,lisp_grovel)-> Body= nop(lisp_groveling(Function,Result));
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

% Prolog vars
compile_body(_Ctx,_Env,Result,Var, true):- is_ftVar(Var), !, Result = Var.
compile_body(Ctx,Env,Result,Var, Code):- is_ftVar(Var), !, % NEVER SEEN
  debug_var("EVAL",Var),
  compile_body(Ctx,Env,Result,[eval,Var], Code).

% Lazy Reader
compile_body(Ctx,Env,Result, 's'(Str),  Body):-
  parse_sexpr_untyped(string(Str),Expression),!,
  must_compile_body(Ctx,Env,Result, Expression,  Body).
 
% Compiler Plugin 
compile_body(Ctx,Env,Result,InstrS,Code):-
  shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code),!.

% PROGN
compile_body(Ctx,Env,Result,[progn|Forms], Body):- !, must_compile_progn(Ctx,Env,Result,Forms, [],Body).

% SOURCE TRANSFORMATIONS
compile_body(Ctx,Env,Result,[M|MACROLEFT], Code):- 
  term_variables([M|MACROLEFT],VarsS),
  compiler_macro_left_right(M,MACROLEFT,MACRORIGHT),
  term_variables(MACRORIGHT,VarsE),
  VarsE==VarsS,!,
  must_compile_body(Ctx,Env,Result,MACRORIGHT, Code).

% SELF EVALUATING OBJECTS
compile_body(_Cx,_Ev, [],[],true):- !.
compile_body(_Cx,_Ev, [],nil,true):- !.
compile_body(_Cx,_Ev,SelfEval,SelfEval,true):- notrace(is_self_evaluationing_object(SelfEval)),!.

% numbers
compile_body(_Ctx,_Env,Value,Atom,true):- atom(Atom),atom_number_exta(Atom,Value),!.

atom_number_exta(Atom,Value):- atom_number(Atom,Value).
atom_number_exta(Atom,Value):- atom_concat('-.',R,Atom),atom_concat('-0.',R,NAtom),!,atom_number(NAtom,Value).
atom_number_exta(Atom,Value):- atom_concat('.',R,Atom),atom_concat('0.',R,NAtom),!,atom_number(NAtom,Value).


% symbols
compile_body(Ctx,Env,Value,Atom, Body):- atom(Atom),!, 
  must_or_rtrace(compile_symbol_getter(Ctx,Env,Value, Atom, Body)).

% QUOTE 
compile_body(_Cx,_Ev,Item,[quote, Item],  true):- !.

% COMMENTS
is_comment([COMMENT|_]):- atom(COMMENT),!,atom_concat('$COMMENT',_,COMMENT).
is_comment(COMMENTP):- compound(COMMENTP),!,COMMENTP=..[COMMENT|_],!,atom_concat('$COMMENT',_,COMMENT).
compile_body(_Ctx,_Env,[],COMMENT,true):- is_comment(COMMENT),!.


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

% `, Backquoted commas
compile_body(_Cx,Env,Result,['$BQ',Form], Code):-!,compile_bq(Env,Result,Form,Code).
compile_body(_Cx,Env,Result,['`',Form], Code):-!,compile_bq(Env,Result,Form,Code).

% DEFUN
compile_body(Ctx,_Env,Name,[defun,Name0,FormalParms|FunctionBody0], CompileBody):- 
    combine_setfs(Name0,Name),
    must_or_rtrace(maybe_get_docs(function,Name,FunctionBody0,FunctionBody,DocCode)),
    FunctionHead=[Name|FormalParms],
    CompileBody = (% asserta((Head  :- (fail, <<==(FunctionHead , FunctionBody)))),
                   DocCode,
                   HeadDefCode,
                   asserta(user:function_lambda(defun,Name, FormalParms, FunctionBody)),
                   FunctionAssert),!,
    make_compiled(Ctx,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode),
    (local_override(with_forms,lisp_grovel) -> FunctionAssert = true; FunctionAssert = asserta((Head  :- (!,  BodyCode)))).

make_compiled(Ctx,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode):-
    expand_function_head(Ctx,CallEnv,FunctionHead, Head, HeadEnv, Result,HeadDefCode,HeadCode),
    debug_var("RET",Result),debug_var("Env",CallEnv),
    if_must_compile_body(Ctx,CallEnv,Result,[progn|FunctionBody],Body0),
    body_cleanup(((CallEnv=HeadEnv,HeadCode,Body0)),BodyCode).


% #+
compile_body(Ctx,Env,Result,['#+',Flag,Form], Code):- !, symbol_value(Env,'*features*',List),
   (member(Flag,List) -> must_compile_body(Ctx,Env,Result,Form, Code) ; Code = true).
% #-
compile_body(Ctx,Env,Result,['#-',Flag,Form], Code):- !, symbol_value(Env,'*features*',List),
( \+ member(Flag,List) -> must_compile_body(Ctx,Env,Result,Form, Code) ; Code = true).  
% EVAL-WHEN
compile_body(Ctx,Env,Result,['eval-when',Flags|Forms], Code):- !, 
 ((member(X,Flags),is_when(X) )
  -> must_compile_body(Ctx,Env,Result,[progn,Forms], Code) ; Code = true).
% DEFMACRO
compile_body(_Cx,_Ev,Name,[defmacro,Name,FormalParms|Body0], CompileBody):- !,
      maybe_get_docs(function,Name,Body0,Body,DocCode),
      CompileBody =(DocCode,retractall(user:macro_lambda(defmacro,Name,_,_)),
	asserta(user:macro_lambda(defmacro,Name, FormalParms, Body))).


% EVAL 
compile_body(Ctx,Env,Result,['eval',Form1],(Body1,cl_eval(Result1,Result))):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1).



is_when(X):- dbmsg(warn(free_pass(is_when(X)))).

combine_setfs(Name0,Name):-atom(Name0),Name0=Name.
combine_setfs(Name0,Name):-atomic_list_concat(Name0,-,Name).


% DOLIST
compile_body(Ctx,Env,Result,['dolist',[Var,List]|FormS], Code):- !,
    compile_body(Ctx,Env,ResultL,List,ListBody),
    compile_body(Ctx,Env,Result,[progn,FormS], Body),
    Code = (ListBody,
        forall(member(X,ResultL),
              lexically_bind(Var,X,Body))).

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

	must_compile_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
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
	must_compile_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
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

	must_compile_body(Ctx,BindingsEnvironment,Result,[progn|BodyForms], BodyFormsBody),
         Body = ( ValueBody,BindingsEnvironment=[Bindings|Env], BodyFormsBody ).

% LET*
compile_body(Ctx,Env,Result,['let*', []| BodyForms], Body):- compile_body(Ctx,Env,Result,[progn| BodyForms], Body).
compile_body(Ctx,Env,Result,['let*', [Binding1|NewBindings]| BodyForms], Body):-
   compile_body(Ctx,Env,Result,['let', [Binding1],[progn, ['let*', NewBindings| BodyForms]]], Body).

% VALUES (r1 . rest )
compile_body(Ctx,Env,Result,['values',R1|EvalList], (ArgBody,Body)):-!,
    expand_arguments(Ctx,Env,funcall,0,[R1|EvalList], ArgBody, [Result|Results]),
    Body = nb_setval('$mv_return',[Result|Results]).
compile_body(_Ctx,_Env,[],['values'], nb_setval('$mv_return',[])):-!.

:- nb_setval('$mv_return',[]).

% Macro MULTIPLE-VALUE-BIND
compile_body(Ctx,Env,Result,['multiple-value-bind',Vars,Eval1|ProgN], Body):-
  must_compile_body(Ctx,Env,Result,[let,Vars,[progn,Eval1,['setq-values',Vars]|ProgN]],Body).

% Macro MULTIPLE-VALUE-LIST
compile_body(Ctx,Env,Result,['multiple-value-list',Eval1], (Body,nb_current('$mv_return',Result))):-
  debug_var('MV_RETURN',Result),
  debug_var('IgnoredRet',IResult),
  must_compile_body(Ctx,Env,IResult,Eval1,Body).

% Macro MULTIPLE-VALUE-CALL
compile_body(Ctx,Env,Result,['multiple-value-call',Function|Progn], Body):-
  compile_body(Ctx,Env,Result,[progn,[progn|Progn],['apply',Function,['return-values']]],Body).

% synthetic RETURN-VALUES -> values
compile_body(_Ctx,_Env,Values,['return-values'], nb_current('$mv_return',Values)).

% synthetic SETQ-VALUES (vars*)
compile_body(_Ctx,Env,[],['setq-values',Vars], setq_values(Env,Vars)):-!.
setq_values(Env,Vars):- nb_current('$mv_return',Values),setq_values(Env,Vars,Values).
setq_values(_Env,_,[]):-!.
setq_values(_Env,[],_):-!.
setq_values(Env,[Var|Vars],[Val|Values]):- 
   set_symbol_value(Env,Var,Val),
   setq_values(Env,Vars,Values).



%   zip_with(Xs, Ys, Pred, Zs)
%   is true if Pred(X, Y, Z) is true for all X, Y, Z.

zip_with([], [], _, []).
zip_with([X|Xs], [Y|Ys], Pred, [Z|Zs]):-
	lpa_apply(Pred, [X, Y, Z]),
	zip_with(Xs, Ys, Pred, Zs).


% SETQ - PSET
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_assigns(Ctx,Env,Result,BodyForms, Body),!.

% SOURCE GROVEL MODE
compile_body(_Ctx,_Env,[],_, true):- local_override(with_forms,lisp_grovel),!.

% Use a previous DEFMACRO
compile_body(Cxt,Env,RResult,[Procedure|Arguments],CompileBodyCode):-
  user:macro_lambda(_Scope,Procedure, FormalParams, LambdaExpression),!,
  must_or_rtrace(bind_parameters(NewEnv, FormalParams, Arguments,BindCode)),!,
  append(_,[],NewEnv),!,
  NextEnv = [NewEnv|Env],  
  call(BindCode),
  must_or_rtrace(expand_commas(NewEnv,CommaResult,LambdaExpression,CodeS)),
  body_cleanup_keep_debug_vars(CodeS,Code),
  dbmsg(macro(macroResult(BindCode,Code,CommaResult))),
  (local_override(with_forms,lisp_grovel)-> (dumpST) ; true),
  call(Code),
  compile_body(Cxt,NextEnv,CompileBody0Result,CommaResult, MCBR),
  call(MCBR),
  compile_body(Cxt,NextEnv,RResult,CompileBody0Result, CompileBody),
  CompileBodyCode = (CompileBody).


:- dynamic(op_replacement/2).
/*
op_replacement(+,plus).
op_replacement(-,minus).
op_replacement(*,mult).
op_replacement(<,lessThan).
op_replacement(>,greaterThan).
*/

compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body):- user:op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).


compile_body(Ctx,Env,Result,[FunctionName ], Body):- is_list(FunctionName),!,
  must_compile_body(Ctx,Env,Result,FunctionName,Body).

compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- is_list(FunctionName),!,
  must_compile_body(Ctx,Env,Result,[funcall_list,FunctionName | FunctionArgs],Body).

compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- \+ atom(FunctionName),!,
  trace,must_compile_body(Ctx,Env,Result,[funcall_foo,FunctionName | FunctionArgs],Body).

% Operator
compile_body(_Ctx,_Env,Result,[FunctionName | FunctionArgs], Body):- lisp_operator(FunctionName),!,
   append([FunctionArgs], [Result], ArgsPlusResult),
   debug_var([FunctionName,'_Ret'],Result),
   find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction),
   % current_predicate(_,ExpandedFunction),
   Body = (ExpandedFunction).

uses_exact(FunctionName,ArgInfo):-  user:arglist_info(FunctionName,_,_,ArgInfo),!,ArgInfo.complex ==0 .
compile_body(Ctx,CallEnv,Result,[FunctionName | FunctionArgs], Body):- uses_exact(FunctionName,_ArgInfo),
      !,
      expand_arguments(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args),
      append([Args], [Result], ArgsPlusResult),
      debug_var([FunctionName,'_Ret'],Result),      
      find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).

uses_rest(FunctionName,ArgInfo):-  user:arglist_info(FunctionName,_,_,ArgInfo),!,ArgInfo.complex \==0 .
% Non built-in function expands into an explicit function call
compile_body(Ctx,CallEnv,Result,[FunctionName | FunctionArgs], Body):- !, % uses_rest(FunctionName,_ArgInfo),
      !,
      expand_arguments(Ctx,CallEnv,FunctionName,0,FunctionArgs,ArgBody, Args),
      append([Args], [Result], ArgsPlusResult),
      debug_var([FunctionName,'_Ret'],Result),      
      find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).

/*
% FUNCTION APPLY
compile_body(Ctx,Env,Result,['apply',Function|ARGS], Body):- ...
% FUNCTION FUNCALL
compile_body(Ctx,Env,Result,['funcall',Function|ARGS], Body):- ...
*/
% Non built-in function expands into an explicit function call
compile_body(Ctx,CallEnv,Result,[FunctionName | FunctionArgs], Body):-    
      !,
      expand_arguments(Ctx,CallEnv,FunctionName,0, FunctionArgs,ArgBody, Args),
      append(Args, [Result], ArgsPlusResult),
      debug_var([FunctionName,'_Ret'],Result),      
      find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction),      
      Body = (ArgBody,ExpandedFunction).
   

find_function_or_macro(FunctionName,ArgsPlusResult,ExpandedFunction):-
    length(ArgsPlusResult,Len),
    (some_function_or_macro(FunctionName,Len,['','cl_','pf_','sf_','mf_'],NewName) 
      -> ExpandedFunction =.. [NewName | ArgsPlusResult];
    (sf_package_prefix(Prefix),atom_concat(Prefix,FunctionName,SF),prologcase_name(SF,ProposedName),
    ( ExpandedFunction =.. [ ProposedName | ArgsPlusResult]))).

some_function_or_macro(FunctionName,Len,[Name|NameS],NewName):-
   atom_concat(Name,FunctionName,ProposedPName),   
   (((ProposedPName = ProposedName; prologcase_name(ProposedPName,ProposedName)),
    functor(P,ProposedName,Len),current_predicate(_,P),\+ predicate_property(user:P,imported_from(system)))-> ProposedName=NewName;
   some_function_or_macro(FunctionName,Len,NameS,NewName)).

sf_package_prefix('cl_').
 
must_compile_progn(Ctx,Env,Result,Forms, PreviousResult, Body):-
   must_or_rtrace(compile_progn(Ctx,Env,Result,Forms, PreviousResult,Body)).

compile_progn(_Cx,_Ev,Result,Var,_PreviousResult,cl_eval([progn|Var],Result)):- is_ftVar(Var),!.
compile_progn(_Cx,_Ev,Result,[], PreviousResult,true):-!, PreviousResult = Result.
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
