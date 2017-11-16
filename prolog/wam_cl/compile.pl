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


%new_compile_ctx(Ctx):- new_assoc(Ctx)put_attr(Ctx,type,ctx).
new_compile_ctx(Ctx):- list_to_rbtree([type-ctx],Ctx0),put_attr(Ctx,tracker,Ctx0).

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
  must_or_rtrace(call(Code)),!.


lisp_compile(SExpression):-
  notrace(as_sexp(SExpression,Expression)),
  dbmsg(lisp_compiled_eval(Expression)),
  lisp_compile(Expression,Code),!,
  dbmsg(Code).

lisp_compile(Expression,Body):-
   debug_var('_Ignored',Result),
   lisp_compile(Result,Expression,Body).

lisp_compile(Result,SExpression,Body):-
   env_toplevel(Env),
   lisp_compile(Env,Result,SExpression,Body).

lisp_compile(Env,Result,Expression,Body):-
   new_compile_ctx(Ctx),
   must_or_rtrace(lisp_compile(Ctx,Env,Result,Expression,Body)).

lisp_compile(Ctx,Env,Result,SExpression,Body):-
   notrace(as_sexp(SExpression,Expression)),
   must_or_rtrace(compile_forms(Ctx,Env,Result,[Expression],Body)).


compile_forms(Ctx,Env,Result,FunctionBody,Code):-
   must_compile_progn(Ctx,Env,Result,FunctionBody, [], Body),!,
   body_cleanup(Ctx,Body,Code).

:- nop( debug_var('FirstForm',Var)),
   nb_linkval('$compiler_PreviousResult',the(Var)).

% compile_body(Ctx,Env,Result,Function, Body).
% Expands a Lisp-like function body into its Prolog equivalent


must_compile_body(Ctx,Env,Result,Function, Body):-
  maybe_debug_var('_rCtx',Ctx),
  maybe_debug_var('_rEnv',Env),
  maybe_debug_var('_rResult',Result),
  maybe_debug_var('_rForms',Function),
  maybe_debug_var('_rBody',Body),
  must_or_rtrace(compile_body(Ctx,Env,Result,Function, Body)),
  % nb_current('$compiler_PreviousResult',THE),setarg(1,THE,Result),
  !.


if_must_compile_body(Ctx,Env,Result,Function, Body):-
  must_or_rtrace(compile_body(Ctx,Env,Result,Function, Body)).

% PROG
/*(defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/
:- dynamic(compiler_macro_left_right/3).
:- discontiguous(compiler_macro_left_right/3).
compiler_macro_left_right(prog,[Vars|TagBody], [block,[],[let,Vars,[tagbody|TagBody]]]) :- trace.

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
  must_compile_body(Ctx,Env,Result,[eval,Var], Code).

% Lazy Reader
compile_body(Ctx,Env,Result, 's'(Str),  Body):-
  parse_sexpr_untyped(string(Str),Expression),!,
  must_compile_body(Ctx,Env,Result, Expression,  Body).

% Compiler Plugin
compile_body(Ctx,Env,Result,InstrS,Code):-
  shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code),!.

% PROGN
compile_body(Ctx,Env,Result,[progn|Forms], Body):- !, must_compile_progn(Ctx,Env,Result,Forms,[],Body).

% SOURCE TRANSFORMATIONS
compile_body(Ctx,Env,Result,[M|MACROLEFT], Code):- atom(M),
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

% DEFSETF (short form)
%compile_body(Ctx,Env,Symbol,[defsetf,AccessFun,UpdateFn],assert(defsetf_short(AccessFun,UpdateFn))).
%compile_body(Ctx,Env,Symbol,[defsetf,AccessFun,FormalParms,Variables|FunctionBody0],assert(defsetf_short(AccessFun,UpdateFn)))

is_def_at_least_two_args(defgeneric).
is_def_at_least_two_args(define_compiler_macro).
is_def_at_least_two_args(define_method_combination).
is_def_at_least_two_args(define_setf_expander).
is_def_at_least_two_args(define_setf_expander).
is_def_at_least_two_args(defmethod).
is_def_at_least_two_args(defsetf).
is_def_at_least_two_args(deftype).
is_def_at_least_two_args(flet).
is_def_at_least_two_args(labels).
is_def_at_least_two_args(macrolet).
is_def_at_least_two_args(symbol_macrolet).

%compile_body(_Ctx,_Env,Symbol,[Fun,Symbol,A2|AMORE],assert(P)):- is_def_at_least_two_args(Fun),!,P=..[Fun,Symbol,A2,AMORE].
%compile_body(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert(P)):- is_def_at_least_two_args(Fun),same_symbol(Fun,Fun0),!,P=..[Fun,Symbol,A2,AMORE].

% handler-caserestart-casedestructuring-bind


% DEFUN
compile_body(Ctx,Env,Symbol,[defun,Name,FormalParms|FunctionBody0], CompileBody):-
    combine_setfs(Name,Symbol),
    must_or_rtrace(find_function_or_macro_name(Symbol,_Len, Function)),
    must_or_rtrace(maybe_get_docs(function,Function,FunctionBody0,FunctionBody,DocCode)),
    FunctionHead=[Function|FormalParms],
    debug_var('Setf-symbol-function',SetfR),
    SETFUNCTION = place_op(Env,setf, [symbol_function,Symbol], [Function],  SetfR),
    %compile_body(Ctx,Env,SetfR,[setf,[symbol_function,[quote,Symbol]],Function],SETFUNCTION),
    CompileBody = (DocCode,
   HeadDefCode,
   asserta(user:function_lambda(defun(Name),Function, FormalParms, FunctionBody)),   
   FunctionAssert,
   add_opv(Function,typeof,function),
   add_opv(Function,classof,claz_compiled_function),
   SETFUNCTION),!,
    make_compiled(Ctx,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode),
    (local_override(with_forms,lisp_grovel) -> FunctionAssert = true; FunctionAssert = asserta((Head  :- (  BodyCode)))).


show_ctx_info(Ctx):- term_attvars(Ctx,CtxVars),maplist(del_attr_rev2(freeze),CtxVars),show_ctx_info2(Ctx).
show_ctx_info2(Ctx):- ignore((get_attr(Ctx,tracker,Ctx0),show_ctx_info3(Ctx0))).
show_ctx_info3(Ctx):- is_rbtree(Ctx),!,forall(rb_in(Key, Value, Ctx),wdmsg(Key=Value)).
show_ctx_info3(Ctx):- wdmsg(ctx=Ctx).

make_compiled(Ctx,FunctionHead,FunctionBody,Head,HeadDefCode,(BodyCode)):-
    expand_function_head(Ctx,CallEnv,FunctionHead, Head, HeadEnv, Result,HeadDefCode,HeadCode),
    debug_var("RET",Result),debug_var("Env",CallEnv),
    if_must_compile_body(Ctx,CallEnv,Result,[progn|FunctionBody],Body0),
    show_ctx_info(Ctx),
    body_cleanup(Ctx,((CallEnv=HeadEnv,HeadCode,Body0)),BodyCode).

% same_symbol(OP1,OP2):-!, OP1=OP2.
same_symbol(OP1,OP2):- var(OP1),var(OP2),trace_or_throw(same_symbol(OP1,OP2)).
same_symbol(OP1,OP2):- var(OP2),atom(OP1),!,same_symbol(OP2,OP1).
same_symbol(OP2,OP1):- var(OP2),atom(OP1),!,prologcase_name(OP1,OP3),!,freeze(OP2,((atom(OP2),same_symbol(OP2,OP3)))).
same_symbol(OP1,OP2):-
  (atom(OP1),atom(OP2),(
   OP1==OP2 -> true;  
   prologcase_name(OP1,N1),
   (OP2==N1 -> true ;
   (prologcase_name(OP2,N2),
     (OP1==N2 -> true ; N1==N2))))).

% #+
compile_body(Ctx,Env,Result,[OP,Flag,Form], Code):- same_symbol(OP,'#+'),!, same_symbol('*features*',FEATURES), symbol_value(Env,FEATURES,List),
   (member(Flag,List) -> must_compile_body(Ctx,Env,Result,Form, Code) ; Code = true).
% #-
compile_body(Ctx,Env,Result,[OP,Flag,Form], Code):- same_symbol(OP,'#-'),!, same_symbol('*features*',FEATURES), symbol_value(Env,FEATURES,List),
( \+ member(Flag,List) -> must_compile_body(Ctx,Env,Result,Form, Code) ; Code = true).
% EVAL-WHEN
compile_body(Ctx,Env,Result,[OP,Flags|Forms], Code):-  same_symbol(OP,'eval-when'), !,
 ((member(X,Flags),is_when(X) )
  -> must_compile_body(Ctx,Env,Result,[progn,Forms], Code) ; Code = true).
% DEFMACRO
compile_body(Ctx,_Env,Name,[defmacro,Name0,FormalParmsB|FunctionBody0], CompileBody):-
    combine_setfs(Name0,Name1),
    must_or_rtrace(find_function_or_macro_name(Name1,_Len, Name)),
    add_alphas(Ctx,Name),
    must_or_rtrace(maybe_get_docs(function,Name,FunctionBody0,FunctionBodyB,DocCode)),
    %reader_intern_symbols
    =([FormalParmsB,FunctionBodyB],[FormalParms,FunctionBody]),
    FunctionHead=[Name|FormalParms],
    CompileBody = (% asserta((Head  :- (fail, <<==(FunctionHead , FunctionBody)))),
   DocCode,
   HeadDefCode,
   retractall(user:macro_lambda(defmacro(Name0),Name,_,_,_)),
   asserta(user:macro_lambda(defmacro(Name0),Name, FormalParms, [progn | FunctionBody],Alphas)),
   add_opv(Name,typeof,sys_macro),
   add_opv(Name,classof,operator),
   nop(FunctionAssert)),!,

    make_compiled(Ctx,FunctionHead,FunctionBody,Head,HeadDefCode,BodyCode),
    (local_override(with_forms,lisp_grovel) -> FunctionAssert = true; FunctionAssert = asserta((Head  :- (!,  BodyCode)))),
    get_alphas(Ctx,Alphas),
    wdmsg( :- lisp_compile( [defmacro,Name0, FormalParms, [progn | FunctionBody]])),
    wdmsg(((Head  :- (!,  BodyCode)))),
    wdmsg(alphas=Alphas),
    nl,nl.

get_value_or_default(Ctx,Name,Value,IfMissing):- oo_get_attr(Ctx,Name,Value)->true;Value=IfMissing.

get_alphas(Ctx,Alphas):- get_attr(Ctx,tracker,Ctx0),get_alphas0(Ctx0,Alphas).
get_alphas0(Ctx,Alphas):- get_value_or_default(Ctx,alphas,Alphas,[]).

add_alphas(Ctx,Alphas):- must_or_rtrace((get_attr(Ctx,tracker,Ctx0),add_alphas0(Ctx0,Alphas))).
add_alphas0(Ctx,Alpha):- atom(Alpha),!,get_value_or_default(Ctx,alphas,Alphas,[]),oo_put_attr(Ctx,alphas,[Alpha|Alphas]).
add_alphas0(_Ctx,Alphas):- \+ compound(Alphas),!.
add_alphas0(Ctx,Alphas):- Alphas=..[_|ARGS],maplist(add_alphas0(Ctx),ARGS).
/*
compile_body(_Cx,OuterEnv,Name,[defmacro,Name0,FormalParms|Body0], CompileBody):- !,
   combine_setfs(Name0,Name1),
   must_or_rtrace(find_function_or_macro_name(Name1,_Len, Name)),
%      expand_function_head(Ctx,CallEnv,FunctionHead, Head, HeadEnv, Result,HeadDefCode,HeadCode),
      maybe_get_docs(function,Name,Body0,Body,DocCode),
      find_alphas(Ctx,OuterEnv,[defmacro,Name,FormalParms|Body],Alphas),
      CompileBody =(DocCode,retractall(user:macro_lambda(defmacro,Name,_,_,_)),
	asserta(user:macro_lambda(defmacro,Name, FormalParms, [progn | Body], Alphas))).
*/

% EVAL
compile_body(Ctx,Env,Result,['eval',Form1],(Body1,cl_eval(Result1,Result))):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1).



is_when(X):- dbmsg(warn(free_pass(is_when(X)))).

combine_setfs(Name0,Name):-atom(Name0),Name0=Name.
combine_setfs(Name0,Name):-atomic_list_concat(['f_combined'|Name0],'__',Name).


% DOLIST
compile_body(Ctx,Env,Result,['dolist',[Var,List]|FormS], Code):- !,
    must_compile_body(Ctx,Env,ResultL,List,ListBody),
    must_compile_body(Ctx,Env,Result,[progn,FormS], Body),
    Code = (ListBody,
        forall(member(X,ResultL),
      lexically_bind(Var,X,Body))).


must_compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody):-
  must_or_rtrace(compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody)).

% IF (null ...)
compile_test_body(Ctx,Env,TestResult,[null,Test],TestBody,TestResultBody):-
   debug_var("TestNullResult",TestResult),
   must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
   TestResultBody = (TestResult == []).

compile_test_body(Ctx,Env,TestResult,[=,V1,V2],(TestBody1,TestBody2),TestResultBody):-
   debug_var("TestEqualResult",TestResult),
   must_compile_body(Ctx,Env,TestResult1,V1,  TestBody1),
   must_compile_body(Ctx,Env,TestResult2,V2,  TestBody2),
   TestResultBody = (TestResult1 =:= TestResult2).


% IF TEST
compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody):-
   debug_var("GTestResult",TestResult),
   must_compile_body(Ctx,Env,TestResult,Test,  TestBody),
   TestResultBody = (TestResult \== []).



% IF-3
compile_body(Ctx,Env,Result,[if, Test, IfTrue, IfFalse], Body):-
	!,
   debug_var("IFTEST",TestResult),
   debug_var("TrueResult",TrueResult),
   debug_var("FalseResult",FalseResult),

   compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody),
   must_compile_body(Ctx,Env,TrueResult,IfTrue, TrueBody),
   must_compile_body(Ctx,Env,FalseResult,IfFalse, FalseBody),

        Body = (	TestBody,
			( TestResultBody
				-> 	TrueBody,
					Result      = TrueResult
				;  	FalseBody,
					Result      = FalseResult	) ).

% COND
compile_body(_Cx,_Ev,[],[cond ], true):- !.
compile_body(_Cx,_Ev,[],[cond ,[]], true):- !.
compile_body(Ctx,Env,Result,[cond, List |Clauses], Body):- 
  must_or_rtrace((
        [Test|ResultForms] = List,
        debug_var("CONDTESTA",TestResult),
        debug_var("ResultFormsResult",ResultFormsResult),
        debug_var("ClausesResult",ClausesResult),
        debug_var("CondAResult",Result))),
   Result  = ClausesResult,
   %Result  = ResultFormsResult,
   freeze(Result,var(Result)),
   must_compile_test_body(Ctx,Env,TestResult,Test,TestBody,TestResultBody),
   must_compile_progn(Ctx,Env,ResultFormsResult,ResultForms, TestResult, ResultFormsBody),
   must_compile_body(Ctx,Env,ClausesResult,[cond| Clauses],  ClausesBody),
   Body = (	 
                   ((TestBody, TestResultBody) -> ( ResultFormsBody,Result  = ResultFormsResult); ClausesBody)),!.
   


compile_body(Ctx,Env,Result,[cond, List |Clauses], Body):- !,
  must_or_rtrace((
        [Test|ResultForms] = List,
        debug_var("CONDTESTB",TestResult),
        debug_var("ResultFormsResult",ResultFormsResult),
        debug_var("ClausesResult",ClausesResult),
        debug_var("CondBResult",Result))),
	must_compile_body(Ctx,Env,TestResult,Test,TestBody),
	must_compile_progn(Ctx,Env,ResultFormsResult,ResultForms, TestResult, ResultFormsBody),
	must_compile_body(Ctx,Env,ClausesResult,[cond| Clauses],  ClausesBody),
	Body = (	 
			(( (TestBody, TestResult \==[])
				->(	ResultFormsBody,
					Result      = ResultFormsResult)
				;	(ClausesBody,
					Result      = ClausesResult )	))).



% CONS inine
compile_body(Ctx,Env,Result,[cons, IN1,IN2], Body):- \+ current_prolog_flag(lisp_inline,false),
	!,
        must_compile_body(Ctx,Env,MID1,IN1,  ValueBody1),
        must_compile_body(Ctx,Env,MID2,IN2,  ValueBody2),
        Body = (ValueBody1,ValueBody2,Result=[MID1|MID2]).


p_or_s([F|Args],F0,Args0):-!,F0=F,Args0=Args.
p_or_s(POrSTerm,F,Args):- POrSTerm=..[F|Args].

% (function (lambda ... ))
compile_body(Ctx,Env,Result,POrSTerm, Body):- p_or_s(POrSTerm,function,[lambda,LambdaArgs| LambdaBody]),
	!,

	must_compile_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
        debug_var('LArgs',LambdaArgs),
        debug_var('LResult',ClosureResult),
        debug_var('LEnv',ClosureEnvironment),
     Result = closure(LambdaArgs,
			[ClosureEnvironment, ClosureResult]^ClosureBody,
			Env),
	Body = true.

% (function ?? )
compile_body(_Cx,_Ev,function(Function),POrSTerm, true):- p_or_s(POrSTerm,function,[Function]).

% ((closure ...)..)
compile_body(Ctx,Env,ClosureResult,[POrSTerm|ActualParams],Code):- 
        p_or_s(POrSTerm,closure,[LambdaArgs,[ClosureEnvironment, ClosureResult]^ClosureBody,AltEnv]),
	!,
	bind_formal_parameters(LambdaArgs, ActualParams, [ClosureEnvironment,AltEnv], AltEnvBindings,BindCode),
        must_or_rtrace(call(BindCode)),
	must_compile_body(Ctx,[AltEnvBindings|Env],ClosureResult,ClosureBody, Code).
	

% (lambda ...)
compile_body(Ctx,Env,Result,[lambda,LambdaArgs|LambdaBody], Body):-
	!,
	must_compile_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
        debug_var('LArgs',LambdaArgs),
        debug_var('LResult',ClosureResult),
        debug_var('LEnv',ClosureEnvironment),
     Result = closure(LambdaArgs,
			[ClosureEnvironment, ClosureResult]^ClosureBody,
			Env),
	Body = true.





% PROGV    % ( progv ' ( a ) ` ( , ( + 1 1 ) ) a ) => 2
compile_body(Ctx,Env,Result,[progv,VarsForm,ValuesForm|FormS],Code):- !,
   must_compile_body(Ctx,Env,VarsRs,VarsForm,Body1),
   must_compile_body(Ctx,Env,ValuesRs,ValuesForm,Body2),
   must_compile_progn(Ctx,Env,Result,FormS,BodyS),
   Code = (Body1, Body2 , maplist(bind_dynamic_value(Env),VarsRs,ValuesRs), BodyS).

normalize_let([],[]).
normalize_let([Decl|NewBindingsIn],[Norm|NewBindings]):-
  must_or_rtrace(normalize_let1(Decl,Norm)),!,
  normalize_let(NewBindingsIn,NewBindings).


normalize_let1([bind, Variable, Form],[bind, Variable, Form]).
normalize_let1([Variable, Form],[bind, Variable, Form]).
normalize_let1( Variable,[bind, Variable, []]).

compile_body(_Ctx,_Env,_Result,[OP|R], _Body):- var(OP),!,trace_or_throw(c_b([OP|R])).

% LET
compile_body(Ctx,Env,Result,[OP, NewBindingsIn| BodyForms], Body):- (var(OP)-> throw(var(OP)) ; OP==let),!,
 must_or_rtrace(is_list(NewBindingsIn)),!,
 must_or_rtrace(compile_let(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body)).


compile_let(Ctx,Env,Result,[let, []| BodyForms], Body):- !, must_compile_progn(Ctx,Env,Result, BodyForms, [], Body).
compile_let(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body):- !,
     must_or_rtrace(normalize_let(NewBindingsIn,NewBindings)),!,
	zip_with(Variables, ValueForms, [Variable, Form, [bind, Variable, Form]]^true, NewBindings),
	must_or_rtrace(expand_arguments(Ctx,Env,'funcall',1,ValueForms, ValueBody, Values)),
        freeze(Var,ignore((var(Val),debug_var('_Init',Var,Val)))),
        freeze(Var,ignore(((var(Val),add_tracked_var(Ctx,Var,Val))))),
        zip_with(Variables, Values, [Var, Val, bv(Var,Val)]^true,Bindings),
        add_alphas(Ctx,Variables),
        debug_var("LETENV",BindingsEnvironment),
        ignore((member(VarN,[Variable,Var]),atom(VarN),var(Val),debug_var([VarN,'_Let'],Val))),        
	must_compile_progn(Ctx,BindingsEnvironment,Result,BodyForms, [], BodyFormsBody),
         Body = ( ValueBody,BindingsEnvironment=[Bindings|Env], BodyFormsBody ).

% LET*
compile_body(Ctx,Env,Result,[OP, []| BodyForms], Body):- same_symbol(OP,'let*'), !, must_compile_body(Ctx,Env,Result,[progn| BodyForms], Body).
compile_body(Ctx,Env,Result,[OP, [Binding1|NewBindings]| BodyForms], Body):- same_symbol(OP,'let*'),
   must_or_rtrace(compile_let(Ctx,Env,Result,['let', [Binding1],[progn, [OP, NewBindings| BodyForms]]], Body)).

% VALUES (r1 . rest )
compile_body(Ctx,Env,Result,['values',R1|EvalList], (ArgBody,Body)):-!,
    expand_arguments(Ctx,Env,funcall,0,[R1|EvalList], ArgBody, [Result|Results]),
    Body = nb_setval('$mv_return',[Result|Results]).
compile_body(_Ctx,_Env,[],['values'], nb_setval('$mv_return',[])):-!.

:- nb_setval('$mv_return',[]).

% Macro MULTIPLE-VALUE-BIND
compile_body(Ctx,Env,Result,[OP,Vars,Eval1|ProgN], Body):- same_symbol(OP,'multiple-value-bind'),
  must_compile_body(Ctx,Env,Result,[let,Vars,[progn,Eval1,['setqvalues',Vars]|ProgN]],Body).

% Macro MULTIPLE-VALUE-LIST
compile_body(Ctx,Env,Result,[OP,Eval1], (Body,nb_current('$mv_return',Result))):-
  same_symbol(OP,'multiple-value-list'),
  debug_var('MV_RETURN',Result),
  debug_var('IgnoredRet',IResult),
  must_compile_body(Ctx,Env,IResult,Eval1,Body).

% Macro MULTIPLE-VALUE-CALL
compile_body(Ctx,Env,Result,[OP,Function|Progn], Body):-
  same_symbol(OP,'multiple-value-call'),
  must_compile_body(Ctx,Env,Result,[progn,[progn|Progn],['apply',Function,['returnvalues']]],Body).

% synthetic RETURN-VALUES -> values
compile_body(_Ctx,_Env,Values,['returnvalues'], nb_current('$mv_return',Values)).

% synthetic SETQ-VALUES (vars*)
compile_body(_Ctx,Env,[],['setqvalues',Vars], setq_values(Env,Vars)):-!.
setq_values(Env,Vars):- nb_current('$mv_return',Values),setq_values(Env,Vars,Values).
setq_values(_Env,_,[]):-!.
setq_values(_Env,[],_):-!.
setq_values(Env,[Var|Vars],[Val|Values]):-
   set_symbol_value(Env,Var,Val),
   setq_values(Env,Vars,Values).

set_symbol_value(Var,Val):-
  current_env(Env),
  set_symbol_value(Env,Var,Val).

%   zip_with(Xs, Ys, Pred, Zs)
%   is true if Pred(X, Y, Z) is true for all X, Y, Z.

zip_with([], [], _, []).
zip_with([X|Xs], [Y|Ys], Pred, [Z|Zs]):-
	lpa_apply(Pred, [X, Y, Z]),
	zip_with(Xs, Ys, Pred, Zs).


compile_body(Ctx,Env,Result,BodyForms, Body):- atom(BodyForms),!,
   must_or_rtrace(compile_assigns(Ctx,Env,Result,BodyForms, Body)),!.

% SETQ - PSET
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_assigns(Ctx,Env,Result,BodyForms, Body),!.

% SOURCE GROVEL MODE
%compile_body(_Ctx,_Env,[],_, true):- local_override(with_forms,lisp_grovel),!.

compile_body(Ctx,Env,Result,BodyForms, Body):- 
  must_or_rtrace(compile_funop(Ctx,Env,Result,BodyForms, Body)),!.

/*must_compile_progn(Ctx,Env,Result,Forms, Body):-
   % local_override('$compiler_PreviousResult',the(PreviousResult)),
   PreviousResult = [],
   must_compile_progn(Ctx,Env,Result,Forms, PreviousResult,Body).
*/

must_compile_progn(Ctx,Env,Result,Forms, PreviousResult, Body):-
  maybe_debug_var('_rCtx',Ctx),
  maybe_debug_var('_rEnv',Env),
  maybe_debug_var('_rResult',Result),
  maybe_debug_var('_rPrevRes',PreviousResult),
  maybe_debug_var('_rForms',Forms),
  maybe_debug_var('_rBody',Body),
   must_or_rtrace(compile_progn(Ctx,Env,Result,Forms, PreviousResult,Body)).

compile_progn(_Cx,_Ev,Result,Var,_PreviousResult,cl_eval([progn|Var],Result)):- is_ftVar(Var),!.
compile_progn(_Cx,_Ev,Result,[], PreviousResult,true):-!, PreviousResult = Result.
compile_progn(Ctx,Env,Result,[Form | Forms], _PreviousResult, Body):-  !,
   %locally(
     %local_override('$compiler_PreviousResult',the(PreviousResult)),
	must_compile_body(Ctx,Env,FormResult, Form,FormBody), %),
	must_compile_progn(Ctx,Env,Result, Forms, FormResult, FormSBody),
        Body = (FormBody,FormSBody).
compile_progn(Ctx,Env,Result, Form , _PreviousResult, Body):-
        % locally(
  % local_override('$compiler_PreviousResult',the(PreviousResult)),
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
