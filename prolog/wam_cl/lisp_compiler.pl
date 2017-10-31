/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
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

% :- module(lisp_compiler,[lisp_call/2]).
/*******************************************************************
 *
 * Example definitions:
 * second(l) <<== first(rest(l)).
 * list_3(a, b, c) <<== cons(a, cons(b, cons(c, nil))).
 *
 * Example use:
 * ?| - lisp_call([second,[quote, [a,b,c]]], Result).
 * Result = b
 *
 * ?| - second([a,b,c], Result).
 * Result = b
 *
 * ?| - lisp_call([list_3, tom, dick, harry], Result).
 * Result = [tom, dick, harry]
 *
 * ?| - list_3(tom, dick, harry, Result).
 * Result = [tom, dick, harry]
 *
 *******************************************************************/
:- set_module(class(library)).
:- ensure_loaded(lpa_to_swi).

:- style_check.

:- if(exists_source(library(sexpr_reader))).
:- use_module(library(sexpr_reader)).
read_and_parse(Expr):- current_input(In),parse_sexpr_untyped(In, Expr).
:- endif.


:- ensure_loaded(library(higher_order)).
:- ensure_loaded(library(list_utilities)).
:- require([colormsg1/1]).


:- ensure_loaded(library(must_trace)).
:- ensure_loaded(library(logicmoo_util_terms)).
:- ensure_loaded(library(logicmoo_util_common)).

:- dynamic(tst:is_local_test/1).
:- multifile(tst:is_local_test/1).
:- discontiguous(tst:is_local_test/1).
:- dynamic(tst:is_local_test/2).
:- multifile(tst:is_local_test/2).
:- discontiguous(tst:is_local_test/2).
:- dynamic(tst:is_local_test/3).
:- multifile(tst:is_local_test/3).
:- discontiguous(tst:is_local_test/3).


% :- ensure_loaded(builtin_lisp_functions). % Lisp primitives: this directives is at the end of the file
% :- ensure_loaded(lisp_library).	% Functions defined in lisp: this directive is at the end of the file
					% allowing them to be compiled correctly


:- op(1200, xfx, <<== ).	% function definition
:- op(1200,  fx, <<== ).	% functional imperative definition

:- dynamic special_var/2.	% closure environments



% debug_var(_A,_Var):-!.
debug_var(X,Y):- notrace(catch(debug_var0(X,Y),_,fail)) -> true ; rtrace(debug_var0(X,Y)).

p_n_atom(Cmpd,UP):- sub_term(Atom,Cmpd),nonvar(Atom),\+ number(Atom), Atom\==[], catch(p_n_atom0(Atom,UP),_,fail),!.
p_n_atom(Cmpd,UP):- term_to_atom(Cmpd,Atom),p_n_atom0(Atom,UP),!.

filter_var_chars([H|X],Y):- \+ char_type(H,prolog_var_start),!,filter_var_chars0([86,118,H|X],Y).
filter_var_chars(X,Y):- filter_var_chars0(X,Y).

filter_var_chars0([],[]).
filter_var_chars0([45|T],[95|Rest]):-!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],[H|Rest]):-  code_type(H, prolog_identifier_continue),!,filter_var_chars0(T,Rest).
filter_var_chars0([H|T],Rest):- number_codes(H,Codes), filter_var_chars0(T,Mid),append([95, 99|Codes],[95|Mid],Rest).

p_n_atom0(Atom,UP):- atom(Atom),!,name(Atom,[C|Was]),to_upper(C,U),filter_var_chars([U|Was],CS),name(UP,CS).
p_n_atom0(String,UP):- string(String),!,string_to_atom(String,Atom),!,p_n_atom0(Atom,UP).
p_n_atom0([C|S],UP):- !,notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,p_n_atom0(Atom,UP).

debug_var0(_,NonVar):-nonvar(NonVar),!.
debug_var0([C|S],Var):- notrace(catch(atom_codes(Atom,[C|S]),_,fail)),!,debug_var0(Atom,Var).
debug_var0([AtomI|Rest],Var):-!,maplist(p_n_atom,[AtomI|Rest],UPS),atomic_list_concat(UPS,NAME),debug_var0(NAME,Var),!.
debug_var0(Atom,Var):- p_n_atom(Atom,UP),  
  check_varname(UP),
  add_var_to_env_loco(UP,Var),!.

add_var_to_env_loco(UP,Var):- \+ atom_concat('_',_,UP), var(Var),
  get_var_name(Var,Name),atomic(Name),\+ atom_concat('_',_,Name),
  
  atom_concat(UP,Name,New),add_var_to_env(New,Var).
add_var_to_env_loco(UP,Var):-add_var_to_env(UP,Var).

check_varname(UP):- name(UP,[C|_]),(char_type(C,digit)->throw(check_varname(UP));true).


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
lisp_error_description(first_not_cons,      102, 'First: This is not a cons cell: ').
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
	expand_function_head(FunctionHead, Head, ArgBindings, Result),
                    debug_var("RET",Result),
                    debug_var("Env",Env),
	must_compile_body(ctx{head:Head,argbindings:ArgBindings},Env,Result,implicit_progn([FunctionBody]),Body0),
        Body = (Env=[ArgBindings],Body0),
    body_cleanup(Body,Code))).

lisp_compiler_term_expansion( ( <<== FunctionBodyP),
		( :-   Code) ):-
        must_det_l((expand_pterm_to_sterm(FunctionBodyP,FunctionBody),
	must_compile_body(_Cx,toplevel,_Result,implicit_progn([FunctionBody]), Body),
   body_cleanup(Body,Code))).

body_cleanup(Body,Code):-
   term_attvars(Body,AttVars),
   maplist(del_attr_rev2(freeze),AttVars),
   mize_body(',',Body,Code).


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

lisp_compile(SExpression,Body):-
   debug_var('_Ignored',Result),
   as_sexp(SExpression,Expression),
   lisp_compile(Result,Expression,Body).

lisp_compile(Result,SExpression,Body):- 
   as_sexp(SExpression,Expression),
   lisp_compile(ctx{head:lisp_compile(),argbindings:[]},toplevel,Result,Expression,Body).

lisp_compile(Ctx,Env,Result,FunctionBody,Body):- 
   compile_forms(Ctx,Env,Result,[FunctionBody],Body).


compile_forms(Ctx,Env,Result,FunctionBody,Code):-
   must_compile_body(Ctx,Env,Result,implicit_progn(FunctionBody), Body),!,
   body_cleanup(Body,Code).


as_sexp(Stream,Expression):- is_stream(Stream),!,must(parse_sexpr_untyped(Stream,Expression)).
as_sexp(s(Str),Expression):- must(parse_sexpr_untyped(string(Str),Expression)),!.
as_sexp(Str,Expression):- notrace(catch(text_to_string(Str,String),_,fail)),!, must(parse_sexpr_untyped(string(String),Expression)),!.
as_sexp(Str,Expression):- is_list(Str),!,maplist(expand_pterm_to_sterm,Str,Expression).
as_sexp(Str,Expression):- expand_pterm_to_sterm(Str,Expression),!.

dbmsg(X):- writeln('/*'), dbmsg0(X),writeln('*/').
dbmsg0((Textbody:-Body)):-body==Textbody,colormsg1('==>'(body)),!,dbmsg0(Body).
dbmsg0(Var):- var(Var),!,colormsg1(dbmsg_var(Var)).
dbmsg0((A,B)):-compound(A),compound(B),functor(A,F,N),functor(B,F,N),!,dbmsg0(A),dbmsg0(B).
%dbmsg0(asserta(Body)):- !, colormsg1(Body).
dbmsg0(ABody):- ABody=..[A,Body],nonvar(Body), Body = (H :- B) , !, colormsg1((dbmsg(A,H) :- B)).
dbmsg0(H :- Body):- !,colormsg1(H :- Body),!.
dbmsg0(:- Body):- !,colormsg1(:- Body),!.
dbmsg0(Body):- !,colormsg1(:- Body),!.
% dbmsg(:- Body):- !, dmsg(:- Body).

colormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall(Ctrl,fmt90(Msg)).

del_attr_rev2(Name,Var):- del_attr(Var,Name).


'safe_univ_2'(Comp,[F|List]):- (compound(Comp)->compound_name_arguments(Comp,F,List);Comp=..[F|List]).


expand_function_head([FunctionName | FormalArgs], Head, ArgBindings, Result):-!,
        freeze(Arg,debug_var(Arg,Val)),
	zip_with(FormalArgs, ActualArgs, [Arg, Val, bv(Arg, [Val|_])]^true, ArgBindings),
	append(ActualArgs, [Result], HeadArgs),
	Head =.. [FunctionName | HeadArgs].
expand_function_head(FunctionName , Head, ArgBindings, Result):-
    expand_function_head([FunctionName], Head, ArgBindings, Result).


% compile_body(Ctx,Env,Result,Function, Body).
% Expands a Lisp-like function body into its Prolog equivalent

must_compile_body(Ctx,Env,Result,Function, Body):-
  must_or_rtrace(compile_body(Ctx,Env,Result,Function, Body)).


must_or_rtrace(G):-
  (quietly(G)->true;rtrace(G)),!.

expand_pterm_to_sterm(VAR,VAR):- \+ compound(VAR),!.
expand_pterm_to_sterm([X|L],[Y|Ls]):-!,expand_pterm_to_sterm(X,Y),expand_pterm_to_sterm(L,Ls),!.
expand_pterm_to_sterm(X,[F|Y]):- compound_name_arguments(X,F,L),expand_pterm_to_sterm(L,Y),!.
expand_pterm_to_sterm(X,X).


:- dynamic(shared_lisp_compiler:plugin_expand_function_body/5).
:- multifile(shared_lisp_compiler:plugin_expand_function_body/5).
:- discontiguous(shared_lisp_compiler:plugin_expand_function_body/5).

:- discontiguous(compile_body/5).



/*(defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/
compiler_macro_left_right(prog,[Vars|TagBody], [block,[],[let,Vars,[tagbody|TagBody]]]).
% (defmacro unless (test-form &rest forms) `(if (not ,test-form) (progn ,@forms)))
compiler_macro_left_right(unless,[Test|IfFalse] , [if, Test, [], [progn|IfFalse]]).
% (defmacro when (test-form &rest forms) `(if ,test-form (progn ,@forms)))
compiler_macro_left_right( when,[Test|IfTrue]  , [if, Test, [progn|IfTrue], []]).

compile_body(Ctx,Env,Result,[M|MACROLEFT], Code):- 
  term_variables([M|MACROLEFT],VarsS),
  compiler_macro_left_right(M,MACROLEFT,MACRORIGHT),
  term_variables(MACRORIGHT,VarsE),
  VarsE==VarsS,!,
  must_compile_body(Ctx,Env,Result,MACRORIGHT, Code).

compile_body(Ctx,Env,Result,InstrS,Code):-
  shared_lisp_compiler:plugin_expand_function_body(Ctx,Env,Result,InstrS,Code),!.

compile_body(_Cx,_Ev,Name,[defun,Name,Args|FunctionBody], CompileBody):-
    FunctionHead=[Name|Args],
    CompileBody = (asserta((Head  :- (fail, <<==(FunctionHead , FunctionBody)))),
                   asserta((Head  :- (!,  Code)))),
      expand_function_head(FunctionHead, Head, ArgBindings, Result),
      must_compile_body(ctx{head:Head,argbindings:ArgBindings},Env,Result,implicit_progn(FunctionBody),  Body0),
      debug_var("RET",Result),
      debug_var("DEnv",Env),
      Body = (Env=[ArgBindings],Body0),
      term_attvars(Body,AttVars),
      maplist(del_attr_rev2(freeze),AttVars),
      mize_body(',',Body,Code).


compile_body(_Cx,_Ev,SelfEval,SelfEval,true):- notrace(is_self_evaluationing_object(SelfEval)),!.
compile_body(_Cx,_Ev, [],nil,true):- !.
compile_body(_Cx,_Ev,Item,[quote, Item],  true):- !.

compile_body(_Cx,_Ev,[],[progn],  true):- !.
compile_body(Ctx,Env,Result,[progn,Forms], Body):- !, must_compile_body(Ctx,Env,Result,Forms, Body).
compile_body(Ctx,Env,Result,[progn|Forms], Body):- !, must_compile_progn(Ctx,Env,Result,Forms, [],Body).
compile_body(Ctx,Env,Result,implicit_progn(Forms), Body):- is_list(Forms),!,must_compile_progn(Ctx,Env,Result,Forms, [],Body).
compile_body(Ctx,Env,Result,implicit_progn(Forms), Body):- !,must_compile_body(Ctx,Env,Result,Forms, Body).



compile_body(Ctx,Env,Result, 's'(Str),  Body):-
  parse_sexpr_untyped(string(Str),Expression),!,
  must_compile_body(Ctx,Env,Result, Expression,  Body).

/*
compile_body(Ctx,Env,Result, List,  Body):-  \+ is_list(List),!,
  expand_pterm_to_sterm(List,PTerm),
  compile_body(Ctx,Env,Result, PTerm,  Body).
*/
compile_body(Ctx,Env,Result,[if, Test, IfTrue], Body):-must_compile_body(Ctx,Env,Result,[if, Test, IfTrue, []],Body).


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


expand_function_body_unused_needs_throw(Ctx,Env,Result,[car, IN], Body):- \+ current_prolog_flag(lisp_inline,false),
	!,
        must_compile_body(Ctx,Env,MID,IN, ValueBody),
        Body = (ValueBody,(MID =[Result|_]->true;Result=MID)).

compile_body(Ctx,Env,Result,[cons, IN1,IN2], Body):- \+ current_prolog_flag(lisp_inline,false),
	!,
        must_compile_body(Ctx,Env,MID1,IN1,  ValueBody1),
        must_compile_body(Ctx,Env,MID2,IN2,  ValueBody2),
        Body = (ValueBody1,ValueBody2,Result=[MID1|MID2]).




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
compile_body(_Cx,_Ev,[function|Function], [function|Function], true):- !.


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




normalize_let([],[]).
normalize_let([Decl|NewBindingsIn],[Norm|NewBindings]):-
  must(normalize_let1(Decl,Norm)),
  normalize_let(NewBindingsIn,NewBindings).

normalize_let1([bind, Variable, Form],[bind, Variable, Form]).
normalize_let1([Variable, Form],[bind, Variable, Form]).
normalize_let1(Variable,[bind, Variable, []]).


compile_body(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body):-
     must(normalize_let(NewBindingsIn,NewBindings)),!,        
	zip_with(Variables, ValueForms, [Variable, Form, [bind, Variable, Form]]^true, NewBindings),
	expand_arguments(Ctx,ValueForms, ValueBody, Values, Env),
	zip_with(Variables, Values, [Var, Val, bv(Var, [Val|Unused])]^true,Bindings),

   must((debug_var("_U",Unused),
   debug_var("LETENV",BindingsEnvironment),
   ignore((member(VarN,[Variable,Var]),atom(VarN),debug_var([VarN,'_Let'],Val))))), 

	must_compile_body(Ctx,BindingsEnvironment,Result,implicit_progn(BodyForms), BodyFormsBody),
         Body = ( ValueBody,BindingsEnvironment=[Bindings|Env], BodyFormsBody ).

compile_body(Ctx,Env,Result,[SetQ, Atom, ValueForm, Atom2| Rest], Body):- is_parallel_op(SetQ),!, 
   pairify([Atom, ValueForm, Atom2| Rest],Atoms,Forms),
   maplist(expand_ctx_env_forms(Ctx,Env),Forms,BodyS1,Results),
   maplist(set_with_prolog_var(Ctx,Env,SetQ),Atoms,Results,BodyS2),   
   ((op_return_type(SetQ,RT),RT=name) ->  last(Atoms,Result) ; last(Results,Result)),
   append(BodyS1,BodyS2,BodyS),list_to_conjuncts(BodyS,Body).


compile_body(Ctx,Env,Result,[SetQ, Atom, ValueForm, Atom2| Rest], Body):- is_pair_op(SetQ), 
   must_compile_body(Ctx,Env,_ResultU,[SetQ, Atom, ValueForm], Body1),
   must_compile_body(Ctx,Env,Result,[SetQ, Atom2| Rest],  Body2),
   Body = (Body1 , Body2).

compile_body(Ctx,Env,Result,[Defvar, Var], Body):- is_def_nil(Defvar),!,
  must_compile_body(Ctx,Env,Result,[Defvar, Var , nil],Body).

compile_body(Ctx,Env,Result,[Getf, Atom| ValuesForms], Body):- is_place_op(Getf),     
	must_maplist(expand_ctx_env_forms(Ctx,Env),ValuesForms, ValuesBody,ResultVs),
        list_to_conjuncts(ValuesBody,BodyS),
        Body = (BodyS, place_op(Getf, Atom, ResultVs,Env,Result)).

compile_body(Ctx,Env,Result,[SetQ, Atom, ValueForm], Body):- is_symbol_setter(SetQ),
       % (EnvIn\==[]-> true ; break),
	!,	
	must_compile_body(Ctx,Env,ResultV,ValueForm, ValueBody),
        Body = (ValueBody, symbol_setter(SetQ, Atom, ResultV, Env)),
        ((op_return_type(SetQ,RT),RT=name) ->  =(Atom,Result) ; =(ResultV,Result)).







initState:attr_unify_hook(_,_).


symbol_value(Var,Env,Value):-
  symbol_value_or(Var,Env,
    last_chance_symbol_value(Var,Env,Value),Value).

last_chance_symbol_value(Var,_Env,Result):- nb_current(Var,Result),!.
last_chance_symbol_value(Var,_Env,_Result):- 
  lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

symbol_value_or(Var,Env,G,Value):-
 (env_memb(Bindings, Env),bvof(bv(Var, Value0),Bindings))-> extract_variable_value(Value0, Value, _);
   (special_var(Var, Value) -> true;  G).


set_symbol_value(Var,Env,Result):-var(Result),!,symbol_value(Var,Env,Result).
set_symbol_value(Var,Env,Result):- !,
     ((	env_memb(Bindings, Env),
                bvof(bv(Var, Value0),Bindings)
      ->	nb_setarg(1,Value0,Result)
      ;	special_var(Var, Old)
      ->	once(retract(special_var(Var, Old))),
                asserta(special_var(Var, Result))
      ;         last_chance_set_symbol_value(Var,Env,Result))).
set_symbol_value(Var,Env,Result):- 
      (	env_memb(Bindings, Env),
                bvof(bv(Var, Value0),Bindings)
      ->	extract_variable_value(Value0, _, Hole),
                Hole = [Result|_]
      ;	special_var(Var, Old)
      ->	once(retract(special_var(Var, Old))),
                asserta(special_var(Var, Result))
      ;         last_chance_set_symbol_value(Var,Env,Result)).
last_chance_set_symbol_value(Var,_Env,Result):- nb_setval(Var,Result),!.
last_chance_set_symbol_value(Var,_Env,_Result):- 
  lisp_error_description(atom_does_not_exist, ErrNo, _),throw(ErrNo, Var).


sym_arg_val_env(Var,InValue,Value,Env):-
  set_symbol_value(Var,Env,InValue),
  Value=InValue,!.

sym_arg_val_env(Var,InValue,Value,Env):- !,
  symbol_value_or(Var,Env,(nonvar(InValue),InValue=Value),Value)-> true;
    lisp_error_description(unbound_atom, ErrNo, _),throw(ErrNo, Var).

place_op(incf, Var, [Value], Env, Result):- atom(Var),!,
  symbol_value(Var, Env , Old),
  Result is Old+Value,
  set_symbol_value(Var, Env , Result).
place_op(decf, Var, [Value], Env, Result):- atom(Var),!,
  symbol_value(Var, Env , Old),
  Result is Old-Value,
  set_symbol_value(Var, Env , Result).
place_op(setf, Var, [Result], Env, Result):- atom(Var),!,
  set_symbol_value(Var, Env , Result).

%TODO Make it a constantp
symbol_setter(defconstant, Var, Result, _Environment):-
   ( special_var(Var, _) -> once(retract(special_var(Var, _))); true),
   asserta(special_var(Var, Result)).

symbol_setter(defparameter, Var, Result, _Environment):-
   ( special_var(Var, _) -> once(retract(special_var(Var, _))); true),
   asserta(special_var(Var, Result)).

symbol_setter(defvar, Var, Result, _Environment):-
     special_var(Var, _) -> true ; asserta(special_var(Var, Result)).

symbol_setter(setq, Var, Result, Env):- !, set_symbol_value(Var,Env,Result).

symbol_setter(psetq, Var, Result, Env):- !,
  symbol_setter(setq, Var, Result, Env).


is_symbol_setter(OP):- is_pair_op(OP).
is_symbol_setter(OP):- is_parallel_op(OP).
is_symbol_setter(OP):- is_def_nil(OP).

op_return_type(Op,name):- is_def_nil(Op).
op_return_type(defconstant,name).

is_def_nil(defparameter).
is_def_nil(defvar).

is_def_nil(defconstant).

is_pair_op(setq).
is_pair_op(psetq).

is_pair_op(setf).
is_pair_op(psetf).


is_place_op(setf).
is_place_op(psetf).
is_place_op(getf).
is_place_op(incf).
is_place_op(decf).
is_place_op(rotatef).
is_place_op(shiftf).


is_parallel_op(psetf).
is_parallel_op(psetq).

pairify([],[],[]).
pairify([Atom, ValueForm | Rest],[Atom | Atoms],[ValueForm | Forms]):-
   pairify(Rest,Atoms,Forms).

set_with_prolog_var(_Cx,Env,SetQ,Atom,Result,symbol_setter(SetQ, Atom, Result, Env)).

expand_ctx_env_forms(Ctx, Env,Forms,Body, Result):- 
   must_compile_body(Ctx,Env,Result,Forms, Body).

compile_body(Ctx,Env,Result,Atom, Body):- Atom==mapcar,!, dbmsg(compile_body(Ctx,Env,Result,Atom, Body)), dumpST,break.

compile_body(Ctx,Env,Value, Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = symbol_value(Atom,Env, Value).

compile_body(Ctx,Env,InValue, Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        (get_attr(Value,initState,t);get_attr(InValue,initState,t)),
	!,
        Body = true.

compile_body(Ctx,Env,Value,Atom, Body):- atom(Atom),
        find_incoming_value(Ctx,Env,Atom,InValue,Value),
        put_attr(Value,initState,t),
        put_attr(InValue,initState,t),
	!,
        Body = sym_arg_val_env(Atom,InValue,Value,Env).

compile_body(_Cx,Env,Value, Atom,  Body):- atom(Atom),
   debug_var([Atom,'_Stack'],Value0),
   debug_var([Atom,'_VAL'],Value),
	!,
	lisp_error_description(unbound_atom, ErrNo, _),
	Body = (once((	env_memb(Bindings, Env),
			bvof(bv(Atom, Value0),Bindings),
			extract_variable_value(Value0, Value, _)
		    ;	special_var(Atom, Value)
		    ;	throw(ErrNo, Atom)	)	)).	


op_replacement(+,plus).
op_replacement(-,minus).
op_replacement(*,mult).
op_replacement(<,lessThan).
op_replacement(>,greaterThan).

compile_body(Ctx,Env,Result,[Op | FunctionArgs], Body):- op_replacement(Op,Op2), !,
  must_compile_body(Ctx,Env,Result,[Op2 | FunctionArgs],Body).


compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- \+ atom(FunctionName),!,
  must_compile_body(Ctx,Env,Result,[funcall,FunctionName | FunctionArgs],Body).

compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):- FunctionName \==funcall,
  member(bv(Atom0,_),Ctx.argbindings),Atom0==FunctionName,!,
  must_compile_body(Ctx,Env,Result,[funcall,FunctionName | FunctionArgs],Body).

% Non built-in function expands into an explicit function call
compile_body(Ctx,Env,Result,[FunctionName | FunctionArgs], Body):-
      !,
      expand_arguments(Ctx,FunctionArgs, ArgBody, Args, Env),
      append(Args, [Result], ArgsResult),
      debug_var([FunctionName,'_Ret'],Result),
      ExpandedFunction =.. [FunctionName | ArgsResult],
      Body = (	ArgBody,
                      ExpandedFunction	).
   

	expand_arguments(_Ctx,[], true, [], _Environment).
	expand_arguments(Ctx,[Arg|Args], Body, [Result|Results], Env):-
		must_compile_body(Ctx,Env,Result,Arg, ArgBody),
                Body = (ArgBody, ArgsBody),
		expand_arguments(Ctx,Args, ArgsBody, Results, Env).


must_compile_progn(Ctx,Env,Result,Forms, PreviousResult, Body):-
   must_or_rtrace(compile_progn(Ctx,Env,Result,Forms, PreviousResult,Body)).
must_compile_progn1(Ctx,Env,Result,Forms, PreviousResult, Body):-
   must_or_rtrace(compile_progn1(Ctx,Env,Result,Forms, PreviousResult,Body)).

compile_progn(_Cx,_Ev,Result,[], Result,true).
compile_progn(Ctx,Env,Result,[Form | Forms], _PreviousResult, Body):-  !,
	must_compile_body(Ctx,Env,FormResult, Form,FormBody),
	must_compile_progn(Ctx,Env,Result, Forms, FormResult, FormSBody),
        Body = (FormBody,FormSBody).
compile_progn(Ctx,Env,Result, Form , _PreviousResult, Body):-
	must_compile_body(Ctx,Env,Result,Form, Body).

conjoin_0(A,B,A):- B==true,!.
conjoin_0(A,B,B):- A==true,!.
conjoin_0((A,AA),B,(A,AAB)):-!, conjoin(AA,B,AAB).
conjoin_0(A,B,(A,B)).

mize_body(_,A,A):- \+ compound(A),!.
mize_body(F,(A,B),AB):-!,mize_body(F,A,AA),mize_body(F,B,BB),conjoin_0(AA,BB,AB).
mize_body(_,(A -> B ; C),(AA -> BB ; CC)):-!,mize_body1(->,A,AA),mize_body1(';',B,BB),mize_body1(';',C,CC).
mize_body(_,(B ; C),( BB ; CC)):-!,mize_body1(';',B,BB),mize_body1(';',C,CC).
mize_body(F,A,C):- mize_body1(F,A,B),mize_body2(F,B,C),!.
mize_body(_F,A,B):- A=..[F|AA], must_maplist(mize_body(F),AA,BB),B=..[F|BB].
mize_body(_,A,A):-!.

mize_body1(_,A,A):- var(A),del_attr(A,initState).
mize_body1(_,A,A):- \+ compound(A),!.
mize_body1(F,(A,B),AB):-!,mize_body1(F,A,AA),mize_body1(F,B,BB),conjoin_0(AA,BB,AB).
mize_body1(F,A,B):- is_list(A),must_maplist(mize_body1(F),A,B).
mize_body1(_,A,L=[R]):- A =@= (L=[R, []]).
mize_body1(_F,A,B):- A=..[F|AA],must_maplist(mize_body1(F),AA,BB),B=..[F|BB].
mize_body1(_,A,A):-!.

mize_body2(_,A,A):- \+ compound(A),!.
%mize_body2(_,A=B,pass2(A=B)):- var(A),var(B),A=B,!.
mize_body2(_F,A,B):- A=..[F|AA], must_maplist(mize_body2(F),AA,BB),B=..[F|BB].
mize_body2(_,A,A):-!.


ssip_compiler_term_expansion(Symbol,lambda(Args,Body),[OOUT]):- atom(Symbol),is_list(Args),
  length(Args,A1),
  A is A1+1,
  cfunctor(P,Symbol,A),
 (predicate_property(P,defined)->gensym(Symbol,SymbolR);Symbol=SymbolR),
  Head=..[SymbolR|Args],
  subst(Body,Symbol,SymbolR,BodyM),
  OUT= ((Head <<== BodyM)),
  must(lisp_compiler_term_expansion(OUT,OOUT)),!.

ssip_compiler_term_expansion(Symbol,Symbol2,ssip_define(Symbol,Symbol2)):-!.

:- discontiguous(ssip_define/2).
:- multifile(ssip_define/2).

% The hook into the compiler
term_expansion(Symbol==Function,O) :- I= (Symbol==Function),ssip_compiler_term_expansion(Symbol,Function,O),nl,nl,
  flatten([I,O],L),
  maplist(dbmsg,L),!.
  % in_cmt(maplist(portray_clause,L)),!.
term_expansion(I,O) :- lisp_compiler_term_expansion(I,O),I\==O,nl,nl,
  flatten([I,O],L),
  maplist(dbmsg,L),!.


% Now Prolog can understand them, compile the additional library files


:- ensure_loaded(builtin_lisp_functions).
:- ensure_loaded(lisp_library).
:- ensure_loaded(streams).
:- ensure_loaded(tests).

%% [tim.prolog]siptest.
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:39:27 1986
%% this file contains samples for SIP.


:- fixup_exports.




fact == lambda([n], if(=(n,0),1,n*fact(sub1(n)))).


add1 == lambda([n], n+1).

sub1 == lambda([n], n-1).

% higher order functions

mapcar ==
  lambda([fun,l],
         if(null(l),
            nil,
            cons(fun(car(l)),mapcar(fun,cdr(l))))).

% simple list manipulation functions.

length == lambda([l], if(null(l),0,add1(length(cdr(l))))).

append == lambda([l1,l2],if(null(l1),l2,cons(car(l1),append(cdr(l1),l2)))).


% stuff for streams.

filter ==
  lambda([fun,s],
         if('emptyStream?'(s),
            s,
            if(fun(head(s)),
               consStream(head(s),filter(fun,tail(s))),
               filter(fun,tail(s))))).

from(n) <<== consStream(n,from(n+1)).
% from == lambda([n],consStream(n,from(n+1))).

nthStream == lambda([s,n],if(n=1,head(s),nthStream(tail(s),n-1))).

integers == from(1).

% environments

makeCounter ==
  lambda([],
         begin(counter == 0,
               lambda([],setq(counter,1+counter)))).

caaaar == lambda([x],car(car(car(car(x))))).

caar == lambda([x],car(car(x))).

reverse ==
  lambda([l],
     if(null(l),
        l,
        append(reverse(cdr(l)),(cons(car(l),nil))))).

:- set_prolog_flag(double_quotes,string).


c1:- print_eval_string("
 (defun sum_with_map (xs)
  (let (( running_total 0))
    (let ((summer 
            (function 
               (lambda (n)
                (setq running_total (+ running_total n))))))
       (mapcar summer  xs) running_total))) "
).

c2:- lisp_compile(s("
(defun sum_with_map (xs)
  (let (( running_total 0))
    (let ((summer 
            (function 
               (lambda (n)
                (setq running_total (+ running_total n))))))
       (mapcar summer  xs) running_total)))
 "
  ),O),writeln(O).


