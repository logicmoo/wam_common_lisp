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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 * Changes since 2001:
 *
 *
 *******************************************************************/
:- module(body, []).
:- set_module(class(library)).
:- include('header').

:- discontiguous(compile_body_form/5).


% =============================================================================
% Body TRANSFORMATIONS declared throughout the file as..
%
%   compiler_macro_left_right/2
%
%    but performed *only* here 
%   (very early!?)
% =============================================================================
:- dynamic(compiler_macro_left_right/3).
:- discontiguous(compiler_macro_left_right/3).

compile_body_form(Ctx,Env,Result,[M|MACROLEFT], Code):- atom(M),
  term_variables([M|MACROLEFT],VarsS),
  compiler_macro_left_right(MS,MACROLEFT,MACRORIGHT),
  same_symbol(M,MS),
  term_variables(MACRORIGHT,VarsE),
  VarsE==VarsS,!,
  must_compile_body(Ctx,Env,Result,MACRORIGHT, Code).

% =============================================================================
% IF,WHEN,UNLESS,COND,*CASE,*TYPECASE
% =============================================================================
% (defmacro unless (test-form &rest forms) `(if (not ,test-form) (progn ,@forms)))
compiler_macro_left_right(unless,[Test|IfFalse] , [if, Test, [], [progn|IfFalse]]).
% (defmacro when (test-form &rest forms) `(if ,test-form (progn ,@forms)))
compiler_macro_left_right( when,[Test|IfTrue]  , [if, Test, [progn|IfTrue], []]).
% IF/1
compiler_macro_left_right(if,[Test, IfTrue], [if, Test, IfTrue ,[]]).

% related compiler macros need to happen before this
compile_body_form(Ctx,Env,Result,[CONDIF|Rest], Code):- 
  compile_condifs(Ctx,Env,Result,[CONDIF|Rest], Code),!.

% =============================================================================
% WHILE
/*
(defmacro while (cond &rest forms)
  (let ((start (gensym)))
  `(tagbody ,start 
     (when ,cond (progn ,@forms) (go ,start)))))
*/
/*
(let ((x 10)) (while (> (decf x) 1) (print x )))
(LET ((x 10 ))(while (> (DECF x )1 )(PRINT x )(SETQ x (1- x ))))
(LET ((x 10 ))(while (> (DECF x )1 )(PRINT x )(SETQ x (1- x ))))
*/
% =============================================================================

compiler_macro_left_right(u_while,[Cond|Forms], 
  [tagbody,Start,[when,Cond,[progn|Forms],[go,Start]]]) :- gensym('while',Start).
compiler_macro_left_right(while,[Cond|Forms], 
  [tagbody,Start,[when,Cond,[progn|Forms],[go,Start]]]) :- gensym('while',Start).

   
% =============================================================================
% = AND OR XOR 
% =============================================================================
% BinMacro-0
compiler_macro_left_right(BinOP,L, Identity):- binary_macro(BinOP,Identity),L==[].
% BinMacro-1
compiler_macro_left_right(BinOP,[Form1|NoMore], [BinOP,Identity,Form1]):- NoMore==[], binary_macro(BinOP,Identity).
% BinMacro-3+
compile_body_form(Ctx,Env,Result,[BinOP,Form1|Form2],Code):- binary_macro(BinOP,_),
  Form2\=[_],Form2\=[BinOP|_], !, 
  compile_body_form(Ctx,Env,Result1,Form1,Code1),
  compile_body_form(Ctx,Env,Result2,[BinOP|Form2],Code2),
  compile_body_form(Ctx,Env,Result,[BinOP,Result1,Result2],Code3),
  Code = (Code1,Code2,Code3).

% EXT::XOR
binary_macro(ext_xor,[]).
compile_body_form(Ctx,Env,Result,[ext_xor,Form1,Form2],Code):-
  compile_body_form(Ctx,Env,Result1,Form1,Code1),
  compile_body_form(Ctx,Env,Result2,Form2,Code2),
  Code3 = (((Result1 \==[]) -> (Result2 ==[]) ; (Result2 \==[])) -> Result=t;Result=[]),
  Code = (Code1,Code2,Code3).

% AND
compiler_macro_left_right(and,[], []).
compiler_macro_left_right(and,[Form1], Form1).
compiler_macro_left_right(and,[Form1,Form2], [if,Form1,Form2]).
compiler_macro_left_right(and,[Form1|Rest], [and,Form1,[and|Rest]]).

% OR
compiler_macro_left_right(or,[], []).
compiler_macro_left_right(or,[Form1], Form1).
% OR-0
compile_body_form(_Ctx,_Env,[],[or], true).
% OR-1
compile_body_form(Ctx,Env,Result,[or,Form], Body):- must_compile_body(Ctx,Env,Result,Form, Body).
% OR-2+
compile_body_form(Ctx,Env,Result,[or,Form1|Form2],Code):- Form2\=[_],Form2\=[or|_], !, 
  compile_body_form(Ctx,Env,Result,[or,Form1,[or|Form2]],Code).

compile_body_form(Ctx,Env,Result,[or,Form1,Form2],Code):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1),
   must_compile_body(Ctx,Env,Result2,Form2, Body2),
   debug_var("FORM1_Res",Result1),
        Code = (	(Body1,Result1 \== [],Result = Result1) 		
				-> 	true
				;  	(Body2, Result = Result2)).

% OR-2 needs to use body compiler below
compile_body_form(Ctx,Env,Result,[or,Form1,Form2],Code):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1),
   must_compile_body(Ctx,Env,Result2,Form2, Body2),
   debug_var("FORM1_Res",Result1),
        Code = (	Body1,
			( Result1 \== []
				-> 	Result = Result1
				;  	(Body2, Result = Result2))).

% compiler_macro_left_right(or,[Form1,Form2,Form3|Rest], [or,Form1,[or,Form2,[or,Form3,[or|Rest]]]]).



% =============================================================================
% PROG1/PROG2/PROGN
% =============================================================================

% PROG1
compile_body_form(Ctx,Env,Result,[prog1,Form1|FormS],Code):- !,
   must_compile_body(Ctx,Env,Result,Form1, Body1),
   must_compile_progn(Ctx,Env,_ResultS,FormS,Result,Body2),
   Code = (Body1, Body2).

% PROG2
compile_body_form(Ctx,Env,Result,[prog2,Form1,Form2|FormS],Code):- !,
   must_compile_body(Ctx,Env,_Result1,Form1, Body1),
   must_compile_body(Ctx,Env,Result,Form2, Body2),
   must_compile_progn(Ctx,Env,_ResultS,FormS,Result,BodyS),
   Code = (Body1, Body2, BodyS).

% PROGN
compile_body_form(Ctx,Env,Result,[progn,Forms], Body):- !, must_compile_progn(Ctx,Env,Result,[Forms],Body).
compile_body_form(Ctx,Env,Result,[progn|Forms], Body):- !, must_compile_progn(Ctx,Env,Result,Forms,Body).

% =============================================================================
% PROGV 
% =============================================================================

% PROGV    % ( progv ' ( a ) ` ( , ( + 1 1 ) ) a ) => 2
compile_body_form(Ctx,Env,Result,[progv,VarsForm,ValuesForm|FormS],Code):- !,
   must_compile_body(Ctx,Env,VarsRs,VarsForm,Body1),
   must_compile_body(Ctx,Env,ValuesRs,ValuesForm,Body2),
   must_compile_progn(Ctx,Env,Result,FormS,BodyS),
   Code = (Body1, Body2 , maplist(bind_dynamic_value(Env),VarsRs,ValuesRs), BodyS).

% PROG
/*(defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/
compiler_macro_left_right(prog,[Vars|Forms], [block,[],[let,Vars,[tagbody|Forms]]]).


% =============================================================================
% = LET/EXT:LETF = 
% =============================================================================
%compile_body_form(Ctx,Env,Result,[OP, []| BodyForms], Body):- memberchk(OP,ext_letf_xx,ext_letf,let,let_xx),
%  must_compile_body(Ctx,Env,Result,[progn| BodyForms], Body).

compile_body_form(Ctx,Env,Result,[OP, NewBindingsIn| BodyForms], Body):- memberchk(OP,[ext_letf_xx,ext_letf,let,let_xx]), 
  assertion(is_list(NewBindingsIn)),!,
  always(compile_let(Ctx,Env,Result,[OP, NewBindingsIn| BodyForms], Body)).

% =============================================================================
% = WITH SLOTS = 
% =============================================================================

compile_body_form(Ctx,Env,Result,[with_slots,Slots,Obj|Progn],Body):- 
 always(is_list(Slots)),!,
 slot_object_lets(Obj,Slots,Lets),
 compile_body_form(Ctx,Env,Result,[let,Lets|Progn],Body).

   % slot_object_lets(Obj,Slots,Lets)
   slot_object_lets(_Obj,[],[]).
   slot_object_lets(Obj,[S|Slots],[[S,['slot_value',Obj,[quote,S]]]|Lets]):-
     slot_object_lets(Obj,Slots,Lets).

% =============================================================================
% = DOLIST = 
% =============================================================================

% DOLIST
compile_body_form(Ctx,Env,Result,['dolist'|Rest], Code):- !,
  always(compile_dolist(Ctx,Env,Result,['dolist'|Rest], Code)).

wl: init_args(1, cl_dolist).
wl: declared(cl_dolist,inlined).
cl_dolist(VarList,FormS,Result):-
   compile_dolist(_Ctx,_Env,Result,['dolist',VarList|FormS], Code),
   always(Code).
   
   compile_dolist(Ctx,Env,Result,['dolist',[Var,List,RetVar]|FormS], Code):-
      debug_var('DoRetVar',RetVar),
      must_compile_body(Ctx,Env,Result,[let,[RetVar],['dolist',[Var,List]|FormS],RetVar], Code).
   
   compile_dolist(Ctx,Env,Result,['dolist',[Var,List]|FormS], Code):-
       must_compile_body(Ctx,Env,ResultL,List,ListBody),
       must_compile_body(Ctx,Env2,Result,[progn|FormS], Body),
       debug_var('BV',BV),debug_var('Env2',Env2),debug_var('Ele',X),debug_var('List',ResultL),
       Code = (ListBody,                                                                         
         (( BV = bv(Var,X),Env2 = [BV|Env])),
           forall(member(X,ResultL),
             (nb_setarg(2,BV,X),
               Body))).

% =============================================================================
% = COMPILE =  ( file with several other debugging tools?)
% =============================================================================

% (compile ...)
compile_body_form(Ctx,Env,Result,[compile|Forms], Body):- !,
   must_compile_closure_body(Ctx,CompileEnvironment,CompileResult,[progn|Forms],  CompileBody),
   
   debug_var('LResult',CompileResult),
   debug_var('CompileEnvironment',CompileEnvironment),
   Result = closure([CompileEnvironment|Env],CompileResult,[],CompileBody),
   Body = true.


% =============================================================================
% = Multiple Values = 
% =============================================================================
% VALUES (r1 . rest )
compile_body_form(Ctx,Env,Result,['values',R1|EvalList], (ArgBody,Body)):-!,
    expand_arguments(Ctx,Env,funcall,0,[R1|EvalList], ArgBody, [Result|Results]),
    Body = nb_setval('$mv_return',[Result|Results]).
compile_body_form(_Ctx,_Env,[],['values'], nb_setval('$mv_return',[])):-!.

:- nb_setval('$mv_return',[]).

% Macro MULTIPLE-VALUE-BIND
compile_body_form(Ctx,Env,Result,[OP,Vars,Eval1|ProgN], Body):- same_symbol(OP,'multiple-value-bind'),
  %maplist(maybe_special_letvars,VarNames,VarNamesSpecials),
  must_compile_body(Ctx,Env,Result,[let,Vars,[progn,Eval1,['#setqFromValues',Vars]|ProgN]],Body).

  
%maybe_special_letvars(Var,[Var,[if,[boundp,[quote,Var]],[get_var,Var],[]] ]).

% Macro MULTIPLE-VALUE-LIST
compile_body_form(Ctx,Env,Result,[OP,Eval1], (Body,nb_current('$mv_return',Result))):-
  same_symbol(OP,'multiple-value-list'),
  debug_var('MV_RETURN',Result),
  debug_var('IgnoredRet',IResult),
  must_compile_body(Ctx,Env,IResult,Eval1,Body).

% Macro MULTIPLE-VALUE-CALL
compile_body_form(Ctx,Env,Result,[OP,Function|Progn], Body):-
  same_symbol(OP,'multiple-value-call'),
  must_compile_body(Ctx,Env,Result,[progn,[progn|Progn],['apply',Function,['#returnFomLastValues']]],Body).

% synthetic RETURN-VALUES -> values
compile_body_form(_Ctx,_Env,Values,['#returnFomLastValues'], nb_current('$mv_return',Values)).

% synthetic SETQ-VALUES (vars*)
compile_body_form(_Ctx,Env,[],['#setqFromValues',Vars], setq_from_values(Env,Vars)):-!.
   setq_from_values(Env,Vars):- nb_current('$mv_return',Values),setq_from_values_each(Env,Vars,Values).
   setq_from_values_each(_Env,_,[]):- !. %lisp_dump_break,!.
   setq_from_values_each(_Env,[],_):-!.
   setq_from_values_each(Env,[Var|Vars],[Value|Values]):-
      set_var(Env,Var,Value),
      setq_from_values_each(Env,Vars,Values).


% =============================================================================
% EVAL
% =============================================================================
compile_body_form(_Ctx,_Env,Result,[eval,Var], cl_eval(Var,Result)):- \+ is_list(Var), !. 
compile_body_form(_Ctx,_Env,Result,[eval,[A|Var]], cl_eval([A|Var],Result)):- \+ atom(A), !. % NEVER SEEN
compile_body_form(_Ctx,_Env,Result,[eval,Var], cl_eval(Var,Result)):- !.
compile_body_form(Ctx,Env,Result,['eval',Form1],
  (Body1,cl_eval(Result1,Result))):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1).

% =============================================================================
% = LAMBDA/CLOSURES = 
% =============================================================================

% (function (lambda ... ))
compile_body_form(Ctx,_Env,Result,POrSTerm, Body):- p_or_s(POrSTerm,function,[[lambda,LambdaArgs| LambdaBody]]),
      !,
      must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
      debug_var('LArgs',LambdaArgs),
      debug_var('LResult',ClosureResult),
      debug_var('LEnv',ClosureEnvironment),
   debug_var('Function',Result),
   Result = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody),
   Body = true.

% (lambda ...)
compile_body_form(Ctx,Env,Result,[lambda,LambdaArgs|LambdaBody], Body):-
	!,
	must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
   debug_var('LArgs',LambdaArgs),
   debug_var('LResult',ClosureResult),
   debug_var('Lambda',Result),
   debug_var('ClosureEnvironment',ClosureEnvironment),
   Body =
     (Result = closure([ClosureEnvironment|Env],ClosureResult,LambdaArgs,ClosureBody)).

% ((lambda ...) ...)
compile_body_form(Ctx,_Env,Result,[POrSTerm|ARGS],Body):- 
   p_or_s(POrSTerm,lambda,[LambdaArgs|LambdaBody]),
   must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
    ClosureResult = Result,debug_var('ClosureEnv',ClosureEnvironment),debug_var('ClosureResult',Result),
   Body = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody,ARGS,Result).


% (function .)
compile_body_form(Ctx,Env,Result,POrSTerm, Pre):- p_or_s(POrSTerm,function,[Symbol]),
  find_operator_else_function(Ctx,Env,Symbol,Result,Pre).

% ((function .) ...)
compile_body_form(Ctx,Env,Result,[POrSTerm|ARGS],(Pre,Body)):- p_or_s(POrSTerm,function,[Symbol]),
  find_operator_else_function(Ctx,Env,Symbol,FResult,Pre),
  must_compile_body(Ctx,Env,Result,[FResult|ARGS],Body).

% (closure ...)
compile_body_form(_Ctx,_Env,Result,POrSTerm,Body):- 
   p_or_s(POrSTerm,closure,[ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody]),
   debug_var('Closure',Result),debug_var('ClosureResult',ClosureResult),
   Body = (Result = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody)).
% ((closure ...) ...)
compile_body_form(_Ctx,_Env,Result,[POrSTerm|ARGS],Body):- 
   p_or_s(POrSTerm,closure,[ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody]),
    ClosureResult = Result,debug_var('ClosureEnv',ClosureEnvironment),debug_var('ClosureResult',Result),
   Body = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody,ARGS,Result).



% =============================================================================
% EXPANDERs
% =============================================================================

binop_identity(+,0).
binop_identity(u_c43,0).
binop_identity(-,0).
binop_identity(*,1).
binop_identity((/),1).

% BinOP-0 like (+ )
compiler_macro_left_right(BinOP,L, Identity):- binop_identity(BinOP,Identity),L==[].
% BinOP-1   (* 1)
compiler_macro_left_right(BinOP,[Form1|NoMore], [BinOP,Identity,Form1]):- NoMore==[], binop_identity(BinOP,Identity).
% BinOP-3+  (+  1 2 3 ...)
compile_body_form(Ctx,Env,Result,[BinOP,Form1,Form2,Form3|FormS],Code):- fail, binop_identity(BinOP,_Identity),
  compile_body_form(Ctx,Env,Result,[BinOP,[BinOP,Form1,Form2],Form3|FormS],Code),!.
  
% BinOP-3+
compile_body_form(Ctx,Env,Result,[BinOP,Form1,Form2,Form3|FormS],Code):- binop_identity(BinOP,_Identity),
  must_compile_body(Ctx,Env,Result1,[BinOP,Form1,Form2],Code1),
  %rw_add(Ctx,Result1,w),
  freeze(Result1,var(Result1)),
  must_compile_body(Ctx,Env,Result,[BinOP,Result1,Form3|FormS],Code2),
  Code = (Code1,Code2).


:- fixup_exports.

