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
:- ensure_loaded(utils_for_swi).

must_compile_closure_body(Ctx,Env,Result,Function, Body):-
  must_compile_body(Ctx,Env,Result,Function, Body0),
  body_cleanup_keep_debug_vars(Ctx,Body0,Body).


% compile_body(Ctx,Env,Result,Function, Body).
% Expands a Lisp-like function body into its Prolog equivalent

preserved_var:attr_unify_hook(_,_):- fail.

must_compile_body(_Ctx,_Env,ResultO,LispCode, Body):- var(LispCode), get_attr(LispCode,preserved_var,t),!,true=Body,
   ResultO = LispCode.
must_compile_body(Ctx,Env,ResultO,LispCode, BodyO):-
  %notrace((maybe_debug_var('_rCtx',Ctx),
  %maybe_debug_var('_rEnv',Env),
  %maybe_debug_var('_rResult',Result),
  %maybe_debug_var('_LispCode',LispCode),
  %maybe_debug_var('_rBody',Body))),
  resolve_reader_macros(LispCode,Forms),!,
  always((compile_body(Ctx,Env,Result,Forms, Body9)->nonvar(Body9))),
  must_compile_body_pt2(Ctx,Env,Result,ResultO,Forms, Body9,BodyO).

ensure_assignment(X=Y,true):- X=Y,!.
ensure_assignment(X=Y,reset_mv):- X=Y,!.
ensure_assignment(G,G).

must_compile_body_pt2(_Ctx,_Env,Result,ResultO, _Forms, true,Body):- ensure_assignment(Result=ResultO,Body),!.
must_compile_body_pt2(Ctx,_Env,Result,ResultO,_Forms, Body9,BodyO):-
  body_cleanup_no_optimize(Ctx,Body9,Body),
  ((Body==true,fail) -> BodyO=(ResultO=Result) ; (ResultO=Result,BodyO=Body)),
  % nb_current('$compiler_PreviousResult',THE),setarg(1,THE,Result),
  !.

make_restartable_block(Place,CodeWithPlace,LispCode):-
     gensym(restartable_block,GenBlock),gensym(restartable_loop,GenLoop),
     LispCode = [ block, GenBlock,
                    [ tagbody,GenLoop,
                      [ restart_bind,[[ store_value,[ lambda,[u_object],[setf, Place, u_object],[go, GenLoop]]]],
                        [ return_from,GenBlock,[progn|CodeWithPlace] ]]]].



:- discontiguous(compile_body/5).

% ===============================================================
% === ENTRY TO COMPILE-BODY ===
% ===============================================================
% Prolog vars
compile_body(_Ctx,_Env,Result,Var, true):- Result == Var,!.
compile_body(_Ctx,_Env,Result,Var, true):- attvar(Var),!, Result = Var.
compile_body(_Ctx,_Env,Result,Var, true):- is_ftVar(Var), !,dumpST,trace, Result = Var.
compile_body(Ctx,Env,Result,Var, Code):- is_ftVar(Var), !, % NEVER SEEN
  debug_var("EVAL",Var),
  must_compile_body(Ctx,Env,Result,[eval,Var], Code).
compile_body(_Ctx,_Env,_Result,[OP|R], _Body):- var(OP),!,trace_or_throw(c_b([OP|R])).

% Lazy Reader
compile_body(Ctx,Env,Result, 's'(Str),  Body):-
  parse_sexpr_untyped(string(Str),Expression),!,
  must_compile_body(Ctx,Env,Result, Expression,  Body).

%(the number 1)
compile_body(Ctx,Env,Result,['the',_Type,Form1],Body):-!,
  compile_body(Ctx,Env,Result,Form1,Body).
compile_body(Ctx,Env,Result,['truely_the',_Type,Form1],Body):-!,
  compile_body(Ctx,Env,Result,Form1,Body).

% SELF EVALUATING OBJECTS
compile_body(_Cx,_Ev, [],[],true):- !.
compile_body(_Cx,_Ev, [],nil,true):- !.

compile_body(_Ctx,_Env,Result,'$S'([Type|Args]),create_struct([Type|Args],Result)).

% numbers
compile_body(_Cx,_Ev,Result,SelfEval,Body):- notrace(is_self_evaluating_object(SelfEval)),!,
  ensure_assignment(Result=SelfEval,Body).

% =============================================================================
% =  QUOTING =
% =============================================================================

% QUOTE
compile_body(_Cx,_Ev,Item,[quote, Item],  true):- !.
   
   % COMMENTS
   is_comment([COMMENT,String|_],String):- atom(COMMENT),!,atom_concat_or_rtrace('$COMMENT',_,COMMENT).
   is_comment(COMMENTP,String):- compound(COMMENTP),!,COMMENTP=..[COMMENT,String|_],!,atom_concat_or_rtrace('$COMMENT',_,COMMENT).


% ` Backquoted 
compile_body(_Cx,Env,Result,['#BQ',Form], Code):-!,compile_bq(Env,Result,Form,Code),!.
compile_body(_Cx,Env,Result,['`',Form], Code):-!,compile_bq(Env,Result,Form,Code),!.
compile_body(Ctx,Env,Result,['#COMMA',Form], (Code,cl_eval(CommaResult,Result))):-!,compile_body(Ctx,Env,CommaResult,Form,Code),!.
compile_body(Ctx,Env,Result,['#BQ-COMMA-ELIPSE',Form], (Code,cl_eval(CommaResult,Result))):- dump_trace_lisp,!,compile_body(Ctx,Env,CommaResult,Form,Code),!.


% =============================================================================
% Conditonal  evaluation/compilation
% =============================================================================

compile_body(_Ctx,_Env,[],COMMENT,true):- is_comment(COMMENT,_),!.

% #+
compile_body(Ctx,Env,Result,[OP,Flag,Form|MORE], Code):- same_symbol(OP,'#+'),!, 
   always(( get_var(xx_features_xx,FEATURES),
          ( member(Flag,FEATURES) -> must_compile_body(Ctx,Env,Result,Form, Code) ; compile_body(Ctx,Env,Result,MORE, Code)))).
  
% #-
compile_body(Ctx,Env,Result,[OP,Flag,Form|MORE], Code):- same_symbol(OP,'#-'),!,
   always(( get_var(xx_features_xx,FEATURES),
          ( \+ member(Flag,FEATURES) -> must_compile_body(Ctx,Env,Result,Form, Code) ; 
             compile_body(Ctx,Env,Result,MORE, Code)))).

% EVAL-WHEN
compile_body(Ctx,Env,Result,[OP,Flags,Forms], OutCode):-  same_symbol(OP,'eval-when'), !,
  (is_when(Flags) ->
    (must_compile_body(Ctx,Env,Result,[progn,Forms],Code),OutCode = do_when(Flags,Code,Result));
    (Result=[],OutCode=wdmsg(skipping([OP,Flags,Forms])))).

   do_when(Flags,Code,Result):- 
      (is_when(Flags) -> locally_let(sym('sys::*compiler-mode*')=sym(':execute'),Code);Result=[]).
   
% assuem always true (debugging) 
compile_body(Ctx,Env,Result,[OP,_Flags|Forms], Code):-   same_symbol(OP,'eval-when'), !,must_compile_body(Ctx,Env,Result,[progn,Forms],Code).

% Maybe later we'll try something simular?
compile_body(Ctx,Env,Result,[OP,Flags|Forms], Code):-  same_symbol(OP,'eval-when'), !,
 always((
 (member(X,Flags),is_when(X))
  -> must_compile_body(Ctx,Env,Result,
    [let,[[sys_xx_compiler_mode_xx,sys_xx_compiler_mode_xx]],
     [progn,[sys_removef_list_value,sys_xx_compiler_mode_xx,kw_compile_toplevel],
            [sys_insertf_list_value,sys_xx_compiler_mode_xx,kw_execute]|Forms]],Code)
   ; (Result=[],Code = true))).

   f_sys_removef_list_value(Symbol,Value,New):- get_var(Symbol,Was),delete(Was,Value,New),set_var(Symbol,New).
   f_sys_insertf_list_value(Symbol,Value,New):- get_var(Symbol,Was),list_to_set([Value|Was],New),set_var(Symbol,New).
   % COMPILE-TOPLEVEL LOAD-TOPLEVEL EXECUTE
   is_when(List):- is_list(List),!,member(KW,List),is_when(KW).
   is_when(kw_eval):- !,is_when(kw_execute).
   is_when(kw_compile):- !,is_when(kw_compile_toplevel).
   is_when(kw_load):- !,is_when(kw_load_toplevel).
   %is_when(X):- lmsg(warn(free_pass(is_when(X)))).
   is_when(X):- get_var(sys_xx_compiler_mode_xx,List),
     (is_list(List)->memberchk(X,List);X=List).
   % makes  sys_xx_compiler_mode_xx 
   wl:interned_eval("(defparameter sys::*compiler-mode* :execute)").
   

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

compile_body(Ctx,Env,Result,[M|MACROLEFT], Code):- atom(M),
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
compile_body(Ctx,Env,Result,[CONDIF|Rest], Code):- 
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
compile_body(Ctx,Env,Result,[BinOP,Form1|Form2],Code):- binary_macro(BinOP,_),
  Form2\=[_],Form2\=[BinOP|_], !, 
  compile_body(Ctx,Env,Result1,Form1,Code1),
  compile_body(Ctx,Env,Result2,[BinOP|Form2],Code2),
  compile_body(Ctx,Env,Result,[BinOP,Result1,Result2],Code3),
  Code = (Code1,Code2,Code3).

% EXT::XOR
binary_macro(ext_xor,[]).
compile_body(Ctx,Env,Result,[ext_xor,Form1,Form2],Code):-
  compile_body(Ctx,Env,Result1,Form1,Code1),
  compile_body(Ctx,Env,Result2,Form2,Code2),
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
compile_body(_Ctx,_Env,[],[or], true).
% OR-1
compile_body(Ctx,Env,Result,[or,Form], Body):- must_compile_body(Ctx,Env,Result,Form, Body).
% OR-2+
compile_body(Ctx,Env,Result,[or,Form1|Form2],Code):- Form2\=[_],Form2\=[or|_], !, 
  compile_body(Ctx,Env,Result,[or,Form1,[or|Form2]],Code).

compile_body(Ctx,Env,Result,[or,Form1,Form2],Code):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1),
   must_compile_body(Ctx,Env,Result2,Form2, Body2),
   debug_var("FORM1_Res",Result1),
        Code = (	(Body1,Result1 \== [],Result = Result1) 		
				-> 	true
				;  	(Body2, Result = Result2)).

% OR-2 needs to use body compiler below
compile_body(Ctx,Env,Result,[or,Form1,Form2],Code):- !,
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

% PROGN
compile_body(Ctx,Env,Result,[progn,Forms], Body):- !, compile_forms(Ctx,Env,Result,[Forms],Body).
compile_body(Ctx,Env,Result,[progn|Forms], Body):- !, compile_forms(Ctx,Env,Result,Forms,Body).

% =============================================================================
% PROGV 
% =============================================================================

% PROGV    % ( progv ' ( a ) ` ( , ( + 1 1 ) ) a ) => 2
compile_body(Ctx,Env,Result,[progv,VarsForm,ValuesForm|FormS],Code):- !,
   must_compile_body(Ctx,Env,VarsRs,VarsForm,Body1),
   must_compile_body(Ctx,Env,ValuesRs,ValuesForm,Body2),
   compile_forms(Ctx,Env,Result,FormS,BodyS),
   Code = (Body1, Body2 , maplist(bind_dynamic_value(Env),VarsRs,ValuesRs), BodyS).

% PROG
/*(defmacro prog (inits &rest forms)
  `(block nil
    (let ,inits
      (tagbody ,@forms))))
*/
compiler_macro_left_right(prog,[Vars|Forms], [block,[],[let,Vars,[tagbody|Forms]]]).


% =============================================================================
% = LET/FLET = 
% =============================================================================
% LET
compile_body(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body):- assertion(is_list(NewBindingsIn)),!,
 always(compile_let(Ctx,Env,Result,[let, NewBindingsIn| BodyForms], Body)).
% LET*
compile_body(Ctx,Env,Result,[OP, []| BodyForms], Body):- same_symbol(OP,'let*'), !, 
  must_compile_body(Ctx,Env,Result,[progn| BodyForms], Body).
compile_body(Ctx,Env,Result,[OP, [Binding1|NewBindings]| BodyForms], Body):- same_symbol(OP,'let*'),
   always(compile_let(Ctx,Env,Result,['let', [Binding1],[progn, [OP, NewBindings| BodyForms]]], Body)).
% EXT:LETF
compile_body(Ctx,Env,Result,[ext_letf, NewBindingsIn| BodyForms], Body):- assertion(is_list(NewBindingsIn)),!,
 always(compile_let(Ctx,Env,Result,[ext_letf, NewBindingsIn| BodyForms], Body)).
% EXT:LETF*
compile_body(Ctx,Env,Result,[OP, []| BodyForms], Body):- same_symbol(OP,'ext_letf*'), !, 
  must_compile_body(Ctx,Env,Result,[progn| BodyForms], Body).
compile_body(Ctx,Env,Result,[OP, [Binding1|NewBindings]| BodyForms], Body):- same_symbol(OP,'ext_letf*'),
   always(compile_let(Ctx,Env,Result,['ext_letf', [Binding1],[progn, [OP, NewBindings| BodyForms]]], Body)).

% =============================================================================
% = WITH SLOTS = 
% =============================================================================

compile_body(Ctx,Env,Result,[with_slots,Slots,Obj|Progn],Body):- 
 always(is_list(Slots)),!,
 slot_object_lets(Obj,Slots,Lets),
 compile_body(Ctx,Env,Result,[let,Lets|Progn],Body).

   % slot_object_lets(Obj,Slots,Lets)
   slot_object_lets(_Obj,[],[]).
   slot_object_lets(Obj,[S|Slots],[[S,['slot_value',Obj,[quote,S]]]|Lets]):-
     slot_object_lets(Obj,Slots,Lets).

% =============================================================================
% = DOLIST = 
% =============================================================================

% DOLIST
compile_body(Ctx,Env,Result,['dolist'|Rest], Code):- !,
  always(compile_dolist(Ctx,Env,Result,['dolist'|Rest], Code)).

wl: init_args(1, cl_dolist).
wl: declared(cl_dolist,inlined).
cl_dolist(VarList,FormS,Result):-
   compile_dolist(_Ctx,_Env,Result,['dolist',VarList|FormS], Code),
   always(Code).
   
   compile_dolist(Ctx,Env,Result,['dolist',[Var,List,RetVar]|FormS], Code):-
      must_compile_body(Ctx,Env,Result,[let,[[RetVar,RetVar]],['dolist',[Var,List]|FormS],RetVar], Code).
   
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
compile_body(Ctx,Env,Result,[compile|Forms], Body):- !,
   must_compile_closure_body(Ctx,CompileEnvironment,CompileResult,[progn|Forms],  CompileBody),
   
   debug_var('LResult',CompileResult),
   debug_var('CompileEnvironment',CompileEnvironment),
   Result = closure([CompileEnvironment|Env],CompileResult,[],CompileBody),
   Body = true.


% =============================================================================
% = Multiple Values = 
% =============================================================================
% VALUES (r1 . rest )
compile_body(Ctx,Env,Result,['values',R1|EvalList], (ArgBody,Body)):-!,
    expand_arguments(Ctx,Env,funcall,0,[R1|EvalList], ArgBody, [Result|Results]),
    Body = nb_setval('$mv_return',[Result|Results]).
compile_body(_Ctx,_Env,[],['values'], nb_setval('$mv_return',[])):-!.

:- nb_setval('$mv_return',[]).

% Macro MULTIPLE-VALUE-BIND
compile_body(Ctx,Env,Result,[OP,Vars,Eval1|ProgN], Body):- same_symbol(OP,'multiple-value-bind'),
  %maplist(maybe_special_letvars,VarNames,VarNamesSpecials),
  must_compile_body(Ctx,Env,Result,[let,Vars,[progn,Eval1,['#setqFromValues',Vars]|ProgN]],Body).

  
%maybe_special_letvars(Var,[Var,[if,[boundp,[quote,Var]],[get_var,Var],[]] ]).

% Macro MULTIPLE-VALUE-LIST
compile_body(Ctx,Env,Result,[OP,Eval1], (Body,nb_current('$mv_return',Result))):-
  same_symbol(OP,'multiple-value-list'),
  debug_var('MV_RETURN',Result),
  debug_var('IgnoredRet',IResult),
  must_compile_body(Ctx,Env,IResult,Eval1,Body).

% Macro MULTIPLE-VALUE-CALL
compile_body(Ctx,Env,Result,[OP,Function|Progn], Body):-
  same_symbol(OP,'multiple-value-call'),
  must_compile_body(Ctx,Env,Result,[progn,[progn|Progn],['apply',Function,['#returnFomLastValues']]],Body).

% synthetic RETURN-VALUES -> values
compile_body(_Ctx,_Env,Values,['#returnFomLastValues'], nb_current('$mv_return',Values)).

% synthetic SETQ-VALUES (vars*)
compile_body(_Ctx,Env,[],['#setqFromValues',Vars], setq_from_values(Env,Vars)):-!.
   setq_from_values(Env,Vars):- nb_current('$mv_return',Values),setq_from_values_each(Env,Vars,Values).
   setq_from_values_each(_Env,_,[]):- !. %lisp_dump_break,!.
   setq_from_values_each(_Env,[],_):-!.
   setq_from_values_each(Env,[Var|Vars],[Value|Values]):-
      set_var(Env,Var,Value),
      setq_from_values_each(Env,Vars,Values).


% =============================================================================
% EVAL
% =============================================================================
compile_body(_Ctx,_Env,Result,[eval,Var], cl_eval(Var,Result)):- \+ is_list(Var), !. 
compile_body(_Ctx,_Env,Result,[eval,[A|Var]], cl_eval([A|Var],Result)):- \+ atom(A), !. % NEVER SEEN
compile_body(_Ctx,_Env,Result,[eval,Var], cl_eval(Var,Result)):- !.
compile_body(Ctx,Env,Result,['eval',Form1],
  (Body1,cl_eval(Result1,Result))):- !,
   must_compile_body(Ctx,Env,Result1,Form1, Body1).

% =============================================================================
% = LAMBDA/CLOSURES = 
% =============================================================================

% (function (lambda ... ))
compile_body(Ctx,_Env,Result,POrSTerm, Body):- p_or_s(POrSTerm,function,[[lambda,LambdaArgs| LambdaBody]]),
      !,
      must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
      debug_var('LArgs',LambdaArgs),
      debug_var('LResult',ClosureResult),
      debug_var('LEnv',ClosureEnvironment),
   debug_var('Function',Result),
   Result = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody),
   Body = true.

% (lambda ...)
compile_body(Ctx,Env,Result,[lambda,LambdaArgs|LambdaBody], Body):-
	!,
	must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
   debug_var('LArgs',LambdaArgs),
   debug_var('LResult',ClosureResult),
   debug_var('Lambda',Result),
   debug_var('ClosureEnvironment',ClosureEnvironment),
   Body =
     (Result = closure([ClosureEnvironment|Env],ClosureResult,LambdaArgs,ClosureBody)).

% ((lambda ...) ...)
compile_body(Ctx,_Env,Result,[POrSTerm|ARGS],Body):- 
   p_or_s(POrSTerm,lambda,[LambdaArgs|LambdaBody]),
   must_compile_closure_body(Ctx,ClosureEnvironment,ClosureResult,[progn|LambdaBody],  ClosureBody),
    ClosureResult = Result,debug_var('ClosureEnv',ClosureEnvironment),debug_var('ClosureResult',Result),
   Body = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody,ARGS,Result).


% (function .)
compile_body(_Cx,_Ev,function(Function),POrSTerm, true):- p_or_s(POrSTerm,function,[Function]).
% ((function .) ...)
compile_body(Ctx,Env,Result,[POrSTerm|ARGS],Body):- p_or_s(POrSTerm,function,[Function]),
  compile_body(Ctx,Env,Result,[Function|ARGS],Body).


% (closure ...)
compile_body(_Ctx,_Env,Result,POrSTerm,Body):- 
   p_or_s(POrSTerm,closure,[ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody]),
   debug_var('Closure',Result),debug_var('ClosureResult',ClosureResult),
   Body = (Result = closure(ClosureEnvironment,ClosureResult,LambdaArgs,ClosureBody)).
% ((closure ...) ...)
compile_body(_Ctx,_Env,Result,[POrSTerm|ARGS],Body):- 
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
compile_body(Ctx,Env,Result,[BinOP,Form1,Form2,Form3|FormS],Code):- fail, binop_identity(BinOP,_Identity),
  compile_body(Ctx,Env,Result,[BinOP,[BinOP,Form1,Form2],Form3|FormS],Code),!.
  
% BinOP-3+
compile_body(Ctx,Env,Result,[BinOP,Form1,Form2,Form3|FormS],Code):- binop_identity(BinOP,_Identity),
  compile_body(Ctx,Env,Result1,[BinOP,Form1,Form2],Code1),
  %rw_add(Ctx,Result1,w),
  freeze(Result1,var(Result1)),
  compile_body(Ctx,Env,Result,[BinOP,Result1,Form3|FormS],Code2),
  Code = (Code1,Code2).


% =============================================================================
% INTERFACES
% =============================================================================

% Use a previous DEFMACRO
compile_body(Ctx,Env,Result,LispCode,CompileBody):-
  % fail, %DISABLED    (NOT)
  macroexpand_1_or_fail(LispCode,[],CompileBody0Result),
  lmsg(macroexpand:-LispCode),
  lmsg(into:-CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody),
  lmsg(code:-CompileBody),
  !.

% Compiler Plugin
compile_body(Ctx,Env,Result,InstrS,Code):-
  shared_lisp_compiler:plugin_expand_progbody(Ctx,Env,Result,InstrS,_PreviousResult,Code),!.


% example of making CONS inine
shared_lisp_compiler:plugin_expand_progbody(Ctx,Env,Result,[cons, IN1,IN2],_, Body):- 
  \+ current_prolog_flag(lisp_inline,false), !,
        must_compile_body(Ctx,Env,MID1,IN1,  ValueBody1),
        must_compile_body(Ctx,Env,MID2,IN2,  ValueBody2),
        Body = (ValueBody1,ValueBody2,Result=[MID1|MID2]).



% DEFMACRO,MACROLET,MACROEXPAND
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_macro_ops(Ctx,Env,Result,BodyForms, Body),!.

% DEFUN,FSET,LABELS
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_defun_ops(Ctx,Env,Result,BodyForms, Body),!.

% DEFMETHOD,DEFGENRIC
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_genericfs(Ctx,Env,Result,BodyForms, Body),!.


% symbols
compile_body(Ctx,Env,Value, Atom,      Body):- atom(Atom), always(compile_symbol_getter(Ctx,Env,Value, Atom, Body)).

%compile_body(Ctx,Env,Result,BodyForms, Body):- atom(BodyForms),!,always(compile_assigns(Ctx,Env,Result,BodyForms, Body)),!.

% SETF - PSETF
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_setfs(Ctx,Env,Result,BodyForms, Body),!.

% SETQ - PSET
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_assigns(Ctx,Env,Result,BodyForms, Body),!.

% FUNCALL,EVAL,APPLY, RestOf
compile_body(Ctx,Env,Result,BodyForms, Body):- always(compile_funop(Ctx,Env,Result,BodyForms, Body)),!.


:- fixup_exports.

