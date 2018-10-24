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
:- module(comp, []).
:- set_module(class(library)).
:- include('./header').

% :- use_module(library(pce)).

lisp_eval(SExpression):- lisp_compiled_eval(SExpression),!.

lisp_compiled_eval(SExpression):-
  quietly(as_sexp_interned(SExpression,Expression)),
  lisp_compiled_eval(Expression,Result),
  userout(result(Result)),!.

lisp_compiled_eval(SExpression,Result):-
  lquietly(as_sexp_interned(SExpression,Expression)),
  %dbginfo(lisp_compiled_eval(Expression)),
  always(lisp_compile(Result,Expression,Code)),  
  % dbginfo((lisp_compiled_eval(Expression):- Code)),
  (always((Code))),!.

%lisp_compile(SExpression):- source_location(_,_),!,dbginfo((:-lisp_compile(SExpression))).
lisp_compile(SExpression):-
  quietly(as_sexp_interned(SExpression,Expression)),
  userout(:- lisp_compile(Expression)),
  lisp_compile(Expression,Code),!,
  userout(:- Code).

lisp_compile(SExpression,Body):-
   quietly(as_sexp_interned(SExpression,Expression)),
   debug_var('_Ignored',Result),
   lisp_compile(Result,Expression,Body).

lisp_compile(Result,SExpression,Body):-
   %debug_var('TLEnv',Env),
   lisp_compile(_Env,Result,SExpression,Body),!.

lisp_compile(Env,Result,Expression,Body):-
   always(lisp_compile(_Ctx,Env,Result,Expression,(Body))).

lisp_compile(Ctx,Env,Result,SExpression,BodyO):-
   quietly(as_sexp(SExpression,Expression)),
   always(must_compile_progn(Ctx,Env,Result,[Expression],Body)),!,
   body_cleanup_full(Ctx,Body,BodyO),!.
   

:- nop( debug_var('FirstForm',Var)),
   nb_linkval('$compiler_PreviousResult',the(Var)).


compile_each(_Ctx,_Env,[],[],true).
compile_each(Ctx,Env,[VarR|Result],[Var|Eval],Code):-
  must_compile_body(Ctx,Env,VarR,Var,Code0),
  compile_each(Ctx,Env,Result,Eval,Code1),
  conjoin_0(Ctx,Code0,Code1,Code),!.

compile_each_quoted(_Ctx,_Env,[],[],true).
compile_each_quoted(Ctx,Env,RPARAMS,CPARAMS,Code):-
  compile_each(Ctx,Env,PARAMS,CPARAMS,Code1),
  quotify_each(Ctx,Env,RPARAMS,PARAMS,Code2),
  Code = (Code1,Code2),!.

quotify_each(_Ctx,_Env,[],[],true).
quotify_each(Ctx,Env,[VarR|Result],[Var|Eval],Code):-
  must_quotify(Ctx,Env,VarR,Var,Code0),
  quotify_each(Ctx,Env,Result,Eval,Code1),
  conjoin_0(Ctx,Code0,Code1,Code),!.

must_quotify(_Ctx,_Env,SelfEval,SelfEval,true):- var(SelfEval),!.
must_quotify(_Ctx,_Env,SelfEval,SelfEval,true):- quietly(is_self_evaluating_object(SelfEval)),!.
must_quotify(_Ctx,_Env,[quote,Var],Var,true).


% Operator
expand_arguments_maybe_macro(Ctx,Env,FN,_N,MacroArgs,MacroArgs,true):- nonplainvar(FN), is_lisp_operator(Ctx,Env,FN),!.
expand_arguments_maybe_macro(Ctx,Env,FN,N,Args,MacroArgs,ArgBody):-
  expand_arguments(Ctx,Env,FN,N,Args,MacroArgs,ArgBody),!.


% The idea here is that FN/ArgNum may need evaluated or may have its own special evaluator 
expand_arguments(_Ctx,_Env,_FunctionName,_ArgNum,[], [], true):-!.
expand_arguments(Ctx,Env,FN,_, ArgsO, Args, true):- nonvar(FN), is_lisp_operator(Ctx,Env,FN),!,Args=ArgsO.
expand_arguments(Ctx,Env,FN,0,[Arg|Results],[Arg|Args], ArgsBody):- atom(Arg),!,
    expand_arguments(Ctx,Env,FN,1,Results,Args, ArgsBody).
expand_arguments(Ctx,Env,FN,ArgNum,[Result|Results],[Arg|Args], Body):-!,
       must_compile_body(Ctx,Env,Result,Arg, ArgBody),
       Body = (ArgBody, ArgsBody),
       ArgNum2 is ArgNum + 1,
       expand_arguments(Ctx,Env,FN,ArgNum2,Results,Args, ArgsBody).





must_compile_progn(Ctx,Env,Result,FunctionBody,Code):-
   ensure_ctx(Ctx),%ensure_env(Env),
   must_compile_progn(Ctx,Env,Result,FunctionBody, [], Body),!,
   body_cleanup_keep_debug_vars(Ctx,Body,Code).

must_compile_progn(Ctx,Env,Result,FormsIn, PreviousResult, Body):-
  %quietly((maybe_debug_var('_rCtx',Ctx),
  %maybe_debug_var('_rEnv',Env),
  %maybe_debug_var('_rResult',Result),
  %maybe_debug_var('_rPrevRes',PreviousResult),
 % maybe_debug_var('_rForms',Forms),
  %maybe_debug_var('_rBody',Body))),
  lquietly(resolve_reader_macros(FormsIn,Forms)),!,
   always(((compile_progn(Ctx,Env,Result,Forms,PreviousResult,Body0),nonvar(Body0)))),
   lquietly((sanitize_true(Ctx,Body0,Body))).

compile_progn(_Cx,_Ev,Result,Var,_PreviousResult,Out):- quietly(is_ftVar(Var)),!,Out=f_eval([progn|Var],Result).
compile_progn(_Cx,_Ev,Result,[], PreviousResult,true):-!, PreviousResult = Result.
compile_progn(Ctx,Env,Result,[Form | Forms], PreviousResult, Body):-  !,
	must_compile_progbody(Ctx,Env,FormResult, Form,PreviousResult,FormBody),
	must_compile_progn(Ctx,Env,Result, Forms, FormResult, FormSBody),
        Body = (FormBody,FormSBody).
compile_progn(Ctx,Env,Result, Form , PreviousResult, Body):-
        % locally(
  % local_override('$compiler_PreviousResult',the(PreviousResult)),
       must_compile_progbody(Ctx,Env,Result,Form,PreviousResult, Body).


% Compiler Plugin
must_compile_progbody(Ctx,Env,Result,Form,PreviousResult,FormBody):-  
	wl:plugin_expand_progbody(Ctx,Env,Result,Form,PreviousResult,FormBody),!.
must_compile_progbody(Ctx,Env,Result,Form,_PreviousResult,FormBody):-
        % locally(
  % local_override('$compiler_PreviousResult',the(PreviousResult)),
	must_compile_body(Ctx,Env,Result,Form,FormBody).
        %).



p_or_s([F|Args],F0,Args0):-!,F0=F,Args0=Args.
p_or_s(POrSTerm,F,Args):- POrSTerm=..[F|Args].


% compile_body(Ctx,Env,Result,Function, Body).
% Expands a Lisp-like function body into its Prolog equivalent

preserved_var:attr_unify_hook(_,_):- fail.

ensure_assignment(X=Y,true):- X=Y,!.
ensure_assignment(X=Y,reset_mv):- X=Y,!.
ensure_assignment(G,G).

make_restartable_block(Place,CodeWithPlace,LispCode):-
     gensym(restartable_block,GenBlock),gensym(restartable_loop,GenLoop),
     LispCode = [ block, GenBlock,
                    [ tagbody,GenLoop,
                      [ restart_bind,[[ store_value,[ lambda,[u_object],[setf, Place, u_object],[go, GenLoop]]]],
                        [ return_from,GenBlock,[progn|CodeWithPlace] ]]]].



% ===============================================================
% === ENTRY TO COMPILE-BODY ===
% ===============================================================

must_compile_body(_Ctx,_Env,ResultO,LispCode, Body):- var(LispCode), get_attr(LispCode,preserved_var,t),!,true=Body,
   ResultO = LispCode.
must_compile_body(_Ctx,_Env,ResultO,LispCode, Body):- var(LispCode), !,true=Body,
   ResultO = LispCode.

must_compile_body(Ctx,Env,ResultO,LispCode, BodyO):-
  %notrace((maybe_debug_var('_rCtx',Ctx),
  %maybe_debug_var('_rEnv',Env),
  %maybe_debug_var('_rResult',Result),
  %maybe_debug_var('_LispCode',LispCode),
  %maybe_debug_var('_rBody',Body))),
  quietly(resolve_reader_macros(LispCode,Forms)),!,
  always((compile_body(Ctx,Env,Result,Forms, Body9)->nonvar(Body9))),
  must_compile_body_pt2(Ctx,Env,Result,ResultO,Forms, Body9,BodyO).

must_compile_body_pt2(_Ctx,_Env,Result,ResultO, _Forms, true,Body):- ensure_assignment(Result=ResultO,Body),!.
must_compile_body_pt2(Ctx,_Env,Result,ResultO,_Forms, Body9,BodyO):-
  body_cleanup_no_optimize(Ctx,Body9,Body),
  ((Body==true,fail) -> BodyO=(ResultO=Result) ; (ResultO=Result,BodyO=Body)),
  % nb_current('$compiler_PreviousResult',THE),setarg(1,THE,Result),
  !.

:- discontiguous(compile_body/5).
% Prolog vars
% Compiler Plugin

compile_body(Ctx,Env,Result,In1,Code):-
  clause(wl:plugin_expand_progbody_1st(Ctx,Env,Result,In2,_PreviousResult,Code),Body),
  structure_applies_here(In1,In2,Body),!.

compile_body(_Ctx,_Env,Result,Var, true):- Result == Var,!.
compile_body(_Ctx,_Env,ResultO,LispCode, Body):- var(LispCode), get_attr(LispCode,preserved_var,t),!,true=Body,
   ResultO = LispCode.
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
compile_body(_Cx,_Ev,Result,SelfEval,Body):- quietly(is_self_evaluating_object(SelfEval)),!,
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
% ``,,(cons 1 1)
%  `,`,(cons 1 1)

compile_body(Ctx,Env,Result,['#BQ',Form], Code):-!,get_bqd(BQD),compile_bq(Ctx,BQD,Env,Result,Form,Code),!.
compile_body(Ctx,Env,Result,['`',Form], Code):-!,compile_body(Ctx,Env,Result,['#BQ',Form], Code).

compile_body(_Ctx,_Env,['#COMMA',Form],['#COMMA',Form],true).
/*
% error
compile_body(Ctx,Env,Result,['#COMMA',Form], (Code,f_eval(CommaResult,Result))):-!,compile_body(Ctx,Env,CommaResult,Form,Code),!.
% error
compile_body(Ctx,Env,Result,['#BQ-COMMA-ELIPSE',Form], (Code,f_eval(CommaResult,Result))):- 
  % lisp_dump_trace,
  !,compile_body(Ctx,Env,CommaResult,Form,Code),!.
*/

compile_body(_Ctx,_Env,_Result,[Var|_], _Body):- var(Var),!,lisp_dump_break.

% =============================================================================
% Conditonal  evaluation/compilation
% =============================================================================

compile_body(_Ctx,_Env,[],COMMENT,nop(COMMENT)):- is_comment(COMMENT,_),!.

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
    (Result=[],OutCode=dbginfo(skipping([OP,Flags,Forms])))).

   do_when(Flags,Code,Result):- 
      (is_when(Flags) -> locally_let(sym('sys::*compiler-mode*')=sym(':execute'),Code);Result=[]).
   
% assume always true (debugging) 
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
   %is_when(X):- dbginfo(warn(free_pass(is_when(X)))).
   is_when(X):- get_var(sys_xx_compiler_mode_xx,List),
     (is_list(List)->memberchk(X,List);X=List).
   % makes  sys_xx_compiler_mode_xx 
 wl:interned_eval("(defparameter sys::*compiler-mode* :execute)").



% =============================================================================
% = COMPILE =  ( file with several other debugging tools?)
% =============================================================================

% (compile ...)
compile_body(Ctx,Env,Result,[compile|Forms], Body):- !,
   must_compile_closure_body(Ctx,CompileEnvironment,CompileResult,[progn|Forms],  CompileBody),
   
   debug_var('LResult',CompileResult),
   debug_var('CompileEnvironment',CompileEnvironment),
   % ClosureEnvironment,Whole,ClosureResult,FormalParms,ClosureBody,Symbol,ActualParams,ClosureResult
   Result = closure(kw_function,[CompileEnvironment|Env],CompileResult,[],CompileBody),
   Body = true.

plist_to_names_values([],[],[]).
plist_to_names_values([Name,Value|Keys],[Name|Names],[Value|Values]):-  
   plist_to_names_values(Keys,Names,Values).

% (lcompile ...)
%wl:interned_eval("(sys:set-opv `SYS:LCOMPILE :compile-as :function)").
:- multifile(wl:init_args/2).
:- dynamic(wl:init_args/2).
:- discontiguous(wl:init_args/2).
wl:init_args(1,sys_lcompile).
f_sys_lcompile(Form,Keys, ResultO):- 
   lisp_compile(Ctx,Env,FormValue,Form,Part1),
   plist_to_names_values(Keys,Names,Values),
   maplist(f_sys_get_wam_cl_option,Names,Was),
   maplist(f_sys_set_wam_cl_option,Names,Values),
   always(Part1),
   Part2 = must_compile_body(Ctx,CompileEnvironment,CompileResult,FormValue, CompileBody),
  % ignore(CompileEnvironment = Env),
   debug_var('LResult',CompileResult),debug_var('CResult',CResult),debug_var('CompileEnvironment',CompileEnvironment),
   Part3 = body_cleanup_full(Ctx,( (CompileEnvironment = Env,CompileBody,CResult=CompileResult)),Opt),
   Whole = [sys_lcompile,Form|Keys],
   Symbol = sys_lcompile,
   % closure(FType,ClosureEnvironment,Whole,Result,FormalParms,ClosureBody,Symbol,ActualParams,ResultO)
   ResultO = closure(kw_function,[CompileEnvironment|Env],Whole,CompileResult,[],Opt,Symbol),   
   Body = (nl,nl,Part1,Part2,Part3,cmpout(:- Opt),maplist(f_sys_set_wam_cl_option,Names,Was)),
   always(Body),

% (lcompilen ...)
% wl:interned_eval("(sys:set-opv `SYS:LCOMPILEN :compile-as :function)").
wl:init_args(1,sys_lcompile).
f_sys_lcompilen(Form,Forms, Result):- 
  ((append(Progn,[KW|More],Forms),is_keywordp(KW))->Keys=[KW|More];(Progn=Forms,Keys=[])),
    f_sys_lcompile([progn,Form|Progn],Keys,Result).

%compile_body(Ctx,Env,Result,[sys_lcompilen|Forms], Body):- !,compile_body(Ctx,Env,Result,[sys_lcompile,[progn|Progn]|Keys], Body).


compile_body(Ctx,Env,Result,Form1,Body):- compile_body_form(Ctx,Env,Result,Form1,Body).


% =============================================================================
% INTERFACES
% =============================================================================

compile_body(Ctx,Env,Result,Form1,Body):- compile_closures(Ctx,Env,Result,Form1,Body),!.


% Use a previous DEFMACRO
compile_body(Ctx,Env,Result,LispCode,CompileBody):-
  fail, %DISABLED
  macroexpand_1_or_fail(Ctx,Env,LispCode,CompileBody0Result),
  dbginfo(macroexpand:-LispCode),
  dbginfo(into:-CompileBody0Result),
  must_compile_body(Ctx,Env,Result,CompileBody0Result, CompileBody),
  !.


wl:declared(=,binpred).
wl:declared(<,binpred).
wl:declared(>,binpred).
wl:declared('<=',binpred).
wl:declared('>=',binpred).

% BinPRED-1   (< 1)
compile_body(Ctx,Env,t,[BinPRED,Form],Code):- wl:declared(BinPRED,binpred),
   must_compile_body(Ctx,Env,_Result,Form,Code),!.
% BinPRED-3+  (<  1 2 3 ...)
compile_body(Ctx,Env,Result,[BinPRED,Form1,Form2,Form3|FormS],Code):- wl:declared(BinPRED,binpred),
  must_compile_body(Ctx,Env,Result,[and,[BinPRED,Form1,Form2],[BinPRED,Form2,Form3|FormS]],Code).



% Compiler Plugin
compile_body(Ctx,Env,Result,InstrS,Code):-
  wl:plugin_expand_progbody(Ctx,Env,Result,InstrS,_PreviousResult,Code),!.


% example of making CONS inine
wl:plugin_expand_progbody(Ctx,Env,Result,[cons, IN1,IN2],_, Body):- 
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


% symbols (TODO need to resolve symbol-macros)
compile_body(Ctx,Env,Value, Atom,      Body):- atom(Atom), always(compile_symbol_getter(Ctx,Env,Value, Atom, Body)).

% SETQ - PSETQ
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_symbol_setter(Ctx,Env,Result,BodyForms, Body),!.

% SETF, INCF, PUSH - PUSHNEW
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_accessors(Ctx,Env,Result,BodyForms, Body),!.

% PROCLAIM - SET-DOCUMENTATION
compile_body(Ctx,Env,Result,BodyForms, Body):- compile_direct_assertions(Ctx,Env,Result,BodyForms, Body),!.

% FUNCALL,EVAL,APPLY, and everything else
compile_body(Ctx,Env,Result,BodyForms, Body):- 
   always(once(compile_funop(Ctx,Env,Result,BodyForms, Body))),!.



wl:init_args(2,X):- at_least_two_args(X).

at_least_two_args(define_compiler_macro).
at_least_two_args(defsetf).
at_least_two_args(deftype).
at_least_two_args(symbol_macrolet).
at_least_two_args(define_setf_expander).

compile_direct_assertions(_Ctx,_Env,Symbol,[Function,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- quietly(at_least_two_args(Function)),
  \+ is_fboundp(Function),!,
   P=..[Function,Symbol,A2,AMORE].

compile_direct_assertions(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- quietly((at_least_two_args(Function),same_symbol(Function,Fun0))),
  \+ is_fboundp(Function),!,P=..[Function,Symbol,A2,AMORE].


% same_symbol(OP1,OP2):-!, OP1=OP2.
same_symbol(OP1,OP2):- quietly(same_symbol0(OP1,OP2)).

%prologcase_name_or_string(S,N):-prologcase_name(S,N).

same_symbol0(OP1,OP2):- var(OP1),var(OP2),trace_or_throw(same_symbol(OP1,OP2)).
same_symbol0(OP1,OP2):- var(OP1),!,same_symbol0(OP2,OP1).
same_symbol0(OP1,OP2):- var(OP2),!,freeze(OP2,((nonvar(OP2),same_symbol(OP1,OP2)))).

same_symbol0(OP1,OP2):- string(OP1),to_prolog_string(OP2,N2),!,OP1==N2.
same_symbol0(OP1,OP2):- string(OP2),!,same_symbol0(OP2,OP1).

same_symbol0(OP1,OP2):- atom(OP1),atom(OP2),!, same_reduced_atoms(OP1,OP2),!.
same_symbol0(P,OP2):- compound(P),!,arg(1,P,OP1),same_symbol0(OP1,OP2).
same_symbol0(OP1,P):- compound(P),!,arg(1,P,OP2),same_symbol0(OP1,OP2).

same_reduced_atoms(X,X).
same_reduced_atoms(X,Y):- reduce_atom(X,XX),X\==XX,!,same_reduced_atoms(XX,Y).
same_reduced_atoms(Y,X):- reduce_atom(X,XX),X\==XX,!,same_reduced_atoms(Y,XX).

reduce_atom(X,XX):- atom(X),reduce_atom0(X,XX),XX\==''.
reduce_atom0(X,XX):- downcase_atom(X,XX)->X\==XX.
%reduce_atom(X,XX):- atom_concat_or_rtrace('%',XX,X).
%reduce_atom(X,XX):- atom_concat_or_rtrace('$',XX,X).
reduce_atom0(X,XX):- prologcase_name(X,XX)->X\==XX.
reduce_atom0(X,XX):- atom_concat_or_rtrace(':',XX,X).
reduce_atom0(X,XX):- atom_concat_or_rtrace('mf_',XX,X).
reduce_atom0(X,XX):- atom_concat_or_rtrace('sf_',XX,X).
reduce_atom0(X,XX):- atom_concat_or_rtrace('f_',XX,X).
/*
reduce_atom(X,XX):- atom_concat_or_rtrace('u_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace('kw_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace('sys_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace('sys_',XX,X).
reduce_atom(X,XX):- atom_concat_or_rtrace(XX,'_mexpand1',X).
*/


:- '$hide'(same_symbol/2).

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
