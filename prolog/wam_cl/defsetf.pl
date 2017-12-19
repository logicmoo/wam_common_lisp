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
 * The program is a *HUGE* common-lisp compiler/interpreter. 
 *
 *******************************************************************/
:- module(s2tf, []).

:- set_module(class(library)).

:- include('header').

is_setf_op([setf|Accessor],Accessor):- nonvar(Accessor).

wl:init_args(2,X):- at_least_two_args(X).

combine_setfs(Name0,Name):-atom(Name0),!,Name0=Name.
combine_setfs([setf,Name],Combined):- atomic_list_concat([setf,Name],'_',Combined).
combine_setfs([setf,Name],Combined):- atomic_list_concat([setf,Name],'_',Combined).


compile_setfs(_Ctx,_Env,Symbol,[Function,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- notrace(at_least_two_args(Function)),\+ is_fboundp(Function),!,P=..[Function,Symbol,A2,AMORE].

compile_setfs(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- notrace((at_least_two_args(Function),same_symbol(Function,Fun0))),\+ is_fboundp(Function),!,P=..[Function,Symbol,A2,AMORE].


at_least_two_args(define_compiler_macro).
at_least_two_args(defsetf).
at_least_two_args(deftype).
at_least_two_args(symbol_macrolet).
at_least_two_args(define_setf_expander).


:- fixup_exports.


