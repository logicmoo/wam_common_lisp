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
:- module(genr, []).



:- include('./header').

compile_genericfs(_Ctx,_Env,Symbol,[Function,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- 
   notrace(defgen(Function)),\+ is_implemented(Function),!,P=..[Function,Symbol,A2,AMORE].
compile_genericfs(_Ctx,_Env,Symbol,[Fun0,Symbol,A2|AMORE],assert_lsp(Symbol,P)):- 
   notrace((defgen(Function),same_symbol(Function,Fun0))),\+ is_implemented(Function),!,P=..[Function,Symbol,A2,AMORE].

wl:init_args(2,X):- defgen(X).

defgen(defgeneric).
defgen(define_method_combination).
defgen(defmethod).

:- fixup_exports.


