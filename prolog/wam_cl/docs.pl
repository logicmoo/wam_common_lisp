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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(docs, []).

:- set_module(class(library)).

:- include('header.pro').

maybe_get_docs(Type,Name,[String|FunctionBody],FunctionBody,Code):- string(String),!,
  Code = asserta(doc:doc_string(Name,_Package,Type,String)).
maybe_get_docs(_Type,_Name,FunctionBody,FunctionBody, true).

:- fixup_exports.

