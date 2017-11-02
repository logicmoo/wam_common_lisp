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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog (YAP 4x faster).
 *
 *******************************************************************/
:- module(loadfile, []).
:- set_module(class(library)).
:- include('header.pro').



cl:load(File,t):- with_lisp_translation(File,writeExpression).

cl:load_compile(file(File),R):-!,cl:load_compile(File,R).
cl:load_compile(File,t):- exists_file(File),!,with_lisp_translation(File,lisp_compile).
cl:load_compile(Dir,R):- exists_directory(Dir),!,directory_file_path(Dir,'*.lisp',Mask),!, cl:load_compile(Mask,R).
cl:load_compile(Mask,t):- expand_file_name(Mask,List),List\==[Mask],!,maplist(cl:load_compile1,Mask).
cl:load_compile(File,t):- with_lisp_translation(File,lisp_compile).

cl:load_compile1(File):- cl:load_compile(file(File),t).


% cl:load_compile(File,t):- with_lisp_translation(File,lisp_compiled_eval).




