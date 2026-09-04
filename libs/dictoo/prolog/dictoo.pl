:- if((  % FYI, this "if" prevents this file from getting autoload support
       \+ current_prolog_flag(xref, true),
       Type = core,
         prolog_load_context(module, SM),
         (prolog_load_context(file, This), unload_file(This)),       
         INFO = dot_cfg:using_dot_type(Type,SM),
         (clause(INFO,true)->true;asserta(INFO)),
         % debug(dictoo(Type),'~N% ~w~n',[INFO]),
         format(user_error,'~N% ~w~n',[INFO]))).
:- endif.
:- module(dictoo, [  ]).
:- op(2,fx,prolog:'?').

:- multifile(dictoo:is_dot_hook/4).
:- dynamic(dictoo:is_dot_hook/4).
:- module_transparent(dictoo:is_dot_hook/4).
:- multifile(dictoo:dot_overload_hook/4).
:- dynamic(dictoo:dot_overload_hook/4).
:- module_transparent(dictoo:dot_overload_hook/4).


/** <module> dictoo - Dict-like OO Syntax Pack

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2017
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- set_module(class(library)).
:- multifile(dot_cfg:using_dot_type/2).
:- dynamic(dot_cfg:using_dot_type/2).
:- export(dot_cfg:using_dot_type/2).

:- multifile(dot_cfg:dictoo_decl/8).
:- dynamic(dot_cfg:dictoo_decl/8).
:- discontiguous(dot_cfg:dictoo_decl/8).
:- system:use_module(library(logicmoo_common)).

:- dot_cfg:using_dot_type(core,SM),
   SM:use_module(library(dictoo_lib)).

% :- verbose_expansion(on).

