:- if((  % FYI, this "if" prevents this file from getting autoload support
       \+ current_prolog_flag(xref, true),
       Type = decl,
         prolog_load_context(module, SM),
         (prolog_load_context(file, This), unload_file(This)),       
         INFO = dot_cfg:using_dot_type(Type,SM),
         (clause(INFO,true)->true;asserta(INFO)),
         % debug(dictoo(Type),'~N% ~w~n',[INFO]),
         format(user_error,'~N% ~w~n',[INFO]))).
:- endif.

:- module(dictoo_declarations, 
  [dictoo_decl_te/4,
   use_dictoo_te/5]).

/** <module> dictoo_declarations - OO Overloads term_expansion in files

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
:- reexport(library(dictoo_lib)).
:- multifile(dot_cfg:dictoo_decl/8).
:- dynamic(dot_cfg:dictoo_decl/8).
%:- discontiguous(dot_cfg:dictoo_decl/8).

:- module_transparent(expand_dictoo_decl/5).

expand_dictoo_decl(SM,CM,Head,Body,dot_cfg:dictoo_decl(OP ,SM,CM,M,Var,Memb,Value,Body)):- 
  expand_gvs_head(Head,OP,M,Var,Memb,Value).

  
:- module_transparent(dictoo_decl_te/4).
dictoo_decl_te(SM,M,(Head:-Body),OUT):- expand_dictoo_decl(SM,M,Head,Body,OUT).
dictoo_decl_te(SM,M,(Head),OUT):- expand_dictoo_decl(SM,M,Head,true,OUT).

:- module_transparent(use_dictoo_te/5).
use_dictoo_te(MIN,P,SM,M,IN):-   
   \+ current_prolog_flag(gvs_syntax,false),
   prolog_load_context(module, SM),  
  % (debugging(dictoo(decl))->trace;true),
   dot_cfg:using_dot_type(decl,SM),
   nonvar(P),
   nb_current('$term',Term),MIN==Term,
   strip_module(SM:MIN,M,IN),   
   \+ current_prolog_flag(xref, true),
   compound(IN),
   IN \= (:- _),!.

:- user:import(dictoo_declarations:use_dictoo_te/5).

% :- module_transparent(user:term_expansion/4).
:- multifile(user:term_expansion/4).
:- dynamic(user:term_expansion/4).
user:term_expansion(MIN,P,OUT,PO):-
   notrace(use_dictoo_te(MIN,P,SM,M,IN)),
   (debugging(dictoo(decl))->trace;true),
   M:dictoo_decl_te(SM,M,IN,OUT),
   PO = P.

:- set_prolog_flag(gvs_syntax,false).
