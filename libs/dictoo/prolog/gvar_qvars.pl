
:- if((  % FYI, this "if" prevents this file from getting autoload support
       \+ current_prolog_flag(xref, true),
       Type = qvars,
         prolog_load_context(module, SM),
         (prolog_load_context(file, This), unload_file(This)),       
         INFO = dot_cfg:using_dot_type(Type,SM),
         (clause(INFO,true)->true;asserta(INFO)),
         % debug(dictoo(Type),'~N% ~w~n',[INFO]),
         format(user_error,'~N% ~w~n',[INFO]))).
:- endif.
:- module(qvars,[op(3,fx,?)]).

:- set_module(class(library)).
:- multifile(dot_cfg:using_dot_type/2).
:- dynamic(dot_cfg:using_dot_type/2).
:- dot_cfg:using_dot_type(qvars,SM),!,
   SM:use_module(library(gvar_lib)).



