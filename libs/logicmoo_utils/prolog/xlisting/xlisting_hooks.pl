% Hooks that prints additional information about source code
:- module(xlisting_hooks, []).

:- use_module(library(listing)).


%= 	 	 

:- if(true).
%% locate_clauses( ?A, ?OutOthers) is semidet.
%
% Hook To [prolog:locate_clauses/2] For Module Logicmoo_util_term_listing.
% Locate Clauses.
%
:- multifile prolog:locate_clauses/2.
prolog:locate_clauses(A, OutOthers) :- 
 notrace(fail), %unhook for now
 notrace((buggery_ok)),
 ( \+ t_l:in_prolog_locate_clauses(A)),
 locally(t_l:in_prolog_locate_clauses(A),
 (       
   locally(t_l:in_prolog_listing(A),
    ( 
    ignore((predicate_property(baseKB:hook_mpred_listing(A),number_of_clauses(C)),C>0,
      current_prolog_flag(xlisting,true),doall(call_no_cuts(baseKB:hook_mpred_listing(A))))),    
   prolog:locate_clauses(A, OutOthers))))),!.
:- endif.




% Replacement that prints variables in source code

%= 	 	 

% prolog_listing:portray_clause(Stream, Term, M:Options) :- xlisting:prolog_listing_portray_clause(Stream, Term, M:Options).
/*
prolog_listing:list_clause(Head, Body, Ref, Source, Options):-
  prolog_listing:list_clause((Head :- Body), Ref, Source, Options).
prolog_listing:list_clause(M:H, B, R, Source, Options).

list_clause(_Head, _Body, Ref, _Source, Options) :-
    option(source(true), Options),
    (   clause_property(Ref, file(File)),
        clause_property(Ref, line_count(Line)),
        catch(source_clause_string(File,
                                   Line,
                                   String,
                                   Repositioned),
              _,
              fail),
        debug(listing(source),
              'Read ~w:~d: "~s"~n',
              [File, Line, String])
    ->  !,
        (   Repositioned==true
        ->  comment('% From ~w:~d~n', [File, Line])
        ;   true
        ),
        writeln(String)
    ;   decompiled
    ->  fail
    ;   asserta(decompiled),
        comment('% From database (decompiled)~n', []),
        fail
    ).
list_clause(Module:Head, Body, Ref, Source, Options) :-
    restore_variable_names(Module,
                           Head,
                           Body,
                           Ref,
                           Options),
    write_module(Module, Source, Head),
    portray_clause((Head:-Body)).
*/


:- if(false).

%% prolog_listing_list_clauses( ?Pred, ?Source) is semidet.
%
% Prolog Listing List Clauses.
%
prolog_listing_list_clauses(Pred, Source) :-  current_prolog_flag(util_varnames,true),!,
      % scan_for_varnames,
       strip_module(Pred, Module, Head),   
      (current_prolog_flag(listing_docs,true)-> autodoc_pred(Module,Pred); true),
       (    clause(Pred, Body),
           once(( get_clause_vars_copy((Head:-Body),ForPrint),
            prolog_listing:write_module(Module, Source, Head),
            prolog_listing:portray_clause(ForPrint))),
	    fail
	;   true
	).


% System Version 7.3.9
prolog_listing_list_clauses(Pred, Source) :-
  prolog_listing:
  ((      strip_module(Pred, Module, Head),
	(   clause(Pred, Body),
	    write_module(Module, Source, Head),
	    portray_clause((Head:-Body)),
	    fail
	;   true
	))).


% Safe
prolog_listing_portray_clause(Stream, Term, M:Options) :- fail,
	must_be(list, Options),
	prolog_listing:meta_options(is_meta, M:Options, QOptions),
	\+ \+ ( must(call(call,serialize_attvars,Term, Copy)),
		% numbervars(Copy, 0, _,[ singletons(true), attvar(Skip)]),
                prolog_listing:do_portray_clause(Stream, Copy, QOptions)
	      ),!.

:- abolish(user:listing/1).
:- reconsult(library(listing)).
:- user:reconsult(library(listing)).

:- redefine_system_predicate(prolog_listing:portray_clause/3).
:- abolish(prolog_listing:portray_clause/3).
:- meta_predicate prolog_listing:portray_clause(+,+,:).
:- prolog_listing:export(prolog_listing:portray_clause/3).



% Original
prolog_listing:portray_clause(Stream, Term, M:Options) :-
	must_be(list, Options),
	meta_options(is_meta, M:Options, QOptions),
	\+ \+ ( copy_term_nat(Term, Copy),
		numbervars(Copy, 0, _,
			   [ singletons(true)
			   ]),
		prolog_listing:do_portray_clause(Stream, Copy, QOptions)
	      ).


%% list_clauses( ?Pred, ?Context) is semidet.
%
% Override of [prolog_listing:list_clauses/2] For variable name printing and otehr attributes.
% List Clauses.
%
:- redefine_system_predicate(prolog_listing:list_clauses/2).
:- abolish(prolog_listing:list_clauses/2).
prolog_listing:list_clauses(Pred, Context):- prolog_listing_list_clauses(Pred, Context).

:- endif.

