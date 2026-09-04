
:- make.
:- ensure_loaded('../prolog/predicate_streams').
:- dynamic(saved_out/1).
:- if( \+ current_predicate(with_error_to_predicate/2)).
with_error_to_predicate(A,B):-with_err_to_pred(A,B).
:- endif.

predicate_streams:setup_call_cleanup_each(A,B,C):-
   each_call_cleanup(A,B,C).

save_out(Data):- assert(saved_out(Data)).

:- make.

:- with_error_to_predicate(save_out,ls).
:- listing(saved_out/1).
:- listing(file_search_path).

:- write('  '),writeq(( with_error_to_predicate(write,ls))),writeln(.).
:- nl,nl.

:- with_error_to_predicate(write,ls).


