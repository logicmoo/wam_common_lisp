
style_check:- set_prolog_flag(double_quotes,codes),
    multifile(user:file_search_path/2),
    dynamic(user:file_search_path/2),
    (user:file_search_path(library,'.')->true ; asserta(user:file_search_path(library,'.'))).
one(X):- once(X).
:- op(1000,fx,one).
index(MFA,Args):- writeln(index(MFA,Args)).
optimize(MFA):- writeln(optimize(MFA)).


error_hook(X,Y):- writeln(error_hook(X,Y)),fail.
throw(X,Y):- writeln(throw(X,Y)),throw(lpa_throw(X,Y)).
lwrupr(L,U):- var(L)-> downcase_atom(U,L); upcase_atom(L,U).

