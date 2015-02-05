:- style_check(-singleton).
:- style_check(-discontiguous).
% :- style_check(-atom).
:-multifile(user:string_prolog_flag/1).
:-thread_local(user:string_prolog_flag/1).
:-current_prolog_flag(double_quotes, WAS),asserta(user:string_prolog_flag(WAS)).
:- set_prolog_flag(double_quotes, codes). 

/*
 Implementation of R6RS Appendix A

Try R6RS Scheme!


 Introduction

Revised^6 Report on the Algorithmic Language Scheme (R6RS) defines
the operational semantics for Scheme in Appendix A.

  http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-15.html#node_chap_A

This program is a implementation of it.


How to use

Run SWI-Prolog and use evaluate/1 like this:
?- evaluate("(car '(a b c))").


*/

:- set_prolog_flag(double_quotes, codes). 


%%% grammar

ip([ip, N]) :- number(N), !.
mp([mp, N]) :- number(N), !.

s(X, Y) :- reduce_star(X, Z), obs(Z, Y).

p_cal([unknown, _]) :- !.
p_cal([uncaught_exception, V]) :- v(V), !.
p_cal([store, S, ES]) :- sf_s(S), es(ES), !.

a_cal([unknown, _]) :- !.
a_cal([uncaught_exception, V]) :- v(V), !.
a_cal([store, S, [values|V]]) :- sf_s(S), v_s(V), !.

r_cal(exception) :- !.
r_cal(unknown) :- !.
r_cal([values|Rv]) :- rv_cal_s(Rv), !.

rv_cal(pair) :- !.
rv_cal(null) :- !.
rv_cal([quote, Sym]) :- sym(Sym), !.
rv_cal(Sqv) :- sqv(Sqv), !.
rv_cal(condition) :- !.
rv_cal(procedure) :- !.
rv_cal_s([]) :- !.
rv_cal_s([X|Y]) :- rv_cal(X), rv_cal_s(Y), !.

sf([X, V]) :- x(X), v(V), !.
sf([X, bh]) :- x(X), !.
sf([PP, [cons, V1, V2]]) :- pp(PP), v(V1), v(V2), !.
sf_s([]) :- !.
sf_s([X|Y]) :- sf(X), sf_s(Y), !.

es([quote, Seq]) :- seq(Seq), !.
es([quote, Sqv]) :- sqv(Sqv), !.
es([quote, [] ]) :- !.
es([begin, Es|Ess]) :- es(Es), es_s(Ess), !.
es([begin0, Es|Ess]) :- es(Es), es_s(Ess), !.
es([Es|Ess]) :- es(Es), es_s(Ess), !.
es([if, Es1, Es2, Es3]) :- es(Es1), es(Es2), es(Es3), !.
es(['set!', X, Es]) :- x(X), es(Es), !.
es(X) :- x(X), !.
es(N) :- nonproc(N), !.
es(P) :- pproc(P), !.
es([lambda, F, Es|Ess]) :- f(F), es(Es), es_s(Ess), !.
es([letrec, Binds, Es|Ess]) :- bind_s(Binds), es(Es), es_s(Ess), !.
es(['letrec*', Binds, Es|Ess]) :- bind_s(Binds), es(Es), es_s(Ess), !.
es([dw, X, Es1, Es2, Es3]) :- x(X), es(Es1), es(Es2), es(Es3), !.
es([throw, X, Es]) :- x(X), es(Es), !.
es(unspecified) :- !.
es([handlers, Es1|Ess_Es]) :-
  es(Es1), append(Ess,[Es],Ess_Es), es_s(Ess), es(Es), !.
es([l, X, Es]) :- x(X), es(Es), !.
es([reinit, X]) :- x(X), !.
es_s([]) :- !.
es_s([X|Y]) :- es(X), es_s(Y), !.

bind([X, Es]) :- x(X), es(Es), !.
bind_s([]) :- !.
bind_s([X|Y]) :- bind(X), bind_s(Y), !.
no_quote_bind([X, E]) :- x(X), e(E), !.
no_quote_bind_s([]) :- !.
no_quote_bind_s([X|Y]) :- no_quote_bind(X), no_quote_bind_s(Y), !.

binds_split([], [], []) :- !.
binds_split([[V,E]|T], [V|Vr], [E|Er]) :-
  binds_split(T, Vr, Er), !.


f(Xs) :- x_s(Xs), !.
f([X|Xs_dot_x]) :- x(X), append(Xs,['.', X2], Xs_dot_x), x_s(Xs), x(X2), !.
f(X) :- x(X), !.

s(Seq) :- seq(Seq), !.
s( [] ) :- !.
s(Sqv) :- sqv(Sqv), !.
s(Sym) :- sym(Sym), !.
s_s([]) :- !.
s_s([X|Y]) :- s(X), s_s(Y), !.

seq([S|Ss]) :- s(S), s_s(Ss), !.
seq([S|Ss_dot_Sqv]) :-
  s(S), append(Ss, ['.', Sqv], Ss_dot_Sqv), s_s(Ss), sqv(Sqv), !.
seq([S|Ss_dot_Sym]) :-
  s(S), append(Ss, ['.', Sym], Ss_dot_Sym), s_s(Ss), sym(Sym), !.

sqv(N) :- number(N), !.
sqv('#t') :- !.
sqv('#f') :- !.

p([store, Sfs, E]) :- sf_s(Sfs), e(E), !.

e([begin, E|Es]) :- e(E), e_s(Es), !.
e([begin0, E|Es]) :- e(E), e_s(Es), !.
e([E|Es]) :- e(E), e_s(Es), !.
e([if, E1, E2, E3]) :- e(E1), e(E2), e(E3), !.
e(['set!', X, E]) :- x(X), e(E), !.
e([handlers, E|Es_E]) :- e(E), append(Es,[E2],Es_E), e_s(Es), e(E2), !.
e(X) :- x(X), !.
e(N) :- nonproc(N), !.
e(P) :- proc(P), !.
e([dw, X, E1, E2, E3]) :- x(X), e(E1), e(E2), e(E3), !.
e(unspecified) :- !.
e(['l!', X, Es]) :- x(X), es(Es), !.
e([reinit, X]) :- x(X), !.
e_s([]) :- !.
e_s([X|Y]) :- e(X), e_s(Y), !.

v(N) :- nonproc(N), !.
v(P) :- proc(P), !.
v_s([]) :- !.
v_s([X|Y]) :- v(X), v_s(Y), !.

nonproc(Pp) :- pp(Pp), !.
nonproc(null) :- !.
nonproc([quote, Sym]) :- sym(Sym), !.
nonproc(Sqv) :- sqv(Sqv), !.
nonproc(['make-cond', _]) :- !.

proc([lambda, F, E|Es]) :- f(F), e(E), e_s(Es), !.
proc(Ppr) :- pproc(Ppr), !.
proc([throw, X, E]) :- x(X), e(E), !.
proc_s([]) :- !.
proc_s([X|Y]) :- proc(X), proc_s(Y), !.

pproc(X) :- aproc(X), !.
pproc(X) :- proc1(X), !.
pproc(X) :- proc2(X), !.
pproc(list) :- !.
pproc('dynamic-wind') :- !.
pproc(apply) :- !.
pproc(values) :- !.

proc1('null?') :- !.
proc1('pair?') :- !.
proc1(car) :- !.
proc1(cdr) :- !.
proc1('call/cc') :- !.
proc1('procedure?') :- !.
proc1('condition?') :- !.
proc1(X) :- raise_star(X), !.

proc2(cons) :- !.
proc2(consi) :- !.
proc2('set-car!') :- !.
proc2('set-cdr!') :- !.
proc2('eqv?') :- !.
proc2('call-with-values') :- !.
proc2('with-exception-handler') :- !.

aproc('+') :- !.
aproc('-') :- !.
aproc('*') :- !.
aproc('/') :- !.

raise_star('raise-continuable') :- !.
raise_star(raise) :- !.

pp(I) :- ip(I), !.
pp(M) :- mp(M), !.

sym('.') :- !, fail.
sym(X) :- atom(X), !.

x(X) :- keyword(X), !, fail.
x('.') :- !, fail.
x(X) :- atom(X), !.
x_s([]) :- !.
x_s([X|Y]) :- x(X), x_s(Y), !.

keyword(quote) :- !.
keyword(values) :- !.
keyword(throw) :- !.
keyword(mp) :- !.
keyword(ip) :- !.
keyword(lambda) :- !.
keyword(begin) :- !.
keyword(begin0) :- !.
keyword(if) :- !.
keyword(dw) :- !.
keyword('set!') :- !.
keyword(handlers) :- !.
keyword(uncaught_exception) :- !.
keyword('make-cond') :- !.
keyword(letrec) :- !.
keyword('letrec*') :- !.
keyword('l!') :- !.
keyword(reinit) :-!.

n(N) :- number(N), !.
n_s([]) :- !.
n_s([X|Y]) :- n(X), n_s(Y), !.


%%% evaluation context

ctx_p([store, Sfs, E], Hole, [store, Sfs, Next], NextHole, T) :-
  sf_s(Sfs), ctx_e_star(E, Hole, Next, NextHole, T).

ctx_e_star(X, X, Y, Y, multiple).  % multiple values
ctx_e_star(E, Hole, Next, NextHole, T) :-
  ctx_e(E, Hole, Next, NextHole, T).

ctx_e_circle(X, X, Y, Y, single).  % single value
ctx_e_circle(E, Hole, Next, NextHole, T) :-
  ctx_e(E, Hole, Next, NextHole, T).

ctx_e(F, Hole, Next, NextHole, T) :-
  ctx_f(F, [handlers|Procs_E], Next, [handlers|Procs_ENext], normal),
  append(Procs, [E], Procs_E), proc_s(Procs),
  ctx_e_star(E, Hole, ENext, NextHole, T),
  append(Procs, [ENext], Procs_ENext).
ctx_e(F, Hole, Next, NextHole, T) :-
  ctx_f(F, [dw, X, E1, E, E2], Next, [dw, X, E1, ENext, E2], normal),
  x(X), e(E1), e(E2),
  ctx_e_star(E, Hole, ENext, NextHole, T).
ctx_e(F, Hole, Next, NextHole, T) :-
  ctx_f(F, Hole, Next, NextHole, T).

ctx_pg([store, Sfs, G], Hole, [store, Sfs, Next1], NextHole, T) :-
  sf_s(Sfs), ctx_g(G, Hole, Next1, NextHole, T).

ctx_g(F, Hole, Next, NextHole, T) :-
  ctx_f(F, [dw, X, E1, G, E2], Next, [dw, X, E1, GNext, E2], T),
  x(X), e(E1), e(E2),
  ctx_g(G, Hole, GNext, NextHole, T).
ctx_g(F, Hole, Next, NextHole, T) :-
  ctx_f(F, Hole, Next, NextHole, T).

ctx_h(F, Hole, Next, NextHole, T) :-
  ctx_f(F, [handlers|Procs_H], Next, [handlers|Procs_HNext], T),
  append(Procs, [H], Procs_H), proc_s(Procs),
  ctx_h(H, Hole, HNext, NextHole, T),
  append(Procs, [HNext], Procs_HNext).
ctx_h(F, Hole, Next, NextHole, T) :-
  ctx_f(F, Hole, Next, NextHole, T).

ctx_f(X, X, Y, Y, normal).
ctx_f(Vs_F_Vs, Hole, Z, NextHole, T) :-
  append(Vs, [F|Vs2], Vs_F_Vs), v_s(Vs), v_s(Vs2),
  ctx_f_circle(F, Hole, Next1, NextHole, T),
  append(Vs, [Next1|Vs2], Z).
ctx_f([if, F, E1, E2], Hole, [if, Next1, E1, E2], NextHole, T) :-
  e(E1), e(E2), ctx_f_circle(F, Hole, Next1, NextHole, T).
ctx_f(['set!', X, F], Hole, ['set!', X, Next1], NextHole, T) :-
  x(X), ctx_f_circle(F, Hole, Next1, NextHole, T).
ctx_f([begin, F, E|Es], Hole, [begin, Next1, E|Es], NextHole, T) :-
  e(E), e_s(Es), ctx_f_star(F, Hole, Next1, NextHole, T).
ctx_f([begin0, F, E|Es], Hole, [begin0, Next1, E|Es], NextHole, T) :-
  e(E), e_s(Es), ctx_f_star(F, Hole, Next1, NextHole, T).
ctx_f([begin0, [values|Vs], F|Es], Hole,
      [begin0, [values|Vs], Next1|Es], NextHole, T) :-
  v_s(Vs), e_s(Es), ctx_f_star(F, Hole, Next1, NextHole, T).
ctx_f([begin0, unspecified, F|Es], Hole,
      [begin0, unspecified, Next1|Es], NextHole, T) :-
  e_s(Es), ctx_f_star(F, Hole, Next1, NextHole, T).
ctx_f(['call-with-values', [lambda,[],F|Es], V], Hole,
      ['call-with-values', [lambda,[],Next1|Es], V], NextHole, T) :-
  e_s(Es), v(V), ctx_f_star(F, Hole, Next1, NextHole, T).
ctx_f(['l!', X, F], Hole, ['l!', X, Next1], NextHole, T) :-
  x(X), ctx_f_circle(F, Hole, Next1, NextHole, T).

ctx_f_circle(X, X, Y, Y, single).
ctx_f_circle(F, Hole, Next, NextHole, T) :-
  ctx_f(F, Hole, Next, NextHole, T).

ctx_f_star(X, X, Y, Y, multiple).
ctx_f_star(F, Hole, Next, NextHole, T) :-
  ctx_f(F, Hole, Next, NextHole, T).

ctx_u(Vs_Hole_Vs2, Hole, Next, NextHole, normal) :-
  append(Vs, [Hole|Vs2], Vs_Hole_Vs2), v_s(Vs), v_s(Vs2),
  append(Vs, [NextHole|Vs2], Next).
ctx_u([if, Hole, E1, E2], Hole, [if, NextHole, E1, E2], NextHole, normal) :-
  e(E1), e(E2).
ctx_u(['set!', X, Hole], Hole, ['set!', X, NextHole], NextHole, normal) :-
  x(X).
ctx_u(['call-with-values',[lambda,[],Hole],V], Hole,
      ['call-with-values',[lambda,[],NextHole],V], NextHole, normal) :-
  v(V).

ctx_s(X, X, Y, Y, normal).
ctx_s([begin, E|Es_S_Ess], Hole, [begin, E|Z], NextHole, T) :-
  e(E), append(Es, [S|Ess], Es_S_Ess), e_s(Es), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Es, [Next1|Ess], Z).
ctx_s([begin, S|Ess], Hole, [begin, Next1|Ess], NextHole, T) :-
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s([begin0, E|Es_S_Ess], Hole, [begin0, E|Z], NextHole, T) :-
  e(E), append(Es, [S|Ess], Es_S_Ess), e_s(Es), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Es, [Next1|Ess], Z).
ctx_s([begin0, S|Ess], Hole, [begin0, Next1|Ess], NextHole, T) :-
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s(Es_S_Ess, Hole, Z, NextHole, T) :-
  append(Es, [S|Ess], Es_S_Ess), e_s(Es), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Es, [Next1|Ess], Z).
ctx_s([if, S, Es1, Es2], Hole, [if, Next1, Es1, Es2], NextHole, T) :-
  es(Es1), es(Es2),
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s([if, E, S, Es], Hole, [if, E, Next1, Es], NextHole, T) :-
  e(E), es(Es),
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s([if, E1, E2, S], Hole, [if, E1, E2, Next1], NextHole, T) :-
  e(E1), e(E2),
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s(['set!', X, S], Hole, ['set!', X, Next1], NextHole, T) :-
  x(X), ctx_s(S, Hole, Next1, NextHole, T).
ctx_s([handlers|Ss_S_Ess], Hole, [handlers|Z], NextHole, T) :-
  append(Ss, [S|Ess], Ss_S_Ess), s_s(Ss), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Ss, [Next1|Ess], Z).
ctx_s([handlers|Ss_S], Hole, [handlers|Z], NextHole, T) :-
  append(Ss, [S], Ss_S), s_s(Ss),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Ss, [Next1], Z).
ctx_s([throw, X, E], [throw, X, E], Y, Y, _) :-
  x(X), e(E).
ctx_s([lambda, F, S|Ess], Hole, [lambda, F, Next1|Ess], NextHole, T) :-
  f(F), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s([lambda, F, E|Es_S_Ess], Hole, [lambda, F, E|Z], NextHole, T) :-
  f(F), e(E), append(Es, [S|Ess], Es_S_Ess), e_s(Es), es_s(Ess), 
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Es, [Next1|Ess], Z).
ctx_s([letrec, Binds, Es|Ess], Hole, [letrec, Z, Es|Ess], NextHole, T) :-
  es(Es), es_s(Ess), append(NQBs, [[X, S]|Bs], Binds),
  no_quote_bind_s(NQBs), x(X), bind_s(Bs),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(NQBs, [[X, Next1]|Bs], Z).
ctx_s([letrec, NQBs, S|Ess], Hole, [letrec, NQBs, Next1|Ess], NextHole, T) :-
  no_quote_bind_s(NQBs), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s([letrec, NQBs, E|Es_S_Ess], Hole, [letrec, NQBs, E|Z], NextHole, T) :-
  e(E), append(Es, [S|Ess], Es_S_Ess), e_s(Es), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Es, [Next1|Ess], Z).
ctx_s(['letrec*', Binds, Es|Ess], Hole, ['letrec*', Z, Es|Ess], NextHole, T) :-
  es(Es), es_s(Ess), append(NQBs, [[X, S]|Bs], Binds),
  no_quote_bind_s(NQBs), x(X), bind_s(Bs),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(NQBs, [[X, Next1]|Bs], Z).
ctx_s(['letrec*', NQBs, S|Ess], Hole,
      ['letrec*', NQBs, Next1|Ess], NextHole, T) :-
  no_quote_bind_s(NQBs), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T).
ctx_s(['letrec*', NQBs, E|Es_S_Ess], Hole,
      ['letrec*', NQBs, E|Z], NextHole, T) :-
  e(E), append(Es, [S|Ess], Es_S_Ess), e_s(Es), es_s(Ess),
  ctx_s(S, Hole, Next1, NextHole, T),
  append(Es, [Next1|Ess], Z).


%%% reduce

%% begin
reduce(P1, Next) :-
  ctx_p(P1, [begin, [values|Vs], E|Es], Next, [begin, E|Es], normal),
  v_s(Vs), e_s(Es), e(E).
reduce(P1, Next) :-
  ctx_p(P1, [begin, E], Next, E, normal), e(E).
reduce(P1, Next) :-  % rubegin
  ctx_p(P1, [begin, unspecified, E|Es], Next, [begin, E|Es], normal),
  e_s(Es), e(E).

%% begin0
reduce(P1, Next) :-
  ctx_p(P1, [begin0, [values|Vs1], [values|Vs2] | Es],
    Next, [begin0, [values|Vs1] | Es], normal),
  v_s(Vs1), v_s(Vs2), e_s(Es).
reduce(P1, Next) :-
  ctx_p(P1, [begin0, E], Next, E, normal), e(E).

%% quote (immutable)
reduce([store, Sfs, S1], [store, Sfs, Next]) :-
  sf_s(Sfs), ctx_s(S1, [quote, Sqv], Next, Sqv, normal), sqv(Sqv).
reduce([store, Sfs, S1], [store, Sfs, Next]) :-
  sf_s(Sfs), ctx_s(S1, [quote, [] ], Next, null, normal).
reduce([store, Sfs, S1], [store, Sfs, [[lambda, [QP], Next1], Z]]) :-
  sf_s(Sfs), gen_atom(QP), ctx_s(S1, [quote, Seq], Next1, QP, normal),
  seq(Seq), bif_lm(Seq, Z).

%% multiple values
reduce(P1, Next) :-
  ctx_p(P1, V, Next, [values, V], multiple), v(V).
reduce(P1, Next) :-
  ctx_p(P1, [values, V], Next, V, single), v(V).

%% call-with-values
reduce(P1, Next) :-
  ctx_p(P1, ['call-with-values', [lambda,[],[values|Vs]],V], Next,
    [V|Vs], normal),
  v(V), v_s(Vs).
reduce(P1, Next) :-
  ctx_p(P1, ['call-with-values', V1, V2], Next,
    ['call-with-values', [lambda,[],[V1]], V2], normal),
  v(V1), v(V2), not(V1 = [lambda,[],_]).

%% arithmetic
reduce(P1, Next) :-
  ctx_p(P1, [+], Next, 0, normal).
reduce(P1, Next) :-
  ctx_p(P1, [+, N|Ns], Next, Z, normal),
  n(N), n_s(Ns), sum([N|Ns], Z).
reduce(P1, Next) :-
  ctx_p(P1, ['-'], Next, [raise, ['make-cond','arity mismatch']], normal).
reduce(P1, Next) :-
  ctx_p(P1, ['-', N], Next, Z, normal),
  n(N), Z is -N.
reduce(P1, Next) :-
  ctx_p(P1, ['-', N1, N2|Ns], Next, Z, normal),
  n(N1), n(N2), n_s(Ns), sum([N2|Ns], Y), Z is N1 - Y.
reduce(P1, Next) :-
  ctx_p(P1, ['*'], Next, 1, normal).
reduce(P1, Next) :-
  ctx_p(P1, ['*', N|Ns], Next, Z, normal),
  n(N), n_s(Ns), product([N|Ns], Z).
reduce(P1, Next) :-
  ctx_p(P1, ['/'], Next, [raise, ['make-cond','arity mismatch']], normal).
reduce(P1, Next) :-
  ctx_p(P1, ['/', N], Next, Z, normal),
  n(N), (not(N == 0)), Z is 1 // N.
reduce(P1, Next) :-
  ctx_p(P1, ['/', N1, N2|Ns], Next, Z, normal),
  n(N1), n(N2), n_s(Ns), not(member(0, [N2|Ns])),
  product([N2|Ns], Y), Z is N1 // Y.
reduce(P1, Next) :-
  ctx_p(P1, ['/', N1, N2|Ns], Next,
    [raise, ['make-cond','divison by zero']], normal),
  n(N1), n(N2), n_s(Ns), member(0, [N2|Ns]).
reduce(P1, Next) :-
  ctx_p(P1, [Aproc|Vs], Next,
    [raise, ['make-cond','arith-op applied to non-number']], normal),
  aproc(Aproc), v_s(Vs), not(n_s(Vs)).

%% if
reduce(P1, Next) :-
  ctx_p(P1, [if, V, E1, E2], Next, E1, normal),
  v(V), e(E1), e(E2), not(V == '#f').
reduce(P1, Next) :-
  ctx_p(P1, [if, '#f', E1, E2], Next, E2, normal),
  e(E1), e(E2).

%% list
reduce(P1, Next) :-
  ctx_p(P1, [list], Next, null, normal).
reduce(P1, Next) :-
  ctx_p(P1, [list, V|Vs], Next, [cons, V, [list|Vs]], normal),
  v(V), v_s(Vs).
 
%% cons
reduce([store, Sfs, E1], [store, [[[mp,Mp],[cons,V1,V2]]|Sfs], Next1]) :-
  sf_s(Sfs), gen_num(Mp),
  ctx_e(E1, [cons, V1, V2], Next1, [mp, Mp], normal),
  v(V1), v(V2).
reduce([store, Sfs, E1], [store, [[[ip,Ip],[cons,V1,V2]]|Sfs], Next1]) :-
  sf_s(Sfs), gen_num(Ip),
  ctx_e(E1, [consi, V1, V2], Next1, [ip, Ip], normal),
  v(V1), v(V2).

%% car/cdr
reduce([store, St, E1], [store, St, Next1]) :-
  ctx_e(E1, [car, Pp], Next1, V1, normal),
  pp(Pp), append(Sfs, [[Pp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), sf_s(Sfs2), v(V1), v(V2).
reduce([store, St, E1], [store, St, Next1]) :-
  ctx_e(E1, [cdr, Pp], Next1, V2, normal),
  pp(Pp), append(Sfs, [[Pp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), sf_s(Sfs2), v(V1), v(V2).
reduce(P1, Next) :-
  ctx_p(P1, [car, V], Next,
    [raise, ['make-cond','can\'t take car of non-pair']], normal),
  v(V), not(pp(V)).
reduce(P1, Next) :-
  ctx_p(P1, [cdr, V], Next,
    [raise, ['make-cond','can\'t take cdr of non-pair']], normal),
  v(V), not(pp(V)).

%% set-car!/set-cdr!
reduce([store, St, E1], [store, St2, Next1]) :-
  ctx_e(E1, ['set-car!', Mp, V], Next1, unspecified, normal),
  mp(Mp), v(V), append(Sfs, [[Mp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), sf_s(Sfs2), v(V1), v(V2),
  append(Sfs, [[Mp,[cons,V,V2]]|Sfs2], St2).
reduce([store, St, E1], [store, St2, Next1]) :-
  ctx_e(E1, ['set-cdr!', Mp, V], Next1, unspecified, normal),
  mp(Mp), v(V), append(Sfs, [[Mp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), sf_s(Sfs2), v(V1), v(V2),
  append(Sfs, [[Mp,[cons,V1,V]]|Sfs2], St2).
reduce(P1, Next) :-
  ctx_p(
    P1, ['set-car!', V1, V2], Next,
    [raise, ['make-cond','can\'t set-car! on non-pair or an immutable pair']],
    normal),
  v(V1), v(V2), not(mp(V1)).
reduce(P1, Next) :-
  ctx_p(
    P1, ['set-cdr!', V1, V2], Next,
    [raise, ['make-cond','can\'t set-cdr! on non-pair or an immutable pair']],
    normal),
  v(V1), v(V2), not(mp(V1)).

%% null?
reduce(P1, Next) :-
  ctx_p(P1, ['null?', null], Next, '#t', normal).
reduce(P1, Next) :-
  ctx_p(P1, ['null?', V], Next, '#f', normal),
  v(V), not(V = null).

%% pair?
reduce(P1, Next) :-
  ctx_p(P1, ['pair?', Pp], Next, '#t', normal),
  pp(Pp).
reduce(P1, Next) :-
  ctx_p(P1, ['pair?', V], Next, '#f', normal),
  v(V), not(pp(V)).

%% eqv?
reduce(P1, Next) :-
  ctx_p(P1, ['eqv?', V, V], Next, '#t', normal),
  v(V), not(proc(V)).
reduce(P1, Next) :-
  ctx_p(P1, ['eqv?', V1, V2], Next, '#f', normal),
  v(V1), v(V2), not(V1 = V2).

%% 6mark
reduce(P1, Next) :-
  ctx_p(P1, Es_E_Es, Next, [[lambda,[X],Z],E], normal),
  append(Es, [E|Es2], Es_E_Es), e_s(Es), e_s(Es2), e(E), not(v(E)),
  exist_e(Es, Es2),
  gen_atom(X), append(Es, [X|Es2], Z).

%% 6app0
reduce(P1, Next) :-
  ctx_p(P1, [[lambda,[], E|Es]], Next, [begin, E|Es], normal),
  e(E), e_s(Es).

%% 6appN
reduce(P1, Next) :-
  ctx_p(P1, [[lambda,[X|Xs], E|Es], V|Vs], Next,
        [[lambda,Xs, Z|Zs] | Vs], normal),
  x(X), x_s(Xs), e(E), e_s(Es), v(V), v_s(Vs),
  length(Xs, Len), length(Vs, Len),
  not(bir_v(X, [lambda,Xs, E|Es])),
  replace(E, X, V, Z), replace(Es, X, V, Zs).

%% 6appN!
reduce([store, Sfs, E1], [store, [[BP, V]|Sfs], Next1]) :-
  sf_s(Sfs),
  ctx_e(E1, [[lambda,[X|Xs], E|Es], V|Vs], Next1,
        [[lambda,Xs, Z|Zs] | Vs], normal),
  x(X), x_s(Xs), e(E), e_s(Es), v(V), v_s(Vs),
  length(Xs, Len), length(Vs, Len),
  bir_v(X, [lambda,Xs, E|Es]), gen_atom(BP),
  replace(E, X, BP, Z), replace(Es, X, BP, Zs).

%% 6arity
reduce(P1, Next) :-
  ctx_p(P1, [[lambda,Xs, E|Es] | Vs], Next,
        [raise, ['make-cond','arity mismatch']], normal),
  x_s(Xs), e(E), e_s(Es), v_s(Vs),
  length(Xs, Len1), length(Vs, Len2),
  not(Len1 == Len2).

%% 6uapp
reduce(P1, Next) :-
  ctx_p(P1, [[lambda,[X|Xs_dot_Xr], E|Es], V|Vs1_Vs2], Next,
        [[lambda,[X|Xs_Xr], E|Es], V|Vs1_Vs3], normal),
  x(X), e(E), e_s(Es), v(V),
  append(Xs, ['.'|[Xr]], Xs_dot_Xr), x_s(Xs), x(Xr),
  append(Vs1, Vs2, Vs1_Vs2), v_s(Vs1), v_s(Vs2),
  length(Xs, Len), length(Vs1, Len),
  append(Vs1, [[list|Vs2]], Vs1_Vs3), append(Xs, [Xr], Xs_Xr).

%% 6uapp1
reduce(P1, Next) :-
  ctx_p(P1, [[lambda, X, E|Es] | Vs], Next,
        [[lambda, [X], E|Es], [list|Vs]], normal),
  x(X), e(E), e_s(Es), v_s(Vs).

%% 6uarity
reduce(P1, Next) :-
  ctx_p(P1, [[lambda,Xs_dot_Xr, E|Es] | Vs1_Vs2], Next,
        [raise, ['make-cond','arity mismatch']], normal),
  e(E), e_s(Es), v_s(Vs1_Vs2),
  append(Xs, ['.'|[Xr]], Xs_dot_Xr), x_s(Xs), x(Xr),
  length(Xs, Len1), length(Vs1_Vs2, Len2),
  Len2 < Len1.

%% call/cc
reduce([store, Sfs, E1], [store, Sfs, Next1]) :-
  sf_s(Sfs),
  ctx_e(E1, ['call/cc', V1], Next1, [V1, [throw, X, Next2]], normal),
  v(V1),
  ctx_e(E1, ['call/cc', V1], Next2, X, normal),
  gen_atom(X).

%% throw
reduce([store, Sfs, E1], [store, Sfs, Next1]) :-
  sf_s(Sfs),
  ctx_e(E1, [[throw, X, E2]|Vs], _, _, normal),
  x(X), v_s(Vs), bif_t(E1, E2, X, Next1, [values|Vs]).

%% dynamic-wind
reduce(P1, Next) :-
  ctx_p(P1, ['dynamic-wind', Proc1, Proc2, Proc3], Next,
        [begin,[Proc1],[begin0,[dw,X,[Proc1],[Proc2],[Proc3]],[Proc3]]],
    normal),
  proc(Proc1), proc(Proc2), proc(Proc3), gen_atom(X).

reduce(P1, Next) :-
  ctx_p(P1, [dw, X, E1, [values|Vs], E2], Next, [values|Vs], normal),
  x(X), e(E1), v_s(Vs), e(E2).

%% 6var
reduce([store, Sfs_bind_Sfs, E1], [store, Sfs_bind_Sfs, Next1]) :-
  append(Sfs, [[X,V]|Sfs2], Sfs_bind_Sfs),
  sf_s(Sfs), x(X), v(V), sf_s(Sfs2),
  ctx_e(E1, X, Next1, V, normal).

%% set!
reduce([store, Sfs_bind_Sfs, E1], [store, Sfs_bind2_Sfs, Next1]) :-
  ctx_e(E1, ['set!', X, V2], Next1, unspecified, normal),
  append(Sfs, [[X,V]|Sfs2], Sfs_bind_Sfs),
  sf_s(Sfs), v(V), sf_s(Sfs2), x(X),
  v(V2), append(Sfs, [[X,V2]|Sfs2], Sfs_bind2_Sfs).

%% procedure?
reduce(P1, Next) :-
  ctx_p(P1, ['procedure?', Proc], Next, '#t', normal),
  proc(Proc).
reduce(P1, Next) :-
  ctx_p(P1, ['procedure?', Nonproc], Next, '#f', normal),
  nonproc(Nonproc).

%% proc exception
reduce(P1, Next) :-  % 6appe
  ctx_p(P1, [Nonproc|Vs], Next,
        [raise, ['make-cond','can\'t call non-procedure']], normal),
  nonproc(Nonproc), v_s(Vs).
reduce(P1, Next) :-  % 61arity
  ctx_p(P1, [Proc1|Vs], Next,
        [raise, ['make-cond','arity mismatch']], normal),
  proc1(Proc1), v_s(Vs), length(Vs, Len), not(Len == 1).
reduce(P1, Next) :-  %62arity
  ctx_p(P1, [Proc2|Vs], Next,
        [raise, ['make-cond','arity mismatch']], normal),
  proc2(Proc2), v_s(Vs), length(Vs, Len), not(Len == 2).

%% apply
reduce(P1, Next) :-
  ctx_p(P1, [apply, Proc|Vs_null], Next, [Proc|Vs], normal),
  proc(Proc), append(Vs, [null], Vs_null), v_s(Vs).
reduce([store, St, E1], [store, St, Next1]) :-
  ctx_e(E1, [apply, Proc|Vs_Pp], Next1, [apply, Proc|Vs_V1_V2], normal),
  proc(Proc), append(Vs, [Pp], Vs_Pp),
  append(Sfs, [[Pp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), pp(Pp), v(V1), v(V2), sf_s(Sfs2),
  append(Vs, [V1, V2], Vs_V1_V2).

%% apply exception
reduce(P1, Next) :-
  ctx_p(P1, [apply, Nonproc|Vs], Next,
        [raise, ['make-cond','can\'t apply non-procedure']], normal),
 'format'('~p~n',[Nonproc]),
  nonproc(Nonproc), v_s(Vs).
reduce(P1, Next) :-
  ctx_p(P1, [apply, Proc|Vs_V], Next,
        [raise, ['make-cond','apply\'s last argument non-list']], normal),
  proc(Proc), append(Vs, [V], Vs_V), v_s(Vs), v(V),
  not(V = null), not(pp(V)).
reduce(P1, Next) :-
  ctx_p(P1, [apply], Next,
        [raise, ['make-cond','arity mismatch']], normal).
reduce(P1, Next) :-
  ctx_p(P1, [apply, V], Next,
        [raise, ['make-cond','arity mismatch']], normal),
  v(V).

%% with-exception-handler (not in handlers)
reduce(PG1, Next) :-
  ctx_pg(PG1, ['with-exception-handler', Proc1, Proc2],
    Next, [handlers, Proc1, [Proc2]], normal),
  proc(Proc1), proc(Proc2).
reduce(PG1, Next) :-  % 6weherr
  ctx_pg(PG1, ['with-exception-handler', V1, V2],
         Next, [raise, ['make-cond','with-exception-handler expects procs']],
         normal),
  v(V1), v(V2), or(V1, V2, V12), not(proc(V12)).

%% with-exception-handler (in handlers)
reduce(P1, Next) :-
  ctx_p(P1, [handlers|Procs_G], Next, [handlers|Procs_GNext], normal),
  append(Procs, [G], Procs_G), proc_s(Procs),
  ctx_g(G, ['with-exception-handler', Proc1, Proc2], GNext,
        [handlers|Z], normal),
  proc(Proc1), proc(Proc2), append(Procs, [Proc1|[[Proc2]]], Z),
  append(Procs, [GNext], Procs_GNext).
reduce(P1, Next) :-  % 6xwhne
  ctx_p(P1, [handlers|Procs_G], Next, [handlers|Procs_GNext], normal),
  append(Procs, [G], Procs_G), proc_s(Procs),
  ctx_g(G, ['with-exception-handler', V1, V2], GNext,
        [raise, ['make-cond','with-exception-handler expects procs']], normal),
  v(V1), v(V2), or(V1,V2,V12), not(proc(V12)),
  append(Procs, [GNext], Procs_GNext).

%% raise-continuable
reduce(P1, Next) :-
  ctx_p(P1, [handlers|Procs_Proc_G], Next,
    [handlers|Procs_GNext], normal),
  append(Procs, [Proc|[G]], Procs_Proc_G), proc_s(Procs), proc(Proc),
  ctx_g(G, ['raise-continuable', V], GNext, Z, normal),
  v(V), append(Procs, [Proc, V], Z),
  append(Procs, [GNext], Procs_GNext).

%% raise
reduce(P1, Next) :-
  ctx_p(P1, [handlers|Procs_Proc_G], Next,
    [handlers|Procs_Proc_GNext], normal),
  append(Procs, [Proc|[G]], Procs_Proc_G), proc_s(Procs), proc(Proc),
  ctx_g(G, [raise, V], GNext, Z, normal),
  v(V),
  append(Procs,
         [begin, [Proc, V], [raise,['make-cond','handler returned']]], Z),
  append(Procs, [GNext], Procs_Proc_GNext).

%% handlers -> values
reduce(P1, Next) :-
  ctx_p(P1, [handlers|Procs_Values], Next, [values|Vs], normal),
  append(Procs, [[values|Vs]], Procs_Values),
  proc_s(Procs), v_s(Vs).

%% condition?
reduce(P1, Next) :-
  ctx_p(P1, ['condition?', ['make-cond', _]], Next, '#t', normal).
reduce(P1, Next) :-
  ctx_p(P1, ['condition?', X], Next, '#f', normal),
  not(X = ['make-cond', _]).

%% uncaught exception
reduce(PG1, [uncaught_exception, V]) :-
  ctx_pg(PG1, [Raise, V], _, _, normal),
  raise_star(Raise), v(V).
reduce(P1, [uncaught_exception, V]) :-
  ctx_p(P1, [handlers, G], _, _, normal),
  ctx_g(G, [Raise, V], _, _, normal),
  raise_star(Raise), v(V).

%% letrec
reduce([store, Sfs, E1], [store, Sfs2, Next1]) :-
  sf_s(Sfs),
  ctx_e(E1, [letrec, Binds, E|Es], Next1,
    [[lambda, Vars | Body] | Args], normal),
  no_quote_bind_s(Binds), e(E), e_s(Es),
  binds_split(Binds, Vars, Exps),
  length(Vars, Len), make_vars(Len, LVars), make_vars(Len, RVars),
  make_inits(LVars, Vars, Inits),
  multi_replace([E|Es], Vars, LVars, Z),
  make_letrec_args(Exps, Vars, LVars, RVars, Args),
  append(Inits, Z, Body),
  make_letrec_store(LVars, RVars, Sfs, Sfs2).

%% letrec*
reduce([store, Sfs, E1], [store, Sfs2, Next1]) :-
  sf_s(Sfs),
  ctx_e(E1, ['letrec*', Binds, E|Es], Next1, [begin|Body], normal),
  no_quote_bind_s(Binds), e(E), e_s(Es),
  binds_split(Binds, Vars, Exps),
  length(Vars, Len), make_vars(Len, LVars), make_vars(Len, RVars),
  make_star_inits(Exps, LVars, RVars, Inits),
  append(Inits, [E|Es], Body1),
  multi_replace(Body1, Vars, LVars, Body),
  make_letrec_store(LVars, RVars, Sfs, Sfs2).

reduce([store, St, E1], [store, St2, Next1]) :-  % 6initdt
  sf_s(St), ctx_e(E1, ['l!', X, V], Next1, unspecified, normal),
  x(X), v(V),
  append(Sfs, [[X, bh]|Sfs2], St), sf_s(Sfs), sf_s(Sfs2),
  append(Sfs, [[X, V]|Sfs2], St2).
reduce([store, St, E1], [store, St2, Next1]) :-  % 6initv
  sf_s(St), ctx_e(E1, ['l!', X, V], Next1, unspecified, normal),
  x(X), v(V),
  append(Sfs, [[X, V0]|Sfs2], St), sf_s(Sfs), sf_s(Sfs2), v(V0),
  append(Sfs, [[X, V]|Sfs2], St2).
reduce([store, St, E1], [store, St2, Next1]) :-  % 6setdt
  sf_s(St), ctx_e(E1, ['set!', X, V], Next1, unspecified, normal),
  x(X), v(V),
  append(Sfs, [[X, bh]|Sfs2], St), sf_s(Sfs), sf_s(Sfs2),
  append(Sfs, [[X, V]|Sfs2], St2).
reduce([store, St, E1], [store, St, Next1]) :-  % 6dt
  sf_s(St), ctx_e(E1, X, Next1,
    [raise, ['make-cond','letrec variable touched']], normal),
  x(X),
  append(Sfs, [[X, bh]|Sfs2], St), sf_s(Sfs), sf_s(Sfs2).
reduce([store, St, E1], [store, St2, Next1]) :-  % 6init
  sf_s(St), ctx_e(E1, [reinit, X], Next1, [quote,ignore], normal),
  x(X),
  append(Sfs, [[X, '#f']|Sfs2], St), sf_s(Sfs), sf_s(Sfs2),
  append(Sfs, [[X, '#t']|Sfs2], St2).
reduce([store, St, E1], [store, St, Next1]) :-  % 6reinite
  sf_s(St), ctx_e(E1, [reinit, X], Next1,
    [raise,['make-cond','reinvoked continuation of letrec init']], normal),
  x(X),
  append(Sfs, [[X, '#t']|Sfs2], St), sf_s(Sfs), sf_s(Sfs2).



%% unspecified(except begin)
reduce([store, Sfs, unspecified], [unknown, 'unspecified result']) :-
  sf_s(Sfs).
reduce(P1, [unknown, 'unspecified result']) :-
  ctx_p(P1, U, _, _, normal),
  ctx_u(U, unspecified, _, _, normal).
reduce(P1, [unknown, 'equivalence of procedures']) :-
  ctx_p(P1, ['eqv?', Proc, Proc], _, _, normal),
  proc(Proc).
reduce(P1, [unknown, 'context expected one value']) :-
  ctx_p(P1, [values, V1, V2|Vs], _, _, single),
  v(V1), v(V2), v_s(Vs).
reduce(P1, Next) :-
  ctx_p(P1, [handlers|Vs_unspecified], Next, unspecified, normal),
  append(Vs, [unspecified], Vs_unspecified), v_s(Vs).
reduce(P1, Next) :-
  ctx_p(P1, [dw,X,E1,unspecified,E2], Next, unspecified, normal),
  x(X), e(E1), e(E2).
reduce(P1, Next) :-
  ctx_p(P1, [begin0, [values|Vs1], unspecified | Es],
    Next, [begin0, [values|Vs1] | Es], normal),
  v_s(Vs1), e_s(Es).
reduce(P1, Next) :-
  ctx_p(P1, [begin0, unspecified, [values|Vs2] | Es],
    Next, [begin0, unspecified | Es], normal),
  v_s(Vs2), e_s(Es).
reduce(P1, Next) :-
  ctx_p(P1, [begin0, unspecified, unspecified | Es],
    Next, [begin0, unspecified | Es], normal),
  e_s(Es).


reduce(X, X).

%% replace list element
%% (don't change local variable)
replace([], _, _, []) :- !.
replace(X, X, Y, Y) :- !.
replace([lambda,Xs|Es], X, _, [lambda,Xs|Es]) :-
  member(X, Xs), !.  % check (a b . c) type together by not check Xs type
replace([lambda,X|Es], X, _, [lambda,X|Es]) :- !.
replace([H|T], X, Y, [Z1|Z2]) :-
  replace(H, X, Y, Z1),
  replace(T, X, Y, Z2), !.
replace(H, _, _, H) :- !.

sum([], 0).
sum([X|Y], Z) :- sum(Y, W), Z is X + W.

product([], 1).
product([X|Y], Z) :- product(Y, W), Z is X * W.

exist_e([], []) :- fail.
exist_e([], [H|_]) :- e(H), not(v(H)).
exist_e([], [_|T]) :- exist_e([], T).
exist_e([H|_], _) :- e(H), not(v(H)).
exist_e([_|T], X) :- exist_e(T, X).

make_vars(0, []) :- !.
make_vars(N, [H|T]) :-
  M is N - 1,
  gen_atom(H), make_vars(M, T).

make_inits([], [], []).
make_inits([L|Ls], [V|Vs], [['l!', L, V]|T]) :-
  make_inits(Ls, Vs, T).

make_star_inits([], [], [], []).
make_star_inits([E|Es], [L|Ls], [R|Rs], [[begin,['l!',L,E],[reinit,R]]|T]) :-
  make_star_inits(Es, Ls, Rs, T).

multi_replace(L, [], [], L).
multi_replace(L, [X|Xs], [Y|Ys], Result) :-
  replace(L, X, Y, Result1),
  multi_replace(Result1, Xs, Ys, Result).

make_letrec_args([], _, _, [], []).
make_letrec_args([E|Es], Vars, LVars, [R|Rs], [[begin0, E2, [reinit, R]]|T]) :-
  multi_replace(E, Vars, LVars, E2),
  make_letrec_args(Es, Vars, LVars, Rs, T).

make_letrec_store([], [], Sfs, Sfs).
make_letrec_store([L|Ls], [R|Rs], Sfs, [[L,bh]|[[R,'#f']|T]]) :-
  make_letrec_store(Ls, Rs, Sfs, T).

or(X, _, X).
or(_, Y, Y).

eval(X, Y) :- reduce(X, Z), not(X==Z), eval(Z, Y), !.
eval(X, X) :- reduce(X, X), !.

eval_step(X, Y) :-
  reduce(X, Z), not(X==Z), 'format'('~p~n',[Z]), eval_step(Z, Y), !.
eval_step(X, X) :- reduce(X, X), !.


%%% built in function

bif_li( [] , null) :- !.
bif_li([S|Ss], [cons, Y, Z]) :-
  s(S), s_s(Ss), bif_li(S, Y), bif_li(Ss, Z).
bif_li([S, '.', Sqv], [cons, Y, Sqv]) :-
  s(S), sqv(Sqv), bif_li(S, Y).
bif_li([S1, S2|Ss_dot_Sqv], [cons, Y, Z]) :-
  s(S1), s(S2), append(Ss, ['.', Sqv], Ss_dot_Sqv), s_s(Ss), sqv(Sqv),
  bif_li(S1, Y), bif_li([S2|Ss_dot_Sqv], Z).
bif_li([S, '.', Sym], [cons, Y, [quote, Sym]]) :-
  s(S), sym(Sym), bif_li(S, Y).
bif_li([S1, S2|Ss_dot_Sym], [cons, Y, Z]) :-
  s(S1), s(S2), append(Ss, ['.', Sym], Ss_dot_Sym), s_s(Ss), sym(Sym),
  bif_li(S1, Y), bif_li([S2|Ss_dot_Sym], Z).
bif_li(Sym, [quote, Sym]) :- sym(Sym).
bif_li(Sqv, Sqv) :- sqv(Sqv).

bif_lm( [] , null) :- !.
bif_lm([S|Ss], [consi, Y, Z]) :-
  s(S), s_s(Ss), bif_lm(S, Y), bif_lm(Ss, Z).
bif_lm([S, '.', Sqv], [consi, Y, Sqv]) :-
  s(S), sqv(Sqv), bif_lm(S, Y).
bif_lm([S1, S2|Ss_dot_Sqv], [consi, Y, Z]) :-
  s(S1), s(S2), append(Ss, ['.', Sqv], Ss_dot_Sqv), s_s(Ss), sqv(Sqv),
  bif_lm(S1, Y), bif_lm([S2|Ss_dot_Sqv], Z).
bif_lm([S, '.', Sym], [consi, Y, [quote, Sym]]) :-
  s(S), sym(Sym), bif_lm(S, Y).
bif_lm([S1, S2|Ss_dot_Sym], [consi, Y, Z]) :-
  s(S1), s(S2), append(Ss, ['.', Sym], Ss_dot_Sym), s_s(Ss), sym(Sym),
  bif_lm(S1, Y), bif_lm([S2|Ss_dot_Sym], Z).
bif_lm(Sym, [quote, Sym]) :- sym(Sym).
bif_lm(Sqv, Sqv) :- sqv(Sqv).

bif_t(H1, H2, Hole, Result, NextHole) :-
  ctx_h(H1, [dw,X,E1,E_1,E2], _, _, normal),
  e(E1), e(E2), x(X),
  ctx_h(H2, [dw,X,E1,E_2,E2], Result, [dw,X,E1,T,E2], normal),
  bif_t(E_1, E_2, Hole, T, NextHole), !.
bif_t(E1, E2, Hole, [begin, S, K], NextHole) :-
  bif_s(E1, S, 1), bif_r(E2, Hole, K, NextHole), !.

bif_r(H1, Hole, Result, NextHole) :-
  ctx_h(H1, [dw,X,E1,E,E2], Result,
    [begin, E1, [dw,X,E1,K,E2]], normal),
  x(X), e(E1), e(E2),
  bif_r(E, Hole, K, NextHole), !.
bif_r(H1, Hole, Result, NextHole) :-
  ctx_h(H1, Hole, Result, NextHole, normal), !.

bif_s(E, Result, NextHole) :-
  ctx_e(E, [dw,X,E1,H2,E2], _, _, normal),
  x(X), e(E1), e(E2),
  bif_s(H2, Result, [begin0, [dw,X,E1,NextHole,E2], E2]), !.
bif_s(_, X, X) :- !.


%%% built in relation
%%% (don't check first argument type)

bir_v(X, ['set!', X, E]) :-
  e(E), !.
bir_v(X1, ['set!', X2, E]) :-
  x(X2), e(E), bir_v(X1, E), !.
bir_v(X, [begin, E1, E2|Es]) :-
  e(E1), e(E2), e_s(Es),
  bir_v(X, E1), !.
bir_v(X, [begin, E1, E2|Es]) :-
  e(E1), e(E2), e_s(Es),
  bir_v(X, [begin, E2|Es]), !.
bir_v(X, [begin, E]) :-
  e(E), bir_v(X, E), !.
bir_v(X, [E|Es]) :-
  e(E), e_s(es), bif_v(X, [begin | [E|Es]]), !.
bir_v(X, [if, E1, E2, E3]) :-
  e(E1), e(E2), e(E3),
  bir_v_3(X, [E1, E2, E3]), !.
bir_v(X, [begin0, E|Es]) :-
  e(E), e_s(Es), bir_v(X, [begin, E|Es]), !.
bir_v(X, [lambda, Xs, E|Es]) :-
  x_s(Xs), e(E), e_s(Es),
  not(member(X, Xs)), bir_v(X, [begin, E|Es]), !.
bir_v(X, [lambda, Xs_dot_X2, E|Es]) :-
  append(Xs, ['.'|[X2]], Xs_dot_X2), x_s(Xs), x(X2),
  e(E), e_s(Es),
  not(member(X, Xs)), not(X = X2), bir_v(X, [begin, E|Es]), !.
bir_v(X, [lambda, X2, E|Es]) :-
  x(X2), e(E), e_s(Es), not(X = X2), bir_v(X, [begin, E|Es]), !.
bir_v(X, [letrec, Binds, E|Es]) :-
  no_quote_bind_s(Binds), e(E), e_s(Es),
  binds_split(Binds, Vars, Exps),
  not(member(X, Vars)), append(Exps, [E|Es], Z),
  bir_v(X, [begin|Z]), !.
bir_v(X, ['letrec*', Binds, E|Es]) :-
  no_quote_bind_s(Binds), e(E), e_s(Es),
  binds_split(Binds, Vars, Exps),
  not(member(X, Vars)), append(Exps, [E|Es], Z),
  bir_v(X, [begin|Z]), !.
bir_v(X, ['l!', X2, E]) :-
  x(X2), e(E), bir_v(X, ['set!', X2, E]), !.
bir_v(X, [reinit, X2, E]) :-
  x(X2), e(E), bir_v(X, ['set!', X2, E]), !.
bir_v(X, [dw, X2, E1, E2, E3]) :-
  x(X2), e(E1), e(E2), e(E3), bir_v_3(X, [E1, E2, E3]), !.

bir_v_3(X, [E1, _, _]) :-
  e(E1), bir_v(X, E1), !.
bir_v_3(X, [_, E2, _]) :-
  e(E2), bir_v(X, E2), !.
bir_v_3(X, [_, _, E3]) :-
  e(E3), bir_v(X, E3), !.


%%% evaluator
evaluate(Str) :-
  parse(Str, Obj),
  eval([store,[[x,0]],Obj], Ret),
  print_program(Ret).


%%% parser
parse(Str, Obj) :-
  str_to_obj(Str, Obj, _).

remove_space([], []) :- !.
remove_space([N|T], Z) :-
  char_type(N, space),
  remove_space(T, Z), !.
remove_space([N|T], [N|T]) :- !.

next_token(Str, Token, Rest) :-
  remove_space(Str, [H|T]),
  not(delimiter(H)),
  next_token_sub([H|T], [Token,[]], Rest), !.
next_token(Str, H, T) :-
  remove_space(Str, [H|T]), !.

next_token_sub([], [X,X], []) :- !.
next_token_sub([H|T], [X,X], [H|T]) :-
  delimiter(H), !.
next_token_sub([H|T], [[H|X],Y], Rest) :-
  next_token_sub(T, [X,Y], Rest).

str_to_obj(Str, Obj, Rest) :-
  next_token(Str, Token, Rest1),
  token_to_obj(Token, Rest1, Obj, Rest), !.

token_to_obj(LP, Rest, Obj, NextRest) :-
  number(LP), char_code('(', LP),
  str_to_list(Rest, [Obj,[]], NextRest), !.
token_to_obj(QT, Rest, [quote, Obj1], NextRest) :-
  number(QT), char_code('\'', QT),
  str_to_obj(Rest, Obj1, NextRest), !.
token_to_obj(Token, Rest, Obj, Rest) :-
  numstr(Token),
  numstr_to_number(Token, Obj, 0), !.
token_to_obj(Token, Rest, Obj, Rest) :-
  atom_codes(Obj, Token), !.

str_to_list(Str, [X,Y], Rest) :-
  next_token(Str, Token, Rest1),
  str_to_list1(Token, Rest1, [X,Y], Rest).
str_to_list1(N, Rest, [X,X], Rest):-
  number(N), char_code(')', N), !.
str_to_list1(".", Rest1, [['.'|X],Y], Rest) :-
  next_token(Rest1, Token, Rest2),
  str_to_list1(Token, Rest2, [X,Y], Rest), !.
str_to_list1(Token, Rest1, [[Obj|X],Y], Rest) :-
  token_to_obj(Token, Rest1, Obj, Rest2),
  next_token(Rest2, Token2, Rest3),
  str_to_list1(Token2, Rest3, [X,Y], Rest), !.

numstr_to_number([], Acc, Acc).
numstr_to_number([H|T], Result, Acc) :-
  char_code('0', Base),
  Acc1 is (H - Base) + (Acc * 10),
  numstr_to_number(T, Result, Acc1).
  

delimiter(N) :-  char_type(N, space).
delimiter(N) :-  char_code('\'', N).
delimiter(N) :- char_code('(', N).
delimiter(N) :- char_code(')', N).

numstr([]).
numstr([H|T]) :- char_type(H, digit), numstr(T).


%%% printer

print_program([unknown, Msg]) :-
  'format'('unknown: ~p~n', [Msg]).
print_program([uncaught_exception, ['make-cond', Msg]]) :-
  'format'('uncaught exception: ~p~n',[Msg]).
print_program([store, Sfs, [values|Vs]]) :-
  print_values(Sfs, [values|Vs]).
print_values(Sfs, [values, V]) :-
  v(V), print_obj(Sfs, V), !.
print_values(Sfs, [values, V|Vs]) :-
  v(V), v_s(Vs),
  'format'('(values '),
  print_values1(Sfs, [V|Vs]),
  'format'(')'), !.
print_values1(_, []) :- !.
print_values1(Sfs, [V|Vs]) :-
  v(V), v_s(Vs),
  print_obj(Sfs, V),
  'format'(' '),
  print_values1(Sfs, Vs), !.

print_obj(_, null) :-
  'format'('()'), !.
print_obj(_, Sqv) :-
  sqv(Sqv), 'format'('~p', [Sqv]), !.
print_obj(_, [quote,Sym]) :-
  sym(Sym), 'format'('~p', [Sym]), !.
print_obj(_, [lambda,_|_]) :-
  'format'('<closure>'), !.
print_obj(St, Pp) :-
  pp(Pp), append(Sfs, [[Pp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), sf_s(Sfs2), v(V1), v(V2),
  'format'('('),
  print_list(St, V1, V2),
  'format'(')'), !.
print_list(St, CAR, null) :-
  print_obj(St, CAR), !.
print_list(St, CAR, Pp) :-
  pp(Pp), append(Sfs, [[Pp,[cons,V1,V2]]|Sfs2], St),
  sf_s(Sfs), sf_s(Sfs2), v(V1), v(V2),
  print_obj(St, CAR),
  'format'(' '),
  print_list(St, V1, V2), !.
print_list(St, CAR, CDR) :-
  print_obj(St, CAR),
  'format'(' . '),
  print_obj(St, CDR), !.


gen_atom(X) :- (atom(X)->true;gensym('#:G', X)).
gen_num(X) :- gensym('', Y), atom_number(Y, X).

%% If your Prolog system does not have gensym/1, you can use following  code
% gen_atom(X) :- atom(X), retract(atom_content(_)), assert(atom_content(X)), !.
% gen_atom(X) :- atom(X), assert(atom_content(X)), !.
% gen_atom(X) :-
%  atom_content(H), gen_num(N),
%  number_codes(N, Str), atom_codes(T, Str), concat_atom([H, T], X).
% gen_num(X) :- number(X), retract(num_content(_)), assert(num_content(X)), !.
% gen_num(X) :- number(X), assert(num_content(X)), !.
% gen_num(X) :- num_content(X),retract(num_content(X)), !,
%  Y is X+1, assert(num_content(Y)).

:- evaluate("(car '(a b c))").


:-evaluate("(define-syntax define-macro
  (lambda (x)
    \"Define a defmacro.\"
    (syntax-case x ()
      ((_ (macro . args) doc body1 body ...)
       (string? (syntax->datum #'doc))
       #'(define-macro macro doc (lambda args body1 body ...)))
      ((_ (macro . args) body ...)
       #'(define-macro macro #f (lambda args body ...)))
      ((_ macro transformer)
       #'(define-macro macro #f transformer))
      ((_ macro doc transformer)
       (or (string? (syntax->datum #'doc))
           (not (syntax->datum #'doc)))
       #'(define-syntax macro
           (lambda (y)
             doc
             #((macro-type . defmacro)
               (defmacro-args args))
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v)))))))))))").


:- evaluate("(define is-quote-expression?
  (lambda (v)
    (equal? (car v) 'quote)
    (is-quotation? (cdr v)))))").



:- evaluate("(define is-quotation?
  (lambda (v)
    (or (number? v)
        (boolean? v)
        (char? v)
        (string? v)
        (symbol? v)
        (null? v)
        (and (pair? v)
             (is-quotation? (car v))
             (is-quotation? (cdr v)))))").
:-evaluate(" (is-quote-expression? (quote (quote 42))))").

:-evaluate(
"(define-syntax define-macro
  (lambda (x)
    (syntax-case x ()
      ((_ (macro . args) body ...)
       #'(define-macro macro (lambda args body ...)))
      ((_ macro transformer)
       #'(define-syntax macro
           (lambda (y)
             (syntax-case y ()
               ((_ . args)
                (let ((v (syntax->datum #'args)))
                  (datum->syntax y (apply transformer v)))))))))))"
                  ).



:- retract(user:string_prolog_flag(WAS))->set_prolog_flag(string,WAS);true.
