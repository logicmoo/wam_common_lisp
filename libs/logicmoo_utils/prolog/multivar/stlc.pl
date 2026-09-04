:- use_module(library(chr)).

:- chr_type prop --->
    atomic(atom) ; top ; bot ; conj(prop, prop) ; impl(prop, prop) ; disj(prop, prop).
:- chr_type deriv --->
    start(natural)       ; top_i                ; bot_e(deriv) ;
    conj_i(deriv, deriv) ; conj_e1(deriv)       ; conj_e2(deriv) ;
    impl_i(deriv)        ; impl_e(deriv, deriv) ;
    disj_i1(deriv)       ; disj_i2(deriv)       ; disj_e(deriv, deriv, deriv).

%:- table(derives/3).
:- chr_constraint derives(?deriv,?,?prop).

derives(start(N), Gamma, Phi) :- nth0(N, Gamma, Phi).
derives(top_i, Gamma, top).
derives(bot_e(D), Gamma, Phi) :- derives(D, Gamma, bot).

derives(conj_i(A, B), Gamma, conj(Phi, Psi)) :- derives(A, Gamma, Phi), derives(B, Gamma, Psi).
derives(conj_e1(D), Gamma, Phi) :- derives(D, Gamma, conj(Phi, Psi)).
derives(conj_e2(D), Gamma, Psi) :- derives(D, Gamma, conj(Phi, Psi)).

derives(impl_i(D), Gamma, impl(Phi, Psi)) :- derives(D, [Phi | Gamma], Psi).
derives(impl_e(A, B), Gamma, Psi) :- derives(A, Gamma, impl(Phi, Psi)), derives(B, Gamma, Phi).

derives(disj_i1(D), Gamma, disj(Phi, Psi)) :- derives(D, Gamma, Phi).
derives(disj_i2(D), Gamma, disj(Phi, Psi)) :- derives(D, Gamma, Psi).
derives(disj_e(A, B, C), Gamma, Xi) :- derives(A, Gamma, disj(Phi, Psi)), derives(B, [Phi | Gamma], Xi), derives(C, [Psi | Gamma], Xi).

