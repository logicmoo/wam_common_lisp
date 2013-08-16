%% [tim.prolog]SIPCORE, 
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:40:00 1986
%% this file specifies the initial environment for SIP.

% PRIMITIVE "MACROS"

(X==Y) ==> define(X,Y).
`(X) ==> quote(X).
consStream(X,Y) ==> cons(X,delay(Y)).
head(X) ==> car(X).
tail(X) ==> force(cdr(X)).
theEmptyStream ==> nil.
'emptyStream?' ==> 'null?'.

%% PRIMITIVE VARIABLES.
true == quote(true).
false == quote(false).
nil == quote([]).

%% PRIMITIVE FUNCTIONS
car == `pf(car).
cdr == `pf(cdr).
cons == `pf(cons).
'eq?' == `pf(=).
'=' == `pf('=').
('+') == `pf('+').
('-') == `pf('-').
'*' == `pf('*').
'/' == `pf('/').
begin == `pf(begin).
force == `pf(force).
load == `pf(load).
'==>' == `pf('==>').
prolog == `pf(prolog).
print == `pf(print).

callCC == `pf(callCC).

null == lambda([x], 'eq?'(x,nil)).

