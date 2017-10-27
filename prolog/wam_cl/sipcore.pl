%% [tim.prolog]SIPCORE, 
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:40:00 1986
%% this file specifies the initial environment for SIP.
/*
:- set_prolog_flag(backquoted_string,false).
:- dynamic(((( ==> ))/2)).
:- op(1100,xfx, (==)).	

:- op(900,xfy,user:'==>').		% used for macros.
:- op(500,fx,user:'`').		% `x = quote(x).
*/

% PRIMITIVE "MACROS"

(X==Y) ==> define(X,Y).
'`'(X) ==> quote(X).
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
car == quote( pf(car)).
cdr == quote( pf(cdr)).
cons == quote(pf(cons)).
'eq?' == quote(pf(=)).
'=' == quote(pf('=')).
('+') == quote(pf('+')).
('-') == quote(pf('-')).
'*' == quote(pf('*')).
'/' == quote(pf('/')).
begin == quote(pf(begin)).
force == quote(pf(force)).
load == quote(pf(load)).
'==>' == quote(pf('==>')).
prolog == quote(pf(prolog)).
print == quote(pf(print)).

callCC == quote(pf(callCC)).

null == lambda([x], 'eq?'(x,nil)).

