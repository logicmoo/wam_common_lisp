/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (arglists.pl)
 *
 *
 * Douglas'' Notes 2017:
 *
 *
 *

*******************************************************************/
:- module(kp4rms, []).

:- include('header').

xform_with_ident([],_Ident,[]).
xform_with_ident([Y0|YR0],Ident,[Y|YR]):-
   call_as_ident(Ident,Y0,Y),
   xform_with_ident(YR0,Ident,YR).

call_as_ident(Pred,X,Result):- function(Pred,X,Result).

apply_as_pred(EqlPred,X,Y,Z):-call(EqlPred,X,Y,Z,R)->R\==[].
apply_as_pred(EqlPred,X,Y):-call(EqlPred,X,Y,R)->R\==[].
apply_as_pred(EqlPred,X):-call(EqlPred,X,R)->R\==[].

% Maybe move to funcall 
function(f_funcall,Pred,Y,Result):- !, function(Pred,Y,Result).
function(Pred,X,Y,Result):- f_apply(Pred,[X,Y],Result).
function(X,function(X)).
% used by call_as_ident/3
function([],X,X):-!.
function(Pred,X,Result):- call(Pred,X,Result),!.

% Maybe move to arglists
% key_value(Keys,Name,Value,Default).
key_value(Keys,Name,Value):- is_dict(Keys),!,Keys.Name=Value,!.
key_value(Keys,Name,Value):- get_plist_value(f_eql,Keys,Name,zzzz666,Value),Value\==zzzz666.
key_value(Keys,Name,Value,_Default):- key_value(Keys,Name,Value),!.
key_value(_Keys,_Name,Default,Default).

get_identity_pred(Keys,K,Pred):- 
  key_value(Keys,K,Value) -> to_function(Value,Pred); Pred = (=).

get_test_pred(IfMissing,Keys,Pred):-
  key_value(Keys,kw_test_not,Value) -> to_neg_function(Value,Pred);
  key_value(Keys,kw_test,Value)  -> to_function(Value,Pred);
  Pred = IfMissing.

%to_function(function(ValueI),ValueO):-!,to_function(ValueI,ValueO).
to_function(Value,Call):- find_operator_or_die(_Env,kw_function,Value,Call).
to_neg_function(Value,not_fn(Neg)):-to_function(Value,Neg).

not_fn(Value,A):- \+ call(Value,A).
not_fn(Value,A,B):- \+ call(Value,A,B).
not_fn(Value,A,B,C):- \+ call(Value,A,B,C).

nth_param(Optionals,N,Default,Value):- nth1(N,Optionals,Value)->true;Default=Value.
nth_param(Optionals,N,Default,Value,PresentP):- nth1(N,Optionals,Value)->(PresentP=t);(Default=Value,PresentP=[]).


:- fixup_exports.

