/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * @TODO - add writable strings
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(string, []).



% base-string == (vector base-character) 
% simple-base-string == (simple-array base-character (*))

as_string_upper(C,SN):- compound(C),\+ is_list(C),functor(C,_P,A),arg(A,C,S),!, as_string_upper(S,SN).
as_string_upper(S,U):- to_prolog_string_anyways(S,D),string_upper(D,U).

is_characterp(X):-var(X),!,fail.
is_characterp('#\\'(V)):- nonvar(V).

is_stringp(X):- string(X),nop(dbginfo(is_stringp(X))).
is_stringp(X):- is_lisp_string(X).

is_lisp_string(X):-var(X),!,fail.
is_lisp_string('$ARRAY'([_N],claz_base_character,List)):- nonvar(List).

% deduced now
% GROVELED f_stringp(A, R):- t_or_nil(is_stringp(A),R).

f_string(O,S):- to_prolog_string(O,PLS),to_lisp_string(PLS,S).

% only handles the same things as #'STRING
to_prolog_string(SS,SS):- notrace(string(SS)),!.
to_prolog_string(SS,SS):- notrace(var(SS)),!,break.
to_prolog_string([],"").
to_prolog_string('$ARRAY'(_N,claz_base_character,List),SS):- !,always(lisp_chars_to_pl_string(List,SS)),!.
%to_prolog_string('$ARRAY'(_,_,List),SS):-  !,lisp_chars_to_pl_string(List,SS).
to_prolog_string('#\\'(Char),Str):- !, f_char_code('#\\'(Char),Code),text_to_string([Code],Str).
to_prolog_string(S,SN):- is_symbolp(S),!,pl_symbol_name(S,S2),to_prolog_string(S2,SN).

% Only Make a STRING if not already a Prolog String
to_prolog_string_if_needed(L,Loc):- \+ string(L),!,always(to_prolog_string_anyways(L,Loc)).
% Always make a STRING
to_prolog_string_anyways(I,O):- atom(I),upcase_atom(I,I),!,atom_string(I,O).
to_prolog_string_anyways(I,O):- is_pathnamep(I),pl_namestring(I,O),!.
to_prolog_string_anyways(I,O):- to_prolog_string(I,O),!.
to_prolog_string_anyways(I,O):- is_classp(I),claz_to_symbol(I,Symbol),!,to_prolog_string_anyways(Symbol,O).
to_prolog_string_anyways(I,O):- always(atom_string(I,O)),!.



% grabs ugly objects
%to_prolog_string(S,SN):- atom_concat_or_rtrace(':',S0,S),!,to_prolog_string(S0,SN).% TODO add a warjing that hte keyword was somehow misrepresented
%to_prolog_string(S,SN):- atom_concat_or_rtrace('kw_',S0,S),!,to_prolog_string(S0,SN). % TODO add a warjing that hte keyword was somehow missing
%to_prolog_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.

to_lisp_string('$ARRAY'([N],claz_base_character,List),'$ARRAY'([N],claz_base_character,List)):-!.
to_lisp_string(Text,'$ARRAY'([*],claz_base_character,List)):- always((catch(text_to_string(Text,Str),E,
  (dumpST,userout(E),fail)),string_chars(Str,Chars),maplist(make_lisp_character,Chars,List))).

% SHARED SECTION
wl:coercion(In, claz_prolog_string, Out):- to_prolog_string(In,Out).
wl:coercion(In, claz_string, Out):- f_string(In,Out).
wl:coercion(In, claz_character, Out):- make_lisp_character(In,Out).
wl:coercion(In, claz_string, Out):- f_string(In,Out).
wl:coercion(In, claz_cons, Out):- functor(In,_F,A),arg(A,In,Out),is_list(Out).

wl:coercion(List, object(_,'$ARRAY'(A1,A2)), '$ARRAY'(A1,A2,List)).
wl:coercion(In, claz_sequence, Out):- is_stringp(In),to_lisp_string(In,Out).
wl:coercion(In, sequence(string,'$ARRAY'(A1,A2)), List):- string(In),to_lisp_string(In,'$ARRAY'(A1,A2,List)).
wl:coercion(In, sequence(string,'$ARRAY'(A1,A2)), List):- is_stringp(In),to_lisp_string(In,'$ARRAY'(A1,A2,List)).

wl:coercion([H|T], object(Cons,_), [H|T]):- Cons==claz_cons.
wl:coercion([H|T], sequence(claz_cons,claz_cons), [H|T]):-!. 

% index_of_first(N,Pred,X,Y,R)
index_of_first_success(N,Pred,[X|XX],[Y|YY],R):- !,
 ( call(Pred,X,Y) -> R = N;
    (N2 is N+1, index_of_first_success(N2,Pred,XX,YY,R))).
index_of_first_success(_,_,_,_,[]).
% index_of_first(N,Pred,X,Y,R)
index_of_first_failure(N,Pred,[X|XX],[Y|YY],R):- !,
 ( call(Pred,X,Y) -> R = N;
    (N2 is N+1, index_of_first_failure(N2,Pred,XX,YY,R))).
index_of_first_failure(_,_,_,_,[]).

% http://clhs.lisp.se/Body/f_stgeq_.htm

% string>
(wl:init_args(2,string_c62)).
wl:type_checked(f_string_c62(claz_cons,claz_cons,keys,index)).
f_string_c62(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@>,XR,YR,R).


% string>=
(wl:init_args(2,string_c62_c61)).
wl:type_checked(f_string_c62_c61(claz_cons,claz_cons,keys,index)).
f_string_c62_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@>=,XR,YR,R).


% string<
(wl:init_args(2,string_c60)).
wl:type_checked(f_string_c60(claz_cons,claz_cons,keys,index)).
f_string_c60(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@<,XR,YR,R).


% string<=
(wl:init_args(2,string_c60_c61)).
wl:type_checked(f_string_c60_c61(claz_cons,claz_cons,keys,index)).
f_string_c60_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@=<,XR,YR,R).

% string/=
(wl:init_args(2,string_c47_c61)).
wl:type_checked(f_string_c47_c61(claz_cons,claz_cons,keys,index)).
f_string_c47_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,\==,XR,YR,R).

% string-lessp
(wl:init_args(2,string_lessp)).
wl:type_checked(f_string_lessp(claz_cons,claz_cons,keys,index)).
f_string_lessp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,char_lessp,XR,YR,R).

% string-not-lessp
(wl:init_args(2,string_not_lessp)).
wl:type_checked(f_string_not_lessp(claz_cons,claz_cons,keys,index)).
f_string_not_lessp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_lessp,XR,YR,R).

% string-greaterp
(wl:init_args(2,string_greaterp)).
wl:type_checked(f_string_greaterp(claz_cons,claz_cons,keys,index)).
f_string_greaterp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,char_greaterp,XR,YR,R).

% string-not-greaterp
(wl:init_args(2,string_not_greaterp)).
wl:type_checked(f_string_not_greaterp(claz_cons,claz_cons,keys,index)).
f_string_not_greaterp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_greaterp,XR,YR,R).

char_lessp(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX@<YYY.
char_greaterp(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX@>YYY.
char_same(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX==YYY.
char_same(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), XX==YY.

char_exact(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), XX==YY.


% string-equals
wl:type_checked(f_string_equals(claz_cons,claz_cons,keys,boolean)).
(wl:init_args(2,string_equals)).
f_string_equals(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_same,XR,YR,Index),
   t_or_nil(Index==[],R).


% string-not-equal
(wl:init_args(2,string_not_equal)).
wl:type_checked(f_string_not_equal(claz_cons,claz_cons,keys,index)).
f_string_not_equal(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_same,XR,YR,R).


% string=
wl:type_checked(f_string_c61(claz_cons,claz_cons,keys,boolean)).
(wl:init_args(2,string_c61)).
f_string_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_exact,XR,YR,Index),
   t_or_nil(Index==[],R).


%is_string_equal_case_sensitive(X,Y):- to_prolog_string(X,XX),to_prolog_string(Y,YY),XX==YY.
%is_string_equal_case_insensitive(X,Y):- to_prolog_string(X,XX),to_prolog_string(Y,YY),
%  (XX==YY-> true ; (string_upper(XX,XXX),string_upper(YY,YYY),XXX==YYY)).
f_char(String,Index,Char):-f_aref(String,[Index],Char).
  




:- fixup_exports.



