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

:- set_module(class(library)).

% base-string == (vector base-character) 
% simple-base-string == (simple-array base-character (*))

as_string_upper(C,SN):- compound(C),\+ is_list(C),functor(C,_P,A),arg(A,C,S),!, as_string_upper(S,SN).
as_string_upper(S,U):- to_prolog_string(S,D),string_upper(D,U).

is_stringp(X):- string(X).
is_stringp('$ARRAY'([_N],claz_base_character,List)):- nonvar(List).
cl_stringp(A, R):- t_or_nil(is_stringp(A),R).

cl_string(O,S):- to_prolog_string(O,PLS),to_lisp_string(PLS,S).

to_prolog_string(SS,SS):- string(SS),!.
to_prolog_string('$ARRAY'([_N],claz_base_character,List),SS):- !,maplist(to_prolog_codes,List,Codes),text_to_string(Codes,SS).
to_prolog_string('$ARRAY'(_,_,List),SS):- !,maplist(to_prolog_codes,List,Codes),text_to_string(Codes,SS).
to_prolog_string(S,SN):- is_symbolp(S),pl_symbol_name(S,SN).
% grabs ugly objects
to_prolog_string(S,SN):- atom_concat_or_rtrace(':',S0,S),!,to_prolog_string(S0,SN).% TODO add a warjing that hte keyword was somehow misrepresented
to_prolog_string(S,SN):- atom_concat_or_rtrace('kw_',S0,S),!,to_prolog_string(S0,SN). % TODO add a warjing that hte keyword was somehow missing
to_prolog_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.

to_prolog_codes('$CHAR'(Int),Int).
to_prolog_codes((Int),Int).

to_lisp_string('$ARRAY'([N],claz_base_character,List),'$ARRAY'([N],claz_base_character,List)):-!.
to_lisp_string(Str,'$ARRAY'([_],claz_base_character,List)):- atom_chars(Str,Chars),maplist(make_character,Chars,List).

% SHARED SECTION
:- multifile(wl:coercion/3).
wl:coercion(In, prolog_string, Out):- to_prolog_string(In,Out).
wl:coercion(In, string, Out):- cl_string(In,Out).
wl:coercion(In, list, Out):- is_stringp(In),to_lisp_string(In,Out).
wl:coercion(In, sequence(string,chars), Out):- is_stringp(In),to_lisp_string(In,Out).
wl:coercion(In, prolog_list, Out):- functor(In,_F,A),arg(A,In,Out),is_list(Out).

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

% string<
wl:type_checked(cl_string_c60(prolog_list,prolog_list,index)).
cl_string_c60(X,Y,R):- index_of_first_success(0,@<,X,Y,R).

% string>
wl:type_checked(cl_string_c62(prolog_list,prolog_list,index)).
cl_string_c62(X,Y,R):- index_of_first_success(0,@>,X,Y,R).

% string>=
wl:type_checked(cl_string_c62_c61(prolog_list,prolog_list,index)).
cl_string_c62_c61(X,Y,R):- index_of_first_success(0,@>=,X,Y,R).

% string<=
wl:type_checked(cl_string_c60_c61(prolog_list,prolog_list,index)).
cl_string_c60_c61(X,Y,R):- index_of_first_success(0,@<=,X,Y,R).

% string/=
wl:type_checked(cl_string_c47_c61(prolog_list,prolog_list,index)).
cl_string_c47_c61(X,Y,R):- index_of_first_success(0,\==,X,Y,R).


% string-lessp
wl:type_checked(cl_string_lessp(prolog_list,prolog_list,index)).
cl_string_lessp(X,Y,R):- index_of_first_success(0,char_lessp,X,Y,R).

% string-not-lessp
wl:type_checked(cl_string_not_lessp(prolog_list,prolog_list,index)).
cl_string_not_lessp(X,Y,R):- index_of_first_failure(0,char_lessp,X,Y,R).

% string-greaterp
wl:type_checked(cl_string_greaterp(prolog_list,prolog_list,index)).
cl_string_greaterp(X,Y,R):- index_of_first_success(0,char_greaterp,X,Y,R).

% string-not-greaterp
wl:type_checked(cl_string_not_greaterp(prolog_list,prolog_list,index)).
cl_string_not_greaterp(X,Y,R):- index_of_first_failure(0,char_greaterp,X,Y,R).

% string-not-equal
wl:type_checked(cl_string_not_equal(prolog_list,prolog_list,index)).
cl_string_not_equal(X,Y,R):- index_of_first_failure(0,char_same,X,Y,R).

char_lessp(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX@<YYY.
char_greaterp(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX@>YYY.
char_same(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX==YYY.

% string-equals
wl:type_checked(cl_string_equals(prolog_string,prolog_string,boolean)).
cl_string_equals(X,Y,R):- t_or_nil(is_string_equal_case_insensitive(X,Y),R).

% string=
wl:type_checked(cl_string_c61(prolog_string,prolog_string,boolean)).
cl_string_c61(X,Y,R):- t_or_nil(is_string_equal_case_sensitive(X,Y),R).

% string= with Keys ?
wl:type_checked(cl_string_c61(prolog_list,prolog_list,keys,index)).
cl_string_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,_),
   t_or_nil(is_string_equal_case_sensitive(XR,YR),R).

% string-equals with Keys ?
wl:type_checked(cl_string_equals(prolog_list,prolog_list,keys,index)).
cl_string_equals(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,_),
   t_or_nil(is_string_equal_case_insensitive(XR,YR),R).



is_string_equal_case_sensitive(X,Y):- to_prolog_string(X,XX),to_prolog_string(Y,YY),XX==YY.
is_string_equal_case_insensitive(X,Y):- to_prolog_string(X,XX),to_prolog_string(Y,YY),
  (XX==YY-> true ; (string_upper(XX,XXX),string_upper(YY,YYY),XXX==YYY)).

wl:type_checked(cl_length(prolog_list,integer)).
cl_length(Sequence,Len):- length(Sequence,Len).


% string< with Keys ?
wl:type_checked(cl_string_c60(prolog_list,prolog_list,keys,index)).
cl_string_c60(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(0,@<,XR,YR,RM),
   R is Start1+RM.

range_1_and_2(X,Y,[],X,Y,0):-!.
range_1_and_2(X,Y,Keys,XR,YR,Start1):-
   key_value(Keys,start1,Start1,0),key_value(Keys,end1,End1,[]),
   key_value(Keys,start2,Start2,0),key_value(Keys,end2,End2,[]),
   range_subseq(X,Start1,End1,XR),
   range_subseq(Y,Start2,End2,YR).

% key_value(Keys,Name,Value,Default).
key_value(Keys,Name,Value,_Default):- Keys.Name=Value,!.
key_value(_Keys,_Name,Default,Default).

range_subseq(X,0,[],X):-!.
range_subseq(X,N,[],XR):- !, subseqence_from(X,N,XR).
range_subseq(X,0,N,XR):-!, subseqence_until(X,N,XR).
range_subseq(X,Start,End,XR):-!, subseqence_from(X,Start,XM),NewEnd is End - Start,subseqence_until(XM,NewEnd,XR).

subseqence_from(X,0,X):-!.
subseqence_from([_|X],N,XR):- N2 is N -1, subseqence_from(X,N2,XR).

subseqence_until(_,0,[]):-!.
subseqence_until([X|XX],N,[X|XR]):-  N2 is N -1,subseqence_from(XX,N2,XR).




:- fixup_exports.



