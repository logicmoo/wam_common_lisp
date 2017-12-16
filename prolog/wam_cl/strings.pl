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

to_prolog_string_if_needed(L,Loc):- \+ atomic(L),is_stringp(L),!,always(to_prolog_string(L,Loc)).
to_prolog_string_if_needed(L,Loc):- \+ string(L),is_symbolp(L),!,always(to_prolog_string(L,Loc)).


is_characterp(X):-var(X),!,fail.
is_characterp('#\\'(V)):- nonvar(V).

is_stringp(X):-var(X),!,fail.
%is_stringp(X):- string(X),nop(dmsg(is_stringp(X))).
is_stringp('$ARRAY'([_N],claz_base_character,List)):- nonvar(List).

cl_stringp(A, R):- t_or_nil(is_stringp(A),R).

cl_string(O,S):- to_prolog_string(O,PLS),to_lisp_string(PLS,S).

to_prolog_string(SS,SS):- notrace(var(SS)),!,break.
to_prolog_string([],"").
to_prolog_string(SS,SS):- notrace(string(SS)),!.
to_prolog_string('$ARRAY'(_N,claz_base_character,List),SS):- !,always(lisp_chars_to_pl_string(List,SS)),!.
%to_prolog_string('$ARRAY'(_,_,List),SS):-  !,lisp_chars_to_pl_string(List,SS).
to_prolog_string(S,SN):- is_symbolp(S),!,pl_symbol_name(S,S2),to_prolog_string(S2,SN).
to_prolog_string('#\\'(Code),Str):- !, (\+ number(Code)->Char=Code;char_code(Char,Code)),text_to_string(Char,Str).



% grabs ugly objects
%to_prolog_string(S,SN):- atom_concat_or_rtrace(':',S0,S),!,to_prolog_string(S0,SN).% TODO add a warjing that hte keyword was somehow misrepresented
%to_prolog_string(S,SN):- atom_concat_or_rtrace('kw_',S0,S),!,to_prolog_string(S0,SN). % TODO add a warjing that hte keyword was somehow missing
%to_prolog_string(S,SN):- notrace(catch(text_to_string(S,SN),_,fail)),!.

to_lisp_string('$ARRAY'([N],claz_base_character,List),'$ARRAY'([N],claz_base_character,List)):-!.
to_lisp_string(Text,'$ARRAY'([*],claz_base_character,List)):- always((catch(text_to_string(Text,Str),E,(dumpST,wdmsg(E),fail)),string_chars(Str,Chars),maplist(make_character,Chars,List))).

make_character(I,O):-notrace(make_character0(I,O)).
make_character0(S,'#\\'(S)):- var(S),!.
make_character0('#\\'(S),C):- !, make_character0(S,C).
make_character0(S,'#\\'(Char)):- number(S), S < 4096,char_code(Char,S).
make_character0(S,'#\\'(S)):- atom(S),name(S,[_]),!.
make_character0(S,'#\\'(S)):- atom(S),char_code(S,_),!.
make_character0(N,'#\\'(S)):- integer(N),(char_type(N,alnum)->name(S,[N]);S=N),!.
make_character0(N,C):- text_to_string_safe(N,Str),char_code_from_name(Str,Code),make_character0(Code,C),!.
make_character0(C,'#\\'(C)).

to_prolog_char('#\\'(X),O):-!,to_prolog_char(X,O).
to_prolog_char((Code),Char):- number(Code),!,char_code(Char,Code).
to_prolog_char((Atom),Char):- name(Atom,[C|Odes]),!,
  ((Odes==[] -> char_code(Char,C); 
  ((text_to_string(Atom,String),char_code_from_name(String,Code),char_code(Char,Code))))).



% SHARED SECTION
:- multifile(wl:coercion/3).
wl:coercion(In, claz_prolog_string, Out):- to_prolog_string(In,Out).
wl:coercion(In, claz_string, Out):- cl_string(In,Out).
wl:coercion(In, claz_character, Out):- make_character(In,Out).
wl:coercion(In, claz_string, Out):- cl_string(In,Out).
wl:coercion(In, claz_sequence, Out):- is_stringp(In),to_lisp_string(In,Out).
wl:coercion(In, claz_cons, Out):- functor(In,_F,A),arg(A,In,Out),is_list(Out).

wl:coercion(List, object(_,'$ARRAY'(A1,A2)), '$ARRAY'(A1,A2,List)).
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
(wl:init_args(2,cl_string_c62)).
wl:type_checked(cl_string_c62(claz_cons,claz_cons,keys,index)).
cl_string_c62(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@>,XR,YR,R).


% string>=
(wl:init_args(2,cl_string_c62_c61)).
wl:type_checked(cl_string_c62_c61(claz_cons,claz_cons,keys,index)).
cl_string_c62_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@>=,XR,YR,R).


% string<
(wl:init_args(2,cl_string_c60)).
wl:type_checked(cl_string_c60(claz_cons,claz_cons,keys,index)).
cl_string_c60(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@<,XR,YR,R).


% string<=
(wl:init_args(2,cl_string_c60_c61)).
wl:type_checked(cl_string_c60_c61(claz_cons,claz_cons,keys,index)).
cl_string_c60_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,@=<,XR,YR,R).

% string/=
(wl:init_args(2,cl_string_c47_c61)).
wl:type_checked(cl_string_c47_c61(claz_cons,claz_cons,keys,index)).
cl_string_c47_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,\==,XR,YR,R).

% string-lessp
(wl:init_args(2,cl_string_lessp)).
wl:type_checked(cl_string_lessp(claz_cons,claz_cons,keys,index)).
cl_string_lessp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,char_lessp,XR,YR,R).

% string-not-lessp
(wl:init_args(2,cl_string_not_lessp)).
wl:type_checked(cl_string_not_lessp(claz_cons,claz_cons,keys,index)).
cl_string_not_lessp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_lessp,XR,YR,R).

% string-greaterp
(wl:init_args(2,cl_string_greaterp)).
wl:type_checked(cl_string_greaterp(claz_cons,claz_cons,keys,index)).
cl_string_greaterp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_success(Start1,char_greaterp,XR,YR,R).

% string-not-greaterp
(wl:init_args(2,cl_string_not_greaterp)).
wl:type_checked(cl_string_not_greaterp(claz_cons,claz_cons,keys,index)).
cl_string_not_greaterp(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_greaterp,XR,YR,R).

char_lessp(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX@<YYY.
char_greaterp(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX@>YYY.
char_same(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), char_type(XX,upper(XXX)),char_type(YY,upper(YYY)), XXX==YYY.
char_same(X,Y):- to_prolog_char(X,XX),to_prolog_char(Y,YY), XX==YY.


% string-equals
wl:type_checked(cl_string_equals(claz_cons,claz_cons,keys,boolean)).
(wl:init_args(2,cl_string_equals)).
cl_string_equals(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_same,XR,YR,Index),
   t_or_nil(Index==[],R).


% string-not-equal
(wl:init_args(2,cl_string_not_equal)).
wl:type_checked(cl_string_not_equal(claz_cons,claz_cons,keys,index)).
cl_string_not_equal(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_same,XR,YR,R).


% string=
wl:type_checked(cl_string_c61(claz_cons,claz_cons,keys,boolean)).
(wl:init_args(2,cl_string_c61)).
cl_string_c61(X,Y,Keys,R):-
   range_1_and_2(X,Y,Keys,XR,YR,Start1),
   index_of_first_failure(Start1,char_exact,XR,YR,Index),
   t_or_nil(Index==[],R).


%is_string_equal_case_sensitive(X,Y):- to_prolog_string(X,XX),to_prolog_string(Y,YY),XX==YY.
%is_string_equal_case_insensitive(X,Y):- to_prolog_string(X,XX),to_prolog_string(Y,YY),
%  (XX==YY-> true ; (string_upper(XX,XXX),string_upper(YY,YYY),XXX==YYY)).
cl_char(String,Index,Char):-cl_aref(String,[Index],Char).
  



range_1_and_2(X,Y,[],X,Y,0):-!.
range_1_and_2(X,Y,Keys,XR,YR,Start1):-
   key_value(Keys,start1,Start1,0),key_value(Keys,end1,End1,[]),
   key_value(Keys,start2,Start2,0),key_value(Keys,end2,End2,[]),
   range_subseq(X,Start1,End1,XR),
   range_subseq(Y,Start2,End2,YR).

% key_value(Keys,Name,Value,Default).
key_value(Keys,Name,Value):- is_dict(Keys),!,Keys.Name=Value,!.
key_value(Keys,Name,Value):- get_plist_value(Keys,Name,Value).
key_value(Keys,Name,Value,_Default):- key_value(Keys,Name,Value),!.
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



