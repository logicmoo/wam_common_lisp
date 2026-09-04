/* Part of LogicMOO Base An Implementation a MUD server in SWI-Prolog
% ===================================================================
% File 'dcg_meta.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: logicmoo@gmail.com ;
% Version: 'logicmoo_util_bugger.pl' 1.0.0
% Revision:  $Revision: 1.1 $
% Revised At:   $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
:- module(dcg_meta,[
         do_dcg_util_tests/0,
         isVar/1,
         isQVar/1,
         isVarOrVAR/1,
         file_eof//0,
         charvar/1,
         cspace//0,
         cwhite//0,
         mw//1,
         bx/1,
         zalwayz//1,
         zalwayz/1,
         one_blank//0,
         set_dcg_meta_reader_options/2,
         phrase_from_stream_nd/2,
         parse_meta_term/3,
         read_string_until//2,
         read_string_until_no_esc//2,
         dcgOneOrMore//1,
         dcgOptional//1,
         dcgZeroOrMore//1,
         dcgOptionalGreedy//1,
         dcgAnd//2,
         dcgAnd//3,
         dcgAnd//4,
         dcgMust//1,
         %dumpList/1,
         dcgSeqLen//1,
         dcgOr//2,
         dcgNot//1,
         theString//1,
         theString//2,
         theText//1,
         theCode//1,
         dcgLenBetween/4,
         notrace_catch_fail/1,
         notrace_catch_fail/3,
         % unit test functions
         do_dcgTest/3,
         do_dcgTest_startsWith/3,
         decl_dcgTest_startsWith/2,
         decl_dcgTest_startsWith/3,
         decl_dcgTest/2,
         decl_dcgTest/3,
         dcgReorder/4
	 ]).


/** <module> Utility LOGICMOO_DCG_META
This module allows DCGs to use meta predicates like And Or Not.

- @author Douglas R. Miles
- @license LGPL
*/
:- set_module(class(library)).

:- meta_predicate track_stream(*,0).
:- meta_predicate read_string_until(*,*,//,?,?).
:- meta_predicate read_string_until_pairs(*,//,?,?).

:- system:use_module(library(listing)).
:- system:use_module(library(lists)).
:- system:use_module(library(time)).
:- system:use_module(library(readutil)).


:- dynamic(t_l:dcg_meta_reader_options/2).
:- thread_local(t_l:dcg_meta_reader_options/2).

set_dcg_meta_reader_options(N,V):- retractall(t_l:dcg_meta_reader_options(N,_)),asserta(t_l:dcg_meta_reader_options(N,V)).
get_dcg_meta_reader_options(N,V):- t_l:dcg_meta_reader_options(N,V).



% Portray ASCII code sequences (for debugging DCGs)
user:portray(List):- compound(List),compound_name_arity([_,_],F,A),compound_name_arity(List,F,A),
    List=[H|_],integer(H),H>9,user_portray_dcg_seq(List).

user_portray_dcg_seq(List):- \+ is_list(List),!,between(32,1,Len),l_length(Left,Len),append(Left,_,List), ground(Left),!,
   catch(atom_codes(W,Left),_,fail),format("|~w ___|",[W]).
user_portray_dcg_seq(List):- is_codelist(List), catch(atom_codes(Atom,List),_,fail),l_length(List,Len),
  (Len < 32 -> format("`~w`",[Atom]) ;
    (l_length(Left,26),append(Left,_Rest,List),format(atom(Print),"~s",[Left]),format("|~w ... |",[Print]))).

l_length(L,N):- integer(N),!,length(L,N).
l_length(L,N):- is_list(L),!,length(L,N).
l_length(L,N):- !,fail,length(L,N),!.
l_length([_|L],N):- !, l_length0(L,N1),N is N1+1.
l_length0(L,N):- var(L),!,N=0.
l_length0([_|L],N):- !, l_length0(L,N1),N is N1+1.
l_length0(_,1).


% :- ensure_loaded(library(logicmoo_utils)).
% :- ensure_loaded(library(logicmoo_util_strings)).

:- meta_predicate bx(0).
:- meta_predicate expr_with_text(*,2,*,*,*).
:- meta_predicate locally_setval(*,*,0).
:- meta_predicate notrace_catch_fail(0).
:- meta_predicate notrace_catch_fail(0,?,0).
:- meta_predicate phrase_from_buffer_codes(//,*).
:- meta_predicate phrase_from_buffer_codes_nd(//,*).
:- meta_predicate phrase_from_pending_stream(*,//,*).
:- meta_predicate phrase_from_pending_stream(//,?).
:- meta_predicate phrase_from_stream_lazy_part(//,*).
:- meta_predicate read_string_until(*,*,//,?,?).
:- meta_predicate read_string_until_no_esc(*,//,?,?).
:- meta_predicate read_string_until_pairs(*,//,?,?).
:- meta_predicate zalwayz(//,?,?).
:- meta_predicate(zalwayz(0)).
:- meta_predicate track_stream(*,0).
:- meta_predicate always_b(//,?,?).
:- meta_predicate phrase_from_stream_nd(//,+).
:- meta_predicate read_string_until(*,//,?,?).


:- meta_predicate dcgLeftOfMid(?,//,?,?).
:- meta_predicate dcgLeftMidRight(//,//,//,?,?).

:- meta_predicate dcgAnd(//,//,//,//,?,?).
:- meta_predicate dcgAnd(//,//,//,?,?).
:- meta_predicate dcgAnd(//,//,?,?).
:- meta_predicate dcgAndRest(//,*,*,*).
:- meta_predicate dcgBoth(//,//,*,*).
:- meta_predicate dcgIgnore(//,?,?).
:- meta_predicate dcgLeftOf(//,*,*,*).
:- meta_predicate dcgMust(//,?,?).
:- meta_predicate dcgMidLeft(//,*,//,*,?).
:- meta_predicate dcgNot(//,?,?).
:- meta_predicate dcgOnce(//,?,?).
:- meta_predicate dcgOnceOr(//,//,?,?).
:- meta_predicate dcgOneOrMore(//,?,*).
:- meta_predicate dcgOptional(//,?,?).
:- meta_predicate dcgOptionalGreedy(//,?,?).
:- meta_predicate dcgOr(//,//,//,//,//,?,?).
:- meta_predicate dcgOr(//,//,//,//,?,?).
:- meta_predicate dcgOr(//,//,//,?,?).
:- meta_predicate dcgOr(//,//,?,?).
:- meta_predicate dcgReorder(//,//,?,?).
:- meta_predicate dcgStartsWith(//,?,?).
:- meta_predicate dcgStartsWith0(//,?,*).
:- meta_predicate dcgStartsWith1(//,?,?).
:- meta_predicate dcgTraceOnFailure(0).
:- meta_predicate dcgWhile(?,//,?,?).
:- meta_predicate dcgZeroOrMore(//,?,*).
:- meta_predicate decl_dcgTest(?,?).
:- meta_predicate decl_dcgTest(?,?,?).
:- meta_predicate decl_dcgTest_startsWith(?,?,?).
:- meta_predicate do_dcgTest(*,//,0).
:- meta_predicate do_dcgTest_startsWith(?,//,?).
:- meta_predicate suggestVar(2,*,?).
:- meta_predicate theAll(//,?,?).
:- meta_predicate theCode(?,?,?).

% :- meta_predicate decl_dcgTest(?,//).
% :- meta_predicate decl_dcgTest_startsWith(?,*,0).
% :- meta_predicate decl_dcgTest(?,//,0).
% :- meta_predicate theCode(0,*,*).

% this is a backwards compatablity block for SWI-Prolog 6.6.6

:- if(current_prolog_flag(dialect,swi)).
:- dynamic(double_quotes_was_in_dcg/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_in_dcg(WAS)).
:- retract(double_quotes_was_in_dcg(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was_in_dcg(WAS)).
:- set_prolog_flag(double_quotes,string).
:- endif.

isVarOrVAR(V):-var(V),!.
isVarOrVAR('$VAR'(_)).
isVar(V):- (isVarOrVAR(V);isQVar(V)),!.
isQVar(Cvar):-atom(Cvar),atom_concat('?',_,Cvar).

:- dynamic
   decl_dcgTest/2,
   decl_dcgTest/3,
   decl_dcgTest_startsWith/2,
   decl_dcgTest_startsWith/3.


decl_dcgTest(X,Y):- nonvar(Y),!,do_dcgTest(X,Y,true).
decl_dcgTest(X,Y,Z):- nonvar(Y),!,do_dcgTest(X,Y,Z).
decl_dcgTest_startsWith(X,Y):- nonvar(Y),!,do_dcgTest(X,dcgStartsWith(Y),true).
decl_dcgTest_startsWith(X,Y,Z):- nonvar(Y),!,do_dcgTest(X,dcgStartsWith(Y),Z).
% ========================================================================
% getWhatnot helpers
% ========================================================================

getText([],[]).
getText(L,Txt):-member([txt|Txt],L),!.
getText([L|List],Text):-getText(L,Text1),getText(List,Text2),append(Text1,Text2,Text),!.
getText(F,S):-compound_name_arity(F,_,3),arg(2,F,S),!.
getText(S,S).


% ========================================================================
% theWhatnot helpers  (parses contents of terminals)
% ========================================================================
:- style_check(-discontiguous).



equals_text(S,Data):- is_list(Data),member([txt,S0],Data),!,equals_text(S,S0).
equals_text(S,S):- !.
%equals_text(S,S0):- var(S),var(S0),!,S=S0.
equals_text(S,S0):- var(S0),text_to_string(S,S0),!.
equals_text(S,S0):- var(S),text_to_string(S0,S),!.
equals_text(S,S0):- text_to_string(S,SS),text_to_string(S0,SS).

decl_dcgTest("this is text",theText([this,is,text])).

% Text

theText(Text) --> {Text==[],!},[].
theText([S|Text]) --> {nonvar(S),!},theText0(S),!,theText(Text).

theText([S|Text]) --> theText0(S),theText(Text).
theText([]) --> [].

%theText([S|Text],[S0|TextData],More):- member([txt,S0|TextData],Data),equals_text(S,S0),append(Text,More,TextData).

%  dtrace, do_dcgTest_startsWith("this is text", dcg_meta:dcgStartsWith1(theText(["this"])), true) .

% Looser text test?
theText0(_,W,_):- W==[],!,fail.
theText0(S) --> {atomic(S),atom_concat('"',Right,S),atom_concat(New,'"',Right),!},theText(New).
theText0(S) --> {atomic(S),concat_atom([W1,W2|List],' ',S),!},theText([W1,W2|List]).
theText0(S) --> {!}, [Data],{equals_text(S,Data)}.




decl_dcgTest("this is a string",theString("this is a string")).
theString(String) --> theString(String, " ").

atomic_to_string(S,S):- string(S),!.
atomic_to_string(S,Str):-sformat(Str,'~w',[S]).

atomics_to_string_str(L,S,A):-catch(atomics_to_string(L,S,A),_,fail).
atomics_to_string_str(L,S,A):-atomics_to_string_str0(L,S,A).

atomics_to_string_str0([],_Sep,""):-!.
atomics_to_string_str0([S],_Sep,String):-atom(S),!,string_to_atom(String,S).
atomics_to_string_str0([S],_Sep,S):- string(S),!.
atomics_to_string_str0([S|Text],Sep,String):-
   atomic_to_string(S,StrL),
   atomics_to_string_str0(Text,Sep,StrR),!,
   new_a2s([StrL,StrR],Sep,String).

% theString(String,Sep) --> [S|Text], {atomic_list_concat_catch([S|Text],Sep,String),!}.
theString(String,Sep) --> [S|Text], {atomics_to_string_str([S|Text],Sep,String),!}.

decl_dcgTest_startsWith([a,b|_],theCode(X=1),X==1).
decl_dcgTest_startsWith("anything",theCode(X=1),X==1).
decl_dcgTest("",theCode(X=1),X==1).
theCode(Code) --> [],{Code}.


decl_dcgTest([a,b|C],theAll([a,b|C])).
% theAll(X)--> X.
theAll(X, B, C) :- var(X),X=B,C=[],!.
theAll(X, B, C) :- phrase(X, B, C).

decl_dcgTest([a,b|C],theRest(X),X==[a,b|C]).
theRest(X, X, []).



theName(Var,S,_) :-getText(S,Text),suggestVar(=,Text,Var),!.

%suggestVar(_,Subj,Subj2):-makeName(Subj,Subj2),!.

suggestVar(_Gensym,Subj,Subj):-var(Subj),!.%,true,!.
suggestVar(_Gensym,Subj,_Subj2):-var(Subj),!.%,true,!.
suggestVar(Gensym,[W|ORDS],Subj):-!,ignore((once((nonvar(ORDS),toPropercase([W|ORDS],Proper),concat_atom(['Hypothetic'|Proper],'-',Suj),call(Gensym,Suj,SubjSl),ignore(SubjSl=Subj))))),!.
%suggestVar(Gensym,[W|ORDS],Subj):-!,ignore(quietly_pfs(once((nonvar(ORDS),concat_atom(['?'|[W|ORDS]],'',Suj),call(Gensym,Suj,SubjSl),toUppercase(SubjSl,SubjS),ignore(SubjS=Subj))))),!.
suggestVar(_Gensym,[],_):-!.%suggestVar(gensym,[A],Subj),!.
suggestVar(Gensym,A,Subj):-suggestVar(Gensym,[A],Subj),!.



%makeName(Subj,Subj2):-toCreate(Subj,hypotheticDenotation(Subj,_,string(Words))),!,makeName(Words,Subj2),!.
makeName(A,A):-!.
makeName(Subj,Subj2):-var(Subj),!,term_to_atom(Subj,Atom),makeName(['Hypothetic',Atom],Subj2),!.
makeName([],Subj2):-!,makeName(_Subj,Subj2),!.
makeName(Subj,Subj2):-atom(Subj),atom_concat('?',Sub2,Subj),!,makeName(Sub2,Subj2),!.
makeName(A,Subj):-atom(A),!,makeName([A],Subj),!.
makeName([W|ORDS],Subj):-nonvar(ORDS),!,toPropercase([W|ORDS],PCASE),concat_atom(['Hypothetic'|PCASE],'-',Suj),gensym(Suj,Subj),!.

leastOne([_CO|_LSS]).

% ========================================================================
% dcgWhatnot helpers  (meta interprets)
% ========================================================================

% TODO: when using the DCG to generate instead of test it will move the C before the P
% dcgReorder(P,C) --> P, C.
:- export(dcgReorder//2).
dcgReorder(P, C, B, E):- phrase(P, B, D), phrase(C, D, E).

:- export(dcgSeq//2).
dcgSeq(X,Y,[S0,S1|SS],E):-phrase((X,Y),[S0,S1|SS],E).

:- export(dcgBoth//2).
dcgBoth(DCG1,DCG2,S,R) :- append(L,R,S),phrase(DCG1,L,[]),once(phrase(DCG2,L,[])).

dcgAnd(DCG1,DCG2,DCG3,DCG4,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E),phrase(DCG3,S,E),phrase(DCG4,S,E).
dcgAnd(DCG1,DCG2,DCG3,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E),phrase(DCG3,S,E).
dcgAnd(DCG1,DCG2,S,E) :- phrase(DCG1,S,E),phrase(DCG2,S,E).
dcgOr(DCG1,DCG2,DCG3,DCG4,DCG5,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E);phrase(DCG4,S,E);phrase(DCG5,S,E).
dcgOr(DCG1,DCG2,DCG3,DCG4,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E);phrase(DCG4,S,E).
dcgOr(DCG1,DCG2,DCG3,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E);phrase(DCG3,S,E).
dcgOr(DCG1,DCG2,S,E) :- phrase(DCG1,S,E);phrase(DCG2,S,E).
dcgOnceOr(DCG1,DCG2,S,E) :- phrase(DCG1,S,E)->true;phrase(DCG2,S,E).
dcgNot(DCG2,S,E) :- \+ phrase(DCG2,S,E).
dcgIgnore(DCG2,S,E) :- ignore(phrase(DCG2,S,E)).
dcgOnce(DCG2,S,E) :- once(phrase(DCG2,S,E)).

dcgWhile(True,Frag)-->dcgAnd(dcgOneOrMore(True),Frag).

dcgMust((DCG1,List),S,E) :- is_list(List),!,must((phrase(DCG1,S,SE),phrase(List,SE,E))).
dcgMust(DCG1,S,E) :- must(phrase(DCG1,S,E)).

dcgSeqLen(Len, FB, END) :-
        l_length(CD, Len),
        '$append'(CD, END, FB).


% addtext a sofa is in here
% dcgLenBetween(_,_) --> [_].
dcgLenBetween(Start,Start) --> {!}, dcgSeqLen(Start),{!}.
dcgLenBetween(Start,End, FB, END) :- FB==[],!, ((Start>End -> between(End,Start,0) ; between(Start,End,0))),must(END=[]).
dcgLenBetween(Start,End) --> dcgOnceOr(dcgSeqLen(Start),({(Start>End -> Next is Start-1 ; Next is Start+1)},dcgLenBetween(Next,End))).
dcgLenBetween(Len, Start, End, FB, END) :-
       (l_length(CD, Start),
        '$append'(CD, END, FB)) -> ignore(End=Start) ;
        (
            (Start>End -> Next is Start-1 ; Next is Start+1),
            dcgLenBetween(Len, Next, End, FB, END)
        ).




dcgOneOrMore(True) --> True,dcgZeroOrMore(True),{!}.

dcgZeroOrMore(True) --> True,{!},dcgZeroOrMore(True),{!}.
dcgZeroOrMore(_True) -->[].

dcgLeftOf(Mid,[Left|T],S,[MidT|RightT]):-append([Left|T],[MidT|RightT],S),phrase(Mid,MidT),phrase([Left|T],_LeftT).


dcgLeftOfMid([Left|T],Mid,S,[MidT|RightT]):-append([Left|T],[MidT|RightT],S),phrase(Mid,MidT),phrase([Left|T],_LeftT).

dcgLeftMidRight(Left,Mid,Right) --> dcgLeftOfMid(LeftL,Mid),{phrase(Left,LeftL,[])},Right.

dcgMidLeft(Mid,Left,Right) --> dcgLeftOf(Mid,Left),Right.

dcgNone --> [].

dcgOptional(A)--> dcgOnce(dcgOr(A,dcgNone)).

dcgOptionalGreedy(A)--> dcgOnce(dcgOr(A,dcgNone)).

dcgTraceOnFailure(X):-once(X;(dtrace(X))).

:- export(capitalized//1).
capitalized([W|Text]) --> theText([W|Text]),{atom_codes(W,[C|_Odes]),is_upper(C)}.

substAll(B,[],_R,B):-!.
substAll(B,[F|L],R,A):-subst(B,F,R,M),substAll(M,L,R,A).

substEach(B,[],B):-!.
substEach(B,[F-R|L],A):-subst(B,F,R,M),substEach(M,L,A).

dcgAndRest(TheType,_TODO,[S|MORE],[]) :- phrase(TheType,[S],[]),phrase(TheType,[S|MORE],[]).

% =======================================================
% look ahead but ...
% =======================================================

% 1) must be first in list
% 2) doesnt consume
% 3) sees as many items as needed
dcgStartsWith(TheType,SMORE,SMORE) :- phrase(TheType,SMORE,_).

% tests for the above
decl_dcgTest_startsWith("this is text",dcgStartsWith(theText(["this","is"]))).


:- export(dcgStartsWith1//1).
% 1) must be first in list
% 2) doesnt consume
% 3) sees only 1 item
dcgStartsWith1(TheType,[S|MORE],[S|MORE]) :- phrase(TheType,[S],[]).

% tests for the above
decl_dcgTest_startsWith("this is text",dcgStartsWith1(theText(["this"]))).


% 1) must be first in list
% 2) consumes like theRest(...)
% 3) sees as many items as needed
dcgStartsWith0(TheType,SMORE,[]) :- phrase(TheType,SMORE,_).

% tests for the above
decl_dcgTest("this is text",dcgStartsWith0(theText(["this",is]))).

% =======================================================
% DCG Tester
% =======================================================

:- export(do_dcg_util_tests/0).
do_dcg_util_tests:-
   forall(decl_dcgTest(List,Phrase,Call),'@'((do_dcgTest(List,Phrase,Call)),dcg_meta)),
   forall(decl_dcgTest_startsWith(List,Phrase,Call),'@'((do_dcgTest_startsWith(List,Phrase,Call)),dcg_meta)).


do_dcgTest(Input,DCG,Call):- to_word_list(Input,List),OTEST=do_dcgTest(Input,DCG,Call),copy_term(DCG:OTEST,CDCG:TEST),
   once((phrase(DCG,List,Slack),Call,(Slack==[]->dmsg(passed(CDCG,TEST,OTEST));dmsg(warn(Slack,OTEST))))).
do_dcgTest(Input,DCG,Call):- dmsg(warn(failed(DCG, do_dcgTest(Input,DCG,Call)))).


do_dcgTest_startsWith(Input,DCG,Call):- to_word_list(Input,List),OTEST=do_dcgTest(Input,DCG,Call),copy_term(DCG:OTEST,CDCG:TEST),
   once((phrase(DCG,List,Slack),Call,(Slack==[]->wdmsg(warn(CDCG,TEST,OTEST));dmsg(passed(CDCG,TEST,OTEST))))).
do_dcgTest_startsWith(Input,DCG,Call):- wdmsg(warn(failed(DCG, do_dcgTest_startsWith(Input,DCG,Call)))).


decl_dcgTest(List,Phrase,true):-decl_dcgTest(List,Phrase).
decl_dcgTest_startsWith(List,Phrase,true):-decl_dcgTest_startsWith(List,Phrase).



% :-source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),M:export(F/A)).




%dumpList(B):- currentContext(dumpList,Ctx),dumpList(Ctx,B).
dumpList(_,AB):-dmsg(dumpList(AB)),!.

dumpList(_,[]):-!.
%dumpList(Ctx,[A|B]):-!,fmt(Ctx,A),dumpList(Ctx,B),!.
%dumpList(Ctx,B):-fmt(Ctx,dumpList(B)).

% this is a backwards compatablity block for SWI-Prolog 6.6.6
:- if(current_prolog_flag(dialect,swi)).
:- retract(double_quotes_was_in_dcg(WAS)),set_prolog_flag(double_quotes,WAS).
:- endif.


optional(X) --> cwhite, !, optional(X).
optional(X) --> X,!, owhite.
optional(_) --> [].
optional(O,X) --> {debug_var(X,O),append_term(X,O,XO)},!,optional(XO).

mw(X) --> cspace,!, mw(X).
mw(X) --> X,!, owhite.

owhite --> {quietly_pfs(nb_current('$dcgm_whitespace',preserve))},!.
owhite --> cwhite.
owhite --> [].



% cwhite --> comment_expr(S,I,CP),!,{assert(t_l:'$last_comment'('$COMMENT'(S,I,CP)))},!,owhite.
cwhite --> cspace,!,owhite.
cwhite --> {quietly_pfs(nb_current('$dcgm_comments',consume))},file_comment_expr(CMT),!,{assert(t_l:'$last_comment'(CMT))},!,owhite.
cwhite --> {quietly_pfs(nb_current('$dcgm_whitespace',preserve))}, !, {fail}.

cspace --> [C], {nonvar(C),charvar(C),!,C\==10,bx(C =< 32)}.

charvar(C):- integer(C)-> true; (writeln(charvar(C)),dumpST,writeln(charvar(C)),only_debug(break),fail).

one_blank --> [C],!,{C =< 32}.

:- meta_predicate(file_meta_with_comments(2,+,+,-)).
:- meta_predicate(file_meta_with_comments0(2,+,+,-)).
%file_meta_with_comments(Pred, O) --> [], {clause(t_l:'$last_comment'(O),_,Ref),erase(Ref)},!.
/*
file_meta_with_comments(Pred, O, A, B) :-
 attvar(A),
 get_attr(A,pure_input,lazy_input(Stream, PrevPos, Pos, _)),!,
 file_meta_with_comments0(Pred, O, A, B).
*/
file_meta_with_comments(Pred, O, A, B) :-
 file_meta_with_comments0(Pred, O, A, B).

file_meta_with_comments0(Pred, O) --> one_blank,!,file_meta_with_comments(Pred, O).  % WANT?
file_meta_with_comments0(_Pred, C) --> file_comment_expr(C),!.
file_meta_with_comments0(_Pred, EOF) --> file_eof,!,{end_of_file=EOF}.

file_meta_with_comments0(Pred, Out,S,E):- append_term(Pred,Out,PredOut),
  \+ t_l:dcg_meta_reader_options(with_text,true),!,phrase(PredOut,S,E),!.
file_meta_with_comments0(Pred, Out,S,E):- append_term(Pred,O,PredO),
 expr_with_text(Out,PredO,O,S,E),!.

file_comment_expr(C)--> {get_dcg_meta_reader_options(file_comment_reader,Pred), append_term(Pred,C,PredC)},PredC.

read_string_until_no_esc(String,End)--> dcg_notrace(read_string_until(noesc,String,End)).
read_string_until(String,End)--> read_string_until(esc,String,End).

read_string_until(_,[],eoln,S,E):- S==[],!,E=[].
read_string_until(esc,[C|S],End) --> `\\`,!, zalwayz(escaped_char(C)),!,
   read_string_until(esc,S,End),!.
read_string_until(_,[],End) --> End, !.
%read_string_until(Esc,[35, 36|S],HB) --> {kif_ok}, `&%`, !,read_string_until(Esc,S,HB),!.
read_string_until(Esc,[C|S],End) --> [C],!,read_string_until(Esc,S,End),!.
%read_string_until(_,[],_) --> [].

read_string_until_pairs([C|S],End) --> `\\`,!, zalwayz(escaped_char(C)),!, read_string_until_pairs(S,End).
read_string_until_pairs([],HB) --> HB, !.
read_string_until_pairs([C|S],HB) --> [C],read_string_until_pairs(S,HB).

%escaped_char(10) --> eoln.
escaped_char(10) --> `n`,!.
escaped_char(13) --> `r`,!.
escaped_char(9) --> `t`,!.
escaped_char(C) --> [C],!.
escaped_char(C) --> eoln,!,[C].
escaped_char(E) --> [C], {atom_codes(Format,[92,C]),format(codes([E|_]),Format,[])},!.
escaped_char(Code)  --> [C], {escape_to_char([C],Code)},!.

escape_to_char(Txt,Code):- notrace_catch_fail((sformat(S,'_=`\\~s`',[Txt]),read_from_chars(S,_=[Code]))),!.

zalwayz_debug:-!.
zalwayz_debug:- current_prolog_flag(zalwayz,debug).

never_zalwayz(Goal):-
 locally(current_prolog_flag(zalwayz,false),Goal).

zalwayz_zalwayz(Goal):-
 locally(current_prolog_flag(zalwayz,debug),Goal).


zalwayz(G,H,T):- \+ zalwayz_debug, !, phrase(G,H,T).
zalwayz(G,H,T):- phrase(G,H,T),!.
zalwayz(G,H,T):- nb_current('$translation_stream',S),is_stream(S), \+ stream_property(S,tty(true)),!,always_b(G,H,T).
zalwayz(G,H,T):- always_b(G,H,T).

only_debug(G):- \+ zalwayz_debug, !, nop(G),!.
only_debug(G):- !, call(G).

%zalwayz(G):-  !, zalwayz(G).
zalwayz(G):- \+ zalwayz_debug, !, quietly_pfs(catch(G,_,fail)),!.
zalwayz(G):- must(G).
%zalwayz(P,S,L):-  !, zalwayz(P,S,L).

always_b(G,H,T):- only_debug(break),H=[_|_],writeq(phrase_h(G,H,T)),dcg_print_start_of(H),writeq(phrase(G,H,T)),!,trace,ignore(rtrace(phrase(G,H,T))),!,quietly_pfs,dcg_print_start_of(H),writeq(phrase(G,H,T)), only_debug(break),!,fail.
always_b(G,H,T):- writeq(phrase(G,H,T)),dcg_print_start_of(H),writeq(phrase(G,H,T)),!,only_debug(trace),ignore(rtrace(phrase(G,H,T))),!,quietly_pfs,dcg_print_start_of(H),writeq(phrase(G,H,T)), break,!,fail.

dcg_print_start_of(H):- (l_length(L,3000);l_length(L,300);l_length(L,30);l_length(L,10);l_length(L,1);l_length(L,0)),append(L,_,H),!,format('~NTEXT: ~s~n',[L]),!.
bx(CT2):- notrace_catch_fail(CT2,E,(writeq(E:CT2),only_debug(break))),!.
notrace_catch_fail(G,E,C):- catch(G,E,C),!.
notrace_catch_fail(G):- quietly_pfs(catch(G,_,fail)),!.
clean_fromt_ws([],[]).
clean_fromt_ws([D|DCodes],Codes):-
  ((\+ char_type(D,white), \+ char_type(D,end_of_line)) -> [D|DCodes]=Codes ; clean_fromt_ws(DCodes,Codes)).

:- export(txt_to_codes/2).
txt_to_codes(S,Codes):- quietly_pfs(is_stream(S)),!,stream_to_lazy_list(S,Codes),!.
txt_to_codes(AttVar,AttVarO):- quietly_pfs(attvar(AttVar)),!,AttVarO=AttVar.
% txt_to_codes([C|Text],[C|Text]):- integer(C),is_list(Text),!.
% txt_to_codes([C|Text],_):- atom(C),atom_l_length(C,1),!,throw(txt_to_codes([C|Text])).
txt_to_codes(Text,Codes):- notrace_catch_fail((text_to_string_safe(Text,String),!,string_codes(String,Codes))),!.

phrase_from_pending_stream(Grammar, In):-
   remove_pending_buffer_codes(In,CodesPrev),
   phrase_from_pending_stream(CodesPrev, Grammar, In).

phrase_from_pending_stream(CodesPrev,Grammar,In):- CodesPrev=[_,_|_],
   phrase(Grammar,CodesPrev,NewBuffer),!,
   append_buffer_codes(In,NewBuffer).
phrase_from_pending_stream(CodesPrev,Grammar,In):-
  b_setval('$translation_stream',In),
  read_codes_from_pending_input(In,Codes),!,
  ((quietly_pfs(is_eof_codes(Codes))) ->
     phrase_from_eof(Grammar, In);
     (append(CodesPrev,Codes,NewCodes), !,
       (phrase(Grammar, NewCodes, NewBuffer)
        -> append_buffer_codes(In,NewBuffer);
          phrase_from_pending_stream(NewCodes,Grammar,In)))).


dcg_notrace(G,S,E):- tracing -> setup_call_cleanup(notrace,phrase(G,S,E),trace); phrase(G,S,E).
my_lazy_list_location(Loc,S,S):- attvar(S), quietly_pfs(catch(lazy_list_location(Loc,S,S),_,fail)),!.
my_lazy_list_location(file(_,_,-1,-1))-->[].


track_stream(_In,G):- !,G.
track_stream(In,G):- \+ is_stream(In),!,G.
track_stream(In,G):-
   b_setval('$translation_stream',In),
   notrace_catch_fail(stream_position(In,Pos,Pos),_,true),
   character_count(In,Chars),
   stream_property(In,encoding(Was)),
   (setup_call_catcher_cleanup(
        nop(sset_stream(In,encoding(octet))),
        (ignore(notrace_catch_fail(line_count(In,Line),_,(Line = -1))),
         b_setval('$translation_line',Line-Chars),
           ((G),!)),
        Catcher,
        true)->true;Catcher=fail),
     track_stream_cleanup(Catcher,In,Was,Pos).

track_stream_cleanup(Exit,In,Was,_Pos):-
 (Exit==exit ; Exit == (!)),!,
   sset_stream(In,encoding(Was)).
track_stream_cleanup(Catcher,In,Was,Pos):-
   sset_stream(In,encoding(Was)),
   ((nonvar(Pos),supports_seek(In))->stream_position(In,_Was,Pos);true),!,
   (compound(Catcher)-> (arg(1,Catcher,E),throw(E)) ; fail).

sset_stream(S,P):- functor(P,F,A),functor(W,F,A), stream_property(S,W),!,
   (P=@=W->true;notrace(catch(set_stream(S,P),_,true))).


:- meta_predicate locally_setval(*,*,0).

locally_setval(Name,Value,Goal):-
 (nb_current(Name,Was)->true;Was=[]),
  b_setval(Name,Value),
  call(Goal),
  b_setval(Name,Was).



:- thread_local(t_l:'$fake_buffer_codes'/2).

% parse_meta_stream(Pred,  +Stream, -Expr) is det.
%
% Parse Expression from a Stream
%
parse_meta_stream(Pred, S,Expr):-
  catch(
    parse_meta_stream_1(Pred, S,Expr),
    end_of_stream_signal(_Gram,S),
    Expr=end_of_file).

parse_meta_stream_1(Pred, S,Expr):-
  phrase_from_stream_nd(file_meta_with_comments(Pred,Expr),S).


:- meta_predicate(quietly_pfs(0)).
quietly_pfs(G):- !, call(G).
%quietly_pfs(G):- quietly(G).
%phrase_from_stream_nd(Grammar, In) :-  at_end_of_stream(In),trace,!,phrase_from_eof(Grammar, In).

is_tty_alive(In):-
  stream_property(In,tty(true)),
  stream_property(In,mode(read)),
  stream_property(In,end_of_stream(not)).

show_stream_info(In):-
     quietly_pfs((forall(stream_property(In,(BUF)),
    (writeq(show_stream_info(In,(BUF))),nl)))),!.

phrase_from_stream_nd(Grammar,In):-
   quietly_pfs((peek_pending_codes(In,Codes)->Codes=[_,_|_],
   remove_pending_buffer_codes(In,_))),
   (phrase(Grammar,Codes,NewBuffer)-> append_buffer_codes(In,NewBuffer);(append_buffer_codes(In,Codes),fail)).

phrase_from_stream_nd(Grammar, In) :- at_end_of_stream(In),
  peek_pending_codes(In,Pend),is_eof_codes(Pend),!,phrase_from_eof(Grammar, In). %
%phrase_from_stream_nd(Grammar, _) :- clause(t_l:'$last_comment'(I),_,Ref),I=Grammar,erase(Ref).

phrase_from_stream_nd(Grammar, In) :- stream_property(In,tty(true)),!,
 repeat,
 (is_tty_alive(In)-> true ; throw(end_of_stream_signal(Grammar,In))),
 phrase_from_pending_stream(Grammar, In).

phrase_from_stream_nd(Grammar, In) :-  supports_seek(In),
    ignore(notrace_catch_fail(sset_stream(In,buffer_size(819200)))),
    ignore(notrace_catch_fail(sset_stream(In,buffer_size(16384)))),
    ignore(notrace_catch_fail(sset_stream(In,encoding(octet)))),
    ignore(notrace_catch_fail(sset_stream(In,timeout(3.0)))),
    %sset_stream(In,buffer_size(5)), sset_stream(In,encoding(octet)), sset_stream(In,timeout(3.0)),sset_stream(In,type(text)),%sset_stream(In,buffer(false)),
   repeat, (at_end_of_stream(In)->(!,fail);true),

    character_count(In, FailToPosition),
    ((phrase_from_stream_lazy_part(Grammar, In) *-> true ; (seek(In,FailToPosition,bof,_),!,fail))),!.

phrase_from_stream_nd(Grammar, In) :- \+ supports_seek(In),!,
    if_debugging(sreader,show_stream_info(In)),
    read_stream_to_codes(In,Codes),
    b_setval('$translation_stream',In),
    append_buffer_codes(In,Codes),!,
    phrase_from_buffer_codes(Grammar,In).

phrase_from_stream_nd(Grammar, In) :- stream_property(In,file_name(_Name)),!,
    if_debugging(sreader,show_stream_info(In)),
    read_stream_to_codes(In,Codes),
    b_setval('$translation_stream',In),
    append_buffer_codes(In,Codes),!,
    phrase_from_buffer_codes(Grammar,In).


phrase_from_stream_nd(Grammar, In) :- \+ supports_seek(In),!, phrase_from_pending_stream(Grammar, In).
%phrase_from_stream_nd(Grammar, In) :- b_setval('$translation_stream',In), quietly(phrase_from_stream_nd(Grammar, In)).
phrase_from_stream_nd(Grammar, In) :-  supports_seek(In),
    %sset_stream(In,buffer_size(819200)),sset_stream(In,buffer_size(16384)), sset_stream(In,encoding(octet)), sset_stream(In,timeout(3.0)),
    %sset_stream(In,buffer_size(5)), sset_stream(In,encoding(octet)), sset_stream(In,timeout(3.0)),sset_stream(In,type(text)),%sset_stream(In,buffer(false)),
    character_count(In, FailToPosition),
    ((phrase_from_stream_lazy_part(Grammar, In) -> true ; (seek(In,FailToPosition,bof,_),!,fail))),!.


phrase_from_stream_lazy_part(Grammar, In):-
    check_pending_buffer_codes(In),
    seek(In, 0, current, Prev),
    stream_to_lazy_list(In, List),
    nb_setval('$translation_line',Prev),!,
    phrase(Grammar, List, More) ->
    zalwayz((
       length(List,Used),!,
       length(More,UnUsed),!,
       if_debugging(sreader,wdmsg((Offset is Used - UnUsed + Prev))),
       bx(zalwayz(Offset is Used - UnUsed + Prev)),
       % dbginfo((Offset is Used - UnUsed + Prev)) ->
       seek(In,Offset,bof,_NewPos))).
%phrase_from_stream_lazy_part(Grammar, In):- phrase_from_file_part_c(Grammar, In).


peek_pending_codes(In,Codes):- peek_pending_codes0(In,Codes0),!,Codes=Codes0.
peek_pending_codes0(In,Codes):- (t_l:'$fake_buffer_codes'(In,DCodes);Codes=[]),!,clean_fromt_ws(DCodes,Codes).

check_pending_buffer_codes(In):- peek_pending_codes(In,Codes),
  (Codes==[]->true;(throw(remove_pending_buffer_codes(In,Codes)))),!.

clear_pending_buffer_codes:- forall(retract(t_l:'$fake_buffer_codes'(_In,_DCodes)),true).
remove_pending_buffer_codes(In,Codes):- retract(t_l:'$fake_buffer_codes'(In,DCodes)),!,clean_fromt_ws(DCodes,Codes).
remove_pending_buffer_codes(_In,[]). % for first read

append_buffer_codes(In,Codes):- retract(t_l:'$fake_buffer_codes'(In,CodesPrev)),!,append(CodesPrev,Codes,NewCodes),assertz(t_l:'$fake_buffer_codes'(In,NewCodes)),!.
append_buffer_codes(In,Codes):- assertz(t_l:'$fake_buffer_codes'(In,Codes)),!.

wait_on_input(In):- stream_property(In,end_of_stream(Not)),Not\==not,!.
wait_on_input(In):- repeat,wait_for_input([In],List,1.0),List==[In],!.

read_codes_from_pending_input(In,Codes):- \+ is_stream(In),!,remove_pending_buffer_codes(In,Codes).
read_codes_from_pending_input(In,Codes):- stream_property(In,end_of_stream(Not)),Not\==not,!,(Not==at->Codes=end_of_file;Codes=[-1]).
read_codes_from_pending_input(In,Codes):-  stream_property(In, buffer(none)),!,
   repeat,
    once((wait_on_input(In),
    read_pending_codes(In,Codes,[]))),
    (Codes==[] -> (sleep(0.01),fail); true),!.
read_codes_from_pending_input(In,[Code|Codes]):-  get_code(In,Code),read_pending_codes(In,Codes,[]),!.
throw_reader_error(Error):- wdmsg(throw(reader_error(Error))),dumpST,wdmsg(throw(reader_error(Error))),throw(reader_error(Error)).

supports_seek(In):- notrace_catch_fail(stream_property(In,reposition(true))).
% supports_seek(In):- quietly_pfs((notrace_catch_fail((notrace_catch_fail((seek(In, 1, current, _),seek(In, -1, current, _)),error(permission_error(reposition, stream, _), _Ctx),fail)),error(_,_),true))).

phrase_from_eof(Grammar, _):- var(Grammar),!,unify_next_or_eof(Grammar),!.
%phrase_from_eof(Grammar, _):- compound(Grammar),!,arg(1,Grammar,TV),unify_next_or_eof(TV),!.
phrase_from_eof(Grammar, _):- term_variables(Grammar,[TV|_]),unify_next_or_eof(TV),!.
phrase_from_eof(Grammar, In):- throw(end_of_stream_signal(Grammar,In)).

unify_next_or_eof(O) :- clause(t_l:'$last_comment'(I),_,Ref),!,I=O,erase(Ref).
unify_next_or_eof(end_of_file).


%% parse_meta_ascii(Pred,  +Codes, -Expr) is det.
%
% Parse Expression Codes.
%
parse_meta_ascii(Pred, S, Expr) :- is_stream(S),!,parse_meta_stream(Pred, S,Expr).
%parse_meta_ascii(Pred, S, Expr) :- open_string(S,SIS),!,parse_meta_stream(Pred, SIS,Expr).
parse_meta_ascii(Pred, Text, Expr):-
  quietly_pfs(txt_to_codes(Text,Codes)),
  =(ascii_,In),
  append_buffer_codes(In,[10]),
  append_buffer_codes(In,Codes),!,
  phrase_from_buffer_codes_nd(file_meta_with_comments(Pred,Expr), In).

phrase_from_buffer_codes_nd(Grammar, In) :- peek_pending_codes(In,Pend),is_eof_codes(Pend),!,phrase_from_eof(Grammar,In).
phrase_from_buffer_codes_nd(Grammar, In) :-
  repeat,
  (phrase_from_buffer_codes(Grammar, In) *->
    ((peek_pending_codes(In,Pend),is_eof_codes(Pend))->!;true);(!,fail)).

%phrase_from_buffer_codes(_Grammar, _In) :- peek_pending_codes(In,Pend),is_eof_codes(Pend),!,fail.
phrase_from_buffer_codes(Grammar, In):-
   quietly_pfs((remove_pending_buffer_codes(In,NewCodes), NewCodes \== [])),!,
   (must_or_rtrace(phrase(Grammar, NewCodes, More))->append_buffer_codes(In,More);(append_buffer_codes(In,NewCodes),!,fail)).


skipping_buffer_codes(Goal):-
 setup_call_cleanup(
   quietly_pfs((remove_pending_buffer_codes(In,OldCodes), clear_pending_buffer_codes)),
     Goal,
     quietly_pfs((clear_pending_buffer_codes,append_buffer_codes(In,OldCodes)))).

is_eof_codes(Codes):- var(Codes),!,fail.
is_eof_codes(Codes):- Codes == [],!.
is_eof_codes(Codes):- Codes = [Code],!,is_eof_codes(Code).
is_eof_codes(end_of_file).
is_eof_codes(-1).

file_eof(I,O):- notrace(file_eof0(I,O)).
file_eof0(I,O):- I==end_of_file,!,O=[].
file_eof0 --> [X],{ X = -1},!.
file_eof0 --> [X],{ X = 0},!.
file_eof0 --> [X],{ X = end_of_file},!.

expr_with_text(Out,DCG,O,S,E):-
   zalwayz(lazy_list_character_count(StartPos,S,M)),%integer(StartPos),
   call(DCG,M,ME),
   lazy_list_character_count(EndPos,ME,E),!,
   expr_with_text2(Out,DCG,O,StartPos,M,ME,EndPos,S,E).

expr_with_text2(Out,_ ,O,StartPos,M,ME,EndPos,_,_):-
   integer(StartPos),integer(EndPos),!,
   bx(Len is EndPos - StartPos),l_length(Grabber,Len),!,
   get_some_with_comments(O,Grabber,Out,M,ME),!.
expr_with_text2(Out,_ ,O,end_of_file-StartPos,M,ME,end_of_file-EndPos,_,_):-
   integer(StartPos),integer(EndPos),!,
   bx(Len is StartPos - EndPos),l_length(Grabber,Len),!,
   get_some_with_comments(O,Grabber,Out,M,ME),!.

expr_with_text2(Out,DCG,O,StartPos,M,ME,EndPos,S,E):-
   writeq(expr_with_text2(Out,DCG,O,StartPos,EndPos,S,E)),nl,
   get_some_with_comments(O,_Grabber,Out,M,ME),!.


%expr_with_text(Out,DCG,O,S,E):-
%   call(DCG,S,E) -> append(S,Some,E) -> get_some_with_comments(O,Some,Out,S,E),!.
get_some_with_comments(O,_,O,_,_):- compound(O),compound_name_arity(O,'$COMMENT',_),!.
get_some_with_comments(O,Txt,with_text(O,Str),S,_E):-append(Txt,_,S),!,text_to_string(Txt,Str).


dcg_peek_meta(Grammar,List,List):- (var(Grammar)->((N=2;N=1;between(3,20,N)),l_length(Grammar,N)); true),phrase(Grammar,List,_),!.




eoln --> [C],!, {nonvar(C),charvar(C),eoln(C)},!.
eoln(10).
eoln(13).
eoln --> \+ dcg_peek_meta([_]).

:- meta_predicate(parse_meta_term(2,+,-)).

parse_meta_term(Pred, S, Expr) :- is_stream(S),!, parse_meta_stream(Pred, S,Expr).
parse_meta_term(Pred, string(String), Expr) :- !,parse_meta_ascii(Pred, String, Expr).
parse_meta_term(Pred, atom(String), Expr) :- !,parse_meta_ascii(Pred, String, Expr).
parse_meta_term(Pred, text(String), Expr) :- !,parse_meta_ascii(Pred, String, Expr).
parse_meta_term(Pred, (String), Expr) :- string(String),!,parse_meta_ascii(Pred, String, Expr).
parse_meta_term(Pred, [E|List], Expr) :- !, parse_meta_ascii(Pred, [E|List], Expr).
parse_meta_term(Pred, Other, Expr) :-
 quietly((l_open_input(Other,In)->Other\=@=In)),!,
   repeat, (at_end_of_stream(In)->(!,fail);true),
    parse_meta_term(Pred, In, Expr).


quoted_string(Text) --> (double_quoted_string(Text); single_quoted_string(Text)),!.

double_quoted_string(Text)           --> `"`, !, zalwayz(s_string_cont(`"`,Text)),!.
single_quoted_string(Text)           --> `'`, !, zalwayz(s_string_cont(`'`,Text)),!.
single_quoted_string(Text)           --> ````, !, zalwayz(s_string_cont((````;`'`),Text)),!.

s_string_cont(End,"")        --> End,!.
s_string_cont(End,Txt)       --> read_string_until(S,End), {text_to_string_safe(S,Txt)}.

dcg_used_chars(DCG1, O, S, E):- phrase(DCG1,S, E),!,O=S.

:- fixup_exports.

