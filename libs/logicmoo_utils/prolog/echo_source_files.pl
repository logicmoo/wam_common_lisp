/* Part of LogicMOO Base bb_env
% Provides a prolog database *env*
% ===================================================================
% File 'echo_files.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'echo_files.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2016/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File:  $PACKDIR/subclause_expansion/prolog/echo_files.pl
:- module(echo_files,
          [get_file_from_stream/2,assume_caughtup_to/3]).

:- define_into_module(
 [
          check_current_echo/0,
          echo_source_file/0,
          echo_source_file_no_catchup/1,
          echo_source_file/1]).

/** <module> Utility LOGICMOO_PREDICATE_STREAMS
This module allows running prolog files as echos.
@author Douglas R. Miles
@license LGPL

 Prolog source-code will echo while running

*/

:- set_module(class(library)).

:- meta_predicate(into_echo_cmt(:)).


:- module_transparent(echo_source_file/0).
:- module_transparent(check_current_echo/1).
:- module_transparent(into_echo_cmt/1).

:- thread_local(t_l:echoing_file/1).
:- thread_local(t_l:echoing_file_in_cmt/1).
:- thread_local(t_l:file_stream_loc/3).

%! echo_source_file(+File) is det.
%
echo_source_file(F):-
 (\+ t_l:echoing_file(F) -> asserta(t_l:echoing_file(F)) ; true),
 check_current_echo(F).

echo_source_file:- prolog_load_context(file,File), echo_source_file(File).

echo_source_file_no_catchup(F):-
 ignore((
   \+ t_l:echoing_file(F),
   asserta(t_l:echoing_file(F)),!,
   stream_property(S,file_name(F)),
   %get_file_from_stream(S,F),
   character_count(S,Pos),
   echo_files:assume_caughtup_to(F,S,Pos))),!.

check_current_echo:-
   source_location(F,_), prolog_load_context(source,S), S\==F,!,
   check_current_echo(S),check_current_echo(F).
check_current_echo:-
   ignore((prolog_load_context(source,S),check_current_echo(S))),
   ignore((source_location(F,_),S\==F,check_current_echo(F))),
   ignore((prolog_load_context(file,SL),SL\==S,SL\==F,check_current_echo(SL))),!.

check_current_echo(F):- t_l:echoing_file(F),get_file_from_stream(S,F), character_count(S,Pos),catch_up_to_stream(S,Pos),!.
check_current_echo(F):- t_l:echoing_file_in_cmt(F),!,get_file_from_stream(S,F), character_count(S,Pos),into_echo_cmt((catch_up_to_stream(S,Pos))).
check_current_echo(F):- asserta(t_l:echoing_file_in_cmt(F)),!,check_current_echo(F).


into_echo_cmt(Goal):- setup_call_cleanup(format('~N/*~~~n',[]),Goal,format('~N~~*/~n',[])).


:- thread_local(t_l:feedback_started/2).

:- create_prolog_flag(capture_feedback,false,[keep(true)]).

c_es(X):- stream_property(X,alias(current_error)),!.
c_es(X):- stream_property(X,alias(user_error)),!.
c_es(X):- stream_property(X,alias(main_error)),!.
c_es(X):- stream_property(X,file_no(2)),!.

feedback_open(F):- t_l:feedback_started(F,_),!, format('~N/*~~~n'),
  ignore(( \+ current_prolog_flag(capture_feedback,false), fail, feedback_close(F),!,feedback_open(F))).
feedback_open(F):- current_prolog_flag(capture_feedback,true),
  current_input(I),current_output(O),c_es(E),
  new_memory_file(MF),open_memory_file(MF,write,S,[free_on_close(false)]),
  asserta(t_l:feedback_started(F,mf_s(MF,S,I,O,E))),!,tell(S),set_prolog_IO(I,S,S),
  % for ansi color
  set_stream(S,tty(true)).
feedback_open(F):-  format('~N/*~~~n'),assert(t_l:feedback_started(F,current_output)).

feedback_close(F):- retract(t_l:feedback_started(F,current_output)),!,format('~N~~*/~n').
feedback_close(F):- retract(t_l:feedback_started(F,mf_s(MF,S,I,O,E))),!,set_prolog_IO(I,O,E),
  close(S),memory_file_to_string(MF,String),free_memory_file(MF),
  atom_length(String,L), (L>0 -> into_echo_cmt(write(String));true).
feedback_close(_):- told.

mco_info(F,S,_I,Start,End):-
   get_file_range(F,Start,End,STerm),
   read_mco(STerm,Term,Cmnts,QQ,Vs,Sv),
   character_count(S,Pos), get_file_range(F,End,Pos,After), peek_string(S,6,Peek),
   fmsg('~N%~~ ~q ~~%~n',[[string(STerm),term(Term),comments(Cmnts),quasi_quotations(QQ),
    variable_names(Vs),singletons(Sv),after(After),peek(Peek)]]).
fmsg(Fmt,Args):- flush_output,ttyflush,format(user_output,Fmt,Args),ttyflush.

never_echo_term(_:P):-!,compound(P),never_echo_term(P).
never_echo_term(end_tests(_)).
never_echo_term(begin_tests(_)).

:- module_transparent(echo_catchup/4).

echo_catchup(I,P,O,PO):- \+ echo_catchup_f(I,P,O,PO), fail.
echo_catchup_f(I,P,O,PO):-
 notrace((compound(P),
   source_location(F,_L),t_l:echoing_file(F),
   b_getval('$term', Term),I==Term)),
   nonvar(I), \+ never_echo_term(I),
   prolog_load_context(stream,S),stream_property(S,file_name(F)),
   P=..[_,Start,End|_],!,
   ttyflush,
   mco(F,S,I,Start,End,O),!,
   PO=P.

:- style_check(-singleton).
mco(F,S,I,Start,End,O):- I == end_of_file, !, feedback_close(F),fail.
mco(F,S,I,Start,End,O):- t_l:file_stream_loc(F,S,Pos), PosBefore1 is Pos+1, End =< PosBefore1,!, mco_i2(F,S,I,O).
mco(F,S,I,Start,End,O):- feedback_close(F),fail.
mco(F,S,I,Start,End,O):- catch_up_to_stream(S,Start), fail.
mco(F,S,I,Start,End,O):- mco_p(F,S,I,Start,End) -> fail; (print_tree(I), fail).
mco(F,S,I,Start,End,O):- assume_caughtup_to(F,S,End),fail.
mco(F,S,I,Start,End,O):- character_count(S,Pos), catch_up_to_stream(S,Pos), fail.
mco(F,S,I,Start,End,O):- consume_white_space(F,S),fail.
mco(F,S,I,Start,End,O):- character_count(S,Pos), assume_caughtup_to(F,S,Pos), fail.  % for the peek/getch
%mco(F,S,I,Start,End,O):- mco_info(F,S,I,Start,End),fail.
mco(F,S,I,Start,End,O):- mco_i(F,S,I,O),!,feedback_open(F),!.
mco(F,S,I,Start,End,O):- feedback_open(F),fail.

consume_white_space(_,S):- at_end_of_stream(S),!,fail.
consume_white_space(F,S):- character_count(S,Start),get_file_from(F,Start,SubStr),
  open_string(SubStr,S2),consume_white_space_proxy(S2),character_count(S2,Consumed),
  NewPos is Start + Consumed,
  assume_caughtup_to(F,S,NewPos),!.

consume_white_space_proxy(S):- consume_white(S),!,consume_white_space_proxy(S).

consume_white(S):- at_end_of_stream(S),!,fail.
consume_white(S):- peek_char(S,C),char_type(C,space),get_char(S,C),put_char(C).
consume_white(S):- nsl(NSL),atom_length(NSL,L),peek_string(S,L,Str),Str==NSL,!,read_line_to_string(S,_).
consume_white(S):- peek_string(S,2,"%~"),!,read_line_to_string(S,_).
consume_white(S):- peek_string(S,1,"%"),!,read_line_to_string(S,Str),write(Str),nl.
consume_white(S):- peek_string(S,2,"#!"),!,read_line_to_string(S,Str),write(Str),nl.


mco_p(F,_S,_I,Start,End):- %garbage_collect_atoms,
   get_file_range(F,Start,End,STerm),
   read_mco(STerm,Term,Cmnts,QQ,_Vs,_Sv),
  % (Cmnts\==[];QQ\==[]),!,
  % mco_info(F,S,I,Start,End),
   write(STerm),!,
   assume_caughtup_to(F,S,End).

mco_p(F,S,I,Start,End):- print_tree(I),!,assume_caughtup_to(F,S,End).

% mco_i(F,S,I,O):- format('~N/*~~'),
mco_i2(F,S,I,O):- fail.
mco_i(F,S,I,O):- fail.
mco_i(F,S,_-->_,O):- fail.

:- style_check(+singleton).

read_mco(STerm,Term,Cmnts,QQ,Vs,Sv):-
   read_term_from_atom(STerm,CTerm,[cycles(true),comments(Cmnts),quasi_quotations(QQ),variable_names(Vs),singletons(Sv)]),
   read_term_from_atom(STerm,UTerm,[cycles(false),comments(UCmnts),quasi_quotations(UQQ),variable_names(UVs),singletons(USv)]),!,
   (CTerm =@= UTerm -> Term = CTerm ; (Term = UTerm, UCmnts = Cmnts, QQ=UQQ, Vs=UVs, USv=Sv)),!.

get_file_from_stream(S,F):- stream_property(S,file_name(F)).

catch_up_to_stream(S,Pos):- \+ t_l:file_stream_loc(_,S,_),get_file_from_stream(S,F), print_file_range(F,S,0,Pos),!.
catch_up_to_stream(S,Pos):- t_l:file_stream_loc(F,S,PosBefore), Pos>PosBefore, print_file_range(F,S,PosBefore,Pos).
catch_up_to_stream(S):- character_count(S,Pos),catch_up_to_stream(S,Pos).

get_file_range(F,Start,End,SubStr):-
  Len is End-Start,
  read_file_to_string(F,Str,[]),
  sub_string(Str,Start,Len,_,SubStr).
get_file_from(F,Start,SubStr):-
  read_file_to_string(F,Str,[]),
  sub_string(Str,Start,_,0,SubStr).


print_file_range(F,S,Start,End):-
  get_file_range(F,Start,End,SubStr),
  assume_caughtup_to(F,S,End),
  write_ommit_feedback(on,SubStr),!.

nsl('No source location!?').

write_ommit_feedback(S,String):- nsl(NSL), atom_contains(String,NSL),replace_in_string([NSL='/*~NSL~*/'],String,Rest),!,
 write_ommit_feedback(S,Rest).
write_ommit_feedback(on,String):- (sub_string(String,Before,_,After,'\n/*~');sub_string(String,Before,_,After,'/*~')),!,
  sub_atom(String,0,Before,_A,On),write_ommit_feedback(on,On),
  sub_atom(String,After,_,0,Rest),write_ommit_feedback(off,Rest).
write_ommit_feedback(on,String):- (sub_string(String,Before,_,After,'\n%~');sub_string(String,Before,_,After,'%~')),!,
  sub_atom(String,0,Before,_A,On),write_ommit_feedback(on,On),
  sub_atom(String,After,_,0,Rest),write_ommit_feedback(lineoff,Rest).
write_ommit_feedback(on,String):- !, write(String).
write_ommit_feedback(off,String):- (sub_string(String,_,_,After,'~*/\n');sub_string(String,_,_,After,'~*/')),!,
  sub_atom(String,After,_,0,Rest),write_ommit_feedback(on,Rest).
write_ommit_feedback(lineoff,String):- sub_string(String,_,_,After,'\n'), !,
  sub_atom(String,After,_,0,Rest),write_ommit_feedback(on,Rest).
write_ommit_feedback(_,_).


assume_caughtup_current(F,S):- retractall(t_l:file_stream_loc(F,S,_)),character_count(S,Pos),assert(t_l:file_stream_loc(F,S,Pos)).

assume_caughtup_to(F,S,Pos):- retractall(t_l:file_stream_loc(F,S,_)),assert(t_l:file_stream_loc(F,S,Pos)).

se:echo_expander(system:term_expansion(I,P,O,PO), echo_catchup(I,P,O,PO)).

%user:term_expansion(I,P,O,PO):-  echo_catchup(I,P,O,PO).
system:term_expansion(_,_,_,_):-
  notrace((
    se:echo_expander(H,B),
    nth_clause(H,1,Ref),
    \+ (clause(H,_:B,Ref)))),
    ignore(retract(H:- B)),
           asserta(H:-B),
    fail.
%system:term_expansion(I,P,O,PO):-  echo_catchup(I,P,O,PO).


