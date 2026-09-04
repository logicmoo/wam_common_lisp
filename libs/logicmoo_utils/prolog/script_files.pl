/* Part of LogicMOO Base bb_env
% Provides a prolog database *env*
% ===================================================================
% File 'script_files.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'script_files.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2016/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File:  $PACKDIR/subclause_expansion/prolog/script_files.pl
:- module(script_files, [
          this_script_begin/0,
          this_script_ends/0,
          process_script_file/0,
          process_script_file/1,
          process_script_file/2,
          process_stream/1,
          visit_script_term/1]).

/** <module> Utility LOGICMOO_PREDICATE_STREAMS
This module allows running prolog files as scripts. 
@author Douglas R. Miles
@license LGPL

 Prolog source-code will echo while running

*/

:- reexport(echo_source_files).

:- set_module(class(library)).

:- use_module(library(occurs)).
:- use_module(library(gensym)).
:- use_module(library(when)).

:- use_module(library(occurs)).
:- use_module(library(gensym)).
:- use_module(library(when)).


:- use_module(library(backcomp)).
:- use_module(library(codesio)).
:- use_module(library(charsio)).
:- use_module(library(debug)).
:- use_module(library(check)).


:- use_module(library(edinburgh)).
:- use_module(library(debug)).
:- use_module(library(prolog_stack)).
:- use_module(library(make)).


% :- use_module(library(gui_tracer)).
:- use_module(library(system)).
:- use_module(library(socket)).
:- use_module(library(readutil)).
:- abolish(system:time/1).
:- use_module(library(statistics)).
:- use_module(library(ssl)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(prolog_source)).
:- use_module(library(date)).
%:- use_module(library(editline)).
:- use_module(library(listing)).

% :- meta_predicate(process_script_file()).
% :- meta_predicate(process_stream(+)).
:- meta_predicate(visit_script_term(*)).
:- meta_predicate(visit_if(0)).
:- meta_predicate(in_space_cmt(0)).
:- meta_predicate(now_doing(1,?)).
:- meta_predicate each_doing(1,?).
:- meta_predicate doing(1,*).


% % % OFF :- system:use_module('../file_scope').
:- module_transparent(process_script_file/0).
:- module_transparent(this_script_begin/0).
:- module_transparent(process_stream/1).
:- module_transparent(process_this_stream/1).
:- module_transparent(process_script_file/1).
:- module_transparent(visit_script_term/1).
:- module_transparent(in_space_cmt/1).

:- thread_local(t_l:each_file_term/1).
:- thread_local(t_l:quit_processing_stream/1).
:- thread_local(t_l:block_comment_mode/1).
:- thread_local(t_l:echo_mode/1).

is_echo_mode(Mode):- t_l:echo_mode(Cur),!,Mode=Cur.
is_echo_mode(skip(_)).

till_eof(In) :-
        repeat,
            (   at_end_of_stream(In)
            ->  !
            ;   (read_pending_codes(In, Chars, []),
                (is_echo_mode(echo_file) ->
                  echo_format('~s',[Chars]);
                  true),
                fail)
            ).


%! this_script_begin is det.
%
% Process This Script.
%
% Echoing the file
%
% Same as
%  :- process_this_script_with(compile_normally, echo_file).
%
this_script_begin:- 
   assert_until_eof(t_l:echo_mode(echo_file)),
   process_script_file.

%! this_script_ends is det.
%
% Resume normal Prolog file handling
%
this_script_ends:- prolog_load_context(stream,S) -> 
   asserta(t_l:quit_processing_stream(S));
   assertion(prolog_load_context(stream,_)).


%! process_this_script_with(:Pred1) is det.
%
% Process This Script with :Pred1
%
% Not echoing the file
%
% Same as
%  :- process_this_script_with(Pred1, skip(_)).
%
%  Example Pred1s compile_dynamic/1, compile_normally/1,
%  Or one that you define
%
%  Your Pred will be invoked as:
%
%    call(Pred1, :- Clause),
%    call(Pred1, ?- Clause),
%     or call(Pred1,Clause).
% 
%
%  Example:  To compile all file predicates dynamic
%
%  :- process_this_script_with(compile_dynamic).
%  
%
process_this_script_with(Pred1):- 
   (atom(Pred1)->assertion(current_predicate(Pred1/1));true),
   assert_until_eof(t_l:each_file_term(Pred1)),
   process_script_file.   

%% process_this_script_with(:Pred1,+Echo) is det.
%
% Process This Script with :Pred1 echo Echo
%
%  Echo may be file, skip(_) or skip(items)
%
process_this_script_with(Pred1, Echo):- 
   assert_until_eof(t_l:echo_mode(Echo)),
   process_this_script_with(Pred1),
   process_script_file.


%! process_script_file is det.
%
% Process This Script with :Pred1 echo Echo
%
% Same as
%  `:- process_this_script_with(compile_normally, skip(_)).`
%
process_script_file:- 
  prolog_load_context(stream,S) -> process_this_stream(S) ; assertion(prolog_load_context(stream,_)).


process_this_stream(S):- 
  repeat,
  once(process_stream(S)),
  done_processing_stream(S),
  retractall(t_l:quit_processing_stream(S)).

done_processing_stream(S):- t_l:quit_processing_stream(S),!.
done_processing_stream(S):- at_end_of_stream(S).

% in_space_cmt(Goal):- call_cleanup(prepend_each_line(' % ',Goal),echo_format('~N',[])).
in_space_cmt(Goal):- setup_call_cleanup(echo_format('~N /*~n',[]),Goal,echo_format('~N*/~n',[])).


till_eol(S):- read_line_to_string(S,String),
  (is_echo_mode(skip(_))->true ; (echo_format('~N~s~n',[String]))).

  


%% process_stream( ?S) is det.
%
% Process file stream input
%
process_stream(S):- at_end_of_stream(S),!,visit_script_term_pre_expanded(end_of_file).
process_stream(S):- peek_code(S,W),char_type(W,end_of_line),!,get_code(S,W),echo_format('~s',[[W]]).
process_stream(S):- (peek_string(S,2,W);peek_string(S,1,W);peek_string(S,3,W)),process_stream_peeked213(S,W),!.
process_stream(S):- peek_code(S,W),char_type(W,white),\+ char_type(W,end_of_line),get_code(S,W),echo_format('~s',[[W]]),!.

process_stream(S):- must((read_term(S,T,[variable_names(Vs)]),put_variable_names( Vs))),
  call(b_setval,'$variable_names',Vs), b_setval('$term',T), 
  (is_echo_mode(skip(items)) -> true ; write_stream_item(user_error,T)),!,
  flush_output(user_error),
  must(visit_script_term(T)),!,
  echo_format('~N',[]),!.

process_stream_peeked213(S,W):- t_l:block_comment_mode(Was)->
  ((W=="*/")->((retract(t_l:block_comment_mode(Was))));true),!,
  till_eol(S).
process_stream_peeked213(S," /*"):- asserta(t_l:block_comment_mode(invisible)),!,!,till_eol(S).
process_stream_peeked213(S," %"):- !, read_line_to_string(S,_).
process_stream_peeked213(S,"/*"):- !, asserta(t_l:block_comment_mode(visible)),!,till_eol(S).
process_stream_peeked213(S,"#!"):- !, till_eol(S).
process_stream_peeked213(S,"%"):- !,till_eol(S).



echo_format(_Fmt,_Args):- flush_output, t_l:block_comment_mode(Was),Was==invisible,!.
echo_format(Fmt,Args):- t_l:block_comment_mode(_),is_echo_mode(echo_file),!,format(Fmt,Args),flush_output.
echo_format(Fmt,Args):- is_echo_mode(echo_file),!,format(Fmt,Args),flush_output.
echo_format(_Fmt,_Args).


write_stream_item(Out,T):- 
  flush_output,
  format(Out,'~N~n',[]),
  must(with_output_to(Out,portray_clause_w_vars(T))),
  format(Out,'~N~n',[]),!,flush_output(Out).


process_script_file(File):- process_script_file(File,visit_script_term).
process_script_file(File,Process):- open(File,read,Stream),
 locally(tl:each_file_term(Process), process_this_stream(Stream)),!.

expand_script_directive(include(G),Pos,process_script_file(G),Pos).
expand_script_directive(In,Pos,Out,PosOut):- expand_goal(In,Pos,Out,PosOut).

:- create_prolog_flag(if_level,0,[]).

if_level(L):- current_prolog_flag(if_level,IF),!,L=IF.

set_if_level(0):- !, set_prolog_flag(if_level,0).
set_if_level(1):- !, set_prolog_flag(if_level,1).
set_if_level(N):- must(current_prolog_flag(if_level,Level)),NewLevel is Level + N, set_prolog_flag(if_level,NewLevel).

:- thread_local(t_l:on_elseif/1).
:- thread_local(t_l:on_endif/1).
visit_if(_):- current_prolog_flag(ignoring_input,true),!,set_if_level(+ 1).
visit_if(G):- call(G),!,set_if_level(+1), 
    asserta(t_l:on_elseif(set_prolog_flag(ignoring_input,true))),
    asserta(t_l:on_endif(set_prolog_flag(ignoring_input,false))).
visit_if(_):- set_if_level(+1), set_prolog_flag(ignoring_input,true),
    asserta(t_l:on_elseif(set_prolog_flag(ignoring_input,false))),
    asserta(t_l:on_endif(set_prolog_flag(ignoring_input,false))).

do_directive(else):- if_level(0)-> (sanity(retract(t_l:on_elseif(G))),call(G)) ; true.
do_directive(endif):- set_if_level(-1), if_level(0)-> (sanity(retract(t_l:on_endif(G))),call(G)) ; true.


%% visit_script_term(+Pos, +Term, +Vs) is det.
%
% Process A Script Term.
%
visit_script_term(:- if(G)):- !, (visit_if(G)->true;(trace,visit_if(G))).
visit_script_term(:- else):- !, must(do_directive(else)).
visit_script_term(:- endif):- !, must(do_directive(endif)).
visit_script_term( end_of_file ):- !,prolog_load_context(stream,S),till_eof(S),!,
  visit_script_term_pre_expanded(end_of_file).
visit_script_term( _Term ):- current_prolog_flag(ignoring_input,true).
visit_script_term( Term ):- visit_script_term_pre_expanded( Term ).

skip_file_term_expand:- current_prolog_flag(ignoring_input,true),!.
skip_file_term_expand:- current_prolog_flag(skip_file_term_expand,true).

get_term_pos(Pos):- prolog_load_context(term_position,Pos),!.
get_term_pos(_).

visit_script_term_pre_expanded( Term ) :- skip_file_term_expand,
   Term \== end_of_file,!,
   visit_script_term_post_expanded( Term ).

visit_script_term_pre_expanded( T ) :-
     get_term_pos(Pos), !,
     expand_term(T,Pos,Term,_),
     visit_script_term_post_expanded( Term ).


visit_script_term_post_expanded(T):- get_file_compiler(Pred1),!,
   (call(doing(Pred1),T)*-> true ; print_message(warning,failed(call(Pred1,T)))).

get_file_compiler(Pred1):- t_l:each_file_term(Pred1),!.
get_file_compiler(compile_normally).

directive_doing(Pred1,_,M,(?- G)):-
   get_term_pos(Pos), M:expand_goal(G,Pos,GG,_),!, 
   M:in_space_cmt(forall(M:call(Pred1,?- GG),M:portray_one_line(G))).

directive_doing(Pred1,_,M,(:- G)):-
  get_term_pos(Pos), !, M:expand_script_directive(G,Pos,GG,_),!,
  M:(in_space_cmt(call(Pred1,( :- GG)))*-> true ; print_message(warning,failed(GG))).

doing(Pred1,MG):- nonvar(MG),strip_module(MG,M,G), M:directive_doing(Pred1,MG,M,G),!.
doing(Pred1,T):-
  term_to_clause(T,G),
   each_doing(Pred1,G).

each_doing(Pred1,G):- is_list(G),!,maplist(now_doing(Pred1),G).
each_doing(Pred1,G):-now_doing(Pred1,G).

now_doing(Pred1,MG):- nonvar(MG),strip_module(MG,M,G), M:directive_doing(Pred1,MG,M,G),!.
now_doing(Pred1,G):- call(Pred1,G).


get_pred_head_term(G,M:H):- \+ compound(G),!,strip_module(G,M,H).
get_pred_head_term(( :- _) , _):- !,fail.
get_pred_head_term(G:-_,M:H):-!,strip_module(G,M,H).
get_pred_head_term(G,M:H):-!,strip_module(G,M,H).


term_to_clause(T,Clause):- 
   get_term_pos(Pos),
   (skip_file_term_expand -> expand_term(T,Pos,Term,_) ; T = Term),
   term_to_clause2(Term,Clause).

term_to_clause2(Term,Clause):-
   '$set_source_module'(SM, SM),
   strip_module(SM:Term, M, _Plain),
    (   M == SM
    ->  Clause = Term
    ;   Clause = M:Term
    ).


compile_normally(T):- 
   term_to_clause(T,Clause),
   compile_like_normal(Clause).

compile_like_normal(Clause):- 
    source_location(File,Line),
    % '$store_clause'('$source_location'(File, Line):Clause, File).
    system_store_clause('$source_location'(File, Line):Clause, File).


system_store_clause(A, C) :-
        '$clause_source'(A, B, D),
        '$compile_term'(B, _, C, D).




compile_dynamic(MG):- strip_module(MG,M,G), compile_dynamic(M,G).
compile_dynamic(M, ?- G):-!, M:compile_like_normal(?- M:G).
compile_dynamic(M, :- G):-!, M:compile_like_normal(:- M:G).
compile_dynamic(M, G):- 
 (M:get_pred_head_term(G,H)->maybe_dynamic(M,H);true),!,
  M:compile_like_normal(M:G).

maybe_dynamic(M,H):- predicate_property(M:H,static),!.
maybe_dynamic(M,H):- predicate_property(M:H,dynamic),!.
maybe_dynamic(M,H):- functor(H,F,A),M:dynamic(M:F/A).



% :- fixup_exports.

