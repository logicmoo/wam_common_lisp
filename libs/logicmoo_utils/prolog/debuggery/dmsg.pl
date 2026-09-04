/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_dmsg.pl
%:- if((prolog_load_context(source,F),prolog_load_context(file,F))).
%:- module(dmsg,[]).
%:- else.
%module(_,Y):- maplist(export,Y).
%:- endif.

:- define_into_module(
  [ ansi_control_conv/2,
    % source_variables_lwv/1,
    %style_on_off/4,
    ansi_prop/2,
    ansicall/2,
    ansicall/3,
    ansicall_4/3,
    wdmsg_goal/2,
    ansicall_6/3,
    ansifmt/2,
    ansifmt/3,
    cls/0,
    colormsg/2,
    contains_atom/2,
    contrasting_color/2,
    debugm/1,debugm/2,
    defined_message_color/2,
    dfmt/1,dfmt/2,
    dis_pp/1,
    dmsg/1,dmsg/2,dmsg/3,
    dmsg0/1,
    dmsg00/1,
    dmsg000/1,
    dmsg1/1,
    dmsg2/1,
    dmsg3/1,
    dmsg4/1,
    dmsg5/1, % dmsg5/2,
    dmsg_hide/1,
    dmsg_hides_message/1,
    dmsg_log/3,
    dmsg_pretty/1,
    dmsg_show/1,
    dmsg_showall/1,
    dmsg_text_to_string_safe/2,
    dmsginfo/1,
    f_word/2,
    fg_color/2,
    flush_output_safe/0,
    flush_output_safe/1,
    fmt/1,fmt/2,fmt/3,
    fmt0/1,fmt0/2,fmt0/3,
    fmt9/1,
    fmt90/1,
    fmt_ansi/1,
    fmt_or_pp/1,
    fmt_portray_clause/1,
    format_to_message/3,
    functor_color/2,
    get_indent_level/1,
    good_next_color/1,
    if_color_debug/0,
    if_color_debug/1,
    if_color_debug/2,
    in_cmt/1,
    indent_e/1,
    indent_to_spaces/2,
    is_sgr_on_code/1,
    is_tty/1,
    keep_line_pos_w_w/2,
    last_used_fg_color/1,
    logger_property/3,
    loggerFmtReal/3,
    loggerReFmt/2,
    logLevel/2,
    matches_term/2,
    matches_term0/2,
    mesg_arg1/2,
    mesg_color/2,
    msg_to_string/2,
    next_color/1,
    once_in_while/1,
    portray_clause_w_vars/1,
    portray_clause_w_vars/2,
    portray_clause_w_vars/3,
    portray_clause_w_vars/4,
    predef_functor_color/2,
    prepend_each_line/2,
    print_prepended/2,
    print_prepended_lines/2,
    random_color/1,
    setLogLevel/2,
    sformat/4,
    sgr_code_on_off/3,
    sgr_off_code/2,
    sgr_on_code/2,
    sgr_on_code0/2,
    term_color0/2,
    to_stderror/1,
    tst_color/0,
    tst_color/1,
    tst_fmt/0,
    univ_safe_3/2,
    unliked_ctrl/1,
    vdmsg/2,
    wdmsg/1,
    wdmsg/2,
    wdmsgl/1,
    wdmsgl/2,
    wdmsgl/3,
    with_all_dmsg/1,
    with_current_indent/1,
    with_dmsg/2,
    with_no_dmsg/1,
    with_no_dmsg/2,
    with_output_to_console/1,
    with_output_to_each/2,
    with_output_to_main/1,
    with_output_to_stream/2,
    with_show_dmsg/2,
    writeFailureLog/2,
    withFormatter/4]).

:- multifile
        term_color0/2.
:- meta_predicate
        ansicall(?, 0),
        ansicall(?, ?, 0),
        ansicall_4(?, ?, 0),
        ansicall_6(?, ?, 0),
        fmt_ansi(0),
        if_color_debug(0),
        if_color_debug(0, 0),
        in_cmt(0),
        keep_line_pos_w_w(?, 0),
        prepend_each_line(?, 0),
        to_stderror(0),
        with_all_dmsg(0),
        with_current_indent(0),
        with_dmsg(?, 0),
        with_no_dmsg(:),
        with_no_dmsg(?, :),
        with_output_to_console(0),
        with_output_to_main(0),
        with_output_to_stream(?, 0),
        with_show_dmsg(?, 0).

%:- expects_dialect(swi).

:- use_module(library(lists)).
/*lists:selectchk(Elem, List, Rest) :-
    select(Elem, List, Rest0),
    (!),
    Rest=Rest0.*/
%:- lists:export(selectchk/3).
:- use_module(library(option)).

:- autoload(library(apply),[maplist/2]).
:- autoload(library(occurs),[sub_term/2]).
:- autoload(library(memfile),[memory_file_to_atom/2]).
:- autoload(library(debug),[debug/3]).
:- autoload(library(error),[must_be/2]).
:- autoload(library(lists),[member/2,nth1/3]).
:- autoload(library(lists),[append/3]).
%:- autoload(library(lists),[selectchk/3]).

:- autoload(library(listing),[portray_clause/3,listing/1]).

:- thread_local(bfly_tl:bfly_setting/2).

use_html_styles:-!,fail.
use_html_styles:- notrace(use_html_styles0).
use_html_styles0 :- on_x_fail(t_l:print_mode(html)).
use_html_styles0 :- dis_pp(ansi),!,fail.
%use_html_styles0 :- on_x_fail(httpd_wrapper:http_current_request(_)),!.
%use_html_styles0 :- on_x_fail(pengines:pengine_self(_)),!.
use_html_styles0 :- current_predicate(is_butterfly_console/0), (inside_bfly_html_esc;is_butterfly_console),!.
%=

%% style_on_off(Out, ?Ctrl, ?OnCode, ?OffCode) is det.
%
% Sgr Code Whenever Off.
%

dis_pp(ansi):- keep_going,!.
dis_pp(PP):- current_predicate(in_pp/1), in_pp(PP).

using_style(Out,Ctrl,Goal,How):-
  notrace(style_emitter(Out,Emitter)),!,
  using_style_emitter(Emitter, Out,Ctrl,Goal,How),!.

using_style_emitter(sgr,_Out,Ctrl,Goal,How):- fail,
  How = (with_output_to_each(string(S),
     (set_stream_ignore(current_output, tty(true)),call(Goal))),
          terminal_ansi_format([Ctrl],'~s',[S])), !.

using_style_emitter(Emitter,Out,Ctrl,Goal,How):-
  cnvt_in_out(Emitter,Out,Ctrl,OnCode,OffCode),!,
  How = scce_orig((OnCode,!),once(Goal),(OffCode,!)).

style_emitter(Out,NV):- nonvar(NV),style_emitter(Out,Var),!,NV==Var.
style_emitter(Out,none):- dis_pp(ansi), \+ is_tty(Out), !.
style_emitter(Out,sgr):- dis_pp(ansi), is_tty(Out), !.
style_emitter(_Out,html):- use_html_styles,!.
style_emitter(Out,sgr):- dis_pp(bfly), is_tty(Out), !.
style_emitter(_Out,none):- \+ use_html_styles.

%cnvt_in_out(_, _Out,_Ctrl,true,true):- dis_pp(ansi),!.
cnvt_in_out(_, _Out,_Ctrl,true,true):- on_x_fail(httpd_wrapper:http_current_request(_)),!.
cnvt_in_out(none, _Out,_Ctrl,true,true).
cnvt_in_out(_,_Out,html,(in_pp(Was),pp_set(http)),pp_set(Was)).
cnvt_in_out(_,_Out,pp_set(PP),(in_pp(Was),pp_set(PP)),pp_set(Was)).
%cnvt_in_out(sgr, Out,Ctrl,enter_recording_stream(Out,Ctrl,S,H),exit_recording_stream(Out,Ctrl,S,H)).
cnvt_in_out(sgr, Out, Ctrl,OnCodeCall,OffCodeCall) :- sgr_code_on_off(Ctrl,OnCode,OffCode), into_oncode_call(Out,OnCode,OnCodeCall), into_oncode_call(Out,OffCode,OffCodeCall).
cnvt_in_out(sgr, Out,_Ctrl,true,OffCode):- into_oncode_call(Out,[default],OffCode),!.
%cnvt_in_out(html,_Out,C,(bfly_in,I),(O,bfly_out)):- toplevel_pp(bfly), \+ inside_bfly_html_esc, !, html_on_off(C,I,O),!. % @TODO bfly_out/0
%cnvt_in_out(html,_Out,C,(I),(O)):- toplevel_pp(bfly),html_on_off(C,I,O),!. % @TODO bfly_out/0

cnvt_in_out(html,_Out,C,I,O):- html_on_off(C,I,O),!.
cnvt_in_out(_, _Out,_Ctrl,true,true):-!.
cnvt_in_out(Mode, _Out, Ctrl,true,true):- format(user_error,'~N% ~q.~n', [mising_ctrl(Mode, Ctrl)]).

set_output_safe(Strm):- catch(set_output(Strm),_,true).

enter_recording_stream(_Out,_Ctrl,H,S):- new_memory_file(H),open_memory_file(H,write,S),set_output_safe(S).
exit_recording_stream(Out,Ctrl,H,S):- set_output_safe(Out),close(S),memory_file_to_string(H,Str),terminal_ansi_format([Ctrl],'~s',[Str]).

into_oncode_call(Out,OnCode,OnCodeCall):- OnCodeCall= smart_format(Out,'\e[~wm', [OnCode]).


wldmsg_0(_CM,ops):- !.
wldmsg_0(_CM,ops):-
 dzotrace((
 wldmsg_2('======================\\'),
 (prolog_load_context(stream,X)-> dmsgln(prolog_load_context(stream,X)) ; (current_input(X),dmsgln(current_input(X)))),
 ignore((
 fail,
 %dmsgln(forall(stream_property(X,_))),
 % call(stream_property(X,position(Pos))->
 prolog_load_context(module,LM),
 dmsgln(prolog_load_context(module,LM)),
 dmsgln(forall(LM:current_op(_,_,LM:if))),
 dmsgln(forall(LM:current_op(_,_,LM:then))),
 dmsgln(forall(LM:current_op(_,_,LM:'=>'))),
 dmsgln(forall(LM:current_op(_,_,LM:'==>'))),
 dmsgln(forall(LM:current_op(_,_,LM:'-'))),
 dmsgln(forall(prolog_load_context(_,_))))),
 strip_module(_,M,_),
 dmsgln(strip_module(M)),
 dmsgln(call('$current_source_module'(_SM))),
 dmsgln(call('$current_typein_module'(_TM))),
  (source_location(F,L)-> wldmsg_2(X=source_location(F,L)) ; wldmsg_2(no_source_location(X))),
   %dmsgln(forall(byte_count(X,_))),
   %dmsgln(forall(character_count(X,_))),
   dmsgln(forall(line_count(X,_))),
   dmsgln(forall(line_position(X,_))),
  wldmsg_2('======================/'))),
 !.

wldmsg_0(_,CM:Goal):- !, wldmsg_0(CM,Goal).
wldmsg_0(_,forall(CM:Goal)):- callable(Goal), !,ignore((nonvar(Goal),wldmsg_0(CM,forall(Goal)))).
wldmsg_0(_,call(CM:Goal)):- callable(Goal), !,ignore((nonvar(Goal),wldmsg_0(CM,call(Goal)))).
wldmsg_0(CM,List):- is_list(List),!,maplist(wldmsg_0(CM),List).
wldmsg_0(CM,forall(Goal)):- callable(Goal), !, ignore((nonvar(Goal),forall(CM:call(Goal), wldmsg_1(Goal)))).
wldmsg_0(CM,call(Goal)):- callable(Goal), !, ignore((nonvar(Goal),CM:call(Goal), wldmsg_1(Goal))).
wldmsg_0(_,Info):-wldmsg_1(Info).

wldmsg_1(List):- is_list(List),!,maplist(wldmsg_1,List).
wldmsg_1(Info):- compound(Info),compound_name_arguments(Info,F,[A]),!,wldmsg_1(F=A).
wldmsg_1(Info):- compound(Info),compound_name_arguments(Info,(-),[F,A]),!,wldmsg_1(F=A).
wldmsg_1(Info):-
  get_real_user_output(O),flush_output(O),smart_format(O,'~N',[]),flush_output(O),
  wldmsg_2(Info),!.


get_real_user_output(O):-stream_property(O,file_no(1)).
get_real_user_error(O):-stream_property(O,file_no(2)).

:- export(same_streams/2).
same_streams(X,Y):- dzotrace((into_stream(X,XX),into_stream(Y,YY),!,XX==YY)).

into_stream(X,S):- dzotrace(into_stream_0(X,S)).
into_stream_0(file_no(N),XX):- !, stream_property(X,file_no(N)),!,X=XX.
into_stream_0(Atom,XX):- atom(Atom),stream_property(X,alias(Atom)),!,X =XX.
into_stream_0(S,XX):- atomic(S),is_stream(S),!,S = XX.
into_stream_0(S,XX):- stream_property(X,file_name(F)),F==S,!,X=XX.

:- volatile(t_l:thread_local_error_stream/1).
wldmsg_2(Info):- t_l:thread_local_error_stream(Err), output_to_x(Err,Info).
wldmsg_2(Info):- same_streams(current_output,file_no(1)), stream_property(X,file_no(1)), !, output_to_x(X,Info).
wldmsg_2(Info):- same_streams(current_output,file_no(2)), stream_property(X,file_no(2)), !, output_to_x(X,Info).
wldmsg_2(Info):- output_to_x(current_output,Info), stream_property(X,file_no(2)), !, output_to_x(X,Info).

output_to_x(S,Info):- ignore(dzotrace(catch(output_to_x_0(S,Info),_,true))).
output_to_x_0(S,Info):- into_stream(S,X),!, flush_output(X),
  catch(smart_format(X,'~N% ~p~n',[Info]),_,smart_format(X,'~N% DMSGQ: ~q~n',[Info])),flush_output(X).

:- export(prepend_trim/2).
:- export(is_html_white_l/1).
:- export(is_html_white_r/1).
:- export(likely_folded/1).
:- module_transparent(dmsgln/1).
:- export(dmsgln/1).
:- system:import(dmsgln/1).
:- meta_predicate(dmsgln(:)).
:- dynamic(dmsgln/1).
:- module_transparent(dmsgln/1).
:- export(dmsgln/1).
:- system:import(dmsgln/1).
:- meta_predicate(dmsgln(:)).
dmsgln222(CMSpec):- strip_module(CMSpec,CM,Spec),!, ignore(dzotrace(wldmsg_0(CM,Spec))).
% system:dmsgln(List):-!,dzotrace(wldmsg_0(user,List)).
dmsgln(CMSpec):- strip_module(CMSpec,CM,Spec),!, ignore(dzotrace(wldmsg_0(CM,Spec))).



univ_safe_3(A,B):- compound(A),compound_name_arity(A,F,0),!,F=..B.
univ_safe_3(A,B):- A=..B.

:- meta_predicate if_defined_local(:,0).
if_defined_local(G,Else):- current_predicate(_,G)->G;Else.

:- module_transparent
        ansi_control_conv/2,
        ansifmt/2,
        ansifmt/3,
        colormsg/2,
        contrasting_color/2,
        defined_message_color/2,
        dfmt/1,
        dfmt/2,
        dmsg/1,dmsg/2,dmsg/3,
        dmsg0/1,
        dmsg1/1,
        dmsg2/1,
        dmsg3/1,
        dmsg4/1,
        dmsg5/1,
        %dmsg5/2,
        dmsg_hide/1,
        dmsg_hides_message/1,
        dmsg_show/1,
        dmsg_showall/1,
        dmsg_text_to_string_safe/2,
        dmsginfo/1,

        with_output_to_each/2,
        f_word/2,
        fg_color/2,
        flush_output_safe/0,
        flush_output_safe/1,
        fmt/1,
        fmt/2,
        fmt/3,
        fmt0/1,
        fmt0/2,
        fmt0/3,
        fmt9/1,
        fmt_or_pp/1,
        fmt_portray_clause/1,
        functor_color/2,
        get_indent_level/1,
        good_next_color/1,
        if_color_debug/0,
        indent_e/1,
        indent_to_spaces/2,
        is_sgr_on_code/1,
        is_tty/1,
        last_used_fg_color/1,
        mesg_arg1/2,

        msg_to_string/2,
        next_color/1,
        portray_clause_w_vars/1,
        portray_clause_w_vars/2,
        portray_clause_w_vars/3,
        portray_clause_w_vars/4,
        predef_functor_color/2,
        print_prepended/2,
        print_prepended_lines/2,
        random_color/1,
        sformat/4,
        %style_on_off/4,
        sgr_off_code/2,
        sgr_on_code/2,
        sgr_on_code0/2,
        tst_color/0,
        tst_color/1,
        tst_fmt/0,
        unliked_ctrl/1,
        vdmsg/2,
        withFormatter/4,
        writeFailureLog/2.
:- dynamic
        defined_message_color/2,
        term_color0/2.


:- if(current_predicate(lmcode:combine_logicmoo_utils/0)).
:- module(logicmoo_util_dmsg,
[  % when the predciates are not being moved from file to file the exports will be moved here
       ]).

:- else.

:- endif.

:- set_module(class(library)).

:- user:use_module(library(memfile)).
%:- user:use_module(first).
%:- user:ensure_loaded(logicmoo_util_rtrace).
:- ensure_loaded(library(logicmoo/each_call)).
%:- user:ensure_loaded(logicmoo_util_loop_check).


:- meta_predicate(wets(?,0)).
:- export(wets/2).
wets(S,Goal):- var(S),!,with_error_to_string(S,Goal).
wets(S,Goal):- is_stream(S),!,with_error_to_stream(S,Goal).
wets(S,Goal):- compound(S), with_error_to(S,Goal).


:- meta_predicate with_error_to(+,0).
with_error_to(Dest,Goal):-
  with_error_to_each(Dest,once(Goal)).

:- meta_predicate with_error_to_string(+,0).
with_error_to_stream(S,Goal):-
  with_ioe((
     (set_stream_ignore(S,alias(user_error)),
         set_stream_ignore(S,alias(current_error))),
     locally_tl(thread_local_error_stream(S),Goal))).

:- meta_predicate wete(+,0).
wete(Dst,Goal):- with_error_to_each(Dst,Goal).
:- meta_predicate with_error_to_each(+,0).
with_error_to_each(Dest,Goal):- compound(Dest), \+ compound_name_arity(Dest,_,0),
  Dest=..[F,A],stream_u_type(F),!,
  Unset = (set_stream_ignore(Was,alias(current_error)),set_stream_ignore(Was,alias(user_error))),
  once((member(Alias,[user_error,current_error]),stream_property(Was,alias(Alias)))),
  Done = mfs_end(MFS,A),
  MFS = mfs(_,F,_,set_error_stream,Unset),
  call_cleanup(trusted_redo_call_cleanup(mfs_start(MFS),(Goal,Done),Done),Done).

with_error_to_each(Dest,Goal):- with_error_to_stream(Dest,Goal).

new_mfs(MFS):- MFS = mfs(Handle,_,Stream,_,_),
  new_memory_file(Handle), open_memory_file(Handle,write,Stream,[free_on_close(true)]).

mfs_start(MFS):- \+compound(MFS),!,throw(mfs_start(MFS)).
mfs_start(MFS):-
  arg(2,MFS,F), arg(3,MFS,OS), arg(4,MFS,Set), NMFS= mfs(Handle,F,Stream,Set,_Unset),
  (is_stream(OS)
    -> Stream =OS
    ; (new_mfs(NMFS), nb_setarg(1,MFS,Handle),nb_setarg(3,MFS,Stream))),
 call(Set,Stream).

set_error_stream(Stream):- set_stream_ignore(Stream,alias(current_error)),set_stream_ignore(Stream,alias(user_error)).

mfs_end(MFS,A):-
  MFS = mfs(Handle,F,Stream,_Set,Unset),
  ignore((is_stream(Stream),close(Stream), mem_handle_to_substring(Handle,Str),substring_to_type(Str,F,A))),
  call(Unset).




stream_u_type(atom). stream_u_type(string). stream_u_type(codes). stream_u_type(chars).

mem_handle_to_substring(Handle,String):- memory_file_to_string(Handle,String),!.
mem_handle_to_substring(Handle,SubString):-
  memory_file_line_position(Handle, _Line, _LinePos, Offset),
  %seek(Stream, 0, current, Offset)
  memory_file_substring(Handle, 0, Offset, _After, -SubString).

substring_to_type(Str,atom,Atom):- atom_string(Atom,Str).
substring_to_type(Str,string,Str).
substring_to_type(Str,codes,Codes):- string_codes(Str,Codes).
substring_to_type(Str,chars,Chars):- string_chars(Str,Chars).

mem_handle_to_type(Handle,atom,Atom):- !, memory_file_to_atom(Handle,Atom).
mem_handle_to_type(Handle,string,String):- !, memory_file_to_string(Handle,String).
mem_handle_to_type(Handle,codes,Codes):- !, memory_file_to_codes(Handle,Codes).
mem_handle_to_type(Handle,chars,Chars):- !, memory_file_to_string(Handle,Atom),string_chars(Atom,Chars).

:- meta_predicate with_error_to_string(-,0).
with_error_to_string(S,Goal):-
   new_memory_file(Handle),
   open_memory_file(Handle,write,Stream,[free_on_close(true)]),
   call_cleanup(with_error_to_each(Stream,Goal),
           (close(Stream),memory_file_to_string(Handle,S))).

:- meta_predicate with_output_to_each(+,0).

with_output_to_each(Dest,Goal):- compound(Dest), \+ compound_name_arity(Dest,_,0),
  Dest=..[F,A],stream_u_type(F),!,
  current_output(Was),
  Unset = set_output_safe(Was),
  MFS = mfs(_,F,_,set_output_safe,Unset),
  Done = mfs_end(MFS,A),
  call_cleanup(trusted_redo_call_cleanup(mfs_start(MFS),(Goal,Done),Done),Done).
/*
with_output_to_each(Dest,Goal):- Dest=..[F,A],!,
   current_output(Was),
   nb_setarg(1,Dest,""),
   new_memory_file(Handle),
   open_memory_file(Handle,write,Stream,[free_on_close(true)]),
     scce_orig(set_output_safe(Stream),
      scce_orig(true,Goal,
        (close(Stream),mem_handle_to_type(Handle,F,Atom),nb_setarg(1,Dest,Atom),ignore(A=Atom))),
      (set_output_safe(Was))).
*/
with_output_to_each(Dest,Goal):-
   current_output(Was),
    scce_orig(set_output_safe(Dest),Goal,set_output_safe(Was)).



% ==========================================================
% Sending Notes
% ==========================================================
:- thread_local( tlbugger:dmsg_match/2).
% = :- meta_predicate(with_all_dmsg(0)).
% = :- meta_predicate(with_show_dmsg(*,0)).



%=

%% with_all_dmsg( :Goal) is nondet.
%
% Using All (debug)message.
%
with_all_dmsg(Goal):-
   locally(set_prolog_flag(dmsg_level,always),
       locally( tlbugger:dmsg_match(show,_),Goal)).



%=

%% with_show_dmsg( ?TypeShown, :Goal) is nondet.
%
% Using Show (debug)message.
%
with_show_dmsg(TypeShown,Goal):-
  locally(set_prolog_flag(dmsg_level,always),
     locally( tlbugger:dmsg_match(showing,TypeShown),Goal)).

% = :- meta_predicate(with_no_dmsg(0)).

%=

%% with_no_dmsg( :Goal) is nondet.
%
% Using No (debug)message.
%

 % with_no_dmsg(Goal):- current_prolog_flag(dmsg_level,always),!,Goal.
with_no_dmsg(Goal):-locally(set_prolog_flag(dmsg_level,never),Goal).

%=

%% with_no_dmsg( ?TypeUnShown, :Goal) is nondet.
%
% Using No (debug)message.
%
with_no_dmsg(TypeUnShown,Goal):-
 locally(set_prolog_flag(dmsg_level,filter),
  locally( tlbugger:dmsg_match(hidden,TypeUnShown),Goal)).

% dmsg_hides_message(_):- !,fail.

%=

%% dmsg_hides_message( ?C) is det.
%
% (debug)message Hides Message.
%
dmsg_hides_message(_):- current_prolog_flag(dmsg_level,never),!.
dmsg_hides_message(_):- current_prolog_flag(dmsg_level,always),!,fail.
dmsg_hides_message(C):-  tlbugger:dmsg_match(HideShow,Matcher),matches_term(Matcher,C),!,HideShow=hidden.

:- export(matches_term/2).

%% matches_term( ?Filter, ?VALUE2) is det.
%
% Matches Term.
%
matches_term(Filter,_):- var(Filter),!.
matches_term(Filter,Term):- var(Term),!,Filter=var.
matches_term(Filter,Term):- ( \+ \+ (matches_term0(Filter,Term))),!.

%% contains_atom( ?V, ?A) is det.
%
% Contains Atom.
%
contains_atom(V,A):-sub_term(VV,V),nonvar(VV),cfunctor(VV,A,_).

%% matches_term0( :TermFilter, ?Term) is det.
%
% Matches Term Primary Helper.
%
matches_term0(Filter,Term):- Term = Filter.
matches_term0(Filter,Term):- atomic(Filter),!,contains_atom(Term,Filter).
matches_term0(F/A,Term):- (var(A)->member(A,[0,1,2,3,4]);true), cfunctor(Filter,F,A), matches_term0(Filter,Term).
matches_term0(Filter,Term):- sub_term(STerm,Term),nonvar(STerm),call(call,matches_term0(Filter,STerm)),!.

hide_some_hiddens(P,P):- ((\+ compound(P));compound_name_arity(P,_,0)),!.
hide_some_hiddens(pfc_hide(_),pfc_hide($)):-!.
%hide_some_hiddens('{}'(_),'{}'($)):-!.
hide_some_hiddens(S,M):-
   compound_name_arguments(S,F,Args),
   must_maplist(hide_some_hiddens,Args,ArgsO),
   compound_name_arguments(M,F,ArgsO),!.


pretty_and_hide(In, Info):- dzotrace((pretty_numbervars(In,M),hide_some_hiddens(M,Info))),!.

dmsg_pretty(In):- dzotrace( ignore( \+ \+   ( pretty_and_hide(In, Info),dmsg(Info)))).

wdmsg_pretty(In):- !,notrace(in_cmt(format('~q',In))).
wdmsg_pretty(In):- \+ \+ dzotrace((pretty_and_hide(In, Info),wdmsg(Info))).

wdmsg_pretty(F,In):- !,notrace(in_cmt(format(F,In))).
wdmsg_pretty(F,In):- \+ \+ dzotrace((pretty_and_hide(In, Info),wdmsg(F,Info))).

%=

%% dmsg_hide( ?Term) is det.
%
% (debug)message Hide.
%
dmsg_hide(isValueMissing):-!,set_prolog_flag(dmsg_level,never).
dmsg_hide(Term):-set_prolog_flag(dmsg_level,filter),sanity(nonvar(Term)),aina( tlbugger:dmsg_match(hidden,Term)),retractall( tlbugger:dmsg_match(showing,Term)),nodebug(Term).

%=

%% dmsg_show( ?Term) is det.
%
% (debug)message Show.
%
dmsg_show(isValueMissing):-!,set_prolog_flag(dmsg_level,always).
dmsg_show(Term):-set_prolog_flag(dmsg_level,filter),aina( tlbugger:dmsg_match(showing,Term)),ignore(retractall( tlbugger:dmsg_match(hidden,Term))),debug(Term).

%=

%% dmsg_showall( ?Term) is det.
%
% (debug)message Showall.
%
dmsg_showall(Term):-ignore(retractall( tlbugger:dmsg_match(hidden,Term))).


%=

%% indent_e( ?X) is det.
%
% Indent E.
%
indent_e(0):-!.
indent_e(X):- X > 20, XX is X-20,!,indent_e(XX).
indent_e(X):- catchvvnt((X < 2),_,true),write(' '),!.
indent_e(X):-XX is X -1,!,write(' '), indent_e(XX).


%=

%% dmsg_text_to_string_safe( ?Expr, ?Forms) is det.
%
% (debug)message Text Converted To String Safely Paying Attention To Corner Cases.
%
dmsg_text_to_string_safe(Expr,Forms):-on_x_fail(text_to_string(Expr,Forms)).

% ===================================================================
% Lowlevel printng
% ===================================================================
:- multifile lmconf:term_to_message_string/2.
:- dynamic lmconf:term_to_message_string/2.
%% catchvvnt( :GoalT, ?E, :GoalF) is det.
%
% Catchvvnt.
%
catchvvnt(T,E,F):-catchv(quietly(T),E,F).

:- meta_predicate(catchvvnt(0,?,0)).

%=

%% fmt0( ?X, ?Y, ?Z) is det.
%
% Format Primary Helper.
%
%fmt0(user_error,F,A):-!,get_main_error_stream(Err),!,smart_format(Err,F,A).
%fmt0(current_error,F,A):-!,get_thread_current_error(Err),!,smart_format(Err,F,A).
fmt0(X,Y,Z):-catchvvnt((smart_format(X,Y,Z),flush_output_safe(X)),E,dfmt(E:smart_format(X,Y))).

%=

%% fmt0( ?X, ?Y) is det.
%
% Format Primary Helper.
%

is_regular_format_args(X,_):- \+ atomic(X),!,fail.
is_regular_format_args(X,Y):- (string(X);atom(Y)), atom_contains(X,'~').
is_regular_format_args(_,Y):- is_list(Y),!.

system:smart_format(X,Y,Z):- format(X,Y,Z).
smart_format(X,Y):- smart_format([X,Y]).

smart_format(DDD):- \+ is_list(DDD),!, format('~q',[DDD]).

smart_format([X,Y]):- is_regular_format_args(X,Y),!,catch(format(X,Y),error(smart_format(A),B),writeq(smart_format(X,Y)=error(smart_format(A),B))),!.
smart_format([X|More]):- (compound(X);is_stream(X)),!,with_output_to_each(X,smart_format(More)),!.
smart_format([X,Y]):- smart_format(X-Y),!.

:- export(smart_format/3).
:- export(smart_format/2).
:- export(smart_format/1).

fmt0(X,Y):-catchvvnt((smart_format(X,Y),flush_output_safe),E,dfmt(E:smart_format(X,Y))).

%=

%% fmt0( ?X) is det.
%
% Format Primary Helper.
%
fmt0(X):- (atomic(X);is_list(X)), dmsg_text_to_string_safe(X,S),!,format('~w',[S]),!.
fmt0(X):- (atom(X) -> catchvvnt((smart_format(X,[]),flush_output_safe),E,dmsg(E)) ;
  (lmconf:term_to_message_string(X,M) -> 'smart_format'('~q~N',[M]);fmt_or_pp(X))).

%=

%% fmt( ?X) is det.
%
% Format.
%
fmt(X):-fresh_line,fmt_ansi(fmt0(X)).

%=

%% fmt( ?X, ?Y) is det.
%
% Format.
%
fmt(X,Y):- fresh_line,fmt_ansi(fmt0(X,Y)),!.

%=

%% fmt( ?X, ?Y, ?Z) is det.
%
% Format.
%
fmt(X,Y,Z):- fmt_ansi(fmt0(X,Y,Z)),!.



:- module_transparent((format_to_message)/3).

format_to_message(Format,Args,Info):-
  on_xf_cont(((( sanity(is_list(Args))->
     smart_format(string(Info),Format,Args);
     (smart_format(string(Info),'~N~n~p +++++++++++++++++ ~p~n',[Format,Args])))))).

new_line_if_needed:- tracing,!.
new_line_if_needed:- ttyflush,format('~N',[]),flush_output.

%=

%% fmt9( ?Msg) is det.
%
% Fmt9.
%
fmt9(Msg):- new_line_if_needed, must((fmt90(Msg))),!,new_line_if_needed.

fmt90(fmt0(F,A)):-on_x_fail(fmt0(F,A)),!.

fmt90(Msg):- on_x_fail(print_tree_maybe(Msg)),!.
%fmt90(Msg):- on_x_fail(print(Msg)),!.
fmt90(Msg):- dzotrace(on_x_fail(((string(Msg)),smart_format(Msg,[])))),!.

fmt90(V):- on_x_fail(notrace(mesg_color(V,C))), catch(pprint_ecp(C, V),_,fail),!. % (dumpST,format('~N~q. % ~q. ~n',[fmt90(V),E]),fail)
fmt90(Msg):- on_x_fail((with_output_to(string(S),portray_clause_w_vars(Msg)))),format('~s',[S]),!.
fmt90(Msg):- dzotrace(on_x_fail(format('~p',[Msg]))),!.
fmt90(Msg):- dzotrace(writeq(fmt9(Msg))).

print_tree_maybe(G):- compound(G),compound_name_arity(G,F,_), \+ current_op(_,_,F),!,
  print_tree(G).

% :-reexport(library(ansi_term)).
% % % OFF :- system:use_module(library(ansi_term)).


%=

%% tst_fmt is det.
%
% Tst Format.
%

tst_fmt:- tst_fmt(swish).

tst_fmt(PP):- make,call(tst_fmt0(PP)).

tst_fmt0(PP):-
 findall(R,(clause(ansi_term:sgr_code(R, _),_),ground(R)),List),
 ignore((
        ansi_term:ansi_color(FC, _),
        member(FG,[hfg(FC),fg(FC)]),
        % ansi_term:ansi_term:ansi_color(Key, _),
        member(BG,[hbg(default),bg(default)]),
        member(R,List),
        % random_member(R1,List),
    C=[reset,R,FG,BG],
  fresh_line,
  P = with_pp(PP,ansicall(C,format('~N% HTML: ~q~n',[C]))),
  with_pp(PP,ansicall(C,format('~N% ?- ~q. ~n',[P]))),
  % ansicall(C,format('~N% ansicall: ~q~n',[C])),
  \+ in_pp(http), terminal_ansi_format(C,'~N% ansi_term: ~q~n',[C]),
  fail)).


%=

%% fmt_ansi( :Goal) is nondet.
%
% Format Ansi.
%
fmt_ansi(Goal):- (ansicall([reset,bold,hfg(white),bg(black)],ignore(Goal))->true;call(Goal)).


%=

%% fmt_portray_clause( ?X) is det.
%
% Format Portray Clause.
%
fmt_portray_clause(X):- renumbervars_prev(X,Y),!, portray_clause(Y).


%=

%% fmt_or_pp( ?X) is det.
%
% Format Or Pretty Print.
%
fmt_or_pp(portray((X:-Y))):-!,fmt_portray_clause((X:-Y)),!.
fmt_or_pp(portray(X)):- !,cfunctor(X,F,A),fmt_portray_clause((pp(F,A):-X)),!.
fmt_or_pp(X):-format('~q~N',[X]).


%=

%% with_output_to_console( :GoalX) is det.
%
% Using Output Converted To Console.
%
with_output_to_console(X):- get_main_error_stream(Err),!,with_output_to_stream(Err,X).

%=

%% with_output_to_main( :GoalX) is det.
%
% Using Output Converted To Main.
%
with_output_to_main(X):- get_main_error_stream(Err),!,with_output_to_stream(Err,X).


%=

%% dfmt( ?X) is det.
%
% Dfmt.
%
dfmt(X):- get_thread_current_error(Err),!,with_output_to_stream(Err,fmt(X)).

%=

%% dfmt( ?X, ?Y) is det.
%
% Dfmt.
%
dfmt(X,Y):- get_thread_current_error(Err), with_output_to_stream(Err,fmt(X,Y)).


%=

%% with_output_to_stream( ?Stream, :Goal) is det.
%
% Using Output Converted To Stream.
%
with_output_to_stream(Stream,Goal):- is_stream(Stream),!,
   current_output(Saved),
   scce_orig(set_output_safe(Stream),
         Goal,
         set_output_safe(Saved)).
with_output_to_stream(Prop,Goal):- compound(Prop), on_x_fail(stream_property(Stream,Prop)),!,
  with_output_to_stream(Stream,Goal).
with_output_to_stream(Out,Goal):- with_output_to_each(Out,Goal).


%=

%% to_stderror( :Goal) is nondet.
%
% Converted To Stderror.
%
to_stderror(Goal):- get_thread_current_error(Err), with_output_to_stream(Err,Goal).



:- dynamic dmsg_log/3.


:- dynamic(logLevel/2).
:- module_transparent(logLevel/2).
:- multifile(logLevel/2).


:- dynamic logger_property/2.

%=

%% logger_property( ?VALUE1, ?VALUE2, ?VALUE3) is det.
%
% Logger Property.
%
logger_property(todo,once,true).



%=

%% setLogLevel( ?M, ?L) is det.
%
% Set Log Level.
%
setLogLevel(M,L):-retractall(logLevel(M,_)),(nonvar(L)->asserta(logLevel(M,L));true).


%=

%% logLevel( ?S, ?Z) is det.
%
% Log Level.
%
logLevel(debug,ERR):-get_thread_current_error(ERR).
logLevel(error,ERR):-get_thread_current_error(ERR).
logLevel(private,none).
logLevel(S,Z):-current_stream(_X,write,Z),dtrace,stream_property(Z,alias(S)).


%=

%% loggerReFmt( ?L, ?LRR) is det.
%
% Logger Re Format.
%
loggerReFmt(L,LRR):-logLevel(L,LR),L \==LR,!,loggerReFmt(LR,LRR),!.
loggerReFmt(L,L).


%=

%% loggerFmtReal( ?S, ?F, ?A) is det.
%
% Logger Format Real.
%
loggerFmtReal(none,_F,_A):-!.
loggerFmtReal(S,F,A):-
  current_stream(_,write,S),
    fmt(S,F,A),
    flush_output_safe(S),!.



:- thread_local tlbugger:is_with_dmsg/1.


%=

%% with_dmsg( ?Functor, :Goal) is det.
%
% Using (debug)message.
%
with_dmsg(Functor,Goal):-
   locally(tlbugger:is_with_dmsg(Functor),Goal).


% % % OFF :- system:use_module(library(listing)).

%=

%% sformat( ?Str, ?Msg, ?Vs, ?Opts) is det.
%
% Sformat.
%
sformat(Str,Msg,Vs,Opts):- nonvar(Msg),cfunctor(Msg,':-',_),!,with_output_to_each(string(Str),
   (current_output(CO),portray_clause_w_vars(CO,Msg,Vs,Opts))).
sformat(Str,Msg,Vs,Opts):- with_output_to_each(chars(Codes),(current_output(CO),portray_clause_w_vars(CO,':-'(Msg),Vs,Opts))),append([_,_,_],PrintCodes,Codes),'sformat'(Str,'   ~s',[PrintCodes]),!.


free_of_attrs_dmsg(Term):- var(Term),!,(get_attrs(Term,Attrs)-> Attrs==[] ; true).
free_of_attrs_dmsg(Term):- term_attvars(Term,Vs),!,(Vs==[]->true;maplist(free_of_attrs_dmsg,Vs)).


% % % OFF :- system:use_module(library(listing)).

%=

%% portray_clause_w_vars( ?Out, ?Msg, ?Vs, ?Options) is det.
%
% Portray Clause W Variables.
%

portray_clause_w_vars(Out,Msg,Vs,Options):- free_of_attrs_dmsg(Msg+Vs),!, portray_clause_w_vars5(Out,Msg,Vs,Options).
portray_clause_w_vars(Out,Msg,Vs,Options):- fail, if_defined_local(serialize_attvars_now(Msg+Vs,SMsg+SVs),fail),!,
     \+ \+ portray_clause_w_vars2(Out,SMsg,SVs,Options).
portray_clause_w_vars(Out,Msg,Vs,Options):- \+ \+ portray_clause_w_vars2(Out,Msg,Vs,Options).

portray_clause_w_vars2(Out,Msg,Vs,Options):- free_of_attrs_dmsg(Msg+Vs),!, portray_clause_w_vars5(Out,Msg,Vs,Options).
portray_clause_w_vars2(Out,Msg,Vs,Options):-
   term_attvars(Msg,AttVars),
   copy_term(Msg+AttVars,Msg+AttVars,Goals),
   portray_append_goals(Msg,Goals,GMsg),
   portray_clause_w_vars5(Out,GMsg,Vs,Options).

portray_clause_w_vars5(Out,Msg,Vs,Options):-
  copy_term_nat(v(Msg,Vs,Options),v(CMsg,CVs,COptions)),
  portray_clause_w_vars55(Out,CMsg,CVs,COptions),!.
portray_clause_w_vars55(Out,Msg,Vs,Options):-
 \+ \+ ((
 (var(Vs)-> prolog_load_context(variable_names,Vs);true),
 prolog_listing:do_portray_clause(Out,Msg,
  [variable_names(Vs),numbervars(true),
      attributes(ignore),
      character_escapes(true),fullstop(false),quoted(true)|Options]))),!.

is_var_name_goal(C):-compound(C),C=name_variable(_,_).

portray_append_goals(Var,Goals,Var):- Goals==[],!.
portray_append_goals(Var,Goals,Var):- Goals==true,!.
portray_append_goals(Var,Goals,VarO):- exclude(is_var_name_goal,Goals,NewGoals)->Goals\==NewGoals,!,
   portray_append_goals(Var,NewGoals,VarO).
portray_append_goals(Var,Goals,(maplist(call,Goals),Var)):-var(Var),!.
portray_append_goals(H:-B,Goals,H:-CGMsg):-!,portray_append_goals(B,Goals,CGMsg).
portray_append_goals(H:B,Goals,H:CGMsg):-!,portray_append_goals(B,Goals,CGMsg).
portray_append_goals(Var,Goals,(maplist(call,Goals),Var)).



%dzotrace(G):- notrace(G),!.
dzotrace(G):- notrace(woi(no_bfly(G))),!.

woi(G):- scce_orig(G,true,true).

%=

%% portray_clause_w_vars( ?Msg, ?Vs, ?Options) is det.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Msg,Vs,Options):- portray_clause_w_vars(current_output,Msg,Vs,Options).

%=

%% portray_clause_w_vars( ?Msg, ?Options) is det.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Msg,Options):- source_variables_lwv(Msg,Vs),portray_clause_w_vars(current_output,Msg,Vs,Options).

:- export(portray_clause_w_vars/1).

%=

%% portray_clause_w_vars( ?Msg) is det.
%
% Portray Clause W Variables.
%
portray_clause_w_vars(Msg):- portray_clause_w_vars(Msg,[]),!.


%=

%% print_prepended( ?Pre, ?S) is det.
%
% Print Prepended.
%
print_prepended(Pre,S):-prepend_trim(S,S0),atomics_to_string(L,'\n',S0),print_prepended_lines(Pre,L),!.

is_html_white_l('\r').
is_html_white_l('<br/>').
is_html_white_l('<br>').
is_html_white_l('<p>').
is_html_white_l('<p/>').
is_html_white_l('\n').

is_html_white_r(' ').
is_html_white_r('&nbsp;').
is_html_white_r('\t').
is_html_white_r(X):- is_html_white_l(X).

prepend_trim(S,O):- is_html_white_l(W),atom_concat(W,L,S),!,prepend_trim(L,O).
prepend_trim(S,O):- is_html_white_r(W),atom_concat(L,W,S),!,prepend_trim(L,O).
prepend_trim(O,O).



like_clause([S|Lines]):- (atom_contains(S,':-');atom_contains(S,'?-');(append(_,[E],Lines),atom_contains(E,'.'))),!.

%% print_prepended_lines( ?Pre, :TermARG2) is det.
%
% Print Prepended Lines.
%

print_prepended_lines(_,[]):- !.
print_prepended_lines(X,[Line]):- (X==guess; X==(block); X==line), !, cmt_override(LineC,_S,_E),print_prepended_line(LineC, Line).
print_prepended_lines(X,Lines):- cmt_override(LineC,_S,_E), (X==line ; X == LineC), like_clause(Lines), print_prepended_lines0(LineC,Lines).
print_prepended_lines(guess,Lines):- !,
  (like_clause(Lines) -> print_prepended_lines(block,Lines);print_prepended_lines(line,Lines)).
print_prepended_lines(block,[S|Lines]):- !, append(Mid,[E],Lines), % atom_contains(E,'.'),
  cmt_override(_,S,End),
  format('~N~w ',[S]),write(S),
  print_prepended_lines0('   ',Mid),
  format('~N ~w ~w~n',[E,End]).
print_prepended_lines(Pre,A):- !, print_prepended_lines0(Pre,A).

print_prepended_lines0(_Pre,[]).
print_prepended_lines0(_Pre,['']):-!.
print_prepended_lines0(Pre,[H|T]):- print_prepended_line(Pre,H),
  print_prepended_lines0(Pre,T),!.

print_prepended_line(line,S):- cmt_override(Line,_S,_E),!, print_prepended_line(Line,S).
print_prepended_line(Pre,S):- prepend_trim(S,H),
  ignore((H\=="",
  line_pos(current_output,LPos1),new_line_if_needed,line_pos(current_output,LPos2),
  (LPos1\==LPos2->format('~w~w',[Pre,H]); format('~w~w',[Pre,H])))).


%=

%% in_cmt( :Goal) is nondet.
%
% In Comment.
%

% in_cmt(Goal):- tlbugger:no_slow_io,!,format('~N/*~n',[]),call_cleanup(Goal,format('~N*/~n',[])).
% in_cmt(Goal):- use_html_styles,!, Goal.

in_cmt(Goal):- in_cmt(guess,Goal).

with_cmt_override(LineC,S,E,Goal):-
   cmt_override(WLineC,WS,WE),
   scce_orig(nb_setval(cmt_override,lse(LineC,S,E)),Goal,nb_setval(cmt_override,lse(WLineC,WS,WE))).
cmt_override(LineC,S,E):- nb_current(cmt_override,lse(LineC,S,E)),!.
cmt_override('%~ ','/*','*/').

in_cmt(line,Goal):- !, maybe_bfly_html(prepend_each_line(line,Goal)),!.
in_cmt(block,Goal):- cmt_override(_,S,E), !, maybe_bfly_html(scce_orig((write(' '),write(S),write(' ')), call(Goal),(write(' '),write(E),write(' ')))).
in_cmt(guess,Goal):-
   get_indent_level(Indent),
   indent_to_spaces(Indent,Space),
   wots(S,prepend_each_line(Space,Goal)),
   trim2(S,SS),!,
   %atomic_list_concat(SL,'\n',S),
   ( atom_contains(SS,'\n')->in_cmt(block,write(S));in_cmt(line,write(S))).


in_cmt(Block,Goal):- maybe_bfly_html(print_prepended_line(Block,Goal)),!.


%=

%% with_current_indent( :Goal) is nondet.
%
% Using Current Indent.
%
%with_current_indent(Goal):- use_html_styles,!, Goal.
with_current_indent(Goal):-
   get_indent_level(Indent),
   indent_to_spaces(Indent,Space),
   prepend_each_line(Space,Goal).


%=

%% indent_to_spaces( :PRED3N, ?Out) is det.
%
% Indent Converted To Spaces.
%
indent_to_spaces(1,' '):-!.
indent_to_spaces(0,''):-!.
indent_to_spaces(2,'  '):-!.
indent_to_spaces(3,'   '):-!.
indent_to_spaces(N,Out):- 1 is N rem 2,!, N1 is N-1, indent_to_spaces(N1,Spaces),atom_concat(' ',Spaces,Out).
indent_to_spaces(N,Out):- N2 is N div 2, indent_to_spaces(N2,Spaces),atom_concat(Spaces,Spaces,Out).


%=

%% mesg_color( :TermT, ?C) is det.
%
% Mesg Color.
%
mesg_color(_,[reset]):-tlbugger:no_slow_io,!.
mesg_color(T,C):-var(T),!,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color(T,[fg(C)]):- atomic(T), into_color_name(T,C),!.
mesg_color(T,C):- if_defined(is_sgr_on_code(T)),!,C=T.
mesg_color(T,C):-cyclic_term(T),!,C=[reset,blink(slow),bold].
mesg_color("",C):- !,C=[blink(slow),fg(red),hbg(black)],!.
mesg_color([_,_,_,T|_],C):-atom(T),mesg_color(T,C).
mesg_color(T,C):- string(T),!,(f_word(T,F)),!,functor_color(F,C).
mesg_color(List,C):-is_list(List),member(T,List),atom(T),mesg_color(T,C),!.
mesg_color([T|_],C):-nonvar(T),!,mesg_color(T,C),!.
mesg_color(T,C):-(atomic(T);is_list(T)), dmsg_text_to_string_safe(T,S),!,mesg_color(S,C).
mesg_color(T,C):-not(compound(T)),term_to_atom(T,A),!,mesg_color(A,C).
mesg_color(succeed(T),C):-nonvar(T),mesg_color(T,C).
% mesg_color((T),C):- \+ \+ ((predicate_property(T,meta_predicate(_)))),arg(_,T,E),compound(E),!,mesg_color(E,C).
mesg_color(=(T,_),C):-nonvar(T),mesg_color(T,C).
mesg_color(debug(T),C):-nonvar(T),mesg_color(T,C).
mesg_color(_:T,C):-nonvar(T),!,mesg_color(T,C).
mesg_color(:- T,C):-nonvar(T),!,mesg_color(T,C).
mesg_color((H :- T), [bold|C]):-nonvar(T),!,mesg_color(H,C).
mesg_color(T,C):-cfunctor(T,F,_),member(F,[color,ansi]),compound(T),arg(1,T,C),nonvar(C).
mesg_color(T,C):-cfunctor(T,F,_),member(F,[succeed,must,mpred_op_prolog]),compound(T),arg(1,T,E),nonvar(E),!,mesg_color(E,C).
mesg_color(T,C):-cfunctor(T,F,_),member(F,[fmt0,msg,smart_format,fmt]),compound(T),arg(2,T,E),nonvar(E),!,mesg_color(E,C).
mesg_color(T,C):-predef_functor_color(F,C),mesg_arg1(T,F).
mesg_color(T,C):-nonvar(T),defined_message_color(F,C),matches_term(F,T),!.
mesg_color(T,C):-cfunctor(T,F,_),!,functor_color(F,C),!.



%=

%% prepend_each_line( ?Pre, :Goal) is nondet.
%
% Prepend Each Line.
%

maybe_print_prepended(Out,Pre,S):- atomics_to_string(L,'\n',S), maybe_print_pre_pended_L(Out,Pre,L).
maybe_print_prepended(Out,_,[L]):- write(Out,L),!,flush_output(Out).
maybe_print_prepended(Out,Pre,[H|L]):- write(Out,H),nl(Out),!,write(Out,Pre),maybe_print_pre_pended_L(Out,Pre,L).

prepend_each_line(Pre,Goal):- fail,
  current_predicate(predicate_streams:new_predicate_output_stream/2),!,
  current_output(Out),
  call(call,predicate_streams:new_predicate_output_stream([Data]>>maybe_print_prepended(Out,Pre,Data),Stream)),
  set_stream_ignore(Stream,tty(true)),
  %set_stream_ignore(Stream,buffer(false)),
  %undo(ignore(catch(close(Stream),_,true))),!,
  scce_orig(true,
   (with_output_to_each(Stream,once(Goal)),flush_output(Stream)),
    ignore(catch(close(Stream),_,true))),!.

prepend_each_line(Pre,Goal):-
  with_output_to_each(string(Str),Goal)*->once((print_prepended(Pre,Str),new_line_if_needed)).

prepend_each_line1(Pre,Goal):-
  wots(string(Str),Goal)*->once((print_prepended(Pre,Str),new_line_if_needed)).

into_cmt(SSS,Cmt):-
 cmt_override(Line,_,_),
  wots(Cmt,print_prepended(Line, SSS)).

:- meta_predicate if_color_debug(0).
:- meta_predicate if_color_debug(0,0).

%=

%% if_color_debug is det.
%
% If Color Debug.
%
if_color_debug:-current_prolog_flag(dmsg_color,true).

%=

%% if_color_debug( :Goal) is nondet.
%
% If Color Debug.
%
if_color_debug(Goal):- if_color_debug(Goal, true).

%=

%% if_color_debug( :Goal, :GoalUnColor) is det.
%
% If Color Debug.
%
if_color_debug(Goal,UnColor):- (if_color_debug->Goal;UnColor),!.



color_line(C,N):-
 dzotrace((
  new_line_if_needed,
    forall(between(1,N,_),ansi_term:
      terminal_ansi_format([fg(C)],"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%\n",[])))).



% % = :- export((portray_clause_w_vars/4,ansicall/3,ansi_control_conv/2)).

:- thread_local(tlbugger:skipDumpST9/0).
:- thread_local(tlbugger:skipDMsg/0).

% @(dmsg0(succeed(S_1)),[S_1=logic])


:- thread_local(tlbugger:no_slow_io/0).
:- multifile(tlbugger:no_slow_io/0).
%:- asserta(tlbugger:no_slow_io).

:- create_prolog_flag(retry_undefined,none,[type(term),keep(true)]).


:- thread_local(t_l:hide_dmsg/0).
%=

%% dmsg( ?C) is det.
%
% (debug)message.
%
dmsg(_):- t_l:hide_dmsg, \+ tlbugger:no_slow_io, !.
dmsg(C):- dzotrace((tlbugger:no_slow_io,!,stream_property(X,file_no(2)),writeln(X,dmsg(C)))).
dmsg(V):- quietly(likely_folded((locally(set_prolog_flag(retry_undefined,none),
  if_defined_local(dmsg0(V),logicmoo_util_catch:ddmsg(V)))))),!.
%dmsg(F,A):- dzotrace((tlbugger:no_slow_io,on_x_fail(smart_format(atom(S),F,A))->writeln(dmsg(S));writeln(dmsg_fail(F,A)))),!.

:- system:import(dmsg/1).
% system:dmsg(O):-logicmoo_util_dmsg(O).
%=

%% dmsg( ?F, ?A) is det.
%
% (debug)message Primary Helper.
%
dmsg(F,A):- transform_mesg(F,A,FA),!,dmsg(FA).

transform_mesg(F,A,ansi(F,A)):- is_sgr_on_code(F),!.
transform_mesg(warning,A,warning(A)).
transform_mesg(error,A,error(A)).
transform_mesg(info,A,info(A)).
transform_mesg(information,A,A).
transform_mesg(F,A,fmt0(F,A)).

%dmsg(F,A):-
%              if_defined_local(dmsg0(F,A),logicmoo_util_catch:ddmsg(F,A))),!.

%with_output_to_main_error(G):- !,call(G).

with_output_to_main_error(G):-
  t_l:thread_local_error_stream(Where),!,
  with_output_to_each(Where,G).
with_output_to_main_error(G):-
  with_output_to_real_main_error(G).

%:- volatile(tmp:real_main_error/1).
:- dynamic(tmp:real_main_error/1).

save_real_main_error:-
 % volatile(tmp:real_main_error/1),
  dynamic(tmp:real_main_error/1),
  stream_property(Err,file_no(2)),
  asserta(tmp:real_main_error(Err)).

:- initialization(retractall(tmp:real_main_error(_)), prepare_state).
:- ignore(now_and_later(save_real_main_error)).

with_output_to_real_main_error(G):-
  %set_prolog_flag(occurs_check,false),
  %stream_property(Err,file_no(2)),!,
  tmp:real_main_error(Err) -> with_output_to_each(Err,G); with_output_to_each(user_error,G).

/*
with_output_to_main_error(G):-
  set_prolog_flag(occurs_check,false),
  stream_property(Err,file_no(2)),
  with_output_to_each(Err,G).
  */
/*
  ignore((get_thread_current_error(TErr),
    \+ same_streams(TErr,Err),
    with_output_to_each(TErr,G))).

same_streams(TErr,Err):- TErr==Err,!.
same_streams(TErr,Err):- stream_property(TErr,file_no(A)),stream_property(Err,file_no(B)),!,A==B.
*/
:- b_setval('$lm_output_steam',[]).
:- nb_setval('$lm_output_steam',[]).
%% wdmsg( ?X) is semidet.
%
% Wdmsg.
%
wdmsg(_):- ttyflush,current_prolog_flag(debug_level,0),current_prolog_flag(dmsg_level,never),!.
wdmsg(X):- likely_folded(wdmsg_goal(in_cmt(line,fmt(X)),dmsg(X))).

likely_folded(X):- dis_pp(bfly)->pretty_clauses:with_folding_depth(1,X);call(X).

wdmsg_goal(G,G2):-
  quietly((ignore(((format('~N'),ttyflush,with_all_dmsg(G2),format('~N'),ttyflush),
  (fmt_visible_to_console -> true ;ignore(on_x_fail(with_output_to_main_error((G))))))))), !.

fmt_visible_to_console:-
  thread_self(main),
  stream_property(Where,alias(current_output)),!,
  fmt_visible_to_console(Where).

fmt_visible_to_console(Where):- stream_property(Stderr,file_no(2)), same_streams(Where,Stderr),!.
fmt_visible_to_console(Where):- stream_property(StdOut,file_no(1)), same_streams(Where,StdOut),!.
%fmt_visible_to_console(Where):- stream_property(Where,tty(true)),!.




%% wdmsg( ?F, ?X) is semidet.
%
% Wdmsg.
%
wdmsg(_,_):- current_prolog_flag(debug_level,0),current_prolog_flag(dmsg_level,never),!.
wdmsg(F,X):- wdmsg_goal(in_cmt(fmt(F,X)),dmsg(F,X)).


%% wdmsg( ?F, ?X) is semidet.
%
% Wdmsg.
%
wdmsg(W,F,X):- wdmsg_goal(in_cmt(line,fmt(F,X)),dmsg(W,F,X)).

:- meta_predicate wdmsgl(1,+).
:- meta_predicate wdmsgl(+,1,+).

%% wdmsgl( ?CNF) is det.
%
% Wdmsgl.
%
wdmsgl(X):- dzotrace(wdmsgl(fmt9,X)),!.
wdmsgl(With,X):- (must((wdmsgl('',With,X)))),!.

wdmsgl(NAME,With,CNF):- is_ftVar(CNF),!,call(With,NAME=CNF).
wdmsgl(_,With,(C:-CNF)):- call(With,(C :-CNF)),!.
%wdmsgl(_,With,'==>'(CNF,C)):- call(With,(C :- (fwc, CNF))),!.
wdmsgl(_,With,(NAME=CNF)):- wdmsgl(NAME,With,CNF),!.
wdmsgl(NAME,With,CNF):- is_list(CNF),must_maplist_det(wdmsgl(NAME,With),CNF),!.
wdmsgl('',With,(C:-CNF)):- call(With,(C :-CNF)),!.
wdmsgl(NAME,With,(C:-CNF)):- call(With,(NAME: C :-CNF)),!.
wdmsgl(NAME,With,(:-CNF)):- call(With,(NAME:-CNF)),!.
wdmsgl(NAME,With,CNF):- call(With,NAME:-CNF),!.



%% dmsginfo( ?V) is det.
%
% Dmsginfo.
%
dmsginfo(V):-dmsg(info,V).

%=


%% vdmsg( ?L, ?F) is det.
%
% Vdmsg.
%
vdmsg(L,F):-loggerReFmt(L,LR),loggerFmtReal(LR,F,[]).

%=

%% dmsg( ?L, ?F, ?A) is det.
%
% (debug)message.
%
dmsg(L,F,A):-loggerReFmt(L,LR),loggerFmtReal(LR,F,A).

:- thread_local(tlbugger:in_dmsg/1).
:- dynamic tlbugger:dmsg_hook/1.
:- multifile tlbugger:dmsg_hook/1.
:- thread_local(t_l:no_kif_var_coroutines/1).


%=

%% dmsg0( ?V) is det.
%
% (debug)message Primary Helper.
%

dmsg0(V):-notrace((locally(local_override(no_kif_var_coroutines,true),
   ignore(with_output_to_main_error(dmsg00(V)))))),!.

%=

%% dmsg00( ?V) is det.
%
% (debug)message Primary Helper Primary Helper.
%
dmsg00(V):-notrace(var(V)),!,dmsg00(dmsg_var(V)).
dmsg00(V):-cyclic_term(V),!,writeln(cyclic_term),flush_output,writeln(V),!.
dmsg00(call(Code)):- callable(Code), !, with_output_to(string(S),catch((dzotrace(Code)->TF=true;TF=failed),TF,true)),
  (TF=true->dmsg(S);(smart_format(string(S2),'~Ndmsg(call(Code)) of ~q~n~q: ~s ~n',[Code,TF,S]),wdmsg(S2),!,fail)).
dmsg00(V):- catch(dumpst:simplify_goal_printed(V,VV),_,fail),!,dmsg000(VV),!.
dmsg00(V):- dmsg000(V),!.


%% dmsg000( ?V) is det.
%
% (debug)message Primary Helper Primary Helper Primary Helper.
%

dmsg000(V):-
 with_output_to_main_error(
   (dzotrace(smart_format(string(K),'~p',[V])),
   (tlbugger:in_dmsg(K)-> dmsg5(dmsg5(V));  % format_to_error('~N% ~q~n',[dmsg0(V)]) ;
      asserta(tlbugger:in_dmsg(K),Ref),call_cleanup(dmsg1(V),erase(Ref))))),!.

% = :- export(dmsg1/1).


%=

%% dmsg1( ?V) is det.
%
% (debug)message Secondary Helper.
%
dmsg1(V):- !, dmsg3(V).
dmsg1(V):- var(V),!,dmsg1(warn(dmsg_var(V))).
dmsg1(_):- current_prolog_flag(dmsg_level,never),!.
dmsg1(V):- tlbugger:is_with_dmsg(FP),!,univ_safe_3(FP,FPL),append(FPL,[V],VVL),univ_safe_3(VV,VVL),once(dmsg1(VV)),!.
dmsg1(NC):- cyclic_term(NC),!,dtrace,format_to_error('~N% ~q~n',[dmsg_cyclic_term_1]).
dmsg1(NC):- tlbugger:skipDMsg,!,loop_check_early(dmsg2(NC),format_to_error('~N% ~q~n',[skipDMsg])),!.
dmsg1(V):- locally(tlbugger:skipDMsg,((once(dmsg2(V)), ignore((tlbugger:dmsg_hook(V),fail))))),!.

% = :- export(dmsg2/1).

%=

%% dmsg2( :TermNC) is det.
%
% (debug)message Extended Helper.
%
dmsg2(NC):- cyclic_term(NC),!,format_to_error('~N% ~q~n',[dmsg_cyclic_term_2]).
dmsg2(NC):- var(NC),!,format_to_error('~N% DMSG VAR ~q~n',[NC]).
dmsg2(skip_dmsg(_)):-!.
%dmsg2(C):- \+ current_prolog_flag(dmsg_level,always), dmsg_hides_message(C),!.
%dmsg2(trace_or_throw(V)):- dumpST(350),dmsg(warning(V)),fail.
%dmsg2(error(V)):- dumpST(250),dmsg(warning,V),fail.
%dmsg2(warn(V)):- dumpST(150),dmsg(warning,V),fail.
dmsg2(Msg):-quietly((tlbugger:no_slow_io,!,dmsg3(Msg))),!.
dmsg2(color(Ctrl,Msg)):- !,  ansicall(Ctrl,without_color(dmsg3(Msg))).
dmsg2(ansi(Ctrl,Msg)):- !,  ansicall(Ctrl,without_color(dmsg3(Msg))).
dmsg2(Msg):- notrace(mesg_color(Msg,Ctrl)),with_color(ansicall(Ctrl,without_color(dmsg3(Msg)))).


%=

%% dmsg3( ?C) is det.
%
% Dmsg3.
%
dmsg3(C):- tlbugger:no_slow_io,!,writeln(dmsg3(C)).
dmsg3(C):- strip_module(C,_,SM),
  ((cfunctor(SM,Topic,_),debugging(Topic,_True_or_False),logger_property(Topic,once,true),!,
      (dmsg_log(Topic,_Time,C) -> true ; ((get_time(Time),asserta(dmsg_log(todo,Time,C)),!,dmsg4(C)))))),!.

dmsg3(C):-dmsg4(C),!.


%=

%% dmsg4( ?Msg) is det.
%
% Dmsg4.
%
dmsg4(_):- current_prolog_flag(dmsg_level,never),!.
%dmsg4(Msg):- !,dmsg5(Msg).
dmsg4(Msg):- mline_number,dmsg5(Msg).

mline_number:-  dis_pp(bfly),!.
mline_number:- (1 is random(5)),!,ignore(dzotrace(once(show_source_location))).
mline_number.
%=

%% dmsg5( ?Msg) is det.
%
% Dmsg5.
%

dmsg5(Msg):- to_stderror(in_cmt(line,fmt9(Msg))).

%=

%% dmsg5( ?Msg, ?Args) is det.
%
% Dmsg5.
%
%dmsg5(Msg,Args):- dmsg5(fmt0(Msg,Args)).



%=

%% get_indent_level( :PRED2Max) is det.
%
% Get Indent Level.
%
get_indent_level(Max) :- if_prolog(swi,((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,FD)))),Depth is FD div 5,Max is min(Depth,40),!.
get_indent_level(2):-!.


/*
ansifmt(+Attributes, +Format, +Args) is det
Format text with ANSI attributes. This predicate behaves as smart_format/2 using Format and Args, but if the current_output is a terminal, it adds ANSI escape sequences according to Attributes. For example, to print a text in bold cyan, do
?- ansifmt([bold,fg(cyan)], 'Hello ~w', [world]).
Attributes is either a single attribute or a list thereof. The attribute names are derived from the ANSI specification. See the source for sgr_code/2 for details. Some commonly used attributes are:

bold
underline
fg(Color), bg(Color), hfg(Color), hbg(Color)
Defined color constants are below. default can be used to access the default color of the terminal.

black, red, green, yellow, blue, magenta, cyan, white
ANSI sequences are sent if and only if

The current_output has the property tty(true) (see stream_property/2).
The Prolog flag color_term is true.

ansifmt(Ctrl, Format, Args) :- ansifmt(current_output, Ctrl, Format, Args).

ansifmt(Stream, Ctrl, Format, Args) :-
     % we can "assume"
        % ignore(((stream_property(Stream, tty(true)),current_prolog_flag(color_term, true)))), !,
	(   is_list(Ctrl)
	->  maplist(ansi_term:sgr_code_ex, Ctrl, Codes),
	    atomic_list_concat(Codes, (';'), OnCode)
	;   ansi_term:sgr_code_ex(Ctrl, OnCode)
	),
	'smart_format'(string(Fmt), '\e[~~wm~w\e[0m', [Format]),
        retractall(tlbugger:last_used_color(Ctrl)),asserta(tlbugger:last_used_color(Ctrl)),
	'smart_format'(Stream, Fmt, [OnCode|Args]),
	flush_output,!.
ansifmt(Stream, _Attr, Format, Args) :- 'smart_format'(Stream, Format, Args).

*/

% % % OFF :- system:use_module(library(ansi_term)).

% = :- export(ansifmt/2).

%=

%% ansifmt( ?Ctrl, ?Fmt) is det.
%
% Ansifmt.
%
ansifmt(Ctrl,Fmt):- colormsg(Ctrl,Fmt).
% = :- export(ansifmt/3).

%=

%% ansifmt( ?Ctrl, ?F, ?A) is det.
%
% Ansifmt.
%
ansifmt(Ctrl,F,A):- colormsg(Ctrl,(smart_format(F,A))).



%=

%% debugm( ?X) is det.
%
% Debugm.
%
debugm(X):-dzotrace((compound(X),cfunctor(X,F,_),!,debugm(F,X))),!.
debugm(X):-dzotrace((debugm(X,X))).

%=

%% debugm( ?Why, ?Msg) is det.
%
% Debugm.
%
debugm(_,_):-dzotrace(current_prolog_flag(dmsg_level,never)),!.
debugm(Why,Msg):- dzotrace((dmsg(debugm(Why,Msg)),!,debugm0(Why,Msg))).
debugm0(Why,Msg):-
   /*\+ debugging(mpred),*/
   \+ debugging(Why), \+ debugging(mpred(Why)),!, debug(Why,'~N~p~n',[Msg]),!.
debugm0(Why,Msg):- debug(Why,'~N~p~n',[Msg]),!.



%% colormsg( ?Ctrl, ?Msg) is det.
%
% Colormsg.
%
colormsg(d,Msg):- notrace(mesg_color(Msg,Ctrl)),!,colormsg(Ctrl,Msg).
colormsg(Ctrl,Msg):- ansicall(Ctrl,fmt0(Msg)).

% = :- export(ansicall/2).

%=

%% ansicall( ?Ctrl, :Goal) is nondet.
%
% Ansicall.
%

% ansicall(_,Goal):-!,Goal.
%ansicall(Ctrl,Goal):- dzotrace((current_output(Out), ansicall(Out,Ctrl,Goal))).
ansicall(Ctrl,Goal):- ansicall(current_output,Ctrl,Goal),!.

%in_color(Ctrl,Goal):- ansicall(Ctrl,Goal).


%=

%% ansi_control_conv( ?Ctrl, ?CtrlO) is det.
%
% Ansi Control Conv.
%
ansi_control_conv(Ctrl,CtrlO):-tlbugger:no_slow_io,!,flatten([Ctrl],CtrlO),!.
ansi_control_conv(Ctrl,CtrlO):- ansi_control_conv0(Ctrl,CtrlOO),!,CtrlO=CtrlOO.
ansi_control_conv0([],[]):-!.
ansi_control_conv0(MC,Ctrl):- strip_module(MC,_,C),MC\==C,!,ansi_control_conv0(C,Ctrl).
ansi_control_conv0(warn,Ctrl):- !, ansi_control_conv(warning,Ctrl),!.
ansi_control_conv0(Level,Ctrl):- \+ ground(Level), !, flatten([Level],Ctrl),!.%ansi_control_conv0(Level,Ctrl):- ansi_term:level_attrs(Level,Ansi),Level\=Ansi,!,ansi_control_conv(Ansi,Ctrl).

ansi_control_conv0(Color,Ctrl):- ansi_term:sgr_code(Color,_),!,Ctrl=Color.
ansi_control_conv0(Color,Ctrl):- ansi_term:off_code(Color,_),!,Ctrl=Color.
ansi_control_conv0(Color,Ctrl):- ansi_term:ansi_color(Color,_),!,ansi_control_conv0(fg(Color),Ctrl).
ansi_control_conv0([H|T],HT):- ansi_control_conv(H,HH),!,ansi_control_conv(T,TT),!,flatten([HH,TT],HT),!.
ansi_control_conv0(Ctrl,CtrlO):-flatten([Ctrl],CtrlO),!.



%=

%% is_tty( ?Out) is det.
%
% If Is A Tty.
%
:- multifile(tlbugger:no_colors/0).
:- thread_local(tlbugger:no_colors/0).
is_tty(Out):- \+ tlbugger:no_colors, \+ tlbugger:no_slow_io, is_stream(Out),stream_property(Out,tty(true)).

use_tty(S,TTY):- \+ compound(S), is_stream(S), stream_property(S,tty(TTY)),!.
use_tty(_,TTY):- stream_property(current_output,tty(TTY)),!.
use_tty(_,false).

:- meta_predicate(woto_tty(+,+,0)).
:- export(woto_tty/3).
woto_tty(S,TTY,Goal):- with_output_to_each(S,((set_stream_ignore(current_output,tty(TTY))),Goal)).

:- meta_predicate(woto(+,0)).
:- export(woto/2).
woto(S,Goal):- once(use_tty(S,TTY);TTY=true),
  get_stream_setup(Setup), woto_tty(S,TTY,(Setup,Goal)).

get_stream_setup(S):- S = true,!.
get_stream_setup(S):-
 %G = (current_output(CO),maplist(call,Setup)),
 G = maplist(ignore,Setup),
 %S = (writeln(user_output,G),call(G)),
 G = S,
 Out = current_output,
 Template = set_stream_ignore(Prop),
  bagof(Template,(stream_setup(Prop),stream_property(Out,Prop)),Setup).

set_stream_ignore(P):- ((current_output(S),set_stream_ignore(S,P)))->true;true.
%set_stream_ignore(_,_):-!.
set_stream_ignore(S,P):- ignore(notrace(catch(set_stream(S,P),E,(writeln(user_error,E=set_stream(S,P)))))).

stream_setup(encoding(_)).
stream_setup(tty(_)).
stream_setup(representation_errors(_)).

:- meta_predicate(wots(-,0)).
:- export(wots/2).
wots(S,Goal):-
   (nb_current('$wots_stack',Was);Was=[]),
   current_output(Out),
     locally(nb_setval('$wots_stack',[Out|Was]),woto(string(S),Goal)).

:- meta_predicate(wotso(0)).
:- export(wotso/1).
wotso(Goal):- !, call(Goal).
wotso(Goal):- wots(S,Goal), ignore((S\=="",write(S))).

:- meta_predicate(wote(0)).
:- export(wote/1).
wote(G):-stream_property(X,file_no(2)), with_output_to_each(X,G).

:- meta_predicate(weto(0)).
%weto(G):- !, call(G).
:- export(weto/1).
weto(G):-
  stream_property(UE,alias(user_error)),
  stream_property(CO,alias(current_output)),
  UE==CO,!,call(G).

weto(G):- !, with_error_to_each(current_output,G).
weto(G):-
  stream_property(UE,alias(user_error)),
  stream_property(UO,alias(user_output)),
  once(stream_property(CE,alias(current_error));CE=UE),
  once(stream_property(CO,alias(current_output));current_output(CO)),!,
  mscce_orig(
     (set_stream_nop(CO,alias(user_error)),set_stream_nop(CO,alias(user_output)),
         set_stream_nop(CO,alias(current_error)),set_stream_nop(CO,alias(current_output))),

                locally_tl(thread_local_error_stream(CO),call(G)),

     (set_stream_nop(UE,alias(user_error)),set_stream_nop(CE,alias(current_error)),
         set_stream_nop(UO,alias(user_output)),set_stream_nop(CO,alias(current_output)))).
weto(G):- call(G).

set_stream_nop(S,P):- nop(set_stream(S,P)).

:- meta_predicate(with_ioe(0)).
:- export(with_ioe/1).
with_ioe(G):-
  stream_property(UE,alias(user_error)),
  stream_property(UO,alias(user_output)),
  once(stream_property(CE,alias(current_error));CE=UE),
  once(stream_property(CO,alias(current_output));current_output(CO)),!,
  scce_orig(true, G,
     (set_stream_ignore(UE,alias(user_error)),set_stream_ignore(CE,alias(current_error)),
         set_stream_ignore(UO,alias(user_output)),set_stream_ignore(CO,alias(current_output)))).


%=

%% ansicall( ?Out, ?UPARAM2, :Goal) is nondet.
%
% Ansicall.
%
% in_pengines:- if_defined_local(relative_frame(source_context_module,pengines,_)).

ansicall(_,_,Goal):- (tlbugger:skipDumpST9;tlbugger:no_slow_io),!,call(Goal).
%ansicall(Out,Ctrl,Goal):-  woto(Out,ansicall_2(current_output,Ctrl,Goal)).
ansicall(Out,Ctrl,Goal):- Out == current_output,!,ansicall_2(Out,Ctrl,Goal).
ansicall(Out,Ctrl,Goal):-  woto(Out,ansicall_2(current_output,Ctrl,Goal)).

ansicall_2(Out,CtrlIn,Goal):- ((ansi_control_conv(CtrlIn,Ctrl);CtrlIn=Ctrl)),!,
  ansicall_3(Out,Ctrl,Goal).
%ansicall_2(Out,Ctrl,Goal):- \+ dis_pp(bfly), !, ansicall_3(Out,Ctrl,Goal).
%ansicall_2(Out,Ctrl,Goal):- bfly_html_goal(ansicall_3(Out,Ctrl,Goal)).

ansicall_3(Out,Ctrl,Goal):-
   (quietly((retractall(tlbugger:last_used_color(_)),asserta(tlbugger:last_used_color(Ctrl)),!,ansicall_4(Out,Ctrl,Goal)))).

%maybe_bfly_in_out(G):- on_x_fail(bfly_html_goal(G)),!.
%maybe_bfly_in_out(G):- call(G).

mUST_det_ll(X):- conjuncts_to_list(X,List),List\=[_],!,maplist(mUST_det_ll,List).
mUST_det_ll(mUST_det_ll(X)):- !, mUST_det_ll(X).
mUST_det_ll(X):- tracing,!,mUST_not_error(X).
mUST_det_ll((X,Y,Z)):- !, (mUST_det_ll(X),mUST_det_ll(Y),mUST_det_ll(Z)).
mUST_det_ll((X,Y)):- !, (mUST_det_ll(X)->mUST_det_ll(Y)).
%mUST_det_ll(if_t(X,Y)):- !, if_t(mUST_not_error(X),mUST_det_ll(Y)).
mUST_det_ll((A->X;Y)):- !,(mUST_not_error(A)->mUST_det_ll(X);mUST_det_ll(Y)).
mUST_det_ll((A*->X;Y)):- !,(mUST_not_error(A)*->mUST_det_ll(X);mUST_det_ll(Y)).
mUST_det_ll((X;Y)):- !, ((mUST_not_error(X);mUST_not_error(Y))->true;mUST_det_ll_failed(X;Y)).
mUST_det_ll(\+ (X)):- !, (\+ mUST_not_error(X) -> true ; mUST_det_ll_failed(\+ X)).
%mUST_det_ll((M:Y)):- nonvar(M), !, M:mUST_det_ll(Y).
mUST_det_ll(once(A)):- !, once(mUST_det_ll(A)).
mUST_det_ll(X):-
  strip_module(X,M,P),functor(P,F,A),scce_orig(nop(trace(M:F/A,+fail)),(mUST_not_error(X)*->true;mUST_det_ll_failed(X)),
    nop(trace(M:F/A,-fail))).

mUST_not_error(X):- catch(X,E,(E=='$aborted'-> throw(E);(/*arcST,*/wdmsg(E=X),wdmsg(rRTrace(E)=X),rRTrace(X)))).

mUST_det_ll_failed(X):- notrace,wdmsg(failed(X))/*,arcST*/,nortrace,trace,rRTrace(X),!.
% mUST_det_ll(X):- mUST_det_ll(X),!.

rRTrace(X):- !, rtrace(X).
rRTrace(X):- notrace,nortrace, arcST, sleep(0.5), trace, (notrace(\+ current_prolog_flag(gui_tracer,true)) -> rtrace(X); (trace,call(X))).

%=

%% ansicall_4( ?Out, ?Ctrl, :Goal) is nondet.
%
% Ansicall Primary Helper.
%
ansicall_4(_,[],Goal):-!,call(Goal).
ansicall_4(Out,[Ctrl|Set],Goal):-!, ansicall_4(Out,Ctrl,ansicall_4(Out,Set,mUST_det_ll(Goal))).
ansicall_4(Out,Ctrl,Goal):- keep_line_pos_w_w(Out, ansicall_5(Out,Ctrl,mUST_det_ll(Goal))).

ansicall_5(Out,Ctrl,Goal):- maybe_bfly_html(ansicall_6(Out,Ctrl,Goal)).

:- meta_predicate(maybe_bfly_html(0)).
maybe_bfly_html(Goal):- current_predicate(ensure_pp/1)->ensure_pp(Goal);call(Goal).
:- meta_predicate(no_bfly(0)).
no_bfly(Goal):- current_predicate(in_bfly/2)->in_bfly(f,Goal);call(Goal).

:- export(maybe_bfly_html/1).

ansicall_6(Out,Ctrl,Goal):- quietly((must(using_style(Out,Ctrl,Goal,How)),!, call(How))).

:- export(color_format/3).
color_format(MC,F,A):-
 notrace(((ansi_control_conv(MC,CC2),
   color_format2(CC2,F,A)))),!.

:- b_setval('$lm_output_steam',[]).
:- nb_setval('$lm_output_steam',[]).

color_format2(C,F,A):-
  (nb_current('$without_color',Was);Was=u),
  (stream_property(current_output, tty(TTY)); TTY=u),
  color_format3(TTY,Was,C,F,A).

color_format3(_,true,_,F,A):- !, format(F,A).
color_format3(_,false,C,F,A):- !, terminal_ansi_format(C,F,A).
color_format3(_,_,C,F,A):- !, terminal_ansi_format(C,F,A).


:- export(terminal_ansi_format/3).
%terminal_ansi_format(C,F,A):- ansicall_6(current_output,C,format(F,A)),!.
terminal_ansi_format(Attr, Format, Args) :- terminal_ansi_format(current_output, Attr, Format, Args).

terminal_ansi_format(Stream, Class, Format, Args):- terminal_ansi_goal(Stream, Class, format(Format, Args)),!.
terminal_ansi_format(Stream, Class, Format, Args):- ansi_term:ansi_format(Stream, Class, Format, Args),!.

terminal_ansi_goal(Stream, Class, Goal):-
 ansi_term:(
 class_attrs(Class, Attr),
    phrase(sgr_codes_ex(Attr), Codes),
    atomic_list_concat(Codes, ;, Code),
    with_output_to_each(
        Stream,
        scce_orig(
            keep_line_pos(current_output, format('\e[~wm', [Code])),
            call(Goal),
            keep_line_pos(current_output, format('\e[0m'))
        )
    ),
    flush_output).



%=

%% keep_line_pos_w_w( ?S, :GoalG) is det.
%
% Keep Line Pos.
%
% keep_line_pos_w_w(_, G):-!,G.
:- thread_local(bfly_tl:bfly_setting/2).

keep_line_pos_w_w(_, G) :- use_html_styles, !, call(G).
keep_line_pos_w_w(_, G) :- !, call(G).
keep_line_pos_w_w(S, G) :-
      line_pos(S,LPos) ->
         scce_orig(G, set_stream_line_position_safe(S, LPos)) ; call(G).

line_pos(S,LPos):- stream_property(S, position(Pos)),stream_position_data(line_position, Pos, LPos).

set_stream_line_position_safe(S,Pos):-
  catch(set_stream_nop(S, line_position(Pos)),E,dmsg(error(E))).

:- multifile(tlbugger:term_color0/2).
:- dynamic(tlbugger:term_color0/2).


%tlbugger:term_color0(retract,magenta).
%tlbugger:term_color0(retractall,magenta).

%=

%% term_color0( ?VALUE1, ?VALUE2) is det.
%
% Hook To [tlbugger:term_color0/2] For Module Logicmoo_util_dmsg.
% Term Color Primary Helper.
%
tlbugger:term_color0(assertz,hfg(green)).
tlbugger:term_color0(ainz,hfg(green)).
tlbugger:term_color0(aina,hfg(green)).
tlbugger:term_color0(mpred_op,hfg(blue)).



%=

%% f_word( ?T, ?A) is det.
%
% Functor Word.
%
f_word("",""):-!.
f_word(T,A):-concat_atom(List,' ',T),lists:member(A,List),atom(A),atom_length(A,L),L>0,!.
f_word(T,A):-concat_atom(List,'_',T),lists:member(A,List),atom(A),atom_length(A,L),L>0,!.
f_word(T,A):- string_to_atom(T,P),sub_atom(P,0,10,_,A),A\==P,!.
f_word(T,A):- string_to_atom(T,A),!.


%=

%% mesg_arg1( :TermT, ?TT) is det.
%
% Mesg Argument Secondary Helper.
%
mesg_arg1(T,_TT):-var(T),!,fail.
mesg_arg1(_:T,C):-nonvar(T),!,mesg_arg1(T,C).
mesg_arg1(T,TT):-not(compound(T)),!,T=TT.
mesg_arg1(T,C):-compound(T),arg(1,T,F),nonvar(F),!,mesg_arg1(F,C).
mesg_arg1(T,F):-cfunctor(T,F,_).


% = :- export(defined_message_color/2).
:- dynamic(defined_message_color/2).


%=

%% defined_message_color( ?A, ?B) is det.
%
% Defined Message Color.
%
defined_message_color(todo,[fg(red),bg(black),underline]).
defined_message_color(error,[fg(red),hbg(black),bold]).
defined_message_color(warn,[fg(black),hbg(red),bold]).
defined_message_color(A,B):-tlbugger:term_color0(A,B).



%=

%% predef_functor_color( ?F, ?C) is det.
%
% Predef Functor Color.
%
predef_functor_color(F,C):- defined_message_color(F,C),!.
predef_functor_color(F,C):- defined_message_color(F/_,C),!.
predef_functor_color(F,C):- tlbugger:term_color0(F,C),!.


%=

%% functor_color( ?F, ?C) is det.
%
% Functor Color.
%
functor_color(F,C):- predef_functor_color(F,C),!.
functor_color(F,C):- next_color(C),ignore(on_x_fail(assertz(tlbugger:term_color0(F,C)))),!.


:- thread_local(tlbugger:last_used_color/1).

% tlbugger:last_used_color(pink).


%=

%% last_used_fg_color( ?LFG) is det.
%
% Last Used Fg Color.
%
last_used_fg_color(LFG):-tlbugger:last_used_color(LU),fg_color(LU,LFG),!.
last_used_fg_color(default).


%=

%% good_next_color( ?C) is det.
%
% Good Next Color.
%
good_next_color(C):-var(C),!,trace_or_throw(var_good_next_color(C)),!.
good_next_color(C):- last_used_fg_color(LFG),fg_color(C,FG),FG\=LFG,!.
good_next_color(C):- not(unliked_ctrl(C)).


%=

%% unliked_ctrl( ?X) is det.
%
% Unliked Ctrl.
%
unliked_ctrl(fg(blue)).
unliked_ctrl(fg(black)).
unliked_ctrl(fg(red)).
unliked_ctrl(bg(white)).
unliked_ctrl(hbg(white)).
unliked_ctrl(X):-is_list(X),member(E,X),nonvar(E),unliked_ctrl(E).


%=

%% fg_color( ?LU, ?FG) is det.
%
% Fg Color.
%
fg_color(LU,FG):-member(fg(FG),LU),FG\=white,!.
fg_color(LU,FG):-member(hfg(FG),LU),FG\=white,!.
fg_color(_,default).

% = :- export(random_color/1).

%=

%% random_color( ?M) is det.
%
% Random Color.
%
random_color([reset,M,FG,BG,font(Font)]):-Font is random(8),
  findall(Cr,ansi_term:ansi_color(Cr, _),L),
  random_member(E,L),
  random_member(FG,[hfg(E),fg(E)]),not(unliked_ctrl(FG)),
  contrasting_color(FG,BG), not(unliked_ctrl(BG)),
  random_member(M,[bold,faint,reset,bold,faint,reset,bold,faint,reset]),!. % underline,negative


% = :- export(tst_color/0).

%=

%% tst_color is det.
%
% Tst Color.
%
tst_color:- make, ignore((( between(1,20,_),random_member(Goal,[colormsg(C,cm(C)),dmsg(color(C,dm(C))),ansifmt(C,C)]),next_color(C),Goal,fail))).
% = :- export(tst_color/1).

%=

%% tst_color( ?C) is det.
%
% Tst Color.
%
tst_color(C):- make,colormsg(C,C).

% = :- export(next_color/1).

%=

%% next_color( :TermC) is det.
%
% Next Color.
%
next_color(C):- between(1,10,_), random_color(C), good_next_color(C),!.
next_color([underline|C]):- random_color(C),!.

% = :- export(contrasting_color/2).

%=

%% contrasting_color( ?A, ?VALUE2) is det.
%
% Contrasting Color.
%
contrasting_color(white,black).
contrasting_color(A,default):-atom(A),A \= black.
contrasting_color(fg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(hfg(C),bg(CC)):-!,contrasting_color(C,CC),!.
contrasting_color(black,white).
contrasting_color(default,default).
contrasting_color(_,default).

:- thread_local(ansi_prop/2).



%=

%% sgr_on_code( ?Ctrl, :PRED7OnCode) is det.
%
% Sgr Whenever Code.
%
sgr_on_code(Ctrl,OnCode):- sgr_on_code0(Ctrl,OnCode),!.
sgr_on_code(_Foo,7):-!. %  dzotrace((format_to_error('~NMISSING: ~q~n',[bad_sgr_on_code(Foo)]))),!.


%=

%% is_sgr_on_code( ?Ctrl) is det.
%
% If Is A Sgr Whenever Code.
%
is_sgr_on_code(Ctrl):-dzotrace(sgr_on_code0(Ctrl,_)),!.


%=

%% sgr_on_code0( ?Ctrl, :PRED6OnCode) is det.
%
% Sgr Whenever Code Primary Helper.
%
sgr_on_code0(Ctrl,OnCode):- ansi_term:sgr_code(Ctrl,OnCode).
sgr_on_code0(blink, 6).
sgr_on_code0(-Ctrl,OffCode):-  nonvar(Ctrl), sgr_off_code(Ctrl,OffCode).


%=

%% sgr_off_code( ?Ctrl, :GoalOnCode) is det.
%
% Sgr Off Code.
%
sgr_off_code(Ctrl,OnCode):-ansi_term:off_code(Ctrl,OnCode),!.
sgr_off_code(- Ctrl,OnCode):- nonvar(Ctrl), sgr_on_code(Ctrl,OnCode),!.
sgr_off_code(fg(_), CurFG):- (ansi_prop(fg,CurFG)->true;CurFG=39),!.
%sgr_off_code(bg(_), CurBG):- (ansi_prop(ng,CurBG)->true;CurBG=49),!.
sgr_off_code(bg(_), CurBG):- (ansi_prop(bg,CurBG)->true;CurBG=49),!.
sgr_off_code(bold, 21).
sgr_off_code(italic_and_franktur, 23).
sgr_off_code(franktur, 23).
sgr_off_code(italic, 23).
sgr_off_code(underline, 24).
sgr_off_code(blink, 25).
sgr_off_code(blink(_), 25).
sgr_off_code(negative, 27).
sgr_off_code(conceal, 28).
sgr_off_code(crossed_out, 29).
sgr_off_code(framed, 54).
sgr_off_code(overlined, 55).
sgr_off_code(_,0).


style_tag(bold,strong).
style_tag(italic,em).
style_tag(underline,u).
style_style(blink,"animation: blinker 0.6s linear infinite;").
style_style(blink(_),"animation: blinker 0.6s linear infinite;").
%style_style(reset,"all: initial;").
%style_style(reset,"display: block").
style_style(reset,"all: unset;").
style_style(font(2),"filter: brightness(60%);").
style_style(font(3),"font-style: italic;").
style_style(font(7),"filter: invert(100%);").
html_on_off(CC,format('<~w>',[C]),format('</~w>',[C])):- style_tag(CC,C).
html_on_off(CC,format('<div style="~w">',[C]),write('</div>')):- style_style(CC,C).
html_on_off(fg(CC),format('<font color="~w">',[C]),write('</font>')):- into_color_name(CC,C).
html_on_off(hfg(CC),format('<font color="~w" style="filter: brightness(85%);">',[C]),write('</font>')):- into_color_name(CC,C).
html_on_off(hbg(CC),format('<div style="background-color: ~w; filter: brightness(85%);">',[C]),write('</div>')):- into_color_name(CC,C).
html_on_off(bg(CC),format('<div style="background-color: ~w;">',[C]),write('</div>')):- into_color_name(CC,C).
html_on_off(CC,format('<font color="~w">',[C]),write('</font>')):- into_color_name(CC,C).
html_on_off(C,format('<div class="~w">',[C]),write('</div>')).

into_color_name(Default,initial):- Default==default,!.
into_color_name(C,C):- atom(C), ansi_term:ansi_color(C,_).



%=

%% sgr_code_on_off( ?Ctrl, ?OnCode, ?OffCode) is det.
%
% Sgr Code Whenever Off.
%
sgr_code_on_off(Ctrl,OnCode,OffCode):-sgr_on_code(Ctrl,OnCode),sgr_off_code(Ctrl,OffCode),!.
sgr_code_on_off(_Ctrl,_OnCode,[default]):-!.

:- thread_local(t_l:once_shown/2).
once_in_while(G):- once_in(G,60*5).  % every 5 minutes
once_in(G,Often):- term_to_atom(G,A),
  (( \+ (t_l:once_shown(A,WasThen), When is WasThen+Often, get_time(Now), When < Now))
   -> catch(G,_,fail) ; true),
  retractall(t_l:once_shown(A,_)),
  asserta(t_l:once_shown(A,Now)).



%=

%% msg_to_string( :TermVar, ?Str) is det.
%
% Msg Converted To String.
%
msg_to_string(Var,Str):-var(Var),!,sformat(Str,'~q',[Var]),!.
msg_to_string(portray(Msg),Str):- with_output_to_each(string(Str),(current_output(Out),portray_clause_w_vars(Out,Msg,[],[]))),!.
msg_to_string(pp(Msg),Str):- sformat(Str,Msg,[],[]),!.
msg_to_string(fmt(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(smart_format(F,A),Str):-sformat(Str,F,A),!.
msg_to_string(Msg,Str):-atomic(Msg),!,sformat(Str,'~w',[Msg]).
msg_to_string(m2s(Msg),Str):-message_to_string(Msg,Str),!.
msg_to_string(Msg,Str):-sformat(Str,Msg,[],[]),!.


:- thread_local t_l:formatter_hook/4.


%=

%% withFormatter( ?Lang, ?From, ?Vars, ?SForm) is det.
%
% Using Formatter.
%
withFormatter(Lang,From,Vars,SForm):- t_l:formatter_hook(Lang,From,Vars,SForm),!.
withFormatter(_Lang,From,_Vars,SForm):-sformat(SForm,'~w',[From]).


%=

%% flush_output_safe is det.
%
% Flush Output Safely Paying Attention To Corner Cases.
%
flush_output_safe:-ignore(catch(flush_output,_,true)).

%=

%% flush_output_safe( ?X) is det.
%
% Flush Output Safely Paying Attention To Corner Cases.
%
flush_output_safe(X):-ignore(catch(flush_output(X),_,true)).


%=

%% writeFailureLog( ?E, ?X) is det.
%
% Write Failure Log.
%
writeFailureLog(E,X):-
  get_thread_current_error(ERR),
		(fmt(ERR,'\n% error: ~q ~q\n',[E,X]),flush_output_safe(ERR),!,
		%,true.
		fmt('\n% error: ~q ~q\n',[E,X]),!,flush_output).

%unknown(Old, autoload).




%=

%% cls is det.
%
% Clauses.
%
cls:- ignore(catch(system:shell(cls,0),_,fail)).

% % % OFF
:- system:use_module(library(error)).
:- system:use_module(library(random)).
:- system:use_module(library(terms)).
:- system:use_module(library(dif)).
:- system:use_module(library(ctypes)).
:- system:use_module(library(aggregate)).
:- system:use_module(library(pairs)).
:- system:use_module(library(option)).
%:- list_autoload.
%:- autoload_all.
%:- list_autoload.
%:- ensure_loaded(logicmoo_util_varnames).
%:- ensure_loaded(logicmoo_util_catch).
% :- autoload_all([verbose(false)]).

/*
:- 'mpred_trace_none'(fmt(_)).
:- 'mpred_trace_none'(fmt(_,_)).
:- 'mpred_trace_none'(dfmt(_)).
:- 'mpred_trace_none'(dfmt(_,_)).
:- 'mpred_trace_none'(dmsg(_)).
:- 'mpred_trace_none'(dmsg(_,_)).
:- 'mpred_trace_none'(portray_clause_w_vars(_)).
*/

:- ignore((prolog_load_context(source,S),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((cfunctor(H,F,A),
  ignore(((atom(F),\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

:- '$hide'(wdmsg/1).
:- '$hide'(wdmsg/2).
:- '$hide'(dmsg/1).

:- fixup_exports.
:- fixup_module_exports_now.

