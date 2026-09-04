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

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_catch.pl
:- module(ucatch,[]).

:- define_into_module(
          [ !/1,
            addLibraryDir/0,
            get_main_error_stream/1,
            get_thread_current_error/1,
            source_variables_l/1,
            as_clause_no_m/3,
            as_clause_w_m/4,
            as_clause_w_m/5,
            source_module/1,
            bad_functor/1,
            main_self/1,
            set_main_error/0,
            find_main_eror/1,
            call_each/2,
            set_mains/0,
            current_why/1,
            thread_self_main/0,
            badfood/1,
            unsafe_safe/2,
            % quietly/1,
            doall_and_fail/1,
            quietly_must/1,
            on_x_f/3,

            keep_going/0,keep_going0/0,
            must/1,
            show_current_source_location/0,
            varnames_load_context/1,
            current_mfl4/4,
            current_source_location/2,
            hide_trace/1,
            (block)/2,
            (block3)/3,
            with_current_why/2,
            %bubbled_ex/1,
            %bubbled_ex_check/1,
            catchv/3,
            flag_call/1,
            flag_call0/1,
            current_source_file/1,current_source_location0/2,
            lmcache:current_main_error_stream/1,
            lmcache:thread_current_input/2,
            dbgsubst/4,
            dbgsubst0/4,
            ddmsg/1,
            ddmsg/2,
            ddmsg_call/1,
            det_lm/2,
            dif_safe/2,
            dumpST_error/1,
            errx/0,
            format_to_error/2,
            fresh_line_to_err/0,
            functor_catch/3,
            functor_safe/3,

            ib_multi_transparent33/1,
            if_defined/1,if_defined/2,
            input_key/1,
            is_ftCompound/1,%ftCompound/1,
            not_ftCompound/1,
            is_ftNameArity/2,
            is_ftNonvar/1, % ftNonvar/1,ftVar/1,
            is_ftVar/1,

            is_main_thread/0,
            is_pdt_like/0,
            is_release/0,
            need_speed/0,
            allow_unsafe_code/0,
            keep/2,
            loading_file/1,
            on_x_log_throw/1,
            %on_x_log_throwEach/1,
            on_x_log_cont/1,
            on_x_log_fail/1,
            skip_failx_u/1,
            on_xf_log_cont/1,
            on_xf_log_cont_l/1,
            maplist_safe/2,
            maplist_safe/3,
            module_functor/4,

            trace_or_throw/1,
            trace_or_throw/1,

            %must/1,
            must2/2,
            must_det_u/1,
            %must_det_dead/2,
            must_l/1,

            must_det_l/1,
            must_det_l_pred/2,
            call_must_det/2,
            call_each_det/2,
            p_call/2,

            nd_dbgsubst/4,
            nd_dbgsubst1/5,
            nd_dbgsubst2/4,

            not_is_release/0,
            one_must/2,
            one_must_det/2,
            % sanity/1,
            sanity2/2,
            save_streams/0,
            save_streams/1,
            set_block_exit/2,
            showHiddens/0,
            show_new_src_location/1,
            show_new_src_location/2,
            show_source_location/0,
            skipWrapper/0,
            slow_sanity/1,
            strip_arity/3,
            strip_f_module/2,
            get_thread_current_error/1,
            throwNoLib/0,
            to_m_f_arity_pi/5,
            to_mpi_matcher/2,
            to_pi0/3,
            warn_bad_functor/1,
            when_defined/1,
            with_main_error_to_output/1,
            with_current_io/1,
            with_error_to_main/1,
            with_dmsg_to_main/1,
            with_main_input/1,
            with_main_io/1,
            with_preds/6,
            without_must/1,
            hide_non_user_console/0,
            y_must/2,
            vsubst/4,
            must_find_and_call/1


          ]).

%:- include(ucatch).
:- include(first).


:- thread_local tlbugger:show_must_go_on/1.

keep_going:- notrace(keep_going0).

keep_going0:- getenv(keep_going,'-k').
keep_going0:- non_user_console.
keep_going0:- tlbugger:show_must_go_on(X)->X==true,!.
keep_going0:- current_prolog_flag(runtime_must,keep_going),!.
%keep_going0:- current_prolog_flag(debug_on_error,true), !, fail.


% % % OFF :- system:use_module((dmsg)).
% % % OFF :- system:use_module(library(must_sanity)).

vsubst(In,B,A,Out):-var(In),!,(In==B->Out=A;Out=In).
vsubst(In,B,A,Out):-subst(In,B,A,Out).

% % % % OFF :- system:use_module(logicmoo_util_prolog_streams).
:- thread_self(Goal),assert(lmcache:thread_main(user,Goal)).

main_self(main).
main_self(W):-atom(W),atom_concat('pdt_',_,W),!.
main_self(W):-atom(W),atom_concat('client',_,W),!.
main_self(W):-lmcache:thread_main(user,W),!.

thread_self_main:- notrace((thread_self(W),!,main_self(W))).

%% hide_non_user_console is semidet.
%
% Not User Console.
%
hide_non_user_console:-thread_self_main,!,fail.
hide_non_user_console:-current_input(In),stream_property(In,tty(true)),!,fail.
hide_non_user_console:-current_prolog_flag(debug_threads,true),!,fail.
hide_non_user_console:-current_input(In),stream_property(In, close_on_abort(true)).
hide_non_user_console:-current_input(In),stream_property(In, close_on_exec(true)).




/*
:- if(\+ current_predicate(system:nop/1)).
:- user:ensure_loaded(logicmoo_util_supp).
:- endif.
*/


:- meta_predicate


        block3(+, :, ?),
        catchv(0, ?, 0),

        if_defined(:),
        if_defined(+, 0),
        ddmsg_call(0),

                on_xf_log_cont(0),

        skip_failx_u(0),
        on_xf_log_cont_l(0),
        on_x_log_throw(0),
                with_current_why(*,0),
                with_only_current_why(*,0),


        on_x_log_cont(0),
        on_x_log_fail(0),


        % must(0),
        must2(+,0),
        must_find_and_call(0),
        must_det_u(0),
        %must_det_dead(0, 0),

        must_det_l(0),
        must_det_l_pred(1,+),
        call_must_det(1,+),
        call_each_det(1,+),
        call_each(1,+),
        p_call(*,+),

        must_l(0),
        one_must(0, 0),
        one_must_det(0, 0),
        unsafe_safe(0,0),
        % sanity(0),
        sanity2(+,0),
        slow_sanity(0),
        to_mpi_matcher(?, ?),
        when_defined(:),
        with_main_error_to_output(0),
        with_current_io(0),
        with_dmsg_to_main(0),
        with_error_to_main(0),
        with_main_input(0),
        with_main_io(0),
        with_preds(?, ?, ?, ?, ?, 0),
        without_must(0),
        %on_x_log_throwEach(0),
        y_must(?, 0).

:- module_transparent
        !/1,
        addLibraryDir/0,
        as_clause_no_m/3,
        as_clause_w_m/4,
        as_clause_w_m/5,
        bad_functor/1,
        badfood/1,
        (block)/2,
        %bubbled_ex/1,
        %bubbled_ex_check/1,
        current_source_file/1,
        lmcache:current_main_error_stream/1,
        dbgsubst/4,
        dbgsubst0/4,
        ddmsg/1,
        ddmsg/2,
        det_lm/2,
        dif_safe/2,
        errx/0,
        format_to_error/2,
        fresh_line_to_err/0,
        functor_catch/3,
        functor_safe/3,
        with_current_why/2,
        ib_multi_transparent33/1,
        input_key/1,
        is_ftCompound/1,
        not_ftCompound/1,
        is_ftNameArity/2,
        is_ftNonvar/1,
        is_ftVar/1,
        is_main_thread/0,
        is_pdt_like/0,
        is_release/0,
        keep/2,
        loading_file/1,
        %on_x_log_throwEach/1,
        maplist_safe/2,
        maplist_safe/3,
        module_functor/4,

        nd_dbgsubst/4,
        nd_dbgsubst1/5,
        nd_dbgsubst2/4,
        not_is_release/0,
        save_streams/0,
        save_streams/1,
        set_block_exit/2,
        showHiddens/0,
        show_new_src_location/1,
        show_new_src_location/2,

            on_xf_log_cont/1,
            on_xf_log_cont_l/1,
            skip_failx_u/1,
            p_call/2,

        show_source_location/0,
        skipWrapper/0,
        skipWrapper0/0,
        strip_arity/3,
        strip_f_module/2,
        get_thread_current_error/1,
        throwNoLib/0,
        to_m_f_arity_pi/5,
        to_pi0/3,
        warn_bad_functor/1.

:- meta_predicate
   doall_and_fail(0),
   quietly_must(0).

:- set_module(class(library)).

%:- prolog_listing:use_module(library(listing)).

:- use_module(library(occurs)).
:- use_module(library(gensym)).
:- use_module(library(when)).


:- use_module(library(backcomp)).
:- use_module(library(debug)).
:- use_module(library(occurs)).
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
:- use_module(library(codesio)).
:- use_module(library(charsio)).
:- use_module(library(ssl)).
:- use_module(library(prolog_codewalk)).
:- use_module(library(prolog_source)).
:- use_module(library(date)).
%:- use_module(library(editline)).
%:- system:use_module(library(listing)).
%:- unload_file(library(listing)).
:- multifile(prolog_listing:or_layout/1).
:- dynamic(prolog_listing:or_layout/1).
:- multifile(prolog_listing:clause_term/4).
:- dynamic(prolog_listing:clause_term/4).



/** <module> logicmoo_util_catch - catch-like bocks

   Tracer modes:

   quietly/1 - turn off tracer if already on but still dtrace on failure
   must/1 - dtrace on failure
   rtrace/1 - non interactive debug
   sanity/1 - run in quietly/1 when problems were detected previously otherwise skippable slow_sanity/1+hide_trace/1
   assertion/1 - throw on failure
   hide_trace/1 - hide dtrace temporarily
   slow_sanity/1 - skip unless in developer mode

*/

:- thread_local( tlbugger:old_no_repeats/0).
:- thread_local( tlbugger:skip_bugger/0).
:- thread_local( tlbugger:dont_skip_bugger/0).

:-meta_predicate(skip_failx_u(*)).
skip_failx_u(G):- must_det_l(G).
% skip_failx_u(G):-call_each_det([baseKB:call_u,on_xf_log_cont,notrace],G).



%=

%% is_pdt_like is semidet.
%
% If Is A Pdt Like.
%
is_pdt_like:-thread_property(_,alias(pdt_console_server)).
is_pdt_like:-lmcache:thread_main(user,Goal),!,Goal \= main.


%=

%% is_main_thread is semidet.
%
% If Is A Main Thread.
%
is_main_thread:-lmcache:thread_main(user,Goal),!,thread_self(Goal).
is_main_thread:-thread_self_main,!.

:- thread_local(tlbugger:no_colors/0).
:- thread_local(t_l:thread_local_error_stream/1).
:- volatile(t_l:thread_local_error_stream/1).

:- is_pdt_like-> assert(tlbugger:no_colors); true.


% = :- meta_predicate(with_main_error_to_output(0)).

%=

%% with_main_error_to_output( :Goal) is semidet.
%
% Using Main Error Converted To Output.
%
with_main_error_to_output(Goal):-
 current_output(Out),
  locally_tl(thread_local_error_stream(Out),Goal).


with_current_io(Goal):-
  current_input(IN),current_output(OUT),get_thread_current_error(Err),
  scce_orig(set_prolog_IO(IN,OUT,Err),Goal,set_prolog_IO(IN,OUT,Err)).

:- thread_local(t_l:hide_dmsg/0).

% with_no_output(Goal):- !, Goal.
with_no_output(Goal):-
 locally_tl(hide_dmsg,
  with_output_to(string(_),
     with_some_output_to(main_error,
      current_output,
       with_user_error_to(current_output,
        locally_tl(thread_local_error_stream(current_output),
         with_user_output_to(current_output,
          with_dmsg_to_main(Goal))))))).

with_user_error_to(Where,Goal):-
  with_some_output_to(user_error,Where,Goal).

with_user_output_to(Where,Goal):-
  with_some_output_to(user_output,Where,Goal).

into_stream_alias(Some,Alias):- into_stream(Some,S),stream_property(S,alias(Alias)),!.
into_stream_alias(Some,Some).

with_some_output_to(SomeAlias,Where,Goal):-
   into_stream_alias(SomeAlias,Alias),
   with_alias_output_to(Alias,Where,Goal).

with_alias_output_to(Alias,Where,Goal):-
   stream_property(ErrWas,alias(Alias)),
   new_memory_file(MF), open_memory_file(MF,write,Stream),
   scce_orig(set_stream(Stream,alias(Alias)),Goal,set_stream(ErrWas,alias(Alias))),
   close(Stream),
   memory_file_to_string(MF,Data),
   with_output_to(Where,write(Data)).



with_dmsg_to_main(Goal):-
  get_main_error_stream(Err),
   locally_tl(thread_local_error_stream(Err),Goal).

with_error_to_main(Goal):-
  get_main_error_stream(Err),current_error_stream_ucatch(ErrWas),Err==ErrWas,!,Goal.
with_error_to_main(Goal):-
  get_main_error_stream(Err),current_error_stream_ucatch(ErrWas),
   locally_tl(thread_local_error_stream(Err),
   scce_orig(set_stream(Err,alias(user_error)),Goal,set_stream(ErrWas,alias(user_error)))).




%% set_thread_current_error(Id, ?Err) is det.
%
% Thread Current Error Stream.
%

set_thread_error_stream(Id,Err):-
   ( \+ atom(Err)->asserta_new(lmcache:thread_current_error_stream(Id,Err));true),
   (thread_self(Id)->asserta(t_l:thread_local_error_stream(Err));true).


%% get_thread_current_error( ?Err) is det.
%
% Thread Current Error Stream.
%
:- export(get_thread_current_error/1).
get_thread_current_error(Err):- t_l:thread_local_error_stream(Err),!.
get_thread_current_error(Err):- thread_self(ID),lmcache:thread_current_error_stream(ID,Err),!.
get_thread_current_error(Err):- !,Err=user_error.
get_thread_current_error(Err):- stream_property(user_error,file_no(F)),\+ stream_property(main_error,file_no(F)),!,Err=user_error.
get_thread_current_error(Err):- get_thread_current_error0(Err),!.
get_thread_current_error(Err):- get_main_error_stream(Err),!.

get_thread_current_error0(Err):- get_thread_user_error1(Err),stream_property(Err,file_no(FileNo)),FileNo>2,!.
get_thread_current_error0(Err):- get_thread_user_error1(Err),!.

get_thread_user_error1(Err):- get_thread_user_error2(user_error,Err).
% get_thread_user_error1(Err):- get_thread_user_error2(Err,Err).
get_thread_user_error1(Err):- get_thread_user_error2(current_error,Err).

get_thread_user_error2(ErrName,Err):- nonvar(ErrName),
   stream_property(ErrName,file_no(FileNo)),
   stream_property(ErrName,output),FileNo\==2,
   current_output(Out),stream_property(Out,file_no(FileNo)),
   stream_property(Err,file_no(FileNo)),\+ current_input(Err).
get_thread_user_error2(ErrName,Err):-
   current_output(Out),stream_property(Out,file_no(FileNo)),
   stream_property(Err,file_no(FileNo)),
   stream_property(Err,output),FileNo\==2,
   ignore((stream_property(Err,alias(ErrName)))),ignore((Err=ErrName)).
get_thread_user_error2(ErrName,Err):- nonvar(ErrName), stream_property(Err,alias(ErrName)),stream_property(Err,output),!.



%% get_main_error_stream( ?Err) is det.
%
% Current Main Error Stream.
%
get_main_error_stream(Err):- stream_property(Err,alias(main_error)),!.
get_main_error_stream(Err):- lmcache:thread_main(user,ID),lmcache:thread_current_error_stream(ID,Err),!.
get_main_error_stream(Err):- stream_property(Err,file_no(2)),!.
get_main_error_stream(Err):- stream_property(Err,alias(user_error)),!.
get_main_error_stream(Err):- thread_call_blocking_one(main,get_thread_current_error(Err)).

thread_call_blocking_one(Thread,G):- thread_self(Self),
  thread_signal(Thread,
   catch(( (G,deterministic(YN),true)
    -> thread_send_message(Self,thread_call_blocking_one(Thread,G,fail,true))
     ; thread_send_message(Self,thread_call_blocking_one(Thread,G,true,YN))),
     E,thread_send_message(Self,thread_call_blocking_one(Thread,G,throw(E),true)))),
   thread_get_message(thread_call_blocking_one(Thread,G,TF,_R)),!,call(TF).


%=

%% format_to_error( ?F, ?A) is semidet.
%
% Format Converted To Error.
%
format_to_error(F,A):-F==error,!,format_to_error('~q',A).
format_to_error(F,A):- \+ is_list(A),!,format_to_error(F,[A]).
format_to_error(F,A):-get_thread_current_error(Err),!,format(Err,F,A).

%=

%% fresh_line_to_err is semidet.
%
% Fresh Line Converted To Err.
%
fresh_line_to_err:- zotrace((flush_output_safe,get_thread_current_error(Err),format(Err,'~N',[]),flush_output_safe(Err))).

:- dynamic(lmcache:thread_current_input/2).
:- volatile(lmcache:thread_current_input/2).

:- dynamic(lmcache:thread_current_error_stream/2).
:- volatile(lmcache:thread_current_error_stream/2).

%=

%% save_streams is semidet.
%
% Save Streams.
%
save_streams:- thread_self(ID),save_streams(ID),!.

set_mains:-
       stream_property(In, alias(user_input)),set_stream(In,alias(main_input)),
       stream_property(Out, alias(user_output)),set_stream(Out,alias(main_output)),
       find_main_eror(Err),set_stream(Err,alias(main_error)),
       set_stream(Err,alias(current_error)),set_stream(Err, alias(user_error)).

find_main_eror(Err):-stream_property(Err, file_no(2)).
find_main_eror(Err):-stream_property(Err, alias(user_error)).
find_main_eror(Err):-stream_property(Err, alias(main_error)).
find_main_eror(Err):-stream_property(Err, alias(current_error)).
find_main_eror(user_error).

set_main_error:- thread_self_main->set_mains;true.


current_error_stream_ucatch(Err):-
  stream_property(Err,alias(current_error))-> true;  % when we set it
  stream_property(Err,alias(user_error)) -> true;
  stream_property(Err,file_no(2)).


:- autoload(library(backcomp),[thread_at_exit/1]).

%% save_streams( ?ID) is semidet.
%
% Save Streams.
%
%save_streams(_):-!.
save_streams(ID):-
  retractall((lmcache:thread_current_input(ID,_))),
  retractall((lmcache:thread_current_error_stream(ID,_))),
  current_input(In),asserta(lmcache:thread_current_input(ID,In)),
  thread_at_exit(retractall((lmcache:thread_current_input(ID,_)))),
  thread_at_exit(retractall((lmcache:thread_current_error_stream(ID,_)))),
  (stream_property(Err, alias(user_error));current_error_stream_ucatch(Err)),
              asserta(lmcache:thread_current_error_stream(ID,Err)).


:- meta_predicate(with_main_input(0)).

%% with_main_input( :Goal) is semidet.
%
% Using Main Input.
%
with_main_input(Goal):-
    current_output(OutPrev),current_input(InPrev),stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),lmcache:thread_current_input(ID,In),lmcache:thread_current_error_stream(ID,Err),
    scce_orig(set_prolog_IO(In,OutPrev,Err),Goal,set_prolog_IO(InPrev,OutPrev,ErrPrev)).


%=

%% with_main_io( :Goal) is semidet.
%
% Using Main Input/output.
%
 with_main_io(Goal):-
    current_output(OutPrev),
    current_input(InPrev),
    stream_property(ErrPrev,alias(user_error)),
    lmcache:thread_main(user,ID),
     lmcache:thread_current_input(ID,In),
       lmcache:thread_current_error_stream(ID,Err),
    scce_orig(set_prolog_IO(In,Err,Err),Goal,set_prolog_IO(InPrev,OutPrev,ErrPrev)).


% bugger_debug=never turns off just debugging about the debugger
% dmsg_level=never turns off all the rest of debugging
% ddmsg(_):-current_prolog_flag(bugger_debug,false),!.
% ddmsg(D):- current_predicate(_:wdmsg/1),wdmsg(D),!.

%=

%% ddmsg( ?D) is semidet.
%
% Ddmsg.
%
ddmsg(D):- ddmsg("~N~q~n",[D]).
%ddmsg(F,A):- current_predicate(_:wdmsg/2),wdmsg(F,A),!.

%=

%% ddmsg( ?F, ?A) is semidet.
%
% Ddmsg.
%
ddmsg(F,A):- format_to_error(F,A),!.

%=

%% ddmsg_call( :GoalD) is semidet.
%
% Ddmsg Call.
%
ddmsg_call(D):- ( (ddmsg(ddmsg_call(D)),call(D),ddmsg(ddmsg_exit(D))) *-> true ; ddmsg(ddmsg_failed(D))).
:- module_transparent(ddmsg_call/1).



%% doall_and_fail( :Goal) is semidet.
%
% Doall And Fail.
%
doall_and_fail(Call):- time_call(once(doall(Call))),fail.
:- module_transparent(doall_and_fail/1).


quietly_must(G):- quietly(must(G)).
:- module_transparent(quietly_must/1).

:- module_transparent((if_defined/1,if_defined/2)).

%% if_defined( ?G) is semidet.
%
% If Defined.
%
:- module_transparent(if_defined/1).
:- export(if_defined/1).
if_defined(Goal):- if_defined(Goal,((dmsg(warn_undefined(Goal)),!,fail))).

%% if_defined( ?Goal, :GoalElse) is semidet.
%
% If Defined Else.
%
:- module_transparent(if_defined/2).
:- export(if_defined/2).
if_defined((A,B),Else):-!, if_defined(A,Else),if_defined(B,Else).
if_defined(Goal,Else):- current_predicate(_,Goal)*->Goal;Else.
% if_defined(M:Goal,Else):- !, current_predicate(_,OM:Goal),!,OM:Goal;Else.
%if_defined(Goal,  Else):- current_predicate(_,OM:Goal)->OM:Goal;Else.





:- meta_predicate when_defined(:).
:- export(when_defined/1).

%=

%% when_defined( ?Goal) is semidet.
%
% When Defined.
%
when_defined(Goal):-if_defined(Goal,true).

:- if(current_predicate(run_sanity_tests/0)).
:- listing(lmcache:thread_current_error_stream/2).
:- endif.

% = :- meta_predicate(to_mpi_matcher(?,?)).

%=

%% to_mpi_matcher( ?P, ?M) is semidet.
%
% Converted To Predicate Indicator.
%
context_modulez(M):-nonvar(M),!.
context_modulez(V):-context_module(M),visible_import_module(M,V).

visible_import_module(M,V):- M == any,!,current_module(V).
visible_import_module(M,V):- M == exact,!,context_module(V).
visible_import_module(M,V):- M == direct,!,context_module(C),import_module(C,V).
visible_import_module(M,V):- M == inherit,!,context_module(C),default_module(C,V).
visible_import_module(M,V):- M == V,!.
%visible_import_module(_,V):- V == baseKB.
visible_import_module(M,V):- \+ atom(M),!,V=M.
visible_import_module(M,V):- import_module(M,V).
visible_import_module(M,V):- default_module(M,V), M\==V, \+ import_module(M,V).


to_mpi_matcher(P,Matcher):-var(P),!,context_modulez(M),to_mpi_matcher(M:P,Matcher).
to_mpi_matcher(Name/Arity, Matcher) :- atom(Name),integer(Arity),functor(Head, Name, Arity),!,
 to_mpi_matcher(Head,Matcher).
to_mpi_matcher(M:P,M:P):- var(M),!,to_mpi_matcher(P,M:P).
%to_mpi_matcher(M:P,MP):- var(P),!,to_mpi_matcher(M:P,MP).

to_mpi_matcher(CFind,WPI):-
 strip_module(CFind,SC,Find),
 (CFind==Find -> C = any ; C = SC),
 locally(set_prolog_flag(runtime_debug,0),
   ((once(catch(match_predicates(CFind,Found),_,fail)),Found=[_|_],
    findall(WPI,
    ((member(M:F/A,Found),
      functor(PI,F,A),
     (predicate_property(M:PI,imported_from(W)) -> true ; W=M),
      visible_import_module(C,W),
      WPI = W:PI,
      \+ predicate_property(WPI,imported_from(_)))),
     Remaining)))),
     Remaining=[_|_],!,
     sort(Remaining,Set),
     member(WPI,Set).


%to_mpi_matcher(M:Find,MPI):-context_modulez(M),to_pi0(M,Find,MPI).
%to_mpi_matcher(M:PI, Head) :- !, to_mpi_matcher(PI, Head).
%to_mpi_matcher(Find,M:PI):-context_modulez(M),to_pi0(M,Find,M:PI).

to_pi0(M,Find,M:PI):- atom(Find),!,when(nonvar(PI),(nonvar(PI),functor(PI,Find,_))).
to_pi0(M,Find/A,M:PI):-var(Find),number(A),!,when(nonvar(PI),(nonvar(PI),functor(PI,_,A))).
to_pi0(M,Find,PI):-get_pi(Find,PI0),!,(PI0\=(_:_)->(context_modulez(M),PI=(M:PI0));PI=PI0).


%=

%% to_pi0( ?M, :TermFind, :TermPI) is semidet.
%
% Converted To Predicate Indicator Primary Helper.
%

:- thread_local(t_l:last_src_loc/2).

%=

%% input_key( ?K) is semidet.
%
% Input Key.
%
input_key(K):-thread_self(K).


%=

%% show_new_src_location( ?FL) is semidet.
%
% Show New Src Location.
%
show_new_src_location(FL):-input_key(K),!,show_new_src_location(K,FL).


%=

%% show_new_src_location( ?K, ?FL) is semidet.
%
% Show New Src Location.
%
show_new_src_location(_,F:_):-F==user_input,!.
show_new_src_location(K,FL):- t_l:last_src_loc(K,FL),!.
show_new_src_location(K,FL):- retractall(t_l:last_src_loc(K,_)),format_to_error('~N%~~ ~w ~n',[FL]),!,asserta(t_l:last_src_loc(K,FL)).


:- thread_local(t_l:current_why_source/1).


%=

%% sl_to_filename( ?W, ?W) is semidet.
%
% Sl Converted To Filename.
%
sl_to_filename(W,W):-atom(W),exists_file_safe(W).
sl_to_filename(W,W):-atom(W),!.
sl_to_filename(mfl4(_VarNameZ,_,F,_),F):-atom(F),!.
sl_to_filename(_:W,W):-atom(W),!.
sl_to_filename(W,W).
sl_to_filename(W,To):-nonvar(To),To=(W:_),atom(W),!.






%=

%% current_source_file( -CtxColonLinePos) is semidet.
%
% Current Source Location.
%
current_source_file(F:L):- current_source_location(W,L), sl_to_filename(W,F),!.
current_source_file(F):- F = unknown.


source_ctx(B:L):- must((current_source_file(F:L),file_base_name(F,B))).

%=

%% current_source_location0( -Ctx, -LinePos) is semidet.
%
% Current Source Location Primary Helper.
%
current_source_location(F,L):- notrace((clause(current_source_location0(F,L),Body),notrace(catch(Body,_,fail)))),!.

sub_why(Sub,Sub).
sub_why(Sub,Why):- compound(Why),Why=(A,B),(sub_why(Sub,A);sub_why(Sub,B)).

current_source_location0(F,L):- current_why_data(Data),sub_why(Sub,Data), \+ is_list(Data), compound(Sub),Sub=mfl4(_,_,F,L),!.
current_source_location0(F,why):- t_l:current_why_source(F).
current_source_location0(F,L):- source_location(F,L),!.
current_source_location0(F,L):- prolog_load_context(stream,S),line_or_char_count(S,L),stream_property(S,file_name(F)),!.
current_source_location0(F,L):- loading_file(F),stream_property(S,file_name(F)),line_or_char_count(S,L),!.
current_source_location0(F,L):- prolog_load_context(file,F),!,ignore((prolog_load_context(stream,S),!,line_or_char_count(S,L))),!.
current_source_location0(F,L):- loading_file(F),L= (-1).
current_source_location0(F,L):- current_input(S),stream_property(S,alias(F)),line_or_char_count(S,L).
current_source_location0(F,L):- current_filesource(F),ignore((prolog_load_context(stream,S),!,line_or_char_count(S,L))),!.
% current_source_location0(F,L):- prolog_load_context(file,F),current_input(S),line_position(S,L),!.
current_source_location0(M,module):- source_module(M),!.
current_source_location0(M,typein):- '$current_typein_module'(M).

line_or_char_count(S,_):- \+ is_stream(S),!,fail.
line_or_char_count(S,L):- on_x_fail((stream_property(S,position(P)),stream_position_data(line_count,P,L))),!.
line_or_char_count(S,L):- on_x_fail(line_count(S,L)),L\==0,!.
line_or_char_count(S,L):- on_x_fail(line_position(S,L)),L\==1,!.
line_or_char_count(S,L):- on_x_fail(character_count(S,C)),L is -C.

:-export(current_why/1).
:-module_transparent(current_why/1).

%=

%% current_why( ?Why) is semidet.
%
% Current Generation Of Proof.
%
current_why(Why):- current_why_data(Why).
current_why(mfl4(VarNameZ,M,F,L)):- notrace(current_mfl4(VarNameZ,M,F,L)).

current_mfl4(VarNameZ,M,F,L):- current_mfl(M,F,L),ignore(varnames_load_context(VarNameZ)).

current_mfl(M,F,L):-
 current_source_file(F:L),!,
 calc_source_module(M),
  ignore((var(L),L=module(M))).

calc_source_module(M):- source_module(M),clause_b(mtHybrid(M)),!.
calc_source_module(M):- clause_b(defaultAssertMt(M)),!.
calc_source_module(M):- source_module(M).

:- thread_local(t_l:current_why_source/1).

current_why_data(Why):- nb_current('$current_why',wp(Why,_P)).
current_why_data(Why):- t_l:current_why_source(Why).

varnames_load_context(VarNameZ):-
  prolog_load_context(variable_names,Vars),
  varnames_to_lazy_unifiable(Vars,VarNameZ).

varnames_to_lazy_unifiable(Vars,VarNameZ):- Vars==[],!,VarNameZ=_.
varnames_to_lazy_unifiable(Vars,VarNameZ):- nonvar(Vars) -> Vars=VarNameZ;
   freeze(Vars,can_maybe_varname(Vars,VarNameZ)).

can_maybe_varname(Vars1,Vars2):- ignore(Vars1=Vars2).


%% with_only_current_why( +Why, +:Prolog) is semidet.
%
% Restart and Save the Well-founded Semantic Reason while executing code.
%
with_only_current_why(Why,Prolog):-
  (nb_current('$current_why',WAS);WAS=[])->
   setup_call_cleanup(b_setval('$current_why',wp(Why,Prolog)),
    (call(Prolog),b_setval('$current_why',WAS)),
     b_setval('$current_why',WAS)).

%% with_current_why( +Why, +:Prolog) is semidet.
%
% Save Well-founded Semantic Reason recursively while executing code.
%
with_current_why(S,Call):-
  current_why(UU),
  ( ( \+ \+ s_in_why(S,UU)) -> Call;  with_only_current_why((S,UU),Call)).

s_in_why(S,UU):- \+ S \= UU,!.
%s_in_why(_,UU):- \+ compound(UU),!,fail.
%s_in_why(S,[U1|U2]):- !, (s_in_why(S,U1);s_in_why(S,U2)).
s_in_why(S,(U1,U2)):- !, (s_in_why(S,U1);s_in_why(S,U2)).
%s_in_why(S,UU):- sub_goal(U,UU),S=@=U,!.
%sub_goal(U,UU):- sub_term(E,UU),nonvar(E), U\==UU.

:- thread_initialization(nb_setval('$current_why',[])).

% source_module(M):-!,M=u.
:- export(source_module/1).
:- module_transparent(source_module/1).

%=

%% source_module( ?M) is semidet.
%
% Source Module.
%
source_module(M):- nonvar(M),!,source_module(M0),!,(M0=M).
source_module(M):- \+ source_location(_,_),!, '$current_typein_module'(M).
source_module(M):- '$current_source_module'(M),!.
source_module(M):- '$set_source_module'(M,M),!.
source_module(M):- strip_module(_,M,_).
source_module(M):- loading_module(M),!.

:- thread_local(t_l:last_source_file/1).
:- export(loading_file/1).

%=

%% loading_file( ?FIn) is semidet.
%
% Loading File.
%
loading_file(FIn):- (quietly((((source_file0(F) *-> (retractall(t_l:last_source_file(_)),asserta(t_l:last_source_file(F))) ; (fail,t_l:last_source_file(F)))),!,F=FIn))).

%=

%% source_file0( ?F) is semidet.
%
% Source File Primary Helper.
%
source_file0(F):-source_location(F,_).
source_file0(F):-prolog_load_context(file, F).
source_file0(F):-prolog_load_context(source, F).
source_file0(F):-seeing(S),is_stream(S),stream_property(S,file_name(F)),exists_file_safe(F).
source_file0(F):-prolog_load_context(stream, S),stream_property(S,file_name(F)),exists_file_safe(F).
source_file0(F):-findall(E,catch((stream_property( S,mode(read)),stream_property(S,file_name(E)),exists_file_safe(E),
  line_count(S,Goal),Goal>0),_,fail),L),last(L,F).


:-export(source_variables_l/1).

%=

%% source_variables_l( ?AllS) is semidet.
%
% Source Variables (list Version).
%
source_variables_l(AllS):-
 quietly((
  (prolog_load_context(variable_names,Vs1);Vs1=[]),
  (get_varname_list(Vs2);Vs2=[]),
  uexecute_goal_vs(Vs3),
  ignore(Vs3=[]),
  append([Vs1,Vs2,Vs3],All),list_to_set(All,AllS),
  set_varname_list(AllS))).

uexecute_goal_vs(Vs):- uexecute_goal_vs0(Vs),!.
uexecute_goal_vs([]).
uexecute_goal_vs0(Vs):- notrace(catch(ucatch_parent_goal('$toplevel':'$execute_goal2'(_,Vs,_)),_,fail)).
uexecute_goal_vs0(Vs):- notrace(catch(ucatch_parent_goal('$toplevel':'$execute_goal2'(_,Vs)),_,fail)).


%=




%% show_source_location is semidet.
%
% Show Source Location.
%
:-export( show_source_location/0).
show_source_location:- current_prolog_flag(dmsg_level,never),!.
%show_source_location:- quietly((tlbugger:no_slow_io)),!.
show_source_location:- get_source_location(FL)->show_new_src_location(FL),!.
show_source_location:- if_interactive((dumpST,dtrace)).

:- thread_local(t_l:last_shown_current_source_location/1).

show_current_source_location:- ignore((get_source_location(FL),!, show_current_source_location(FL))).
show_current_source_location(FL):- t_l:last_shown_current_source_location(FL),!,
                                   retractall(t_l:last_shown_current_source_location(_)),
                                   public_file_link(FL,FLO),
                                   format_to_error('~N%~~ FILE: ~w ~N',[FLO]),!.
show_current_source_location(FL):- retractall(t_l:last_shown_current_source_location(_)),
                                   asserta(t_l:last_shown_current_source_location(FL)),!,
                                   public_file_link(FL,FLO),
                                   format_to_error('~N%~~ FIlE: ~w ~N',[FLO]),!.

get_source_location(FL):- current_source_file(FL),nonvar(FL),!.
get_source_location(F:L):- source_location(F,L),!.
get_source_location(get_source_location_unknown).


% :- ensure_loaded(hook_database).

:- dynamic(lmconf:http_file_stem/2).
lmconf:http_file_stem('lib/swipl',"https://logicmoo.org:2082/gitlab/logicmoo/logicmoo_workspace/-/tree/master/docker/rootfs/usr/local/lib/swipl").
lmconf:http_file_stem('swi-prolog/pack',"https://logicmoo.org:2082/gitlab/logicmoo/logicmoo_workspace/-/edit/master/packs_sys").
lmconf:http_file_stem(logicmoo_workspace,"https://logicmoo.org:2082/gitlab/logicmoo/logicmoo_workspace/-/edit/master").
lmconf:http_file_stem('~',"https://logicmoo.org:2082/gitlab/logicmoo/prologmud_server/-/tree/master").

:- export(ensure_compute_file_link/2).
:- export(public_file_link/2).
:- export(maybe_compute_file_link/2).

ensure_compute_file_link(S,URL):- \+ ( nb_current('$inprint_message', Messages), Messages\==[] ), maybe_compute_file_link(S,URL),!.
ensure_compute_file_link(S,S).

maybe_compute_file_link(_,_):- !, fail.
maybe_compute_file_link(S,O):- atom(S),!, lmconf:http_file_stem(F,R),atomic_list_concat([_,A],F,S),!,atom_concat(R,A,O).
maybe_compute_file_link(S:L,O):- integer(L),!,maybe_compute_file_link(S,F),format(atom(O),'~w#L~w',[F,L]).

public_file_link(MG,MG):-!.
public_file_link(S,O):-   \+ ( nb_current('$inprint_message', Messages), Messages\==[] ), maybe_compute_file_link(S,M),into_link(S,M,O).
public_file_link(MG,MG).

into_link(_,M,O):- format(atom(O),'* ~w ',[M]),!.
into_link(S,M,O):- format(atom(O),'<a href="~w"><pre>~q</pre></a>',[M,S]).

:-export( as_clause_no_m/3).

%=

%% as_clause_no_m( ?MHB, ?H, ?B) is semidet.
%
% Converted To Clause No Module.
%
as_clause_no_m( MHB,  H, B):- strip_module(MHB,_M,HB), expand_to_hb( HB,  MH, MB),strip_module(MH,_M2H,H),strip_module(MB,_M2B,B).

%=

%% as_clause_w_m( ?MHB, ?M, ?H, ?B) is semidet.
%
% Converted To Clause W Module.
%
as_clause_w_m(MHB, M, H, B):-  as_clause_w_m(MHB, M1H, H, B, M2B), (M1H==user->M2B=M;M1H=M).

%=

%% as_clause_w_m( ?MHB, ?M1H, ?H, ?B, ?M2B) is semidet.
%
% Converted To Clause W Module.
%
as_clause_w_m(MHB, M1H, H, B, M2B):-  expand_to_hb( MHB,  MH, MB),strip_module(MH,M1H,H),strip_module(MB,M2B,B).

:- export(is_ftCompound/1).

%% is_ftNameArity(+F,+A) is semidet.
%
% If Is A Format Type of a Compound specifier
%
is_ftNameArity(F,A):-integer(A), atom(F), (F \= (/)),A>=0.

%% is_ftCompound( ?Goal) is semidet.
%
% If Is A Format Type Compound.
%
is_ftCompound(Goal):-compound(Goal),\+ is_ftVar(Goal).

%% not_ftCompound( ?InOut) is semidet.
%
% Not Compound.
%
not_ftCompound(A):- \+ is_ftCompound(A).

:- export(is_ftVar/1).
:- export(is_ftVar0/1).

%% is_ftVar( :TermV) is semidet.
%
% If Is A Format Type Variable.
%
is_ftVar(V):- notrace(is_ftVar0(V)).
is_ftVar0(V):- \+ compound(V),!,var(V).
is_ftVar0('$VAR'(_)).
is_ftVar0('aVar'(_,_)).
%:- mpred_trace_nochilds(is_ftVar/1).


% quotedDefnIff
/*
:- dynamic(baseKB:ftVar/1).
baseKB:ftVar(X):- ucatch:is_ftVar(X).
:- export(baseKB:ftVar/1).
:- system:import(baseKB:ftVar/1).

:- dynamic(baseKB:ftCompound/1).
baseKB:ftCompound(X):- ucatch:is_ftCompound(X).
:- export(baseKB:ftCompound/1).
:- system:import(baseKB:ftCompound/1).
:- baseKB:export(baseKB:ftCompound/1).

:- dynamic(baseKB:ftNonvar/1).
baseKB:ftNonvar(X):- ucatch:is_ftNonvar(X).
:- export(baseKB:ftNonvar/1).
:- system:import(baseKB:ftNonvar/1).
*/
%=

%% is_ftNonvar( ?V) is semidet.
%
% If Is A Format Type Nonvar.
%
is_ftNonvar(V):- \+ is_ftVar(V).


%================================================================
% maplist/[2,3]
% this must succeed  maplist_safe(=,[Goal,Goal,Goal],[1,2,3]).
% well if its not "maplist" what shall we call it?
%================================================================
% so far only the findall version works .. the other runs out of local stack!?

:- export((   maplist_safe/2,
   maplist_safe/3)).


%=

%% maplist_safe( ?Pred, ?LIST) is semidet.
%
% Maplist Safely Paying Attention To Corner Cases.
%
maplist_safe(_Pred,[]):-!.
maplist_safe(Pred,LIST):-findall(E,(member(E,LIST), on_f_debug(apply(Pred,[E]))),LISTO),!, ignore(LIST=LISTO),!.
% though this should been fine %  maplist_safe(Pred,[A|B]):- copy_term(Pred+A, Pred0+A0), on_f_debug(once(call(Pred0,A0))),     maplist_safe(Pred,B),!.


%=

%% maplist_safe( ?Pred, ?LISTIN, ?LIST) is semidet.
%
% Maplist Safely Paying Attention To Corner Cases.
%
maplist_safe(_Pred,[],[]):-!.
maplist_safe(Pred,LISTIN, LIST):-!, findall(EE, ((member(E,LISTIN),on_f_debug(apply(Pred,[E,EE])))), LISTO),  ignore(LIST=LISTO),!.
% though this should been fine % maplist_safe(Pred,[A|B],OUT):- copy_term(Pred+A, Pred0+A0), debugOnFailureEach(once(call(Pred0,A0,AA))),  maplist_safe(Pred,B,BB), !, ignore(OUT=[AA|BB]).



:- export(bad_functor/1).

%=

%% bad_functor( ?L) is semidet.
%
% Bad Functor.
%
bad_functor(L) :- arg(_,v('|',[],':','/'),L). % .

:- export(warn_bad_functor/1).

%=

%% warn_bad_functor( ?L) is semidet.
%
% Warn Bad Functor.
%
warn_bad_functor(L):-ignore((notrace(bad_functor(L)),!,dtrace,call(ddmsg(bad_functor(L))),break)).

:- export(strip_f_module/2).

%=

%% strip_f_module( ?P, ?PA) is semidet.
%
% Strip Functor Module.
%
strip_f_module(_:P,FA):-nonvar(P),!,strip_f_module(P,F),!,F=FA.
strip_f_module(P,PA):-atom(P),!,P=PA.

strip_f_module(P,FA):- is_list(P),catch(text_to_string(P,S),_,fail),!,maybe_notrace(atom_string(F,S)),!,F=FA.
strip_f_module(P,FA):- quietly(string(P);atomic(P)), maybe_notrace(atom_string(F,P)),!,F=FA.
strip_f_module(P,P).

% use catchv/3 to replace catch/3 works around SWI specific issues arround using $abort/0 and block/3
% (catch/3 allows you to have these exceptions bubble up past your catch block handlers)
% = :- meta_predicate((catchv(0, ?, 0))).
% = :- meta_predicate((catchv(0, ?, 0))).
:- export((catchv/3)).


%! catchv( :Goal, ?E, :GoalRecovery) is nondet.
%
%  Like catch/3 but rethrows block/2 and $abort/0.
%
catchv(Goal,E,Recovery):-
   nonvar(E)
   -> catch(Goal,E,Recovery); % normal mode (the user knows what they want)
   catch(Goal,E,(rethrow_bubbled(E),Recovery)). % prevents promiscous mode

:-export(catchv/3).
:-system:import(catchv/3).


%! bubbled_ex( ?Ex) is det.
%
% Bubbled Exception.
%
bubbled_ex('$aborted').
bubbled_ex('time_limit_exceeded').
bubbled_ex('$time_limit_exceeded').
bubbled_ex(block(_,_)).


%! rethrow_bubbled( ?E) is det.
%
% Bubbled Exception Check.
%
rethrow_bubbled(E):- ( \+ bubbled_ex(E)),!.
rethrow_bubbled(E):-throw(E).



:- export(functor_catch/3).

%=

%% functor_catch( ?P, ?F, ?A) is semidet.
%
% Functor Catch.
%
functor_catch(P,F,A):- catchv(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_catch(F,F,0):-atomic(F),!.
% functor_catch(P,F,A):-catchv(compound_name_arity(P,F,A),E,(ddmsg(E:functor(P,F,A)),dtrace)).


:- export(functor_safe/3).

%=

%% functor_safe( ?P, ?F, ?A) is semidet.
%
% Functor Safely Paying Attention To Corner Cases.
%
functor_safe(P,F,A):- (compound(P)->compound_name_arity(P,F,A);functor(P,F,A)),sanity(warn_bad_functor(F)).
% functor_safe(P,F,A):- catchv(functor(P,F,A),_,compound_name_arity(P,F,A)).
% functor_safe(P,F,A):- catchv(compound_name_arity(P,F,A),_,functor(P,F,A)).
/*
% functor_safe(P,F,A):-var(P),A==0,compound_name_arguments(P,F,[]),!.
functor_safe(P,F,A):-var(P),A==0,!,P=F,!.
functor_safe(P,F,A):-functor_safe0(P,F,A),!.
functor_safe0(M:P,M:F,A):-var(P),atom(M),functor_catch(P,F,A),!,warn_bad_functor(F).
functor_safe0(P,F,A):-var(P),strip_f_module(F,F0),functor_catch(P,F0,A),!,warn_bad_functor(F).
functor_safe0(P,F,0):- quietly(string(P);atomic(P)), maybe_notrace(atom_string(F,P)),warn_bad_functor(F).
functor_safe_compound((_,_),',',2).
functor_safe_compound([_|_],'.',2).
functor_safe_compound(_:P,F,A):- functor_catch(P,F,A),!.
functor_safe_compound(P,F,A):- functor_catch(P,F,A).
functor_safe_compound(P,F,A):- var(F),strip_f_module(P,P0),!,functor_catch(P0,F0,A),strip_f_module(F0,F),!.
functor_safe_compound(P,F,A):- strip_f_module(P,P0),strip_f_module(F,F0),!,functor_catch(P0,F0,A).
*/

% block3(test, (repeat, !(test), fail))).
:- meta_predicate block3(+, :, ?).

%=

%% block3( +Name, ?Goal, ?Var) is semidet.
%
% Block.
%
block3(Name, Goal, Var) :- Goal, keep(Name, Var).   % avoid last-call and GC

%=

%% keep( ?VALUE1, ?VALUE2) is semidet.
%
% Keep.
%
keep(_, _).

%=

%% set_block_exit( ?Name, ?Value) is semidet.
%
% Set Block Exit.
%
set_block_exit(Name, Value) :-  prolog_current_frame(Frame),  prolog_frame_attribute(Frame, parent_goal,  mcall:block3(Name, _, Value)).


:- export(ucatch_parent_goal/1).
ucatch_parent_goal(M:Goal):-
  prolog_current_frame(F),
  prolog_frame_attribute(F, parent, FP),
  prolog_frame_attribute(FP, parent_goal, M:Goal).
%=

%% block( ?Name, ?Goal) is semidet.
%
% Block.
%
block(Name, Goal) :-  block3(Name, Goal, Var),  (   Var == !  ->  !  ;   true  ).

%=

%% !( ?Name) is semidet.
%
% !.
%
!(Name) :- set_block_exit(Name, !).

:- export((block3/3,
            set_block_exit/2,
            (block)/2,
            !/1 )).

:- dynamic(buggerFile/1).
:- abolish(buggerFile/1),prolog_load_context(source,D),asserta(buggerFile(D)).


% hasLibrarySupport :- absolute_file_name('logicmoo_util_library.pl',File),exists_file_safe(File).


%=

%% throwNoLib is semidet.
%
% Throw No Lib.
%
throwNoLib:- dtrace,absolute_file_name('.',Here), buggerFile(BuggerFile), listing(user:library_directory), trace_or_throw(error(existence_error(url, BuggerFile), context(_, status(404, [BuggerFile, from( Here) ])))).

:- dynamic(buggerDir/1).
:- abolish(buggerDir/1),prolog_load_context(directory,D),asserta(buggerDir(D)).


%=

%% addLibraryDir is semidet.
%
% Add Library Dir.
%
addLibraryDir :- buggerDir(Here),atom_concat(Here,'/..',UpOne), absolute_file_name(UpOne,AUpOne),asserta(user:library_directory(AUpOne)).

% if not has library suport, add this direcotry as a library directory
% :-not(hasLibrarySupport) -> addLibraryDir ; true .

% :-hasLibrarySupport->true;throwNoLib.





%=

%% ib_multi_transparent33( ?MT) is semidet.
%
% Ib Multi Transparent33.
%
ib_multi_transparent33(MT):-multifile(MT),module_transparent(MT),dynamic_safe(MT).


%=

%% dif_safe( ?Agent, ?Obj) is semidet.
%
% Dif Safely Paying Attention To Corner Cases.
%
dif_safe(Agent,Obj):- (var(Agent);var(Obj)),!.
dif_safe(Agent,Obj):- Agent\==Obj.

% hide Pred from tracing

%=

%% to_m_f_arity_pi( ?Term, ?M, ?F, ?A, ?PI) is semidet.
%
% Converted To Module Functor Arity Predicate Indicator.
%
to_m_f_arity_pi(M:Plain,M,F,A,PI):-!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(Term,M,F,A,PI):- strip_module(Term,M,Plain),Plain\==Term,!,to_m_f_arity_pi(Plain,M,F,A,PI).
to_m_f_arity_pi(F/A,_M,F,A,PI):-functor_safe(PI,F,A),!.
to_m_f_arity_pi(PI,_M,F,A,PI):-functor_safe(PI,F,A).


%=

%% with_preds( ?H, ?M, ?F, ?A, ?PI, :Goal) is semidet.
%
% Using Predicates.
%
with_preds((H,Y),M,F,A,PI,Goal):-!,with_preds(H,M,F,A,PI,Goal),with_preds(Y,M,F,A,PI,Goal).
with_preds([H],M,F,A,PI,Goal):-!,with_preds(H,M,F,A,PI,Goal).
with_preds([H|Y],M,F,A,PI,Goal):-!,with_preds(H,M,F,A,PI,Goal),with_preds(Y,M,F,A,PI,Goal).
with_preds(M:H,_M,F,A,PI,Goal):-!, with_preds(H,M,F,A,PI,Goal).
with_preds(H,M,F,A,PI,Goal):-forall(to_m_f_arity_pi(H,M,F,A,PI),Goal).



% ===================================================================
% Substitution based on ==
% ===================================================================
% Usage: dbgsubst(+Fml,+Goal,+Sk,?FmlSk)

:- export(dbgsubst/4).

%=

%% dbgsubst( ?A, ?B, ?Goal, ?A) is semidet.
%
% Dbgsubst.
%
dbgsubst(A,B,Goal,A):- B==Goal,!.
dbgsubst(A,B,Goal,D):-var(A),!,ddmsg(dbgsubst(A,B,Goal,D)),dumpST,dtrace(dbgsubst0(A,B,Goal,D)).
dbgsubst(A,B,Goal,D):-dbgsubst0(A,B,Goal,D).


%=

%% dbgsubst0( ?A, ?B, ?Goal, ?D) is semidet.
%
% Dbgsubst Primary Helper.
%
dbgsubst0(A,B,Goal,D):-
      catchv(quietly(nd_dbgsubst(A,B,Goal,D)),E,(dumpST,ddmsg(E:nd_dbgsubst(A,B,Goal,D)),fail)),!.
dbgsubst0(A,_B,_C,A).


%=

%% nd_dbgsubst( ?Var, ?VarS, ?SUB, ?SUB) is semidet.
%
% Nd Dbgsubst.
%
nd_dbgsubst(  Var, VarS,SUB,SUB ) :- Var==VarS,!.
nd_dbgsubst(  P, Goal,Sk, P1 ) :- functor_safe(P,_,N),nd_dbgsubst1( Goal, Sk, P, N, P1 ).

univ_safe_2(A,B):- compound(A),compound_name_arity(A,F,0),!,F=..B.
univ_safe_2(A,B):- A=..B.


%=

%% nd_dbgsubst1( ?Goal, ?Sk, ?P, ?N, ?P1) is semidet.
%
% Nd Dbgsubst Secondary Helper.
%
nd_dbgsubst1( _,  _, P, 0, P  ).
nd_dbgsubst1( Goal, Sk, P, N, P1 ) :- N > 0,univ_safe_2( P, [F|Args]),
            nd_dbgsubst2( Goal, Sk, Args, ArgS ),
            nd_dbgsubst2( Goal, Sk, [F], [FS] ),
            univ_safe_2(P1 , [FS|ArgS]).


%=

%% nd_dbgsubst2( ?X, ?Sk, ?L, ?L) is semidet.
%
% Nd Dbgsubst Extended Helper.
%
nd_dbgsubst2( _,  _, [], [] ).
nd_dbgsubst2( Goal, Sk, [A|As], [Sk|AS] ) :- Goal == A, !, nd_dbgsubst2( Goal, Sk, As, AS).
nd_dbgsubst2( Goal, Sk, [A|As], [A|AS]  ) :- var(A), !, nd_dbgsubst2( Goal, Sk, As, AS).
nd_dbgsubst2( Goal, Sk, [A|As], [Ap|AS] ) :- nd_dbgsubst( A,Goal,Sk,Ap ),nd_dbgsubst2( Goal, Sk, As, AS).
nd_dbgsubst2( _X, _Sk, L, L ).



%=========================================
% Module Utils
%=========================================

%=

%% module_functor( ?PredImpl, ?Module, ?Pred, ?Arity) is semidet.
%
% Module Functor.
%
module_functor(PredImpl,Module,Pred,Arity):-strip_module(PredImpl,Module,NewPredImpl),strip_arity(NewPredImpl,Pred,Arity).


%=

%% strip_arity( ?PredImpl, ?Pred, ?Arity) is semidet.
%
% Strip Arity.
%
strip_arity(Pred/Arity,Pred,Arity).
strip_arity(PredImpl,Pred,Arity):-functor_safe(PredImpl,Pred,Arity).

/*

debug(+Topic, +Format, +Arguments)
Prints a message using format(Format, Arguments) if Topic unies with a topic
enabled with debug/1.
debug/nodebug(+Topic [>le])
Enables/disables messages for which Topic unies. If >le is added, the debug
messages are appended to the given le.
assertion(:Goal)
Assumes that Goal is true. Prints a stack-dump and traps to the debugger otherwise.
This facility is derived from the assert() macro as used in Goal, renamed
for obvious reasons.
*/
:- meta_predicate with_preds(?,?,?,?,?,0).



%set_prolog_flag(N,V):-!,nop(set_prolog_flag(N,V)).


% have to load this module here so we dont take ownership of prolog_exception_hook/4.
:- set_prolog_flag(generate_debug_info, true).
% have to load this module here so we dont take ownership of prolog_exception_hook/4.

% :- ensure_loaded(library(backcomp)).
:- ensure_loaded(library(ansi_term)).
:- ensure_loaded(library(check)).
:- ensure_loaded(library(debug)).
:- ensure_loaded(library(lists)).
:- ensure_loaded(library(make)).
:- ensure_loaded(library(system)).
:- ensure_loaded(library(apply)).

:- thread_local(t_l:session_id/1).
:- multifile(t_l:session_id/1).

:- thread_local(tlbugger:no_colors/0).


% =========================================================================


%=

%% trace_or_throw( ?E) is semidet.
%
%  Trace or throw.
%
:- export(trace_or_throw/1).
trace_or_throw(E):- hide_non_user_console,quietly((thread_self(Self),wdmsg(thread_trace_or_throw(Self+E)),!,throw(abort),
                    thread_exit(trace_or_throw(E)))).


trace_or_throw(E):- wdmsg(trace_or_throw(E)),if_interactive((trace,break),true),dtrace((dtrace,throw(E))).

 %:-interactor.


% false = hide this wrapper

%=

%% showHiddens is semidet.
%
% Show Hiddens.
%
showHiddens:-true.

:- meta_predicate on_x_log_fail(0).
:- export(on_x_log_fail/1).

%=

%% on_x_log_fail( :Goal) is semidet.
%
% If there If Is A an exception in  :Goal goal then log fail.
%
on_x_log_fail(Goal):- catchv(Goal,E,(dmsg(E:Goal),fail)).

on_xf_log_cont(Goal):- (on_x_log_cont(Goal)*->true;dmsg(on_f_log_cont(Goal))).

on_xf_log_cont_l(Goal):- call_each_det(on_xf_log_cont,Goal).

% -- CODEBLOCK

:- export(on_x_log_throw/1).
:- export(on_x_log_cont/1).

%=

%% on_x_log_throw( :Goal) is semidet.
%
% If there If Is A an exception in  :Goal goal then log throw.
%
on_x_log_throw(Goal):- catchv(Goal,E,(ddmsg(on_x_log_throw(E,Goal)),throw(E))).
%on_x_log_throwEach(Goal):-with_each(1,on_x_log_throw,Goal).

%=

%% on_x_log_cont( :Goal) is semidet.
%
% If there If Is A an exception in  :Goal goal then log cont.
%
on_x_log_cont(Goal):- catchv( (Goal*->true;ddmsg(failed_on_x_log_cont(Goal))),E,ddmsg(E:Goal)).

:- thread_local( tlbugger:skipMust/0).
%MAIN tlbugger:skipMust.


:- export(errx/0).

%=

%% errx is semidet.
%
% Errx.
%
errx:-on_x_debug((ain(tlbugger:dont_skip_bugger),do_gc,dumpST(10))),!.

:- thread_local(tlbugger:rtracing/0).



/*

A value 0 means that the corresponding quality is totally unimportant, and 3 that the quality is extremely important;
1 and 2 are intermediate values, with 1 the neutral value. (quality 3) can be abbreviated to quality.

*/
compute_q_value(N,N):- number(N),!.
compute_q_value(false,0).
compute_q_value(fail,0).
compute_q_value(never,0).
compute_q_value(neutral,1).
compute_q_value(true,2).
compute_q_value(quality,3).
compute_q_value(Flag,Value):-current_prolog_flag(Flag,M),!,compute_q_value(M,Value).
compute_q_value(N,1):- atom(N).
compute_q_value(N,V):- compound(N), catch(V is N,_,fail).

/*

Name                        Meaning
---------------------       --------------------------------
logicmoo_compilation_speed  speed of the compilation process

runtime_debug              ease of debugging
logicmoo_space              both code size and run-time space

runtime_safety             run-time error checking
runtime_speed              speed of the object code

unsafe_speedups      speed up that are possibily

*/
flag_call(FlagHowValue):-zotrace(flag_call0(FlagHowValue)).
flag_call0(Flag = Quality):- compute_q_value(Quality,Value),!, set_prolog_flag(Flag,Value).
flag_call0(Flag == Quality):-
  current_prolog_flag(Flag,Current),!,
  compute_q_value(Quality,QValue),
  compute_q_value(Current,CValue),
  QValue==CValue,!.
flag_call0(FlagHowValue):- univ_safe_2(FlagHowValue,[How,Flag,Value]),
    current_prolog_flag(Flag,QVal),compute_q_value(Value,VValue),!,call(How,QVal,VValue).



%=

%% skipWrapper is semidet.
%
% Skip Wrapper.
%

% false = use this wrapper, true = code is good and avoid using this wrapper
:- export(skipWrapper/0).

% skipWrapper:-!.
skipWrapper:- zotrace((bugger:skipWrapper0)).
% skipWrapper:- tracing,!.

skipWrapper0:- current_prolog_flag(bugger,false),!.
skipWrapper0:- tracing, \+ tlbugger:rtracing,!.
skipWrapper0:- tlbugger:dont_skip_bugger,!,fail.
%skipWrapper0:- flag_call(runtime_debug true) ,!,fail.
%skipWrapper0:- current_prolog_flag(unsafe_speedups , true) ,!.
skipWrapper0:- tlbugger:skip_bugger,!.
%skipWrapper0:- is_release,!.
%skipWrapper0:- 1 is random(5),!.
%skipWrapper0:- tlbugger:skipMust,!.

:- '$hide'(skipWrapper/0).

%MAIN tlbugger:skip_bugger.


% = :- meta_predicate(one_must(0,0)).

%=

%% one_must( :GoalMCall, :GoalOnFail) is semidet.
%
% One Must Be Successfull.
%
one_must(MCall,OnFail):-  call(MCall) *->  true ; call(OnFail).



%=

%% must_det_u( :Goal) is semidet.
%
% Must Be Successfull Deterministic.
%

%must_det_u(Goal):- !,maybe_notrace(Goal),!.
must_det_u(Goal):- (quietly(Goal)->!;(ignore(rtrace(Goal)),break)).


%=

%% one_must_det( :Goal, :GoalOnFail) is semidet.
%
% One Must Be Successfull Deterministic.
%
one_must_det(Goal,_OnFail):-Goal,!.
one_must_det(_Call,OnFail):-OnFail,!.


%=

%% must_det_dead( :Goal, :GoalOnFail) is semidet.
%
% Must Be Successfull Deterministic.
%
%must_det_dead(Goal,OnFail):- trace_or_throw(deprecated(must_det_u(Goal,OnFail))),Goal,!.
%must_det_dead(_Call,OnFail):-OnFail.

:- module_transparent(must_det_l/1).

%=

%% must_det_l( :GoalMGoal) is semidet.
%
% Must Be Successfull Deterministic (list Version).
%
:- export(must_det_l/1).
must_det_l(Goal):- call_each_det(must_det_u,Goal).

must_det_l_pred(Pred,Rest):- tlbugger:skip_bugger,!,call(Pred,Rest).
must_det_l_pred(Pred,Rest):- call_each_det(call_must_det(Pred),Rest).

call_must_det(Pred,Arg):- must_det_u(call(Pred,Arg)),!.

is_call_var(Goal):- strip_module(Goal,_,P),var(P).


call_each(Pred,Goal):- call_each_det(Pred,Goal).

call_each_det(Pred,Goal):- notrace(is_call_var(Pred);is_call_var(Goal)),!,trace_or_throw(var_call_each(Pred,Goal)),!.
call_each_det(Pred,[Goal]):- !, dmsg(trace_syntax(call_each_det(Pred,[Goal]))),!,call_each_det(Pred,Goal).
call_each_det(Pred,[Goal|List]):- !, dmsg(trace_syntax(call_each_det(Pred,[Goal|List]))), !, call_each_det(Pred,Goal),!,call_each_det(Pred,List).
% call_each_det(Pred,Goal1):-tlbugger:skip_bugger,!,p_call(Pred,Goal1).
call_each_det(Pred,M:(Goal1,!)):-!, call_each_det(Pred,M:Goal1),!.
call_each_det(Pred,M:(Goal1,!,Goal2)):-!, call_each_det(Pred,M:Goal1),!,call_each_det(Pred,M:Goal2).
call_each_det(Pred,M:(Goal1,Goal2)):-!, call_each_det(Pred,M:Goal1),!,call_each_det(Pred,M:Goal2).
call_each_det(Pred,(Goal1,!)):-!, call_each_det(Pred,Goal1),!.
call_each_det(Pred,(Goal1,!,Goal2)):- !, call_each_det(Pred,Goal1),!,call_each_det(Pred,Goal2).
call_each_det(Pred,(Goal1,Goal2)):- !, call_each_det(Pred,Goal1),!,call_each_det(Pred,Goal2).
call_each_det(Pred,forall(Goal1,Goal2)):- !, forall(Goal1,call_each_det(Pred,Goal2)).
call_each_det(Pred,Goal):- p_call(Pred,Goal),!.

% p_call(Pred,_:M:Goal):-!,p_call(Pred,M:Goal).
p_call([Pred1|PredS],Goal):-!,p_call(Pred1,Goal),p_call(PredS,Goal).
p_call((Pred1,PredS),Goal):-!,p_call(Pred1,Goal),p_call(PredS,Goal).
p_call((Pred1;PredS),Goal):-!,p_call(Pred1,Goal);p_call(PredS,Goal).
p_call(Pred,Goal):-call(Pred,Goal).

must_find_and_call(G):-must(G).

:- module_transparent(det_lm/2).

%=

%% det_lm( ?M, ?Goal) is semidet.
%
% Deterministic Lm.
%
det_lm(M,(Goal,List)):- !,Goal,!,det_lm(M,List).
det_lm(M,Goal):-M:Goal,!.

:- module_transparent(must_l/1).

%=

%% must_l( :Goal) is semidet.
%
% Must Be Successfull (list Version).
%
must_l(Goal):- skipWrapper,!,call(Goal).
must_l(Goal):- var(Goal),trace_or_throw(var_must_l(Goal)),!.
must_l((A,!,B)):-!,must(A),!,must_l(B).
must_l((A,B)):-!,must((A,deterministic(Det),true,(Det==true->(!,must_l(B));B))).
must_l(Goal):- must(Goal).


:- thread_local tlbugger:skip_use_slow_sanity/0.
:- asserta((tlbugger:skip_use_slow_sanity:-!)).

% thread locals should defaults to false  tlbugger:skip_use_slow_sanity.


%=

%% slow_sanity( :Goal) is semidet.
%
% Slow Optional Sanity Checking.
%
slow_sanity(Goal):- ( tlbugger:skip_use_slow_sanity ; must(Goal)),!.


:- meta_predicate(hide_trace(0)).

hide_trace(G):- %JUNIT \+ tracing,
 !,call(G).
hide_trace(G):- !,call(G).
hide_trace(G):- skipWrapper,!,call(G).
hide_trace(G):-
 restore_trace((
   quietly(
      ignore((tracing,
      visible(-all),
      visible(-unify),
      visible(+exception),
      maybe_leash(-all),
      maybe_leash(+exception)))),G)).

:- meta_predicate(on_x_f(0,0,0)).
on_x_f(G,X,F):-catchv(G,E,(dumpST,wdmsg(E),X)) *-> true ; F .

% :- meta_predicate quietly(0).

% quietly(G):- skipWrapper,!,call(G).
% quietly(G):- !,quietly(G).
% quietly(G):- !, on_x_f((G),setup_call_cleanup(wdmsg(begin_eRRor_in(G)),rtrace(G),wdmsg(end_eRRor_in(G))),fail).
/*quietly(G):- on_x_f(hide_trace(G),
                     setup_call_cleanup(wdmsg(begin_eRRor_in(G)),rtrace(G),wdmsg(end_eRRor_in(G))),
                     fail).
*/

:- if(current_prolog_flag(optimise,true)).
is_recompile:-fail.
:- else.
is_recompile:-fail.
:- endif.

% -- CODEBLOCK
% :- export(7sanity/1).
% = :- meta_predicate(sanity(0)).



compare_results(N+NVs,O+OVs):-
   NVs=@=OVs -> true; trace_or_throw(compare_results(N,O)).

allow_unsafe_code :- fail.

unsafe_safe(_,O):- \+ allow_unsafe_code, !, call(O).
unsafe_safe(N,O):- on_diff_throw(N,O).

on_diff_throw(_G1,G2):- call(G2).

:- export(need_speed/0).
need_speed:-current_prolog_flag(unsafe_speedups , true) .

:- export(is_release/0).
%% is_release is semidet.
%
% If Is A Release.

is_release:- current_prolog_flag(unsafe_speedups, false) ,!,fail.
is_release:- !,fail.
is_release:- current_prolog_flag(unsafe_speedups , true) ,!.
is_release:- notrace((\+ flag_call(runtime_debug == true) , \+ (1 is random(4)))).



%% not_is_release is semidet.
%
% Not If Is A Release.
%
:- export(not_is_release/0).
not_is_release:- \+ is_release.


%=

%% badfood( ?MCall) is semidet.
%
% Badfood.
%
badfood(MCall):- numbervars(MCall,0,_,[functor_name('VAR_______________________x0BADF00D'),attvar(bind),singletons(false)]),dumpST.

% -- CODEBLOCK
:- export(without_must/1).
% = :- meta_predicate(without_must(0)).


%=

%% without_must( :Goal) is semidet.
%
% Without Must Be Successfull.
%
without_must(Goal):- locally(tlbugger:skipMust,Goal).

% -- CODEBLOCK
:- export(y_must/2).
:- meta_predicate (y_must(?,0)).

%=

%% y_must( ?Y, :Goal) is semidet.
%
% Y Must Be Successfull.
%
y_must(Y,Goal):- catchv(Goal,E,(wdmsg(E:must_xI__xI__xI__xI__xI_(Y,Goal)),fail)) *-> true ; dtrace(y_must(Y,Goal)).

% -- CODEBLOCK
% :- export(must/1).
%:- meta_predicate(must(0)).
%:- meta_predicate(must(0)).

%=


dumpST_error(Msg):- notrace((ddmsg(error,Msg),dumpST,wdmsg(error,Msg))).


:- thread_self_main->true;writeln(user_error,not_thread_self_main_consulting_ucatch).
:- save_streams.
:- initialization(save_streams,now).
:- initialization(save_streams,after_load).
:- initialization(save_streams,restore).
:- thread_initialization(save_streams).


:- setup_call_cleanup(true,set_main_error,notrace).
:- initialization(set_main_error).
:- initialization(set_main_error,after_load).
:- initialization(set_main_error,restore).
:- notrace.

%:- 'mpred_trace_none'(ddmsg(_)).
%:- 'mpred_trace_none'(ddmsg(_,_)).


sanity2(_Loc,Goal):- sanity(Goal).
must2(_Loc,Goal):- must(Goal).

ge_expand_goal(G,G):- \+ compound(G),!,fail.
ge_expand_goal(G,GO):- expand_goal(G,GO).

% ge_must_sanity(sanity(_),true).
% ge_must_sanity(must(Goal),GoalO):-ge_expand_goal(Goal,GoalO).
% ge_must_sanity(find_and_call(Goal),GoalO):-ge_expand_goal(Goal,GoalO).

% ge_must_sanity(sanity(Goal),nop(sanity(GoalO))):- ge_expand_goal(Goal,GoalO).
% ge_must_sanity(must(Goal),(GoalO*->true;debugCallWhy(failed_must(Goal,FL),GoalO))):- source_ctx(FL),ge_expand_goal(Goal,GoalO).

ge_must_sanity(P,O):- univ_safe_2(P,[F,Arg]),nonvar(Arg),ge_must_sanity(F,Arg,O).

ge_must_sanity(sanity,Goal,sanity2(FL,Goal)):- source_ctx(FL).
ge_must_sanity(must,Goal,must2(FL,Goal)):- source_ctx(FL).
% ge_must_sanity(must_det_l,Goal,must2(FL,Goal)):- source_ctx(FL).

system:goal_expansion(I,P,O,P):- notrace((compound(I), source_location(_,_))),
  (prolog_load_context(module, Module),default_module(Module,ucatch)),
  once(ge_must_sanity(I,O))->I \== O.

:- dynamic(inlinedPred/1).

/*
system:goal_expansion(I,O):- fail, compound(I),functor(I,F,A),inlinedPred(F/A),
  source_location(File,L),clause(I,Body),O= (file_line(F,begin,File,L),Body,file_line(F,end,File,L)).
*/

file_line(F,What,File,L):- (debugging(F)->wdmsg(file_line(F,What,File,L));true).


:- ignore((source_location(S,_1),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

% :- set_prolog_flag(compile_meta_arguments,true).

:- include(dumpst).
:- include(dmsg).
:- fixup_exports.

