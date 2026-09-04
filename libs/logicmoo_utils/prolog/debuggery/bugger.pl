/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: logicmoo@gmail.com;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_bugger.pl
%:- if((prolog_load_context(source,F),prolog_load_context(file,F))).
:- module(bugger,
          [
          debug_logicmoo/1,
          swi_module/2,
          nodebug_logicmoo/1,
          debugging_logicmoo/1,
          logicmoo_topic/2,
            asserta_if_ground/1,
            atom_contains666/2,
            call_count_nth/2,
            bad_idea/0,
            beenCaught/1,
            bin_ecall/4,
            bugger_atom_change/2,
            bugger_atom_change/6,
            bugger_atom_change0/6,
            bugger_error_info/1,
            bugger_expand_goal/2,
            bugger_expand_term/2,
            bugger_flag/1,
            bugger_flag/2,
            bugger_goal_expansion/2,
            bugger_goal_expansion/3,
            bugger_prolog_exception_hook/4,
            bugger_t_expansion/3,
            bugger_term_expansion/2,
            bugger_term_expansion/3,
            buggeroo/0,
            if_debug_module/1,
            call_or_list/1,
            call_skipping_n_clauses/2,
            caller_module/1,
            caller_module2/2,
            callsc/1,
            must_maplist/2,
            must_maplist/3,
            must_maplist/4,

          %all_source_file_predicates_are_transparent_bugger/0,
          all_source_file_predicates_are_transparent_bugger/1,


          debugCallWhy/2,
                dont_make_cyclic/1,

            cli_ntrace/1,
            cleanup_strings/0,
            debugFmt/1,
            debugFmtList/1,
            debugFmtList0/2,
            debugFmtList1/2,
            on_f_debug/1,
            debugOnFailureEach/1,
            default_dumptrace/1,
            default_ecall/3,
            define_if_missing/2,
            differnt_modules/2,
            disabled_this/0,
            do_gc/0,
            do_gc0/0,
            do_gc1/0,
            do_ref_job/2,
            doHideTrace/4,
            fmtString/2,
            fmtString/3,
            forall_member/3,
            format_safe/2,
            fresh_line/0,
            fresh_line/1,
            functor_h0/3,
            functor_source_file/5,
            functor_source_file0/5,
            gmust/2,
            gripe_time/2,
            on_f_debug/1,
            on_f_debug_ignore/1,

            on_x_debug_cont/1,
            on_x_rtraceEach/1,
            has_gui_debug/0,
            hideRest/0,
            hideTrace/0,
            hideTrace/2,
            hideTraceMFA/4,
            hideTraceMFAT/4,
            hideTraceMP/3,
            hidetrace/1,
            %hook_message_hook/0,
            ifThen/2,
            if_interactive/1,
            if_interactive/2,
            if_prolog/2,
            on_x_cont/1,
            ignore_each/1,
            in_file_directive/0,
            in_file_expansion/0,
            in_toplevel/0,
            isConsole/0,
            is_deterministic/1,
            kill_term_expansion/0,
            %list_difference_eq/3,
            list_difference_eq_memberchk_eq/2,
            listify/2,
            loading_module/1,
            loading_module/2,
            local_predicate/2,
            on_f_log_fail/1,
            logOnFailure/1,
            logOnFailure0/1,
            logOnFailureEach/1,
            on_f_log_ignore/2,
            logicmoo_bugger_loaded/0,
            meta_interp/2,
            meta_interp_signal/1,
            module_hotrace/1,
            module_stack/2,
            must_each/1,
            must_each0/1,
            must_maplist_det/2,
            must_maplist_det/3,
            must_maplist_det/4,
            new_a2s/3,
            new_a2s0/3,
            nodebugx/1,
            nth_frame/3,
            nth_frame_attribute/5,
            nth_goal/2,
            nth_pi/2,
            on_prolog_ecall/4,
            on_prolog_ecall_override/5,
            once_if_ground/1,
            once_if_ground/2,
            one_must/3,
            pop_def/1,
            predicate_module/2,
            printPredCount/3,
            programmer_error/1,
            prolog_call/1,
            prolog_current_frame_or_parent/2,
            prolog_current_frames/1,
            with_each/1,
            with_each/2,
            with_each/3,
            prolog_ecall_fa/5,
            prolog_must/1,
            prolog_must_l/1,
            prolog_must_not/1,
            push_def/1,
            randomVars/1,
            real_builtin_predicate/1,
            replace_elements/4,
            rmust_det/1,
            saveUserInput/0,
            set_bugger_flag/2,
            set_gui_debug/1,
            set_no_debug/0,
            set_no_debug_thread/0,
            set_optimize/1,
            set_yes_debug/0,
            set_yes_debug_thread/0,
            showProfilerStatistics/1,
            show_and_do/1,
            show_call/2,
            show_call/1,
            dcall0/1,
            show_entry/2,
            show_entry/1,
            show_failure/2,
            show_failure/1,
            show_success/2,
            show_success/1,
            show_goal_rethrow/2,
            show_module/1,
            shrink_clause/2,
            shrink_clause/3,
            singletons/1,


            test_for_release/1,
            test_for_release_problems/1,
            test_tl/1,
            test_tl/2,
            test_tl/3,
            thread_local_leaks/0,
            on_f_throw/1,
            throw_safe/1,
            time_call/1,
            to_list_of/3,
            traceAll/0,
            traceIf/1,
            dtrace_msg/1,
            traceok/1,
            tryCatchIgnore/1,
            tryHide/1,
            unlistify/2,
            willTrace/0,
            will_debug_else_throw/2,
            with_no_term_expansions/1,
            with_skip_bugger/1,
            writeErrMsg/2,
            writeErrMsg/3,
            writeFileToStream/2,
            writeOverwritten/0,
            writeSTDERR0/1,
            writeSavedPrompt/0,
     op(1150,fx,(baseKB:kb_shared))

          ]).
%:- endif.
:- multifile '$exported_op'/3.
:- dynamic '$exported_op'/3.
:- discontiguous '$exported_op'/3.
'$exported_op'(_,_,_):- fail.

:- multifile '$autoload'/3.
:- discontiguous '$autoload'/3.
:- dynamic '$autoload'/3.
'$autoload'(_,_,_):- fail.

%:- system:reexport(library(must_sanity)).
:- system:reexport(library(logicmoo/redo_locally)).
:- system:reexport(library(debuggery/bugger)).
module(_Skip,Exports):- maplist(export,Exports).

:- include(library(must_sanity)).
:- include(ucatch).
/** <module> Utility LOGICMOO_DEBUGGERY
This module supplies support for utilities, like DCG_must and must_trace, that allow for easier debugging.

 - @author Douglas R. Miles
 - @license LGPL
*/
:- meta_predicate with_skip_bugger(:).
:- meta_predicate
    do_ref_job(:,:),
        bugger_atom_change(:, -),
        bugger_atom_change(:, 0, +, +, -, -),
        bugger_expand_goal(:, -),
        bugger_expand_term(:, -),
        bugger_goal_expansion(:, -),
        bugger_goal_expansion(+, +, -),
        bugger_t_expansion(+, +, -),
        bugger_term_expansion(:, -),
        bugger_term_expansion(+, +, -),
         logOnFailureEach(:),
         debugOnFailureEach(:),
         on_x_rtraceEach(:),
        callsc(:),
        cli_ntrace(:),

        on_x_debug_cont(:),
        on_f_debug(:),
        on_f_debug(:),
   debugCallWhy(+, :),
        forall_member(?, ?, :),
        gmust(:, :),
        gripe_time(+, :),
        hideTrace(:, -),
        hidetrace(:),
        ifThen(:, :),
        if_interactive(:),
        if_interactive(:,0),
        if_prolog(?, :),
        on_x_cont(:),
        beenCaught(:),
        ignore_each(1),
        on_f_log_fail(:),
        on_f_debug_ignore(:),
        logOnFailure0(:),
        logOnFailure(:),
        on_f_log_ignore(+,+),
        meta_interp(:, +),
        must_each(:),
        must_maplist_det(:, ?),
        must_maplist_det(:, ?, ?),
        must_maplist_det(:, ?, ?, ?),
        nodebugx(:),
        once_if_ground(:),
        once_if_ground(:, -),
        one_must(:, 0, :),
        printPredCount(?, 0, ?),
        programmer_error(:),
        prolog_call(:),
        with_each(?, 1, :),
        with_each(?, :),
        with_each(:),
        prolog_ecall_fa(?, 1, ?, ?, :),
        prolog_must(:),
        prolog_must_l(?),
        prolog_must_not(:),
        push_def(:),
        randomVars(:),
        real_builtin_predicate(:),
        rmust_det(:),
        set_gui_debug(:),
        showProfilerStatistics(:),
        show_and_do(:),
        dcall0(:),
        bugger_t_expansion(:,0,:),
        bugger_goal_expansion(:,0,:),
       prolog_must_l(:),
       test_tl(:,*,:),
       bugger_term_expansion(:,0,:),


   show_call(:),
   show_entry(:),
   show_failure(:),
   show_success(:),

        test_tl(+),
        test_tl(1, +),
        test_tl(+, +, +),
        test_tl(:, +, +),
        on_f_throw(:),
        time_call(:),
        traceIf(:),
        dtrace_msg(?),
        traceok(:),
        tryCatchIgnore(:),
        will_debug_else_throw(:, :),
        with_no_term_expansions(:),
        hideTraceMFA(+,+,+,+),
        hideTraceMFAT(+,+,+,+),
        doHideTrace(+,+,+,+),
        dont_make_cyclic(:),
        with_skip_bugger(:).

 :- meta_predicate do_ref_job(:,:).

:- module_transparent
          debug_logicmoo/1,
          nodebug_logicmoo/1,
          debugging_logicmoo/1,
          logicmoo_topic/2,
                dont_make_cyclic/1,

                    show_failure/1,
        asserta_if_ground/1,
        atom_contains666/2,
        bad_idea/0,
        beenCaught/1,
        bin_ecall/4,
        bugger_atom_change0/6,
        bugger_error_info/1,
        bugger_flag/1,
        bugger_flag/2,
        bugger_prolog_exception_hook/4,
        buggeroo/0,
        call_or_list/1,
        call_skipping_n_clauses/2,
        caller_module/1,
        caller_module2/2,
        debugFmt/1,
        debugFmtList/1,
        debugFmtList0/2,
        debugFmtList1/2,
        on_x_rtraceEach/1,
        on_f_debug/1,
        debugOnFailureEach/1,
        on_f_debug_ignore/1,
        default_dumptrace/1,
        default_ecall/3,
        define_if_missing/2,
        differnt_modules/2,
        disabled_this/0,
        doHideTrace/4,
        do_gc/0,
        do_gc0/0,
        do_gc1/0,
        do_ref_job/2,
        fmtString/2,
        fmtString/3,
        format_safe/2,
        fresh_line/0,
        fresh_line/1,
        functor_h0/3,
        functor_source_file/5,
        functor_source_file0/5,
        has_gui_debug/0,
        hideRest/0,
        hideTrace/0,
        hideTraceMFA/4,
        hideTraceMFAT/4,
        hideTraceMP/3,
        %hook_message_hook/0,
        in_file_directive/0,
        in_file_expansion/0,
        in_toplevel/0,
        isConsole/0,
        is_deterministic/1,
        kill_term_expansion/0,
        %list_difference_eq/3,
        list_difference_eq_memberchk_eq/2,
        listify/2,
        loading_module/1,
        loading_module/2,
        local_predicate/2,
        logOnFailureEach/1,
        logicmoo_bugger_loaded/0,
        meta_interp_signal/1,
        module_hotrace/1,
        module_stack/2,
        must_each0/1,
        new_a2s/3,
        new_a2s0/3,
        nth_frame/3,
        nth_frame_attribute/5,
        nth_goal/2,
        nth_pi/2,
        on_prolog_ecall/4,
        on_prolog_ecall_override/5,
        pop_def/1,
        predicate_module/2,
        prolog_current_frame_or_parent/2,
        prolog_current_frames/1,
        replace_elements/4,
        saveUserInput/0,
        set_bugger_flag/2,
        set_no_debug/0,
        set_no_debug_thread/0,
        set_optimize/1,
        set_yes_debug/0,
        set_yes_debug_thread/0,
        show_goal_rethrow/2,
        beenCaught/1,
        show_module/1,
        shrink_clause/2,
        shrink_clause/3,
        singletons/1,
        test_for_release/1,
        test_for_release_problems/1,
        thread_local_leaks/0,
        throw_safe/1,
        to_list_of/3,
        traceAll/0,
        tryHide/1,
        unlistify/2,
        willTrace/0,
        writeErrMsg/2,
        writeErrMsg/3,
        writeFileToStream/2,
        writeOverwritten/0,
        writeSTDERR0/1,
        writeSavedPrompt/0.


% % % % OFF :- system:use_module(library(gui_tracer)).
% % % OFF :- system:use_module(library(check)).


% % % % OFF :- system:use_module('logicmoo_util_rtrace').
:- set_module(class(library)).

:- use_module(library(backcomp)).
:- use_module(library(debug)).
:- use_module(library(occurs)).
:- use_module(library(check)).
:- use_module(library(edinburgh)).
%:- use_module(library(gui_tracer)).
%:- use_module(library(debug)).
/*
:- pce:export(pce:get/3).
:- pce:export(pce:send/2).
:- pce_host:import(pce:get/3).
:- pce_host:import(pce:send/2).
:- gui_tracer:import(edinburgh:debug/0).
:- pce_portray:import(prolog_listing:portray_clause/1).
:- use_module(library(date)).
:- ifprolog:import(date:day_of_the_week/2).
:- ifprolog:import(date:day_of_the_year/2).
*/
:- meta_predicate(if_debug_module(:)).
if_debug_module(MG):- strip_module(MG,M,_),(debugging(M)-> MG; true).


% bugger:isa(_,_).

not_debugging:- \+ ( nb_current('$inprint_message', Messages), Messages\==[] ),
 \+ tracing,
 \+ current_prolog_flag(debug,true).

/*
%% all_source_file_predicates_are_transparent_bugger() is det.
%
% All Module Predicates Are Transparent.
%
:- module_transparent(all_source_file_predicates_are_transparent_bugger/0).
:- export(all_source_file_predicates_are_transparent_bugger/0).
all_source_file_predicates_are_transparent_bugger:-
  must(prolog_load_context(source,SFile)),all_source_file_predicates_are_transparent_bugger(SFile),
  must(prolog_load_context(file,File)),(SFile==File->true;all_source_file_predicates_are_transparent_bugger(File)).
*/

:- system:use_module(library(debug)).

:- module_transparent(all_source_file_predicates_are_transparent_bugger/1).
:- export(all_source_file_predicates_are_transparent_bugger/1).
all_source_file_predicates_are_transparent_bugger(File):-
    debug(logicmoo(loader),'~N~p~n',[all_source_file_predicates_are_transparent_bugger(File)]),
    forall((source_file(ModuleName:P,File),functor(P,F,A)),
      ignore((
        ignore(( \+ atom_concat('$',_,F), ModuleName:export(ModuleName:F/A))),
            \+ (predicate_property(ModuleName:P,(transparent))),
                   % ( nop(dmsg(todo(module_transparent(ModuleName:F/A))))),
                   (module_transparent(ModuleName:F/A))))).



:-
      op(1150,fx,(baseKB:kb_shared)),
      op(1150,fx,meta_predicate),
      op(1150,fx,thread_local).


:- set_prolog_flag(backtrace_show_lines,true).
% :- set_prolog_flag(access_level,system).
:- set_prolog_flag(backtrace_goal_depth,10). % default 3
:- set_prolog_flag(backtrace_depth,100). % default 20
:- set_prolog_flag(backtrace,true). % default true
:- set_prolog_flag(debug_on_error,true). % default true

swi_module(M,Preds):- forall(member(P,Preds),M:export(P)). % ,dmsg(swi_module(M)).


dont_make_cyclic(G):-skipWrapper,!,call(G).
dont_make_cyclic(G):-cyclic_break(G),!,G,cyclic_break(G).

%% bugger_flag( :TermF) is semidet.
%
% Logic Moo Debugger Flag.
%
bugger_flag(F=V):-bugger_flag(F,V).



%% bugger_flag( ?F, ?V) is semidet.
%
% Logic Moo Debugger Flag.
%
bugger_flag(F,V):-current_prolog_flag(F,V).



%% set_bugger_flag( ?F, ?V) is semidet.
%
% Set Logic Moo Debugger Flag.
%
set_bugger_flag(F,V):-current_prolog_flag(F,_Old),!,set_prolog_flag(F,V).
set_bugger_flag(F,V):-create_prolog_flag(F,V,[keep(true),tCol(ftTerm)]),!.





%% writeSTDERR0( ?A) is semidet.
%
% Write S True Structure (debug) E R R Primary Helper.
%
      writeSTDERR0(A):-dmsg(A).



%% debugFmt( ?A) is semidet.
%
% Debug Format.
%
      debugFmt(A):-dmsg(A).
/*

:- export((     on_x_debug/1, % Throws unless [Fail or Debug]
     on_x_log_throw/1, % Succeeds unless no error and failure occured

     on_x_cont/1, % same
        on_x_debug_cont/1,

        must_det/1, % must leave no coice points behind
     on_f_throw/1, % Throws unless [Fail or Debug]

     % cant ignore - Throws but can be set to [Throw, Fail or Ignore or Debug]
     dsddf must/1, % must succeed at least once
     sanity/1, % doesnt run on release
     gmust/2, % like must/1 but arg2 must be ground at exit

     must_each/1,  % list block must succeed once .. it smartly only debugs to the last failures
     on_x_log_cont/1,
     on_f_debug/1, % Succeeds but can be set to [Fail or Debug]
     on_f_log_fail/1,  % Fails unless [+Ignore]
          % can ignore
     on_x_fail/1, % for wrapping code may throw to indicate failure
   must_not_repeat/1)).  % predicate must never bind the same arguments the same way twice
*/



:- meta_predicate(call_count_nth(0,?)).
call_count_nth(C,N):-findall(C,C,L),nth1(N,L,C).


% :- if_may_hide('$hide'(skipWrapper/0)).
% :- if_may_hide('$hide'(tracing/0)).
% :- if_may_hide('$set_predicate_attribute'(tlbugger:skipMust,hide_childs,true)).
% :- if_may_hide('$set_predicate_attribute'(tlbugger:skipWrapper,hide_childs,true)).

:- export(ignore_each/1).
% = %= :- meta_predicate (ignore_each(1)).



%% ignore_each( :PRED1A) is semidet.
%
% Ignore Each.
%
ignore_each((A,B)):-ignore_each(A),ignore_each(B),!.
ignore_each(A):-ignore(A).

:- meta_predicate
	must_maplist(:, ?),
	must_maplist(:, ?, ?),
        must_maplist(:, ?, ?, ?).

%% 	must_maplist(:Goal, ?List)
%
%	True if Goal can successfully  be   applied  on  all elements of
%	List. Arguments are reordered to gain  performance as well as to
%	make the predicate deterministic under normal circumstances.




%% must_maplist( :PRED1Goal, ?Elem) is semidet.
%
% Must Be Successfull Maplist.
%
must_maplist(_, []).
must_maplist(Goal, [Elem|Tail]) :-
	must(call(Goal, Elem)),
	must_maplist(Goal, Tail).

%% 	must_maplist(:Goal, ?List1, ?List2)
%
%	As must_maplist/2, operating on pairs of elements from two lists.



%% must_maplist( :PRED2Goal, ?Elem1, ?Elem2) is semidet.
%
% Must Be Successfull Maplist.
%
must_maplist(_, [], []).
must_maplist( Goal, [Elem1|Tail1], [Elem2|Tail2]) :-
	must(call(Goal, Elem1, Elem2)),
	must_maplist( Goal, Tail1, Tail2).




%% must_maplist( :PRED3Goal, ?Elem1, ?Elem2, ?Elem3) is semidet.
%
% Must Be Successfull Maplist.
%
must_maplist(_, [], [],[]).
must_maplist( Goal, [Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3]) :-
	must(call(Goal, Elem1, Elem2, Elem3)),
	must_maplist( Goal, Tail1, Tail2, Tail3).




:- meta_predicate
	must_maplist_det(:, ?),
	must_maplist_det(:, ?, ?),
        must_maplist_det(:, ?, ?, ?).

%% 	must_maplist_det(:Goal, ?List)
%
%	True if Goal can successfully  be   applied  on  all elements of
%	List. Arguments are reordered to gain  performance as well as to
%	make the predicate deterministic under normal circumstances.




%% must_maplist_det( :PRED1Goal, ?Elem) is semidet.
%
% Must Be Successfull Maplist.
%
must_maplist_det(_, []):-!.
must_maplist_det(Goal, [Elem|Tail]) :-
	must(call(Goal, Elem)),!,
	must_maplist_det(Goal, Tail).

%% 	must_maplist_det(:Goal, ?List1, ?List2)
%
%	As must_maplist_det/2, operating on pairs of elements from two lists.



%% must_maplist_det( :PRED2Goal, ?Elem1, ?Elem2) is semidet.
%
% Must Be Successfull Maplist.
%
must_maplist_det(_, [], []):-!.
must_maplist_det( Goal, [Elem1|Tail1], [Elem2|Tail2]) :-
	must(call(Goal, Elem1, Elem2)),!,
	must_maplist_det( Goal, Tail1, Tail2).




%% must_maplist_det( :PRED3Goal, ?Elem1, ?Elem2, ?Elem3) is semidet.
%
% Must Be Successfull Maplist.
%
must_maplist_det(_, [], [],[]):-!.
must_maplist_det( Goal, [Elem1|Tail1], [Elem2|Tail2], [Elem3|Tail3]) :-
	must(call(Goal, Elem1, Elem2, Elem3)),!,
	must_maplist_det( Goal, Tail1, Tail2, Tail3).



:- ensure_loaded(library(lists)).


%% throw_safe( ?Exc) is semidet.
%
% Throw Safely Paying Attention To Corner Cases.
%
throw_safe(Exc):-trace_or_throw(Exc).

:- thread_local( t_l:testing_for_release/1).




%% test_for_release( ?File) is semidet.
%
% Test For Release.
%
test_for_release(File):-  source_file(File), \+ make:modified_file(File), !.
test_for_release(File):-
 G = test_for_release(File),
  scce_orig(dmsg("~N~nPress Ctrl-D to begin ~n~n  :- ~q. ~n~n",[G]),
  if_interactive(prolog),
   setup_call_cleanup(dmsg("~N~nStarting ~q...~n",[G]),
      locally_tl(testing_for_release(File),ensure_loaded(File)),
      test_for_release_problems(File))).




%% test_for_release_problems( ?File) is semidet.
%
% Test For Release Problems.
%
test_for_release_problems(_):-!.
test_for_release_problems(File):-
      dmsg("~N~nListing problems after ~q...~n",[File]),
      list_undefined,
      nop(after_boot(if_defined(gxref,true))),!.

%= :- meta_predicate  if_interactive(:).




%% if_interactive( :Goal) is semidet.
%
% If Interactive.
%
if_interactive(Goal):-if_interactive(Goal,true),!.

if_interactive(_Goal,Else):- keep_going, !,
   terminal_ansi_goal(current_output,[fg(red)],(format("~n(NOT INTERACTIVE (~q))~n",[Else]))),!,
   terminal_ansi_goal(current_output,[fg(yellow)],(call(Else))).

if_interactive(Goal,Else):-
  thread_self(main),
  current_input(In),
  stream_property(In,input),
  stream_property(In,tty(true)),
  dumpST,
  read_pending_codes(In,_,_),
  WantTrace=call(true),
  between(1,10,N),
  (N == 10 -> (format("~n(NOT INTERACTIVE (~q))~n",[Else]),!,(WantTrace,call(Else))) ;
  ( format(
"===================================================================

 Waiting... IF_INTERACTIVE ...

(g)o:
~q.

(s)kip   (d)ump  (t)race  (b)reak.

(c)ontinue non-interactively:

~q.
===================================================================",[Goal,Else]),
    notrace(with_tty_raw((wait_for_input([In],RL,2.0),RL \== [],get_single_char(C1),char_type(C1,to_upper(C))))),
    writeln([C]),
 (  C = 'g' -> (wdmsg("(... starting goal)~n",[]), !, (WantTrace,call(Goal)));
    C = 's' -> (dmsg("(... skipping goal)~n",[]), !);
    C = 'c' -> (dmsg("(... continuing as inf not interactive)~n",[]),!,(WantTrace,call(Else)));
    C = 'b' -> (dmsg("(... breaking)~n",[]),break,fail);
    C = 'd' -> (dmsg("(... dump )~n",[]),dumpST,fail);
    C = 't' -> (dmsg("(... trace )~n",[]),nb_setarg(1,WantTrace,trace),fail);
    true -> fail))).


if_interactive(_Goal,Else):-
   (format("~n(NOT INTERACTIVE (~q))~n",[Else]),!,call(Else)).


:- create_prolog_flag(bugger_debug,filter,[type(term),keep(true)]).
:- create_prolog_flag(dmsg_level,[never,error,warning,info,filter,always],[type(term),keep(true)]).
:- create_prolog_flag(dmsg_color,true,[type(boolean),keep(true)]).

% :- mpred_trace_nochilds(system:catch/3).


%:-multifile( tlbugger:bugger_prolog_flag/2).
% :- export( tlbugger:bugger_prolog_flag/2).


% = %= :- meta_predicate (callsc(:)).



%% callsc( :GoalG) is semidet.
%
% Callsc.
%
callsc(G):-G.
% :- '$hide'(callsc/1).
% :- current_predicate(M:callsc/1),mpred_trace_nochilds(M,callsc,1,0,0).


/*
current_prolog_flag(N,VV):-
   (( tlbugger:bugger_prolog_flag(N,V),
   ignore(current_prolog_flag(N,VO)),!,(VO=@=V -> true; ddmsg_call(set_prolog_flag(N,VO))));(current_prolog_flag(N,V),asserta( tlbugger:bugger_prolog_flag(N,V)))),!, V=VV.

set_prolog_flag(N,V):- current_prolog_flag(N,VV),!,
        (V==VV ->  true ; (asserta( tlbugger:bugger_prolog_flag(N,V)),set_prolog_flag(N,V))).
*/


%:- set_prolog_flag(dmsg_level,filter).
% :- set_prolog_flag(dmsg_color,false).

:- dynamic(double_quotes_was/1).
:- multifile(double_quotes_was/1).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).
:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
:- current_prolog_flag(double_quotes,WAS),asserta(double_quotes_was(WAS)).




%% define_if_missing( :PRED3M, ?List) is semidet.
%
% Define If Missing.
%
define_if_missing(M:F/A,List):-current_predicate(M:F/A)->true;((forall(member(C,List),M:assertz(C)),export(M:F/A))).

define_if_missing(system:atomics_to_string/3, [
  ( system:atomics_to_string(List, Separator, String):- new_a2s(List, Separator, String) ) ]).

define_if_missing(system:atomics_to_string/2, [
  ( system:atomics_to_string(List, String):- new_a2s(List, '', String) ) ]).




%% new_a2s( ?List, ?Separator, ?String) is semidet.
%
% New A2s.
%
new_a2s(List, Separator, String):-catchv(new_a2s0(List, Separator, String),_,((dtrace,new_a2s0(List, Separator, String)))).



%% new_a2s0( ?List, ?Separator, ?String) is semidet.
%
% New A2s Primary Helper.
%
new_a2s0(List, Separator, String):-
 (atomic(String) -> (string_to_atom(String,Atom),concat_atom(List, Separator, Atom));
     (concat_atom(List, Separator, Atom),string_to_atom(String,Atom))).



:- export(bad_idea/0).



%% bad_idea is semidet.
%
% Bad Idea used to make code that shuld not be ran in release mode
%
bad_idea:- current_prolog_flag(bad_idea,true).


% ===================================================================
% Bugger Term Expansions
% ===================================================================

%=  :- mpred_trace_childs(must(:)).


%:- ensure_loaded(dmsg).


%% prolog_call( :Goal) is semidet.
%
% Prolog Call.
%
prolog_call(Call):-call(Call).
% :- mpred_trace_childs(prolog_call(:)).

:- export(hidetrace/1).



%% hidetrace( ?X) is semidet.
%
% Hide Trace.
%
hidetrace(X):- X.
% :- mpred_trace_none(hidetrace(:)).

:- export( tlbugger:use_bugger_expansion/0).
:- dynamic( tlbugger:use_bugger_expansion/0).
:- retractall( tlbugger:use_bugger_expansion).
% :- asserta( tlbugger:use_bugger_expansion).




%% functor_h0( ?P, ?F, :PRED1A) is semidet.
%
% Functor Head Primary Helper.
%
functor_h0(P,F,A):-var(P),!,throw(functor_h_var(P,F,A)).
functor_h0(_:P,F,A):-nonvar(P),!,functor_h0(P,F,A).
functor_h0((P :- _),F,A):-nonvar(P),!,functor_h0(P,F,A).
functor_h0(P,_:F,A):-atom(F),compound(P),compound_name_arity(P,F,A),!.
functor_h0(P,F,A):-compound(P),compound_name_arity(P,F,A),!.
functor_h0(F,F,1):-!.

% = %= :- meta_predicate (bugger_t_expansion(+,+,-)).



%% bugger_t_expansion( +OUT1, +T, -T) is semidet.
%
% Logic Moo Debugger True Structure Expansion.
%
bugger_t_expansion(_,T,T):-var(T),!.
bugger_t_expansion(CM,(H:-B),(H:-BB)):-!,bugger_t_expansion(CM,B,BB).
bugger_t_expansion(_,T,T):-not(compound(T)),!.
% bugger_t_expansion(_,C =.. List,compound_name_arguments(C,F,ARGS)):-List =@= [F|ARGS],!.
bugger_t_expansion(_,prolog_call(T),T):-!.
bugger_t_expansion(_,dynamic(T),dynamic(T)):-!.
bugger_t_expansion(_,format(F,A),format_safe(F,A)):-!.
bugger_t_expansion(CM,zotrace(T),quietly(TT)):-!,bugger_t_expansion(CM,(T),(TT)).
bugger_t_expansion(_,F/A,F/A):-!.
bugger_t_expansion(_,M:F/A,M:F/A):-!.
bugger_t_expansion(CM,[F0|ARGS0],[F1|ARGS1]):- !,bugger_t_expansion(CM,F0,F1),bugger_t_expansion(CM,ARGS0,ARGS1).
% bugger_t_expansion(CM,T,AA):-  tlbugger:use_bugger_expansion,compound_name_arguments(T,F,[A]),unwrap_for_debug(F),!,bugger_t_expansion(CM,A,AA),
%  dmsg(bugger_term_expansion((T->AA))),!.
bugger_t_expansion(_,use_module(T),use_module(T)):-!.
bugger_t_expansion(_,module(A,B),module(A,B)):-!.
bugger_t_expansion(_,listing(A),listing(A)):-!.
bugger_t_expansion(CM,M:T,M:TT):-!,bugger_t_expansion(CM,T,TT),!.
bugger_t_expansion(_,test_is(A),test_is_safe(A)):-!.
bugger_t_expansion(_,delete(A,B,C),delete(A,B,C)):-!.
bugger_t_expansion(CM,T,TT):-
     compound_name_arguments(T,F,A),quietly((bugger_t_expansion(CM,A,AA),
     functor_h0(T,FH,AH))),
    ( (fail,bugger_atom_change(CM,T,F,FH,AH,FF))-> true; FF=F ),
    compound_name_arguments(TT,FF,AA),!,
    ((true;T =@= TT)-> true;  dmsg(bugger_term_expansion(CM,(T->TT)))),!.

% = %= :- meta_predicate (bugger_atom_change(:,0,+,+,-,-)).



%% bugger_atom_change( ?CM, :GoalT, +F, +FH, -FA, -FF) is semidet.
%
% Logic Moo Debugger Atom Change.
%
bugger_atom_change(CM,T,F,FH,FA,FF):- tlbugger:use_bugger_expansion, bugger_atom_change0(CM,T,F,FH,FA,FF).



%% bugger_atom_change0( ?CM, ?T, ?F, ?FH, ?FA, ?FF) is semidet.
%
% Logic Moo Debugger Atom Change Primary Helper.
%
bugger_atom_change0(_CM,T,_F,FH,FA,FF):- current_predicate_module(T,M1),atom_concat(FH,'_safe',FF),functor_safe(FFT,FF,FA),current_predicate_module(FFT,M2),differnt_modules(M1,M2).

% = %= :- meta_predicate (bugger_atom_change(:,(-))).



%% bugger_atom_change( ?CM, -TT) is semidet.
%
% Logic Moo Debugger Atom Change.
%
bugger_atom_change(CM:T,TT):-
     functor_h0(T,FH,AH),
     F = CM:T,
    (bugger_atom_change(CM,T,F,FH,AH,FF)->true;FF=F),!,
    compound_name_arity(TT,FF,AH).





%% differnt_modules( ?User2, ?User1) is semidet.
%
% Differnt Modules.
%
differnt_modules(User2,User1):- (User1==user;User2==user),!.
differnt_modules(User2,User1):- User1 \== User2.


:- dynamic(unwrap_for_debug/1).
% unwrap_for_debug(F):-member(F,[notrace,quietly]).
% unwrap_for_debug(F):-member(F,[traceok,must,must_det,quietly]).
%unwrap_for_debug(F):-member(F,['on_x_debug',on_x_debug]),!,fail.
%unwrap_for_debug(F):-member(FF,['OnError','OnFailure','LeastOne','Ignore','must']),atom_concat(_,FF,F),!.

% = %= :- meta_predicate (bugger_goal_expansion(:,-)).



%% bugger_goal_expansion( ?CM, -TT) is semidet.
%
% Logic Moo Debugger Goal Expansion.
%
bugger_goal_expansion(CM:T,TT):-  tlbugger:use_bugger_expansion,!,bugger_goal_expansion(CM,T,TT).
% = %= :- meta_predicate (bugger_goal_expansion(+,+,-)).



%% bugger_goal_expansion( +CM, +T, -T3) is semidet.
%
% Logic Moo Debugger Goal Expansion.
%
bugger_goal_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,on_x_fail(expand_term(T2,T3)).

% = %= :- meta_predicate (bugger_expand_goal(:,-)).



%% bugger_expand_goal( :GoalT, -IN2) is semidet.
%
% Logic Moo Debugger Expand Goal.
%
bugger_expand_goal(T,_):- fail,dmsg(bugger_expand_goal(T)),fail.

% = %= :- meta_predicate (bugger_expand_term(:,-)).



%% bugger_expand_term( :GoalT, -IN2) is semidet.
%
% Logic Moo Debugger Expand Term.
%
bugger_expand_term(T,_):- fail, dmsg(bugger_expand_term(T)),fail.

:- export(format_safe/2).



%% format_safe( ?A, ?B) is semidet.
%
% Format Safely Paying Attention To Corner Cases.
%
format_safe(A,B):-catchv(format(A,B),E,(dumpST,dtrace_msg(E:format(A,B)))).

% = %= :- meta_predicate (bugger_term_expansion(:,-)).



%% bugger_term_expansion( ?CM, -TT) is semidet.
%
% Logic Moo Debugger Term Expansion.
%
bugger_term_expansion(CM:T,TT):- compound(T),  tlbugger:use_bugger_expansion,!,bugger_term_expansion(CM,T,TT).
% = %= :- meta_predicate (bugger_term_expansion(+,+,-)).



%% bugger_term_expansion( +CM, +T, -T3) is semidet.
%
% Logic Moo Debugger Term Expansion.
%
bugger_term_expansion(CM,T,T3):- once(bugger_t_expansion(CM,T,T2)),T\==T2,!,nop(dmsg(T\==T2)),catchv(expand_term(T2,T3),_,fail).

%      expand_goal(G,G2):- compound(G),bugger_expand_goal(G,G2),!.


% goal_expansion(G,G2):- compound(G),bugger_goal_expansion(G,G2).

% expand_term(G,G2):- compound(G),bugger_expand_term(G,G2),!.


:- export(traceok/1).
%=  = %= :- meta_predicate (quietly(:)).
% = %= :- meta_predicate (traceok(:)).






%% thread_local_leaks is semidet.
%
% Thread Local Leaks.
%
thread_local_leaks:-!.






%% dtrace_msg( ?E) is semidet.
%
% (debug) Trace Msg.
%
dtrace_msg(E):- dumpST,wdmsg(E),dtrace(wdmsg(E)),!.



%% has_gui_debug is semidet.
%
% Has Gui Debug.
%
has_gui_debug :- current_prolog_flag(windows,true),!.
has_gui_debug :- ( \+ current_prolog_flag(gui,true) ),!,fail.
has_gui_debug :- getenv('DISPLAY',NV),NV\==''.

:- export(nodebugx/1).
:- module_transparent(nodebugx/1).


:- thread_local(tlbugger:ifHideTrace/0).

:- create_prolog_flag(nodebugx,false,[keep(true)]).
%% nodebugx( :GoalX) is semidet.
%
% Nodebugx.
%
nodebugx(X):- current_prolog_flag(nodebugx,true),!,call(X).
% nodebugx(X):- prolog_debug:debugging(Topic, true, _),!,scce_orig(nodebug(Topic),nodebugx(X),debug(Topic)).
nodebugx(X):- current_prolog_flag(debug_threads,true),!,call(X).
nodebugx(X):-
 locally(set_prolog_flag(nodebugx,true),
  locally(-tlbugger:ifCanTrace,
   locally(tlbugger:ifWontTrace,
    locally(tlbugger:show_must_go_on(true),
       locally(tlbugger:ifHideTrace,quietly(X)))))).

debugging_logicmoo(Mask):- logicmoo_topic(Mask,Topic),prolog_debug:debugging(Topic, TF, _),!,TF=true.

:- export(debugging_logicmoo_setting/3).
:- module_transparent(debugging_logicmoo_setting/3).
debugging_logicmoo_setting(_,true,[user_error]):- tracing.
:- multifile(prolog_debug:debugging/3).
:- dynamic(prolog_debug:debugging/3).
:- module_transparent(prolog_debug:debugging/3).

:- asserta((prolog_debug:debugging(X,Y,Z):- current_predicate(debugging_logicmoo_setting/3),
 predicate_property(bugger:debugging_logicmoo_setting(_,_,_),defined), debugging_logicmoo_setting(X,Y,Z))).
:- asserta((prolog_debug:debugging(_,False,[]):- current_prolog_flag(nodebugx,true),!,False=false)).


logicmoo_topic(Mask,Topic):-var(Mask),!,Topic=logicmoo(_).
logicmoo_topic(logicmoo,Topic):-!,Topic=logicmoo(_).
logicmoo_topic(Mask,Topic):-prolog_debug:debugging(Topic, _, _),Topic=@=Mask,!.
logicmoo_topic(Mask,Topic):-atomic(Mask),!,logicmoo_topic(logicmoo(Mask),Topic),!.
logicmoo_topic(Topic,Topic):-(ground(Topic)->nodebug(Topic);true).

nodebug_logicmoo(Mask):-
  forall(retract(prolog_debug:debugging(Mask, true, O)),asserta(prolog_debug:debugging(Mask, false, O))),
   logicmoo_topic(Mask,Topic),
   forall(retract(prolog_debug:debugging(Topic, true, O)),asserta(prolog_debug:debugging(Topic, false, O))),
   (ground(Mask)->nodebug(Topic);true),!.

debug_logicmoo(Mask):-
  forall(retract(prolog_debug:debugging(Mask, false, O)),asserta(prolog_debug:debugging(Mask, true, O))),
   logicmoo_topic(Mask,Topic),
   forall(retract(prolog_debug:debugging(Topic, false, O)),asserta(prolog_debug:debugging(Topic, true, O))),
   (ground(Mask)->debug(Topic);debug(Topic)),!.



:- dynamic isDebugging/1.

:- multifile was_module/2.
:- dynamic was_module/2.
:- module_transparent was_module/2.

:- thread_local(tlbugger:has_auto_trace/1).

%term_expansion(G,G2):- loop_check(bugger_term_expansion(G,G2)).
%goal_expansion(G,G2):- loop_check(bugger_goal_expansion(G,G2)).




% - 	list_difference_eq(+List, -Subtract, -Rest)
%
%	Delete all elements of Subtract from List and unify the result
%	with Rest. Element comparision is done using ==/2.



%% list_difference_eq( :TermX, ?Ys, ?L) is semidet.
%
% List Difference Using (==/2) (or =@=/2) ).
%
list_difference_eq([],_,[]).
list_difference_eq([X|Xs],Ys,L) :-
 	(  list_difference_eq_memberchk_eq(X,Ys)
 	-> list_difference_eq(Xs,Ys,L)
 	;  L = [X|T],
 	  list_difference_eq(Xs,Ys,T)
 	).



%% list_difference_eq_memberchk_eq( ?X, :TermY) is semidet.
%
% List Difference Using (==/2) (or =@=/2) ) Memberchk Using (==/2) (or =@=/2) ).
%
list_difference_eq_memberchk_eq(X, [Y|Ys]) :- (  X == Y -> true ;  list_difference_eq_memberchk_eq(X, Ys) ).


%= :- meta_predicate  meta_interp(:,+).




%% meta_interp_signal( :TermV) is semidet.
%
% Meta Interp Signal.
%
meta_interp_signal(meta_call(V)):-!,nonvar(V).
meta_interp_signal(meta_callable(_,_)).
meta_interp_signal(_:meta_call(V)):-!,nonvar(V).
meta_interp_signal(_:meta_callable(_,_)).

:- export(meta_interp/2).



%% meta_interp( ?CE, +A) is semidet.
%
% Meta Interp.
%
meta_interp(CE,A):- quietly((var(A); \+ if_defined(stack_check,fail))),!, throw(meta_interp(CE,A)).
meta_interp(_CE,A):- maybe_leash(+all),meta_interp_signal(A),!,fail.
meta_interp(CE,M:X):- atom(M),!,meta_interp(CE,X).
meta_interp(_,true):-!.
meta_interp(CE,A):- call(CE, meta_callable(A,NewA)),!,call(NewA).
meta_interp(CE,\+(A)):-!,\+(meta_interp(CE,A)).
meta_interp(CE,not(A)):-!,\+(meta_interp(CE,A)).
meta_interp(CE,once(A)):-!,once(meta_interp(CE,A)).
meta_interp(CE,must(A)):-!,must(meta_interp(CE,A)).
meta_interp(CE,(A->B;C)):-!,(meta_interp(CE,A)->meta_interp(CE,B);meta_interp(CE,C)).
meta_interp(CE,(A*->B;C)):-!,(meta_interp(CE,A)*->meta_interp(CE,B);meta_interp(CE,C)).
meta_interp(CE,(A;B)):-!,meta_interp(CE,A);meta_interp(CE,B).
meta_interp(CE,(A->B)):-!,meta_interp(CE,A)->meta_interp(CE,B).
meta_interp(CE,(A,!)):-!,meta_interp(CE,A),!.
meta_interp(CE,(A,B)):-!,meta_interp(CE,A),meta_interp(CE,B).
%meta_interp(_CE,!):- !, cut_block(!).
meta_interp(CE,A):- show_call(why,call(CE,meta_call(A))).


% was_module(Mod,Exports) :- nop(was_module(Mod,Exports)).




% ===================================================

% = %= :- meta_predicate (once_if_ground(:)).



%% once_if_ground( :Goal) is semidet.
%
% Once If Ground.
%
once_if_ground(Call):-not(ground(Call)),!,Call.
once_if_ground(Call):- once(Call).

% = %= :- meta_predicate (once_if_ground(:,-)).



%% once_if_ground( :Goal, -T) is semidet.
%
% Once If Ground.
%
once_if_ground(Call,T):-not(ground(Call)),!,Call,deterministic(D),(D=yes -> T= (!) ; T = true).
once_if_ground(Call,!):-once(Call).

% ===================================================




%% to_list_of( ?VALUE1, :TermRest, ?Rest) is semidet.
%
% Converted To List Of.
%
to_list_of(_,[Rest],Rest):-!.
to_list_of(RL,[R|Rest],LList):-
      to_list_of(RL,R,L),
      to_list_of(RL,Rest,List),
      univ_safe_2(LList,[RL,L,List]),!.

% ===================================================




%% call_or_list( ?Rest) is semidet.
%
% Call Or List.
%
call_or_list([Rest]):-!,call(Rest).
call_or_list(Rest):-to_list_of(';',Rest,List),!,call(List).




%% call_skipping_n_clauses( ?N, ?H) is semidet.
%
% Call Skipping N Clauses.
%
call_skipping_n_clauses(N,H):-
   findall(B,clause_safe(H,B),L),length(L,LL),!,LL>N,length(Skip,N),append(Skip,Rest,L),!,call_or_list(Rest).

% =========================================================================

:- thread_local( tlbugger:wastracing/0).
% :- mpred_trace_none( tlbugger:wastracing/0).

% =========================================================================
% cli_ntrace(+Call) is nondet.
% use call/1 with dtrace turned off



%% cli_ntrace( :GoalX) is semidet.
%
% Cli N Trace.
%
cli_ntrace(X):- tracing -> locally( tlbugger:wastracing,call_cleanup((notrace,call(X)),dtrace)) ; call(X).



%% traceok( :GoalX) is semidet.
%
% Traceok.
%
traceok(X):-  tlbugger:wastracing -> call_cleanup((dtrace,call(X)),notrace) ; call(X).

% :- mpred_trace_none(tlbugger:skip_bugger).
% :- mpred_trace_none(skipWrapper).


% =========================================================================


% = %= :- meta_predicate (show_entry(Why,0)).



%% show_entry( +Why, :Goal) is semidet.
%
% Show Entry.
%
show_entry(Why,Call):-debugm(Why,show_entry(Call)),show_call(Why,Call).



%% show_entry( :Goal) is semidet.
%
% Show Entry.
%
show_entry(Call):-show_entry(mpred,Call).

%= :- meta_predicate  dcall0(:).



%% dcall0( :Goal) is semidet.
%
% Dirrectly Call Primary Helper.
%
%dcall0(Goal):- catch(Goal,_,(trace,Goal)). % on_x_debug(Goal). % dmsg(show_call(why,Goal)),Goal.
dcall0(Goal):- on_x_debug(Goal). % dmsg(show_call(why,Goal)),Goal.

%= :- meta_predicate  show_call(+,0).



%% show_call( +Why, :Goal) is semidet.
%
% Show Call.
%
show_call(Why,Goal):- show_success(Why,Goal)*->true;(dmsg(sc_failure(Why,Goal)),!,fail).


%% show_call( :Goal) is semidet.
%
% Show Call.
%
show_call(Goal):- strip_module(Goal,Why,_),show_call(Why,Goal).

%= :- meta_predicate  show_failure(+,0).



%% show_failure( +Why, :Goal) is semidet.
%
% Show Failure.
%
show_failure(Why,Goal):-one_must(dcall0(Goal),(nop(dumpST),debugm1(Why,show_failed(Why,Goal)),nop(break),!,fail)).



%% show_failure( :Goal) is semidet.
%
% Show Failure.
%
show_failure(Goal):- strip_module(Goal,Why,_),show_failure(Why,Goal).


%% show_success( +Why, :Goal) is semidet.
%
% Show Success.
%

% show_success(_Why,Goal):-!,Goal.
show_success(Why,Goal):-
  zotrace((ignore((cyclic_term(Goal),dumpST, dmsg(try_show_success(Why,cyclic_term)))))),
  dcall0(Goal),
  zotrace((cyclic_term(Goal)-> dmsg(show_success(Why,cyclic_term)) ; debugm(Why,show_success(Why,Goal)))).

%=

%% debugm1( ?Why, ?Msg) is det.
%
% Debugm1.
%
debugm1(Why,Msg):- % dmsg(debugm(Why,Msg)),
                   ignore(zotrace(debugm10(Why,Msg))).

% :- debug(mpred).
debugm10(Why,Msg):- % \+ debugging(mpred),
    \+ debugging(Why), \+ debugging(mpred(Why)),!, debug(Why,'~N~p~n',[Msg]),!.
debugm10(Why,Msg):- dmsg(debugm(Why,Msg)), debug(Why,'~N~p~n',[Msg]),!.



%% show_success( :Goal) is semidet.
%
% Show Success.
%
show_success(Goal):- strip_module(Goal,Why,_),show_success(Why,Goal).

%= :- meta_predicate  on_f_log_fail(:).
:- export(on_f_log_fail/1).



%% on_f_log_fail( :Goal) is semidet.
%
% Whenever Functor Log Fail.
%
on_f_log_fail(Goal):-one_must(Goal,zotrace((ignore(dumpST),wdmsg(on_f_log_fail(Goal)),cleanup_strings,!,fail))).



% ==========================================================
% can/will Tracer.
% ==========================================================



%% shrink_clause( ?P, ?Body, ?Prop) is semidet.
%
% Shrink Clause.
%
shrink_clause(P,Body,Prop):- (Body ==true -> Prop=P ; (Prop= (P:-Body))).





%% shrink_clause( ?HB, ?HB) is semidet.
%
% Shrink Clause.
%
shrink_clause( (H:-true),H):-!.
shrink_clause( HB,HB).


:- thread_local(tlbugger:ifCanTrace/0).
:- asserta((tlbugger:ifCanTrace:-!)).
% :- '$hide'(tlbugger:ifCanTrace/0).
% thread locals should defaults to false: tlbugger:ifCanTrace.
%MAIN

:- export(tlbugger:ifWontTrace/0).
:- thread_local(tlbugger:ifWontTrace/0).
% :- '$hide'(tlbugger:ifWontTrace/0).

% :- '$hide'(tlbugger:ifHideTrace/0).

%:-meta_predicate(set_no_debug).
:- export(set_no_debug/0).

:- dynamic(is_set_no_debug/0).




%% set_no_debug is semidet.
%
% Set No Debug.
%
set_no_debug:-
  notrace,
  must_det_l((
   asserta(is_set_no_debug),
   set_prolog_flag(generate_debug_info, true),
   retractall(tlbugger:ifCanTrace),
   retractall(tlbugger:ifWontTrace),
   asserta(tlbugger:ifWontTrace),
   set_prolog_flag(report_error,false),
   set_prolog_flag(debug_on_error,false),
   set_prolog_flag(debug, false),
   set_prolog_flag(query_debug_settings, debug(false, false)),
   set_gui_debug(fail),
   maybe_leash(-all),
   maybe_leash(+exception),
   visible(-cut_call),!,
   notrace, nodebug)),!.

:- export(set_no_debug_thread/0).



%% set_no_debug_thread is semidet.
%
% Set No Debug Thread.
%
set_no_debug_thread:-
  must_det_l((
   retractall(tlbugger:ifCanTrace),
   retractall(tlbugger:ifWontTrace),
   asserta(tlbugger:ifWontTrace))),!.

:- if(prolog_dialect:exists_source(library(gui_tracer))).
%= :- meta_predicate  set_gui_debug(:).

%% set_gui_debug( :GoalTF) is semidet.
%
% Set Gui Debug.
%
set_gui_debug(TF):- current_prolog_flag(gui,true),!,
   ((TF, has_gui_debug, set_yes_debug, ignore((use_module(library(gui_tracer)),catchv(guitracer,_,true))))
     -> set_prolog_flag(gui_tracer, true) ;
        set_prolog_flag(gui_tracer, false)).
:- endif.
set_gui_debug(false):-!.
set_gui_debug(true):- dmsg("Warning: no GUI").

:- module_transparent(set_yes_debug/0).
:- export(set_yes_debug/0).



%% set_yes_debug is semidet.
%
% Set Yes Debug.
%
set_yes_debug:-
  must_det_l((
   set_prolog_flag(generate_debug_info, true),
   set_prolog_flag(report_error,true),
   set_prolog_flag(debug_on_error,true),
   % set_prolog_flag(debug, true),
   set_prolog_flag(query_debug_settings, debug(true, true)),
   % set_gui_debug(true),
   maybe_leash(+all),
   maybe_leash(+exception),
   visible(+cut_call),
   notrace, debug)),!.




%% set_yes_debug_thread is semidet.
%
% Set Yes Debug Thread.
%
set_yes_debug_thread:-
  set_yes_debug,
   (tlbugger:ifCanTrace->true;assert(tlbugger:ifCanTrace)),
   retractall(tlbugger:ifWontTrace).

:- tlbugger:use_bugger_expansion->true;assert(tlbugger:use_bugger_expansion).

% :- set_yes_debug.





%% isConsole is semidet.
%
% If Is A Console.
%
isConsole :- current_output(X),!,stream_property(X,alias(user_output)).
%isConsole :- telling(user).




%% willTrace is semidet.
%
% will  Trace.
%
willTrace:-tlbugger:ifWontTrace,!,fail.
willTrace:-not(isConsole),!,fail.
willTrace:-tlbugger:ifCanTrace.




%% hideTrace is semidet.
%
% hide  Trace.
%
hideTrace:-
  hideTrace([quietly/1], -all),
  % hideTrace(computeInnerEach/4, -all),

  hideTrace(
   [maplist_safe/2,
       maplist_safe/3], -all),


  hideTrace([hideTrace/0,
     tlbugger:ifCanTrace/0,
     ctrace/0,
     willTrace/0], -all),

  hideTrace([traceafter_call/1], -all),
  % hideTrace([notrace_call/1], -all),

  hideTrace([
   call/1,
   call/2,
   apply/2,
   '$bags':findall/3,
   '$bags':findall/4,
   once/1,
   ','/2,
   catch/3,
   catchv/3,
   member/2], -all),

  hideTrace(setup_call_catcher_cleanup/4,-all),

  hideTrace(system:throw/1, +all),
  % hideTrace(system:dmsg/2, +all),
  hideTrace(message_hook/3 , +all),
  hideTrace(system:message_to_string/2, +all),
  !,hideRest,!.
  % findall(File-F/A,(functor_source_file(M,P,F,A,File),M==user),List),sort(List,Sort),dmsg(Sort),!.

/*
hideRest:- fail, buggerDir(BuggerDir),
   functor_source_file(M,_P,F,A,File),atom_concat(BuggerDir,_,File),hideTraceMFA(M,F,A,-all),
   fail.  */



%% hideRest is semidet.
%
% Hide Rest.
%
hideRest:- functor_source_file(system,_P,F,A,_File),hideTraceMFA(system,F,A, - all), fail.
hideRest.

% = %= :- meta_predicate (hideTrace(:,-)).




%% functor_source_file( ?M, ?P, ?F, ?A, ?File) is semidet.
%
% Functor Source File.
%
functor_source_file(M,P,F,A,File):-functor_source_file0(M,P,F,A,File). % sanity(ground((M,F,A,File))),must(nonvar(P)).



%% functor_source_file0( ?M, ?P, ?F, ?A, ?File) is semidet.
%
% Functor Source File Primary Helper.
%
functor_source_file0(M,P,F,A,File):-current_predicate(F/A),functor_safe(P,F,A),source_file(P,File),predicate_module(P,M).


localize_module(C):- nonvar(C),!.
localize_module(C):- context_module(CM),(C=CM;(current_module(C),C\==CM)).

%% predicate_module( ?P, ?M) is semidet.
%
% Predicate Module.
%
predicate_module(MPI,M):- strip_module(MPI,C,PI),predicate_module(C,PI,M),!.
predicate_module(M:_,M):-!. %strip_module(P,M,_F),!.
predicate_module(_P,user):-!. %strip_module(P,M,_F),!.
% predicate_module(P,M):- strip_module(P,M,_F),!.

predicate_module(C,PI,M):- var(PI),!,localize_module(C),C:predicate_property(PI,imported_from(M)),!.
predicate_module(C,'//'(F,A),M):-!,atom(F),integer(A),AA is A +2,functor(P,F,AA),predicate_module_0(C,P,M).
predicate_module(C,'/'(F,A),M):- !,atom(F),integer(A),functor(P,F,A),predicate_module_0(C,P,M).
predicate_module(C,P,M):- predicate_module_0(C,P,M).

predicate_module_0(C,P,M):- atom(C),!,predicate_module_atom(C,P,M).
predicate_module_0(C,P,M):- var(P),!,trace_or_throw(predicate_module_0(C,P,M)).
predicate_module_0(C,P,M):- localize_module(C),predicate_module_atom(user,P,M),!,ignore(C=M).

predicate_module_atom(C,P,M):- C:predicate_property(P,imported_from(M)).
predicate_module_atom(C,P,M):- C:predicate_property(P,defined),!,M=C.





%% hideTrace( ?MA, -T) is semidet.
%
% hide  Trace.
%
hideTrace(_:A, _) :-
    var(A), !, dtrace, fail,
    throw(error(instantiation_error, _)).
hideTrace(_:[], _) :- !.
hideTrace(A:[B|D], C) :- !,
    hideTrace(A:B, C),
    hideTrace(A:D, C),!.

hideTrace(M:A,T):-!,hideTraceMP(M,A,T),!.
hideTrace(MA,T):-hideTraceMP(_,MA,T),!.




%% hideTraceMP( ?M, ?P, ?T) is semidet.
%
% hide  Trace Module Pred.
%
hideTraceMP(M,F/A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMP(M,P,T):-functor_safe(P,F,0),dtrace,hideTraceMFA(M,F,_A,T),!.
hideTraceMP(M,P,T):-functor_safe(P,F,A),hideTraceMFA(M,F,A,T),!.




%% tryCatchIgnore( :GoalMFA) is semidet.
%
% Try Catch Ignore.
%
tryCatchIgnore(MFA):- catchv(MFA,_E,true). % dmsg(tryCatchIgnoreError(MFA:E))),!.
tryCatchIgnore(_MFA):- !. % dmsg(tryCatchIgnoreFailed(MFA)).

% tryHide(_MFA):-showHiddens,!.



%% tryHide( ?MFA) is semidet.
%
% Try Hide.
%
tryHide(MFA):- tryCatchIgnore('$hide'(MFA)).




%% hideTraceMFA( ?M, ?F, ?A, ?T) is semidet.
%
% hide  Trace Module Functor a.
%
hideTraceMFA(_,M:F,A,T):-!,hideTraceMFA(M,F,A,T),!.
hideTraceMFA(M,F,A,T):-nonvar(A),functor_safe(P,F,A),predicate_property(P,imported_from(IM)),IM \== M,!,nop(dmsg(doHideTrace(IM,F,A,T))),hideTraceMFA(IM,F,A,T),!.
hideTraceMFA(M,F,A,T):-hideTraceMFAT(M,F,A,T),!.




%% hideTraceMFAT( ?M, ?F, ?A, ?T) is semidet.
%
% hide  Trace Module Functor a True Stucture.
%
hideTraceMFAT(M,F,A,T):-doHideTrace(M,F,A,T),!.




%% doHideTrace( ?M, ?F, ?A, ?ATTRIB) is semidet.
%
% do hide  Trace.
%
doHideTrace(_M,_F,_A,[]):-!.
doHideTrace(M,F,A,[hide|T]):- tryHide(M:F/A),!,doHideTrace(M,F,A,T),!.
doHideTrace(M,F,A,[-all]):- '$hide'(M:F/A),fail.
doHideTrace(M,F,A,ATTRIB):- ( \+ is_list(ATTRIB)),!,doHideTrace(M,F,A,[ATTRIB]).
doHideTrace(M,F,A,ATTRIB):- tryHide(M:F/A),!,
  tryCatchIgnore(dtrace(M:F/A,ATTRIB)),!.





%% ctrace is semidet.
%
% Class Trace.
%
ctrace:-willTrace->dtrace;notrace.




%% buggeroo is semidet.
%
% Buggeroo.
%
buggeroo:-hideTrace,traceAll,atom_concat(guit,racer,TRACER), catchv(call(TRACER),_,true),debug,list_undefined.




%% singletons( ?VALUE1) is semidet.
%
% Singletons.
%
singletons(_).

/*
 Stop turning GC on/off
:- set_prolog_flag(backtrace_goal_depth, 2000).
:- set_prolog_flag(debugger_show_context,true).
:- set_prolog_flag(trace_gc,true).
:- set_prolog_flag(gc,true).
:- set_prolog_flag(debug,true).
:- set_prolog_flag(debugger_write_options,[quoted(true), portray(true), max_depth(1000), attributes(portray),spacing(next_argument)]).
 put_attr(VV,vn,'YY'),writeq(vv(VV)).

:- set_prolog_flag(toplevel_print_factorized,true). % default false
:- set_prolog_flag(toplevel_print_anon,true).
:- set_prolog_flag(toplevel_mode,backtracking). % OR recursive

*/
:- set_prolog_flag(backtrace_depth,   2000).
:- set_prolog_flag(backtrace_show_lines, true).
:- set_prolog_flag(debugger_show_context,true).



%% set_optimize( ?TF) is semidet.
%
% Set Optimize.
%
set_optimize(_):- !.
set_optimize(TF):- set_prolog_flag(gc,TF),set_prolog_flag(last_call_optimisation,TF),set_prolog_flag(optimise,TF),do_gc0.




%% do_gc is semidet.
%
% Do Gc.
%
% do_gc:- !.
do_gc:- do_gc0,!.




%% do_gc0 is semidet.
%
% Do Gc Primary Helper.
%
do_gc0:- current_prolog_flag(gc, true),!,do_gc1.
do_gc0:- set_prolog_flag(gc, true), do_gc1, set_prolog_flag(gc,false),
         dmsg(warning(set_prolog_flag(gc,false))).




%% do_gc1 is semidet.
%
% Do Gc Secondary Helper.
%
do_gc1:- notrace((
  trim_stacks,
  cleanup_strings,
  garbage_collect_atoms,
  %garbage_collect_clauses
  %statistics
  garbage_collect)).





%% fresh_line is semidet.
%
% Fresh Line.
%
fresh_line:-current_output(Strm),fresh_line(Strm),!.



% :- multifile(lmcache:is_prolog_stream/1).
% :- dynamic(lmcache:is_prolog_stream/1).

%% fresh_line( ?Strm) is semidet.
%
% Fresh Line.
%
%fresh_line(Strm):-lmcache:is_prolog_stream(Strm),on_x_fail(format(Strm,'~n',[])),!.
fresh_line(Strm):-on_x_fail(format(Strm,'~N',[])),!.
fresh_line(Strm):-on_x_fail((stream_property(Strm,position('$stream_position'(_,_,POS,_))),(POS>0->nl(Strm);true))),!.
fresh_line(Strm):-on_x_fail(nl(Strm)),!.
fresh_line(_).




%% ifThen( :GoalWhen, :GoalDo) is semidet.
%
% If Then.
%
ifThen(When,Do):-When->Do;true.

% :- current_predicate(F/N),dtrace(F/N, -all),fail.
/*
traceAll:- current_predicate(F/N),
  functor_safe(P,F,N),
  local_predicate(P,F/N),
  trace(F/N, +fail),fail.
traceAll:- not((predicate_property(clearCateStack/1,_))),!.
traceAll:-findall(_,(member(F,[member/2,dmsg/1,takeout/3,findall/3,clearCateStack/1]),trace(F, -all)),_).
*/



%% traceAll is semidet.
%
%  Trace all.
%
traceAll:-!.





%% forall_member( ?C, ?C1, :Goal) is semidet.
%
% Forall Member.
%
forall_member(C,[C],Call):-!,once(Call).
forall_member(C,C1,Call):-forall(member(C,C1),once(Call)).




%% prolog_must( :Goal) is semidet.
%
% Prolog Must Be Successfull.
%
prolog_must(Call):-must(Call).


% gmust is must with sanity



%% gmust( :GoalTrue, :Goal) is semidet.
%
% Gmust.
%
gmust(True,Call):-catchv((Call,(True->true;throw(retry(gmust(True,Call))))),retry(gmust(True,_)),(dtrace,Call,True)).

% must is used declaring the predicate must suceeed




%% on_f_throw( :Goal) is semidet.
%
% Whenever Functor Throw.
%
on_f_throw(Call):-one_must(Call,throw(on_f_throw(Call))).



%% on_x_cont( :GoalCX) is semidet.
%
% If there If Is A an exception in  :Goal Class x then cont.
%
on_x_cont(CX):-ignore(catchv(CX,_,true)).

% pause_trace(_):- quietly(((debug,visible(+all),maybe_leash(+exception),maybe_leash(+call)))),dtrace.

%debugCall(Goal):-quietly,dmsg(debugCall(Goal)),dumpST, pause_trace(errored(Goal)),ggtrace,Goal.
%debugCallF(Goal):-quietly,dmsg(debugCallF(Goal)),dumpST, pause_trace(failed(Goal)),gftrace,Goal.





%% with_skip_bugger( :Goal) is semidet.
%
% Using Skip Logic Moo Debugger.
%
with_skip_bugger(Goal):-setup_call_cleanup(asserta( tlbugger:skip_bugger,Ref),Goal,erase(Ref)).




%% on_x_rtraceEach( :Goal) is semidet.
%
% If there If Is A an exception in  :Goal goal then r Trace each.
%
on_x_rtraceEach(Goal):-with_each(1,on_x_debug,Goal).



%% on_x_debug_cont( :Goal) is semidet.
%
% If there If Is A an exception in  :Goal goal then debug cont.
%
on_x_debug_cont(Goal):-ignore(on_x_debug(Goal)).




%% with_each( :GoalWrapperGoal) is semidet.
%
% Using Each.
%
with_each(WrapperGoal):- univ_safe_2(WrapperGoal,[Wrapper,Goal]),with_each(Wrapper,Goal).



%% with_each( ?Wrapper, :Goal) is semidet.
%
% Using Each.
%
with_each(Wrapper,Goal):-with_each(1,Wrapper,Goal).





%% on_f_debug( :Goal) is semidet.
%
% Whenever Functor Debug.
%
on_f_debug(Goal):-  Goal *-> true; ((nortrace,notrace,debugCallWhy(failed(on_f_debug(Goal)),Goal)),fail).



%% debugCallWhy( ?Why, :GoalC) is semidet.
%
% Debug Call Generation Of Proof.
%
debugCallWhy(Why, C):- wdmsg(Why),catch(dtrace(C),E,wdmsg(cont_X_debugCallWhy(E,Why, C))).


%% debugOnFailureEach( :Goal) is semidet.
%
% Debug Whenever Failure Each.
%
debugOnFailureEach(Goal):-with_each(1,on_f_debug,Goal).



%% on_f_debug_ignore( :Goal) is semidet.
%
% Whenever Functor Debug Ignore.
%
on_f_debug_ignore(Goal):-ignore(on_f_debug(Goal)).


logOnFailure(Goal):-on_f_log_fail(Goal).



%% logOnFailure0( :Goal) is semidet.
%
% Log Whenever Failure Primary Helper.
%
logOnFailure0(Goal):- one_must(Goal,(dmsg(on_f_log_fail(Goal)),fail)).



%% logOnFailureEach( :Goal) is semidet.
%
% Log Whenever Failure Each.
%
logOnFailureEach(Goal):-with_each(1,on_f_log_fail,Goal).



%% on_f_log_ignore( :Goal) is semidet.
%
% Whenever Functor Log Ignore.
%
on_f_log_ignore(M,Goal):-ignore(logOnFailure0(on_x_log_throw(M:Goal))).


%on_f_debug(Goal):-ctrace,Goal.
%on_f_debug(Goal):-catchv(Goal,E,(writeFailureLog(E,Goal),throw(E))).
%on_f_throw/1 is like Java/C's assert/1
%debugOnFailure1(Module,Goal):-dtrace,on_f_debug(Module:Goal),!.
%debugOnFailure1(arg_domains,Goal):-!,on_f_log_fail(Goal),!.




%% beenCaught( :TermGoal) is semidet.
%
% Been Caught.
%
beenCaught(must(Goal)):- !, beenCaught(Goal).
beenCaught((A,B)):- !,beenCaught(A),beenCaught(B).
beenCaught(Goal):- fail, predicate_property(Goal,number_of_clauses(_Count)), clause(Goal,(_A,_B)),!,clause(Goal,Body),beenCaught(Body).
beenCaught(Goal):- catchv(once(Goal),E,(dmsg(caugth(Goal,E)),beenCaught(Goal))),!.
beenCaught(Goal):- traceAll,dmsg(tracing(Goal)),debug,dtrace,Goal.


% = %= :- meta_predicate (with_no_term_expansions(:)).



%% with_no_term_expansions( :Goal) is semidet.
%
% Using No Term Expansions.
%
with_no_term_expansions(Call):-
  locally_hide(term_expansion(_,_),
    locally_hide(term_expansion(_,_),
    locally_hide(goal_expansion(_,_),
      locally_hide(goal_expansion(_,_),Call)))).




%% kill_term_expansion is semidet.
%
% Kill Term Expansion.
%
kill_term_expansion:-
   abolish(term_expansion,2),
   abolish(goal_expansion,2),
   dynamic(term_expansion/2),
   dynamic(goal_expansion/2),
   multifile(term_expansion/2),
   multifile(goal_expansion/2).




%% local_predicate( ?P, :TermARG2) is semidet.
%
% Local Predicate.
%
local_predicate(_,_/0):-!,fail.
local_predicate(_,_/N):-N>7,!,fail.
local_predicate(P,_):-real_builtin_predicate(P),!,fail.
local_predicate(P,_):-predicate_property(P,imported_from(_)),!,fail.
%local_predicate(P,_):-predicate_property(P,file(F)),!,atom_contains666(F,'aiml_'),!.
local_predicate(P,F/N):-functor_safe(P,F,N),!,fail.




%% atom_contains666( ?F, ?C) is semidet.
%
% Atom Contains666.
%
atom_contains666(F,C):- quietly((atom(F),atom(C),sub_atom(F,_,_,_,C))).

% = %= :- meta_predicate (real_builtin_predicate(:)).





%% real_builtin_predicate( :GoalG) is semidet.
%
% Real Builtin Predicate.
%
real_builtin_predicate(G):- predicate_property(G,foreign),!.
real_builtin_predicate(G):- \+ predicate_property(G,defined),!,fail.
%real_builtin_predicate(G):- predicate_property(G,imported_from(W))-> W==system,!.
%real_builtin_predicate(G):- strip_module(G,_,GS),predicate_property(system:GS,BI),BI==built_in,!.
real_builtin_predicate(G):-    \+ predicate_property(G,dynamic),
   predicate_property(G,BI),BI==built_in,
   get_functor(G,F,A),
   M=_,
   %suggest_m(M),current_assertion_module(M)
   if_defined(baseKB:mpred_prop(M,F,A,prologBuiltin),fail),
   !.





%% will_debug_else_throw( :GoalE, :Goal) is semidet.
%
% Will Debug Else Throw.
%
will_debug_else_throw(E,Goal):- dmsg(bugger(will_debug_else_throw(E,Goal))),rtrace,Goal.




%% show_goal_rethrow( ?E, ?Goal) is semidet.
%
% Show Goal Rethrow.
%
show_goal_rethrow(E,Goal):-
   dmsg(bugger(show_goal_rethrow(E,Goal))),
   throw(E).




%% on_prolog_ecall( ?F, ?A, ?Var, ?Value) is semidet.
%
% Whenever Prolog Ecall.
%
on_prolog_ecall(F,A,Var,Value):-
  bin_ecall(F,A,Var,Value),!.
on_prolog_ecall(F,A,Var,Value):-
  default_ecall(IfTrue,Var,Value),
  on_prolog_ecall(F,A,IfTrue,true),!.





%% default_ecall( ?VALUE1, ?VALUE2, ?VALUE3) is semidet.
%
% Default Ecall.
%
default_ecall(asis,call,call).
default_ecall(asis,fake_failure,fail).
default_ecall(asis,error,nocatch).

default_ecall(neverfail,call,call).
default_ecall(neverfail,fail,fake_bindings).
default_ecall(neverfail,error,show_goal_rethrow).

default_ecall(onfailure,call,none).
default_ecall(onfailure,fail,reuse).
default_ecall(onfailure,error,none).

default_ecall(onerror,call,none).
default_ecall(onerror,fail,none).
default_ecall(onerror,error,reuse).





%% on_prolog_ecall_override( ?F, ?A, ?Var, ?SentValue, ?Value) is semidet.
%
% Whenever Prolog Ecall Override.
%
on_prolog_ecall_override(F,A,Var,_SentValue, Value):- on_prolog_ecall(F,A,Var,Value), Value \== reuse,!.
on_prolog_ecall_override(_F,_A,_Var, Value, Value).




%% bin_ecall( ?F, ?A, ?VALUE3, ?VALUE4) is semidet.
%
% Bin Ecall.
%
bin_ecall(F,A,unwrap,true):-member(F/A,[(';')/2,(',')/2,('->')/2,('call')/1]).
bin_ecall(F,A,fail,
 throw(never_fail(F/A))):-
   member(F/A,
    [(retractall)/1]).
bin_ecall(F,A,asis,true):-member(F/A,[('must')/1]).


% :- mpred_trace_childs(with_each/2).





%% with_each( ?UPARAM1, :PRED1VALUE2, :Goal) is semidet.
%
% Using Each.
%
with_each(_,_,Call):-var(Call),!,dtrace,randomVars(Call).
% with_each(BDepth,Wrapper,M:Call):- fail,!, '@'( with_each(BDepth,Wrapper,Call), M).

with_each(_,_,Call):-skipWrapper,!,Call.
with_each(BDepth,Wrapper, (X->Y;Z)):- atom(Wrapper),atom_concat('on_f',_,Wrapper),!,(X -> with_each(BDepth,Wrapper,Y) ; with_each(BDepth,Wrapper,Z)).
with_each(N, Wrapper, Call):- N < 1, !, call(Wrapper,Call).
with_each(BDepth,Wrapper, (X->Y;Z)):- with_each(BDepth,Wrapper,X) -> with_each(BDepth,Wrapper,Y) ; with_each(BDepth,Wrapper,Z).
with_each(PDepth,Wrapper, (X , Y)):- BDepth is PDepth-1, !,(with_each(BDepth,Wrapper,X),with_each(BDepth,Wrapper,Y)).
with_each(PDepth,Wrapper, [X | Y]):- BDepth is PDepth-1, !,(with_each(BDepth,Wrapper,X),!,with_each(BDepth,Wrapper,Y)).
with_each(BDepth,Wrapper,Call):-functor_safe(Call,F,A),prolog_ecall_fa(BDepth,Wrapper,F,A,Call).

% :- mpred_trace_childs(with_each/3).

% fake = true



%% prolog_ecall_fa( ?UPARAM1, :PRED1VALUE2, ?F, ?A, :Goal) is semidet.
%
% Prolog Ecall Functor-arity.
%
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,fake,true),!,
  atom_concat(F,'_FaKe_Binding',FAKE),
  snumbervars(Call,FAKE,0),
  dmsg(error(fake(succeed,Call))),!.

% A=0 , (unwrap = true ; asis = true)
prolog_ecall_fa(_,_,F,0,Call):-
  (on_prolog_ecall(F,0,unwrap,true);on_prolog_ecall(F,0,asis,true)),!,
  call(Call).

% A=1 , (unwrap = true )
prolog_ecall_fa(BDepth,Wrapper,F,1,Call):-
  on_prolog_ecall(F,1,unwrap,true),
  compound(Call),arg(1,Call,Arg),!,
  with_each(BDepth,Wrapper,Arg).

% A>1 , (unwrap = true )
prolog_ecall_fa(BDepth,Wrapper,F,A,Call):-
  on_prolog_ecall(F,A,unwrap,true),!,
  univ_safe_2(Call,[F|OArgs]),
  functor_safe(Copy,F,A),
  univ_safe_2(Copy,[F|NArgs]),
  replace_elements(OArgs,E,with_each(BDepth,Wrapper,E),NArgs),
  call(Copy).

% A>1 , (asis = true )
prolog_ecall_fa(_,_,F,A,Call):-
  on_prolog_ecall(F,A,asis,true),!,
  call(Call).

% each = true
prolog_ecall_fa(BDepth,Wrapper,F,A,Call):-
  (on_prolog_ecall(F,A,each,true);BDepth>0),!,
  BDepth1 is BDepth-1,
  predicate_property(Call,number_of_clauses(_Count)),
  % any with bodies
  clause(Call,NT),NT \== true,!,
  clause(Call,Body),
   with_each(BDepth1,Wrapper,Body).

prolog_ecall_fa(_,Wrapper,_F,_A,Call):-
  call(Wrapper,Call).




%% replace_elements( :TermA, ?A, ?B, :TermB) is semidet.
%
% Replace Elements.
%
replace_elements([],_,_,[]):-!.
replace_elements([A|ListA],A,B,[B|ListB]):-replace_elements(ListA,A,B,ListB).




%% prolog_must_l( ?T) is semidet.
%
% Prolog Must Be Successfull (list Version).
%
prolog_must_l(T):-T==[],!.
prolog_must_l([H|T]):-!,must(H), prolog_must_l(T).
prolog_must_l((H,T)):-!,prolog_must_l(H),prolog_must_l(T).
prolog_must_l(H):-must(H).




%% programmer_error( :GoalE) is semidet.
%
% Programmer Error.
%
programmer_error(E):-dtrace, randomVars(E),dmsg("~q~n",[error(E)]),dtrace,randomVars(E),!,throw(E).



%=  :- mpred_trace_childs(must/1).

% must(C):- ( 1 is random(4)) -> rmust_det(C) ; C.




%% rmust_det( :GoalC) is semidet.
%
% Rmust Deterministic.
%
rmust_det(C):- C *-> true ; dtrace(C).
% rmust_det(C)-  catchv((C *-> true ; debugCallWhy(failed(must(C)),C)),E,debugCallWhy(thrown(E),C)).



%% must_each( :GoalList) is semidet.
%
% Must Be Successfull Each.
%
must_each(List):-var(List),trace_or_throw(var_must_each(List)).
must_each([List]):-!,must(List).
must_each([E|List]):-!,must(E),must_each0(List).



%% must_each0( :TermList) is semidet.
%
% Must Be Successfull Each Primary Helper.
%
must_each0(List):-var(List),trace_or_throw(var_must_each(List)).
must_each0([]):-!.
must_each0([E|List]):-E,must_each0(List).

%=  :- mpred_trace_childs(one_must/2).
:- meta_predicate one_must(:,0,0).



%% one_must( :GoalC1, :GoalC2, :GoalC3) is semidet.
%
% One Must Be Successfull.
%
one_must(C1,C2,C3):-one_must(C1,one_must(C2,C3)).




%% is_deterministic( :TermAtomic) is semidet.
%
% If Is A Deterministic.
%
is_deterministic(once(V)):-var(V),trace_or_throw(is_deterministic(var_once(V))).
is_deterministic(M:G):-atom(M),!,is_deterministic(G).
is_deterministic(Atomic):-atomic(Atomic),!.
is_deterministic(Ground):-ground(Ground),!.
is_deterministic((_,Cut)):-Cut==!.
is_deterministic(_ = _).
is_deterministic(_ =@= _).
is_deterministic(_ =.. _).
is_deterministic(_ == _).
is_deterministic(_ \== _).
is_deterministic(_ \== _).
is_deterministic(atom(_)).
is_deterministic(compound(_)).
is_deterministic(findall(_,_,_)).
is_deterministic(functor_safe(_,_,_)).
is_deterministic(functor_safe(_,_,_)).
is_deterministic(ground(_)).
is_deterministic(nonvar(_)).
is_deterministic(not(_)).
is_deterministic(once(_)).
is_deterministic(var(_)).
%is_deterministic(Call):-predicate_property(Call,nodebug),!.
%is_deterministic(Call):-predicate_property(Call,foreign),!.






%% randomVars( :GoalTerm) is semidet.
%
% Random Variables.
%
randomVars(Term):- random(R), StartR is round('*'(R,1000000)), !,
 ignore(Start=StartR),
 snumbervars(Term, Start, _).




%% prolog_must_not( :Goal) is semidet.
%
% Prolog Must Be Successfull Not.
%
prolog_must_not(Call):-Call,!,dtrace,!,programmer_error(prolog_must_not(Call)).
prolog_must_not(_Call):-!.

% %retractall(E):- retractall(E),functor_safe(E,File,A),dynamic(File/A),!.


% =================================================================================
% Utils
% =================================================================================




%% printPredCount( ?Msg, :GoalPred, ?N1) is semidet.
%
% Print Predicate Count.
%
printPredCount(Msg,Pred,N1):- compound(Pred), debugOnFailureEach((arg(_,Pred,NG))),nonvar(NG),!,
  findall(Pred,Pred,LEFTOVERS),length(LEFTOVERS,N1),dmsg(num_clauses(Msg,Pred,N1)),!.

printPredCount(Msg,Pred,N1):-!,functor_safe(Pred,File,A),functor_safe(FA,File,A), predicate_property(FA,number_of_clauses(N1)),dmsg(num_clauses(Msg,File/A,N1)),!.






%% showProfilerStatistics( :GoalFileMatch) is semidet.
%
% Show Profiler Statistics.
%
showProfilerStatistics(FileMatch):-
  statistics(global,Mem), MU is (Mem / 1024 / 1024),
  printPredCount('showProfilerStatistics: '(MU),FileMatch,_N1).



% ===============================================================================================
% UTILS
% ===============================================================================================




%% if_prolog( ?UPARAM1, :GoalG) is semidet.
%
% If Prolog.
%
if_prolog(swi,G):-call(G). % Run B-Prolog Specifics
if_prolog(_,_):-!. % Dont run SWI Specificd or others


% = %= :- meta_predicate (time_call(:)).



%% time_call( :Goal) is semidet.
%
% Time Call.
%
time_call(Call):- gripe_time(0.5,Call).

/*  statistics(runtime,[MSecStart,_]),
  ignore(show_failure(Call)*->),
  statistics(runtime,[MSecEnd,_]),
   MSec is (MSecEnd-MSecStart),
   Time is MSec/1000,
   ignore((Time > 0.5 , dmsg('Time'(Time)=Call))).
*/

% = %= :- meta_predicate (gripe_time(+,0)).
:- export(gripe_time/2).



%% gripe_time( +TooLong, :Goal) is nondet.
%
% Gripe Time.
%

call_for_time(Goal,ElapseCPU,ElapseWALL,Success):-
   statistics(cputime,StartCPU0),statistics(walltime,[StartWALL0,_]),
   My_Starts = start(StartCPU0,StartWALL0),
   (Goal*->Success=true;Success=fail),
   statistics(cputime,EndCPU),statistics(walltime,[EndWALL,_]),
   arg(1,My_Starts,StartCPU), ElapseCPU is EndCPU-StartCPU,nb_setarg(1,My_Starts,EndCPU),
   arg(2,My_Starts,StartWALL), ElapseWALL is  (EndWALL-StartWALL)/1000,nb_setarg(2,My_Starts,EndWALL).

gripe_time(_TooLong,Goal):- current_prolog_flag(runtime_speed,0),!,Goal.
gripe_time(_TooLong,Goal):- current_prolog_flag(runtime_debug,0),!,Goal.
gripe_time(_TooLong,Goal):- current_prolog_flag(runtime_debug,1),!,Goal.
% gripe_time(_TooLong,Goal):- \+ current_prolog_flag(runtime_debug,3),\+ current_prolog_flag(runtime_debug,2),!,Goal.
gripe_time(TooLong,Goal):-
 call_for_time(Goal,ElapseCPU,ElapseWALL,Success),
 (ElapseCPU>TooLong -> wdmsg(gripe_CPUTIME(Success,warn(ElapseCPU>TooLong),Goal)) ;
   (ElapseWALL>TooLong -> wdmsg(gripe_WALLTIME(Success,warn(ElapseWALL>TooLong),Goal,cputime=ElapseCPU)) ;
     true)),
  Success.



%% cleanup_strings is semidet.
%
% Cleanup Strings.
%
cleanup_strings:-!.
cleanup_strings:-garbage_collect_atoms.



%=========================================
% Module Utils
%=========================================


:- export(loading_module/1).
:- module_transparent(loading_module/1).
:- export(loading_module/2).
:- module_transparent(loading_module/2).
:- export(show_module/1).
:- module_transparent(show_module/1).




%% loading_module( ?M, ?U) is semidet.
%
% Loading Module.
%
:- module_transparent(loading_module/2).
loading_module(M,Why):- quietly(loading_module0(M,Why)).

:- module_transparent(loading_module0/2).
:- export(loading_module0/2).
loading_module0(M,use_module(U)):- if_defined(parent_goal(_:catch(M:use_module(U),_,_),_)).
loading_module0(M,ensure_loaded(U)):- if_defined(parent_goal(_:catch(M:ensure_loaded(U),_,_),_)).
loading_module0(M,consult(F)):- if_defined(parent_goal(_:'$consult_file_2'(F,M,_,_,_),_)).
loading_module0(M,source_location(F)):- source_location(F,_),source_file_property(F,module(M)).
loading_module0(M,file(F)):- prolog_load_context(file,F),source_file_property(F,module(M)).
loading_module0(M,source(F)):- prolog_load_context(source,F),source_file_property(F,module(M)).
loading_module0(M,strip_module):- strip_module(_,M,_).
loading_module0(M,prolog_load_context):- prolog_load_context(module,M).
loading_module0(M,stream_property(F)):- stream_property(_X,file_name(F)),source_file_property(F,module(M)).
loading_module0(M,source_context_module):- source_context_module(M).





%% prolog_current_frames( ?Each) is semidet.
%
% Prolog Current Frames.
%
prolog_current_frames(Each):- prolog_current_frame(Frame),prolog_current_frame_or_parent(Frame,Each).



%% prolog_current_frame_or_parent( ?Frame, ?Each) is semidet.
%
% Prolog Current Frame Or Parent.
%
prolog_current_frame_or_parent(Frame,Each):- Each=Frame;
  (prolog_frame_attribute(Frame,parent,Parent),prolog_current_frame_or_parent(Parent,Each)).

:- module_transparent(caller_module/1).



%% caller_module( ?Module) is semidet.
%
% Caller Module.
%
caller_module(Module):-caller_module2(Module,v(function_expansion,func,user,'$toplevel','$apply','$expand')).



%% caller_module( ?Module, ?Skipped) is semidet.
%
% Hook To [t_l:caller_module/2] For Module Logicmoo_util_bugger.
% Caller Module.
%
caller_module2(Module,Skipped):- module_stack(Module,_), compound(Skipped), \+ arg(_,Skipped,Module).

:- module_transparent(module_stack/2).



%% module_stack( ?M, ?VALUE2) is semidet.
%
% Module Stack.
%
module_stack(M,strip_module):- strip_module(_,M,_).
module_stack(M,prolog_load_context):- prolog_load_context(module, M).
module_stack(M,'$current_typein_module'):- '$current_typein_module'(M).
module_stack(M,of):- predicate_property(M:of(_,_),imported_from(func)).
module_stack(M,frame):- prolog_current_frames(Each), prolog_frame_attribute(Each,context_module,M).





%% loading_module( ?M) is semidet.
%
% Loading Module.
%
loading_module(M):- (((loading_module(M,_),M\=user));M=user),!.




%% show_module( ?W) is semidet.
%
% Show Module.
%
show_module(W):-dmsg("<!--:~w",[W]),ignore((show_call(why,(loading_module(_,_))),fail)),dmsg("~w:-->",[W]).



% ========================================================================================
% Some prologs have a printf() tCol predicate.. so I made up fmtString/fmt in the Cyc code that calls the per-prolog mechaism
% in SWI it''s formzat/N and sformat/N
% ========================================================================================
:- dynamic(isConsoleOverwritten_bugger/0).





%% fmtString( ?X, ?Y, ?Z) is semidet.
%
% Format String.
%
fmtString(X,Y,Z):-sformat(X,Y,Z).



%% fmtString( ?Y, ?Z) is semidet.
%
% Format String.
%
fmtString(Y,Z):-sformat(Y,Z).




%% saveUserInput is semidet.
%
% Save User Input.
%
saveUserInput:-retractall(isConsoleOverwritten_bugger),flush_output.



%% writeSavedPrompt is semidet.
%
% Write Saved Prompt.
%
writeSavedPrompt:-not(isConsoleOverwritten_bugger),!.
writeSavedPrompt:-flush_output.



%% writeOverwritten is semidet.
%
% Write Overwritten.
%
writeOverwritten:-isConsoleOverwritten_bugger,!.
writeOverwritten:-assert(isConsoleOverwritten_bugger).




%% writeErrMsg( ?Out, ?E) is semidet.
%
% Write Err Msg.
%
writeErrMsg(Out,E):- message_to_string(E,S),fmt(Out,'<cycml:error>~s</cycml:error>\n',[S]),!.



%% writeErrMsg( ?Out, ?E, ?Goal) is semidet.
%
% Write Err Msg.
%
writeErrMsg(Out,E,Goal):- message_to_string(E,S),fmt(Out,'<cycml:error>goal "~q" ~s</cycml:error>\n',[Goal,S]),!.



%% writeFileToStream( ?Dest, ?Filename) is semidet.
%
% Write File Converted To Stream.
%
writeFileToStream(Dest,Filename):-
    catchv((
    open(Filename,'r',Input),
    repeat,
        get_code(Input,Char),
        put(Dest,Char),
    at_end_of_stream(Input),
    close(Input)),E,
    fmt('<cycml:error goal="~q">~w</cycml:error>\n',[writeFileToStream(Dest,Filename),E])).




% =================================================================================
% Utils
% =================================================================================
% test_call(G):-writeln(G),ignore(once(catchv(G,E,writeln(E)))).




%% debugFmtList( ?ListI) is semidet.
%
% Debug Format List.
%
debugFmtList(ListI):-quietly((copy_term(ListI,List),debugFmtList0(List,List0),randomVars(List0),dmsg(List0))),!.



%% debugFmtList0( :TermA, :TermB) is semidet.
%
% Debug Format List Primary Helper.
%
debugFmtList0([],[]):-!.
debugFmtList0([A|ListA],[B|ListB]):-debugFmtList1(A,B),!,debugFmtList0(ListA,ListB),!.




%% debugFmtList1( ?Value, ?Value) is semidet.
%
% Debug Format List Secondary Helper.
%
debugFmtList1(Value,Value):-var(Value),!.
debugFmtList1(Name=Number,Name=Number):-number(Number).
debugFmtList1(Name=Value,Name=Value):-var(Value),!.
debugFmtList1(Name=Value,Name=(len:Len)):-copy_term(Value,ValueO),append(ValueO,[],ValueO),is_list(ValueO),length(ValueO,Len),!.
debugFmtList1(Name=Value,Name=(F:A)):-functor_safe(Value,F,A).
debugFmtList1(Value,shown(Value)).

% ===============================================================================================
% unlistify / listify
% ===============================================================================================




%% unlistify( ?L, ?L) is semidet.
%
% Unlistify.
%
unlistify([L],O):-nonvar(L),unlistify(L,O),!.
unlistify(L,L).




%% listify( ?OUT, ?OUT) is semidet.
%
% Listify.
%
listify(OUT,OUT):-not(not(is_list(OUT))),!.
listify(OUT,[OUT]).





%% traceIf( :Goal) is semidet.
%
%  Trace if.
%
traceIf(_Call):-!.
traceIf(Call):-ignore((Call,dtrace)).

%getWordTokens(WORDS,TOKENS):-concat_atom(TOKENS,' ',WORDS).
%is_string(S):- pce_expansion:is_string(S).




% :-(forall(current_predicate(FA),mpred_trace_nochilds(FA))).
% hide this module from tracing
% :-(forall(current_predicate(logicmoo_util_strings:FA),mpred_trace_nochilds(logicmoo_util_strings:FA))).




%% module_hotrace( ?M) is semidet.
%
% Module Ho Trace.
%
module_hotrace(M):- forall(predicate_property(P,imported_from(M)),mpred_trace_nochilds(M:P)).



% = %= :- meta_predicate (test_tl(1,+)).



%% test_tl( :PRED1Pred, +Term) is semidet.
%
% Test Thread Local.
%
test_tl(Pred,Term):-call(Pred,Term),!.
test_tl(Pred,Term):-compound(Term),functor_safe(Term,F,_),call(Pred,F),!.

% = %= :- meta_predicate (test_tl(+)).



%% test_tl( +C) is semidet.
%
% Test Thread Local.
%
test_tl(M:C):-!,call(M:C).
test_tl(C):-functor(C,F,A),test_tl(C,F,A).

% = %= :- meta_predicate (test_tl(+,+,+)).



%% test_tl( +C, +F, +A) is semidet.
%
% Test Thread Local.
%
test_tl(C,F,A):-current_predicate(baseKB:F/A),call(baseKB:C).
test_tl(C,F,A):-current_predicate(t_l:F/A),call(t_l:C).
test_tl(C,F,A):-current_predicate(t_l_global:F/A),call(t_l_global:C).


% asserta_if_ground(_):- !.



%% asserta_if_ground( ?G) is semidet.
%
% Asserta If Ground.
%
asserta_if_ground(G):- ground(G),asserta(G),!.
asserta_if_ground(_).


% =====================================================================================================================
:- module_hotrace(user).
% =====================================================================================================================

% :- ignore((source_location(File,_Line),module_property(M,file(File)),!,forall(current_predicate(M:F/A),mpred_trace_childs(M:F/A)))).

% :- mpred_trace_childs(must/1).
% :- mpred_trace_childs(must/2).
% :- mpred_trace_childs(must_flag/3).

% though maybe dtrace



%% default_dumptrace( ?VALUE1) is semidet.
%
% Default Dump Trace.
%
default_dumptrace(dtrace).

:- thread_local(is_pushed_def/3).

% = %= :- meta_predicate (push_def(:)).



%% push_def( ?Pred) is semidet.
%
% Push Def.
%
push_def(Pred):-must((get_functor(Pred,F,A),prolog_load_context(file,CurrentFile),
   functor_safe(Proto,F,A))),must(forall(clause(Proto,Body),is_pushed_def(CurrentFile,Proto,Body))),!.

:- meta_predicate(pop_def(:)).



%% pop_def( ?Pred) is semidet.
%
% Pop Def.
%
pop_def(Pred):-must((get_functor(Pred,F,A),prolog_load_context(file,CurrentFile),
   functor_safe(Proto,F,A))),forall(retract(is_pushed_def(CurrentFile,Proto,Body)),assertz((Proto:-Body))),!.





%% show_and_do( :GoalC) is semidet.
%
% Show And Do.
%
show_and_do(C):-wdmsg(show_and_do(C)),!,dtrace,C.




:- module_transparent(nth_pi/2).
:- module_transparent(nth_goal/2).
:- module_transparent(nth_frame/3).
:- module_transparent(nth_frame_attribute/5).



%% nth_pi( ?Nth, ?Value) is semidet.
%
% Nth Predicate Indicator.
%
nth_pi(Nth, Value):- prolog_current_frame(Frame), nth_frame_attribute(Nth,-1, Frame, predicate_indicator, Value).



%% nth_goal( ?Nth, ?Value) is semidet.
%
% Nth Goal.
%
nth_goal(Nth, Value):- prolog_current_frame(Frame), nth_frame_attribute(Nth,-1, Frame, goal, Value).



%% nth_frame( ?Nth, ?Key, ?Value) is semidet.
%
% Nth Frame.
%
nth_frame(Nth, Key, Value):- prolog_current_frame(Frame), nth_frame_attribute(Nth,-1, Frame, Key, Value).



%% nth_frame_attribute( ?Nth, ?NthIn, ?Frame, ?Key, ?Value) is semidet.
%
% Nth Frame Attribute.
%
nth_frame_attribute(Nth,NthIn, Frame, Key, Value):-
 quietly((
   (NthIn>=0,Nth=NthIn,prolog_frame_attribute(Frame, Key, Value));
   ((prolog_frame_attribute(Frame, parent, ParentFrame),
     NthNext is NthIn + 1, nth_frame_attribute(Nth,NthNext, ParentFrame, Key, Value))))).




%% in_file_expansion is semidet.
%
% In File Expansion.
%
in_file_expansion :- nth_pi(LF,_:'$load_file'/_),nth_pi(TL,'$toplevel':_/0),!,LF<TL,
  (nth_pi(ED,_:'$execute_directive_3'/_)-> (LF<ED) ; true).




%% in_file_directive is semidet.
%
% In File Directive.
%
in_file_directive :- nth_pi(LF,_:'$load_file'/_),nth_pi(TL,'$toplevel':_/0),!,LF<TL,
  (nth_pi(ED,_:'$execute_directive_3'/_)-> (LF>ED) ; false).




%% in_toplevel is semidet.
%
% In Toplevel.
%
in_toplevel :- nth_pi(LF,_:'$load_file'/_),nth_pi(TL,'$toplevel':_/0),!,LF>TL,
  (nth_pi(ED,_:'$execute_directive_3'/_)-> (ED>TL) ; true).



:- dynamic(did_ref_job/1).



%% do_ref_job( :GoalBody, ?Ref) is semidet.
%
% Do Ref Job.
%
do_ref_job(_Body,Ref):-did_ref_job(Ref),!.
do_ref_job(Body ,Ref):-asserta(did_ref_job(Ref)),!,show_call(why,Body).


% bugger_prolog_exception_hook(error(syntax_error(operator_expected),_),_,_,_).



%% bugger_prolog_exception_hook( ?Info, ?VALUE2, ?VALUE3, ?VALUE4) is semidet.
%
% Logic Moo Debugger Prolog Exception Hook.
%
bugger_prolog_exception_hook(Info,_,_,_):- bugger_error_info(Info),!, dumpST,dmsg(prolog_exception_hook(Info)), dtrace.




%% bugger_error_info( ?C) is semidet.
%
% Logic Moo Debugger Error Info.
%
bugger_error_info(C):-contains_var(type_error,C).
bugger_error_info(C):-contains_var(instantiation_error,C).
bugger_error_info(C):-contains_var(existence_error(procedure,_/_),C).



% Installs exception reporter.
:- multifile(user:prolog_exception_hook/4).

:- dynamic(user:prolog_exception_hook/4).

% Writes exceptions with stacktrace into stderr.
% Fail/0 call at the end allows the exception to be
% processed by other hooks too.



%% disabled_this is semidet.
%
% Disabled This.
%
disabled_this:- asserta((user:prolog_exception_hook(Exception, Exception, Frame, _):-
 \+ current_prolog_flag(no_debug_ST,true),
 set_prolog_flag(no_debug_ST,true),
 get_thread_current_error(ERR),
    (   Exception = error(Term) ;   Exception = error(Term, _)),
    Term \= type_error(number,_),
    Term \= type_error(character_code,_),
    Term \= type_error(character,_),
    Term \= type_error(text,_),
    Term \= syntax_error(_),
    Term \= existence_error(procedure,iCrackers1),
    prolog_frame_attribute(Frame,parent,PFrame),
    prolog_frame_attribute(PFrame,goal,Goal),
    format(ERR, 'Error ST-Begin: ~p', [Term]), nl(ERR),
    ignore((lmcache:thread_current_input(main,In),see(In))),
    dumpST,

    dtrace(Goal),
    format(ERR, 'Error ST-End: ~p', [Term]), nl(ERR),
    nl(ERR), fail)),
    set_prolog_flag(no_debug_ST,false).

% :-disabled_this.

:- dynamic(baseKB:no_buggery/0).
% show the warnings origins
:- multifile(user:message_hook/3).
:- dynamic(user:message_hook/3).
:- thread_local(tlbugger:no_buggery_tl/0).


:- '$hide'(rtrace:maybe_leash/1).
% :- '$hide'('$syspreds':visible/1).
% :- '$hide'('$syspreds':leash/1).
% :- '$hide'(visible/1).
% :- '$hide'(notrace/0).
% :- '$hide'(quietly/1).
% :- '$hide'(dtrace/0).
% :-'$set_predicate_attribute'(!, trace, 1).

% :-hideTrace.

%:-module(user).
%:-prolog.

:- retract(double_quotes_was(WAS)),set_prolog_flag(double_quotes,WAS).
% :- mpred_trace_none(locally/2).
% :- '$set_predicate_attribute'(locally(_,_), hide_childs, 0).


% :- '$hide'(tlbugger:_/_).
% :- '$hide'(tlbugger:A/0).

:-'$hide'(system:notrace/1).



/*

must_det(Level,Goal) :- Goal,
  (deterministic(true) -> true ;
    (print_message(Level, assertion_failed(deterministic, Goal)),
       (member(Level,[informational,warn]) -> ! ; assertion_failed(deterministic, Goal)))).



*/

% :- module_property(user, exports(List)),mpred_trace_childs(List).

% :- must((source_context_module(X),!,X==user)).
% :- must(('$set_source_module'(X,X),!,X==user)).

:- '$set_predicate_attribute'(t_l:dont_varname, trace, 0).
:- swi_system_utilities:unlock_predicate(system:true/0).
:- '$set_predicate_attribute'(system:true, trace, 0).
:- swi_system_utilities:lock_predicate(system:true/0).

%

:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).



:- export(logicmoo_bugger_loaded/0).




%% logicmoo_bugger_loaded is semidet.
%
% Logicmoo Logic Moo Debugger Loaded.
%
logicmoo_bugger_loaded.

% :- source_location(S,_),prolog_load_context(module,M),forall(source_file(M:H,S),(functor(H,F,A),M:module_transparent(M:F/A),M:export(M:F/A))).
% :- source_location(S,_),forall(source_file(H,S),(functor(H,F,A),logicmoo_util_bugger:export(logicmoo_util_bugger:F/A),logicmoo_util_bugger:module_transparent(logicmoo_util_bugger:F/A))).


% :- all_module_predicates_are_transparent.
% :- module_predicates_are_exported.
% :- module_meta_predicates_are_transparent(user).
% :- all_module_predicates_are_transparent(logicmoo_util_catch).


% :- mpred_trace_childs(prolog_ecall_fa/5).
% :- mpred_trace_childs(with_each/3).

:- fixup_module_exports_now.

