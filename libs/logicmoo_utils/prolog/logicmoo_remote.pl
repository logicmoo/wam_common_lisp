/* Part of LogicMOO Base Logicmoo Path Setups
% ===================================================================
    File:         'logicmoo_util_library.pl'
    Purpose:       To load the logicmoo libraries as needed
    Contact:       $Author: dmiles $@users.sourceforge.net ;
    Version:       'logicmoo_util_library.pl' 1.0.0
    Revision:      $Revision: 1.7 $
    Revised At:    $Date: 2002/07/11 21:57:28 $
    Author:        Douglas R. Miles
    Maintainers:   logicmoo
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    SCM:           https://github.com/logicmoo/PrologMUD/tree/master/pack/logicmoo_base
    Copyleft:      1999-2015, LogicMOO Prolog Extensions
    License:       Lesser GNU Public License
% ===================================================================
*/

:- module(logicmoo_remote,
          [ test_pengines/0,
            pengine_server/0                    % start server
          ]).

:- debug(pengine(delay)).
% run pengine server for remote tests in a separate process.
% :- debug(pengine(external_server)).

% the regular things we need for testing.
:- use_module(library(plunit)).
%:- use_module(library(lists)).
:- use_module(library(debug)).
:- use_module(library(pengines)).
:- use_module(library(pengines_sandbox)).
:- use_module(library(option)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_files)).

/** <module> Test suite for pengines
*/

test_pengines :-
    run_tests([ local_pengines,
                remote_pengines,
                application
              ]).

% :- debug(pengine(_)).

:- begin_tests(local_pengines).

test(simple, Results = [a,b,c]) :-
    pengine_create(
        [ src_text("p(a). p(b). p(c).")
        ]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(chunk, Results = [a,b,c]) :-
    pengine_create(
        [ src_text("p(a). p(b). p(c).")
        ]),
    collect(X, p(X), Results, [chunk(2)]),
    assertion(no_more_pengines).
test(chunk2, Results = [a,b,c]) :-
    pengine_create(
        [ src_text("p(a). p(b). p(c).")
        ]),
    collect_state(X, p(X), State, [chunk(2), next(2)]),
    Results = State.results,
    assertion(State.replies = 2),
    assertion(no_more_pengines).
test(stop, Results = [a,b]) :-
    pengine_create(
        [ src_text("p(a). p(b). p(c).")
        ]),
    collect(X, p(X), Results, [stop_after(2)]),
    assertion(no_more_pengines).
test(two, Sorted = [a,b,c,d,e,f]) :-
    pengine_create(
        [ src_text("p(a). p(b). p(c).")
        ]),
    pengine_create(
        [ src_text("p(d). p(e). p(f).")
        ]),
    collect(X, p(X), Results, []),
    msort(Results, Sorted),
    assertion(no_more_pengines).
test(alias, Name == pippi) :-
    pengine_create(
        [ alias(pippi),
          id(Id)
        ]),
    pengine_property(Id, alias(Name)),
    assertion(( pengine_property(Id, thread(Thread)),
                (   thread_property(Thread, alias(ThreadAlias))
                ->  ThreadAlias \== Name
                ;   true
                ))),
    collect(_, fail, Results, []),
    assertion(Results == []),
    assertion(no_more_pengines).
test(ask_simple, Results = [a,b,c]) :-
    pengine_create(
        [ ask(p(X)),
          template(X),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(Results, []),
    assertion(no_more_pengines).
test(ask_fail, Results = []) :-
    pengine_create(
        [ ask(p(X)),
          template(X),
          src_text("p(_) :- fail.")
        ]),
    collect(Results, []),
    assertion(no_more_pengines).

:- end_tests(local_pengines).

:- begin_tests(remote_pengines,
               [ setup(pengine_server(_URL)),
                 cleanup(stop_pengine_server)
               ]).

test(simple, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(chunk, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(X, p(X), Results, [chunk(2)]),
    assertion(no_more_pengines).
test(stop, Results = [a,b]) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(X, p(X), Results, [stop_after(2)]),
    assertion(no_more_pengines).
test(two, Sorted = [a,b,c,d,e,f]) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    pengine_create(
        [ server(Server),
          src_text("p(d). p(e). p(f).")
        ]),
    collect(X, p(X), Results, []),
    msort(Results, Sorted),
    assertion(no_more_pengines).
test(rpc_det, Xs == [1]) :-
    pengine_server(Server),
    findall(X, pengine_rpc(Server,
                           X = 1,
                           []),
            Xs),
    assertion(no_more_pengines).
test(rpc_all, Xs == [1,2,3]) :-
    pengine_server(Server),
    findall(X, pengine_rpc(Server,
                           member(X, [1,2,3]),
                           []),
            Xs),
    assertion(no_more_pengines).
test(rpc_first, X == 1) :-
    pengine_server(Server),
    pengine_rpc(Server,
                member(X, [1,2,3]),
                []),
    !,
    assertion(no_more_pengines).
test(rpc_fail, true) :-
    pengine_server(Server),
    \+ pengine_rpc(Server,
                   fail,
                   []),
    assertion(no_more_pengines).
test(pengine_and_rpc, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    findall(R, pengine_rpc(Server, member(R, [1,2,3]), []), Rs),
    assertion(Rs == [1,2,3]),
    collect(X, p(X), Results, []),
    assertion(no_more_pengines).
test(simple_app, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          application(papp)
        ]),
    collect(X, p1(X), Results, []),
    assertion(no_more_pengines).
test(noapp, error(existence_error(pengine_application, nopapp))) :-
    pengine_server(Server),
    pengine_create(
        [ server(Server),
          application(nopapp)
        ]),
    collect(X, p1(X), _Results, []),
    assertion(no_more_pengines).
test(ask_simple, Results = [a,b,c]) :-
    pengine_server(Server),
    pengine_create(
        [ ask(p(X)),
          template(X),
          server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(Results, []),
    assertion(no_more_pengines).
test(ask_simple_no_template, Results = [p(a),p(b),p(c)]) :-
    pengine_server(Server),
    pengine_create(
        [ ask(p(_X)),
          server(Server),
          src_text("p(a). p(b). p(c).")
        ]),
    collect(Results, []),
    assertion(no_more_pengines).
test(rpc_nested, Xs == [1,2,3]) :-
    pengine_server(Server),
    findall(X, pengine_rpc(Server,
                           pengine_rpc(Server,
                                       member(X, [1,2,3]),
                                       []),
                           []),
            Xs),
    assertion(no_more_pengines).

:- end_tests(remote_pengines).

:- begin_tests(application).

test(simple, Results = [a,b,c]) :-
    pengine_create(
        [ application(papp)
        ]),
    collect(X, p1(X), Results, []),
    assertion(no_more_pengines).
test(self, true) :-
    pengine_create([ application(papp)
                   ]),
    collect(X, pengine_self(X), Results, []),
    Results = [Self],
    assertion(atom(Self)),
    assertion(no_more_pengines).
test(noapp, error(existence_error(pengine_application, nopapp))) :-
    pengine_create(
        [ application(nopapp)
        ]),
    collect(X, p1(X), _Results, []),
    assertion(no_more_pengines).

:- end_tests(application).


                 /*******************************
                 *          APPLICATION         *
                 *******************************/

:- pengine_application(papp).
:- use_module(papp:library(pengines)).

papp:p1(a).
papp:p1(b).
papp:p1(c).


                 /*******************************
                 *           UTILITIES          *
                 *******************************/

%!  collect(+Template, :Goal, -Results, +Options)
%
%   Collect answers from all pengines in Results.  Options:
%
%     * stop_after(N)
%     Stop collecting results after N answers
%     * next(N)
%     Passed to pengine_next/2
%
%   Other options are passed to pengine_ask/3.

collect(Results, Options) :-
    collect(-, -, Results, Options).

collect(Template, Goal, Results, Options) :-
    collect_state(Template, Goal, State, Options),
    Results = State.results.

collect_state(Template, Goal, State, Options) :-
    partition(next_option, Options, NextOptions, Options1),
    partition(state_option, Options1, StateOptions, AskOptions),
    dict_create(State, state,
                [ results([]),
                  replies(0),
                  options(_{ask:AskOptions, next:NextOptions})
                | StateOptions
                ]),
    pengine_event_loop(collect_handler(Template, Goal, State), []).

state_option(stop_after(_)).
next_option(next(_)).

collect_handler(Template, Goal, State, create(Id, _)) :-
    Goal \== (-),
    pengine_ask(Id, Goal, [template(Template)|State.options.ask]).
collect_handler(_, _, State, success(Id, Values, More)) :-
    append(State.results, Values, R1),
    b_set_dict(results, State, R1),
    Replies1 is State.replies+1,
    b_set_dict(replies, State, Replies1),
    (   StopAfter = State.get(stop_after),
        length(R1, Collected),
        Collected >= StopAfter
    ->  pengine_destroy(Id)
    ;   More == true
    ->  pengine_next(Id, State.options.next)
    ;   true
    ).

%!  no_more_pengines is semidet.
%
%   True if there are no more living pengines. Need to wait a little
%   because they die asynchronously.

no_more_pengines :-
    (   true
    ;   between(1, 10, _),
        sleep(0.01)
    ),
    \+ pengines:current_pengine(_,_,_,_,_,_),
    \+ pengines:child(_,_),
    !.


                 /*******************************
                 *          HTTP SERVER         *
                 *******************************/

:- pengine_sandbox:use_module(library(pengines)).
:- http_handler(/, http_reply_from_files(web, []), [prefix]).

:- dynamic
    pengine_server_port/1.

pengine_server(URL) :-
    debugging(pengine(external_server)),
    !,
    start_external_server(URL).
pengine_server(URL) :-
    local_server(URL).

local_server(URL) :-
    (   pengine_server_port(Port)
    ->  true
    ;   http_server(http_dispatch, [port(Port)]),
        asserta(pengine_server_port(Port))
    ),
    format(atom(URL), 'http://localhost:~d', [Port]).

stop_pengine_server :-
    pengine_server_pid(PID),
    !,
    process_kill(PID, hup),
    process_wait(PID, _Status).             % getting status 2??
%       assertion(Status == exit(0)).
stop_pengine_server :-
    retract(pengine_server_port(Port)),
    !,
    http_stop_server(Port, []).
stop_pengine_server.


                 /*******************************
                 *       EXTERNAL SERVER                *
                 *******************************/

:- dynamic pengine_server_pid/1.

start_external_server(URL) :-
    current_prolog_flag(executable, SWIPL),
    process_create(SWIPL,
                   [ '-q', '-f', 'test_pengines.pl',
                     '-g', 'pengine_server'
                   ],
                   [ stdout(pipe(Out)),
                     process(PID)
                   ]),
    read_line_to_string(Out, URL),
    assertion(string_concat('http://', _, URL)),
    asserta(pengine_server_pid(PID)),
    on_signal(hup, _, hangup).

hangup(_Signal) :-
    format(user_error, 'Got hangup~n', []),
    thread_send_message(main, done).

pengine_server :-
    local_server(URL),
    writeln(URL),
    thread_get_message(Done),
    format(user_error, 'Got ~p', [Done]).

%:- discontiguous(logicmoo_utils:'$exported_op'/3).
%:- logicmoo_utils:use_module(library(logicmoo_common)).
:- reexport(library(logicmoo_common)).
:- reexport(library(logicmoo_startup)).

:- use_module(library(pengines)).

test_remote_swi :-
    pengine_create([
        server('http://pengines.swi-prolog.org'),
        src_text("
            q(X) :- p(X).
            p(a). p(b). p(c).
        ")
    ]),
    pengine_event_loop(handle, []).

test_remote_logicmoo :-
    pengine_create([
        server('http://logicmoo.org'),
        src_text("
            q(X) :- p(X).
            p(a). p(b). p(c).
        ")
    ]),
    pengine_event_loop(handle, []).

test_remote_logicmoo :-
    pengine_create([
        server('http://logicmoo.org'),
        src_text("
            q(X) :- p(X),assert(r(X)).
            p(a). p(b). p(c).
        ")
    ]),
    pengine_event_loop(handle, []).

handle(create(ID, _)) :-
    pengine_ask(ID, q(_X), []).
handle(success(ID, [X], false)) :-
    writeln(X),
    pengine_destroy(ID, []).

handle(success(ID, [X], true)) :-
    writeln(X),
    pengine_next(ID, []).


test_remote_logicmoo2 :- 
  ask_remote_logicmoo(member(X,[1,2,3])).

ask_remote_logicmoo(Goal) :-
    pengine_create([
        server('http://logicmoo.org'),
        src_text("
            q(X) :- p(X),assert(r(X)).
            p(a). p(b). p(c).
        ")
    ]),
   term_variables(Goal,Vs),
   HND = handle_goal(Goal,Vs),
   pengine_event_loop(HND, []).

handle_goal(_,_,Event):- dmsg(Event),fail.
handle_goal(Goal,_,create(ID, _)) :-
    pengine_ask(ID, Goal, []).
handle_goal(Goal,Vs,success(ID, Vs, true)) :-
    writeln(Goal),
    pengine_next(ID, []).
handle_goal(Goal,Vs,success(ID, Vs, false)) :-
    writeln(Goal),
    pengine_destroy(ID, []).

:- test_remote_logicmoo2.
