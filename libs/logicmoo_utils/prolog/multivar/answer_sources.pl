/*
Answer Sources in Prolog (SWI) - Preview
Answer Sources: Extensions
Copyright (c) 2015 Julio P. Di Egidio
http://julio.diegidio.name/
All rights reserved.
Answer Sources: Extensions
--------------------------
Extends answer sources with few utilities and the basic combinators.
NOTE:
- Predicates in this module do not validate their input.
- Access to predicates in this module is not sychronised.
TODO:
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(SWI 7.2.3)

/*
:- module(nan_kernel_ex,
	[	using_source/4,    % (@, @, -, :) is nondet
		using_sources/4,   % (:, +, -, :) is nondet
		source_first/2,    % (+, ?)       is semidet
		source_enum/2,     % (+, ?)       is nondet
		append_sources/2,  % (+, -)       is det
		compose_sources/2,  % (+, -)       is det

                    source_exists/2,      % (+, ?)          is semidet
		source_open/5,        % (+, +, @, @, -) is det
		source_open/3,        % (@, @, -)       is det
		source_close/1,       % (+)             is det
		source_reset/1,       % (+)             is det
		source_next/2,        % (+, ?)          is semidet
		source_next_begin/1,  % (+)             is det
		source_next_end/2     % (+, ?)          is semidet
	]).
:- reexport('Nan.Kernel').
*/

:- use_module(library(apply)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	using_source  (@AnsPatt, @Goal, -Source, :GUsing)  is nondet
%!	using_sources (:PComb, +Sources, -Source, :GUsing) is nondet
/*
?- using_source(s1, sleep(2), _S1,
   using_source(s2, sleep(2), _S2,
   using_source(s3, sleep(2), _S3,
   using_sources(compose_sources, [_S1, _S2, _S3], _S,
   (   time(source_next(_S, answer(_, the([A1, A2, A3]))))
   ))))).
% 546 inferences, 0.000 CPU in 2.000 seconds (0% CPU, Infinite Lips)
A1 = answer(last, the(s1)),
A2 = answer(last, the(s2)),
A3 = answer(last, the(s3)).
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate
	using_source(+, 0, -, 0).

using_source(AnsP, G, Src, GU) :-
	setup_call_cleanup(
		source_open(AnsP, G, Src),
		GU,
		source_close(Src)
	).

:- meta_predicate
	using_sources(2, +, -, 0).

using_sources(PC, Srcs, Src, GU) :-
	setup_call_cleanup(
		call(PC, Srcs, Src),
		GU,
		source_close(Src)
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	source_first (+Source, -Answer) is det
%!	source_first (+Source, ?Answer) is semidet
%!	
%!	Gets the first answer from a given source.
%!	Resets the source Source, gets the first answer from it, and
%!	unifies the answer with Answer.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_first(Src, Ans) :-
	source_reset(Src),
	source_next(Src, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	source_enum (+Source, -Answer) is multi
%!	source_enum (+Source, ?Answer) is nondet
%!	
%!	Enumerates answers from a given source.
%!	Gets on backtracking answers from the source Source, and unifies
%!	each answer with Answer.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_enum(Src, Ans) :-
	source_next(Src, Ans0),
	source_enum__sel(Src, Ans0, Ans).

source_enum__sel(_, Ans0, Ans) :-
	Ans \= Ans0, !, fail.
source_enum__sel(_, Ans0, Ans0).
source_enum__sel(Src, _, Ans) :-
	source_enum(Src, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	append_sources (+Sources, -Source) is det
%!	
%!	Append combinator over a list of sources.
%!	Creates a new source Source that combines answers from the sources
%!	listed in Sources.  Source gets an answer from the first source in
%!	Sources that is not closed, defaulting to failure when all the
%!	sources are closed (or the list is empty).  Resettings Source
%!	resets all Sources (that are not closed).
/*
?- source_open(I, between(1, 2, I), S1),
   source_open(I, between(3, 4, I), S2).
S1 = source(t0, 156),
S2 = source(t0, 157).
?- append_sources([$S1, $S2], S).
S = source(t1, 158).
?- source_close($S1).
true.
?- findall(A, source_enum($S, answer(_, the(A))), As).
As = [3, 4].
?- findall(A, source_enum($S, answer(_, the(A))), As).
As = [].
?- source_reset($S).
true.
?- findall(A, source_enum($S, answer(_, the(A))), As).
As = [3, 4].
?- source_close($S2),
   source_reset($S).
true.
?- findall(A, source_enum($S, answer(_, the(A))), As).
As = [].
?- source_close($S).
true.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

append_sources(Srcs, Src) :-
	PReset = append_s__reset,
	PNext = append_s__next,
	maplist(append_s__ts0__do, Srcs, Ts0),
	source_open(Srcs, Ts0, PReset, PNext, Src).

append_s__reset(Srcs, Ts0, Ts1) :-
	maplist(append_s__reset__do, Srcs, Ts0, Ts1).

append_s__next(Srcs, Ts0, Ts1, Ans) :-
	foldl(append_s__next__do, Srcs, Ts0, Ts1, Ans, Ans),
	(var(Ans) -> Ans = answer(fail, no) ; true).

append_s__ts0__do(_, t).

append_s__reset__do(Src, t, t) :-
	source_exists(Src, true), !,
	source_reset(Src).
append_s__reset__do(_, _, f).

append_s__next__do(_, T0, T0, Ans, Ans) :-
	nonvar(Ans), !.
append_s__next__do(Src, t, t, Ans, Ans) :-
	source_exists(Src, true), !,
	source_next(Src, Ans).
append_s__next__do(_, _, f, Ans, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	compose_sources (+Sources, -Source) is det
%!	
%!	Compose combinator over a list of sources.
%!	Creates a new source Source that combines answers from the sources
%!	listed in Sources.  In parallel, Source gets one answer from every
%!	one of Sources (if any source is closed, defaults to failure), and
%!	returns a list of the answers so collected.  Resettings Source
%!	resets all Sources (that are not closed).
/*
?- source_open(I, between(1, 2, I), S1),
   source_open(I, between(3, 4, I), S2).
S1 = source(t0, 39),
S2 = source(t0, 40).
?- compose_sources([$S1, $S2], S).
S = source(t1, 41).
?- source_enum($S, answer(_, the(As))).
As = [answer(more, the(1)), answer(more, the(3))] ;
As = [answer(last, the(2)), answer(last, the(4))] ;
As = [answer(fail, no), answer(fail, no)] ;
As = [answer(fail, no), answer(fail, no)] .
?- source_reset($S).
true.
?- source_enum($S, answer(_, the(As))).
As = [answer(more, the(1)), answer(more, the(3))] .
?- source_reset($S1).
true.
?- source_enum($S, answer(_, the(As))).
As = [answer(more, the(1)), answer(last, the(4))] ;
As = [answer(last, the(2)), answer(fail, no)] ;
As = [answer(fail, no), answer(fail, no)] .
?- source_close($S1), source_reset($S).
true.
?- source_enum($S, answer(_, the(As))).
As = [answer(fail, no), answer(more, the(3))] ;
As = [answer(fail, no), answer(last, the(4))] ;
As = [answer(fail, no), answer(fail, no)] .
?- source_close($S2), source_reset($S).
true.
?- source_enum($S, answer(_, the(As))).
As = [answer(fail, no), answer(fail, no)] .
?- source_close($S).
true.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

compose_sources(Srcs, Src) :-
	PReset = compose_s__reset,
	PNext = compose_s__next,
	maplist(compose_s__ts0__do, Srcs, Ts0),
	source_open(Srcs, Ts0, PReset, PNext, Src).

compose_s__reset(Srcs, Ts0, Ts1) :-
	maplist(compose_s__reset__do, Srcs, Ts0, Ts1).

compose_s__next(Srcs, Ts0, Ts1, answer(more, the(Anss))) :-
	maplist(compose_s__next_b__do, Srcs, Ts0, Ts01),
	maplist(compose_s__next_e__do, Srcs, Ts01, Ts1, Anss).

compose_s__ts0__do(_, t).

compose_s__reset__do(Src, t, t) :-
	source_exists(Src, true), !,
	source_reset(Src).
compose_s__reset__do(_, _, f).

compose_s__next_b__do(Src, t, t) :-
	source_exists(Src, true), !,
	source_next_begin(Src).
compose_s__next_b__do(_, _, f).

compose_s__next_e__do(Src, t, t, Ans) :-
	source_exists(Src, true), !,
	source_next_end(Src, Ans).
compose_s__next_e__do(_, _, f, answer(fail, no)).

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Raw
 Nan.Kernel.pl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
*/
/*
Answer Sources in Prolog (SWI) - Preview
Answer Sources
Copyright (c) 2015 Julio P. Di Egidio
http://julio.diegidio.name/
All rights reserved.
Answer Sources
--------------
Answer sources can be seen as generalized iterators, allowing a given
program to control answer production in another. Each answer source
works as a separate Prolog interpreter...
Multithreading => parallelism...
NOTE:
- Predicates in this module do not validate their input.
- Access to predicates in this module is not sychronised.
TODO:
- Redesign in terms of a thread-pool.
- Rewrite to get rid of the global cuts.
- Abstract away cross-cutting concerns:
      (validation?), exceptions, logging, database, id/key gen.
- Remove logging calls with nan_kernel_debug(false).
Main SWI specifics: threads, message queues, global cuts.
*/
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%(SWI 7.2.3)
/*
:- module(nan_kernel,
	[	source_exists/2,      % (+, ?)          is semidet
		source_open/5,        % (+, +, @, @, -) is det
		source_open/3,        % (@, @, -)       is det
		source_close/1,       % (+)             is det
		source_reset/1,       % (+)             is det
		source_next/2,        % (+, ?)          is semidet
		source_next_begin/1,  % (+)             is det
		source_next_end/2     % (+, ?)          is semidet
	]).
*/
:- use_module(library(debug)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	source_exists     (+Source, ?Exists)          is semidet
%!	source_open (+Sources, +State0, @PReset, @PNext, -Source) is det
%!	source_open       (@AnsPatt, @Goal, -Source)  is det
%!	source_close      (+Source)                   is det
%!	source_reset      (+Source)                   is det
%!	source_next       (+Source, ?Answer)          is semidet
%!	source_next_begin (+Source)                   is det
%!	source_next_end   (+Source, ?Answer)          is semidet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_exists(Src, Exists) :-
	source_sid(Src, Sid),
	source_db_exists_(Sid, Exists).

:- meta_predicate
	source_open(+, +, 3, 4, -).

source_open(Srcs, T0, PR, PN, Src) :-
	source_open_(Srcs, T0, PR, PN, Sid),
	source_sid(Src, Sid).

:- meta_predicate
	source_open(+, 0, -).

source_open(AnsP, G, Src) :-
	source_open_(AnsP, G, Sid),
	source_sid(Src, Sid).

source_close(Src) :-
	source_sid(Src, Sid),
	source_close_(Sid).

source_reset(Src) :-
	source_sid(Src, Sid),
	source_reset_(Sid).

source_next(Src, Ans) :-
	source_sid(Src, Sid),
	source_next_(Sid, Ans).

source_next_begin(Src) :-
	source_sid(Src, Sid),
	source_next_begin_(Sid).

source_next_end(Src, Ans) :-
	source_sid(Src, Sid),
	source_next_end_(Sid, Ans).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_open_ (+Srcs, +T0, @PR, @PN, -Sid) is det
%	source_open_ (@AnsP, @G, -Sid)            is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate
	source_open_(+, +, 3, 4, -).

source_open_(Srcs, T0, PR, PN, Sid) :-
	source_new_sid_(t1, Sid),
	source_log_act_(
		(	copy_term([PR, PN], [PR1, PN1]),
			source_db_add_(Sid, t1(Srcs, T0, PR1, PN1))
		), Sid, 'OPEN'
	).

:- meta_predicate
	source_open_(+, 0, -).

source_open_(AnsP, G, Sid) :-
	source_new_sid_(t0, Sid),
	source_log_act_(
		(	source_open__do(Sid, AnsP, G)
		), Sid, 'OPEN'
	).

:- meta_predicate
	source_open__do(+, +, 0).

source_open__do(Sid, AnsP, G) :-
	source_open__pre(Sid, AnsP, G, [Pid, Tid, GExec]),
	source_open__all(Sid, Pid, Tid, GExec, ErrA1),
	(	source_err_(ErrA1, true, _)
	->	source_open__abort(Sid, ErrA2)
	;	true
	),
	source_throw_([ErrA1, ErrA2]),
	source_log_(Sid, 'OPEN', 'OPENED').

:- meta_predicate
	source_open__pre(+, +, 0, -).

source_open__pre(Sid, AnsP, G, [Pid, Tid, GExec]) :-
	copy_term([AnsP, G], [AnsP1, G1]),
	source_sid_key(Sid, Tid),
	atom_concat(Tid, '_p', Pid),
	GExec = source_exec(Sid, AnsP1, G1).

:- meta_predicate
	source_open__all(+, +, +, 0, -).

source_open__all(Sid, Pid, Tid, GExec, ErrA) :-
	source_catch_(
		(	source_db_add_(Sid, t0(Pid, Tid)),
			message_queue_create(_, [alias(Pid)]),
			thread_create(GExec, _, [alias(Tid)])
		), ErrA
	).

source_open__abort(Sid, ErrA) :-
	source_catch_(
		source_close_(Sid), ErrA
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_close_ (+Sid) is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_close_(Sid) :-
	source_sid_type(Sid, t1), !,
	source_log_act_(
		(	source_db_del_(Sid)
		), Sid, 'CLOSE'
	).

source_close_(Sid) :-
	source_db_get_(Sid, t0(Pid, Tid)),
	source_log_act_(
		(	source_close__do(Sid, Pid, Tid)
		), Sid, 'CLOSE'
	).

source_close__do(Sid, Pid, Tid) :-
	source_close__thread(Sid, Tid, StaT, ErrA1),
	source_close__queue(Pid, ErrA2),
	source_close__db(Sid, ErrA3),
	source_throw_([ErrA1, ErrA2, ErrA3]),
	source_log_(Sid, 'CLOSE', '~|CLOSED~8+(StaT = ~w)', [StaT]).

source_close__thread(Sid, Tid, StaT, ErrA) :-
	source_catch_(
		(	(	thread_property(Tid, status(running))
			->	source_msg_send_(Sid, 'CLOSE', Tid, close)
			;	true
			), thread_join(Tid, StaT)
		), ErrA
	).

source_close__queue(Qid, ErrA) :-
	source_catch_(
		message_queue_destroy(Qid), ErrA
	).

source_close__db(Sid, ErrA) :-
	source_catch_(
		source_db_del_(Sid), ErrA
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_reset_      (+Sid)       is det
%	source_next_       (+Sid, ?Ans) is semidet
%	source_next_begin_ (+Sid)       is det
%	source_next_end_   (+Sid, ?Ans) is semidet
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_reset_(Sid) :-
	source_sid_type(Sid, t1), !,
	source_db_get_(Sid, t1(Srcs, T0, PR, _)),
	source_log_act_(
		(	source_reset__do(Srcs, PR, T0, T1),
			source_next__t1_state(Sid, T1)
		), Sid, 'RESET'
	).

source_reset_(Sid) :-
	source_db_get_(Sid, t0(_, Tid)),
	source_log_act_(
		(	source_reset__do(Sid, 'RESET', Tid)
		), Sid, 'RESET'
	).

source_next_(Sid, Ans) :-
	source_sid_type(Sid, t1), !,
	source_db_get_(Sid, t1(Srcs, T0, _, PN)),
	source_log_act_(
		(	source_next__do(Srcs, PN, T0, T1, Ans),
			source_next__t1_state(Sid, T1)
		), Sid, 'NEXT'
	).

source_next_(Sid, Ans) :-
	source_db_get_(Sid, t0(Pid, Tid)),
	source_log_act_(
		(	source_next_begin__do(Sid, 'NEXT', Tid),
			source_next_end__do(Sid, 'NEXT', Pid, Ans)
		), Sid, 'NEXT'
	).

source_next_begin_(Sid) :-
	source_sid_type(Sid, t1), !,
	source_log_act_(
		(	true
		), Sid, 'NEXT_B'
	).

source_next_begin_(Sid) :-
	source_db_get_(Sid, t0(_, Tid)),
	source_log_act_(
		(	source_next_begin__do(Sid, 'NEXT_B', Tid)
		), Sid, 'NEXT_B'
	).

source_next_end_(Sid, Ans) :-
	source_sid_type(Sid, t1), !,
	source_db_get_(Sid, t1(Srcs, T0, _, PN)),
	source_log_act_(
		(	source_next__do(Srcs, PN, T0, T1, Ans),
			source_next__t1_state(Sid, T1)
		), Sid, 'NEXT_E'
	).

source_next_end_(Sid, Ans) :-
	source_db_get_(Sid, t0(Pid, _)),
	source_log_act_(
		(	source_next_end__do(Sid, 'NEXT_E', Pid, Ans)
		), Sid, 'NEXT_E'
	).

source_next__t1_state(Sid, T1) :-
	source_db_get_(Sid, t1(Srcs, T0, PR, PN)),
	(	T1 \== T0
	->	source_db_del_(Sid),
		source_db_add_(Sid, t1(Srcs, T1, PR, PN))
	;	true
	).

:- meta_predicate
	source_reset__do(+, 3, +, -).

source_reset__do(Srcs, PR, T0, T1) :-
	call(PR, Srcs, T0, T1), !.

:- meta_predicate
	source_next__do(+, 4, +, -, ?).

source_next__do(Srcs, PN, T0, T1, Ans) :-
	call(PN, Srcs, T0, T1, Ans1), !, Ans = Ans1.

source_reset__do(Sid, Act, Tid) :-
	source_msg_send_(Sid, Act, Tid, reset).

source_next_begin__do(Sid, Act, Tid) :-
	source_msg_send_(Sid, Act, Tid, next).

source_next_end__do(Sid, Act, Pid, Ans) :-
	source_msg_recv_(Sid, Act, Pid, Msg),
	(	Msg = fail        -> Ans = answer(fail, no)
	;	Msg = last(AnsP)  -> Ans = answer(last, the(AnsP))
	;	Msg = more(AnsP)  -> Ans = answer(more, the(AnsP))
	;	Msg = except(Err) -> throw(Err)
	;	throw(source_error(unknown_message(data, Sid, Msg), _))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	source_exec (+Sid, ?AnsP, :G) is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public
	source_exec/3.

:- meta_predicate
	source_exec(+, ?, 0).

source_exec(Sid, AnsP, G) :-
	source_db_get_(Sid, t0(Pid, Tid)),
	source_log_act_(
		call_cleanup(
			source_exec__loop_0(Sid, Pid, Tid, AnsP, G),
			exception(Err),
			source_msg_send_(Sid, 'EXEC', Pid, except(Err))
		), Sid, 'EXEC'
	).

:- meta_predicate
	source_exec__loop_0(+, +, +, ?, 0).

source_exec__loop_0(Sid, Pid, Tid, AnsP, G) :-
	repeat,
	source_msg_recv_(Sid, 'EXEC', Tid, Msg),
	(	Msg == reset -> fail
	;	Msg == close -> !
	;	Msg == next  -> !,
			source_exec__loop_1(Sid, Pid, Tid, AnsP, G)
	;	throw(source_error(unknown_message(ctrl, Sid, Msg), _))
	).

:- meta_predicate
	source_exec__loop_1(+, +, +, ?, 0).

source_exec__loop_1(Sid, Pid, Tid, AnsP, G) :-
	prolog_current_choice(Loop1),
	repeat,
	prolog_current_choice(Loop2),
	source_exec__loop_2(Sid, Pid, AnsP, G),
	source_exec__recv(Sid, Tid, Loop1, Loop2).

:- meta_predicate
	source_exec__loop_2(+, +, ?, 0).

source_exec__loop_2(Sid, Pid, AnsP, G) :-
	(	call_cleanup(G, Det = true),
		(	Det == true
		->	source_msg_send_(Sid, 'EXEC', Pid, last(AnsP))
		;	source_msg_send_(Sid, 'EXEC', Pid, more(AnsP))
		),
		source_log_(Sid, 'EXEC', '~|CALLED~8+(Det = ~w)', [Det])
	;	repeat,
		source_msg_send_(Sid, 'EXEC', Pid, fail)
	).

source_exec__recv(Sid, Tid, Loop1, Loop2) :-
	source_msg_recv_(Sid, 'EXEC', Tid, Msg),
	(	Msg == next  -> fail
	;	Msg == close -> prolog_cut_to(Loop1)
	;	Msg == reset -> prolog_cut_to(Loop2),
			source_exec__recv(Sid, Tid, Loop1, Loop2)
	;	throw(source_error(unknown_message(ctrl, Sid, Msg), _))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_msg_send_ (+Sid, +Act, +Qid, +Msg) is det
%	source_msg_recv_ (+Sid, +Act, +Qid, -Msg) is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_msg_send_(Sid, Act, Qid, Msg) :-
	thread_send_message(Qid, Msg),
	source_log_msg_(Sid, Act, '>>', Msg).

source_msg_recv_(Sid, Act, Qid, Msg) :-
	thread_get_message(Qid, Msg),
	source_log_msg_(Sid, Act, '<<', Msg).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_catch_ (:GAct, -ErrA)         is det
%	source_throw_ (+ErrAs)               is det
%	source_err_   (+ErrA, -HasErr, -Err) is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate
	source_catch_(0, -).

source_catch_(GAct, ErrA) :-
	catch(
		(	call(GAct),
			HasErr = false
		),
		Err, HasErr = true
	),
	source_err_(ErrA, HasErr, Err).

source_throw_(ErrAs) :-
	source_throw___loop(ErrAs, Errs),
	(	Errs = []    -> true
	;	Errs = [Err] -> throw(Err)
	;	throw(source_error(many_errors, Errs))
	).

source_throw___loop([], []).
source_throw___loop([ErrA| ErrAs], Errs) :-
	source_err_(ErrA, HasErr, Err),
	(	HasErr == true
	->	Errs = [Err| Errs1]
	;	Errs = Errs1
	),
	source_throw___loop(ErrAs, Errs1).

source_err_(ErrA, HasErr, Err) :-
	ErrA = err(HasErr, Err).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_log_act_ (:GAct, +Sid, +Act)         is det
%	source_log_msg_ (+Sid, +Act, +Dir, +QMsg)   is det
%	source_log_     (+Sid, +Act, +Msg1)         is det
%	source_log_     (+Sid, +Act, +Fmt1, +Args1) is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- meta_predicate
	source_log_act_(0, +, +).

source_log_act_(GAct, Sid, Act) :-
	setup_call_cleanup(
		source_log_(Sid, Act, 'Start...'),
		GAct,
		source_log_(Sid, Act, 'Done.')
	).

source_log_msg_(Sid, Act, Dir, QMsg) :-
	(	debugging(nan_kernel)
	->	(	Act == 'EXEC'
		->	(Dir == '>>' -> Typ = data ; Typ = ctrl)
		;	(Dir == '>>' -> Typ = ctrl ; Typ = data)
		),
		Args = [Dir, Typ, QMsg],
		source_log__do(Sid, Act, '~|--~a--~8+(~a) ~w', Args)
	;	true
	).

source_log_(Sid, Act, Msg1) :-
	(	debugging(nan_kernel)
	->	source_log__do(Sid, Act, Msg1, [])
	;	true
	).

source_log_(Sid, Act, Fmt1, Args1) :-
	(	debugging(nan_kernel)
	->	source_log__do(Sid, Act, Fmt1, Args1)
	;	true
	).

source_log__do(Sid, Act, Fmt1, Args1) :-
	get_time(Tm),
	Tm1 is floor(float_fractional_part(Tm / 100) * 100_000),
	format(atom(TM), '~3d', [Tm1]),
	Term = nan_kernel__source(Sid, Act, TM, Fmt1, Args1),
	print_message(informational, Term).

:- multifile
	prolog:message//1.

prolog:message(nan_kernel__source(Sid, Act, TM, Fmt1, Args1)) -->
	{	source_sid_sel_(_, TNum, Id, Sid),
		format(atom(Msg1), Fmt1, Args1),
		Args = [TM, TNum, Id, Act, Msg1]
	},	['~a : source(t~d, ~d) : ~|~w~6+ : ~a'-Args].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%	source_db_exists_ (+Sid, ?Exists) is semidet
%!	source_db_gen     (?Sid, ?Term)   is nondet
%	source_db_add_    (+Sid, +Term)   is det
%	source_db_get_    (+Sid, -Term)   is det
%	source_db_del_    (+Sid)          is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

source_db_exists_(Sid, Exists) :-
	source_sid_key(Sid, Key),
	(	recorded(Key, _)
	->	Exists = true
	;	Exists = false
	).

:- public
	source_db_gen/2.

source_db_gen(Sid, Term) :-
	recorded(Key, Term),
	source_sid_key(Sid, Key).

source_db_add_(Sid, Term) :-
	source_db__val(has_not, Sid, Key),
	recordz(Key, Term).

source_db_get_(Sid, Term) :-
	source_db__val(has, Sid, _, Term, _).

source_db_del_(Sid) :-
	source_db__val(has, Sid, _, _, Ref),
	erase(Ref).

source_db__val(has_not, Sid, Key) :-
	source_sid_key(Sid, Key),
	(	\+ recorded(Key, _)
	->	true
	;	throw(source_error(record_exists_already(Sid, Key), _))
	).
source_db__val(has, Sid, Key, Term, Ref) :-
	source_sid_key(Sid, Key),
	(	recorded(Key, Term, Ref)
	->	true
	;	throw(source_error(record_does_not_exist(Sid, Key), _))
	).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%!	source_sid      (+Source, -Sid)           is semidet
%!	source_sid      (-Source, +Sid)           is det
%!	source_sid_type (+Sid, -Type)             is det
%!	source_sid_key  (+Sid, -Key)              is det
%!	source_sid_key  (-Sid, +Key)              is det
%	source_sid_sel_ (+Type, +TNum, +Id, -Sid) is det
%	source_sid_sel_ (-Type, -TNum, -Id, +Sid) is det
%	source_new_sid_ (+Type, -Sid)             is det
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- public
	source_sid/2,
	source_sid_key/2.

source_sid(source(Type, Id), Sid) :-
	source_sid__do(Type, _, Id, Sid).

source_sid_type(Sid, Type) :-
	source_sid__do(Type, _, _, Sid).

source_sid_key(Sid, Key) :-
	var(Key), !,
	source_sid__do(_, TNum, Id, Sid),
	ACs = [nan_kernel__source__t, TNum, '__', Id],
	atomic_list_concat(ACs, Key).
source_sid_key(Sid, Key) :-
	atom_concat(nan_kernel__source__t, K1, Key),
	sub_atom(K1, 0, 1, _, TVal),
	sub_atom(K1, 3, _, 0, IdVal),
	atom_number(TVal, TNum),
	atom_number(IdVal, Id),
	source_sid__num(type, TNum),
	source_sid__num(id, Id),
	source_sid_sel_(_, TNum, Id, Sid).

source_sid__do(Type, TNum, Id, Sid) :-
	source_sid_sel_(Type, TNum, Id, Sid),
	source_sid__num(type, TNum),
	source_sid__num(id, Id).

source_sid_sel_(t1, 1, Id, t1(Id)) :- !.
source_sid_sel_(t0, 0, Id, t0(Id)).

source_sid__num(type, Num) :-
	integer(Num), Num >= 0, Num =< 1.
source_sid__num(id, Num) :-
	integer(Num), Num >= 0.

source_new_sid_(Type, Sid) :-
	flag(nan_kernel__source, Id, Id + 1),
	succ(Id, Id1),
	source_sid_sel_(Type, _, Id1, Sid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- use_module(library(prolog_server)).
:- prolog_server(4023,[allow(_)]).

:- use_module(library(heaps)).

nop(_).
wdbg(P):-format(user_error,'~NWDBG: ~q.~n',[P]),
	flush_output(user_error).

e_call(E,Goal) :-
        engine_foc(E,Goal),
        setup_call_cleanup(
           engine_post(E,call(Goal)),
           engine_next(E, Goal),
            wdbg(engine_done(E,Goal))).

engine_foc(E,Goal):-
   engine_post(E,call(Goal)),
   engine_next(E, Goal).

engine_foc(E,_Goal):-
	catch(current_engine(E),_,fail),!.
engine_foc(E,Goal):-
        engine_create(Goal, engine_do_all , E).

engine_do_all:-!,engine_fetch(Do),Do.
engine_do_all:-
        repeat,
	(engine_fetch(call(Goal))->
        call_cleanup(call(Goal),fail)).



e_findall(Templ, Goal, List) :-
        setup_call_cleanup(
            engine_create(Templ, Goal, E),
            e_get_answers(E, List),
            engine_destroy(E)).

e_get_answers(E, [H|T]) :-
        engine_next(E, H), !,
        get_answers(E, T).
e_get_answers(_, []).




create_heap(E) :-
        empty_heap(H),
        engine_create(_, update_heap(H), E).

update_heap(H) :-
        engine_fetch(Command),
        (   update_heap(Command, Reply, H, H1)
        ->  true
        ;   H1 = H,
            Reply = false
        ),
        engine_yield(Reply),
        update_heap(H1).

update_heap(add(Priority, Key), true, H0, H) :-
        add_to_heap(H0, Priority, Key, H).
update_heap(get(Priority, Key), Priority-Key, H0, H) :-
        get_from_heap(H0, Priority, Key, H).

heap_add(Priority, Key, E) :-
        engine_post(E, add(Priority, Key), true).

heap_get(Priority, Key, E) :-
        engine_post(E, get(Priority, Key), Priority-Key).

:- meta_predicate merge(?,0, ?,0, -).

merge(T1,G1, T2,G2, A) :-
        engine_create(A, merge(T1,G1, T2,G2), E),
        repeat,
            (   engine_next(E, A)
            ->  true
            ;   !, fail
            ).

merge(T1,G1, T2,G2) :-
        engine_create(T1, G1, E1),
        engine_create(T2, G2, E2),
        (   engine_next(E1, S1)
        ->  (   engine_next(E2, S2)
            ->  order_solutions(S1, S2, E1, E2)
            ;   yield_remaining(S1, E1)
            )
        ;   engine_next(E2, S2),
            yield_remaining(S2, E2)
        ).

order_solutions(S1, S2, E1, E2) :- !,
        (   S1 @=< S2
        ->  engine_yield(S1),
            (   engine_next(E1, S11)
            ->  order_solutions(S11, S2, E1, E2)
            ;   yield_remaining(S2, E2)
            )
        ;   engine_yield(S2),
            (   engine_next(E2, S21)
            ->  order_solutions(S1, S21, E1, E2)
            ;   yield_remaining(S1, E1)
            )
        ).

yield_remaining(S, E) :-
        engine_yield(S),
        engine_next(E, S1),
        yield_remaining(S1, E).



:- meta_predicate merge_answers(?,0, ?,0, -).

merge_answers(T1,G1, T2,G2, A) :-
        findall(T1, G1, L1),
        findall(T2, G2, L2),
        ord_union(L1, L2, Ordered),
        member(A, Ordered).

