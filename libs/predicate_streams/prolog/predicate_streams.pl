:- module(predicate_streams,
          [
            with_input_from_predicate/2,     % +Pred1, +Goal
            with_output_to_predicate/2,      % +Pred1, +Goal
            with_error_to_predicate/2,       % +Pred1, +Goal
            with_error_to_buffer_predicate/3, % +BufferType, +Pred1, +Goal

            with_predicate_input_stream/3,   % +Pred1, -Stream, +Goal
            with_predicate_output_stream/3,  % +Pred1, -Stream, +Goal

            is_predicate_stream/1,           % +Stream
            current_predicate_stream/1,      % ?Stream
            tl:on_stream_close/2,               % ?Stream, +Goal
			tl:on_stream_write/2,               % ?Stream, +Pred1
			tl:on_stream_read/2,                % ?Stream, +Pred1

            set_current_input/1,             % +Stream
            set_current_output/1,            % +Stream
            set_current_error/1,             % +Stream

            new_predicate_output_stream/2,   % +Pred1, -Stream
            new_predicate_input_stream/2,    % +Pred1, -Stream

            current_error_stream/1                  % -Stream
            
          ]).


/** <module> predicate_streams - Abstract Predicate Streams Utility LOGICMOO PREDICATE STREAMS
This module creates virtual/abstract input and output streams in prolog using predicates. 
@author Douglas R. Miles
@license LGPL

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2015

    This program is free software; you can redistribute it and/or
    modify it.

    18+ Years ago I remember these predicates existed as the building
    blocks for Sockets in some Prolog I cannot remember.

*/
:- meta_predicate
        with_input_from_predicate(:, 0),
        with_output_to_predicate(:, 0),
        with_error_to_predicate(:, 0),
        with_error_to_buffer_predicate(+, :, 0),

        new_predicate_output_stream(:,-),
        new_predicate_input_stream(:,-),

        stream_write_3(*,1,?),

        whatevah(:),
        with_predicate_output_stream(:, -, 0),
        with_predicate_input_stream(:, -, 0).

% Syntactical commenting
:- meta_predicate(no_op(*)).
no_op(_).

:- use_module(library(prolog_stream)).

current_predicate_stream(S):- psg:i_current_predicate_stream(S).

%! psg:i_current_predicate_stream(?Stream) is nondet.
%
%  Current Streams made by this API
%
:- multifile(psg:i_current_predicate_stream/1).
:- dynamic(psg:i_current_predicate_stream/1).
:- volatile(psg:i_current_predicate_stream/1).
:- multifile(tl:on_stream_write/2).
:- thread_local(tl:on_stream_write/2).

% Hooks
:- if(true).
:- thread_local(tl:on_stream_write/2).
:- thread_local(tl:on_stream_close/2).
:- thread_local(tl:on_stream_read/2).
:- else.
:- dynamic(tl:on_stream_write/2).
:- dynamic(tl:on_stream_close/2).
:- dynamic(tl:on_stream_read/2).
:- endif.

:- volatile(tl:on_stream_write/2).
:- volatile(tl:on_stream_close/2).
:- volatile(tl:on_stream_read/2).


:- meta_predicate(redo_cleanup_each(0,0,0)).
redo_cleanup_each(Setup,Goal,Cleanup):-
   \+ \+ '$sig_atomic'(Setup),
   catch(
     ((Goal, deterministic(DET)),
       '$sig_atomic'(Cleanup),
         (DET == true -> !
          ; (true;('$sig_atomic'(Setup),fail)))),
      E,
      ('$sig_atomic'(Cleanup),throw(E))).



% When $user_input == $current_input
%current_input_is_user_input:- !,false.
current_input_is_user_input:- stream_property(Stream,alias(user_input)),stream_property(Stream,alias(current_input)).

% When $user_output == $current_output
%current_output_is_user_output:- !,false.
current_output_is_user_output:- stream_property(Stream,alias(user_output)),stream_property(Stream,alias(current_output)).

% When $user_error == $current_error
%current_error_is_user_error:- !,false.
current_error_is_user_error:- \+ stream_property(_Stream,alias(current_error)).
current_error_is_user_error:- stream_property(Stream,alias(user_error)),stream_property(Stream,alias(current_error)).

% set current input stream and aliases
set_current_input(user_input):-!.  
set_current_input(In):- stream_property(Was,  alias(current_input)),!,quietly(set_current_input_to(Was,In)).
:- '$hide'(set_current_input/1).  
set_current_input_to(In,In):- !.
set_current_input_to(_Was,In):-
 (current_input_is_user_input -> set_stream(In,  alias(user_input)) ; true),
 set_stream(In,  alias(current_input)),
 set_input(In).
:- '$hide'(set_current_input_to/2).


% set current output stream and aliases
set_current_output(user_output):-!.  
set_current_output(Out):- stream_property(Was,  alias(current_output)),!,quietly(set_current_output_to(Was,Out)).
:- '$hide'(set_current_output/1).  
set_current_output_to(Out,Out):- !.
set_current_output_to(Was,Out):-
  whatevah(flush_output(Was)),
 (current_output_is_user_output -> set_stream(Out,  alias(user_output)) ; true),
 set_stream(Out,  alias(current_output)),
 set_output(Out).
:- '$hide'(set_current_output_to/2).


% set current error stream and aliases
set_current_error(Err):- current_error_stream(Was),!,quietly(set_current_error_to(Was,Err)).
:- '$hide'(set_current_error/1).  
set_current_error_to(Err,Err):- !.
set_current_error_to(ErrWas,Err):-
 nop(whatevah(flush_output(ErrWas))), 
% (current_error_is_user_error -> set_stream(Err,  alias(user_error)) ; true),
% set_stream(Err,  alias(current_error)),
 set_error(Err),
 !.
:- '$hide'(set_current_error_to/2).


set_error(user_error):-!.
set_error(current_error):-!.
set_error(Err):-
 quietly((current_input(In), current_output(Out), 
 set_prolog_IO(In,Out,Err))).
:- '$hide'(set_error/1).  

% Get current error stream
current_error_stream(Err):-
  stream_property(Err,alias(current_error))-> true;  % when we set it
  stream_property(Err,alias(user_error)) -> true;
  stream_property(Err,file_no(2)).
:- '$hide'(current_error_stream/1).  

% Helps when Ctrl-C is hit while stream is busy
maybe_restore_input(Stream):-
  stream_property(Stream,alias(current_input)),
  \+ stream_property(Stream,alias(user_input)),
  stream_property(Was,alias(user_input)),!,
  set_current_input(Was).
maybe_restore_input(Stream):-
  stream_property(Stream,alias(current_input)),
  stream_property(Stream,alias(user_input)),
  original_user_stream(user_input,Was),!,
  set_current_input(Was).
maybe_restore_input(_).
:- '$hide'(maybe_restore_input/1).  

:- volatile(original_user_stream/2).
:- dynamic(original_user_stream/2).

save_orig_stream(Name):- 
  stream_property(In,alias(Name)),
  call(asserta,original_user_stream(Name,In)),
  thread_self(Self),atom_concat(Self,'_',SN),
  atom_concat(SN,Name,Main),set_stream(In,alias(Main)),
  set_stream(In,alias(Name)),!.

know_original_user_input:- 
 ignore((\+ original_user_stream(_,_),
   save_orig_stream(user_input),
   save_orig_stream(user_output),
   save_orig_stream(user_error))).

:- if(( \+ source_file_property(reloading, true))).
:- if((thread_self(M) , M == main )).
:- know_original_user_input.
:- else.
:- if(current_prolog_flag(debug,true)).
:- thread_self(Self),throw(know_original_user_input(Self)),!.
:- endif.
% :- know_original_user_input.
:- endif.
:- endif.

:- initialization(know_original_user_input,restore).
:- initialization(know_original_user_input).

%! is_predicate_stream(+Stream) is det.
%
%  Checks to see if Stream was made by this API
%
is_predicate_stream(Stream):-
   must_be(nonvar,Stream),
   psg:i_current_predicate_stream(Stream).


% =====================================================
% Override std streams
% =====================================================

%! with_output_to_predicate( :Pred1, :Goal) is nondet.
%
%  Redirects output stream to a predicate
%
%  ===
%  ?- with_output_to_predicate({}/[X]>>assert(saved_output(X)),
%     (write("hi there"),nl,writeln("how are you?"))),
%     listing(saved_output/1).
%
%  saved_output("hi there\n").
%  saved_output("how are you?\n").
%
%  ===

with_output_to_predicate(Pred1,Goal):-
   current_output(Prev),
   % stream_property(Prev,buffer_size(Size)),
   % stream_property(Prev,buffer(Buffer)),
   % Buffer = line,
    with_predicate_output_stream(Pred1,Stream,
     ( % set_stream(Stream, buffer(Buffer)),
      % set_stream(Stream, buffer_size(Size)),
       redo_cleanup_each(
          set_current_output(Stream),
          Goal,
          set_current_output(Prev)))).



%! with_error_to_predicate( :Pred1, :Goal) is nondet.
%
%  Redirects error stream to a predicate
%
%  ===
%  ?- with_error_to_predicate(write,threads).
%  ... writes thread info to stdout instead of stderr...
%  ===

with_error_to_predicate(Pred1,Goal):-
  with_error_to_buffer_predicate(line,Pred1,Goal).

with_error_to_buffer_predicate(Buffer,Pred1,Goal):-
 current_error_stream(Prev),
 with_predicate_output_stream(Pred1,Stream,
(  set_stream(Stream, buffer(Buffer)),
   set_stream(Stream, buffer_size(40960)),      
      redo_cleanup_each(
          set_current_error(Stream),
          Goal,
         set_current_error(Prev)))).


%! with_input_from_predicate( :Pred1, :Goal) is nondet.
%
%  ===
%  ?- with_input_from_predicate(=('hello.\n'), read(World)).
%  World = hello.
%  ===
%
%  ===
%  Auto presses Y<Enter>
%  ?- with_input_from_predicate({}/[X]>>X='Y\n', poor_interactive_goal).
%  ===

with_input_from_predicate(Pred1,Goal):-
    current_input(Prev),
    with_predicate_input_stream(Pred1,Stream,
        redo_cleanup_each(
           set_current_input(Stream),
           Goal,
           set_current_input(Prev))).


% =====================================================
% With Predciate I/O
% =====================================================

%! with_predicate_output_stream( :Pred1, ?Stream, :Goal) is nondet.
%
%  Helper that creates and destroys (closes) a predicate output stream
%
with_predicate_output_stream(Pred1,Stream,Goal):-
    setup_call_cleanup(
       new_predicate_output_stream(Pred1,Stream),
       Goal,
       force_close(Stream)).


%! with_predicate_input_stream( :Pred1, ?Stream, :Goal) is nondet.
%
%  Helper that creates and destroys (closes) a predicate input stream
%
%  used by with_output_to_predicate/2, with_error_to_predicate/2
%
with_predicate_input_stream(Pred1,Stream,Goal):-
    setup_call_cleanup(
       new_predicate_input_stream(Pred1,Stream),
       Goal,
       force_close(Stream)).


% =====================================================
% All magic is below here
% =====================================================

:- use_module(library(prolog_stream)).

:- if((fail , exists_source(library(loop_check)))).

:- multifile(pshook:stream_write/2).
:- dynamic(pshook:stream_write/2).
pshook:stream_write(Stream,Data):- 
  loop_check(stream_write_0(Stream,Data),true).

stream_write_0(Stream,Data):-
 quietly((tracing-> write(main_user_error,Data);
 (  nop(whatevah(flush_output(Stream))),
    forall(tl:on_stream_write(Stream,Pred1),
    once(loop_check_term(
            call(Pred1,Data),            
            ttt_no_loop_mutex,
           ( (
              (write(main_user_error,skip(Data))),
             (set_stream(Stream,buffer(full))))))))))),
  nop(whatevah(flush_output(Stream))).

:- else.

:- if( \+ current_predicate(pshook:stream_write/2)).
:- multifile(pshook:stream_write/2).
:- dynamic(pshook:stream_write/2).
pshook:stream_write(Stream,Data):-
  forall(tl:on_stream_write(Stream,Pred1),stream_write_3(Stream,Pred1,Data)).

:- thread_local(tl:loop_check_stream_write/3).

stream_write_3(Stream,Pred1,_Data):- tl:loop_check_stream_write(Stream,Pred1,_DataOther),!.
stream_write_3(Stream,Pred1,Data):- 
  setup_call_cleanup(
   call(asserta,tl:loop_check_stream_write(Stream,Pred1,Data),Ref),
    once((call(Pred1,Data))),
    erase(Ref)).

:- '$hide'(stream_write/2).

pshook:stream_read(Stream,Data):- tl:on_stream_read(Stream,Pred1) *-> call(Pred1,Data) ; Data = -1.

:- '$hide'(stream_read/2).
force_close(Stream):- whatevah(close(Stream,[force(true)])).
:- '$hide'(force_close/1).  

pshook:stream_close(Stream):-
  quietly((
   retract(psg:i_current_predicate_stream(Stream)),   
   maybe_restore_input(Stream), % this is a so we dont hit the tracer in Ctrl-C
   (stream_property(Stream,output)-> stream_close_output(Stream) ; true),
   whatevah(forall(retract(tl:on_stream_read(Stream,Pred1)),
        nop(debug(predicate_streams,'~N% ~q.~n',[(stream_close(Stream):-tl:on_stream_read(Pred1))])))),
   forall(retract(tl:on_stream_close(Stream,Call)),whatevah(Call)))),!.
pshook:stream_close(_Stream).
:- '$hide'(stream_close/1).  

:- endif.


:- endif.





stream_close_output(Stream):-
  quietly(( 
   %whatevah(stream_write(Stream,'')),
   whatevah(flush_output(Stream)),
   % whatevah(forall(retract(tl:on_stream_write(Stream,Pred1)), nop(debug(predicate_streams,'~N% ~q.~n',[(stream_close(Stream):-tl:on_stream_write(Pred1))])))),
   !)).
stream_close_output(_).
:- '$hide'(stream_close_output/1).


%! new_predicate_output_stream(:Pred1,-Stream)
%
%  Creates a new output stream that each write
%  Invokes: call(+Pred1,+Data).

new_predicate_output_stream(Pred1,Stream):-
  open_prolog_stream(pshook, write, Stream, []),
   asserta(tl:on_stream_write(Stream,Pred1)),
   assert(psg:i_current_predicate_stream(Stream)),
   thread_at_exit(force_close(Stream)).

%! new_predicate_input_stream(:Pred1,-Stream)
%
%  Creates a new input stream that each read/getch()
%  Invokes: call(+Pred1,-Data).
%
%  todo Discuss how to handle peek_char/2

new_predicate_input_stream(Pred1,Stream):-
  open_prolog_stream(pshook, read, Stream, []),
   assert(psg:i_current_predicate_stream(Stream)),
   asserta(tl:on_stream_read(Stream,Pred1)),
   set_stream(Stream, buffer_size(1)),
   thread_at_exit(force_close(Stream)).


%! whatevah( :Goal) is semidet.
%
% As pronounced by a teenage girl
%
whatevah(Goal):- quietly('$sig_atomic'(ignore(catch(Goal,error(A,B),(writeln(main_user_error,error(A,B)),break))))).
:- '$hide'(whatevah/1).  
  

% set_stream(Stream, buffer(line)), % useful?
% set_stream(Stream, buffer_size(1)),   % useful?
% set_stream(Stream, close_on_exec(true)), % useful?
% set_stream(Stream, close_on_abort(true)), % useful?
