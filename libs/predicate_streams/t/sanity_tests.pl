% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/predicate_streams.pl
     
:- module_transparent
        buffer_chars/1,
        read_received/1,
        some_test/0,
        with_io_restore/1,
        test1_0/1,
        test2/1.


:- use_module(library(predicate_streams)).
:- debug(predicate_streams).

% Writeq/1s a term the user_error and flushes
%:- if( \+ current_predicate(dmsg/1)).
dmsg(M):-format(user_error,'~N % dmsg: ~q.~n',[M]),flush_output(user_error).
%:- endif.


:- with_error_to_predicate(write,format(user_error,'~s',["ls"])).



:- discontiguous some_test/0.


% with_io_restore(Goal) is semidet.
%
% Using Input/output.


% with_error_to_predicate( :PRED1Callback, :Goal) is semidet.
%
% Using Err Converted To Predicate.

some_test :- with_error_to_predicate(format_to_error('~s'),ls).

 	 	 

% some_test is semidet.
%
% Some Test.
%
some_test :- dynamic(received_chars/1).

  

% test predciate to receive char codes

 	 	 

% buffer_chars( ?N) is semidet.
%
% Buffer Chars.
%
buffer_chars(end_of_file):-!,assertz(received_chars(end_of_file)).
buffer_chars(N):-number(N),!,char_code(C,N),assertz(received_chars(C)).
buffer_chars(C):-name(C,Chars),maplist(buffer_chars,Chars).


some_test :- with_output_to_predicate(buffer_chars,write('hello. ')).
some_test :- with_output_to_predicate(buffer_chars,write('World.\n')).
% lets just be nasy to ourselves here (confirms we handle closing of current output)
some_test :- with_output_to_predicate(buffer_chars,told).
some_test :- with_output_to_predicate(buffer_chars,(current_output(Out),close(Out))).
% Not bad !

some_test :- listing(received_chars/1).

/*
received_chars(h).
received_chars(e).
received_chars(l).
received_chars(l).
received_chars(o).
received_chars('.').
received_chars(' ').
received_chars('W').
received_chars(o).
received_chars(r).
received_chars(l).
received_chars(d).
received_chars('.').
received_chars('\n').
received_chars(end_of_file).
received_chars(end_of_file).

Looks good !
*/


some_test :- with_output_to_predicate(dmsg, (current_output(Out),forall(stream_property(Out,Prop),dmsg(stream_property(Out,Prop))))).

% dmsg: stream_property(<stream>(0x232b8a0),mode(write)).
% dmsg: stream_property(<stream>(0x232b8a0),output).
% dmsg: stream_property(<stream>(0x232b8a0),position('$stream_position'(0,1,0,0))).
% dmsg: stream_property(<stream>(0x232b8a0),eof_action(eof_code)).
% dmsg: stream_property(<stream>(0x232b8a0),reposition(false)).
% dmsg: stream_property(<stream>(0x232b8a0),type(text)).
% dmsg: stream_property(<stream>(0x232b8a0),buffer(full)).
% dmsg: stream_property(<stream>(0x232b8a0),buffer_size(0)).
% dmsg: stream_property(<stream>(0x232b8a0),close_on_abort(true)).
% dmsg: stream_property(<stream>(0x232b8a0),encoding(wchar_t)).
% dmsg: stream_property(<stream>(0x232b8a0),locale(default)).
% dmsg: stream_property(<stream>(0x232b8a0),newline(posix)).
% dmsg: stream_property(<stream>(0x232b8a0),representation_errors(error)).


% our test callback

 	 	 

% read_received( ?A) is semidet.
%
% Read Received.
%
read_received(A):- A == end_of_file,!. %  Dmiles was lazy
read_received(C):- retract(received_chars(A)), (A==end_of_file -> C="" ; C =A).
read_received("").

% Test wtih read/1 works awesome
some_test :- with_input_from_predicate(read_received, (read(X), writeln(read(X)))).
% dmsg: read(hello)

% This test is just for deciding the scope .. (about asking callback to understand peeking or not)
some_test :- with_input_from_predicate(read_received, (peek_char(X), writeln(peek_char(X)))).
% dmsg: peek_char('W')

some_test :- listing(received_chars).

/*
some_test :- dynamic received_chars/1.

received_chars(o).
received_chars(r).
received_chars(l).
received_chars(d).
received_chars('.').
received_chars('\n').
received_chars(end_of_file).
received_chars(end_of_file).
*/

some_test :- with_input_from_predicate(=(""), (current_input(In),forall(stream_property(In,Prop),dmsg(stream_property(In,Prop))))).

% dmsg: stream_property(<stream>(0x9cfd70),mode(read)).
% dmsg: stream_property(<stream>(0x9cfd70),input).
% dmsg: stream_property(<stream>(0x9cfd70),position('$stream_position'(0,1,0,0))).
% dmsg: stream_property(<stream>(0x9cfd70),end_of_stream(not)).
% dmsg: stream_property(<stream>(0x9cfd70),eof_action(eof_code)).
% dmsg: stream_property(<stream>(0x9cfd70),reposition(false)).
% dmsg: stream_property(<stream>(0x9cfd70),type(text)).
% dmsg: stream_property(<stream>(0x9cfd70),buffer(full)).
% dmsg: stream_property(<stream>(0x9cfd70),buffer_size(0)).
% dmsg: stream_property(<stream>(0x9cfd70),close_on_abort(true)).
% dmsg: stream_property(<stream>(0x9cfd70),encoding(wchar_t)).
% dmsg: stream_property(<stream>(0x9cfd70),locale(default)).
% dmsg: stream_property(<stream>(0x9cfd70),newline(posix)).
% dmsg: stream_property(<stream>(0x9cfd70),representation_errors(error)).
% dmsg: stream_property(<stream>(0x9cfd70),timeout(infinite)).


% Passes
some_test :- \+ with_input_from_predicate(=(""), \+ at_end_of_stream(current_input)).

% Passes
some_test :- with_input_from_predicate(=('hello.\n'), read(World)),writeln(world=World).

some_test :- with_output_to_predicate({}/[X]>>format('~n% ~q~n',[X]),(writeln("hi there"),writeln("how are you?"))).

% Test 1

 	 	 

% test1_0( ?In) is semidet.
%
% test Secondary Helper  Primary Helper.
%
test1_0(In) :-
        repeat,
               get_char(In, Char),
               dmsg(getch(In,Char)),
               Char == end_of_file.
           

% Passes
some_test :- with_input_from_predicate(read_received, test1_0(current_input)).


% setup for test 2 
% :- with_output_to_predicate(buffer_chars, format('Read it all\n',[])).
% :- listing(received_chars/1).


% Test 2 is indeed asks much, but still is reasonable

 	 	 

% test2( ?In) is semidet.
%
% Test Extended Helper.
%
test2(In) :-
        repeat,
            (   at_end_of_stream(In)
            ->  !
            ;   read_pending_input(In, Chars, []),
                dmsg(read_pending_input(In, Chars, [])),
                fail
            ).

% TODO
% :- with_input_from_predicate(read_received, test2(current_input)).

some_test :- writeln(done).


