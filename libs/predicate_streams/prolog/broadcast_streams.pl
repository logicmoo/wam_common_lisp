:- module(broadcast_streams,
          [
            new_broadcast_stream/2     % +StreamsIn, +NewStream
          ]).

/** <module> broadcast_streams - Output stream targeting mutiple outputs

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2015
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- use_module(library(predicate_streams)).
:- use_module(library(yall)).

%! new_broadcast_stream(+list:StreamsIn,-NewStream)
%
%  Creates a new output stream that each write 
%  outputs to all of the output streams

new_broadcast_stream(List,Stream):- 
  new_predicate_output_stream([Data]>>forall(member(S,List),write(S,Data)),Stream).

:- if(false).
new_broadcast_stream(List,Stream):-
  new_predicate_output_stream(write_to_streams(List),Stream).
write_to_streams(List,Data):- maplist(write_rev(Data),List).
write_rev(Data,Stream):- write(Stream,Data).
:- endif.
