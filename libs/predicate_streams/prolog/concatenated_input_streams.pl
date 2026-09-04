:- module(concatenated_input_streams,
          [
            new_concat_input_stream/2     % +StreamsIn, +NewStream
          ]).

/** <module> concatenated_input_streams - Lazy Input stream from concatenating several inputs

    Author:        Douglas R. Miles
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (C): 2015
                       
    This program is free software; you can redistribute it and/or
    modify it.

*/

:- use_module(library(predicate_streams)).
:- use_module(library(yall)).


%! new_concat_input_stream(+list:StreamsIn,-NewStream)
%
%  Creates a new input stream that each read 
%   happens along the list of StreamsIn

new_concat_input_stream(List,Stream):-   
  new_predicate_input_stream(
    [[Code]]>>
     ((member(S,List),\+ at_end_of_stream(S),get_code(S,Code)) ->
       check_eof(List,Stream);throw(read_past_input)),
   Stream).


check_eof([S|List],Stream):- at_end_of_stream(S)->check_eof(List,Stream);true.
check_eof([],Stream):-set_stream(Stream,end_of_stream(at)).

