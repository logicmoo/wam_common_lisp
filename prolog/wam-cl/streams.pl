/*******************************************************************
 *
 * A Lisp compiler, written in Prolog
 *
 * (streams_test.pl)
 *
 * (c) Neil Smith, 2001
 *
 * The Lisp compiler supports lexical closures and other higher-
 * order programming techniques.  To demonsrate this, here is a 
 * simple and naive implementation of streams, and some examples
 * of integer stream generators, including an infinite stream. 
 * Code for this program was taken from 'Structure and Interpretation
 * of Computer Programs' by Abelson et al., and from 'Lisp' by 
 * Winston and Horn.
 *
 *******************************************************************/

stream_first(stream) <<==
	first(stream).

stream_rest(stream) <<==
	lisp_apply(second(stream), []).

stream_cons(a, b) <<==
	list_2( a, b).

stream_null(stream) <<==
	null(stream).


% take the first n items from a stream, placing them in a normal list
stream_take(n, stream) <<==
  if( or( equalp(n, 0), stream_null( stream)),
      [],
      cons(stream_first(stream), 
           stream_take(minus(n, 1), stream_rest( stream)))). 

% remove the first n items from a stream
stream_drop(n, stream) <<==
  if( or( equalp( n, 0), stream_null( stream)),
      stream,
      stream_drop( minus( n, 1), stream_rest( stream))).


% creates a stream of integers from low to high
stream_interval(low, high) <<==
  if( equalp(low, high),
    [],
    stream_cons( low, function(lambda([], stream_interval( plus( low, 1), high))))). 


% creates an infinite stream of integers, starting at n
stream_ints_from(n) <<==
  stream_cons( n, function(lambda([], stream_ints_from( plus( n, 1))))).


% tests on streams
t1 <<==
	stream_take(3, stream_interval(1,5)).

t2 <<== 
	stream_take(5, stream_drop(10, stream_ints_from(1))).
