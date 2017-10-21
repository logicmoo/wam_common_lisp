SIP, Tim Finin (tim@cis.upenn.edu), University of Pennsylvania, April 1987.

SIP (Scheme in Prolog) is a partial interpreter for a subset of
Scheme.  There are two versions: one with (sip2) and one without
(sip1) continuation semantics.  The relevant files are:

	sip1	- interpreter w/o continuations.
	sip2	- interpreter with continuations
	sipcore	- base file for both, defines primitive variables, etc.
	siptest - examples

Start SIP by loading either "sip1" or "sip2" and issuing the query:

	?- sip.

This should create an initial environment (by loading the file
"sipcore" and then enter a read-eval-print loop.  You can re-start the
read-eval-print loop w/o recreating the initial environment by issuing
the prolog query:

	?- sipREP.

A few notes:

The bad news: the syntax is unLispy, due to the prolog reader. Instead
of (foo a 1) you must say foo(a,1).  Look at the file "siptest" to see
some samples of simple expressions.  Similarly, you must type a "."
after every input.  The usual prolog special characters (e.g.
operators) may cause the reader to complain.  There is no way to call
a function with no arguments!  The good news: infix operators are
allowed, to some degree.  In particular "S == E" means (define S E).

More syntax kludges: Cal a no-argument function FOO like [FOO].  Call
a lambda-form like: [lambda([n],n+2),3].  BEGIN only takes two
arguments - you can embed them, of course.

evaling prolog(P) invokes prolog on query P and returns #!t if it
succeeded and #!F otherwise.

The character ` acts like ' in normal Scheme in that it quotes the
next expression in the input.

evaling prolog(`abort) is the (dirty) way to exit the read-eval-print
loop.  In fact, evaling prolog(`abort) is the only way to exit the
read-eval-print loop.

Just to make my job easier, I added a macro like facility.  Evaling S1
==> S2 defines a rewriting type macro that transforms S1 into S2.  For
example, we could (and do) do:

		tail(X) ==> force(cdr(X)).



	
