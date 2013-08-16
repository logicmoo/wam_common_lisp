Posted to comp.lang.prolog

Date: Sun, 5 Apr 87 01:42:37 EST
From: Tim Finin <t...@linc.cis.upenn.edu>
Subject: Scheme

A while back there was a discussion on the SCHEME newsgroup concerning
implementations of logic programming languages in Scheme.  David Moon
wondered if anyone had implemented Prolog in Scheme.  

What I found most interesting about the exercise has more to do with
Prolog than with Scheme - It is very difficult to implement an
efficient interpreter for a language which has side-effects in Prolog.

I could not find a way to represent environments which had what I
consider to be the neccessary features:

1 - unreferenced enviroments should be automatically GCed.

2 - looking up the value of a variable should be cheap and,
    in particular, should not depend on the the number of values it
    has received in the past.

3 - variable assignment should be cheap and, in particular should not
            require copying abritrary portions of an environment.

4 - The interpreter should not require an infinite stack nor
    should the host prolog be required to detect and optimize
    for tail recursion.

I basically considered two alternatives for representing the
environment:

  o represent an environment as a term which contains a sequence of
    variable-name/variable-value pairs.  This achieves (1) in most
    prologs
    but must give up on either (2) or (3).

  o represent an environment as a set of assertions in the clausal
    database of the form: bound(Symbol,Value,EnvironmentID).  This
    wins on
    (2) and (3) but loses on (1).

This makes me think that a side-effect predicate like RPLACARG
(discussed in Prolog-Digest about a year ago) is not such a bad idea.
It also reinforces the notion that Lisp is either a (i) more general
or (ii) lower level language than Prolog, depending, of course, on
your point of view.

-- Tim

