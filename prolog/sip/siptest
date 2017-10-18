%% [tim.prolog]siptest.
%% Tim Finin, University of Pennsylvania, Mon Oct 27 10:39:27 1986
%% this file contains samples for SIP.

fact == lambda([n], if(=(n,0),1,n*fact(sub1(n)))).

add1 == lambda([n], n+1).

sub1 == lambda([n], n-1).

% higher order functions

mapcar == 
  lambda([f,l],
         if(null(l),
            nil,
            cons(f(car(l)),mapcar(f,cdr(l))))).

% simple list manipulation functions.

length == lambda([l], if(null(l),0,add1(length(cdr(l))))).

append == lambda([l1,l2],if(null(l1),l2,cons(car(l1),append(cdr(l1),l2)))).

reverse == 
  lambda([l], 
     if(null(l),
        l,
        append(reverse(cdr(l)),(cons(car(l),nil))))).

% stuff for streams.

filter == 
  lambda([f,s], 
         if('emptyStream?'(s),
            s,
            if(f(head(s)),
               consStream(head(s),filter(f,tail(s))),
               filter(f,tail(s))))).

from == lambda([n],consStream(n,from(n+1))).

nthStream == lambda([s,n],if(n=1,head(s),nthStream(tail(s),n-1))).

integers == from(1).

% environments

makeCounter ==
  lambda([],
         begin(counter == 0,
               lambda([],set(counter,1+counter)))).

      