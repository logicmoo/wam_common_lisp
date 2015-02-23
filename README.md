#TASK:

is to create a Prolog version of a Common Lisp Interpreter.   
Initially starting out with Lisp500.  I haven't checked yet if Lisp 500 will be good enough to run most Lisp programs it's not quite common Lisp obviously but at least it's accepted widely enough that it even CL-BENCH references it.

If there's a better common Lisp out there for us and we get there faster than I'll take you up on the offer to use that Lisp and said this Lisp for example one might find ABCL's *.lisp files are better than core500.lisp 
The reason for choosing Lisp 500 is it would require you to only implement 158 initial trampolines from there core500.lisp  is loaded on top of your Prolog system initial which is the lisp500.pl.
 
Presently lisp500.pl has Prolog terms including comments as you can see it's manipulating memory arrays and a completely idiomatic port of the 158  C  functions would probably not be ideal but I'm allowing this to be possible if it allows you to get the job completed faster.    See,  If I was doing this work myself,  I would probably begin to work on the C idiomatically but then within a short amount of time realize that this approach is creating more work compared to implementing the exact function that the core Lisp file requires CAR/APPLY etc.  But I don't feel comfortable going straight to the Prolog version until I've at least had a crack at idiomatic C.  I leave this up to your discretion.

Regardless how this first part was done you would get to the point where you are loading the core500.lisp.  You now have a toy Lisp interpreter.    


#BOUNTY REQUIREMENTS:

$1,000 = lisp500 equality

$1,500 = KM running ( package or unpackaged version )

$2,000 = pass ANSI tests

$2,500 = CLOS

If "KM running"  target was a little too far off then I would first go for passing of the ANSI Common Lisp Test Suite and get $2000.  http://common-lisp.net/project/ansi-test/ or as much as possible.   After a reasonable number of ANSI tests can be passed I would then start using smaller list programs which can be added to the test directory the final program that I like to feel the run is called the knowledge machine and that's in the test directory right now and in there is a script file that runs the tests for the seat the KM system.

#NOTES:

CL ANSI tests I suppose at this very moment I should be testing the C version in seeing how far it gets in the ANSI tests to get his myself a baseline to find out if Lisp500 is actually really worth it.  
I would like to also find out if there is a way of eventually getting the CLOS out of Lisp 500.  Some people suggested using closette for this particular work order getting classes in necessary but it is in the plan and if it seems like this is not an happen starting with with 500 but starting with the different lisp  I would try to figure this as soon as possible.

Some work towards this can be at the doc files of [https://code.google.com/p/evita-common-lisp/source/browse/doc?r=22ae8ca5396af3f39eb215c69f02dd7d80279794#doc%253Fstate%253Dclosed](https://code.google.com/p/evita-common-lisp/source/browse/doc?r=22ae8ca5396af3f39eb215c69f02dd7d80279794#doc%253Fstate%253Dclosed)



#FURTHER THOUGHTS:

Some people say it's absolutely absurd to try to implement common Lisp inside a Prolog because of the final result would be to inefficient run to slow use it too much memory etc . A week ago when the JavaScript version of miniKanren ran faster than the Scheme version because in their particular use case the user types in code and expected it to run as quickly as possible in which it did because it skipped over some compilation step.  All the JavaScript version did was create a list of uncompiled closures.  Our analogy here is that Lisp macros can almost always be compiled down into a set of Lisp functions after they been expanded properly.  However we also know that they can be interpreted and that the Lisp interpreter does not really have to compile these macros and said do term replacement.  While this is prolog chief capability that is to put terms inside of terms during unification you may ask is really Lisp just macro after macro and would we really get a speed of benefit by doing this.  This argument may be flawed but it was just a thought.

My usecase .. A propositional resolution program such as CYC, SNARK or KM (knowledge machine) is an exercise into how quickly it can select properly with correct piece of code to invoke this or that .   I believe does not lend itself that greatly to compilation.   This thread will  probably quickly have very knowledgeable people helping me out by telling me why this is not a great idea.  Truthfully I rather doubt that my hypothesis is true that it would run these programs faster but I do believe they will not run is slow and some programming communities would predict.    That said the task is not to make it run fast for me but just to work at all would be wonderful.  

