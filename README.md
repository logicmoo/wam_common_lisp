#TASK:

is to create a Prolog version of a Common Lisp Interpreter.   
Initially starting out with Lisp500.  I haven't checked yet if Lisp 500 will be good enough to run most Lisp programs it's not quite common Lisp obviously but at least it's accepted widely enough that it even CL-BENCH references it.

If there's a better common Lisp out there for us and we get there faster than I'll take you up on the offer to use that Lisp instead for example: You mightt find ABCL's *.lisp files are better than core500.lisp (And actually will get you to $2500 faster) 

The reason for choosing Lisp500 is it would require you to only implement 68 initial trampolines needed by init500.lisp  is loaded on top of your Prolog system initial which is the lisp500.pl.
 
Presently lisp500.pl has Prolog terms including comments as you can see it's manipulating memory arrays and possible  completely do an idiomatic port of the 158  C  functions but seems ok to do the 68 functions instead.  I leave this up to your discretion.

Regardless how this first part was done you would get to the point where you are loading the core500.lisp.  You now have a toy Lisp interpreter.    

#BOUNTY REQUIREMENTS:  (SWI-Prolog Please)

$1,000 = lisp500 equality

$1,500 = KM running ( package or unpackaged version )

$2,000 = pass ANSI tests

$2,500 = CLOS

If "KM running"  target was a little too far off then I would first go for passing of the ANSI Common Lisp Test Suite and get $2000.  http://common-lisp.net/project/ansi-test/ or as much as possible.   After a reasonable number of ANSI tests can be passed I would then start using smaller list programs which can be added to the test directory the final program that I like to feel the run is called the knowledge machine and that's in the test directory right now and in there is a script file that runs the tests for the seat the KM system.

#NOTES:

CL ANSI tests I suppose at this very moment I should be testing the C version in seeing how far it gets in the ANSI tests to get his myself a baseline to find out if Lisp500 is actually really worth it.  
I would like to also find out if there is a way of eventually getting the CLOS out of Lisp 500.  Some people suggested using closette for this particular work order getting classes in necessary but it is in the plan and if it seems like this is not an happen starting with LISP500 but starting with the different lisp  I would try to figure this as soon as possible.

Some work towards this can be at the doc files of [https://code.google.com/p/evita-common-lisp/source/browse/doc?r=22ae8ca5396af3f39eb215c69f02dd7d80279794#doc%253Fstate%253Dclosed](https://code.google.com/p/evita-common-lisp/source/browse/doc?r=22ae8ca5396af3f39eb215c69f02dd7d80279794#doc%253Fstate%253Dclosed)



#FURTHER THOUGHTS:

Some people say it's absolutely absurd to try to implement common Lisp inside a Prolog because of the final result would be to inefficient run to slow use it too much memory etc . A week ago when the JavaScript version of miniKanren ran faster than the Scheme version because in their particular use case the user types in code and expected it to run as quickly as possible in which it did because it skipped over some compilation step.  All the JavaScript version did was create a list of uncompiled closures.  Our analogy here is that Lisp macros can almost always be compiled down into a set of Lisp functions after they been expanded properly.  However we also know that they can be interpreted and that the Lisp interpreter does not really have to compile these macros and said do term replacement.  While this is prolog chief capability that is to put terms inside of terms during unification you may ask is really Lisp just macro after macro and would we really get a speed of benefit by doing this.  This argument may be flawed but it was just a thought.

My usecase .. A propositional resolution program such as CYC, SNARK or KM (knowledge machine) is an exercise into how quickly it can select properly with correct piece of code to invoke this or that .   I believe does not lend itself that greatly to compilation.   This thread will  probably quickly have very knowledgeable people helping me out by telling me why this is not a great idea.  Truthfully I rather doubt that my hypothesis is true that it would run these programs faster but I do believe they will not run is slow and some programming communities would predict.    That said the task is not to make it run fast for me but just to work at all would be wonderful.  

ONLY 68 (not 158 trampolines)#
lisp500_array([struct_symbol_init_symi_ARRAY,[["NIL"],["T"],["&REST"],["&BODY"],["&OPTIONAL"],["&KEY"],["&WHOLE"],["&ENVIRONMENT"],["&AUX"],["&ALLOW-OTHER-KEYS"],
  ["DECLARE",eval_declare,-1],["SPECIAL"],["QUOTE",eval_quote,1],["LET",eval_let,-2],
  ["LET_PTR_",eval_letm,-2],["FLET",eval_flet,-2],["LABELS",eval_labels,-2],
  ["MACROLET",eval_macrolet,-2],["SYMBOL-MACROLET",eval_symbol_macrolet,-2],["SETQ",eval_setq,2],
  ["FUNCTION",eval_function,1],["TAGBODY",eval_tagbody,-1],["GO",eval_go,1],
  ["BLOCK",eval_block,-2],["RETURN-FROM",eval_return from,2],["CATCH",eval_catch,-2],
  ["THROW",eval_throw,-2],["UNWIND-PROTECT",eval_unwind_protect,-2],["IF",eval_if,-3],
  ["MULTIPLE-VALUE-CALL",eval_multiple_value_call,-2],["MULTIPLE-VALUE-PROG1",eval_multiple_value_prog1,-2],
  ["PROGN",eval_body,-1],["PROGV",eval_progv,-3],["_SETF",eval_setf,2],
  ["FINISH-FILE-STREAM",lfinish_fs,1],["MAKEI",lmakei,-3],["DPB",ldpb,3],
  ["LDB",lldb,2],["BACKQUOTE"],["UNQUOTE"],["UNQUOTE-SPLICING"],["IBOUNDP",liboundp,2],["LISTEN-FILE-STREAM",llisten_fs,1],
  ["LIST",llist,-1],["VALUES",lvalues,-1],["FUNCALL",lfuncall,-2],
  ["APPLY",lapply,-2],["EQ",leq,2],["CONS",lcons,2],
  ["CAR",lcar,1,setfcar,2],["CDR",lcdr,1,setfcdr,2],["=",lequ,-2],
  ["<",lless,-2],["+",lplus,-1],["-",lminus,-2],
  ["_PTR_",ltimes,-1],["/",ldivi,-2],["MAKE-FILE-STREAM",lmake_fs,2],
  ["HASH",lhash,1],["IERROR"],["GENSYM",lgensym,0],["STRING",lstring,-1],
  ["FASL",lfasl,1],["MAKEJ",lmakej,2],["MAKEF",lmakef,0],
  ["FREF",lfref,1],["PRINT",lprint,1],["GC",gc,0],["CLOSE-FILE-STREAM",lclose_fs,1],
  ["IVAL",lival,1],["FLOOR",lfloor,-2],["READ-FILE-STREAM",lread_fs,3],
  ["WRITE-FILE-STREAM",lwrite_fs,4],["LOAD",lload,1],["IREF",liref,2,setfiref,3],
  ["LAMBDA"],["CODE-CHAR",lcode_char,1],["CHAR-CODE",lchar_code,1],["_PTR_STANDARD-INPUT_PTR_"],["_PTR_STANDARD-OUTPUT_PTR_"], ["_PTR_ERROR-OUTPUT_PTR_"],["_PTR_PACKAGES_PTR_"],["STRING=",lstring_equal,2],
  ["IMAKUNBOUND",limakunbound,2],["EVAL",leval,-2],["JREF",ljref,2,setfjref,3],
  ["RUN-PROGRAM",lrp,-2],["UNAME",luname,0]]]).
