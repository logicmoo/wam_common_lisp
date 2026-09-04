

:- include(sanity_tests).

:- use_module(library(each_call_cleanup)).


test(each_call_cleanup_3):- forall(each_call_cleanup(writeln(start),(between(1,3,X),writeln(X)), writeln(end)),true). 

test(each_call_cleanup_ref_mid):- 
  forall(
    each_call_cleanup(
           true,
	   (asserta(scce0,REF),between(1,3,X),writeln(REF:X),erase(REF)),
          writeln(end)),
    true). 



% todo - fix so ref is bound
test(each_call_cleanup_ref_balanced):- 
  nop(forall(
    redo_call_cleanup(
          asserta(scce0,REF),
	   (between(1,3,X),writeln(REF:X)),
          (erase(REF),writeln(end))),
    true)),wdmsg(todo(test(each_call_cleanup_ref_balanced))). 


% todo - fix so ref is bound
test(each_call_cleanup_ref_early):- 
  nop(forall(
    redo_call_cleanup(
          asserta(scce0,REF),
	   (between(1,3,X),writeln(REF:X),erase(REF)),
          writeln(end)),
    true)),wdmsg(todo(test(each_call_cleanup_ref_early))). 




