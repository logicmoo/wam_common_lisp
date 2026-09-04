
:- include(sanity_tests).


% ===================================================================
%  TESTING
% ===================================================================

:- abolish(scce_test/1).
:- dynamic(scce_test/1).
:- retractall(scce_test(_)).

w0(X):- setup_call_cleanup_each(writeln(start),(between(1,3,X),writeln(X)), writeln(end)).
w0(REF,X):-
 flag(scce_test_flag,_,1),
 setup_call_cleanup_each(
         (flag(scce_test_flag,F,F+1),asserta(scce_test(F),REF),nl,nl,wdmsg(enter(REF:F:X))),
         (between(1,3,X),wdmsg(goal(REF:F:X))),
         (erase(REF),wdmsg(cleanup(REF:F:X)),nl)),
 \+ call(scce_test(_)),
 flag(scce_test_flag,FWas,FWas),
 FWas is F + 1,
 wdmsg(success(REF:F=FWas:X)),
 nl,nl.


% Could current predicates such as:   profile/1  quietly/1 with_output_to_*/2  PL_call()  (possibly ignore/1)  benefit as well like with_output_to/2 does?

% we wanted to code this
:- meta_predicate with_output_to_scc(*,0).
with_output_to_scc(D,G):- current_output(W), setup_call_cleanup(set_output(D),G,set_output(W)).

% However it doesn't work with didnt work with setup_call_cleanup/3 
% becasue "hi" was sent to 'user_error' 2 times!
scce_sad:- with_output_to_scc(user_error,( between(1,3,M),wdmsg(M))),  wdmsg("hi"), fail.

% So we ended up lowering out expectations:
:- meta_predicate with_output_to_wa(*,0).
with_output_to_wa(D,G):- current_output(W), setup_call_cleanup(set_output(D),once(G),set_output(W)).

% Fixed, since "hi" was no longer sent to 'user_error' !    (However 2 and 3 were no longer sent!?)
scce_sadder:- with_output_to_scc(user_error,( between(1,3,M),wdmsg(M))),  wdmsg("hi"), fail.

% But really all along I assumed we wanted this:
:- meta_predicate with_output_to_scce(*,0).
with_output_to_scce(D,G):- current_output(W),scce_orig(set_output(D),G,set_output(W)).

% *Still* Fixed since "hi" was no longer sent to 'user_error' !   (However *this time* 2 and 3 *are* sent as expected)
scce_good:- with_output_to_scce(user_error,( between(1,3,M),wdmsg(M))), wdmsg("hi"), fail.

% ===================================================================
%  SCCE_V4
% ===================================================================
:- meta_predicate scce_idea(0,0,0).

scce_idea(S,G,C):-
  b_setval(setup_e_4,v(S,G,C)),
  nb_linkval(setup_e_4,v(S,G,C)),
  setup_call_cleanup(setup_f_4,
   (((G,deterministic(Det),true) *->
     (Det == true -> ! ; (cleanup_4;(setup_e_4,fail))))),
   cleanup_4), 
   (exit_4(S,G,C)).
  
exit_4(S,G,C):-nb_getval(cleanup_4,v(S1,G1,C1)),ignore(S=S1),ignore(G=G1),ignore(C=C1).

cleanup_4:- nb_getval(cleanup_4,v(_S1,_G1,C1)),call(C1).
setup_f_4:- 
  nb_getval(setup_e_4,Orig),Orig=v(S,_G,_C),
  copy_term(Orig,Copy),Copy=v(S0,G0,C0),  
  call(S0),nb_setval(cleanup_4,v(S,G0,C0)).
setup_e_4:- 
  nb_getval(setup_e_4,Orig),
  copy_term(Orig,v(S0,G0,C0)),
  call(S0),nb_setval(cleanup_4,v(S0,G0,C0)).

% :- set_prolog_flag(scce,pure).
% :- set_prolog_flag(scce,scce_orig).
% :- set_prolog_flag(scce,setup_call_cleanup).















% ===================================================================
%  Now unused versions
% ===================================================================

end_of_file.



setup0:-  nb_getval(in,IN),
	  IN  =  v(S1,G1,C1),
	  copy_term(S1,S2),
	  call(S1),	  
	  NEXT = v(S2,G1,C1),
	  nb_setval(in,NEXT).


setup2:-  nb_getval(in,IN),
	  IN  =  v(S1,G1,C1),
	  copy_term(S1,S2),
	  call(S1),	  
	  NEXT = v(S2,G1,C1),
	  nb_setval(in,NEXT).


scce2(S0,G0,C0):-
 IN = v(S0,G0,C0),
 ((quietly((
  nb_setval(orig,IN),
  nb_setval(blank_orig, BLANK_orig),  
  make_nb_setter(IN  , _, nb_setarg, BLANK_orig),
  nb_setval(in,IN),
  IN = v(S0,G0,C0),
  make_nb_setter(IN  , _, nb_setarg, BLANK_in),
  nb_setval(blank_in, BLANK_in))))),
    copy_term(S0+G0+C0,S9+_G9+_C9),
  % make_nb_setter(G0+S9+IN+C9+G9+C0+S0,UnsetSetup),
  setup2(S9),
  call(call,G0),
  ignore(cleanup2),
  setup2(S0),
  ignore(call(call,C0)).

cleanup2:- quietly(nb_getval(in,v(_,_,C))),call(C).
setup2(S0):- 
          quietly((nb_getval(in,IN),
	  IN  =  v(S0,G0,C0),
	  NX  =  v(S1,_G1,_C1),
	  copy_term(IN,NX))),
	  call_gvar(blank_in),
	  call(S0),
	  nb_setval(in,v(S1,G0,C0)).
	  


scce1(S0,G0,C0):- !,
  IN = v(S0,G0,C0,Vs),
  nop((UnsetSetup0,UnsetSetup1,UnsetSetup2)),
  term_variables(scce1(S0,G0,C0),Vs),
  nb_setval(in,IN),
  make_nb_setter(IN, BLANK_in),
  nb_setval(blank_in, BLANK_in),
  shared_vars(S0,(G0+C0),SGCVs), 
  copy_term(SGCVs,VC0), make_nb_setter5(SGCVs,VC0,nb_setarg,S0,UnsetSetup0),
  copy_term(SGCVs,VC1), make_nb_setter5(SGCVs,VC1,nb_setarg,(S0+G0),UnsetSetup1),
  copy_term(SGCVs,VC2), make_nb_setter5(SGCVs,VC2,nb_setarg,(S0+G0),UnsetSetup2),
  S = call(S0),
  G = (call(call,G0),quietly(UnsetSetup0)),
  C = (call(call,C0),quietly(UnsetSetup1)),
     catch((
        call((once(S0),G,deterministic(Det),true))
        *->
        (Det == true
         -> (once(C0),!)
          ; (once(C0);(once((UnsetSetup2,S))),fail))
     ; (once(C0),!,fail)),
     E, (ignore(once(C)),throw(E))).


scce1(S0,G0,C0):-
  IN = v(S0,G0,C0,Vs),
  term_variables(IN,Vs),
  nb_setval(in,IN),
  make_nb_setter(IN, BLANK_in),
  nb_setval(blank_in, BLANK_in),
  shared_vars(S0,(G0+C0),SGCVs), 
  copy_term(SGCVs,VC0),
  copy_term(SGCVs,VC1),
  make_nb_setter5(SGCVs,VC0,nb_setarg,IN,UnsetSetup0),
  make_nb_setter5(SGCVs,VC1,nb_setarg,IN,UnsetSetup1),
  !,
  setup1(true,S0),
  call(G0),
  ignore(C0),
  UnsetSetup0,
  setup1(UnsetSetup1,S0),
  call(C0).

  
cleanup1:- quietly(nb_getval(in,v(_,_,C,_))),call(C).

setup1(UnsetSetup,S0):-
        quietly((
	  nb_getval(in,IN),
	  IN  =  v(S0,G0,C0),
	  NX  =  v(S1,_G1,_C1),
	  copy_term(IN,NX))),
	  quietly(call_gvar(blank_in)),	
	  once(S0),
	  quietly(UnsetSetup),
	  nb_setval(in,v(S1,G0,C0)).


scce_idea(S0,G0,C0):-
    S = call(throw(sss)),
    G = call(throw(ggg)),
    C = call(throw(uuu)),
    MergeVars = ignore((arg(1,C,C0),arg(1,G,G0),arg(1,S,S0))),   
    S2 = ((\+ \+ ((nb_setarg(1,S,S0),nb_setarg(1,G,G0),nb_setarg(1,C,C0) )))),
    scce_orig(S2,(S2,MergeVars),(call(call,S),call(call,G)),(MergeVars,call(call,C))).

scce_idea(S0,G0,C0):-
    G = call(throw(ggg)),
    C = call(throw(uuu)),
    S = ((\+ \+ ((S0,nb_setarg(1,G,G0),nb_setarg(1,C,C0) ))),MergeVars = (arg(1,G,G0),MergeC), MergeC = arg(1,C,C0)),
    scce_orig(S,(nop(wdmsg(G)),nl,call(call,G)),(wdmsg(C),call(call,C),MergeVars,wdmsg(MergeVars))),MergeVars,nl.

scce_idea(S0,G0,C0):-
    S = call(S0),
    G = call(throw(ggg)),
    C = call(throw(uuu)),
    _MergeVars = ignore((arg(1,C,C0),arg(1,G,G0),arg(1,S,S0))),
    SU = ((\+ \+ ((nb_setarg(1,S,S0),nb_setarg(1,G,G0),nb_setarg(1,C,C0) )))),
    scce_orig(call(call,S),(SU,S),call(call,G),call(call,C)).


scce_idea(S,G,C):-
       term_variables(v(S,G,C),Vs),
       duplicate_term(v(S,G,C),v(SD,GD,CD)),
       gather_nb_setargs_goals(Vs,v(SD,GD,CD),NBSetargClosure),
       SEach = (\+ \+ ((SD,NBSetargClosure))),
       scce_orig(SEach,GD,CD).


scce_idea(S0,G0,C0):- fail,
     make_lkey(scc1(S0,G0,C0),Key),
     copy_term(scc1(S0,G0,C0),scc1(S1,G1,C1)),
     MergeVars = ignore((C1=C0,G1=G0,S1=S0)),
     S2 = ((\+ \+ ((nb_setarg(1,S,S0),nb_setarg(1,G,G0),nb_setarg(1,C,C0) )))),
     S = call(S2),
     G = call(G1),
     C = call(C1),
     S1,!,
     call((G,deterministic(Det),true))
        *->
        (Det == true
         -> (once(C),!)
          ; (once(C);(once(S),fail)))
     ; (once(C),!,fail)),
     E, (ignore(once(C)),throw(E)).


end_of_file.


get_vars:- nb_getval(setup,S),nb_getval(goal,G),nb_getval(cleanup,C),term_variables(v(S,G,C),Vs),nb_setval(sgcvars,Vs).
set_vars:- nb_getval(setup,S),nb_getval(goal,G),nb_getval(cleanup,C),term_variables(v(S,G,C),Vs),nb_getval(sgcvars,VVs),must(Vs=VVs).

merge_vars:- nb_getval(merge_vars,MV),call(MV).

call_gvar(Var):- nb_getval(Var,C),must(call(call,C)).

% goal_saved:- call_gvar(goal_saved).

use_undoer:- call_gvar(undoer).

% :- nb_setval(query_result,sol(0,1,false,false)).
with_no_dupies(G,List):-S=v([]),G,arg(1,S,Was),List=[CG|Was],copy_term(G,CG),nb_setarg(1,S,List).

:- dynamic(scce0/0).





  
/*


scce3(S0,G0,C0):- fail,
    G = call(throw(ggg)),
    C = call(throw(uuu)),

    shared_vars(S0,G0,SGvs),
    copy_term(S0+G0+SGvs,_CS0+_CG0+CSGvs),

    make_nb_setter(SGvs,CSGvs,G0,nb_setarg,SubSGs),

    S = ((\+ \+ ((S0,nb_setarg(1,G,G0),nb_setarg(1,C,C0) ))),

      scce_orig(S,(SubSGs,S),(call(call,G)),(call(call,C),UNSET)).
*/

scce3(S0,G0,C0):- !,

 quietly(( S = call(call,S0),
  G = call(call,G0),
  C = call(call,C0),
  UnSU = (UnS,nb_setarg(2,G,G0)),
  make_nb_setter(scce3(S,G,C),UnS))),

 quietly((

  nb_setval(orig,IN),
  IN = v(S0,G0,C0),
  nb_setval(blank_orig, BLANK_orig),  
  make_nb_setter(IN  , _, nb_setarg, BLANK_orig),

  nb_setval(in,IN),
  IN = v(S0,G0,C0),
  nb_setval(blank_in, BLANK_in),  
  make_nb_setter(IN  , _, nb_setarg, BLANK_in))),

     setup1,!,
       (call((G,deterministic(Det),true))
        *->
        (Det == true
         -> (once(C),!)
          ; (once(C);(once((UnSU,setup1)),fail)))
     ; (once(C),!,fail)).



scce11(S0,G0,C0):- 
  IN  =  v(S0,G0,C0),
  nb_setval(in,IN),  
  make_nb_setter(IN  , _BLANK, nb_setarg, BLANK_IN),
  nb_setval(blank_in,BLANK_IN),
  setup1,
  scce_orig(setup2,call_goal_saved_nd,cleanup).
  
call_scc1:- create_undoer,use_undoer,once(setup1),fail.
call_scc1:- call_goal_saved_nd *-> (cleanup ,(was_det->!;(setup2,fail))) ; (cleanup,!,fail).
% call_scc1:- repeat, once(setup2), call_goal_saved_nd *->cleanup; setupup2.

create_undoer:-
  nb_setval(in,v(S0,G0,C0)),
  make_nb_setter(v(S0,G0,C0),_,nb_setarg,Undoer),
  nb_setval(undoer,Undoer).











	

end_of_file.
scce1(S0,G0,C0):-
 quietly((
  nb_setval(orig,IN),
  nb_setval(blank_orig,BLANK_IN),  
    IN = v(S0,G0,C0),
  COPY = v(S1,G1,C1),
  NEXT = v(S2,G2,C2),
  make_nb_setter(IN  , _BLANK, nb_setarg, BLANK_IN),  
  copy_term(IN,COPY),
  copy_term(IN,NEXT) )),
  setup_call_cleanup(S0, G0 ,C0),
  ignore(IN = COPY),
  ignore(IN = NEXT).


scce_idea(_S2,G1,_C1):-
  (G1 *-> (cleanup ; (setup1,fail)) ; (cleanup,fail)).

scce_idea(S0,G0,C0):- 
  IN  =  v(S0,G0,C0),
  nb_setval(in,IN),  
  make_nb_setter(IN  , _BLANK, nb_setarg, BLANK_IN),
  nb_setval(blank_in,BLANK_IN),
  setup1,
  scce_orig(setup2,call_goal_saved_nd,cleanup).

  
/*
          OUT =  v(S0,G0,C0),
	     
	  make_nb_setter(IN  ,NEXT, nb_setarg, NEXT_TO_IN),
	  make_nb_setter(NEXT, OUT , nb_setarg,OUT_TO_NEXT),
	  make_nb_setter(IN  , OUT , arg,OUT_ARG_IN),


setup2:-  nb_getval(setup2,v(S1,G1,C1)),call(S1),nb_setval(in,v(S1,G1,C1)).

*/

  make_nb_setter(v(S0,G0,C0),_,nb_setarg,Undoer),
  copy_term(v(S0,G0,C0),v(SV,GV,CV)),
  make_nb_setter(v(S0,G0,C0),v(SV,GV,CV),nb_setarg,Undoer0),
  make_nb_setter(v(SV,GV,CV),v(S0,G0,C0),nb_setarg,Redoer0),
  make_nb_setter(v(SV,GV,CV),v(S0,G0,C0),arg,FillerIn),
  asserta((setup1:-  nb_getval(setup,S1),call(S1),get_vars,nb_setval(goal,G1),nb_setval(cleanup,C1))),
  asserta((setup2:-  nb_getval(setup,S2),call(S2),true,   ,nb_setval(goal,G2),nb_setval(cleanup,C2))),
  G = call(G0),
  nb_setval(goal_saved,G),
  scce_orig(in,setup2,goal_saved,cleanup).
  FillerIn.

  
  


  term_variables(scc1(S0,G0,C0),Vs0),
  copy_term(scc1(S0,G0,C0),scc1(SV,GV,CV)),
  term_variables(scc1(SV,GV,CV),VsV),
  nb_setval(merge_vars,Vs0=VsV),
  
  asserta((cleanup:- nb_getval(cleanup,C),merge_vars,call(C))),
  asserta((setup2:- nb_getval(setup,S2),S2,nb_setval(goal,G2),nb_setval(cleanup,C2))),

  scce_orig(in,call1,cleanup).

     make_lkey(scc1(S0,G0,C0),Key),
     copy_term(scc1(S0,G0,C0),scc1(S1,G1,C1)),
     MergeVars = ignore((C1=C0,G1=G0,S1=S0)),
     S2 = ((\+ \+ ((nb_setarg(1,S,S0),nb_setarg(1,G,G0),nb_setarg(1,C,C0) )))),
     S = call(S2),
     G = call(G1),
     C = call(C1),
     S1,!,     
    


scce(S,G,C):-
     scce_key(scce(S,G,C),Key),
     setup_call_cleanup_each(key_call(Key,1),key_call(Key,2),key_call(Key,3)).

key_call(Key,Arg):- nb_current(Key,In),arg(Arg,In,Goal),Goal,nb_setval(Key,In).
scce_key(In,Key):- format(atom(Key),'~q',[In]),nb_setval(Key,In).

% :- '$set_source_module'(system_scce).

