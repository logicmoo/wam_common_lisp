/* Part of SWI-Prolog

    Author:        Douglas R. Miles, ...
    E-mail:        logicmoo@gmail.com
    WWW:           http://www.logicmoo.org
    Copyright (c)  2016,2017,2021, LogicMOO Basic Tools
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.
                   
    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(each_call_cleanup,
   [
      each_call_cleanup/3,             % +Setup, +Goal, +Cleanup      
      each_call_catcher_cleanup/4,     % +Setup, +Goal, ?Catcher, +Cleanup
      redo_call_cleanup/3,             % +Setup, +Goal, +Cleanup
      trusted_redo_call_cleanup/3      % +Setup, +Goal, +Cleanup
    ]).

/** <module>Utility LOGICMOO EACH CALL
Before a clause does a redo it allows code to be called. To execute between calls during backtracking. Allows us to put code before and after a clause. 

Utility LOGICMOO_EACH_CALL_CLEANUP
	Works together with Each Call to allow code before and after a clause for backtracking. 

- @author Douglas R. Miles
- @license LGPL 

@see  https://groups.google.com/forum/#!searchin/comp.lang.prolog/redo_call_cleanup%7Csort:relevance/comp.lang.prolog/frH_4RzMAHg/2bBub5t6AwAJ

*/

:- set_module(class(library)).

:- meta_predicate
  redo_call_cleanup(0,0,0),
  call_then_cut(0),
  each_call_catcher_cleanup(0,0,?,0),
  each_call_cleanup(0,0,0),
  trusted_redo_call_cleanup(0,0,0).



% call_then_cut(G):- call((G,(deterministic(true)->!;true)))

call_then_cut(G):- 
  prolog_current_choice(CP),  
  prolog_choice_attribute(CP,parent,PC),
  prolog_choice_attribute(PC,frame,Frame),prolog_frame_attribute(Frame,goal,PG),
     prolog_choice_attribute(CP,frame,CFrame),prolog_frame_attribute(CFrame,goal,CG),nop(dmsg(call_then_cut(PG,CG))),
  call((G,(deterministic(true)->prolog_cut_to(PC);true))).



:- module_transparent(pt1/1).
:- module_transparent(pt2/1).


/*
?- undo((write(foo), nl)), !, (X=1; X=2).
X = 1 ;
X = 2.

foo
?- undo(writeln('done!')), (X=1; X=2), writeln(side_effect=X) undo(writeln(removing_side_effect=X)).
X = 1 ;
X = 2.

done!
?- undo(writeln('done!')),  (X=1; X=2).
X = 1 ;
X = 2.

done!

skip_tracing(G):-
  setup_call_cleanup_redo(notrace,G,trace).

undo(writeln('done!')), (X=1; X=2), writeln(side_effect=X) undo(writeln(removing_side_effect=X)).

*/


%! redo_call_cleanup(:Setup, :Goal, :Cleanup).
%
% @warn Setup/Cleanup do not share variables.
% If that is needed, use each_call_cleanup/3 

redo_call_cleanup(Setup,Goal,Cleanup):- 
   assertion(each_call_cleanup:unshared_vars(Setup,Goal,Cleanup)),
   trusted_redo_call_cleanup(Setup,Goal,Cleanup).

trusted_redo_call_cleanup(Setup,Goal,Cleanup):- 
   HdnCleanup = mquietly(Cleanup),   
   setup_call_cleanup(Setup, 
     ((Goal,deterministic(DET)),
        (notrace(DET == true) -> ! ; 
           ((HdnCleanup,notrace(nb_setarg(1,HdnCleanup,true)));
            (Setup,notrace(nb_setarg(1,HdnCleanup,Cleanup)),notrace(fail))))),
        HdnCleanup).

:- '$hide'(trusted_redo_call_cleanup/3).


%! each_call_catcher_cleanup(:Setup, :Goal, +Catcher, :Cleanup).
%
%   Call Setup before Goal like normal but *also* before each Goal is redone.
%   Also call Cleanup *each* time Goal is finished
%  @bug Goal does not share variables with Setup/Cleanup Pairs

each_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup):-
   setup_call_catcher_cleanup(true, 
     each_call_cleanup(Setup, Goal, Cleanup), Catcher, true).

:- thread_local(ecc:'$each_call_cleanup'/2).
:- thread_local(ecc:'$each_call_undo'/2).

%! each_call_cleanup(:Setup, :Goal, :Cleanup).
%
%   Call Setup before Goal like normal but *also* before each Goal is redone.
%   Also call Cleanup *each* time Goal is finished
%  @bug Goal does not share variables with Setup/Cleanup Pairs

each_call_cleanup(Setup,Goal,Cleanup):- 
 ((ground(Setup);ground(Cleanup)) -> 
  trusted_redo_call_cleanup(Setup,Goal,Cleanup);
  setup_call_cleanup(
   asserta((ecc:'$each_call_cleanup'(Setup,Cleanup)),HND), 
   trusted_redo_call_cleanup(pt1(HND),Goal,pt2(HND)),
   (pt2(HND),erase(HND)))).

 		 /*******************************
		 *	  UTILITIES		*
		 *******************************/

:- public(ecc_throw_failure/1).

ecc_throw_failure(Why):- throw(error(assertion_error(Why),_)).

pt1(HND) :- 
   clause(ecc:'$each_call_cleanup'(Setup,Cleanup),true,HND) 
   ->
   ('$sig_atomic'(Setup) -> 
     asserta(ecc:'$each_call_undo'(HND,Cleanup)) ; 
       ecc_throw_failure(failed_setup(Setup)))
   ; 
   ecc_throw_failure(pt1(HND)).

pt2(HND) :- 
  retract(ecc:'$each_call_undo'(HND,Cleanup)) ->
    ('$sig_atomic'(Cleanup)->true ;ecc_throw_failure(failed_cleanup(Cleanup)));
      ecc_throw_failure(failed('$each_call_undo'(HND))).

:- if(true).
:- system:import(each_call_cleanup/3).
:- system:import(each_call_catcher_cleanup/4).
:- system:import(redo_call_cleanup/3).
:- system:import(pt1/1).
:- system:import(pt2/1).
:- endif.

% Only checks for shared vars (not shared structures)
% @TODO what if someone got tricky with setarging?
unshared_vars(Setup,_,_):- ground(Setup),!.
unshared_vars(Setup,Goal,Cleanup):- 
   term_variables(Setup,SVs),
   term_variables(Cleanup,CVs),
   ( CVs==[] -> true; unshared_set(SVs,CVs)),
   term_variables(Goal,GVs),
   ( GVs==[] -> true; 
     (unshared_set(SVs,GVs),
      unshared_set(CVs,GVs))).

unshared_set([],_).
unshared_set([E1|Set1],Set2):- 
   not_in_identical(E1,Set2),
   unshared_set(Set1,Set2).

not_in_identical(X, [Y|Ys]) :- X \== Y, not_in_identical(X, Ys).


