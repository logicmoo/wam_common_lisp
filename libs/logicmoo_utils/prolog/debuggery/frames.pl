% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_prolog_frames.pl
:- module(frames,
          [ current_frames/4,
            current_next_frames/4,
            in_pengines/0,
            find_parent_frame_attribute/5,
            parent_goal/2,
            parent_goal/1,
            prolog_frame_match/3,
            relative_frame/3,
            stack_check/0,
            stack_check/1,
            stack_check/2,
            stack_check_else/2,
            stack_check_or_call/2,
            stack_depth/1
          ]).
:- module_transparent
        current_frames/4,
        current_next_frames/4,
        in_pengines/0,
        find_parent_frame_attribute/5,
        parent_goal/2,
        parent_goal/1,
        prolog_frame_match/3,
        relative_frame/3,
        stack_check/0,
        stack_check/1,
        stack_check/2,
        stack_check_else/2,
        stack_check_or_call/2,
        stack_depth/1.

:- set_module(class(library)).

  

/*
:- mpred_trace_nochilds(stack_depth/1).
:- mpred_trace_nochilds(stack_check/0).
:- mpred_trace_nochilds(stack_check/1).
:- mpred_trace_nochilds(stack_check/2).
*/

%= 	 	 

%% stack_depth( ?Level) is semidet.
%
% Stack Depth.
%
stack_depth(Level):-quietly((prolog_current_frame(Frame),prolog_frame_attribute(Frame,level,Level))).


:-  module_transparent stack_check/0.
:-  module_transparent stack_check/1.

%% stack_check is semidet.
%
% Stack Check.
%
stack_check:- sanity(stack_check(6606)).

%= 	 	 

%% stack_check( ?BreakIfOver) is semidet.
%
% Stack Check.
%
stack_check(BreakIfOver):- stack_check_else(BreakIfOver, (ds,trace,break,trace_or_throw(stack_check(BreakIfOver)))).

%= 	 	 

%% stack_check( ?BreakIfOver, ?Error) is semidet.
%
% Stack Check.
%
stack_check(BreakIfOver,Error):- stack_check_else(BreakIfOver, trace_or_throw(stack_check(BreakIfOver,Error))).

%= 	 	 

%% stack_check_else( ?BreakIfOver, ?Call) is semidet.
%
% Stack Check Else.
%
stack_check_else(BreakIfOver,Call):- stack_depth(Level) ,  ( Level < BreakIfOver -> true ; (dbgsubst(Call,stack_lvl,Level,NewCall),NewCall)).

stack_check_or_call(BreakIfOver,Call):- stack_depth(Level) ,  ( Level < BreakIfOver -> true ; call(Call)).



%= 	 	 

%% in_pengines is semidet.
%
% In Pengines.
%
in_pengines:- zotrace(relative_frame(context_module,pengines,_)).

% ?- relative_frame(context_module,X,Y).
:- export(relative_frame/3).

%= 	 	 

%% relative_frame( ?Attrib, ?Term, ?Nth) is semidet.
%
% Relative Frame.
%
relative_frame(Attrib,Term,Nth):- find_parent_frame_attribute(Attrib,Term,Nth,_RealNth,_FrameNum).



%= 	 	 

%% parent_goal( ?Goal) is semidet.
%
% Parent Goal.
%

:- export(parent_goal/1).
parent_goal(Goal):- nonvar(Goal), quietly((prolog_current_frame(Frame),prolog_frame_attribute(Frame,parent,PFrame),
  prolog_frame_attribute(PFrame,parent_goal,Goal))).
parent_goal(Goal):- !, quietly((prolog_current_frame(Frame),prolog_frame_attribute(Frame,parent,PFrame0),
  prolog_frame_attribute(PFrame0,parent,PFrame),
  goals_above(PFrame,Goal))).

goals_above(Frame,Goal):- prolog_frame_attribute(Frame,goal,Term),unify_goals(Goal,Term).
goals_above(Frame,Goal):- prolog_frame_attribute(Frame,parent,PFrame), goals_above(PFrame,Goal).

unify_goals(Goal,Term):- (var(Goal);var(Term)),!,Term=Goal.
unify_goals(M:Goal,N:Term):-!, unify_goals0(Goal,Term),M=N.
unify_goals(Goal,_:Term):-!, unify_goals0(Goal,Term).
unify_goals(_:Goal,Term):-!, unify_goals0(Goal,Term).

unify_goals0(X,X).

%= 	 	 

%% parent_goal( ?Goal, ?Nth) is semidet.
%
% Parent Goal.
%
parent_goal(Goal,Nth):-  number(Nth),!, prolog_current_frame(Frame),prolog_frame_attribute(Frame,parent,PFrame),nth_parent_goal(PFrame,Goal,Nth).
parent_goal(Goal,Nth):-  find_parent_frame_attribute(goal,Goal,Nth,_RealNth,_FrameNum).


%= 	 	 

%% nth_parent_goal( ?Frame, ?Goal, ?Nth) is semidet.
%
% Nth Parent Goal.                              
%
nth_parent_goal(Frame,Goal,Nth):- Nth>0, Nth2 is Nth-1, prolog_frame_attribute(Frame,parent,PFrame),!,zotrace((nth_parent_goal(PFrame,Goal,Nth2))).
nth_parent_goal(Frame,Goal,_):- zotrace((prolog_frame_attribute(Frame,goal,Goal))),!.

:- export(find_parent_frame_attribute/5).

%= 	 	 

%% find_parent_frame_attribute( ?Attrib, ?Term, ?Nth, ?RealNth, ?FrameNum) is semidet.
%
% Find Parent Frame Attribute.
%
find_parent_frame_attribute(Attrib,Term,Nth,RealNth,FrameNum):-quietly((ignore(Attrib=goal),prolog_current_frame(Frame),
                  current_frames(Frame,Attrib,5,NextList))),!,                 
                  catch(nth1(Nth,NextList,Out),E,(wdmsg(E),trace,nth1(Nth,NextList,Out))),
                  Out = RealNth-FrameNum-Term.



%= 	 	 

%% prolog_frame_match( ?Frame, :TermAttrib, :TermTerm) is semidet.
%
% Prolog Frame Match.
%
prolog_frame_match(Frame,goal,Term):-!,prolog_frame_attribute(Frame,goal,TermO),!,Term=TermO.
prolog_frame_match(Frame,parent_goal,Term):-nonvar(Term),!,prolog_frame_attribute(Frame,parent_goal,Term).
prolog_frame_match(Frame,not(Attrib),Term):-!,nonvar(Attrib),not(prolog_frame_attribute(Frame,Attrib,Term)).
prolog_frame_match(_,[],X):-!,X=[].
prolog_frame_match(Frame,[I|IL],[O|OL]):-!,prolog_frame_match(Frame,I,O),!,prolog_frame_match(Frame,IL,OL),!.
prolog_frame_match(Frame,Attrib,Term):-prolog_frame_attribute(Frame,Attrib,Term).


%= 	 	 

%% current_frames( ?Frame, ?Attrib, :GoalN, ?NextList) is semidet.
%
% Current Frames.
%
current_frames(Frame,Attrib,N,NextList):- notrace(current_frames0(Frame,Attrib,N,NextList)).
current_frames0(Frame,Attrib,N,NextList):- N>0, N2 is N-1,prolog_frame_attribute(Frame,parent,ParentFrame),!,current_frames0(ParentFrame,Attrib,N2,NextList).
current_frames0(Frame,Attrib,0,NextList):- current_next_frames(Attrib,1,Frame,NextList).


%= 	 	 

%% current_next_frames( ?Attrib, ?Nth, ?Frame, ?NextList) is semidet.
%
% Current Next Frames.
%
current_next_frames(Attrib,Nth,Frame,[Nth-Frame-Term|NextList]):- zotrace((prolog_frame_match(Frame,Attrib,Term))), !,
   (prolog_frame_attribute(Frame,parent,ParentFrame) -> 
    ( Nth2 is Nth+1, current_next_frames(Attrib,Nth2, ParentFrame,NextList));
         NextList=[]).
current_next_frames(Attrib,Nth,Frame,NextList):- 
   (prolog_frame_attribute(Frame,parent,ParentFrame) -> 
    ( Nth2 is Nth+1, current_next_frames(Attrib,Nth2, ParentFrame,NextList));
         NextList=[]).
current_next_frames(_,_,_,[]).



:- ignore((source_location(S,_),prolog_load_context(module,M),module_property(M,class(library)),
 forall(source_file(M:H,S),
 ignore((functor(H,F,A),
  ignore(((\+ atom_concat('$',_,F),(export(F/A) , current_predicate(system:F/A)->true; system:import(M:F/A))))),
  ignore(((\+ predicate_property(M:H,transparent), module_transparent(M:F/A), \+ atom_concat('__aux',_,F),debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))))).

 
