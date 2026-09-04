/* Part of LogicMOO Base Logicmoo Debug Tools
% ===================================================================
% File '$FILENAME.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: '$FILENAME.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% Licience: LGPL
% ===================================================================
*/
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.
end_of_file.

% File: /opt/PrologMUD/pack/logicmoo_base/prolog/logicmoo/util/logicmoo_util_catch.pl
:- module(logicmoo_util_block3,
   [
   % !/1,block/2,keep/2,block/3,set_block_exit/2
   ]).

:- meta_predicate !/1,keep/2,set_block_exit/2,block/2,block/3.

% block(test, (repeat, !(test), fail))).
:- meta_predicate block(+, :, ?). 

%= 	 	 

%% block( +Name, ?Goal, ?Var) is semidet.
%
% Block.
%
block(Name, Goal, Var) :- Goal, keep(Name, Var).	% avoid last-call and GC 

%= 	 	 

%% keep( ?VALUE1, ?VALUE2) is semidet.
%
% Keep.
%
keep(_, _).  

%= 	 	 

%% set_block_exit( ?Name, ?Value) is semidet.
%
% Set Block Exit.
%
set_block_exit(Name, Value) :-  prolog_current_frame(Frame),  prolog_frame_attribute(Frame, parent_goal,  mcall:block(Name, _, Value)). 

%= 	 	 

%% block( ?Name, ?Goal) is semidet.
%
% Block.
%
block(Name, Goal) :-  block(Name, Goal, Var),  (   Var == !  ->  !  ;   true  ). 

%= 	 	 

%% !( ?Name) is semidet.
%
% !.
%
!(Name) :- set_block_exit(Name, !). 

% :- export((block/3, set_block_exit/2, block/2, !/1 )).

