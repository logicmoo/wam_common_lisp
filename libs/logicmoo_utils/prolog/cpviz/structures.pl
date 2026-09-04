%   Package: structures
%   Author : Mats Carlsson
%   Updated: 18 March 2010
%   Purpose: quick and dirty replacement for ECLiPSe's arg-by-name syntax,
%            hard-wired for 'visualization' and 'visualizer' only
%   Development version, in flux!

:- module(structures, [get_struct/2]).

/***
Example usage
=============

| ?- get_struct(visualization(parent:Parent,tree_stream:Stream), S).
S = visualization(_A,_B,_C,_D,_E,_F,_G,Parent,_H,_I,_J,Stream,_K) ? 

% source_info
| ?- get_struct(visualization, S).
S = visualization(_A,_B,_C,_D,_E,_F,_G,_H,_I,_J,_K,_L,_M) ? 

Translation from ECLiPSe to SICStus
===================================

Replace:

p(visualization{root:Root,
		tree_root:TreeRoot,
		output:Dir,
		tree_stream:TreeStream,
		stream:Stream}):- ...

by:

p(S) :-
	get_struct(visualization(root:Root,
				 tree_root:TreeRoot,
				 output:Dir,
				 tree_stream:TreeStream,
				 stream:Stream), S) :-
	...


% :-export struct(visualization(root,
%                               tree_root,
%                               output,
%                               ignore_fixed,
%                               var_arg,
%                               name_arg,
%                               focus_arg,
%                               parent,
%                               stream,
%                               range_from,
%                               range_to,
%                               tree_stream,
%                               visualizers)).
% :-export struct(visualizer(id,
%                            type,
%                            type_name,
%                            previous,
%                            display,
%                            x,
%                            y,
%                            group,
%                            width,
%                            height,
%                            min,
%                            max)).
***/

get_struct(Pattern, Term) :-
	Pattern =.. [PHead|PTail],
	get_struct(PHead, PTail, Term), !.

get_struct(visualization, Fields, Term) :-
	functor(Term, visualization, 13),
	(   foreach(K:V,Fields),
	    param(Term)
	do  get_field_argno(K, visualization, I),
	    arg(I, Term, V)	    
	).
get_struct(visualizer, Fields, Term) :-
	functor(Term, visualizer, 12),
	(   foreach(K:V,Fields),
	    param(Term)
	do  get_field_argno(K, visualizer, I),
	    arg(I, Term, V)	    
	).

get_field_argno(root, visualization, 1).
get_field_argno(tree_root, visualization, 2).
get_field_argno(output, visualization, 3).
get_field_argno(ignore_fixed, visualization, 4).
get_field_argno(var_arg, visualization, 5).
get_field_argno(name_arg, visualization, 6).
get_field_argno(focus_arg, visualization, 7).
get_field_argno(parent, visualization, 8).
get_field_argno(stream, visualization, 9).
get_field_argno(range_from, visualization, 10).
get_field_argno(range_to, visualization, 11).
get_field_argno(tree_stream, visualization, 12).
get_field_argno(visualizers, visualization, 13).
get_field_argno(id, visualizer, 1).
get_field_argno(type, visualizer, 2).
get_field_argno(type_name, visualizer, 3).
get_field_argno(previous, visualizer, 4).
get_field_argno(display, visualizer, 5).
get_field_argno(x, visualizer, 6).
get_field_argno(y, visualizer, 7).
get_field_argno(group, visualizer, 8).
get_field_argno(width, visualizer, 9).
get_field_argno(height, visualizer, 10).
get_field_argno(min, visualizer, 11).
get_field_argno(max, visualizer, 12).

