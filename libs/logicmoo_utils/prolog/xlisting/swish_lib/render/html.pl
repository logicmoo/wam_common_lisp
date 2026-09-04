/* <module> swish_render_html
% Provides HTML Rendering in SWISH
%
%
% Logicmoo Project PrologMUD: A MUD server written in Prolog
% Maintainer: Douglas Miles
% Dec 13, 2035
%
*/
:- if(exists_source(swish(lib/render))).

:- module(swish_render_html,
	  [ term_rendering//3			% +Term, +Vars, +Options
	  ]).
% % % OFF :- system:use_module(swi(library/http/html_write)).
% % % OFF :- system:use_module(swish(lib/render)).

:- register_renderer(html, "Render html representations").

/* <module> SWISH html renderer

*/

%%	term_rendering(+Term, +Vars, +Options)//
%
%	Render an N-queens  problem.  This   renderer  assumes  that the
%	solution is represented by a permutation   of a list of integers
%	1..N, where the I-th integer describes   the column of the queen
%	at row I.



term_rendering(Term, _Vars, _Options) --> {compound(Term),Term=html(_)}, html(Term).

:- endif.
