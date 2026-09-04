:- module(bfly_term_html,
          [ bfly_term//2                             % +Term, +Options
          ]).

/** <module> Represent Prolog terms as HTML

This file is primarily designed to   support running Prolog applications
over the web. It provides a   replacement for write_term/2 which renders
terms as structured HTML.
*/

:- reexport(pretty_clauses).
