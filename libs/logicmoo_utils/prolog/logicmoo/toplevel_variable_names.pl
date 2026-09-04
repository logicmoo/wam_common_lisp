:- module(toplevel_variable_names, []).
/** <module> Utility LOGICMOO TOPLEVEL VARIABLE NAMES
	Allows manipulation of variables that were used in the query. 
- @author Douglas R. Miles
- @license LGPL 
*/

:- set_module(class(library)).


user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):- fail,
    % Have vars to expand and varnames are empty
    quietly((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])), % this prevents the loop
    b_setval('$variable_names', Bindings),  
    debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    expand_query(Goal, Expanded, Bindings, ExpandedBindings).



/*


user:expand_query(Goal, Expanded, Bindings, ExpandedBindings):-    
    % Have vars to expand and varnames are empty
    quietly((Bindings\==[],prolog_load_context(variable_names,Vs), Vs ==[])),
    b_setval('$variable_names', Bindings),  % this prevents the loop
    debug(expand_query,'~q',[b_setval('$variable_names', Bindings)]),
    (toplevel_variables:expand_query(Goal, Expanded0, Bindings, ExpandedBindings0) -> true; 
      (Goal = Expanded0, Bindings = ExpandedBindings0)),
    (user:expand_query(Expanded0, Expanded, ExpandedBindings0, ExpandedBindings) -> true ; 
     (Expanded0 = Expanded, ExpandedBindings0 = ExpandedBindings)).


*/

