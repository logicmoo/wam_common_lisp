

% :- include(gvar_fixup_exports).

:- if( \+ current_prolog_flag(xref,true)).

:- if( \+ current_predicate(f:gvar_file_predicates_are_exported/2)).

lmconfig:never_export_named_gvar(attr_unify_hook/2).
lmconfig:never_export_named_gvar(attribute_goals/3).
lmconfig:never_export_named_gvar(project_attributes/2).
lmconfig:never_export_named_gvar(attr_portray_hook/2).


:- module_transparent(f:gvar_file_predicates_are_exported/2).
:- export(f:gvar_file_predicates_are_exported/2).
f:gvar_file_predicates_are_exported(S,LC):-
 forall(source_file(M:H,S),
 ignore((functor(H,F,A), \+ atom_concat('$',_,F), \+ lmconfig:never_export_named_gvar(F/_),
  ignore(((atom(LC),atom(M), LC\==M,M:export(M:F/A),LC:multifile(M:F/A),fail,atom_concat('$',_,F),LC:import(M:F/A)))),
  ignore(((\+ atom_concat('$',_,F),\+ atom_concat('__aux',_,F),LC:export(M:F/A), 
  ignore(((current_predicate(system:F/A)->true; system:import(M:F/A)))))))))).

%% f:gvar_file_predicates_are_transparent() is det.
%
% All Module Predicates Are Transparent.
:- module_transparent(f:gvar_file_predicates_are_transparent/0).
f:gvar_file_predicates_are_transparent:-
 source_location(S,_), prolog_load_context(module,LC),
 f:gvar_file_predicates_are_transparent(S,LC).

:- module_transparent(f:gvar_file_predicates_are_transparent/2).
f:gvar_file_predicates_are_transparent(S,LC):- 
 forall(source_file(M:H,S),
 (functor(H,F,A),  
  ignore(((\+ predicate_property(M:H,transparent), ignore( LC = M), 
  module_transparent(M:F/A), 
  \+ atom_concat('__aux',_,F),
   nop(debug(modules,'~N:- module_transparent((~q)/~q).~n',[F,A]))))))).


%% f:gvar_file_predicates_are_exported() is det.
%
% All Module Predicates Are Exported.

:- module_transparent(f:gvar_file_predicates_are_exported/0).

f:gvar_file_predicates_are_exported:- current_prolog_flag(xref,true),!.
f:gvar_file_predicates_are_exported:-
 source_location(S,_), prolog_load_context(module,LC),
 % writeln(f:gvar_file_predicates_are_exported(S,LC)),
 f:gvar_file_predicates_are_exported(S,LC).

:- endif.

:- 
   f:gvar_file_predicates_are_exported,
   f:gvar_file_predicates_are_transparent.

:- endif.

