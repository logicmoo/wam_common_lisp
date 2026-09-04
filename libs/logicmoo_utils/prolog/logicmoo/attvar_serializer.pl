/*  Logicmoo Debug Tools
% ===================================================================
% File 'logicmoo_util_varnames.pl'
% Purpose: An Implementation in SWI-Prolog of certain debugging tools
% Maintainer: Douglas Miles
% Contact: $Author: dmiles $@users.sourceforge.net ;
% Version: 'logicmoo_util_varnames.pl' 1.0.0
% Revision: $Revision: 1.1 $
% Revised At:  $Date: 2002/07/11 21:57:28 $
% ===================================================================
*/
% NEW
:- module(attvar_serializer, [
          deserialize_attvars/2,deserialize_attvars/3,
          serialize_attvars/2,serialize_attvars_now/2,
          put_dyn_attrs/2,
          find_or_create_var/3,
          verbatum_var/1,
          holds_attrs/1,
          system_expanded_attvars/4,
          is_term_expanding_in_file/2,
          system_expanded_attvars/2]).


/** <module> Utility LOGICMOO ATTVAR SERIALIZER
This module works with Utility LOGICMOO ATTVAR READER to allows us to keep attributed variables and files inside the prolog database.

- @author Douglas R. Miles
- @license LGPL 
*/:- set_module(class(library)).


:- module_transparent((deserialize_attvars/2,deserialize_attvars/3,
          serialize_attvars_now/2,
          put_dyn_attrs/2,
          find_or_create_var/3,
          system_expanded_attvars/4,
          system_expanded_attvars/2)).


:- multifile(lmcache:use_attvar_expander/1).
:- dynamic(lmcache:use_attvar_expander/1).

:- multifile(lmcache:never_use_attvar_expander/1).
:- dynamic(lmcache:never_use_attvar_expander/1).

lmcache:never_use_attvar_expander(attvar_serializer).

% % % OFF :- system:use_module(library(gvar_lib)).% WAS OFF  :- system:reexport(library(hook_database)).% WAS OFF  :- system:reexport(library(logicmoo/util_varnames)).

:- if( \+ prolog_load_context(reload, true)).
:- multifile(baseKB:mpred_is_impl_file/1).
:- dynamic(baseKB:mpred_is_impl_file/1).
:- prolog_load_context(file,F),call(assert,baseKB:mpred_is_impl_file(F)).
:- endif.

% use this module to avoid some later conflicts
%:- if(exists_source(library(base32))).% WAS OFF  :- system:use_module(library(base32)).
%:- endif.

find_or_create_var(_Vs,N,V):-var(N),variable_name(N,_Name),N=V.
find_or_create_var(Vs,N,V):-var(N),!,find_or_create_named_var(Vs,N,V).
find_or_create_var(Vs,L,V):-is_list(L),!,member(VN=N,L),VN=vn,!,find_or_create_named_var(Vs,N,V).
find_or_create_var(Vs,'$VAR'(A),V):-
   (atom(A)->find_or_create_named_var(Vs,A,V);
   (atomic(A)-> (format(atom(N),'~w',[A]),find_or_create_named_var(Vs,N,V));
   find_or_create_named_var(Vs,A,V))).
find_or_create_var(Vs,N,V):-!,find_or_create_named_var(Vs,N,V).

find_named_var(member,Vs,N,V):-member(NN=VV,Vs),V==VV,!,must(NN=N).
find_named_var(member,Vs,N,V):-member(NN=VV,Vs),NN==N,!,must(VV=V).
find_named_var(variable_name,_Vs,N,V):- var(V),variable_name(V,Name),N==Name.
find_named_var(get_varname_list,_Vs,N,V):-get_varname_list(VsE),find_named_var(member,VsE,N,V).
% find_named_var(vnn(Name,How),Vs,N,V):- var(N),variable_name(N,Name),!,find_named_var(How,Vs,Name,V).

put_into_list(Vs,N,V):- Vs = [_|VT], nb_setarg(2,Vs,[N=V|VT]).

find_or_create_named_var(Vs,N,V):-  find_named_var(How,Vs,N,V),
   (How==member -> true; put_into_list(Vs,N,V)).
   % (member(localvs,Vs)->put_attr(V,vn,N);true),!.
find_or_create_named_var(Vs,N,V):- var(N),variable_name(N,Name), put_into_list(Vs,N,V), put_into_list(Vs,Name,V),!.
find_or_create_named_var(Vs,N,V):- variable_name(V,Name), put_into_list(Vs,N,V), put_into_list(Vs,Name,V),!.
find_or_create_named_var(Vs,N,V):- put_into_list(Vs,N,V),!,(member('$variable_names',Vs)->quietly(add_var_to_env(N,V);true)).


deserialize_attvars(V,O):- \+compound(V),!,O=V.
deserialize_attvars(V,O):- get_varname_list(Vs),!,loop_check(deserialize_attvars([localvs,'$variable_names'|Vs], V,O)),!.

deserialize_attvars(_Vs, V,O):- cyclic_term(V),!,O=V.
deserialize_attvars(Vs, V,O):-  var(V), get_var_name(V,N),!,find_or_create_var(Vs,N,V),!,V=O.
deserialize_attvars(_Vs, V,O):- \+ compound(V),!,O=V.
deserialize_attvars(_ ,(H:-BI),O):- fail, split_attrs(BI,AV,BO),AV\==true,AV\=bad:_,term_attvars((H:-BO),[]),must(call(AV)),!,(BO==true->(O=H);O=(H:-BO)).
deserialize_attvars(Vs,Dict,V):- is_dict(Dict,M),!,dict_pairs(Dict,M,Pairs),
   (atom(M)->find_or_create_var(Vs,V,M);V=M),
   put_dyn_attrs_1(V,Pairs).
deserialize_attvars(Vs,'$VAR'(N),V):- !, find_or_create_var(Vs,'$VAR'(N),V),put_dyn_attrs(V,N).
deserialize_attvars(Vs,'aVar'(N),V):- !, find_or_create_var(Vs,N,V),put_dyn_attrs(V,N).
deserialize_attvars(Vs,'aVar'(N,S),V):- !, find_or_create_var(Vs,N,V),put_dyn_attrs(V,N),put_dyn_attrs(V,S),!.
deserialize_attvars(Vs,C,A):- compound_name_arguments(C,F,Args),
  maplist(deserialize_attvars(Vs),Args,OArgs),compound_name_arguments(A,F,OArgs).


:- meta_predicate put_dyn_attrs(?,*).
put_dyn_attrs(V,S):- V==S,!.
put_dyn_attrs(V,S):- attvar(S),get_attrs(S,SS),!,put_dyn_attrs(V,SS),!.
put_dyn_attrs(_,S):- \+ compound(S),!.
put_dyn_attrs(V,S):- S=att(_,_,_), \+ attvar(V),!, put_attrs(V,S).
put_dyn_attrs(V,S):- put_dyn_attrs_1(V,S),!.
put_dyn_attrs(V,S):- trace_or_throw(bad_put_dyn_attrs(V,S)),!.

:- meta_predicate put_dyn_attrs_1(?,*).
put_dyn_attrs_1(_V,NC):- \+ compound(NC),!.
put_dyn_attrs_1(V,att(N,V,Rest)):- !, put_attr(V,N,V),put_dyn_attrs_1(V,Rest).
put_dyn_attrs_1(V,M=AV):- atom(M), ensure_attr_setup(M),!, must(put_attr(V,M,AV)).
put_dyn_attrs_1(V,M-AV):- ensure_attr_setup(M),!, must(put_attr(V,M,AV)).
put_dyn_attrs_1(V,M:AV):- atom(M),!, M:put_dyn_attrs_1(V,AV).
put_dyn_attrs_1(V,[H|T]):- !, put_dyn_attrs(V,H),put_dyn_attrs_1(V,T),!.
put_dyn_attrs_1(V,S):- is_dict(S),dict_keys(S,Keys),!,put_dyn_attrs_1(V,Keys).
put_dyn_attrs_1(_V,MAV):- must(MAV),!.

ensure_attr_setup(M):- atom(M),current_predicate(attribute_goals,M:attribute_goals(_,_,_)),!.
ensure_attr_setup(M):- atom(M),assert_if_new((M:attribute_goals(V,[put_attr(V,M,A)|R],R):- get_attr(V, M,A))).

is_term_expanding_in_file(I,_):- var(I),!,fail.
is_term_expanding_in_file(I,Type):- source_file(_,_),nb_current('$term',CT),(CT==I->Type=term;((CT=(_:-B),B==I,Type=goal))).

system_expanded_attvars(M:goal,P,I,O):- var(P),
   \+ is_term_expanding_in_file(I,_),
   \+ lmcache:never_use_attvar_expander(M),
   prolog_load_context(module,LC),
   \+ lmcache:never_use_attvar_expander(LC),
   current_prolog_flag(read_attvars,true), 
   \+ current_prolog_flag(xref,true), 
   system_expanded_attvars(I,O),
   dmsg(goal_xform(I --> O)).

system_expanded_attvars(M:term,P,I,CO):- nonvar(P), compound(I), I\= (:-(_)),
   \+ lmcache:never_use_attvar_expander(M),
   current_prolog_flag(read_attvars,true), 
   \+ current_prolog_flag(xref,true), 
   is_term_expanding_in_file(I,term),
   prolog_load_context(module,LC),
   \+ lmcache:never_use_attvar_expander(LC),
   (prolog_load_context(source,LC1)-> (\+ lmcache:never_use_attvar_expander(LC1)) ; true),
   system_expanded_attvars(I,O),
   clausify_attributes(O,CO),
   dmsg(term_xform(I --> CO)),
   % update what we just read 
   b_setval('$term',CO).

:- user:dynamic(expand_query/4).
:- user:multifile(expand_query/4).
   
free_of_attvars(Term):- term_attvars(Term,Vs),!,Vs==[].

free_of_attrs(Term):- var(Term),!,(get_attrs(Term,Attrs)-> Attrs==[] ; true).
free_of_attrs(Term):- term_attvars(Term,Vs),!,(Vs==[]->true;maplist(free_of_attrs,Vs)).


%% serialize_attvars( +AttvarTerm, -PrintableTerm) is semidet.
%
% serialize attributed variables (this is for printing and term_expansions currently)
%
serialize_attvars(I,O):- serialize_attvars_now(I,O),
  sanity(ignore(show_failure(free_of_attvars(O)))),
  sanity(show_failure(free_of_attrs(O))).

serialize_attvars_now(V,S):- var(V),must(serialize_1v(V,S)),!.
serialize_attvars_now(I,O):- \+ compound(I),!,O=I.
serialize_attvars_now(C,A):- compound_name_arguments(C,F,Args),maplist(serialize_attvars_now,Args,OArgs),compound_name_arguments(A,F,OArgs).


serialize_attvar_term(I,O):- copy_term(I,O), serialize_attvar_term_now(O),
   sanity(ignore(show_failure(free_of_attvars(O)))),
   sanity(show_failure(free_of_attrs(O))).

% ?- 
% rtrace(( X = hi(T),put_attr(T,sk,foo),serialize_attvar_term(X,SS), sanity(\+  has_skolem_attrvars(SS)))).


serialize_attvar_term_now(V):- attvar(V),trace_or_throw(serialize_attvar_term(V)).
serialize_attvar_term_now(I):- \+ compound(I),!.
serialize_attvar_term_now(C):- functor(C,_,A),serialize_attvar_term_now(A,C).

serialize_attvar_term_now(0,_):-!.
serialize_attvar_term_now(A,C):- arg(A,C,E),serialize_attvar_term_now(A,C,E),Am1 is A-1,serialize_attvar_term_now(Am1,C).

serialize_attvar_term_now(A,C,E):- attvar(E),!,get_put_attr_serial(E,New),setarg(A,C,New),!.
serialize_attvar_term_now(_,C,E):- compound(E)->serialize_attvar_term_now(C);true.

get_put_attr_serial(E,New):- get_attr(E,'$$sv$$',New),!.
get_put_attr_serial(E,Next):- serialize_3(E,Name,Atts),MyCopy =_, replace_subst4as(Atts,E,MyCopy,NewAtts),Next='aVar'(Name,att('$linkval',MyCopy,NewAtts)),del_attrs(E),put_attr(E,'$$sv$$',Next),!.

replace_subst4as( T1, S1, S2, T2 ) :-
    segment_subst4as( T1, Pre, S1, Post ),
    append_subst4as( S2, Post, S2_plus_Post ),
    append_subst4as( Pre, S2_plus_Post, T2 ).
segment_subst4as( T, Pre, S, Post ) :-
    segment_1_subst4as( S, T, Pre, Post ).
segment_1_subst4as( [], L, [], L ) :- !.
segment_1_subst4as( [H|T_], [H|T], [], Post ) :-
    segment_1_subst4as( T_, T, [], Post ),
    !.
segment_1_subst4as( S, [H|T], [H|U], Post ) :-
    segment_1_subst4as( S, T, U, Post ).
append_subst4as( [], L, L ).
append_subst4as( [H|T], L, [H|T1] ) :-
    append_subst4as( T, L, T1 ).


serialize_1v(V,'$VAR'(Name)):- get_attrs(V, att(vn, Name, [])),!.
serialize_1v(V,'aVar'('$VAR'(N),SO)):- get_attrs(V, S),variable_name_or_ref(V,N),put_attrs(TEMP,S),
   del_attr(TEMP,vn),!,remove_attrs(TEMP, SO),!.
serialize_1v(V,'$VAR'(N)):-  variable_name_or_ref(V,N).
serialize_1v(V,'aVar'(S)):- remove_attrs(V, S),!.
serialize_1v(V,V).


serialize_3(V,N,Atts):- (get_var_name(V,N);variable_name_or_ref(V,N)),del_attr(V,vn),(get_attrs(V, Atts)->true;Atts=[]),!.


remove_attrs(Var,Attrs):-get_attrs(Var,Attrs),!,remove_all_attrs(Var).
remove_attrs(Var,Attrs):-copy_term(Var,Copy,Attrs),Copy=Var,remove_all_attrs(Var).

% ?- rtrace(( X = hi(T),put_attr(T,sk,foo),serialize_attvar_term(X,SS), \+  has_skolem_attrvars(SS))).
% ?- X = hi(T),put_attr(T,sk,foo),rtrace(serialize_attvars(X,SS)), \+  has_skolem_attrvars(SS).


% ?- X = hi(T),put_attr(T,sk,foo),remove_all_attrs(T).

remove_all_attrs(Var):- attvar(Var),del_attrs(Var), sanity( \+ attvar(Var)),!.
remove_all_attrs(Term):- term_attvars(Term,Vars),maplist(remove_all_attrs,Vars).

%% verbatum_term(TermT) is semidet.
%
% System Goal Expansion Sd.
%
verbatum_term(I):- attvar(I),!,fail.
verbatum_term(I):- \+ compound(I),!. % this is intended to include the non-attrbuted variables
verbatum_term('$was_imported_kb_content$'(_,_)).
verbatum_term('varname_info'(_,_,_,_)).
verbatum_term(V):-verbatum_var(V).

holds_attrs(V):-var(V),!.
holds_attrs(V):-verbatum_var(V),!.


% move to logicmoo_utils_common.pl? 
verbatum_var(Var):-var(Var),!,fail.
verbatum_var('$VAR'(_)).
verbatum_var('aVar'(_)).
verbatum_var('aVar'(_,_)).





%% system_expanded_attvars( :TermT, :TermARG2) is semidet.
%
% System Goal Expansion Sd.
%
system_expanded_attvars(I,O):- (var(I);compound(I)),!,loop_check((deserialize_attvars(I,O))),O\=@=I,!.

:- fixup_exports.


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
end_of_file.

/*

% % :- '$set_source_module'( system).


:- public '$store_clause'/2.

'$store_clause'(A, C) :-
        '$clause_source'(A, B, D),
        '$store_clause'(B, _, C, D).

'$store_clause'((_, _), _, _, _) :- !,
        print_message(error, cannot_redefine_comma),
        fail.
'$store_clause'(A, _, B, C) :-
        '$valid_clause'(A), !,
        (   '$compilation_mode'(database)
        ->  '$record_clause'(A, B, C)
        ;   '$record_clause'(A, B, C, D),
            '$qlf_assert_clause'(D, development)
        ).

'$compile_term'(Clause, Layout, Id, SrcLoc) :-
	catch('$store_clause'(Clause, Layout, Id, SrcLoc), E,
	      '$print_message'(error, E)),
        catch((writeq(user_error,'$store_clause'(Clause,  Id)),nl(user_error)),_,true).

'$compile_aux_clauses'(Clauses, File) :-
	setup_call_cleanup(
	    '$start_aux'(File, Context),
	    '$store_aux_clauses'(Clauses, File),
	    '$end_aux'(File, Context)).

'$store_aux_clauses'(Clauses, File) :-
	is_list(Clauses), !,
	forall('$member'(C,Clauses),
	       '$compile_term'(C, _Layout, File)).
'$store_aux_clauses'(Clause, File) :-
	'$compile_term'(Clause, _Layout, File).
*/
/*
   compile_predicates(['$expand_goal'/2, '$expand_term'/4]),!.
:-   
   % '$set_predicate_attribute'('$expand_goal'(_,_), system, true),
   % '$set_predicate_attribute'('$expand_term'(_,_,_,_), system, true),
   '$set_predicate_attribute'('$expand_goal'(_,_), hide_childs, false),
   '$set_predicate_attribute'('$expand_term'(_,_,_,_), hide_childs, false).
   
*/


