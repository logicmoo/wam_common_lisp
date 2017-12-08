/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. 
 *
 *******************************************************************/
:- module(dasm, []).


cl_disassemble(function(Symbol), Code):- !, cl_disassemble((Symbol), Code).
cl_disassemble(Obj, Code):- get_opv(Obj,function,Obj2),!,cl_disassemble(Obj2, Code).
cl_disassemble(StringL,Code):- to_prolog_string_if_needed(StringL,String),!,cl_disassemble(String,Code).
cl_disassemble(Function, Code):- string(Function),downcase_atom(Function,DC),!,cl_disassemble(DC, Code).
cl_disassemble(Function, t):- 
  % listing(Function),
  writeln('#| DISASSEMBLY FOR':Function),
 
  findall('$OBJ'(claz_prolog,(P:-B)),
   (current_predicate(W:Function/Arity),   
  %listing(W:Function/Arity),
   functor(P,Function,Arity),
   \+ predicate_property(W:P,imported_from(_)),
   clause(W:P,B),

   always((make_pretty(Function,Arity,(W:P :- B),O),
   nl,(portray_clause_w_vars(O))))),
   _Code),
  writeln('|#').

make_pretty(Function,Arity,I,O):- shrink_lisp_strings(I,M), pretty_varnames(Function,Arity,M,O).

may_debug_var(_,_,V):- nonvar(V),!.
may_debug_var(_,_,V):- variable_name(V,_),!.
may_debug_var(L,R,V):- atom_concat('cl_',LL,L),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- atomic_list_concat([_A1,A2,A3|AS],'_',L),atomic_list_concat([A2,A3|AS],'_',LL),may_debug_var(LL,R,V).
may_debug_var(L,R,V):- debug_var([L,R],V).

may_debug_var(_,V):- nonvar(V),!.
may_debug_var(_,V):- variable_name(V,_),!.
may_debug_var(R,V):- debug_var(R,V).

pretty_varnames(_,_,H,H):- \+ compound(H),!. % may_debug_var(F,'_Call',H).
pretty_varnames(F,A,(W:H), (W:HH)):- !, pretty_varnames(F,A,H,HH).
pretty_varnames(F,A,(H :- B),(HH:-BB)):- pretty_varnames(F,A,H,HH),pretty_varnames(F,A,B,BB).
pretty_varnames(F,A,(H , B),(HH , BB)):- pretty_varnames(F,A,H,HH),pretty_varnames(F,A,B,BB).
pretty_varnames(F,A,(H ; B),(HH ; BB)):- pretty_varnames(F,A,H,HH),pretty_varnames(F,A,B,BB).
pretty_varnames(F,A,(H -> B),(HH -> BB)):- pretty_varnames(F,A,H,HH),pretty_varnames(F,A,B,BB).
pretty_varnames(_,_,[H | B],[H | B]):- may_debug_var('CAR',H),may_debug_var('CDR',B).
pretty_varnames(_,_,H,H):- 
 always((functor(H,F,A),
   H=..[F,P1|ARGS],
   may_debug_var(F,'_Param',P1),
   arg(A,H,R),may_debug_var(F,'_Ret',R),   
   must_maplist_det(pretty_varnames(F,A),ARGS,_ARGSO))),!. % ,HH=..[F,P1|ARGSO].
pretty_varnames(_,_,G,G).


:- fixup_exports.

      
end_of_file.