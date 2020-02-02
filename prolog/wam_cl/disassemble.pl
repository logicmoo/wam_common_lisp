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

:- ensure_loaded(library(logicmoo/pretty_vars)).

:- meta_predicate(maplist_not_tail(1,*)).

%f_disassemble(Function, Code):- string(Function),downcase_atom(Function,DC),!,f_disassemble(DC, Code).
f_disassemble(function(Symbol),Options, Code):- !, f_disassemble(Symbol,Options, Code).
f_disassemble([quote,Symbol], Options,Code):- !, f_disassemble(Symbol, Options,Code).
f_disassemble(StringL,Options,Code):- \+ string(StringL),is_stringp(StringL),to_prolog_string_if_needed(StringL,String),!,f_disassemble(String,Options,Code).
f_disassemble(Function,_Options, Prolog):- 
  writeln('#| DISASSEMBLY FOR':Function),
   make_holder(Holder),
   print_related_clauses(Holder,_Module,Function),
  nb_holder_value(Holder,ListOut),
  Prolog = '$OBJ'(claz_prolog,ListOut),
  nop(ListOut==[]-> xlisting(Function) ; true),
  writeln('|#').


clauses_related(M,Obj,H,B,PrintKeyRef):- nonvar(Obj), get_opv(Obj,symbol_function,Obj2),clauses_related(M,Obj2,H,B,PrintKeyRef).
%clauses_related(M,Obj,H,B,PrintKeyRef):- nonvar(Obj), get_opv(Obj2,symbol_function,Obj),clauses_related(M,Obj2,H,B,PrintKeyRef).
clauses_related(_,P,H,B,PrintKeyRef):-
   H= wl:lambda_def(_DefType,H1,H2,_Args,_Body),
   clause_interface(H,B,PrintKeyRef),
  (related_functor(P,H1);related_functor(P,H2)).
clauses_related(_,P,H,B,PrintKeyRef):-
   H= wl:arglist_info(H1,H2,_,_),
   clause_interface(H,B,PrintKeyRef),
  (related_functor(P,H1);related_functor(P,H2)).
clauses_related(_,P,H,B,PrintKeyRef):-
   H= wl:init_args(_,H1),
   clause_interface(H,B,PrintKeyRef),
  (related_functor(P,H1)).
clauses_related(Module,P,Module:H,B,PrintKeyRef):- 
  current_module(Module),
  current_predicate(_,Module:H),
  \+ predicate_property(Module:H,imported_from(_)),
  \+ predicate_property(Module:H,foreign),  
  clause_interface(Module:H,B,PrintKeyRef),
  related_functor(P,H).


related_functor(P,Q):- to_related_functor(P,PP),to_related_functor(Q,QQ),QQ=PP,!.
%to_related_functor(P,_):- \+ callable(P),!,fail.
to_related_functor(P,PP):- string(P),atom_string(A,P),!,to_related_functor(A,PP).
to_related_functor(P,PP):- \+ compound(P),!,to_related_functor_each(P,PP).
to_related_functor(P,PP):- compound_name_arguments(P,F,[A,B,C|_]),!,
  (to_related_functor_each(F,PP);to_related_functor(A,PP);to_related_functor(B,PP);to_related_functor(C,PP)).
to_related_functor(P,PP):- compound_name_arguments(P,F,[]),!,(to_related_functor_each(F,PP)).
to_related_functor(P,PP):- compound_name_arguments(P,F,[A]),!,(to_related_functor_each(F,PP);to_related_functor(A,PP)).
to_related_functor(P,PP):- compound_name_arguments(P,F,[A,B]),!,(to_related_functor_each(F,PP);to_related_functor(A,PP);to_related_functor(B,PP)).



to_related_functor_each(P,_):- \+ atom(P),!,fail.
to_related_functor_each(P,PP):- to_related_functor_each1(P,PP).
to_related_functor_each(P,PP):- to_related_functor_each1(P,PPP),to_related_functor_each0(PPP,PP).

to_related_functor_each1(P,P).
to_related_functor_each1(P,PP):- downcase_atom(P,PP),PP\==P.

to_related_functor_each0(P,PP):-atom_concat('f_',PP,P).
to_related_functor_each0(P,PP):-atom_concat('u_',PP,P).
to_related_functor_each0(P,PP):-atom_concat('mf_',PP,P).

print_related_clauses(ExceptFor,_OModule,P):-
 ignore((
   no_repeats(clauses_related(_Module,P,H,B,PrintKeyRef)),
   PC = (H :- B),   
   nb_holder_value(ExceptFor,Printed),
   \+ member(PrintKeyRef,Printed),
   nb_holder_append(ExceptFor,PrintKeyRef),
   once(print_clause_plain(PC)),
   fail)).

print_clause_plain(I):-
  current_prolog_flag(color_term, Was),
  make_pretty(I,O),
    setup_call_cleanup(set_prolog_flag(color_term, false),
     (nl,lcolormsg1((O))),
     set_prolog_flag(color_term, Was)).


lcolormsg1(Msg):- mesg_color(Msg,Ctrl),!,ansicall_maybe(Ctrl,fmt9(Msg)).

% print_clause_plain(C):- portray_clause_w_vars(O).

make_pretty(I,O):- !,notrace((shrink_lisp_strings(I,O),make_pretty_vars(O))).
%make_pretty(I,O):- is_user_output,!,shrink_lisp_strings(I,O), pretty1(O),pretty2(O),pretty3(O).
%make_pretty(I,O):- I=O, pretty1(O),pretty2(O),pretty3(O).

:- fixup_exports.





end_of_file.