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
cl_disassemble(Function, Code):- string(Function),downcase_atom(Function,DC),!,cl_disassemble(DC, Code).
cl_disassemble(Function, Code):- 
  writeln('#| DISASSEMBLY FOR':Function),
  find_listings(Function),
  findall('$OBJ'(claz_prolog,(P:-B)),
   (current_predicate(W:Function/Arity),
  % listing(W:Function/Arity),
   functor(P,Function,Arity),clause(W:P,B)),
   Code),
  writeln('|#').

find_listings(F):- user:listing(F).

:- fixup_exports.

      
end_of_file.