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
cl_disassemble(Function, Code):- 
  writeln('#| DISASSEMBLY FOR':Function),
  user:listing(Function),
  findall('$OBJ'(claz_prolog,(P:-B)),
   (current_predicate(W:Function/Arity),
  % listing(W:Function/Arity),
   functor(P,Function,Arity),clause(W:P,B)),
   Code),
  writeln('|#').


:- fixup_exports.

      
end_of_file.