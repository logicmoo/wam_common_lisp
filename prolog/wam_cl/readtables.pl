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
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(readtables, []).

:- set_module(class(library)).

:- include('header.pro').


f_u_make_read_table(Out):-create_struct(read_table,Out).
  

% reader_intern_symbols(ExprS1,ExprS1):- current_prolog_flag(no_symbol_fix,true),!.
reader_intern_symbols(ExprS1,Expr):-
  reading_package(Package),!,
  reader_intern_symbols(Package,ExprS1,Expr),!.

reader_intern_symbols(_,Var,Var):- (var(Var);Var==[]),!.
reader_intern_symbols(Package,SymbolName,Symbol):-
   atom(SymbolName),atom_symbol(SymbolName,Package,Symbol),!.
reader_intern_symbols(_Package,Some,Some):- \+ compound(Some),!.

% #<unbound>
reader_intern_symbols(_,'$OBJ'([unbound]),'$OBJ'(unbound,[])):-!.
reader_intern_symbols(Package,'$OBJ'(Expr),'$OBJ'(ExprO)):-!,reader_intern_symbols(Package,(Expr),(ExprO)).

reader_intern_symbols(Package,ExprI,ExprO):- ExprI=..[F,C|Expr],F=='$OBJ',  
  ((find_or_create_class(C,K),atom(K));reader_intern_symbols(Package,C,K)),
  must_maplist(reader_intern_symbols(Package),Expr,TT),ExprO=..[F,K|TT].
reader_intern_symbols(Package,ExprI,ExprO):- ExprI=..[F|Expr],atom_concat('$',_,F),!,
  must_maplist(reader_intern_symbols(Package),Expr,TT),ExprO=..[F|TT].

reader_intern_symbols(Package,[S|Some],[SR|SomeR]):- 
  reader_intern_symbols(Package,S,SR),
  reader_intern_symbols(Package,Some,SomeR).

reader_intern_symbols(Package,C1,C2):- 
  compound_name_arguments(C1,F,C1O),
  must_maplist(reader_intern_symbols(Package),C1O,C2O),C2=..[F|C2O].
reader_intern_symbols(_Package,Some,Some).


simple_atom_token(SymbolName):- atom_concat_or_rtrace('$',_,SymbolName),upcase_atom(SymbolName,SymbolName).
simple_atom_token(SymbolName):- string_upper(SymbolName,UP),string_lower(SymbolName,DOWN),!,UP==DOWN.

atom_symbol(SymbolName,_,Token):- simple_atom_token(SymbolName),!,SymbolName=Token.
atom_symbol(SymbolName,_,Obj):- cl_type_of(SymbolName,X)->X\==t,SymbolName=Obj.
atom_symbol(SymbolName,Package,Symbol):-
  string_upper(SymbolName,SymbolNameU), 
  string_list_concat([SymbolName1|SymbolNameS],":",SymbolNameU),
  must_or_rtrace(atom_symbol_s(SymbolName1,SymbolNameS,Package,Symbol)),!.


% :KEYWORD
atom_symbol_s("",[SymbolName],_UPackage,Symbol):- !,atom_symbol_s(SymbolName,[],pkg_kw,Symbol).
% #::SYMBOL
atom_symbol_s("#",["",SymbolName],UPackage,_Symbol):- throw('@TODO *** - READ from #<INPUT CONCATENATED-STREAM #<INPUT STRING-INPUT-STREAM> #<IO TERMINAL-STREAM>>: token ":BAR" after #: should contain no colon'(atom_symbol_s("#",["",SymbolName],UPackage))).  
% #:SYMBOL
atom_symbol_s("#",[SymbolName],_UPackage,Symbol):- cl_make_symbol(SymbolName,Symbol).
% SYMBOL
atom_symbol_s(SymbolName,[],Package,Symbol):- intern_symbol(SymbolName,Package,Symbol,_).
% PACKAGE::SYMBOL
atom_symbol_s(PName,   ["", SymbolName],_UPackage,Symbol):- find_package_or_die(PName,Package),intern_symbol(SymbolName,Package,Symbol,_IntExt).
% PACKAGE:SYMBOL
atom_symbol_s(PName,   [SymbolName],_UPackage,Symbol):- find_package_or_die(PName,Package),atom_symbol_public(SymbolName,Package,Symbol).

% KEYWORD must already exist
atom_symbol_public(SymbolName,Package, Symbol):- Package == pkg_kw,!, (package_find_symbol(SymbolName,Package,Symbol,_IntExt)->true;throw('symbol_not_exists'(SymbolName,Package))).
% SYMBOL must exists AND be public
atom_symbol_public(SymbolName,Package, Symbol):- package_find_symbol(SymbolName,Package,Symbol,IntExt), 
   (IntExt\==kw_internal -> true ;throw('symbol_not_exported'(SymbolName,Package))).
atom_symbol_public(SymbolName,Package,_Symbol):- throw('symbol_not_exists'(SymbolName,Package)).


string_list_concat(StrS,Sep,String):- atomic_list_concat(L,Sep,String),atomics_to_strings(L,StrS).
atomics_to_strings([A|L],[S|StrS]):-atom_string(A,S),!,atomics_to_strings(L,StrS).
atomics_to_strings([],[]).

atom_symbol_test(SymbolName,Symbol):- reading_package(Package),atom_symbol(SymbolName,Package,Symbol),!.


:- fixup_exports.

end_of_file.

