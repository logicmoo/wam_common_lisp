/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (xxxxx.pl)
 *
 *
 * Douglas'' Notes:
 *   some of this code will be moved to readers probably
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 *******************************************************************/
:- module(readtables, []).

:- meta_predicate rewrite_some(2,*,*).


:- include('./header').


f_sys_make_read_table(Out):-create_struct(read_table,Out).
  

% reader_intern_symbols(ExprS1,ExprS1):- current_prolog_flag(no_symbol_fix,true),!.
reader_intern_symbols(ExprS1,Expr):-
  reading_package(Package),!,
  reader_intern_symbols(Package,ExprS1,Expr),!.

reader_intern_symbols(_,Var,Var):- (var(Var);Var==[]),!.
reader_intern_symbols(Package,SymbolName,Symbol):- atom(SymbolName),!,atom_symbol(Package,SymbolName,Symbol).

reader_intern_symbols(_Package,Some,Some):- \+ compound(Some),!.
reader_intern_symbols(Package,[S|Some],[SR|SomeR]):-!, 
  reader_intern_symbols(Package,S,SR),
  reader_intern_symbols(Package,Some,SomeR),!.
reader_intern_symbols(_,I,I):- is_comment(I,_),!.
reader_intern_symbols(_Package,'$STRING'(Expr),LispO):- !, text_to_string_safe(Expr,Text),!,notrace(to_lisp_string(Text,LispO)).
reader_intern_symbols(_Package,'$NUMBER'(X,Y),'$NUMBER'(X,Y)):-!.
reader_intern_symbols(_Package,'$COMPLEX'(X,Y),'$COMPLEX'(X,Y)):-!.
reader_intern_symbols(_Package,'#\\'(X),'#\\'(X)):-!.

reader_intern_symbols(P,I,O):- resolve_reader_macros(I,M)->I\==M,!,reader_intern_symbols(P,M,O).

% #<unbound>
reader_intern_symbols(_,'$OBJ'([Unbound]),'$OBJ'(unbound,[])):- Unbound == unbound,!.
% #'symbol
reader_intern_symbols(_,'$OBJ'(Function,F),function(F)):- Function==function,!.
reader_intern_symbols(Package,'$OBJ'(Expr),'$OBJ'(ExprO)):-!,reader_intern_symbols(Package,(Expr),(ExprO)).

reader_intern_symbols(Package,ExprI,ExprO):- ExprI=..[F,C,D|Expr],F=='$ARRAY',!,  
  ((foc_class(D,K),atom(K));reader_intern_symbols(Package,C,K)),
  must_maplist(reader_intern_symbols(Package),Expr,TT),ExprO=..[F,C,K|TT].

reader_intern_symbols(Package,ExprI,ExprO):- ExprI=..[F,C|Expr],F=='$OBJ',!,
  ((foc_class(C,K),atom(K));reader_intern_symbols(Package,C,K)),
  must_maplist(reader_intern_symbols(Package),Expr,TT),ExprO=..[F,K|TT].
reader_intern_symbols(Package,ExprI,ExprO):- ExprI=..[F|Expr],atom_concat_or_rtrace('$',_,F),!,
  must_maplist(reader_intern_symbols(Package),Expr,TT),ExprO=..[F|TT].

reader_intern_symbols(Package,C1,C2):- 
  compound_name_arguments(C1,F,C1O),!,
  must_maplist(reader_intern_symbols(Package),C1O,C2O),C2=..[F|C2O].
reader_intern_symbols(_Package,Some,Some).


simple_atom_token(SymbolName):- atom_concat_or_rtrace('#',_,SymbolName),upcase_atom(SymbolName,SymbolName).
simple_atom_token(SymbolName):- atom_concat_or_rtrace('$',_,SymbolName),upcase_atom(SymbolName,SymbolName).
simple_atom_token(SymbolName):- string_upper(SymbolName,UP),string_lower(SymbolName,DOWN),!,UP==DOWN.

atom_symbol(_,SymbolName,Token):- simple_atom_token(SymbolName),!,SymbolName=Token.
atom_symbol(_,SymbolName,Obj):- notrace(f_type_of(SymbolName,X))->X\==t,SymbolName=Obj.
atom_symbol(Package,SymbolName,Symbol):-
  string_upper(SymbolName,SymbolNameU), 
  string_list_concat([SymbolName1|SymbolNameS],":",SymbolNameU),
  always(atom_symbol_s(SymbolName1,SymbolNameS,Package,Symbol)),!.


% :KEYWORD
atom_symbol_s("",[SymbolName],_UPackage,Symbol):- !,atom_symbol_s(SymbolName,[],pkg_kw,Symbol).
% #::SYMBOL
atom_symbol_s("#",["",SymbolName],UPackage,_Symbol):- throw('@TODO *** - READ from #<INPUT CONCATENATED-STREAM #<INPUT STRING-INPUT-STREAM> #<IO TERMINAL-STREAM>>: token ":BAR" after #: should contain no colon'(atom_symbol_s("#",["",SymbolName],UPackage))).  
% #:SYMBOL
atom_symbol_s("#",[SymbolName],_UPackage,Symbol):- f_make_symbol(SymbolName,Symbol).
% SYMBOL
atom_symbol_s(SymbolName,[],Package,Symbol):- !,intern_symbol(SymbolName,Package,Symbol,_).
% PACKAGE::SYMBOL
atom_symbol_s(PName,   ["", SymbolName],_UPackage,Symbol):- !, find_package_or_die(PName,Package),
  intern_symbol(SymbolName,Package,Symbol,_IntExt).
% PACKAGE:SYMBOL must exist AND also exported
atom_symbol_s(PName,   [SymbolName],_UPackage,Symbol):- find_package_or_die(PName,Package),atom_symbol_public(SymbolName,Package,Symbol).


lisp_auto_export_expected(pkg_sys).
lisp_auto_export_expected(_Package):- \+ current_prolog_flag(lisp_primordial,false),!.


% KEYWORD already exist or get created
atom_symbol_make_public(SymbolName,Package, Symbol):- Package == pkg_kw,!, 
  (package_find_symbol(SymbolName,Package,Symbol,_IntExt)->true;create_keyword(SymbolName,Symbol)).
% SYMBOL if exists will become exported
atom_symbol_make_public(SymbolName,Package, Symbol):- package_find_symbol(SymbolName,Package,Symbol,IntExt), 
   (IntExt\==kw_internal -> true ; f_export(Symbol,Package,_)).

% SYMBOL was found on used-by-list
atom_symbol_make_public(SymbolName,Package,Symbol):- 
   get_opv_i(Users,uses,Package),
   package_find_symbol(SymbolName,Users,Symbol,_IntExt),
   show_call_trace(f_import(Symbol,Package,_)),
   % should we move the home package?
   show_call_trace(f_export(Symbol,Package,_)),!.
% SYMBOL if not exists will become exported
atom_symbol_make_public(SymbolName,Package,Symbol):- true,
   intern_symbol(SymbolName,Package,Symbol,_),f_export(Symbol,Package,_).



% PACKAGE:SYMBOL will be made exported
atom_symbol_public(SymbolName,Package, Symbol):- lisp_auto_export_expected(Package),!,atom_symbol_make_public(SymbolName,Package,Symbol),!.
% KEYWORD must already exist
atom_symbol_public(SymbolName,Package, Symbol):- Package == pkg_kw,!, (package_find_symbol(SymbolName,Package,Symbol,_IntExt)->true;throw('symbol_not_exists'(SymbolName,Package))).
% SYMBOL must exists AND be exported
atom_symbol_public(SymbolName,Package, Symbol):- package_find_symbol(SymbolName,Package,Symbol,IntExt), 
   (IntExt\==kw_internal -> true ;throw('symbol_not_exported'(SymbolName,Package))).
atom_symbol_public(SymbolName,Package,_Symbol):- throw('symbol_not_exists'(SymbolName,Package)).


string_list_concat(StrS,Sep,String):- atomic_list_concat(L,Sep,String),atomics_to_strings(L,StrS).
atomics_to_strings([A|L],[S|StrS]):- atom(A),atom_string(A,S),!,atomics_to_strings(L,StrS).
atomics_to_strings([],[]).

atom_symbol_test(SymbolName,Symbol):- reading_package(Package),atom_symbol(Package,SymbolName,Symbol),!.


:- dynamic(is_rename_w/4).

make_renames:- 
 fix_symbols,
 tell('rename.data'),
 forall(member(Assert,[is_rename_w(_,_,_,_)]),
   forall(Assert,format('~q.~n',[Assert]))), told.

rewrite_packages:- 
  rewrite_some(rewrite_package,'pi.data','pi.data_r').
rewrite_package(P,PR):- P=..[F,A,B,C],PR=..[F,A,C,B].

rewrite_with_renames:-
  make_renames,
  rewrite_some(readname_terms,'pi.data','pi.data_r'),
  rewrite_some(readname_terms,'si.data','si.data_r'),
  rewrite_some(readname_terms,'ci.data','ci.data_r').

rewrite_some(Call,In,Out):-
 open(In,read,IS),
 open(Out,write,OS),
 repeat,
 read_term(IS,Term,[]),
 (Term==end_of_file->! ; 
 (once((call(Call,Term,TermO),
 format(OS,'~q.~n',[TermO])))),fail).


readname_terms(Term,Term):-var(Term),!.
readname_terms([H|Term],[HH|TermO]):- !,readname_terms(H,HH),readname_terms(Term,TermO).
readname_terms(Term,TermO):- compound(Term), compound_name_arguments(Term,F,TermL),
     must_maplist(readname_terms,TermL,TermLO),TermO=..[F|TermLO].
readname_terms(Term,TermO):- \+ atom(Term),!,Term=TermO.
readname_terms(Term,TermO):- is_rename_w(Term,_,_,TermO),!.
readname_terms(TermI,TermO):- atom_concat('f_',Term,TermI), is_rename_w(Term,_,_,TermOO),atom_concat('f_',TermOO,TermO),!.
readname_terms(TermI,TermO):- atom_concat('sf_',Term,TermI), is_rename_w(Term,_,_,TermOO),atom_concat('sf_',TermOO,TermO),!.
readname_terms(TermI,TermO):- atom_concat('mf_',Term,TermI), is_rename_w(Term,_,_,TermOO),atom_concat('mf_',TermOO,TermO),!.
readname_terms(TermI,TermO):- atom_concat('claz_',Term,TermI), is_rename_w(Term,_,_,TermOO),atom_concat('claz_',TermOO,TermO),!.
readname_terms(Term,Term).



fix_symbols:-
  forall(symbol_overlap(sys(pkg_cl,_S1),sys(pkg_clos,S2)),move_symbol_into(S2,sys(pkg_cl))),
  forall(symbol_overlap(sys(pkg_sys,S1),sys(pkg_sys,_S2)),move_symbol_into(S1,sys(pkg_sys))),
  forall(symbol_in(int(pkg_cl,S1)),move_symbol_into(S1,sys(pkg_sys))),
  forall(symbol_in(sys(pkg_sys,S1)),move_symbol_into(S1,sys(pkg_sys))).

fix_symbols2:-
  symbol_in(int(pkg_sys,S1)),move_symbol_into(S1,int(pkg_sys)).


symbol_in(sys(P1,S1)):- package_external_symbols(P1,_,S1).
symbol_in(int(P1,S1)):- package_internal_symbols(P1,_,S1).

move_symbol_into(S1,int(P2)):- f_symbol_package(S1,P1),pl_symbol_name(S1,N1),intern_symbol(N1,P2,S2,_),
  add_rename(S1,P1,int(P2),S2).
move_symbol_into(S1,sys(P2)):- f_symbol_package(S1,P1),pl_symbol_name(S1,N1),intern_symbol(N1,P2,S2,_),f_export(S2,P2,_),
  add_rename(S1,P1,sys(P2),S2).

%  forall(retract(soops:o_p_v(S1,X,Y)),(skipped_p(X)->true;assert(soops:o_p_v(S2,X,Y)))),
add_rename(S1,P1,INEXT,S2):-
   assert_if_new(is_rename_w(S1,P1,INEXT,S2)),
   wdmsg(add_rename(S1,P1,INEXT,S2)).

skipped_p(symbol_package).


symbol_overlap(sys(P1,S1),sys(P2,S2)):-
  dif(P1,pkg_kw),dif(P2,pkg_kw),package_external_symbols(P1,N1,S1),dif(P1,P2), package_external_symbols(P2,N1,S2).
symbol_overlap(int(P1,S1),int(P2,S2)):-
  dif(P1,pkg_kw),dif(P2,pkg_kw),package_internal_symbols(P1,N1,S1),dif(P1,P2), package_internal_symbols(P2,N1,S2).
symbol_overlap(sys(P1,S1),int(P2,S2)):-
  dif(P1,pkg_kw),dif(P2,pkg_kw),package_external_symbols(P1,N1,S1),dif(P1,P2), package_internal_symbols(P2,N1,S2).
symbol_overlap(int(P1,S1),sys(P2,S2)):-
  dif(P1,pkg_kw),dif(P2,pkg_kw),package_internal_symbols(P1,N1,S1),dif(P1,P2), package_external_symbols(P2,N1,S2).




resolve_reader_macros(I,O):- remove_comments(I,M),resolve_inlines(M,M2),remove_comments(M2,O).


str_to_expression(Str, Expression):- lisp_add_history(Str),as_sexp_interned(string(Str), Expression),!.
str_to_expression(Str, Expression):- with_input_from_string(Str,read_and_parse(Expression)),!.

remove_comments(IO,IO):- \+ compound(IO),!.
remove_comments([I|II],[O|OO]):-!,remove_comments(I,O),!,remove_comments(II,OO).
remove_comments([I|II],O):- is_comment(I,_),!,remove_comments(II,O).
remove_comments(IO,IO).


feature_member(Flag0,Features):- reader_intern_symbols(pkg_kw,Flag0,Flag),!,feature_member0(Flag,Features).
feature_member0(Flag,Features):- memberchk(Flag,Features).
feature_member0([kw_or|X],Features):- member(E,X), feature_member0(E,Features).
feature_member0([kw_and|X],Features):- \+ ( member(E,X), \+ feature_member0(E,Features)).


resolve_1inline([OP,_Flag,_Form], _Code):- \+ atomic(OP),!,fail.
resolve_1inline([_OP,Flag,_Form], _Code):- var(Flag),!,fail.
% #+
resolve_1inline([OP,Flag,Form], Code):- same_symbol(OP,'#+'),!,
   always(( get_var(xx_features_xx,FEATURES),
     (feature_member(Flag,FEATURES) -> Code = Form ; Code = '$COMMENT'(flag_removed(+Flag,Form))))).
   
% #-
resolve_1inline([OP,Flag,Form], Code):- same_symbol(OP,'#-'),!,
   always(( get_var(xx_features_xx,FEATURES),
     (\+ feature_member(Flag,FEATURES) -> Code = Form ; Code = '$COMMENT'(flag_removed(-Flag,Form))))).


resolve_inlines(IO,IO):- \+ compound(IO),!.
resolve_inlines([I|II],O):- is_comment(I,_),!,resolve_inlines(II,O).
resolve_inlines([A,B,C],OO):- resolve_1inline([A,B,C],O),!,resolve_inlines(O,OO).
resolve_inlines([L,A,B,C|II],OO):- resolve_1inline([A,B,C],O),!,resolve_inlines([L,O|II],OO).
resolve_inlines([[A,B,C]|II],[O|OO]):- resolve_1inline([A,B,C],O),!,resolve_inlines(II,OO).
resolve_inlines([A,B,C|II],OO):- resolve_1inline([A,B,C],O),!,resolve_inlines([O|II],OO).
resolve_inlines([I|II],[O|OO]):-resolve_inlines(I,O),!,resolve_inlines(II,OO).
resolve_inlines(IO,IO).


as_sexp(I,O):- as_sexp1(I,M),!,resolve_reader_macros(M,M2),!,remove_comments(M2,O),!.
as_sexp_interned(I,OO):- is_list(I),!,I=OO.
as_sexp_interned(I,OO):- as_sexp(I,O),!,reader_intern_symbols(O,OO),!.

as_sexp1(Var,Var):-var(Var).
as_sexp1(NIL,NIL):-NIL==[],!.
as_sexp1(Stream,Expression):- is_stream(Stream),!,always(parse_sexpr_untyped(Stream,SExpression)),!,
  as_sexp2(SExpression,Expression).
as_sexp1(s(Str),Expression):- !, always(parse_sexpr_untyped(string(Str),SExpression)),!,as_sexp2(SExpression,Expression).
as_sexp1((Str),Expression):- string(Str),!, always(parse_sexpr_untyped(string(Str),SExpression)),!,as_sexp2(SExpression,Expression).
as_sexp1((Str),Expression):- atom(Str),name(Str,List), member(E,List),\+ char_type(E,alpha),
   parse_sexpr_untyped(string(Str),SExpression),as_sexp2(SExpression,Expression),!.
as_sexp1(Str,Expression):- fail, \+ ((is_list(Str),nth0(0,Str,E),atom(E),name(E,[_]))),
    notrace(catch(text_to_string(Str,String),_,fail)),!, 
    always(parse_sexpr_untyped(string(String),SExpression)),!,
    as_sexp2(SExpression,Expression).
as_sexp1(Str,Expression):- as_sexp2(Str,Expression),!.

as_sexp2(Str,Expression):- is_list(Str),!,maplist(expand_pterm_to_sterm,Str,Expression).
as_sexp2(Str,Expression):- expand_pterm_to_sterm(Str,Expression),!.
                                                  


expand_pterm_to_sterm(VAR,VAR):- notrace(is_ftVar(VAR)),!.
expand_pterm_to_sterm('NIL',[]):-!.
expand_pterm_to_sterm(nil,[]):-!.
expand_pterm_to_sterm(VAR,VAR):- \+ compound(VAR),!.
expand_pterm_to_sterm([X|L],[Y|Ls]):-!,expand_pterm_to_sterm(X,Y),expand_pterm_to_sterm(L,Ls),!.
expand_pterm_to_sterm(ExprI,ExprI):- compound_name_arguments(ExprI,F,_),keep_as_pl_verbatum(F),!.
expand_pterm_to_sterm(ExprI,[ExprI]):- compound_name_arity(ExprI,_,0),!.
expand_pterm_to_sterm(ExprI,ExprO):- compound_name_arguments(ExprI,F,Expr),keep_as_pl_term(F),!,expand_pterm_to_sterm(Expr,TT),ExprO=..[F|TT].
expand_pterm_to_sterm(X,[F|Y]):- compound_name_arguments(X,F,L),expand_pterm_to_sterm(L,Y),!.

keep_as_pl_verbatum(closure).

keep_as_pl_term(function).
keep_as_pl_term(closure).
keep_as_pl_term(prolog).
keep_as_pl_term(ugly).
keep_as_pl_term('$OBJ').
keep_as_pl_term('#\\').
keep_as_pl_term(v).
keep_as_pl_term(obj).
keep_as_pl_term(D):-atom_concat_or_rtrace('$',_,D).



:- fixup_exports.

end_of_file.



