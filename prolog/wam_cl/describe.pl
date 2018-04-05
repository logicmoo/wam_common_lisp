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
:- module(docs, []).



:- include('./header').

wl:init_args(1,apropos_list).
f_apropos_list(StringLCI,RestNKeys,Symbols):-
  f_sys_apropos_do_syms(StringLCI,RestNKeys,nop,Symbols).

wl:init_args(1,apropos).
f_apropos(StringLCI,RestNKeys,[]):-
  f_sys_apropos_do_syms(StringLCI,RestNKeys,f_sys_apropos_symbol,_).


f_sys_apropos_do_syms(StringLCI,RestNKeys,Fn,Symbols):-
  nth_param(RestNKeys,1,[],Packages),
  key_value(RestNKeys,kw_public,[],Public),
  %key_value(RestNKeys,kw_verbose,[],Verbose),
  nl,
  (Packages==[]-> (PkgExt = _,PkgInt = _) ; (find_package_or_die(Packages,PkgInt),
     freeze(PkgExt,(PkgExt=PkgInt; package_use_list(PkgInt,PkgExt))))),
  to_prolog_string_anyways(StringLCI,StringCI),
  string_upper(StringCI,MatchString),
  locally_let(sym('cl:*package*')=pkg_kw,
   findall(Symbol,
  ((Public==[]->(package_internal_symbols(PkgInt,String,Symbol),
    note_if_matches(MatchString,String,call(Fn,Symbol,RestNKeys,_)));
    true);package_external_symbols(PkgExt,String,Symbol),
     note_if_matches(MatchString,String,call(Fn,Symbol,RestNKeys,_))),
    Symbols)).
  

note_if_matches(MatchString,String,Call):- (atom_contains(String,MatchString)->Call;true).

f_sys_apropos_symbol(Symbol,_Verbose,t):- f_prin1(Symbol,_),
  ((is_fboundp(Symbol),f_symbol_function(Symbol,Function))->(write(' (fbound) '),f_prin1(Function,_));true),!,
  ((is_boundp(Symbol),f_symbol_value(Symbol,Value))->(write(' (bound) '),f_prin1(Value,_));true),!,
  nl.          

f_describe(Obj,Opts,Ret):-
 pl_describe(Obj,Opts,[],Ret).

pl_describe(Obj,Opts,_Skipping,Ret):-
   f_type_of(Obj,Type),
   f_class_of(Obj,Class),
   format('~N',[]),
   sformat(Repr,'~q',[Obj]),
   f_sys_get_iprops(Obj,Parts),
   f_print([[kw_object|Obj],[kw_type|Type],[kw_class|Class],[sys_prolog|Repr]|Parts],Ret).

maybe_get_docs(Type,Name,[Str|FunctionBody],FunctionBody,Code):- is_stringp(Str),to_prolog_string(Str,String),
  Code = assert_lsp(Name,doc:doc_string(Name,_Package,Type,String)).
maybe_get_docs(_Type,_Name,FunctionBody,FunctionBody, true).

:- fixup_exports.

