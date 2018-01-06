/*******************************************************************
 *
 * A Common Lisp compiler/interpretor, written in Prolog
 *
 * (lisp_compiler.pl)
 *
 *
 * Douglas'' Notes:
 *
 * (c) Douglas Miles, 2017
 *
 * The program is a *HUGE* common-lisp compiler/interpreter. It is written for YAP/SWI-Prolog .
 *
 * Changes since 2001:
 *
 *
 *******************************************************************/
:- module(gr0v, []).
:- set_module(class(library)).

% wl:body_compiled/1 symbols considered fboundp (compile_body does the work)
% Also means we *might* not always notice 'defmacros' of these
wl:body_compiled(multiple_value_bind).  wl:body_compiled(multiple_value_call).  wl:body_compiled(multiple_value_list).  
wl:body_compiled(values).  wl:body_compiled(values_list).  
wl:body_compiled(let).  wl:body_compiled(let_xx). 
wl:body_compiled(ext_letf).  wl:body_compiled(ext_letf_xx).  
wl:body_compiled(eval). wl:body_compiled(apply). 
wl:body_compiled(function). wl:body_compiled(lambda).
wl:body_compiled(macrolet). wl:body_compiled(flet). wl:body_compiled(labels). 
wl:body_compiled(or).  wl:body_compiled(and). wl:body_compiled(ext_xor).
wl:body_compiled(progn).  wl:body_compiled(prog).  
wl:body_compiled(prog1).  wl:body_compiled(prog2).  wl:body_compiled(progv). 
wl:body_compiled(tagbody). wl:body_compiled(go).
wl:body_compiled(block). wl:body_compiled(return). wl:body_compiled(return_from).
wl:body_compiled(if). wl:body_compiled(cond).  wl:body_compiled(unless). wl:body_compiled(when).  
wl:body_compiled(while).  wl:body_compiled(do). wl:body_compiled(dolist).  
wl:body_compiled(typecase). wl:body_compiled(ctypecase). wl:body_compiled(etypecase). 
wl:body_compiled(case). wl:body_compiled(ccase). wl:body_compiled(ecase). 
wl:body_compiled(quote).  

wl:body_compiled(setq). 




was_pkg_prefix(sys,pkg_sys).
was_pkg_prefix(ext,pkg_ext).
was_pkg_prefix(u,pkg_user).
was_pkg_prefix(clos,pkg_clos).


% grovel_system_symbols:-!.
grovel_system_symbols:- prolog_load_context(source,File),assertz(wl:interned_eval(call(grovel_system_symbols(File)))).

guess_symbol_name(HC,UPPER):- atomic_list_concat(HC,'_',HCN), get_opv(HCN,symbol_name,UPPER),!.
guess_symbol_name(HC,UPPER):- maplist(resolve_char_codes,HC,RHC),atomics_to_string(RHC,'-',STR),string_upper(STR,UPPER),!.

resolve_char_codes('','_').
%resolve_char_codes(C48,C):- notrace(catch((name(C48,[99|Codes]),number_codes(N,Codes),name(C,[N])),_,fail)),!,fail.
resolve_char_codes(C48,_):- notrace(catch((name(C48,[99|Codes]),number_codes(_,Codes)),_,fail)),!,fail.
resolve_char_codes(C,C).


grovel_system_symbols(File):- 
 ignore(((source_file(M:P,File),functor(P,F,A), A>0,  
  ((atomic_list_concat([MF,Pkg|HC],'_',F),memberchk(MF,['sf','f','mf']),was_pkg_prefix(Pkg,Package))-> true ;
    (atomic_list_concat([MF|HC],'_',F),memberchk(MF,['sf','f','mf']),Package=pkg_cl)),
    guess_symbol_name(HC,UPPER),
 always(((
  f_intern(UPPER,Package,Symbol),     
  f_export(Symbol,Package,_),
  (get_opv(Symbol,symbol_function,F)-> true ;show_call_trace(set_opv(Symbol,symbol_function,F))),
  wdmsg((grovelled_source_file_symbols(UPPER,Package,Symbol,M,F))))))),fail)).

list_lisp_undefined(Pkg):- 
 ignore(((get_opv(X,symbol_package,Pkg),once((Y=symbol_function,get_opv(X,Y,F),get_opv(X,symbol_name,Str),
   \+ current_predicate(F/_))),
  wdmsg(lisp_undefined(Pkg,X,Str,Y,F))),fail)).



grovel_preds(M):-
 %module_property(M,file(File)),
 
 doall((
  source_file(M:P,_File),
  %current_predicate(_,M:P), \+ predicate_property(M:P,imported_from(_)),
  %predicate_property(M:P,module(M)),
  functor(P,F,A),
  once(forall(clause(wl:grovel_pred(M,F,A),B),call(B))),
  fail)).

wl:grovel_pred(M,F,1):-
  atom(F),atom(M),
  atom_concat_or_rtrace('is_',R,F),atom_concat(_,'p',R),
  doall(((get_opv_iiii(_Sym,symbol_function,SF),
  (atom(SF),atom_concat(Prefix,R,SF),
   \+ atomic_list_concat([_,_,_|_],'_',Prefix),
   Head=..[SF,N,RetVal],
   PBody=..[F,N],
   (assert_lsp(user:Head :- t_or_nil(M:PBody,RetVal))))),fail)).


make_special_operator(Symbol):-
  atom_concat('sf_',Symbol,SF),
  set_opv(Symbol,symbol_function,SF),
  set_opv(SF,type_of,ext_special_operator).

:- assertz(wl:interned_eval(call(maplist(make_special_operator,[
         block,
         let_xx,
         return_from,
         catch,
         load_time_value,
         setq,
         eval_when,
         locally,
         symbol_macrolet,
         flet,
         macrolet,
         tagbody,
         function,
         multiple_value_call,
         the,
         go,
         multiple_value_prog1,
         throw,
         if,
         progn,
         unwind_protect,
         labels,
         progv,
         let,
         quote])))).


:- fixup_exports.

:- include('header').

end_of_file.