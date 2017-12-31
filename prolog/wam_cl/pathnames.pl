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
:- module(pathname, []).

:- set_module(class(library)).

:- include('header').

wl:init_args(0,make_pathname).
cl_make_pathname(I,O):- cl_make_instance([claz_pathname|I],O).

wl:init_args(1,compile_file_pathname).
cl_compile_file_pathname(OSFile,Keys,PLFileOut):-
   F = kw_output_file,
    current_env(Env),
   get_kw(Env,Keys,F,sys_output_file, 
     PLFile, pl_compiled_filename(OSFile,PLFile), _Present),
   to_lisp_pathname(PLFile,PLFileOut).


/*


i can understand it might be important ideology to follow this when discussing an implementation of common lisp in common lisp
"you cannot discuss or implement prototype based inheritance since common lisp does not contain terminology your implementation uses"
but such constraints would only limit the implementation of common lisp in other languages
also sticking to only CL terminology would eccourage pedantic people instead of polymaths who program in at least 5 languages a week


 I am not going to talk down to anyone (not you)..  i am going to treat you like you know just as much as i do about things..
if you do not want to become a contributor, i cannot force you to 
it would also limit the implementation of other languages in common lisp 
after 35 years of coding interpretors and compilers for several languages, I am just too stuck in my ways, 
if you think i am incapable of being helped. please put me on ignore 
I suppose people will stop using UABCL due to my literary skills

*COMPILE-FILE-TRUENAME*
*COMPILE-FILE-PATHNAME*
*COMPILE-FILE-ZIP*
*/
% *COMPILE-FILE-CLASS-EXTENSION*  sys_xx_compile_file_class_extension_xx
% *COMPILE-FILE-TYPE*    sys_xx_compile_file_type_xx


% (LOAD "sanity-test")
pl_compiled_filename(OSFile,PLFile):-
   cl_truename(OSFile,TrueOSFile),
   to_prolog_pathname(TrueOSFile,OSFileAtom),
   cl_symbol_value(sys_xx_compile_file_type_xx,StrExt),to_prolog_string(StrExt,Ext),
   %file_name_extension(BaseName,_Was,OSFileAtom),file_name_extension(BaseName,Ext,PLFile).
   file_name_extension(OSFileAtom,Ext,PLFile).
pl_compiled_filename(Obj,PL):- to_prolog_string(Obj,M),pl_compiled_filename0(M,PL).

pl_compiled_filename0(Obj,PL):- compound(Obj),arg(2,Obj,From),string(From),
   pl_probe_file(From,File),pl_compiled_filename(File,PL),!.
pl_compiled_filename0(File,PL):- get_var(sys_xx_compile_file_type_xx,Ext),
   pl_probe_file(File,Found),atomic_list_concat([Found,Ext],'.',PL),exists_file(PL),!.


to_prolog_pathname(Cmp,Out):- compound(Cmp),Cmp='$OBJ'(claz_pathname,S),!,always(to_prolog_pathname(S,Out)).
to_prolog_pathname(Ref,Value):- atom(Ref),!,((is_symbolp(Ref),is_boundp(Ref),symbol_value(Ref,PValue))->to_prolog_pathname(PValue,Value);Ref=Value).
to_prolog_pathname(Ref,O):- is_pathnamep(Ref),get_opv(Ref,name,V),!,always(show_call_trace(to_prolog_pathname(V,O))).
to_prolog_pathname(Str,O):- is_stringp(Str),!,string_to_prolog_atom(Str,O).
to_prolog_pathname(Obj,PL):- string_to_prolog_atom(Obj,PL).
%to_prolog_pathname0('$OBJ'(_T,TXT),PL):- to_prolog_string(TXT,STR),!,string_to_prolog_atom(STR,PL).
%to_prolog_pathname0(TXT,PL):- string_to_prolog_atom(TXT,PL),!.

is_pathnamep(P):- i_class(P,C),!,C==claz_pathname.

to_lisp_pathname('$OBJ'(claz_pathname,PathString),'$OBJ'(claz_pathname,PathString)):-!.
to_lisp_pathname(Pathname,'$OBJ'(claz_pathname,PathString)):- 
  (is_stringp(Pathname)->PathStr=Pathname;text_to_string(Pathname,PathStr)),
  to_lisp_string(PathStr,PathString).

% Uses Symbol value: *DEFAULT-PATHNAME-DEFAULTS*
cl_truename(In,Pathname):- or_die(cl_probe_file(In,Pathname)).
cl_probe_file(In,Pathname):- 
   or_nil((pl_probe_file(In,O)->to_lisp_pathname(O,Pathname))).

nil_lastvar((_,G)):-!, nil_lastvar(G).
nil_lastvar(G):- functor(G,_,A),arg(A,G,[]).

or_die(G):- G ->true;throw(die(G)).
or_nil(G):- G ->true;nil_lastvar(G).



path_probe(Dir):- cl_symbol_value(xx_default_pathname_defaults_xx,Str),to_prolog_pathname(Str,Path),(Path\==''-> Dir=Path;Dir='.').
path_probe(Path):-cl_symbol_value(ext_xx_file_search_xx,Str),(member(E,Str)*->to_prolog_pathname(E,Path);Path='.').
path_probe(Str):- cl_symbol_value(ext_xx_lisp_home_xx,Str),to_prolog_pathname(Str,Path),Path\==''.
path_probe(FD):- stream_property(_,file_name(FD)).

pl_probe_file(In,M):- path_probe(FD),once((with_fstem(FD,In,M))),!.

set_default_path_early:-
   working_directory(X,X),
   assertz(wl:interned_eval(call((to_lisp_pathname(X,Path),
     set_opv(sym('cl:*default-pathname-defaults*'),value,Path))))).

:- prolog_load_context(directory,X),
   assertz(wl:interned_eval(call((to_lisp_pathname(X,Path),
     set_opv(sym('ext:*LISP-HOME*'),value,Path))))).

wl:interned_eval(call((to_lisp_pathname("",Path),
     set_opv(sym('cl:*default-pathname-defaults*'),value,Path)))).


% Uses Symbol value: *SOURCE-FILE-TYPES*
check_file_types(SearchTypes):- 
   cl_symbol_value(custom_xx_source_file_types_xx,FileTypes),
   maplist(to_file_exts,FileTypes,SearchTypes),!.
check_file_types(['.cl','.lisp','.lsp','.el']).

to_file_exts(Str,Atom):- to_prolog_string_anyways(Str,At),atom_concat_or_rtrace('.',At,Atom),!.

string_to_prolog_atom(TXT,A):- to_prolog_string_anyways(TXT,T),!,always((text_to_string(T,S),atom_string(A,S))),!.
%string_to_prolog_atom(TXT,A):- always((text_to_string(TXT,S),atom_string(A,S))),!.

with_fstem(Path,File,Found):-   
   check_file_types(SearchTypes),!,
   found_strem(Path,File,[''|SearchTypes],Found),!.

found_strem(Path0,File0,SearchTypes,Found):- 
   to_prolog_pathname(Path0,Path),!, to_prolog_pathname(File0,File),    !,
   ((absolute_file_name(File,Found,[relative_to(Path),
     extensions(SearchTypes),access(read),file_errors(fail),expand(true),solutions(all)]),exists_file(Found))-> true;
   (fail,absolute_file_name(File,Found,[relative_to(Path),
     access(read),file_type(directory),file_errors(fail),expand(true),solutions(all)]),exists_directory(Found))).




:- fixup_exports.

end_of_file.