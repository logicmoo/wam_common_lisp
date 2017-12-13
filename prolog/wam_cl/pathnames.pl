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

cl_truename(In,Pathname):- pl_truename(In,O),to_lisp_pathname(O,Pathname).

cl_make_pathname(I,O):- to_lisp_pathname(I,O).

cl_compile_file_pathname(OSFile,Keys,PLFileOut):-
   F = kw_output_file,
   kw_obtain_value_else_p(Keys,F,sys_output_file, 
     PLFile, guess_compile_file_pathname(OSFile,PLFile), _Present),
   to_lisp_pathname(PLFile,PLFileOut).

guess_compile_file_pathname(OSFile,PLFile):- file_base_name(OSFile,BaseName),atom_concat_or_rtrace(BaseName,'.pro',PLFile).

/*
*COMPILE-FILE-TRUENAME*
*COMPILE-FILE-PATHNAME*
*COMPILE-FILE-ZIP*
*/
% *COMPILE-FILE-CLASS-EXTENSION*  sys_xx_compile_file_class_extension_xx
% *COMPILE-FILE-TYPE*    sys_xx_compile_file_type_xx

% (LOAD "sanity-test")
pl_compiled_filename(Obj,PL):- to_prolog_string(Obj,M),pl_compiled_filename0(M,PL).
pl_compiled_filename0(Obj,PL):- compound(Obj),arg(2,Obj,From),string(From),
   pl_truename(From,File),pl_compiled_filename(File,PL),!.
pl_compiled_filename0(File,PL):- get_var(sys_xx_compile_file_type_xx,Ext),
   pl_truename(File,Found),atomic_list_concat([Found,Ext],'.',PL),exists_file(PL),!.


to_prolog_pathname(Cmp,Out):- compound(Cmp),Cmp='$OBJ'(claz_pathname,S),!,always(to_prolog_pathname(S,Out)).
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
pl_truename(In,M):- cl_symbol_value(xx_default_pathname_defaults_xx,Str),always((to_prolog_pathname(Str,Path))),Path\=='',!, with_fstem(Str,In,M).
pl_truename(In,M):- with_fstem(".",In,M),!.
pl_truename(In,M):- stream_property(_,file_name(FD)),once((with_fstem(FD,In,M))).

% Uses Symbol value: *SOURCE-FILE-TYPES*
check_file_types(SearchTypes):- 
   cl_symbol_value(custom_xx_source_file_types_xx,FileTypes),
   maplist(to_file_exts,FileTypes,SearchTypes),!.
check_file_types(['.cl','.lisp','.lsp','.el']).

to_file_exts(Str,Atom):- to_prolog_string_anyways(Str,At),atom_concat_or_rtrace('.',At,Atom),!.

string_to_prolog_atom(TXT,A):- to_prolog_string_anyways(TXT,T),!,always((text_to_string(T,S),atom_string(A,S))),!.
%string_to_prolog_atom(TXT,A):- always((text_to_string(TXT,S),atom_string(A,S))),!.

with_fstem(Path,File,Found):-   
   notrace(check_file_types(SearchTypes)),!,
   notrace(found_strem(Path,File,[''|SearchTypes],Found)),!.

found_strem(Path0,File0,SearchTypes,Found):- 
   to_prolog_pathname(Path0,Path),!, to_prolog_pathname(File0,File),    !,
   ((absolute_file_name(File,Found,[relative_to(Path),
     extensions(SearchTypes),access(read),file_errors(fail),expand(true),solutions(all)]),exists_file(Found))-> true;
   (fail,absolute_file_name(File,Found,[relative_to(Path),
     access(read),file_type(directory),file_errors(fail),expand(true),solutions(all)]),exists_directory(Found))).




:- fixup_exports.

end_of_file.