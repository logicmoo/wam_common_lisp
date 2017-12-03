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

:- include('header.pro').

cl_truename(In,Pathname):- search_for(In,O),make_pathanme(O,Pathname).

make_pathanme(Pathname,'$OBJ'(claz_pathname,Pathname)).
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
   search_for(From,File),pl_compiled_filename(File,PL),!.
pl_compiled_filename0(File,PL):- symbol_value(sys_xx_compile_file_type_xx,Ext),
   search_for(File,Found),atomic_list_concat([Found,Ext],'.',PL),exists_file(PL),!.


pathname_atom('$OBJ'(clz_pathname,S),Out):- !,pathname_atom0(S,Out).
pathname_atom(Obj,PL):- to_prolog_string(Obj,M),!,pathname_atom0(M,PL).
pathname_atom0('$OBJ'(_T,TXT),PL):- to_prolog_string(TXT,STR),!,txt2a(STR,PL).
pathname_atom0(TXT,PL):- txt2a(TXT,PL),!.


% Uses Symbol value: *DEFAULT-PATHNAME-DEFAULTS*
search_for(In,M):- cl_symbol_value(xx_default_pathname_defaults_xx,Str),pathname_atom(Str,Path),Path\=='',!, with_fstem(Str,In,M).
search_for(In,M):- with_fstem(".",In,M),!.
search_for(In,M):- stream_property(_,file_name(FD)),once((with_fstem(FD,In,M))).

% Uses Symbol value: *SOURCE-FILE-TYPES*
check_file_types(SearchTypes):- 
   cl_symbol_value(custom_xx_source_file_types_xx,FileTypes),
   maplist(to_file_exts,FileTypes,SearchTypes),!.
check_file_types(['.cl','.lisp','.lsp','.el']).

to_file_exts(Str,Atom):- to_prolog_string(Str,At),atom_concat_or_rtrace('.',At,Atom),!.
%txt2a(TXT,A):- to_prolog_string(TXT,T),!,always((text_to_string(T,S),atom_string(A,S))),!.
txt2a(TXT,A):- always((text_to_string(TXT,S),atom_string(A,S))),!.

with_fstem(Path,File,Found):-   
   notrace(check_file_types(SearchTypes)),
   found_strem(Path,File,[''|SearchTypes],Found),!.

found_strem(Path0,File0,SearchTypes,Found):- 
   txt2a(Path0,Path),txt2a(File0,File),    !,
   ((absolute_file_name(File,Found,[relative_to(Path),
     extensions(SearchTypes),access(read),file_errors(fail),expand(true),solutions(all)]),exists_file(Found))-> true;
   (fail,absolute_file_name(File,Found,[relative_to(Path),
     access(read),file_type(directory),file_errors(fail),expand(true),solutions(all)]),exists_directory(Found))).




:- fixup_exports.

      
end_of_file.