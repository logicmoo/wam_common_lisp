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
pl_compiled_filename(Obj,PL):-compound(Obj),arg(2,Obj,From),string(From),
   search_for(From,File),pl_compiled_filename(File,PL),!.
pl_compiled_filename(File,PL):- symbol_value(sys_xx_compile_file_type_xx,Ext),
   search_for(File,Found),atomic_list_concat([Found,Ext],'.',PL),exists_file(PL),!.



% Uses Symbol value: *DEFAULT-PATHNAME-DEFAULTS*
search_for(In,O):- cl_symbol_value(xx_default_pathname_defaults_xx,Str0),
   (Str0 == "" -> Str="." ; Str=Str0), with_fstem(Str,In,M),!,cl_string(M,O).
search_for(In,O):- with_fstem('.',In,M),!,cl_string(M,O).
search_for(In,O):- stream_property(_,file_name(FD)),with_fstem(FD,In,M),!,cl_string(M,O).

% Uses Symbol value: *SOURCE-FILE-TYPES*
check_file_types(SearchTypes):- 
   cl_symbol_value(custom_xx_source_file_types_xx,FileTypes),
   maplist(to_file_exts(),FileTypes,SearchTypes).
check_file_types(['.cl','.lisp','.lsp','.el']).

to_file_exts(Str,Atom):-txt2a(Str,At),atom_concat('.',At,Atom).
txt2a(T,A):- text_to_string(T,S),atom_string(A,S),!.

with_fstem(F0,File0,Found):-   
   check_file_types(SearchTypes),
   with_fstem(F0,File0,[''|SearchTypes],Found).

with_fstem(F0,File0,[''|SearchTypes],Found):- 
   txt2a(F0,F),txt2a(File0,File),
   absolute_file_name(File,Found,[relative_to(F),
     extensions([''|SearchTypes]),access(read),file_errors(fail)]),exists_file(Found).



:- fixup_exports.

      
end_of_file.