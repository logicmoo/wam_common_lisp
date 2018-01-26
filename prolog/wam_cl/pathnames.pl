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



:- include('header').

wl:init_args(0,make_pathname).
f_make_pathname(I,O):- f_make_instance([claz_pathname|I],O).

wl:init_args(1,compile_file_pathname).
f_compile_file_pathname(OSFile,Keys,PLFileOut):-
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
   f_truename(OSFile,TrueOSFile),
   to_prolog_pathname(TrueOSFile,OSFileAtom),
   f_symbol_value(sys_xx_compile_file_type_xx,StrExt),to_prolog_string(StrExt,Ext),
   %file_name_extension(BaseName,_Was,OSFileAtom),file_name_extension(BaseName,Ext,PLFile).
   file_name_extension(OSFileAtom,Ext,PLFile).
pl_compiled_filename(Obj,PL):- to_prolog_string(Obj,M),pl_compiled_filename0(M,PL).

pl_compiled_filename0(Obj,PL):- compound(Obj),arg(2,Obj,From),string(From),
   pl_probe_file(From,File),pl_compiled_filename(File,PL),!.
pl_compiled_filename0(File,PL):- get_var(sys_xx_compile_file_type_xx,Ext),
   pl_probe_file(File,Found),atomic_list_concat([Found,Ext],'.',PL),exists_file(PL),!.

to_prolog_pathname(Cmp,Out):- compound(Cmp),Cmp='$OBJ'(claz_pathname,S),!,always(to_prolog_pathname(S,Out)).
to_prolog_pathname(Ref,Value):- atom(Ref),!,((is_symbolp(Ref),is_boundp(Ref),f_symbol_value(Ref,PValue))->to_prolog_pathname(PValue,Value);Ref=Value).
to_prolog_pathname(Str,O):- is_stringp(Str),!,string_to_prolog_atom(Str,O).
to_prolog_pathname(Obj,PL):- string_to_prolog_atom(Obj,PL).
to_prolog_pathname(Ref,O):- is_pathnamep(Ref),get_opv(Ref,name,V),!,always(show_call_trace(to_prolog_pathname(V,O))).
%to_prolog_pathname0('$OBJ'(_T,TXT),PL):- to_prolog_string(TXT,STR),!,string_to_prolog_atom(STR,PL).
%to_prolog_pathname0(TXT,PL):- string_to_prolog_atom(TXT,PL),!.

is_pathnamep(P):- i_class(P,C),!,C==claz_pathname.

to_lisp_pathname(P,P):- is_pathnamep(P),!.
to_lisp_pathname('$OBJ'(claz_pathname,PathString),'$OBJ'(claz_pathname,PathString)):-!.
to_lisp_pathname(Pathname,'$OBJ'(claz_pathname,PathString)):- 
  (is_stringp(Pathname)->PathStr=Pathname;text_to_string(Pathname,PathStr)),
  to_lisp_string(PathStr,PathString).

% Uses Symbol value: *DEFAULT-PATHNAME-DEFAULTS*
f_truename(In,Pathname):- or_die(f_probe_file(In,Pathname)).
f_probe_file(In,Pathname):- 
   or_nil((pl_probe_file(In,O)->to_lisp_pathname(O,Pathname))).

nil_lastvar((_,G)):-!, nil_lastvar(G).
nil_lastvar(G):- functor(G,_,A),arg(A,G,[]).

or_die(G):- G ->true;throw(die(G)).
or_nil(G):- G ->true;nil_lastvar(G).

% "(make-pathname :host "abacus" :device "C" :directory '(:absolute "My Documents" "werther-project") :name "wlskdjaeks" :type "doc")"



path_probe(Dir):- f_symbol_value(xx_default_pathname_defaults_xx,Str),to_prolog_pathname(Str,Path),(Path\==''-> Dir=Path;Dir='.').
path_probe(Path):-f_symbol_value(sys_xx_file_search_xx,Str),(member(E,Str)*->to_prolog_pathname(E,Path);Path='.').
path_probe(Str):- f_symbol_value(sys_xx_lisp_home_xx,Str),to_prolog_pathname(Str,Path),Path\==''.
path_probe(FD):- stream_property(_,file_name(FD)).

pl_probe_file(In,M):- path_probe(FD),once((with_fstem(FD,In,M))),!.

set_default_path_early:-
   working_directory(X,X),
   assertz(wl:interned_eval(call((to_lisp_pathname(X,Path),
     set_opv(sym('cl:*default-pathname-defaults*'),symbol_value,Path))))).

:- prolog_load_context(directory,X),
   assertz(wl:interned_eval(call((to_lisp_pathname(X,Path),
     set_opv(sym('sys:*LISP-HOME*'),symbol_value,Path))))).

wl:interned_eval(call((to_lisp_pathname("",Path),
     set_opv(sym('cl:*default-pathname-defaults*'),symbol_value,Path)))).


% Uses Symbol value: *SOURCE-FILE-TYPES*
check_file_types(SearchTypes):- 
   f_symbol_value(sys_xx_source_file_types_xx,FileTypes),
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

f_pathname(S,P):- is_stream(S),stream_property(S,file(File)),!,f_pathname(File,P).
f_pathname(P,P):- is_pathnamep(P),!.
f_pathname(String,Pathname):- f_sys_string_to_pathname(String,Pathname).

f_namestring(Pathname,String):- trace,
    get_opv(Pathname,pathname_directory,D),
    (D==[]->DS1='';(loc_to_pl(D,DS),atom_concat(DS,'/',DS1))),
    get_opv(Pathname,pathname_name,N),
    (N==[]->DS2=DS1;(loc_to_pl(N,NS),atom_concat(DS1,NS,DS2))),
    get_opv(Pathname,pathname_type,E),
    (E==[]->sformat(String,'~w',[DS2]);(loc_to_pl(E,ES),sformat(String,'~w.~w',[DS2,ES]))),
    !.
f_namestring(Atom,String):- f_string(Atom,String),!.
f_namestring(In,Out):- to_prolog_pathname(In,Atom),atom_string(Atom,String),to_lisp_string(String,Out).
f_namestring(Pathname,String):-
    get_opv(Pathname,pathname_namestring,PN),!,
    string_to_prolog_atom(PN,Atom),atom_string(Atom,String).
f_namestring(Pathname,String):-
    throw(todo(f_namestring(Pathname,String))).

f_sys_string_to_pathname(String,Pathname):- 
  to_prolog_string(String,String0),atom_string(PlPath,String0),
  file_base_name(PlPath,BaseExt),
  file_name_extension(Base, Ext,BaseExt),atom_string(Base,Name),
  maybe_nil_pathname(BaseExt,Ext,Type),
  file_directory_name(PlPath,PlDir),lisp_dir_list(PlDir,PlPath,LispDir),
  f_make_instance([claz_pathname,kw_name,Name,kw_type,Type,kw_directory,LispDir],Pathname).

lisp_dir_list('.',PlPath,[kw_relative]):- atom_concat('.',_,PlPath),!.
lisp_dir_list('.',_PlPath,[]).
lisp_dir_list('/',_,[kw_absolute]).
lisp_dir_list(PlDir,_,LispDir):- 
  ((is_absolute_file_name(PlDir),atomic_list_concat([_|List],'/',PlDir))
     -> LispDir = [kw_absolute|DirStrs] ; 
    (atomic_list_concat(List,'/',PlDir),LispDir = [kw_relative|DirStrs])),
  must_maplist(maybe_nil_pathname('.'),List,DirStrs).

loc_to_pl(L,A):- a_2_l(A,L),!.
loc_to_pl(L,A):- is_list(L),must_maplist(loc_to_pl,L,LL),atomic_list_concat(LL,'/',A).
loc_to_pl(S,A):- atom_string(A,S).

a_2_l('.',kw_relative):-!.
a_2_l('..',kw_up):-!.
a_2_l('*',kw_wild):-!.
a_2_l('',kw_absolute):-!.
maybe_nil_pathname(PlPath,'',""):- atom_concat(_,'.',PlPath),!.
maybe_nil_pathname(_,'',[]):-!.
maybe_nil_pathname(_,A,L):- a_2_l(A,L),!.
maybe_nil_pathname(_,Atom,String):- atom_string(Atom,String).



wl:interned_eval('
(defclass pathname ()
  ((host      :accessor pathname-host
              :initarg :host
              :initform nil)
   (device    :accessor pathname-device
              :initarg :device
              :initform :unspecific)
   (directory :accessor pathname-directory
              :initarg :directory
              :initform nil)
   (name      :accessor pathname-name
              :initarg :name
              :initform nil)
   (type      :accessor pathname-type
              :initarg :type
              :initform nil)
   (version   :accessor pathname-version
              :initarg :version
              :initform nil))
  (:documentation "A physical pathname."))
').

:- fixup_exports.

end_of_file.