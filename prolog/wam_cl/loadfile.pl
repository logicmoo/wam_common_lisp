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
:- module(loadfile, []).

:- set_module(class(library)).

:- include('header.pro').

cddd:- cd('/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/t/daydreamer/').

dd:- cddd,cl_grovel_file('dd.cl',_).

dd1:- cddd,cl_load('dd_compile.cl',_),
           cl_load('gate_compile.cl',_).

dd2:- cddd,cl_load('dd.cl',_).

tdd:- cl_grovel_file('../../t/daydreamer/*.cl',_).
tdd1:- cl_compile_file('../../t/daydreamer/*.cl',_).
tdd2:- cl_load('../../t/daydreamer/*.cl',_).

cl_compile_file_mask(Mask,keys(Keys),TF):- 
   with_each_file(do_compile_1file(Keys,TF),Mask).


/*
Function COMPILE-FILE
Syntax:

compile-file input-file &key output-file verbose print external-format

=> output-truename, warnings-p, failure-p

Arguments and Values:

input-file---a pathname designator. (Default fillers for unspecified components are taken from *default-pathname-defaults*.)
output-file---a pathname designator. The default is implementation-defined.
verbose---a generalized boolean. The default is the value of *compile-verbose*.
print---a generalized boolean. The default is the value of *compile-print*.
external-format---an external file format designator. The default is :default.
output-truename---a pathname (the truename of the output file), or nil.
warnings-p---a generalized boolean.
failure-p---a generalized boolean.

Description:

compile-file transforms the contents of the file specified by input-file into implementation-dependent binary data 
 which are placed in the file specified by output-file.

The file to which input-file refers should be a source file. output-file can be used to specify an output pathname; 
  the actual pathname of the compiled file to which compiled code will be output is computed 
   as if by calling compile-file-pathname.

If input-file or output-file is a logical pathname, it is translated into a physical pathname as if by calling 
  translate-logical-pathname.

If verbose is true, compile-file prints a message in the form of a comment (i.e., with a leading semicolon) 
     to standard output indicating what file is being compiled and other useful information.
 If verbose is false, compile-file does not print this information.

If print is true, information about top level forms in the file being compiled is printed to standard output. 
  Exactly what is printed is implementation-dependent, but nevertheless some information is printed. If print is nil, 
  no information is printed.

The external-format specifies the external file format to be used when opening the file; 
 see the function open. compile-file and load must cooperate in such a way that the resulting compiled 
 file can be loaded without specifying an external file format anew; see the function load.

compile-file binds *readtable* and *package* to the values they held before processing the file.

*compile-file-truename* is bound by compile-file to hold the truename of the pathname of the file being compiled.

*compile-file-pathname* is bound by compile-file to hold a pathname denoted by the first argument to compile-file, 
  merged against the defaults; that is, (pathname (merge-pathnames input-file)).

The compiled functions contained in the compiled file become available for use when the compiled file is loaded into Lisp. 
  Any function definition that is processed by the compiler, including #'(lambda ...) forms and local function definitions 
  made by flet, labels and defun forms, result in an object of type compiled-function.

The primary value returned by compile-file, output-truename, is the truename of the output file, 
 or nil if the file could not be created.

The secondary value, warnings-p, is false if no conditions of type error or warning were detected by 
  the compiler, and true otherwise.

The tertiary value, failure-p, is false if no conditions of type error or warning (other than style-warning) were 
 detected by the compiler, and true otherwise.

For general information about how files are processed by the file compiler, see Section 3.2.3 (File Compilation).

Programs to be compiled by the file compiler must only contain externalizable objects; for details on such objects, 
  see Section 3.2.4 (Literal Objects in Compiled Files). For information on how to extend the set of externalizable 
  objects, see the function make-load-form and Section 3.2.4.4 (Additional Constraints on Externalizable Objects).

Examples: None.

Affected By:  *error-output*, *standard-output*, *compile-verbose*, *compile-print*

Exceptional Situations:

For information about errors detected during the compilation process, see Section 3.2.5
  (Exceptional Situations in the Compiler).

An error of type file-error might be signaled if (wild-pathname-p input-file) returns true.

If either the attempt to open the source file for input or the attempt to open the compiled file for output fails,
  an error of type file-error is signaled.

See Also:

compile, declare, eval-when, pathname, logical-pathname, 
  Section 20.1 (File System Concepts), Section 19.1.2 (Pathnames as Filenames)

Notes: None.
*/

cl_compile_file(File,R):-
  cl_compile_file(File,keys([]),R).
cl_compile_file(File,keys(Keys),R):-
  do_compile_1file(Keys,File),!,
  cl_truename(File,R),!.

do_compile_1file(_Keys,File):- pl_compiled_filename(File,PL),!,wdmsg(cl_compile_file(PL)),ensure_loaded(PL).
do_compile_1file(_Keys,File0):-

   %ignore(R=t),
   search_for(File0,File),
   prolog_to_os_filename(File,OSFile),
   atom_concat_or_rtrace(OSFile,'.trans.pl',PLFile),
   locally_let(
     [sym('sys::*compile-file-pathname*')=str(File),
      sym('sys::*compile-file-truename*')=str(OSFile),
      sym('sys::*output-file-pathname*')=str(PLFile),
      sym('cl:*package*')=value(sym('*package*'))],   
        setup_call_cleanup(
         open(PLFile,write,Stream),          
            with_each_file(with_each_form(lisp_compile_to_prolog(Stream)),File),
          close(Stream))).

lisp_compile_to_prolog(Stream,Expression):- is_stream(Stream),!,  
  with_output_to(Stream,lisp_compile_to_prolog(Expression)),!.
lisp_compile_to_prolog(Package,Expression):-  
  must_or_rtrace(locally_let(xx_package_xx=Package,
    must_or_rtrace(lisp_compile_to_prolog(Expression)))),!.

lisp_compile_to_prolog(COMMENTP):- is_comment(COMMENTP,String),!,write('/*'),write(String),writeln('*/').
lisp_compile_to_prolog(Expression):-   
 must_or_rtrace((
  as_sexp(Expression,SExpression),  
  nl,
  flush_all_output_safe,
  write('/*********** '),
  ignore((nb_current('$lisp_translation_stream',In),stream_property(In,file_name(File)),write(File))),
  ignore((nb_current('$lisp_translation_line',LineChars),write(:),write(LineChars))),
  writeln(' **********************/'),
  reading_package(Pkg),
  dbmsg(:- lisp_compile_to_prolog(Pkg,SExpression)),
  lisp_compile_to_prolog_pass1(SExpression))),!.


%write_trans(:-((A,B))):-!,write_trans(:-A),write_trans(:-B).
%write_trans(:-asserta((A:-B))):- !, fmt9((A:-B)).
write_trans(P):- dbmsg(P).

% lisp_compile_to_prolog/1 already interned the important stuff
lisp_compile_to_prolog_pass1(_Expression):- source_location(_,_),!.
lisp_compile_to_prolog_pass1(SExpression):- 
  reader_intern_symbols(SExpression,FExpression),
  
   Expression = FExpression, 
   debug_var('_Ignored',Result),
   lisp_compile(Result,Expression,PrologCode),
   write_trans(:- PrologCode),
   must(lisp_compile_to_prolog_pass3(PrologCode)),!.
   

lisp_compile_to_prolog_pass2(:- PrologCode):- !, lisp_compile_to_prolog_pass3(PrologCode).
lisp_compile_to_prolog_pass2(_).

lisp_compile_to_prolog_pass3((A,B)):-!, lisp_compile_to_prolog_pass3(A),lisp_compile_to_prolog_pass3(B).
lisp_compile_to_prolog_pass3(asserta(PrologCode)):- !, lisp_compile_to_prolog_pass3(PrologCode).
lisp_compile_to_prolog_pass3(assertz(PrologCode)):- !, lisp_compile_to_prolog_pass3(PrologCode).
lisp_compile_to_prolog_pass3(assert(PrologCode)):- !, lisp_compile_to_prolog_pass3(PrologCode).

% lisp_compile_to_prolog_pass3(PrologCode):- \+ compound(PrologCode),!.

lisp_compile_to_prolog_pass3(cl_in_package(Into, Package)):-!, cl_in_package(Into, Package).
lisp_compile_to_prolog_pass3(cl_use_package(Package, Load_Ret)):-!, cl_use_package(Package, Load_Ret).
lisp_compile_to_prolog_pass3(cl_load(File, Load_Ret)):- !, must_or_rtrace(cl_load(File, Load_Ret)).
lisp_compile_to_prolog_pass3(cl_compile_file(File, Load_Ret)):- !, must_or_rtrace(cl_compile_file(File, Load_Ret)).
lisp_compile_to_prolog_pass3(_).
/*
lisp_compile_to_prolog_pass3(MP):- strip_module(MP,_,P),functor(P,F,_),arg(_,
  v(doc_string,macro_lambda,function_lambda,arglist_info),F),!,
  asserta(MP),
  write_trans(MP).
lisp_compile_to_prolog_pass3(MP):- write_trans(MP).
   
   %*compile-file-truename*
*/


cl_grovel_file(File,t):- pl_compiled_filename(File,PL),!,wdmsg(ensure_groveled(PL)),ensure_loaded(PL).
cl_grovel_file(File,t):- in_comment(format('~N; Grovel.. ~w~n',[File])),
   locally(local_override(with_forms,lisp_grovel),
    with_each_file(with_each_form(lisp_grovel_in_package),File)).

cl_load('$OBJ'(_Pathname,Loc),T):- string(Loc),!,cl_load(Loc,T).
cl_load(File,T):-
  local_override(with_forms,lisp_grovel),!,in_comment(format('~N; Grovel.. (LOAD ~w)~n',[File])),cl_grovel_file(File,T),!.
cl_load(File,t):- pl_compiled_filename(File,PL),!,in_comment(dbmsg(ensure_loaded(PL))),!,ensure_loaded(PL).
cl_load(File,t):- 
  cl_grovel_file(File,t),
  with_each_file(with_each_form(lisp_reader_compiled_eval()),File).


lisp_reader_compiled_eval(Forms):- reader_intern_symbols(Forms,FForms),lisp_compiled_eval(FForms).

lisp_grovel_in_package(Form):-
  reader_intern_symbols(Form,FForm),
  lisp_grovel(FForm).

lisp_grovel([load,File|_]):- !, cl_grovel_file(File, _Load_Ret).
lisp_grovel(['compile-file',File|_]):- !, cl_grovel_file(File, _Load_Ret).
lisp_grovel(['in-package',Package|_]):- !, cl_in_package(Package, _Load_Ret).
lisp_grovel(['use-package',Package|_]):- !, cl_use_package(Package, _Load_Ret).

lisp_grovel(Form):- must(lisp_compile(Form,PrologCode)),!,
  must(grovel_prolog_code(PrologCode)),!.

grovel_prolog_code(PrologCode):- \+ compound(PrologCode),!.
grovel_prolog_code(:- PrologCode):- grovel_prolog_code(PrologCode).
grovel_prolog_code(cl_load(File, Load_Ret)):- !, cl_grovel_file(File, Load_Ret).
grovel_prolog_code(cl_compile_file(File, Load_Ret)):- !, cl_grovel_file(File, Load_Ret).
grovel_prolog_code(cl_grovel_file(File, Load_Ret)):- !, cl_grovel_file(File, Load_Ret).
grovel_prolog_code(asserta(PrologCode)):- !, grovel_prolog_code(PrologCode).
grovel_prolog_code(assertz(PrologCode)):- !, grovel_prolog_code(PrologCode).
grovel_prolog_code(assert(PrologCode)):- !, grovel_prolog_code(PrologCode).
grovel_prolog_code((A,B)):-!, grovel_prolog_code(A),grovel_prolog_code(B).
grovel_prolog_code(MP):- strip_module(MP,_,P),functor(P,F,_),arg(_,
  v(doc_string,macro_lambda,function_lambda,arglist_info),F),!,asserta(MP).
grovel_prolog_code(_).
 

with_flist(How,List):- must_maplist(with1file(How),List).

with1file(How,File):- must_or_rtrace(call(How,File)).

with_each_form(How,File):- local_override(with_forms,What),What\==How,!,with_each_form(What,File).
with_each_form(How,File):-
   dmsg(with_lisp_translation(file(File),How)),
   with_lisp_translation(file(File),How).

expand_directory_file_path(FDir,Ext,List):- directory_file_path(FDir,Ext,Mask),expand_file_name(Mask,List),List\==[Mask].

with_directory(How,FDir):- expand_directory_file_path(FDir,'*.cl',List),!,with_flist(How,List).
with_directory(How,FDir):- expand_directory_file_path(FDir,'*.lisp',List),!,with_flist(How,List).
with_directory(How,FDir):- expand_directory_file_path(FDir,'*.lsp',List),!,with_flist(How,List).


with_each_file(How,File):- string(File),name(Atom,File),!,with_each_file(How,Atom).
with_each_file(How,File):- compound(File),!,absolute_file_name(File,Abs),file_directory_name(Abs,Dir),exists_directory(Dir),!,
  with_each_file(How,Abs).
with_each_file(How,File):- exists_file(File),!,with1file(How,File).
with_each_file(How,FDir):- exists_directory(FDir),with_directory(How,FDir),!.
with_each_file(How,Mask):- expand_file_name(Mask,List),List\==[Mask],!,with_flist(How,List).
with_each_file(How,File):- stream_property(_,file_name(FD)),with_fstem(FD,File,Found),!,with1file(How,Found).
with_each_file(How,FDir):- exists_directory(FDir),!,with_directory(How,FDir),!.
with_each_file(How,File):- working_directory(CD,CD),with_fstem(CD,File,Found),!,with1file(How,Found).

% with_each_file(How,File):- with_lisp_translation(File,lisp_compiled_eval).


:- fixup_exports.

/*


*/













               /*









make_pass(1,
 [ 'xabcl/abcl-contrib.lisp',
     'xabcl/adjoin.lisp',
     'xabcl/and.lisp',
     'xabcl/apropos.lisp',
     'xabcl/arrays.lisp',
     'xabcl/assert.lisp',
     'xabcl/assoc.lisp',
     'xabcl/autoloads.lisp',
     'xabcl/aver.lisp',
     'xabcl/backquote.lisp',
     'xabcl/bit-array-ops.lisp',
     'xabcl/boole.lisp',
     'xabcl/boot.lisp',
     'xabcl/butlast.lisp',
     'xabcl/byte-io.lisp',
     'xabcl/case.lisp',
     'xabcl/chars.lisp',
     'xabcl/check-type.lisp',
     'xabcl/coerce.lisp',
     'xabcl/collect.lisp',
     'xabcl/compile-file.lisp',
     'xabcl/compile-file-pathname.lisp',
     'xabcl/compiler-error.lisp',
     'xabcl/compiler-macro.lisp',
     'xabcl/compiler-pass1.lisp',
     'xabcl/compiler-pass2.lisp',
     'xabcl/compiler-types.lisp',
     'xabcl/compile-system.lisp',
     'xabcl/concatenate.lisp',
     'xabcl/cond.lisp',
     'xabcl/copy-seq.lisp',
     'xabcl/copy-symbol.lisp',
     'xabcl/count.lisp',
     'xabcl/debug.lisp',
     'xabcl/define-modify-macro.lisp',
     'xabcl/define-symbol-macro.lisp',
     'xabcl/defmacro.lisp',
     'xabcl/defpackage.lisp',
     'xabcl/defsetf.lisp',
     'xabcl/defstruct.lisp',
     'xabcl/deftype.lisp',
     'xabcl/delete-duplicates.lisp',
     'xabcl/delete.lisp',
     'xabcl/deposit-field.lisp',
     'xabcl/describe-compiler-policy.lisp',
     'xabcl/describe.lisp',
     'xabcl/destructuring-bind.lisp',
     'xabcl/digest.lisp',
     'xabcl/directory.lisp',
     'xabcl/disassemble.lisp',
     'xabcl/do-all-symbols.lisp',
     'xabcl/documentation.lisp',
     'xabcl/do-external-symbols.lisp',
     'xabcl/do.lisp',
     'xabcl/dolist.lisp',
     'xabcl/do-symbols.lisp',
     'xabcl/dotimes.lisp',
     'xabcl/dribble.lisp',
     'xabcl/dump-class.lisp',
     'xabcl/dump-form.lisp',
     'xabcl/early-defuns.lisp',
     'xabcl/ed.lisp',
     'xabcl/enough-namestring.lisp',
     'xabcl/ensure-directories-exist.lisp',
     'xabcl/error.lisp',
     'xabcl/extensible-sequences-base.lisp',
     'xabcl/extensible-sequences.lisp',
     'xabcl/fasl-concat.lisp',
     'xabcl/fdefinition.lisp',
     'xabcl/featurep.lisp',
     'xabcl/fill.lisp',
     'xabcl/find-all-symbols.lisp',
     'xabcl/find.lisp',
     'xabcl/format.lisp',
     'xabcl/gentemp.lisp',
     'xabcl/get-pid.lisp',
     'xabcl/gray-streams.lisp',
     'xabcl/gui.lisp',
     'xabcl/inline.lisp',
     'xabcl/inspect.lisp',
     'xabcl/java-collections.lisp',
     'xabcl/java.lisp',
     'xabcl/jvm-class-file.lisp',
     'xabcl/jvm-instructions.lisp',
     'xabcl/jvm.lisp',
     'xabcl/known-functions.lisp',
     'xabcl/known-symbols.lisp',
     'xabcl/late-setf.lisp',
     'xabcl/lcm.lisp',
     'xabcl/ldb.lisp',
     'xabcl/ldiff.lisp',
     'xabcl/list-length.lisp',
     'xabcl/list.lisp',
     'xabcl/load.lisp',
     'xabcl/loop.lisp',
     'xabcl/macros.lisp',
     'xabcl/make-hash-table.lisp',
     'xabcl/make-load-form-saving-slots.lisp',
     'xabcl/make-sequence.lisp',
     'xabcl/make-string.lisp',
     'xabcl/make-string-output-stream.lisp',
     'xabcl/map1.lisp',
     'xabcl/map-into.lisp',
     'xabcl/map.lisp',
     'xabcl/mask-field.lisp',
     'xabcl/member-if.lisp',
     'xabcl/mismatch.lisp',
     'xabcl/mop.lisp',
     'xabcl/multiple-value-bind.lisp',
     'xabcl/multiple-value-list.lisp',
     'xabcl/multiple-value-setq.lisp',
     'xabcl/nsubstitute.lisp',
     'xabcl/nth-value.lisp',
     'xabcl/numbers.lisp',
     'xabcl/open.lisp',
     'xabcl/or.lisp',
     'xabcl/package.lisp',
     'xabcl/parse-integer.lisp',
     'xabcl/parse-lambda-list.lisp',
     'xabcl/pathnames.lisp',
     'xabcl/pprint-dispatch.lisp',
     'xabcl/pprint.lisp',
     'xabcl/precompiler.lisp',
     'xabcl/print.lisp',
     'xabcl/print-object.lisp',
     'xabcl/print-unreadable-object.lisp',
     'xabcl/proclaim.lisp',
     'xabcl/profiler.lisp',
     'xabcl/prog.lisp',
     'xabcl/psetf.lisp',
     'xabcl/query.lisp',
     'xabcl/read-circle.lisp',
     'xabcl/read-conditional.lisp',
     'xabcl/read-from-string.lisp',
     'xabcl/read-sequence.lisp',
     'xabcl/reduce.lisp',
     'xabcl/remf.lisp',
     'xabcl/remove-duplicates.lisp',
     'xabcl/remove.lisp',
     'xabcl/replace.lisp',
     'xabcl/require.lisp',
     'xabcl/restart.lisp',
     'xabcl/revappend.lisp',
     'xabcl/rotatef.lisp',
     'xabcl/run-benchmarks.lisp',
     'xabcl/run-program.lisp',
     'xabcl/run-shell-command.lisp',
     'xabcl/runtime-class.lisp',
     'xabcl/search.lisp',
     'xabcl/sequences.lisp',
     'xabcl/setf.lisp',
     'xabcl/sets.lisp',
     'xabcl/shiftf.lisp',
     'xabcl/signal.lisp',
     'xabcl/socket.lisp',
     'xabcl/sort.lisp',
     'xabcl/source-transform.lisp',
     'xabcl/step.lisp',
     'xabcl/strings.lisp',
     'xabcl/sublis.lisp',
     'xabcl/substitute.lisp',
     'xabcl/subst.lisp',
     'xabcl/subtypep.lisp',
     'xabcl/tailp.lisp',
     'xabcl/threads.lisp',
     'xabcl/time.lisp',
     'xabcl/top-level.lisp',
     'xabcl/trace.lisp',
     'xabcl/tree-equal.lisp',
     'xabcl/typep.lisp',
     'xabcl/upgraded-complex-part-type.lisp',
     'xabcl/with-accessors.lisp',
     'xabcl/with-hash-table-iterator.lisp',
     'xabcl/with-input-from-string.lisp',
     'xabcl/with-open-file.lisp',
     'xabcl/with-output-to-string.lisp',
     'xabcl/with-package-iterator.lisp',
     'xabcl/with-slots.lisp',
     'xabcl/with-standard-io-syntax.lisp',
     'xabcl/write-sequence.lisp'
   ]).
make_pass(2,['xabcl/clos.lisp',
   'xabcl/asdf.lisp',
   'xabcl/autoloads-gen.lisp']).


*/
