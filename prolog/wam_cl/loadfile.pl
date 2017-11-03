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




dd:- cl_compile_file('../../t/daydreamer/dd_compile.cl',_).
% dd:- cl_compile_file('../../t/daydreamer/*.cl',_).

compile_file1(File):- with_lisp_translation(file(File),lisp_compiled_eval).
cl_load1(File,t):-with_lisp_translation(file(File),lisp_compiled_eval).

cl_load(File,R):- string(File),name(Atom,File),!,cl_load(Atom,R).
cl_load(File,R):- exists_file(File),!, cl_load1(File,R).
cl_load(File,R):- stream_property(_,file_name(F)),
  absolute_file_name(File,Found,[relative_to(F),extensions(['','.cl','.lisp','.lsp']),access(read),file_errors(fail)]),
  exists_file(Found),!,cl_load1(Found,R).
cl_load(File,R):- absolute_file_name(File,Found,[extensions(['','.cl','.lisp','.lsp']),access(read),file_errors(fail)]),
  exists_file(Found),!,cl_load1(File,R).

cl_compile_file(File,t):- exists_file(File),!,compile_file1(File).
cl_compile_file(Dir,R):- exists_directory(Dir),!,directory_file_path(Dir,'*.lisp',Mask),!, cl_compile_file(Mask,R).
cl_compile_file(Mask,t):- expand_file_name(Mask,List),List\==[Mask],!,must_maplist(compile_file1,List).
cl_compile_file(File,t):- compile_file1(File).


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


% cl_compile_file(File,t):- with_lisp_translation(File,lisp_compiled_eval).


:- fixup_exports.

