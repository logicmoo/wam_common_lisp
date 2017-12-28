

cpgrovel () {
   echo -e "\n\n\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1\n;;;; BEGIN FILE $1\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2"
   cat "$1"
   echo -e ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3\n;;;; END FILE $1\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n\n\n\n\n\n\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n\n\n\n\n\n\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n\n\n\n\n\n\n\n\n\n\n;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n\n\n\n\n\n\n\n\n\n"
}

export cpgrovel

# SBCL find -name "*.l*p" -printf "%p %h %s %f %p\n"| sort -r | sort -k2,2 -k4.3 -k4,4 -k1,1r -k2,2n 
# ECL cls ; find -name "*.l*p" -printf "%p %h %s %f %p\n"| sort -r | sort -k2,2r -k4.3 -k4,4 -k1,1r -k2,2n 
# ABCL cls ; find -name "*.l*p" -printf "%p %h %s %f %p\n"| sort -r | sort  -k2,2n -k4.3 -k2,2r -k4.3 -k4,4 -k1,1r -k2,2n 
# GCL  cls ; find -wholename "*lsp/*.l*p" -printf "%p %h %s %f %p\n" | sort -k 3,3n
# cls ; find -wholename "*.l*p" -printf "%p %h %s %f %p\n" | sort -k1.5r -r
# xargs -n 2 -I {} -i bash -c 'echo ==== {} ====; echo cat {}; echo' 
# cls ; find -wholename "*.*el" -printf "%p %h %s %f %p\n" | sort -k3.6
#find -wholename "*.l*p" 
#find . -type f  | xargs -0 -I % sh -c 'echo %; cat %'
#cls ; find -name "*.l*p" -printf "%h %s %p\n"|sort -k1,1r -k2,2n | cut -f 3 -d ' ' 
#cls ; find -name "*.l*p" -printf "%p %h %s %f %p\n"| sort -r | sort -k2,2 -k4.3 -k4,4 -k1,1r -k2,2n | cut -f 5 -d ' ' | xargs -i bash 

__='



cpgrovel ./reference/sbcl/src/code/sysmacs.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cmacros.lisp sbcl
cpgrovel ./reference/sbcl/src/code/setf-funs.lisp sbcl
cpgrovel ./reference/sbcl/src/code/setf.lisp sbcl
cpgrovel ./reference/sbcl/src/code/macroexpand.lisp sbcl
cpgrovel ./reference/sbcl/src/code/macros.lisp sbcl
cpgrovel ./reference/sbcl/src/code/symbol.lisp sbcl
cpgrovel ./reference/sbcl/src/code/list.lisp sbcl
cpgrovel ./reference/sbcl/src/code/load.lisp sbcl
cpgrovel ./reference/sbcl/src/code/maphash.lisp sbcl
cpgrovel ./reference/sbcl/src/code/misc.lisp sbcl
cpgrovel ./reference/sbcl/src/code/module.lisp sbcl
cpgrovel ./reference/sbcl/src/code/readtable.lisp sbcl
cpgrovel ./reference/sbcl/src/code/seq.lisp sbcl
cpgrovel ./reference/sbcl/src/code/string.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cl-specials.lisp sbcl
cpgrovel ./reference/sbcl/src/code/reader.lisp sbcl
cpgrovel ./reference/sbcl/src/code/ansi-stream.lisp sbcl
cpgrovel ./reference/sbcl/src/code/random.lisp sbcl
cpgrovel ./reference/sbcl/src/code/restart.lisp sbcl
cpgrovel ./reference/sbcl/src/code/array.lisp sbcl
cpgrovel ./reference/sbcl/src/code/backq.lisp sbcl
cpgrovel ./reference/sbcl/src/code/barrier.lisp sbcl
cpgrovel ./reference/sbcl/src/code/bignum-random.lisp sbcl
cpgrovel ./reference/sbcl/src/code/bignum.lisp sbcl
cpgrovel ./reference/sbcl/src/code/bit-bash.lisp sbcl
cpgrovel ./reference/sbcl/src/code/char.lisp sbcl
cpgrovel ./reference/sbcl/src/code/class-init.lisp sbcl
cpgrovel ./reference/sbcl/src/code/class.lisp sbcl
cpgrovel ./reference/sbcl/src/code/coerce.lisp sbcl
cpgrovel ./reference/sbcl/src/code/common-os.lisp sbcl
cpgrovel ./reference/sbcl/src/code/condition.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defmacro.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defpackage.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defsetfs.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defstruct.lisp sbcl
cpgrovel ./reference/sbcl/src/code/deftypes-for-target.lisp sbcl
cpgrovel ./reference/sbcl/src/code/describe-policy.lisp sbcl
cpgrovel ./reference/sbcl/src/code/describe.lisp sbcl
cpgrovel ./reference/sbcl/src/code/destructuring-bind.lisp sbcl
cpgrovel ./reference/sbcl/src/code/dyncount.lisp sbcl
cpgrovel ./reference/sbcl/src/code/error-error.lisp sbcl
cpgrovel ./reference/sbcl/src/code/error.lisp sbcl
cpgrovel ./reference/sbcl/src/code/eval.lisp sbcl
cpgrovel ./reference/sbcl/src/code/fd-stream.lisp sbcl
cpgrovel ./reference/sbcl/src/code/fdefinition.lisp sbcl
cpgrovel ./reference/sbcl/src/code/filesys.lisp sbcl
cpgrovel ./reference/sbcl/src/code/final.lisp sbcl
cpgrovel ./reference/sbcl/src/code/float-trap.lisp sbcl
cpgrovel ./reference/sbcl/src/code/float.lisp sbcl
cpgrovel ./reference/sbcl/src/code/fop.lisp sbcl
cpgrovel ./reference/sbcl/src/code/foreign-load.lisp sbcl
cpgrovel ./reference/sbcl/src/code/foreign.lisp sbcl
cpgrovel ./reference/sbcl/src/code/format-directive.lisp sbcl
cpgrovel ./reference/sbcl/src/code/format-time.lisp sbcl
cpgrovel ./reference/sbcl/src/code/full-eval.lisp sbcl
cpgrovel ./reference/sbcl/src/code/function-names.lisp sbcl
cpgrovel ./reference/sbcl/src/code/funutils.lisp sbcl
cpgrovel ./reference/sbcl/src/code/gc.lisp sbcl
cpgrovel ./reference/sbcl/src/code/globals.lisp sbcl
cpgrovel ./reference/sbcl/src/code/hash-table.lisp sbcl
cpgrovel ./reference/sbcl/src/code/inspect.lisp sbcl
cpgrovel ./reference/sbcl/src/code/interr.lisp sbcl
cpgrovel ./reference/sbcl/src/code/irrat.lisp sbcl
cpgrovel ./reference/sbcl/src/code/kernel.lisp sbcl
cpgrovel ./reference/sbcl/src/code/loop.lisp sbcl
cpgrovel ./reference/sbcl/src/code/ntrace.lisp sbcl
cpgrovel ./reference/sbcl/src/code/numbers.lisp sbcl
cpgrovel ./reference/sbcl/src/code/octets.lisp sbcl
cpgrovel ./reference/sbcl/src/code/package.lisp sbcl
cpgrovel ./reference/sbcl/src/code/pathname.lisp sbcl
cpgrovel ./reference/sbcl/src/code/pprint.lisp sbcl
cpgrovel ./reference/sbcl/src/code/pred.lisp sbcl
cpgrovel ./reference/sbcl/src/code/primordial-extensions.lisp sbcl
cpgrovel ./reference/sbcl/src/code/primordial-type.lisp sbcl
cpgrovel ./reference/sbcl/src/code/print.lisp sbcl
cpgrovel ./reference/sbcl/src/code/repack-xref.lisp sbcl
cpgrovel ./reference/sbcl/src/code/profile.lisp sbcl
cpgrovel ./reference/sbcl/src/code/purify.lisp sbcl
cpgrovel ./reference/sbcl/src/code/quantifiers.lisp sbcl
cpgrovel ./reference/sbcl/src/code/query.lisp sbcl
cpgrovel ./reference/sbcl/src/code/room.lisp sbcl
cpgrovel ./reference/sbcl/src/code/run-program.lisp sbcl
cpgrovel ./reference/sbcl/src/code/save.lisp sbcl
cpgrovel ./reference/sbcl/src/code/sc-offset.lisp sbcl
cpgrovel ./reference/sbcl/src/code/serve-event.lisp sbcl
cpgrovel ./reference/sbcl/src/code/signal.lisp sbcl
cpgrovel ./reference/sbcl/src/code/sort.lisp sbcl
cpgrovel ./reference/sbcl/src/code/source-location.lisp sbcl
cpgrovel ./reference/sbcl/src/code/specializable-array.lisp sbcl
cpgrovel ./reference/sbcl/src/code/step.lisp sbcl
cpgrovel ./reference/sbcl/src/code/stream.lisp sbcl
cpgrovel ./reference/sbcl/src/code/sxhash.lisp sbcl
cpgrovel ./reference/sbcl/src/code/thread.lisp sbcl
cpgrovel ./reference/sbcl/src/code/time.lisp sbcl
cpgrovel ./reference/sbcl/src/code/timer.lisp sbcl
cpgrovel ./reference/sbcl/src/code/toplevel.lisp sbcl
cpgrovel ./reference/sbcl/src/code/type-class.lisp sbcl
cpgrovel ./reference/sbcl/src/code/type-init.lisp sbcl
cpgrovel ./reference/sbcl/src/code/typep.lisp sbcl

cpgrovel ./reference/sbcl/src/compiler/fndb.lisp sbcl
cpgrovel ./reference/sbcl/doc/manual/docstrings.lisp sbcl
cpgrovel ./reference/sbcl/doc/manual/create-contrib-doc-list.lisp sbcl

cpgrovel ./reference/sbcl/src/code/debug-info.lisp sbcl
cpgrovel ./reference/sbcl/src/code/debug-int.lisp sbcl
cpgrovel ./reference/sbcl/src/code/debug-var-io.lisp sbcl
cpgrovel ./reference/sbcl/src/code/debug.lisp sbcl


cpgrovel ./reference/sbcl/src/code/misc-aliens.lisp sbcl
cpgrovel ./reference/sbcl/src/code/parse-body.lisp sbcl
cpgrovel ./reference/sbcl/src/code/parse-defmacro-errors.lisp sbcl
cpgrovel ./reference/sbcl/src/code/show.lisp sbcl
cpgrovel ./reference/sbcl/src/code/xset.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/set-up-cold-packages.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/ansify.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cold-error.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cold-init-helper-macros.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cold-init.lisp sbcl
cpgrovel ./reference/sbcl/src/code/condition-boot.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/compile-cold-sbcl.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-array.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-cl.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-class.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-classoid.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-constants.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-defmethod.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-extensions.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-fasl.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-float.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-format.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-full-eval.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-impl.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-package.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-pprint.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-print.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-raw-slots.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-source-location.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-step.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-thread.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-time.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-type.lisp sbcl
cpgrovel ./reference/sbcl/src/code/huffman.lisp sbcl
cpgrovel ./reference/sbcl/src/code/early-alieneval.lisp sbcl


cpgrovel ./reference/sbcl/src/code/cross-byte.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-char.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-condition.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-early.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-float.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-io.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-misc.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-modular.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-thread.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cross-type.lisp sbcl
cpgrovel ./reference/sbcl/src/code/alien-type.lisp sbcl
cpgrovel ./reference/sbcl/src/code/alloc.lisp sbcl
cpgrovel ./reference/sbcl/src/code/cas.lisp sbcl
cpgrovel ./reference/sbcl/src/code/deadline.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defbangconstant.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defbangstruct.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defbangtype.lisp sbcl
cpgrovel ./reference/sbcl/src/code/defboot.lisp sbcl
cpgrovel ./reference/sbcl/src/code/exhaust.lisp sbcl
cpgrovel ./reference/sbcl/src/code/force-delayed-defbangconstants.lisp sbcl
cpgrovel ./reference/sbcl/src/code/force-delayed-defbangstructs.lisp sbcl
cpgrovel ./reference/sbcl/src/code/host-alieneval.lisp sbcl
cpgrovel ./reference/sbcl/src/code/host-c-call.lisp sbcl
cpgrovel ./reference/sbcl/src/code/host-pprint.lisp sbcl
cpgrovel ./reference/sbcl/src/code/immobile-space.lisp sbcl
cpgrovel ./reference/sbcl/src/code/late-cas.lisp sbcl
cpgrovel ./reference/sbcl/src/code/late-condition.lisp sbcl
cpgrovel ./reference/sbcl/src/code/late-extensions.lisp sbcl
cpgrovel ./reference/sbcl/src/code/late-format.lisp sbcl
cpgrovel ./reference/sbcl/src/code/late-type.lisp sbcl
cpgrovel ./reference/sbcl/src/code/linkage-table.lisp sbcl
cpgrovel ./reference/sbcl/src/code/shaketree.lisp sbcl
cpgrovel ./reference/sbcl/src/code/sharpm.lisp sbcl
cpgrovel ./reference/sbcl/src/code/stubs.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-alieneval.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-allocate.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-c-call.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-char.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-defstruct.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-error.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-exception.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-extensions.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-format.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-hash-table.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-lfhash.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-load.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-misc.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-package.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-pathname.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-random.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-sap.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-signal-common.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-signal.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-stream.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-sxhash.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-thread.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-type.lisp sbcl
cpgrovel ./reference/sbcl/src/code/target-unicode.lisp sbcl
cpgrovel ./reference/sbcl/src/code/traceroot.lisp sbcl
cpgrovel ./reference/sbcl/src/code/uncross.lisp sbcl
cpgrovel ./reference/sbcl/src/code/unix-foreign-load.lisp sbcl
cpgrovel ./reference/sbcl/src/code/unix-pathname.lisp sbcl
cpgrovel ./reference/sbcl/src/code/unix.lisp sbcl
cpgrovel ./reference/sbcl/src/code/unportable-float.lisp sbcl
cpgrovel ./reference/sbcl/src/code/warm-error.lisp sbcl
cpgrovel ./reference/sbcl/src/code/warm-lib.lisp sbcl
cpgrovel ./reference/sbcl/src/code/warm-mswin.lisp sbcl
cpgrovel ./reference/sbcl/src/code/weak.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/chill.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/defun-load-or-cload-xcompiler.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/muffler.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/shared.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/shebang.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/slam.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/snapshot.lisp sbcl
cpgrovel ./reference/sbcl/src/cold/warm.lisp sbcl



cpgrovel ./reference/sbcl/src/interpreter/basic-env.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/checkfuns.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/debug.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/env.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/eval.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/function.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/macros.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/sexpr.lisp sbcl
cpgrovel ./reference/sbcl/src/interpreter/special-forms.lisp sbcl

cpgrovel ./reference/sbcl/src/pcl/boot.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/braid.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/cache.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/combin.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/compiler-support.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/cpl.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/ctor.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/ctypes.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/defclass.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/defcombin.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/defs.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/dfun.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/dlisp.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/dlisp3.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/documentation.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/early-low.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/env.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/fixup.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/fngen.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/fsc.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/generic-functions.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/gray-streams-class.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/gray-streams.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/init.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/low.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/macros.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/methods.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/pre-warm.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/precom1.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/precom2.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/print-object.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/sequence.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/slot-name.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/slots-boot.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/slots.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/std-class.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/time.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/vector.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/walk.lisp sbcl
cpgrovel ./reference/sbcl/src/pcl/wrapper.lisp sbcl

cpgrovel ./reference/sbcl/load-xc.lisp sbcl
cpgrovel ./reference/sbcl/loader.lisp sbcl
cpgrovel ./reference/sbcl/make-genesis-2.lisp sbcl
cpgrovel ./reference/sbcl/make-host-1.lisp sbcl
cpgrovel ./reference/sbcl/make-host-2.lisp sbcl
cpgrovel ./reference/sbcl/make-target-2-load.lisp sbcl

'
