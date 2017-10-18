(in-package :ccl)

#|

compiler warning suppression
The first example patches the toplevel-loop with a handler for compiler
warnings.  Its only dependence on undocumented features is that it knows
that the compiler signals conditions of type CCL::COMPILER-WARNING.  Its
drawback is that it needs to throw to toplevel to install itself.

---------------------------------------------------------------------------

|#

#+:MCL
(progn
  (export '*signal-compiler-warnings*)
  
  (defparameter *signal-compiler-warnings* nil)
  
  (defun compiler-warning-muffler ()
    (handler-bind ((compiler-warning
                    #'(lambda (condition)
                        (unless *signal-compiler-warnings*
                          (muffle-warning condition)))))
      (toplevel-loop)))
  
  (defun install-compiler-warning-muffler ()
    (unless (eq #'compiler-warning-muffler (%set-toplevel))
      (%set-toplevel #'compiler-warning-muffler)
      (toplevel)))
  
  ;(install-compiler-warning-muffler)
  )


#|
---------------------------------------------------------------------------

Another way to do this is to patch the internal function that the
compiler calls to signal its warnings. Doing it this way is much more
likely to break in future releases of MCL. I would have used ADVISE,
but the syntax of ADVISE has changed since 2.0b1.

---------------------------------------------------------------------------

(in-package :ccl)

(export *signal-compiler-warnings*)

(defparameter *signal-compiler-warnings* t)

(defvar *compiler-warning-signaller* #'signal-compiler-warning)

(let ((*warn-if-redefine* nil)
      (*warn-if-redefine-kernel* nil))
  (defun signal-compiler-warning (&rest rest)
    (declare (dynamic-extent rest))
    (when *signal-compiler-warnings*
      (apply *compiler-warning-signaller* rest))))

---------------------------------------------------------------------------

|#