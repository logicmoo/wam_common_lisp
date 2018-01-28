;;				-[Thu Feb 22 08:38:07 1990 by jkf]-
;; cpatch.cl
;;  compiler patch for the fast clos
;;  
;; copyright (c) 1990 Franz Inc.
;;

#+pcl-user-instances
(eval-when (compile load eval)
(error "Cannot use user-instances in EXCL on Sun4 with cpatch.lisp and
        quadlap.lisp optimizations (see low.lisp).")
)
(in-package :comp)

(def-quad-op tail-funcall qp-end-block
  ;; u = (argcount function-object)
  ;;
  ;; does a tail call to the function-object given
  ;; never returns
  )

(defun-in-runtime sys::copy-function (func))

(in-package :hyperion)

(def-quad-hyp r-tail-funcall comp::tail-funcall (u d quad)
  ;; u = (argcount function)
  ;;
  (r-move-single-to-loc (treg-loc (car u)) *count-reg*)
  (r-move-single-to-loc (treg-loc (cadr u)) *fcnin-reg*)
  (re restore *zero-reg* *zero-reg*)
  (re move.l `(d #.r-function-start-adj #.*fcnout-reg*) '#.*ctr2-reg*)
  (re jmpl '(d 0 #.*ctr2-reg*) *zero-reg*)
  (re nop))
  
  

