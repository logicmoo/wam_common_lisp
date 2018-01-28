(in-package 'pcl)

;;; This next kludge disables macro memoization (the default) since somewhere
;;; in PCL, the memoization is getting in the way.

(eval-when (load eval)
    (format t "~&;;; Resetting *MACROEXPAND-HOOK* to #'FUNCALL~%")
    (setq lisp::*macroexpand-hook* #'funcall))

