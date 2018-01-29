
(in-package 'compiler)

;;; The production version:
(proclaim '(optimize (safety 0) (space 3) (speed 3)))

;#-CLOS
(defmacro setf-namep (setf-list)
  `(let (setf-symbol)
     (and (consp ,setf-list)
	  (= 2 (length ,setf-list))
	  (eq (car ,setf-list) 'setf)
	  (setq setf-symbol (get (second ,setf-list) 'sys::setf-symbol))
	  (symbolp setf-symbol)
	  setf-symbol)))

;;; Disable PDE facilities within LISP kernel:
(setq *features* (delete ':pde *features*))
;;; Disable record-source-pathname within LISP kernel:
(defmacro record-source-pathname (x y))
