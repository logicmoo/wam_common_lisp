;; Core facilities required by the vendored ANSI test framework.

(is equal
    '(1 2 . 3)
    (list* 1 2 3))

(defparameter *ansi-alias-left* nil)
(defparameter *ansi-alias-right* nil)

(is equal
    '(t (nil a) (nil a))
    (progn
      (setq *ansi-alias-left* (list nil))
      (setq *ansi-alias-right* *ansi-alias-left*)
      (setf (cdr *ansi-alias-right*) (list 'a))
      (list (eq *ansi-alias-left* *ansi-alias-right*)
            *ansi-alias-left*
            *ansi-alias-right*)))

(is equal
    '(t t (nil entry))
    (let* ((key (list 'key))
           (value (list 'value))
           (cell (list nil))
           (cell-key 'cell)
           (table (make-hash-table :test 'eq)))
      (setf (gethash key table) value)
      (setf (gethash cell-key table) cell)
      (setf (cdr cell) (list 'entry))
      (list (eq value (gethash key table))
            (null (gethash (list 'key) table))
            (gethash 'cell table))))

(defparameter *ansi-copy-init-count* 0)

(defstruct ansi-copy-probe
  (value (incf *ansi-copy-init-count*)))

(is eql
    1
    (let ((object (make-ansi-copy-probe)))
      (copy-ansi-copy-probe object)
      *ansi-copy-init-count*))

(is equal
    '((1))
    (let ((stack nil))
      (multiple-value-list (push (values 1 2) stack))))

(is equal
    '(1 ((2)))
    (let ((place (list (list 1 2))))
      (list (pop (car place)) place)))

(is equal
    '(1 (2))
    (let ((vector (vector '(1 2))))
      (list (pop (aref vector 0)) (aref vector 0))))

(is equal
    '(42)
    (multiple-value-list
     (catch (values 'tag 'ignored)
       42)))

(is eql
    42
    (catch 'ansi-probe
      (ignore-errors (throw 'ansi-probe 42))))

(defun ansi-recursive-go-probe (number escape)
  (block done
    (tagbody
     start
       (if escape
           (funcall escape)
           (let ((jump (lambda () (go target))))
             (return-from done (ansi-recursive-go-probe 0 jump))))
     target
       (return-from done number))))

(is eql
    1
    (ansi-recursive-go-probe 1 nil))

(defpackage ansi-use-a (:use))
(defpackage ansi-use-b (:use))
(defpackage ansi-package-probe
  (:use ansi-use-a)
  (:use ansi-use-b)
  (:nicknames ansi-package-a)
  (:nicknames ansi-package-b)
  (:shadow "CAR"))

(is equal
    '(2 "ANSI-PACKAGE-PROBE" "ANSI-PACKAGE-PROBE" "ANSI-PACKAGE-PROBE")
    (let ((shadow (find-symbol "CAR" "ANSI-PACKAGE-PROBE")))
      (list (length (package-use-list "ANSI-PACKAGE-PROBE"))
            (package-name (find-package "ANSI-PACKAGE-A"))
            (package-name (find-package "ANSI-PACKAGE-B"))
            (package-name (symbol-package shadow)))))

(is equal
    '(t t "/" "C:/foo/")
    (list (pathnamep (car '(#P"foo.txt")))
          (pathnamep (aref '#(#P"foo.txt") 0))
          (namestring (pathname "/"))
          (namestring (pathname "C:/foo/"))))
