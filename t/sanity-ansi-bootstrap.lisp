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

(defparameter *ansi-eval-when-probe* nil)

(eval-when (:load-toplevel :execute)
  (setq *ansi-eval-when-probe* 1)
  (setq *ansi-eval-when-probe*
        (+ *ansi-eval-when-probe* 1)))

(is eql
    2
    *ansi-eval-when-probe*)

(is equal
    '(t t "condition" (1 2))
    (let ((condition
           (make-condition 'simple-error
                           :format-control "condition"
                           :format-arguments '(1 2))))
      (list (typep condition 'simple-error)
            (typep condition 'condition)
            (simple-condition-format-control condition)
            (simple-condition-format-arguments condition))))

(is equal
    '(:inner :outer :caught)
    (let ((events nil))
      (handler-case
          (handler-bind
              ((error (lambda (condition)
                        (declare (ignore condition))
                        (setq events (append events '(:outer))))))
            (handler-bind
                ((simple-error
                  (lambda (condition)
                    (declare (ignore condition))
                    (setq events (append events '(:inner))))))
              (error "nested handlers")))
        (error ()
          (setq events (append events '(:caught)))))
      events))

(is eql
    42
    (restart-case
        (invoke-restart 'use-value 42)
      (use-value (value) value)))

(is equal
    '(:warned :continued)
    (let ((events nil))
      (handler-bind
          ((warning
            (lambda (condition)
              (setq events (append events '(:warned)))
              (muffle-warning condition))))
        (warn "warning")
        (setq events (append events '(:continued))))
      events))

(is equal
    '(1 :cleaned)
    (let ((cleanup nil))
      (list (unwind-protect 1 (setq cleanup :cleaned))
            cleanup)))

(is equal
    '(:handled :cleaned)
    (let ((cleanup nil))
      (list (handler-case
                (unwind-protect
                     (error "cleanup")
                  (setq cleanup :cleaned))
              (error () :handled))
            cleanup)))

(is equal
    '(a b)
    (read-from-string "(a b) trailing"))

(is equal
    '(#\( #\))
    (list #\( #\)))

(is eql
    12
    (ash 3 2))

(is equal
    '(t t nil)
    (let* ((string (make-string 5 :initial-element #\x))
           (copy nil))
      (setf (subseq string 1 4) "abc")
      (setq copy (copy-seq string))
      (setf (elt string 4) #\y)
      (list (string= "xabcy" string)
            (string= "xabcx" copy)
            (eq string copy))))

(is equal
    '(nil)
    (multiple-value-list
     (ignore-errors
       (eql 'wamcl-reg.2 (make-array 1)))))

(is equal
    '(1 list t)
    (let ((condition
           (make-condition 'type-error
                           :datum 1
                           :expected-type 'list)))
      (list (type-error-datum condition)
            (type-error-expected-type condition)
            (typep condition 'type-error))))

(is equal
    '(t t)
    (let ((function (compile nil '(lambda (value) value))))
      (list (functionp function)
            (typep function 'function))))

(defparameter *ansi-single-eval-when-probe* 0)

(eval-when (:execute)
  (setq *ansi-single-eval-when-probe* 7))

(is eql
    7
    *ansi-single-eval-when-probe*)

(is equal
    '(2 3 4)
    (mapcar #'(lambda (value) (+ value 1)) '(1 2 3)))

(is eql
    42
    (block result
      (handler-case
          (return-from result 42)
        (error () 99))))

(is eql
    :outer
    (handler-case
        (handler-case
            1
          (:no-error (value)
            (declare (ignore value))
            (error "no-error clause")))
      (error () :outer)))

(is eql
    :cleaned
    (let ((cleanup nil))
      (or (unwind-protect
              (prolog-inline "fail")
            (setq cleanup :cleaned))
          cleanup)))

(is equal
    '(:eof 0)
    (multiple-value-list
     (read-from-string "" nil :eof)))

(is equal
    t
    (string= "n" "\n"))

(is equal
    '(#\a #\b #\c)
    (concatenate 'list "ab" '(#\c)))

(is equal
    t
    (string= "abc"
             (concatenate 'string '(#\a) "bc")))

(is equal
    '(1 3 2)
    (remove-duplicates '(1 2 1 3 2)))

(is equal
    '(1 2 3)
    (remove-duplicates '(1 2 1 3 2) :from-end t))

(is equal
    '(t t)
    (let ((object (list 'identity)))
      (list (eq object (funcall #'(lambda () object)))
            (eq object (funcall #'(lambda (value) value) object)))))

(is equal
    '(:case)
    (let ((events nil))
      (handler-bind
          ((error
            (lambda (condition)
              (declare (ignore condition))
              (setq events (append events '(:outer))))))
        (handler-case
            (error "inner handler-case")
          (error ()
            (setq events (append events '(:case))))))
      events))

(defparameter *ansi-compile-only-eval-when-probe* 0)

(eval-when (:compile-toplevel)
  (setq *ansi-compile-only-eval-when-probe* 1)
  (setq *ansi-compile-only-eval-when-probe* 2))

(is eql
    0
    *ansi-compile-only-eval-when-probe*)

(is eql
    :reader-error
    (handler-case
        (read-from-string ")" nil :eof)
      (error () :reader-error)))

(defparameter *ansi-shared-closure-object* (list 1))

(is equal
    '(t (2))
    (let ((result
           (funcall #'(lambda () *ansi-shared-closure-object*))))
      (setf (car result) 2)
      (list (eq result *ansi-shared-closure-object*)
            *ansi-shared-closure-object*)))

(is equal
    '(2 2)
    (list (find-if #'identity '(nil 2))
          (position-if #'identity '(nil nil 3))))

(is eql
    :outer
    (handler-case
        (handler-case
            (error
             (make-condition 'type-error
                             :datum 1
                             :expected-type 'string))
          (simple-error () :inner))
      (type-error () :outer)))

(is eql
    9
    (restart-case
        (invoke-restart (car (compute-restarts)) 9)
      (use-value (value) value)))

(is equal
    '(abc 3)
    (multiple-value-list
     (read-from-string "abc" t nil :end nil)))

(is equal
    t
    (let ((string (make-string 3 :initial-element #\x)))
      (setf (subseq string 1) "abcd")
      (string= "xab" string)))
