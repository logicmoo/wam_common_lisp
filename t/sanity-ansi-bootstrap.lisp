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

(is equal
    '(1 3)
    (let ((sum 0))
      (loop for weight in '(1 2)
            collect (incf sum weight))))

(is equal
    '(t nil t)
    (list (every #'plusp '(1 2))
          (every #'plusp '(1 0))
          (some #'plusp '(0 2))))

(is equal
    '(t nil)
    (list (every #'< '(1 2) '(2 3))
          (some #'> '(1 2) '(2 3))))

(is equal '(0 1 2 2 3 0 1 2 2 3 101)
    (mapcar #'integer-length
            (list 0 1 2 3 4 -1 -2 -3 -4 -5 (ash 1 100))))

(is equal '(1.5 integer)
    (handler-case (integer-length 1.5)
      (type-error (condition)
        (list (type-error-datum condition)
              (type-error-expected-type condition)))))

(is equal '(t nil)
    (list (random-state-p (make-random-state t))
          (random-state-p nil)))

(is equal '(nil t)
    (let* ((state (make-random-state))
           (copy (make-random-state state)))
      (list (eq state copy)
            (equal (loop repeat 5 collect (random 100000 state))
                   (loop repeat 5 collect (random 100000 copy))))))

(is equal t
    (let* ((*random-state* (make-random-state))
           (copy (make-random-state nil)))
      (make-random-state t)
      (equal (loop repeat 5 collect (random 100000))
             (loop repeat 5 collect (random 100000 copy)))))

(is equal t
    (let* ((state (make-random-state))
           (alias state)
           (copy (make-random-state state)))
      (random 100000 state)
      (random 100000 copy)
      (= (random 100000 alias) (random 100000 copy))))

(is equal '(t t t)
    (list (= 0 (random 1))
          (let ((value (random (ash 1 100))))
            (and (integerp value) (<= 0 value) (if (< value (ash 1 100)) t nil)))
          (let ((value (random 2.5)))
            (and (eq 'single-float (type-of value))
                 (<= 0 value) (if (< value 2.5) t nil)))))

(is equal '(double-float short-float long-float)
    (list (type-of (random 2.0d0))
          (type-of (random 2.0s0))
          (type-of (random 2.0l0))))

(is equal '(t t t t t t)
    (mapcar #'(lambda (limit)
                (handler-case (progn (random limit) nil)
                  (type-error () t)))
            '(0 -1 0.0 -1.0 1/2 bad-limit)))

(is equal '(t t t t)
    (list (handler-case (random 3 nil) (type-error () t))
          (handler-case (make-random-state 7) (type-error () t))
          (handler-case (random 3 nil nil) (program-error () t))
          (handler-case (make-random-state nil nil) (program-error () t))))

(is equal t
    (let* ((state (make-random-state))
           (copy (make-random-state state)))
      (handler-case (random 0 state) (type-error () nil))
      (= (random 100000 state) (random 100000 copy))))

(is equal '(t nil t nil :found)
    (list (every #'identity nil)
          (some #'identity nil)
          (every #'< '(1 2 999) #(2 3))
          (some #'> #(1 2) '(2 3 0))
          (some #'identity '(nil :found :later))))

(is equal '(nil 2)
    (let ((calls 0))
      (list (every #'(lambda (value) (incf calls) (< value 2)) #(1 2 3))
            calls)))

(is equal '(:found 2)
    (let ((calls 0))
      (list (some #'(lambda (value) (incf calls) value) '(nil :found :later))
            calls)))

(is equal t
    (every #'characterp "abc"))

(is equal '(t t t t)
    (list (handler-case (every #'identity) (program-error () t))
          (handler-case (some #'identity) (program-error () t))
          (handler-case (every #'identity 42) (type-error () t))
          (handler-case (some #'identity '(1 . 2)) (type-error () t))))

(is equal '(t t t t)
    (list (string= "abc" (coerce '(#\a #\b #\c) 'string))
          (string= "abc" (coerce "abc" 'simple-base-string))
          (string= "abc" (coerce #(#\a #\b #\c) 'simple-string))
          (string= "" (coerce nil 'base-string))))

(is equal '(#\a #\b #\c) (coerce "abc" 'list))
(is equal '(1 2 3) (coerce (coerce '(1 2 3) 'vector) 'list))
(is equal '(1 2) (coerce (coerce '(1 2) 'simple-vector) 'list))

(is equal '(t nil)
    (let* ((element (list 'shared))
           (source (list element))
           (vector (coerce source 'vector))
           (copy (coerce vector 'list)))
      (list (eq element (car copy)) (eq source copy))))

(is equal '(1.0 0.5 short-float double-float long-float)
    (list (coerce 1 'float)
          (coerce 1/2 'single-float)
          (type-of (coerce 1 'short-float))
          (type-of (coerce 1 'double-float))
          (type-of (coerce 1 'long-float))))

(is equal '(short-float double-float long-float)
    (list (type-of (coerce 1.0s0 'float))
          (type-of (coerce 1.0d0 'float))
          (type-of (coerce 1.0l0 'float))))

(is equal '(#\a #\b #\C)
    (list (coerce #\a 'character)
          (coerce "b" 'character)
          (coerce 'c 'character)))

(is equal '(7 8 9)
    (list (funcall (coerce #'identity 'function) 7)
          (funcall (coerce 'identity 'function) 8)
          (funcall (coerce '(lambda (x) x) 'function) 9)))

(is equal '(t t t t t t)
    (list (handler-case (coerce '(1 2) 'string) (type-error () t))
          (handler-case (coerce 1 'list) (type-error () t))
          (handler-case (coerce 65 'character) (type-error () t))
          (handler-case (coerce "ab" 'character) (type-error () t))
          (handler-case (coerce 'abc 'float) (type-error () t))
          (handler-case (coerce 1 'function) (type-error () t))))

(is equal '(3 1)
    (let ((value 1) (calls 0))
      (list (incf value (progn (incf calls) 2)) calls)))

(defparameter *ansi-coerced-string* (coerce '(#\a #\b #\c) 'simple-base-string))

(is equal '(3 t)
    (list (length *ansi-coerced-string*)
          (string= "cba" (reverse *ansi-coerced-string*))))

(is equal '(t (3 2 1) (3 2 1))
    (list (string= "cba" (reverse (coerce '(#\a #\b #\c) 'string)))
          (coerce (reverse (coerce '(1 2 3) 'vector)) 'list)
          (reverse '(1 2 3))))

(is equal 3 (length #.(coerce "abc" 'simple-base-string)))

(is equal t
    (progn
      (prolog-inline "getrand(S),f_make_random_state([],A),f_make_random_state([A],B),f_random(1000,[A],X),f_random(1000,[B],X),getrand(S),catch(f_random(0,[A],_),error(type_error(_,_),_),true),getrand(S),f_random(1000,[A],Y),f_random(1000,[B],Y)")
      t))

(is equal '(nil 1)
    (let ((calls 0))
      (list (assert (progn (incf calls) t)) calls)))

(is equal t
    (handler-case (assert nil)
      (simple-error () t)))

(is equal 42
    (handler-case (assert nil () 'type-error :datum 42 :expected-type 'string)
      (type-error (condition) (type-error-datum condition))))

(is equal '(nil 0)
    (let ((calls 0))
      (list (assert t () (progn (incf calls) "unused")) calls)))

(is equal '(nil t)
    (let ((ready nil))
      (list (handler-bind ((simple-error #'(lambda (condition)
                                             (declare (ignore condition))
                                             (setq ready t)
                                             (invoke-restart 'continue))))
              (assert ready))
            ready)))

(defmacro ansi-bootstrap-body (&body forms)
  `(progn ,@forms))

(is equal '(progn 1 2)
    (macroexpand-1 '(ansi-bootstrap-body 1 2)))

(is equal '(2 3)
    (list (ansi-bootstrap-body 1 2)
          (ansi-bootstrap-body 3)))

(is equal '((a b) (a) nil nil (a) (a b . c))
    (list (butlast '(a b c))
          (butlast '(a b c) 2)
          (butlast '(a b) 9)
          (butlast nil)
          (butlast '(a b . c))
          (butlast '(a b . c) 0)))

(is equal '(nil t)
    (let* ((element (list 'shared))
           (source (list element 'last))
           (copy (butlast source)))
      (list (eq source copy) (eq element (car copy)))))

(is equal '(t t t)
    (list (handler-case (butlast '(a) -1) (type-error () t))
          (handler-case (butlast 'atom) (type-error () t))
          (handler-case (butlast '(a) 1 2) (program-error () t))))

(is equal '(nil t t nil)
    (list (typep 3 'ratio) (typep 1/2 'ratio)
          (typep 3 'integer) (typep 1/2 'integer)))

(is equal '(t) (multiple-value-list (typep 3 'integer)))

;; Compiling a nested DEFMACRO must not execute it.
(if nil (defmacro ansi-never-if () 33) nil)
(is equal nil (macro-function 'ansi-never-if))
(is equal '(ansi-never-if) (macroexpand-1 '(ansi-never-if)))

(defun ansi-install-runtime-macro ()
  (defmacro ansi-runtime-macro () 34))
(is equal nil (macro-function 'ansi-runtime-macro))
(ansi-install-runtime-macro)
(is equal 34 (ansi-runtime-macro))

(if t (defmacro ansi-executed-if () 35) nil)
(is equal 35 (ansi-executed-if))

(let () (if nil (defmacro ansi-never-let () 36) nil))
(is equal nil (macro-function 'ansi-never-let))

(progn
  (defmacro ansi-top-progn () 37)
  (is equal 37 (ansi-top-progn)))

(defmacro ansi-top-definition ()
  '(defmacro ansi-expanded-top () 38))
(progn
  (ansi-top-definition)
  (is equal 38 (ansi-expanded-top)))

(eval-when ()
  (defmacro ansi-never-eval-when () 39))
(is equal nil (macro-function 'ansi-never-eval-when))

(eval-when (:execute)
  (defmacro ansi-top-eval-when () 40)
  (is equal 40 (ansi-top-eval-when)))

;; COERCE-created closures receive values, not expressions to evaluate again.
(is equal '(quote abc)
    (funcall (coerce '(lambda (x) x) 'function) '(quote abc)))
(is equal 'ansi-unbound-data
    (funcall (coerce '(lambda (x) x) 'function) 'ansi-unbound-data))
(is equal '(t t t 7 8)
    (let* ((fn (coerce '(lambda (x) x) 'function))
           (data (list 'quote 'abc)))
      (list (eq data (funcall fn data))
            (eq 'ansi-unbound-data (funcall fn 'ansi-unbound-data))
            (eq data (funcall fn data))
            (funcall fn 7)
            (funcall fn 8))))
(is equal '(changed)
    (let* ((fn (coerce '(lambda (x) x) 'function))
           (data (list 'original))
           (alias (funcall fn data)))
      (rplaca alias 'changed)
      data))

;; Predicate secondary values must not escape EVERY or SOME.
(is equal '(t)
    (multiple-value-list (every #'(lambda (x) (values 123 456)) '(t))))
(is equal '(123)
    (multiple-value-list (some #'(lambda (x) (values 123 456)) '(t))))
(is equal '(nil)
    (multiple-value-list (every #'(lambda (x) (values nil 456)) '(t t))))
(is equal '(nil)
    (multiple-value-list (some #'(lambda (x) (values nil 456)) '(t t))))
(is equal '(t)
    (multiple-value-list (every #'identity (values nil :stale))))
(is equal '(nil)
    (multiple-value-list (some #'identity (values nil :stale))))
(is equal '(t)
    (multiple-value-list
      (every #'(lambda (x) (if x (values t :extra) (values nil :extra)))
             '(t t))))
(is equal '(:found)
    (multiple-value-list
      (some #'(lambda (x) (if x (values :found :extra) (values nil :extra)))
            '(nil :found :later))))

;; Coercion to an already satisfied sequence type preserves mutable identity.
(is equal '(t (changed))
    (let* ((source (list 'a))
           (alias (coerce source 'list)))
      (rplaca alias 'changed)
      (list (eq source alias) source)))
(is equal '(t t t t t)
    (let ((source (make-string 2 :initial-element #\a)))
      (list (eq source (coerce source 'string))
            (eq source (coerce source 'simple-string))
            (eq source (coerce source 'base-string))
            (eq source (coerce source 'simple-base-string))
            (eq source (coerce source 'vector)))))
(is equal '(t t)
    (let* ((source (make-string 2 :initial-element #\a))
           (alias (coerce source 'string)))
      (setf (aref alias 0) #\b)
      (list (eq source alias) (string= source "ba"))))
(is equal '(t t (9 2))
    (let* ((source (coerce '(1 2) 'vector))
           (alias (coerce source 'simple-vector)))
      (setf (aref alias 0) 9)
      (list (eq source (coerce source 'vector))
            (eq source alias)
            (coerce source 'list))))
(is equal '(t t t)
    (list (handler-case (coerce '(a) '(vector t 1)) (type-error () t))
          (handler-case (coerce "a" '(string 1)) (type-error () t))
          (handler-case (coerce '(a) '(cons t t)) (type-error () t))))

(defparameter *ansi-saved-random-states* (list (make-random-state)))
(is equal t (random-state-p (car *ansi-saved-random-states*)))

(is equal '(nil t)
    (let ((element (list 'shared)))
      (let ((first (list element)) (second (list element)))
        (list (eq first second) (eq (car first) (car second))))))

(is equal t
    (every #'(lambda (value) (if (< value 5.0e-324) t nil))
           (loop repeat 10 collect (random 5.0e-324))))
