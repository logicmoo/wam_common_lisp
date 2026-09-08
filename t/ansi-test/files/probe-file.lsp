;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Mon Jan  5 20:46:29 2004
;;;; Contains: Tests of PROBE-FILE

(deftest probe-file.1
  (probe-file #p"nonexistent")
  nil)

(deftest probe-file.2
  (let ((s (open #p"probe-file.txt" :direction :input)))
    (prog1
        (equalpt (truename #p"probe-file.txt")
                 (probe-file s))
      (close s)))
  t)

(deftest probe-file.3
  (let ((s (open #p"probe-file.txt" :direction :input)))
    (close s)
    (equalpt (truename #p"probe-file.txt")
             (probe-file s)))
  t)

(deftest probe-file.4
  (equalpt (truename #p"probe-file.txt")
           (probe-file "CLTEST:probe-file.txt"))
  t)

;;; Specialized string tests

(deftest probe-file.5
  (do-special-strings
   (str "probe-file.txt" nil)
   (let ((s (open str :direction :input)))
     (assert (equalpt (truename #p"probe-file.txt") (probe-file s)))
     (close s)))
  nil)

;;; Error tests

(deftest probe-file.error.1
  (signals-error (probe-file) program-error)
  t)

(deftest probe-file.error.2
  (signals-error (probe-file #p"probe-file.txt" nil) program-error)
  t)

(deftest probe-file.error.3
  (signals-error-always (probe-file (make-pathname :name :wild)) file-error)
  t t)

(deftest probe-file.error.4
  (signals-error-always (probe-file "CLTEST:*.FOO") file-error)
  t t)
