                                        ;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sat Nov 29 05:06:57 2003
;;;; Contains: Tests of the function PATHNAME


(deftest pathname.1
    (loop for x in *pathnames*
       always (eq x (pathname x)))
  t)

(deftest pathname.2
    (equalt #p"pathname.txt" (pathname "pathname.txt"))
  t)

(deftest pathname.3
    (let ((s (open "pathname.txt" :direction :input)))
      (prog1 (equalt (truename (pathname s))
                     (truename #p"pathname.txt"))
        (close s)))
  t)

(deftest pathname.4
    (let ((s (open "pathname.txt" :direction :input)))
      (close s)
      (equalt (truename (pathname s))
              (truename #p"pathname.txt")))
  t)

(deftest pathname.5
    (loop for x in *logical-pathnames*
       always (eq x (pathname x)))
  t)

(deftest pathname.6
    (equalt #p"pathname.txt"
            (pathname
             (make-array 12
                         :initial-contents "pathname.txt"
                         :element-type 'base-char)))
  t)

(deftest pathname.7
    (equalt #p"pathname.txt"
            (pathname (make-array 15
                                  :initial-contents "pathname.txtXXX"
                                  :element-type 'base-char
                                  :fill-pointer 12)))
  t)

(deftest pathname.8
    (equalt #p"pathname.txt"
            (pathname (make-array 12
                                  :initial-contents "pathname.txt"
                                  :element-type 'base-char
                                  :adjustable t)))
  t)

(deftest pathname.9
    (equalt #p"pathname.txt"
            (pathname (make-array 15
                                  :initial-contents "pathname.txtXXX"
                                  :element-type 'character
                                  :fill-pointer 12)))
  t)

(deftest pathname.10
    (equalt #p"pathname.txt"
            (pathname (make-array 12
                                  :initial-contents "pathname.txt"
                                  :element-type 'character
                                  :adjustable t)))
  t)

(deftest pathname.11
    (loop for etype in '(standard-char base-char character)
       collect
         (equalt #p"pathname.txt"
                 (pathname
                  (let* ((s (make-array 15
                                        :initial-contents
                                        "XXpathname.txtX"
                                        :element-type etype)))
                    (make-array 12
                                :element-type etype
                                :displaced-to s
                                :displaced-index-offset 2)))))
  (t t t))

;;; Error tests

(deftest pathname.error.1
  (signals-error (pathname) program-error)
  t)

(deftest pathname.error.2
  (signals-error (pathname (first *pathnames*) nil) program-error)
  t)
