;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Thu Jan 29 22:06:28 2004
;;;; Contains: Tests of BROADCAST-STREAM-STREAMS

(in-package :cl-test)

(deftest broadcast-stream-streams.1
  (broadcast-stream-streams (make-broadcast-stream))
  nil)

(deftest broadcast-stream-streams.2
  (equalt
   (broadcast-stream-streams (make-broadcast-stream *standard-output*))
   (list *standard-output*))
  t)

;;; Ensure that the last component is taken for
;;; file-{position,length,string-length} functions.
(deftest broadcast-stream-streams.3
    (let ((first-stream (make-string-output-stream)))
      (with-open-file (last-stream "bss-last.txt"
                                   :direction :output
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
        (format last-stream "Hello world!~%")
        (finish-output last-stream)     ; for buffered streams
        (let ((broadcast (make-broadcast-stream first-stream last-stream)))
          (values
           (= 13 (file-length last-stream))
           (= 13 (file-length broadcast) (file-length last-stream))
           (= 13 (file-position broadcast) (file-position last-stream))
           (= 2
              (file-string-length broadcast "jd")
              (file-string-length last-stream "jd"))))))
  t t t t)

(deftest broadcast-stream-streams.4
    (let ((stream (make-broadcast-stream)))
      (values (= 0 (file-length stream))
              (= 0 (file-position stream))
              (= 1 (file-string-length stream "foo"))
              (eq :default (stream-external-format stream))))
  t t t t)

(deftest broadcast-stream-streams.error.1
  (signals-error (broadcast-stream-streams) program-error)
  t)

(deftest broadcast-stream-streams.error.2
  (signals-error (broadcast-stream-streams (make-broadcast-stream) nil)
                 program-error)
  t)
