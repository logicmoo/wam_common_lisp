;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMMON-LISP")


(export 'time)
(export '(decode-universal-time encode-universal-time))


(in-package "SYSTEM")


(proclaim '(optimize (safety 2) (space 3)))


(defmacro time (form)
  "Syntax: (time form)
Evaluates FORM, outputs the realtime and runtime used for the evaluation to
*TRACE-OUTPUT*, and then returns all values of FORM."
  (let*((real-start (gentemp))
	(real-end (gentemp))
	(run-start (gentemp))
	(run-end (gentemp))
	(gc-start (gentemp))
	(gc-end (gentemp))
	(x (gentemp)))
    `(let ((,real-start (get-internal-real-time))
	   (,run-start (get-internal-run-time))
	   (,gc-start (sys:gc-time))
	   ,real-end ,run-end ,gc-end ,x)
      (setq ,x (multiple-value-list ,form))
      (setq ,run-end (get-internal-run-time))
      (setq ,real-end (get-internal-real-time))
      (setq ,gc-end (sys:gc-time))
      (fresh-line *trace-output*)
      (format *trace-output*
             "real time : ~,3F secs~%~
              run time  : ~,3F secs~%~
              GC time   : ~,3F secs~%"
       (/ (- ,real-end ,real-start) internal-time-units-per-second)
       (/ (- ,run-end ,run-start) internal-time-units-per-second)
       (/ (- ,gc-end ,gc-start) internal-time-units-per-second))
      (values-list ,x))))


(defconstant seconds-per-day #.(* 24 3600))

(defun leap-year-p (y)
  (and (zerop (mod y 4))
       (or (not (zerop (mod y 100))) (zerop (mod y 400)))))

(defun number-of-days-from-1900 (y)
  (let ((y1 (1- y)))
    (+ (* (- y 1900) 365)
       (floor y1 4) (- (floor y1 100)) (floor y1 400)
       -460)))

(defconstant month-startdays #(0 31 59 90 120 151 181 212 243 273 304 334 365))

(defun decode-universal-time (ut &optional (tz (sys::get-local-time-zone)))
  (let (sec min hour day month year dow days
	    (dstp (sys::daylight-saving-time-p)))
    (decf ut (round (* tz 3600)))
    (multiple-value-setq (ut sec) (floor ut 60))
    (multiple-value-setq (ut min) (floor ut 60))
    (multiple-value-setq (days hour) (floor ut 24))
    (setq dow (mod days 7))
    (setq year (+ 1900 (floor days 366))) ; Guess!
    (do ((x))
        ((< (setq x (- days (number-of-days-from-1900 year)))
            (if (leap-year-p year) 366 365))
         (setq day (1+ x)))
      (incf year))
    (when (leap-year-p year)
      (when (= day 60)
	(return-from decode-universal-time
	  (values sec min hour 29 2 year dow dstp tz)))
      (when (> day 60) (decf day)))
    (setq month (position day month-startdays :test #'<)
	  day (- day (svref month-startdays (1- month))))
    (values sec min hour day month year dow dstp tz)))

(defun encode-universal-time (sec min h d m y
                              &optional (tz (sys::get-local-time-zone)))
  (incf h tz)
  (when (<= 0 y 99)
    (multiple-value-bind (sec min h d m y1 dow dstp tz)
	(get-decoded-time)
      (declare (ignore sec min h d m dow dstp tz))
      (incf y (- y1 (mod y1 100)))
      (cond ((< (- y y1) -50) (incf y 100))
	    ((>= (- y y1) 50) (decf y 100)))))
  (when (and (leap-year-p y) (> m 2)) (incf d))
  (+ (* (+ (number-of-days-from-1900 y) (svref month-startdays m) d)
        seconds-per-day)
     (* h 3600) (* min 60) sec))
