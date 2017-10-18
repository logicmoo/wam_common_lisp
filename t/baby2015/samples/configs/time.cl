;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: TIME; Base: 10  -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  ZetaLisp Time Package functions

(in-package "TIME")

(babylon::bab-provide 'time)

(defun time-difference (x y)
  (- x y))

#|

; the magic number is 68400

(defun test ()
  (let ((time (get-universal-time)))
    (print time)
    (multiple-value-bind 
      (second minute hour date month year)
      (decode-universal-time time)
      (format t "~%decoded: ~S ~S ~S ~S ~S ~S" 
              second minute hour date month year)
      (let ((time1 (encode-universal-time second minute hour date month year)))
        (multiple-value-bind 
          (second1 minute1 hour1 date1 month1 year1)
          (decode-universal-time time1)
          (format t "~%encoded: ~S ~S ~S ~S ~S ~S" 
                  second1 minute1 hour1 date1 month1 year1)
          (- time time1))))))
? (test)

2779501776 
decoded: 36 49 3 29 1 1988
encoded: 36 49 8 28 1 1988
68400
? 

|#

(defun print-universal-time (universal-time stream)
  (multiple-value-bind
    (second minute hour date month year day-of-week d-s-t-p time-zone)
    (decode-universal-time universal-time)     ;;; (+ 68400 universal-time))
    (declare (ignore DAY-OF-WEEK D-S-T-P TIME-ZONE))        
    (format stream 
            "~s.~s.~s, ~s:~s:~s" 
            date month year hour minute second)))

(defun print-current-time (stream)
  (multiple-value-bind
    (second minute hour date month year day-of-week d-s-t-p time-zone)
    (decode-universal-time (get-universal-time))  ;;; (+ 68400 (get-universal-time)))
    (declare (ignore DAY-OF-WEEK D-S-T-P TIME-ZONE))
    (format stream 
            "~s.~s.~s, ~s:~s:~s" 
            date month year hour minute second)))

(defun parse-universal-time (time-string)
  (let ((str-lngth (length time-string))
        (hour 0) (minute 0) (second 0) 
        YEAR MONTH DATE START)
    (multiple-value-setq (date start) 
      (parse-integer time-string :start 0 :junk-allowed t))
    (multiple-value-setq (month start) 
      (parse-integer time-string :start (1+ start) :junk-allowed t))
    (multiple-value-setq (year start) 
      (parse-integer time-string :start (1+ start) :junk-allowed t))
    (if (< year 100)
      (progn (setq year (+ 1900 year))))
    (if (< start str-lngth)
      (progn 
        (multiple-value-setq (hour start) 
          (parse-integer time-string :start (1+ start) :junk-allowed t))
        (if (< start str-lngth)
          (progn
            (multiple-value-setq (minute start)
              (parse-integer time-string :start (1+ start) :junk-allowed t))
            (if (< start str-lngth)
              (multiple-value-setq (second start) 
                (parse-integer time-string :start (1+ start) 
                               :junk-allowed t)))))))
    (encode-universal-time second minute hour date month year)))

;;; eof



