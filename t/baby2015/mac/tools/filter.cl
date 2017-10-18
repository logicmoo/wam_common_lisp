;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: User; Base: 10  -*-

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;  BABYLON  pattern matching and filtering


(defun char-sequence-match (p d)
  "character sequence pattern matcher.
Use ? to match one single character,
or * to match any sequence of (at least one) characters."
  (cond ((and (null p) (null d)) t)
        ((or (null p) (null d)) nil)
        ((or (char= (car p) #\?)
             (char= (car p) (car d)))
         (char-sequence-match (cdr p) (cdr d)))
        ((char= (car p) #\*)
         (or (char-sequence-match (cdr p) (cdr d))
             (char-sequence-match p (cdr d))))))

(defun filter-list (list &optional (pattern "*"))
  "Filter a list using a string pattern."
  (if (or (string= pattern "")
          (string= pattern "*"))  
    (copy-list list)
    (let ((pattern-sequence (coerce pattern 'list)))
      (remove-if-not #'(lambda (element)
                         (if (char-sequence-match 
                              pattern-sequence
                              (coerce (string element) 'list)) 
                           t 
                           nil))
                     list))))

#|

(filter-list '(HANS HARBIG OTTO EMIL) "*I*")
(filter-list '(HANS HARBIG OTTO EMIL) "HA*")
(filter-list '(HANS HARBIG OTTO EMIL) "H?R*")
(filter-list '(HANS HARBIG OTTO EMIL) "*H?R*")
(filter-list '(HANS HARBIG OTTO EMIL) "*")
(filter-list '(HANS HARBIG OTTO EMIL) "")
(filter-list '(HANS HARBIG OTTO EMIL))
(filter-list '(HANS HARBIG OTTO EMIL) "*TT*")


|#

