;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

 

;;  AUTHOR:  Juergen Walther


;;; extensions to Coral Common Lisp

(defun ignore (&rest args) args)

(defun string-append (&rest strings)		; used only in mac:interface
  (coerce (apply #'concatenate 'vector strings) 'string))

#|
(defun string-length (string)                   ; in read-expr-from-window in bc-mixin
  (length string))
|#

(defun get-prompt-string ()
  "? ")

;;-----------------------------------------------------------------
;;              system dependencies
;;-----------------------------------------------------------------

(defmacro rest2 (liste)
  `(cdr (cdr ,liste)))

(defmacro rest3 (liste)
  `(cdr (cdr (cdr ,liste))))

(defmacro baberror (fstr &rest args)
 `(error ,fstr ,@args))

(defun babpprint (object &optional stream)
  (pprint object stream))

;;; eof

