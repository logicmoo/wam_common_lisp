;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1989    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG



;;  AUTHOR:  E. Gross
;;  DATE:    January 1989

;;-----------------------------------------------------------------
;;              system dependencies
;;-----------------------------------------------------------------

(defmacro baberror (fstr &rest args)
 `(error ,fstr ,@args))


(defun babpprint (object &optional stream)
  (pprint object stream))

;;-----------------------------------------------------------------

; (defun ignore (x) x)      ;;; use (declare (ignore ...))

(defun get-prompt-string ()
  "> ")

(defun load-user-babylon-init-file ()
  (load (merge-pathnames "bab-init.cl" (user-homedir-pathname))
	:verbose nil :if-does-not-exist nil))

(defun get-babylon-default-pathname (&optional (type 'system))
  (cond ((eq type 'system)
	 (transform-pathstring "babhome/samples>foo" 'source))
	((eq type 'user)
	 (merge-pathnames "foo.cl" (user-homedir-pathname)))))


(defun merge-babylon-pathnames (name default)
  (merge-pathnames name default))


;;-----------------------------------------------------------------

(defun get-stream-for-dialog ()
  *default-dialog-stream*)

;;; eof

