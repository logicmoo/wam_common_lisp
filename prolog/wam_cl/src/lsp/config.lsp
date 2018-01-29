;;
;; Configuration file for ECL
;;
(in-package "LISP")
;;
;; * Short and long site names
;;
;; Edit these with the name of your site:
;;
(defun short-site-name ()
  "Args: ()
Returns, as a string, the location of the machine on which ECL runs."
  "")

(defun long-site-name () 
  "Args: ()
Returns, as a string, the location of the machine on which ECL runs."
  "")

;;
;; * ECL version, architecture, etc
;;
(defun lisp-implementation-version ()
  "Args:()
Returns the version of your ECL as a string."
  "0.8")

(defun machine-type ()
  "Args: ()
Returns, as a string, the type of the machine on which ECL runs."
  "UNKNOWN")

;; obtained from uname(2) where available
(defun machine-instance ()
  "Args: ()
Returns, as a string, the identifier of the machine on which ECL runs. Obtained
from uname(2) where available."
  "x86_64")

(defun machine-version ()
  "Args: ()
Returns, as a string, the version of the machine on which ECL runs. Obtained from
uname(2) where available."
  "@MACHINE@")

(defun software-type ()
  "Args: ()
Returns, as a string, the type of the software under which ECL runs."
  "Linux")

(defun software-version ()
  "Args: ()
Returns, as a string, the version of the software under which ECL runs."
  "4.4.0-112-generic")

;;
;; * Set up some room
;;
#-boehm-gc
(progn
  (sys::allocate 'CONS 200)
  (sys::allocate 'STRING 40))

;;
;; * Set configuration pathnames. Notice the trailing slash!
;;   Otherwise it would not be a directory.
;;
(si::pathname-translations "SYS" '(("**;*.*" "/usr/local/lib/ecl/**/*.*")))
(si::pathname-translations "HOME" '(("**;*.*" "~/**/*.*")))
