;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987, 1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG



;;  AUTHOR:  Juergen Walther

(defvar *bab-menu-font* '("Monaco" 9))

(def$flavor mac-menu-mixin
  () ()
  (:documentation "This is the menu mixin for the macintosh."))

(defvar *babylon-menu-position* (make-point (- 500 (* 6 *item-width*)) 38))

;(defobfun (set-window-position *dialog*) (h &optional v)
;  (usual-set-window-position h v)
;  (setf *babylon-menu-position* (make-point h v)))


#|
; these are the possible types of items in an item list for :choose-from-menu

(send-kb :choose-from-menu
         '("string"
           symbol
           number
           (name . symbol)
           ("namstring" . sss)
           (list (some more complicated lists))
           ("Hallo" :no-select t)
           ("Value" :value *recompile*)
           ("Funcall" :funcall ed-beep)
           ("Eval" :eval (format t "~%Dies ist der Effect der Eval Form")))
         )
we use select-item-from-list with a special table-print-function and operate
on the selection to return results

; multiple choose menu items are built like (Symbol String (t))
; string is shown and a list of symbols of selected Strings is returned

(send-kb :mult-choose-from-menu 
         '((eins "Eins" (t))
           (zwei "Zwei" (t)))
         "Waehle einige Eintraege aus")

|#

(defun single-selection-print (item stream)
  (cond ((atom item) (princ item stream))
        (t (princ (first item) stream))))

(defun select-single (item-list &rest args)
  (first (select-item-from-list item-list 
                                :window-title (if args (car args) "Select One of")
                                :table-print-function #'single-selection-print
                                :selection-type :single)))
        
(def$method (mac-menu-mixin :choose-from-menu) (item-list &rest args)
  "Choose one item from a list of items structured according to Lispm conventions.
Map these items to a pair, whose car is the string or symbol to be displayed,
and whose cdr is a function, to be funcalled with no arguments, or a form,
to be evaluated."
  (let ((item (apply #'select-single item-list args)))
    (cond ((atom item) item)
          ((atom (cdr item)) (cdr item))
          ((= (length item) 2) (second item))
          (t (case (second item)
               (:no-select nil)
               (:value (third item))
               (:eval  (eval (third item)))
               (:funcall (funcall (third item))))))))

(defun multiple-selection-print (item stream)
  (princ (second item) stream))

(def$method (mac-menu-mixin :mult-choose-from-menu) (item-list &rest args)
  (mapcar #'first 
          (select-item-from-list item-list 
                                 :window-title (if args (car args) "Select Any of")
                                 :table-print-function #'multiple-selection-print
                                 :selection-type :disjoint)))
        

;;; eof

