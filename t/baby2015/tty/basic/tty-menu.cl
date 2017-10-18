;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     August 1987
;; AUTHOR:   E. Gross

;; This file depends on:  common>*>flavors
;;                        common>c-fns
;;                        mata>kb-tab-*

;; Contents: a menu handler.

;;--------------------------------------------------------------------
;;--------------------------------------------------------------------


(def$flavor tty-menu-mixin
       () ()
  :settable-instance-variables
  (:required-instance-variables dialog-stream language)
  (:documentation "a mixin providing methods to select items from a menu"))


;;-------------------------------------------------------------------------------
;;                       CHOOSE-FROM-MENU
;;-------------------------------------------------------------------------------


(def$method (tty-menu-mixin :choose-from-menu) (item-list &optional label &rest ignore)
  "presents a menu and executes the operation specified for the selected item.
each item from item-list specifies a row of the menu. an item is selected
by entering the number of the item, which is displayed if the item is selectable.
the most general form of item is (name operation argument . options). 
symbol or string describe what is displayed besides the item number,
operation might be :value, :funcall :eval or :no-select causing to return,
call or eval argument. :no-select makes the item non-selectable.
options are ignored. short form of item specifications are:
name, (name . value) or (name value) which correspond to
(name :value name) and (name :value value) respectively.
label which has to be a string is used as menu headline."
  (declare (ignore ignore))
  (declare (list item-list))
  (let ((*standard-input* dialog-stream)
        (*default-dialog-stream* dialog-stream))
    (terpri)
    (princ (getentry star-str babylon-io-table))
    (terpri)
    (princ "*        0 ")
    (princ (getentry no-select-str babylon-io-table))
    (format t "~70T") (princ "*") (terpri)    
    (princ "*  MENU:   ")
    (if label (princ label))
    (format t "~70T") (princ "*") (terpri)
    (show-menu-loop item-list 1)
    (princ (getentry star-str babylon-io-table))
    (terpri)
    (princ "  --->   ")
    (force-output dialog-stream)
    (let ((answer (read)))
      (cond ((and (numberp answer)
                  (> answer 0)
                  (< answer (1+ (length item-list))))
             (let ((item (nth (1- answer) item-list)))
               (if (and (consp item)
                        (consp (rest item))
                        (equal (second item) :no-select))
                 ($send self :choose-from-menu item-list label)
                 (execute-menu-action (nth (1- answer) item-list)))))
            ((equal answer 0) nil)
            (t ($send self :choose-from-menu item-list label))))))

(defun show-menu-loop (item-list nr)
  "recursive function which displays one row of the menu per call"

  (declare (list item-list) (fixnum nr))
  (cond ((null item-list))
        (t (let ((info (s-display-info (first item-list))))
             (princ "*        ")
             (if (rest info)
               (princ nr))
             (format t "~11T")(princ (first info))  
             (format t "~70T")(princ "*") (terpri)
             (show-menu-loop (rest item-list) (1+ nr))))))


(defun s-display-info (item)
  "prepars a item for displaying"

 (cond ((or (stringp item) (symbolp item)) (cons item t))
       ((not (consp (cdr item))) (cons (first item) t))
       ((equal (second item) :no-select) (cons (first item) nil))
       (t (cons (first item) t))))


(defun execute-menu-action (item)
  "executes the action which correspond to item"

 (cond 
  ((or (stringp item) (symbolp item)) item)
  ((atom (cdr item)) (cdr item))
  ((= (length item) 2) (second item))
  (t (case (second item)
      (:value (third item))
      (:eval (eval (third item)))
      (:funcall (apply (third item) nil))))))


;;-------------------------------------------------------------------------------
;;                 MULT-CHOOSE-FROM-MENU 
;;-------------------------------------------------------------------------------



(def$method (tty-menu-mixin :mult-choose-from-menu)
	   (mult-choose-item-list &optional header)
    "presents a menu and returns the list of values specified for the selected items.
each item from mult-choose-item-list specifies a row of the menu. items are selected
by entering their numbers sequentially in a single line. each item has to have the form
(value string (t)) where string is what is displayed - together with the number of the item.
header which has to be a string is used as menu headline."

  (let ((*standard-input* dialog-stream)
	(*default-dialog-stream* dialog-stream)
	(header (or header (getentry mult-choose-header-str babylon-io-table)))
	(aug-item-list (augment-mult-choose-item-list mult-choose-item-list 1)))
    (terpri)
    (princ (getentry star-str babylon-io-table))
    (terpri)
    (print-header header)    
    (mapc #'print-item aug-item-list)
    (princ (getentry star-str babylon-io-table))
    (collect-results (mult-prompt-assoc aug-item-list))))


(defun augment-mult-choose-item-list (mult-choose-item-list nr)
  "creates a modified item list by adding an integer in front of each item.
this integer is used for selecting the item"
  (declare (list mult-choose-item-list) (fixnum nr))
  (cond ((null mult-choose-item-list) nil)
	(t (let ((item (first mult-choose-item-list))
		 new-item)
	     (cond ((third item)
		    (setq new-item (cons nr item)))
		   (t (setq new-item (cons '- item))))
	     (unless (eq (first new-item) '-)
	       (setq nr (1+ nr)))
	     (cons new-item
		   (augment-mult-choose-item-list (rest mult-choose-item-list) nr))))))


(defun print-item (item)
  "displays one row of the menu e.g. one item"

  (princ "*") 
  (princ "        ")   ; 8 blanks
  (if (fourth item) (princ (first item)))
  (format t "~11T") (princ (third item))
  (format t "~70T") (princ "*")
  (terpri))


(defun print-header (header)
  "displays the menu header"

  (princ  "*")    (princ " MENU:")
  (format t "~11T")  (princ header)
  (format t "~70T")  (princ "*")
  (terpri))


(defun mult-prompt-assoc (a-list &optional (pstring "  ---> "))
  "accept multi-integer input from user and returns a list of the corresponding
items, if input has been legal."

  (terpri)
  (princ pstring)
  (let* ((input-string (concatenate 'string "(" (read-line) ")" ))
	 (answers (read-from-string input-string))
	 (result
	   (catch 'input-error
	     (mapcar #'(lambda (answer)
			 (or (unless (eq answer '-)
			       (assoc answer a-list)) 
			     (throw 'input-error
			       (format t (getentry illegal-choice-fstr babylon-io-table)
				       answer))))
			   answers))))
    (or result
	(mult-prompt-assoc a-list pstring))))


(defun collect-results (a-list)
  "creates a list out of the second elements of a-list's lists"

  (mapcar #'cadr a-list))


;;; eof

