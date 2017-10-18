;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; function-definitions used by the model-constructs and the layers

(defun test-subsetp (list1 list2)
  ; performs a subsetp test for list1 and list2 but list1 must not be empty
  (if (null list1) nil
      (subsetp list1 list2)))

(defun collect-elements (double-list diff-list)
  ; pushes the elements of the double-list without the keyword at the beginning of each
  ; sublist into diff-list  (used by :get-unknwon)
  (if (null double-list) diff-list                          ; return diff-list
      (collect-elements (cdr double-list)                   ; call at most 2 times
                        (union (cdar double-list) diff-list))))

(defun delete-dup (old-list new-value-list)
  ; deletes all elements in old-list that are also in new-value-list;
  ; takes into consideration the keyword of old-list
  (cond ((null old-list) nil)
        ((intersection new-value-list old-list)             ; intersection not empty
         (let ((differ (set-difference (cdr old-list) new-value-list))
               (keyword (car old-list)))
           (if (null differ) nil                            ; difference not empty
               (cons keyword differ))))
        (t old-list)))

(defun remove-dup (old-list new-value-list)
  ; removes elements in old-list that are also in new-value-list;
  ; takes into consideration the keyword of old-list
  (cond ((null old-list) nil)
        ((intersection new-value-list old-list)             ; intersection not empty
         (let ((differ (removel (cdr old-list) new-value-list))
               (keyword (car old-list)))
           (if (null differ) nil                            ; difference not empty
               (cons keyword differ))))
        (t old-list)))

(defun removel (list remove-list)
  ; removes exactly those elements in list that are also in remove-list
    (cond ((null remove-list) list)
          ((member (car remove-list) list)
            (removel (remove (car remove-list) list :count 1)
                     (cdr remove-list)))                    ; call again recursively
          (t (removel list (cdr remove-list)))))            ; call again recursively

(defun make-2-lists (item1 item2)
  ; returns a list of item1 and item2 but leaves one of them out if it is empty
  (cond ((and (null item1) (null item2)) nil)
        ((null item1) (list item2))
        ((null item2) (list item1))
        (T (list item1 item2))))

(defun construct-list (keyword list)
  (if (null list) nil
      (cons keyword list)))

(defun convert-to-list (arg)
  ; returns a list in any case
  (if (listp arg) arg
      (list arg)))

(defun eval-atom-or-list (expression)
  ; eval the expression if it isn't an atom
   (if (atom expression) expression
       (eval expression)))

;;; eof

