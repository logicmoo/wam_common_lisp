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

(defobfun (set-window-position *dialog*) (h &optional v)
  (usual-set-window-position h v)
  (setf *babylon-menu-position* (make-point h v)))

#|
pop-up dialogs

this file creates a new class of dialog objects which can be used for easily
popping up options in front of the user.  It illustrates Dialogs, objects,
the init-list-default function, and the nfunction special form.

pop-up dialogs contain a single table.  When the user clicks in a the table,
an action is run.  pop-up dialog can be modal or modeless.

The function POP-UP is used to create pop-up dialogs.

POP-UP accepts any even number of arguments.  The arguments should alternate
between keywords and values (like the argument to oneof).  POP-UP accepts the
standard window init-list options, but you usually don't need to supply these
(except perhaps for :window-title).  In addition, POP-UP accepts the following
pseudo-keyword arguments:

:item-list           A list of items to display in the pop-up dialog.

:dispatch-function   The action to call when the user clicks on a cell

                     If :dispatch-function is a function, it will be funcalled
                     with the contents of the clicked cell as the argument.

                     If :dispatch-function is the keyword :ask-item, then
                     each item can have its own action.  The :item-list must be
                     an alist.  The car of each pair will be displayed
                     in the table.  The cdr of each pair should be a function
                     or form.  This function or form provides the action for
                     the corresponding item.  If it is a function
                     it will be funcalled (with no arguments).  If it is not a
                     function, it will be eval'ed.  You only need to use the
                     :ask-item option when each item does something very
                     different.

:modal               If :modal is non-nil (the default), then POP-UP displays
                     the dialog as a modal dialog.  After the user clicks, the
                     return-from-modal-dialog is called.  The value returned by
                     the action is returned by the call to POP-UP.

                     If :modal is nil, POP-UP simply displays the dialog window
                     as a modeless dialog.  The dialog will remain visible, even
                     after the user clicks.

:table-width         The height of a pop-up dialog is computed automatically.
                     The width, however, will always be the same unless it is
                     specified by the user.  This is because calculating the
                     width can be very computationally intensive.  If the user
                     wishes to specify a non-default width, :table-width may
                     be given.  It should be an integer giving a number of
                     pixels.
|#

;;make a sub-class of sequence-dialog-items used for displaying a-lists
(defobject *alist-dialog-item* *sequence-dialog-item*)

(defobfun (cell-contents *alist-dialog-item*) (cell)
   (car (usual-cell-contents cell)))

(defobfun (full-cell-contents *alist-dialog-item*) (cell)
  (elt (table-sequence) (cell-to-index cell)))

(defobfun (value-cell-contents *alist-dialog-item*) (cell)
  (cdr (full-cell-contents cell)))


;here's the class of pop-up dialogs
(defobject *pop-up-dialog* *dialog*)

;this is where most of the work is done
(defobfun (exist *pop-up-dialog*) (init-list)
  (let* ((item-list (getf init-list :item-list ()))
         (list-length (length item-list))
         (table-height (min (- *screen-height* 175)
                            (* 14 list-length)))
         (table-width (getf init-list :table-width 120))
         (dispatch-function (getf init-list :dispatch-function ()))
         (modal-p (getf init-list :modal t))
         (the-table (oneof (if (eq dispatch-function :ask-item)
                             *alist-dialog-item*
                             *sequence-dialog-item*)
                           :dialog-item-size (make-point table-width
                                                         table-height)
                           :dialog-item-position #@(2 2)
                           :cell-size (make-point (- table-width 15)
                                                  12)
                           :table-sequence item-list
                           :table-hscrollp nil
                           :table-dimensions (make-point 1 list-length)
                           :dialog-item-action
                           (if (eq dispatch-function :ask-item)
                             ;we use nfunction so that we can call usual
                             (nfunction
                              dialog-item-action
                              (lambda ()
                                (when (selected-cells)
                                  (let*
                                    ((the-cell (car (selected-cells)))
                                     (the-action (value-cell-contents the-cell))
                                     (returned-value
                                      (if (functionp the-action)
                                        (funcall the-action)
                                        (eval the-action))))
                                    (if modal-p
                                      (return-from-modal-dialog returned-value)
                                      (usual-dialog-item-action))))))
                             (nfunction
                              dialog-item-action
                              (lambda ()
                                (when (selected-cells)
                                  (let*
                                    ((returned-value
                                      (funcall dispatch-function
                                               (cell-contents
                                                (car (selected-cells))))))
                                    (if modal-p
                                      (return-from-modal-dialog returned-value)
                                      (usual-dialog-item-action))))))))))
    (usual-exist
     (init-list-default init-list
                        :window-type (if modal-p :tool ;:double-edge-box
                                         :document)
                        :close-box-p nil
                        :window-size (make-point (+ 4 table-width)
                                                 (+ 4 table-height))
                        :window-position #@(250 38)
                        :window-show nil
                        :window-title (getf init-list :window-title "Select One of")
                        :window-font *bab-menu-font* 
                        :dialog-items (list the-table)))))

 
(defun pop-up (&rest args)
  (let ((the-pop-up (apply #'oneof *pop-up-dialog* args)))
    (if (getf args :modal t)
      (modal-dialog the-pop-up)
      (ask the-pop-up (window-show)))))


;here's the class for mult-choose dialogs

(defobject *mult-choose-dialog* *dialog*)

(proclaim '(object-variable (*mult-choose-dialog* dispatch-function)))

(defobfun (exist *mult-choose-dialog*) (init-list)
  (let* ((item-list (getf init-list :item-list ()))
         (list-length (length item-list))
         (table-height (min (- *screen-height* 75)
                            (* 12 list-length)))
         (table-width (getf init-list :table-width 120))
         (dispatch-function (getf init-list :dispatch-function ()))
         (modal-p (getf init-list :modal t))
         (the-table (oneof *alist-dialog-item*
                     :dialog-item-size (make-point table-width table-height)
                     :dialog-item-position #@(2 25)
                     :cell-size (make-point (- table-width 15) 12)
                     :table-sequence item-list
                     :table-hscrollp nil
                     :selection-type :disjoint
                     :table-dimensions (make-point 1 list-length)
                     )))
    (usual-exist
     (init-list-default init-list
                        :window-type (if modal-p :tool ;:double-edge-box
                                         :document)
                        :close-box-p nil
                        :window-size (make-point (+ 4 table-width)
                                                 (+ 34 table-height))
                        :window-position #@(250 38)
                        :window-show nil
                        :window-font *bab-menu-font*
                        :window-title (getf init-list :window-title "Select Any of")
                        :dialog-items 
                        (list 
                         (oneof *button-dialog-item*
                           :dialog-item-text "  Do It  "
                           :dialog-item-position #@(20 7)
                           :dialog-item-action 
                           (nfunction
                            dialog-item-action
                            (lambda () 
                              (return-from-modal-dialog 
                               (mapcar #'(lambda (cell)
                                           (ask the-table (value-cell-contents cell)))
                                       (ask the-table (selected-cells))))
                               )))
                          (oneof *button-dialog-item*
                           :dialog-item-text "  Abort  "
                           :dialog-item-position #@(120 7)
                           :dialog-item-action 
                           (nfunction
                            dialog-item-action
                            (lambda ()
                              (return-from-modal-dialog nil))))
                          the-table)))))


#|
example calls to POP-UP


(pop-up :item-list '(1 2 3 4 5 1 2 3 4 5
                     1 2 3 1 2 3 1 2 3 1
                     3 2 1 3 2 1 1 2 3 1)
        :table-width 150
        :dispatch-function #'(lambda (n)
                               (dotimes (x n)
                                 (ed-beep))
                               n))

(pop-up :item-list '(1 2 3 4 5 6 7 8 9 10)
        :modal nil
        :window-title "peep"
        :dispatch-function #'(lambda (n)
                               (dotimes (x n)
                                 (ed-beep))
                               n))

(pop-up :item-list '("abc" "def" "ghi" "jkl" "mno" "pqr" "stu" "vwx" "yza")
        :dispatch-function #'(lambda (a-string)
                               (print a-string))
        :window-title "Print String"
        :selection-type :disjoint
        :modal t)


(pop-up :item-list '(1 'two "three" (4) 5 6 7 8)
        :dispatch-function #'(lambda (form)
                               (inspect form))
        :window-title "Inspect"
        :modal nil)


(pop-up :item-list '(("Beep" . (ed-beep))
                     ("Beep Twice" . (progn (ed-beep) (ed-beep)))
                     ("Say Hello" . (print "Hello"))
                     ("Emacs Mode Off" . (setq *emacs-mode* nil)))
        :table-width 150
        :dispatch-function :ask-item)


(pop-up :item-list `(("Beep" . ed-beep)
                     ("Beep Twice" . ,#'(lambda () (ed-beep) (ed-beep)))
                     ("Inspect Type-in" . ,#'(lambda ()
                                              (inspect
                                               (read-from-string
                                                (get-string-from-user
                                                 "Type in for inspect"))))))
        :dispatch-function :ask-item
        :modal nil
        :table-width 140)
|#

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
           ("Value" :value symbol-value)
           ("Funcall" :funcall ed-beep)
           ("Eval" :eval (format t "~%Dies ist der Effect der Eval Form")))
         )
|#

(def$method (mac-menu-mixin :choose-from-menu) (item-list &rest args)
  "Choose one item from a list of items structured according to Lispm conventions.
Map these items to a pair, whose car is the string or symbol to be displayed,
and whose cdr is a function, to be funcalled with no arguments, or a form,
to be evaluated."
  (pop-up :item-list 
          (remove nil                ; ignore all item types not recognized here
                  (mapcar #'(lambda (item)
                              (cond ((atom item) ;(or (stringp item) (symbolp item) (numberp item))
                                     `(,item . ',item))
                                    ((atom (cdr item))
                                     `(,(first item) . ',(cdr item)))
                                    ((= (length item) 2)
                                     `(,(first item) . ',(second item)))
                                    (t (case (second item)
                                         (:no-select `(,(first item) . 'nil))
                                         (:value `(,(first item) . ',(third item)))
                                         (:eval `(,(first item)  . ,(third item)))
                                         (:funcall `(,(first item) . ,(third item)))))))
                          item-list))
          :dispatch-function :ask-item
          :table-width (* 6 *item-width*)
          :window-position *babylon-menu-position*
          :window-title (if args (car args) "Select One of")))

; multiple choose menu items are built like (Symbol String (t))
; string is shown and a list of symbols of selected Strings is returned

(def$method (mac-menu-mixin :mult-choose-from-menu) (item-list &rest args)
  (modal-dialog 
   (oneof *mult-choose-dialog* 
          :item-list (mapcar #'(lambda (item)
                                 `(,(second item) . ,(first item)))
                             item-list)
          :selection-type :disjoint
          :table-width (* 6 *item-width*)
          :window-position *babylon-menu-position*
          :window-title (if args (car args) "Select Any of"))))

