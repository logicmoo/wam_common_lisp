;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Base: 10. ;Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1987   BY
;;           G M D 
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     August 1987
;; AUTHOR:   E. Gross

;; Contents: a handler for menu entries of a dynamically changing menu.

;;--------------------------------------------------------------------
;;--------------------------------------------------------------------
 
(def$flavor menu-item-handler
	((current-item-list nil)
	 (sub-menu-stack nil)
	 (item-list-alist nil)
	 (cmd-menu nil))
	()
  :settable-instance-variables
  (:documentation "a handler for menu entries of a dynamically changing menu.
		 item-list-alist contains an alist in which the data are lists
		 of menu entries. these lists are used to compose a current item
		 list for the menu which is stored in current-item-list.
		 the standard way to build the current item list is to concatenate
		 the items for the key :top with the items for one other key.
		 the keys of the changing part of the current item list are stored
		 in the history stack sub-menu-stack.
                 cmd-menu contains an object for menu management which handles 
                 the methods choose-from-menu and mult-choose-from-menu."))

(def$method (menu-item-handler :after :init) (&rest plist)
  "initialize the menu management object." 
  (declare (ignore plist))
  (setf cmd-menu ($send self :gen-or-get-cmd-menu))
  ($send self :send-if-handles :set-up-commands))

(def$method (menu-item-handler :gen-or-get-cmd-menu) ()
  "kb will be the menu manager."
  self)

;;--------------------------------------------------------------------


(def$method (menu-item-handler :add-operations)
	   (key operations &optional (mode :after))
  "concatenates operations i.e. a list of menu items to the part ofitem-list-alist
which is indexed by key. possible modes are :before and :after. in the former case
the new items are put in front of the previous items, in the latter case they are
appended."
  
  (let ((key-list-pair (assoc key item-list-alist)))
    (cond ((null key-list-pair)
	   (setf item-list-alist
		 (cons (cons key operations) item-list-alist)))
	  ((eq mode :after)
	   (setf (rest key-list-pair)
		 (append (rest key-list-pair) operations)))
	  ((eq mode :before)
	   (setf (rest key-list-pair)
		 (append operations (rest key-list-pair)))))
    t))   


(def$method (menu-item-handler :add-sub-operations)
	    (superkey expand-operation key operations &optional (mode :after))
  "adds the menu item expand-operation to the items indexed by superkey
and operations which is a list of menu items to the list indexed by key.
mode with values :before or :after determines the placement of the expand-operation.
expand-operation is supposed to make operations current."
  
    ($send self :add-operations
	   superkey  (list expand-operation) mode)
    ($send self :add-operations
	   key operations))


(def$method (menu-item-handler :make-menus-current) (&rest keys)
  "builds a current item list by concatenating the items for the keys provided."
  
  (setf current-item-list
	(apply #'append (mapcar #'(lambda (key)
				    (rest (assoc key item-list-alist)))
				keys)))
  t)

;;---------------------------------------------------------------------
;;              methods to update the current-item-list
;;---------------------------------------------------------------------

(def$method (menu-item-handler :open-menu)
	    (key &optional (stack-depth 3))
  "builds a current item list out of the items for :top and for key.
key is pushed to the history stack which is truncated to a maximal
depth of stack-depth by deleting the eldest item of the stack.
the method returns :activate which triggers displaying the changed menu."
  
  (declare (fixnum stack-depth))
  (setf sub-menu-stack (cons key sub-menu-stack))
  (if (> (length sub-menu-stack) stack-depth)
      (setf sub-menu-stack (butlast sub-menu-stack)))
  ($send self :make-menus-current :top (first sub-menu-stack))
  :activate)

(def$method (menu-item-handler :close-menu) ()
  "builds a current item list out of the items for :top and for the previously
used key, which will be retrieved by poping the sub-menu-stack.
the method returns :activate which triggers displaying the changed menu."
  
  (setf sub-menu-stack (rest sub-menu-stack))
  ($send self :make-menus-current :top (first sub-menu-stack))
  :activate)


;;---------------------------------------------------------------------
;;                 auxiliary methods for testing
;;---------------------------------------------------------------------

(def$method (menu-item-handler :clear-menu) ()
  "deletes all menu items."
  (setf current-item-list nil)
  (setf item-list-alist nil))

(def$method (menu-item-handler :get-menu-keys) ()
  "returns the keys of the item lists."
  (mapcar #'first item-list-alist))

(def$method (menu-item-handler :get-sub-menu) (key)
  "returns the menu items indexed by key."
  (rest (assoc key item-list-alist)))

;;; eof

