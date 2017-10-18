;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")


;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     uralt
;; AUTHORS:  Eckehard Gross 

;; This file depends on:  common>*
;;                        meta>kb-stub
;;                        prolog>basic>*
;;                        prolog>mini>mp-proc

;; contents: a mixin making the facilities of mini-prolog-processor available
;;           for a knowledge base.
;;           mini-prolog-processor adds trace facilities to those of
;;           basic-prolog-processor.

;;--------------------------------------------------------------------------
;;                  FLAVOR MINI-PROLOG-MIXIN 
;;--------------------------------------------------------------------------


(def$flavor mini-prolog-mixin
	((prolog-trace-window nil))
	(basic-prolog-mixin)
  :settable-instance-variables
  (:documentation "this mixin makes the facilities of mini-prolog-processor available.
mini-prolog-processor adds trace facilities to those of basic-prolog-processor."))


(assign-typefkt 'prolog-type 'mini-prolog-mixin)


(def$method (mini-prolog-mixin :generate-prolog-processor) ()
  "generates an instance of mini-prolog-processor.
the instance is bound to the instance variable prolog-processor."
  (setf prolog-processor (make-$instance 'mini-prolog-processor 
					:meta-processor self
					:alternate-meta-processor
					(make-$instance 'kb-stub
						       :meta-processor self)
					:root-type 'mini-goalbox)))


(def$method (mini-prolog-mixin :set-up-prolog-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'prolog table)
	     :prolog (gethash 'prolog-commands table))
      ($send self :add-operations
	     :prolog (gethash 'prolog-toggle-command table))  
      ($send self :add-sub-operations
	     :prolog (gethash 'prolog-trace table)
	     :prolog-trace (gethash 'prolog-trace-commands table)))))

(def$method (mini-prolog-mixin :after :make-yourself-current) ()
  ($send prolog-processor :synchronize-trace))

(def$method (mini-prolog-mixin :send-prolog-trace-window) (selector &rest args)
  "passes messages to prolog-trace-window."
  (lexpr-$send prolog-trace-window selector args))

;;------------------------------------------------------------------------------------

(def$method (mini-prolog-mixin :toggle-prolog-trace) ()
  "toggles rule tracing."
  ($send prolog-processor :toggle-prolog-trace))


(def$method (mini-prolog-mixin :set-prolog-trace-options) ()  
  (do ((method ($send self :choose-from-menu
		      (getentry option-menu-items prolog-io-table)
		      (getentry option-menu-title prolog-io-table))
	       ($send self :choose-from-menu
		      (getentry option-menu-items prolog-io-table)
		      (getentry option-menu-title prolog-io-table))))
      ((eq method 'exit))
    (cond ((null method))
	  ((consp method)
	   (lexpr-$send self (first method) (rest method)))
	  (t ($send self method)))))


;;------------------------------------------------------------------------------------


(def$method (mini-prolog-mixin :show-trace-status) ()
  "displays all predicates currently traced and the trace modus selected"

  (let ((prolog-trace ($send prolog-processor :prolog-trace))
	(trace-mode ($send prolog-processor :trace-mode))
	(trace-list ($send prolog-processor :trace-list)))
    (if (null prolog-trace)
	($send self :babylon-format (getentry if-toggled-fst prolog-io-table)))
    ($send self :babylon-format
		 (getentry trace-for-preds-fstr prolog-io-table) trace-mode)
    (cond ((null trace-list)($send self :babylon-format 
				   (getentry none-fstr prolog-io-table)))
	  ((eq trace-list 'all)($send self :babylon-format 
				      (getentry all-fstr prolog-io-table)))
	  (t ($send self :babylon-format
			   "  ~{~S   ~}" (rest trace-list))))))

(def$method (mini-prolog-mixin :after :show-status) ()
  ($send self :show-trace-status))

;;------------------------------------------------------------------------------------

(def$method (mini-prolog-mixin :set-prolog-trace-mode) (mode)
  "sets the trace mode."
  ($send prolog-processor :set-trace-mode mode))

;;------------------------------------------------------------------------------------
  
(def$method (mini-prolog-mixin :trace-preds) (preds)
  ($send prolog-processor :set-trace-list preds)
  ($send prolog-processor :synchronize-trace))


(defun diff-list (alist blist)
  (do ((talist (copy-list alist) (delete (first tblist) talist))
       (tblist blist (rest tblist)))
      ((or (null tblist)(null talist)) talist))) 

(def$method (mini-prolog-mixin :select-for-trace) (&optional clear) 
  (let ((axset-names ($send prolog-processor :axioms))
	(new-preds nil))
    (if clear ($send prolog-processor :set-trace-list nil))
    (cond ((rest axset-names)
	   (do ((axset-name ($send self :select-axset-name axset-names)
			    ($send self :select-axset-name axset-names))
		(trace-list ($send prolog-processor :trace-list)))
	       ((eq axset-name 'exit) t)
	     (setf new-preds
		   ($send self :select-preds-for-tracing axset-name))
	     (setf trace-list 
		   (cond ((equal new-preds '(nil)) trace-list)
			 ((eq trace-list 'all)
			  (cons '%top
				(nconc (mapcan #'(lambda (name)
						   (copy-list (get-predicates name)))
					       (remove axset-name axset-names))
				       new-preds)))
			 (trace-list
			  (nconc (diff-list trace-list (get-predicates axset-name))
				 new-preds))
			 (t (cons '%top new-preds))))
	     ($send prolog-processor :set-trace-list trace-list)))
	  (t (setf new-preds
		   ($send self :select-preds-for-tracing (first axset-names)))
	     (unless (equal new-preds '(nil))
	       ($send prolog-processor :set-trace-list (cons '%top new-preds)))))
    ($send prolog-processor :synchronize-trace)))

(def$method (mini-prolog-mixin :select-preds-for-tracing) (axset-name)
  (let* ((header (concatenate 'string
			      (getentry trace-menu-title prolog-io-table)
			      (string axset-name)))
	 (items (getentry trace-menu-items prolog-io-table))
	 (mode  ($send self :choose-from-menu items header)))
    (case mode
      (trace-all (get-predicates axset-name))
      (trace-none nil)
      (toggle ($send self :select-preds-to-toggle axset-name))
      (exit (list nil)))))


(def$method (mini-prolog-mixin :select-preds-to-toggle) (axset-name)
  (let* ((preds-with-mark 
          ($send prolog-processor :get-preds-with-mark axset-name))
	 (items (cons (getentry toggle-trace-menu-item prolog-io-table)
		      (mapcar #'(lambda (element)
				  `(,(car element)
				    ,(format nil "~A ~S"
					     (if (cdr element) "#" " ")
					     (car element))
				    (t)))
				  preds-with-mark)))
	 (header (concatenate 'string
			      (getentry toggle-trace-menu-title prolog-io-table)
			      (string axset-name)))
         (preds-to-toggle ($send self :mult-choose-from-menu items header))
         (preds-to-trace nil))
    (dolist (pair preds-with-mark (nreverse preds-to-trace))
      (let ((pred (car pair)))
        (if (member pred preds-to-toggle)
          (if (null (cdr pair))
            (setf preds-to-trace (cons pred preds-to-trace)))
          (if (cdr pair)
            (setf preds-to-trace (cons pred preds-to-trace))))))))



;;------------------------------------------------------------------------------------

(defun set-prolog-trace-options ()
  (and (warn-if-no-prolog)
       (send-kb :set-prolog-trace-options)))

;;; eof

