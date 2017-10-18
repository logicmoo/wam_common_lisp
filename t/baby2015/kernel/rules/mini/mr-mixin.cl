;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10. -*-

(in-package "BABYLON")

;;           Copyright   1988    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  E. Gross
;; DATE:     December 1988
;;         


;;-----------------------------------------------------------------------------
;;                   FLAVOR MINI-RULE-MIXIN 
;;-----------------------------------------------------------------------------


(def$flavor mini-rule-mixin
	    ((rule-trace-window nil)
	     (rule-trace-displayer :display-rule-trace))
  (basic-rule-mixin)
  :settable-instance-variables)


(def$method (mini-rule-mixin  :generate-rule-processor) ()
  "generates an instance of mini-rule-processor."
  (setf rule-processor
	(make-$instance 'mini-rule-processor 
			:meta-processor self
			:alternate-meta-processor (make-$instance 'kb-stub
								  :meta-processor self))))


(assign-typefkt 'rule-reference-type 'mini-rule-mixin)


(def$method (mini-rule-mixin :send-rule-trace-window) (selector &rest args)
  "passes messages to rule-trace-window."
  (lexpr-$send rule-trace-window selector args))


(def$method (mini-rule-mixin :set-up-rule-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'rule table)
	     :rule (gethash 'rule-commands table))
      ($send self :add-operations
	     :rule (gethash 'rule-trace-maincommands table))
      ($send self :add-sub-operations
	     :rule (gethash 'rule-trace table)
	     :rule-trace (gethash 'rule-trace-commands table)))))

;;;---------------------------------------------------------------------------

(def$method (mini-rule-mixin :display-trace-element) (trace-element)
  (case (trace-element-keyword trace-element)
    (:verify-hypothesis
      (let ((hypothesis (trace-element-term trace-element)))
	($send self :send-rule-trace-window :format " ")
	($send self :send-rule-trace-window :format
	       (getentry test-hypothesis-fstr rule-io-table)
	       (if (lisp hypothesis) hypothesis `(,hypothesis)))
	($send self :send-rule-trace-window :format  " ")))
    (:try-rule
      ($send self :send-rule-trace-window :format "~A[~S ~S] ~S"
	     (getentry try-rule-str rule-io-table)
	     (trace-element-rule-set-name trace-element)
	     (rule-name (trace-element-rule trace-element))
	     (trace-element-mode trace-element)))
    (:in-then-part
      ($send self :send-rule-trace-window :format "~A~S."
	     (getentry attempt-to-verify-str rule-io-table)
	     (trace-element-term trace-element)))
    (:remember
      ($send self :send-rule-trace-window :format "~A[~S ~S] ~Ss ~S."
	     (getentry rule-str rule-io-table)
	     (trace-element-rule-set-name trace-element)
	     (rule-name (trace-element-rule trace-element))
	     (get-action-type (rule-right-hand-side (trace-element-rule trace-element)))
	     (trace-element-term trace-element)))
    (:store
      ($send self :send-rule-trace-window :format "~A[~S ~S]~A~S"
	     (getentry rule-str rule-io-table)
	     (trace-element-rule-set-name trace-element) 
	     (rule-name (trace-element-rule trace-element))
	     (getentry execute-str rule-io-table)
	     (trace-element-term trace-element)))
    (:ask-user
     ($send self :send-rule-trace-window :format "~A[~S ~S] ~A ~S"
	    (getentry sharp-sign-str rule-io-table)
	    (trace-element-rule-set-name trace-element) 
	    (rule-name (trace-element-rule trace-element))
	    (getentry ask-user-str rule-io-table)
	    (trace-element-term trace-element)))))


;;;--------------------------------------------------------------------------


(def$method (mini-rule-mixin :rule-trace) ()
  "gets rule trace status."
  ($send rule-processor :rule-trace))

(defun toggle-rule-trace ()
  "toggles rule tracing."
  (if (current-kb-typep 'mini-rule-mixin)
      (send-rule :toggle-rule-trace)))

(def$method (mini-rule-mixin :toggle-rule-trace) ()
  "toggles rule tracing."
  ($send rule-processor :toggle-rule-trace))

(def$method (mini-rule-mixin :set-rule-trace-mode) (mode)
  "sets rule trace mode."
  ($send rule-processor :set-trace-mode mode))

;;;---------------------------------------------------------------------------

(def$method (mini-rule-mixin  :mark-all) (mode)
  (dolist (rset-name ($send rule-processor :get-rule-set-names) t)
    ($send rule-processor :mark-rules mode rset-name
	   ($send rule-processor :get-rule-names rset-name))))
	  
;;;---------------------------------------------------------------------------


(def$method (mini-rule-mixin :select-rules-to-toggle) (rule-set-name)
  (let* ((rules-with-mark ($send rule-processor :get-rules-with-mark rule-set-name))
	 (items (cons (getentry toggle-mark-menu-item rule-io-table)
		      (mapcar #'(lambda (element)
				  `(,(first element)
				    ,(format nil "~A ~S"
					     (case (rest element)
					       (hide " ")
					       (show "#"))
					     (first element))
				    (t)))
				  rules-with-mark)))
	 (header (concatenate 'string
			      (getentry toggle-mark-menu-title rule-io-table)
			      (string rule-set-name))))
    ($send self :mult-choose-from-menu items header)))


(def$method (mini-rule-mixin  :select-rules-for-tracing) (&optional clear)
  (let ((rule-set-names ($send rule-processor :get-rule-set-names)))
    (if clear ($send self :mark-all 'hide))
    (do ((rset-name ($send self :select-rule-set-name)
		    (if (rest rule-set-names)
			($send self :select-rule-set-name)
			'exit)))
	 ((eq rset-name 'exit) t)
	 (let* ((header (concatenate 'string
				     (getentry mark-menu-title rule-io-table)
				     (string rset-name)))
		(items  (getentry mark-menu-items rule-io-table))
		(mode ($send self :choose-from-menu items header)))
	   (case mode
	     (hide-all ($send rule-processor :mark-rules 'hide rset-name
			      ($send rule-processor :get-rule-names rset-name)))
	     (show-all ($send rule-processor :mark-rules 'show rset-name
			      ($send rule-processor :get-rule-names rset-name)))	
	     (toggle   ($send rule-processor :toggle-rules rset-name
			      ($send self :select-rules-to-toggle rset-name)))
	     (exit     (return t)))))))

;;;---------------------------------------------------------------------------

(def$method (mini-rule-mixin :call-rule-trace-displayer) ()
  ($send self rule-trace-displayer))


(def$method (mini-rule-mixin :display-rule-trace) (&optional (filter #'identity))
  (let ((filtered-list ($send rule-processor :filter-trace-list filter)))
    ($send self :send-rule-trace-window :format " ")
    (if filtered-list
	(dolist (trace-element filtered-list t)
	  ($send self :display-trace-element trace-element))
	($send self :send-rule-trace-window :format
	       (getentry no-entry-str rule-io-table)))))

(def$method (mini-rule-mixin :display-rules-used) ()
  ($send self :display-rule-trace #'rule-used-p))

(def$method (mini-rule-mixin :display-rules-tried) ()
  ($send self :display-rule-trace  #'rule-tried-p))

(def$method (mini-rule-mixin :display-rules-asking) ()
  ($send self :display-rule-trace  #'rule-asking-p))

;;;---------------------------------------------------------------------------

(def$method (mini-rule-mixin :set-rule-trace-options) ()
  (do ((method ($send self :choose-from-menu
		      (getentry option-menu-items rule-io-table)
		      (getentry option-menu-title rule-io-table))
	       ($send self :choose-from-menu
		      (getentry option-menu-items rule-io-table)
		      (getentry option-menu-title rule-io-table))))
      ((eq method 'exit))
    (cond ((null method))
	  ((consp method)
	   (lexpr-$send self (first method) (rest method)))
	  (t ($send self method)))))

;;; eof

