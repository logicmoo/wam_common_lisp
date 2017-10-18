;;; -*- Mode:Lisp; Package:BABYLON; Base:10; Syntax:Common-Lisp -*-

(in-package "BABYLON")

;;           Copyright   1988    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  E. Gross
;; DATE:     December 1988
;;        


;; contents: a flavor which realizes tracing for the rule processor.

(def$flavor rule-trace-mixin
	    ((rule-trace nil)
	     (trace-mode 'direct)
	     (trace-indicator nil)
	     (trace-list nil))
  ()
  (:settable-instance-variables)
  (:required-instance-variables meta-processor)
  (:documentation "this flavor provides tracing facilities for the rule processor.
it implements tracing by before demons to some of the important methods of the
rule-interpreter."))


(def$method (rule-trace-mixin :after :init) (&rest plist)
  (declare (ignore plist))
  (setf trace-indicator
        (intern (symbol-name ($send meta-processor :kb-name)) :keyword)))


(def$method (rule-trace-mixin :after :reset-proc) ()
  ($send self :reset-trace-list))


;;;-------------------------------------------------------------------------------

(def$method (rule-trace-mixin :trace-status) ()
  (if rule-trace
      (format nil (getentry trace-on-fstr  babylon-io-table) "Rule")   ;not rule-io-table
      (format nil (getentry trace-off-fstr babylon-io-table) "Rule")))


(def$method (rule-trace-mixin :toggle-rule-trace) ()
  "Toggles rule trace mode."
  (setf rule-trace (if rule-trace nil t)))


(def$method (rule-trace-mixin :needed-to-show) (fact right-hand-side)
  "Explanation what some term is needed for."
  (if right-hand-side
      ($send meta-processor :choose-from-menu
		    (mapcar #'(lambda (action)
				`(,(from-list-to-string action)
				  :no-select t))
			    (get-rule-actions right-hand-side))
		    (format nil
			    (getentry is-needed-to-show-fstr rule-io-table) 
			    (from-list-to-string fact)
			    (get-action-type right-hand-side)))
      ($send meta-processor :choose-from-menu
		    `((,(format nil
				(getentry is-hypothesis-fstr rule-io-table)
				(from-list-to-string fact))
		       :no-select t)))))


(def$method (rule-trace-mixin :display-traced-rule) (rule rule-set)
  "Display rule or notify if no display available."
  (or ($send self :send-if-handles :display-rule rule rule-set)
      ($send meta-processor :choose-from-menu
	    `((,(getentry no-display-available-str rule-io-table) :no-select t)))))


;;------------------------------------------------------------------------------------------
;;               stuff for marking rules
;;------------------------------------------------------------------------------------------

(def$method (rule-trace-mixin :mark-rule) (mode rule-set-name rule-name)
  (let ((trace-status (get rule-name trace-indicator)))
    (case mode
      (hide (unless (member rule-set-name trace-status)
	      (setf (get rule-name trace-indicator)
		    (cons rule-set-name trace-status))))
      (show (if (member rule-set-name trace-status)
		(if (null (rest trace-status))
		    (remprop rule-name trace-indicator)
		    (setf (get rule-name trace-indicator)
			  (delete rule-set-name trace-status))))))))

(defun get-rule-mark (rule-name rule-set-name indicator)
  (if (member rule-set-name (get rule-name indicator))
      'hide
      'show))


(def$method (rule-trace-mixin :get-rules-with-mark) (rule-set-name)
  (do ((rlist ($send self :get-rule-names rule-set-name) (rest rlist))
       (result nil))
      ((null rlist) (nreverse result))
    (setf result
	  (cons (cons (first rlist)
		      (get-rule-mark (first rlist) rule-set-name trace-indicator))
		      result))))

	    
(def$method (rule-trace-mixin :mark-rules) (mode rule-set-name rule-name-list)
  (dolist (rule-name rule-name-list t)
    ($send self :mark-rule mode rule-set-name rule-name)))


(def$method (rule-trace-mixin :toggle-rules) (rule-set-name rule-name-list)
  (dolist (rule-name rule-name-list t)
    (let ((toggled-mark (case (get-rule-mark rule-name rule-set-name trace-indicator)
			  (hide 'show)
			  (show 'hide))))
      ($send self :mark-rule toggled-mark rule-set-name rule-name))))



;;-----------------------------------------------------------------------------
;;                trace elements
;;-----------------------------------------------------------------------------

(defstruct trace-element
  term		       ;hypothesis or  action
  rule-set-name	       ;rule-set name of current-rule
  rule		       ;current rule
  mode		       ;forward or backward
  keyword              ;dependent on rule-interpreter or data-base
                             ;  :in-then-part
                             ;  :remember
                             ;  :store
                             ;  :ask-user-yes
                             ;  :ask-user-no
                             ;  :ask-user-unknown
                             ;  :try-rule
                             ;  :verify-hypothesis
  )


(def$method (rule-trace-mixin :reset-trace-list) ()
  "Reset instance variable trace-list of this flavor"
  (setf trace-list nil))


(defun trace-element-to-show (trace-element mode indicator)
  (and (or (eq mode 'direct) (eq mode 'comb))
       (eq (get-rule-mark (rule-name (trace-element-rule trace-element))
			  (trace-element-rule-set-name trace-element)
			  indicator)
		'show)))


(def$method (rule-trace-mixin :filter-trace-list) (filter)
  ;; order reversed
  (do ((rlist trace-list (rest rlist))
       (result nil))
      ((null rlist) result)
    (if (and (trace-element-to-show (first rlist) 'direct trace-indicator)
	     (funcall filter (first rlist)))
	(setf result (cons (first rlist) result)))))


(defun rule-used-p (trace-element)
  (member (trace-element-keyword trace-element)
	  '(:remember :store)))

(defun rule-tried-p (trace-element)
  (eq (trace-element-keyword trace-element) :try-rule))

(defun rule-asking-p (trace-element)
  (eq (trace-element-keyword trace-element) :ask-user))


;;-------------------------------------------------------------------------------
;;               tracing methods
;;-------------------------------------------------------------------------------


(defun trace-element-to-store (trace-element mode indicator)
  (declare (ignore trace-element indicator))
  (or (eq mode 'back) (eq mode 'comb)))


(def$method (rule-trace-mixin :before :in-then-part)
	    (fact rule-set current-rule)
  "Trace verification attempt of term within then part."  
  (when rule-trace
      (let ((trace-element (make-trace-element
			     :term fact
			     :rule-set-name (rule-set-name rule-set)
			     :rule current-rule
			     :mode nil
			     :keyword :in-then-part)))	
	(if (trace-element-to-store trace-element trace-mode trace-indicator)
	    (setf trace-list (cons trace-element trace-list)))
	(if (trace-element-to-show trace-element trace-mode trace-indicator)
	    ($send meta-processor :display-trace-element trace-element)))))

(def$method (rule-trace-mixin :before :remember) (action rule-set rule)
  "Trace rule conclusion of type REMEMBER."
  (when rule-trace
    (let ((trace-element (make-trace-element
			   :term  action 
			   :rule-set-name (rule-set-name rule-set)
			   :rule rule
			   :mode nil
			   :keyword :remember)))
	(if (trace-element-to-store trace-element trace-mode trace-indicator)
	    (setf trace-list (cons trace-element trace-list)))
	(if (trace-element-to-show trace-element trace-mode trace-indicator)
	    ($send meta-processor :display-trace-element trace-element)))))


(def$method (rule-trace-mixin :before :store) (action rule-set rule)
  "Trace rule conclusion of type STORE. not yet used"
  (when rule-trace
    (let ((trace-element (make-trace-element
			   :term action
			   :rule-set-name (rule-set-name rule-set)
			   :rule rule
			   :mode nil
			   :keyword :store)))
      (if (trace-element-to-store trace-element trace-mode trace-indicator)
	  (setf trace-list (cons trace-element trace-list)))
      (if (trace-element-to-show trace-element trace-mode trace-indicator)
	  ($send meta-processor :display-trace-element trace-element)))))

     
(def$method (rule-trace-mixin :before :ask-user)
	    (action rule rule-set flag type)
  (declare (ignore flag type))
  (when rule-trace
    (let ((trace-element
	    (make-trace-element
	      :term action 
	      :rule-set-name (rule-set-name rule-set)
	      :rule rule
	      :mode nil
	      :keyword :ask-user)))
      (if (trace-element-to-store trace-element trace-mode trace-indicator)
	  (setf trace-list (cons trace-element trace-list)))
      (if (trace-element-to-show trace-element trace-mode trace-indicator)
	  ($send meta-processor :display-trace-element trace-element)))))

   
(def$method (rule-trace-mixin :before :try-rule) (rule rule-set mode)
  "Trace application attempt of rule in rule set rule-set backward or forward mode."
  (when rule-trace
    (let ((trace-element (make-trace-element 
			   :term nil
			   :rule-set-name (rule-set-name rule-set)
			   :rule rule 
			   :mode mode
			   :keyword :try-rule)))
      (if (trace-element-to-store trace-element trace-mode trace-indicator)
	  (setf trace-list (cons trace-element trace-list)))
      (if (trace-element-to-show trace-element trace-mode trace-indicator)
	  ($send meta-processor :display-trace-element trace-element)))))
   

(def$method (rule-trace-mixin :before :verify-hypothesis) (hypothesis rule rule-set)
  "trace verification attempt of hypothesis."
  
  (when rule-trace
    (let ((trace-element (make-trace-element
			   :term hypothesis
			   :rule-set-name (rule-set-name rule-set)
			   :rule rule 
			   :mode nil
			   :keyword :verify-hypothesis)))
      (if (trace-element-to-store trace-element trace-mode trace-indicator)
	  (setf trace-list (cons trace-element trace-list)))
      (if (trace-element-to-show trace-element trace-mode trace-indicator)
	  ($send meta-processor :display-trace-element trace-element)))))
   
    
;;;------------------------------------------------------------------------


