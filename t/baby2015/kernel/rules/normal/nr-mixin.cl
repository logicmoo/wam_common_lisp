;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10. -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG



;; AUTHORS:  Franco di Primio, Eckehard Gross, Juergen Walther


(def$flavor normal-rule-mixin
	((explanation-window nil)
	 (rule-ted nil))
	(mini-rule-mixin)
  :settable-instance-variables)

(def$method (normal-rule-mixin :generate-rule-processor) ()
  "generates a prolog-processor binding it to the instance variable prolog-processor."
  (setf rule-processor (make-$instance 'normal-rule-processor 
				      :meta-processor self
				      :alternate-meta-processor
				      (make-$instance 'kb-stub
						      :meta-processor self))))

(assign-typefkt 'rule-reference-type 'normal-rule-mixin)

(def$method (normal-rule-mixin :send-explanation-window) (selector &rest args)
  (when explanation-window
    (if (eql selector :format)
	($send explanation-window :expose))
    (lexpr-$send explanation-window selector args)))

(def$method (normal-rule-mixin :send-rule-ted) (selector &rest args)
  (if rule-ted (lexpr-$send rule-ted  selector args)))

(def$method (normal-rule-mixin :set-up-rule-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'rule table)
	     :rule (gethash 'rule-commands table))
      ($send self :add-operations
	     :rule (gethash 'rule-trace-maincommands table))
      ($send self :add-sub-operations
	     :rule (gethash 'rule-trace table)
	     :rule-trace (gethash 'rule-trace-commands table))
      ($send self :add-operations
	     :rule (gethash 'rule-develop-commands table)))))


;;--------------------------------------------------------------------------
;;                  FOR HANDLING RULES 
;;--------------------------------------------------------------------------


(def$method (normal-rule-mixin :print-rule) ()
  "Print a rule of a rule set after selecting one."
  ($send rule-processor :print-rule))

(def$method (normal-rule-mixin :inspect-terms) ()
  "Inspect terms."
  ($send rule-processor :inspect-terms))



;;--------------------------------------------------------------------------
;;                  EXPLAINING RESULTS 
;;--------------------------------------------------------------------------

(def$method (normal-rule-mixin :explain-results) ()
  "Explain results.
Actually only the results of rule processor actions are explained."
  ($send rule-processor :explain-results))


(defun explain-results (&rest ignore)
  "Explain results.  
Actually only the results of rule processor actions are explained."
  (declare (ignore ignore))
  (send-kb :explain-results))

;;; eof

