;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")



(defbabylon-entry explanation-str frame-io-table english
	  " Show Explanations on ")

(defbabylon-entry explanation-str frame-io-table german
	  " Gib Erlaeuterungen fuer ")
 
(defun build-mult-choose-item-list (list)
  (mapcar #'(lambda (element)
	      `(,element  ,(format nil " ~S" element) (t)))
	  list))


(defun build-explain-item-list (choices explanations)
  (cond (choices
	 (append '((help "CONTEXT  " (t))
		   (nil  "         " nil))
		 (build-mult-choose-item-list choices)))
	(explanations '((help " CONTEXT  "  (t))
			(slot " SLOT     " (t))))
	(t '((help " CONTEXT  "  (t))))))



(def$method (poss-val-mixin :provide-local-help)
	    (slot &optional (window *current-knowledge-base*))
  (let* ((explanations ($send self :get slot :explain-answers))
	 (choices (explain-answers-choices explanations))
	 (item-list (build-explain-item-list choices explanations))
	 (selections (send-kb :mult-choose-from-menu
			      item-list
			      (getentry explanation-str frame-io-table))))
    (cond ((null selections) nil)
	  ((member 'help selections) 'help)
	  ((and (null choices) (member 'slot selections))
	   (lexpr-$send window :format
			(substitute-o-and-s object-name slot explanations)))
	  (t (dolist (choice selections)
	       (let ((explanation
		       (rest (assoc choice explanations :test 'equal))))
		 (lexpr-$send window :format
			      (substitute-o-and-s object-name slot explanation))))))))


;;-------------------------------------------------------------------------------


(defbabylon-entry choose-one-of-header-str  frame-io-table english
  " Choose one of: ")

(defbabylon-entry choose-one-of-header-str  frame-io-table german 
  " Waehle einen Eintrag aus ")


(defbabylon-entry expect-fstr frame-io-table english
  " Expected Value ~:[~;NOT~] ~S ~S ")

(defbabylon-entry expect-fstr frame-io-table german
  " Erwarteter Wert ~:[~;NICHT~] ~S ~S ")

(defun menu-choose-translate (desired-value negation-flag)
  (if desired-value
      `((,(format nil
		  (getentry expect-fstr frame-io-table)
		  negation-flag
		  (first desired-value)
		  (second desired-value))
	 :no-select t)
	("" :no-select t))))


(def$method (poss-val-mixin :one-of-read-method)
	    (slot desired-value negation-flag standard-option)
  "method presenting a menu to select a value for a slot of type :one-of."
  (let* ((possible-values ($send self :get slot :possible-values))
	 (items (append `((" EXPLAIN " :value :explain))
			(or (menu-choose-translate desired-value negation-flag)
			    `(("  " :no-select t)))
			standard-option
			(get-poss-val-args possible-values)))
	 (header (getentry choose-one-of-header-str frame-io-table)))
    (do ((result (send-kb :choose-from-menu items header)
		 (send-kb :choose-from-menu items header)))
	((and (not (null result))
	      (not (eq result :explain)))
	 (send-kb :babylon-format "~S" result)
	 result)
      (when (eq result :explain)
	(cond ((eql ($send self :provide-local-help slot) 'help)
	       (send-kb :babylon-format "~:C" *help-key*)
	       (return 'help))
	      (t ($send self :prompt-for-value slot)))))))

(setf (get :one-of :supp-method) :one-of-read-method)

;;-------------------------------------------------------------------------------

(defbabylon-entry choose-some-fstr  frame-io-table english
  " Choose some of: ")

(defbabylon-entry choose-some-fstr  frame-io-table german 
  " Waehle einige Eintraege ")


(defun menu-mult-choose-translate (desired-value negation-flag)
  (if desired-value
      `((nil ,(format nil (getentry expect-fstr frame-io-table)
		      negation-flag
		      (first desired-value)
		      (second desired-value))
	     nil)
	(nil "  " nil))))

(def$method (poss-val-mixin :some-of-read-method)
	    (slot desired-value negation-flag standard-option)
  "method presenting a menu to select some values for a slot of type :some-of."
  ;;standard-option i.e. unknown undetermined
  (let* ((possible-values ($send self :get slot :possible-values))
	 (items (append
		  `((explain " EXPLAIN " (t)))
		  (or (menu-mult-choose-translate desired-value negation-flag)
		      '((nil "  " nil)))
		  (build-mult-choose-item-list
		    `(,@standard-option . ,(get-poss-val-args possible-values)))))
	 (header (getentry choose-some-fstr frame-io-table)))
    (do ((result (send-kb :mult-choose-from-menu items header)
		 (send-kb :mult-choose-from-menu items header)))
	((not (member 'explain  result))
	 (setf result 
	       (cond ((member 'help result) 'help)
		     ((null result)
		      ($send self :get-value-only slot))
		     ((and (not (null standard-option ))
			   (member (first standard-option) result))
		      (first standard-option))
		     (t (make-multiple-answer result))))
	 (send-kb :babylon-format "~S" result)
	 result)
      (cond ((eql ($send self :provide-local-help slot) 'help)
	     (send-kb :babylon-format "~:C" *help-key*)
	     (return 'help))
	    (t ($send self :prompt-for-value slot))))))
	    

(setf (get :some-of :supp-method) :some-of-read-method)

;;-------------------------------------------------------------------------------
;; entspricht  fast default-read-method ; ruft :provide-local-help

(defbabylon-entry for-explain-fstr frame-io-table english
	  "~%For explanations enter ~:C ")

(defbabylon-entry for-explain-fstr frame-io-table german
	  "~%Fuer Erlaeuterungen gib ~:C  ein")

(def$method (poss-val-mixin :any-read-method)
	    (slot desired-value negation-flag standard-option)
  "default method to support the user entering a value for slot."
  (let ((possible-values ($send self :get slot :possible-values)))
    (send-kb :babylon-format
	     (format-expectations desired-value
				  negation-flag
				  (get-poss-val-type possible-values)
				  (get-poss-val-args possible-values)))
    (send-kb :babylon-format
	     (getentry for-explain-fstr frame-io-table) *help-key*)
    ($send self :prompt-for-value slot)
    (let ((result (send-kb :babylon-read (list *help-key* *c-help-key*))))
      (cond ((eql result *c-help-key*)
	     ($send self :any-read-method
		    slot desired-value negation-flag standard-option))
	    ((or (eq result 'help)
		 (eql result *help-key*))
	     (cond ((eql ($send self :provide-local-help slot) 'help)
		    (send-kb :babylon-format "~:C" *help-key*)
		    'help)
		   (t ($send self :any-read-method
			     slot desired-value negation-flag standard-option))))
	    (t (let ((check-result ($send self :check-value slot result)))
		 (if check-result
		     (get-check-result check-result)		     
		     ($send self :any-read-method
			    slot
			    desired-value
			    negation-flag
			    standard-option))))))))

(setf (get :any :supp-method) :any-read-method)

;;-------------------------------------------------------------------------------

(def$method (poss-val-mixin :instance-of-read-method)
	    (slot desired-value negation-flag standard-option)
  "method presenting a menu to select an instance for a slot of type :instance-of."
  (let* ((possible-value-args
          (get-poss-val-args ($send self :get slot :possible-values)))
         (frame-name 
          (get-frame-name-with-check (first possible-value-args)))
       	 (items (append `((" EXPLAIN " :value :explain))
			(or (menu-choose-translate desired-value negation-flag)
			    `(("  " :no-select t)))
			standard-option
			(get-all-instances frame-name)))
	 (header (getentry choose-one-of-header-str frame-io-table)))
    (do ((result (send-kb :choose-from-menu items header)
		 (send-kb :choose-from-menu items header)))
	((and (not (null result))
	      (not (eq result :explain)))
	 (send-kb :babylon-format "~S" result)
	 result)
      (when (eq result :explain)
	(cond ((eql ($send self :provide-local-help slot) 'help)
	       (send-kb :babylon-format "~:C" *help-key*)
	       (return 'help))
	      (t ($send self :prompt-for-value slot)))))))

(setf (get :instance-of :supp-method) :instance-of-read-method)


;;; eof



