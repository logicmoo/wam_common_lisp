;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   F. di P R I M I O, J. W A L T H E R





(def$flavor rule-develop-mixin
	   ()
	   ()
  (:required-instance-variables
   meta-processor rules current-rule-set)
  (:documentation "This is the rule-develop-mixin flavor.
It provides the development environment for the rule paradigma."))


;;----------------------------------------------------------------------------

(def$method (rule-develop-mixin :select-term)
	   (term-list &optional label (max-lines nil))
  (let* ((rest-term-list (if max-lines (nthcdr max-lines term-list)))
	 (term-choice
	   ($send meta-processor :choose-from-menu			  
		 (append (mapcar #'(lambda (a-term)
				     `(,(format nil  "~S" a-term)
				       :value ,a-term))
				 (ldiff term-list rest-term-list))
			 `(("" :no-select t))
			 (if rest-term-list 
			     `(("next page" :value next)))  ;;;; !!!!!
			 `((,(getentry do-nothing-str rule-io-table)
			    :value do-nothing)))
		 label)))
    (cond ((null term-choice)
	   ($send self :select-term term-list label max-lines))
	  ((eq term-choice 'next)
	   ($send self :select-term rest-term-list label max-lines))
	  ((eq term-choice 'do-nothing) nil)
	  (t term-choice))))

;;----------------------------------------------------------------------------

(def$method (rule-develop-mixin :select-rule-set) (&optional label)
  (let ((rule-set-choice
	  ($send meta-processor :choose-from-menu
		(nconc (mapcar #'(lambda (a-rule-set)
				   `(,(format nil (getentry rule-set-fstr rule-io-table)
					      a-rule-set)
				     :value ,a-rule-set))
			       ($send self :get-rule-set-names))
		       `(("" :no-select t)
			 (,(getentry do-nothing-str rule-io-table) :value do-nothing)))
		label)))
    (cond ((null rule-set-choice)
	   ($send self :select-rule-set label))
	  ((eq rule-set-choice 'do-nothing) nil)
	  (t ($send self :get-rule-set rule-set-choice rules)))))

;;----------------------------------------------------------------------------


(def$method (rule-develop-mixin :select-rule) (rule-list &optional label (max-lines nil))
  (let* ((rest-rule-list (if max-lines (nthcdr max-lines rule-list)))
	 (rule-choice
	   ($send meta-processor :choose-from-menu			  
		 (append (mapcar #'(lambda (a-rule)
				     `(,(format nil  "~S" (rule-name a-rule))
				       :value ,a-rule))
				 (ldiff rule-list rest-rule-list))
			 `(("" :no-select t))
			 (if rest-rule-list 
			     `(("next page" :value next)))
			 `((,(getentry do-nothing-str rule-io-table)
			    :value do-nothing)))
		 label)))
    (cond ((null rule-choice)
	   ($send self :select-rule rule-list label max-lines))
	  ((eq rule-choice 'next)
	   ($send self :select-rule rest-rule-list label max-lines))
	  ((eq rule-choice 'do-nothing) nil)
	  (t rule-choice))))


;;----------------------------------------------------------------------------

;; these functions are needed for inspecting terms


(defun filter-first (term terms)
  (mapcan #'(lambda (a-term)
	      (if (match-first term a-term)
		  (list a-term)))
	  terms))


(defun filter-second (term terms)
  (mapcan #'(lambda (a-term)
	      (if (match-second term a-term)
		  (list a-term)))
	  terms))


;(defun filter-first-and-second (term terms)
;  (mapcan #'(lambda (a-term)
;	      (if (match-first-and-second term a-term)
;		  (list a-term)))
;	  terms))


(defun get-slot-of-term (term)
  (if (listp term) (second (compute-term term))))


(defun compute-used-slots (terms)
  (prog (result slot)
     loop (if (null terms)
	      (return (nreverse result)))
	  (setq slot (get-slot-of-term (first terms)))
	  (cond ((not slot) (setq terms (rest terms))
			    (go loop)))
	(cond ((member slot result)
	       (setq terms (rest terms))
	       (go loop))
	      (t (push slot result)
		 (setq terms (rest terms))
		 (go loop)))))


;; Dies wird gebraucht damit ein partielles match auch dann gemacht werden 
;; kann, wenn ein rule-term ein atomarer Freitext ist.


(def$method (rule-develop-mixin :used-terms)
	    (&optional rule-set (test #'EQUAL))
  "internal method."
  (let ((all-terms
	  (mapcan #'(lambda (a-rule)
		      (append (get-rule-conditions (rule-left-hand-side a-rule))
			      (copy-list	       ; statt append
				(get-rule-actions (rule-right-hand-side a-rule)))))
		  (rule-set-rules (or rule-set current-rule-set)))))
    (prog (result term)
       loop
	  (if (null all-terms)
	      (return (nreverse result)))
	  (setq term (pop all-terms))
	  (cond ((member term result :test test)
		 (go loop))
		(t (push term result)
		   (go loop))))))

(defun collect-term-components (terms component)
  (remove-doubles
    (mapcan #'(lambda (a-term)
		(setq a-term (compute-term a-term))
		(cond ((atom a-term) nil)
		      (t (list (funcall component a-term)))))
	    terms)))


(def$method (rule-develop-mixin :filter-terms) (terms &optional match-type)
  (let ((match-type-choice
	  (or match-type
	      ($send meta-processor :choose-from-menu
		    (getentry match-choose-item-list rule-io-table) 
		    (getentry match-choose-menu-str rule-io-table)))))
    
    (cond ((null match-type-choice)
	   ($send self :filter-terms terms))
	  ((eq match-type-choice 'do-nothing) nil)
	  ((eq match-type-choice 'equal) terms)
	  ((eq match-type-choice 'filter-first)
	   (let ((chosen-first-element
		   ($send self :select-term
				 (collect-term-components terms 'first)
				 (getentry match-choose-element-first-str rule-io-table)
				 *max-menu-entries*)))
	     (cond ((not (null chosen-first-element))
		    (filter-first `(,chosen-first-element nil) terms))
		   (t ($send self :filter-terms terms)))))
	  ((eq match-type-choice 'filter-second)
	   (let ((chosen-second-element
		   ($send self :select-term
				 (compute-used-slots terms) 
				 (getentry match-choose-slots-str rule-io-table)
				 *max-menu-entries*)))
	     (cond ((not (null chosen-second-element))
		    (filter-second `(nil ,chosen-second-element) terms))
		   ((not (null match-type)) nil)
		   (t ($send self :filter-terms terms)))))
	  ((eq match-type-choice 'filter-first-and-second)
	   (let ((filtered-terms ($send self :filter-terms terms 'filter-first)))
	     (if (null filtered-terms)
		 ($send self :filter-terms terms)
		 (or ($send self :filter-terms  filtered-terms 'filter-second)
		     ($send self :filter-terms terms))))))))


(defun gen-mult-choose-item-list (expr-list)
  (mapcar #'(lambda (expr)
	      `(,expr  ,(format nil "~A" expr) (t)))
	  expr-list))


(def$method (rule-develop-mixin :inspect-terms) (&optional (rule-set-name nil))
  (let* ((rule-set-choice
          (or (assoc rule-set-name rules)
              ($send self :select-rule-set
                     (getentry which-rule-set-to-inspect-str rule-io-table))))
         (used-terms
          (if rule-set-choice ($send self :used-terms rule-set-choice)))
         (terms-to-inspect
          (if used-terms ($send self :filter-terms used-terms)))
         (chosen-terms
          (if terms-to-inspect
            ($send meta-processor :mult-choose-from-menu
                   (gen-mult-choose-item-list terms-to-inspect)
                   (format nil (getentry match-choose-term-str rule-io-table) 
                           (length (the list terms-to-inspect))
                           (first rule-set-choice))))))    
    (if chosen-terms
      ($send self :display-terms-for-inspection chosen-terms rule-set-choice))
    (cond ((not (null rule-set-name)))
          ((null rule-set-choice))
          (t ($send self :inspect-terms)))))

(def$method (rule-develop-mixin :display-terms-for-inspection) (terms rule-set)
  (let ((term-choice
	  (if (null (rest terms))
	      (first terms)
	      ($send self :select-term
			    terms (getentry print-rule-term-header-str rule-io-table)))))
    (when term-choice
      ($send self :display-rules-for-term
		    term-choice
		    ($send self :inif term-choice rule-set)
		    ($send self :inthen term-choice rule-set))
      (if (rest terms)
	  ($send self :display-terms-for-inspection terms rule-set)))))

;;--------------------------------------------------------------------------------

(def$method (rule-develop-mixin :inspect-terms2) (&optional (rule-set-name nil))
  (let* ((rule-set-choice
	   (or (assoc rule-set-name rules)
	       ($send self :select-rule-set
			     (getentry which-rule-set-to-inspect-str rule-io-table))))
	 (used-terms
	   (if rule-set-choice ($send self :used-terms rule-set-choice)))
	 (terms-to-inspect
	   (if used-terms ($send self :filter-terms used-terms))))
    
    (if terms-to-inspect
	($send self :display-terms-for-inspection2 terms-to-inspect rule-set-choice))
    (cond ((not (null rule-set-name)))
	  ((null rule-set-choice))
	  (t ($send self :inspect-terms2)))))

(def$method (rule-develop-mixin :display-terms-for-inspection2) (terms rule-set)
  (let ((term-choice
	  ($send self :select-term
			terms (getentry print-rule-term-header-str rule-io-table))))
    (when term-choice
      ($send self :display-rules-for-term
		    term-choice
		    ($send self :inif term-choice rule-set)
		    ($send self :inthen term-choice rule-set))
      (if (rest terms)
	  ($send self :display-terms-for-inspection2 terms rule-set)))))

;;--------------------------------------------------------------------------------

(def$method (rule-develop-mixin :display-rules-for-term)
	   (a-term inif-rules inthen-rules)
  "internal method."
  (let* ((item-list
	   (append (if inif-rules
			`((- "" nil)
			  (- ,(getentry used-as-condition-str rule-io-table) nil)
			  (- "" nil)
			  ,@(mapcar #'(lambda (a-rule-body)
					`(,a-rule-body
					  ,(format nil "       ~S" (rule-name a-rule-body))
					  (t)))
				    inif-rules)))		   
		    (if inthen-rules
			`((- "" nil)
			  (- ,(getentry used-as-action-str rule-io-table) nil)
			  (- "" nil)
			  ,@(mapcar #'(lambda (a-rule-body)
					`(,a-rule-body
					  ,(format nil "       ~S" (rule-name a-rule-body))
					  (t)))
				    inthen-rules)))
		    `((- "" nil))))
	 (chosen-rules
	   ($send meta-processor :mult-choose-from-menu
		 item-list
		 (format nil (getentry list-rules-for-term-fstr rule-io-table)
			 a-term))))
    
    (if (not (null chosen-rules))
	(mapc #'(lambda (a-rule-body)
		  ;; nicht so verstaendlich
		  ;; bedeutet, dass nicht gebrowst werden soll
		  ($send self :display-rule a-rule-body nil nil nil))
	      chosen-rules))))




;;-----------------------------------------------------------------------------

(defun make-rule-header (rule-name item-len)
  (declare (fixnum item-len))
  (let ((str (format nil (getentry rule-fstr rule-io-table) rule-name)))
    `(,(COMPLETE-TO-N str (- item-len (length str)))
      :no-select t #+:lispm :font #+:lispm fonts:medfnb
      )))


(defun make-op-header (op item-len side)
  (declare (fixnum item-len))
  (let ((str (format nil " ~S  ~S" side op)))
    `(,(COMPLETE-TO-N str (- item-len (length str)))
      :no-select t #+:lispm :font #+:lispm fonts:medfnb
      )))


(defun make-term-item (term mode rules item-len spaces-for-item)
  (declare (fixnum item-len spaces-for-item))
  (let ((str (format nil "~A~A" spaces-for-item (from-list-to-string term))))
    `(,(COMPLETE-TO-N str (- item-len (length str)))
      ,@(if (null rules)
          `(:no-select t)
          `(:value (,mode ,term ,rules))))))


(def$method (rule-develop-mixin :make-rule-items)
  (rule rule-set ted-flag item-len spaces-for-item)
  `(,(make-rule-header (rule-name rule) item-len)
    ("" :no-select t)
    ,(make-op-header
      (get-junctor (rule-left-hand-side rule)) item-len 'IF)
    ,@(mapcar #'(lambda (premise)
                  (let ((trules (if ted-flag
                                  ($send self :inthen
                                         (compute-term premise) rule-set))))
                    (make-term-item premise :inthen trules item-len spaces-for-item)))
              (get-rule-conditions (rule-left-hand-side rule)))
    ("" :no-select t)
    ,(make-op-header
      (get-action-type (rule-right-hand-side rule)) item-len 'THEN)
    ,@(mapcar #'(lambda (action)
                  (let ((trules (if ted-flag
                                  ($send self :inif (compute-term action) rule-set))))
                    (make-term-item action :inif trules item-len spaces-for-item)))
              (get-rule-actions (rule-right-hand-side rule)))))


(def$method (rule-develop-mixin :display-rule)
	   (rule-body
	     &optional (rule-set nil)
	     (label-for-previous-rule nil)
	     (ted-flag t)
	     (item-len *item-width*)
	     (spaces-for-item "   "))
  "internal method."
  (setq rule-set (or rule-set current-rule-set))
  (let* ((item-list
	   ($send self :make-rule-items
		 rule-body rule-set ted-flag item-len spaces-for-item))
	 (term-choice
	   ($send meta-processor :choose-from-menu
		 item-list (or label-for-previous-rule "  "))))
    
    (if term-choice
	(let* ((rule-choice-label
		 (format nil "~A~A~A "
			 spaces-for-item
			 (second term-choice)
			 (case (first term-choice)
			   (:inif   (getentry in-lhs-str rule-io-table))
			   (:inthen (getentry in-rhs-str rule-io-table)))))
	       (rule-choice (if (null (rest (third term-choice)))
				(first (third term-choice))
				($send self :select-rule
					      (third term-choice)
					      rule-choice-label
					      *max-menu-entries*))))
	  (if rule-choice
	      ($send self :display-rule
		    rule-choice
		    rule-set
		    (format nil
			    (getentry prev-rule-fstr rule-io-table)
			    (rule-name rule-body))
		    ted-flag
		    item-len
		    spaces-for-item))))))

;;----------------------------------------------------------------------------------------
   
;; Diese Methode ist mehr oder weniger ad hoc fuer TED-Erweiterungen
;; definiert worden. (Franco)

(def$method (rule-develop-mixin :display-named-rule)
	   (rule-set-name rule-name)
  (let ((rule-set-body ($send self :get-rule-set rule-set-name))     ; the rule-base
	(rule-body ($send self :get-rule rule-set-name rule-name)))   ; the rule
    (unless (or (null rule-set-body)
		(null rule-body))
	($send self :display-rule rule-body rule-set-body))))


(def$method (rule-develop-mixin :print-rule) (&optional rule-set)
  (let* ((rule-set-choice
	   (or rule-set
	       ($send self :select-rule-set
			     (getentry in-which-rule-set-to-print-str rule-io-table))))
	 (rule-choice
	   (if rule-set-choice
	       ($send self :select-rule
			     (rest rule-set-choice)
			     (format nil (getentry which-rule-to-print-fstr rule-io-table)
				     (first rule-set-choice))
			     *max-menu-entries*))))
    (when rule-choice
      ($send self :display-rule rule-choice rule-set-choice)
      ($send self :print-rule rule-set-choice))))


;;; eof

