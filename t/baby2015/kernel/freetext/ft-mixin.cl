;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10. -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  Eckehard Gross, Juergen Walther



(def$flavor free-text-mixin
        (free-text-processor)
	()
  :settable-instance-variables
  (:required-instance-variables
   kb-name procs active-proc system-trace system-trace-window))

(def$method (free-text-mixin :after :init) (&rest plist)
  (declare (ignore plist))
  ($send self :generate-free-text-processor)
  (setf procs (cons free-text-processor procs)))

(def$method (free-text-mixin :generate-free-text-processor) ()
  (setf free-text-processor (make-$instance 'basic-free-text-processor
					    :meta-processor self)))

(defrequest free-text
	    :prolog             :eval-free-text-for-prolog
            :recall             :eval-free-text
	    :recall-immediate   :eval-free-text
            :remember           :eval-free-text
            :store              :eval-free-text
            :ask                :ask-user)

(defmacro free-text-type (request)
  (declare (ignore request))
	   ''free-text)

(assign-typefkt 'free-text-type 'free-text-mixin)


(def$method (free-text-mixin :ask-user)
	   (fact mode  &optional (negation-flag nil))
  (when system-trace
    ($send self :send-system-trace-window :format
	   (getentry meta-free-text-trace-fstr free-text-io-table)
	   mode fact))
  (setf active-proc free-text-processor)
  (prog (answer)
     A (setq answer
	     ($send free-text-processor :ask-user fact negation-flag))
	(case answer
	  (help	(if (eq ($send self :help) 'why)
		    (return 'why)
		    (go A)))
	  (t (return answer)))))


(def$method (free-text-mixin :eval-free-text) (fact mode)
  (when system-trace
    ($send self :send-system-trace-window :format 
	   (getentry meta-free-text-trace-fstr free-text-io-table)
	   mode fact))
  (setf active-proc free-text-processor)
  (case mode
    ((:RECALL :RECALL-IMMEDIATE)
               ($send free-text-processor :recall fact))
    (:REMEMBER ($send free-text-processor :remember fact))
    (:STORE    ($send free-text-processor :store fact))))



(def$method (free-text-mixin :ask-user-for-prolog) (fact mode)
  (declare (ignore mode))
  (prog (answer)
     A (setq answer
	     ($send free-text-processor :ask-user fact))
	(case answer
	  (help	(if (eq ($send self :help) 'why)
		    ($send self :prolog-why))
		(go A))
	  (TRUE (return t))
	  ((UNKNOWN FALSE) (return nil))
	  ;; (prompt (return ($send self :read-clauses)))
	  (t ;; signal error!! (define entry in deutsch and english)
	   (return answer)))))

(defun translate-free-texts-into-prolog-facts (facts)
  (mapcar #'list facts))

(defun is-free-text-meta-predicate (x)	
  (member x *free-text-meta-predicates*))



(def$method (free-text-mixin :eval-free-text-for-prolog)
	   (request mode)
  (when system-trace
    ($send self :send-system-trace-window :format
	   (getentry meta-free-text-trace-fstr free-text-io-table)
	   mode request))
  (setf active-proc free-text-processor)
  (cond ((and (consp request)
	      (is-free-text-meta-predicate (first request)))
	 ($send self :eval-free-text-meta-predicate request mode))
	((and (consp request)
	      (CONTAINS-VARS request))
	 ;; if the request contains variables, then return all true requests
	 ;; which can be unified with request.
	 (let* ((predicate (first request))
		(true-facts
;		  ;; this is unfortunately not possible from the prolog perspective!
;		  (if (IS-VARIABLE predicate)
;		      ($send free-text-processor :true-facts)
;		      ($send free-text-processor :get-true-facts-for predicate))
		  ($send free-text-processor :get-true-facts-for predicate)))
	   (cond ((null true-facts) nil)
		 (t (translate-free-texts-into-prolog-facts true-facts)))))
	(t ;; else establish the state of request:
	 ;; if the state is determined, return it
	 ;; else ask the user.
	 (let ((answer ($send free-text-processor :recall request)))
	   (if (IS-UNDETERMINED answer)
	       (setq answer ($send self :ask-user-for-prolog request mode)))
	   answer))))

(def$method (free-text-mixin :eval-free-text-meta-predicate)
	   (request mode)
 (declare  (ignore mode))
  (case (first request)
    (FREE-TEXT (let* ((true-facts ($send free-text-processor :true-facts))
		      (false-facts ($send free-text-processor :false-facts))
		      (fact (second request))
		      (all-facts (append true-facts false-facts))
		      (result nil))
		 (cond ((CONTAINS-VARS fact)
			(cond ((and (consp fact)	;;index on first element
				    (not (IS-VARIABLE (first fact))))
			       (dolist (a-fact all-facts (nreverse result))
				 (when (and (consp a-fact)
					    (equal (first a-fact) (first fact)))
				   (setf result
					 (cons `((,(first request) ,a-fact)) result)))))
			      (t (dolist (a-fact all-facts (nreverse result)) 
				   (when (consp a-fact)
				     (setf result
					   (cons `((,(first request) ,a-fact)) result)))))))
		       ((member fact all-facts :test #'equal) t)
		       (t nil))))
  (t ;; signal error!
    nil)))


;;; eof

