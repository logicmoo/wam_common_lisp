;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; BASE: 10. ;Package: BABYLON -*-

(in-package "BABYLON")


;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     April 1987
;; AUTHORS:  Franco di Primio, Eckehard Gross

;; This file depends on:  common>*
;;                        frames>basic>frames
;;                        

;; contents: a mixin making the facilities of basic-frame-processor available
;;           for a knowledge base.

;;--------------------------------------------------------------------------
;;                   FLAVOR BASIC-FRAME-MIXIN
;;--------------------------------------------------------------------------

(def$flavor basic-frame-mixin
        (frame-processor
	 (pkg nil) ;; the package where to put frames and instances
	           ;; just as a hash table
	 (frames nil)
	 (instances nil))
	()
  :settable-instance-variables
  (:required-instance-variables
    procs kb-name active-proc system-trace system-trace-window)
  (:documentation "This mixin makes the facilities of basic-frame-processor available."))


(def$method (basic-frame-mixin :after :init) (&rest plist)
  (declare (ignore plist))
  ($send self :set-up-prefix)  
  ($send self :generate-frame-processor)
  (setf procs (cons frame-processor procs)))

(def$method (basic-frame-mixin :set-up-prefix) ()
  "generates a package for the names of frames and instances."
  (let ((pkg-name (or pkg kb-name)))
    (setf pkg (or (find-package pkg-name)
		  (make-package pkg-name :use nil #+:lispm :size #+:lispm 200.)))))

(def$method (basic-frame-mixin :generate-frame-processor) ()
  "generates an instance of basic-frame-processor associated with the kb."
  (setf frame-processor (make-$instance 'basic-frame-processor
				       :meta-processor self)))


(def$method (basic-frame-mixin :set-up-frame-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'frame table)
	     :frame (gethash 'frame-commands table)))))


(def$method (basic-frame-mixin :toggle-frcheck) ()
  #-:FMCS ($send frame-processor :toggle-frcheck)
  #+:FMCS (setf *redefine-warnings* ($send frame-processor :toggle-frcheck)))

;;--------------------------------------------------------------------------
;;                  METHODS FOR FRAME OR INSTANCE CONSTRUCTION 
;;--------------------------------------------------------------------------


(def$method (basic-frame-mixin :add-to-frames) (a-frame-name)
  (cond ((null frames)
	 (setf frames `(,a-frame-name))
	 ($send frame-processor :set-frames-list frames))
	((member a-frame-name frames))
	(t (setf frames (nconc frames `(,a-frame-name))))))


(defmacro defframe (frame-name &body body)
  (and (current-kb-typep 'basic-frame-mixin)
       ($send (send-kb :frame-processor) :new-frame-form frame-name body)))


(defmacro defbehavior (behavior-spec lambda-list &body behavior-body)
  (and (current-kb-typep 'basic-frame-mixin)
       ($send (send-kb :frame-processor)
	     :new-behavior-form behavior-spec lambda-list behavior-body)))


(def$method (basic-frame-mixin :add-to-instances) (an-instance-name)
  (cond ((null instances)
	 (setf instances `(,an-instance-name))
	 ($send frame-processor :set-instances-list instances))
	((member an-instance-name instances))
	(t (setf instances (nconc instances `(,an-instance-name))))))


(defmacro definstance (instance-name of frame-name &body body)
  (declare (ignore of))
  `(let ()
     (declare (special ,instance-name))
     (and (current-kb-typep 'basic-frame-mixin) 
	  ($send (send-kb :frame-processor) :new-instance
	         ',instance-name ',frame-name ',body))))

;; Diese Funktion sollte in die Dokumentation aufgenommen werden.
;; changed: Franco 23.10.

(defmacro create-unnamed-instance
	  (instance-name frame-name &optional with-specification)
  (and (current-kb-typep 'basic-frame-mixin) 
       ($send (send-kb :frame-processor) :new-unnamed-instance
	     instance-name frame-name with-specification)))

(defun create-instance-of (frame-name &optional with-specification)
  "Diese Funktion erlaubt, Instanzen dynamisch zu definieren.
   Die Instanzen, die damit kreiert werden, koennen nur mit SEND
   angesprochen werden. Sie sind fuer die Wissensbasis und den Frame
   nicht zugaenglich. In der Umgebung der Instanz ist object-name
   wie self gebunden.
   frame-name muss der name eines Frames sein (wird evaluiert)
   with-specification (wird evaluiert) und ermoeglicht eine Initialisierung
   mit der gleichen Syntax und Semantik wie DEFINSTANCE."
  (and (current-kb-typep 'basic-frame-mixin) 
       ($send (send-kb :frame-processor) :new-unnamed-instance
	     (gensym) frame-name with-specification)))


;;--------------------------------------------------------------------------
;;                  METHODS FOR REQUEST EVALUATION 
;;--------------------------------------------------------------------------


(defun %is-frame-name (name prefix)
  ;returns nil if x is not a symbol
  (let ((object-internal-name (%get-object-name name prefix)))
    (if object-internal-name 
	(frame-definition object-internal-name))))


(defun %is-instance-name (name prefix)
  (let ((instance-internal-name (%get-object-name name prefix)))
    (if instance-internal-name 
	(instance-definition instance-internal-name))))


(defun %is-behavior (selector instance prefix)
  (and (keywordp selector)
       ($send (get (%get-object-name instance prefix) :instance)
		     :operation-handled-p selector)))

(defun %is-slot (slot instance prefix)
  (member slot ($send (get (%get-object-name instance prefix) :instance) :slots)))



#+:SABN(defmacro frame-type (request)		
        `(if (is-true-list ,request)
           (cond ((%is-instance-name (first ,request) pkg)
                  'frame-reference)
                 ((is-frame-meta-predicate (first ,request))
                  'frame-meta-predicate-reference)
                 ((%is-frame-name (first ,request) pkg)
                  'frame-class-reference)
                 ((and (%is-instance-name (second ,request) pkg)
                       (%is-slot (first ,request) (second ,request) pkg))
                  'frame-predicate-reference)
                 ((and (%is-instance-name (third ,request) pkg)
                       (%is-slot (second ,request) (third ,request) pkg))
                  'frame-predicate-reference)
                 ((and (%is-instance-name (second ,request) pkg)
                       (%is-behavior (first ,request) (second ,request) pkg))
                  'behavior-reference))))

#-:SABN(defmacro frame-type (request)		
        `(if (is-true-list ,request)
           (cond ((%is-instance-name (first ,request) ($slot 'pkg))
                  'frame-reference)
                 ((is-frame-meta-predicate (first ,request))
                  'frame-meta-predicate-reference)
                 ((%is-frame-name (first ,request) ($slot 'pkg))
                  'frame-class-reference)
                 ((and (%is-instance-name (second ,request) ($slot 'pkg))
                       (%is-slot (first ,request) (second ,request) ($slot 'pkg)))
                  'frame-predicate-reference)
                 ((and (%is-instance-name (third ,request) ($slot 'pkg))
                       (%is-slot (second ,request) (third ,request) ($slot 'pkg)))
                  'frame-predicate-reference)
                 ((and (%is-instance-name (second ,request) ($slot 'pkg))
                       (%is-behavior (first ,request) (second ,request) ($slot 'pkg)))
                  'behavior-reference))))	   

(assign-typefkt 'frame-type 'basic-frame-mixin)


(defrequest frame-reference
	    :recall             :eval-frame-reference
            :recall-immediate   :eval-frame-reference
	    :remember           :eval-frame-reference
	    :store              :eval-frame-reference
	    :ask                :ask-eval-frame-reference
	    :prolog             :eval-prolog-frame-reference)



;;--------------------------------------------------------------------------


(def$method (basic-frame-mixin :eval-frame-reference) (frame-reference mode)
  "evaluates a frame reference."
  (when system-trace
    ($send self :send-system-trace-window :format
		  (getentry meta-frame-trace-fstr frame-io-table) mode frame-reference))
  (setf active-proc frame-processor)
  ($send frame-processor :eval-reference
	frame-reference
	(if (eq mode :recall-immediate) :recall mode)))

;;--------------------------------------------------------------------------

(def$method (basic-frame-mixin :ask-with-help)
	   (request &optional (negation-flag nil))
  (let ((answer ($send frame-processor :ask request negation-flag)))
    (cond ((is-help answer)
	   (case ($send self :help)
	     (why 'why)
	     (t   ($send self :ask-with-help request negation-flag))))
	  (t answer))))

(def$method (basic-frame-mixin :ask-eval-frame-reference)
	   (frame-reference mode &optional (negation-flag nil))
  "evaluates a frame reference after asking the user."
  (when system-trace
    ($send self :send-system-trace-window :format 
		  (getentry meta-frame-trace-fstr frame-io-table) mode frame-reference))
  (setf active-proc frame-processor)
  (let ((answer ($send self :ask-with-help frame-reference negation-flag)))
    (case answer
      (why 'why)
      (t (setq answer
	       ($send frame-processor :eval-reference frame-reference :recall))
	 (if answer
	     'true
	     'false)))))

;;--------------------------------------------------------------------------

(def$method (basic-frame-mixin :eval-prolog-frame-reference) (frame-reference mode)
  "evaluates frame references used in prolog clauses."
  (when system-trace
    ($send self :send-system-trace-window :format 
		  (getentry meta-frame-trace-fstr frame-io-table) mode frame-reference))
  (setf active-proc frame-processor)
  (prog (answer)
     A  (setq answer
	      ($send frame-processor :eval-reference  frame-reference :recall))
	(cond ((is-undetermined answer)
	       (setq answer ($send self :ask-with-help frame-reference nil))
	       (if (eq answer 'why)
		   ($send self :prolog-why))  ;indirect prolog-ref
	       (go A))
	      ((null answer) (return nil))
	      ((eq answer 'unknown) (return nil))
	      (t (return t)))))


;;--------------------------------------------------------------------------

(defrequest frame-meta-predicate-reference
	    :prolog :eval-frame-meta-predicate-reference)


;(defvar *frame-meta-predicates* '(and or))

(defun is-frame-meta-predicate (x)	
  (member x *frame-meta-predicates*))


(def$method (basic-frame-mixin :eval-frame-meta-predicate-reference) (goal mode)
  "evaluates references of a special type used in prolog.
goal := (<meta-predicate> . <args>)"
  (when system-trace
    ($send self :send-system-trace-window :format 
		  (getentry meta-frame-trace-fstr frame-io-table) mode goal))
  (setf active-proc frame-processor)
  (let ((result nil))
    (case (first goal)
      (FRAME (let ((the-frames ($send frame-processor :frames-list)))
	       (if (IS-VARIABLE (second goal))
		   (dolist (a-frame the-frames (nreverse result))
		     (setf result (cons `((,(first goal) ,a-frame)) result)))
		   (if (member (second goal) the-frames)
		       t
		       nil))))
      (FRAME-DEF (let ((the-frames ($send frame-processor :frames-list)))
		   (dolist (a-frame the-frames (nreverse result))
		     (setf result
			   (cons `((,(first goal) ,(get-frame-def a-frame))) result)))))
      ((HAS-SUPER SUPER)
       (if (IS-VARIABLE (second goal))
	   nil
	   (let ((supercls (GET-ALL-SUPERS (second goal))))
	     (cond ((IS-VARIABLE (third goal))
		    (dolist (a-superc supercls (nreverse result))
		      (setf result
			    (cons `((,(first goal) ,(second goal) ,a-superc)) result))))
		   (t (if (member (third goal) supercls)
			  t
			  nil))))))
      (INSTANCE (let ((the-instances ($send frame-processor :instances-list)))
		  (if (IS-VARIABLE (second goal))
		      (dolist (an-instance the-instances (nreverse result))
			(setf result (cons `((,(first goal) ,an-instance)) result)))
		      (if (member (second goal) the-instances)
			  t
			  nil))))
      (INSTANCE-DEF (let ((the-instances ($send frame-processor :instances-list)))
		      (dolist (an-instance the-instances (nreverse result))
			(setf result
			      (cons `((,(first goal) ,(get-instance-def an-instance)))
				    result)))))
      ((HAS-SLOT SLOT)
       (if (IS-VARIABLE (second goal))
	   nil
	   (let ((slots ($send (get-instance (second goal)) :slots))) 
	     (if (IS-VARIABLE (third goal))
		 (dolist (a-slot slots (nreverse result))
		   (setf result (cons `((,(first goal) ,(second goal) ,a-slot)) result)))
		 (if (member (third goal) slots)
		     t
		     nil)))))
      ((HAS-PROPERTY PROPERTY)
       (if (or (IS-VARIABLE (second goal))
	       (IS-VARIABLE (third goal)))
	   nil
	   (let ((instance-name (third goal))
		 (slot-name (second goal))
		 (desired-value (fourth goal)))
	     (let ((prop-names
		     ($send (get-instance instance-name) :GET-PROPERTIES slot-name)))
	       (if (IS-VARIABLE desired-value)
		   (dolist (a-prop-name prop-names (nreverse result))
		     (setf result
			   (cons `((,(first goal) ,slot-name ,instance-name ,a-prop-name))
				 result)))
		   (if (member desired-value prop-names) t nil))))))
      
      (t ;; signal error !!
	nil))))


;;--------------------------------------------------------------------------


(defrequest frame-class-reference
	    :prolog :eval-frame-class-reference)

(def$method (basic-frame-mixin :eval-frame-class-reference)
	   (goal mode)
    "evaluates references of a special type used in prolog.
goal = (<frame-name> <variable>) | (<frame-name> <instance-name>)"
  (when system-trace
    ($send self :send-system-trace-window :format
		  (getentry meta-frame-trace-fstr frame-io-table) mode goal))
  (setf active-proc frame-processor)
  (cond ((IS-VARIABLE (second goal))
	 ;; Hier werden CLAUSES der Form ((<frame-name> <instance-name>))
	 ;; uebergeben.
	 ;; Es findet eine Uebersetzung statt! (Nicht so effizient!)
	 (let ((result nil))
	   (dolist (an-instance (GET-ALL-INSTANCES (first goal)) (nreverse result))
	     (setf result (cons `((,(first goal) ,an-instance)) result)))))
	((IS-INSTANCE (second goal))
	 ;; Hier kann ein direkter Typecheck gemacht werden (effizient!)
	 ;; Der prolog-processor versteht T und NIL.
	 ;; Man beachte, dass an dieser Stelle der FRAME-PROZESSOR umgangen wird.
	 ;; Es wird aber etwas benutzt (die Nachricht :TYPE), was in jeder
	 ;; sogenannten Wissensbasis auch benutzt werden kann.
	 (if ($send (GET-INSTANCE (second goal)) :type (first goal))
	     t
	     nil))))

;;--------------------------------------------------------------------------

(defrequest frame-predicate-reference
	    :prolog :eval-frame-predicate-reference)

(def$method (basic-frame-mixin :get-ask) (object slot prop-name)
  (when system-trace
    ($send self :send-system-trace-window :format
	  (getentry meta-frame-trace-one-fstr frame-io-table)
	  `(:get ,object ,slot ,prop-name)))
  (setf active-proc frame-processor)
  (prog (answer)
     A  (setq answer ($send frame-processor :get object slot prop-name))
	(cond ((is-undetermined answer)
	       (setq answer ($send self :ask-with-help
					  `(,object ,slot ,prop-name) nil))
	       (if (eq answer 'why)
		   ($send self :prolog-why))	;indirect prolog-ref
	       (go A))
	      (t (return answer)))))
  

(defun make-clauses (pred an-instance-name values &rest prop-name)
  "make clauses according to Prolog syntax."
  (cond ((IS-MULTIPLE-VALUE values)
	 (mapcar #'(lambda (a-value)
	      `((,@prop-name ,pred ,an-instance-name ,a-value)) )
		 (rest values)))
	(t `(((,@prop-name ,pred ,an-instance-name ,values))))))

(def$method (basic-frame-mixin :eval-frame-predicate-reference) (goal mode)
  "evaluates frame references of a syntactical form used by prolog.
goal:= (<slot> <object> <arg3>) | (<prop-name> <slot> <object> <arg3>)"
  (declare (ignore mode))
  (let* ((normalized-goal (cond ((keywordp (first goal)) goal)
				(t `(:value . ,goal))))
	 (prop-name (first normalized-goal))
	 (slot (second normalized-goal))
	 (object (third normalized-goal))
	 (desired-value (fourth normalized-goal)))
    (cond ((IS-VARIABLE desired-value)	   	   
	   (let ((value ($send self :get-ask object slot prop-name)))
	     (if (eq normalized-goal goal)		 
		 (make-clauses slot object value prop-name)
		 (make-clauses slot object value))))
	  (t (let ((premise `(,object ,slot ,prop-name = ,desired-value)))
	       ($send self :eval-prolog-frame-reference premise :RECALL))))))


;;--------------------------------------------------------------------------


(defrequest behavior-reference
	    :prolog :eval-behavior-reference)


(def$method (basic-frame-mixin :eval-behavior-reference) (goal mode)
  "evaluates behavior references of a syntactical form used by prolog."
  (when system-trace
    ($send self :send-system-trace-window :format
		  (getentry meta-frame-trace-fstr frame-io-table) mode goal))
  (setf active-proc frame-processor)
  (let ((behavior (first goal))
	(the-instance (second goal))
	(args (butlast (cddr goal)))
	(result (first (last goal))))
    (if (CONTAINS-VARS args)
	nil
	(let ((call-result
		(lexpr-$send (GET-INSTANCE the-instance) behavior args)))
	  (if (IS-VARIABLE result) ;; gives a list of clauses as result
	      `(((,behavior ,the-instance ,@args ,call-result)))
	      ;; else T or NIL	      
	      (equal result call-result))))))

               
;;--------------------------------------------------------------------------
;;              METHODS FOR HANDLING OBJECTS
;;--------------------------------------------------------------------------


(def$method (basic-frame-mixin  :describe-frame)
	   (frame-name &optional window)
  "describes the frame named <frame-name> on <window>.
if <window> is not specified the dialog-stream is used instead."

  (setf window (or window self))
  ($send window :format  "~%Frame: ~S" frame-name)
  ($send window :format 
	"~%Slots: ~{~S ~}"
	(compute-slot-names (get-frame-slots frame-name)))
  ($send window :format 
	"~%Supers: ~{~S  ~}"
	(get-supers frame-name))
  ($send window :format 
	"~%Behaviors: ~{~{~* ~S ~^ ~S~} ~}"
	(get-frame-behavior-specs frame-name))
  ($send window :format 
	"~%Instances: ~{~S ~}"
	(get-instance-list frame-name))
  ($send window :format "~%"))


(def$method (basic-frame-mixin :select-describe-frame) (&optional window)
  "describes a frame selected via menu on <window>.
if <window> is not specified the dialog-stream is used instead."

  (do* ((items (append frames `(,(getentry exit-select-item frame-io-table))))
	(label (format nil
			(getentry describe-which-question-str frame-io-table)
			'frame))
	(frame-name ($send self :choose-from-menu items label)
		    ($send self :choose-from-menu items label)))
       ((eq frame-name 'exit) t)
    (if (not (null frame-name))
	($send self :describe-frame frame-name window))))


(def$method (basic-frame-mixin :inspect-frames) ()
  ($send self :select-describe-frame))



(def$method (basic-frame-mixin :describe-instance)
	   (instance-name &optional (all-properties t) window)
  "describes the instance named <instance-name> on <window>.
the value of each slot and all its properties are shown unless <all-properties> is nil.
if <window> is not specified the dialog-stream is used instead."

  (let ((unparsed-instance ($send (get-instance instance-name)
					 :unparse-instance nil all-properties)))
    (setf window (or window self))
    ($send window :format  "~%Instance: ~S of Frame: ~S"
		  instance-name (second unparsed-instance))
    (mapc #'(lambda (unparsed-slot)
	      (cond (all-properties
		     ($send window :format
				   "~%Slot: ~S" (first unparsed-slot))
		     ($send window :format
				   "~{~%      ~S ~S~}" (rest unparsed-slot)))
		    (t ($send window :format
				     "~%Slot: ~{~S  ~S ~S~}" unparsed-slot))))
	  (cddr unparsed-instance))
    ($send window :format "~%")))


(def$method (basic-frame-mixin :select-describe-instance)
	   (&optional (all-properties t) window)
  "describes an instance selected via menu on <window>.
the value of each slot and all its properties are shown unless <all-properties> is nil.
if <window> is not specified the dialog-stream is used instead."
 
  (do* ((items (append instances `(,(getentry toggle-mode-item frame-io-table)
				   ,(getentry exit-select-item frame-io-table))))
	(label (format nil
			(getentry describe-which-question-str frame-io-table)
			'instance))
	(instance-name ($send self :choose-from-menu items label)
		       ($send self :choose-from-menu items label)))
       ((eq instance-name  'exit) t)
    (cond ((eq instance-name 'mode)
	   (setq all-properties (if (null all-properties) t)))
	  ((not (null instance-name))
	   ($send self :describe-instance
			 instance-name all-properties  window)))))


(def$method (basic-frame-mixin :inspect-instances) ()
  ($send self :select-describe-instance nil))



;;--------------------------------------------------------------------------
;;                        AUX STUFF
;;--------------------------------------------------------------------------

(defun send-fp (message &rest args)
  (lexpr-$send (send-kb :frame-processor) message args))
