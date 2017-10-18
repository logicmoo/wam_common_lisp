;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: FMCS -*-

(in-package "FMCS")

;;; --------------------------------------------------------------------------
;;;  Hand coded object standard-class  
;;; --------------------------------------------------------------------------

(setq STANDARD-CLASS 
      (make-mcsobject 
       :env 
       (vector 
	   'isit           ; :isit  will be set below
	   'standard-class ; :name
	   nil             ; :supers
	   nil             ; :cplist
	   '(isit          ; :all-slots
	     name supers cplist all-slots all-slot-defaults own-slots
	     methods basicnew-fn slot-accessor-fn subclasses)
	  '((name nil)     ; :all-slot-defaults 
	    (supers nil)(cplist nil)(all-slots nil)(all-slot-defaults nil)
	    (own-slots nil)(methods (make-hash-table :test #'eq))
	    (basicnew-fn nil)(slot-accessor-fn nil) (subclasses nil))
	  '(name           ; :own-slots 
	     supers cplist all-slots all-slot-defaults own-slots 
	     methods basicnew-fn slot-accessor-fn subclasses)
	  (make-hash-table :test #'eq)   ; :methods
          ; :basicnew-fn 
             #'(lambda (isit &key (name nil) (supers nil) (own-slots nil))
		(send-fast
		  (make-mcsobject 
		    :env
		    (vector isit name supers nil nil nil own-slots 
			    (make-hash-table :test #'eq)
			    nil nil nil))
		  :init name supers own-slots))
             ; :slot-accessor-fn
            #'(lambda (slot)
              (case slot
                (isit              (index-of-isit)) 
                (name              (index-of-name)) 
                (supers            (index-of-supers)) 
                (cplist            (index-of-cplist))
                (all-slots         (index-of-all-slots)) 
                (own-slots         (index-of-own-slots)) 
                (all-slot-defaults (index-of-all-slot-defaults))
                (methods           (index-of-methods)) 
                (basicnew-fn       (index-of-basicnew-fn)) 
                (slot-accessor-fn  (index-of-slot-accessor-fn))
                (subclasses        (index-of-subclasses))
                (t (error "no slot"))))
          nil            ; :subclasses
          )))


;;; Slot 'isit of standard-class have to be set to itself

(setf (svref (mcs-env standard-class) (index-of-isit)) standard-class)


;;; ---- INSTANCE CREATOR METHOD ----

(defmethod (standard-class :new) (&rest inits)
  (apply (mcs-get-slot inst-env (index-of-basicnew-fn))
         self inits))

;;; ---- INITIALIZE METHOD ----

(defmethod (standard-class :init) (&rest inits)
  (declare (ignore inits))
  (send-self :compute-cplist) 
  (send-self :inherit-slots-with-defaults)
  (send-self :compute-slot-accessor-fn)
  (send-self :extend-subclasses-of-supers)
  (send-self :compute-slot-access-methods)
  (send-self :compute-basicnew-fn)
  self)


;;; --------------------------------------------------------------------------
;;;  Hand coded object standard-object 
;;; --------------------------------------------------------------------------

(setq STANDARD-OBJECT
      (make-mcsobject 
       :env 
        (vector 
	  standard-class    ; :isit
	  'standard-object  ; :name
	  nil               ; :supers
          nil               ; :cplist 
          '(isit)           ; :all-slots 
	  nil               ; :all-slot-defaults
          '(isit)           ; :own-slots 
	  (make-hash-table :test #'eq) ; :methods 
	  #'(lambda (isit)  ; :basicnew-fn 
		      (send-fast
			(make-mcsobject :env (vector isit))
			:init))
	  #'(lambda (slot)   ; :slot-accessor-fn
		      (case slot
			(isit  (index-of-isit))
			(t (error "no slot"))))
	  (list standard-class) ; :subclasses
          )))

(setf (slot-value standard-object 'cplist) (list standard-object))
(setf (slot-value standard-class 'supers) (list standard-object))
(setf (slot-value standard-class 'cplist) (list standard-class standard-object))

;;; ---- INITIALIZE METHOD ----

(defmethod (standard-object :init) (&rest inits)
  (declare (ignore inits))
  self)
