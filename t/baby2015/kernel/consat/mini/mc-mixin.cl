;;; -*- Mode: LISP; Syntax: Common-lisp; Package: BABYLON; Base: 10 -*-


(in-package "BABYLON")

;
;	MINI-CONSTRAINT-MIXIN
;


(def$flavor mini-constraint-mixin
	((consat-trace-window nil))
	(basic-constraint-mixin)
  
  :settable-instance-variables
  (:required-instance-variables procs kb-name)
  (:documentation "Anteil des Constraint-Systems am Metaprozessor")
  )  



(def$method (mini-constraint-mixin :generate-constraint-processor) ()
	   
  "erzeugt einen Constraint-Prozessor "

  (setf constraint-processor
	(make-$instance 'mini-constraint-processor
			:meta-processor self)))


(def$method (mini-constraint-mixin :set-up-constraint-cmds) ()
  (let ((table (get 'cmd-table ($send self :language))))
    (when (and table ($send self :operation-handled-p :add-operations))
      ($send self :add-sub-operations
	     :top (gethash 'consat table)
	     :consat (gethash 'consat-commands table))
      ($send self :add-operations
	     :consat (gethash 'consat-trace-commands table)))))


(assign-typefkt 'constraint-type 'mini-constraint-mixin)



(def$method (mini-constraint-mixin :send-consat-trace-window) (selector &rest args)
  "passes messages to consat-trace-window."
  (lexpr-$send consat-trace-window selector args))



(defun trace-constraints ()
  
  (catch 'no-select
    (send-kb :choose-trace-mode)))


(def$method (mini-constraint-mixin :choose-trace-mode) ()
  
  "ermoeglicht dem Benutzer das An- und Ausschalten von trace-Modes"
  
  (let ((constr ($send self :choose-c-type)))
    (update-constraint-trace-mode
      (choose-constraint-trace-mode
	(build-constraint-trace-item-list
	  constr))
      constr)))


(defun constraint-assoc-tracedp (constraint-assoc)
  ($send (get-object-of-c-assoc constraint-assoc)
	 :send-if-handles :traced-p))


(def$method (mini-constraint-mixin :update-constraint-trace) ()
  ($send constraint-processor :set-trace
	 (or (some #'constraint-assoc-tracedp constraints)
	     (some #'constraint-assoc-tracedp constraint-nets))))


(defun build-constraint-trace-item-list (constraint-list)
  (cons (getentry mark-explain-item constraint-io-table)
	(mapcar #'(lambda (constraint-assoc)
		    `(,constraint-assoc
		      ,(format nil "~A ~S"
			       (if (constraint-assoc-tracedp constraint-assoc)
				   "#"
				   " ")
			       (get-name-of-c-assoc constraint-assoc))
		      (t)))
		constraint-list)))


(defun choose-constraint-trace-mode (trace-item-list)
  
  "bittet den Benutzer um die Wahl der Constraints,
   deren Trace-Modes umgeschaltet werden sollen"
  
  (send-kb :mult-choose-from-menu
	   trace-item-list
	   (getentry toggle-trace-modes constraint-io-table)))


(defun update-constraint-trace-mode (result-item-list constraints)
  
  "alle Constraints in result-item-list erhalten die Nachricht :trace-on,
falls sie nicht protokolliert werden, bzw. :trace-off im entgegengesetzten Fall."
  
  (declare (ignore constraints))
  (when result-item-list 
    (mapc #'(lambda (constraint-assoc)
              (setf (get-object-of-c-assoc constraint-assoc)
		    ($send (get-object-of-c-assoc constraint-assoc)
			   (if (constraint-assoc-tracedp constraint-assoc) 
			     :trace-off
			     :trace-on)
			   (get-name-of-c-assoc constraint-assoc))))
          result-item-list)
    (send-kb :update-constraint-trace)))


(def$method (mini-constraint-mixin :protocol) (expr-type expr)
  
  " Eingabe: expr-type = :enter, :exit, :choice"
  
  ($send self :send-consat-trace-window :format "~A"
	 (case expr-type
	   (:enter  (format nil "    enter:  ~S" expr))
	   (:exit   (format nil "    exit:   ~S" expr))
	   (:choice (format nil "CHOICE:  ~S = ~S"
			    (get-var expr) (first (get-value-spec expr))))
	   (:fail   (format nil (getentry fail constraint-io-table) expr)))))



;;; eof

