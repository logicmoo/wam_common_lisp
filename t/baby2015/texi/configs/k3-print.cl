;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; adopted to BABYLON 2.3 by Juergen Walther 30.5.94
;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89


(eval-when (eval compile load)   
  (bab-require 'normal-constraint-mixin)
  (bab-provide 'normal-constraint-mixin))


(defun print-constraints (&optional (file-or-stream nil))
  
  ;;; prints restriction nets and primitive and compound constraints
  ;;; of *current-knowledge-base*
  ;;; Note: There is also a corresponding method of :constraint-processor
  
  (if file-or-stream
    (with-open-file (outfile file-or-stream :direction :output)
      (pr-constraints-to-what outfile))
    (pr-constraints-to-what (send-kb :dialog-stream))))


(defun pr-constraints-to-what (outfile)
  (format outfile "Restrictions of ~S:" *current-knowledge-base*)
  (terpri outfile)(terpri outfile)
  (print-restrictions (send-kb :restriction-nets) outfile)
  (terpri outfile) (terpri outfile)
  (format outfile "Primitive Constraints of ~S:" *current-knowledge-base*)
  (terpri outfile)(terpri outfile)
  (print-primitive-constraints (send-kb :constraints) outfile)
  (terpri outfile) (terpri outfile) 
  (format outfile "Compound Constraints of ~S:" *current-knowledge-base*)
  (terpri outfile)(terpri outfile)
  (print-compound-constraints (send-kb :constraint-nets) outfile)
  (terpri outfile) (terpri outfile))



(defun print-restrictions (name-restr-list &optional stream)
  
  ; name-restr-list is a list ((name1 restr1) ... (name-N restr-N))
  ; where name-i is the name of a restriction net and 
  ;       restr-i is a restriction instance
  
  (cond ((null name-restr-list))
        (t (print-a-restriction-net (car name-restr-list) stream)
           (terpri stream)
           (print-restrictions (cdr name-restr-list) stream))))


(defun print-a-restriction-net (a-restriction &optional stream) 
  (let* ((name (car a-restriction))
         (restr-inst (cdr a-restriction))
         (restr-list ($send restr-inst :restrictions)))
    (print (format stream "Restrictions of Restriction Net ~S (~S):" name restr-inst))
    (terpri stream)
    (dolist (restr-elem restr-list)
      (print restr-elem stream))))

(defun print-primitive-constraints (name-constr-list &optional stream)
  (dolist (elem name-constr-list)
    (print-a-constr elem stream)
    (terpri stream)))

(defun print-a-constr (constr-inst &optional stream)
  ($send (cdr constr-inst) :print (car constr-inst) stream))

(defun print-compound-constraints (name-comp-list &optional stream)
  (dolist (elem name-comp-list)
    (print-a-compound-constraint elem stream)
    (terpri stream)))

(defun print-a-compound-constraint (constr-inst &optional stream)
  ($send (cdr constr-inst) :print (car constr-inst) stream))



(def$method (normal-constraint-processor :print-all-nets) 
  (&optional (file-or-stream nil))
  
  ;;; prints restriction nets and primitive and compound constraints
  ;;; of constraint-processor.
  ;;; Note: There is also a corresponding function "print-constraints"
  
  (print-constraints file-or-stream))

;;;--------------------------------------------------------------------------------
;;;--------------------- printing restriction net values --------------------------
;;;--------------------------------------------------------------------------------

(defun print-net (net-name &optional (stream-or-file nil))
  
  ;; prints a message
  ;;    slot <s> of instance <i> has value <v>
  ;; for every slot occuring in the restriction net
  
  (let ((restr-net (send-constraint-processor :find-restriction net-name)))
    (if restr-net
      ($send restr-net :print-restr-net-values stream-or-file)
      (print "Sorry, a net of the specified name is not known"
             (send-kb :dialog-stream)))))


(defun print-rnet (net-name &optional (stream-or-file nil))
 
  ;; prints a message
  ;;    Variable associated with slot <s> of instance <i> has value <v>
  ;; for every slot occuring in the interface of the restriction net
  
 (let ((restr-net (send-constraint-processor :find-restriction net-name)))
    (if restr-net
	($send restr-net :print-restr-rnet-values stream-or-file)
	(print "Sorry, a net of the specified name is not known"
	       (send-kb :dialog-stream))))) 




(def$method (normal-constraint-processor :find-restriction) (net-name)
  (cdr (assoc net-name (<-  self :restriction-nets)  :test #'equal)))


(def$method (restriction-net :print-restr-net-values) (&optional (stream-or-file nil))
  (if stream-or-file
    (with-open-file (outfile stream-or-file :direction :output)
      ($send self :print-all-slots outfile))
    ($send self :print-all-slots (send-kb :dialog-stream))))

(def$method (restriction-net :print-restr-rnet-values) (&optional (stream-or-file nil))
  (if stream-or-file
    (with-open-file (outfile stream-or-file :direction :output)
      ($send self :print-all-slot-variables outfile))
    ($send self :print-all-slot-variables (send-kb :dialog-stream))))

(def$method (restriction-net :print-all-slots) (&optional (stream-or-file nil))
  
  ;; :print-all-slots is called from :print-net-values.
  ;; with stream-or-file=nil it can be used stand-alone. Otherwise not.
  
  (terpri stream-or-file)
  (terpri stream-or-file)
  (format stream-or-file "Printing the values of all slots of restriction net ~S:"
          self)
  (terpri stream-or-file)
  (terpri stream-or-file)
  (dolist (elem ($send self :interface))
    (let ((slot (second elem))
          (instance (first elem)))
      (format stream-or-file "Slot  ~s  of instance  ~s  has value  ~s"
              slot 
              instance
              (<- instance :get slot))
      (terpri stream-or-file))))

(def$method (restriction-net :print-all-slot-variables) 
  (&optional (stream-or-file nil))
  
  ;; :print-all-slots is called from :print-rnet-values.
  ;; with stream-or-file=nil it can be used stand-alone. Otherwise not.
  
  (terpri stream-or-file)
  (terpri stream-or-file)
  (format stream-or-file "Printing the values of all variables of restriction net ~S:"
          self)
  (terpri stream-or-file)
  (terpri stream-or-file)
  (dolist (elem ($send self :interface-assignment))
    (let ((slot (second (first elem)))
          (instance (first (first elem)))
          (values (cdr elem)))
      (format stream-or-file "Slot  ~s  of instance  ~s  has value  ~s"
              slot 
              instance
              values)
      (terpri stream-or-file))))

;;;--------------------------------------------------------------------------------
;;;------------------- reset slots associated with net variables ------------------
;;;--------------------------------------------------------------------------------


(defun reset-slots-of-net (net-name)
  
  ;; if a restriction net with net-name exists, all values slots refered to
  ;; by its interface variables are :put to the value "undetermined" (-).
  
  (let ((restr-net (send-constraint-processor :find-restriction net-name)))
    (if restr-net
      ($send restr-net :reset-restr-net-slots)
      (print "Sorry, a net of the specified name is not known"
             (send-kb :dialog-stream))))) 



(def$method (restriction-net :reset-restr-net-slots) () 
  (dolist (elem ($send self :interface))
    (let ((slot (second elem))
          (instance (first elem))) 
      (<- instance :put slot '-))))


(defun restart-net (net-name)

  ;; restart-net resets the restriction net with name net-name and also 
  ;; :puts all slots associated with the net interface variables to
  ;; "undetermined" (-).
  ;;
  ;; Caution! The current version not only resets the state of net-name but also
  ;; of all other restriction nets and constraint-nets of the current 
  ;; normal-constraint-processor. This should only be a temporary 
  ;; solution until a more fine-grained :reset-proc method for net-name becomes available!
 
  (reset-slots-of-net net-name)
  (send-constraint-processor :reset-proc))


(def$method (restriction-net :restart-rnet) (restr-net)
  
  ;; restart-rnet resets the restriction net restr-net and also 
  ;; :puts all slots associated with the net interface variables to
  ;; "undetermined" (-).
  ;;
  ;; Caution! The current version not only resets the state of restr-net but also
  ;; of all other restriction nets and constraint-nets of the current 
  ;; normal-constraint-processor. This should only be a temporary 
  ;; solution until a more fine-grained :reset-proc method for net-name becomes available!
  ;; a functional version of :restart-rnet is "restart-net" which takes a net name as 
  ;; argument.
  
  (<- restr-net  :reset-restr-net-slots)
  (send-constraint-processor :reset-proc))


;;; eof

