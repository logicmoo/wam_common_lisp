;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: FMCS -*-

(in-package "FMCS")

;;; --------------------------------------------------------------------------
;;; Trace utilities
;;; --------------------------------------------------------------------------

(defvar *indent-for-methods-trace* 0.)
(declaim (fixnum *indent-for-methods-trace*))

(defun increment-indent-for-methods-trace ()
  (setq *indent-for-methods-trace* (+ (the fixnum *indent-for-methods-trace*) 2.)))

(defun decrement-indent-for-methods-trace ()
  (setq *indent-for-methods-trace* (- (the fixnum *indent-for-methods-trace*) 2.)))

(defun TRACED-DEMON-COMBINATION
       (self class-env inst-env selector applicable-methods args)
  (let ((before-methods (before-of applicable-methods))
        (primary-methods (primary-of applicable-methods))
        (after-methods (after-of applicable-methods))
        (class-name (mcs-get-slot class-env (index-of-name)))
        (result nil))
    (declare (list before-methods primary-methods after-methods))
    (format *trace-output* "~%~V@T Entering method ~S of class ~S"
            (increment-indent-for-methods-trace)  selector class-name)
     (format *trace-output* "~%~V@T Executing ~S before methods"
             *indent-for-methods-trace* (length before-methods))
     (loop 
       (if (null before-methods) (return ()))
       (apply (pop before-methods) 
              self class-env inst-env args))
     (setq result
           (apply (first primary-methods) 
            self class-env inst-env :primary-caller (rest primary-methods) 
            args args))
     (format *trace-output* "~%~V@T Executing ~S after methods"
             *indent-for-methods-trace* (length after-methods))
     (loop 
       (if (null after-methods) (return ()))
       (apply (pop after-methods) 
              self class-env inst-env args))
     (format *trace-output* "~%~V@T Exiting method  ~S of class ~S with result: ~S" 
             *indent-for-methods-trace* selector class-name result)
    (decrement-indent-for-methods-trace)
     result))

(defun TRACED-SIMPLE-COMBINATION
       (self class-env inst-env selector applicable-methods args)
  (let ((primary-methods (primary-of applicable-methods))
        (class-name (mcs-get-slot class-env (index-of-name)))
        result)
    (increment-indent-for-methods-trace)
    (format *trace-output* "~%~V@T Entering method ~S of class ~S"
            *indent-for-methods-trace* selector class-name)
    (setq result
          (apply (first primary-methods)
                 self class-env inst-env :primary-caller (rest primary-methods)
                 args args))
    (format *trace-output* "~%~V@T Exiting method  ~S of class ~S with result: ~S" 
            *indent-for-methods-trace* selector class-name result)
    (decrement-indent-for-methods-trace)
    result))


;;;(export 'MCS-TRACE)
(defun MCS-TRACE (a_class selector)
  (let* ((class-env (mcs-env a_class))
         (combined-method (get-combined-method selector)))
    (if combined-method
      (let ((combination-fn (svref combined-method 0)))
        (setf (svref combined-method 0)
              (case combination-fn
                (simple-method-combination 'traced-simple-combination)
                (demon-method-combination 'traced-demon-combination)
                (standard-method-combination 'traced-standard-combination)
                (t combination-fn))))
      (progn
        (combine-class-method a_class selector)
        (if (get-combined-method selector)
          (mcs-trace a_class selector))))))

;;;(export 'MCS-UNTRACE)
(defun MCS-UNTRACE (a_class selector)
  (let* ((class-env (mcs-env a_class))
         (combined-method (get-combined-method selector)))
    (if combined-method
      (let ((combination-fn (svref combined-method 0)))
        (setf (svref combined-method 0)
          (case combination-fn
            (traced-simple-combination 'simple-method-combination)
            (traced-demon-combination 'demon-method-combination)
            (traced-standard-combination 'standard-method-combination)
            (t combination-fn)
            ))
        ))))

;;;(export 'MCS-IS-TRACED)
(defun MCS-IS-TRACED (a_class selector)
  (let* ((class-env (mcs-env a_class))
         (combined-method (get-combined-method selector)))
    (if combined-method
      (case (svref combined-method 0)
        (traced-simple-combination t)
        (traced-demon-combination t)
        (traced-standard-combination t)
        (t nil)))))

(defmethod (standard-class :trace-methods) (&rest selectors)
  (dolist (selector selectors 'done)
    (mcs-trace self selector)))

(defmethod (standard-class :untrace-methods) (&rest selectors)
  (dolist (selector selectors 'done)
    (mcs-untrace self selector)))


;;; --------------------------------------------------------------------------
;;; Protocol utilities
;;; --------------------------------------------------------------------------

(defmethod (standard-class :get-local-protocol) ()
  (let ((protocol ()))
    (maphash #'(lambda (key val)
                 (if (method-entry-methods-list val)
                   (setq protocol (cons key protocol))))
             (mcs-get-slot inst-env (index-of-methods)))
    protocol))

(defmethod (standard-class :get-protocol) ()
  (let ((protocol ())
        (opened (mcs-get-slot inst-env (index-of-cplist))))
    (loop
      (if (null opened) (return ()))
      (maphash #'(lambda (key val)
                   (declare (ignore val))
                   (setq protocol (cons key protocol)))
               (mcs-slot-value (pop opened) (index-of-methods))))
    (remove-duplicates protocol :test #'eq)))

;;; --------------------------------------------------------------------------
;;; Other utilities
;;; --------------------------------------------------------------------------


(defun WHERE-METHOD-LOOKUP (class-env a_selector)
  (declare (inline GET-METHOD-ENTRY))
  (let ((r-class-precedence-list
         (reverse (mcs-get-slot class-env (index-of-cplist))))
        (around-methods nil) (before-methods nil)
        (primary-methods nil) (after-methods nil))
    (loop
      (if (null r-class-precedence-list) 
        (return
         (values (if primary-methods          
                   (if around-methods 
                     'standard-method-combination
                     (if (or before-methods after-methods) 
                       'demon-method-combination
                       'simple-method-combination))
                   (if (or after-methods before-methods around-methods)
                     'missing-primary-method))
                 (list around-methods before-methods 
                       primary-methods (reverse after-methods)))))
      (let* ((c-class (pop r-class-precedence-list))
             (method (get-method-entry c-class a_selector)))
        (if method
          (let ((own-methods-list (method-entry-methods-list method)))
            (let ((around-method (get-qualified-method :around own-methods-list))
                  (before-method (get-qualified-method :before own-methods-list))
                  (primary-method (get-qualified-method :primary own-methods-list))
                  (after-method (get-qualified-method :after own-methods-list))
                  (c-class-name (slot-value c-class 'name))) 
              (if before-method 
                (setq before-methods (cons c-class-name before-methods)))
              (if after-method
                (setq after-methods (cons c-class-name after-methods)))
              (if primary-method
                (setq primary-methods (cons c-class-name primary-methods)))
              (if around-method
                (setq around-methods (cons c-class-name around-methods)))))))
      )))

(defmethod (standard-object :how-combined) (selector)
  (multiple-value-bind
    (method-combination-fn where-defined-list)
    (where-method-lookup class-env selector)
    (if (null method-combination-fn)
      (progn 
        (format t "~%no method, :default-handler is called")
        (send-self :how-combined :default-handler))
      (progn 
        (if (eq method-combination-fn 'missing-primary-method)
          (format t "~%illegal combination, missing primary method")
          (format t "~%selector ~S has combination type: ~S"
                  selector method-combination-fn))
        (if (first where-defined-list)
          (format t "~%around methods are defined in class: ~{~%   ~s ~}"
                  (first where-defined-list)))
        (if (second where-defined-list)
          (format t "~%before methods are defined in class: ~{~%   ~s ~}"
                  (second where-defined-list)))
        (if (third where-defined-list)
          (format t "~%primary methods are defined in class: ~{~%   ~s ~}"
                  (third where-defined-list)))
        (if (fourth where-defined-list)
          (format t "~%after methods are defined in class: ~{~%   ~s ~}"
                  (fourth where-defined-list)))))))


(defmethod (standard-object :which-operations) ()
  (send-fast (get-slot 'isit) :get-protocol))

(defmethod (standard-object :describe) ()
  (format t "~&~S, an object of class ~S,~% has instance variable values:~%" 
          self (get-class-slot 'name))
  (dolist (ivar (get-class-slot 'all-slots))
    (format t "~%       ~S:~27T~S" ivar (slot-value self ivar))))

(defmethod (standard-object :describe-short) ()
  (format t "an object of class ~S with instance variable values:~%~S" 
          (get-class-slot 'name)
          (rest (mapcar #'(lambda (ivar)
                      `(,ivar ,(slot-value self ivar)))
                  (get-class-slot 'all-slots)))))

(defmethod (standard-object :apropos) (substring)
  (remove nil 
          (mapcar #'(lambda (method)
                      (if (search substring (string method) 
                                  :test #'char-equal) method))
                  (send-fast (get-slot 'isit) :get-protocol))
          :test #'eq))


;;; eof

