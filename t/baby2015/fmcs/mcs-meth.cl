;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: FMCS -*-

(in-package "FMCS")


;;; ------------------------------------------------------------------
;;; Inheritance methods
;;; ------------------------------------------------------------------

(defmethod (standard-class :compute-cplist) ()
  (let ((result ())
        (r-supers (reverse (mcs-get-slot inst-env (index-of-supers)))))
    (loop
      (if (null r-supers) (return ()))
      (setf result
            (append (mcs-slot-value (pop r-supers) (index-of-cplist))
                    result)))
    (setf (mcs-get-slot inst-env (index-of-cplist))
          (remove-duplicates (cons self result)
                             :test #'eq))))

(defmethod (standard-class :inherit-slots-with-defaults) ()
  (let* ((own-slots-specification 
          (mcs-get-slot inst-env (index-of-own-slots)))
         (slots-result (mapcar #'(lambda (el) (if (listp el) (first el) el))
                               own-slots-specification))
         (defaults-result (mapcar #'(lambda (el) (if (listp el) 
                                                   el
                                                   (list el nil)))
                                  own-slots-specification))
         (components (rest (mcs-get-slot inst-env (index-of-cplist)))))
    (loop
      (if (null components) (return ()))
      (setq slots-result 
            (append (mcs-slot-value (first components) (index-of-all-slots))
                    slots-result))
      (setq defaults-result 
            (append defaults-result 
                    (mcs-slot-value (first components) 
                                    (index-of-all-slot-defaults))))
      (pop components))
    (setf (mcs-get-slot inst-env  (index-of-all-slots))
          (remove-duplicates slots-result :test #'eq :from-end t))
    (setf (mcs-get-slot inst-env (index-of-all-slot-defaults))
          (remove-duplicates defaults-result :test #'eq :key #'car :from-end t))))

(defmethod (standard-class :compute-slot-access-methods) ()
  (let* ((slots (rest (mcs-get-slot inst-env (index-of-all-slots))))
         (nr (1+ (length slots)))
         (counter (1+ (index-of-isit)))
         (array standard-accessors))
    (declare (fixnum nr counter)
             (vector array)
             (list slots))
    (if (> nr (length array))
      (setq standard-accessors (adjust-standard-accessors array nr)))
    (loop
      (if (null slots) (return ()))
      (let ((slot-name (pop slots))
            (fn-pair (aref array counter)))
         (setf (gethash (intern (string slot-name) :keyword)
                        (mcs-get-slot inst-env (index-of-methods)))
               (make-method-entry :type 'standard
                                  :methods-list
                                  (acons :primary  (car fn-pair) ())
                                  :combined-method nil))
         (setf (gethash (intern (concatenate 'string "SET-" (string slot-name))
                                :keyword)
                        (mcs-get-slot inst-env (index-of-methods)))
               (make-method-entry :type 'standard
                                  :methods-list
                                  (acons :primary  (cdr fn-pair) ())
                                  :combined-method nil))
         (setq counter (1+ counter))))))
 
(defmethod (standard-class :compute-slot-accessor-fn) ()
  (setf (mcs-get-slot inst-env (index-of-slot-accessor-fn))
        (compile 
         nil
         `(lambda (slot)
            (case slot
              ,@(let ((slots (mcs-get-slot inst-env (index-of-all-slots)))
                      (list-of-var-pos-pairs nil)
                      (counter (index-of-isit)))
                  (declare (fixnum counter))
                  (loop
                    (if (null slots) (return list-of-var-pos-pairs))
                    (setq list-of-var-pos-pairs
                          (append list-of-var-pos-pairs
                                  (list (cons (pop slots) (list counter)))))
                    (setq counter (1+ counter)))
                  list-of-var-pos-pairs)
              (t (error "No slot ~S in instances of <mcs-class ~S>."
                        slot  ',(mcs-get-slot inst-env (index-of-name)))))))))

(defmethod (standard-class :extend-subclasses-of-supers) ()
  (dolist (super (mcs-get-slot inst-env (index-of-supers)))
    (setf (mcs-slot-value super (index-of-subclasses))
          (cons self (mcs-slot-value super (index-of-subclasses)))
          )))

(defmethod (standard-class :compute-basicnew-fn) (&rest keys)
  (let ((key-list (rest (mcs-get-slot inst-env (index-of-all-slots))))
	(slot-list (rest (mcs-get-slot inst-env (index-of-all-slots))))
	(keys+defaults (mcs-get-slot inst-env (index-of-all-slot-defaults))))
    (when keys
      (setq key-list keys)
      (setq slot-list (mapcar #'(lambda (slot)
				  (if (member slot key-list :test #'eq)
				    slot 
				    (second (assoc slot keys+defaults 
						   :test #'eq))))
			      slot-list))
      (setq keys+defaults (mapcar #'(lambda (key)
				      (assoc key keys+defaults :test #'eq))
				  key-list)))
    (setf (mcs-get-slot inst-env (index-of-basicnew-fn))
	  (compile nil 
		   `(lambda (isit &key ,@keys+defaults)
		      (send-fast
			(make-mcsobject
			  :env (vector isit ,@slot-list))
			:init ,@key-list))))))

#|
(defmethod (standard-class :recompute-cplist) ()
  (setf (mcs-get-slot inst-env (index-of-cplist))
        (send-self :inheritance-algorithm)))


(defmethod (standard-class :inheritance-algorithm) ()
  (labels
    ((traverse-node (a_class result)
                      (if (member a_class result :test #'eq) 
                        result
                        (cons a_class 
                              (traverse-list 
                               (reverse (mcs-slot-value a_class 
                                                        (index-of-supers)))
                               result))
                        ))
     (traverse-list (r-supers result)
                    (if (null r-supers) 
                      result
                      (traverse-list 
                       (rest r-supers) 
                       (traverse-node (first r-supers) result))
                      )))
    (cons self
          (traverse-list (reverse (mcs-get-slot inst-env (index-of-supers)))
                         nil))))
|#

;;; ------------------------------------------------------------------
;;; Object protocol methods
;;; ------------------------------------------------------------------

(defmethod (standard-object :isit) ()
  (mcs-get-slot inst-env (index-of-isit)))

(defmethod (standard-object :class-name) ()
  (mcs-get-slot class-env (index-of-name)))

(defmethod (standard-object :class-p) () 
  (if (member 'supers (get-class-slot 'all-slots) :test #'eq)
    t nil))

(defmethod (standard-object :metaclass-p) () 
  (if (and (member 'cplist (get-class-slot 'all-slots) :test #'eq) 
           (member standard-class (mcs-get-slot inst-env (index-of-cplist))
                   :test #'eq))
    t))

(defmethod (standard-object :default-handler) (&rest message)
  (send-self :error-handler (first message)))

(defmethod (standard-object :error-handler) (selector)
  (error "~S can not handle this message: ~S" 
         (mcs-get-slot class-env (index-of-name)) selector))

(defmethod (standard-object :operation-handled-p) (operation)
  (let ((opened (mcs-get-slot class-env (index-of-cplist))))
    (loop
      (if (null opened) (return ()))
      (let ((a-class (pop opened)))
        (if (get-method-entry a-class operation)
          (return T))))))

(defmethod (standard-object :send-if-handles) (operation &rest arguments)
  (let ((opened (mcs-get-slot class-env (index-of-cplist))))
    (loop
      (if (null opened) (return ()))
      (let ((a-class (pop opened)))
        (if (get-method-entry a-class operation)
          (return (standard-message-handler self class-env inst-env operation
                                            arguments)))))))

;; eof




