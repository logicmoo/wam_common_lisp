(defun get-properties (plist indicator-list)
  (tagbody
   start
     (when plist
       (when (member (car plist) indicator-list :test #'eq)
	 (return-from get-properties
	   (values (car plist) (cadr plist) (cddr plist))))
       (setf plist (cddr plist))
       (go start))))
(defun getf (plist indicator &optional default)
  (tagbody
   start
     (when plist
       (when (eq (car plist) indicator)
	 (return-from plist (cadr plist)))
       (setf plist (cddr plist))
       (go start)))
  default)

(defmacro define-setf-expander (access-fn lambda-list &rest forms)
  `',access-fn)

(define-setf-expander getf (place indicator &optional default &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (let ((t-indicator (gensym))
	  (t-default (gensym))
	  (t-place (gensym))
	  (t-plist (gensym))
	  (store (gensym)))
      (values (list* t-indicator t-default dummies)
	      (list* indicator default vals)
	      (list store)
	      `(let ((,t-place ,getter))
		(do ((,t-plist ,t-place (cddr ,t-plist)))
		    ((not ,t-plist)
		     (let ((,(car newval) (list* ,t-indicator ,store t-place)))
		       ,setter))
		  (when (eq (car ,t-plist) ,t-indicator)
		    (setf (cadr ,t-plist) ,store)
		    (return)))
		,store)
	      `(getf ,getter ,ind ,def)))))

(defmacro remf (place indicator)
  (let ((t-plist (gensym))
	(t-indicator (gensym)))
    `(let ((,t-plist ,place)
	   (,t-indicator ,indicator))
      (if (eq (car ,t-plist) ,t-indicator)
	  (progn (setf ,place (cddr ,t-plist)) t)
	  (tagbody
	     (setf ,t-plist (cdr ,t-plist))
	   start
	     (when (cdr ,t-plist)
	       (when (eq (cadr ,t-plist) ,t-indicator)
		 (setf (cdr ,t-plist) (cddr ,t-plist))
		 (return-from nil t))
	       (setf ,t-plist (cddr ,t-plist))
	       (go start)))))))

