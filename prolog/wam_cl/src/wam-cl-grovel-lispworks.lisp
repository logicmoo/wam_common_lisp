(defun find-synonym-types (type1 types)
  (remove-if-not (lambda (type2)
                   (and (not (eq type1 type2))
                        (subtypep type1 type2)
                        (subtypep type2 type1)))
                 types))

(defun find-all-types-in-packages (packages &key include-nil)
  (let ((types nil))
    (loop for package in packages
          when (find-package package)
          do (do-external-symbols (sym (find-package package))
               (when (ignore-errors (subtypep sym t))
                 (let ((synonyms (find-synonym-types sym types)))
                   (if synonyms
                       (pushnew sym (get (first synonyms) :type-synonyms))
                     (pushnew sym types))))))
    (if include-nil types (remove nil types))))

(defun direct-subtypes (supertype &key all-types)
  (loop with subtypes = (remove supertype (loop for type in all-types
                                                when (subtypep type supertype)
                                                collect type))

        for type in subtypes
        when (loop for type2 in (remove type subtypes)
                   never (subtypep type type2))
        collect type))

#+capi
(defun class-color (class-name)
  (typecase (find-class class-name)
    (standard-class  :blue)
    (built-in-class  :violet)
    (structure-class :brown)
    (otherwise       :red)))

#+capi
(defun graph-subtypes-from-t ()
  (let ((all-types (find-all-types-in-packages '("CL" "CLOS"))))
    (capi:contain
     (make-instance
      'capi:graph-pane
      :roots '(t)
      :children-function (lambda (type)
                           (direct-subtypes type :all-types all-types))
      :node-pane-function #'(lambda (graph-pane node)
                              (declare (ignore graph-pane))
                              (make-instance
                               'capi:item-pinboard-object
                               :text (format
                                      nil "~{~a~^, ~}"
                                      (cons node
                                            (get node :type-synonyms)))
                               :graphics-args `(:foreground
                                                ,(if (find-class node
                                                                 nil)
                                                     (class-color node)
                                                   :black))))
      :edge-pane-function #'(lambda(self from to)
                              (declare (ignore self from to))
                              (make-instance
                               'capi:arrow-pinboard-object
                               :graphics-args '(:foreground
                                                :grey))))
     :title "Common Lisp Subtypes in LispWorks")))







;; THIS IS A PROOF OF CONCEPT. This has been "released" in case someone
;;; actually wants to develop it into a usable tool. Don't even think
;;; about using this version for any sort of production use.

;;; Author: Juho Snellman
;;; http://jsnell.iki.fi/blog/archive/2005-07-06.html

(defparameter *retain* (make-hash-table :test 'eq))
(defparameter *ignore* (make-hash-table :test 'eq))
(defparameter *classes* nil)
(defparameter *generic-functions* nil)
(defparameter *stream* (make-broadcast-stream))
(defparameter *objects* nil)

(defmacro ignore-functions (&rest symbols)
  `(dolist (symbol ',symbols)
    (setf (gethash (fdefinition symbol) *ignore*) t)))

(defmacro ignore-variables (&rest symbols)
  `(dolist (symbol ',symbols)
    (setf (gethash symbol *ignore*) t)))

(defun keep (object)
  (setf (gethash object *retain*) t))

(defun kept-p (object)
  (or (gethash object *retain*)
      (gethash object *ignore*)))

(defun ignored-p (object)
  (gethash object *ignore*))

;; compiler
(ignore-functions eval
                  sb-impl::%defun)
(keep 'sb-c::*cached-info-environment*)
(keep 'sb-c::*info-environment*)
(keep 'sb-c::*info-cache-vector*)

;; disassembler
(ignore-variables sb-disassem::*disassem-inst-formats*
                  sb-disassem::*disassem-arg-types*
                  sb-disassem::*disassem-insts*
                  sb-disassem::*disassem-fun-cache*)

;; debugger
(progn
  (ignore-functions invoke-debugger cerror
                    sb-kernel:find-caller-name-and-frame)
  (ignore-variables sb-impl::*handler-clusters*
                    sb-impl::*invoke-debugger-hook*
                    sb-debug::*debug-loop-fun*
                    sb-di::*compiled-debug-funs*
                    sb-impl::*debugger-hook*))

;; tracing
(ignore-functions sb-debug::trace-redefined-update)

;; pcl
(ignore-functions sb-pcl::get-new-fun-generator)
(ignore-variables sb-pcl::*mf1* sb-pcl::*mf1p* 
                  sb-pcl::*mf1cp* sb-pcl::*mf2*
                  sb-pcl::*effective-method-cache*
                  sb-pcl::*method-function-plist*
                  sb-pcl::*find-class*
                  sb-pcl::*pv-table-cache-update-info*
                  sb-pcl::*name->class->slotd-table*
                  sb-pcl::*mf2p* sb-pcl::*mf2cp*)
(keep #'sb-pcl::generating-lisp)
(keep 'sb-pcl::*pv-key-to-pv-table-table*)
(keep 'sb-pcl::*slot-name-lists-outer*)
(keep 'sb-pcl::*slot-name-lists-inner*)
(keep 'sb-pcl::*all-pv-table-list*)
(keep 'sb-pcl::*eql-specializer-table*)
(keep 'sb-pcl::*eql-specializer-methods*)

(defvar *delete-debug-info* t)
(defmethod delete-debug-info ((code-obj sb-vm::code-component))
  (when *delete-debug-info*
    (setf (sb-c::%code-debug-info code-obj) nil)))

(defgeneric grovel (object))

(defmethod grovel ((null null))
  (values))

(defmethod grovel ((cons cons))
  (keep cons)
  (grovel (car cons))
  (grovel (cdr cons)))

(defmethod grovel ((array array))
  (keep array)
  (when (eq (array-element-type array) 't)
    (labels ((grovel-array (dimensions indices)
	       (if dimensions
		   (dotimes (i (car dimensions))
		     (grovel-array (cdr dimensions)
				   (cons i indices)))
		   (when indices
		     (grovel (apply #'aref array indices))))))		     
      (grovel-array (reverse (array-dimensions array)) nil))))
  
(defmethod grovel (obj)
  (cond ((= (sb-vm::widetag-of obj) sb-vm:unbound-marker-widetag)
         (values))
        ((= (sb-vm::widetag-of obj) sb-vm:value-cell-header-widetag)
         (keep obj)
         (grovel (sb-vm::value-cell-ref obj)))
        (t
         (format *stream* "Don't know how to handle ~s~%" obj))))

(defmethod grovel ((num number))
  (values))

(defmethod grovel ((char character))
  (values))

(defmethod grovel ((package package))
  (values))

(defmethod grovel ((pointer sb-impl::system-area-pointer))
  (values))

(defmethod grovel ((pointer sb-impl::weak-pointer))
  (keep pointer)
  (grovel (sb-impl::weak-pointer-value pointer)))

(defun grovel-slots (object &key skip)
  (dolist (slot (sb-mop::class-slots (class-of object)))
    (let ((name (sb-mop::slot-definition-name slot)))
      (unless (member name skip)
        (when (slot-boundp object name)
          (grovel (slot-value object name)))))))

(defmethod grovel ((instance sb-pcl::slot-object))
  (keep instance)
  (format *stream* "keeping instance=~s~%" (type-of instance))
  (grovel-slots instance))

(defmethod grovel ((class sb-kernel::classoid))
  (keep class)
  (grovel-slots class :skip '(sb-kernel::subclasses)))
  
(defmethod grovel ((class class))
  (keep class)
  (format *stream* "keeping class ~s~%" class)
  (push class *classes*)
  (dolist (fun *generic-functions*)
    (when (and (slot-boundp class 'sb-pcl::direct-methods)
               (typep fun 'sb-pcl::standard-generic-function)
               (slot-boundp fun 'sb-pcl::methods))
      (let ((methods (intersection (sb-mop::specializer-direct-methods class)
                                   (sb-mop::generic-function-methods fun))))
        (dolist (method methods)
          (format *stream* ".. ~s | ~s~%" class fun)
          (grovel method)))))
  (grovel-slots class :skip '(sb-pcl::direct-methods
                             sb-pcl::direct-subclasses)))

(defmethod grovel ((fun generic-function))
  (keep fun)
  (push fun *generic-functions*)
  (dolist (class *classes*)
    (when (and (slot-boundp class 'sb-pcl::direct-methods)
               (typep fun 'sb-pcl::standard-generic-function)
               (slot-boundp fun 'sb-pcl::methods))
      (let ((methods (intersection (sb-mop::specializer-direct-methods class)
                                   (sb-mop::generic-function-methods fun))))
        (dolist (method methods)
          (format *stream* "// ~s | ~s~%" class fun)
          (grovel method)))))
  (grovel-slots fun :skip '(sb-pcl::initial-methods sb-pcl::dfun-state
                            sb-pcl::methods)))

(defmethod grovel ((ignore sb-c::vop-info))
  (values))

#+nil
(defmethod grovel ((ignore sb-pcl::fast-method-call))
  (dolist (o *objects*)
    (format t ">> ~s~%" o))
  (close *stream*)
  (sb-ext:quit :unix-status 1))

(defmethod grovel ((symbol symbol))
  (keep symbol)
  #+nil
  (when (fboundp symbol)
    (grovel (symbol-function symbol)))
  (grovel (sb-c::info :type :translator symbol))
  (grovel (sb-c::info :alien-type :translator symbol))
  (when (boundp symbol)
    (unless (keywordp symbol)
      (format *stream*
	      "keeping var=~s (~s)~%" symbol (type-of (symbol-value symbol))))
    (grovel (symbol-value symbol))))

(defmethod grovel ((fun sb-vm::fdefn))
  (keep fun)
  (grovel (sb-vm::fdefn-fun fun)))

(defmethod grovel ((fun sb-vm::funcallable-instance))
  (grovel (sb-vm::%funcallable-instance-fun fun)))

(defun code-constants (code-obj)
  (loop for i from sb-vm::code-constants-offset
        below (sb-vm::get-header-data code-obj)
        collect (sb-vm::code-header-ref code-obj i)))

(defun code-object (fun)
  (let ((header (sb-kernel:widetag-of fun)))
    (ecase header
      (#.sb-vm:simple-fun-header-widetag
       (sb-vm::fun-code-header fun))
      (#.sb-vm:funcallable-instance-header-widetag
       (sb-vm::%funcallable-instance-fun fun))
      (#.sb-vm:closure-header-widetag
       (sb-vm::fun-code-header (sb-vm::%closure-fun fun))))))

(defmethod grovel ((fun function))
  (format *stream* "keeping fun=~s~%" fun)
  (keep fun)
  (let ((code-obj (code-object fun)))
    (delete-debug-info code-obj)
    (dolist (obj (code-constants code-obj))
      (grovel obj)))
  (case (sb-kernel::widetag-of fun)
    (#.sb-vm:closure-header-widetag
     (dotimes (i (1- (sb-kernel:get-closure-length fun)))
       (grovel (sb-kernel:%closure-index-ref fun i))))))

(defmethod grovel :around (object)
  #+nil
  (when (eq object #'SB-VM::|CACHED-FUN--MOV[(REG ((OP 11) (IMM NIL TYPE 'SIGNED-IMM-DATA)) '(NAME TAB REG ,  IMM))]-REG-PRINTER|)
    (dolist (o *objects*)
      (format *stream* ">> ~s~%" o))
    (error "foo"))
  ;; gah... 
  (when (and (eq (type-of object) 'sb-pcl::funcallable-instance)
	     (= (sb-vm::widetag-of object) 1))
    (return-from grovel))
  ;; gah^2... 
  (when (and (eq (type-of object) 'function)
	     (= (sb-vm::widetag-of object) 1))
    (return-from grovel))
  (unless (kept-p object)
    (let ((*objects* (cons object *objects*)))
      (call-next-method))))
  
(defvar *stub-lambda* (lambda (&rest args)
                        (format t "stub ~s~%" (car args))
                        (force-output)
                        (format t "args ~s~%" (cdr args))
                        (force-output)
                        (backtrace)
                        (sb-unix::unix-exit 42)))

#+nil
(defmacro make-stub (name)  
  *stub-lambda*)
(defmacro make-stub (name)
  `(lambda (&rest args)
    (declare (optimize (debug 0) (space 3)))
    (apply *stub-lambda* ,name args)))
  
(defun clear-caches ()
  (setf sb-disassem::*disassem-inst-formats* nil
        sb-disassem::*disassem-arg-types* nil)
  (let ((fc sb-disassem::*disassem-fun-cache*))
    (setf (sb-disassem::fun-cache-labellers fc) nil
          (sb-disassem::fun-cache-printers fc) nil
          (sb-disassem::fun-cache-prefilters fc) nil))
  (sb-c::info-cache-clear)
  (sb-kernel::type=-cache-clear)
  (sb-kernel::values-subtypep-cache-clear)
  (sb-kernel::values-type-intersection-cache-clear)
  (sb-kernel::csubtypep-cache-clear)
  (sb-kernel::values-type-union-cache-clear)
  (sb-kernel::type-union2-cache-clear)
;  (sb-kernel::type-intersection2-cache-clear)
;  (sb-kernel::type-negation-cache-clear)
;  (sb-kernel::%type-intersection-cache-clear)
;  (sb-kernel::%type-union-cache-clear)
  (sb-kernel::ctype-of-cache-clear)
  (keep 'sb-kernel::*ctype-of-cache-vector*)
  (sb-kernel::%%make-array-type-cached-cache-clear)
  (sb-kernel::%%make-union-type-cached-cache-clear)
  (sb-kernel::make-values-type-cached-cache-clear)
  (sb-kernel::%coerce-to-values-cache-clear)
  (sb-kernel::values-specifier-type-cache-clear)
  (sb-vm::primitive-type-aux-cache-clear)
  (sb-c::weaken-type-cache-clear))
  
(defun clear-info-environment ()
  (sb-kernel::compact-environment-aux "Auxiliary" 200)
  (dolist (number '(0 1 2 3 4 5 6 7 8 9 10 11 12
                    14 15 16 17 19
                    26
                    27 28
                    29
                    36 37 38 39
                    ))
    (dolist (env sb-c::*info-environment*)
      (when (typep env 'sb-c::compact-info-env)
        (dotimes (i (length (sb-c::compact-info-env-entries env)))
          (when (= (aref (sb-c::compact-info-env-entries-info env) i)
                   number)
            (if (/= number 12)
                (setf (aref (sb-c::compact-info-env-entries env) i) nil)
                (let* ((fdefn (aref (sb-c::compact-info-env-entries env) i))
                       (name (sb-impl::fdefn-name fdefn)))
                  (unless (kept-p (sb-impl::fdefn-fun fdefn))
                    (setf (sb-impl::fdefn-fun fdefn)
                          (make-stub name)))))))))))
                    
(defun delete-unreachable-methods ()
  (labels ((delete-for-class (class)
             (when (slot-boundp class 'sb-pcl::direct-methods)
               (dolist (method (sb-mop::specializer-direct-methods class))
                 (unless (kept-p method)
                   (let ((*print-pretty* nil))
                     (format *stream* "clearing ~s~%" method))
                   (setf (slot-value method 'sb-pcl::function)
                         (let ((name (sb-mop:generic-function-name
                                      (sb-mop:method-generic-function method))))
                           (make-stub name))))))
             (dolist (subclass (sb-mop:class-direct-subclasses class))
               (delete-for-class subclass))))
    (delete-for-class (find-class t))))

(in-package sb-vm)

(defun valid-obj (space x)
  (or (not (eq space :dynamic))
      ;; this test looks bogus if the allocator doesn't work linearly,
      ;; which I suspect is the case for GENCGC.  -- CSR, 2004-06-29
      (< (get-lisp-obj-address x)
         sb-vm::dynamic-space-end
         #+nil (get-lisp-obj-address *ignore-after*))))

(in-package cl-user)

(defun delete-unreachable-symbols (map)
  (let ((*retain* map))
    (delete-unreachable-methods)
    (gc :full t)
    (do-all-symbols (symbol)
      (without-package-locks
        (when (or (ignored-p symbol)
                  (not (kept-p symbol)))
          (when (boundp symbol)
            (unless (constantp symbol)
              (format *stream* "deleting/1 ~s (~s)~%" symbol
                      (type-of (symbol-value symbol)))
              (setf (symbol-value symbol) nil)))
          (unintern symbol))
        (when (and (fboundp symbol)
                   (or (ignored-p (symbol-function symbol))
                       (not (kept-p (symbol-function symbol)))))
          (format *stream* "deleting/2 ~s~%" symbol)
          (setf (symbol-function symbol)
                (make-stub symbol)))))
    (clear-info-environment)
    (format *stream* "done!~%")))

(defun shake (function)
  (gc :full t)
  (sb-pcl::precompile-random-code-segments)
  (clear-caches)      
  (dolist (sym sb-vm::*static-funs*)
    (grovel (symbol-function sym)))      
  (dolist (sym sb-vm::*static-symbols*)
    (grovel sym))  
  (grovel function)
  (grovel #'shake-lisp-and-die)
  (grovel #'sb-c::%check-fast-method-fun-name)
  (grovel #'SB-KERNEL::SIMPLE-CONDITION-PRINTER)
  (grovel #'type-error-datum)
  (grovel #'type-error-expected-type)
  ;; Why does this data structure contain symbols instead of fdefns anyway?
  (dolist (r sb-impl::*output-routines*)
    (let ((name (nth 2 r)))
      (when (kept-p name)
        (grovel (symbol-function name)))))  
  (clear-caches)
  (print *retain*))

(defun shake-lisp-and-die (core fun &key (delete-debug-info t))
  (gc :full t)
  (with-open-file (*stream* #p"/tmp/log.shake"
			    :direction :output :if-exists :supersede)
    (let ((*delete-debug-info* delete-debug-info))
      (delete-unreachable-symbols (shake fun))))
  (gc :full t)
  (room)
  (sb-vm::instance-usage :dynamic)
  (save-lisp-and-die core :purify t :toplevel fun))

