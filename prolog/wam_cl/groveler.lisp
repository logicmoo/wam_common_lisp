
(defun docinfo ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (format t "package_nickname('~a','~a').~%" pn pn)
   (dolist (nn (package-nicknames p))
     (format t "package_nickname('~a','~a').~%" pn (string-downcase nn)))
   (dolist (nn (package-use-list p))
     (format t "package_use_list('~a','~a').~%" pn (string-downcase (package-name nn))))
   (dolist (nn (package-shadowing-symbols p))
     (format t "package_shadowing_symbols('~a','~a').~%" pn nn))

 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)   
    (let (( sn (string-downcase (symbol-name sym))))
       (let ((doc (documentation sym 'function))) (when doc (format t "doc_string('~a','~a',function,~a).~%" sn pn (write-to-string doc))))
       (let ((doc (documentation sym 'variable))) (when doc (format t "doc_string('~a','~a',variable,~a).~%" sn pn (write-to-string doc))))
     (when (special-operator-p sym)
         (format t "special('~a','~a').~%" sn pn))

))))))

(docinfo)


(defun get-arglists ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (do-all-symbols (sym)    
    (when (eq (symbol-package sym) p)   
        (when (fboundp sym) 
	   (let* (( sn (string-downcase (symbol-name sym)))(sf (symbol-function sym)) 
	     (sfll (if (SYSTEM::MACROP sf) (SYSTEM::MACRO-LAMBDA-LIST sf)  (FUNCTION-LAMBDA-EXPRESSION sf))))
	  
	   (when sfll (format t "arglistss('~a','~a','~a').~%" sn pn sfll)))))
))))
(get-arglists)


#-sbcl
(defun psyminfo ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (format t "package_nickname('~a','~a').~%" pn pn)
   (dolist (nn (package-nicknames p))
     (format t "package_nickname('~a','~a').~%" pn (string-downcase nn)))
   (dolist (nn (package-use-list p))
     (format t "package_use_list('~a','~a').~%" pn (string-downcase (package-name nn))))
   (dolist (nn (package-shadowing-symbols p))
     (format t "package_shadowing_symbols('~a','~a').~%" pn nn))
 (when (and (not (string= "sb-c" pn))(not (string= "sb-vm" pn))(not (string= "java" pn)))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)   
    (let (( sn (string-downcase (symbol-name sym))))
     
     (format t "symbol_info('~a','~a',package,~(~a~)).~%" sn pn (second (multiple-value-list  (find-symbol (symbol-name sym) p))))
    (when (not (string= "keyword" pn))
      (when (boundp sym) 
       (handler-case  
        (progn (format t "symbol_info('~a','~a',value,'~a',~a).~%" sn pn (symbol-value sym) (if (constantp sym) "constant" "variable"))
      (let ((doc (documentation sym 'variable))) (when doc (format t "doc_string('~a','~a',variable,\"~a\").~%" sn pn (princ-to-string doc)))))))
     (when (fboundp sym) 
      (let* ((sf (symbol-function sym)) (sft (string-downcase (symbol-name (type-of sf)))))
       (when (special-operator-p sym)
         (setq sft (concatenate 'string "special-" sft)))
       (format t "symbol_info('~a','~a',function_type,'~(~a~)').~%" sn pn sft)
       (let ((doc (documentation sym 'function))) (when doc (format t "doc_string('~a','~a',function,\"~a\").~%" sn pn (princ-to-string doc))))))
     '(when (symbol-plist sym) (format t "symbol_plist('~a','~a').~%" sn  (symbol-plist sym)))

))))))))

(psyminfo)





(defun method-info (m)(list (METHOD-QUALIFIERS m)(MOP:METHOD-LAMBDA-LIST m)(MOP:METHOD-SPECIALIZERS m)(MOP:METHOD-FUNCTION m)))
(defun print_whatnot (b)(princ b))
(defun print-trip (str a b)(unless (eq a b)(princ "mop_direct('")(princ a)(princ "','")(princ str)(princ "','")(princ b )(princ "').")(terpri)))
(defun print-subclasses (root &optional pre-print)
(let ((class (typecase root (class root) (symbol (find-class root)) (t (class-of root)))))
(dolist (item (mapcar #'MOP:slot-definition-name (MOP:class-direct-slots class)))(print-trip "slot" (class-name class) item))
(dolist (item (mapcar #'class-name (MOP:class-direct-superclasses class)))(print-trip "subclass" item  (class-name class)))
(print-trip "precedance" (class-name class) (mapcar #'class-name (cdr (MOP:class-precedence-list class))))
(when pre-print (print-trip "subclass"  (class-name pre-print) (class-name class)))
(dolist (item (mapcar #'method-info (MOP:class-direct-methods class)))  (print-trip "method"  (class-name class) item ))
(dolist (item (MOP:class-direct-subclasses class))
(print-subclasses item class))))
(print-subclasses t)


(defun method-info (m)
 (list 
  (METHOD-QUALIFIERS m)
  (CLOS:METHOD-LAMBDA-LIST m)
  (CLOS:METHOD-SPECIALIZERS m)
  (CLOS:METHOD-FUNCTION m)
  (CLOS:ACCESSOR-METHOD-SLOT-DEFINITION m)
  (DOCUMENTATION m)))

(defun print_whatnot (b)(princ b))
(defun print-trip (str a b)(unless (eq a b)(princ "mop_direct('")(princ a)(princ "','")(princ str)(princ "','")(princ b )(princ "').")(terpri)))
(defun print-subclasses (root &optional pre-print)
(let ((class (typecase root
(class root) (symbol (find-class root)) (t (class-of root)))))
(dolist (item (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots class)))
  (print-trip "slot" (class-name class) item))
(dolist (item (mapcar #'sb-mop:class-name (sb-mop:class-direct-superclasses class)))
  (print-trip "subclass" item  (class-name class)))
(print-trip "precedance" (class-name class) (mapcar #'sb-mop:class-name (cdr (sb-mop:compute-class-precedence-list class))))
(when pre-print (print-trip "subclass"  (class-name pre-print) (class-name class)))
; (dolist (item (mapcar #'method-info (sb-mop:class-direct-methods class)))  (print-trip "method"  (class-name class) item ))
(dolist (item (sb-mop:class-direct-subclasses class))
(print-subclasses item class))))

(print-subclasses t)

