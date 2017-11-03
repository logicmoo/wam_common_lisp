
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


