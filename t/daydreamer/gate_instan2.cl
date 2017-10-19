(defun ob$instan-special
   (template bindings depth omit-slots include-slots substit abstract omit-proc)
  (cond
   ((ty$instance? template 'uor)
    (ob$instantiate2 (ob$get template 'obj) bindings depth omit-slots
                     include-slots substit abstract omit-proc))
   ((ty$instance? template 'uand)
    (cond
     ((any (lambda (elem) (if (not (ob? elem)) elem nil))
           (ob$gets template 'obj)))
     (else
      (yloop
       (initial (result nil)
                (found nil))
       (yfor elem in (ob$gets template 'obj))
       (yuntil result)
       (ydo
        (if (and (var? elem)
                 (setq found (bd-hyper-lookup (variable-name elem) bindings)))
            (cond
             ((var? found)
              (setq *any-unbound?* t)
;              (ndbg *gate-dbg* ob-warn "(?~A binding cycle)~%"
;                    (variable-name found))
              (setq result found))
             ((and (ob? found) (vars-in? found))
              (setq result (ob$instantiate2 found bindings depth omit-slots
                                           include-slots substit abstract
                                           omit-proc)))
             (else
              (setq result (ob$instantiate2 found bindings depth omit-slots
                                           include-slots substit abstract
                                           omit-proc))))))
       (yresult
        (if result
            result
            (if (any? (lambda (elem) (and (ob? elem) (not (var? elem))))
                      (ob$gets template 'obj))
                (let ((result-ob (ob$create-empty)))
                 (setq *instan-obs* (cons (cons template result-ob)
                                         *instan-obs*))
                 (yloop
                  (initial (result nil))
                  (yfor elem in (ob$gets template 'obj))
                  (ydo
                   (if (and (ob? elem) (not (var? elem)))
                       (progn
                        (setq result (ob$instantiate2 elem bindings depth
                                                     omit-slots include-slots
                                                     substit abstract
                                                     omit-proc))
                        (yloop
                         (yfor pair in (ob$pairs result))
                         (ydo 
                          ; Todo: something about type here.
                          (ob$add result-ob (slots-name pair)
                                  (slots-value pair))))
                        (if (ob? result)
                            (ob$destroy result)
                            (error "~A not ob to destroy" result))))))
                        ; Todo: should not always destroy? What if result
                        ; isn't a copy?
                  result-ob)
                (ob$copy template))))))))
   ((ty$instance? template 'unot)
    (ob$fcreate `(UNOT obj ,(ob$get template 'obj))))
   ((ty$instance? template 'udist)
    (ob$fcreate `(UDIST obj ,(ob$get template 'obj))))
   ((ty$instance? template 'uproc)
    'uproc-answer-true)
   ((ty$instance? template 'uselect)
    (let ((ob (ob$instantiate2 (ob$get template 'pattern)
                                 bindings depth omit-slots
                 include-slots substit abstract omit-proc)))
      (if (ob? ob)
          (ob$get ob (ob$get template 'slot))
          ob)))
   ((ty$instance? template 'ucode)
    (let ((old-ob-bindings *ob-bindings*)
          (result nil))
      (setq *ob-bindings* bindings)
      (setq result (eval (ob$get template 'proc)))
      (setq *ob-bindings* old-ob-bindings)
      result))
   ((ty$instance? template 'ubind!)
    (let ((result (ob$instantiate2 (ob$get template 'pattern) bindings
                                    depth omit-slots
                                    include-slots substit abstract omit-proc)))
      (bd-bind! (variable-name (ob$get template 'var))
                result
                bindings)
      result))
   (else (error "~A unknown special" template))))

