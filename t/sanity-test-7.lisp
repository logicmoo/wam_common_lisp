
;; durring compilatiom
(eval-when (:compile-toplevel)
  (print 'toplevel/compile-toplevel))

;; post compiled
(eval-when (:load-toplevel)
  (print 'toplevel/load-toplevel))

;; only loading
(eval-when (:execute)
  (print 'toplevel/execute))
 
(defun foo ()
  (list (eval-when (:compile-toplevel)
          (print 'foo/compile-toplevel))
        (eval-when (:load-toplevel)
          (print 'foo/load-toplevel))
        (eval-when (:execute)
          (print 'foo/execute))))
 
(print (foo))
 

