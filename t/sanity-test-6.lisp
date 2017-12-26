(defmacro alpha (x y) `(beta ,x ,y))  ;;   ALPHA
(defmacro beta (x y) `(gamma ,x ,y))  ;;   BETA
 (defmacro delta (x y) `(gamma ,x ,y))  ;;   EPSILON
 (defmacro expand (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand form env)
     `(values ',expansion ',expanded-p)))  ;;   EXPAND
 (defmacro expand-1 (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand-1 form env)
     `(values ',expansion ',expanded-p)))  ;;   EXPAND-1

(print '(foo))
(prolog-call "lisp")
(print "EOF")
