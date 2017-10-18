;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-
 
;;; not all the predicates may be used as generators!

(defaxiom-set set-axioms

    ((member _x (_x . _tail)))
    ((member _x (_h . _tail)) <- 
     (member _x _tail))

    ((append nil _x _x))
    ((append (_h . _tail1) _x (_h . _tail2)) <- 
     (append _tail1 _x _tail2))

    ((union nil _set _set))
    ((union (_h . _tail) _set2 _set3) <- 
     (member _h _set2) (cut) (union _tail _set2 _set3))
    ((union (_h . _tail1) _set2 (_h . _tail2)) <- 
     (union _tail1 _set2 _tail2))

    ((subset (_h . _tail) _set) <- 
     (member _h _set) (cut) (subset _tail _set))
    ((subset nil _set))

    ((disjoint _set1 _set2) <- 
     (not (and (member _z _set1) (member _z _set2))))

    ((intersection nil _set nil))
    ((intersection (_h . _tail1) _set2 (_h . _tail2)) <- 
     (member _h _set2) (cut) (intersection _tail1 _set2 _tail2)) 
    ((intersection (_h . _tail) _set2 _set3) <- 
     (intersection _tail _set2 _set3)))

;;; eof

