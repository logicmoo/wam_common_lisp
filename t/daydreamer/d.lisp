
(defpackage "COMMON-LISP-USER" (:nicknames "U" "USER" "CL-USER"))

(let ((*lisp-file-type* "cl") 
        (*default-pathname-defaults* (merge-pathnames "daydreamer/"))) (load "dd"))

 
