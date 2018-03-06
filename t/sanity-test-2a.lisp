

(defmacro lambda-as-macro (&whole form &rest bvl-decls-and-body)
  (declare (ignore bvl-decls-and-body))
  `#',form)
  

