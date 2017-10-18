;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; ===========================================================================
;;; RELATION PART
;;; contains the relations:

#|
(DEF-RELATION related-themes OF relation
(DEF-RELATION provide-resources OF relation
|#


(DEF-RELATION related-themes OF relation
  WITH properties = ((symetric reflexive))
       domains    = ((themes themes))
       assumable  = false)   ; originally it was true

(DEF-CLAUSE-SET related-themes
  ((related-themes knowledge-acquisition knowledge-modeling))
  ((related-themes knowledge-acquisition machine-learning))
  ((related-themes knowledge-modeling machine-learning))
  ((related-themes knowledge-modeling expert-system-tools))
  ((related-themes knowledge-modeling knowledge-representation))
  ((related-themes knowledge-modeling planning))
  ((related-themes machine-learning knowledge-representation))
  ((related-themes planning robotics))
  ((related-themes knowledge-representation expert-system-tools))
  ((related-themes knowledge-representation constraints))
  ((related-themes software-architecture programming))
  ((related-themes programming interfaces)))
  

;;; eof

