;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1991    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;;  AUTHOR:  Juergen Walther

;;; this file is loaded after starting an babylon image

(progn
  (terpri)
  (princ ";;; Loading babylon init file ...")
  (setf *recompile* nil))

;;; do not know do do that in non MCL implementations!?

#+:MCL(eval-enqueue `(in-package "BABYLON"))


;;; eof

