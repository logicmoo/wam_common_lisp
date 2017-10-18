;;-*- Mode: Lisp; Package: CCL -*-

(in-package :ccl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A bunch of methods that delegate from fred-window to its window-key-handler



(defmethod ED-RUBOUT-CHAR ((f fred-delegation-mixin))
  (ed-rubout-char (fred-item f)))

(defmethod ED-END-OF-BUFFER ((f fred-delegation-mixin))
  (ed-end-of-buffer (fred-item f)))

;;; eof

