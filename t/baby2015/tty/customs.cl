;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base:10 -*-

(in-package "BABYLON")


(setf *default-procs*
      '(mini-frame-mixin mini-rule-mixin mini-prolog-mixin free-text-mixin lisp-mixin))

(setf *default-interface* '(normal-interface-mixin))

(setf *default-kb-configuration* 'globalc)

(setf *help-key* #\?)

(setf *c-help-key* #\!)

(setf *end-key* #\e)


(def$flavor normal-interface-mixin
	    ()
  (mini-interface-mixin)
  :settable-instance-variables)

(def$flavor normal-babylon
	    ()
  (mini-babylon)
  :settable-instance-variables)


(load-user-babylon-init-file)

;;; eof

