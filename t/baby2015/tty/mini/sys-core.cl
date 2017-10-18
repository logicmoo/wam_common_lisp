;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;           Copyright   1987    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG

;; DATE:     August 1987
;; AUTHOR:   Eckehard Gross

;; This file depends on:  common>*

;;Contents: a basic component to build a system knowledge-base.
;;          provides defaults for processor-mixins and interface-mixins
;;          provides methods to switch between kbs, to load or kill a kb
;;          starts babylon
;;          the methods provided by this flavor could easily be replaced by functions         
  
(def$flavor system-core
	((kb-name '*babylon*)
	 (language *default-language*)
	 (default-procs nil)
	 (default-interface nil)
	 (default-kb-configuration nil))
	()
  :settable-instance-variables
  (:required-instance-variables dialog-stream)
  (:documentation "basic component for a system knowledge-base"))


;;------------------------------------------------------------ 

(def$method (system-core :select-kb) ()
  ($send self :run))

(def$method (system-core :run) ()
  t)

(def$method (system-core :kill-kb) ()
  (setq *babylon* nil))

;;---------------------------------------------------------------------------- 

(def$method (system-core :system-status) ()
  "displays which kb is current."

  (let ((*language* language))
    (if (not (is-activated-kb))
	($send self :notify (getentry none-kb-current-str babylon-io-table))      
	($send self :notify
	       (format nil (getentry current-kb-fstr babylon-io-table)
		       (send-kb :kb-name))))))


(def$method (system-core :select-current-kb) ()
  "activates the current kb."

  (let ((*language* language))
    (cond ((is-activated-kb) (send-kb :select-kb) :exit)
	  (t ($send self :notify
		    (getentry none-kb-current-str babylon-io-table))))))


(def$method (system-core :get-known-knowledge-bases) ()
  *known-knowledge-bases*)


(def$method (system-core :choose-kb) ()
  "internal method. presents a menu to select a kb."

  (let ((*language* language)
	(known-kbs ($send self :get-known-knowledge-bases)))
    (cond ((null known-kbs)
	   ($send self :notify (getentry none-kb-known-str babylon-io-table))
	   nil)
	  (t (let ((kbname ($send self :choose-from-menu 
				  known-kbs
				  (getentry choose-kb-str babylon-io-table))))
	       (if kbname
		   (symbol-value kbname)))))))

(def$method (system-core :select-any-kb) ()
  "activates a kb selected via menu thereby making it current."

  (let ((kb ($send self :choose-kb)))
    (when kb
      ($send kb :select-kb)
      :exit)))


(def$method (system-core :kill-any-kb) ()
  "kills a kb selected via menu."

  (let ((*language* language)
	(kb ($send self :choose-kb)))
    (when kb
      (let ((action (format nil (getentry kill-kb-fstr babylon-io-table)
			    ($send kb :kb-name))))
      (when ($send self :confirm action)
	($send kb :kill-kb))))))

(def$method (system-core :load-any-file) ()
  "prompts for a file name to be loaded."
  
  (let ((*language* language)
	(*default-dialog-stream* dialog-stream)
	(old-kb *current-knowledge-base*)
	(old-ax (first *axiom-sets*)))
    (multiple-value-bind (file-loaded source-file)
	(cc-load ($send self :prompt-for-input
			(getentry enter-file-fstr babylon-io-table))
		 :recompile nil)
      (declare (ignore file-loaded))
      (unless (eq old-ax (first *axiom-sets*))
	(setf (get (first *axiom-sets*) :babsource) source-file))
      (unless (eq old-kb *current-knowledge-base*)
	(send-kb :send-if-handles :loaded source-file)
	(send-kb :send-if-handles :expose-kb)))))
    
(def$method (system-core :exit-babylon) ()
  :exit)


;;------------------------------------------------------------ 

(defun call-babylon ()
  (send-bab :select-kb))


(defmacro make-babylon (type &rest init-plist)
  "constructor macro for a system kb."
  
  `(progn     
     (if (not (flavorp ,type))
	 (bab-require ,type))
     (if (and (boundp '*babylon*)
	      (flavor-typep *babylon* 'system-core))
	 ($send *babylon* :kill-kb))
     (setq *babylon*  (make-window-or-instance ,type ,@init-plist))))

;;eof

