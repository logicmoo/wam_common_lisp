;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; DATE:     April 1987
;; AUTHORS:  Franco di Primio, Eckehard Gross

;; This file depends on:  common>*
;;                        frames>basic>frames
;;                        frames>basic>bf-inter
;;                        
;; Contents: a minimal version of a frame processor
 

;;-----------------------------------------------------------------------------
;;                   FLAVOR BASIC-FRAME-PROCESSOR 
;;-----------------------------------------------------------------------------

(def$flavor basic-frame-processor
	()
	(frame-interpreter frame-base)
  (:documentation "This flavor represents a minimal version of a frame processor."))


;; reset-slot-value problematisch

;(def$method (basic-frame-processor :reset-instances) (&optional (completely t))
;  (mapc #'(lambda (an-instance-name)
;	    (if completely
;		(eval (get-instance-def an-instance-name))
;		;; resets only :VALUE property
;		($send (get-instance an-instance-name) :reset-slots-value)))
;	instances-list))


(def$method (basic-frame-processor :reset-proc) ()
  "resets the processor to initial state."
  (mapcar #'(lambda (an-instance-name)
              ;(eval (get-instance-def an-instance-name)))
              (<- an-instance-name :reset-yourself))  
          ; now you can specialize the reset-yourself method per frame
          instances-list))

;
;(def$method (basic-frame-processor :reset-pointer) ()
;  "synchronizes the lists of frames and instances maintained
;by the knowledge-base and the frame-processor."
;  (setf frames-list ($send meta-processor :frames))
;  (setf instances-list ($send meta-processor :instances)))


(def$method (basic-frame-processor :kb-inform) (&optional (stream *default-dialog-stream*))
  "prints statistics on frames and instances."
  (let ((frames ($send meta-processor :frames))
        (instances ($send meta-processor :instances)))
    (declare (list frames instances))
    (format stream (getentry no-of-frames-fstr frame-io-table) (length frames))
    (format stream (getentry no-of-instances-fstr frame-io-table) (length instances))
    t))


(def$method (basic-frame-processor :print) (&optional (stream *default-dialog-stream*)) 
  "prints all definitions of frames and instances."
  (let ((frames ($send meta-processor :frames)))
    (format stream  (getentry object-header-str frame-io-table))
    (cond (frames
	   (mapc #'(lambda (a-frame-name)
		     (format stream 
			     (getentry frame-header-fstr frame-io-table)
			     a-frame-name)
		     (PRINT-FRAME a-frame-name stream) 
		     (PRINT-INSTANCES a-frame-name stream))		     
		 frames)))
    t))


#-:FMCS(compile-$flavor-$methods basic-frame-processor)


;;; eof

