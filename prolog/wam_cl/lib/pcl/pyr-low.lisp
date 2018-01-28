;;; -*- Mode:LISP; Package:PCL; Base:10; Syntax:Common-lisp -*-
;;;
;;; *************************************************************************
;;; Copyright (c) 1985, 1986, 1987, 1988, 1989, 1990 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;; 
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;; 
;;; Any person obtaining a copy of this software is requested to send their
;;; name and post office or electronic mail address to:
;;;   CommonLoops Coordinator
;;;   Xerox PARC
;;;   3333 Coyote Hill Rd.
;;;   Palo Alto, CA 94304
;;; (or send Arpanet mail to CommonLoops-Coordinator.pa@Xerox.arpa)
;;;
;;; Suggestions, comments and requests for improvements are also welcome.
;;; *************************************************************************
;;; 
;;; This is the Pyramid version of low.lisp -- it runs with versions 1.1
;;; and newer -- Created by David Bein Mon May  4 11:22:30 1987
;;;
(in-package 'pcl)

  ;;   
;;;;;; Cache No's
  ;;  

;;; The purpose behind the shift is that the bottom 2 bits are always 0
;;; We use the same scheme for symbols and objects although a good
;;; case may be made for shifting objects more since they will
;;; be aligned differently...

;(defmacro symbol-cache-no (symbol mask)
;  `(logand (the fixnum (ash (lisp::%sp-make-fixnum ,symbol) -2))
;	  (the fixnum ,mask)))

(defmacro object-cache-no (symbol mask)
  `(logand (the fixnum (ash (lisp::%sp-make-fixnum ,symbol) -2))
	  (the fixnum ,mask)))



