;;;-*-Mode:LISP; Package:(PCL LISP 1000); Base:10; Syntax:Common-lisp -*-
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

(in-package 'pcl)

;;;
;;; pre-allocate generic function caches.  The hope is that this will put
;;; them nicely together in memory, and that that may be a win.  Of course
;;; the first gc copy will probably blow that out, this really wants to be
;;; wrapped in something that declares the area static.
;;;
;;; This preallocation only creates about 25% more caches than PCL itself
;;; uses need.  Some ports may want to preallocate some more of these.
;;; 
(eval-when (load)
  (flet ((allocate (n size)
	   (mapcar #'free-cache-vector
		   (mapcar #'get-cache-vector
			   (make-list n :initial-element size)))))
    (allocate 128 4)
    (allocate 64 8)
    (allocate 64 9)
    (allocate 32 16)
    (allocate 16 17)
    (allocate 16 32)
    (allocate 1  64)))

