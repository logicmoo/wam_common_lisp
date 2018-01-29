;;; -*- Mode: Lisp; Base: 10; Syntax: Common-Lisp; Package: DSYS -*-
;;; File: sysdef.lisp 
;;; Author: Richard Harris
;;;
;;;	ROSE - Rensselaer Object System for Engineering
;;;	Common Lisp Implementation
;;;
;;; 			   Copyright (c) 1990 by 
;;; 	      Rensselaer Polytechnic Institute, Troy, New York.
;;; 			    All Rights Reserved
;;;
;;;	THE SOFTWARE AND ACCOMPANYING WRITTEN MATERIALS ARE PROVIDED
;;;	\"AS IS\" AND WITHOUT ANY WARRANTY, INCLUDING BUT NOT LIMITED 
;;;	TO THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;	A PARTICULAR PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND
;;;	PERFORMANCE OF THE SOFTWARE AND USE OF THE ACCOMPANYING WRITTEN
;;;	MATERIALS IS ASSUMED BY YOU.  IN NO EVENT SHALL RENSSELAER 
;;;	POLYTECHNIC INSTITUTE BE LIABLE FOR ANY LOST REVENUE, LOST 
;;;	PROFITS OR OTHER INCIDENTAL OR CONSEQUENTIAL DAMAGES, EVEN
;;;	IF ADVISED OF THE POSSIBILITIES OF SUCH DAMAGES, WHERE DAMAGES
;;;	ARISE OUT OF OR IN CONNECTION WITH THE USE OF, PERFORMANCE OR
;;;	NONPERFORMANCE OF THIS SOFTWARE.
;;;
;;;	This software and accompanying written materials may not be 
;;;	distributed outside your organization or outside the United 
;;;	States of America without express written authorization from
;;;	Rensselaer Polytechnic Institute.
;;;
;;;	This work has been sponsored in part by Defense Advanced Research 
;;;	Projects Agency (DARPA) under contract number MDA972-88-C0047 for
;;; 	DARPA Initiative in Concurrent Engineering (DICE).  This material
;;;	may be reproduced by or for the U.S. Government pursuant to the 
;;;	copyright license under the clause at DFARS 252.227-7013 7/26/90.
;;; 

(in-package "DSYS")

(defvar *pcl-compiled-p* nil)
(defvar *pcl-loaded-p* nil)

(unless (boundp 'pcl::*redefined-functions*)
  (setq pcl::*redefined-functions* nil))

(defun pcl::reset-pcl-package ()		; Try to do this safely
  (let* ((vars '(pcl::*pcl-directory* 
		 pcl::*default-pathname-extensions* 
		 pcl::*pathname-extensions*
		 pcl::*redefined-functions*))
	 (names (mapcar #'symbol-name vars))
	 (values (mapcar #'symbol-value vars)))
    (let ((pkg (find-package "PCL")))
      (do-symbols (sym pkg)
	(when (eq pkg (symbol-package sym))
	  (if (constantp sym)
	      (unintern sym pkg)
	      (progn
		(makunbound sym)
		(unless (eq sym 'pcl::reset-pcl-package)
		  (fmakunbound sym))
		#+cmu (fmakunbound `(setf ,sym))
		(setf (symbol-plist sym) nil))))))
    (let ((pkg (find-package "SLOT-ACCESSOR-NAME")))
      (when pkg
	(do-symbols (sym pkg)
	  (makunbound sym)
	  (fmakunbound sym)
	  (setf (symbol-plist sym) nil))))
    (let ((pcl (find-package "PCL")))
      (mapcar #'(lambda (name value)
		  (let ((var (intern name pcl)))
		    (proclaim `(special ,var))
		    (set var value)))
	      names values))      
    (dolist (sym pcl::*redefined-functions*)
      (setf (symbol-function sym) (get sym ':definition-before-pcl)))
    nil))

(defun reset-pcl-package ()
  #-cmu
  (unless (compiled-function-p #'pcl::reset-pcl-package)
    (compile 'pcl::reset-pcl-package))
  (pcl::reset-pcl-package)
  (let ((defsys (subfile '("pcl") :name "defsys")))
    (setq pcl::*pcl-directory* defsys)
    (load-file defsys))
  (mapc #'(lambda (path)
	    (setf (lfi-fwd (get-loaded-file-info path)) 0))
	(pcl-binary-files)))

(defun pcl-binary-files ()
  (pcl::system-binary-files 'pcl::pcl))

(defun maybe-load-defsys (&optional compile-defsys-p)
  (let ((defsys (subfile '("pcl") :name "defsys"))
	(*use-default-pathname-type* nil)
	(*skip-load-if-loaded-p* t)
	(*skip-compile-file-fwd* 0))
    (set 'pcl::*pcl-directory* defsys)
    (when compile-defsys-p
      (compile-file defsys))
    (let ((b-s 'pcl::*boot-state*))
      (when (and (boundp b-s) (symbol-value b-s))
	#+ignore (reset-pcl-package)))
    (load-file defsys)))  

(defun maybe-load-pcl (&optional force-p)
  (unless (and (null force-p)
	       (fboundp 'pcl::system-binary-files)
	       (every #'(lambda (path)
			  (let* ((path-fwd (file-write-date path))
				 (lfi (get-loaded-file-info path)))
			    (and lfi path-fwd (= path-fwd (lfi-fwd lfi)))))
		      (pcl-binary-files)))
    (reset-pcl-package)
    (pcl::load-pcl)))

(defsystem pcl
    (:pretty-name "PCL")
  #+akcl
  (:forms 
   :compile (let ((cfn (subfile '("pcl") :name "collectfn" :type "lisp")))
	      (unless (probe-file cfn)
		(run-unix-command 
		 (format nil "ln -s ~A ~A"
			 (namestring (merge-pathnames "../cmpnew/collectfn.lsp" 
						      si::*system-directory*))
			 (namestring cfn))))))
				     
  #+akcl
  "collectfn"
  (:forms 
   :compile
   (progn
     (maybe-load-defsys t)
     (if (and (fboundp 'pcl::operation-transformations)
	      (every #'(lambda (trans)
			 (eq (car trans) :load))
		     (pcl::operation-transformations 'pcl::pcl :compile)))
	 (maybe-load-pcl)
	 (let ((b-s 'pcl::*boot-state*))
	   (when (and (boundp b-s) (symbol-value b-s))
	     (reset-pcl-package))
	   #+akcl (compiler::emit-fn t)
	   #+akcl (load (merge-pathnames "../lsp/sys-proclaim.lisp" 
					 si::*system-directory*))
	   (#+cmu with-compilation-unit #-cmu progn
	    #+cmu (:optimize 
		   '(optimize (user::debug-info #+small .5 #-small 2)
		              (speed #+testing 1 #-testing 2)
		              (safety #+testing 3 #-testing 0)
		              #+ignore (user::inhibit-warnings 2))
		   :context-declarations
		   '(#+ignore
		     (:external (declare (user::optimize-interface 
					  (safety 2) (debug-info 1))))))
	     (proclaim #+testing *testing-declaration* 
		       #-testing *fast-declaration*)
	     (pcl::compile-pcl))
	   (reset-pcl-package)
	   (maybe-load-pcl t))))
   :load
   (progn 
     (maybe-load-pcl)
     #+cmu (lisp::purify))))

(defparameter *pcl-files*
  '((("systems") "lisp"
     "pcl")
    (("pcl") "lisp"
     "sysdef"
     "boot" "braid" "cache" "cloe-low" "cmu-low" "combin" "compat"
     "construct" "coral-low" "cpatch" "cpl" "ctypes" "defclass" "defcombin"
     "defs" "defsys" "dfun" "dlap" "env" "excl-low" "fin" "fixup" "fngen" "fsc"
     "gcl-patches" "genera-low" "gold-low" "hp-low" "ibcl-low" "ibcl-patches"
     "init" "iterate" "kcl-low" "kcl-patches" "lap" "low" "lucid-low" "macros"
     "methods" "pcl-env-internal" "pcl-env" "pkg" "plap" "precom1" "precom2"
     "precom4" "pyr-low" "pyr-patches" "quadlap" "rel-7-2-patches" "rel-8-patches"
     "slots" "std-class" "sys-proclaim" "ti-low" "ti-patches" "vaxl-low" "vector" "walk"
     "xerox-low" "xerox-patches")
    (("pcl") "text"
     "12-7-88-notes" "3-17-88-notes" "3-19-87-notes" "4-21-87-notes"
     "4-29-87-notes" "5-22-87-notes" "5-22-89-notes" "8-28-88-notes"
     "get-pcl" "kcl-mods" "kcl-notes" "lap" "notes" "pcl-env" "readme")))

