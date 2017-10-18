;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1988    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHORS:  J.Walther, E. Gross
;; DATE:     April 1994, June 1988

(defvar *babylon-modules* nil)

(defvar *babylon-translations* nil)

(defvar *babylon-module-search-path* nil)

(defvar *babylon-patches-search-path* nil)

(defvar *recompile* nil)

(defvar *trans-path-fkt* nil)

(defmacro defbabylon-translation (name first-tname &optional second-tname)
  `(progn (push ,(if second-tname
		     `'(,name ,first-tname ,second-tname)
		     `'(,name ,first-tname)) 
		*babylon-translations*)
	  ,name))


(defun find-translation (string type)
  (let ((desc (assoc string *babylon-translations* :test #'string-equal)))    
    (when (not (null desc))
      (case type
	(source (second desc))
	(bin    (or (third  desc) (second desc)))))))
 

(defun transform-pathstring1 (pathstring type)
  (let* ((pos (position #\^ pathstring))
	 (logpath (if pos (subseq pathstring 0 (1+ pos))))
	 (transpath (if logpath (find-translation logpath type))))
    (cond ((null pos) nil)
	  ((null transpath)
	   (error (format nil "unknown logical pathname ~A" logpath)))
	  (t (concatenate 'string transpath (subseq pathstring (1+ pos)))))))


(defun transform-pathstring (pathstring type)
  (do* ((old-path (or (find-translation (string pathstring) type)
		      (string pathstring))
		  new-path) 
	(new-path (transform-pathstring1 old-path type)
		  (transform-pathstring1 old-path type)))
       ((null new-path)
	(if *trans-path-fkt*
	    (funcall *trans-path-fkt*
		     old-path (string *bab-host*) type)
	    old-path))))

(defun cc-load (file-name &key (recompile *recompile*) (error t))
  (let* ((source-file (transform-pathstring file-name 'source))
	 (binary-file (transform-pathstring file-name 'bin)))
    (cond ((probe-file binary-file)
	   (cond ((and recompile
                       (probe-file source-file)
		       (> (file-write-date source-file)
			  (file-write-date binary-file)))
		  (format *terminal-io* "~&Compiling ~A" (namestring source-file))
		  (compile-file source-file :output-file binary-file)
		  (values (load binary-file) source-file))
		 (t (values (load binary-file) source-file))))	  
	  ((probe-file source-file)
	   (cond (recompile 
		  (format *terminal-io* "~&Compiling ~A" (namestring source-file))
		  (compile-file source-file :output-file binary-file)
		  (values (load binary-file) source-file))
		 (t (values (load source-file) source-file))))
	  (error (if (equal source-file binary-file)
		     (error "~A not found" (namestring source-file))
		     (error "neither ~A nor ~A found"
			    (namestring binary-file) (namestring source-file)))))))

(defun search-cc-load (module-name pathlist)  
  (catch 'found
    (dolist (path-name pathlist)
      (when (cc-load
	      (concatenate 'string
			   path-name
			   (string-downcase module-name))
	      :error nil)
	(throw 'found t)))))

(defun bab-require (modul-name &optional pathlist)
  (unless (find (string modul-name) *babylon-modules* :test #'string-equal)
    (let ((short-name (or (find-translation (string modul-name) 'source)
			  (string modul-name))))
      (cond ((not (null pathlist))
	     (dolist (path pathlist)
	       (cc-load path))
	     modul-name)
	    ((search-cc-load short-name *babylon-module-search-path*)
	     modul-name)
	    (t (error "module ~S not found" modul-name)))
      (if (search-cc-load short-name *babylon-patches-search-path*)
	  (format *terminal-io* "~&Patches loaded for ~S" modul-name)))))

(defun bab-provide (module)
  (pushnew (if (symbolp module) (symbol-name module) module)
           *babylon-modules* :test #'string-equal))


;;; eof

