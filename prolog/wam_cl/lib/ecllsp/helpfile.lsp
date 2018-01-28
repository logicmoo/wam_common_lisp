;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package "SYSTEM")

(export '(get-documentation set-documentation))

;;;;----------------------------------------------------------------------
;;;;  Help files
;;;;

(defun read-help-file (path)
  (let* ((*package* (find-package "CL"))
	 (file (open path :direction :input)))
    (do ((end nil)
	 (h (make-hash-table :size 1024 :test #'eql)))
	(end h)
      (do ((c (read-char file nil)))
	  ((or (not c) (eq c #\^_))
	   (when (not c) (setq end t)))
	)
      (when (not end)
	(let* ((key (read file))
	       (value (read file)))
	  (si::hash-set key h value))))))

(defun dump-help-file (hash-table path &optional (merge nil))
  (let ((entries nil))
    (when merge
      (let ((old-hash (read-help-file path)))
	(push old-hash *documentation-pool*)
	(maphash #'(lambda (key doc)
		     (when doc
		       (do ((list doc)
			    (doc-type (first list))
			    (string (second list)))
			   (list)
			 (set-documentation key doc-type string))))
		 hash-table)
	(setq hash-table (pop *documentation-pool*))))
    (maphash #'(lambda (key doc)
		 (when (and (symbolp key) doc)
		   (push (cons key doc) entries)))
	     hash-table)
    (setq entries (sort entries #'string-lessp :key #'car))
    (let* ((*package* (find-package "CL"))
	   (file (open path :direction :output)))
      (dolist (l entries)
	(format file "~A~S~%~S~%" #\^_ (car l) (rest l)))
      (close file)
      path)))

(defun search-help-file (key path &aux (pos 0))
  (labels ((bin-search (file start end &aux (delta 0) (middle 0) sym)
	     (declare (fixnum start end delta middle))
	     (when (< start end)
	       (setq middle (round (+ start end) 2))
	       (file-position file middle)
	       (if (and (plusp (setq delta (scan-for #\^_ file)))
			(<= delta (- end middle)))
		 (if (equal key (setq sym (read file)))
		   t
		   (if (string< key sym)
		     (bin-search file start (1- middle))
		     (bin-search file (+ middle delta) end)))
		 (bin-search file start (1- middle)))))
	   (scan-for (char file)
	     (do ((v #\space (read-char file nil nil))
		  (n 0 (1+ n)))
		 ((or (eql v #\^_) (not v)) (if v n -1))
	       (declare (fixnum n)))))
    (when (not (probe-file path))
      (return-from search-help-file nil))
    (let* ((*package* (find-package "CL"))
	   (file (open path :direction :input))
	   output)
      (when (bin-search file 0 (file-length file))
	(setq f t)
	(setq output (read file)))
      (close file)
      output)))

;;;;----------------------------------------------------------------------
;;;; Documentation system
;;;;

#+ecl-min
(progn
  (*make-special '*documentation-pool*)
  (setq *documentation-pool* nil)
  (*make-special '*keep-documentation*)
  (setq *keep-documentation* t))
#-ecl-min
(progn
  (defvar *documentation-pool* (list (make-hash-table :test #'eq :size 128)
				     "SYS:help.doc"))
  (defvar *keep-documentation* t))

(defun new-documentation-pool (&optional (size 1024))
  "Args: (&optional hash-size)
Sets up a new hash table for storing documentation strings."
  (push (make-hash-table :test #'eql :size size)
	*documentation-pool*))

(defun dump-documentation (file &optional (merge nil))
  "Args: (filespec &optional (merge nil))
Saves the current hash table for documentation strings to the specificed file.
If MERGE is true, merges the contents of this table with the original values in
the help file."
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (dump-help-file dict file merge)
      (rplaca *documentation-pool* file))))

(defun get-documentation (object doc-type &aux output)
  (dolist (dict *documentation-pool*)
    (cond ((hash-table-p dict)
	   (when (and (setq output (gethash object dict))
		      (setq output (getf output doc-type)))
	     (return-from get-documentation output)))
	  ((and (symbolp object) (stringp dict))
	   (when (and (setq output (search-help-file object dict))
		      (setq output (getf output doc-type)))
	     (return-from get-documentation output)))))))

(defun set-documentation (object doc-type string)
  (when (not (or (stringp string) (null string)))
    (error "~S is not a valid documentation string" string))
  (let ((dict (first *documentation-pool*)))
    (when (hash-table-p dict)
      (let ((plist (gethash object dict)))
	(setq plist (if string
			(put-f plist string doc-type)
			(rem-f plist doc-type)))
	(if plist
	  (si::hash-set object dict plist)
	  (remhash object dict)))))
  string)

(defun expand-set-documentation (symbol doc-type string)
  (when (and *keep-documentation* string)
    (when (not (stringp string))
      (error "~S is not a valid documentation string" string))
    `((set-documentation ',symbol ',doc-type ,string))))

#-clos
(defun documentation (object type)
  "Args: (symbol doc-type)
Returns the DOC-TYPE doc-string of SYMBOL; NIL if none exists.  Possible doc-
types are:
	FUNCTION  (special forms, macros, and functions)
	VARIABLE  (global variables)
	TYPE      (type specifiers)
	STRUCTURE (structures)
	SETF      (SETF methods)
All built-in special forms, macros, functions, and variables have their doc-
strings."
  (cond ((member type '(function type variable setf structure))
	 (when (not (symbolp object))
	   (error "~S is not a symbol." object))
	 (si::get-documentation object type))
	(t
	 (error "~S is an unknown documentation type" type))))

#+ecl-min
(when (null *documentation-pool*) (new-documentation-pool 1024))
