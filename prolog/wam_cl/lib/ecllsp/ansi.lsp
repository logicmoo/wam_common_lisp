;;;;  ANSI.LSP  -- Compatibility with ANSI Common Lisp

;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "SYSTEM")

(defun print-unreadable-object-function (object stream type identity function)
  (declare (:dynamic-extent function))
  (princ "#<" stream)
  (when type
    (prin1 (type-of object) stream))
  (when (and type function) (princ " " stream))
  (when function (funcall function))
  (when (and (or type function) identity) (princ " " stream))
  (when identity (princ (si:pointer object) stream))
  (princ ">" stream)
  nil)
  
(defmacro print-unreadable-object
	  ((object stream &key type identity) &body body)
  (if body
      `(flet ((.print-unreadable-object-body. () ,@body))
	 (print-unreadable-object-function
	   ,object ,stream ,type ,identity #'.print-unreadable-object-body.))
    `(print-unreadable-object-function ,object ,stream ,type ,identity nil)))
