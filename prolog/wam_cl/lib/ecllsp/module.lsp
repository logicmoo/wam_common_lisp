;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;	module routines

(in-package "SYSTEM")

(defvar *modules* nil
  "List of module names that have been loaded into ECL.
See PROVIDE and REQUIRE.")

(defun provide (module-name)
  "Args: (module-name)
Declares the start of a program module.  Usually used at the beginning of a
program file.  MODULE-NAME may be a string or a symbol.  If it is a string, it
is pushed onto *MODULES*.  If it is a symbol, its print name is pushed.  See
REQUIRE."
  (setq *modules*
        (adjoin (string module-name) *modules* :test #'string=)))


(defun require (module-name
                &optional (pathname (string-downcase (string module-name))))
  "Args: (module-name &optional pathname)
If the specified module name is not found in *MODULES*, then loads the files
specified by PATHNAME.  Otherwise, does nothing.  MODULE-NAME may be a string
or a symbol.  If it is a symbol, the print name of the symbol is used as the
module name.  PATHNAME may be a pathname object or it may be a list of
pathname objects.  If PATHNAME is not given, then ECL tries to load the file
whose file name is MODULE-NAME and whose filetype is either .FASL, .LSP, or
none.  See PROVIDE."
  (let ((*default-pathname-defaults* #P""))
    (unless (member (string module-name)
                    *modules* :test #'string=)
      (if (atom pathname)
	  (load pathname)
	  (dolist (p pathname)
	    (load p))))))

