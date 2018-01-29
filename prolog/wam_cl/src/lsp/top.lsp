;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;  top.lsp
;;;;
;;;;  Top-level loop, break loop, and error handlers
;;;;
;;;;  Revised on July 11, by Carl Hoffman.
;;;;  Modified Oct 1986 by Ken Rimey.
;;;;  Reworked March 1987, by Edward Wang.
;;;;  Merged into new distribution Sept 1987, by Edward Wang.
;;;;  Reworked for Threads November 1988, by Giuseppe Attardi.
;;;;  Reworked for CLOS November 1988, by Giuseppe Attardi.

(in-package #:compiler) ; just to bypass mtcl boot problems

(in-package "COMMON-LISP")
;(require '(iolib seqlib trace))

(export '(+ ++ +++ - * ** *** / // ///))
(export '(break warn))
(export '*break-on-warnings*)
(export '*break-enable*)
#+Threads
(export '(%disable-scheduler %enable-scheduler))

(in-package "SYSTEM")

(export '(*break-readtable* *lisp-init-file-list* *tpl-evalhook*))
#+Threads
(export '(*scheduler-disabled-in-error* *break-level*))

#+Threads
(defvar *scheduler-disabled-in-error* t)

(eval-when (compile) (proclaim '(optimize (safety 2) (space 3))))

(defvar +)
(defvar ++)
(defvar +++)
(defvar -)
(defvar *)
(defvar **)
(defvar ***)
(defvar /)
(defvar //)
(defvar ///)

;(defvar *lisp-init-file-list* '("./init" "~/.dcl"))
(defvar *lisp-init-file-list* '("./init" ))

(defvar *quit-tag* (cons nil nil))
(defvar *quit-tags* nil)
(defvar *continue-tag* (cons nil nil))	; return tag from current error
(defvar *break-level* 0)		; nesting level of error loops
(defvar *break-env* nil)
(defvar *ihs-base* 1)
(defvar *ihs-top* 1)
(defvar *ihs-current* 1)
(defvar *frs-base* 0)
(defvar *frs-top* 0)
(defvar *tpl-continuable* t)
(defvar *tpl-prompt-hook* nil)
(defvar *eof* (cons nil nil))

(defvar *break-enable* t)
(defvar *break-message* nil)
(defvar *break-on-warnings* nil)
(defvar *break-readtable* nil)
(defvar *tpl-level* -1)			; nesting level of top-level loops
(defvar *step-level* 0)			; repeated from trace.lsp

(defvar *break-hidden-functions* nil)
(defvar *break-hidden-packages* (list (find-package 'system)))

(defvar *tpl-commands* nil)
(defconstant tpl-commands
   '(("Top level commands"
      ((:cf :compile-file) tpl-compile-command :string
       ":cf		Compile file"
       ":compile-file &string &rest files		[Top level command]~@
	:cf &string &rest files				[Abbreviation]~@
	~@
	Compile files.  With no arguments, uses values from latest :cf~@
	command.  File extensions are optional.~%")
      ((:exit :eof) quit :eval
       ":exit or ^D	Exit Lisp"
       ":exit &eval &optional (status 0)		[Top level command]~@
	~@
	Exit Lisp without further confirmation.~%")
      ((:ld :load) tpl-load-command :string
       ":ld		Load file"
       ":load &string &rest files			[Top level command]~@
	:ld &string &rest files				[Abbreviation]~@
	~@
	Load files.  With no arguments, uses values from latest :ld~@
	or :cf command. File extensions are optional.~%")
      ((:step) tpl-step-command nil
       ":step		Single step form"
       ":step form					[Top level command]~@
	~@
	Evaluate form in single step mode.  While stepping, a new break~@
	level is invoked before every evaluation.  Extra commands are~@
	available at this time to control stepping and form evaluation.~%")
      ((:tr :trace) tpl-trace-command nil
       ":tr(ace)	Trace function"
       ":trace &rest functions				[Top level command]~@
	:tr &rest functions				[Abbreviation]~@
	~@
	Trace specified functions.  With no arguments, show currently~@
	traced functions.~@
	~@
	See also: :untrace.~%")
      ((:untr :untrace) tpl-untrace-command nil
       ":untr(ace)	Untrace function"
       ":untrace &rest functions			[Top level command]~@
	:untr &rest functions				[Abbreviation]~@
	~@
	Untrace specified functions.  With no arguments, untrace~@
	all functions.~@
	~@
	See also: :trace.~%")
      )
     ("Help commands"
      ((:apropos) tpl-apropos-command nil
       ":apropos	Apropos"
       ":apropos string &optional package		[Top level command]~@
	~@
	Finds all available symbols whose print names contain string.~@
	If a non NIL package is specified, only symbols in that package are considered.~@
	~%")
      ((:doc document) tpl-document-command nil
       ":doc(ument)	Document"
       ":document symbol				[Top level command]~@
	~@
	Displays documentation about function, print names contain string.~%")
      ((? :h :help) tpl-help-command nil
       ":h(elp) or ?	Help.  Type \":help help\" for more information"
       ":help &optional topic				[Top level command]~@
	:h &optional topic				[Abbrevation]~@
      	~@
	Print information on specified topic.  With no arguments, print~@
	quick summery of top level commands.~@
	~@
	Help information for top level commands follows the documentation~@
	style found in \"Common Lisp, the Language\"; and, in general, the~@
	commands themselves follow the conventions of Common Lisp functions,~@
	with the exception that arguments are normally not evaluated.~@
	Those commands that do evaluate their arguments are indicated by the~@
	keyword &eval in their description.  A third class of commands~@
	treat their arguments as whitespace-separated, case-sensitive~@
	strings, requiring double quotes only when necessary.  This style~@
	of argument processing is indicated by the keyword &string.~@
	For example, the :load command accepts a list of file names:
	~@
	:load &string &rest files			[Top level Command]~@
	~@
	whereas :exit, which requires an optional evaluated argument, is~@
	~@
	:exit &eval &optional status			[Top level Command]~%")
      )))

(defconstant break-commands
  '("Break commands"
     ((:q :quit) tpl-quit-command nil
       ":q(uit)		Return to some previous break level"
       ":quit &optional n				[Break command]~@
	:q &optional n					[Abbreviation]~@
	~@
	Without argument, return to top level;~@
	otherwise return to break level n.~%")
      ((:pop) (tpl-pop-command) :constant
       ":pop		Pop to previous break level"
       ":pop						[Break command]~@
	~@
	Pop to previous break level, or if already in top level,~@
	exit Lisp after confirmation.~%")
      ((:c :continue) tpl-continue-command nil
       ":c(ontinue)	Continue execution"
       ":continue					[Break command]~@
	:c						[Abbreviation]~@
	~@
	Continue execution.  Return from current break level to the caller.~@
	This command is only available when the break level is continuable~@
	(e.g., called from a correctable error or the function break).~%")
      ((:b :backtrace) tpl-backtrace nil
       ":b(acktrace)	Print backtrace"
       ":backtrace &optional n				[Break command]~@
	:b &optional n					[Abbreviation]~@
	~@
	Show function call history.  Only those functions called since~@
	the previous break level are shown.  In addition, functions compiled~@
	in-line or explicitly hidden are not displayed.  Without an argument,~@
	a concise backtrace is printed with the current function in upper~@
	case.  With integer argument n, the n functions above and including~@
	the current one are printed in a verbose format.  The current~@
	function is indicated with a '@' at the beginning of the line.~@
	With non-integer argument, all functions since the the previous break~@
	level are printed verbosely.~@
	~@
	See also: :function, :previous, :next.~%")
      ((:f :function) tpl-print-current nil
       ":f(unction)	Show current function"
       ":function					[Break command]~@
	:f						[Abbreviation]~@
	~@
	Show current function.  The current function is the implicit focus~@
	of attention for several other commands.  When it is an interpreted~@
 	function, its lexical environment is available for inspection and~@
	becomes the environment for evaluating user input forms.~@
	~@
	See also: :backtrace, :next, previous, :variables.~%")
      ((:p :previous) tpl-previous nil
       ":p(revious)	Go to previous function"
       ":previous &optional (n 1)			[Break command]~@
	:p &optional (n 1)				[Abbreviation]~@
	~@
	Move to the nth previous visible function in the backtrace.~@
 	It becomes the new current function.~@
	~@
	See also: :backtrace, :function, :go, :next.~%")
      ((:n :next) tpl-next nil
       ":n(ext)		Go to next function"
       ":next &optional (n 1)				[Break command]~@
	:n &optional (n 1)				[Abbreviation]~@
	~@
	Move to the nth next visible function in the backtrace.  It becomes~@
	the new current function.~@
	~@
	See also: :backtrace, :function, :go, :previous.~%")
      ((:g :go) tpl-go nil
       ":g(o)		Go to next function"
       ":go &optional (n 1)				[Break command]~@
	:g &optional (n 1)				[Abbreviation]~@
	~@
	Move to the function at IHS[i].~@
	See also: :backtrace, :function, :next, :previous.~%")
      ((:fs :forward-search) tpl-forward-search :string
       ":fs             Search forward for function"
       ":forward-search &string substring		[Break command]~@
	:fs &string substring				[Abbreviation]~@
	~@
	Search forward in the backtrace for function containing substring.~@
	The match is case insensitive.~@
	~@
	See also: :backtrace, :function, :next.~%")
      ((:bs :backward-search) tpl-backward-search :string
       ":bs             Search backward for function"
       ":backward-search &string substring		[Break command]~@
	:bs &string substring				[Abbreviation]~@
	~@
	Search backward in the backtrace for function containing substring.~@
	The match is case insensitive.~@
	~@
	See also: :backtrace, :function, :previous.~%")
      ((:v :variables) tpl-variables-command nil
       ":v(ariables)	Show local variables, functions, blocks, and tags"
       ":variables &optional no-values			[Break command]~@
	:v &optional no-values				[Abbreviation]~@
	~@
	Show lexical variables, functions, block names, and tags local~@
	to the current function.  The current function must be interpreted.~@
	The values of local variables and functions are also shown,~@
	unless the argument is non-null.~%")
      ((:l :local) tpl-local-command nil
       ":l(ocal)	Return the nth local value on the stack"
       ":local &optional (n 0)				[Break command]~@
	:l &optional (n 0)				[Abbreviation]
	~@
	For compiled functions, return the value of the nth lexical variable.~@
	As is done normally, the returned value is both printed by the top~@
	level as well as saved in the variable *.~%")
      ((:hide) tpl-hide nil
       ":hide		Hide function"
       ":hide function					[Break command]~@
	~@
	Hide function.  A hidden function is not displayed in a backtrace.~@
	~@
	See also: :backtrace, :unhide, :hide-package.~%")
      ((:unhide) tpl-unhide nil
       ":unhide		Unhide function"
       ":unhide function				[Break command]~@
	~@
	Unhide function.  The specified function will be displayed in future~@
	backtraces, unless its home package is also hidden.~@
	~@
	See also: :backtrace, :hide, :unhide-package.~%")
      ((:hp :hide-package) tpl-hide-package nil
       ":hp		Hide package"
       ":hide-package package				[Break command]~@
	:hp package					[Abbreviation]~@
	~@
	Hide package.  Functions in a hidden package are not displayed~@
	in a backtrace.~@
	~@
	See also: :backtrace, :unhide-package.~%")
      ((:unhp :unhide-package) tpl-unhide-package nil
       ":unhp		Unhide package"
       ":unhide-package package				[Break command]~@
	:unhp package					[Abbreviation]~@
	~@
	Unhide package.  Functions in the specified package will be displayed~@
	in future backtraces, unless they are individually hidden.~@
	~@
	See also: :backtrace, :hide-package, :hide, :unhide.~%")
      ((:unhide-all) tpl-unhide-all nil
       ":unhide-all     Unhide all variables and packages"
       ":unhide-all					[Break command]~@
	~@
	Unhide all variables and packages.  All functions will be displayed~@
	in future backtraces.~@
	~@
	See also: :hide, :unhide, :hide-package, :unhide-package.~%")
      ((:vs :value-stack) tpl-vs-command nil
       ":vs             Show value stack"
       ":value-stack &optional n			[Break command]~@
	:vs &optional n					[Abbreviation]~@
	~@
	Without an argument, show the entire value stack since the previous~@
	break level.  With an integer argument n, print nothing, but return~@
	the nth value stack entry.~@
	~@
	See also: :local.~%")
      ((:bds :binding-stack) tpl-bds-command nil
       ":bds            Show binding stack"
       ":binding-stack &optional variable		[Break command]~@
	:bds &optional variable				[Abbreviation]~@
	~@
	Without an argument, show the entire binding stack since the previous~@
	break level.  With a variable name, print nothing, but return the~@
	value of the given variable on the binding stack.~%")
      ((:m :message) tpl-print-message nil
       ":m(essage)      Show error message"
       ":message					[Break command]~@
	:m						[Abbreviation]~@
	~@
	Show current error message.~%")
      ((:hs :help-stack) tpl-help-stack-command nil
       ":hs		Help stack"
       ":help-stack					[Break command]~@
	:hs						[Abbrevation]~@
	~@
	Lists the functions to access the LISP system stacks.~%")
      )
  )

(eval-when (compile eval)
  (defmacro notinline (form)
    `(locally (declare (notinline ,(car form)))
	      ,form)))

(defun top-level ()
  (let ((+ nil) (++ nil) (+++ nil)
	(- nil)
	(* nil) (** nil) (*** nil)
	(/ nil) (// nil) (/// nil))

    (unless *lisp-initialized*
      (setq *ihs-top* 0)
      (catch *quit-tag*
	(let ((*break-enable* nil))

	  (dolist (file *lisp-init-file-list*)
	    (when (notinline (load file :if-does-not-exist nil))
	      (return)))
	  (when (> (argc) 1)
	    (setq *system-directory* (argv 1)))
	  (when (>= (argc) 5)
	    (let ((flags (argv 4)))
	      (compile-file
	       (argv 2) :output-file (argv 3)
	       :o-file (case (schar flags 1) (#\0 nil) (#\1 t) (t (argv 5)))
	       :c-file (case (schar flags 2) (#\0 nil) (#\1 t) (t (argv 6)))
	       :h-file (case (schar flags 3) (#\0 nil) (#\1 t) (t (argv 7)))
	       :data-file (case (schar flags 4) (#\0 nil) (#\1 t) (t (argv 8)))
	       :system-p (char-equal (schar flags 0) #\S)))
	    (quit (if compiler::*error-p* 1 0)))))

      (format t "ECL (ECoLisp) ~A" (lisp-implementation-version))
      (format t "~%Copyright (C) 1984 Taiichi Yuasa and Masami Hagiya~@
	Copyright (C) 1993 Giuseppe Attardi~@
	ECL is free software, and you are welcome to redistribute it~@
	under certain conditions; see file 'Copyright' for details.")
      (format *standard-output* "~%Type :h for Help.  ")

      (setq *lisp-initialized* t))

    (in-package "USER")

    (catch *quit-tag*
      (let ((*tpl-level* -1))
	(tpl)))))

(defun warn (format-string &rest args)
  (let ((*print-level* 4)
	(*print-length* 4)
	(*print-case* :upcase))
    (cond (*break-on-warnings*
	   (apply #'break format-string args))
	  (t (format *error-output* "~&;;; Warning: ")
	     (let ((*indent-formatted-output* t))
	       (format *error-output* "~?" format-string args))
	     (terpri *error-output*)
	     nil))))

#+Threads
(defun universal-error-handler
  (error-name correctable function-name
	      continue-format-string error-format-string
	      args)
  (declare (ignore error-name))
  (unwind-protect
      (if (or (plusp *break-level*) (not *scheduler-disabled-in-error*))
	  (let ((*print-pretty* nil)
		(*print-level* 4)
		(*print-length* 4)
		(*print-case* :upcase))
	    (format *error-output* "~&~:[E~;Correctable e~]rror: " correctable)
	    (let ((*indent-formatted-output* t))
	      (format *error-output* "~?~%Signalled by ~A." error-format-string args
		      (or function-name "an anonymous function")))
	    (when correctable
		  (format *error-output* "~&~:[Continuing~;If continued~]: "
			  *break-enable*)
		  (let ((*indent-formatted-output* t))
		    (format *error-output* "~?" continue-format-string args)))
	    (terpri *error-output*)
	    (break-level correctable
			 (format nil "~?" error-format-string args)))
;;;	(let ((break-exit nil))
	  (let ((*print-pretty* nil)
		(*print-level* 4)
		(*print-length* 4)
		(*print-case* :upcase))
	    (sys:thread-break-in)
	    (format *error-output* "~&~:[E~;Correctable e~]rror: " correctable)
	    (let ((*indent-formatted-output* t))
	      (format *error-output* "~?~%Signalled by ~A." error-format-string args
		      (or function-name "an anonymous function")))
	    (when correctable
		  (format *error-output* "~&~:[Continuing~;If continued~]: "
			  *break-enable*)
		  (let ((*indent-formatted-output* t))
		    (format *error-output* "~?" continue-format-string args)))
	    (terpri *error-output*)
;;;	    (setq break-exit
	    (break-level correctable
			 (format nil "~?" error-format-string args))))
;;;))
    (sys:thread-break-resume)))

#-Threads
(defun universal-error-handler
  (error-name correctable function-name
	      continue-format-string error-format-string
	      args)
  (declare (ignore error-name))
  (let ((*print-pretty* nil)
        (*print-level* 4)
        (*print-length* 4)
        (*print-case* :upcase))
    (format *error-output* "~&~:[E~;Correctable e~]rror: " correctable)
    (let ((*indent-formatted-output* t))
      (format *error-output* "~?~%Signalled by ~A." error-format-string args
	      (or function-name "an anonymous function")))
    (when correctable
      (format *error-output* "~&~:[Continuing~;If continued~]: "
	      *break-enable*)
      (let ((*indent-formatted-output* t))
	(format *error-output* "~?" continue-format-string args)))
    (terpri *error-output*))
  (break-level correctable (format nil "~?" error-format-string args)))

(defun break (&optional format-string &rest args &aux message)
  (let ((*print-pretty* nil)
	(*print-level* 4)
	(*print-length* 4)
	(*print-case* :upcase))
    (cond (format-string
	   (format *error-output* "~&Break: ")
	   (let ((*indent-formatted-output* t))
	     (format *error-output* "~?" format-string args))
	   (terpri *error-output*)
	   (setq message (format nil "~?" format-string args)))
	  (t (format *error-output* "~&Break.~%")
	     (setq message nil))))
  (break-level t message)
  nil)

(defun terminal-interrupt (correctablep)
  (let ((*break-enable* t))
    (if correctablep
      (cerror "Continues execution." "Console interrupt.")
      (error "Console interrupt -- cannot continue."))))

(defun break-level (continuable *break-message*)
  (unless *break-enable*
    (throw *quit-tag* nil))
  (let ((*break-level* (1+ *break-level*))
	(*break-env* nil)
	(*standard-input* *debug-io*)
	(*standard-output* *debug-io*)
	(*readtable* (or *break-readtable* *readtable*)))
    (catch *continue-tag*
      (when (listen *debug-io*)
	    (warn "Clearing input from *debug-io*")
	    (clear-input *debug-io*))
      (tpl :continuable continuable
	   :commands (adjoin break-commands *tpl-commands*)))))

(defun tpl (&key ((:continuable *tpl-continuable*) t)
		 ((:commands *tpl-commands*) tpl-commands)
		 ((:prompt-hook *tpl-prompt-hook*) nil)
		 (quiet nil))
  (let* ((*ihs-base* (1+ *ihs-top*))
	 (*ihs-top* (1- (ihs-top)))
	 (*ihs-current* *ihs-top*)
	 (*frs-base* (or (sch-frs-base *frs-top* *ihs-base*) (1+ (frs-top))))
	 (*frs-top* (frs-top))
	 (*read-suppress* nil)
	 (*quit-tags* (cons *quit-tag* *quit-tags*))
	 (*quit-tag* (cons nil nil))
	 (*tpl-level* (1+ *tpl-level*))
	 values)
    (set-current-ihs)
    (unless quiet
      (break-where))
    (loop
     (setq +++ ++ ++ + + -)
     (when (zerop *tpl-level*)
	   (reset-stack-limits))
     (when (catch *quit-tag*
	     (tpl-prompt)
	     (setq - (notinline (tpl-read)))
	     (setq values
		   (multiple-value-list
		    (evalhook - nil nil *break-env*)))
	     (setq /// // // / / values *** ** ** * * (car /))
	     (tpl-print values)
	     nil)
	   (break-where)))))

(defun tpl-prompt ()
  (fresh-line)
  (when *tpl-prompt-hook*
    (funcall *tpl-prompt-hook*))
  (format t "~A~V,,,'>A "
	  (if (eq *package* (find-package 'user)) "" (package-name *package*))
	  (- *tpl-level* *step-level* -1)
	  ""))

(defun tpl-read ()
  (finish-output)
  (loop
    (case (peek-char nil *standard-input* nil :eof)
      ((#\))
       (warn "Ignoring an unmatched right parenthesis.")
       (read-char))
      ((#\space #\tab)
       (read-char))
      ((#\newline #\return)
       (read-char)
       ;; avoid repeating prompt on successive empty lines:
       (let ((command (tpl-make-command :newline "")))
	 (when command (return command))))
      (:eof
       (terpri)
       (return (tpl-make-command :eof "")))
      (#\:
       (return (tpl-make-command (read-preserving-whitespace)
				 (read-line))))
      (#\?
       (read-char)
       (case (peek-char nil *standard-input* nil :eof)
	 ((#\space #\tab #\newline #\return :eof)
	  (return (tpl-make-command :help (read-line))))
	 (t
	  (unread-char #\?)
	  (return (read)))))
      (t
       (return (read))))))

(defun tpl-make-command (name line &aux (c nil))
  (dolist (commands *tpl-commands*)
    (when (setq c (assoc name (cdr commands) :test #'member))
      (return)))
  (cond ((null c)
	 (if (eq name :newline)		; special handling for Newline.
	     nil
	   `(tpl-unknown-command ',name)))
	((eq (third c) :eval)
	 `(,(second c) . ,(tpl-parse-forms line)))
	((eq (third c) :string)
	 `(,(second c) . ,(tpl-parse-strings line)))
	((eq (third c) :constant)
	 (second c))
	(t
	 `(,(second c) . ,(tpl-parse-forms line t)))))

(defun tpl-parse-forms (line &optional quote)
  (with-input-from-string (stream line)
    (do ((form (read stream nil *eof*) (read stream nil *eof*))
	 (list nil))
	((eq form *eof*) (nreverse list))
      (push (if quote `',form form) list))))

(defun tpl-parse-strings (line)
  (do ((i 0 end)
       (start)
       (end)
       (list nil)
       (space-p #'(lambda (c) (or (eql c #\space) (eql c #\tab))))
       (length (length line)))
      ((>= i length) (nreverse list))
    (cond ((null (setq start (position-if-not space-p line :start i)))
	   (setq end length))
	  ((eql (schar line start) #\")
	   (multiple-value-bind
	       (string n)
	       (read-from-string line t nil :start start)
	     (push string list)
	     (setq end n)))
	  (t
	   (setq end (or (position-if space-p line :start start) length))
	   (push (subseq line start end) list)))))

(defun tpl-print (values)
  (fresh-line)
  (dolist (v values)
    (prin1 v)
    (terpri)))

(defun tpl-unknown-command (command)
  (format t "Unknown top level command: ~s~%" command)
  (values))

(defun tpl-pop-command (&rest any)
  (declare (ignore any))
  (throw (pop *quit-tags*) t))

(defun tpl-continue-command (&rest any)
  (declare (ignore any))
  (if *tpl-continuable*
    (throw *continue-tag* nil)
    (format t "Can't continue from error.~%"))
  (values))

(defun tpl-quit-command (&optional (level 0))
  (when (and (>= level 0) (< level *tpl-level*))
    (let ((x (nth (- *tpl-level* level 1) *quit-tags*)))
      (throw x x)))
  (tpl-print-current))

(defun tpl-previous (&optional (n 1))
  (do ((i (1- *ihs-current*) (1- i)))
      ((or (< i *ihs-base*) (<= n 0)))
    (when (ihs-visible i)
      (setq *ihs-current* i)
      (decf n)))
  (set-break-env)
  (tpl-print-current))

(defun tpl-next (&optional (n 1))
  (do ((i (1+ *ihs-current*) (1+ i)))
      ((or (> i *ihs-top*) (<= n 0)))
    (when (ihs-visible i)
      (setq *ihs-current* i)
      (decf n)))
  (set-break-env)
  (tpl-print-current))

(defun tpl-go (ihs-index)
  (setq *ihs-current* (min (max ihs-index *ihs-base*) *ihs-top*))
  (if (ihs-visible *ihs-current*)
      (progn (set-break-env) (tpl-print-current))
      (tpl-previous)))

(defun tpl-print-message ()
  (when *break-message*
    (princ *break-message*)
    (terpri))
  (values))

(defun tpl-variables-command (&optional no-values)
  (let ((*print-level* 2)
	(*print-length* 4)
	(*print-pretty* t)
        (fun (ihs-fun *ihs-current*))
        name args)
    (if (and (compiled-function-p fun)
	     (symbolp (setq name (compiled-function-name fun)))
	     (setq args (get name 'arglist)))
	(progn
#|
	  (format t
		  "Local variables:~%")
	  (do ((args args (cdr args))
	       (i (ihs-vs *ihs-current*) (1+ i)))
	      ((null args))
	    (declare (fixnum i))
	    (format t "~:[~s: ~s~;~s~]~%" no-values (car args) (vs i)))
|#
)
	(apply #'format t
	       "Local variables:~#[~; none~:;~:[ ~1{~s~}~:@{, ~s~}~;~
					  ~:@{~%  ~s: ~s~}~]~]~%"
	       (not no-values) (car *break-env*)))
    (apply #'format t
	   "~#[~;~:;Local functions:~:[ ~1{~s~}~:@{, ~s~}~;~
				       ~:@{~%  ~s: ~s~}~]~%~]"
	   (not no-values) (cadr *break-env*))
    (apply #'format t
	   "~#[~:;Block names: ~@{~s~^, ~}.~%~]"
	   (mapcan #'(lambda (x) (and (eq (cadr x) 'block) (list (car x))))
		   (caddr *break-env*)))
    (apply #'format t
	   "~#[~:;Tags: ~@{~s~^, ~}.~%~]"
	   (mapcan #'(lambda (x) (when (eq (cadr x) 'tag) (list (car x))))
		   (caddr *break-env*)))
    (values)))
#|
(defun tpl-vs-command (&optional x)
  (let ((min (ihs-vs *ihs-base*))
	(max (1- (ihs-vs (1+ *ihs-top*))))
	y)
    (cond ((integerp x)
	   (if (and (>= x min) (<= x max))
	     (vs x)
	     (format t "Illegal value stack index.~%")))
	  ((null x)
	   (setq x min)
	   (setq y max)
	   (do ((ii *ihs-base* (1+ ii))
		(*print-level* 2)
		(*print-length* 4)
		(*print-pretty* t))
	       ((or (>= ii *ihs-top*) (>= (ihs-vs ii) x))
		(do ((vi x (1+ vi)))
		    ((> vi y))
		  (do ()
		      ((> (ihs-vs ii) vi))
		    (when (ihs-visible ii)
		      (print-ihs ii))
		    (incf ii))
		  (format t "    VS[~d]: ~s~%" vi (vs vi)))))
	   (values))
	  (t
	   (format t "Argument must be a number.~%")
	   (values)))))

(defun tpl-local-command (&optional (n 0))
  (tpl-vs-command (+ (ihs-vs *ihs-current*) n)))
|#
(defun tpl-bds-command (&optional var)
  (if var
    (do ((bi (1+ (frs-bds (max 0 (1- *frs-base*)))) (1+ bi))
	 (last (frs-bds (1+ *frs-top*))))
	((> bi last)
	 (format t "Variable not found.~%")
	 (values))
      (when (eq (bds-var bi) var)
	(return (bds-val bi))))
    (do ((bi (1+ (frs-bds (max 0 (1- *frs-base*)))) (1+ bi))
	 (last (frs-bds (1+ *frs-top*)))
	 (fi *frs-base*)
	 (*print-level* 2)
	 (*print-length* 4)
	 (*print-pretty* t))
	((> bi last) (values))
      (do ()
	  ((or (> fi *frs-top*) (>= (frs-bds fi) bi)))
	(print-frs fi)
	(incf fi))
      (format t "BDS[~d]: ~s = ~s~%"
	      bi (bds-var bi) (bds-val bi)))))

(defun tpl-backtrace (&optional n)
  (cond ((null n)
	 (simple-backtrace))
	(t
	 (let ((from (if (integerp n)
		       (max (1+ (- *ihs-current* n)) *ihs-base*)
		       *ihs-base*))
	       (to (if (integerp n) *ihs-current* *ihs-top*)))
	   (do ((i from (1+ i))
		(j (or (sch-frs-base *frs-base* from) (1+ *frs-top*)))
		(*print-level* 2)
		(*print-length* 4)
		(*print-pretty* t))
	       ((> i to) (values))
	     (when (ihs-visible i)
	       (print-ihs i))
	     (do () ((or (> j *frs-top*) (> (frs-ihs j) i)))
	       (print-frs j)
	       (incf j))))))
  (values))

(defun simple-backtrace ()
   (let ((*print-pretty* nil))         ; because CLOS allows (setf foo)
                                       ; as function names
     (princ "Backtrace:")
     (do ((i *ihs-base* (1+ i))
	  (b nil t))
	 ((> i *ihs-top*))
	 (when (ihs-visible i)
	       (let ((*print-case* (if (= i *ihs-current*) :upcase :downcase)))
		 (format t "~:[~; >~] ~S" b (ihs-fname i)))))
     (terpri)))

#|
(defun print-ihs (i)
  (format t "~:[  ~;@ ~]IHS[~d]: ~s ---> VS[~d]~%"
	  (= i *ihs-current*) i
	  (let ((fun (ihs-fun i)))
	    (cond ((or (symbolp fun) (compiled-function-p fun)) fun)
		  ((consp fun)
		   (case (car fun)
		     (lambda fun)
		     (lambda-block (cdr fun))
		     (lambda-closure (cons 'lambda (cddddr fun)))
		     (lambda-block-closure (cddddr fun))
		     #+clos
		     (setf fun)
		     (t '(:zombi))))
		  #+clos
		  ((sys:gfunp fun) fun)
		  (t :zombi)))
	  (ihs-vs i)))
|#

(defun print-frs (i)
  (format *debug-io* "    FRS[~d]: ---> IHS[~d],BDS[~d]~%"
	  i (frs-ihs i) (frs-bds i)))

#|
(defun print-frs (i)
  (format *debug-io* "~&    FRS[~d]: ~s ---> IHS[~d],VS[~d],BDS[~d]"
          i (frs-kind i) (frs-ihs i) (frs-vs i) (frs-bds i)))

(defun frs-kind (i &aux x)
  (case (frs-class i)
    (:catch
     (if (spicep (frs-tag i))
       (or (and (setq x (member (frs-tag i) (vs (+ (frs-vs i) 2))
				:key #'caddr :test #'eq))
		(if (eq (cadar x) 'block)
		  `(block ,(caar x) ***)
		  `(tagbody
		     ,@(reverse
			(mapcar #'car (remove (frs-tag i) x :test-not #'eq
					      :key #'caddr)))
		     ***)))
	   `(block/tagbody ,(frs-tag i)))
       `(catch ',(frs-tag i) ***)))
    (:protect '(unwind-protect ***))
    (t `(system-internal-catcher ,(frs-tag i)))))
|#

(defun break-where ()
  (if (eq (ihs-fname (1+ *ihs-top*)) 'TOP-LEVEL)
    (format t "Top level.~%")
    (format t "Broken at ~:@(~S~).~%" (ihs-fname *ihs-current*))))

(defun tpl-print-current ()
  (format t "Broken at ~:@(~S~)." (ihs-fname *ihs-current*))
  (values))

(defun tpl-hide (fname)
  (unless (member fname *break-hidden-functions* :test #'eq)
    (push fname *break-hidden-functions*)
    (unless (ihs-visible *ihs-current*)
      (set-current-ihs)))
  (values))

(defun tpl-unhide (fname)
  (setq *break-hidden-functions*
	(delete fname *break-hidden-functions* :test #'eq))
  (values))

(defun tpl-unhide-package (package)
  (setq *break-hidden-packages*
	(delete (find-package package) *break-hidden-packages* :test #'eq))
  (values))

(defun tpl-unhide-all ()
  (setq *break-hidden-functions* nil)
  (setq *break-hidden-packages* nil)
  (values))

(defun tpl-hide-package (package)
  (setq package (find-package package))
  (unless (member package *break-hidden-packages* :test #'eq)
    (push package *break-hidden-packages*)
    (unless (ihs-visible *ihs-current*)
      (set-current-ihs)))
  (values))

(defun ihs-visible (i)
  (let ((fname (ihs-fname i)))
    #+clos
    (when (and (consp fname) (eq 'setf (car fname)))
	  (setq fname (cadr fname)))
    (or (eq fname 'eval)
	(eq fname 'evalhook)
	(and (not (member (symbol-package fname) *break-hidden-packages*
			  :test #'eq))
	     (not (null fname))
	     (not (member fname *break-hidden-functions* :test #'eq))))))

(defun ihs-fname (i)
  (let ((function (ihs-fun i)))
    (cond ((symbolp function) function)
          ((consp function)
           (case (car function)
             (lambda 'lambda)
             (lambda-block (cadr function))
             (lambda-block-closure (nth 4 function))
             (lambda-closure 'lambda-closure)
	     #+clos
	     (setf function)
             (t :zombi)))
          ((compiled-function-p function)
           (compiled-function-name function))
	  #+clos
	  ((sys:gfunp function) (sys:gfun-name function))
          (t :zombi))))

(defun set-current-ihs ()
  (do ((i *ihs-current* (1- i)))
      ((or (and (ihs-visible i) (setq *ihs-current* i))
	   (<= i *ihs-base*))))
  (set-break-env))

(defun set-break-env ()
  (setq *break-env*
	(if (ihs-compiled-p *ihs-current*)
	  nil
	  (ihs-env *ihs-current*))))

(defun ihs-compiled-p (i)
  (let ((function (ihs-fun i)))
       (or (and (symbolp function) (not (special-form-p function)))
           (compiled-function-p function)
	   #+clos
	   (sys:gfunp function))))

#|
(defun super-go (i tag &aux x)
  (when (and (>= i *frs-base*) (<= i *frs-top*) (spicep (frs-tag i)))
    (if (setq x (member (frs-tag i) (vs (+ (frs-vs i) 2))
			:key #'caddr :test #'eq))
      ; Interpreted TAGBODY.
      (when (and (eq (cadar x) 'tag)
		 (member tag (mapcar #'car (remove (frs-tag i) x
						   :test-not #'eq
						   :key #'caddr))))
	(internal-super-go (frs-tag i) tag t))
      ; Maybe, compiled cross-closure TAGBODY.
      ; But, it may also be compiled cross-closure BLOCK, in which case
      ; SUPER-GO just RETURN-FROMs with zero values.
      (internal-super-go (frs-tag i) tag nil)))
  (format *debug-io* "~s is invalid tagbody identification for ~s." i tag))
|#
(defun tpl-backward-search (string)
  (do ((ihs (1- *ihs-current*) (1- ihs)))
      ((< ihs *ihs-base*)
       (format *debug-io* "Search for ~a failed.~%" string))
    (when (and (ihs-visible ihs)
	       (search string (symbol-name (ihs-fname ihs))
		       :test #'char-equal))
      (setq *ihs-current* ihs)
      (set-current-ihs)
      (tpl-print-current)
      (return)))
  (values))

(defun tpl-forward-search (string)
  (do ((ihs (1+ *ihs-current*) (1+ ihs)))
      ((> ihs *ihs-top*)
       (format *debug-io* "Search for ~a failed.~%" string))
    (when (and (ihs-visible ihs)
	       (search string (symbol-name (ihs-fname ihs))
		       :test #'char-equal))
      (setq *ihs-current* ihs)
      (set-current-ihs)
      (tpl-print-current)
      (return)))
  (values))

(defun tpl-apropos-command (&optional string pkg)
  (when string (apropos string pkg)))

(defun tpl-document-command (&optional symbol)
  (when symbol (help symbol)))

(defun tpl-step-command (&optional form)
  (when form (step* form)))

(defun tpl-trace-command (&rest functions)
  (trace* functions))

(defun tpl-untrace-command (&rest functions)
  (untrace* functions))

(defvar *tpl-last-load* nil)

(defun tpl-load-command (&rest files)
  (when files
    (setq *tpl-last-load* files))
  (dolist (file *tpl-last-load*) (load file))
  *tpl-last-load*)

(defvar *tpl-last-compile* nil)

(defun tpl-compile-command (&rest files)
  (when files
    (setq *tpl-last-compile* files))
  (dolist (file *tpl-last-compile*) (compile-file file))
  (setq *tpl-last-load* *tpl-last-compile*))

(defun tpl-help-command (&optional topic)
  (cond ((null topic)
	 (dolist (commands *tpl-commands*)
	   (format t "~%~A:~%" (car commands))
	   (dolist (c (cdr commands))
	     (when (fourth c)
	       (format t "~A.~%" (fourth c))))))
	((or (stringp topic) (symbolp topic))
	 (let (c)
	   (setq topic (intern (string topic) (find-package 'keyword)))
	   (dolist (commands *tpl-commands*)
	     (when (setq c (assoc topic (cdr commands) :test #'member))
	       (return)))
	   (cond ((null (fifth c))
		  (format t "No such help topic: ~s~%"
			  (string topic)))
		 (t
		  (terpri)
		  (format t (fifth c))
		  (terpri)))))
	(t
	 (format t "Not a valid help topic: ~s~%" topic)))
  (values))

(defun tpl-help-stack-command ()
  (format t "
Use the following functions to directly access ECL stacks.

Invocation History Stack:
(sys:IHS-TOP)	Returns the index of the TOP of the IHS.
(SYS:IHS-VS i)	Returns the VS index of the i-th entity in IHS.
(SYS:IHS-FUN i)	Returns the function of the i-th entity in IHS.

Frame (catch, block) Stack:
(sys:FRS-TOP)	Returns the index of the TOP of the FRS.
(SYS:FRS-VS i)	Returns the VS index of the i-th entity in FRS.
(SYS:FRS-BDS i)	Returns the BDS index of the i-th entity in FRS.
(SYS:FRS-IHS i)	Returns the IHS index of the i-th entity in FRS.

Binding Stack:
(sys:BDS-TOP)	Returns the index of the TOP of the BDS.
(SYS:BDS-VAR i)	Returns the symbol of the i-th entity in BDS.
(SYS:BDS-VAL i)	Returns the value of the i-th entity in BDS.

(SYS:SUPER-GO i tag)
	Jumps to the specified tag established by the TAGBODY frame at
	FRS[i].  Both arguments are evaluated.  If FRS[i] happens to be
	a non-TAGBODY frame, then (THROW (SYS:IHS-TAG i) (VALUES)) is
	performed.

Note that these functions are named by external symbols in the SYSTEM
package."
))
