;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                    Exporting external symbols of LISP package


(in-package #:lisp)

(export '(
	  &whole
	  &environment
	  &body
	  *
	  **
	  ***
	  *break-enable*
	  *break-on-warnings*
	  *features*
	  *modules*
	  +
	  ++
	  +++
	  -
	  /
	  //
	  ///
	  COMMON
	  KYOTO
	  KCL
	  ECL
	  abs
	  acos
	  acosh
	  adjust-array
	  adjustable-array-p
	  apropos
	  apropos-list
	  arglist
	  array-dimension
	  array-dimensions
	  array-element-type
	  array-has-fill-pointer-p
	  array-in-bounds-p
	  array-rank
	  array-row-major-index
	  asin
	  asinh
	  assert
	  atanh
	  bit
	  bit-and
	  bit-andc1
	  bit-andc2
	  bit-eqv
	  bit-ior
	  bit-nand
	  bit-nor
	  bit-not
	  bit-orc1
	  bit-orc2
	  bit-xor
	  break
	  byte
	  byte-position
	  byte-size
	  ccase
	  cerror
	  check-type
	  cis
	  coerce
	  compile
	  compile-file
	  concatenate
	  cosh
	  count
	  count-if
	  count-if-not
	  ctypecase
	  decf
	  decode-universal-time
	  defconstant
	  define-modify-macro
	  define-setf-method
	  defparameter
	  defsetf
	  defstruct
	  deftype
	  defvar
	  delete
	  delete-duplicates
	  delete-if
	  delete-if-not
	  deposit-field
	  describe
	  disassemble
	  do*
	  do-all-symbols
	  do-external-symbols
	  do-symbols
	  documentation
	  dolist
	  dotimes
	  dpb
	  dribble
	  ecase
	  ed
	  eighth
	  encode-universal-time
	  error
	  etypecase
	  eval-when
	  every
	  fceiling
	  ffloor
	  fifth
	  fill
	  fill-pointer
	  find
	  find-all-symbols
	  find-if
	  find-if-not
	  first
	  format
	  fourth
	  fround
	  ftruncate
	  get-decoded-time
	  get-setf-method
	  get-setf-method-multiple-value
	  get-universal-time
	  getf
	  ignore
	  incf
	  inspect
	  intersection
	  isqrt
	  ldb
	  ldb-test
	  lisp-implementation-type
	  logandc1
	  logandc2
	  lognand
	  lognor
	  lognot
	  logorc1
	  logorc2
	  logtest
	  long-site-name
	  loop
	  machine-instance
	  machine-type
	  machine-version
	  make-array
	  make-sequence
	  map
	  mask-field
	  merge
	  mismatch
	  mod
	  multiple-value-setq
	  nintersection
	  ninth
	  notany
	  notevery
	  nset-difference
	  nset-exclusive-or
	  nsubstitute
	  nsubstitute-if
	  nsubstitute-if-not
	  nunion
	  phase
	  pop
	  position
	  position-if
	  position-if-not
	  prin1-to-string
	  princ-to-string
	  prog*
	  provide
	  psetf
	  push
	  pushnew
	  rational
	  rationalize
	  read-from-string
	  reduce
	  rem
	  remf
	  remove
	  remove-duplicates
	  remove-if
	  remove-if-not
	  replace
	  require
	  rotatef
	  room
	  sbit
	  search
	  second
	  set-difference
	  set-exclusive-or
	  setf
	  seventh
	  shiftf
	  short-site-name
	  signum
	  sinh
	  sixth
	  software-type
	  software-version
	  some
	  sort
	  stable-sort
	  step
	  structure
	  subsetp
	  substitute
	  substitute-if
	  substitute-if-not
	  subtypep
	  tanh
	  tenth
	  third
	  time
	  trace
	  type
	  typecase
	  typep
	  union
	  untrace
	  variable
	  vector
	  vector-pop
	  vector-push
	  vector-push-extend
	  warn
	  with-input-from-string
	  with-open-file
	  with-open-stream
	  with-output-to-string
	  write-to-string
	  y-or-n-p
	  yes-or-no-p

	  proclaim
	  proclamation
	  special
	  type
	  ftype
	  function
	  inline
	  notinline
	  ignore
	  optimize
	  speed
	  space
	  safety
	  compilation-speed
	  declaration

	  *eval-when-compile*

	  clines
	  defcfun
	  defentry
	  defla
	  defcbody			; Beppe
	  definline			; Beppe
	  defunC			; Beppe
	  void
	  object
	  char*				; Beppe
	  char
	  int
	  float
	  double
	  ))

;;; ----------------------------------------------------------------------
;;;
(defun eval-feature (x)
  (cond ((symbolp x)
         (member x *features*
                 :test #'(lambda (a b)
                           (or (eql a b)
			       (and (symbolp a) (symbolp b)
				    (string-equal (symbol-name a)
						  (symbol-name b)))))))
	((atom x) (error "~ is not allowed as a feature" x))
        ((eq (car x) 'AND)
         (dolist (x (cdr x) t) (unless (eval-feature x) (return nil))))
        ((eq (car x) 'OR)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eq (car x) 'NOT)
	 (not (eval-feature (second x))))
	(t (error "~S is not a feature expression." x))))

;;; Revised by G. Attardi
(defun check-no-infix (stream subchar arg)
  (when arg
    (error "Reading from ~S: no number should appear between # and ~A"
	   stream subchar)))

(defun sharp-+-reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (and (not *read-suppress*) (eval-feature feature))
	(read stream t nil t)
	(let ((*read-suppress* t)) (read stream t nil t) (values)))))

(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader
                              (sys::standard-readtable))

(defun sharp---reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (or *read-suppress* (eval-feature feature))
	(let ((*read-suppress* t)) (read stream t nil t) (values))
	(read stream t nil t))))

(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader
                              (sys::standard-readtable))

;;; ----------------------------------------------------------------------

#+CLOS
(export '(
	  add-method
	  call-next-method
	  change-class
	  class-changed
	  class-name
	  class-of
	  defclass
	  defgeneric
	  define-method-combination
	  defmethod
	  ensure-generic-function
	  find-class
	  generic-flet
	  generic-function
	  generic-labels
	  get-method
	  initialize-instance
	  invalid-method-error
	  make-instance
	  make-instance-obsolete
	  make-method-call
	  method
	  method-combination-error
	  method-qualifiers
	  next-method-p
	  no-applicable-method
	  print-object
	  remove-method
	  slot-boundp
	  slot-exists-p
	  slot-makunbound
	  slot-missing
	  slot-unbound
	  slot-value
	  symbol-macrolet
	  update-instance-structure
	  with-accessors
	  with-added-methods
	  with-slots

	  class
	  built-in
	  standard-class
	  standard-generic-function
	  standard-method
	  standard-object
	  structure-class
	  structure-object
	  ))

