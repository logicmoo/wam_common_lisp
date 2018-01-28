;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; FFI	Symbols used in the foreign function interface

(defpackage "FFI"
  (:export clines
	   defcfun
	   defentry
	   defla
	   defcbody			; Beppe
	   definline			; Beppe
	   defunC			; Beppe
	   void
	   object
	   char*			; Beppe
	   ;;char
	   int
	   ;;float
	   double
	   ))

(in-package "FFI")

(defmacro clines (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defcfun (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defentry (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defla (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro defcbody (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))

(defmacro definline (fun arg-types type code)
  `(eval-when (compile load eval)
              ;; defCbody must go first, because it clears symbol-plist of fun
              (defCbody ,fun ,arg-types ,type ,code)
              (proclaim '(function ,fun ,arg-types ,type))
              (setf (get ',fun ':inline-always)
                    '((,arg-types ,type
                       t                ; side-effect-p
                       nil ,code)))))

(defmacro defunC (&whole all)
  (error "The FFI special form ~S cannot be used in the interpreter."
	 (car all)))
