;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; Compatibility functions for T2.8/T3 (Scheme) code running under Common Lisp
;
; 19990429: begun
; 19990503: more work
;
;*******************************************************************************
(defpackage "COMMON-LISP-USER" (:nicknames "USER" "CL-USER"))

(setq else t)
(setq *repl-wont-print* nil)
(defun t-or-nil (a) (if a 't nil))
(defmacro string->symbol (a) `(intern ,a))
(defmacro symbol->string (a) `(symbol-name ,a))
(defmacro string-length (a) `(length ,a))
(defmacro string-empty? (a) `(= (length ,a) 0))
(defmacro string-slice (a b c) `(subseq ,a ,b ,c))
(defmacro substring (a b c) `(subseq ,a ,b ,c))
(defmacro string-nthtail (a b) `(subseq ,a ,b (length ,a)))
(defmacro nthchdr (a b) `(subseq ,a ,b (length ,a)))
(defmacro string-downcase! (a) `(string-downcase ,a))
(defmacro string-write (a b) `(write-string ,b ,a))
(defmacro map-string! (a b) `(map `string ,a ,b))
(defmacro string-equal? (a b) `(string-equal ,a ,b))
(defmacro chdr (a) `(subseq ,a 1 (length ,a)))
(defmacro nthchar (a b) `(elt ,a ,b))
(defmacro digit? (a b) `(digit-char-p ,a ,b))
(defmacro string-append (&rest args) `(concatenate 'string ,@args))
(defmacro any? (a b) `(t-or-nil (some ,a ,b)))
(defmacro any (a b) `(some ,a ,b))
(defmacro null? (a) `(null ,a))
(defmacro eq? (a b) `(eql ,a ,b))
(defmacro alikeq? (a b) `(equalp ,a ,b))
(defmacro neq? (a b) `(not (eql ,a ,b)))
(defmacro memq? (a b) `(t-or-nil (member ,a ,b)))

#-abcl
(defmacro memq (a b) `(member ,a ,b))
(defmacro gen-id (symbol) `(gensym ,symbol))
(defmacro div (a b) `(/ ,a ,b))
(defmacro procedure? (x) `(functionp ,x))
(defmacro number? (x) `(numberp ,x))
(defmacro flonum? (x) `(floatp ,x))
(defmacro fixnum->flonum (x) x)
(defmacro symbol? (x) `(symbolp ,x))
(defmacro pair? (a) `(consp ,a))
(defmacro string? (a) `(stringp ,a))
(defmacro uppercase? (x) `(upper-case-p ,x))
(defmacro delq! (a b) `(delete ,a ,b))

(defun listify (a) (if (listp a) a (list a)))
(defmacro append! (a b) `(nconc (listify ,a) (listify ,b)))
(defmacro ascii->char (x) `(code-char ,x))

#-abcl
 (defmacro assq (a b) `(assoc ,a ,b))

(defmacro increment-me (a) `(setq ,a (+ ,a 1)))
(defmacro string-posq (a b) `(position ,a ,b))
(defmacro nth-elem (a b) `(nth ,b ,a))
(defmacro newline (a) `(terpri ,a))
(defmacro -1+ (a) `(+ -1 ,a))
(defmacro fl+ (a b) `(+ ,a ,b))
(defmacro fl- (a b) `(- ,a ,b))
(defmacro fl* (a b) `(* ,a ,b))
(defmacro fl/ (a b) `(/ ,a ,b))
(defmacro fl< (a b) `(< ,a ,b))
(defmacro fl> (a b) `(> ,a ,b))
(defmacro fl>= (a b) `(>= ,a ,b))
(defmacro fl<= (a b) `(<= ,a ,b))
(defmacro fl= (a b) `(> ,a ,b))
(defmacro file-exists? (a) `(probe-file ,a))
(defmacro comment (a) nil)
(defmacro mem? (a b c) `(t-or-nil (member ,b ,c :test ,a)))
(defmacro mem (a b c) `(member ,b ,c :test ,a))
(defmacro every? (a b) `(t-or-nil (every ,a ,b)))
(defmacro tlast (a) `(car (last ,a 1)))
(defun standard-input () *standard-input*)
(defun standard-output () *standard-output*)
(defun string-head (x) (char x 0))
(defun walkcdr (fn x)
  (yloop (initial (rest x))
         (ywhile rest)
         (ydo (apply fn (list rest))
              (setq rest (cdr rest)))))

; End of file.
