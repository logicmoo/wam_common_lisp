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
;;(defpackage "COMMON-LISP-USER" (:nicknames "USER" "CL-USER"))

;;; defpackage.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

;;(in-package "SYSTEM")

;;; Adapted from CMUCL.

(defun designated-package-name (designator)
  (cond ((packagep designator)
         (package-name designator))
        (t
         (string designator))))

(defun stringify-names (names)
  (mapcar #'string names))

(defun check-disjoint (&rest args)
  (let ((rest-args args))
    (dolist (arg1 args)
      (let ((key1 (car arg1))
            (set1 (cdr arg1)))
        (setq rest-args (cdr rest-args))
        (dolist (arg2 rest-args)
          (let* ((key2 (car arg2))
                 (set2 (cdr arg2))
                 (common (remove-duplicates (intersection set1 set2 :test #'string=))))
            (when common
              (error 'program-error
                     :format-control
                     "Parameters ~S and ~S must be disjoint, but have common elements: ~S"
                     :format-arguments
                     (list key1 key2 common)))))))))

(defun ensure-available-symbols (symbols)
   symbols)

(defmacro defpackage (package &rest options)
  (let ((nicknames nil)
        (size nil)
        (shadows nil)
        (shadowing-imports nil)
        (use nil)
        (use-p nil)
        (imports nil)
        (interns nil)
        (exports nil)
        (local-nicknames nil)
        (doc nil))
    (dolist (option options)
      (unless (consp option)
        (error 'program-error "bad DEFPACKAGE option: ~S" option))
      (case (car option)
        (:nicknames
         (setq nicknames (stringify-names (cdr option))))
        (:size
         (cond (size
                (error 'program-error "can't specify :SIZE twice"))
               ((and (consp (cdr option))
                     (typep (second option) 'unsigned-byte))
                (setq size (second option)))
               (t
                (error 'program-error
                       "bad :SIZE, must be a positive integer: ~S"
                       (second option)))))
        (:shadow
         (let ((new (stringify-names (cdr option))))
           (setq shadows (append shadows new))))
        (:shadowing-import-from
         (let ((package-name (designated-package-name (cadr option)))
               (symbol-names (stringify-names (cddr option))))
           (let ((assoc (assoc package-name shadowing-imports
                               :test #'string=)))
             (if assoc
                 (setf (cdr assoc) (append (cdr assoc) symbol-names))
                 (setq shadowing-imports
                       (acons package-name symbol-names shadowing-imports))))))
        (:use
         (let ((new (mapcar #'designated-package-name (cdr option))))
           (setq use (delete-duplicates (nconc use new) :test #'string=))
           (setq use-p t)))
        (:import-from
         (let ((package-name (designated-package-name (cadr option)))
               (symbol-names (stringify-names (cddr option))))
           (let ((assoc (assoc package-name imports
                               :test #'string=)))
             (if assoc
                 (setf (cdr assoc) (append (cdr assoc) symbol-names))
                 (setq imports (acons package-name symbol-names imports))))))
        (:intern
         (let ((new (stringify-names (cdr option))))
           (setq interns (append interns new))))
        (:export
         (let ((new (stringify-names (cdr option))))
           (setq exports (append exports new))))
        (:documentation
         (when doc
           (error 'program-error "can't specify :DOCUMENTATION twice"))
         (setq doc (coerce (cadr option) 'simple-string)))
        (:local-nicknames
         (dolist (nickdecl (cdr option))
           (unless (= (length nickdecl) 2)
             (error 'program-error "Malformed local nickname declaration ~A"
                    nickdecl))
           (let ((local-nickname (string (first nickdecl)))
                 (package-name (designated-package-name (second nickdecl))))
             (when (member local-nickname '("CL" "COMMON-LISP" "KEYWORD")
                           :test #'string=)
               (cerror "Continue anyway"
                       (format nil "Trying to define a local nickname for package ~A"
                               local-nickname)))
             (when (member local-nickname (list* package nicknames)
                           :test #'string=)
               (cerror "Continue anyway"
                       "Trying to override the name or a nickname (~A) ~
                        with a local nickname for another package ~A"
                       local-nickname package-name))
             (push (list local-nickname package-name) local-nicknames))))
        (t
         (error 'program-error "bad DEFPACKAGE option: ~S" option))))
    (check-disjoint `(:intern ,@interns) `(:export  ,@exports))
    (check-disjoint `(:intern ,@interns)
                    `(:import-from
                      ,@(apply #'append (mapcar #'rest imports)))
                    `(:shadow ,@shadows)
                    `(:shadowing-import-from
                      ,@(apply #'append (mapcar #'rest shadowing-imports))))
    `(prog1
       (%defpackage ,(string package) ',nicknames ',size
		    ',shadows (ensure-available-symbols ',shadowing-imports)
		    ',(if use-p use nil)
		    (ensure-available-symbols ',imports) ',interns ',exports
		    ',local-nicknames ',doc)
       ,(when (and (symbolp package) (not (keywordp package)))
	  `(record-source-information-for-type ',package :package))
       (record-source-information-for-type ,(intern (string package) :keyword) :package)
       )))

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
