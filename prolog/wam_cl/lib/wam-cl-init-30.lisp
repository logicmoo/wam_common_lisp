;;; #+BUILTIN Means to ignore since it should already be defined
;;; #+WAM-CL Means we want it
;;; #+LISP500 Means probably we dont want it
;;; #+ALT Alternative definition
;;; #+ABCL From ABCL
;;; #+SBCL From SBCL
;;; #+ECL From ECL
;;; #+SICL From SICL


(in-package "SYSTEM")


#+(or WAM-CL LISP500) 
(defmacro check-type (place typespec &optional string)
  `(tagbody
    start
    (unless (typep ,place ',typespec)
      (restart-case
	  (error 'type-error :datum ,place :expected-type ',typespec)
	(store-value (value)
	  (setf ,place value)))
      (go start))))



#+(or WAM-CL LISP500) 
(defun abort (&optional condition)
  (invoke-restart (find-restart 'abort condition))
  (error 'control-error))

#+(or WAM-CL LISP500) 
(defun continue (&optional condition)
  (invoke-restart (find-restart 'continue condition)))

#+(or WAM-CL LISP500) 
(defun muffle-warning (&optional condition)
  (invoke-restart (find-restart 'muffle-warning condition))
  (error 'control-error))

#+(or WAM-CL LISP500) 
(defun store-value (value &optional condition)
  (invoke-restart (find-restart 'store-value condition) value))

#+(or WAM-CL LISP500) 
(defun use-value (value &optional condition)
  (invoke-restart (find-restart 'use-value condition) value))


#+(or WAM-CL LISP500) 
(defun integer-string (integer &optional (radix 10))
  (if (= integer 0)
      "0"
      (labels ((recur (i l)
		 (if (= i 0)
		     l
		     (multiple-value-bind (ni r)
			 (floor i radix)
		       (recur ni (cons (code-char (+ (if (< r 10) 48 55) r))
				       l))))))
	(apply #'string (if (< 0 integer)
			    (recur integer nil)
			    (cons (code-char 45) (recur (- integer) nil)))))))



#+(or WAM-CL LISP500)
(defun designator-symbol (designator)
  (if (symbolp designator)
      designator
      (find-symbol designator)))

#+BUILTIN 
(defun symbolp (object) (or (null object) (eq (type-of object) 'symbol)))

#+BUILTIN 
(defun keywordp (object) 
(and (symbolp object)
       (string= (package-name (symbol-package object)) "KEYwORD")))

#+BUILTIN 
(defun make-symbol (name)
  (let ((symbol (makei 9 0 name nil nil nil nil (- 1) 0)))
    (imakunbound symbol 4)
    (imakunbound symbol 5)
    (imakunbound symbol 6)
    symbol))

#+(or WAM-CL LISP500)
(defvar *gensym-counter* 0)
#+(or WAM-CL LISP500)
(defun gen-sym (&optional x)
  (let ((prefix (if (stringp x) x "G"))
	(suffix (if (fixnump x)
		    x
		    (let ((x *gensym-counter*))
		      (setf *gensym-counter* (+ 1 *gensym-counter*))))))
    (make-symbol (conc-string prefix (integer-string suffix)))))

(let ((gentemp-counter 0))
  (defun gentemp (&optional (prefix "T") (package *package*))
    (setf gentemp-counter (+ 1 gentemp-counter))
    (intern (conc-string prefix (integer-string gentemp-counter))
	    package)))


#+BUILTIN 
#+(or WAM-CL LISP500)
(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

#+(or WAM-CL LISP500)
(defun (setf get) (new-value symbol indicator &optional default)
  (setf (getf (symbol-plist symbol) indicator default) new-value))

#+(or WAM-CL LISP500)
(defun (setf rest) (new-tail list) (setf (cdr list) new-tail))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; BEGIN FILE ./remf.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; remf.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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

;;; Adapted from SBCL.

(defmacro remf (place indicator &environment env)
  "Place may be any place expression acceptable to SETF, and is expected
   to hold a property list or (). This list is destructively altered to
   remove the property specified by the indicator. Returns T if such a
   property was present, NIL if not."
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (do* ((d dummies (cdr d))
	  (v vals (cdr v))
	  (let-list nil)
	  (ind-temp (gensym))
	  (local1 (gensym))
	  (local2 (gensym)))
	 ((null d)
          ;; See ANSI 5.1.3 for why we do out-of-order evaluation
	  (push (list ind-temp indicator) let-list)
	  (push (list (car newval) getter) let-list)
	  `(let* ,(nreverse let-list)
	     (do ((,local1 ,(car newval) (cddr ,local1))
		  (,local2 nil ,local1))
		 ((atom ,local1) nil)
	       (cond ((atom (cdr ,local1))
		      (error "Odd-length property list in REMF."))
		     ((eq (car ,local1) ,ind-temp)
		      (cond (,local2
			     (rplacd (cdr ,local2) (cddr ,local1))
			     (return t))
			    (t (setq ,(car newval) (cddr ,(car newval)))
			       ,setter
			       (return t))))))))
      (push (list (car d) (car v)) let-list))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; END FILE ./remf.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


#+(or WAM-CL LISP500)
(defmacro remprop (symbol indicator) `(remf (symbol-plist ,symbol) ,indicator))

#+BUILTIN 
#+(or WAM-CL LISP500)
(defun makunbound (symbol) (imakunbound symbol 4))

#+BUILTIN 
#+(or WAM-CL LISP500)
(defun set (symbol value) (setf (symbol-value symbol) value))

#+(or WAM-CL LISP500)
(defun designator-string (designator)
  (if (stringp designator)
      designator
      (if (characterp designator)
	  (string designator)
	  (symbol-name designator))))

#+BUILTIN 
#+(or WAM-CL LISP500)
(defvar *package* (car (cdr *packages*)))

#+(or WAM-CL LISP500)
(defun list-all-packages ()
  (copy-list *packages*))

#+(or WAM-CL LISP500)
(defun /= (number &rest numbers)
  (tagbody
   start
     (when numbers
       (dolist (n numbers)
	 (when (= number n)
	   (return-from /=)))
       (setq number (pop numbers))
       (go start)))
  t)



#+BUILTIN
#+(or WAM-CL LISP500) 
(defun > (&rest numbers) (apply #'< (reverse numbers)))


#+BUILTIN
#+(or WAM-CL LISP500) 
(defun <= (number &rest numbers)
  (dolist (n numbers t)
    (when (< n number)
      (return-from <=))
    (setq number n)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun >= (number &rest numbers)
  (dolist (n numbers t)
    (when (< number n)
      (return-from >=))
    (setq number n)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun max (real &rest reals)
  (dolist (r reals real)
    (when (< real r)
      (setq real r))))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun min (real &rest reals)
  (dolist (r reals real)
    (when (< r real)
      (setq real r))))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun oddp (integer)
  (= (mod integer 2) 1))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun evenp (integer)
  (= (mod integer 2) 0))


#+(or WAM-CL LISP500) 
(defun minusp (real)
  (< real 0))


#+(or WAM-CL LISP500) 
(defun plusp (real)
  (< 0 real))


#+(or WAM-CL LISP500) 
(defun zerop (real)
  (= real 0))


#+(or WAM-CL LISP500) 
(defun abs (number)
  (if (< number 0)
      (- number)
      number))



#+(or WAM-CL LISP500) 
(defun byte (size position)
  (cons size position))

#+(or WAM-CL LISP500) 
(defun byte-size (bytespec)
  (car bytespec))

#+(or WAM-CL LISP500) 
(defun byte-position (bytespec)
  (cdr bytespec))

#+(or WAM-CL LISP500) 
(defun char= (&rest characters)
  (apply #'= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char/= (&rest characters)
  (apply #'/= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char< (&rest characters)
  (apply #'< (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char> (&rest characters)
  (apply #'> (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char<= (&rest characters)
  (apply #'<= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char>= (&rest characters)
  (apply #'>= (mapcar #'char-code characters)))

#+(or WAM-CL LISP500) 
(defun char-equal (&rest characters)
  (apply #'char= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-not-equal (&rest characters)
  (apply #'char/= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-lessp (&rest characters)
  (apply #'char< (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-greaterp (&rest characters)
  (apply #'char> (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-not-greaterp (&rest characters)
  (apply #'char<= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun char-not-lessp (&rest characters)
  (apply #'char>= (mapcar #'char-upcase characters)))

#+(or WAM-CL LISP500) 
(defun character (character)
  (if (characterp character)
      character
      (let ((string (designator-string character)))
	(if (= (length string) 1)
	    (aref string 0)
	    (error 'type-error :datum string :expected-type '(string 1))))))

#+LISP500
(defun characterp (object) (= (ldb '(5 . 0) (ival object)) 24))

#+(or WAM-CL LISP500) 
(defun alpha-char-p (character)
  (let ((code (char-code character)))
    (or (< 64 code 91)
	(< 96 code 123)
	(< 159 code))))

#+(or WAM-CL LISP500) 
(defun alphanumericp (character)
  (let ((code (char-code character)))
    (or (< 47 code 58)
	(< 64 code 91)
	(< 96 code 123)
	(< 159 code))))

#+(or WAM-CL LISP500) 
(defun digit-char (weight &optional (radix 10))
  (when (< weight radix)
    (if (< weight 10)
	(code-char (+ 48 weight))
	(code-char (+ 55 weight)))))

#+(or WAM-CL LISP500) 
(defun digit-char-p (char &optional (radix 10))
  (let* ((code (char-code char))
	 (weight (if (< 47 code 58)
		     (- code 48)
		     (if (< 64 code 91)
			 (- code 55)
			 (when (< 96 code 123)
			   (- code 87))))))
    (and weight (< weight radix) weight)))

#+(or WAM-CL LISP500) 
(defun standard-char-p (character)
  (let ((code (char-code character)))
    (or (= code 10)
	(< 31 code 127))))

#+(or WAM-CL LISP500) 
(defun char-upcase (character)
  (let ((code (char-code character)))
    (if (< 96 code 123)
	(code-char (- code 32))
	character)))

#+(or WAM-CL LISP500) 
(defun char-downcase (character)
  (let ((code (char-code character)))
    (if (< 64 code 91)
	(code-char (+ code 32))
	character)))

#+(or WAM-CL LISP500) 
(defun upper-case-p (character)
  (< 64 (char-code character) 91))

#+(or WAM-CL LISP500) 
(defun lower-case-p (character)
  (< 96 (char-code character) 123))

#+(or WAM-CL LISP500) 
(defun both-case-p (character)
  (or (upper-case-p character) (lower-case-p character)))

#+(or WAM-CL LISP500) 
(defun char-int (character)
  (char-code character))

#+(or WAM-CL LISP500) 
(defconstant char-code-limit 256)

#+(or WAM-CL LISP500) 
(let ((char-names '((0 . "Null")
		    (8 . "Backspace")
		    (9 . "Tab")
		    (10 . "Newline")
		    (12 . "Page")
		    (13 . "Return")
		    (32 . "Space")
		    (127 . "Rubout"))))
  (defun char-name (character)
    (let* ((code (char-code character))
	   (name (cdr (assoc code char-names))))
      (or name (when (< code 32)
		 (conc-string "U+" (integer-string code))))))
  (defun name-char (name)
    (setq name (designator-string name))
    (if (< (length name) 2)
	(aref name 0)
	(if (= (char-code (aref name 0)) 85)
	    (code-char (parse-integer name :start 2))
	    (let ((code (car (rassoc name char-names :test #'string-equal))))
	      (when code (code-char code)))))))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun atom (object) (not (consp object)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun rplaca (cons object) (setf (car cons) object) cons)

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun rplacd (cons object) (setf (cdr cons) object) cons)



#+(or WAM-CL LISP500) 
(defun copy-tree (tree)
  (if (consp tree) (cons (copy-tree (car tree)) (copy-tree (cdr tree))) tree))

#+(or WAM-CL LISP500) 
(defun sublis (alist tree &rest rest)
  (if (consp tree)
      (let ((a (apply #'sublis alist (car tree) rest))
	    (d (apply #'sublis alist (cdr tree) rest)))
	(if (and (eq a (car tree)) (eq d (cdr tree)))
	    tree
	    (cons a d)))
      (let ((a (apply #'assoc tree alist rest)))
	(if a (cdr a) tree))))

#+(or WAM-CL LISP500) 
(defun nsublis (alist tree &rest rest)
  (if (consp tree)
      (progn
	(setf (car tree) (apply #'nsublis alist (car tree) rest))
	(setf (cdr tree) (apply #'nsublis alist (cdr tree) rest))
	tree)
      (let ((a (apply #'assoc tree alist rest)))
	(if a (cdr a) tree))))

#+(or WAM-CL LISP500) 
(defun copy-list (list)
  (if (consp list) (cons (car list) (copy-list (cdr list))) list))

#+(or WAM-CL LISP500) 
(defun make-list (size &key initial-element)
  (if (= size 0) nil
      (cons initial-element
	    (make-list (- size 1) :initial-element initial-element))))

#+(or WAM-CL LISP500) 
(defun list* (&rest objects)
  (if (cdr objects)
      (cons (car objects) (apply #'list* (cdr objects)))
      (car objects)))

#+(or WAM-CL LISP500) 
(defun list-length (list)
  (let ((slow list)
	(fast list)
	(odd nil)
	(len 0))
    (tagbody
     start
       (when (atom fast) (return-from list-length len))
       (setf fast (cdr fast))
       (setf len (+ 1 len))
       (when odd (setf slow (cdr slow)))
       (setf odd (not odd))
       (unless (eq slow fast) (go start)))))

#+(or WAM-CL LISP500) 
(defun listp (object) (or (consp object) (eq object nil)))



#+(or WAM-CL LISP500) 
(defun nth (n list) (if (< n 1) (car list) (nth (- n 1) (cdr list))))
'(defun (setf nth) (new-object n list)
  (if (< n 1)
      (setf (car list) new-object)
      (setf (nth (- n 1) (cdr list)) new-object)))


#+(or WAM-CL LISP500) 
(defun endp (list) (not list))

#+(or WAM-CL LISP500) 
(defun nconc (&rest lists)
  (if (cdr lists)
      (if (car lists)
	  (progn (setf (cdr (last (car lists))) (apply #'nconc (cdr lists)))
		 (car lists))
	  (apply #'nconc (cdr lists)))
      (car lists)))

#+(or WAM-CL LISP500) 
(defun revappend (list tail)
  (if list
      (revappend (cdr list) (cons (car list) tail))
      tail))

#+(or WAM-CL LISP500) 
(defun nreconc (list tail)
  (if list
      (let ((new-list (cdr list)))
	(setf (cdr list) tail)
	(nreconc new-list list))
      tail))

#+(or WAM-CL LISP500) 
(defun butlast (list &optional (n 1))
  (let* ((r (cons nil nil))
	 (e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e r)
    (tagbody
     start
       (unless (consp list) (return-from butlast nil))
       (unless (< n 1)
	 (setf e (setf (cdr e) (cons (car list) nil)))
	 (setf list (cdr list))
	 (setf n (- n 1))
	 (go start)))
    (cdr r)))

#+(or WAM-CL LISP500) 
(defun nbutlast (list &optional (n 1))
  (let* ((e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e list)
    (tagbody
     start
       (unless (consp list) (return-from nbutlast nil))
       (unless (< n 2)
	 (setf e (cdr e))
	 (setf n (- n 1))
	 (go start)))
    (setf (cdr e) nil)
    list))

#+(or WAM-CL LISP500) 
(defun last (list &optional (n 1))
  (let* ((e list)
	 (m 0))
    (tagbody
     start
       (when (consp e)
	 (setf m (+ m 1))
	 (setf e (cdr e))
	 (go start)))
    (setf n (- m n))
    (setf e list)
    (tagbody
     start
       (when (< n 1) (return-from last e))
       (setf e (cdr e))
       (setf n (- n 1))
       (go start))))

#+(or WAM-CL LISP500) 
(defun ldiff (list object)
  (let* ((r (cons nil nil))
	 (e r))
    (tagbody
     start
       (unless (or (eq object list) (atom list))
	 (setf e (setf (cdr e) (cons (car list) nil)))
	 (setf list (cdr list))
	 (go start)))
    (cdr r)))

#+(or WAM-CL LISP500) 
(defun tailp (object list)
  (tagbody
   start
     (when (eq object list) (return-from tailp t))
     (unless (consp list) (return-from tailp nil))
     (setf list (cdr list))
     (go start)))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun nthcdr (n list) (if (< n 1) list (nthcdr (- n 1) (cdr list))))


#+(or WAM-CL LISP500) 
(defun rest (list) (cdr list))


#+(or WAM-CL LISP500) 
(labels ((all-end (lists)
	   (dolist (elem lists nil)
	     (unless elem (return-from all-end t))))
	 (all-car (lists)
	   (when lists (cons (caar lists) (all-car (cdr lists)))))
	 (all-cdr (lists)
	   (when lists (cons (cdar lists) (all-cdr (cdr lists))))))
  (defun mapc (function &rest lists)
    (let ((list-1 (car lists)))
      (tagbody
       start
	 (when (all-end lists) (return-from mapc list-1))
	 (apply function (all-car lists))
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun mapcar (function &rest lists)
    (let ((result nil)
	  (end nil))
      (tagbody
       start
	 (when (all-end lists) (return-from mapcar result))
	 (let ((cons (cons (apply function (all-car lists)) nil)))
	   (setf end (if end (setf (cdr end) cons) (setf result cons))))
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun mapl (function &rest lists)
    (let ((list-1 (car lists)))
      (tagbody
       start
	 (when (all-end lists) (return-from mapl list-1))
	 (apply function lists)
	 (setf lists (all-cdr lists))
	 (go start))))
  (defun maplist (function &rest lists)
    (let ((result nil)
	  (end nil))
      (tagbody
       start
	 (when (all-end lists) (return-from maplist result))
	 (let ((cons (cons (apply function lists) nil)))
	   (setf end (if end (setf (cdr end) cons) (setf result cons))))
	 (setf lists (all-cdr lists))
	 (go start)))))



#+(or WAM-CL LISP500) 
(defun mapcan (function &rest lists)
  (apply #'nconc (apply #'mapcar function lists)))

#+(or WAM-CL LISP500) 
(defun mapcon (function &rest lists)
  (apply #'nconc (apply #'maplist function lists)))

#+(or WAM-CL LISP500) 
(defun acons (key datum alist) (cons (cons key datum) alist))

#+(or WAM-CL LISP500) 
(defun copy-alist (alist)
  (when alist (cons (if (consp (car alist))
			(cons (caar alist) (cdar alist))
			(car alist))
		    (copy-alist (cdr alist)))))

#+(or WAM-CL LISP500) 
(defun pairlis (keys data &optional alist)
  (tagbody
   start
     (when (and keys data)
       (setf alist (acons (car keys) (car data) alist))
       (setf keys (cdr keys))
       (setf data (cdr data))
       (go start)))
  alist)



#+(or WAM-CL LISP500) 
(defun some-list-2 (predicate list1 list2)
  (tagbody
   start
     (when (and list1 list2)
       (when (funcall predicate (car list1) (car list2))
	 (return-from some-list-2 t))
       (pop list1)
       (pop list2)
       (go start))))


(flet 
  ((satisfies (object elem &key key test test-not)
	 (let* ((zi (if key (funcall key elem) elem))
		(r (funcall (or test test-not #'eql) object zi)))
	   (if test-not (not r) r)))

       (satisfies-if (predicate elem &key key)
	 (funcall predicate (if key (funcall key elem) elem)))
       (satisfies-if-not (predicate elem &key key)
	 (not (funcall predicate (if key (funcall key elem) elem))))
       (seq-start (sequence &key (start 0) end from-end)
	 (if (listp sequence)
	     (if from-end
		 (let ((acc nil)
		       (sequence (nthcdr start sequence)))
		   (tagbody
		    start
		      (when (and sequence (or (not end) (< start end)))
			(push sequence acc)
			(setf sequence (cdr sequence))
			(setf start (+ 1 start))
			(go start)))
		   (list 3 acc start))
		 (list 2 (nthcdr start sequence) start))
	     (if from-end (cons 1 (- end 1)) (cons 0 start))))
       (seq-position (iter)
	 (case (car iter)
	   ((0 1) (cdr iter))
	   (t (caddr iter))))
       (seq-next (iter)
	 (case (car iter)
	   (0 (setf (cdr iter) (+ 1 (cdr iter))))
	   (1 (setf (cdr iter) (- (cdr iter) 1)))
	   (2 (setf (cadr iter) (cdadr iter))
	      (setf (caddr iter) (+ 1 (caddr iter))))
	   (t (setf (cadr iter) (cdadr iter))
	      (setf (caddr iter) (- (caddr iter) 1)))))
       (seq-ref (sequence iter)
	 (case (car iter)
	   ((0 1) (aref sequence (cdr iter)))
	   (2 (caadr iter))
	   (t (caaadr iter))))
       (seq-set (sequence iter value)
	 (case (car iter)
	   ((0 1) (setf (aref sequence (cdr iter)) value))
	   (2 (setf (caadr iter) value))
	   (t (setf (caaadr iter) value))))
       (seq-end-p (sequence iter &key start end from-end)
	 (case (car iter)
	   (0 (or (= (cdr iter) (length sequence))
		  (and end (= end (cdr iter)))))
	   (1 (< (cdr iter) start))
	   (2 (or (null (cadr iter)) (and end (= end (caddr iter)))))
	   (t (or (null (cadr iter)) (< (caddr iter) start)))))
       (seq-result (sequence iter result)
	 (case (car iter)
	   (0 (make-array (length result)
			  :element-type (array-element-type sequence)
			  :initial-contents (reverse result)))
	   (1 (make-array (length result)
			  :element-type (array-element-type sequence)
			  :initial-contents result))
	   (2 (reverse result))
	   (3 result))))



#+BUILTIN 
  (defun member (item list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies item (car list) rest)
	   (return-from member list))
	 (setf list (cdr list))
	 (go start))))


#+BUILTIN 
  (defun member-if (predicate list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies-if predicate (car list) rest)
	   (return-from member-if list))
	 (setf list (cdr list))
	 (go start))))

#+BUILTIN 
  (defun member-if-not (predicate list &rest rest)
    (tagbody
       start
       (when list
	 (when (apply #'satisfies-if-not predicate (car list) rest)
	   (return-from member-if list))
	 (setf list (cdr list))
	 (go start))))
  (defun subst (new old tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new old (car tree) rest))
	      (d (apply #'subst new old (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies old tree rest) new tree)))
  (defun subst-if (new predicate tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new predicate (car tree) rest))
	      (d (apply #'subst new predicate (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies-if predicate tree rest) new tree)))
  (defun subst-if-not (new predicate tree &rest rest)
    (if (consp tree)
	(let ((a (apply #'subst new predicate (car tree) rest))
	      (d (apply #'subst new predicate (cdr tree) rest)))
	  (if (and (eq a (car tree)) (eq d (cdr tree)))
	      tree
	      (cons a d)))
	(if (apply #'satisfies-if-not predicate tree rest) new tree)))
  (defun nsubst (new old tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new old (car tree) rest))
	  (setf (cdr tree) (apply #'subst new old (cdr tree) rest))
	  tree)
	(if (apply #'satisfies old tree rest) new tree)))
  (defun nsubst-if (new predicate tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new predicate (car tree) rest))
	  (setf (cdr tree) (apply #'subst new predicate (cdr tree) rest))
	  tree)
	(if (apply #'satisfies-if predicate tree rest) new tree)))
  (defun nsubst-if-not (new predicate tree &rest rest)
    (if (consp tree)
	(progn
	  (setf (car tree) (apply #'subst new predicate (car tree) rest))
	  (setf (cdr tree) (apply #'subst new predicate (cdr tree) rest))
	  tree)
	(if (apply #'satisfies-if-not predicate tree rest) new tree)))

#+BUILTIN 
  (defun assoc (item alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies item (car elem) rest)
	(return-from assoc elem))))
  (defun assoc-if (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if predicate (car elem) rest)
	(return-from assoc-if elem))))
  (defun assoc-if-not (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if-not predicate (car elem) rest)
	(return-from assoc-if-not elem))))
  (defun rassoc (item alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies item (cdr elem) rest)
	(return-from rassoc elem))))
  (defun rassoc-if (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if predicate (cdr elem) rest)
	(return-from rassoc-if elem))))
  (defun rassoc-if-not (predicate alist &rest rest)
    (dolist (elem alist)
      (when (apply #'satisfies-if-not predicate (cdr elem) rest)
	(return-from rassoc-if-not elem))))
  (defun adjoin (item list &rest rest)
    (dolist (elem list (cons item list))
      (when (apply #'satisfies item elem rest)
	(return-from adjoin list))))
  (defun set-exclusive-or (list-1 list-2 &rest rest &key key)
    (let ((result nil))
      (dolist (item list-1)
	(unless (apply #'member (if key (funcall key item) item) list-2 rest)
	  (push item result)))
      (dolist (item list-2)
	(block matches
	  (dolist (elem list-1)
	    (when (apply #'satisfies
			 (if key (funcall key elem) elem) item rest)
	      (return-from matches)))
	  (push item result)))
      result))
  (defun nset-exclusive-or (list-1 list-2 &rest rest &key key)
    (let ((result nil)
	  (list nil)
	  (item nil))
      (tagbody
       start-1
	 (unless list-1 (go start-2))
	 (setf item (car list-1))
	 (setf list list-2)
	 (setf prev nil)
       start-1-in
	 (unless list (go end-1-in))
	 (let ((elem (if key (funcall key (car list)) (car list))))
	   (when (apply #'satisfies item (if key (funcall key elem) elem) rest)
	     (if prev
		 (setf (cdr prev) (cdr list))
		 (setf list-2 (cdr list)))
	     (setf list-1 (cdr list-1))
	     (go start-1)))
	 (setf prev list)
	 (setf list (cdr list))
	 (go start-1-in)
       end-1-in
	 (setf item (cdr list-1))
	 (setf (cdr list-1) result)
	 (unless result (setf end list-1))
	 (setf result list-1)
	 (setf list-1 item)
	 (go start-1)
       start-2
	 (return-from nset-exclusive-or
	   (if end (progn (setf (cdr end) list-2) result) list-2)))))
  (defun fill (sequence item &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (seq-set sequence iter item)
	   (seq-next iter)
	   (go start))))
    sequence)
  (defun every (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (unless (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every nil))
	   (mapc #'seq-next iters)
	   (go start))))
    t)
  (defun some (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (let ((result (apply predicate (mapcar #'seq-ref sequences iters))))
	     (when result (return-from some result)))
	   (mapc #'seq-next iters)
	   (go start)))))
  (defun notevery (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (unless (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every t))
	   (mapc #'seq-next iters)
	   (go start)))))
  (defun notany (predicate &rest sequences)
    (let ((iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (when (apply predicate (mapcar #'seq-ref sequences iters))
	     (return-from every nil))
	   (mapc #'seq-next iters)
	   (go start))))
    t)
#+(BUILTIN BORKEN)
  (defun map-into (result-sequence function &rest sequences)
    (let ((result-iter (seq-start result-sequence))
	  (iters (mapcar #'seq-start sequences)))
      (tagbody
       start
	 (unless (some-list-2 #'seq-end-p sequences iters)
	   (seq-set result-sequence result-iter
		    (apply function (mapcar #'seq-ref sequences iters)))
	   (seq-next result-iter)
	   (mapc #'seq-next iters)
	   (go start))))
    result-sequence)
#+(BUILTIN BORKEN)
  (defun reduce (function sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (if (apply #'seq-end-p sequence iter rest)
	  (funcall function)
	  (let ((elem (seq-ref sequence iter)))
	    (seq-next iter)
	    (unless (apply #'seq-end-p sequence iter rest)
	      (tagbody
	       start
		 (setq elem (funcall function elem (seq-ref sequence iter)))
		 (seq-next iter)
		 (unless (apply #'seq-end-p sequence iter rest)
		   (go start))))
	    elem))))
  (defun count (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies item (seq-ref sequence iter) rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))
  (defun count-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if predicate (seq-ref sequence iter) rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))
  (defun count-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest))
	  (count 0))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if-not predicate (seq-ref sequence iter)
			rest)
	     (setf count (+ 1 count)))
	   (seq-next iter)
	   (go start)))
      count))

#+BUILTIN 
  (defun find (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies item elem rest)
	       (return-from find elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun find-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies-if predicate elem rest)
	       (return-from find-if elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun find-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (when (apply #'satisfies-if-not predicate elem rest)
	       (return-from find-if-not elem)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun position (item sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies item (seq-ref sequence iter) rest)
	     (return-from position (seq-position iter)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN 
  (defun position-if (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if predicate (seq-ref sequence iter) rest)
	     (return-from position-if (seq-position iter)))
	   (seq-next iter)
	   (go start)))))

#+BUILTIN
  (defun position-if-not (predicate sequence &rest rest)
    (let ((iter (apply #'seq-start sequence rest)))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (when (apply #'satisfies-if-not predicate (seq-ref sequence iter)
			rest)
	     (return-from position-if-not (seq-position iter)))
	   (seq-next iter)
	   (go start)))))
  (defun remove (item sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies item elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
  (defun remove-if (predicate sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies-if predicate elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
  (defun remove-if-not (predicate sequence &rest rest &key count)
    (let ((iter (apply #'seq-start sequence rest))
	  (result nil))
      (tagbody
       start
	 (unless (apply #'seq-end-p sequence iter rest)
	   (let ((elem (seq-ref sequence iter)))
	     (unless (and (apply #'satisfies-if-not predicate elem rest)
			  (or (not count) (not (minusp (decf count)))))
	       (push elem result)))
	   (seq-next iter)
	   (go start)))
      (seq-result sequence iter result)))
)                     


#+BUILTIN
#+(or WAM-CL LISP500) 
(defun null (object) (if object nil t))

#+BUILTIN
#+(or WAM-CL LISP500) 
(defun not (object) (if object nil t))




#+(or WAM-CL LISP500) 
(defun mod (x y) (multiple-value-call #'(lambda (q r) r) (floor x y)))

#+(or WAM-CL LISP500) 
#+BUILTIN
(defun functionp (object) (eq (type-of object) 'function))



#+(or WAM-CL LISP500) 
(defun coerce (object result-type)
  (if (typep object result-type)
      object
      (case result-type
	((t) object)
	(character (character object))
	(function (if (and (consp object) (eq (car object) 'lambda))
		      (eval (list 'function object))
		      (if (fboundp object)
			  (fdefinition object))
			  (error 'type-error :datum object
				 :expected-type result-type)))
	(t (error 'type-error :datum object :expected-type result-type)))))




#+(or WAM-CL LISP500) 
(defmacro deftype (name lambda-list &rest forms)
  `(ensure-type ',name #'(lambda ,lambda-list (block ,name ,@forms))))


#+(or WAM-CL LISP500) 
(defun *= (cons number)
  (or (not cons) (eq (car cons) '*) (= (car cons) number)))



#+(or WAM-CL LISP500) 
(defun ensure-type (name expander)
  (let ((cons (assoc name *type-expanders*)))
    (if cons
	(setf (cdr cons) expander)
	(push (cons name expander) *type-expanders*))
    name))



#+WAM-CL
#+(or WAM-CL LISP500) 
(defun ensure-package (name nicknames shadow shadowing-import-from use
		       import-from intern export)
  (let ((package (find-package name)))
    (unless package
      (setq package (make-package name :nicknames nicknames)))
    (shadow shadow package)
    (mapc #'(lambda (list)
	      (let ((imported-package (find-package (car list)))
		    (symbol-names (cdr list)))
		(shadowing-import (mapcar #'(lambda (symbol-name)
					      (find-symbol symbol-name
							   imported-package))
					  symbol-names)
				  package)))
	  shadowing-import-from)
    (use-package use package)
    (mapc #'(lambda (list)
	      (let ((imported-package (find-package (car list)))
		    (symbol-names (cdr list)))
		(import (mapcar #'(lambda (symbol-name)
				    (find-symbol symbol-name imported-package))
				symbol-names)
			package)))
	  import-from)
    (mapc #'(lambda (symbol-name) (intern symbol-name package)) intern)
    (export export package)
    package))

#+WAM-CL
#+(or WAM-CL LISP500) 
(defmacro defpackage (defined-package-name &rest options)
  (flet ((option (option-name)
	   (mapcan #'(lambda (option)
		       (when (eq (car option) option-name)
			 (mapcar #'designator-string (cdr option))))
		   options))
	 (options (option-name)
	   (mapcan #'(lambda (option)
		       (when (eq (car option) option-name)
			 (list (mapcar #'designator-string (cdr option)))))
		   options)))
    `(ensure-package ,(designator-string defined-package-name)
      ,(option :nicknames)
      ,(option :shadow) ,(options :shadowing-import-from) ,(option :use)
      ,(options :import-from) ,(option :intern) ,(option :export))))



#+BUILTIN
#+(or WAM-CL LISP500) 
(defun backquote-expand (list level)
  (if (consp list) 
      (if (eq 'backquote (car list)) 
	  (list 'list ''backquote 
		(backquote-expand (car (cdr list)) (+ level 1))) 
	  (if (eq 'unquote (car list)) 
	      (if (= level 0) 
		  (car (cdr list)) 
		  (list 'list ''unquote 
			(backquote-expand (car (cdr list)) (- level 1)))) 
	      (if (eq 'unquote-splicing (car list)) 
		  (if (= level 0) 
		      (values (car (cdr list)) t) 
		      (list 'list ''unquote-splicing 
			    (backquote-expand (car (cdr list)) (- level 1)))) 
		  (labels ((collect (list) 
			     (if (consp list) 
				 (cons (multiple-value-call 
					   #'(lambda (value 
						      &optional splicingp) 
					       (if splicingp 
						   value 
						   (list 'list value))) 
				       (backquote-expand (car list) level)) 
				     (collect (cdr list))) 
				 (list (list 'quote list))))) 
		    (cons 'append (collect list)))))) 
      (list 'quote list))) 

#+BUILTIN
#+(or WAM-CL LISP500) 
(defmacro backquote (form)
  (backquote-expand form 0))


#+(or WAM-CL LISP500) 
(defmacro with-simple-restart ((name format-control &rest format-arguments)
			       &rest forms)
  (let ((tag (gensym)))
    `(block ,tag
      (restart-bind
	  ((,name
	    #'(lambda () (return-from ,tag (values nil t)))
	     :interactive-function #'(lambda () nil)
	     :report-function #'(lambda (stream)
				  (apply #'format stream ',format-control
					 ',format-arguments))
	     :test-function #'(lambda () t)))
	,@forms))))


#+(or WAM-CL LISP500) 
(defun break (&optional format-control &rest format-arguments)
  (with-simple-restart (continue "Return from BREAK.")
    (let ((*debugger-hook* nil))
      (invoke-debugger (make-condition 'simple-condition
				       :format-control format-control
				       :format-arguments format-arguments))))
  nil)



