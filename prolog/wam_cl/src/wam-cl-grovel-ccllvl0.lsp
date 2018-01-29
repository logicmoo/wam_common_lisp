

\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/nfasload.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)

(require "FASLENV" "ccl:xdump;faslenv")


(defconstant $primsizes (make-array 23
                                    :element-type '(unsigned-byte 16)
                                    :initial-contents
                                    '(41 61 97 149 223 337 509 769 887 971 1153 1559 1733
                                      2609 2801 3917 5879 8819 13229 19843 24989 29789 32749)))
(defconstant $hprimes (make-array 8 
                                  :element-type '(unsigned-byte 16)
                                  :initial-contents '(5 7 11 13 17 19 23 29)))

;;; Symbol hash tables: (htvec . (hcount . hlimit))

(defmacro htvec (htab) `(%car ,htab))
(defmacro htcount (htab) `(%cadr ,htab))
(defmacro htlimit (htab) `(%cddr ,htab))
)

(eval-when (:execute :compile-toplevel)
  (assert (= 80 numfaslops)))





(defvar *fasl-dispatch-table* #80(%bad-fasl))





(defun %bad-fasl (s)
  (error "bad opcode near position ~d in FASL file ~s"
         (%fasl-get-file-pos s)
         (faslstate.faslfname s)))

(defun %cant-epush (s)
  (if (faslstate.faslepush s)
    (%bad-fasl s)))

(defun %epushval (s val)
  (setf (faslstate.faslval s) val)
  (when (faslstate.faslepush s)
    (setf (svref (faslstate.faslevec s) (faslstate.faslecnt s)) val)
    (incf (the fixnum (faslstate.faslecnt s))))
  val)

(defun %simple-fasl-read-buffer (s)
  (let* ((fd (faslstate.faslfd s))
         (buffer (faslstate.iobuffer s))
         (bufptr (%get-ptr buffer)))
    (declare (dynamic-extent bufptr)
             (type macptr buffer bufptr))
    (%setf-macptr bufptr (%inc-ptr buffer target::node-size))
    (setf (%get-ptr buffer) bufptr)
    (let* ((n (fd-read fd bufptr $fasl-buf-len)))
      (declare (fixnum n))
      (if (> n 0)
        (setf (faslstate.bufcount s) n)
        (error "Fix this: look at errno, EOF")))))

 
(defun %simple-fasl-read-byte (s)
  (loop
    (let* ((buffer (faslstate.iobuffer s))
           (bufptr (%get-ptr buffer)))
      (declare (dynamic-extent bufptr)
               (type macptr buffer bufptr))
      (if (>= (the fixnum (decf (the fixnum (faslstate.bufcount s))))
              0)
        (return
         (prog1
           (%get-unsigned-byte bufptr)
           (setf (%get-ptr buffer)
                 (%incf-ptr bufptr))))
        (%fasl-read-buffer s)))))

(defun %fasl-read-word (s)
  (the fixnum 
    (logior (the fixnum (ash (the fixnum (%fasl-read-byte s)) 8))
            (the fixnum (%fasl-read-byte s)))))


(defun %fasl-read-long (s)
  (logior (ash (%fasl-read-word s) 16) (%fasl-read-word s)))

(defun %fasl-read-signed-long (s)
  (logior (ash (%word-to-int (%fasl-read-word s)) 16)
          (%fasl-read-word s)))


(defun %fasl-read-count (s)
  (do* ((val 0)
        (shift 0 (+ shift 7))
        (done nil))
       (done val)
    (let* ((b (%fasl-read-byte s)))
      (declare (type (unsigned-byte 8) b))
      (setq done (logbitp 7 b) val (logior val (ash (logand b #x7f) shift))))))

(defun %simple-fasl-read-n-bytes (s ivector byte-offset n)
  (declare (fixnum byte-offset n))
  (do* ()
       ((= n 0))
    (let* ((count (faslstate.bufcount s))
           (buffer (faslstate.iobuffer s))
           (bufptr (%get-ptr buffer))
           (nthere (if (< count n) count n)))
      (declare (dynamic-extent bufptr)
               (type macptr buffer bufptr)
               (fixnum count nthere))
      (if (= nthere 0)
        (%fasl-read-buffer s)
        (progn
          (decf n nthere)
          (decf (the fixnum (faslstate.bufcount s)) nthere)
          (%copy-ptr-to-ivector bufptr 0 ivector byte-offset nthere)
          (incf byte-offset nthere)
          (setf (%get-ptr buffer)
                (%incf-ptr bufptr nthere)))))))
        

(defun %fasl-read-utf-8-string (s string nchars nextra)
  (declare (fixnum nchars nextra))
  (if (eql 0 nextra)
    (dotimes (i nchars)
      (setf (%scharcode string i) (%fasl-read-byte s)))
    (flet ((trailer-byte ()
             (when (> nextra 0)
               (decf nextra)
               (let* ((b (%fasl-read-byte s)))
                 (declare ((unsigned-byte 8) b))
                 (and (>= b #x80)
                      (< b #xc0)
                      (logand b #x3f))))))
      (declare (inline trailer-byte))
      (dotimes (i nchars)
        (let* ((b0 (%fasl-read-byte s)))
          (declare ((unsigned-byte 8) b0))
          (setf (%scharcode string i)
                (or
                 (cond ((< b0 #x80) b0)
                       ((and (>= b0 #xc2)
                             (< b0 #xe0))
                        (let* ((b1 (trailer-byte)))
                          (and b1 (logior (ash (logand b0 #x1f) 6) b1))))
                       ((and (>= b0 #xe0)
                             (< b0 #xf0))
                        (let* ((b1 (trailer-byte))
                               (b2 (trailer-byte)))
                          (and b1 b2 (logior (ash (logand b0 #x0f) 12)
                                             (logior (ash b1 6)
                                                     b2)))))
                       ((and (>= b0 #xf0)
                             (< b0 #xf5))
                        (let* ((b1 (trailer-byte))
                               (b2 (trailer-byte))
                               (b3 (trailer-byte)))
                          (and b1
                               b2
                               b3
                               (logior (ash (logand b0 #x7) 18)
                                       (logior (ash b1 12)
                                               (logior (ash b2 6)
                                                       b3)))))))
                 (char-code #\Replacement_Character))))))))


(defun %fasl-vreadstr (s)
  (let* ((nchars (%fasl-read-count s))
         (nextra (%fasl-read-count s))
         (copy t)
         (n nchars)
         (str (faslstate.faslstr s)))
    (declare (fixnum nchars n nextra))
    (if (> n (length str))
      (setq str (make-string n :element-type 'base-char))
      (setq copy nil))
    (%fasl-read-utf-8-string s str nchars nextra)
    (values str nchars copy)))


(defun %fasl-read-n-string (s string start n)
  (declare (fixnum start n))
  (do* ((i start (1+ i))
        (n n (1- n)))
       ((<= n 0))
    (declare (fixnum i n))
    (setf (%scharcode string i) (%fasl-read-byte s))))

(defun %fasl-nvreadstr (s)
  (let* ((nchars (%fasl-read-count s))
         (copy t)
         (n nchars)
         (str (faslstate.faslstr s)))
    (declare (fixnum n nchars))
    (if (> n (length str))
        (setq str (make-string n :element-type 'base-char))
        (setq copy nil))
    (%fasl-read-n-string  s str 0 nchars)
    (values str n copy)))

(defun %fasl-copystr (str len)
  (declare (fixnum len))
  (let* ((new (make-string len :element-type 'base-char)))
    (declare (simple-base-string new))
    (declare (optimize (speed 3)(safety 0)))
    (dotimes (i len new)
      (setf (schar new i) (schar str i)))))

(defun %fasl-dispatch (s op)
  (declare (fixnum op)) 
  (setf (faslstate.faslepush s) (logbitp $fasl-epush-bit op))
  #+debug
  (format t "~& dispatch: op = ~d at ~x" (logand op (lognot (ash 1 $fasl-epush-bit)))
          (1- (%fasl-get-file-pos s)))
  (funcall (svref (faslstate.fasldispatch s) (logand op (lognot (ash 1 $fasl-epush-bit)))) 
           s))

(defun %fasl-expr (s)
  (%fasl-dispatch s (%fasl-read-byte s))
  (faslstate.faslval s))

(defun %fasl-expr-preserve-epush (s)
  (let* ((epush (faslstate.faslepush s))
         (val (%fasl-expr s)))
    (setf (faslstate.faslepush s) epush)
    val))


(defun %fasl-vmake-symbol (s &optional idx)
  (let* ((n (%fasl-read-count s))
         (nextra (%fasl-read-count s))
         (str (make-string n :element-type 'base-char)))
    (declare (fixnum n))
    (%fasl-read-utf-8-string s str n nextra)
    (let* ((sym (make-symbol str)))
      (when idx (ensure-binding-index sym))
      (%epushval s sym))))

(defun %fasl-nvmake-symbol (s &optional idx)
  (let* ((n (%fasl-read-count s))
         (str (make-string n :element-type 'base-char)))
    (declare (fixnum n))
    (%fasl-read-n-string s str 0 n)
    (let* ((sym (make-symbol str)))
      (when idx (ensure-binding-index sym))
      (%epushval s sym))))

(defun %fasl-vintern (s package &optional binding-index)
  (multiple-value-bind (str len new-p) (%fasl-vreadstr s)
    (with-package-lock (package)
      (multiple-value-bind (symbol access internal-offset external-offset)
          (%find-symbol str len package)
        (unless access
          (unless new-p (setq str (%fasl-copystr str len)))
          (setq symbol (%add-symbol str package internal-offset external-offset)))
        (when binding-index
          (ensure-binding-index symbol))
        (%epushval s symbol)))))

(defun %fasl-nvintern (s package &optional binding-index)
  (multiple-value-bind (str len new-p) (%fasl-nvreadstr s)
    (with-package-lock (package)
      (multiple-value-bind (symbol access internal-offset external-offset)
          (%find-symbol str len package)
        (unless access
          (unless new-p (setq str (%fasl-copystr str len)))
          (setq symbol (%add-symbol str package internal-offset external-offset)))
        (when binding-index
          (ensure-binding-index symbol))
        (%epushval s symbol)))))

(defvar *package-refs*)
(setq *package-refs* (make-hash-table :test #'equal))
(defvar *package-refs-lock*)
(setq *package-refs-lock* (make-lock))

(defun register-package-ref (name)
  (unless (typep name 'string)
    (report-bad-arg name 'string))
  (let* ((ref
          (or (gethash name *package-refs*)
              (with-lock-grabbed (*package-refs-lock*)
                (or
                 (gethash name *package-refs*) ; check again
                 (let* ((r (make-package-ref name)))
                   (setf (gethash name *package-refs*) r)))))))
    (unless (package-ref.pkg ref)
      (setf (package-ref.pkg ref) (find-package name)))
    ref))


(dolist (p %all-packages%)
  (dolist (name (pkg.names p))
    (setf (package-ref.pkg (register-package-ref name)) p)))


(defun find-package (name)
  (if (typep  name 'package)
    name
    (%find-pkg (string name))))

(defun %pkg-ref-find-package (ref)
  (package-ref.pkg ref))

(defun set-package (name &aux (pkg (find-package name)))
  (if pkg
    (setq *package* pkg)
    (set-package (%kernel-restart $xnopkg name))))

  
(defun %find-pkg (name &optional (len (length name)))
  (declare (fixnum len))
  (with-package-list-read-lock
      (dolist (p %all-packages%)
        (if (dolist (pkgname (pkg.names p))
              (when (and (= (the fixnum (length pkgname)) len)
                         (dotimes (i len t)
                           ;; Aref: allow non-simple strings
                           (unless (eq (aref name i) (schar pkgname i))
                             (return))))
                (return t)))
          (return p)))))



(defun pkg-arg (thing &optional deleted-ok)
  (let* ((xthing (cond ((or (symbolp thing) (typep thing 'character))
                        (string thing))
                       ((typep thing 'string)
                        (ensure-simple-string thing))
                       (t
                        thing))))
    (let* ((typecode (typecode xthing)))
        (declare (fixnum typecode))
        (cond ((= typecode target::subtag-package)
               (if (or deleted-ok (pkg.names xthing))
                 xthing
                 (error "~S is a deleted package ." thing)))
              ((= typecode target::subtag-simple-base-string)
               (or (%find-pkg xthing)
                   (%kernel-restart $xnopkg xthing)))
              (t (report-bad-arg thing 'simple-string))))))

(defun %fasl-vpackage (s)
  (multiple-value-bind (str len new-p) (%fasl-vreadstr s)
    (let* ((p (%find-pkg str len)))
      (%epushval s (or p (%kernel-restart $XNOPKG (if new-p str (%fasl-copystr str len))))))))


(defun %fasl-nvpackage (s)
  (multiple-value-bind (str len new-p) (%fasl-nvreadstr s)
    (let* ((p (%find-pkg str len)))
      (%epushval s (or p  (%kernel-restart $XNOPKG (if new-p str (%fasl-copystr str len))))))))

(defun %fasl-vlistX (s dotp)
  (let* ((len (%fasl-read-count s)))
    (declare (fixnum len))
    (let* ((val (%epushval s (cons nil nil)))
           (tail val))
      (declare (type cons val tail))
      (setf (car val) (%fasl-expr s))
      (dotimes (i len)
        (setf (cdr tail) (setq tail (cons (%fasl-expr s) nil))))
      (if dotp
        (setf (cdr tail) (%fasl-expr s)))
      (setf (faslstate.faslval s) val))))

(deffaslop $fasl-noop (s)
  (%cant-epush s))


(deffaslop $fasl-vetab-alloc (s)
  (%cant-epush s)
  (setf (faslstate.faslevec s) (make-array (the fixnum (%fasl-read-count s)))
        (faslstate.faslecnt s) 0))

(deffaslop $fasl-platform (s)
  (%cant-epush s)
  (let* ((platform (%fasl-expr s))
         (host-platform (%get-kernel-global 'host-platform)))
    (declare (fixnum platform host-platform))
    (unless (= platform host-platform)
      (error "Not a native fasl file : ~s" (faslstate.faslfname s)))))


(deffaslop $fasl-veref (s)
  (let* ((idx (%fasl-read-count s)))
    (declare (fixnum idx))
    (if (>= idx (the fixnum (faslstate.faslecnt s)))
      (%bad-fasl s))
    (%epushval s (svref (faslstate.faslevec s) idx))))

#+x86-target
;;; Read a "concatenated" lisp function, in which the machine code
;;; and constants are both contained in the same underlying uvector.
(deffaslop $fasl-clfun (s)
  (let* ((size-in-elements (%fasl-read-count s))
         (size-of-code (%fasl-read-count s))
         (vector (%alloc-misc size-in-elements target::subtag-function))
         (function (function-vector-to-function vector)))
    (declare (fixnum size-in-elements size-of-code))
    (%epushval s function)
    (%fasl-read-n-bytes s vector 0 (ash size-of-code target::word-shift))
    #+x8632-target
    (%update-self-references vector)
    (do* ((numconst (- size-in-elements size-of-code))
          (i 0 (1+ i))
          (constidx size-of-code (1+ constidx)))
         ((= i numconst)
          (setf (faslstate.faslval s) function))
      (declare (fixnum i numconst constidx))
      (setf (%svref vector constidx) (%fasl-expr s)))))
    
    
(deffaslop $fasl-lfuncall (s)
  (let* ((fun (%fasl-expr-preserve-epush s)))
    ;(break "fun = ~s" fun)
     (%epushval s (funcall fun))))

(deffaslop $fasl-globals (s)
  (setf (faslstate.faslgsymbols s) (%fasl-expr s)))

(deffaslop $fasl-char (s)
  (%epushval s (code-char (%fasl-read-count s))))

;;; Deprecated
(deffaslop $fasl-fixnum (s)
  (%epushval
   s
   (logior (the fixnum (ash (the fixnum (%word-to-int (%fasl-read-word s)))
                            16))
           (the fixnum (%fasl-read-word s))) ))

(deffaslop $fasl-s32 (s)
  (%epushval s (%fasl-read-signed-long s)))

(deffaslop $fasl-s64 (s)
  (%epushval s (logior (ash (%fasl-read-signed-long s) 32)
                       (%fasl-read-long s))))

(deffaslop $fasl-dfloat (s)
  ;; A double-float is a 3-element "misc" object.
  ;; Element 0 is always 0 and exists solely to keep elements 1 and 2
  ;; aligned on a 64-bit boundary.
  (%epushval s (double-float-from-bits (%fasl-read-long s) (%fasl-read-long s))))

(deffaslop $fasl-sfloat (s)
  (%epushval s (host-single-float-from-unsigned-byte-32 (%fasl-read-long s))))

(deffaslop $fasl-vstr (s)
  (let* ((nchars (%fasl-read-count s))
         (nextra (%fasl-read-count s))
         (str (make-string (the fixnum nchars) :element-type 'base-char)))
    (%epushval s str)
    (%fasl-read-utf-8-string s str nchars nextra)))


(deffaslop $fasl-nvstr (s)
  (let* ((n (%fasl-read-count s))
         (str (make-string (the fixnum n) :element-type 'base-char)))
    (%epushval s str)
    (%fasl-read-n-string s str 0 n)))

(deffaslop $fasl-word-fixnum (s)
  (%epushval s (%word-to-int (%fasl-read-word s))))

(deffaslop $fasl-vmksym (s)
  (%fasl-vmake-symbol s))

(deffaslop $fasl-nvmksym (s)
  (%fasl-nvmake-symbol s))

(deffaslop $fasl-vmksym-special (s)
  (%fasl-vmake-symbol s t))

(deffaslop $fasl-nvmksym-special (s)
  (%fasl-nvmake-symbol s t))

(deffaslop $fasl-vintern (s)
  (%fasl-vintern s *package*))

(deffaslop $fasl-nvintern (s)
  (%fasl-nvintern s *package*))

(deffaslop $fasl-vintern-special (s)
  (%fasl-vintern s *package* t))

(deffaslop $fasl-nvintern-special (s)
  (%fasl-nvintern s *package* t))




(deffaslop $fasl-vpkg-intern (s)
  (let* ((pkg (%fasl-expr-preserve-epush s)))
    #+paranoia
    (setq pkg (pkg-arg pkg))
    (%fasl-vintern s pkg)))

(deffaslop $fasl-nvpkg-intern (s)
  (let* ((pkg (%fasl-expr-preserve-epush s)))
    #+paranoia
    (setq pkg (pkg-arg pkg))
    (%fasl-nvintern s pkg)))

(deffaslop $fasl-vpkg-intern-special (s)
  (let* ((pkg (%fasl-expr-preserve-epush s)))
    #+paranoia
    (setq pkg (pkg-arg pkg))
    (%fasl-vintern s pkg t)))

(deffaslop $fasl-nvpkg-intern-special (s)
  (let* ((pkg (%fasl-expr-preserve-epush s)))
    #+paranoia
    (setq pkg (pkg-arg pkg))
    (%fasl-nvintern s pkg t)))

(deffaslop $fasl-vpkg (s)
  (%fasl-vpackage s))

(deffaslop $fasl-nvpkg (s)
  (%fasl-nvpackage s))

(deffaslop $fasl-cons (s)
  (let* ((cons (%epushval s (cons nil nil))))
    (declare (type cons cons))
    (setf (car cons) (%fasl-expr s)
          (cdr cons) (%fasl-expr s))
    (setf (faslstate.faslval s) cons)))

(deffaslop $fasl-vlist (s)
  (%fasl-vlistX s nil))

(deffaslop $fasl-vlist* (s)
  (%fasl-vlistX s t))

(deffaslop $fasl-nil (s)
  (%epushval s nil))

(deffaslop $fasl-timm (s)
  (rlet ((p :int))
    (setf (%get-long p) (%fasl-read-long s))
    (%epushval s (%get-unboxed-ptr p))))

(deffaslop $fasl-symfn (s)
  (%epushval s (%function (%fasl-expr-preserve-epush s))))
    
(deffaslop $fasl-eval (s)
  (%epushval s (eval (%fasl-expr-preserve-epush s))))

;;; For bootstrapping. The real version is cheap-eval in l1-readloop
(when (not (fboundp 'eval))
  (defun eval (form)
    (if (and (listp form)
             (let ((f (%car form)))
               (and (symbolp f)
                    (functionp (fboundp f)))))
      (do* ((tail (%cdr form) (%cdr tail)))
           ((null tail) (apply (%car form) (%cdr form)))
        (let* ((head (car tail)))
          (when (and (consp head) (eq (car head) 'quote))
            (setf (car tail) (cadr head)))))
      (error "Can't eval yet: ~s" form))))


(deffaslop $fasl-vivec (s)
  (let* ((subtag (%fasl-read-byte s))
         (element-count (%fasl-read-count s))
         (size-in-bytes (subtag-bytes subtag element-count))
         (vector (%alloc-misc element-count subtag))
         (byte-offset (or #+32-bit-target (if (= subtag target::subtag-double-float-vector) 4) 0)))
    (declare (fixnum subtag element-count size-in-bytes))
    (%epushval s vector)
    (%fasl-read-n-bytes s vector byte-offset size-in-bytes)
    vector))

(defun fasl-read-ivector (s subtag)
  (let* ((element-count (%fasl-read-count s))
         (size-in-bytes (subtag-bytes subtag element-count))
         (vector (%alloc-misc element-count subtag)))
    (declare (fixnum subtag element-count size-in-bytes))
    (%epushval s vector)
    (%fasl-read-n-bytes s vector 0 size-in-bytes)
    vector))
  
(deffaslop $fasl-u8-vector (s)
  (fasl-read-ivector s target::subtag-u8-vector))

(deffaslop $fasl-s8-vector (s)
  (fasl-read-ivector s target::subtag-s8-vector))

(deffaslop $fasl-u16-vector (s)
  (fasl-read-ivector s target::subtag-u16-vector))

(deffaslop $fasl-s16-vector (s)
  (fasl-read-ivector s target::subtag-s16-vector))

(deffaslop $fasl-u32-vector (s)
  (fasl-read-ivector s target::subtag-u32-vector))

(deffaslop $fasl-s32-vector (s)
  (fasl-read-ivector s target::subtag-s32-vector))

#+64-bit-target
(deffaslop $fasl-u64-vector (s)
  (fasl-read-ivector s target::subtag-u64-vector))

#+64-bit-target
(deffaslop $fasl-s64-vector (s)
  (fasl-read-ivector s target::subtag-s64-vector))

(deffaslop $fasl-bit-vector (s)
  (fasl-read-ivector s target::subtag-bit-vector))

(deffaslop $fasl-bignum32 (s)
  (let* ((element-count (%fasl-read-count s))
         (size-in-bytes (* element-count 4))
         (num (%alloc-misc element-count target::subtag-bignum)))
    (declare (fixnum element-count size-in-bytes))
    (%fasl-read-n-bytes s num 0 size-in-bytes)
    (setq num (%normalize-bignum-2 t num))
    (%epushval s num)
    num))

(deffaslop $fasl-single-float-vector (s)
  (fasl-read-ivector s target::subtag-single-float-vector))

(deffaslop $fasl-double-float-vector (s)
  #+64-bit-target
  (fasl-read-ivector s target::subtag-double-float-vector)
  #+32-bit-target
  (let* ((element-count (%fasl-read-count s))
         (size-in-bytes (subtag-bytes target::subtag-double-float-vector
                                      element-count))
         (vector (%alloc-misc element-count
                              target::subtag-double-float-vector)))
    (declare (fixnum element-count size-in-bytes))
    (%epushval s vector)
    (%fasl-read-n-bytes s vector (- target::misc-dfloat-offset
                                    target::misc-data-offset)
                        size-in-bytes)
    vector))



#-x86-target
(deffaslop $fasl-code-vector (s)
  (let* ((element-count (%fasl-read-count s))
         (size-in-bytes (* 4 element-count))
         (vector (allocate-typed-vector :code-vector element-count)))
    (declare (fixnum element-count size-in-bytes))
    (%epushval s vector)
    (%fasl-read-n-bytes s vector 0 size-in-bytes)
    (%make-code-executable vector)
    vector))

(defun fasl-read-gvector (s subtype)
  (let* ((n (%fasl-read-count s))
         (vector (%alloc-misc n subtype)))
    (declare (fixnum n subtype))
    (%epushval s vector)
    (dotimes (i n)
      (setf (%svref vector i) (%fasl-expr s)))
    #+arm-target (when (= subtype arm::subtag-function)
                   (%fix-fn-entrypoint vector))
    (setf (faslstate.faslval s) vector)))

(deffaslop $fasl-vgvec (s)
  (let* ((subtype (%fasl-read-byte s)))
    (fasl-read-gvector s subtype)))
  
(deffaslop $fasl-ratio (s)
  (let* ((r (%alloc-misc target::ratio.element-count target::subtag-ratio)))
    (%epushval s r)
    (setf (%svref r target::ratio.numer-cell) (%fasl-expr s)
          (%svref r target::ratio.denom-cell) (%fasl-expr s))
    (setf (faslstate.faslval s) r)))

(deffaslop $fasl-complex (s)
  (let* ((realpart (%fasl-expr-preserve-epush s))
         (imagpart (%fasl-expr-preserve-epush  s))
         (c (complex realpart imagpart)))
    (%epushval s c)
    (setf (faslstate.faslval s) c)))

(deffaslop $fasl-t-vector (s)
  (fasl-read-gvector s target::subtag-simple-vector))

(deffaslop $fasl-function (s)
  (fasl-read-gvector s target::subtag-function))

(deffaslop $fasl-istruct (s)
  (fasl-read-gvector s target::subtag-istruct))

(deffaslop $fasl-vector-header (s)
  (fasl-read-gvector s target::subtag-vectorH))

(deffaslop $fasl-array-header (s)
  (fasl-read-gvector s target::subtag-arrayH))


(deffaslop $fasl-defun (s)
  (%cant-epush s)
  (%defun (%fasl-expr s) (%fasl-expr s)))

(deffaslop $fasl-macro (s)
  (%cant-epush s)
  (%macro (%fasl-expr s) (%fasl-expr s)))

(deffaslop $fasl-defconstant (s)
  (%cant-epush s)
  (%defconstant (%fasl-expr s) (%fasl-expr s) (%fasl-expr s)))

(deffaslop $fasl-defparameter (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s)))
    (%defvar sym (%fasl-expr s))
    (set sym val)))

;;; (defvar var)
(deffaslop $fasl-defvar (s)
  (%cant-epush s)
  (%defvar (%fasl-expr s)))

;;; (defvar var initfom doc)
(deffaslop $fasl-defvar-init (s)
  (%cant-epush s)
  (let* ((sym (%fasl-expr s))
         (val (%fasl-expr s)))
    (unless (%defvar sym (%fasl-expr s))
      (set sym val))))


(deffaslop $fasl-prog1 (s)
  (let* ((val (%fasl-expr s)))
    (%fasl-expr s)
    (setf (faslstate.faslval s) val)))



(deffaslop $fasl-src (s)
  (%cant-epush s)
  (let* ((source-file (%fasl-expr s)))
    ; (format t "~& source-file = ~s" source-file)
    (setq *loading-file-source-file* source-file)))

(deffaslop $fasl-toplevel-location (s)
  (%cant-epush s)
  (setq *loading-toplevel-location* (%fasl-expr s)))

(defvar *modules* nil)

;;; Bootstrapping version
(defun provide (module-name)
  (push (string module-name) *modules*))

(deffaslop $fasl-provide (s)
  (provide (%fasl-expr s)))

(deffaslop $fasl-istruct-cell (s)
  (%epushval s (register-istruct-cell (%fasl-expr-preserve-epush s))))



;;; files compiled with code coverage do this
;; list of lfuns and (source-fn-name vector-of-lfuns external-format id), the latter put there by fasloading.
(defvar *code-covered-functions* nil)

(defun register-code-covered-functions (functions &optional external-format id)
  ;; unpack the parent-note references - see comment at fcomp-digest-code-notes
  (labels ((reg (lfun refs)
	     (unless (memq lfun refs)
	       (let* ((lfv (function-to-function-vector lfun))
		      (start #+ppc-target 0 #+x86-target (%function-code-words lfun))
		      (refs (cons lfun refs)))
		 (declare (dynamic-extent refs))
		 (loop for i from start below (uvsize lfv) as imm = (uvref lfv i)
		       do (typecase imm
			    (code-note
			     (let ((parent (code-note-parent-note imm)))
			       (when (integerp parent)
				 (setf (code-note-parent-note imm) (uvref lfv parent)))))
			    (function (reg imm refs))))))))
    (loop for fn across functions do (reg fn nil)))
  (let ((a (assoc (pathname *loading-file-source-file*)
                  *code-covered-functions*
                  :test #'(lambda (p q)
			    (and (equalp (pathname-name p) (pathname-name q))
				 ;; same name, so worth trying harder to match 'em up.
				 (or (equal p q)
				     (let ((p (full-pathname p)) (q (full-pathname q)))
				       (and p q (equalp p q)))
				     (let ((p (probe-file p)) (q (probe-file q)))
				       (and p q (equalp p q)))))))))
    (when (null a)
      (push (setq a (list nil nil nil nil)) *code-covered-functions*))
    (setf (car a) *loading-file-source-file*
          (cadr a) functions
          (caddr a) external-format
          (cadddr a) id))
  nil)

;;; The loader itself

(defun %simple-fasl-set-file-pos (s new)
  (let* ((fd (faslstate.faslfd s))
         (posoffset (fd-tell fd)))
    (if (>= (decf posoffset new) 0)
      (let* ((count (faslstate.bufcount s)))
        (if (>= (decf count posoffset ) 0)
          (progn
            (setf (faslstate.bufcount s) posoffset)
            (incf #+32-bit-target (%get-long (faslstate.iobuffer s))
                  #+64-bit-target (%%get-signed-longlong (faslstate.iobuffer s)
                                                        0)
                  count)
            (return-from %simple-fasl-set-file-pos nil)))))
    (progn
      (setf (faslstate.bufcount s) 0)
      (fd-lseek fd new #$SEEK_SET))))

(defun %simple-fasl-get-file-pos (s)
  (- (fd-tell (faslstate.faslfd s)) (faslstate.bufcount s)))

(defparameter *%fasload-verbose* t)

;;; the default fasl file opener sets up the fasl state and checks the header
(defun %simple-fasl-open (string s)
  (let* ((ok nil)
	 (fd (fd-open string #$O_RDONLY))
	 (err 0))
    (declare (fixnum fd))
    (if (>= fd 0)
      (if (< (fd-lseek fd 0 #$SEEK_END) 4)
        (setq err $xnotfasl)
        (progn
          (setq err 0)
          (setf (faslstate.bufcount s) 0
                (faslstate.faslfd s) fd)
          (fd-lseek fd 0 #$SEEK_SET)
          (multiple-value-setq (ok err) (%fasl-check-header s))))
      (setq err fd))
    (unless (eql err 0) (setf (faslstate.faslerr s) err))
    ok))

;;; once the fasl state is set up, this checks the fasl header and
;;; returns (values ok err)
(defun %fasl-check-header (s)
  (let* ((signature (%fasl-read-word s)))
    (declare (fixnum signature))
    (if (= signature $fasl-file-id)
	(values t 0)
      (if (= signature $fasl-file-id1)
	  (progn
	    (%fasl-set-file-pos s (%fasl-read-long s))
	    (values t 0))
	(values nil $xnotfasl)))))

(defun %simple-fasl-close (s)
  (let* ((fd (faslstate.faslfd s)))
    (when fd (fd-close fd))))

(defun %simple-fasl-init-buffer (s)
  (declare (ignore s))
  nil)

(defvar *fasl-api* nil)
(setf *fasl-api* (%istruct 'faslapi
			   #'%simple-fasl-open
			   #'%simple-fasl-close
			   #'%simple-fasl-init-buffer
			   #'%simple-fasl-set-file-pos
			   #'%simple-fasl-get-file-pos
			   #'%simple-fasl-read-buffer
			   #'%simple-fasl-read-byte
			   #'%simple-fasl-read-n-bytes))

(defun %fasl-open (string s)
  (funcall (faslapi.fasl-open *fasl-api*) string s))
(defun %fasl-close (s)
  (funcall (faslapi.fasl-close *fasl-api*) s))
(defun %fasl-init-buffer (s)
  (funcall (faslapi.fasl-init-buffer *fasl-api*) s))
(defun %fasl-set-file-pos (s new)
  (funcall (faslapi.fasl-set-file-pos *fasl-api*) s new))
(defun %fasl-get-file-pos (s)
  (funcall (faslapi.fasl-get-file-pos *fasl-api*) s))
(defun %fasl-read-buffer (s)
  (funcall (faslapi.fasl-read-buffer *fasl-api*) s))
(defun %fasl-read-byte (s)
  (funcall (faslapi.fasl-read-byte *fasl-api*) s))
(defun %fasl-read-n-bytes (s ivector byte-offset n)
  (funcall (faslapi.fasl-read-n-bytes *fasl-api*) s ivector byte-offset n))

(defun bootstrapping-fasl-min-version ()
  (logior #xff00 (logand #xff target::fasl-min-version)))

(%fhave 'target-fasl-min-version #'bootstrapping-fasl-min-version)

(defun bootstrapping-fasl-max-version ()
  (logior #xff00 (logand #xff target::fasl-max-version)))

(%fhave 'target-fasl-max-version #'bootstrapping-fasl-max-version)

(defun %fasload (string &optional (table *fasl-dispatch-table*))
  ;;(dbg string) 
  (when (and *%fasload-verbose*
	     (not *load-verbose*))
    (%string-to-stderr ";Loading ") (pdbg string))
  (let* ((s (%istruct
             'faslstate
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil
             nil)))
    (declare (dynamic-extent s))
    (setf (faslstate.faslfname s) string)
    (setf (faslstate.fasldispatch s) table)
    (setf (faslstate.faslversion s) 0)
    (%stack-block ((buffer (+ target::node-size $fasl-buf-len)))
      (setf (faslstate.iobuffer s) buffer)
      (%fasl-init-buffer s)
      (let* ((parse-string (make-string 255 :element-type 'base-char)))
        (declare (dynamic-extent parse-string))
        (setf (faslstate.oldfaslstr s) nil
              (faslstate.faslstr s) parse-string)
	(unwind-protect
             (when (%fasl-open string s)
               (let* ((nblocks (%fasl-read-word s)))
                 (declare (fixnum nblocks))
                 (unless (= nblocks 0)
                   (let* ((pos (%fasl-get-file-pos s)))
                     (dotimes (i nblocks)
                       (%fasl-set-file-pos s pos)
                       (%fasl-set-file-pos s (%fasl-read-long s))
                       (incf pos 8)
                       (let* ((version (%fasl-read-word s)))
                         (declare (fixnum version))
                         (if (or (> version (target-fasl-max-version))
                                 (< version (target-fasl-min-version)))
                           (%err-disp (if (>= version #xff00) $xfaslvers $xnotfasl))
                           (progn
                             (setf (faslstate.faslversion s) version)
                             (%fasl-read-word s) 
                             (%fasl-read-word s) ; Ignore kernel version stuff
                             (setf (faslstate.faslevec s) nil
                                   (faslstate.faslecnt s) 0)
                             (do* ((op (%fasl-read-byte s) (%fasl-read-byte s)))
                                  ((= op $faslend))
                               (declare (fixnum op))
                               (%fasl-dispatch s op))))))))))
	  (%fasl-close s))
	(let* ((err (faslstate.faslerr s)))
	  (if err
            (progn
              (when *%fasload-verbose*
                (let* ((herald ";!!Error loading ")
                       (hlen (length herald))
                       (len (length string))
                       (msg (make-string (+ hlen len))))
                  (declare (dynamic-extent msg))
                  (%copy-ivector-to-ivector herald 0 msg 0 (* hlen 4))
                  (%copy-ivector-to-ivector string 0 msg (* hlen 4) (* len 4))
                  (bug msg)))
              (values nil err))
            (values t nil)))))))


(defun %new-package-hashtable (size)
  (%initialize-htab (cons nil (cons 0 0)) size))

(defun %initialize-htab (htab size)
  (declare (fixnum size))
  ;; Ensure that "size" is relatively prime to all secondary hash values.
  ;; If it's small enough, pick the next highest known prime out of the
  ;; "primsizes" array.  Otherwize, iterate through all all of "hprimes"
  ;; until we find something relatively prime to all of them.
  (setq size
        (if (> size 32749)
          (do* ((nextsize (logior 1 size) (+ nextsize 2)))
               ()
            (declare (fixnum nextsize))
            (when (dotimes (i 8 t)
                    (unless (eql 1 (gcd nextsize (uvref #.$hprimes i)))
                      (return)))
              (return nextsize)))
          (dotimes (i (the fixnum (length #.$primsizes)))
            (let* ((psize (uvref #.$primsizes i)))
              (declare (fixnum psize))
              (if (>= psize size) 
                (return psize))))))
  (setf (htvec htab) (make-array size #|:initial-element 0|#))
  (setf (htcount htab) 0)
  (setf (htlimit htab) (the fixnum (- size (the fixnum (ash size -3)))))
  htab)


(defun %resize-htab (htab)
  (declare (optimize (speed 3) (safety 0)))
  (without-interrupts
   (let* ((old-vector (htvec htab))
          (old-len (length old-vector)))
     (declare (fixnum old-len)
              (simple-vector old-vector))
     (let* ((nsyms 0))
       (declare (fixnum nsyms))
       (dovector (s old-vector)
         (when (symbolp s) (incf nsyms)))
       (%initialize-htab htab 
                         (the fixnum (+ 
                                      (the fixnum 
                                        (+ nsyms (the fixnum (ash nsyms -2))))
                                      2)))
       (let* ((new-vector (htvec htab))
              (nnew 0))
         (declare (fixnum nnew)
                  (simple-vector new-vector))
         (dotimes (i old-len (setf (htcount htab) nnew))
           (let* ((s (svref old-vector i)))
               (if (symbolp s)
                 (let* ((pname (symbol-name s)))
                   (setf (svref 
                          new-vector 
                          (nth-value 
                           2
                           (%get-htab-symbol 
                            pname
                            (length pname)
                            htab)))
                         s)
                   (incf nnew)))))
         htab)))))
        
(defun hash-pname (str len)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((primary (mixup-hash-code (%pname-hash str len))))
    (declare (fixnum primary))
    (values primary (aref (the (simple-array (unsigned-byte 16) (8)) $hprimes) (logand primary 7)))))
    


(defun %get-hashed-htab-symbol (str len htab primary secondary)
  (declare (optimize (speed 3) (safety 0))
           (fixnum primary secondary len))
  (let* ((vec (htvec htab))
         (vlen (length vec)))
    (declare (fixnum vlen))
    (do* ((idx (fast-mod primary vlen) (+ i secondary))
          (i idx (if (>= idx vlen) (- idx vlen) idx))
          (elt (svref vec i) (svref vec i)))
         ((eql elt 0) (values nil nil i))
      (declare (fixnum i idx))
      (when (symbolp elt)
        (let* ((pname (symbol-name elt)))
          (if (and 
               (= (the fixnum (length pname)) len)
               (dotimes (j len t)
                 (unless (eq (schar str j) (schar pname j))
                   (return))))
            (return (values t (%symptr->symbol elt) i))))))))

(defun %get-htab-symbol (string len htab)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (p s) (hash-pname string len)
    (%get-hashed-htab-symbol string len htab p s)))

(defun %find-symbol (string len package)
  (declare (optimize (speed 3) (safety 0)))
  (multiple-value-bind (found-p sym internal-offset)
                       (%get-htab-symbol string len (pkg.itab package))
    (if found-p
      (values sym :internal internal-offset nil)
      (multiple-value-bind (found-p sym external-offset)
                           (%get-htab-symbol string len (pkg.etab package))
        (if found-p
          (values sym :external internal-offset external-offset)
          (dolist (p (pkg.used package) (values nil nil internal-offset external-offset))
            (multiple-value-bind (found-p sym)
                                 (%get-htab-symbol string len (pkg.etab p))
              (when found-p
                (return (values sym :inherited internal-offset external-offset))))))))))
          
(defun %htab-add-symbol (symbol htab idx)
  (declare (optimize (speed 3) (safety 0)))
  (setf (svref (htvec htab) idx) (%symbol->symptr symbol))
  (if (>= (incf (the fixnum (htcount htab)))
          (the fixnum (htlimit htab)))
    (%resize-htab htab))
  symbol)

(defun %set-symbol-package (symbol package-or-nil)
  (declare (optimize (speed 3) (safety 0)))
  (let* ((symvec (symptr->symvector (%symbol->symptr symbol)))
         (old-pp (%svref symvec target::symbol.package-predicate-cell)))
    (if (consp old-pp)
      (setf (car old-pp) package-or-nil)
      (setf (%svref symvec target::symbol.package-predicate-cell) package-or-nil))))


(let* ((force-export-packages (list *keyword-package*))
       (force-export-packages-lock (make-lock)))
  (defun force-export-packages ()
    (with-lock-grabbed (force-export-packages-lock)
      (copy-list force-export-packages)))
  (defun package-force-export (p)
    (let* ((pkg (pkg-arg p)))
      (with-lock-grabbed (force-export-packages-lock)
        (pushnew pkg force-export-packages))
    pkg))
  (defun force-export-package-p (pkg)
    (with-lock-grabbed (force-export-packages-lock)
      (if (memq pkg force-export-packages)
        t))))


(defun %insert-symbol (symbol package internal-idx external-idx &optional force-export)
  (let* ((symvec (symptr->symvector (%symbol->symptr symbol)))
         (package-predicate (%svref symvec target::symbol.package-predicate-cell))
         (keyword-package (eq package *keyword-package*)))
    ;; Set home package
    (if package-predicate
      (if (listp package-predicate)
        (unless (%car package-predicate) (%rplaca package-predicate package)))
      (setf (%svref symvec target::symbol.package-predicate-cell) package))
    (if (or force-export (force-export-package-p package))
      (progn
        (%htab-add-symbol symbol (pkg.etab package) external-idx)
        (if keyword-package
          ;;(define-constant symbol symbol)
          (progn
            (%set-sym-global-value symbol symbol)
            (%symbol-bits symbol 
                          (logior (ash 1 $sym_vbit_special) 
                                  (ash 1 $sym_vbit_const)
                                  (the fixnum (%symbol-bits symbol)))))))
      (%htab-add-symbol symbol (pkg.itab package) internal-idx))
    (let* ((hook (pkg.intern-hook package)))
      (when hook (funcall hook symbol)))
    symbol))

;;; PNAME must be a simple string!
(defun %add-symbol (pname package internal-idx external-idx &optional force-export)
  (let* ((sym (make-symbol pname)))
    (%insert-symbol sym package internal-idx external-idx force-export)))




;;; The initial %toplevel-function% sets %toplevel-function% to NIL;
;;; if the %fasload call fails, the lisp should exit (instead of
;;; repeating the process endlessly ...


(defvar %toplevel-function%
  #'(lambda ()
      (declare (special *xload-cold-load-functions*
                        *xload-cold-load-documentation*
                        *xload-startup-file*
                        *early-class-cells*))
      (%set-tcr-toplevel-function (%current-tcr) nil) ; should get reset by l1-boot.
      (setq %system-locks% (%cons-population nil))
      ;; Need to make %ALL-PACKAGES-LOCK% early, so that we can casually
      ;; do SET-PACKAGE in cold load functions.
      (setq %all-packages-lock% (make-read-write-lock))
      (dolist (f (prog1 *xload-cold-load-functions* (setq *xload-cold-load-functions* nil)))
        (funcall f))
      (dolist (pair (prog1 *early-class-cells* (setq *early-class-cells* nil)))
        (setf (gethash (car pair) %find-classes%) (cdr pair)))
      (dolist (p %all-packages%)
        (%resize-htab (pkg.itab p))
        (%resize-htab (pkg.etab p)))
      (dolist (f (prog1 *xload-cold-load-documentation* (setq *xload-cold-load-documentation* nil)))
        (apply 'set-documentation f))
      ;; Can't bind any specials until this happens
      (let* ((max 0))
        (%map-areas #'(lambda (symvec)
                        (when (= (the fixnum (typecode symvec))
                                 target::subtag-symbol)
                          (let* ((s (symvector->symptr symvec))
				 (idx (symbol-binding-index s)))
                            (when (> idx 0)
                              (cold-load-binding-index s))
                            (when (> idx max)
                              (setq max idx))))))
        (%set-binding-index max))
      (%fasload *xload-startup-file*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/nfasload.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-cfm-support.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.


; l0-cfm-support.lisp

(in-package "CCL")

#+windows-target
(progn
  (defvar *windows-invalid-handle* nil)
  (setq *windows-invalid-handle* (%int-to-ptr #+64-bit-target #xffffffffffffffff #+32-bit-target #xffffffff)))


;;; We have several different conventions for representing an
;;; "entry" (a foreign symbol address, possibly represented as
;;; something cheaper than a MACPTR.)  Destructively modify
;;; ADDR so that it points to where ENTRY points.
(defun entry->addr (entry addr)
  #+ppc32-target
  ;; On PPC32, all function addresses have their low 2 bits clear;
  ;; so do fixnums.
  (%setf-macptr-to-object addr entry)
  #+ppc64-target
  ;; On PPC64, some addresses can use the fixnum trick.  In other
  ;; cases, an "entry" is just a MACPTR.
  (if (typep entry 'fixnum)
    (%setf-macptr-to-object addr entry)
    (%setf-macptr addr entry))
  ;; On x86, an "entry" is just an integer.  There might elswehere be
  ;; some advantage in treating those integers as signed (they might
  ;; be more likely to be fixnums, for instance), so ensure that they
  ;; aren't.
  #+(or x86-target arm-target)
  (%setf-macptr addr (%int-to-ptr
                      (if (< entry 0)
                        (logand entry (1- (ash 1 target::nbits-in-word)))
                        entry)))
  #-(or ppc-target x86-target arm-target) (dbg "Fix entry->addr"))




;;; Bootstrapping. Real version is in l1-aprims.
;;; Called by expansion of with-pstrs

(defun byte-length (string &optional script start end)
    (declare (ignore script))
    (when (or start end)
      (error "Don't support start or end args yet"))
    (if (base-string-p string)
      (length string)
      (error "Don't support non base-string yet.")))




(defun external-entry-point-p (x)
  (istruct-typep x 'external-entry-point))

;;; On both Linux and FreeBSD, RTLD_NEXT and RTLD_DEFAULT behave
;;; the same way wrt symbols defined somewhere other than the lisp
;;; kernel.  On Solaris, RTLD_DEFAULT will return the address of
;;; an imported symbol's procedure linkage table entry if the symbol
;;; has a plt entry (e.g., if it happens to be referenced by the
;;; lisp kernel.)  *RTLD-NEXT* is therefore a slightly better
;;; default; we've traditionaly used *RTLD-DEFAULT*.  
(defvar *rtld-next*)
(defvar *rtld-default*)
(defvar *rtld-use*)
(setq *rtld-next* (%incf-ptr (%null-ptr) -1)
      *rtld-default* (%int-to-ptr #+(or linux-target darwin-target windows-target)  0
				  #-(or linux-target darwin-target windows-target)  -2)
      *rtld-use* #+solaris-target *rtld-next* #-solaris-target *rtld-default*)

#+(or linux-target freebsd-target solaris-target)
(progn

(defvar *dladdr-entry*)
  
;;; I can't think of a reason to change this.
(defvar *dlopen-flags* nil)
(setq *dlopen-flags* (logior #$RTLD_GLOBAL #$RTLD_NOW))
)

(defvar *eeps* nil)

(defvar *fvs* nil)

(defun eeps ()
  (or *eeps*
      (setq *eeps* (make-hash-table :test #'equal))))

(defun fvs ()
  (or *fvs*
      (setq *fvs* (make-hash-table :test #'equal))))

(defun unload-foreign-variables (lib)
  (let* ((fvs (fvs)))
    (when fvs
      (maphash #'(lambda (k fv)
                   (declare (ignore k))
                   (when (fv.addr fv)
                     (when (or (null lib) (eq (fv.container fv) lib))
                       (setf (fv.addr fv) nil)
                       (resolve-foreign-variable fv nil))))
               fvs))))

;;; Walk over all registered entrypoints, invalidating any whose container
;;; is the specified library.  Return true if any such entrypoints were
;;; found.
(defun unload-library-entrypoints (lib)
  (let* ((count 0))
    (declare (fixnum count))
    (maphash #'(lambda (k eep)
		 (declare (ignore k))
                 (when (eep.address eep)
                   (when (or (null lib) (eq (eep.container eep) lib))
                     (setf (eep.address eep) nil)
                     (resolve-eep eep nil)
                     (incf count))))
	     (eeps))
    
    (not (zerop count))))

(defun shared-library-with-name (name)
  (let* ((namelen (length name)))
    (dolist (lib *shared-libraries*)
      (let* ((libname (shlib.soname lib)))
	(when (%simple-string= name libname 0 0 namelen (length libname))
	  (return lib))))))

(defun generate-external-functions (path)
  (let* ((names ()))
    (maphash #'(lambda (k ignore)
		 (declare (ignore ignore))
		 (push k names)) (eeps))
    (with-open-file (stream path
			    :direction :output
			    :if-exists :supersede
			    :if-does-not-exist :create)
      (dolist (k names) (format stream "~&extern void * ~a();" k))
     
      (format stream "~&external_function external_functions[] = {")
      (dolist (k names) (format stream "~&~t{~s,~a}," k k))
      (format stream "~&~t{0,0}~&};"))))

    
(defvar *shared-libraries* nil)

#+(or linux-target freebsd-target solaris-target)
(progn

;; (pref ptr :link_map.l_addr) is an integer on Linux and a Pointer on FreeBSD
;; This macro returns a pointer on all platforms
(defmacro link_map.l_addr (ptr)
  (let* ((record (%find-foreign-record :link_map))
         (field (%find-foreign-record-type-field record :l_addr))
         (offset (/ (foreign-record-field-offset field) 8)))
    `(%get-ptr ,ptr ,offset)))

(defmacro link_map.l_ld (ptr)
  (let* ((record (%find-foreign-record :link_map))
         (field (%find-foreign-record-type-field record :l_ld))
         (offset (/ (foreign-record-field-offset field) 8)))
    `(%get-ptr ,ptr ,offset)))

(defun soname-ptr-from-link-map (map)
  (let* ((path (pref map :link_map.l_name)))
    (if (or (%null-ptr-p path)
            (not (eql (%get-unsigned-byte path 0) (char-code #\/))))
      (let* ((p (malloc 1)))
        (setf (%get-unsigned-byte p 0) 0)
        p)
      (if (eql (%get-unsigned-byte path 0) 0)
        path
        (with-macptrs ((dyn-strings)
                       (dynamic-entries (link_map.l_ld map)))
          (if (%null-ptr-p dynamic-entries)
            (%null-ptr)
            (let* ((soname-offset nil))
              ;; Walk over the entries in the file's dynamic segment; the
              ;; last such entry will have a tag of #$DT_NULL.  Note the
              ;; (loaded,on Linux; relative to link_map.l_addr on FreeBSD)
              ;; address of the dynamic string table and the offset of the
              ;; #$DT_SONAME string in that string table.
              ;; Actually, the above isn't quite right; there seem to
              ;; be cases (involving vDSO) where the address of a library's
              ;; dynamic string table is expressed as an offset relative
              ;; to link_map.l_addr as well.
              (loop
                (case #+32-bit-target (pref dynamic-entries :<E>lf32_<D>yn.d_tag)
                      #+64-bit-target (pref dynamic-entries :<E>lf64_<D>yn.d_tag)
                      (#. #$DT_NULL (return))
                      (#. #$DT_SONAME
                          (setq soname-offset
                                #+32-bit-target (pref dynamic-entries
                                                      :<E>lf32_<D>yn.d_un.d_val)
                                #+64-bit-target (pref dynamic-entries
                                                      :<E>lf64_<D>yn.d_un.d_val)))
                      (#. #$DT_STRTAB
                          (%setf-macptr dyn-strings
                                        ;; Try to guess whether we're dealing
                                        ;; with a displacement or with an
                                        ;; absolute address.  There may be
                                        ;; a better way to determine this,
                                        ;; but for now we assume that absolute
                                        ;; addresses aren't negative and that
                                        ;; displacements are.
                                        (let* ((disp (%get-signed-natural
                                                      dynamic-entries
                                                      target::node-size)))
                                          #+(or freebsd-target solaris-target android-target)
                                          (%inc-ptr (pref map :link_map.l_addr) disp)
                                          #-(or freebsd-target solaris-target android-target)
                                          (let* ((udisp #+32-bit-target (pref dynamic-entries
                                                                              :<E>lf32_<D>yn.d_un.d_val)
                                                        #+64-bit-target (pref dynamic-entries
                                                                              :<E>lf64_<D>yn.d_un.d_val)))
                                            (if (and (> udisp (pref map :link_map.l_addr))
                                                     (< udisp (%ptr-to-int dynamic-entries)))
                                              (%int-to-ptr udisp)
                                              (%int-to-ptr 
                                               (if (< disp 0) 
                                                 (+ disp (pref map :link_map.l_addr))
                                                 disp))))))))
                (%setf-macptr dynamic-entries
                              (%inc-ptr dynamic-entries
                                        #+32-bit-target
                                        (record-length :<E>lf32_<D>yn)
                                        #+64-bit-target
                                        (record-length :<E>lf64_<D>yn))))
              (if (and soname-offset
                       (not (%null-ptr-p dyn-strings)))
                (%inc-ptr dyn-strings soname-offset)
                ;; Use the full pathname of the library.
                (pref map :link_map.l_name)))))))))

(defun shared-library-at (base)
  (dolist (lib *shared-libraries*)
    (when (eql (shlib.base lib) base)
      (return lib))))



(defun shlib-from-map-entry (m)
  (let* ((base (link_map.l_addr m)))
    ;; On relatively modern Linux systems, this is often NULL.
    ;; I'm not sure what (SELinux ?  Pre-binding ?  Something else ?)
    ;; counts as being "relatively modern" in this case.
    ;; The link-map's l_ld field is a pointer to the .so's dynamic
    ;; section, and #_dladdr seems to recognize that as being an
    ;; address within the library and returns a reasonable "base address".
    (when (%null-ptr-p base)
      (let* ((addr (%library-base-containing-address (link_map.l_ld m))))
        (if addr (setq base addr))))
    (unless (%null-ptr-p base)
      (or (let* ((existing-lib (shared-library-at base)))
            (when (and existing-lib (null (shlib.map existing-lib)))
              (setf (shlib.map existing-lib) m
                    (shlib.pathname existing-lib)
                    (%get-cstring (pref m :link_map.l_name))
                    (shlib.base existing-lib) base))
            existing-lib)
          (let* ((soname-ptr (soname-ptr-from-link-map m))
                 (soname (unless (%null-ptr-p soname-ptr) (%get-cstring soname-ptr)))
                 (pathname (%get-cstring (pref m :link_map.l_name)))
                 (shlib (shared-library-with-name soname)))
            (if shlib
              (setf (shlib.map shlib) m
                    (shlib.base shlib) base
                    (shlib.pathname shlib) pathname)
              (push (setq shlib (%cons-shlib soname pathname m base))
                    *shared-libraries*))
            shlib)))))


(defun %get-r-debug ()
  (let* ((addr (ff-call (%kernel-import target::kernel-import-get-r-debug)
			address)))
    (unless (%null-ptr-p addr)
      addr)))

(defun %link-map-address ()
  (let* ((r_debug (%get-r-debug)))
    (if r_debug
      (pref r_debug :r_debug.r_map)
      (let* ((p (or (foreign-symbol-address "_dl_loaded")
		    (foreign-symbol-address "_rtld_global"))))
	(if p
	  (%get-ptr p))))))

(defun %walk-shared-libraries (f)
  (let* ((loaded (%link-map-address)))
    (do* ((map (pref loaded :link_map.l_next) (pref map :link_map.l_next)))
         ((%null-ptr-p map))
      (funcall f map))))


(defun %dlopen-shlib (l)
  (with-cstrs ((n (shlib.soname l)))
    (ff-call (%kernel-import target::kernel-import-GetSharedLibrary)
	     :address n
	     :unsigned-fullword *dlopen-flags*
	     :void)))
  
(defun init-shared-libraries ()
  (setq *dladdr-entry* (foreign-symbol-entry "dladdr"))
  (when (null *shared-libraries*)
    (%walk-shared-libraries #'shlib-from-map-entry)
      ;; On Linux, it seems to be necessary to open each of these
      ;; libraries yet again, specifying the RTLD_GLOBAL flag.
      ;; On FreeBSD, it seems desirable -not- to do that.
    #+linux-target
    (progn
      ;; The "program interpreter" (aka the dynamic linker) is itself
      ;; on *shared-libraries*; it seems to be the thing most recently
      ;; pushed on that list.  Remove it: there's little reason for it
      ;; to be there, and on some platforms (Linux ARM during the
      ;; transition to hard float) the dynamic linker name/pathname
      ;; depend on how the kernel was compiled and linked.  We -don't-
      ;; want to later open the "other" dynamic linker.
      (setq *shared-libraries* (cdr *shared-libraries*)) ; find a better way.
      (dolist (l *shared-libraries*)
        (%dlopen-shlib l)))))

(init-shared-libraries)




                     
                     

(defun open-shared-library-internal (name)
  (let* ((handle (with-cstrs ((name name))
                   (ff-call
                    (%kernel-import target::kernel-import-GetSharedLibrary)
                    :address name
                    :unsigned-fullword *dlopen-flags*
                    :address)))
         (link-map #+(and linux-target (not android-target)) handle
                   #+(or freebsd-target solaris-target)
                   (if (%null-ptr-p handle)
                     handle
                     (rlet ((p :address))
                       (if (eql 0 (ff-call
                                   (foreign-symbol-entry "dlinfo")
                                   :address handle
                                   :int #$RTLD_DI_LINKMAP
                                   :address p
                                   :int))
                         (pref p :address)
                         (%null-ptr))))
                   #+android-target (if (%null-ptr-p handle)
                                      handle
                                      (pref handle :soinfo.linkmap))))
    (if (%null-ptr-p link-map)
      (values nil (dlerror))
      (prog1 (let* ((lib (shlib-from-map-entry link-map)))
	       (incf (shlib.opencount lib))
               (setf (shlib.handle lib) handle)
	       lib)
	(%walk-shared-libraries
	 #'(lambda (map)
             (let* ((addr (link_map.l_addr map)))
               (unless (or (%null-ptr-p addr)
                           (shared-library-at addr))
                 (let* ((new (shlib-from-map-entry map)))
                   (%dlopen-shlib new))))))))))

)


#+darwin-target
(progn

(defun shared-library-with-handle (handle)
  (dolist (lib *shared-libraries*)
    (when (eql (shlib.handle lib) handle)
      (return lib))))









;;; end darwin-target
  )  

#+windows-target
(progn
  (defvar *current-process-handle*)
  (defvar *enum-process-modules-addr*)
  (defvar *get-module-file-name-addr*)
  (defvar *get-module-base-name-addr*)
  (defvar *get-module-handle-ex-addr*)

  (defun nbackslash-to-forward-slash (namestring)
    (dotimes (i (length namestring) namestring)
      (when (eql (schar namestring i) #\\)
        (setf (schar namestring i) #\/))))

  (defun init-windows-ffi ()
    (%revive-macptr *windows-invalid-handle*)
    (setq *current-process-handle* (ff-call (foreign-symbol-entry "GetCurrentProcess") :address)) 
    (setq *enum-process-modules-addr* (foreign-symbol-entry "EnumProcessModules"))   
    (setq *get-module-file-name-addr* (foreign-symbol-entry "GetModuleFileNameA"))
    (setq *get-module-base-name-addr* (foreign-symbol-entry "GetModuleBaseNameA"))
    (setq *get-module-handle-ex-addr* (foreign-symbol-entry "GetModuleHandleExA")))

  (init-windows-ffi)
  
  (defun hmodule-pathname (hmodule)
    (do* ((bufsize 128))
         ()
      (%stack-block ((name bufsize))
        (let* ((needed (ff-call *get-module-file-name-addr*
                                :address hmodule
                                :address name
                                :signed-fullword bufsize
                                :signed-fullword)))
          (if (eql 0 needed)
            (return nil)
            (if (<= bufsize needed)
              (setq bufsize (+ bufsize bufsize))
              (return (nbackslash-to-forward-slash (%str-from-ptr name needed)))))))))

  (defun hmodule-basename (hmodule)
    (do* ((bufsize 64))
         ()
      (%stack-block ((name bufsize))
        (let* ((needed (ff-call *get-module-base-name-addr*
                                :address *current-process-handle*
                                :address hmodule
                                :address name
                                :signed-fullword bufsize
                                :signed-fullword)))
          (if (eql 0 needed)
            (return nil)
            (if (< bufsize needed)
              (setq bufsize needed)
              (return (%str-from-ptr name needed))))))))

  (defun existing-shlib-for-hmodule (hmodule)
    (dolist (shlib *shared-libraries*)
      (when (eql hmodule (shlib.map shlib)) (return shlib))))
      
  
  (defun shared-library-from-hmodule (hmodule)
    (or (existing-shlib-for-hmodule hmodule)
        (let* ((shlib (%cons-shlib (hmodule-basename hmodule)
                                   (hmodule-pathname hmodule)
                                   hmodule
                                   hmodule)))
          (push shlib *shared-libraries*)
          shlib)))

  (defun for-each-loaded-module (f)
    (let* ((have (* 16 (record-length #>HMODULE))))
      (rlet ((pneed #>DWORD))
        (loop
          (%stack-block ((modules have))
            (ff-call *enum-process-modules-addr*
                     :address *current-process-handle*
                     :address modules
                     #>DWORD have
                     :address pneed)
            (let* ((need (pref pneed #>DWORD)))
              (if (> need have)
                (setq have need)
                (return
                  (do* ((i 0 (+ i (record-length #>HMODULE))))
                       ((= i need))
                    (funcall f (%get-ptr modules i)))))))))))

  (defun init-shared-libraries ()
    (for-each-loaded-module #'shared-library-from-hmodule))
  
  (defun shlib-containing-entry (addr &optional name)
    (with-macptrs ((p (%int-to-ptr addr)))
      (shlib-containing-address p name)))

  (defun shlib-containing-address (addr &optional name)
    (declare (ignore name))
    (rlet ((phmodule :address (%null-ptr)))
      (let* ((found (ff-call *get-module-handle-ex-addr*
                             #>DWORD (logior
                                      #$GET_MODULE_HANDLE_EX_FLAG_FROM_ADDRESS
                                      #$GET_MODULE_HANDLE_EX_FLAG_UNCHANGED_REFCOUNT)
                             :address addr
                             :address phmodule
                             #>BOOL)))
        (unless (eql 0 found)
          (let* ((hmodule (pref phmodule :address)))
            (dolist (lib *shared-libraries*)
              (when (eql (shlib.map lib)  hmodule)
                (return lib))))))))


  (defun open-shared-library-internal (name)
    (let* ((hmodule (with-cstrs ((name name))
                      (ff-call
                       (%kernel-import target::kernel-import-GetSharedLibrary)
                       :address name
                       :unsigned-fullword 0
                       :address)))
           (shlib (unless (%null-ptr-p hmodule)
                    (shared-library-from-hmodule hmodule))))
      (if shlib
        (progn
          (incf (shlib.opencount shlib))
          (setf (shlib.handle shlib) hmodule)
          shlib)
        (values nil (%windows-error-string (get-last-windows-error))))))

  (init-shared-libraries)

  (defun revive-shared-libraries ()
    (dolist (lib *shared-libraries*)
      (setf (shlib.map lib) nil
            (shlib.handle lib) nil
            (shlib.pathname lib) nil
            (shlib.base lib) nil)
      (let* ((soname (shlib.soname lib))
             (soname-len (length soname)))
        (block found
          (for-each-loaded-module
           (lambda (m)
             (let* ((module-soname (hmodule-basename m)))
               (when (%simple-string= soname module-soname 0 0 soname-len (length module-soname))
                 (let* ((m (%inc-ptr m 0)))
                   (setf (shlib.base lib) m
                         (shlib.map lib) m
                         (shlib.pathname lib) (hmodule-pathname m)))
                 (return-from found)))))))))

  (defun reopen-user-libraries ()
    (dolist (lib *shared-libraries*)
      (unless (shlib.map lib)
        (let* ((handle (with-cstrs ((name (shlib.soname lib)))
                         (ff-call
                          (%kernel-import target::kernel-import-GetSharedLibrary)
                          :address name
                          :unsigned-fullword 0
                          :address))))
          (unless (%null-ptr-p handle)
            (setf (shlib.handle lib) handle
                  (shlib.base lib) handle
                  (shlib.map lib) handle
                  (shlib.pathname lib) (hmodule-pathname handle)
                  (shlib.opencount lib) 1))))))
           
              

;;; end windows-target
  )  


(defun ensure-open-shlib (c force)
  (if (or (shlib.handle c) (not force))
    *rtld-use*
    (error "Shared library not open: ~s" (shlib.soname c))))

(defun resolve-container (c force)
  (if c
    (ensure-open-shlib c force)
    *rtld-use*
    ))




;;; An "entry" can be fixnum (the low 2 bits are clear) which represents
;;; a (32-bit word)-aligned address.  That convention covers all
;;; function addresses on ppc32 and works for addresses that are
;;; 0 mod 8 on PPC64, but can't work for things that're byte-aligned
;;; (x8664 and other non-RISC platforms.)
;;; For PPC64, we may have to cons up a macptr if people use broken
;;; linkers.  (There are usually cache advantages to aligning ppc
;;; function addresses on at least a 16-byte boundary, but some
;;; linkers don't quite get the concept ...)

(defun foreign-symbol-entry (name &optional (handle *rtld-use*))
  "Try to resolve the address of the foreign symbol name. If successful,
return a fixnum representation of that address, else return NIL."
  (with-cstrs ((n name))
    #+ppc-target
    (with-macptrs (addr)      
      (%setf-macptr addr
		    (ff-call (%kernel-import target::kernel-import-FindSymbol)
			     :address handle
			     :address n
			     :address))
      (unless (%null-ptr-p addr)	; No function can have address 0
	(or (macptr->fixnum addr) (%inc-ptr addr 0))))
    #+(or x8632-target arm-target)
    (let* ((addr (ff-call (%kernel-import target::kernel-import-FindSymbol)
			  :address handle
			  :address n
			  :unsigned-fullword)))
      (unless (eql 0 addr) addr))
    #+x8664-target
    (let* ((addr (ff-call (%kernel-import target::kernel-import-FindSymbol)
                          :address handle
                          :address n
                          :unsigned-doubleword)))
      (unless (eql 0 addr) addr))))

(defvar *statically-linked* nil)

#+(or linux-target freebsd-target solaris-target)
(progn

(defun %library-base-containing-address (address)
  (rletZ ((info :<D>l_info))
    (let* ((status (ff-call *dladdr-entry*
                            :address address
                            :address info :signed-fullword)))
      (declare (integer status))
      (unless (zerop status)
        (pref info :<D>l_info.dli_fbase)))))
  
(defun shlib-containing-address (address &optional name)
  (declare (ignore name))
  (let* ((base (%library-base-containing-address address)))
    (if base
      (shared-library-at base))))


(defun shlib-containing-entry (entry &optional name)
  (unless *statically-linked*
    (with-macptrs (p)
      (entry->addr entry p)
      (shlib-containing-address p name))))
)

#+darwin-target
(progn
(defvar *dyld-image-count*)
(defvar *dyld-get-image-header*)
(defvar *dyld-get-image-name*)
(defvar *nslookup-symbol-in-image*)
(defvar *nsaddress-of-symbol*)
(defvar *nsmodule-for-symbol*)
(defvar *ns-is-symbol-name-defined-in-image*)
(defvar *dladdr-entry* 0)
(defvar *dlopen-entry* 0)
(defvar *dlerror-entry* 0)

(defun setup-lookup-calls ()
  (setq *dladdr-entry* (foreign-symbol-entry "dladdr"))
  (setq *dlopen-entry* (foreign-symbol-entry "dlopen"))
  (setq *dlerror-entry* (foreign-symbol-entry "dlerror")) 
  (setq *dyld-image-count* (foreign-symbol-entry "_dyld_image_count"))
  (setq *dyld-get-image-header* (foreign-symbol-entry "_dyld_get_image_header"))
  (setq *dyld-get-image-name* (foreign-symbol-entry "_dyld_get_image_name"))
)

(setup-lookup-calls)

(defun open-shared-library-internal (name)
  (with-cstrs ((cname name))
    (let* ((handle (ff-call *dlopen-entry*
                            :address cname
                            :int (logior #$RTLD_GLOBAL #$RTLD_NOW)
                            :address)))
      (if (%null-ptr-p handle)
        (values nil (%get-cstring (ff-call *dlerror-entry* :address)))
        (let* ((lib (shared-library-with-handle handle)))
          (unless lib
            (setq lib (%cons-shlib name name nil nil))
            (setf (shlib.handle lib) handle)
	    (push lib *shared-libraries*))
          (incf (shlib.opencount lib))
          (values lib nil))))))

;;;
;;; When restarting from a saved image
;;;
(defun reopen-user-libraries ()
  (dolist (lib *shared-libraries*)
    (setf (shlib.handle lib) nil
	  (shlib.base lib) nil))
  (dolist (lib *shared-libraries*)
    (with-cstrs ((cname (shlib.soname lib)))
      (let* ((handle (ff-call *dlopen-entry*
                              :address cname
                              :int (logior #$RTLD_GLOBAL #$RTLD_NOW)
                              :address)))
        (unless (%null-ptr-p handle)
          (setf (shlib.handle lib) handle))))))

(defun shlib-containing-address (address &optional name)
  (declare (ignore name))
  (%stack-block ((info (record-length #>Dl_info) :clear t))
    (unless (zerop (ff-call *dladdr-entry*
                            :address address
                            :address info
                            :signed-fullword))
      (let* ((addr (pref info #>Dl_info.dli_fbase))
             (name (%get-cstring (pref info #>Dl_info.dli_fname)))
             (namelen (length name)))
        (dolist (lib *shared-libraries*)
          (let* ((shlibname  (shlib.pathname lib))
                 (shlibnamelen (length shlibname)))
          (when (%simple-string= name shlibname 0 0 namelen shlibnamelen)
            (unless (shlib.base lib)
              (setf (shlib.base lib) addr)
              #+no; don't change soname of existing library
              (let* ((soname  (soname-from-mach-header addr)))
                (when soname
                  (setf (shlib.soname lib) soname))))
            (return lib))))))))

(defun shlib-containing-entry (entry &optional name)
  (unless name
    (error "foreign name must be non-NIL."))
  (with-macptrs (addr)
    (entry->addr entry addr)
    (shlib-containing-address addr name)))

(defun soname-from-mach-header (header)
  (do* ((p (%inc-ptr header
                     #+64-bit-target (record-length :mach_header_64)
                     #-64-bit-target (record-length :mach_header))
           (%inc-ptr p (pref p :load_command.cmdsize)))
        (i 0 (1+ i))
        (n (pref header
                 #+64-bit-target :mach_header_64.ncmds
                 #-64-bit-target :mach_header.ncmds)))
       ((= i n))
    (when (= #$LC_ID_DYLIB (pref p :load_command.cmd))
      (return (%get-cstring (%inc-ptr p (record-length :dylib_command)))))))

                 
                     
                                                           
(defun init-shared-libraries ()
  (do* ((count (ff-call *dyld-image-count* :unsigned-fullword))
        (i 1 (1+ i)))
       ((= i count))
    (declare (fixnum i count))
    (let* ((addr (ff-call *dyld-get-image-header* :unsigned-fullword i :address))
           (nameptr (ff-call *dyld-get-image-name* :unsigned-fullword i :address))
           (name (%get-cstring nameptr ))
           (lib (%cons-shlib (or (soname-from-mach-header addr) name) name nil addr)))
      (setf (shlib.handle lib)
            (ff-call *dlopen-entry* :address nameptr :unsigned-fullword (logior #$RTLD_GLOBAL #$RTLD_NOLOAD)))
      (push lib *shared-libraries*))))

(init-shared-libraries)

;; end Darwin progn
)

#-(or linux-target darwin-target freebsd-target solaris-target windows-target)
(defun shlib-containing-entry (entry &optional name)
  (declare (ignore entry name))
  *rtld-default*)


(defun resolve-eep (e &optional (require-resolution t))
  (or (eep.address e)
      (let* ((name (eep.name e))
	     (container (eep.container e))
             (handle (resolve-container container require-resolution))
	     (addr (foreign-symbol-entry name handle)))
	(if addr
	  (progn
	    (unless container
	      (setf (eep.container e) (shlib-containing-entry addr name)))
	    (setf (eep.address e) addr))
	  (if require-resolution
	    (error "Can't resolve foreign symbol ~s" name))))))



(defun foreign-symbol-address (name &optional (map *rtld-use*))
  "Try to resolve the address of the foreign symbol name. If successful,
return that address encapsulated in a MACPTR, else returns NIL."
  (with-cstrs ((n name))
    (let* ((addr (ff-call (%kernel-import target::kernel-import-FindSymbol) :address map :address n :address)))
      (unless (%null-ptr-p addr)
        addr))))

(defun resolve-foreign-variable (fv &optional (require-resolution t))
  (or (fv.addr fv)
      (let* ((name (fv.name fv))
	     (container (fv.container fv))
             (handle (resolve-container container require-resolution))
	     (addr (foreign-symbol-address name handle)))
	(if addr
	  (progn
	    (unless container
	      (setf (fv.container fv) (shlib-containing-address addr name)))
	    (setf (fv.addr fv) addr))
	  (if require-resolution
	    (error "Can't resolve foreign symbol ~s" name))))))

(defun load-eep (name)
  (let* ((eep (or (gethash name (eeps)) (setf (gethash name *eeps*) (%cons-external-entry-point name)))))
    (resolve-eep eep nil)
    eep))

(defun load-fv (name type)
  (let* ((fv (or (gethash name (fvs)) (setf (gethash name *fvs*) (%cons-foreign-variable name type)))))
    (resolve-foreign-variable fv nil)
    fv))

         




#+(or linux-target freebsd-target solaris-target)
(progn

;;; Return the position of the last dot character in name, if that
;;; character is followed by one or more decimal digits (e.g., the
;;; start of a numeric suffix on a library name.)  Return NIL if
;;; there's no such suffix.
(defun last-dot-pos (name)
  (do* ((i (1- (length name)) (1- i))
        (default i)
        (trailing-digits nil))
       ((<= i 0) default)
    (declare (fixnum i))
    (let* ((code (%scharcode name i)))
      (declare (type (mod #x110000) code))
      (if (and (>= code (char-code #\0))
               (<= code (char-code #\9)))
        (setq trailing-digits t)
        (if (= code (char-code #\.))
          (return (if trailing-digits i))
          (return default))))))
  
;;; It's assumed that the set of libraries that the OS has open
;;; (accessible via the _dl_loaded global variable) is a subset of
;;; the libraries on *shared-libraries*.

(defun revive-shared-libraries ()
  (dolist (lib *shared-libraries*)
    (setf (shlib.map lib) nil
	  (shlib.pathname lib) nil
	  (shlib.base lib) nil)
    (let* ((soname (shlib.soname lib))
           (last-dot (if soname (last-dot-pos soname))))
      (when soname
	(with-cstrs ((soname soname))
	  (let* ((map (block found
			(%walk-shared-libraries
			 #'(lambda (m)
			     (with-macptrs (libname)
			       (%setf-macptr libname
					     (soname-ptr-from-link-map m))
			       (unless (%null-ptr-p libname)
				 (when (or (%cstrcmp soname libname)
                                           (and last-dot
                                                (%cnstrcmp soname libname (1+ last-dot))))
				   (return-from found  m)))))))))
	    (when map
	      ;;; Sigh.  We can't reliably lookup symbols in the library
	      ;;; unless we open the library (which is, of course,
	      ;;; already open ...)  ourselves, passing in the
	      ;;; #$RTLD_GLOBAL flag.
              #+linux-target
	      (ff-call (%kernel-import target::kernel-import-GetSharedLibrary)
		       :address soname
		       :unsigned-fullword *dlopen-flags*
		       :void)
	      (setf (shlib.base lib) (link_map.l_addr map)
		    (shlib.pathname lib) (%get-cstring
					  (pref map :link_map.l_name))
                    (shlib.soname lib) (%get-cstring (soname-ptr-from-link-map map))
		    (shlib.map lib) map))))))))

;;; Repeatedly iterate over shared libraries, trying to open those
;;; that weren't already opened by the kernel.  Keep doing this until
;;; we reach stasis (no failures or no successes.)

(defun %reopen-user-libraries ()
  (loop
      (let* ((win nil)
	     (lose nil))
	(dolist (lib *shared-libraries*)
	  (let* ((map (shlib.map lib))
                 (handle (shlib.handle lib)))
	    (unless map
	      (with-cstrs ((soname (shlib.soname lib)))
		(setq handle
                      (ff-call
                       (%kernel-import target::kernel-import-GetSharedLibrary)
                       :address soname
                       :unsigned-fullword *dlopen-flags*
                       :address))
                #-(or freebsd-target solaris-target android-target) (setq map handle)
                #+android-target (setq map
                                       (if (%null-ptr-p handle)
                                         handle
                                         (pref handle :soinfo.linkmap)))
                #+(or freebsd-target solaris-target)
                (setq map
                      (if (%null-ptr-p handle)
                        handle
                        (rlet ((p :address))
                          (if (eql 0 (ff-call
                                      (foreign-symbol-entry "dlinfo")
                                      :address handle
                                      :int #$RTLD_DI_LINKMAP
                                      :address p
                                      :int))
                            (pref p :address)
                            (%null-ptr)))))
		(if (%null-ptr-p map)
		  (setq lose t)
		  (setf (shlib.pathname lib)
			(%get-cstring (pref map :link_map.l_name))
			(shlib.base lib)
			(link_map.l_addr map)
			(shlib.map lib) map
                        (shlib.handle lib) handle
			win t))))))
	(when (or (not lose) (not win)) (return)))))
)


(defun refresh-external-entrypoints ()
  #+linux-target
  (setq *statically-linked* (not (eql 0 (%get-kernel-global 'statically-linked))))
  (%revive-macptr *rtld-next*)
  (%revive-macptr *rtld-default*)
  #+(or linux-target freebsd-target solaris-target)
  (unless *statically-linked*
    (setq *dladdr-entry* (foreign-symbol-entry "dladdr"))
    (revive-shared-libraries)
    (%reopen-user-libraries))
  #+darwin-target
  (progn
    (setup-lookup-calls)
    (reopen-user-libraries))
  #+windows-target
  (progn
    (init-windows-ffi)
    (revive-shared-libraries)
    (reopen-user-libraries))
  (when *eeps*
    (without-interrupts 
     (maphash #'(lambda (k v) 
                  (declare (ignore k)) 
                  (setf (eep.address v) nil) 
                  (resolve-eep v nil))
              *eeps*)))
  (when *fvs*
    (without-interrupts
     (maphash #'(lambda (k v)
                  (declare (ignore k))
                  (setf (fv.addr v) nil)
                  (resolve-foreign-variable v nil))
              *fvs*))))

(defun open-shared-library (name &optional (process #+darwin-target :initial
                                                    #-darwin-target :current))
  "If the library denoted by name can be loaded by the operating system,
return an object of type SHLIB that describes the library; if the library
is already open, increment a reference count. If the library can't be
loaded, signal a SIMPLE-ERROR which contains an often-cryptic message from
the operating system."
    (multiple-value-bind (lib error-string)
        (if (or (eq process :current)
                (eq process *current-process*)
                (and (eq process :initial)
                     (eq *current-process* *initial-process*)))
          (open-shared-library-internal name)
          
          (call-in-process (lambda ()
                             (handler-case (open-shared-library-internal  name)
                               (error (condition) (values nil (format nil "~a" condition)))))
                                                                     
                             
                           (if (eq process :initial)
                             *initial-process*
                             process)))
      (or lib
          (error "Error opening shared library ~a : ~a." name error-string))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-cfm-support.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-int.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")
)


(defun lsh (fixnum count)
  (require-type fixnum 'fixnum)
  (require-type count 'fixnum)
  (if (> count 0) 
    (%ilsl count fixnum)
    (%ilsr (- count) fixnum)))

; this called with fixnum
(defun %iabs  (n)
  (declare (fixnum n))
  (if (minusp  n) (- n) n))

; called with any integer - is there a cmu version of integer/bignum-abs?
(defun %integer-abs (n)
  (number-case n
    (fixnum
     (locally
	 (declare (fixnum n))
       (if (minusp n) (- n) n)))
    (bignum
     (if (minusp n) (- n) n))))


(eval-when (:compile-toplevel :execute)
  (assert (< (char-code #\9) (char-code #\A) (char-code #\a))))

(defun token2int (string start len radix)
  ; simple minded in case you hadn't noticed
  (let* ((n start)
         (end (+ start len))
         (char0 (schar string n))
         (val 0)
         minus)
    (declare (fixnum n end start len radix)) ; as if it mattered
    (when (or (eq char0 #\+)(eq char0 #\-))
      (setq n (1+ n))
      (if (eq char0 #\-)(setq minus t)))
    (while (< n end)
      (let ((code (%scharcode string n)))
        (if (<= code (char-code #\9)) 
          (setq code (- code (char-code #\0)))
          (progn
            (when (>= code (char-code #\a))
              (setq code (- code (- (char-code #\a) (char-code #\A)))))
            (setq code (- code (- (char-code #\A) 10)))))
        (setq val (+ (* val radix) code))
        (setq n (1+ n))))
    (if minus (- val) val)))
  

(defun %integer-to-string (int &optional (radix 10))
  (%pr-integer int radix nil t))


;;; it may be hard to believe, but this is much faster than the lap
;;; version (3 or 4X) for fixnums that is (stream-write-string vs
;;; stream-tyo ???)

(defun %pr-integer (int &optional (radix 10) (stream *standard-output*) return-it  negate-it)
  (declare (fixnum radix)) ; assume caller has checked
  (if stream 
    (if (eq stream t) (setq stream *terminal-io*))
    (setq stream *standard-output*))
  (let ((digit-string "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"))    
    (cond ((fixnump int)  ; ugh                      
           (let ((temstring (make-string (- target::nbits-in-word target::fixnumshift) :element-type 'base-char))
                 (i (- target::nbits-in-word  target::fixnumshift 1))
                 (neg (< int 0))
                 (rem 0))
             (declare (fixnum i rem))
             (declare (dynamic-extent temstring))
             (when neg (setq int (- int)))
             (when (not (fixnump int))
               (return-from %pr-integer (%pr-integer int radix stream return-it t)))
             (locally (declare (fixnum int))  
               (loop
                 (multiple-value-setq  (int rem) (%fixnum-truncate int radix))                 
                 (setf (%schar temstring i)(%schar digit-string rem))
                 (when (eq 0 int)
                   (return))
                 (setq i (1- i)))
               (when neg 
                 (setf (%schar temstring (setq i (1- i))) #\-))
               (if return-it
                 (%substr temstring i (- target::nbits-in-word
                                         target::fixnumshift))
                 (write-string temstring stream :start i :end (- target::nbits-in-word target::fixnumshift))))))          
          (t (let* ((size-vect #(nil nil 32 21 16 14 13 12 11
                                 11   10 10  9  9  9  9  8  8
                                 8     8  8  8  8  8  7  7  7
                                 7     7  7  7  7  7  7  7  7 7))
                    ;; overestimate # digits by a little for weird
                    ;; radix
                    (bigwords (uvsize int))
                    (strlen (1+ (* bigwords (svref size-vect radix))))
                    (temstring (make-string strlen :element-type 'base-char))
                    (i (1- strlen))
                    (neg (< int 0))
                    ; ;(rem 0)
                    ;; ;better-bignum-print?
                    )  ; warn
               (declare (dynamic-extent temstring)
                        (fixnum i strlen))
               (flet ((do-it (newbig)
                        (print-bignum-2 newbig radix temstring digit-string)))
                 (declare (dynamic-extent #'do-it))
                 (setq i (with-one-negated-bignum-buffer int do-it)))                            
               (when (or neg negate-it) 
                 (setf (%schar temstring (setq i (1- i))) #\-))
               (if return-it
                 (%substr temstring i strlen)
                 (write-string temstring stream :start i :end strlen)))))))



;;; *BASE-POWER* holds the number that we keep dividing into the bignum for
;;; each *print-base*.  We want this number as close to *most-positive-fixnum*
;;; as possible, i.e. (floor (log most-positive-fixnum *print-base*)).
;;; 
(defparameter *base-power* ())

;;; *FIXNUM-POWER--1* holds the number of digits for each *print-base* that
;;; fit in the corresponding *base-power*.
;;; 
(defparameter *fixnum-power--1* ())

(do* ((b (make-array 37 :initial-element nil))
      (f (make-array 37 :initial-element nil))
      (base 2 (1+ base)))
     ((= base 37) (setq *base-power* b *fixnum-power--1* f))
  (do ((power-1 -1 (1+ power-1))
       (new-divisor base (* new-divisor base))
       (divisor 1 new-divisor))
      ((not (fixnump new-divisor))
       (setf (aref b base) divisor)
       (setf (aref f base) power-1))))


(defun print-bignum-2 (big radix string digit-string)
  (declare (optimize (speed 3) (safety 0))
           (simple-base-string string digit-string))
  (let* ((divisor (aref *base-power* radix))
         (power (aref *fixnum-power--1* radix))
         (index (1- (length string)))
         (rem 0))
    (declare (fixnum index divisor power))
    ;;(print index)
    (loop
      (multiple-value-setq (big rem) (truncate big divisor))
      (let* ((int rem)
             (rem 0)
             (final-index (- index power 1)))
        (loop
          (multiple-value-setq (int rem) (%fixnum-truncate int radix))
          (setf (schar string index)(schar digit-string rem))
          (when (eql 0 int)
            (return index))
          (setq index (1- index)))
        (if (zerop big)
          (return index)
          (dotimes (i (- index final-index) index)
            (declare (fixnum i))
            (setq index (1- index))
            (setf (schar string index) #\0)))))))

#+x8664-target
(defun %bignum-hex-digits (b string)
  (let* ((size (uvsize b))
	 (temp-string (make-string 8))
	 (end 0))
    (declare (type fixnum size end))
    (declare (dynamic-extent temp-string))
    (locally (declare (optimize (speed 3) (safety 0)))
      (loop for i of-type fixnum from (the fixnum (1- size)) downto 0
	    for start2 of-type fixnum by 32
	    do (%ub-fixnum-hex-digits 7 (bignum-ref b i) temp-string)
	    (setq end (%i+ end 8))
	    (%copy-ivector-to-ivector temp-string 0 string start2 32)))
    (values string end)))

(defun write-unsigned-byte-hex-digits (u stream)
  (setq u (require-type u '(unsigned-byte)))
  #-x8664-target
  (write u :stream stream :base 16)
  #+x8664-target
  (if (fixnump u)
    (let* ((scratch (make-string 15)))
      (declare (dynamic-extent scratch))
      (%ub-fixnum-hex-digits 14 u scratch)
      (let ((start (dotimes (i 15 nil)
		     (declare (fixnum i)
			      (optimize (speed 3) (safety 0)))
		     (unless (char= #\0 (schar scratch i))
		       (return i)))))
	(if start
	  (write-string scratch stream :start start)
	  (write-string "0" stream))))
    (let* ((scratch (make-string (the fixnum (ash (the fixnum (uvsize u)) 3))))
	   (start 0))
      (declare (dynamic-extent scratch)
	       (fixnum start))
      (multiple-value-bind (string end)
	  (%bignum-hex-digits u scratch)
	;; skip leading zeros (there will be a non-zero digit)
	(let ((i 0))
	  (declare (type (unsigned-byte 56) i))
	  (loop
	    (if (char= #\0 (schar string i))
	      (setq start (%i+ start 1))
	      (return))
	    (setq i (%i+ i 1))))
	(write-string string stream :start start :end end))))
  nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-int.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-bignum32.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")


#+32-bit-target                         ; the whole shebang
(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")
  
  (defconstant digit-size 32)
  (defconstant half-digit-size (/ digit-size 2))
  
  (defconstant maximum-bignum-length (1- (ash 1 24)))

  (deftype bignum-index () `(integer 0 (,maximum-bignum-length)))
  (deftype bignum-element-type () `(unsigned-byte ,digit-size))
  (deftype bignum-half-element-type () `(unsigned-byte ,half-digit-size))
  (deftype bignum-type () 'bignum)
  (defmacro %bignum-digits (bignum)`(uvsize ,bignum))

  (defmacro digit-bind ((&rest digits) form &body body)
    `(multiple-value-bind ,digits
                          ,form
       (declare (type bignum-half-element-type ,@digits))
       ,@body))

  (defmacro digit-set ((&rest digits) form)
    `(multiple-value-setq ,digits
                          ,form))

  (defmacro digit-zerop (h l)
    `(and (zerop ,h) (zerop ,l)))
 


  ;;;; BIGNUM-REPLACE and WITH-BIGNUM-BUFFERS.

  ;;; BIGNUM-REPLACE -- Internal.
  ;;;
  (defmacro bignum-replace (dest src &key (start1 '0) end1 (start2 '0) end2
                                 from-end)
    (once-only ((n-dest dest)
		 (n-src src))
      (if (and (eq start1 0)(eq start2 0)(null end1)(null end2)(null from-end))
        ; this is all true for some uses today <<
        `(%copy-ivector-to-ivector ,n-src 0 ,n-dest 0 (%ilsl 2 (min (the fixnum (%bignum-length ,n-src))
                                                                    (the fixnum (%bignum-length ,n-dest)))))
        (let* ((n-start1 (gensym))
               (n-end1 (gensym))
               (n-start2 (gensym))
               (n-end2 (gensym)))
          `(let ((,n-start1 ,start1)
                 (,n-start2 ,start2)
                 (,n-end1 ,(or end1 `(%bignum-length ,n-dest)))
                 (,n-end2 ,(or end2 `(%bignum-length ,n-src))))
             ,(if (null from-end)            
                `(%copy-ivector-to-ivector
                  ,n-src (%i* 4 ,n-start2) 
                  ,n-dest (%i* 4 ,n-start1)
                  (%i* 4 (min (%i- ,n-end2 ,n-start2) 
                              (%i- ,n-end1 ,n-start1))))
                `(let ((nwds (min (%i- ,n-end2 ,n-start2)
                                  (%i- ,n-end1 ,n-start1))))
                   (%copy-ivector-to-ivector
                    ,n-src (%ilsl 2 (%i- ,n-end2 nwds))
                    ,n-dest (%ilsl 2 (%i- ,n-end1 nwds))
                    (%i* 4 nwds))))))))) 
  

  ;;;; Shifting.
  
  (defconstant all-ones-half-digit #xFFFF)  
  

  

  
  (defmacro %logxor (h1 l1 h2 l2)
    (once-only ((h1v h1)(l1v l1)(h2v h2)(l2v l2))
      `(values (%ilogxor ,h1v ,h2v)(%ilogxor ,l1v ,l2v))))
  
  
  (defmacro %lognot (h l)
    (once-only ((h1v h)(l1v l))
      `(values (%ilognot ,h1v)(%ilognot ,l1v))))

  (defmacro %allocate-bignum (ndigits)
    `(%alloc-misc ,ndigits target::subtag-bignum))

  (defmacro %normalize-bignum-macro (big)
    `(%normalize-bignum-2 t ,big))

  (defmacro %mostly-normalize-bignum-macro (big)
    `(%normalize-bignum-2 nil ,big))


;;; %ALLOCATE-BIGNUM must zero all elements.
;;;
  (declaim (inline  %bignum-length))

;;; Temp space needed to (Karatsuba)-square N-digit argument
  (defmacro mpn-kara-mul-n-tsize (n)
    `(* 8 (+ ,n 32)))
;;; Need the same amount of space to do Karatsuba multiply.
  (defmacro mpn-kara-sqr-n-tsize (n)
    `(mpn-kara-mul-n-tsize ,n))
  
)




#+32-bit-target
(progn
;;; Extract the length of the bignum.
;;; 
(defun %bignum-length (bignum)
  (uvsize bignum)) 





;;;; Addition.
(defun add-bignums (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b)))
    (declare (bignum-index len-a len-b))
    (when (> len-b len-a)
      (rotatef a b)
      (rotatef len-a len-b))
    (let* ((len-res (1+ len-a))
	   (res (%allocate-bignum len-res))
	   (carry 0)
	   (sign-b (%bignum-sign b)))
	(dotimes (i len-b)
	  (setq carry (%add-with-carry res i carry a i b i)))
	(if (/= len-a len-b)
	  (finish-bignum-add  res carry a sign-b len-b len-a)
	  (%add-with-carry res len-a carry (%bignum-sign a) nil sign-b nil))
	(%normalize-bignum-macro res))))

;;; Could do better than this, surely.
(defun add-bignum-and-fixnum (bignum fixnum)
  (with-small-bignum-buffers ((bigfix fixnum))
    (add-bignums bignum bigfix)))



;;; B was shorter than A; keep adding B's sign digit to each remaining
;;; digit of A, propagating the carry.
(defun finish-bignum-add (result carry a sign-b start end)
  (declare (type bignum-index start end))
  (do* ((i start (1+ i)))
       ((= i end)
	(%add-with-carry result end carry (%bignum-sign a) nil sign-b nil))
    (setq carry (%add-with-carry result i carry a i sign-b nil))))






;;;; Subtraction.
(defun subtract-bignum (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (len-res (1+ (max len-a len-b)))
	 (res (%allocate-bignum len-res)))
    (declare (bignum-index len-a len-b len-res))
    (bignum-subtract-loop a len-a b len-b res)
    (%normalize-bignum-macro res)))

(defun bignum-subtract-loop (a len-a b len-b res)
  (declare (bignum-index len-a len-b ))
  (let* ((len-res (%bignum-length res)))
    (declare (bignum-index len-res))
    (let* ((borrow 1)
	   (sign-a (%bignum-sign a))
	   (sign-b (%bignum-sign b)))
      (dotimes (i (the bignum-index (min len-a len-b)))
	(setq borrow (%subtract-with-borrow res i borrow a i b i)))
      (if (< len-a len-b)
	(do* ((i len-a (1+ i)))
	     ((= i len-b)
	      (if (< i len-res)
		(%subtract-with-borrow res i borrow sign-a nil sign-b nil)))
	  (setq borrow (%subtract-with-borrow res i borrow sign-a nil b i)))
	(do* ((i len-b (1+ i)))
	     ((= i len-a)
	      (if (< i len-res)
		(%subtract-with-borrow res i borrow sign-a nil sign-b nil)))
	  (setq borrow (%subtract-with-borrow res i borrow a i sign-b nil)))))))


;;;; Multiplication.

;;; These parameters match GMP's.
(defvar *sqr-basecase-threshold* 5)
(defvar *sqr-karatsuba-threshold* 22)
(defvar *mul-karatsuba-threshold* 10)

;;; Squaring is often simpler than multiplication.  This should never
;;; be called with (>= N *sqr-karatsuba-threshold*).
(defun mpn-sqr-basecase (prodp up n)
  (declare (fixnum prodp up n))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (umulppm up up prodp)
  (when (> n 1)
    (%stack-block ((tarr (* 4 (* 2 *sqr-karatsuba-threshold*))))
      (let* ((tp (macptr->fixnum tarr)))
	(mpn-mul-1 tp
		   (the fixnum (1+ up))
		   (the fixnum (1- n))
		   up
		   (the fixnum (+ tp (the fixnum (1- n)))))
	(do* ((i 2 (1+ i)))
	     ((= i n))
	  (declare (fixnum i))
	  (mpn-addmul-1 (the fixnum (- (the fixnum (+ tp (the fixnum (+ i i))))
				       2))
			(the fixnum (+ up i))
			(the fixnum (- n i))
			(the fixnum (+ up (the fixnum (1- i))))
			(the fixnum (+ tp (the fixnum (+ n (the fixnum (- i 2))))))))
	(do* ((i 1 (1+ i))
	      (ul (1+ up) (1+ ul)))
	     ((= i n))
	  (declare (fixnum i ul))
	  (umulppm ul ul (the fixnum (+ prodp (the fixnum (+ i i))))))
	(let* ((2n-2 (- (the fixnum (+ n n)) 2))
	       (carry (mpn-lshift-1 tp tp 2n-2)))
	  (declare (fixnum 2n-2 carry))
	  (setq carry
                (+ carry
                   (the fixnum (mpn-add-n (the fixnum (1+ prodp))
                                          (the fixnum (1+ prodp))
                                          tp
                                          2n-2))))
	  (add-fixnum-to-limb carry (the fixnum (+ prodp
						   (the fixnum (1-
								(the fixnum
								  (+ n n))))))))))))

;;; For large enough values of N, squaring via Karatsuba-style
;;; divide&conquer is faster than in the base case.
(defun mpn-kara-sqr-n (p a n ws)
  (declare (fixnum p a n ws))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%stack-block ((limbs 16))
    (let* ((w (macptr->fixnum limbs))
	   (w0 (1+ w))
	   (w1 (1+ w0))
	   (xx (1+ w1))
	   (n2 (ash n -1))
	   (x 0)
	   (y 0)
	   (i 0))
      (declare (fixnum w w0 w1 xx n2 x y i))
      (cond ((logbitp 0 n)
	     ;; Odd length
	     (let* ((n3 (- n n2))
		    (n1 0)
		    (nm1 0))
	       (declare (fixnum n3 n1 nm1))
	       (copy-limb (the fixnum (+ a n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum
		    (- (the fixnum (mpn-sub-n p a (the fixnum (+ a n3)) n2))))
		  w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ a i)) w0)
		     (copy-limb (the fixnum (+ a (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (= i 0))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ a n3)
			   y a)
		     (setq y (+ a n3)
			   x a))
		   (mpn-sub-n p x y n2)))
	       (copy-limb w (the fixnum (+ p n2)))
	       (setq n1 (1+ n))
	       (cond ((< n3 *sqr-basecase-threshold*)
		      (mpn-mul-basecase ws p n3 p n3)
		      (mpn-mul-basecase p a n3 a n3))
		     ((< n3 *sqr-karatsuba-threshold*)
		      (mpn-sqr-basecase ws p n3)
		      (mpn-sqr-basecase p a n3))
		     (t
		      (mpn-kara-sqr-n ws p n3 (the fixnum (+ ws n1)))
		      (mpn-kara-sqr-n p  a n3 (the fixnum (+ ws n1)))))
	       (cond ((< n2 *sqr-basecase-threshold*)
		      (mpn-mul-basecase (the fixnum (+ p n1))
					(the fixnum (+ a n3))
					n2
					(the fixnum (+ a n3))
					n2))
		     ((< n2 *sqr-karatsuba-threshold*)
		      (mpn-sqr-basecase (the fixnum (+ p n1))
					(the fixnum (+ a n3))
					n2))
		     (t
		      (mpn-kara-sqr-n (the fixnum (+ p n1))
				      (the fixnum (+ a n3))
				      n2
				      (the fixnum (+ ws n1)))))
	       (mpn-sub-n ws p ws n1)
	       (setq nm1 (1- n))
	       (unless (zerop (the fixnum
				(mpn-add-n ws
					   (the fixnum (+ p n1))
					   ws
					   nm1)))
		 (copy-limb (the fixnum (+ ws nm1)) xx)
		 (add-fixnum-to-limb 1 xx)
		 (copy-limb xx (the fixnum (+ ws nm1)))
		 (if (limb-zerop xx)
		   (add-fixnum-to-limb 1 (the fixnum (+ ws n)))))
	       (unless (zerop
			(the fixnum
			  (mpn-add-n (the fixnum (+ p n3))
				     (the fixnum (+ p n3))
				     ws
				     n1)))
		 (mpn-incr-u (the fixnum (+ p (the fixnum (+ n1 n3))))
			     1))))
	    (t ; N is even
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ a i)) w0)
	       (copy-limb (the fixnum (+ a (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (= i 0))
		 (return)))
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ a n2)
		     y a)
	       (setq y (+ a n2)
		     x a))
	     (mpn-sub-n p x y n2)
	     (cond ((< n2 *sqr-basecase-threshold*)
		    (mpn-mul-basecase ws p n2 p n2)
		    (mpn-mul-basecase p a n2 a n2)
		    (mpn-mul-basecase (the fixnum (+ p n))
				      (the fixnum (+ a n2))
				      n2
				      (the fixnum (+ a n2))
				      n2))
		   ((< n2 *sqr-karatsuba-threshold*)
		    (mpn-sqr-basecase ws p n2)
		    (mpn-sqr-basecase p a n2)
		    (mpn-sqr-basecase (the fixnum (+ p n))
				      (the fixnum (+ a n2))
				      n2))
		   (t
		    (mpn-kara-sqr-n ws p n2 (the fixnum (+ ws n)))
		    (mpn-kara-sqr-n p  a n2 (the fixnum (+ ws n)))
		    (mpn-kara-sqr-n (the fixnum (+ p n))
				    (the fixnum (+ a n2))
				    n2
				    (the fixnum (+ ws n)))))
	     (let* ((ww (- (the fixnum (mpn-sub-n ws p ws n)))))
	       (declare (fixnum ww))
               (setq ww (+ ww (mpn-add-n ws (the fixnum (+ p n)) ws n)))
	       (setq ww (+ ww (mpn-add-n (the fixnum (+ p n2))
                                         (the fixnum (+ p n2))
                                         ws
                                         n)))
	       (mpn-incr-u (the fixnum (+ p (the fixnum (+ n2 n)))) ww)))))))

;;; Karatsuba subroutine: multiply A and B, store result at P, use WS
;;; as scrach space.  Treats A and B as if they were both of size N;
;;; if that's not true, caller must fuss around the edges.
(defun mpn-kara-mul-n (p a b n ws)
  (declare (fixnum p a b n ws))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%stack-block ((limbs 16))
    (let* ((w (macptr->fixnum limbs))
	   (w0 (1+ w))
	   (w1 (1+ w0))
	   (xx (1+ w1))
	   (x 0)
	   (y 0)
	   (i 0)
	   (n2 (ash n -1))
	   (sign 0))
      (declare (fixnum w w0 w1 xx x y i n2 sign))
      (cond ((logbitp 0 n)
	     (let* ((n1 0)
		    (n3 (- n n2))
		    (nm1 0))
	       (declare (fixnum n1 n3 nm1))
	       (copy-limb (the fixnum (+ a n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum (- (mpn-sub-n p a (the fixnum (+ a n3)) n2))) w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ a i)) w0)
		     (copy-limb (the fixnum (+ a (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (zerop i))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ a n3)
			   y a
			   sign -1)
		     (setq x a
			   y (+ a n3)))
		   (mpn-sub-n p x y n2)))
	       (copy-limb w (the fixnum (+ p n2)))
	       (copy-limb (the fixnum (+ b n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum (- (the fixnum (mpn-sub-n (the fixnum (+ p n3))
							b
							(the fixnum (+ b n3))
							n2))))
		  w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ b i)) w0)
		     (copy-limb (the fixnum (+ b (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (zerop i))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ b n3)
			   y b
			   sign (lognot sign))
		     (setq x b
			   y (+ b n3)))
		   (mpn-sub-n (the fixnum (+ p n3)) x y n2)))
	       (copy-limb w (the fixnum (+ p n)))
	       (setq n1 (1+ n))
	       (cond
		 ((< n2 *mul-karatsuba-threshold*)
		  (cond
		    ((< n3 *mul-karatsuba-threshold*)
		     (mpn-mul-basecase ws p n3 (the fixnum (+ p n3)) n3)
		     (mpn-mul-basecase p a n3 b n3))
		    (t
		     (mpn-kara-mul-n ws p (the fixnum (+ p n3)) n3 (the fixnum (+ ws n1)))
		     (mpn-kara-mul-n p a b n3 (the fixnum (+ ws n1)))))
		  (mpn-mul-basecase (the fixnum (+ p n1))
				    (the fixnum (+ a n3))
				    n2
				    (the fixnum (+ b n3))
				    n2))
		 (t
		  (mpn-kara-mul-n ws p (the fixnum (+ p n3)) n3 (the fixnum (+ ws n1)))
		  (mpn-kara-mul-n p a b n3 (the fixnum (+ ws n1)))
		  (mpn-kara-mul-n (the fixnum (+ p n1))
				  (the fixnum (+ a n3))
				  (the fixnum (+ b n3))
				  n2
				  (the fixnum (+ ws n1)))))
	       (if (not (zerop sign))
		 (mpn-add-n ws p ws n1)
		 (mpn-sub-n ws p ws n1))
	       (setq nm1 (1- n))
	       (unless (zerop (the fixnum (mpn-add-n ws
						     (the fixnum (+ p n1))
						     ws
						     nm1)))
		 (copy-limb (the fixnum (+ ws nm1)) xx)
		 (add-fixnum-to-limb 1 xx)
		 (copy-limb xx (the fixnum (+ ws nm1)))
		 (if (limb-zerop xx)
		   (add-fixnum-to-limb 1 (the fixnum (+ ws n)))))
	       (unless (zerop (the fixnum
				(mpn-add-n (the fixnum (+ p n3))
					   (the fixnum (+ p n3))
					   ws
					   n1)))
		 (mpn-incr-u (the fixnum
			       (+ p (the fixnum (+ n1 n3)))) 1))))
	    (t				; even length
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ a i)) w0)
	       (copy-limb (the fixnum (+ a (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (zerop i))
		 (return)))
	     (setq sign 0)
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ a n2)
		     y a
		     sign -1)
	       (setq x a
		     y (+ a n2)))
	     (mpn-sub-n p x y n2)
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ b i)) w0)
	       (copy-limb (the fixnum (+ b (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (zerop i))
		 (return)))	      
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ b n2)
		     y b
		     sign (lognot sign))
	       (setq x b
		     y (+ b n2)))
	     (mpn-sub-n (the fixnum (+ p n2)) x y n2)
	     (cond
	       ((< n2 *mul-karatsuba-threshold*)
		(mpn-mul-basecase ws p n2 (the fixnum (+ p n2)) n2)
		(mpn-mul-basecase p a n2 b n2)
		(mpn-mul-basecase (the fixnum (+ p n))
				  (the fixnum (+ a n2))
				  n2
				  (the fixnum (+ b n2))
				  n2))
	       (t
		(mpn-kara-mul-n ws p (the fixnum (+ p n2)) n2
				(the fixnum (+ ws n)))
		(mpn-kara-mul-n p a b n2 (the fixnum (+ ws n)))
		(mpn-kara-mul-n (the fixnum (+ p n))
				(the fixnum (+ a n2))
				(the fixnum (+ b n2))
				n2
				(the fixnum (+ ws n)))))
	     (let* ((ww (if (not (zerop sign))
			  (mpn-add-n ws p ws n)
			  (- (the fixnum (mpn-sub-n ws p ws n))))))
	       (declare (fixnum ww))
	       (setq ww (+ ww (mpn-add-n ws (the fixnum (+ p n)) ws n)))
	       (setq ww (+ ww (mpn-add-n (the fixnum (+ p n2))
                                         (the fixnum (+ p n2))
                                         ws
                                         n)))
	       (mpn-incr-u (the fixnum (+ p (the fixnum (+ n2 n)))) ww)))))))

;;; Square UP, of length UN.  I wonder if a Karatsuba multiply might be
;;; faster than a basecase square.
(defun mpn-sqr-n (prodp up un)
  (declare (fixnum prodp up un))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (< un *sqr-basecase-threshold*)
    (mpn-mul-basecase prodp up un up un)
    (if (< un *sqr-karatsuba-threshold*)
      (mpn-sqr-basecase prodp up un)
      (%stack-block ((wsptr (mpn-kara-sqr-n-tsize un)))
	(mpn-kara-sqr-n prodp up un (macptr->fixnum wsptr))))))

;;; Subroutine: store AxB at P.  Assumes A & B to be of length N
(defun mpn-mul-n (p a b n)
  (declare (fixnum p a b n))
  (declare (optimize (speed 3) (safety 0) (space 0)))  
  (if (< n *mul-karatsuba-threshold*)
    (mpn-mul-basecase p a n b n)
    (%stack-block ((wsptr (mpn-kara-mul-n-tsize n)))
      (mpn-kara-mul-n p a b n (macptr->fixnum wsptr)))))


;;; Multiply [UP,UN] by [VP,VN].  UN must not be less than VN.
;;; This does Karatsuba if operands are big enough; if they are
;;; and they differ in size, this computes the product of the
;;; smaller-size slices, then fixes up the resut.
(defun mpn-mul (prodp up un vp vn)
  (declare (fixnum prodp up un vp vn))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  ;(assert (>= un vn 1))
  (if (and (= up vp) (= un vn))
    (mpn-sqr-n prodp up un)
    (if (< vn *mul-karatsuba-threshold*)
      (mpn-mul-basecase prodp up un vp vn)
      (let* ((l vn))
	(declare (fixnum l))
	(mpn-mul-n prodp up vp vn)
	(unless (= un vn)
	  (incf prodp vn)
	  (incf up vn)
	  (decf un vn)
	  (if (< un vn)
	    (psetq un vn vn un up vp vp up))
	  (%stack-block ((wsptr
			  (the fixnum
			    (+ 8
			       (the fixnum
				 (* 4
				    (the fixnum
				      (+ vn
					 (if (>= vn *mul-karatsuba-threshold*)
					   vn
					   un)))))))))
	    (setf (%get-unsigned-long wsptr 0) 0
		  (%get-unsigned-long wsptr 4) 0)
	    (let* ((tt (macptr->fixnum wsptr))
		   (c (1+ tt))
		   (ws (1+ c)))
	      (declare (fixnum tt c ws ))
	      (do* ()
		   ((< vn *mul-karatsuba-threshold*))
		(mpn-mul-n ws up vp vn)
		(cond ((<= l (the fixnum (+ vn vn)))
		       (add-fixnum-to-limb (mpn-add-n prodp prodp ws l) tt)
		       (unless (= l (the fixnum (+ vn vn)))
			 (copy-fixnum-to-limb
			  (mpn-add-1 (the fixnum (+ prodp l))
				     (the fixnum (+ ws l))
				     (the fixnum (- (the fixnum (+ vn vn)) l))
				     tt)
			  tt)
			 (setq l (the fixnum (+ vn vn)))))
		      (t
		       (copy-fixnum-to-limb
			(mpn-add-n prodp prodp ws (the fixnum (+ vn vn))) c)
		       (add-fixnum-to-limb
			(mpn-add-1 (the fixnum (+ prodp (the fixnum (+ vn vn))))
				   (the fixnum (+ prodp (the fixnum (+ vn vn))))
				   (the fixnum (- l (the fixnum (+ vn vn))))
				   c)
			tt)))
		(incf prodp vn)
		(decf l vn)
		(incf up vn)
		(decf un vn)
		(if (< un vn)
		  (psetq up vp vp up un vn vn un)))
	      (unless (zerop vn)
		(mpn-mul-basecase ws up un vp vn)
		(cond ((<= l (the fixnum (+ un vn)))
		       (add-fixnum-to-limb
			(mpn-add-n prodp prodp ws l)
			tt)
		       (unless (= l (the fixnum (+ un vn)))
			 (copy-fixnum-to-limb
			  (mpn-add-1 (the fixnum (+ prodp l))
				     (the fixnum (+ ws l))
				     (the fixnum (- (the fixnum (+ un vn)) l))
				     tt)
			  tt)))
		      (t
		       (copy-fixnum-to-limb
			(mpn-add-n prodp prodp ws (the fixnum (+ un vn)))
			c)
		       (add-fixnum-to-limb
			(mpn-add-1
			 (the fixnum (+ prodp (the fixnum (+ un vn))))
			 (the fixnum (+ prodp (the fixnum (+ un vn))))
			 (the fixnum (- (the fixnum (- l un)) vn))
			 c)
			tt)))))))))))

(defun multiply-bignums (a b)
  (let* ((signs-differ (not (eq (bignum-minusp a) (bignum-minusp b)))))
    (flet ((multiply-unsigned-bignums (a b)
	     (let* ((len-a (%bignum-length a))
		    (len-b (%bignum-length b))
		    (len-res (+ len-a len-b))
		    (res (%allocate-bignum len-res)) )
	       (declare (bignum-index len-a len-b len-res))
	       (if (and (>= len-a 16)
			(>= len-b 16)
			#+(or x8632-target arm-target)
			nil)
		 (let* ((ubytes (* len-a 4))
			(vbytes (* len-b 4))
			(rbytes (* len-res 4)))
		   (declare (fixnum ubytes vbytes rbytes))
		   (%stack-block ((uptr ubytes)
				  (vptr vbytes)
				  (rptr rbytes))
		     (let* ((up (macptr->fixnum uptr))
			    (vp (macptr->fixnum vptr))
			    (rp (macptr->fixnum rptr)))
		       (declare (fixnum up vp rp))
		       (%copy-ivector-to-ptr a 0 uptr 0 ubytes)
		       (if (eq a b)	; maybe try eql ..
			 (mpn-mul rp up len-a up len-a)
			 (progn
			   (%copy-ivector-to-ptr b 0 vptr 0 vbytes)
			   (if (< len-a len-b)
			     (mpn-mul rp vp len-b up len-a)
			     (mpn-mul rp up len-a vp len-b)))))
		     (%copy-ptr-to-ivector rptr 0 res 0 rbytes)))
		 (dotimes (i len-a)
		   (declare (type bignum-index i))
		   (%multiply-and-add-harder-loop-2 a b res i len-b)))
		 res)))
      (let* ((res (with-negated-bignum-buffers a b multiply-unsigned-bignums)))
	(if signs-differ (negate-bignum-in-place res))
	(%normalize-bignum-macro res)))))


(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum-type bignum) (fixnum fixnum))
  (let* ((bignum-len (%bignum-length bignum))
         (bignum-plus-p (bignum-plusp bignum))
	 (fixnum-plus-p (not (minusp fixnum)))
         (negate-res (neq bignum-plus-p fixnum-plus-p)))
    (declare (type bignum-type bignum)
	     (type bignum-index bignum-len))
    (flet ((do-it (bignum fixnum  negate-res)
             (let* ((bignum-len (%bignum-length bignum))
                    (result (%allocate-bignum (the fixnum (1+ bignum-len)))))
               (declare (type bignum-type bignum)
	                (type bignum-index bignum-len))
               (%multiply-and-add-fixnum-loop bignum-len bignum fixnum result)
               (when negate-res
                 (negate-bignum-in-place result))
               (%normalize-bignum-macro result ))))
      (declare (dynamic-extent #'do-it))
      (if bignum-plus-p
        (do-it bignum (if fixnum-plus-p fixnum (- fixnum))  negate-res)
        (with-bignum-buffers ((b1 (the fixnum (1+ bignum-len))))
          (negate-bignum bignum nil b1)
          (do-it b1 (if fixnum-plus-p fixnum (- fixnum))  negate-res))))))

;; assume we already know result won't fit in a fixnum
;; only caller is fixnum-*-2
;;

(defun multiply-fixnums (a b)
  (declare (fixnum a b))
  (* a b))


;;;; GCD.


;;; Both args are > 0.
(defun bignum-fixnum-gcd (bignum fixnum)
  (let* ((rem (bignum-truncate-by-fixnum-no-quo bignum fixnum)))
    (declare (fixnum rem))
    (if (zerop rem)
      fixnum
      (%fixnum-gcd rem fixnum))))



;;; NEGATE-BIGNUM -- Public.
;;;
;;; Fully-normalize is an internal optional.  It cause this to always return
;;; a bignum, without any extraneous digits, and it never returns a fixnum.
;;;
(defun negate-bignum (x &optional (fully-normalize t) res)
  (declare (type bignum-type x))
  (let* ((len-x (%bignum-length x))
	 (len-res (1+ len-x))
         (minusp (bignum-minusp x)))
    (declare (type bignum-index len-x len-res))
    (if (not res) (setq res (%allocate-bignum len-res))) ;Test len-res for range?
    (let ((carry (bignum-negate-loop-really x len-x res)))  ; i think carry is always 0
      (if (eq carry 0)
        (if minusp (%bignum-set res len-x 0 0)(%bignum-set res len-x #xffff #xffff))
        (digit-bind (h l)
                    (if minusp 
                      (%add-the-carry 0 0 carry)
                      (%add-the-carry #xffff #xffff carry))
                    
          (%bignum-set res len-x h l))))
    (if fully-normalize
      (%normalize-bignum-macro res)
      (%mostly-normalize-bignum-macro res))))

;;; NEGATE-BIGNUM-IN-PLACE -- Internal.
;;;
;;; This assumes bignum is positive; that is, the result of negating it will
;;; stay in the provided allocated bignum.
;;;
(defun negate-bignum-in-place (bignum)
  (bignum-negate-loop-really bignum (%bignum-length bignum) bignum)
  bignum)


  

(defun copy-bignum (bignum)
  (let ((res (%allocate-bignum (%bignum-length bignum))))
    (bignum-replace res bignum)
    res))



;;; BIGNUM-ASHIFT-RIGHT -- Public.
;;;
;;; First compute the number of whole digits to shift, shifting them by
;;; skipping them when we start to pick up bits, and the number of bits to
;;; shift the remaining digits into place.  If the number of digits is greater
;;; than the length of the bignum, then the result is either 0 or -1.  If we
;;; shift on a digit boundary (that is, n-bits is zero), then we just copy
;;; digits.  The last branch handles the general case which uses a macro that a
;;; couple other routines use.  The fifth argument to the macro references
;;; locals established by the macro.
;;;


(defun bignum-ashift-right (bignum x)
  (declare (type bignum-type bignum)
           (fixnum x))
  (let ((bignum-len (%bignum-length bignum)))
    (declare (type bignum-index bignum-len))
    (multiple-value-bind (digits n-bits) (truncate x digit-size)
      (declare (type bignum-index digits)(fixnum n-bits))
      (cond
       ((>= digits bignum-len)
        (if (bignum-plusp bignum) 0 -1))
       ((eql 0 n-bits)
        (bignum-ashift-right-digits bignum digits))
       (t
        (let* ((res-len (- bignum-len digits))
               (res (%allocate-bignum res-len))
               (len-1 (1- res-len)))
          (declare (fixnum res-len len-1))
          (bignum-shift-right-loop-1 n-bits res bignum len-1 digits)          
          (%normalize-bignum-macro res )))))))

			       



;;; BIGNUM-ASHIFT-RIGHT-DIGITS -- Internal.
;;;
(defun bignum-ashift-right-digits (bignum digits)
  (declare (type bignum-type bignum)
	   (type bignum-index digits))
  (let* ((res-len (- (%bignum-length bignum) digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len)
	     (type bignum-type res))
    (bignum-replace res bignum :start2 digits)
    (%normalize-bignum-macro res)))


;;; BIGNUM-BUFFER-ASHIFT-RIGHT -- Internal.
;;;
;;; GCD uses this for an in-place shifting operation.  This is different enough
;;; from BIGNUM-ASHIFT-RIGHT that it isn't worth folding the bodies into a
;;; macro, but they share the basic algorithm.  This routine foregoes a first
;;; test for digits being greater than or equal to bignum-len since that will
;;; never happen for its uses in GCD.  We did fold the last branch into a macro
;;; since it was duplicated a few times, and the fifth argument to it
;;; references locals established by the macro.
;;;
#|
(defun bignum-buffer-ashift-right (bignum bignum-len x)
  (declare (type bignum-index bignum-len) (fixnum x))
  (multiple-value-bind (digits n-bits)
		       (truncate x digit-size)
    (declare (type bignum-index digits))
    (cond
     ((zerop n-bits)
      (let ((new-end (- bignum-len digits)))
	(bignum-replace bignum bignum :end1 new-end :start2 digits
			:end2 bignum-len)
	(%normalize-bignum-buffer bignum new-end)))
     (t
      (shift-right-unaligned bignum digits n-bits (- bignum-len digits)
			     ((= j res-len-1)
                              (digit-bind (h l) (%bignum-ref bignum i)
                                (digit-set (h l) (%ashr h l n-bits))
			        (%bignum-set bignum j h l))
			      (%normalize-bignum-buffer bignum res-len)))))))
|#
#|
(defun bignum-buffer-ashift-right (bignum bignum-len x)
  (declare (type bignum-index bignum-len) (fixnum x))
  (multiple-value-bind (digits n-bits) (truncate x digit-size)
    (declare (type bignum-index digits)(fixnum n-bits))
    (macrolet ((clear-high-digits ()
                 `(do* ((i (1- (the fixnum (%bignum-length bignum))) (1- i))
                        (j digits (1- j)))
                       ((= 0 j))
                    (declare (fixnum i j))
                    (%bignum-set bignum i 0 0))))
      (cond
       ((zerop n-bits)
        (let* ((new-end (- bignum-len digits)))
          (declare (fixnum new-end))
          (bignum-replace bignum bignum :end1 new-end :start2 digits
                          :end2 bignum-len)
          (clear-high-digits)
          (%normalize-bignum-buffer bignum new-end)))
       (t
        (let* ((res-len (- bignum-len digits))
               (len-1 (1- res-len)))
          (declare (fixnum res-len len-1))
          (bignum-shift-right-loop-1 n-bits bignum bignum len-1 digits)
          ; clear the old high order digits - assume always positive
          ; (when (neq 0 digits)(push digits poof))
          (clear-high-digits)
          (%normalize-bignum-buffer bignum res-len)))))))
|#

 

;;; BIGNUM-ASHIFT-LEFT -- Public.
;;;
;;; This handles shifting a bignum buffer to provide fresh bignum data for some
;;; internal routines.  We know bignum is safe when called with bignum-len.
;;; First we compute the number of whole digits to shift, shifting them
;;; starting to store farther along the result bignum.  If we shift on a digit
;;; boundary (that is, n-bits is zero), then we just copy digits.  The last
;;; branch handles the general case.
;;;
(defun bignum-ashift-left (bignum x &optional bignum-len)
  (declare (type bignum-type bignum)
	   (fixnum x)
	   (type (or null bignum-index) bignum-len))
  (multiple-value-bind (digits n-bits)
		       (truncate x digit-size)
    (declare (fixnum digits n-bits))
    (let* ((bignum-len (or bignum-len (%bignum-length bignum)))
	   (res-len (+ digits bignum-len 1)))
      (declare (fixnum bignum-len res-len))
      (when (> res-len maximum-bignum-length)
	(error "Can't represent result of left shift."))
      (if (zerop n-bits)
        (bignum-ashift-left-digits bignum bignum-len digits)
        (bignum-ashift-left-unaligned bignum digits n-bits res-len)))))

;;; BIGNUM-ASHIFT-LEFT-DIGITS -- Internal.
;;;
(defun bignum-ashift-left-digits (bignum bignum-len digits)
  (declare (type bignum-index bignum-len digits))
  (let* ((res-len (+ bignum-len digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len))
    (bignum-replace res bignum :start1 digits :end1 res-len :end2 bignum-len
		    :from-end t)
    res))

;;; BIGNUM-ASHIFT-LEFT-UNALIGNED -- Internal.
;;;
;;; BIGNUM-TRUNCATE uses this to store into a bignum buffer by supplying res.
;;; When res comes in non-nil, then this foregoes allocating a result, and it
;;; normalizes the buffer instead of the would-be allocated result.
;;;
;;; We start storing into one digit higher than digits, storing a whole result
;;; digit from parts of two contiguous digits from bignum.  When the loop
;;; finishes, we store the remaining bits from bignum's first digit in the
;;; first non-zero result digit, digits.  We also grab some left over high
;;; bits from the last digit of bignum.
;;;

(defun bignum-ashift-left-unaligned (bignum digits n-bits res-len
                                              &optional (res nil resp))
  (declare (type bignum-index digits res-len)
           (type (mod #.digit-size) n-bits))
  (let* (;(remaining-bits (- digit-size n-bits))
         (res-len-1 (1- res-len))
         (res (or res (%allocate-bignum res-len))))
    (declare (type bignum-index res-len res-len-1))
    (bignum-shift-left-loop n-bits res bignum res-len-1 (the fixnum (1+ digits)))
    ; if resp provided we don't care about returned value
    (if (not resp) (%normalize-bignum-macro res))))




;;;; Relational operators.



;;; BIGNUM-COMPARE -- Public.
;;;
;;; This compares two bignums returning -1, 0, or 1, depending on whether a
;;; is less than, equal to, or greater than b.
;;;
;(proclaim '(function bignum-compare (bignum bignum) (integer -1 1)))
(defun bignum-compare (a b)
  (declare (type bignum-type a b))
  (let* ((a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (if (eq a-plusp b-plusp)
      (let* ((len-a (%bignum-length a))
	     (len-b (%bignum-length b)))
	(declare (type bignum-index len-a len-b))
	(cond ((= len-a len-b)
	       (do* ((i (1- len-a) (1- i)))
		    ((zerop i) (%compare-digits a b 0))
		 (declare (fixnum i))
		 (let* ((signum (%compare-digits a b i)))
		   (declare (fixnum signum))
		   (unless (zerop signum)
		     (return signum)))))
	      ((> len-a len-b)
	       (if a-plusp 1 -1))
	      (t (if a-plusp -1 1))))
      (if a-plusp 1 -1))))






;;;; Integer length and logcount


(defun bignum-integer-length (big)
  (the fixnum (- (the fixnum (ash (the fixnum (%bignum-length big)) 5))
		 (the fixnum (%bignum-sign-bits big)))))

; (not (zerop (logand integer1 integer2)

(defun bignum-logtest (num1 num2)
  (let* ((length1 (%bignum-length num1))
         (length2 (%bignum-length num2))
         (n1-minusp (bignum-minusp num1))
         (n2-minusp (bignum-minusp num2)))
    (declare (fixnum length1 length2))
    (if (and n1-minusp n2-minusp) ; both neg, get out quick
      T        
      (let ((val (bignum-logtest-loop (min length1 length2) num1 num2)))
                 #|(do* ((index 0 (1+ index)))
	              ((= index (min length1 length2)) nil)
                   ; maybe better to start from high end of shorter?
                   (multiple-value-bind (hi1 lo1)(%bignum-ref num1 index)
                     (multiple-value-bind (hi2 lo2)(%bignum-ref num2 index)
                       (when (or (not (zerop (%ilogand hi1 hi2)))
                                 (not (zerop (%ilogand lo1 lo2))))
                         (return t)))))))|#
        (or val
            (when (not (eql length1 length2)) ; lengths same => value nil
              (if (< length1 length2)
                n1-minusp
                n2-minusp)))))))



(defun logtest-fix-big (fix big)
  (declare (fixnum fix))
  (if (eql 0 (the fixnum fix))
    nil
    (if (> (the fixnum fix) 0) 
      (let ()
        (multiple-value-bind (hi lo)(%bignum-ref big 0)
          (declare (fixnum hi lo))
          (or (not (zerop (logand fix lo)))
              (not (zerop (logand (ash fix (- 16)) hi))))))
      t)))


(defun bignum-logcount (bignum)
  (declare (type bignum-type bignum))
  (let* ((length (%bignum-length bignum))
	 (plusp (bignum-plusp bignum))
	 (result 0))
    (declare (type bignum-index length)
	     (fixnum result))
    (if plusp
      (dotimes (index length result)
	(incf result (the fixnum (%logcount bignum index))))
      (dotimes (index length result)
	(incf result (the fixnum (%logcount-complement bignum index)))))))


;;;; Logical operations.

;;; NOT.
;;;

;;; BIGNUM-LOGICAL-NOT -- Public.
;;;
(defun bignum-logical-not (a)
  (declare (type bignum-type a))
  (let* ((len (%bignum-length a))
	 (res (%allocate-bignum len)))
    (declare (type bignum-index len))
    (dotimes (i len res)
      (%bignum-lognot i a res))))




;;; AND.
;;;

;;; BIGNUM-LOGICAL-AND -- Public.
;;;
(defun bignum-logical-and (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
      ((< len-a len-b)
       (if a-plusp
	 (logand-shorter-positive a len-a b (%allocate-bignum len-a))
	 (logand-shorter-negative a len-a b len-b (%allocate-bignum len-b))))
      ((< len-b len-a)
       (if b-plusp
	 (logand-shorter-positive b len-b a (%allocate-bignum len-b))
	 (logand-shorter-negative b len-b a len-a (%allocate-bignum len-a))))
      (t (logand-shorter-positive a len-a b (%allocate-bignum len-a))))))

;;; LOGAND-SHORTER-POSITIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is positive.  Because this
;;; is AND, we don't care about any bits longer than a's since its infinite 0
;;; sign bits will mask the other bits out of b.  The result is len-a big.
;;;
(defun logand-shorter-positive (a len-a b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a))
  (dotimes (i len-a)
    (%bignum-logand i a b res))
  (%normalize-bignum-macro res))

;;; LOGAND-SHORTER-NEGATIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is negative.  Because this
;;; is AND, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b.  The result is len-b big.
;;;
(defun logand-shorter-negative (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logand i a b res))
  (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b)
  (%normalize-bignum-macro res))



;;;
;;;
;;; bignum-logandc2

(defun bignum-logandc2 (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
     ((< len-a len-b)
      (logandc2-shorter-any a len-a b len-b (if a-plusp (%allocate-bignum len-a) (%allocate-bignum len-b))))
     ((< len-b len-a) ; b shorter 
      (logandc1-shorter-any b len-b a len-a (if b-plusp (%allocate-bignum len-a)(%allocate-bignum len-b))))
     (t (logandc2-shorter-any a len-a b len-b (%allocate-bignum len-a))))))

(defun logandc2-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logandc2 i a b res))
  (if (bignum-minusp a)
    (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
        (digit-bind (h l) (%bignum-ref b i)
          (%bignum-set res i (%ilognot h) (%ilognot l)))))
  (%normalize-bignum-macro res))



(defun logandc1-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logandc1 i a b res))
  (if (bignum-plusp a)
    (if (neq len-a len-b)
      (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b)))
  (%normalize-bignum-macro res))



(defun fix-big-logand (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logand fix big res)))
      (if res
        (progn
          (bignum-replace res big :start1 1 :start2 1 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))
  

(defun fix-big-logandc2 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc2 fix big res)))
      (if res
        (progn
          (do ((i 1 (1+ i)))
              ((= i len-b))
            (declare (type bignum-index i))
            (digit-lognot-move i big res))
          (%normalize-bignum-macro res))
        val))))

(defun fix-big-logandc1 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (>= fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc1 fix big res)))
      (if res
        (progn  
          (bignum-replace res big :start1 1 :start2 1 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))







;;; IOR.
;;;

;;; BIGNUM-LOGICAL-IOR -- Public.
;;;
(defun bignum-logical-ior (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
     ((< len-a len-b)
      (if a-plusp
	  (logior-shorter-positive a len-a b len-b (%allocate-bignum len-b))
	  (logior-shorter-negative a len-a b len-b (%allocate-bignum len-b))))
     ((< len-b len-a)
      (if b-plusp
	  (logior-shorter-positive b len-b a len-a (%allocate-bignum len-a))
	  (logior-shorter-negative b len-b a len-a (%allocate-bignum len-a))))
     (t (logior-shorter-positive a len-a b len-b (%allocate-bignum len-a))))))

;;; LOGIOR-SHORTER-POSITIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is positive.  Because this
;;; is IOR, we don't care about any bits longer than a's since its infinite
;;; 0 sign bits will mask the other bits out of b out to len-b.  The result
;;; is len-b long.
;;;
(defun logior-shorter-positive (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logior i a b res))
  (if (not (eql len-a len-b))
    (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b))
  (%normalize-bignum-macro res))

;;; LOGIOR-SHORTER-NEGATIVE -- Internal.
;;;
;;; This takes a shorter bignum, a and len-a, that is negative.  Because this
;;; is IOR, we just copy any bits longer than a's since its infinite 1 sign
;;; bits will include any bits from b.  The result is len-b long.
;;;
(defun logior-shorter-negative (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logior i a b res))
  ; silly to propagate sign and then normalize it away
  ; but may need to do at least once - but we are only normalizing from len-a?
  ; ah but the sign needs to be correct
  (do ((i len-a (1+ i)))
      ((= i len-b))
    (declare (type bignum-index i))
    (%bignum-set res i #xffff #xffff))
  (%normalize-bignum-macro res))




;;; XOR.
;;;

;;; BIGNUM-LOGICAL-XOR -- Public.
;;;
(defun bignum-logical-xor (a b)
  (declare (type bignum-type a b))
  (let ((len-a (%bignum-length a))
	(len-b (%bignum-length b)))
    (declare (type bignum-index len-a len-b))
    (if (< len-a len-b)
	(bignum-logical-xor-aux a len-a b len-b (%allocate-bignum len-b))
	(bignum-logical-xor-aux b len-b a len-a (%allocate-bignum len-a)))))

;;; BIGNUM-LOGICAL-XOR-AUX -- Internal.
;;;
;;; This takes the the shorter of two bignums in a and len-a.  Res is len-b
;;; long.  Do the XOR.
;;;
(defun bignum-logical-xor-aux (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (%bignum-logxor i a b res))
  (unless (= len-a len-b)
    (let ((sign (if (bignum-minusp a) #xffff 0)))
      (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
        (digit-bind (h l) (%bignum-ref b i)
          (%bignum-set res i (%ilogxor sign h)(%ilogxor sign l))))))
  (%normalize-bignum-macro res))





;;;; LDB (load byte)

; [slh] 'twas all commented out - thank gawd


;;;; TRUNCATE.

;;; This is the original sketch of the algorithm from which I implemented this
;;; TRUNCATE, assuming both operands are bignums.  I should modify this to work
;;; with the documentation on my functions, as a general introduction.  I've
;;; left this here just in case someone needs it in the future.  Don't look
;;; at this unless reading the functions' comments leaves you at a loss.
;;; Remember this comes from Knuth, so the book might give you the right general
;;; overview.
;;; 
;;;
;;; (truncate x y):
;;;
;;; If X's magnitude is less than Y's, then result is 0 with remainder X.
;;;
;;; Make x and y positive, copying x if it is already positive.
;;;
;;; Shift y left until there's a 1 in the 30'th bit (most significant, non-sign
;;;       digit)
;;;    Just do most sig digit to determine how much to shift whole number.
;;; Shift x this much too.
;;; Remember this initial shift count.
;;;
;;; Allocate q to be len-x minus len-y quantity plus 1.
;;;
;;; i = last digit of x.
;;; k = last digit of q.
;;;
;;; LOOP
;;;
;;; j = last digit of y.
;;;
;;; compute guess.
;;; if x[i] = y[j] then g = #xFFFFFFFF
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) -32) #xFFFFFFFF).
;;;    c = (logand (* g y[j-1]) #xFFFFFFFF).
;;; if a < b, okay.
;;; if a > b, guess is too high
;;;    g = g - 1; go back to "check guess".
;;; if a = b and c > x[i-2], guess is too high
;;;    g = g - 1; go back to "check guess".
;;; GUESS IS 32-BIT NUMBER, SO USE THING TO KEEP IN SPECIAL REGISTER
;;; SAME FOR A, B, AND C.
;;;
;;; Subtract g * y from x[i - len-y+1]..x[i].  See paper for doing this in step.
;;; If x[i] < 0, guess is fucked.
;;;    negative g, then add 1
;;;    zero or positive g, then subtract 1
;;; AND add y back into x[len-y+1..i].
;;;
;;; q[k] = g.
;;; i = i - 1.
;;; k = k - 1.
;;;
;;; If k>=0, goto LOOP.
;;;
;;;
;;; Now quotient is good, but remainder is not.
;;; Shift x right by saved initial left shifting count.
;;;
;;; Check quotient and remainder signs.
;;; x pos y pos --> q pos r pos
;;; x pos y neg --> q neg r pos
;;; x neg y pos --> q neg r neg
;;; x neg y neg --> q pos r neg
;;;
;;; Normalize quotient and remainder.  Cons result if necessary.
;;;




;;; BIGNUM-TRUNCATE -- Public.
;;;
;;; This divides x by y returning the quotient and remainder.  In the general
;;; case, we shift y to setup for the algorithm, and we use two buffers to save
;;; consing intermediate values.  X gets destructively modified to become the
;;; remainder, and we have to shift it to account for the initial Y shift.
;;; After we multiple bind q and r, we first fix up the signs and then return
;;; the normalized results.
;;;


(defun bignum-truncate (x1 y1 &optional no-rem)
  (declare (type bignum-type x1 y1))
  (let* ((x-plusp (bignum-plusp x1))
	 (y-plusp (bignum-plusp y1)))
    (flet 
      ((do-it (x y) 
         (let* ((len-x (%bignum-length x))
                (len-y (%bignum-length y)))
           (declare (fixnum len-x len-y))
           
           (let ((c (bignum-compare y x)))
             (cond 
              ((eql c 1)  ; >
               (return-from bignum-truncate (values 0 x1)))
              ((eql c 0)(values 1 0))  ; =  might as well since did compare anyway
              ((< len-y 2)
               (multiple-value-bind (q r)
                                    (bignum-truncate-single-digit x len-x y no-rem)
                 (values q
                         (unless no-rem
                           (cond (x-plusp r)
                                 ((typep r 'fixnum) (the fixnum (- (the fixnum r))))
                                 (t (negate-bignum-in-place r)
                                    (%normalize-bignum-macro r )))))))
              (t
               (let* ((len-x+1 (1+ len-x)))
                 (declare (fixnum len-x+1))
                 (with-bignum-buffers ((truncate-x len-x+1)
                                       (truncate-y (the fixnum (1+ len-y))))
                   (let ((y-shift (shift-y-for-truncate y)))
                     (shift-and-store-truncate-buffers truncate-x truncate-y x len-x y len-y y-shift)
                     (values (do-truncate truncate-x truncate-y len-x+1 len-y)
                             ;; DO-TRUNCATE must execute first.
                             (when (not no-rem)                               
                               (when (not (eql 0 y-shift))                                  
                                 (let* ((res-len-1 (1- len-y)))
                                   (declare (fixnum res-len-1))
                                   (bignum-shift-right-loop-1 y-shift truncate-x truncate-x res-len-1 0)))                                
                               (let ((the-res (%normalize-bignum-macro truncate-x )))
                                 (if (not (fixnump the-res))
                                   (if x-plusp (copy-bignum the-res) (negate-bignum the-res))
                                   (if x-plusp the-res (the fixnum (- (the fixnum the-res)))))
                                     ))))))))))))
      (multiple-value-bind (q r)(with-negated-bignum-buffers x1 y1 do-it)
        (let ((quotient (cond ((eq x-plusp y-plusp) q)
                              ((typep q 'fixnum) (the fixnum (- (the fixnum q))))
                              (t (negate-bignum-in-place q)
                                 (%normalize-bignum-macro q )))))
          (if no-rem
            quotient            
            (values quotient r)))))))

(defun bignum-rem (x1 y1)
  (declare (type bignum-type x1 y1))  
  (let* ((x-plusp (bignum-plusp x1)))
    (flet 
      ((do-it (x y) 
         (let* ((len-x (%bignum-length x))
                (len-y (%bignum-length y)))
           (declare (fixnum len-x len-y))           
           (let ((c (bignum-compare y x)))
             (cond 
              ((eql c 1) (return-from bignum-rem x1))
              ((eql c 0) 0)  ; =  might as well since did compare anyway
              ((< len-y 2)
               (let ((r (bignum-truncate-single-digit-no-quo x len-x y)))  ; phooey 
                 (cond (x-plusp r)
                       ((typep r 'fixnum) (the fixnum (- (the fixnum r))))
                       (t (negate-bignum-in-place r)
                          (%normalize-bignum-macro r )))))
              (t
               (let* ((len-x+1 (1+ len-x)))
                 (declare (fixnum len-x+1))
                 (with-bignum-buffers ((truncate-x len-x+1)
                                       (truncate-y (the fixnum (1+ len-y))))
                   (let ((y-shift (shift-y-for-truncate y)))
                     (shift-and-store-truncate-buffers truncate-x truncate-y x len-x y len-y y-shift)
                     (do-truncate-no-quo truncate-x truncate-y len-x+1 len-y)
                     (when (not (eql 0 y-shift))                                 
                       (let* ((res-len-1 (1- len-y)))
                         (declare (fixnum res-len-1))
                         (bignum-shift-right-loop-1 y-shift truncate-x truncate-x res-len-1 0)))
                     (let ((the-res (%normalize-bignum-macro truncate-x)))
                       (if (not (fixnump the-res))
                         (if x-plusp (copy-bignum the-res) (negate-bignum the-res))
                         (if x-plusp the-res (the fixnum (- (the fixnum the-res)))))))))))))))
      (declare (dynamic-extent #'do-it))
      (with-negated-bignum-buffers x1 y1 do-it))))



;;; BIGNUM-TRUNCATE-SINGLE-DIGIT -- Internal.
;;;
;;; This divides x by y when y is a single bignum digit.  BIGNUM-TRUNCATE fixes
;;; up the quotient and remainder with respect to sign and normalization.
;;;
;;; We don't have to worry about shifting y to make its most significant digit
;;; sufficiently large for %FLOOR to return 32-bit quantities for the q-digit
;;; and r-digit.  If y is a single digit bignum, it is already large enough
;;; for %FLOOR.  That is, it has some bits on pretty high in the digit.
;;;
;;; x is positive
(defun bignum-truncate-single-digit (x len-x y &optional no-rem)
  (declare (type bignum-index len-x))
  (let* ((maybe-q (%allocate-bignum 2))
         (q (if (<= len-x 2) maybe-q (%allocate-bignum len-x)))
	 (r-h 0)
         (r-l 0))
    (declare (dynamic-extent maybe-q))
    (digit-bind (y-h y-l) (%bignum-ref y 0)
      (multiple-value-setq (r-h r-l)(%floor-loop-quo x q y-h y-l))      
      (if (eq q maybe-q)
        (progn 
          (setq q (%normalize-bignum-macro q))
          (if (not (fixnump q)) (setq q (copy-bignum q))))
        (setq q (%normalize-bignum-macro q )))
      ;; might as well make a fixnum if possible
      (if no-rem
        q
        (if (> (%digits-sign-bits r-h r-l)  target::fixnumshift)
          (values q (%ilogior (%ilsl 16 r-h) r-l))
          (let ((rem (%allocate-bignum 1)))
            (%bignum-set rem 0 r-h r-l)
            (values q rem)))))))

;;; aka rem
(defun bignum-truncate-single-digit-no-quo (x len-x y)
  (declare (type bignum-index len-x))
  (declare (ignore len-x))
  (let (;(q (%allocate-bignum len-x))
	(r-h 0)
        (r-l 0))
    (progn
      (digit-bind (y-h y-l) (%bignum-ref y 0)
        (multiple-value-setq (r-h r-l)(%floor-loop-no-quo x y-h y-l))
        ; might as well make a fixnum if possible
        (if (> (%digits-sign-bits r-h r-l)  target::fixnumshift)
          (%ilogior (%ilsl 16 r-h) r-l)
          (let ((rem (%allocate-bignum 1)))
            (%bignum-set rem 0 r-h r-l)
            rem))))))

;; so big deal - we save a one digit bignum for y 
;; and bigger deal if x is negative - we copy or negate x, computing result destructively
;;  - thus avoiding making a negated x in addition to result
;; 
(defun bignum-truncate-by-fixnum (x y)
  (declare (fixnum y))
  (when (eql y 0)(error (make-condition 'division-by-zero :operation 'truncate :operands (list x y))))
  (let* ((len-x (%bignum-length x))
         (x-minus (bignum-minusp x))
         (maybe-q (%allocate-bignum 3))
         (q (if x-minus
              (if (<= len-x 2)
                (dotimes (i 3 (negate-bignum-in-place maybe-q))
                  (if (< i len-x)
                    (multiple-value-bind (hi lo) (%bignum-ref x i)
                      (%bignum-set maybe-q i hi lo))
                    (%bignum-set maybe-q i 65535 65535)))
                (negate-bignum x))
              (if (<= len-x 2) ; this was broken if negative because bignum-replace just copies min len-a len-b digits
                (progn
                  (bignum-replace maybe-q x)                
                  maybe-q)
                (%allocate-bignum len-x))))      ;  q is new big or -x
         ;(len-q (%bignum-length q))
         (y-minus (minusp y))         
         (y (if y-minus (- y) y)))
    (declare (fixnum y))
    (declare (type bignum-index len-x))
    (declare (dynamic-extent maybe-q))
    (let* ((r-h 0)
           (r-l 0)
           (y-h (%ilogand #xffff (%iasr 16 y)))
           (y-l (%ilogand #xffff y)))
      (multiple-value-setq (r-h r-l)(%floor-loop-quo (if x-minus q x) q y-h y-l))      
      (let* ((r (%ilogior (%ilsl 16 r-h) r-l)))
        (declare (fixnum r))
        (when (neq x-minus y-minus)(negate-bignum-in-place q))
        (setq q (%normalize-bignum-macro q ))
        (values (if (eq q maybe-q) (copy-bignum q) q)
                (if x-minus (the fixnum (- r)) r))))))

(defun bignum-truncate-by-fixnum-no-quo (x y)
  (declare (fixnum y))
  (when (eql y 0)(error (make-condition 'division-by-zero :operation 'truncate :operands (list x Y))))
  (let* ((len-x (%bignum-length x))
         (x-minus (bignum-minusp x))
         (y-minus (minusp y))         
         (y (if y-minus (- y) y)))
    (declare (fixnum y))
    (declare (type bignum-index len-x))
      (let* (;(LEN-Q (%BIGNUM-LENGTH Q))
             (r-h 0)
             (r-l 0)
             (y-h (%ilogand #xffff (%iasr 16 y)))
             (y-l (%ilogand #xffff y)))
        (if x-minus
          (with-bignum-buffers ((q (the fixnum (1+ len-x))))
            (negate-bignum x nil q)
            (multiple-value-setq (r-h r-l)(%floor-loop-no-quo q y-h y-l)))
          (multiple-value-setq (r-h r-l)(%floor-loop-no-quo x y-h y-l)))        
        (let* ((r (%ilogior (%ilsl 16 r-h) r-l)))
          (declare (fixnum r))
          (if x-minus (the fixnum (- r)) r)))))


;;; DO-TRUNCATE -- Internal.
;;;
;;; This divides *truncate-x* by *truncate-y*, and len-x and len-y tell us how
;;; much of the buffers we care about.  TRY-BIGNUM-TRUNCATE-GUESS modifies
;;; *truncate-x* on each interation, and this buffer becomes our remainder.
;;;
;;; *truncate-x* definitely has at least three digits, and it has one more than
;;; *truncate-y*.  This keeps i, i-1, i-2, and low-x-digit happy.  Thanks to
;;; SHIFT-AND-STORE-TRUNCATE-BUFFERS.
;;;


(defun do-truncate (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 ;; Add one for extra sign digit in case high bit is on.
         (len-res (1+ len-q))
         (maybe-q (%allocate-bignum 2))         
	 (q (if (<= len-res 2) maybe-q (%allocate-bignum len-res)))
	 (k (1- len-q))
	 (i (1- len-x))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q len-res k i  low-x-digit))
    (declare (dynamic-extent maybe-q))
    (loop
      (digit-bind (h l)
                  (digit-bind (guess-h guess-l)
                              (bignum-truncate-guess-2 truncate-x i truncate-y (the fixnum (1- len-y)))                                  
                    (try-bignum-truncate-guess truncate-x truncate-y guess-h guess-l len-y low-x-digit))
        (%bignum-set q k h l))
      (cond ((zerop k) (return))
            (t (decf k)
               (decf low-x-digit)
               (setq i (1- i)))))
    (if (eq q maybe-q)
      (progn 
        (setq q (%normalize-bignum-macro q))
        (if (fixnump q) q (copy-bignum q)))
      (%normalize-bignum-macro q))))

(defun do-truncate-no-quo (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 (k (1- len-q))
	 (i (1- len-x))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i  low-x-digit))
    (loop
      (digit-bind (guess-h guess-l) (bignum-truncate-guess-2 truncate-x i truncate-y (the fixnum (1- len-y)))                                 
        (try-bignum-truncate-guess truncate-x truncate-y guess-h guess-l len-y low-x-digit)
        (cond ((zerop k) (return))
              (t (decf k)
                 (decf low-x-digit)
                 (setq i (1- i))))))
    nil))

;;; TRY-BIGNUM-TRUNCATE-GUESS -- Internal.
;;;
;;; This takes a digit guess, multiplies it by *truncate-y* for a result one
;;; greater in length than len-y, and subtracts this result from *truncate-x*.
;;; Low-x-digit is the first digit of x to start the subtraction, and we know x
;;; is long enough to subtract a len-y plus one length bignum from it.  Next we
;;; check the result of the subtraction, and if the high digit in x became
;;; negative, then our guess was one too big.  In this case, return one less
;;; than guess passed in, and add one value of y back into x to account for
;;; subtracting one too many.  Knuth shows that the guess is wrong on the order
;;; of 3/b, where b is the base (2 to the digit-size power) -- pretty rarely.
;;;

(defun try-bignum-truncate-guess (truncate-x truncate-y guess-h guess-l len-y low-x-digit)
  (declare (type bignum-index low-x-digit len-y))

  (let ((carry-digit-h 0)
        (carry-digit-l 0)
	(borrow 1)
	(i low-x-digit))
    (declare (type bignum-index i)
	     (fixnum borrow carry-digit-h carry-digit-l))
    ;; Multiply guess and divisor, subtracting from dividend simultaneously.
    (dotimes (j len-y)
      (multiple-value-bind (y-h y-l) (%bignum-ref truncate-y j)
	(multiple-value-bind (high-h high-l low-h low-l)
	    (%multiply-and-add-1 guess-h
			       guess-l
			       y-h
			       y-l
			       carry-digit-h
			       carry-digit-l)
	  (setq carry-digit-h high-h
		carry-digit-l high-l)
	  (multiple-value-bind (tx-h tx-l) (%bignum-ref truncate-x i)
	    (multiple-value-bind (x-h x-l temp-borrow)
		(%subtract-with-borrow-1 tx-h tx-l low-h low-l borrow)
	      (%bignum-set truncate-x i x-h x-l)
	      (setq borrow temp-borrow)))))
      (incf i))
    (multiple-value-bind (tx-h tx-l) (%bignum-ref truncate-x i)
      (multiple-value-bind (x-h x-l)
	  (%subtract-with-borrow-1 tx-h tx-l carry-digit-h carry-digit-l borrow)
	(%bignum-set truncate-x i x-h x-l)))
    ;; See if guess is off by one, adding one Y back in if necessary.


    (cond ((%digit-0-or-plusp truncate-x i)
	   (values guess-h guess-l))
	  (t
	   ;; If subtraction has negative result, add one divisor value back
	   ;; in.  The guess was one too large in magnitude.
           ;; hmm - happens about 1.6% of the time
           (bignum-add-loop-+ low-x-digit truncate-x truncate-y len-y)
           (%subtract-one guess-h guess-l)
	   ;(%subtract-with-borrow guess-h guess-l 0 1 1)
           ))))



;;; BIGNUM-TRUNCATE-GUESS -- Internal.
;;;
;;; This returns a guess for the next division step.  Y1 is the highest y
;;; digit, and y2 is the second to highest y digit.  The x... variables are
;;; the three highest x digits for the next division step.
;;;
;;; From Knuth, our guess is either all ones or x-i and x-i-1 divided by y1,
;;; depending on whether x-i and y1 are the same.  We test this guess by
;;; determining whether guess*y2 is greater than the three high digits of x
;;; minus guess*y1 shifted left one digit:
;;;    ------------------------------
;;;   |    x-i    |   x-i-1  | x-i-2 |
;;;    ------------------------------
;;;    ------------------------------
;;; - | g*y1 high | g*y1 low |   0   |
;;;    ------------------------------
;;;                ...                   <   guess*y2     ???
;;; If guess*y2 is greater, then we decrement our guess by one and try again.
;;; This returns a guess that is either correct or one too large.
;;;
;;; the y's come from *truncate-y*, x's from *truncate-x*
;;; doing this in lap is not screamingly difficult - x's at i, i-1, i-2





(defun bignum-truncate-guess-2 (x xidx y yidx)
  (digit-bind (guess-h guess-l)
              (%floor-99 x xidx y yidx)
    (truncate-guess-loop guess-h guess-l x xidx y yidx)))



    

;;; SHIFT-Y-FOR-TRUNCATE -- Internal.
;;;
;;; This returns the amount to shift y to place a one in the second highest
;;; bit.  Y must be positive.  If the last digit of y is zero, then y has a
;;; one in the previous digit's sign bit, so we know it will take one less
;;; than digit-size to get a one where we want.  Otherwise, we count how many
;;; right shifts it takes to get zero; subtracting this value from digit-size
;;; tells us how many high zeros there are which is one more than the shift
;;; amount sought.
;;;
;;; Note: This is exactly the same as one less than the integer-length of the
;;; last digit subtracted from the digit-size.
;;; 
;;; We shift y to make it sufficiently large that doing the 64-bit by 32-bit
;;; %FLOOR calls ensures the quotient and remainder fit in 32-bits.
;;;
(defun shift-y-for-truncate (y)
  (the fixnum (1- (the fixnum (%bignum-sign-bits y)))))

;;; SHIFT-AND-STORE-TRUNCATE-BUFFERS -- Internal.
;;;
;;; Stores two bignums into the truncation bignum buffers, shifting them on the
;;; way in.  This assumes x and y are positive and at least two in length, and
;;; it assumes *truncate-x* and *truncate-y* are one digit longer than x and y.
;;;
(defun shift-and-store-truncate-buffers (truncate-x truncate-y x len-x y len-y shift)
  (declare (type bignum-index len-x len-y)
	   (type (integer 0 (#.digit-size)) shift))
  (cond ((eql 0 shift)
	 (bignum-replace truncate-x x :end1 len-x)
	 (bignum-replace truncate-y y :end1 len-y))
	(t
	 (bignum-ashift-left-unaligned x 0 shift (the fixnum (1+ len-x)) truncate-x)
	 (bignum-ashift-left-unaligned y 0 shift (the fixnum (1+ len-y)) truncate-y))))




;;;; General utilities.


;;; %NORMALIZE-BIGNUM-BUFFER -- Internal.
;;;
;;; Internal in-place operations use this to fixup remaining digits in the
;;; incoming data, such as in-place shifting.  This is basically the same as
;;; the first form in %NORMALIZE-BIGNUM, but we return the length of the buffer
;;; instead of shrinking the bignum.
;;;



    




;;; %NORMALIZE-BIGNUM -- Internal.
;;;
;;; This drops the last digit if it is unnecessary sign information.  It
;;; repeats this as needed, possibly ending with a fixnum.  If the resulting
;;; length from shrinking is one, see if our one word is a fixnum.  Shift the
;;; possible fixnum bits completely out of the word, and compare this with
;;; shifting the sign bit all the way through.  If the bits are all 1's or 0's
;;; in both words, then there are just sign bits between the fixnum bits and
;;; the sign bit.  If we do have a fixnum, shift it over for the two low-tag
;;; bits.
;;;

(defun %normalize-bignum (res)
  ;(declare (optimize (speed 3)(safety 0)))
  (%normalize-bignum-2 t res))

;;; %MOSTLY-NORMALIZE-BIGNUM -- Internal.
;;;
;;; This drops the last digit if it is unnecessary sign information.  It
;;; repeats this as needed, possibly ending with a fixnum magnitude but never
;;; returning a fixnum.
;;;

(defun %mostly-normalize-bignum (res &optional len)
  (declare (ignore len))
  (%normalize-bignum-2 nil res))




; think its ok
(defun ldb32 (hi-data lo-data size pos)
  (declare (fixnum hi-data lo-data size pos))
  (let* ((hi-bit (+ pos size))
         (mask (%i- (%ilsl size 1) 1)))
    (declare (fixnum hi-bit mask))    
    (%ilogand mask (if (< hi-bit 16)
                     (%iasr pos lo-data)
                     (if (>= pos 16)
                       (%ilsr (the fixnum (- pos 16)) hi-data)
                       (%ilogior 
                         (%iasr pos lo-data)
                         (%ilsl (the fixnum (- 16 pos)) hi-data)))))))





; this was wrong for negative bigs when byte includes or exceeds sign
(defun %ldb-fixnum-from-bignum (bignum size position)
  (declare (fixnum size position))
  (let* ((low-idx (ash position -5))
         (low-bit (logand position 31))
         (hi-bit (+ low-bit size))
         (len (%bignum-length bignum))
         (minusp (bignum-minusp bignum)))
    (declare (fixnum size position low-bit hi-bit low-idx len))
    (if (>= low-idx len)
      (if minusp (1- (ash 1 size)) 0)      
      (multiple-value-bind (hi lo)(%bignum-ref bignum low-idx)
        (let ((chunk-lo (ldb32 hi lo (min size (%i- 32 low-bit)) low-bit)))
          (let ((val
                 (if (< hi-bit 32) 
                   chunk-lo
                   (progn
                     (setq low-idx (1+ low-idx))
                     (multiple-value-setq (hi lo)
                       (if (>= low-idx len)
                         (if minusp (values #xffff #xffff)(values 0 0))
                         (%bignum-ref bignum low-idx)))
                     (let ((chunk-hi (ldb32 hi lo (%i- size (%i- 32 low-bit)) 0)))
                       (%ilogior (ash chunk-hi (%i- 32 low-bit)) chunk-lo))))))
            val))))))

(defun load-byte (size position integer)
  (if (and (bignump integer)
           (<= size (- 31 target::fixnumshift))
           (fixnump position))
    (%ldb-fixnum-from-bignum integer size position)
    (let ((mask (byte-mask size)))
      (if (and (fixnump mask) (fixnump integer)(fixnump position))
        (%ilogand mask (%iasr position integer))
        (logand mask (ash integer (- position)))))))    


#+safe-but-slow
(defun %bignum-bignum-gcd (u v)
  (setq u (abs u) v (abs v))
  (do* ((g 1 (ash g 1)))
       ((or (oddp u) (oddp v))
	(do* ()
	     ((zerop u) (* g v))
	  (cond ((evenp u) (setq u (ash u -1)))
		((evenp v) (setq v (ash v -1)))
		(t (let* ((temp (ash (abs (- u v)) -1)))
		     (if (< u v)
		       (setq v temp)
		       (setq u temp)))))))
    (setq u (ash u -1) v (ash v -1))))


(defun %positive-bignum-bignum-gcd (u0 v0)
  (let* ((u-len (%bignum-length u0))
	 (v-len (%bignum-length v0)))
    (declare (fixnum u-len v-len))
    (if (or (< u-len v-len)
	    (and (= u-len v-len)
		 (< (bignum-compare u0 v0) 0)))
      (psetq u0 v0 v0 u0 u-len v-len v-len u-len))
    (with-bignum-buffers ((u u-len)
			  (u2 u-len)
			  (v v-len)
			  (v2 v-len))
      (bignum-replace u u0)
      (bignum-replace v v0)
      (let* ((u-trailing-0-bits (%bignum-count-trailing-zero-bits u))
	     (u-trailing-0-digits (ash u-trailing-0-bits -5))
	     (v-trailing-0-bits (%bignum-count-trailing-zero-bits v))
	     (v-trailing-0-digits (ash v-trailing-0-bits -5)))
	(declare (fixnum u-trailing-0-bits v-trailing-0-bits))
	(unless (zerop u-trailing-0-bits)
	  (bignum-shift-right-loop-1
	   (logand u-trailing-0-bits 31)
	   u2
	   u
	   (the fixnum (1- (the fixnum (- u-len u-trailing-0-digits ))))
	   u-trailing-0-digits)
	  (rotatef u u2)
	  (%mostly-normalize-bignum-macro u)
	  (setq u-len (%bignum-length u)))
	(unless (zerop v-trailing-0-bits)
	  (bignum-shift-right-loop-1
	   (logand v-trailing-0-bits 31)
	   v2
	   v
	   (the fixnum (1- (the fixnum (- v-len v-trailing-0-digits))))
	   v-trailing-0-digits)
	  (rotatef v v2)
	  (%mostly-normalize-bignum-macro v)
	  (setq v-len (%bignum-length v)))
	(let* ((shift (min u-trailing-0-bits
			   v-trailing-0-bits)))
	  (loop
            (let* ((fix-u (and (= u-len 1)
                               (let* ((hi-u (%bignum-ref-hi u 0)))
                                 (declare (fixnum hi-u))
                                 (= hi-u (the fixnum
                                           (logand hi-u (ash target::target-most-positive-fixnum -16)))))
                               (uvref u 0)))
                   (fix-v (and (= v-len 1)
                               (let* ((hi-v (%bignum-ref-hi v 0)))
                                 (declare (fixnum hi-v))
                                 (= hi-v (the fixnum
                                           (logand hi-v (ash target::target-most-positive-fixnum -16)))))
                               (uvref v 0))))
              (if fix-v
                (if fix-u
                  (return (ash (%fixnum-gcd fix-u fix-v) shift))
                  (return (ash (bignum-fixnum-gcd u fix-v) shift)))
                (if fix-u
                  (return (ash (bignum-fixnum-gcd v fix-u) shift)))))
	      
            (let* ((signum (if (> u-len v-len)
                             1
                             (if (< u-len v-len)
                               -1
                               (bignum-compare u v)))))
              (declare (fixnum signum))
              (case signum
                (0			; (= u v)
                 (if (zerop shift)
                   (let* ((copy (%allocate-bignum u-len)))
                     (bignum-replace copy u)
                     (return copy))
                   (return (ash u shift))))
                (1			; (> u v)
                 (bignum-subtract-loop u u-len v v-len u)
                 (%mostly-normalize-bignum-macro u)
                 (setq u-len (%bignum-length u))
                 (setq u-trailing-0-bits
                       (%bignum-count-trailing-zero-bits u)
                       u-trailing-0-digits
                       (ash u-trailing-0-bits -5))
                 (unless (zerop u-trailing-0-bits)
		   (%init-misc 0 u2)
		   (bignum-shift-right-loop-1
		    (logand u-trailing-0-bits 31)
		    u2
		    u
		    (the fixnum (1- (the fixnum (- u-len
						   u-trailing-0-digits))))
		    u-trailing-0-digits)
		   (rotatef u u2)
		   (%mostly-normalize-bignum-macro u)
		   (setq u-len (%bignum-length u))))
                (t			; (> v u)
                 (bignum-subtract-loop v v-len u u-len v)
                 (%mostly-normalize-bignum-macro v)
                 (setq v-len (%bignum-length v))
                 (setq v-trailing-0-bits
                       (%bignum-count-trailing-zero-bits v)
                       v-trailing-0-digits
                       (ash v-trailing-0-bits -5))
                 (unless (zerop v-trailing-0-bits)
		   (%init-misc 0 v2)
		   (bignum-shift-right-loop-1
		    (logand v-trailing-0-bits 31)
		    v2
		    v
		    (the fixnum (1- (the fixnum (- v-len v-trailing-0-digits))))
		    v-trailing-0-digits)
		   (rotatef v v2)
		   (%mostly-normalize-bignum-macro v)
		   (setq v-len (%bignum-length v))))))))))))

(defun %bignum-bignum-gcd (u v)
  (with-negated-bignum-buffers u v %positive-bignum-bignum-gcd))

(defun unsignedwide->integer (uwidep)
  (with-bignum-buffers ((b 3))
    (setf (uvref b 0) (%get-unsigned-long uwidep 4)
	  (uvref b 1) (%get-unsigned-long uwidep 0))
    (let* ((n (%normalize-bignum b)))
      (if (typep n 'bignum)
        (copy-bignum n)
        n))))

(defun one-bignum-factor-of-two (a)  
  (declare (type bignum-type a))
  (let ((len (%bignum-length a)))
    (declare (fixnum len))
    (dotimes (i len)
      (multiple-value-bind (a-h a-l) (%bignum-ref a i)
        (declare (fixnum a-h a-l))
        (unless (and (= a-h 0)(= a-l 0))
          (return (+ (%ilsl 5 i)
                     (let* ((j 0)
                            (a a-l))
                       (declare (fixnum a j))
                       (if (= a-l 0) (setq j 16 a a-h))
                       (dotimes (i 16)            
                         (if (oddp a)
                           (return (%i+ j i))
                           (setq a (%iasr 1 a))))))))))))

(defun logbitp (index integer)
  "Predicate returns T if bit index of integer is a 1."
  (number-case index
    (fixnum
     (if (minusp (the fixnum index))(report-bad-arg index '(integer 0))))
    (bignum
     ;; assuming bignum cant have more than most-positive-fixnum bits
     ;; (2 expt 24 longs)
     (if (bignum-minusp index)(report-bad-arg index '(integer 0)))
     ;; should error if integer isn't
     (return-from logbitp (minusp (require-type integer 'integer)))))
  (number-case integer
    (fixnum
     (if (%i< index (- target::nbits-in-word target::fixnumshift))
       (%ilogbitp index integer)
       (minusp (the fixnum integer))))
    (bignum
     (let ((bidx (%iasr 5 index))
           (bbit (%ilogand index 31)))
       (declare (fixnum bidx bbit))
       (if (>= bidx (%bignum-length integer))
         (bignum-minusp integer)
         (multiple-value-bind (hi lo) (%bignum-ref integer bidx)
           (declare (fixnum hi lo))
           (if (> bbit 15)
             (%ilogbitp (%i- bbit 16) hi)
             (%ilogbitp bbit lo))))))))

) ; #+32-bit-target
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-bignum32.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-def.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;;; primitives that manipulate function & variable definitions.





(defun functionp (arg)
  "Return true if OBJECT is a FUNCTION, and NIL otherwise."
  (functionp arg))

(defun lfunp (arg)
  (functionp arg))

(defun %proclaim-special (sym &optional initp)
  (let* ((oldbits (%symbol-bits sym)))
    (declare (fixnum oldbits))
    (%symbol-bits sym (bitset $sym_vbit_special oldbits))
    initp))

(setq *lfun-names* (make-hash-table :test 'eq :weak t))

(defun lookup-lfun-name (lfun) 
  (gethash lfun *lfun-names*))


(defun function-name (fun)
  (or (and (functionp fun) (lfun-name fun))
      (if (compiled-function-p (setq fun (closure-function fun)))
        (lfun-name fun))))


(defun bootstrapping-fmakunbound (name)
  (when (consp name)
    (unless (eq (%car name) 'setf)
      (error "Function spec handler not loaded yet"))
    (setq name (setf-function-name (cadr name))))
  (%unfhave name)
  name)

;;; redefined in sysutils.
(%fhave 'fmakunbound #'bootstrapping-fmakunbound)

(defun bootstrapping-fset (name fn)
  (fmakunbound name)
  (%fhave name fn)
  fn)

;Redefined in sysutils.
(%fhave 'fset #'bootstrapping-fset)

(defun fset-symbol (name fn)
  (fset (require-type name 'symbol) fn))


(defun bootstrapping-record-source-file (fn &optional type)
  (declare (ignore fn type))
  nil)

;Redefined in l1-utils.
(%fhave 'record-source-file #'bootstrapping-record-source-file)


(setq *fasload-print* nil)
(setq *save-doc-strings* t)



(%fhave '%defun-encapsulated-maybe ;Redefined in encapsulate
        (qlfun bootstrapping-defun-encapsulated (name fn)
          (declare (ignore name fn))
          nil))

(%fhave 'encapsulated-function-name  ;Redefined in encapsulate - used in l1-io
        (qlfun bootstrapping-encapsulated-function-name (fn)
          (declare (ignore fn))
          nil))

(%fhave 'set-function-info (qlfun set-function-info  (name info)
                                  (if (typep info 'string)
                                    (set-documentation name 'function info))
                                  name))

(defun %defun (named-fn &optional info)
  (unless (typep named-fn 'function)
    (dbg named-fn))
  (let* ((name (function-name named-fn)))
    (unless (and name
                 (or (symbolp name)
                     (setf-function-name-p name)))
      (dbg named-fn))
  (record-source-file name 'function)
  (when (not (%defun-encapsulated-maybe name named-fn))
    (when (and (symbolp name) (macro-function name nil))
      (warn "The macro ~s is being redefined as a function." name)
      (fmakunbound name))
    (fset name named-fn))
  (set-function-info name info)
  (when *fasload-print* (format t "~&~S~%" name))
  name))

(defun validate-function-name (name)
  (if (symbolp name)
    name
    (if (setf-function-name-p name)
      (setf-function-name (cadr name))
      (report-bad-arg name 'function-name))))

;;;    There are three kinds of things which can go in the function
;;;    cell of a symbol: 1) A function.  2) The thing which is the
;;;    value of %unbound-function%: a 1-element vector whose 0th
;;;    element is a code vector which causes an "undefined function"
;;;    error to be signalled.  3) A macro or special-form definition,
;;;    which is a 2-element vector whose 0th element is a code vector
;;;    which signals a "can't apply macro or special form" error when
;;;    executed and whose 1st element is a macro or special-operator
;;;    name.  It doesn't matter what type of gvector cases 2 and 3
;;;    are.  Once that's decided, it wouldn't hurt if %FHAVE
;;;    typechecked its second arg.

(defun %fhave (name def)
  (let* ((fname (validate-function-name name)))
    (setf (%svref (symptr->symvector (%symbol->symptr fname)) target::symbol.fcell-cell) def)))

;;; FBOUNDP is true of any symbol whose function-cell contains something other
;;; than %unbound-function%; we expect FBOUNDP to return that something.
(defun fboundp (name)
  "Return true if name has a global function definition."
  (let* ((fname (validate-function-name name))
         (def (%svref (symptr->symvector (%symbol->symptr fname)) target::symbol.fcell-cell)))
    (unless (eq def %unbound-function%)
      def)))

;;; %UNFHAVE doesn't seem to want to deal with SETF names or function specs.
;;; Who does ?

(defun %unfhave (sym)
  (let* ((symvec (symptr->symvector (%symbol->symptr sym)))
         (old (%svref symvec target::symbol.fcell-cell))
         (unbound %unbound-function%))
    (setf (%svref symvec target::symbol.fcell-cell) unbound)
    (not (eq old unbound))))

;;; It's guaranteed that lfun-bits is a fixnum.  Might be a 30-bit fixnum ...





(defun lfun-vector-name (fun &optional (new-name nil set-name-p))
  (let* ((bits (lfun-bits fun)))
    (declare (fixnum bits))
    (if (and (logbitp $lfbits-gfn-bit bits)
	     (not (logbitp $lfbits-method-bit bits)))
      (progn
        (if set-name-p
          (%gf-name fun new-name)
          (%gf-name fun)))
      (let* ((has-name-cell (not (logbitp $lfbits-noname-bit bits))))
	(if has-name-cell
	  (let* ((lfv (lfun-vector fun))
                 (name-idx (- (the fixnum (uvsize lfv)) 2))
		 (old-name (%svref lfv name-idx)))
	    (declare (fixnum name-idx))
	    (if (and set-name-p (not (eq old-name new-name)))
	      (setf (%svref lfv name-idx) new-name))
	    old-name))))))

(defun lfun-name (fun &optional (new-name nil set-name-p))
  (multiple-value-bind (stored-name stored?) (lookup-lfun-name fun)
    (unless stored?
      (setq stored-name (lfun-vector-name fun)))
    (when (and set-name-p (neq new-name stored-name))
      (if (and stored? (eq new-name (lfun-vector-name fun)))
        (remhash fun *lfun-names*)
        (if (logbitp $lfbits-noname-bit (the fixnum (lfun-bits fun)))   ; no name-cell in function vector.
          (puthash fun *lfun-names* new-name)
          (lfun-vector-name fun new-name))))
    stored-name))

(defun lfun-bits (function &optional new)
  (unless (functionp function)
    (setq function (require-type function 'function)))
  (let* ((lfv (lfun-vector function))
         (idx (1- (the fixnum (uvsize lfv))))
         (old (%svref lfv idx)))
    (declare (fixnum idx))
    (if new
      (setf (%svref lfv idx) new))
    old))
    
(defun %macro-have (symbol macro-function)
  (declare (special %macro-code%))      ; magically set by xloader.
  (%fhave symbol
          #-arm-target (vector %macro-code% macro-function)
          #+arm-target (%fix-fn-entrypoint (gvector :pseudofunction 0 %macro-code% macro-function))))


(defun special-operator-p (symbol)
  "If the symbol globally names a special form, return T, otherwise NIL."
  (let ((def (fboundp symbol)))
    (and #-arm-target (typep def 'simple-vector)
         #+arm-target (= (typecode def) arm::subtag-pseudofunction)
         (not (lfunp #-arm-target (svref def 1)
                     #+arm-target (uvref def 2))))))

(defun special-form-p (x) (special-operator-p x))

(defun setf-function-name-p (thing)
  (and (consp thing)
       (consp (%cdr thing))
       (null (%cddr thing))
       (eq (%car thing) 'setf)
       (symbolp (%cadr thing))))

(defun macro-function (form &optional env)
  "If SYMBOL names a macro in ENV, returns the expansion function,
   else returns NIL. If ENV is unspecified or NIL, use the global
   environment only."
  (setq form (require-type form 'symbol))
  (when env
    ;; A definition-environment isn't a lexical environment, but it can
    ;; be an ancestor of one.
    (unless (istruct-typep env 'lexical-environment)
        (report-bad-arg env 'lexical-environment))
      (let ((cell nil))
        (tagbody
          top
          (if (setq cell (%cdr (assq form (lexenv.functions env))))
            (return-from macro-function 
              (if (eq (car cell) 'macro) (%cdr cell))))
          (unless (listp (setq env (lexenv.parent-env env)))
            (go top)))))
      ;; Not found in env, look in function cell.
  (%global-macro-function form))

(defun %fixnum-ref-macptr (fixnum &optional (offset 0))
  (%int-to-ptr (%fixnum-ref-natural fixnum offset)))

(defun %fixnum-set-macptr (fixnum offset &optional (newval offset newval-p))
  (%fixnum-set-natural fixnum (if newval-p offset 0) (%ptr-to-int newval))
  newval)

(defun nth-catch-frame-tag (n)
  (declare (fixnum n))
  (let* ((frame (%catch-top (%current-tcr))))
    (dotimes (i n (%svref frame target::catch-frame.catch-tag-cell))
      (setq frame (%svref frame target::catch-frame.link-cell)))))

;;; This function is magic, and it can only be called from
;;; an unwind-protect cleanup form (making it even more magic.)
;;; If we can tell that we reached the unwind-protect via THROW,
;;; return a list of the target catch tag and all values being
;;; thrown.
#+x86-target
(defun %throwing-through-cleanup-p ()
  ;; when we enter and unwind-protect cleanup on x8664, the
  ;; top frame on the tstack contains state information that's
  ;; used both by THROW and by normal exit from the protected
  ;; form.  That state information contains a count of the number
  ;; of catch/unwind-protect frames still to be processed (non-zero
  ;; only in the case where we're actually throwing), the value(s)
  ;; being thrown, and a return address that isn't interesting to
  ;; us.  It's an historical accident that that information is stored
  ;; differently in the cases where a single value is being thrown
  ;; and multiple values are thrown.
  ;; A tstack frame is always doubleword aligned, and the first two
  ;; words are a backpointer to the previous tstack frame and a
  ;; pointer into the main lisp stack.  In the single value case,
  ;; we then have 3 words: return address, frame count, value;
  ;; in the multiple-value we have 3 fixed words (value count,
  ;; return address, frame count) with the values following the
  ;; frame count (value 0 follows immediately.)
  ;; A cleanup form is always called from either .SPnthrowvalues
  ;; of .SPnthrow1value, and those subprims can be called either
  ;; by .SPthrow (in which case the return address in the frame
  ;; will have no function associated with it) or by Lisp code
  ;; (in which case it will.)
  ;; We (have to) just assume that the frame on top of the temp
  ;; stack is context info for the nthrow stuff.  Tracing this
  ;; function may violate this assumption and cause misbehavior
  ;; here.
  (let* ((frame (%current-tsp))
         (single-value-case (not (typep (%lisp-word-ref frame 2) 'fixnum)))
         (frame-count (%lisp-word-ref frame (if single-value-case 3 4)))
         (throwing (null (%return-address-function (if single-value-case
                                                     (%lisp-word-ref frame 2)
                                                     (%lisp-word-ref frame 3))))))
    (declare (fixnum frame))
    (if throwing
      (collect ((info))
        (info (nth-catch-frame-tag frame-count))
        (if single-value-case
          (info (%lisp-word-ref frame 4))
          (let* ((valptr (+ frame 5)))
            (declare (fixnum valptr))
            (dotimes (i (%lisp-word-ref frame 2))
              (declare (fixnum i))
              (info (%lisp-word-ref valptr i)))))
        (info)))))

;;; end of l0-def.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-def.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-complex.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(eval-when (:compile-toplevel)
  (require "NUMBER-MACROS"))

(defun coerce-to-complex-type (num type)
  (cond ((complexp num)
         (let ((real (%realpart num))
               (imag (%imagpart num)))
           (if (and (typep real type)
                    (typep imag type))
             num
             (complex (coerce real type)
                      (coerce imag type)))))
        (t (complex (coerce num type)))))

;;; end of l0-complex.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-complex.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-io.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
; -*- Mode: Lisp; Package: CCL; -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")




(defun utf-8-octets-in-string (string start end)
  (if (>= end start)
    (do* ((noctets 0)
          (i start (1+ i)))
         ((= i end) noctets)
      (declare (fixnum noctets))
      (let* ((code (char-code (schar string i))))
        (declare (type (mod #x110000) code))
        (incf noctets
              (if (< code #x80)
                1
                (if (< code #x800)
                  2
                  (if (< code #x10000)
                    3
                    4))))))
    0))

(defun utf-16-octets-in-string (string start end)
  (if (>= end start)
    (do* ((noctets 0)
          (i start (1+ i)))
         ((= i end) noctets)
      (declare (fixnum noctets))
      (let* ((code (char-code (schar string i))))
        (declare (type (mod #x110000) code))
        (incf noctets
              (if (< code #x10000)
                2
                4))))
    0))

(defun utf-8-memory-encode (string pointer idx start end)
  (declare (fixnum idx))
  (do* ((i start (1+ i)))
       ((>= i end) idx)
    (let* ((code (char-code (schar string i))))
      (declare (type (mod #x110000) code))
      (cond ((< code #x80)
             (setf (%get-unsigned-byte pointer idx) code)
             (incf idx))
            ((< code #x800)
             (setf (%get-unsigned-byte pointer idx)
                   (logior #xc0 (the fixnum (ash code -6))))
             (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                   (logior #x80 (the fixnum (logand code #x3f))))
             (incf idx 2))
            ((< code #x10000)
             (setf (%get-unsigned-byte pointer idx)
                   (logior #xe0 (the fixnum (ash code -12))))
             (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                   (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
             (setf (%get-unsigned-byte pointer (the fixnum (+ idx 2)))
                   (logior #x80 (the fixnum (logand code #x3f))))
             (incf idx 3))
            (t
             (setf (%get-unsigned-byte pointer idx)
                   (logior #xf0
                           (the fixnum (logand #x7 (the fixnum (ash code -18))))))
             (setf (%get-unsigned-byte pointer (the fixnum (1+ idx)))
                   (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -12))))))
             (setf (%get-unsigned-byte pointer (the fixnum (+ idx 2)))
                   (logior #x80 (the fixnum (logand #x3f (the fixnum (ash code -6))))))
             (setf (%get-unsigned-byte pointer (the fixnum (+ idx 3)))
                   (logior #x80 (logand #x3f code)))
             (incf idx 4))))))

(defun native-utf-16-memory-encode (string pointer idx start end)
  (declare (fixnum idx))
  (do* ((i start (1+ i)))
       ((>= i end) idx)
    (let* ((code (char-code (schar string i)))
           (highbits (- code #x10000)))
      (declare (type (mod #x110000) code)
               (fixnum  highbits))
      (cond ((< highbits 0)
             (setf (%get-unsigned-word pointer idx) code)
             (incf idx 2))
            (t
             (setf (%get-unsigned-word pointer idx) (logior #xd800 (the fixnum (ash highbits -10))))
             (incf idx 2)
             (setf (%get-unsigned-word pointer idx) (logior #xdc00 (the fixnum (logand highbits #x3ff))))
             (incf idx 2))))))

(defun utf-8-memory-decode (pointer noctets idx string)
  (declare (fixnum noctets idx))
  (do* ((i 0 (1+ i))
        (end (+ idx noctets))
        (index idx (1+ index)))
       ((>= index end) (if (= index end) index 0))
    (let* ((1st-unit (%get-unsigned-byte pointer index)))
      (declare (type (unsigned-byte 8) 1st-unit))
      (let* ((char (if (< 1st-unit #x80)
                     (code-char 1st-unit)
                     (if (>= 1st-unit #xc2)
                       (let* ((2nd-unit (%get-unsigned-byte pointer (incf index))))
                         (declare (type (unsigned-byte 8) 2nd-unit))
                         (if (< 1st-unit #xe0)
                           (if (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                             (code-char
                              (logior
                               (the fixnum (ash (the fixnum (logand #x1f 1st-unit)) 6))
                               (the fixnum (logxor 2nd-unit #x80)))))
                           (let* ((3rd-unit (%get-unsigned-byte pointer (incf index))))
                             (declare (type (unsigned-byte 8) 3rd-unit))
                             (if (< 1st-unit #xf0)
                               (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                        (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                        (or (>= 1st-unit #xe1)
                                            (>= 2nd-unit #xa0)))
                                 (code-char (the fixnum
                                              (logior (the fixnum
                                                        (ash (the fixnum (logand 1st-unit #xf))
                                                             12))
                                                      (the fixnum
                                                        (logior
                                                         (the fixnum
                                                           (ash (the fixnum (logand 2nd-unit #x3f))
                                                                6))
                                                         (the fixnum (logand 3rd-unit #x3f))))))))
                               (if (< 1st-unit #xf8)
                                 (let* ((4th-unit (%get-unsigned-byte pointer (incf index))))
                                   (declare (type (unsigned-byte 8) 4th-unit))
                                   (if (and (< (the fixnum (logxor 2nd-unit #x80)) #x40)
                                            (< (the fixnum (logxor 3rd-unit #x80)) #x40)
                                            (< (the fixnum (logxor 4th-unit #x80)) #x40)
                                            (or (>= 1st-unit #xf1)
                                                (>= 2nd-unit #x90)))
                                     (code-char
                                      (logior
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logand 1st-unit 7)) 18))
                                          (the fixnum
                                            (ash (the fixnum (logxor 2nd-unit #x80)) 12))))
                                       (the fixnum
                                         (logior
                                          (the fixnum
                                            (ash (the fixnum (logxor 3rd-unit #x80)) 6))
                                          (the fixnum (logxor 4th-unit #x80)))))))))))))))))
        (setf (schar string i) (or char (note-vector-decoding-problem pointer index :utf-8)))))))

(defun utf-8-length-of-memory-encoding (pointer noctets start)
  (do* ((i start)
        (end (+ start noctets))
        (nchars 0 (1+ nchars)))
       ((= i end) (values nchars (- i start)))
    (let* ((code (%get-unsigned-byte pointer i))
           (nexti (+ i (cond ((< code #xc2) 1)
                             ((< code #xe0) 2)
                             ((< code #xf0) 3)
                             ((< code #xf8) 4)
                             (t 1)))))
      (declare (type (unsigned-byte 8) code))
      (if (> nexti end)
        (return (values nchars (- i start)))
        (setq i nexti)))))



;;; write nbytes bytes from buffer buf to file-descriptor fd.
(defun fd-write (fd buf nbytes)
  (ignoring-eintr
   (int-errno-ffcall
    (%kernel-import target::kernel-import-lisp-write)
             :int fd :address buf :ssize_t nbytes :ssize_t)))

(defun fd-read (fd buf nbytes)
  (ignoring-eintr
   (int-errno-ffcall
    (%kernel-import target::kernel-import-lisp-read)
             :int fd :address buf :ssize_t nbytes :ssize_t)))


(let* ((pathname-encoding-name ()))
  (declare (ignorable pathname-encoding-name))
  (defun pathname-encoding-name ()
    #+darwin-target :utf-8
    #+windows-target :utf-16le
    #-(or darwin-target windows-target) pathname-encoding-name)
  (defun set-pathname-encoding-name (new)
    #+(or darwin-target windows-target) (declare (ignore new))
    #+darwin-target :utf-8
    #+windows-target :utf-16le
    #-(or darwin-target windows-target)
    (let* ((encoding (ensure-character-encoding new)))
      (setq pathname-encoding-name
            (unless (eq encoding (get-character-encoding nil))
              (character-encoding-name encoding))))))


(defun fd-open-path (p flags create-mode)
  (let* ((fd (int-errno-ffcall
              (%kernel-import target::kernel-import-lisp-open)
              :address p :int flags :mode_t create-mode :int)))
    (declare (fixnum fd))
    (when (or (= fd (- #$EMFILE))
              (= fd (- #$ENFILE)))
      (gc)
      (drain-termination-queue)
      (setq fd (int-errno-ffcall
                (%kernel-import target::kernel-import-lisp-open)
                :address p :int flags :mode_t create-mode :int)))
    fd))

(defun fd-open (path flags &optional (create-mode #o666))
  #+darwin-target (with-utf-8-cstrs ((p path))
                    (fd-open-path p flags create-mode))
  #+windows-target (with-native-utf-16-cstrs ((p path))
                     (fd-open-path p flags create-mode))
  #-(or darwin-target windows-target)
  (let* ((encoding (pathname-encoding-name)))
    (if encoding
      (with-encoded-cstrs encoding ((p path))
        (fd-open-path p flags create-mode))
      (with-cstrs ((p path))
        (fd-open-path p flags create-mode)))))

(defun fd-chmod (fd mode)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-fchmod)
                    :int fd
                    :mode_t mode
                    :int))

(defun fd-lseek (fd offset whence)
  (int-errno-ffcall
   (%kernel-import target::kernel-import-lisp-lseek)
   :int fd
   :signed-doubleword offset
   :int whence
   :signed-doubleword))

(defun fd-close (fd)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-close)
                    :int fd
                    :int)) 

(defun fd-tell (fd)
  (fd-lseek fd 0 #$SEEK_CUR))

;;; Kernels prior to 2.4 don't seem to have a "stat" variant
;;; that handles 64-bit file offsets.
(defun fd-size (fd)
  (rlet ((stat #+win64-target #>_stat64 #+win32-target #>__stat64 #-windows-target :stat))
    (if (eql 0 (ff-call (%kernel-import target::kernel-import-lisp-fstat)
                        :int fd
                        :address stat
                        :int))
      (pref stat
            #-windows-target :stat.st_size
            #+win64-target #>_stat64.st_size
            #+win32-target #>__stat64.st_size)
      -1)))


(defun fd-ftruncate (fd new)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-ftruncate)
                    :int fd :off_t new :int))

(defun %string-to-stderr (str)
  (with-cstrs ((s str))
    (fd-write 2 s (length str))))

(defun pdbg (string)
  (%string-to-stderr string)
  (%string-to-stderr #.(string #\LineFeed)))



;;; Not really I/O, but ...
(defun malloc (size)
  (ff-call 
   (%kernel-import target::kernel-import-malloc)
   :unsigned-fullword size :address))

(defun free (ptr)
  (let* ((size (uvsize ptr))
         (flags (if (= size target::xmacptr.size)
                  (uvref ptr target::xmacptr.flags-cell)
                  $flags_DisposPtr)))
    (declare (fixnum size flags))
    (if (= flags $flags_DisposPtr)
      (with-macptrs ((addr ptr))
        (when (= size target::xmacptr.size)
          (%setf-macptr ptr (%null-ptr))
          (setf (uvref ptr target::xmacptr.flags-cell) $flags_Normal))
        (ff-call 
         (%kernel-import target::kernel-import-free)
         :address addr :void)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-io.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-utils.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
; -*- Mode: Lisp;  Package: CCL; -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.



; l0-utils.lisp


(in-package "CCL")

(defun %proclaim-notspecial (sym)
  (%symbol-bits sym (logandc2 (%symbol-bits sym) (ash 1 $sym_bit_special))))


(defun heap-area-name (code)
  (cond ((eq code area-void) :void)
        ((eq code area-cstack) :cstack)
        ((eq code area-vstack) :vstack)
        ((eq code area-tstack) :tstack)
        ((eq code area-readonly) :readonly)
        ((eq code area-watched) :watched)
        ((eq code area-managed-static) :managed-static)
        ((eq code area-static) :static)
        ((eq code area-dynamic) :dynamic)
        (t code)))

(defun heap-area-code (name)
  (case name
    (:void area-void)
    (:cstack area-cstack)
    (:vstack area-vstack)
    (:tstack area-tstack)
    (:readonly area-readonly)
    (:watched area-watched)
    (:managed-static area-managed-static)
    (:static area-static)
    (:dynamic area-dynamic)
    (t (if (and (fixnump name)
                (<= area-readonly name area-dynamic))
         name
         (heap-area-code (require-type name '(member :void :cstack :vstack :tstack
                                                     :readonly :managed-static :static :dynamic)))))))


;;; We MAY need a scheme for finding all of the areas in a lisp library.
(defun %map-areas (function &optional area)
  (let* ((area (cond ((or (eq area t) (eq area nil)) nil)
                     ((consp area) (mapcar #'heap-area-code area)) ;; list of areas
                     (t (heap-area-code area))))
         (mincode area-readonly)
         (maxcode area-dynamic))
  (declare (fixnum maxcode mincode))
  (do* ((a (%normalize-areas) (%lisp-word-ref a (ash target::area.succ (- target::fixnumshift))))
        (code area-dynamic (%lisp-word-ref a (ash target::area.code (- target::fixnumshift))))
        (dynamic t nil))
       ((= code area-void))
    (declare (fixnum code))
    (if (and (<= code maxcode)
             (>= code mincode)
             (or (null area)
                 (eql code area)
                 (and (consp area) (member code area))))
      (if dynamic 
        (walk-dynamic-area a function)
        (unless (= code area-dynamic)        ; ignore egc areas, 'cause walk-dynamic-area sees them.
          (walk-static-area a function)))))))


;;; there'll be functions in static lib areas.
;;; (Well, there would be if there were really static lib areas.)

(defun %map-lfuns (f)
  (let* ((filter #'(lambda (obj) (when (= (the fixnum (typecode obj))
                                          target::subtag-function)
                                   (funcall f (lfun-vector-lfun obj))))))
    (declare (dynamic-extent filter))
    (%map-areas filter '(:dynamic :static :managed-static :readonly))))


(defun ensure-simple-string (s)
  (cond ((simple-string-p s) s)
        ((stringp s)
         (let* ((len (length s))
                (new (make-string len :element-type 'base-char)))
           (declare (fixnum len)(optimize (speed 3)(safety 0)))
           (multiple-value-bind (ss offset) (array-data-and-offset s)
             (%copy-ivector-to-ivector ss (ash offset 2) new 0 (ash len 2)))
           new))
        (t (report-bad-arg s 'string))))

(defun nremove (elt list)
  (let* ((handle (cons nil list))
         (splice handle))
    (declare (dynamic-extent handle))
    (loop
      (if (eq elt (car (%cdr splice)))
        (unless (setf (%cdr splice) (%cddr splice)) (return))
        (unless (cdr (setq splice (%cdr splice)))
          (return))))
    (%cdr handle)))


(eval-when (:compile-toplevel :execute)
  (defmacro need-use-eql-macro (key)
    `(let* ((typecode (typecode ,key)))
       (declare (fixnum typecode))
       (or (= typecode target::subtag-macptr)
            (and (< typecode (- target::nbits-in-word target::fixnumshift))
         (logbitp (the (integer 0 (#.(- target::nbits-in-word target::fixnumshift)))
                    typecode)
                  (logior (ash 1 target::tag-fixnum)
                          (ash 1 target::subtag-bignum)
                          (ash 1 target::subtag-single-float)
                          (ash 1 target::subtag-double-float)
                          (ash 1 target::subtag-ratio)
                          (ash 1 target::subtag-complex)))))))

)

(defun asseql (item list)
  (if (need-use-eql-macro item)
    (dolist (pair list)
      (if pair
	(if (eql item (car pair))
	  (return pair))))
    (assq item list)))

(defun assequal (item list)
  (dolist (pair list)
    (if pair
      (if (equal item (car pair))
        (return pair)))))


;;; (memeql item list) <=> (member item list :test #'eql :key #'identity)
(defun memeql (item list)
  (if (need-use-eql-macro item)
    (do* ((l list (%cdr l)))
         ((endp l))
      (when (eql (%car l) item) (return l)))
    (memq item list)))

(defun memequal (item list)
  (do* ((l list (%cdr l)))
       ((endp l))
    (when (equal (%car l) item) (return l))))


; (member-test item list test-fn) 
;   <=> 
;     (member item list :test test-fn :key #'identity)
(defun member-test (item list test-fn)
  (if (or (eq test-fn 'eq)(eq test-fn  #'eq)
          (and (or (eq test-fn 'eql)(eq test-fn  #'eql))
               (not (need-use-eql-macro item))))
    (do* ((l list (cdr l)))
         ((null l))
      (when (eq item (car l))(return l)))
    (if (or (eq test-fn 'eql)(eq test-fn  #'eql))
      (do* ((l list (cdr l)))
           ((null l))
        (when (eql item (car l))(return l)))    
      (do* ((l list (cdr l)))
           ((null l))
        (when (funcall test-fn item (car l)) (return l))))))

(defun s32->u32 (s32)
  (%stack-block ((buf 4))
    (setf (%get-signed-long buf) s32)
    (%get-unsigned-long buf)))

(defun u32->s32 (u32)
  (%stack-block ((buf 4))
    (setf (%get-unsigned-long buf) u32)
    (%get-signed-long buf)))


(defun car (x) (car x))
(defun cdr (x) (cdr x))

; end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-utils.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-aprims.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.


(in-package "CCL")

; l0-aprims.lisp

;;; This weak list is used to track semaphores as well as locks.
(defvar %system-locks% nil)


(defun record-system-lock (l)
  (atomic-push-uvector-cell %system-locks% population.data l)
  l)

;;; This has to run very early in the initial thread.
(defun %revive-system-locks ()
  (dolist (s (population-data %system-locks%))
    (%revive-macptr s)
    (%setf-macptr s
                  (case (uvref s target::xmacptr.flags-cell)
                    (#.$flags_DisposeRecursiveLock
                     (ff-call
                      (%kernel-import target::kernel-import-new-recursive-lock)
                      :address))
                    (#.$flags_DisposeRwlock
                     (ff-call
                      (%kernel-import target::kernel-import-rwlock-new)
                      :address))
		    (#.$flags_DisposeSemaphore
		     (ff-call
		      (%kernel-import target::kernel-import-new-semaphore)
		      :signed-fullword 0
		      :address))))
    (set-%gcable-macptrs% s)))

(dolist (p %all-packages%)
  (setf (pkg.lock p) (make-read-write-lock)))

(defparameter %all-packages-lock% nil)



(defun %cstr-pointer (string pointer &optional (nul-terminated t))
  (if (typep string 'simple-base-string)
    (locally (declare (simple-base-string string)
                      (optimize (speed 3) (safety 0)))
      (let* ((n (length string)))
        (declare (fixnum n))
        (dotimes (i n)
          (setf (%get-unsigned-byte pointer i)
                (let* ((code (%scharcode string i)))
                  (declare (type (mod #x110000) code))
                  (if (< code 256)
                    code
                    (char-code #\Sub)))))
        (when nul-terminated
          (setf (%get-byte pointer n) 0)))
      nil)
    (%cstr-segment-pointer string pointer 0 (length string) nul-terminated)))

(defun %cstr-segment-pointer (string pointer start end &optional (nul-terminated t))
  (declare (fixnum start end))
  (let* ((n (- end start)))
    (multiple-value-bind (s o) (dereference-base-string string)
      (declare (fixnum o))
      (do* ((i 0 (1+ i))
            (o (the fixnum (+ o start)) (1+ o)))
           ((= i n))
        (declare (fixnum i o))
        (setf (%get-unsigned-byte pointer i)
              (let* ((code (char-code (schar s o))))
                (declare (type (mod #x110000) code))
                (if (< code 256)
                  code
                  (char-code #\Sub))))))
    (when nul-terminated
      (setf (%get-byte pointer n) 0))
    nil))

(defun string (thing)
  "Coerces X into a string. If X is a string, X is returned. If X is a
   symbol, X's pname is returned. If X is a character then a one element
   string containing that character is returned. If X cannot be coerced
   into a string, an error occurs."
  (etypecase thing
    (string thing)
    (symbol (symbol-name thing))
    (character
     (let* ((s (make-string 1)))
       (setf (schar s 0) thing)
       s))))


(defun dereference-base-string (s)
  (multiple-value-bind (vector offset) (array-data-and-offset s)
    (unless (typep vector 'simple-base-string) (report-bad-arg s 'base-string))
    (values vector offset (length s))))

(defun make-gcable-macptr (flags)
  (let ((v (%alloc-misc target::xmacptr.element-count target::subtag-macptr)))
    (setf (uvref v target::xmacptr.address-cell) 0) ; ?? yup.
    (setf (uvref v target::xmacptr.flags-cell) flags)
    (set-%gcable-macptrs% v)
    v))

(defun %make-recursive-lock-ptr ()
  (record-system-lock
   (%setf-macptr
    (make-gcable-macptr $flags_DisposeRecursiveLock)
    (ff-call (%kernel-import target::kernel-import-new-recursive-lock)
             :address))))

(defun %make-rwlock-ptr ()
  (record-system-lock
   (%setf-macptr
    (make-gcable-macptr $flags_DisposeRwLock)
    (ff-call (%kernel-import target::kernel-import-rwlock-new)
             :address))))
  
(defun make-recursive-lock ()
  (make-lock nil))

(defun %make-lock (pointer name)
  (gvector :lock pointer 'recursive-lock 0 name nil nil))

(defun make-lock (&optional name)
  "Create and return a lock object, which can be used for synchronization
between threads."
  (%make-lock (%make-recursive-lock-ptr) name))

(defun lock-name (lock)
  (uvref (require-type lock 'lock) target::lock.name-cell))

(defun recursive-lock-ptr (r)
  (if (and (eq target::subtag-lock (typecode r))
           (eq (%svref r target::lock.kind-cell) 'recursive-lock))
    (%svref r target::lock._value-cell)
    (report-bad-arg r 'recursive-lock)))

(defun recursive-lock-whostate (r)
  (if (and (eq target::subtag-lock (typecode r))
           (eq (%svref r target::lock.kind-cell) 'recursive-lock))
    (or (%svref r target::lock.whostate-cell)
        (setf (%svref r target::lock.whostate-cell)
              (%lock-whostate-string "Lock wait" r)))
    (if (typep r 'string)
      r
      (report-bad-arg r 'recursive-lock))))


(defun read-write-lock-ptr (rw)
  (if (and (eq target::subtag-lock (typecode rw))
           (eq (%svref rw target::lock.kind-cell) 'read-write-lock))
    (%svref rw target::lock._value-cell)
    (report-bad-arg rw 'read-write-lock)))

(defun make-read-write-lock ()
  "Create and return a read-write lock, which can be used for
synchronization between threads."
  (gvector :lock (%make-rwlock-ptr) 'read-write-lock 0 nil nil nil))

(defun rwlock-read-whostate (rw)
  (if (and (eq target::subtag-lock (typecode rw))
           (eq (%svref rw target::lock.kind-cell) 'read-write-lock))
    (or (%svref rw target::lock.whostate-cell)
        (setf (%svref rw target::lock.whostate-cell)
              (%lock-whostate-string "Read lock wait" rw)))
    (report-bad-arg rw 'read-write-lock)))

(defun rwlock-write-whostate (rw)
  (if (and (eq target::subtag-lock (typecode rw))
           (eq (%svref rw target::lock.kind-cell) 'read-write-lock))
    (or (%svref rw target::lock.whostate-2-cell)
        (setf (%svref rw target::lock.whostate-2-cell)
              (%lock-whostate-string "Write lock wait" rw)))
    (report-bad-arg rw 'read-write-lock)))
  

(defun %make-semaphore-ptr ()
  (let* ((p (ff-call (%kernel-import target::kernel-import-new-semaphore)
	     :signed-fullword 0
             :address)))
    (if (%null-ptr-p p)
      (error "Can't create semaphore.")
      (record-system-lock
       (%setf-macptr
	(make-gcable-macptr $flags_DisposeSemaphore)
	p)))))

(defun make-semaphore ()
  "Create and return a semaphore, which can be used for synchronization
between threads."
  (%istruct 'semaphore (%make-semaphore-ptr)))

(defun semaphorep (x)
  (istruct-typep x 'semaphore))

(setf (type-predicate 'semaphore) 'semaphorep)

(defun make-list (size &key initial-element)
  "Constructs a list with size elements each set to value"
  (unless (and (typep size 'fixnum)
               (>= (the fixnum size) 0))
    (report-bad-arg size '(and fixnum unsigned-byte)))
  (locally (declare (fixnum size))
    (if (>= size (ash 1 16))
      (values (%allocate-list initial-element size))
      (do* ((result '() (cons initial-element result)))
           ((zerop size) result)
        (decf size)))))

; end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-aprims.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-error.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(defparameter *error-format-strings* 
  '((1 . "Unbound variable: ~S .")
    (2 . "Invalid reference to ~s at index ~s.")
    (3 . "Too many arguments.")
    (4 . "Too few arguments.")
    (5 . "Argument ~S is not of the required type.")
    (6 . "Undefined function: ~S .")
    (7 . "Invalid assignment of ~s at index ~s, to ~s.")
    (8 . "Can't coerce ~S to ~S")
    (9 . "Funcallable instance ~S was called with args ~s, but has no FUNCALLABLE-INSTANCE-FUNCTION")
    (10 . "Out of memory.")
    (11 . "Default image file not found.")
    (12 . "No translation for ~S")
    (13 . "~S can't be FUNCALLed or APPLYed.")
    (14 . "~S is not a symbol or lambda expression")
    (15 . "Declaration ~S in unexpected position")
    (16 . "Can't setq constant ~S")
    (17 . "Odd number of forms to setq in ~S")
    (18 . "Illegal arg to setq ~S")
    (19 . "~S is not a symbol.")
    (20 . "~S is a constant.")
    (21 . "Bad initialization form: ~S")
    (22 . "Symbol macro ~S is declared or proclaimed special")
    (23 . "Too many arguments in ~S")
    (24 . "Local macro cannot reference lexically defined variable ~S")
    (25 . "Local macro cannot reference lexically defined function ~S")
    (26 . "Local macro cannot reference lexically defined tag ~S")
    (27 . "Local macro cannot reference lexically defined block ~S")
    (28 . "Can't find tag ~S")
    (29 . "Duplicate tag ~S")
    (30 . "Can't find block ~S")
    (31 . "Bad lambda list  ~S.")
    (32 . "~S is not a valid lambda expression.")
    (33 . "Can't throw to tag ~S .")
    (34 . "Object ~S is not of type ~S.")
    (35 . "FUNCTION can't reference lexically defined macro ~S")
    (36 . "Unimplemented FPU instruction ~^~S.")
    (41 . "Unmatched ')'.")
    (42 . "~S and ~S must be on the same volume.")
    (43 . "Filename ~S contains illegal character ~S")
    (44 . "Illegal use of wildcarded filename ~S")
    (45 . "~S is not a FASL or TEXT file.")
    (46 . "Cannot rename directory to file ~S")
    (47 . "Found a directory instead of a file or vice versa ~S")
    (48 . "Cannot copy directories: ~S")
    (49 . "String too long for pascal record")
    (50 . "Cannot create ~S")
    (64 . "Floating point overflow")
    (66 . "Can't divide by zero.")
    (75 . "Stack overflow. Bytes requested: ~d")
    (76 . "Memory allocation request failed.")
    (77 . "~S exceeds array size limit of ~S bytes.")
    (94. "Printer error.")
    (95. "Can't load printer.")
    (96. "Can't get printer parameters.")
    (97. "Can't start up printer job.")
    (98. "Floating point exception.")
    (111 . "Unexpected end of file encountered.")
    (112 . "Array index ~S out of bounds for ~S .")
    (113 . "Reader error: ~S encountered.")
    (114 . "Reader error: Unknown reader macro character ~S .")
    (115 . "Can't redefine constant ~S .")
    (116 . "Reader error: Illegal character ~S .")
    (117 . "Reader error: Illegal symbol syntax.")
    (118 . "Reader error: Dot context error.")
    (119 . "Reader error: Bad value ~S for *READ-BASE*.")
    (120 . "Can't construct argument list from ~S.")
    (121 . "Wrong FASL version.")
    (122 . "Not a FASL file.")
    (123 . "Undefined function ~s called with arguments ~s.")
    (124 . "Image file incompatible with current version of Lisp.")
    (127 .   "Using ~S in ~S ~%would cause name conflicts with symbols inherited by that package: ~%~:{~S  ~S~%~}")
    (128 .   "Importing ~S to ~S would conflict with inherited symbol ~S ." )
    (129 .   "Reader error: Malformed number in a #b/#o/#x/#r macro." )
    (130 .   "There is no package named ~S ." )
    (131 .   "Reader error: No external symbol named ~S in package ~S ." )
    (132 .   "Bad FASL file: internal inconsistency detected." )
    (133 .   "Importing ~S to ~S would conflict with symbol ~S ." )
    (134 .   "Uninterning ~S from ~S would cause conflicts among ~S ." )
    (135 .   "~S is not accessible in ~S ." )
    (136 .   "Exporting ~S in ~S would cause a name conflict with ~S in ~S ." )
    (137 .   "Using ~S in ~S ~%would cause name conflicts with symbols already present in that package: ~%~:{~S  ~S~%~}")
    (139 .   "Reader macro function ~S called outside of reader." )
    (140 .   "Reader error: undefined character ~S in a ~S dispatch macro." )
    (141 .   "Reader dispatch macro character ~S doesn't take an argument." )
    (142 .   "Reader dispatch macro character ~S requires an argument." )
    (143 .   "Reader error: Bad radix in #R macro." )
    (144 .   "Reader error: Duplicate #~S= label." )
    (145 .   "Reader error: Missing #~S# label." )
    (146 .   "Reader error: Illegal font number in #\\ macro." )
    (147 .   "Unknown character name ~S in #\\ macro." )
    (148 .   "~S cannot be accessed with ~S subscripts." )
    (149 .   "Requested size is too large to displace to ~S ." )
    (150 .   "Too many elements in argument list ~S ." )
    (151 .    "Arrays are not of the same size" )
    (152 . "Conflicting keyword arguments : ~S ~S, ~S ~S .")
    (153 . "Incorrect keyword arguments in ~S .")
    (154 . "Two few arguments in form ~S .")
    (155 . "Too many arguments in form ~S .")
    (157 . "The value ~S is not of the expected type ~S.")
    (158 . "~S is not a structure.")
    (159 . "Access to slot ~S of structure ~S is out of bounds.")
    (160 . "Form ~S does not match lambda list ~A .")
    (161 . "Temporary number space exhausted.")
    (163 . "Illegal #+/- expression ~S.")
    (164 . "File ~S does not exist.")
    (165 . "~S argument ~S is not of the required type.")
    (166 . "~S argument ~S is not of type ~S.")
    (167 . "Too many arguments in ~S.")
    (168 . "Too few arguments in ~S.")
    (169 . "Arguments don't match lambda list in ~S.")
    (170 . "~S is not a proper list.")
    (171 . "~S is not an array with a fill pointer.")
    (172 . "~S is not an adjustable array.")
    (173 . "Can't access component ~D of ~S.")
    (174 . "~S doesn't match array element type of ~S.")
    (175 . "Stack group ~S is exhausted.")
    (176 . "Stack group ~S called with arguments ~:S; exactly 1 argument accepted.")
    (177 . "Attempt to return too many values.")
    (178 . "Can't dynamically bind ~S. ")
    (200 . "Foreign exception: ~S. ")))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-error.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-numbers.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;;
;;; level-0;l0-numbers.lisp

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (require "LISPEQU")
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")



  (defvar *dfloat-dops* '((* . %double-float*-2!)(/ . %double-float/-2!)
			  (+ . %double-float+-2!)(- . %double-float--2!)))
  
  (defvar *sfloat-dops* '((* . %short-float*-2!)(/ . %short-float/-2!)
			  (+ . %short-float+-2!)(- . %short-float--2!)))

  (defmacro dfloat-rat (op x y &optional (destructive-op (cdr (assq op *dfloat-dops*))))
    (if destructive-op
	(let ((f2 (gensym)))
	  `(let ((,f2 (%double-float ,y (%make-dfloat))))
	    (,destructive-op ,x ,f2 ,f2)))          
	`(,op (the double-float ,x) (the double-float (%double-float ,y)))))

  (defmacro rat-dfloat (op x y &optional (destructive-op (cdr (assq op *dfloat-dops*))))
    (if destructive-op
	(let ((f1 (gensym)))
	  `(let ((,f1 (%double-float ,x (%make-dfloat)))) 
	    (,destructive-op ,f1 ,y ,f1)))
	`(,op (the double-float (%double-float ,x)) (the double-float ,y))))

  (defmacro sfloat-rat (op x y &optional (destructive-op (cdr (assq op *sfloat-dops*))))
    (let* ((use-destructive-op
            (target-word-size-case
             (32 destructive-op)
             (64 nil))))
      (if use-destructive-op
	(let ((f2 (gensym)))
	  `(let ((,f2 (%short-float ,y (%make-sfloat)))) 
	    (,destructive-op ,x ,f2 ,f2)))
	`(,op (the short-float ,x) (the short-float (%short-float ,y))))))

  (defmacro rat-sfloat (op x y &optional (destructive-op (cdr (assq op *sfloat-dops*))))
    (let* ((use-destructive-op
            (target-word-size-case
             (32 destructive-op)
             (64 nil))))
      (if use-destructive-op
        (let ((f1 (gensym)))
          `(let ((,f1 (%short-float ,x (%make-sfloat)))) 
            (,destructive-op ,f1 ,y ,f1)))
        `(,op (the short-float (%short-float ,x)) (the short-float ,y)))))


  


  (declaim (inline  %make-complex %make-ratio))
  (declaim (inline canonical-complex))
  (declaim (inline build-ratio))
  (declaim (inline maybe-truncate)))



(defun %make-complex (realpart imagpart)
  (gvector :complex realpart imagpart))

(defun %make-ratio (numerator denominator)
  (gvector :ratio numerator denominator))
 


; this is no longer used
(defun %integer-signum (num)
  (if (fixnump num)
    (%fixnum-signum num)
    ; there is no such thing as bignum zero we hope
    (if (bignum-minusp num) -1 1)))


; Destructive primitives.
(macrolet ((defdestructive-df-op (destructive-name op)
             `(progn
                (defun ,destructive-name (x y result)
                  (declare (double-float x y result))
                  (%setf-double-float result (the double-float (,op (the double-float x) (the double-float y))))))))
  (defdestructive-df-op %double-float+-2! +)
  (defdestructive-df-op %double-float--2! -)
  (defdestructive-df-op %double-float*-2! *)
  (defdestructive-df-op %double-float/-2! /))

#-64-bit-target
(macrolet ((defdestructive-sf-op (destructive-name op)
             `(progn
                (defun ,destructive-name (x y result)
                  (declare (short-float x y result))
                  (%setf-short-float result (the short-float (,op x y)))))))
  (defdestructive-sf-op %short-float+-2! +)
  (defdestructive-sf-op %short-float--2! -)
  (defdestructive-sf-op %short-float*-2! *)
  (defdestructive-sf-op %short-float/-2! /))


(defun %negate (x)
  (number-case x
    (fixnum  (- (the fixnum x)))
    (double-float  (%double-float-negate! x (%make-dfloat)))
    (short-float
     #+32-bit-target (%short-float-negate! x (%make-sfloat))
     #+64-bit-target (%short-float-negate x))
    (bignum (negate-bignum x))
    (ratio (%make-ratio (%negate (%numerator x)) (%denominator x)))
    (complex (complex (%negate (%realpart X))(%negate (%imagpart X))) )))

(defun %double-float-zerop (n)
  (zerop (the double-float n)))

(defun %short-float-zerop (n)
  (zerop (the single-float n)))

(defun zerop (number)
  "Is this number zero?"
  (number-case number
    (integer (eq number 0))
    (short-float (%short-float-zerop number))
    (double-float (%double-float-zerop number))
    (ratio nil)
    (complex
     (number-case (%realpart number)
       (short-float (and (%short-float-zerop (%realpart number))
                         (%short-float-zerop (%imagpart number))))
       (double-float (and (%double-float-zerop (%realpart number))
                          (%double-float-zerop (%imagpart number))))
       (t (and (eql 0 (%realpart number))(eql 0 (%imagpart number))))))))

(defun %short-float-plusp (x)
  (> (the single-float x) 0.0f0))

(defun %double-float-plusp (x)
  (> (the double-float x) 0.0d0))

(defun plusp (number)
  "Is this real number strictly positive?"
  (number-case number
    (fixnum (%i> number 0))
    (bignum (bignum-plusp number))
    (short-float (%short-float-plusp number))
    (double-float (%double-float-plusp number))
    (ratio (plusp (%numerator number)))))


(defun minusp (number)
  "Is this real number strictly negative?"
  (number-case number
    (fixnum (%i< number 0))
    (bignum (bignum-minusp number))
    (short-float (%short-float-minusp number))
    (double-float (%double-float-minusp number))
    (ratio (minusp (%numerator number)))))


(defun oddp (n)
  "Is this integer odd?"
  (case (typecode n)
    (#.target::tag-fixnum (logbitp 0 (the fixnum n)))
    (#.target::subtag-bignum (%bignum-oddp n))
    (t (report-bad-arg n 'integer))))

(defun evenp (n)
  "Is this integer even?"
  (case (typecode n)
    (#.target::tag-fixnum (not (logbitp 0 (the fixnum n))))
    (#.target::subtag-bignum (not (%bignum-oddp n)))
    (t (report-bad-arg n 'integer))))

;; expansion slightly changed
(defun =-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (eq x y))
              (double-float (eq 0 (fixnum-dfloat-compare x y)))
              (short-float (eq 0 (fixnum-sfloat-compare x y)))
              ((bignum ratio) nil)
              (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (double-float (number-case y ; x
                    (double-float (= (the double-float x)(the double-float y))) ;x 
                    (short-float (with-stack-double-floats ((dy y))
                                   (= (the double-float x) (the double-float dy))))
                    (fixnum (eq 0 (fixnum-dfloat-compare  y x)))
                    (bignum (eq 0 (bignum-dfloat-compare y x)))
                    (ratio (= (rational x) y))
                    (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (short-float (number-case y
                   (short-float (= (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (= (the double-float dx) (the double-float y))))
                   (fixnum (eq 0 (fixnum-sfloat-compare y x)))
                   (bignum (eq 0 (bignum-sfloat-compare y x)))
                   (ratio (= (rational x) y))
                   (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (bignum (number-case y 
              (bignum (eq 0 (bignum-compare x y)))
              ((fixnum ratio) nil)
              (double-float (eq 0 (bignum-dfloat-compare x y)))
              (short-float (eq 0 (bignum-sfloat-compare x y)))
              (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (ratio (number-case y
             (integer nil)
             (ratio
              (and (eql (%numerator x) (%numerator y))
                   (eql (%denominator x) (%denominator y))))
             (float (= x (rational y)))
             (complex (and (zerop (%imagpart y)) (= x (%realpart y))))))
    (complex (number-case y
               (complex (and (= (%realpart x) (%realpart y))
                             (= (%imagpart x) (%imagpart y))))
               ((float rational)
                (and (zerop (%imagpart x)) (= (%realpart x) y)))))))

(defun /=-2 (x y)
  (declare (notinline =-2))
  (not (= x y)))


; true iff (< x y) is false.
(defun >=-2 (x y)
  (declare (notinline <-2))
  (not (< x y)))



(defun <=-2 (x y)
  (declare (notinline >-2))
  (not (> x y)))

(defun <-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (< (the fixnum x) (the fixnum y)))
              (double-float (eq -1 (fixnum-dfloat-compare x y)))
              (short-float (eq -1 (fixnum-sfloat-compare x y)))
              (bignum (bignum-plusp y))
              (ratio (< x (ceiling (%numerator y)(%denominator y))))))
    (double-float (number-case y ; x
                    (double-float (< (the double-float x)(the double-float y))) ;x
                    (short-float (with-stack-double-floats ((dy y))
                                   (< (the double-float x) (the double-float dy))))
                    (fixnum (eq 1 (fixnum-dfloat-compare  y x)))
                    (bignum (eq 1 (bignum-dfloat-compare y x)))
                    (ratio (< (rational x) y))))
    (short-float (number-case y
                    (short-float (< (the short-float x) (the short-float y)))
                    (double-float (with-stack-double-floats ((dx x))
                                    (< (the double-float dx) (the double-float y))))
                    (fixnum (eq 1 (fixnum-sfloat-compare y x)))
                    (bignum (eq 1 (bignum-sfloat-compare y x)))
                    (ratio (< (rational x) y))))
    (bignum (number-case y 
              (bignum (EQ -1 (bignum-compare x y)))
              (fixnum (not (bignum-plusp x)))
              (ratio (< x (ceiling (%numerator y)(%denominator y))))
              (double-float (eq -1 (bignum-dfloat-compare x y)))
              (short-float (eq -1 (bignum-sfloat-compare x y)))))
    (ratio (number-case y
             (integer (< (floor (%numerator x)(%denominator x)) y))
             (ratio
              (< (* (%numerator (the ratio x))
                    (%denominator (the ratio y)))
                 (* (%numerator (the ratio y))
                    (%denominator (the ratio x)))))
             (float (< x (rational y)))))))



(defun >-2 (x y)
  ;(declare (optimize (speed 3)(safety 0)))
  (number-case x
    (fixnum (number-case y
              (fixnum (> (the fixnum x) (the fixnum y)))
              (bignum (not (bignum-plusp y)))
              (double-float (eq 1 (fixnum-dfloat-compare x y)))
              (short-float (eq 1 (fixnum-sfloat-compare x y)))
              ;; or (> (* x denom) num) ?
              (ratio (> x (floor (%numerator y) (%denominator y))))))
    (double-float (number-case y
                    (double-float (> (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (> (the double-float x) (the double-float dy))))
                    (fixnum (eq -1 (fixnum-dfloat-compare  y x)))
                    (bignum (eq -1 (bignum-dfloat-compare y x)))
                    (ratio (> (rational x) y))))
    (short-float (number-case y
                    (short-float (> (the short-float x) (the short-float y)))
                    (double-float (with-stack-double-floats ((dx x))
                                   (> (the double-float dx) (the double-float y))))
                    (fixnum (eq -1 (fixnum-sfloat-compare  y x)))
                    (bignum (eq -1 (bignum-sfloat-compare y x)))
                    (ratio (> (rational x) y))))
    (bignum (number-case y
              (fixnum (bignum-plusp x))
              (bignum (eq 1 (bignum-compare x y)))
              ;; or (> (* x demon) num)
              (ratio (> x (floor (%numerator y) (%denominator y))))
              (double-float (eq 1 (bignum-dfloat-compare x y)))
              (short-float (eq 1 (bignum-sfloat-compare x y)))))
    (ratio (number-case y
             ;; or (> num (* y denom))
             (integer (> (ceiling (%numerator x) (%denominator x)) y))
             (ratio
              (> (* (%numerator (the ratio x))
                    (%denominator (the ratio y)))
                 (* (%numerator (the ratio y))
                    (%denominator (the ratio x)))))
             (float (> x (rational y)))))))


; t if any bits set after exp (unbiased)
(defun hi-lo-fraction-p (hi lo exp)
  (declare (fixnum hi lo exp))
  (if (> exp 24)
    (not (eql 0 (%ilogand lo (%ilsr (- exp 25) #xfffffff))))
    (or (not (zerop lo))(not (eql 0 (%ilogand hi (%ilsr exp #x1ffffff)))))))



(defun negate-hi-lo (hi lo)
  (setq hi (%ilogxor hi #x3ffffff))
  (if (eq 0 lo)    
    (setq hi (+ hi 1))
    (setq lo (+ (%ilogxor lo #xfffffff) 1)))
  (values hi lo))



(defun fixnum-dfloat-compare (int dfloat)
  (declare (double-float dfloat) (fixnum int))
  (if (and (eq int 0)(= dfloat 0.0d0))
    0
    (with-stack-double-floats ((d1 int))
      (locally (declare (double-float d1))
        (if (eq int (%truncate-double-float->fixnum d1))
          (cond ((< d1 dfloat) -1)
                ((= d1 dfloat) 0)
                (t 1))
          ;; Whatever we do here should have the effect
          ;; of comparing the integer to the result of calling
          ;; RATIONAL on the float.  We could probably
          ;; skip the call to RATIONAL in more cases,
          ;; but at least check the obvious ones here
          ;; (e.g. different signs)
          (multiple-value-bind (mantissa exponent sign)
              (integer-decode-double-float dfloat)
            (declare (type (integer -1 1) sign)
                     (fixnum exponent))
            (cond ((zerop int)
                   (- sign))
                  ((and (< int 0) (eql sign 1)) -1)
                  ((and (> int 0) (eql sign -1)) 1)
                  (t
                   ;; See RATIONAL.  Can probably avoid this if
                   ;; magnitudes are clearly dissimilar.
                   (if (= sign -1) (setq mantissa (- mantissa)))
                   (let* ((rat (if (< exponent 0)
                                 (/ mantissa (ash 1 (the fixnum (- exponent))))
                                 (ash mantissa exponent))))
                     (if (< int rat)
                       -1
                       (if (eq int rat)
                         0
                         1)))))))))))



(defun fixnum-sfloat-compare (int sfloat)
  (declare (short-float sfloat) (fixnum int))
  (if (and (eq int 0)(= sfloat 0.0s0))
    0
    (#+32-bit-target target::with-stack-short-floats #+32-bit-target ((s1 int))
     #-32-bit-target let* #-32-bit-target ((s1 (%int-to-sfloat int)))
                     (locally
                         (declare (short-float s1))
                       (if (eq (%truncate-short-float->fixnum s1) int)
                         (cond ((< s1 sfloat) -1)
                               ((= s1 sfloat) 0)
                               (t 1))
                         ;; Whatever we do here should have the effect
                         ;; of comparing the integer to the result of calling
                         ;; RATIONAL on the float.  We could probably
                         ;; skip the call to RATIONAL in more cases,
                         ;; but at least check the obvious ones here
                         ;; (e.g. different signs)
                         (multiple-value-bind (mantissa exponent sign)
                             (integer-decode-short-float sfloat)
                           (declare (type (integer -1 1) sign)
                                    (fixnum exponent))
                           (cond ((zerop int)
                                  (- sign))
                                 ((and (< int 0) (eql sign 1)) -1)
                                 ((and (> int 0) (eql sign -1)) 1)
                                 (t
                                  ;; See RATIONAL.  Can probably avoid this if
                                  ;; magnitudes are clearly dissimilar.
                                  (if (= sign -1) (setq mantissa (- mantissa)))
                                  (let* ((rat (if (< exponent 0)
                                                (/ mantissa (ash 1 (the fixnum (- exponent))))
                                                (ash mantissa exponent))))
                                    (if (< int rat)
                                      -1
                                      (if (eq int rat)
                                        0
                                        1)))))))))))


        
;;; lotta stuff to avoid making a rational from a float
;;; returns -1 less, 0 equal, 1 greater
(defun bignum-dfloat-compare (int float)
  (cond 
   ((and (eq int 0)(= float 0.0d0)) 0)
   (t
    (let* ((fminus  (%double-float-minusp float))
           (iminus (minusp int))
           (gt (if iminus -1 1)))
      (declare (fixnum gt))
      (if (neq fminus iminus)
        gt  ; if different signs, done
        (let ((intlen (integer-length int)) 
              (exp (- (the fixnum (%double-float-exp float)) 1022)))
          (declare (fixnum intlen exp))
          (cond 
           ((and (not fminus) (< intlen exp)) -1)
           ((> intlen exp)  gt)   ; if different exp, done
           ((and fminus (or (< (1+ intlen) exp)
                            (and (= (1+ intlen) exp)
                                 (neq (one-bignum-factor-of-two int) intlen))))
            ;(print 'zow)
            (the fixnum (- gt)))  ; ; integer-length is strange for neg powers of 2            
           (t (multiple-value-bind (hi lo)(fixnum-decode-float float)
                (declare (fixnum hi lo)) 
                (when fminus (multiple-value-setq (hi lo)(negate-hi-lo hi lo)))
                (let* ((sz 26)  ; exp > 28 always
                       (pos (- exp 25))
                       (big-bits (%ldb-fixnum-from-bignum int sz pos)))
                  (declare (fixnum pos big-bits sz))
                  ;(print (list big-bits hi sz pos))
                  (cond 
                   ((< big-bits hi) -1)
                   ((> big-bits hi) 1)
                   (t (let* ((sz (min (- exp 25) 28))
                             (pos (- exp 25 sz)) ; ?
                             (ilo (if (< exp 53) (ash lo (- exp 53)) lo))                                    
                             (big-bits (%ldb-fixnum-from-bignum int sz pos)))
                        (declare (fixnum pos sz ilo big-bits))
                        ;(PRINT (list big-bits ilo))
                        (cond
                         ((< big-bits ilo) -1)
                         ((> big-bits ilo) 1)
                         ((eq exp 53) 0)
                         ((< exp 53)
                          (if (not (hi-lo-fraction-p hi lo exp)) 0 -1)) ; -1 if pos 
                         (t (if (%i< (one-bignum-factor-of-two int) (- exp 53)) 1 0)))))))
                )))))))))



;;; I don't know if it's worth doing a more "real" version of this.
(defun bignum-sfloat-compare (int float)
  (with-stack-double-floats ((df float))
    (bignum-dfloat-compare int df)))

;;;; Canonicalization utilities:

;;; CANONICAL-COMPLEX  --  Internal
;;;
;;;    If imagpart is 0, return realpart, otherwise make a complex.  This is
;;; used when we know that realpart and imagpart are the same type, but
;;; rational canonicalization might still need to be done.
;;;

(defun canonical-complex (realpart imagpart)
  (if (eql imagpart 0)
    realpart
    (%make-complex realpart imagpart)))




(defun +-2 (x y)     
  (number-case x
    (fixnum (number-case y
              (fixnum (+ (the fixnum x) (the fixnum y)))
              (double-float (rat-dfloat + x y))
              (short-float (rat-sfloat + x y))
              (bignum (add-bignum-and-fixnum y x))
              (complex (complex (+ x (%realpart y))
                                (%imagpart y)))
              (ratio (let* ((dy (%denominator y)) 
                            (n (+ (* x dy) (%numerator y))))
                       (%make-ratio n dy)))))
    (double-float (number-case y
                    (double-float (+ (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (+ (the double-float x) (the double-float dy))))
                    (rational (dfloat-rat + x y))
                    (complex (complex (+ x (%realpart y)) 
                                      (%imagpart y)))))
    (short-float (number-case y                                
                   (short-float (+ (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (+ (the double-float dx) (the double-float y))))
                   (rational (sfloat-rat + x y))
                   (complex (complex (+ x (%realpart y))
                                     (%imagpart y)))))
    (bignum (number-case y
              (bignum (add-bignums x y))
              (fixnum (add-bignum-and-fixnum x y))
              (double-float (rat-dfloat + x y))
              (short-float (rat-sfloat + x y))
              (complex (complex (+ x (realpart y)) 
                                (%imagpart y)))
              (ratio
               (let* ((dy (%denominator y))
                      (n (+ (* x dy) (%numerator y))))
                 (%make-ratio n dy)))))
    (complex (number-case y
               (complex (complex (+ (%realpart x) (%realpart y))
                                 (+ (%imagpart x) (%imagpart y))))
               ((rational float) (complex (+ (%realpart x) y) (%imagpart x)))))
    (ratio (number-case y
             (ratio
              (let* ((nx (%numerator x))
                     (dx (%denominator x))
                     (ny (%numerator y))
                     (dy (%denominator y))
                     (g1 (gcd dx dy)))
                (if (eql g1 1)
                  (%make-ratio (+ (* nx dy) (* dx ny)) (* dx dy))
                  (let* ((t1 (+ (* nx (truncate dy g1)) (* (truncate dx g1) ny)))
                         (g2 (gcd t1 g1))
                         (t2 (truncate dx g1)))
                    (cond ((eql t1 0) 0)
                          ((eql g2 1) (%make-ratio t1 (* t2 dy)))
                          (t
                           (let* ((nn (truncate t1 g2))
                                  (t3 (truncate dy g2))
                                  (nd (if (eql t2 1) t3 (* t2 t3))))
                             (if (eql nd 1) nn (%make-ratio nn nd)))))))))
             (integer
              (let* ((dx (%denominator x)) (n (+ (%numerator x) (* y dx))))
                (%make-ratio n dx)))
             (double-float (rat-dfloat + x y))
             (short-float (rat-sfloat + x y))
             (complex (complex (+ x (%realpart y)) 
                               (%imagpart y)))))))

(defun --2 (x y)     
  (number-case x
    (fixnum (number-case y
              (fixnum (- (the fixnum x) (the fixnum y)))
              (double-float (rat-dfloat - x y))
              (short-float (rat-sfloat - x y))
              (bignum 
               (with-small-bignum-buffers ((bx x))
                        (subtract-bignum bx y)))
              (complex (complex (- x (%realpart y))
                                (- (%imagpart y))))
              (ratio (let* ((dy (%denominator y)) 
                            (n (- (* x dy) (%numerator y))))
                       (%make-ratio n dy)))))
    (double-float (number-case y
                    (double-float (- (the double-float x) (the double-float y)))
                    (short-float (with-stack-double-floats ((dy y))
                                   (- (the double-float x) (the double-float dy))))
                    (rational (dfloat-rat - x y))
                    (complex (complex (- x (%realpart y)) 
                                      (- (%imagpart y))))))
    (short-float (number-case y                                
                   (short-float (- (the short-float x) (the short-float y)))
                   (double-float (with-stack-double-floats ((dx x))
                                   (- (the double-float dx) (the double-float y))))
                   (rational (sfloat-rat - x y))
                   (complex (complex (- x (%realpart y))
                                     (- (%imagpart y))))))
    (bignum (number-case y
              (bignum (subtract-bignum x y))
              (fixnum (if (eql y target::target-most-negative-fixnum)
                        (with-small-bignum-buffers ((by y))
                          (subtract-bignum x by))
                        (add-bignum-and-fixnum x (- y))))
              (double-float (rat-dfloat - x y))
              (short-float (rat-sfloat - x y))
              (complex (complex (- x (realpart y)) 
                                (- (%imagpart y))))
              (ratio
               (let* ((dy (%denominator y))
                      (n (- (* x dy) (%numerator y))))
                 (%make-ratio n dy)))))
    (complex (number-case y
               (complex (complex (- (%realpart x) (%realpart y))
                                 (- (%imagpart x) (%imagpart y))))
               ((rational float) (complex (- (%realpart x) y) (%imagpart x)))))
    (ratio (number-case y
             (ratio
              (let* ((nx (%numerator x))
                     (dx (%denominator x))
                     (ny (%numerator y))
                     (dy (%denominator y))
                     (g1 (gcd dx dy)))
                (if (eql g1 1)
                  (%make-ratio (- (* nx dy) (* dx ny)) (* dx dy))
                  (let* ((t1 (- (* nx (truncate dy g1)) (* (truncate dx g1) ny)))
                         (g2 (gcd t1 g1))
                         (t2 (truncate dx g1)))
                    (cond ((eql t1 0) 0)
                          ((eql g2 1) (%make-ratio t1 (* t2 dy)))
                          (t
                           (let* ((nn (truncate t1 g2))
                                  (t3 (truncate dy g2))
                                  (nd (if (eql t2 1) t3 (* t2 t3))))
                             (if (eql nd 1) nn (%make-ratio nn nd)))))))))
             (integer
              (let* ((dx (%denominator x)) (n (- (%numerator x) (* y dx))))
                (%make-ratio n dx)))
             (double-float (rat-dfloat - x y))
             (short-float (rat-sfloat - x y))
             (complex (complex (- x (%realpart y)) 
                               (- (%imagpart y))))))))


;;; BUILD-RATIO  --  Internal
;;;
;;;    Given a numerator and denominator with the GCD already divided out, make
;;; a canonical rational.  We make the denominator positive, and check whether
;;; it is 1.
;;;

(defun build-ratio (num den)
  (if (minusp den) (setq num (- num) den (- den)))
  (case den
    (0 (divide-by-zero-error 'build-ratio num den))
    (1 num)
    (t (%make-ratio num den))))




;;; MAYBE-TRUNCATE  --  Internal
;;;
;;;    Truncate X and Y, but bum the case where Y is 1.
;;;


(defun maybe-truncate (x y)
  (if (eql y 1)
    x
    (truncate x y)))


(defun *-2 (x y)
  ;(declare (optimize (speed 3)(safety 0)))
  (flet ((integer*ratio (x y)
	   (if (eql x 0) 0
	       (let* ((ny (%numerator y))
		      (dy (%denominator y))
		      (gcd (gcd x dy)))
		 (if (eql gcd 1)
		     (%make-ratio (* x ny) dy)
		     (let ((nn (* (truncate x gcd) ny))
			   (nd (truncate dy gcd)))
		       (if (eql nd 1)
			   nn
			   (%make-ratio nn nd)))))))
	 (complex*real (x y)
	   (complex (* (%realpart x) y) (* (%imagpart x) y))))
    (number-case x
      (double-float (number-case y
                      (double-float (* (the double-float x)(the double-float y)))
                      (short-float (with-stack-double-floats ((dy y))
                                     (* (the double-float x) (the double-float dy))))
                      (rational (dfloat-rat * x y))
                      (complex (complex*real y x))))
      (short-float (number-case y
                      (double-float (with-stack-double-floats ((dx x))
                                     (* (the double-float dx) (the double-float y))))
                      (short-float (* (the short-float x) (the short-float y)))
                      (rational (sfloat-rat * x y))
                      (complex (complex*real y x))))
      (bignum (number-case y
                (fixnum
                 (if (eql y target::target-most-negative-fixnum)
                   (with-small-bignum-buffers ((by y))
                     (multiply-bignums x by))
                   (multiply-bignum-and-fixnum x y)))
                (bignum (multiply-bignums x y))
                (double-float (dfloat-rat * y x))
                (short-float (sfloat-rat * y x))
                (ratio (integer*ratio x y))
                (complex (complex*real y x))))
      (fixnum (number-case y
                (bignum (if (eql x target::target-most-negative-fixnum)
                          (with-small-bignum-buffers ((bx x))
                            (multiply-bignums y bx))
                          (multiply-bignum-and-fixnum y x)))
                (fixnum (multiply-fixnums (the fixnum x) (the fixnum y)))
                (short-float (sfloat-rat * y x))
                (double-float (dfloat-rat * y x))
                (ratio (integer*ratio x y))
                (complex (complex*real y x))))
      (complex (number-case y
                 (complex (let* ((rx (%realpart x))
	                         (ix (%imagpart x))
	                         (ry (%realpart y))
	                         (iy (%imagpart y)))
	                    (complex (- (* rx ry) (* ix iy)) (+ (* rx iy) (* ix ry)))))
                 (real (complex*real x y))))
      (ratio (number-case y
               (ratio (let* ((nx (%numerator x))
	                     (dx (%denominator x))
	                     (ny (%numerator y))
	                     (dy (%denominator y))
	                     (g1 (gcd nx dy))
	                     (g2 (gcd dx ny)))
	                (build-ratio (* (maybe-truncate nx g1)
			                (maybe-truncate ny g2))
		                     (* (maybe-truncate dx g2)
			                (maybe-truncate dy g1)))))
               (integer (integer*ratio y x))
               (double-float (rat-dfloat * x y))
               (short-float (rat-sfloat * x y))
               (complex (complex*real y x)))))))



(defun integer*integer (x y &optional res)
  (declare (ignore res))
  (number-case x      
      (fixnum (number-case y
                (fixnum (* (the fixnum x) (the fixnum y)))
                (t (multiply-bignum-and-fixnum y x))))
      (bignum (number-case y
                (fixnum (multiply-bignum-and-fixnum x y))
                (t (multiply-bignums x y))))))



  

;;; INTEGER-/-INTEGER  --  Internal
;;;
;;;    Divide two integers, producing a canonical rational.  If a fixnum, we
;;; see if they divide evenly before trying the GCD.  In the bignum case, we
;;; don't bother, since bignum division is expensive, and the test is not very
;;; likely to suceed.
;;;
(defun integer-/-integer (x y)
  (if (and (typep x 'fixnum) (typep y 'fixnum))
    (multiple-value-bind (quo rem) (%fixnum-truncate x y)
      (if (eql 0 rem)
        quo
        (let ((gcd (gcd x y)))
          (declare (fixnum gcd))
          (if (eql gcd 1)
            (build-ratio x y)
            (build-ratio (%fixnum-truncate x gcd) (%fixnum-truncate y gcd))))))
      (let ((gcd (gcd x y)))
        (if (eql gcd 1)
          (build-ratio x y)
          (build-ratio (truncate x gcd) (truncate y gcd))))))



(defun /-2 (x y)
  (macrolet ((real-complex-/ (x y)
	       (let ((ry (gensym))
		     (iy (gensym))
		     (r (gensym))
		     (dn (gensym)))
		 `(let* ((,ry (%realpart ,y))
			 (,iy (%imagpart ,y)))
		    (if (> (abs ,ry) (abs ,iy))
		      (let* ((,r (/ ,iy ,ry))
			     (,dn (* ,ry (+ 1 (* ,r ,r)))))
			(complex (/ ,x ,dn)
                                 (/ (- (* ,x ,r)) ,dn)))
		      (let* ((,r (/ ,ry ,iy))
			     (,dn (* ,iy (+ 1 (* ,r ,r)))))
			(complex (/ (* ,x ,r) ,dn)
                                 (/ (- ,x) ,dn))))))))
    (number-case x
      (double-float (number-case y
		      (double-float (/ (the double-float x) (the double-float y)))
		      (short-float (with-stack-double-floats ((dy y))
				     (/ (the double-float x)
					(the double-float dy))))
		      (rational (dfloat-rat / x y))
		      (complex (real-complex-/ x y))))
      (short-float (number-case y
		     (short-float (/ (the short-float x) (the short-float y)))
		     (double-float (with-stack-double-floats ((dx x))
				     (/ (the double-float dx)
					(the double-float y))))
		     (rational (sfloat-rat / x y))
		     (complex (real-complex-/ x y))))
      (integer (number-case y
		 (double-float (rat-dfloat / x y))
		 (short-float (rat-sfloat / x y))
		 (integer (integer-/-integer x y))
		 (complex (real-complex-/ x y))
		 (ratio
		  (if (eql 0 x)
		    0
		    (let* ((ny (%numerator y)) 
			   (dy (%denominator y)) 
			   (gcd (gcd x ny)))
		      (build-ratio (* (maybe-truncate x gcd) dy)
				   (maybe-truncate ny gcd)))))))
      (complex (number-case y
		 (complex (let* ((rx (%realpart x))
				 (ix (%imagpart x))
				 (ry (%realpart y))
				 (iy (%imagpart y)))
			    (if (> (abs ry) (abs iy))
			      (let* ((r (/ iy ry))
				     (dn (+ ry (* r iy))))
				(complex (/ (+ rx (* ix r)) dn)
						   (/ (- ix (* rx r)) dn)))
			      (let* ((r (/ ry iy))
				     (dn (+ iy (* r ry))))
				(complex (/ (+ (* rx r) ix) dn)
						   (/ (- (* ix r) rx) dn))))))
		 ((rational float)
		  (complex (/ (%realpart x) y) (/ (%imagpart x) y)))))
      (ratio (number-case y
	       (double-float (rat-dfloat / x y))
	       (short-float (rat-sfloat / x y))
	       (integer
		(when (eql y 0)
		  (divide-by-zero-error '/ x y))
		(let* ((nx (%numerator x)) (gcd (gcd nx y)))
		  (build-ratio (maybe-truncate nx gcd)
			       (* (maybe-truncate y gcd) (%denominator x)))))
	       (complex (real-complex-/ x y))
	       (ratio
		(let* ((nx (%numerator x))
		       (dx (%denominator x))
		       (ny (%numerator y))
		       (dy (%denominator y))
		       (g1 (gcd nx ny))
		       (g2 (gcd dx dy)))
		  (build-ratio (* (maybe-truncate nx g1)
				  (maybe-truncate dy g2))
			       (* (maybe-truncate dx g2)
				  (maybe-truncate ny g1))))))))))



(defun divide-by-zero-error (operation &rest operands)
  (error (make-condition 'division-by-zero
           :operation operation
           :operands operands)))


(defun 1+ (number)
  "Returns NUMBER + 1."
  (+-2 number 1))

(defun 1- (number)
  "Returns NUMBER - 1."
  (--2 number 1))




(defun conjugate (number)
  "Return the complex conjugate of NUMBER. For non-complex numbers, this is
  an identity."
  (number-case number
    (complex (complex (%realpart number) (- (%imagpart number))))
    (number number)))

(defun numerator (rational)
  "Return the numerator of NUMBER, which must be rational."
  (number-case rational
    (ratio (%numerator rational))
    (integer rational)))

(defun denominator (rational)
  "Return the denominator of NUMBER, which must be rational."
  (number-case rational
    (ratio (%denominator rational))
    (integer 1)))



(defun abs (number)
  "Return the absolute value of the number."
  (number-case number
   (fixnum
    (locally (declare (fixnum number))
      (if (minusp number) (- number) number)))
   (double-float
    (%double-float-abs number))
   (short-float
    (%short-float-abs number))
   (bignum
    (if (bignum-minusp number)(negate-bignum number) number))
   (ratio
    (if (minusp number) (- number) number))    
   (complex
    (let ((rx (%realpart number))
          (ix (%imagpart number)))
      (number-case rx
        (rational
         (sqrt (+ (* rx rx) (* ix ix))))
        (short-float
         (%short-float (%double-float-hypot (%double-float rx)
					    (%double-float ix))))
        (double-float
         (%double-float-hypot rx ix)))))))



(defun phase (number)
  "Return the angle part of the polar representation of a complex number.
  For complex numbers, this is (atan (imagpart number) (realpart number)).
  For non-complex positive numbers, this is 0. For non-complex negative
  numbers this is PI."
  (number-case number
    (rational
     (if (minusp number)
       (%short-float pi)
       0.0f0))
    (double-float
     (if (%double-float-sign number)
       (%double-float pi)
       0.0d0))
    (complex
     (atan (%imagpart number) (%realpart number)))
    (short-float
     (if (%short-float-sign number)
       (%short-float pi)
       0.0s0))))



; from Lib;numbers.lisp, sort of
(defun float (number &optional other)
  "Converts any REAL to a float. If OTHER is not provided, it returns a
  SINGLE-FLOAT if NUMBER is not already a FLOAT. If OTHER is provided, the
  result is the same float format as OTHER."
  (if (null other)
    (if (typep number 'float)
      number
      (%short-float number))
    (if (typep other 'double-float)
      (%double-float number)
      (if (typep other 'short-float)
        (%short-float number)
        (float number (require-type other 'float))))))





;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be negative then decrement the quotient and augment the remainder by
;;; the divisor.
;;;
(defun floor (number &optional divisor)
  "Return the greatest integer not greater than number, or number/divisor.
  The second returned value is (mod number divisor)."
  (if (null divisor)(setq divisor 1))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
               (plusp number)
               (minusp number)))
      (if (called-for-mv-p)
        (values (1- tru) (+ rem divisor))
        (1- tru))
      (values tru rem))))



(defun %fixnum-floor (number divisor)
  (declare (fixnum number divisor))
  (if (eq divisor 1)
    (values number 0)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (if (eq rem 0)
        (values tru 0)
        (locally (declare (fixnum tru rem))
          (if (and ;(not (zerop rem))
	           (if (minusp divisor)
                     (plusp number)
                     (minusp number)))
            (values (the fixnum (1- tru)) (the fixnum (+ rem divisor)))
            (values tru rem)))))))



;;; If the numbers do not divide exactly and the result of (/ number divisor)
;;; would be positive then increment the quotient and decrement the remainder by
;;; the divisor.
;;;
(defun ceiling (number &optional divisor)
  "Return the smallest integer not less than number, or number/divisor.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (multiple-value-bind (tru rem) (truncate number divisor)
    (if (and (not (zerop rem))
	     (if (minusp divisor)
               (minusp number)
               (plusp number)))
      (if (called-for-mv-p)
        (values (+ tru 1) (- rem divisor))
        (+ tru 1))
      (values tru rem))))



(defun %fixnum-ceiling (number  divisor)
  "Returns the smallest integer not less than number, or number/divisor.
  The second returned value is the remainder."
  (declare (fixnum number divisor))
  (multiple-value-bind (tru rem) (%fixnum-truncate number divisor)
    (if (eq 0 rem)
      (values tru 0)
      (locally (declare (fixnum tru rem))
        (if (and ;(not (zerop rem))
	     (if (minusp divisor)
               (minusp number)
               (plusp number)))
          (values (the fixnum (+ tru 1))(the fixnum  (- rem divisor)))
          (values tru rem))))))



(defun integer-decode-denorm-short-float (mantissa sign)
  (declare (fixnum mantissa sign))
  (do* ((bias 0 (1+ bias))
	(sig mantissa (ash sig 1)))
       ((logbitp 23 sig)
	(values sig
		(- (- IEEE-single-float-bias)
		   IEEE-single-float-digits
		   bias)
		sign))))


(defun integer-decode-short-float (sfloat)
  (multiple-value-bind (mantissa exp sign)(fixnum-decode-short-float sfloat)
    (let* ((biased (- exp IEEE-single-float-bias IEEE-single-float-digits)))
      (setq sign (if (eql 0 sign) 1 -1))
      (if (eq exp 255)
	(error "Can't decode NAN/Inf: ~s" sfloat))
      (if (eql 0 exp)
	(if (eql 0 mantissa)
	  (values 0 biased sign)
	  (integer-decode-denorm-short-float (ash mantissa 1) sign))
	(values (logior #x800000 mantissa) biased sign)))))




;;; INTEGER-DECODE-FLOAT  --  Public
;;;
;;;    Dispatch to the correct type-specific i-d-f function.
;;;
(defun integer-decode-float (x)
  "Returns three values:
   1) an integer representation of the significand.
   2) the exponent for the power of 2 that the significand must be multiplied
      by to get the actual value.  This differs from the DECODE-FLOAT exponent
      by FLOAT-DIGITS, since the significand has been scaled to have all its
      digits before the radix point.
   3) -1 or 1 (i.e. the sign of the argument.)"
  (number-case x
    (short-float
     (integer-decode-short-float x))
    (double-float
     (integer-decode-double-float x))))


;;; %UNARY-TRUNCATE  --  Interface
;;;
;;;    This function is called when we are doing a truncate without any funky
;;; divisor, i.e. converting a float or ratio to an integer.  Note that we do
;;; *not* return the second value of truncate, so it must be computed by the
;;; caller if needed.
;;;
;;;    In the float case, we pick off small arguments so that compiler can use
;;; special-case operations.  We use an exclusive test, since (due to round-off
;;; error), (float most-positive-fixnum) may be greater than
;;; most-positive-fixnum.
;;;
(defun %unary-truncate (number)
  (number-case number
    (integer number)
    (ratio (truncate-no-rem (%numerator number) (%denominator number)))
    (double-float
     (if (and (< (the double-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 0.0d0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 0.0d0)
	         (the double-float number)))
       (%truncate-double-float->fixnum number)
       (%truncate-double-float number)))
    (short-float
     (if (and (< (the short-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 0.0s0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 0.0s0)
	         (the short-float number)))
       (%truncate-short-float->fixnum number)
       (%truncate-short-float number)))))



; cmucl:compiler:float-tran.lisp
(defun xform-truncate (x)
  (let ((res (%unary-truncate x)))
    (values res (- x res))))



(defun truncate (number &optional divisor)
  "Returns number (or number/divisor) as an integer, rounded toward 0.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (when (not (called-for-mv-p))
    (return-from truncate (truncate-no-rem number divisor)))
  (macrolet 
      ((truncate-rat-dfloat (number divisor)
         `(with-stack-double-floats ((fnum ,number)
                                     (f2))
           (let ((res (%unary-truncate (%double-float/-2! fnum ,divisor f2))))
             (values res 
                     (- (the double-float fnum) (the double-float  (%double-float*-2! (%double-float res f2) ,divisor f2)))))))
       (truncate-rat-sfloat (number divisor)
         #+32-bit-target
         `(target::with-stack-short-floats ((fnum ,number)
                                            (f2))
           (let ((res (%unary-truncate (%short-float/-2! fnum ,divisor f2))))
             (values res 
                     (- (the single-float fnum) (the single-float (%short-float*-2! (%short-float res f2) ,divisor f2))))))
         #+64-bit-target
         `(let* ((temp (%short-float ,number))
                 (res (%unary-truncate (/ (the short-float temp)
                                          (the short-float ,divisor)))))
           (values res
            (- (the short-float temp)
             (the short-float (* (the short-float (%short-float res))
                                 (the short-float ,divisor)))))))
       )
    (number-case number
      (fixnum
       (number-case divisor
         (fixnum (if (eq divisor 1) (values number 0) (%fixnum-truncate number divisor)))
         (bignum (if (eq number target::target-most-negative-fixnum)
		   (with-small-bignum-buffers ((bn number))
                     (multiple-value-bind (q r) (bignum-truncate bn divisor)
                       (if (eq r bn)
                         (values q number)
                         (values q r))))
		   (values 0 number)))
         (double-float (truncate-rat-dfloat number divisor))
         (short-float (truncate-rat-sfloat number divisor))
         (ratio (let ((q (truncate (* number (%denominator divisor)) ; this was wrong
                                   (%numerator divisor))))
                  (values q (- number (* q divisor)))))))
      (bignum (number-case divisor
                (fixnum (if (eq divisor 1)
			  (values number 0)
                          (if (eq divisor target::target-most-negative-fixnum);; << aargh
                            (with-small-bignum-buffers ((bd divisor))
                              (bignum-truncate number bd))
                            (bignum-truncate-by-fixnum number divisor))))
                (bignum (bignum-truncate number divisor))
                (double-float  (truncate-rat-dfloat number divisor))
                (short-float (truncate-rat-sfloat number divisor))
                (ratio (let ((q (truncate (* number (%denominator divisor)) ; so was this
                                          (%numerator divisor))))
                         (values q (- number (* q divisor)))))))
      (short-float (if (eql divisor 1)
                     (let* ((res (%unary-truncate number)))
                       (values res (- number res)))
                     (number-case divisor
                       (short-float
                        #+32-bit-target
                        (target::with-stack-short-floats ((f2))
                          (let ((res (%unary-truncate (%short-float/-2! number divisor f2))))
                            (values res 
                                    (-
                                     (the single-float number)
                                     (the single-float (%short-float*-2! (%short-float res f2) divisor f2))))))
                        #+64-bit-target
                        (let ((res (%unary-truncate
                                    (/ (the short-float number)
                                       (the short-float divisor)))))
                          (values res
                                  (- (the short-float number)
                                     (* (the short-float (%short-float res))
                                        (the short-float divisor))))))
                       ((fixnum bignum ratio)
                        #+32-bit-target
                        (target::with-stack-short-floats ((fdiv divisor)
                                                          (f2))
                          (let ((res (%unary-truncate (%short-float/-2! number fdiv f2))))
                            (values res 
                                    (-
                                     (the single-float number)
                                     (the single-float (%short-float*-2! (%short-float res f2) fdiv f2))))))
                        #+64-bit-target
                        (let* ((fdiv (%short-float divisor))
                               (res (%unary-truncate
                                     (/ (the short-float number)
                                        (the short-float fdiv)))))
                          (values res (- number (* res fdiv))))
                                     
                        )
                       (double-float
                        (with-stack-double-floats ((fnum number)
                                                   (f2))
                          (let* ((res (%unary-truncate (%double-float/-2! fnum divisor f2))))
                            (values res
                                    (-
                                     (the double-float fnum)
                                     (the double-float (%double-float*-2! (%double-float res f2) divisor f2))))))))))
      (double-float (if (eql divisor 1)
                      (let ((res (%unary-truncate number)))
                        (values res (- number res)))
                      (number-case divisor
                        ((fixnum bignum ratio short-float)
                         (with-stack-double-floats ((fdiv divisor)
                                                    (f2))
                           (let ((res (%unary-truncate (%double-float/-2! number fdiv f2))))
                             (values res 
                                     (-
                                      (the double-float number)
                                      (the double-float (%double-float*-2! (%double-float res f2) fdiv f2)))))))                        
                        (double-float
                         (with-stack-double-floats ((f2))
                           (let ((res (%unary-truncate (%double-float/-2! number divisor f2))))
                             (values res 
                                     (-
                                      (the double-float number)
                                      (the double-float  (%double-float*-2! (%double-float res f2) divisor f2))))))))))
      (ratio (number-case divisor
               (double-float (truncate-rat-dfloat number divisor))
               (short-float (truncate-rat-sfloat number divisor))
               (rational
                (let ((q (truncate (%numerator number)
                                   (* (%denominator number) divisor))))
                  (values q (- number (* q divisor))))))))))

(defun truncate-no-rem (number  divisor)
  "Returns number (or number/divisor) as an integer, rounded toward 0."
  (macrolet 
    ((truncate-rat-dfloat (number divisor)
       `(with-stack-double-floats ((fnum ,number)
                                      (f2))
         (%unary-truncate (%double-float/-2! fnum ,divisor f2))))
     (truncate-rat-sfloat (number divisor)
       #+32-bit-target
       `(target::with-stack-short-floats ((fnum ,number)
                                      (f2))
         (%unary-truncate (%short-float/-2! fnum ,divisor f2)))
       #+64-bit-target
       `(let ((fnum (%short-float ,number)))
         (%unary-truncate (/ (the short-float fnum)
                           (the short-float ,divisor))))))
    (number-case number
    (fixnum
     (if (eql number target::target-most-negative-fixnum)
       (if (zerop divisor)
         (error 'division-by-zero :operation 'truncate :operands (list number divisor))
         (with-small-bignum-buffers ((bn number))
           (let* ((result (truncate-no-rem bn divisor)))
             (if (eq result bn)
               number
               result))))
       (number-case divisor
         (fixnum (if (eq divisor 1) number (values (%fixnum-truncate number divisor))))
         (bignum 0)
         (double-float (truncate-rat-dfloat number divisor))
         (short-float (truncate-rat-sfloat number divisor))
         (ratio (let ((q (truncate (* number (%denominator divisor))
                                   (%numerator divisor))))
                  q)))))
     (bignum (number-case divisor
               (fixnum (if (eq divisor 1) number
                         (if (eq divisor target::target-most-negative-fixnum)
                           (with-small-bignum-buffers ((bd divisor))
                             (bignum-truncate number bd :no-rem))
                           (bignum-truncate-by-fixnum number divisor))))
               (bignum (bignum-truncate number divisor :no-rem))
               (double-float  (truncate-rat-dfloat number divisor))
               (short-float (truncate-rat-sfloat number divisor))
               (ratio (let ((q (truncate (* number (%denominator divisor))
                                         (%numerator divisor))))
                        Q))))
     (double-float (if (eql divisor 1)
                     (let ((res (%unary-truncate number)))
                       RES)
                     (number-case divisor
                       ((fixnum bignum ratio)
                        (with-stack-double-floats ((fdiv divisor)
                                                   (f2))
                          (let ((res (%unary-truncate (%double-float/-2! number fdiv f2))))
                            RES)))
                       (short-float
                        (with-stack-double-floats ((ddiv divisor)
                                                   (f2))
                          (%unary-truncate (%double-float/-2! number ddiv f2))))
                       (double-float
                        (with-stack-double-floats ((f2))
                          (%unary-truncate (%double-float/-2! number divisor f2)))))))
     (short-float (if (eql divisor 1)
                    (let ((res (%unary-truncate number)))
                      RES)
                    (number-case divisor
                      ((fixnum bignum ratio)
                       #+32-bit-target
                       (target::with-stack-short-floats ((fdiv divisor)
                                                 (f2))
                         (let ((res (%unary-truncate (%short-float/-2! number fdiv f2))))
                           RES))
                       #+64-bit-target
                       (%unary-truncate (/ (the short-float number)
                                           (the short-float (%short-float divisor)))))
                      (short-float
                       #+32-bit-target
                       (target::with-stack-short-floats ((ddiv divisor)
                                                      (f2))
                         (%unary-truncate (%short-float/-2! number ddiv f2)))
                       #+64-bit-target
                       (%unary-truncate (/ (the short-float number)
                                           (the short-float (%short-float divisor)))))
                      (double-float
                       (with-stack-double-floats ((n2 number)
						      (f2))
                         (%unary-truncate (%double-float/-2! n2 divisor f2)))))))
    (ratio (number-case divisor
                  (double-float (truncate-rat-dfloat number divisor))
                  (short-float (truncate-rat-sfloat number divisor))
                  (rational
                   (let ((q (truncate (%numerator number)
                                      (* (%denominator number) divisor))))
                     Q)))))))


;;; %UNARY-ROUND  --  Interface
;;;
;;;    Similar to %UNARY-TRUNCATE, but rounds to the nearest integer.  If we
;;; can't use the round primitive, then we do our own round-to-nearest on the
;;; result of i-d-f.  [Note that this rounding will really only happen with
;;; double floats, since the whole single-float fraction will fit in a fixnum,
;;; so all single-floats larger than most-positive-fixnum can be precisely
;;; represented by an integer.]
;;;
;;; returns both values today

(defun %unary-round (number)
  (number-case number
    (integer (values number 0))
    (ratio (let ((q (round (%numerator number) (%denominator number))))             
             (values q (- number q))))
    (double-float
     (if (and (< (the double-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 1.0d0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 1.0d0)
                 (the double-float number)))
       (let ((round (%unary-round-to-fixnum number)))
         (values round (- number round)))
       (multiple-value-bind (trunc rem) (truncate number)         
         (if (not (%double-float-minusp number))
           (if (or (> rem 0.5d0)(and (= rem 0.5d0) (oddp trunc)))
             (values (+ trunc 1) (- rem 1.0d0))
             (values trunc rem))
           (if (or (> rem -0.5d0)(and (evenp trunc)(= rem -0.5d0)))
             (values trunc rem)
             (values (1- trunc) (+ 1.0d0 rem)))))))
    (short-float
     (if (and (< (the short-float number) 
                 (float (1- (ash 1 (- (1- target::nbits-in-word) target::fixnumshift))) 1.0s0))
              (< (float (ash -1 (- (1- target::nbits-in-word) target::fixnumshift)) 1.0s0)
                 (the double-float number)))
       (let ((round (%unary-round-to-fixnum number)))
         (values round (- number round)))
       (multiple-value-bind (trunc rem) (truncate number)         
         (if (not (%short-float-minusp number))
           (if (or (> rem 0.5s0)(and (= rem 0.5s0) (oddp trunc)))
             (values (+ trunc 1) (- rem 1.0s0))
             (values trunc rem))
           (if (or (> rem -0.5s0)(and (evenp trunc)(= rem -0.5s0)))
             (values trunc rem)
             (values (1- trunc) (+ 1.0s0 rem)))))))))

(defun %unary-round-to-fixnum (number)
  (number-case number
    (double-float
     (%round-nearest-double-float->fixnum number))
    (short-float
     (%round-nearest-short-float->fixnum number))))

                         
                                
         
; cmucl:compiler:float-tran.lisp
#|
(defun xform-round (x)
  (let ((res (%unary-round x)))
    (values res (- x res))))
|#

#|
(defun round (number &optional divisor)
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (if (eql divisor 1)
    (xform-round number)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (let ((thresh (if (integerp divisor) (ash (abs divisor) -1)(/ (abs divisor) 2)))) ; does this need to be a ratio?
        (cond ((or (> rem thresh)
                   (and (= rem thresh) (oddp tru)))
               (if (minusp divisor)
                 (values (- tru 1) (+ rem divisor))
                 (values (+ tru 1) (- rem divisor))))
              ((let ((-thresh (- thresh)))
                 (or (< rem -thresh)
                     (and (= rem -thresh) (oddp tru))))
               (if (minusp divisor)
                 (values (+ tru 1) (- rem divisor))
                 (values (- tru 1) (+ rem divisor))))
              (t (values tru rem)))))))
|#


(defun %fixnum-round (number divisor)
  (declare (fixnum number divisor))
  (multiple-value-bind (quo rem)(truncate number divisor) ; should => %fixnum-truncate
    (if (= 0 rem)
      (values quo rem)
      (locally (declare (fixnum quo rem))
        (let* ((minusp-num (minusp number))
               (minusp-div (minusp divisor))
               (2rem (* rem (if (neq minusp-num minusp-div) -2 2))))
          ;(declare (fixnum 2rem)) ; no way jose  
          ;(truncate (1- most-positive-fixnum) most-positive-fixnum)
          ; 2rem has same sign as divisor
          (cond (minusp-div              
                 (if (or (< 2rem divisor)
                         (and (= 2rem divisor)(logbitp 0 quo)))
                   (if minusp-num
                     (values (the fixnum (+ quo 1))(the fixnum (- rem divisor)))
                     (values (the fixnum (- quo 1))(the fixnum (+ rem divisor))))
                   (values quo rem)))
                (t (if (or (> 2rem divisor)
                           (and (= 2rem divisor)(logbitp 0 quo)))
                     (if minusp-num
                       (values (the fixnum (- quo 1))(the fixnum (+ rem divisor)))
                       (values (the fixnum (+ quo 1))(the fixnum (- rem divisor))))
                     (values quo rem)))))))))
#|
; + + => + +
; + - => - +
; - + => - -
; - - => + -
(defun %fixnum-round (number divisor)
  (declare (fixnum number divisor))
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (if (eq divisor 1)
    (values number 0)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (if (= 0 rem)
        (values tru rem)
        (locally (declare (fixnum tru rem))
          (let* ((minusp-num (minusp number))
                 (minusp-div (minusp divisor))
                 (half-div (ash (if minusp-div (- divisor) divisor) -1))
                 (abs-rem (if minusp-num (- rem) rem)))           
            (declare (fixnum half-div abs-rem)) ; true of abs-rem?
            (if (or (> abs-rem half-div)
                    (and 
                     (not (logbitp 0 divisor))
                     (logbitp 0 tru) ; oddp
                     (= abs-rem half-div)))
              (if (eq minusp-num minusp-div)
                (values (the fixnum (+ tru 1))(the fixnum (- rem divisor)))
                (values (the fixnum (- tru 1))(the fixnum (+ rem divisor))))
              (values tru rem))))))))
|#



;; makes 1 piece of garbage instead of average of 2
(defun round (number &optional divisor)
  "Rounds number (or number/divisor) to nearest integer.
  The second returned value is the remainder."
  (if (null divisor)(setq divisor 1))
  (if (eql divisor 1)
    (%unary-round number)
    (multiple-value-bind (tru rem) (truncate number divisor)
      (if (= 0 rem)
        (values tru rem)
        (let* ((mv-p (called-for-mv-p))
               (minusp-num (minusp number))
               (minusp-div (minusp divisor))
               (2rem (* rem (if (neq minusp-num minusp-div) -2 2))))
          ; 2rem has same sign as divisor
          (cond (minusp-div              
                 (if (or (< 2rem divisor)
                         (and (= 2rem divisor)(oddp tru)))
                   (if mv-p
                     (if minusp-num
                       (values (+ tru 1)(- rem divisor))
                       (values (- tru 1)(+ rem divisor)))
                     (if minusp-num (+ tru 1)(- tru 1)))
                   (values tru rem)))
                (t (if (or (> 2rem divisor)
                           (and (= 2rem divisor)(oddp tru)))
                     (if mv-p
                       (if minusp-num
                         (values (- tru 1)(+ rem divisor))
                         (values (+ tru 1)(- rem divisor)))
                       (if minusp-num (- tru 1)(+ tru 1)))
                     (values tru rem)))))))))


;; #-PPC IN L1-NUMBERS.LISP (or implement %%numdiv)
;; Anyone caught implementing %%numdiv will be summarily executed.
(defun rem (number divisor)
  "Returns second result of TRUNCATE."
  (number-case number
    (fixnum
     (number-case divisor
       (fixnum (nth-value 1 (%fixnum-truncate number divisor)))
       (bignum
        (if (and (eql number target::target-most-negative-fixnum)
                 (eql divisor (- target::target-most-negative-fixnum)))
          0
          number))
       (t (nth-value 1 (truncate number divisor)))))
    (bignum
     (number-case divisor
       (fixnum
        (if (eq divisor target::target-most-negative-fixnum)
          (nth-value 1 (truncate number divisor))
          (bignum-truncate-by-fixnum-no-quo number divisor)))
       (bignum
        (bignum-rem number divisor))
       (t (nth-value 1 (truncate number divisor)))))
    (t (nth-value 1 (truncate number divisor)))))

;; #-PPC IN L1-NUMBERS.LISP (or implement %%numdiv)
;; See above.
(defun mod (number divisor)
  "Returns second result of FLOOR."
  (let ((rem (rem number divisor)))
    (if (and (not (zerop rem))
	     (if (minusp divisor)
		 (plusp number)
		 (minusp number)))
	(+ rem divisor)
	rem)))

(defun cis (theta)
  "Return cos(Theta) + i sin(Theta), i.e. exp(i Theta)."
  (cond ((complexp theta)
         (error "Argument to CIS is complex: ~S" theta))
        ((or (typep theta 'ratio)
             (> (abs theta) #.(ash 1 23)))
         (if (typep theta 'double-float)
           (%extended-cis theta)
           (coerce (%extended-cis theta) '(complex single-float))))
        (t
         (complex (cos theta) (sin theta)))))


(defun complex (realpart &optional (imagpart 0))
  "Return a complex number with the specified real and imaginary components."
  (number-case realpart
    (short-float
      (number-case imagpart
         (short-float (%make-complex-single-float realpart imagpart))
         (double-float (%make-complex-double-float (%double-float realpart) imagpart))
         (rational (%make-complex-single-float realpart (%short-float imagpart)))))
    (double-float 
     (number-case imagpart
       (double-float (%make-complex-double-float realpart imagpart))
       ((short-float rational) (%make-complex-double-float  realpart (%double-float imagpart)))))
    (rational (number-case imagpart
                (double-float (%make-complex-double-float
                               (%double-float realpart)
                               imagpart))
                (short-float (%make-complex-single-float (%short-float realpart) imagpart))
                (rational (canonical-complex realpart imagpart))))))  

;; #-PPC IN L1-NUMBERS.LISP
(defun realpart (number)
  "Extract the real part of a number."
  (number-case number
    (complex-single-float (%complex-single-float-realpart number))
    (complex-double-float (%complex-double-float-realpart number))
    (complex (%svref number target::complex.realpart-cell))
    (number number)))

;; #-PPC IN L1-NUMBERS.LISP
(defun imagpart (number)
  "Extract the imaginary part of a number."
  (number-case number
    (complex-single-float (%complex-single-float-imagpart number))
    (complex-double-float (%complex-double-float-imagpart number))
    (complex (%svref number target::complex.imagpart-cell))
    (float (* 0 number))
    (rational 0)))

(defun logand-2 (x y)  
  (number-case x
    (fixnum (number-case y
              (fixnum
               (%ilogand (the fixnum x)(the fixnum y)))
              (bignum (fix-big-logand x y))))
    (bignum (number-case y
              (fixnum (fix-big-logand y x))
              (bignum (bignum-logical-and x y))))))

(defun logior-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (%ilogior2 x y))
              (bignum
               (if (zerop x)
                 y
                 (with-small-bignum-buffers ((bx x))
                   (bignum-logical-ior bx y))))))
    (bignum (number-case y
              (fixnum (if (zerop y)
                        x
                        (with-small-bignum-buffers ((by y))
                          (bignum-logical-ior x by))))
              (bignum (bignum-logical-ior x y))))))

(defun logxor-2 (x y)
  (number-case x
    (fixnum (number-case y
              (fixnum (%ilogxor2 x y))
              (bignum
               (with-small-bignum-buffers ((bx x))
                 (bignum-logical-xor bx y)))))
    (bignum (number-case y
              (fixnum (with-small-bignum-buffers ((by y))
                        (bignum-logical-xor x by)))
              (bignum (bignum-logical-xor x y))))))

               

; see cmucl:compiler:srctran.lisp for transforms

(defun lognand (integer1 integer2)
  "Complement the logical AND of INTEGER1 and INTEGER2."
  (lognot (logand integer1 integer2)))

(defun lognor (integer1 integer2)
  "Complement the logical AND of INTEGER1 and INTEGER2."
  (lognot (logior integer1 integer2)))

(defun logandc1 (x y)
  "Return the logical AND of (LOGNOT integer1) and integer2."
  (number-case x
    (fixnum (number-case y               
              (fixnum (%ilogand (%ilognot x) y))
              (bignum  (fix-big-logandc1 x y))))    ; (%ilogand-fix-big (%ilognot x) y))))
    (bignum (number-case y
              (fixnum  (fix-big-logandc2 y x))      ; (%ilogandc2-fix-big y x))
              (bignum (bignum-logandc2 y x))))))    ;(bignum-logical-and (bignum-logical-not x)  y))))))


#| ; its in numbers
(defun logandc2 (integer1 integer2)
  "Returns the logical AND of integer1 and (LOGNOT integer2)."
  (logand integer1 (lognot integer2)))
|#

(defun logorc1 (integer1 integer2)
  "Return the logical OR of (LOGNOT integer1) and integer2."
  (logior (lognot integer1) integer2))

#|
(defun logorc2 (integer1 integer2)
  "Returns the logical OR of integer1 and (LOGNOT integer2)."
  (logior integer1 (lognot integer2)))
|#

(defun logtest (integer1 integer2)
  "Predicate which returns T if logand of integer1 and integer2 is not zero."
 ; (not (zerop (logand integer1 integer2)))
  (number-case integer1
    (fixnum (number-case integer2
              (fixnum (not (= 0 (%ilogand integer1 integer2))))
              (bignum (logtest-fix-big integer1 integer2))))
    (bignum (number-case integer2
              (fixnum (logtest-fix-big integer2 integer1))
              (bignum (bignum-logtest integer1 integer2)))))) 



(defun lognot (number)
  "Return the bit-wise logical not of integer."
  (number-case number
    (fixnum (%ilognot number))
    (bignum (bignum-logical-not number))))

(defun logcount (integer)
  "Count the number of 1 bits if INTEGER is positive, and the number of 0 bits
  if INTEGER is negative."
  (number-case integer
    (fixnum
     (%ilogcount (if (minusp (the fixnum integer))
                   (%ilognot integer)
                   integer)))
    (bignum
     (bignum-logcount integer))))



(defun ash (integer count)
  "Shifts integer left by count places preserving sign. - count shifts right."
  (etypecase integer
    (fixnum
     (etypecase count
       (fixnum
	(if (eql integer 0)
	  0
	  (if (eql count 0)
	    integer
	    (let ((length (integer-length (the fixnum integer))))
	      (declare (fixnum length count))
	      (cond ((and (plusp count)
			  (> (+ length count)
			     (- (1- target::nbits-in-word) target::fixnumshift)))
		     (with-small-bignum-buffers ((bi integer))
		       (bignum-ashift-left bi count)))
		    ((and (minusp count) (< count (- (1- target::nbits-in-word))))
		     (if (minusp integer) -1 0))
		    (t (%iash (the fixnum integer) count)))))))
       (bignum
	(if (minusp count)
	  (if (minusp integer) -1 0)          
	  (error "Count ~s too large for ASH" count)))))
    (bignum
     (etypecase count
       (fixnum
        (if (eql count 0) 
          integer
          (if (plusp count)
            (bignum-ashift-left integer count)
            (bignum-ashift-right integer (- count)))))
       (bignum
        (if (minusp count)
          (if (minusp integer) -1 0)
          (error "Count ~s too large for ASH" count)))))))

(defun integer-length (integer)
  "Return the number of significant bits in the absolute value of integer."
  (number-case integer
    (fixnum
     (%fixnum-intlen (the fixnum integer)))
    (bignum
     (bignum-integer-length integer))))


; not CL, used below
(defun byte-mask (size)
  (1- (ash 1 (the fixnum size))))

(defun byte-position (bytespec)
  "Return the position part of the byte specifier bytespec."
  (if (> bytespec 0)
    (- (integer-length bytespec) (logcount bytespec))
    (- bytespec)))


; CMU CL returns T.
(defun upgraded-complex-part-type (type)
  "Return the element type of the most specialized COMPLEX number type that
   can hold parts of type SPEC."
  (declare (ignore type))
  'real)

;;; This is the MRG31k3p random number generator described in
;;; P. L'Ecuyer and R. Touzin, "Fast Combined Multiple Recursive
;;; Generators with Multipliers of the form a = +/- 2^d +/- 2^e",
;;; Proceedings of the 2000 Winter Simulation Conference, Dec. 2000,
;;; 683--689.
;;;
;;; A link to the paper is available on L'Ecuyer's web site:
;;; http://www.iro.umontreal.ca/~lecuyer/papers.html.
;;;
;;; This generator has a period of about 2^185.  It produces values in
;;; in the half-open interval [0, 2^31 - 1).
;;;
;;; It uses 6 words of state.

(defconstant mrg31k3p-m1 #.(- (expt 2 31) 1))
(defconstant mrg31k3p-m2 #.(- (expt 2 31) 21069))
(defconstant mrg31k3p-limit #.(1- (expt 2 31))
	     "Exclusive upper bound on values returned by %mrg31k3p.")


;;; This is a portable version of the MRG31k3p generator.  It's not
;;; too bad in a 64-bit CCL, but the generator pretty much has to be
;;; in LAP for 32-bit ports.
#-(or x8632-target ppc32-target x8664-target ppc64-target arm-target)
(defun %mrg31k3p (state)
  (let* ((v (random.mrg31k3p-state state)))
    (declare (type (simple-array (unsigned-byte 32) (*)) v)
	     (optimize speed))
    (let ((y1 (+ (+ (ash (logand (aref v 1) #x1ff) 22)
		    (ash (aref v 1) -9))
		 (+ (ash (logand (aref v 2) #xffffff) 7)
		    (ash (aref v 2) -24)))))
      (declare (type (unsigned-byte 32) y1))
      (if (>= y1 mrg31k3p-m1) (decf y1 mrg31k3p-m1))
      (incf y1 (aref v 2))
      (if (>= y1 mrg31k3p-m1) (decf y1 mrg31k3p-m1))
      (setf (aref v 2) (aref v 1)
	    (aref v 1) (aref v 0)
	    (aref v 0) y1))
    (let ((y1 (+ (ash (logand (aref v 3) #xffff) 15)
		 (* 21069 (ash (aref v 3) -16))))
	  (y2 (+ (ash (logand (aref v 5) #xffff) 15)
		 (* 21069 (ash (aref v 5) -16)))))
      (declare (type (unsigned-byte 32) y1 y2))
      (if (>= y1 mrg31k3p-m2) (decf y1 mrg31k3p-m2))
      (if (>= y2 mrg31k3p-m2) (decf y2 mrg31k3p-m2))
      (incf y2 (aref v 5))
      (if (>= y2 mrg31k3p-m2) (decf y2 mrg31k3p-m2))
      (incf y2 y1)
      (if (>= y2 mrg31k3p-m2) (decf y2 mrg31k3p-m2))
      (setf (aref v 5) (aref v 4)
	    (aref v 4) (aref v 3)
	    (aref v 3) y2))
    (let* ((x10 (aref v 0))
	   (x20 (aref v 3)))
      (if (<= x10 x20)
	(+ (- x10 x20) mrg31k3p-m1)
	(- x10 x20)))))

(eval-when (:compile-toplevel :execute)
  (declaim (inline %16-random-bits)))

(defun %16-random-bits (state)
  (logand #xffff (the fixnum (%mrg31k3p state))))

#+64-bit-target
(defun %big-fixnum-random (number state)
  (declare (fixnum number)
	   (ftype (function (random-state) fixnum) %mrg31k3p))
  (let ((low (ldb (byte 30 0) (%mrg31k3p state)))
	(high (ldb (byte 30 0) (%mrg31k3p state))))
    (declare (fixnum low high))
    (fast-mod (logior low (the fixnum (ash high 30)))
	      number)))

;;; When using a dead simple random number generator, it's reasonable
;;; to take 16 bits of the output and discard the rest.  With a more
;;; expensive generator, however, it may be worthwhile to do more bit
;;; fiddling here here so that we can use all of the random bits
;;; produced by %mrg31k2p.
#+32-bit-target
(defun %bignum-random (number state)
  (let* ((bits (+ (integer-length number) 8))
         (half-words (ash (the fixnum (+ bits 15)) -4))
         (long-words (ash (+ half-words 1) -1))
         (dividend (%alloc-misc long-words target::subtag-bignum))
         (16-bit-dividend dividend)
         (index 1))
    (declare (fixnum long-words index bits)
             (dynamic-extent dividend)
             (type (simple-array (unsigned-byte 16) (*)) 16-bit-dividend) ;lie
             (optimize (speed 3) (safety 0)))
    (loop
       ;; This had better inline due to the lie above, or it will error
       #+big-endian-target
       (setf (aref 16-bit-dividend index) (%16-random-bits state))
       #+little-endian-target
       (setf (aref 16-bit-dividend (the fixnum (1- index)))
	     (%16-random-bits state))
       (decf half-words)
       (when (<= half-words 0) (return))
       #+big-endian-target
       (setf (aref 16-bit-dividend (the fixnum (1- index)))
	     (%16-random-bits state))
       #+little-endian-target
       (setf (aref 16-bit-dividend index) (%16-random-bits state))
       (decf half-words)
       (when (<= half-words 0) (return))
       (incf index 2))
    ;; The bignum code expects normalized bignums
    (let* ((result (mod (%normalize-bignum-2 t dividend) number)))
      (if (eq dividend result)
	(copy-bignum result)
	result))))

(defun %float-random (number state)
  (let ((ratio (gvector :ratio (random target::target-most-positive-fixnum state) target::target-most-positive-fixnum)))
    (declare (dynamic-extent ratio))
    (* number ratio)))

(defun random (number &optional (state *random-state*))
  (if (not (typep state 'random-state)) (report-bad-arg state 'random-state))
  (cond
    ((and (fixnump number) (> (the fixnum number) 0))
     #+32-bit-target
     (fast-mod (%mrg31k3p state) number)
     #+64-bit-target
     (if (< number mrg31k3p-limit)
       (fast-mod (%mrg31k3p state) number)
       (%big-fixnum-random number state)))
    ((and (typep number 'double-float) (> (the double-float number) 0.0))
     (%float-random number state))
    ((and (typep number 'short-float) (> (the short-float number) 0.0s0))
     (%float-random number state))
    ((and (bignump number) (> number 0))
     (%bignum-random number state))
    (t (report-bad-arg number '(or (integer (0)) (float (0.0)))))))

(eval-when (:compile-toplevel :execute)
  (defmacro bignum-abs (nexp)
    (let ((n (gensym)))
      `(let ((,n ,nexp))
         (if  (bignum-minusp ,n) (negate-bignum ,n) ,n))))
  
  (defmacro fixnum-abs (nexp)
    (let ((n (gensym)))
      `(let ((,n ,nexp))
         (if (minusp (the fixnum ,n))
           (if (eq ,n target::target-most-negative-fixnum)
             (- ,n)
             (the fixnum (- (the fixnum ,n))))
           ,n))))
  )
  

;;; TWO-ARG-GCD  --  Internal
;;;
;;;    Do the GCD of two integer arguments.  With fixnum arguments, we use the
;;; binary GCD algorithm from Knuth's seminumerical algorithms (slightly
;;; structurified), otherwise we call BIGNUM-GCD.  We pick off the special case
;;; of 0 before the dispatch so that the bignum code doesn't have to worry
;;; about "small bignum" zeros.
;;;
(defun gcd-2 (n1 n2)
  ;(declare (optimize (speed 3)(safety 0)))
  (cond 
   ((eql n1 0) (%integer-abs n2))
   ((eql n2 0) (%integer-abs n1))
   (t (number-case n1
        (fixnum 
         (number-case n2
          (fixnum
	   (if (eql n1 target::target-most-negative-fixnum)
	     (if (eql n2 target::target-most-negative-fixnum)
	       (- target::target-most-negative-fixnum)
	       (bignum-fixnum-gcd (- target::target-most-negative-fixnum) (abs n2)))
	     (if (eql n2 target::target-most-negative-fixnum)
	       (bignum-fixnum-gcd (- target::target-most-negative-fixnum) (abs n1))
	       (locally
		   (declare (optimize (speed 3) (safety 0))
			    (fixnum n1 n2))
		 (if (minusp n1)(setq n1 (the fixnum (- n1))))
		 (if (minusp n2)(setq n2 (the fixnum (- n2))))
               (%fixnum-gcd n1 n2)))))
           (bignum (if (eql n1 target::target-most-negative-fixnum)
		     (%bignum-bignum-gcd n2 (- target::target-most-negative-fixnum))
		     (bignum-fixnum-gcd (bignum-abs n2)(fixnum-abs n1))))))
	(bignum
	 (number-case n2
	   (fixnum
            (if (eql n2 target::target-most-negative-fixnum)
              (%bignum-bignum-gcd (bignum-abs n1)(fixnum-abs n2))
              (bignum-fixnum-gcd (bignum-abs n1)(fixnum-abs n2))))
	   (bignum (%bignum-bignum-gcd n1 n2))))))))

#|
(defun fixnum-gcd (n1 n2)
  (declare (optimize (speed 3) (safety 0))
           (fixnum n1 n2))                    
  (do* ((k 0 (%i+ 1 k))
        (n1 n1 (%iasr 1 n1))
        (n2 n2 (%iasr 1 n2)))
       ((oddp (logior n1 n2))
        (do ((temp (if (oddp n1) (the fixnum (- n2)) (%iasr 1 n1))
                   (%iasr 1 temp)))
            (nil)
          (declare (fixnum temp))
          (when (oddp temp)
            (if (plusp temp)
              (setq n1 temp)
              (setq n2 (- temp)))
            (setq temp (the fixnum (- n1 n2)))
            (when (zerop temp)
              (let ((res (%ilsl k n1)))
                (return res))))))
    (declare (fixnum n1 n2 k))))
|#



(defun %quo-1 (n)
  (/ 1 n))

;; Compute (sqrt (+ (* x x) (* y y))), but
;; try to be a little more careful about it.
;; Both x and y must be double-floats.
(defun %double-float-hypot (x y)
  (with-stack-double-floats ((a) (b) (c))
    (%%double-float-abs! x a)
    (%%double-float-abs! y b)
    (when (> a b)
      (psetq a b b a))
    (if (= b 0d0)
      0d0
      (progn
	(%double-float/-2! a b c)
	(* b (fsqrt (+ 1d0 (* c c))))))))
					
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-numbers.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-symbol.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;;; No error checking, no interrupts, no protect_caller, no nuthin.
;;; No error, no cons.  No problem.
(defun %progvrestore (saved)
  (declare (optimize (speed 3) (safety 0)))
  (dolist (pair saved)
    (%set-sym-value (car pair) (cdr pair))))

;;; Check that something that's supposed to be a proper list of
;;; symbols is; error otherwise.
;;; This is called only by the compiler output of a PROGV form.
;;; It checks for the maximum length that the progvsave subprim
;;; can handle.

(defun check-symbol-list (l &optional (max-length
                                        (floor (- 4096 20) (* target::node-size 3))
                                       ))
  (let ((len (list-length l)))
    (if (and len
             (or (null max-length)
                 (< len max-length))
             (dolist (s l t) 
               (unless (and (symbolp s)
                            (not (constant-symbol-p s))
                            (not (logbitp $sym_vbit_global (the fixnum (%symbol-bits s))))
                            (ensure-binding-index s))
                 (return nil))))
      l
      (error "~s is not a proper list of bindable symbols~@[ of length < ~s~]." l max-length))))

;;; The type-checking done on the "plist" arg shouldn't be removed.
(defun set-symbol-plist (sym plist)
  (when plist
    (let* ((len (list-length plist)))
      (unless (and len (evenp len))
        (error "Bad plist: ~s" plist))))
  (let* ((vector (symptr->symvector (%symbol->symptr sym)))
         (cell (%svref vector target::symbol.plist-cell))
         (consp (consp cell)))
    (if plist
      (if consp
        (setf (cdr cell) plist)
        (cdr (setf (%svref vector target::symbol.plist-cell) (cons nil plist))))
      (progn
        (if consp
          (setf (%svref vector target::symbol.plist-cell) (%car cell)))
        nil))))


(eval-when (:compile-toplevel :execute)
  (declaim (inline %pl-search)))

(defun %pl-search (l key)
  (declare (list l) (optimize (speed 3)))
  (loop
    (if (eq (car l) key)
      (return)
      (if l
        (setq l (cdr (the list (cdr l))))
        (return))))
  l)


(defun symbol-plist (sym)
  "Return SYMBOL's property list."
  (let* ((cell (%svref (symptr->symvector (%symbol->symptr sym)) target::symbol.plist-cell)))
    (if (consp cell)
      (cdr cell))))


(defun get (sym key &optional default)
  "Look on the property list of SYMBOL for the specified INDICATOR. If this
  is found, return the associated value, else return DEFAULT."
  (let* ((cell (%svref (symptr->symvector (%symbol->symptr sym)) target::symbol.plist-cell))
         (tail (if (consp cell)
                 (%pl-search (cdr cell ) key))))
    (if tail (%cadr tail) default)))

(defun put (sym key value)
  (let* ((symptr (%symbol->symptr sym))
         (vector (symptr->symvector symptr))
         (cell  (%svref vector target::symbol.plist-cell))
         (plist (if (consp cell) (cdr cell)))
         (tail (%pl-search plist key)))
    (if tail 
      (%rplaca (%cdr tail) value)
      (progn
        (setq plist (cons key (cons value plist)))
        (if (consp cell)
          (setf (cdr cell) plist)
          (setf (%svref vector target::symbol.plist-cell) (cons nil plist)))))
    value))


(defun get-type-predicate (name)
  (let* ((symvec (symptr->symvector (%symbol->symptr name)))
         (pp (%svref symvec target::symbol.package-predicate-cell)))
    (if (consp pp)
      (%cdr pp))))

(defun set-type-predicate (name function)
  (let* ((bits (%symbol-bits name))
         (symvec (symptr->symvector (%symbol->symptr name)))
         (spp (%svref symvec target::symbol.package-predicate-cell)))
    (declare (fixnum bits))
    (if (logbitp $sym_vbit_typeppred bits)
      (%rplacd spp function)
      (progn
        (%symbol-bits name (the fixnum (bitset $sym_vbit_typeppred bits)))
        (setf (%svref symvec target::symbol.package-predicate-cell) (cons spp function))))
    function))

(defun symbol-value (sym)
  "Return SYMBOL's current bound value."
  (let* ((val (%sym-value sym)))
    (if (eq val (%unbound-marker))
      (%kernel-restart $xvunbnd sym)
      val)))

(defun set (sym value)
  "Set SYMBOL's value cell to NEW-VALUE."
  (let* ((bits (%symbol-bits sym)))
    (declare (fixnum bits))
    (if (logbitp $sym_vbit_const bits)
      (%err-disp $XCONST sym)
      (%set-sym-value sym value))))

(defun constant-symbol-p (sym)
  (and (symbolp sym)
       (%ilogbitp $sym_vbit_const (%symbol-bits sym))))

;;; This leaves the SPECIAL bit alone, clears the others.
(defun makunbound (sym)
  "Make SYMBOL unbound, removing any value it may currently have."
  (if (and *warn-if-redefine-kernel*
           (constant-symbol-p sym))
    (cerror "Make ~S be unbound anyway."
            "~S is a constant; making it unbound might be a bad idea." sym))
  (%symbol-bits sym (the fixnum (logand (logior #xff00 (ash 1 $sym_bit_special))
                                        (the fixnum (%symbol-bits sym)))))
  (%set-sym-value sym (%unbound-marker))
  sym)

(defun non-nil-symbolp (x)
  "Returns symbol if true"
  (if (symbolp x) x))

(defun symbol-package (sym)
  "Return the package SYMBOL was interned in, or NIL if none."
  (let* ((pp (%svref (symptr->symvector (%symbol->symptr sym)) target::symbol.package-predicate-cell)))
    (if (consp pp) (car pp) pp)))

(defun boundp (sym)
  "Return non-NIL if SYMBOL is bound to a value."
  (not (eq (%sym-value sym) (%unbound-marker))))

(defun make-symbol (name)
  "Make and return a new symbol with the NAME as its print name."
  (symvector->symptr
   (%gvector target::subtag-symbol
             (ensure-simple-string name) ; pname
             (%unbound-marker)          ; value cell
             %unbound-function%         ; function cell
             nil                        ; package&predicate
             0                          ; flags
             nil                        ; plist
             0)))                       ; binding-index

(defun %symbol-bits (sym &optional new)
  (let* ((p (%symbol->symptr sym))
         (bits (%svref (symptr->symvector p) target::symbol.flags-cell)))
    (if new
      (setf (%svref (symptr->symvector p) target::symbol.flags-cell) new))
    bits))

(defun %sym-value (name)
  (%symptr-value (%symbol->symptr name)))

(defun %set-sym-value (name val)
  (%set-symptr-value (%symbol->symptr name) val))
    
(defun %sym-global-value (name)
  (%svref (symptr->symvector (%symbol->symptr name)) target::symbol.vcell-cell))

(defun %set-sym-global-value (name val)
  (setf (%svref (symptr->symvector (%symbol->symptr name)) target::symbol.vcell-cell) val))

(defun symbol-name (sym)
  "Return SYMBOL's name as a string."
  #+(or ppc32-target x8632-target x8664-target arm-target)
  (%svref (symptr->symvector (%symbol->symptr sym)) target::symbol.pname-cell)
  #+ppc64-target
  (if sym                               ;NIL's pname is implicit
    (%svref (%symbol->symptr sym) ppc64::symbol.pname-cell)
    "NIL")
  )




(defun %global-macro-function (symbol)
  (let* ((fbinding (fboundp symbol)))
    (if (and #-arm-target (typep fbinding 'simple-vector)
             #+arm-target (= (typecode fbinding) arm::subtag-pseudofunction)
             (= (the fixnum (uvsize fbinding)) #-arm-target 2 #+arm-target 3))
      (let* ((fun (%svref fbinding #-arm-target 1 #+arm-target 2)))
        (if (functionp fun) fun)))))

(defun %symbol-binding-address (sym)
  (%symptr-binding-address (%symbol->symptr sym)))

(defun symbol-binding-index (sym)
  (%svref (symptr->symvector (%symbol->symptr sym)) target::symbol.binding-index-cell))

(defvar *interrupt-level* -1)

;;; Special binding indices, and the inverse mapping between indices
;;; and symbols
(let* ((binding-index-lock (make-lock))
       (binding-index-reverse-map (make-hash-table :test #'eq :weak :value))
       (next-binding-index 0))
  (defun %set-binding-index (val) (setq next-binding-index val))
  (defun next-binding-index () (1+ next-binding-index))
  (defun ensure-binding-index (sym)
    (with-lock-grabbed (binding-index-lock)
      (let* ((symvec (symptr->symvector (%symbol->symptr sym)))
             (idx (%svref symvec target::symbol.binding-index-cell))
             (bits (%symbol-bits sym)))
        (declare (fixnum idx bits))
        (if (or (logbitp $sym_vbit_global bits)
                (logbitp $sym_vbit_const bits))
          (unless (zerop idx)
            (remhash idx binding-index-reverse-map)
            (setf (%svref symvec target::symbol.binding-index-cell) 0))
          (if (zerop idx)
            (let* ((new-idx (incf next-binding-index)))
              (setf (%svref symvec target::symbol.binding-index-cell) new-idx)
              (setf (gethash new-idx binding-index-reverse-map) sym))))
        sym)))
  (defun binding-index-symbol (idx)
    (with-lock-grabbed (binding-index-lock)
      (gethash idx binding-index-reverse-map)))
  (defun cold-load-binding-index (sym)
    ;; Index may have been assigned via xloader.  Update
    ;; reverse map
    (with-lock-grabbed (binding-index-lock)
      (let* ((idx (%svref (symptr->symvector (%symbol->symptr sym))
                          target::symbol.binding-index-cell)))
        (declare (fixnum idx))
        (unless (zerop idx)
          (setf (gethash idx binding-index-reverse-map) sym))))))

       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-symbol.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-hash.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;;;;;;;;;;;;;
;;
;; See hash.lisp for documentation


(eval-when (:compile-toplevel :execute)
  (require "HASHENV" "ccl:xdump;hashenv")
  (require :number-case-macro)
  (assert (and (not (eql (%unbound-marker) (%slot-unbound-marker)))
               (not (eql (%unbound-marker) (%illegal-marker)))
               (not (eql (%slot-unbound-marker) (%illegal-marker)))))
  (define-symbol-macro deleted-hash-key-marker (%slot-unbound-marker))
  (define-symbol-macro deleted-hash-value-marker (%slot-unbound-marker))
  (define-symbol-macro free-hash-marker (%unbound-marker))
  (define-symbol-macro rehashing-value-marker (%illegal-marker))
  (declaim (inline nhash.vector-size))
  (declaim (inline mixup-hash-code))
  (declaim (inline hash-table-p))
  (declaim (inline %%eqhash))
  (declaim (inline index->vector-index vector-index->index swap))
  (declaim (inline %already-rehashed-p %set-already-rehashed-p))
  (declaim (inline need-use-eql))
  (declaim (inline %needs-rehashing-p))
  (declaim (inline compute-hash-code))
  (declaim (inline eq-hash-find eq-hash-find-for-put))
  (declaim (inline read-lock-hash-table write-lock-hash-table  unlock-hash-table))
  (declaim (inline %hash-symbol))
  (declaim (inline hash-mod))
  (declaim (inline set-hash-key-conditional set-hash-value-conditional))
  (declaim (inline hash-lock-free-p lock-free-gethash))
  (declaim (inline invalid-hash-key-p)))


#+eq-hash-monitor
(progn
(defparameter eq-hash-find-calls 0)
(defparameter eq-hash-find-probes 0)
(defparameter eq-hash-find-for-put-calls 0)
(defparameter eq-hash-find-for-put-probes 0)
)


(defun %cons-hash-table (keytrans-function compare-function vector
                         threshold rehash-ratio rehash-size find find-new owner lock-free-p &optional min-size)
  (%istruct
   'HASH-TABLE                          ; type
   keytrans-function                    ; nhash.keytransF
   compare-function                     ; nhash.compareF
   nil                                  ; nhash.rehash-bits
   vector                               ; nhash.vector
   (if lock-free-p $nhash.lock-free 0)  ; nhash.lock
   owner                                ; nhash.owner 
   threshold                            ; nhash.grow-threshold
   rehash-ratio                         ; nhash.rehash-ratio
   rehash-size                          ; nhash.rehash-size
   0                                    ; nhash.puthash-count
   (if lock-free-p
     (make-lock)
     (unless owner (make-read-write-lock))) ; nhash.exclusion-lock
   find                                 ; nhash.find
   find-new                             ; nhash.find-new
   nil                                  ; nhash.read-only
   (or min-size 0)                      ; nhash.min-size
   ))

(defun nhash.vector-size (vector)
  (nhash.vector.size vector))

(defun hash-mod (hash entries vector)
  (fast-mod-3 hash entries (nhash.vector.size-reciprocal vector)))

;; For lock-free hash tables
(defun set-hash-key-conditional (index vector old new)
  (%set-hash-table-vector-key-conditional (%i+ target::misc-data-offset
                                               (ash (the fixnum index) target::word-shift))
                                          vector
                                          old
                                          new))

(defun set-hash-value-conditional (index vector old new)
  (store-gvector-conditional (%i+ index 1) vector old new))

(defun hash-lock-free-p (hash)
  (logtest $nhash.lock-free (the fixnum (nhash.lock hash))))
 
;;; Is KEY something which can be EQL to something it's not EQ to ?
;;; (e.g., is it a number or macptr ?)
;;; This can be more general than necessary but shouldn't be less so.
(defun need-use-eql (key)
  (let* ((typecode (typecode key)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-macptr)
        (and (< typecode (- target::nbits-in-word target::fixnumshift))
             (logbitp (the (integer 0 (#.(- target::nbits-in-word target::fixnumshift)))
                        typecode)
                      (logior (ash 1 target::subtag-bignum)
                              #-64-bit-target
                              (ash 1 target::subtag-single-float)
                              (ash 1 target::subtag-double-float)
                              (ash 1 target::subtag-ratio)
                              (ash 1 target::subtag-complex)))))))

;;; Don't rehash at all, unless some key is address-based (directly or
;;; indirectly.)
(defun %needs-rehashing-p (vector)
  (let* ((flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (if (logbitp $nhash_track_keys_bit flags)
      ;; GC is tracking key movement
      (logbitp $nhash_key_moved_bit flags)
      ;; GC is not tracking key movement
      (if (logbitp $nhash_component_address_bit flags)
        (not (eql (the fixnum (%get-gc-count)) (the fixnum (nhash.vector.gc-count vector))))))))

(defun %set-does-not-need-rehashing (vector)
  (let* ((flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (setf (nhash.vector.gc-count vector) (%get-gc-count))
    (when (logbitp $nhash_track_keys_bit flags)
      (setf (nhash.vector.flags vector)
            (logand (lognot (ash 1 $nhash_key_moved_bit)) flags)))))


;;; Tempting though it may be to remove this, a hash table loaded from
;;; a fasl file likely needs to be rehashed, and the MAKE-LOAD-FORM
;;; for hash tables needs to be able to call this or something similar.
(defun %set-needs-rehashing (hash)
  (let* ((vector (nhash.vector hash))
         (flags (nhash.vector.flags vector)))
    (declare (fixnum flags))
    (setf (nhash.vector.gc-count vector) (the fixnum (1- (the fixnum (%get-gc-count)))))
    (when (logbitp $nhash_track_keys_bit flags)
      (setf (nhash.vector.flags vector) (logior (ash 1 $nhash_key_moved_bit) flags)))))

#-cross-compiling
;;;
;;; This is a fairly straightforward translation of the "one-at-a-time"
;;; hash function described at:
;;; http://www.burtleburtle.net/bob/hash/doobs.html
;;;
(defun mixup-hash-code (fixnum)
  (declare (fixnum fixnum)
           (optimize (speed 3) (safety 0)))
  (setq fixnum (logand fixnum target::most-positive-fixnum))
  (do* ((hash 0))
       ((zerop fixnum)
        (setq hash (+ hash (the fixnum (ash hash 3)))
              hash (logxor hash (the fixnum (ash hash -11))))
        (the fixnum (+ hash (the fixnum (ash hash 15)))))
    (declare (fixnum hash))
    (setq hash (+ hash (the fixnum (logand fixnum #xff)))
          fixnum (ash fixnum -8)
          hash (+ hash (the fixnum (ash hash 10)))
          hash (logxor hash (the fixnum (ash hash -6))))))

#+cross-compiling
(defun mixup-hash-code (code)
  (logand code target::target-most-positive-fixnum))

(defun rotate-hash-code (fixnum)
  (declare (fixnum fixnum))
  (let* ((low-3 (logand 7 fixnum))
         (but-low-3 (%ilsr 3 fixnum))
         (low-3*64K (%ilsl 13 low-3))
         (low-3-in-high-3 (%ilsl (- 32 3 3) low-3)))
    (declare (fixnum low-3 but-low-3 low-3*64K low-3-in-high-3))
    (the fixnum (+ low-3-in-high-3
                   (the fixnum (logxor low-3*64K but-low-3))))))




(defconstant $nhash-track-keys-mask
  #.(- (ash 1 $nhash_track_keys_bit)))

(defconstant $nhash-clear-key-bits-mask #xfffff)
(defparameter *nil-hash* nil)

(defun %hash-symbol (sym)
  (if sym    
    (let* ((vector (%symptr->symvector sym))
           (cell (%svref vector target::symbol.plist-cell))
           (consp (consp cell)))
      (or (if consp (%car cell) cell)
          (let* ((pname (%svref vector target::symbol.pname-cell))
                 (hash (mixup-hash-code (%pname-hash pname (uvsize pname)))))
            (declare (type simple-string pname) (fixnum hash))
            (if consp
              (setf (%car cell) hash)
              (setf (%svref vector target::symbol.plist-cell) hash)))))
    (or *nil-hash*
        (setq *nil-hash* (mixup-hash-code (%pname-hash "NIL" 3))))))
              
;;; Hash on address, or at least on some persistent, immutable
;;; attribute of the key.  If all keys are fixnums or immediates (or if
;;; that attribute exists), rehashing won't ever be necessary.
(defun %%eqhash (key)
  (let* ((typecode (typecode key)))
    (if (eq typecode target::tag-fixnum)
      (values (mixup-hash-code key) nil)
      (if (eq typecode target::subtag-instance)
        (values (mixup-hash-code (instance.hash key)) nil)
        (if (symbolp key)
          (values (%hash-symbol key) nil)
          (let ((hash (mixup-hash-code (strip-tag-to-fixnum key))))
            (if (immediate-p-macro key)
              (values hash nil)
              (values hash :key ))))))))


#+32-bit-target
(defun swap (num)
  (declare (fixnum num))
  (the fixnum (+ (the fixnum (%ilsl 16 num))(the fixnum (%ilsr 13 num)))))

#+64-bit-target
(defun swap (num)
  (declare (fixnum num))
  (the fixnum (+ (the fixnum (%ilsl 32 num))(the fixnum (%ilsr 29 num)))))

;;; teeny bit faster when nothing to do
(defun %%eqlhash-internal (key)
  (number-case key
    (fixnum (mixup-hash-code key)) ; added this 
    (double-float (%dfloat-hash key))
    (short-float (%sfloat-hash key))
    (bignum (%bignum-hash key))
    (ratio (logxor (swap (%%eqlhash-internal (numerator key)))
                   (%%eqlhash-internal (denominator key))))
    (complex
     (logxor (swap (%%eqlhash-internal (realpart key)))
             (%%eqlhash-internal (imagpart key))))
    (t (cond ((macptrp key)
              (%macptr-hash key))
             (t key)))))

               


;;; new function

(defun %%eqlhash (key)
  ;; if key is a macptr, float, bignum, ratio, or complex, convert it
  ;; to a fixnum
  (if (hashed-by-identity key)
    (%%eqhash key)
    (let ((primary  (%%eqlhash-internal key)))
      (if (eq primary key)
        (%%eqhash key)
        (mixup-hash-code (strip-tag-to-fixnum primary))))))


(defun %%equalhash (key)
  (let* ((id-p (hashed-by-identity key))
         (hash (if (and key (not id-p)) (%%eqlhash-internal key)))
         addressp)
    (cond ((null key) (mixup-hash-code 17))
          #+64-bit-target
          ((and (typep key 'single-float)
                (zerop (the single-float key)))
           0)
          ((immediate-p-macro key) (mixup-hash-code (strip-tag-to-fixnum key)))
          ((and hash (neq hash key)) hash)  ; eql stuff
          (t (typecase key
                (simple-string (%string-hash 0 key (length key)))
                (string
                 (let ((length (length key)))
                   (multiple-value-bind (data offset) (array-data-and-offset key)
                     (%string-hash offset data length))))
                (bit-vector (bit-vector-hash key))
                (cons
                 (let ((hash 0))
                   (do* ((i 0 (1+ i))
                         (done nil)
                         (list key (unless done (cdr list))))
                        ((or done (> i 11))) ; who figured 11?
                     (declare (fixnum i))
                     (multiple-value-bind (h1 a1) (%%equalhash
                                                   (if (atom list)
                                                     (progn
                                                       (setq done t)
                                                       list)
                                                     (%car list)))
                       (when a1 (setq addressp t))
                       ; fix the case of lists of same stuff in different order
                       ;(setq hash (%ilogxor (fixnum-rotate h1 i) hash))
                       (setq hash (%i+ (rotate-hash-code hash) h1))
                       ))
                   (values (mixup-hash-code hash) addressp)))
                (pathname (%%equalphash key))
                (t (%%eqlhash key)))))))

(defun %string-hash-folding-case (start string len)
  (declare (index start len)
           (optimize (speed 3) (safety 0)))
  (let* ((copy (make-string len)))
    (declare (dynamic-extent copy))
    (dotimes (i len (values (%string-hash 0 copy len)))
      (declare (index i))
      (setf (schar copy i)
            (char-upcase (schar string start))
            start (1+ start)))))

(defun %hash-pathname (key)
  (let* ((logical (istruct-typep key 'logical-pathnames))
         (case-sensitive *case-sensitive-filesystem*)
         (hash 0))
    (flet ((incorporate (component)
             (setq hash (logand target::target-most-positive-fixnum
                                (+ hash
                                   (if case-sensitive
                                     (%%equalhash component)
                                     (%%equalphash component)))))))
      (if logical
        (progn
          (setq hash (%%equalphash (%logical-pathname-host key)))
          (incorporate (%logical-pathname-version key)))
        (incorporate (%physical-pathname-device key)))
      (dolist (element (%pathname-directory key))
        (incorporate element))
      (incorporate (%pathname-name key))
      (incorporate (%pathname-type key))
      (mixup-hash-code hash))))

(defun update-hash-flags (hash vector addressp)
  (when addressp
    (flet ((new-flags (flags addressp)
             (declare (fixnum flags))
             (if (eq :key addressp)
               ;; hash code depended on key's address
               (if (logbitp $nhash_component_address_bit flags)
                 flags
                 (logior $nhash-track-keys-mask
                         (if (logbitp $nhash_track_keys_bit flags)
                           flags
                           (bitclr $nhash_key_moved_bit flags))))
               ;; hash code depended on component address
               (bitset $nhash_component_address_bit
                       (logand (lognot $nhash-track-keys-mask) flags)))))
      (declare (inline new-flags))
      (if (hash-lock-free-p hash)
        (loop
          (let* ((flags (nhash.vector.flags vector))
                 (new-flags (new-flags flags addressp)))
            (when (or (eq flags new-flags)
                      (store-gvector-conditional nhash.vector.flags vector flags new-flags))
              (return))))
        (setf (nhash.vector.flags vector)
              (new-flags (nhash.vector.flags vector) addressp))))))

(defun compute-hash-code (hash key update-hash-flags &optional
                               (vector (nhash.vector hash))) ; vectorp))
  (let ((keytransF (nhash.keytransF hash))
        primary addressp)
    (if (not (fixnump keytransF))
      ;; not EQ or EQL hash table
      (progn
        (multiple-value-setq (primary addressp) (funcall keytransF key))
        (let ((immediate-p (immediate-p-macro primary)))
          (setq primary (strip-tag-to-fixnum primary))
          (unless immediate-p
            (setq primary (mixup-hash-code primary))
            (setq addressp :key))))
      ;; EQ or EQL hash table
      (if (and (not (eql keytransF 0))
	       (need-use-eql key))
	;; EQL hash table
	(setq primary (%%eqlhash-internal key))
	;; EQ hash table - or something eql doesn't do
	(multiple-value-setq (primary addressp) (%%eqhash key))))
    (when update-hash-flags
      (when addressp
        (update-hash-flags hash vector addressp)))
    (let* ((entries (nhash.vector-size vector)))
      (declare (fixnum entries))
      (values primary
              (hash-mod primary entries vector)
              entries))))

(defun %already-rehashed-p (primary rehash-bits)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (simple-array bit (*)) rehash-bits))
  (eql 1 (sbit rehash-bits primary)))

(defun %set-already-rehashed-p (primary rehash-bits)
  (declare (optimize (speed 3)(safety 0)))
  (declare (type (simple-array bit (*)) rehash-bits))
  (setf (sbit rehash-bits primary) 1))


(defun hash-table-p (hash)
  (istruct-typep hash 'hash-table))



(defparameter *shared-hash-table-default* t
  "Be sure that you understand the implications of changing this
before doing so.")

(defparameter *lock-free-hash-table-default* :shared
  "If NIL, hash tables default to using the standard algorithms, with locks for shared tables.
   If :SHARED, shared hash tables default to using the \"lock-free\" algorithm,
   which is faster for typical access but slower for rehashing or growing the table.
   Otherwise, all hash tables default to the lock-free algorithm")

(defun make-hash-table (&key (test 'eql)
                             (size 60)
                             (rehash-size 1.5)
                             (rehash-threshold .85)
                             (hash-function nil)
                             (weak nil)
                             (finalizeable nil)
                             (address-based t)  ;; Ignored
                             (lock-free *lock-free-hash-table-default*)
                             (shared *shared-hash-table-default*))
  "Create and return a new hash table. The keywords are as follows:
     :TEST -- Indicates what kind of test to use.
     :SIZE -- A hint as to how many elements will be put in this hash
       table.
     :REHASH-SIZE -- Indicates how to expand the table when it fills up.
       If an integer (which must be greater than 0), add space for that
       many elements. If a floating point number (which must be greater
       than 1.0), multiply the size by that amount.
     :REHASH-THRESHOLD -- Indicates how dense the table can become before
       forcing a rehash. Can be any positive number <=1, with density
       approaching zero as the threshold approaches 0. Density 1 means an
       average of one entry per bucket."
  (declare (ignore address-based)) ;; TODO: could reinterpret as "warn if becomes address-based"
  (unless (and test (or (functionp test) (symbolp test)))
    (report-bad-arg test '(and (not null) (or symbol function))))
  (unless (or (functionp hash-function) (symbolp hash-function))
    (report-bad-arg hash-function '(or symbol function)))
  (unless (and (realp rehash-threshold) (<= 0.0 rehash-threshold) (<= rehash-threshold 1.0))
    (report-bad-arg rehash-threshold '(real 0 1)))
  (unless (or (and (fixnump rehash-size) (<= 1 rehash-size))
              (and (realp rehash-size) (< 1.0 rehash-size)))
    (report-bad-arg rehash-size '(or (integer 1 *) (real (1) *))))
  (unless (fixnump size) (report-bad-arg size 'fixnum))
  (setq rehash-threshold (/ 1.0 (max 0.01 rehash-threshold)))
  (let* ((default-hash-function
             (cond ((or (eq test 'eq) (eq test #'eq)) 
                    (setq test 0))
                   ((or (eq test 'eql) (eq test #'eql)) 
                    (setq test -1))
                   ((or (eq test 'equal) (eq test #'equal))
                    (setq test #'equal) #'%%equalhash)
                   ((or (eq test 'equalp) (eq test #'equalp))
                    (setq test #'equalp) #'%%equalphash)
                   (t (setq test (require-type test 'symbol))
                      (or hash-function 
                          (error "non-standard test specified without hash-function")))))
         (find-function
          (case test
            (0 #'eq-hash-find)
            (-1 #'eql-hash-find)
            (t #'general-hash-find)))
         (find-put-function
          (case test
            (0 #'eq-hash-find-for-put)
            (-1 #'eql-hash-find-for-put)
            (t #'general-hash-find-for-put))))
    (setq hash-function
          (if hash-function
            (require-type hash-function 'symbol)
            default-hash-function))
    (when (and weak (neq weak :value) (neq test 0))
      (error "Only EQ hash tables can be weak."))
    (when (and finalizeable (not weak))
      (error "Only weak hash tables can be finalizeable."))
    (when (and (eq lock-free :shared) (not shared))
      (setq lock-free nil))
    (multiple-value-bind (grow-threshold total-size)
        (compute-hash-size size 0 rehash-threshold)
      (let* ((flags (+ (if weak (ash 1 $nhash_weak_bit) 0)
                       (ecase weak
                         ((t nil :key) 0)
                         (:value (ash 1 $nhash_weak_value_bit)))
                       (if finalizeable (ash 1 $nhash_finalizeable_bit) 0)
                       (if lock-free (ash 1 $nhash_keys_frozen_bit) 0)))
             (hash (%cons-hash-table 
                    hash-function test
                    (%cons-nhash-vector total-size flags)
                    grow-threshold rehash-threshold rehash-size
                    find-function find-put-function
                    (unless shared *current-process*)
                    lock-free
                    size)))
        (setf (nhash.vector.hash (nhash.vector hash)) hash)
        hash))))

(defun compute-hash-size (size rehash-size rehash-ratio)
  (let* ((new-size (max 30 (if (fixnump rehash-size)
                             (%i+ size rehash-size)
                             (max (1+ size) (ceiling (* size rehash-size)))))))
    (declare (fixnum size new-size))
    (let ((vector-size (%hash-size (max (+ new-size 2) (ceiling (* new-size rehash-ratio))))))
      ; TODO: perhaps allow more entries, based on actual size:
      ;  (values (min (floor vector-size rehash-ratio) (%i- vector-size 2)) vector-size))
      (values new-size vector-size)
      )))

;;;  Suggested size is a fixnum: number of pairs.  Return a fixnum >=
;;;  that size that is relatively prime to all secondary keys.
(defun %hash-size (suggestion)
  (declare (fixnum suggestion))
  (declare (optimize (speed 3)(safety 0)))
  (if (<= suggestion #.(aref secondary-keys 7))
    (setq suggestion (+ 2 #.(aref secondary-keys 7)))
     (setq suggestion (logior 1 suggestion)))
  (loop
    (dovector (key secondary-keys (return-from %hash-size suggestion))
      (when (eql 0 (fast-mod suggestion key))
        (return)))
    (incf suggestion 2)))


(defvar *continue-from-readonly-hashtable-lock-error* t)

(defun signal-read-only-hash-table-error (hash)
  (cond ((hash-lock-free-p hash)
         ;; We don't really do anything different if this is set, so no problem
         (cerror "Modify it anyway"
                 "Attempt to modify readonly hash table ~s" hash))
        (*continue-from-readonly-hashtable-lock-error*
         (cerror "Make the hash-table writable. DANGEROUS! This could damage your lisp if another thread is acccessing this table. CONTINUE ONLY IF YOU KNOW WHAT YOU'RE DOING!"
                 "Hash-table ~s is readonly" hash)
         (assert-hash-table-writeable hash)
         (write-lock-hash-table hash))
        (t (error "Hash-table ~s is readonly" hash))))

(defun read-lock-hash-table (hash)
  (if (nhash.read-only hash)
    :readonly
    (let* ((lock (nhash.exclusion-lock hash)))
      (if lock
        (read-lock-rwlock lock)
        (unless (eq (nhash.owner hash) *current-process*)
          (error "Not owner of hash table ~s" hash))))))

(defun write-lock-hash-table (hash)
  (if (nhash.read-only hash)
    (signal-read-only-hash-table-error hash)
    (let* ((lock (nhash.exclusion-lock hash)))
      (if lock
        (write-lock-rwlock lock)
        (unless (eq (nhash.owner hash) *current-process*)
          (error "Not owner of hash table ~s" hash))))))


(defun unlock-hash-table (hash was-readonly)
  (unless was-readonly
    (let* ((lock (nhash.exclusion-lock hash)))
      (if lock
        (unlock-rwlock lock)))))

(defun index->vector-index (index)
  (declare (fixnum index))
  (the fixnum (+ $nhash.vector_overhead (the fixnum (+ index index)))))

(defun vector-index->index (index)
  (declare (fixnum index))
  (the fixnum (ash (the fixnum (- index $nhash.vector_overhead)) -1)))

(defun hash-table-count (hash)
  "Return the number of entries in the given HASH-TABLE."
  (setq hash (require-type hash 'hash-table))
  (when (hash-lock-free-p hash)
    (return-from hash-table-count (lock-free-hash-table-count hash)))
  (the fixnum (nhash.vector.count (nhash.vector hash))))

(defun hash-table-rehash-size (hash)
  "Return the rehash-size HASH-TABLE was created with."
  (nhash.rehash-size (require-type hash 'hash-table)))

(defun hash-table-rehash-threshold (hash)
  "Return the rehash-threshold HASH-TABLE was created with."
  (/ 1.0 (nhash.rehash-ratio (require-type hash 'hash-table))))

(defun hash-table-size (hash)
  "Return a size that can be used with MAKE-HASH-TABLE to create a hash
   table that can hold however many entries HASH-TABLE can hold without
   having to be grown."
  (let* ((hash (require-type hash 'hash-table))
         (vector (nhash.vector hash)))
    (values (floor (nhash.vector.size vector) (nhash.rehash-ratio hash)))))

(defun hash-table-test (hash)
  "Return the test HASH-TABLE was created with."
  (let ((f (nhash.compareF (require-type hash 'hash-table))))
    (if (fixnump f)
      (if (eql 0 f) 'eq 'eql)
      (let ((name (if (symbolp f) f (function-name f))))
        (if (memq name '(equal equalp)) name f)))))

;;; sometimes you'd rather have the function than the symbol.
(defun hash-table-test-function (hash)
  (let ((f (nhash.compareF (require-type hash 'hash-table))))
    (if (fixnump f)
      (if (eql 0 f) #'eq #'eql)
      f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; nearly-lock-free hash tables
;;
;; A modification of the lock-free hash table algorithm described by Cliff Click Jr.  in
;; http://blogs.azulsystems.com/cliff/2007/03/a_nonblocking_h.html.
;;
;; The modifications have to do with the fact that our goal is just to minimize the
;; performance impact of thread-safety, by eliminating the need for locking on every
;; read.  I don't bother with aspects of his algorithm that aren't relevant to that goal.
;;
;; The main difference from Click's algorithm is that I don't try to do rehashing
;; concurrently.  Instead, rehashing grabs a lock, so that only one thread can be
;; rehashing at any given time, and readers/writers will block waiting for the rehashing
;; to finish.
;;
;; In our implementation the following are the possible states of a hash table entry:
;;   (where "object" means any object other than the special markers):
;;
;; State      Key               Value
;; FREE       free-hash-marker  free-hash-marker
;; INSERTING  object            free-hash-marker
;; DELETING0  deleted-marker    free-hash-marker  ;; abandoned insert
;; IN-USE     object            object
;; DELETING1  object            deleted-marker
;; DELETING2  deleted-marker    object
;; DELETED    deleted-marker    deleted-marker
;; REHASHING  object            rehashing-value-marker
;; REHASHING  free-hash-marker  rehashing-value-marker
;; REHASHING  deleted-marker    rehashing-value-marker
;;
;; No other states are allowed - at no point in time can a hash table entry be in any
;; other state.   In addition, the only transitions allowed on the key slot are
;; free-hash-marker -> object/deleted-marker -> deleted-marker.  Once a key slot
;; is claimed, it must never change to free or another key value (even after the hash
;; vector has been discarded after rehashing, because some process might still be
;; looking at it).
;; In particular, rehashing in place is not an option.  All rehashing creates a new
;; vector and copies into it.  This means it's kinda risky to use lock-free hash
;; tables with address-based keys, because they will thrash in low-memory situations,
;; but we don't disallow it because a particular use might not have this problem.
;;
;; The following operations may take place:
;;
;; * gethash: find matching key - if no match, return not found.  Else fetch value,
;;   if value is rehashing-value-marker then maybe-rehash and try again;
;;   if value is free-hash-marker or deleted-marker, return not found, else return found value.
;; * puthash: find matching key or FREE slot.
;;   ** If found key, fetch value.
;;      if value is rehashing-value-marker then maybe-rehash and try again;
;;      else store-conditional the value -> new value, if fails try again.
;;   ** Else have FREE slot, store-key-conditional free-hash-marker -> key,
;;      if that succeeds, check whether key moved (invalidating the hash), if so,
;;         back out to DELETED0 and try again,
;;      else store-conditional free-hash-marker -> new value,
;;      if either store fails, maybe-rehash and try again.
;; * remhash: find matching key - if no match, done.  Else fetch value,
;;   if value is rehashing-value-marker then maybe-rehash and try again;
;;   if value is free-hash-marker or deleted-marker, done.
;;   else store-conditional the value -> deleted-marker, if fails try again.
;;    if succeeds, clobber key with deleted-marker to allow it to get gc'd.
;; * clrhash: grab the rehash lock, then set all slots to DELETED (transitioning through either
;;    DELETING1 or DELETING2 state).
;; * rehash: grab a lock, estimate number of entries, make a new vector.  loop over
;; old vector, at each entry fetch the old value with atomic swap of
;; rehashing-value-marker.  This prevents any further state changes involving the
;; value.  It doesn't prevent state changes involving the key, but the only ones that
;; can happen is FREE -> INSERTING, and DELETINGn -> DELETED, all of which are
;; equivalent from the point of view of rehashing.  Anyway, if the old value was
;; rehashing-value-marker then bug (because we have a lock).  If the old value is
;; free-hash-marker or deleted-marker then do nothing, else get the entry key and
;; rehash into the new vector -- if no more room, start over.  When done, store the
;; new vector in the hash table and release lock.
;;
;; * gc: for weak tables, gc may convert IN-USE states to DELETED states.


(defun lock-free-rehash (hash)
  ;;(break "We think we need to rehash ~s" (nhash.vector hash))
  (with-lock-context
    (without-interrupts ;; not re-entrant
      (let ((lock (nhash.exclusion-lock hash)))
        (%lock-recursive-lock-object lock)
        ;; TODO: might also want to rehash if deleted entries are a large percentage
        ;; of all entries, more or less.
        (when (or (%i<= (nhash.grow-threshold hash) 0) ;; no room
                  (%needs-rehashing-p (nhash.vector hash))) ;; or keys moved
          (%lock-free-rehash hash))
        (%unlock-recursive-lock-object lock)))))


;; TODO: This is silly.  We're implementing atomic swap using store-conditional,
;; but internally store-conditional is probably implemented using some kind of
;; an atomic swap!!
(defun atomic-swap-gvector (index gvector value)
  (loop
    (let ((old-value (%svref gvector index)))
      (when (store-gvector-conditional index gvector old-value value)
        (return old-value)))))

;; Interrupts are disabled and caller has the hash lock on the table, blocking other
;; threads attempting a rehash.
;; Other threads might be reading/writing/deleting individual entries, but they
;; will block if they see a value = rehashing-value-marker.
;; GC may run, updating the needs-rehashing flags and deleting weak entries in both
;; old and new vectors.
(defun %lock-free-rehash (hash)
  (let* ((old-vector (nhash.vector hash))
         (inherited-flags (logand $nhash_weak_flags_mask (nhash.vector.flags old-vector)))
         (grow-threshold (nhash.grow-threshold hash))
         count new-vector vector-size)
    ;; Prevent puthash from adding new entries.
    (setf (nhash.grow-threshold hash) 0)
    (setq count (lock-free-count-entries hash))
    (multiple-value-setq (grow-threshold vector-size)
      (if (%i<= grow-threshold 0) ; if ran out of room, grow, else get just enough.
        (compute-hash-size count (nhash.rehash-size hash) (nhash.rehash-ratio hash))
        (compute-hash-size (max (nhash.min-size hash) (+ count grow-threshold)) 0 (nhash.rehash-ratio hash))))
    (setq new-vector (%cons-nhash-vector vector-size inherited-flags))
    (loop with full-count = grow-threshold
          for i from $nhash.vector_overhead below (uvsize old-vector) by 2
          do (let* ((value (atomic-swap-gvector (%i+ i 1) old-vector rehashing-value-marker))
                    (key (%svref old-vector i)))
               (when (eq value rehashing-value-marker) (error "Who else is doing this?"))
               (unless (or (eq value free-hash-marker)
                           (eq value deleted-hash-value-marker)
                           (eq key deleted-hash-key-marker))
                 (let* ((new-index (%growhash-probe new-vector hash key))
                        (new-vector-index (index->vector-index new-index)))
                   (%set-hash-table-vector-key new-vector new-vector-index key)
                   (setf (%svref new-vector (%i+ new-vector-index 1)) value)
                   (decf grow-threshold)
                   (when (%i<= grow-threshold 0)
		     (error "Bug: undeleted entries?")))))
          finally (setf (nhash.vector.count new-vector) (- full-count grow-threshold)))

    (when (%needs-rehashing-p new-vector) ;; keys moved, but at least can use the same new-vector.
      (%lock-free-rehash-in-place hash new-vector))
    (setf (nhash.vector.hash new-vector) hash)
    (setf (nhash.vector hash) new-vector)
    (setf (nhash.grow-threshold hash) grow-threshold)))

;; This is called on a new vector that hasn't been installed yet, so no other thread is
;; accessing it.  However, gc might be deleting stuff from it, which is why it tests
;; key for deleted-hash-key-marker in addition to free-hash-marker value
(defun %lock-free-rehash-in-place (hash vector)
  (let* ((vector-index (- $nhash.vector_overhead 2))
         (size (nhash.vector-size vector))
         (rehash-bits (%make-rehash-bits hash size))
         (index -1))
    (declare (fixnum size index vector-index))
    (%set-does-not-need-rehashing vector)
    (loop
      (when (>= (incf index) size) (return))
      (setq vector-index (+ vector-index 2))
      (unless (%already-rehashed-p index rehash-bits)
        (let* ((key (%svref vector vector-index)))
	  (if (or (eq key free-hash-marker) (eq key deleted-hash-key-marker))
	    (unless (eq key free-hash-marker)
	      (setf (%svref vector vector-index) free-hash-marker
		    (%svref vector (%i+ vector-index 1)) free-hash-marker))
            (let* ((last-index index)
		   (value (%svref vector (%i+ vector-index 1)))
                   (first t))
              (loop
                (let ((found-index (%rehash-probe rehash-bits hash key vector)))
                  (%set-already-rehashed-p found-index rehash-bits)
                  (when (eq last-index found-index)
                    (return))
                  (let* ((found-vector-index (index->vector-index found-index))
                         (newvalue (%svref vector (the fixnum (1+ found-vector-index))))
                         (newkey (%svref vector found-vector-index)))
                    (declare (fixnum found-vector-index))
                    (when first         ; or (eq last-index index) ?
                      (setq first nil)
                      (setf (%svref vector (the fixnum (1+ vector-index))) free-hash-marker)
                      (setf (%svref vector vector-index) free-hash-marker))
                    (%set-hash-table-vector-key vector found-vector-index key)
                    (setf (%svref vector (the fixnum (1+ found-vector-index))) value)
                    (when (or (eq newkey deleted-hash-key-marker) (eq newkey free-hash-marker))
                      (return))
                    (when (eq key newkey)
                      (cerror "Delete one of the entries." "Duplicate key: ~s in ~s ~s ~s ~s ~s"
                              key hash value newvalue index found-index)                       
                      (return))
                    (setq key newkey
                          value newvalue
                          last-index found-index))))))))))
  t )


(defun lock-free-gethash (key hash default)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (loop
    (let* ((vector (nhash.vector hash))
           (vector-index (funcall (the function (nhash.find hash)) hash key)))
      (declare (fixnum vector-index))
      ;; Need to punt if vector changed because no way to know whether nhash.find was
      ;; using old or new vector.
      (when (eq vector (nhash.vector hash))
        (cond ((eql vector-index -1)
               (unless (%needs-rehashing-p vector)
                 (return-from lock-free-gethash (values default nil))))
              (t (let ((value (%svref vector (%i+ vector-index 1))))
                   (unless (eq value rehashing-value-marker)
                     (if (or (eq value deleted-hash-value-marker)
                             (eq value free-hash-marker))
                       (return-from lock-free-gethash (values default nil))
                       (return-from lock-free-gethash (values value t)))))))))
    ;; We're here because the table needs rehashing or it was getting rehashed while we
    ;; were searching. Take care of it and try again.
    (lock-free-rehash hash)))

(defun lock-free-remhash (key hash)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (nhash.read-only hash)
    (signal-read-only-hash-table-error hash)) ;; continuable
  (loop
    (let* ((vector (nhash.vector hash))
           (vector-index (funcall (the function (nhash.find hash)) hash key)))
      (declare (fixnum vector-index))
      ;; Need to punt if vector changed because no way to know whether nhash.find was
      ;; using old or new vector.
      (when (eq vector (nhash.vector hash))
        (cond ((eql vector-index -1)
               (unless (%needs-rehashing-p vector)
                 (return-from lock-free-remhash nil)))
              (t (let ((old-value (%svref vector (%i+ vector-index 1)))
		       (old-key (%svref vector vector-index)))
                   (unless (eq old-value rehashing-value-marker)
                     (when (or (eq old-value deleted-hash-value-marker) ;; deleted
                               (eq old-value free-hash-marker) ;; or about to be added
			       (eq old-key deleted-hash-key-marker)) ;; or in clrhash...
                       (return-from lock-free-remhash nil))
		     (when (without-interrupts
			     (when (set-hash-value-conditional vector-index vector old-value deleted-hash-value-marker)
			       (atomic-decf (nhash.vector.count vector))
			       t))
		       ;; Clear the key slot so can be gc'd.
		       (setf (%svref vector vector-index) deleted-hash-key-marker)
                       (return-from lock-free-remhash t)))))))
      ;; We're here because the table needs rehashing or it was getting rehashed while we
      ;; were searching.  Take care of it and try again.
      (lock-free-rehash hash))))

(defun replace-nhash-vector (hash size flags)
  (let ((vector (%cons-nhash-vector size flags)))
    (setf (nhash.vector.hash vector) hash)
    (setf (nhash.vector hash) vector)))

(defun lock-free-clrhash (hash)
  (when (nhash.read-only hash)
    (signal-read-only-hash-table-error hash)) ;;continuable
  (with-lock-context
      (without-interrupts
        (let ((lock (nhash.exclusion-lock hash)))
          (%lock-recursive-lock-object lock)    ;; disallow rehashing (or other clrhashing)
          ;; Note that since we can't reuse deleted slots, deleting entries doesn't increase capacity.
          ;; As a heuristic, reuse existing vector if there is enough capacity left to grow again to
          ;; current size, otherwise make a fresh one.
          (if (< (lock-free-hash-table-count hash) (nhash.grow-threshold hash))
            (loop
              with vector = (nhash.vector hash)
              for i fixnum from (%i+ $nhash.vector_overhead 1) below (uvsize vector) by 2
              as val = (%svref vector i)
              unless (or (eq val free-hash-marker) (eq val deleted-hash-value-marker))
              do (setf (%svref vector (%i- i 1)) deleted-hash-key-marker
                       (%svref vector i) deleted-hash-value-marker)
              finally (setf (nhash.vector.count vector) 0))
            (multiple-value-bind (grow-threshold vector-size)
                                 (compute-hash-size (nhash.min-size hash) 0 (nhash.rehash-ratio hash))
              (setf (nhash.grow-threshold hash) 0) ;; prevent puthash from adding new entries
              (loop with vector = (nhash.vector hash) ;; mark entries as obsolete
                for i fixnum from (%i+ $nhash.vector_overhead 1) below (uvsize vector) by 2
                do (setf (%svref vector i) rehashing-value-marker))
              (let ((flags (logand $nhash_weak_flags_mask (nhash.vector.flags (nhash.vector hash)))))
		(when (> vector-size 1000)
		  ;; Install a tiny temp vector to let the old one get gc'd before consing a big new vector.
		  (replace-nhash-vector hash 1 0))
                (replace-nhash-vector hash vector-size flags))
              (setf (nhash.grow-threshold hash) grow-threshold)))
          (%unlock-recursive-lock-object lock))))
  hash)

(defun lock-free-puthash (key hash value)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (when (or (eq value rehashing-value-marker)
            (eq value free-hash-marker)
            (eq value deleted-hash-value-marker))
    (error "Illegal value ~s for storing in a hash table" value))
  (when (nhash.read-only hash)
    (signal-read-only-hash-table-error hash)) ;;continuable
  (loop
    (let* ((vector (nhash.vector hash))
	   (address (strip-tag-to-fixnum key))
	   ;; This also makes sure the vector's track_keys bit is set if key is address based (in an eq table),
	   ;; and component_address bit is set if a key component is address based (in an equal[p] table).
           (vector-index (funcall (nhash.find-new hash) hash key)))
      ;; Need to punt if vector changed because no way to know whether nhash.find-new was
      ;; using old or new vector.
      (when (eq vector (nhash.vector hash))
        (cond ((or (eql vector-index -1)
                   (eq (%svref vector vector-index) free-hash-marker))
               (unless (or (%needs-rehashing-p vector)
                           (%i<= (nhash.grow-threshold hash) 0))
                 ;; Note if the puthash fails, grow-threshold will end up too small. This
                 ;; just means we might rehash sooner than absolutely necessary, no real
                 ;; harm done (the most likely cause of failing is that somebody is
                 ;; already rehashing anyway).  DON'T try to incf it back on failure --
                 ;; that risks grow-threshold ending up too big (e.g. if somebody rehashes
                 ;; before the incf), which _could_ be harmful.
                 (atomic-decf (nhash.grow-threshold hash))
                 (when (set-hash-key-conditional vector-index vector free-hash-marker key)
		   ;; %needs-rehashing-p is not quite enough in the track_keys case, since gc cannot
		   ;; track this key until it's actually added to the table.  Check now.
		   (if (and (%ilogbitp $nhash_track_keys_bit (nhash.vector.flags vector))
			    (not (eq address (strip-tag-to-fixnum key))))
		     (setf (%svref vector vector-index) deleted-hash-key-marker)		       ;; Back out and try again.
		     (when (without-interrupts
			     (when (set-hash-value-conditional vector-index vector free-hash-marker value)
			       (atomic-incf (nhash.vector.count vector))
			       t))
		       (return-from lock-free-puthash value))))))
              (t (let ((old-value (%svref vector (%i+ vector-index 1))))
                   (unless (or (eq old-value rehashing-value-marker)
                               ;; In theory, could reuse the deleted slot since we know it had this key
                               ;; initially, but that would complicate the state machine for very little gain.
                               (eq old-value deleted-hash-value-marker)
                               ;; This means we're competing with someone inserting this key.  We could continue
                               ;; except then would have to sync up nhash.vector.count, so don't.
                               (eq old-value free-hash-marker))
                     (when (set-hash-value-conditional vector-index vector old-value value)
                       (return-from lock-free-puthash value))))))))
    ;; We're here because the table needs rehashing or it was getting rehashed while we
    ;; were searching, or no room for new entry, or somebody else claimed the key from
    ;; under us (that last case doesn't need to retry, but it's unlikely enough that
    ;; it's not worth checking for).  Take care of it and try again.
    (lock-free-rehash hash)))

(defun lock-free-hash-table-count (hash)
  (let* ((vector (nhash.vector hash))
	 (count (nhash.vector.count vector)))
    #+debug-hash
    (let ((entries (lock-free-count-entries hash)))
	   (unless (eq entries count)
	     (error "hash count mismatch, count=~s actual ~s" count entries)))
    count))

(defun lock-free-count-entries (hash)
  ;; Other threads could be adding/removing entries while we count, some of
  ;; which will be included in the count (i.e. will be treated as if they
  ;; happened after counting) and some won't (i.e. will be treated as if
  ;; they happened before counting), but not necessarily in correlation
  ;; with their temporal relationship.
  (loop
    with vector = (nhash.vector hash)
    for i fixnum from $nhash.vector_overhead below (uvsize vector) by 2
    count (let ((value (%svref vector (%i+ i 1)))
                (key (%svref vector i)))
            (when (eq value rehashing-value-marker)
              ;; This table is being rehashed.  Wait for it to be
              ;; done and try again.
              (lock-free-rehash hash)
              (return-from lock-free-count-entries (lock-free-count-entries hash)))
            (and (neq value free-hash-marker)
                 (neq value deleted-hash-value-marker)
                 (neq key deleted-hash-key-marker)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun gethash (key hash &optional default)
  "Finds the entry in HASH-TABLE whose key is KEY and returns the associated
   value and T as multiple values, or returns DEFAULT and NIL if there is no
   such entry. Entries can be added using SETF."
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (or (eq key free-hash-marker)
            (eq key deleted-hash-key-marker))
    (return-from gethash (values default nil)))
  (when (hash-lock-free-p hash)
    (return-from gethash (lock-free-gethash key hash default)))
  (let* ((value nil)
         (gc-locked nil)
         (readonly nil)
         (foundp nil))
    (with-lock-context
      (without-interrupts
        (setq readonly (eq (read-lock-hash-table hash) :readonly))
        (let* ((vector (nhash.vector hash)))
          (if (and (eq key (nhash.vector.cache-key vector))
                   ;; Check twice: the GC might nuke the cached key/value pair
                   (progn (setq value (nhash.vector.cache-value vector))
                          (eq key (nhash.vector.cache-key vector))))
            (setq foundp t)
            (loop
              (let* ((vector-index (funcall (nhash.find hash) hash key)))
                (declare (fixnum vector-index))
                (cond ((setq foundp (not (eql vector-index -1)))
                       ;; Referencing both key and value here - and referencing
                       ;; value first - is an attempt to compensate for the
                       ;; possibility that the GC deletes a weak-on-key pair.
                       (setq value (%svref vector (%i+ vector-index 1)))
                       (when (nhash.owner hash)
                         (setf (nhash.vector.cache-key vector)
                               (%svref vector vector-index)
                               (nhash.vector.cache-value vector)
                               value
                               (nhash.vector.cache-idx vector)
                               (vector-index->index (the fixnum vector-index))))
                       (return))
                      ((%needs-rehashing-p vector)
                       (%lock-gc-lock)
                       (setq gc-locked t)
                       (unless readonly
                         (let* ((lock (nhash.exclusion-lock hash)))
                           (when lock (%promote-rwlock lock))))
                       (when (%needs-rehashing-p vector)
                         (%rehash hash)))
                      (t (return)))))))
        (when gc-locked (%unlock-gc-lock))
        (unlock-hash-table hash readonly)))
    (if foundp
      (values value t)
      (values default nil))))

(defun remhash (key hash)
  "Remove the entry in HASH-TABLE associated with KEY. Return T if there
   was such an entry, or NIL if not."
  (unless (typep hash 'hash-table)
    (setq hash (require-type hash 'hash-table)))
  (when (hash-lock-free-p hash)
    (return-from remhash (lock-free-remhash key hash)))
  (let* ((foundp nil))
    (with-lock-context
      (without-interrupts
       (write-lock-hash-table hash)
       (%lock-gc-lock)
       (let* ((vector (nhash.vector hash)))
         (when (%needs-rehashing-p vector)
           (%rehash hash))
         (if (eq key (nhash.vector.cache-key vector))
           (progn
             (setf (nhash.vector.cache-key vector) free-hash-marker
                   (nhash.vector.cache-value vector) nil)
             (let ((vidx (index->vector-index (nhash.vector.cache-idx vector))))
               (setf (%svref vector vidx) deleted-hash-key-marker)
               (setf (%svref vector (the fixnum (1+ vidx))) nil))
             (atomic-incf (the fixnum (nhash.vector.deleted-count vector)))
             (atomic-decf (the fixnum (nhash.vector.count vector)))
             (setq foundp t))
           (let* ((vector-index (funcall (nhash.find hash) hash key)))
             (declare (fixnum vector-index))
             (unless (eql vector-index -1)
               ;; always clear the cache cause I'm too lazy to call the
               ;; comparison function and don't want to keep a possibly
               ;; deleted key from being GC'd
               (setf (nhash.vector.cache-key vector) free-hash-marker
                     (nhash.vector.cache-value vector) nil)
               ;; Update the count
               (atomic-incf (the fixnum (nhash.vector.deleted-count vector)))
               (atomic-decf (the fixnum (nhash.vector.count vector)))
               ;; Delete the value from the table.
               (setf (%svref vector vector-index) deleted-hash-key-marker
                     (%svref vector (the fixnum (1+ vector-index))) nil)
               (setq foundp t)))))
       ;; Return T if we deleted something
       (%unlock-gc-lock)
       (unlock-hash-table hash nil)))
    foundp))

;;; what if somebody is mapping, growing, rehashing? 
(defun clrhash (hash)
  "This removes all the entries from HASH-TABLE and returns the hash table
   itself."
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (hash-lock-free-p hash)
    (return-from clrhash (lock-free-clrhash hash)))
  (with-lock-context
    (without-interrupts
     (write-lock-hash-table hash)
     (let* ((vector (nhash.vector hash))
            (size (nhash.vector-size vector))
            (count (+ size size))
            (index $nhash.vector_overhead))
       (declare (fixnum size count index))
       (dotimes (i count)
         (setf (%svref vector index) free-hash-marker)
         (incf index))
       (incf (the fixnum (nhash.grow-threshold hash))
             (the fixnum (+ (the fixnum (nhash.vector.count vector))
                            (the fixnum (nhash.vector.deleted-count vector)))))
       (setf (nhash.vector.count vector) 0
             (nhash.vector.cache-key vector) free-hash-marker
             (nhash.vector.cache-value vector) nil
             (nhash.vector.finalization-alist vector) nil
             (nhash.vector.free-alist vector) nil
             (nhash.vector.deleted-count vector) 0
             (nhash.vector.flags vector) (logand $nhash_weak_flags_mask
                                                 (nhash.vector.flags vector))))
     (unlock-hash-table hash nil)
     hash)))

(defun invalid-hash-key-p (key)
  ;; Anything else ?
  (or (eq key free-hash-marker)
      (eq key deleted-hash-key-marker)))

(defun puthash (key hash default &optional (value default))
  (declare (optimize (speed 3) (space 0)))
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (invalid-hash-key-p key)
    (error "Can't use ~s as a hash-table key" key))
  (when (hash-lock-free-p hash)
    (return-from puthash (lock-free-puthash key hash value)))
  (with-lock-context
    (without-interrupts
     (block protected
       (tagbody
          (write-lock-hash-table hash)
        AGAIN
          (%lock-gc-lock)
          (let ((vector (nhash.vector hash)))
            (when (%needs-rehashing-p vector)
              (%rehash hash))
            (when (eq key (nhash.vector.cache-key vector))
              (let* ((idx (nhash.vector.cache-idx vector)))
                (declare (fixnum idx))
                (setf (%svref vector (the fixnum (1+ (the fixnum (index->vector-index idx)))))
                      value)
                (setf (nhash.vector.cache-value vector) value)
                (return-from protected)))               
            (let* ((vector-index (funcall (nhash.find-new hash) hash key))
                   (old-value (%svref vector vector-index)))
              (declare (fixnum vector-index))

              (cond ((eq old-value deleted-hash-key-marker)
                     (%set-hash-table-vector-key vector vector-index key)
                     (setf (%svref vector (the fixnum (1+ vector-index))) value)
                     (atomic-incf (nhash.vector.count vector))
                     ;; Adjust deleted-count
                     (atomic-decf (nhash.vector.deleted-count vector)))
                    ((eq old-value free-hash-marker)
                     (when (eql 0 (nhash.grow-threshold hash))
                       (%unlock-gc-lock)
                       (%grow-hash-table hash)
                       (go AGAIN))
                     (%set-hash-table-vector-key vector vector-index key)
                     (setf (%svref vector (the fixnum (1+ vector-index))) value)
                     (decf (the fixnum (nhash.grow-threshold hash)))
                     (atomic-incf (the fixnum (nhash.vector.count vector))))
                    (t
                     ;; Key was already there, update value.
                     (setf (%svref vector (the fixnum (1+ vector-index))) value)))
              (setf (nhash.vector.cache-idx vector) (vector-index->index vector-index)
                    (nhash.vector.cache-key vector) key
                    (nhash.vector.cache-value vector) value)))))
     (%unlock-gc-lock)
     (unlock-hash-table hash nil)))
  value)


(defun count-entries (hash)
  (if (hash-lock-free-p hash)
    (lock-free-count-entries hash)
    (let* ((vector (nhash.vector hash))
           (size (uvsize vector))
           (idx $nhash.vector_overhead)
           (count 0))
      (loop
        (when (neq (%svref vector idx) free-hash-marker)
          (incf count))
        (when (>= (setq idx (+ idx 2)) size)
          (return count))))))

(defun grow-hash-table (hash)
  (unless (typep hash 'hash-table)
    (setq hash (require-type hash 'hash-table)))
  (%grow-hash-table hash))

(defun %grow-hash-table-in-place-p (hash)
  ;; Arbitrarily: if the number of deleted entries is > half
  ;; the number of used entries, do an in-place rehash.
  (let* ((vec (nhash.vector hash)))
    (> (the fixnum (nhash.vector.deleted-count vec))
       (the fixnum (ash (the fixnum (nhash.vector.count vec)) -1)))))
            

;;; Interrupts are disabled, and the caller has an exclusive
;;; lock on the hash table.
(defun %grow-hash-table (hash)
  (block grow-hash-table
    (let* ((old-vector (nhash.vector hash))
           (old-size (nhash.vector.count old-vector))
           (old-total-size (nhash.vector.size old-vector))
           (flags 0)
           (flags-sans-weak 0)
           (weak-flags nil))
      (declare (fixnum old-total-size flags flags-sans-weak))
      (when (%grow-hash-table-in-place-p hash)
        ;; There are enough deleted entries. Rehash to get rid of them
        (%rehash hash)
        (return-from grow-hash-table))
      (multiple-value-bind (size total-size)
                           (compute-hash-size 
                            old-size (nhash.rehash-size hash) (nhash.rehash-ratio hash))
        (unless (eql 0 (nhash.grow-threshold hash))       ; maybe it's done already - shouldnt happen                
          (return-from grow-hash-table ))
        (progn
          (unwind-protect
            (let ((gc-count (%get-gc-count))
                  vector)
              (setq flags (nhash.vector.flags old-vector)
                    flags-sans-weak (logand flags (logxor -1 $nhash_weak_flags_mask))
                    weak-flags (logand flags $nhash_weak_flags_mask))
              (setf (nhash.vector.flags old-vector) flags-sans-weak)      ; disable GC weak stuff
              (when (%grow-hash-table-in-place-p hash)
                (setf (nhash.vector.flags old-vector) flags)
                (setq weak-flags nil)
                (return-from grow-hash-table (%rehash hash)))
              (setq vector (%cons-nhash-vector total-size 0))
              (do* ((index 0 (1+ index))
                    (vector-index (index->vector-index 0) (+ vector-index 2)))
                   ((>= index old-total-size))
                (declare (fixnum index vector-index))
                
                 (let ((key (%svref old-vector vector-index)))
                   (unless (or (eq key free-hash-marker)
                               (eq key deleted-hash-key-marker))
                     (let* ((new-index (%growhash-probe vector hash key))
                            (new-vector-index (index->vector-index new-index)))
                       (setf (%svref vector new-vector-index) key)
                       (setf (%svref vector (the fixnum (1+ new-vector-index)))
                             (%svref old-vector (the fixnum (1+ vector-index))))))))
              (progn
               (setf (nhash.vector.finalization-alist vector)
                     (nhash.vector.finalization-alist old-vector)
                     (nhash.vector.free-alist vector)
                     (nhash.vector.free-alist old-vector)
                     (nhash.vector.count vector) old-size
                     (nhash.vector.flags vector)
                     (logior (the fixnum weak-flags)
                             (the fixnum (nhash.vector.flags vector))))
               (setf (nhash.rehash-bits hash) nil
                     (nhash.vector hash) vector
                     (nhash.vector.hash vector) hash
                     (nhash.vector.cache-key vector) free-hash-marker
                     (nhash.vector.cache-value vector) nil
                     (nhash.vector.gc-count vector) gc-count
                     (nhash.grow-threshold hash) (- size old-size))
               (setq weak-flags nil)       ; tell clean-up form we finished the loop
               ;; If the old vector's in some static heap, zero it
               ;; so that less garbage is retained.
	       (%init-misc 0 old-vector)))
            (when weak-flags
              (setf (nhash.vector.flags old-vector)
                    (logior (the fixnum weak-flags)
                            (the fixnum (nhash.vector.flags old-vector)))))))))))



(defun general-hash-find (hash key)
  (%hash-probe hash key nil))

(defun general-hash-find-for-put (hash key)
  (%hash-probe hash key (if (hash-lock-free-p hash) :free :reuse)))

;;; returns a single value:
;;;   index - the index in the vector for key (where it was or where
;;;           to insert if the current key at that index is deleted-hash-key-marker
;;;           or free-hash-marker)



(defun %hash-probe (hash key for-put-p)
  (declare (optimize (speed 3) (space 0)))
  (multiple-value-bind (hash-code index entries)
                       (compute-hash-code hash key for-put-p)
    (locally (declare (fixnum hash-code index entries))
      (let* ((compareF (nhash.compareF hash))
             (vector (nhash.vector hash))
             (vector-index 0)
             table-key
             (first-deleted-index nil))
        (declare (fixnum vector-index))
        (macrolet ((return-it (form)
                     `(return-from %hash-probe ,form)))
          (macrolet ((test-it (predicate)
                       (unless (listp predicate) (setq predicate (list predicate)))
                       `(progn
                          (setq vector-index (index->vector-index index)
                                table-key (%svref vector vector-index))
                          (cond ((eq table-key free-hash-marker)
                                 (return-it (if for-put-p
                                              (or first-deleted-index
                                                  vector-index)
                                              -1)))
                                ((eq table-key deleted-hash-key-marker)
                                 (when (and (eq for-put-p :reuse)
                                            (null first-deleted-index))
                                   (setq first-deleted-index vector-index)))
                                ((,@predicate key table-key)
                                 (return-it vector-index))))))
            (macrolet ((do-it (predicate)
                         `(progn
                            (test-it ,predicate)
                            ; First probe failed. Iterate on secondary key
                            (let ((initial-index index)
                                  (secondary-hash (%svref secondary-keys (logand 7 hash-code)))
                                  (DEBUG-COUNT 0))
                              (declare (fixnum secondary-hash initial-index))
                              (loop
                                (INCF DEBUG-COUNT)
                                (incf index secondary-hash)
                                (when (>= index entries)
                                  (decf index entries))
                                (when (eql index initial-index)
                                  (return-it (if for-put-p
                                               (or first-deleted-index
                                                   #+NOT-SO-HELPFUL (error "Bug: no room in table")
                                                   (bug (format nil "No room in table after ~s tests, ~%initial ~s index ~s entries ~s for-put-p ~s"
                                                                DEBUG-COUNT initial-index index entries for-put-p))
                                                   )
                                               -1)))
                                (test-it ,predicate))))))
              (if (fixnump comparef)
                ;; EQ or EQL hash table
                (if (or (eql 0 comparef)
                        (immediate-p-macro key)
                        (not (need-use-eql key)))
                  ;; EQ hash table or EQL == EQ for KEY
                  (do-it eq)
                  (do-it eql))
                ;; general compare function
                (do-it (funcall comparef))))))))))

(defun eq-hash-find (hash key)
  (declare (optimize (speed 3) (safety 0)))
  #+eq-hash-monitor (progn
                      (incf eq-hash-find-calls)
                      (incf eq-hash-find-probes))
  (let* ((vector (nhash.vector hash))
         (hash-code
          (let* ((typecode (typecode key)))
            (if (eq typecode target::tag-fixnum)
              (mixup-hash-code key)
              (if (eq typecode target::subtag-instance)
                (mixup-hash-code (instance.hash key))
                (if (symbolp key)
                  (%hash-symbol key)
                  (mixup-hash-code (strip-tag-to-fixnum key)))))))
         (entries (nhash.vector-size vector))
         (vector-index (index->vector-index (hash-mod hash-code entries vector)))
         (table-key (%svref vector vector-index)))
    (declare (fixnum hash-code  entries vector-index))
    (if (eq table-key key)
      vector-index
      (if (eq table-key free-hash-marker)
        -1
        (let* ((secondary-hash (%svref secondary-keys-*-2
                                       (logand 7 hash-code)))
               (initial-index vector-index)             
               (count (+ entries entries))
               (length (+ count $nhash.vector_overhead)))
          (declare (fixnum secondary-hash initial-index count length))
          (loop
            #+eq-hash-monitor (incf eq-hash-find-probes)
            (incf vector-index secondary-hash)
            (when (>= vector-index length)
              (decf vector-index count))
            (setq table-key (%svref vector vector-index))
            (when (= vector-index initial-index)
              (return -1))
            (if (eq table-key key)
              (return vector-index)
              (when (eq table-key free-hash-marker)
                (return -1)))))))))

;;; As above, but note whether the key is in some way address-based
;;; and update the hash-vector's flags word if so.
;;; This only needs to be done by PUTHASH, and it only really needs
;;; to be done if we're adding a new key.
(defun eq-hash-find-for-put (hash key)
  (declare (optimize (speed 3) (safety 0)))
  #+eq-hash-monitor (progn
                      (incf eq-hash-find-for-put-calls)
                      (incf eq-hash-find-for-put-probes))
  (let* ((vector (nhash.vector hash))
         (hash-code
          (let* ((typecode (typecode key)))
            (if (eq typecode target::tag-fixnum)
              (mixup-hash-code key)
              (if (eq typecode target::subtag-instance)
                (mixup-hash-code (instance.hash key))
                (if (symbolp key)
                  (%hash-symbol key)
                  (progn
                    (unless (immediate-p-macro key)
                      (update-hash-flags hash vector :key))
                    (mixup-hash-code (strip-tag-to-fixnum key))))))))
         (entries (nhash.vector-size vector))
         (vector-index (index->vector-index (hash-mod hash-code entries vector)))
         (table-key (%svref vector vector-index))
         (reuse (not (hash-lock-free-p hash))))
    (declare (fixnum hash-code vector-index))
    (if (or (eq key table-key)
            (eq table-key free-hash-marker))
      vector-index
      (let* ((secondary-hash (%svref secondary-keys-*-2
                                     (logand 7 hash-code)))
             (initial-index vector-index)             
             (first-deleted-index (and reuse
                                       (eq table-key deleted-hash-key-marker)
                                       vector-index))
             (count (+ entries entries))
             (length (+ count $nhash.vector_overhead)))
        (declare (fixnum secondary-hash initial-index count length))
        (loop
          #+eq-hash-monitor (incf eq-hash-find-for-put-probes)
          (incf vector-index secondary-hash)
          (when (>= vector-index length)
            (decf vector-index count))
          (setq table-key (%svref vector vector-index))
          (when (= vector-index initial-index)
            (return (or first-deleted-index
                        (error "Bug: no room in table"))))
          (if (eq table-key key)
            (return vector-index)
            (if (eq table-key free-hash-marker)
              (return (or first-deleted-index vector-index))
              (if (and reuse
                       (null first-deleted-index)
                       (eq table-key deleted-hash-key-marker))
                (setq first-deleted-index vector-index)))))))))

(defun eql-hash-find (hash key)
  (declare (optimize (speed 3) (safety 0)))
  (if (need-use-eql key)
    (let* ((vector (nhash.vector hash))
           (hash-code (%%eqlhash-internal key))
           (entries (nhash.vector-size vector))
           (vector-index (index->vector-index (hash-mod hash-code entries vector)))
           (table-key (%svref vector vector-index)))
      (declare (fixnum hash-code entries vector-index))
      (if (eql key table-key)
        vector-index
        (if (eq table-key free-hash-marker)
          -1
          (let* ((secondary-hash (%svref secondary-keys-*-2
                                         (logand 7 hash-code)))
                 (initial-index vector-index)
                 (count (+ entries entries))
                 (length (+ count $nhash.vector_overhead)))
            (declare (fixnum secondary-hash initial-index count length))
            (loop
              (incf vector-index secondary-hash)
              (when (>= vector-index length)
                (decf vector-index count))
              (setq table-key (%svref vector vector-index))
              (when (= vector-index initial-index)
                (return -1))
              (if (eql table-key key)
                (return vector-index)
                (when (eq table-key free-hash-marker)
                  (return -1))))))))
    (eq-hash-find hash key)))

(defun eql-hash-find-for-put (hash key)
  (declare (optimize (speed 3) (safety 0)))
  (if (need-use-eql key)
    (let* ((vector (nhash.vector hash))
           (hash-code (%%eqlhash-internal key))
           (entries (nhash.vector-size vector))
           (vector-index (index->vector-index (hash-mod hash-code entries vector)))
           (table-key (%svref vector vector-index))
           (reuse (not (hash-lock-free-p hash))))
      (declare (fixnum hash-code entries vector-index))
      (if (or (eql key table-key)
              (eq table-key free-hash-marker))
        vector-index
        (let* ((secondary-hash (%svref secondary-keys-*-2
                                       (logand 7 hash-code)))
               (initial-index vector-index)
               (first-deleted-index (and reuse
                                         (eq table-key deleted-hash-key-marker)
                                         vector-index))
               (count (+ entries entries))
               (length (+ count $nhash.vector_overhead)))
          (declare (fixnum secondary-hash initial-index count length))
          (loop
            (incf vector-index secondary-hash)
            (when (>= vector-index length)
              (decf vector-index count))
            (setq table-key (%svref vector vector-index))
            (when (= vector-index initial-index)
              (return (or first-deleted-index
                          (error "Bug: no room in table"))))
            (if (eql table-key key)
              (return vector-index)
              (if (eq table-key free-hash-marker)
                (return (or first-deleted-index vector-index))
                (if (and reuse
                         (null first-deleted-index)
                         (eq table-key deleted-hash-key-marker))
                  (setq first-deleted-index vector-index))))))))
    (eq-hash-find-for-put hash key)))

(defun %make-rehash-bits (hash &optional (size (nhash.vector-size (nhash.vector hash))))
  (declare (fixnum size))
  (let ((rehash-bits (nhash.rehash-bits hash)))
    (unless (and rehash-bits
                 (>= (uvsize rehash-bits) size))
      (return-from %make-rehash-bits
        (setf (nhash.rehash-bits hash) (make-array size :element-type 'bit :initial-element 0))))
    (fill (the simple-bit-vector rehash-bits) 0)))

;;; Rehash.  Caller should have exclusive access to the hash table
;;; and have disabled interrupts.
(defun %rehash (hash)
  (when (hash-lock-free-p hash)
    (error "How did we get here?"))
  (let* ((vector (nhash.vector hash))
         (flags (nhash.vector.flags vector))
         (vector-index (- $nhash.vector_overhead 2))
         (size (nhash.vector-size vector))
         (rehash-bits (%make-rehash-bits hash size))
         (index -1))
    (declare (fixnum size index vector-index))
    (setf (nhash.vector.flags vector)
          (logand flags $nhash-clear-key-bits-mask))
    (setf (nhash.vector.cache-key vector) free-hash-marker
          (nhash.vector.cache-value vector) nil)
    (%set-does-not-need-rehashing vector)
    (loop
      (when (>= (incf index) size) (return))
      (setq vector-index (+ vector-index 2))
      (unless (%already-rehashed-p index rehash-bits)
        (let* ((key (%svref vector vector-index))
               (deleted (eq key deleted-hash-key-marker)))
          (unless
            (when (or deleted (eq key free-hash-marker))
              (if deleted  ; one less deleted entry
                (progn
                  (atomic-decf  (nhash.vector.deleted-count vector))
                  (incf (nhash.grow-threshold hash))
                  ;; Change deleted to free
                  (setf (%svref vector vector-index) free-hash-marker)))
              t)
            (let* ((last-index index)
                   (value (%svref vector (the fixnum (1+ vector-index))))
                   (first t))
                (loop
                  (let ((vector (nhash.vector hash))
                        (found-index (%rehash-probe rehash-bits hash key)))
                    (%set-already-rehashed-p found-index rehash-bits)
                    (if (eq last-index found-index)
                      (return)
                      (let* ((found-vector-index (index->vector-index found-index))
                             (newkey (%svref vector found-vector-index))
                             (newvalue (%svref vector (the fixnum (1+ found-vector-index)))))
			(declare (fixnum found-vector-index))
                        (when first ; or (eq last-index index) ?
                          (setq first nil)
                          (setf (%svref vector vector-index) free-hash-marker)
                          (setf (%svref vector (the fixnum (1+ vector-index))) free-hash-marker))
                        (%set-hash-table-vector-key vector found-vector-index key)
                        (setf (%svref vector (the fixnum (1+ found-vector-index))) value)                       
                        (when (or (eq newkey free-hash-marker)
                                  (setq deleted (eq newkey deleted-hash-key-marker)))
                          (when deleted
                            (atomic-decf (nhash.vector.deleted-count vector))
                            (incf (nhash.grow-threshold hash)))
                          (return))
                        (when (eq key newkey)
                          (cerror "Delete one of the entries." "Duplicate key: ~s in ~s ~s ~s ~s ~s"
                                  key hash value newvalue index found-index)                       
                          (atomic-decf (nhash.vector.count vector))
                          (incf (nhash.grow-threshold hash))
                          (return))
                        (setq key newkey
                              value newvalue
                              last-index found-index)))))))))))
    t )

;;; Hash to an index that is not set in rehash-bits
  
(defun %rehash-probe (rehash-bits hash key &optional (vector (nhash.vector hash)))
  (declare (optimize (speed 3)(safety 0)))  
  (multiple-value-bind (hash-code index entries)(compute-hash-code hash key t vector)
    (declare (fixnum hash-code index entries))
    (when (null hash-code)(cerror "nuts" "Nuts"))
    (let* ((vector-index (index->vector-index index)))
      (if (or (not (%already-rehashed-p index rehash-bits))
              (eq key (%svref vector vector-index)))
        (return-from %rehash-probe index)
        (let ((second (%svref secondary-keys (%ilogand 7 hash-code))))
          (declare (fixnum second))
          (loop
            (setq index (+ index second))
            (when (>= index entries)
              (setq index (- index entries)))
            (when (or (not (%already-rehashed-p index rehash-bits))
                      (eq key (%svref vector (index->vector-index index))))
              (return-from %rehash-probe index))))))))

;;; Returns one value: the index of the entry in the vector
;;; Since we're growing, we don't need to compare and can't find a key that's
;;; already there.
(defun %growhash-probe (vector hash key)
  (declare (optimize (speed 3)(safety 0)))
  (multiple-value-bind (hash-code index entries)(compute-hash-code hash key t vector)
    (declare (fixnum hash-code index entries))
    (let* ((vector-index (index->vector-index  index))
           (vector-key nil))
      (declare (fixnum vector-index))
      (if (or (eq free-hash-marker
                  (setq vector-key (%svref vector vector-index)))
              (eq deleted-hash-key-marker vector-key))
        (return-from %growhash-probe index)
        (let ((second (%svref secondary-keys (%ilogand 7 hash-code))))
          (declare (fixnum second))
          (loop
            (setq index (+ index second))
            (when (>= index entries)
              (setq index (- index entries)))
            (when (or (eq free-hash-marker
                          (setq vector-key (%svref vector (index->vector-index index))))
                      (eq deleted-hash-key-marker vector-key))
              (return-from %growhash-probe index))))))))

;;;;;;;;;;;;;
;;
;; Mapping functions are in "ccl:lib;hash"
;;



;;;;;;;;;;;;;
;;
;; Hashing functions
;; EQ & the EQ part of EQL are done in-line.
;;









;;; so whats so special about bit vectors as opposed to any other vectors of bytes
;;; For starters, it's guaranteed that they exist in the implementation; that may
;;; not be true of other immediate vector types.
(defun bit-vector-hash (bv)
  (declare (optimize (speed 3)(safety 0)))
  (let ((length (length bv)))
    (declare (fixnum length)) ;will this always be true? it's true of all vectors.
    (multiple-value-bind (data offset) (array-data-and-offset bv)
      (declare (type simple-bit-vector data) (fixnum offset))
      (let* ((hash 0)
             (limit (+ length offset))
             (nbytes (ash (the fixnum (+ length 7)) -3)))
        (declare (fixnum hash limit nbytes))
        (dotimes (i nbytes (mixup-hash-code hash))
          (let* ((w 0))
            (declare (fixnum w))
            (dotimes (j 8 (setq hash (+ (the fixnum (ash hash -3))  w)))
              (setq w (the fixnum
                        (logxor
                         (the fixnum
                           (ash (if (< offset limit) 
                                  (the fixnum (sbit data offset))
                                  0)
                                (the fixnum j)))
                         w)))
              (incf offset))))))))

#|
(defun bit-vector-hash (bv)
  (declare (optimize (speed 3)(safety 0)))
  (let ((length (length bv)))
    (declare (fixnum length))
    (let* ((all (+ length 15))
           (nwds (ash all -4))
           (rem (logand all 15))
           (hash 0)
           (mask (ash (the fixnum (1- (the fixnum (expt 2 rem))))(the fixnum(- 16 rem)))))
      (declare (fixnum all nwds rem hash mask))
      (multiple-value-bind (data offset)
                           (array-data-and-offset bv)
        (declare (fixnum offset))
        (locally (declare (type (simple-array (unsigned-byte 16) (*)) data))
          (dotimes (i nwds)
            (setq hash (%i+ hash (aref data (the fixnum (+ i offset))))))
          (when (neq 0 mask)            
            (setq hash (%i+ hash (%ilogand mask (aref data (the fixnum (+ offset nwds)))))))
          (mixup-hash-code hash))))))
|#


;;; Same as %%equalhash, but different:
;;;  1) Real numbers are hashed as if they were double-floats.  The real components of complex numbers
;;;     are hashed as double-floats and XORed together.
;;;  2) Characters and strings are hashed in a case-insensitive manner.
;;;  3) Hash tables are hashed based on their size and type.
;;;  4) Structures and CL array types are hashed based on their content.


;;; check fixnum befor immediate-p. call %%eqlhash

(defun %%equalphash (key)
  (cond ((or (fixnump key)(short-float-p key))
         (%dfloat-hash (float key 1.0d0))) 
        ((immediate-p-macro key)
         (mixup-hash-code (strip-tag-to-fixnum (if (characterp key)(char-upcase key) key))))
        ((bignump key)
         (if (<= most-negative-double-float key most-positive-double-float)
           (%dfloat-hash (float key 1.0d0))  ; with-stack-double-floats
           (%%eqlhash-internal key)))
        ((double-float-p key)
         (%dfloat-hash key))
        ((ratiop key)
         (%ilogxor (%%equalphash (numerator key)) (%%equalphash (denominator key))))
        ((complexp key)
         (%ilogxor (%%equalphash (realpart key)) (%%equalphash (imagpart key))))
        ((hash-table-p key)
         (equalphash-hash-table key))
        ((pathnamep key)
         (%hash-pathname key))
        ((or (istructp key)
             (structurep key))  ; was (gvectorp key)
         (%%equalphash-structure 11 key))
        ((arrayp key)
         (%%equalphash-array 11 key))
        ((consp key)
         (%%equalphash-aux 11 key))
        (t (%%eqlhash key))))


(defun equalphash-hash-table (hash-table)
  (let ((hash (%%equalhash "HASH-TABLE"))
        addressp)
    (declare (fixnum hash))
    (incf hash (the fixnum (%%eqhash (hash-table-count hash-table))))
    (multiple-value-bind (h ap) (%%eqhash (nhash.comparef hash-table))
      (declare (fixnum h))
      (incf hash h)
      (if ap (setq addressp t)))
    (multiple-value-bind (h ap) (%%eqhash (nhash.keytransF hash-table))
      (declare (fixnum h))
      (incf hash h)
      (if ap (setq addressp t)))
    (values hash addressp)))

(defun %%equalphash-structure (limit key)
  (let* ((size (uvsize key))
         (hash (mixup-hash-code size))
         addressp)
    (declare (fixnum limit size hash))
    (dotimes (i size)
      (multiple-value-bind (h ap) (%%equalphash-aux limit (%svref key i))
        (declare (fixnum h))
        (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash)) h)))
        (if ap (setq addressp t)))
      (when (<= (decf limit) 0)
        (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash))
                                  (the fixnum (mixup-hash-code 11)))))
        (return)))
    (values hash addressp)))

(defun %%equalphash-array (limit key)
  (multiple-value-bind (array offset) (array-data-and-offset key)
    (let* ((rank (array-rank key))
           (vectorp (eql rank 1))
           (size (if vectorp (length key) (array-total-size key)))
           (hash (mixup-hash-code rank))
           addressp)
      (declare (fixnum size hash limit rank))
      (if vectorp
        (setq hash
              (the fixnum
                   (+ (the fixnum (rotate-hash-code hash))
                      (the fixnum (mixup-hash-code size)))))
        (dotimes (i rank)
          (declare (fixnum i))
          (setq hash
                (the fixnum 
                     (+ (the fixnum (rotate-hash-code hash))
                        (the fixnum
                             (mixup-hash-code (array-dimension key i))))))))      
      (dotimes (i size)
        (declare (fixnum i))
        (multiple-value-bind (h ap) (%%equalphash-aux limit (uvref array offset))
          (declare (fixnum h))
          (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash)) h)))
          (if ap (setq addressp t)))
        (when (<= (decf limit) 0)
          (setq hash (the fixnum (+ (the fixnum (rotate-hash-code hash))
                                    (the fixnum (mixup-hash-code 11)))))
          (return))
        (incf offset))
      (values hash addressp))))

(defun %%equalphash-aux (limit key)
  (if (<= limit 0) 
    (mixup-hash-code 11)
    (if (null key) (mixup-hash-code 17)
        (cond ((consp key)
               (let ((hash 0)
                     address-p)
                 (do ((l limit (1- l)))
                     ((eq l 0)(values hash address-p))
                   (multiple-value-bind (ahash ap)
                                        (%%equalphash-aux l (if (consp key)(car key) key))
                     (setq hash (mixup-hash-code (logxor ahash hash)))
                     (if ap (setq address-p t)))
                   (when (not (consp key))
                     (return (values hash address-p)))
                   (setq key (cdr key)))))
              ((typep key 'hash-table)
               (equalphash-hash-table key))
              ; what are the dudes called that contain bits? they are uvectors but not gvectors?
              ; ivectors.
              ((or (istructp key)
                   (structurep key))    ;was (gvectorp key)
               (%%equalphash-structure limit key))
              ((or (arrayp key))  ; (uvectorp key))
               (%%equalphash-array limit key))
              (t (%%equalphash key))))))

(defun alist-hash-table (alist &rest hash-table-args)
  (declare (dynamic-extent hash-table-args))
  (if (typep alist 'hash-table)
    alist
    (let ((hash-table (apply #'make-hash-table hash-table-args)))
      (dolist (cons alist) (puthash (car cons) hash-table (cdr cons)))
      hash-table)))

(defun %hash-table-equalp (x y)
  ;; X and Y are both hash tables
  (and (eq (hash-table-test x)
           (hash-table-test y))
       (eql (hash-table-count x)
            (hash-table-count y))
       (block nil
         (let* ((default (cons nil nil))
                (foo #'(lambda (k v)
                         (let ((y-value (gethash k y default)))
                           (unless (and (neq default y-value)
                                        (equalp v y-value))
                             (return nil))))))
           (declare (dynamic-extent foo default))
           (maphash foo x))
         t)))

(defun sxhash (s-expr)
  "Computes a hash code for S-EXPR and returns it as an integer."
  (logand (sxhash-aux s-expr 7 17) target::target-most-positive-fixnum))

(defun sxhash-aux (expr counter key)
  (declare (fixnum counter))
  (if (> counter 0)
    (typecase expr
      ((or string bit-vector number character)  (+ key (%%equalhash expr)))
      (logical-pathname
       (dotimes (i (uvsize expr) key)
         (declare (fixnum i))
         (setq key (+ key (sxhash-aux (%svref expr i) (1- counter) key)))))
      (pathname
       ;; Don't consider %PHYSICAL-PATHNAME-VERSION to be significant
       (dotimes (i (uvsize expr) key)
         (declare (fixnum i))
         (unless (= i %physical-pathname-version)
           (setq key (+ key (sxhash-aux (%svref expr i) (1- counter) key))))))
      (symbol (+ key (%%equalhash (symbol-name expr))))
      (cons (sxhash-aux
             (cdr expr)
             (the fixnum (1- counter))             
             (+ key (sxhash-aux (car expr) (the fixnum (1- counter)) key))))
      (t (+  key (%%equalhash (symbol-name (%type-of expr))))))
    key))



#+(or ppc32-target x8632-target)
(defun immediate-p (thing)
  (let* ((tag (lisptag thing)))
    (declare (fixnum tag))
    (or (= tag target::tag-fixnum)
        (= tag target::tag-imm))))

#+ppc64-target
(defun immediate-p (thing)
  (let* ((tag (lisptag thing)))
    (declare (fixnum tag))
    (or (= tag ppc64::tag-fixnum)
        (= (logand tag ppc64::lowtagmask) ppc64::lowtag-imm))))

#+x8664-target
(defun immediate-p (thing)
  (let* ((tag (lisptag thing)))
    (declare (type (unsigned-byte 3) tag))
    (logbitp tag
             (logior (ash 1 x8664::tag-fixnum)
                     (ash 1 x8664::tag-imm-0)
                     (ash 1 x8664::tag-imm-1)))))



(defun %cons-nhash-vector (size &optional (flags 0))
  (declare (fixnum size))
  (let* ((vector (%alloc-misc (+ (+ size size) $nhash.vector_overhead) target::subtag-hash-vector free-hash-marker)))
    (%init-nhash-vector vector flags)
    vector))

(defun %init-nhash-vector (vector flags)
  (let ((size (vector-index->index (uvsize vector))))
    (declare (fixnum size))
    (setf (nhash.vector.link vector) 0
          (nhash.vector.flags vector) flags
          (nhash.vector.gc-count vector) (%get-gc-count)
          (nhash.vector.free-alist vector) nil
          (nhash.vector.finalization-alist vector) nil
          (nhash.vector.hash vector) nil
          (nhash.vector.deleted-count vector) 0
          (nhash.vector.count vector) 0
          (nhash.vector.cache-key vector) free-hash-marker
          (nhash.vector.cache-value vector) nil
          (nhash.vector.cache-idx vector) nil
          (nhash.vector.size vector) size
          (nhash.vector.size-reciprocal vector) (floor (ash 1 (- target::nbits-in-word target::fixnumshift)) size))))

(defun assert-hash-table-readonly (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (or (nhash.read-only hash)
      (when (nhash.owner hash)
        (error "Hash~table ~s is thread-private and can't be made read-only for that reason" hash))
      (if (hash-lock-free-p hash)
        (setf (nhash.read-only hash) t)
        (with-lock-context
          (without-interrupts
           (write-lock-hash-table hash)
           (let* ((flags (nhash.vector.flags (nhash.vector hash))))
             (declare (fixnum flags))
             (when (or (logbitp $nhash_track_keys_bit flags)
                       (logbitp $nhash_component_address_bit flags))
               (format t "~&Hash-table ~s uses address-based hashing and can't yet be made read-only for that reason." hash)
               (unlock-hash-table hash nil)
               (return-from assert-hash-table-readonly nil))
             (setf (nhash.read-only hash) t)
             (unlock-hash-table hash nil)
             t))))))

;; This is dangerous, if multiple threads are accessing a read-only
;; hash table. Use it responsibly.
(defun assert-hash-table-writeable (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (nhash.read-only hash)
    (setf (nhash.read-only hash) nil)
    t))

(defun readonly-hash-table-p (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (nhash.read-only hash))

(defun hash-table-owner (hash)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (nhash.owner hash))

(defun claim-hash-table (hash &optional steal)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (let* ((owner (nhash.owner hash)))
    (if owner
      (or (eq owner *current-process*)
          (when steal
            (setf (nhash.owner hash) *current-process*)))
      (progn
        (unless (hash-lock-free-p hash)
          (write-lock-hash-table hash)
          (setf (nhash.exclusion-lock hash) nil))
        (setf (nhash.owner hash) *current-process*)
        t))))

  
;; ** TODO: for lock-free hash tables, maybe we don't need to copy,
;; we could map over the actual hash table vector, because it's
;; always valid.
(defun lock-free-enumerate-hash-keys-and-values (hash keys values)
  (do* ((in (nhash.vector hash))
        (in-idx $nhash.vector_overhead (+ in-idx 2))
        (insize (uvsize in))
        (outsize (length (or keys values)))
        (out-idx 0))
       ((or (= in-idx insize)
            (= out-idx outsize))
        out-idx)
    (declare (fixnum in-idx insize out-idx outsize))
    (let* ((key (%svref in in-idx)))
      (unless (eq key free-hash-marker)
        (let ((val (%svref in (%i+ in-idx 1))))
          (when (eq val rehashing-value-marker)
            ;; This table is being rehashed.  Wait to finish and try again
            (lock-free-rehash hash)
            (return-from lock-free-enumerate-hash-keys-and-values
                         (lock-free-enumerate-hash-keys-and-values hash keys values)))
          (unless (or (eq key deleted-hash-key-marker)
                      (eq val deleted-hash-value-marker)
                      (eq val free-hash-marker))
            (when keys (setf (%svref keys out-idx) key))
            (when values (setf (%svref values out-idx) val))
            (incf out-idx)))))))

(defun enumerate-hash-keys-and-values (hash keys values)
  (unless (typep hash 'hash-table)
    (report-bad-arg hash 'hash-table))
  (when (hash-lock-free-p hash)
    (return-from enumerate-hash-keys-and-values
                 (lock-free-enumerate-hash-keys-and-values hash keys values)))
  (with-lock-context
    (without-interrupts
     (let* ((readonly (eq (read-lock-hash-table hash) :readonly)))
       (do* ((in (nhash.vector hash))
             (in-idx $nhash.vector_overhead (+ in-idx 2))
             (insize (uvsize in))
             (outsize (length (or keys values)))
             (out-idx 0))
           ((or (= in-idx insize)
                (= out-idx outsize))
              (unlock-hash-table hash readonly)
              out-idx)
         (declare (fixnum in-idx insize out-idx outsize))
         (let* ((key (%svref in in-idx)))
           (unless (or (eq key free-hash-marker)
                       (eq key deleted-hash-key-marker))
             (when keys
               (setf (%svref keys out-idx) key))
             (when values
               (setf (%svref values out-idx) (%svref in (%i+ in-idx 1))))
             (incf out-idx))))))))

(defun enumerate-hash-keys (hash out)
  (enumerate-hash-keys-and-values hash out nil))


(defun release-thread-private-hash-table (hash)
  (unless (and (typep hash 'hash-table)
               (not (nhash.read-only hash))
               (not (hash-lock-free-p hash)))
    (error "~&~s is not a thread-private hash table. " hash))
  (store-gvector-conditional nhash.owner hash *current-process* nil))

(defun acquire-thread-private-hash-table (hash)
  (unless (and (typep hash 'hash-table)
               (not (nhash.read-only hash))
               (not (hash-lock-free-p hash)))
    (error "~&~s is not a thread-private hash table. " hash))
  (store-gvector-conditional nhash.owner hash *current-process* nil))

(defun thread-private-hash-table-owner (hash)
  (unless (and (typep hash 'hash-table)
               (not (nhash.read-only hash))
               (not (hash-lock-free-p hash)))
    (error "~&~s is not a thread-private hash table. " hash))
  (nhash.owner hash))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-hash.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-float.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.
;;;
;;; level-0;l0-float.lisp

(in-package "CCL")

(eval-when (:compile-toplevel :execute)
  (require "NUMBER-MACROS")
  (require :number-case-macro) 
  (defconstant two^23 (ash 1 23))
  (defconstant single-float-pi (coerce pi 'single-float))
  (defconstant double-float-half-pi (/ pi 2))
  (defconstant single-float-half-pi (coerce (/ pi 2) 'single-float))
  (defconstant single-float-log2 0.6931472)                ; (log 2)
  (defconstant double-float-log2 0.6931471805599453d0)     ; (log 2.0d0)
  (defconstant double-float-log2^23 15.942385152878742d0)  ; (log (expt 2 23))
)

;;; used by float reader
(defun make-float-from-fixnums (hi lo exp sign &optional result)
  ;;(require-null-or-double-float-sym result)
  ;; maybe nuke all these require-types?
  ;;(setq hi (require-type hi 'fixnum))
  ;;(setq lo (require-type lo 'fixnum))
  ;;(setq exp (require-type exp 'fixnum))
  ;;(setq sign (require-type sign 'fixnum))
  (let ((the-float (or result (%make-dfloat))))
    (%make-float-from-fixnums the-float hi lo exp sign)
    the-float))


#+32-bit-target
(defun make-short-float-from-fixnums (significand biased-exp sign &optional result)
  (%make-short-float-from-fixnums (or result (%make-sfloat)) significand biased-exp sign))

#+64-bit-target
(defun make-short-float-from-fixnums (significand biased-exp sign)
  (declare (fixnum significand biased-exp sign))
  (host-single-float-from-unsigned-byte-32
   (logior
    (the fixnum (if (< sign 0) (ash 1 31) 0))
    (the fixnum (ash biased-exp IEEE-single-float-exponent-offset))
    (the fixnum (logand significand
                        (1- (ash 1 IEEE-single-float-hidden-bit)))))))


(defun float-sign (n1 &optional n2) ; second arg silly
  "Return a floating-point number that has the same sign as
   FLOAT1 and, if FLOAT2 is given, has the same absolute value
   as FLOAT2."
  (if (and n2 (not (typep n2 'float)))
    (setq n2 (require-type n2 'float)))
  (number-case n1
    (double-float                       
     (if (%double-float-sign n1) 
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) n2 (- n2))
         -1.0d0)
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) (- n2) n2)
         1.0d0)))
    (short-float
     (if (%short-float-sign n1)
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) n2 (- n2))
         -1.0s0)
       (if n2
         (if (if (typep n2 'double-float) (%double-float-minusp n2) (%short-float-minusp n2)) (- n2) n2)
         1.0s0)))))



(defun %double-float-minusp (n)
  (and (%double-float-sign n)(not (%double-float-zerop n))))

(defun %short-float-minusp (n)
  (and (%short-float-sign n) (not (%short-float-zerop n))))

(defun %double-float-abs (n)
  (if (not (%double-float-sign n))
    n 
    (%%double-float-abs! n (%make-dfloat))))

#+32-bit-target
(defun %short-float-abs (n)
  (if (not (%short-float-sign n))
    n 
    (%%short-float-abs! n (%make-sfloat))))

(defun fixnum-decode-float (n)
  (etypecase n
    (double-float (%integer-decode-double-float n))))

(defun nan-or-infinity-p (n)
  (etypecase n
    (double-float (eq 2047 (%double-float-exp n)))
    (short-float (eq 255 (%short-float-exp n)))))

; not sure this is right
(defun infinity-p (n)
  (etypecase n
    (double-float (multiple-value-bind (hi lo exp)(fixnum-decode-float n)
                    (and (eq 2047 exp)
                         (eq #x1000000 hi)
                         (eq 0 lo))))
    (short-float
     #+32-bit-target
     (multiple-value-bind (high low)(%sfloat-hwords n)
                  (let*  ((mantissa (%ilogior2 low (%ilsl 16 (%ilogand2 high #x007F))))
                          (exp (%ilsr 7 (%ilogand2 high #x7F80))))
                    (and (eq exp 255)
                         (eq 0 mantissa))))
     #+64-bit-target
     (let* ((bits (single-float-bits n))
            (exp (ldb (byte IEEE-single-float-exponent-width
                            IEEE-single-float-exponent-offset)
                      bits))
            (mantissa (ldb (byte IEEE-single-float-mantissa-width
                            IEEE-single-float-mantissa-offset)
                           bits)))
       (declare (fixnum bits exp mantissa))
       (and (= exp 255)
            (zerop mantissa))))))

#+32-bit-target
(defun fixnum-decode-short-float (float)
  (multiple-value-bind (high low)(%sfloat-hwords float)
    (let*  ((mantissa (%ilogior2 low (%ilsl 16 (%ilogand2 high #x007F))))
            (exp (%ilsr 7 (%ilogand2 high #x7F80))))
      (if (and (neq exp 0)(neq exp 255))(setq mantissa (%ilogior mantissa #x800000)))
      (values mantissa exp (%ilsr 15 high)))))

#+64-bit-target
(defun fixnum-decode-short-float (float)
  (let* ((bits (single-float-bits float)))
    (declare (fixnum bits))
    (let* ((mantissa (ldb (byte IEEE-single-float-mantissa-width
                                IEEE-single-float-mantissa-offset)
                          bits))
           (exp (ldb (byte IEEE-single-float-exponent-width
                           IEEE-single-float-exponent-offset)
                     bits))
           (sign (lsh bits -31)))
      (declare (fixnum mantissa exp sign))
      (unless (or (= exp 0) (= exp 255))
        (setq mantissa (logior mantissa (ash 1 IEEE-single-float-hidden-bit))))
      (values mantissa exp sign))))
                  
                   

#+32-bit-target
(defun integer-decode-double-float (n)
  (multiple-value-bind (hi lo exp sign)(%integer-decode-double-float n)
    ; is only 53 bits and positive so should be easy
    ;(values (logior (ash hi 28) lo) exp sign)))
    ; if denormalized, may fit in a fixnum
    (setq exp (- exp (if (< hi #x1000000) 
                       (+ IEEE-double-float-mantissa-width IEEE-double-float-bias)
                       (+ IEEE-double-float-mantissa-width (1+ IEEE-double-float-bias)))))
    (if (< hi (ash 1 (1- target::fixnumshift))) ; aka 2
      (values (logior (ash hi 28) lo) exp sign)
      ; might fit in 1 word?
      (let ((big (%alloc-misc 2 target::subtag-bignum)))
        (make-big-53 hi lo big)
        (if (< hi #x1000000) (%normalize-bignum big))
        (values big exp sign)))))

#+64-bit-target
(defun integer-decode-double-float (n)
  (multiple-value-bind (hi lo exp sign)(%integer-decode-double-float n)
    (setq exp (- exp (if (< hi #x1000000) 
                       (+ IEEE-double-float-mantissa-width IEEE-double-float-bias)
                       (+ IEEE-double-float-mantissa-width (1+ IEEE-double-float-bias)))))
    (values (logior (ash hi 28) lo) exp sign)))
    

;;; actually only called when magnitude bigger than a fixnum
#+32-bit-target
(defun %truncate-double-float (n)
  (multiple-value-bind (hi lo exp sign)(%integer-decode-double-float n)
    (if (< exp (1+ IEEE-double-float-bias)) ; this is false in practice
      0
      (progn
        (setq exp (- exp (+ IEEE-double-float-mantissa-width (1+ IEEE-double-float-bias))))
        (if (eq sign 1)  ; positive
          (logior (ash hi (+ 28 exp))(ash lo exp))
          (if (<= exp 0) ; exp positive - negate before shift - else after
            (let ((poo (logior (ash hi (+ 28 exp))(ash lo exp))))
              (- poo))
            (let ((poo (logior (ash hi 28) lo)))
              (ash (- poo) exp))))))))

#+64-bit-target
(defun %truncate-double-float (n)
  (multiple-value-bind (mantissa exp sign) (integer-decode-float n)
    (* sign (ash mantissa exp))))



; actually only called when bigger than a fixnum
(defun %truncate-short-float (n)
  (multiple-value-bind (mantissa exp sign)(fixnum-decode-short-float n)
    (if (< exp (1+ IEEE-single-float-bias)) ; is magnitude less than 1 - false in practice
      0
      (progn
        (setq exp (- exp (+ IEEE-single-float-mantissa-width (1+ IEEE-single-float-bias))))
        (ash (if (eq sign 0) mantissa (- mantissa)) exp)))))

(defun decode-float (n)
  "Return three values:
   1) a floating-point number representing the significand. This is always
      between 0.5 (inclusive) and 1.0 (exclusive).
   2) an integer representing the exponent.
   3) -1.0 or 1.0 (i.e. the sign of the argument.)"
  (number-case n
    (double-float
     (let* ((old-exp (%double-float-exp n))
            (sign (if (%double-float-sign n) -1.0d0 1.0d0)))    
       (if (eq 0 old-exp)
         (if (%double-float-zerop n)
           (values 0.0d0 0 sign)
           (let* ((val (%make-dfloat))
                  (zeros (dfloat-significand-zeros n)))
	     (%%double-float-abs! n val)
             (%%scale-dfloat! val (+ 2 IEEE-double-float-bias zeros) val) ; get it normalized
             (set-%double-float-exp val IEEE-double-float-bias)      ; then bash exponent
             (values val (- old-exp zeros IEEE-double-float-bias) sign)))
         (if (> old-exp IEEE-double-float-normal-exponent-max)
           (error "Can't decode NAN or infinity ~s" n)
           (let ((val (%make-dfloat)))
             (%%double-float-abs! n val)
             (set-%double-float-exp val IEEE-double-float-bias)
             (values val (- old-exp IEEE-double-float-bias) sign))))))
    (short-float
     (let* ((old-exp (%short-float-exp n))
            (sign (if (%short-float-sign n) -1.0s0 1.0s0)))
       (if (eq 0 old-exp)
         (if (%short-float-zerop n)
           (values 0.0s0 0 sign)
           #+32-bit-target
           (let* ((val (%make-sfloat))
                  (zeros (sfloat-significand-zeros n)))
	     (%%short-float-abs! n val)
             (%%scale-sfloat! val (+ 2 IEEE-single-float-bias zeros) val) ; get it normalized
             (set-%short-float-exp val IEEE-single-float-bias)      ; then bash exponent
             (values val (- old-exp zeros IEEE-single-float-bias) sign))
           #+64-bit-target
           (let* ((zeros (sfloat-significand-zeros n))
                  (val (%%scale-sfloat (%short-float-abs n)
				       (+ 2 IEEE-single-float-bias zeros))))
             (values (set-%short-float-exp val IEEE-single-float-bias)
                     (- old-exp zeros IEEE-single-float-bias) sign)))
         (if (> old-exp IEEE-single-float-normal-exponent-max)
           (error "Can't decode NAN or infinity ~s" n)
           #+32-bit-target
           (let ((val (%make-sfloat)))
             (%%short-float-abs! n val)
             (set-%short-float-exp val IEEE-single-float-bias)
             (values val (- old-exp IEEE-single-float-bias) sign))
           #+64-bit-target
	   (values (set-%short-float-exp (%short-float-abs n)
					 IEEE-single-float-bias)
		   (- old-exp IEEE-single-float-bias) sign)))))))

; (* float (expt 2 int))
(defun scale-float (float int)
  "Return the value (* f (expt (float 2 f) ex)), but with no unnecessary loss
  of precision or overflow."
  (unless (fixnump int)(setq int (require-type int 'fixnum)))
  (number-case float
    (double-float
     (let* ((float-exp (%double-float-exp float))
            (new-exp (+ float-exp int)))
       (if (eq 0 float-exp) ; already denormalized?
         (if (%double-float-zerop float)
           float 
           (let ((result (%make-dfloat)))
             (%%scale-dfloat! float (+ (1+ IEEE-double-float-bias) int) result)))
         (if (<= new-exp 0)  ; maybe going denormalized        
           (if (<= new-exp (- IEEE-double-float-digits))
             0.0d0 ; should this be underflow? - should just be normal and result is fn of current fpu-mode
             ;(error "Can't scale ~s by ~s." float int) ; should signal something                      
             (let ((result (%make-dfloat)))
               (%copy-double-float float result)
               (set-%double-float-exp result 1) ; scale by float-exp -1
               (%%scale-dfloat! result (+ IEEE-double-float-bias (+ float-exp int)) result)              
               result))
           (if (> new-exp IEEE-double-float-normal-exponent-max) 
             (error (make-condition 'floating-point-overflow
                                    :operation 'scale-float
                                    :operands (list float int)))
             (let ((new-float (%make-dfloat)))
               (%copy-double-float float new-float)
               (set-%double-float-exp new-float new-exp)
               new-float))))))
    (short-float
     (let* ((float-exp (%short-float-exp float))
            (new-exp (+ float-exp int)))
       (if (eq 0 float-exp) ; already denormalized?
         (if (%short-float-zerop float)
           float
           #+32-bit-target
           (let ((result (%make-sfloat)))
             (%%scale-sfloat! float (+ (1+ IEEE-single-float-bias) int) result))
           #+64-bit-target
           (%%scale-sfloat float (+ (1+ IEEE-single-float-bias) int)))
         (if (<= new-exp 0)  ; maybe going denormalized        
           (if (<= new-exp (- IEEE-single-float-digits))
             ;; should this be underflow? - should just be normal and
             ;; result is fn of current fpu-mode (error "Can't scale
             ;; ~s by ~s." float int) ; should signal something
             0.0s0
             #+32-bit-target
             (let ((result (%make-sfloat)))
               (%copy-short-float float result)
               (set-%short-float-exp result 1) ; scale by float-exp -1
               (%%scale-sfloat! result (+ IEEE-single-float-bias (+ float-exp int)) result)              
               result)
             #+64-bit-target
             (%%scale-sfloat (set-%short-float-exp float 1)
                             (+ IEEE-single-float-bias (+ float-exp int))))
           (if (> new-exp IEEE-single-float-normal-exponent-max) 
             (error (make-condition 'floating-point-overflow
                                    :operation 'scale-float
                                    :operands (list float int)))
             #+32-bit-target
             (let ((new-float (%make-sfloat)))
               (%copy-short-float float new-float)
               (set-%short-float-exp new-float new-exp)
               new-float)
             #+64-bit-target
             (set-%short-float-exp float new-exp))))))))

(defun %copy-float (f)
  ;Returns a freshly consed float.  float can also be a macptr.
  (cond ((double-float-p f) (%copy-double-float f (%make-dfloat)))
        ((macptrp f)
         (let ((float (%make-dfloat)))
           (%copy-ptr-to-ivector f 0 float (* 4 target::double-float.value-cell) 8)
           float))
        (t (error "Illegal arg ~s to %copy-float" f))))

(defun float-precision (float)     ; not used - not in cltl2 index ?
  "Return a non-negative number of significant digits in its float argument.
  Will be less than FLOAT-DIGITS if denormalized or zero."
  (number-case float
     (double-float
      (if (eq 0 (%double-float-exp float))
        (if (not (%double-float-zerop float))
        ; denormalized
          (- IEEE-double-float-mantissa-width (dfloat-significand-zeros float))
          0)
        IEEE-double-float-digits))
     (short-float 
      (if (eq 0 (%short-float-exp float))
        (if (not (%short-float-zerop float))
        ; denormalized
          (- IEEE-single-float-mantissa-width (sfloat-significand-zeros float))
          0)
        IEEE-single-float-digits))))


(defun %double-float (number &optional result)
  ;(require-null-or-double-float-sym result)
  ; use number-case when macro is common
  (number-case number
    (double-float
     (if result 
       (%copy-double-float number result)
         number))
    (short-float
     (%short-float->double-float number (or result (%make-dfloat))))
    (fixnum
     (%fixnum-dfloat number (or result (%make-dfloat))))
    (bignum (%bignum-dfloat number result))
    (ratio 
     (if (not result)(setq result (%make-dfloat)))
     (let* ((num (%numerator number))
            (den (%denominator number)))
       ; dont error if result is floatable when either top or bottom is not.
       ; maybe do usual first, catching error
       (if (not (or (bignump num)(bignump den)))
         (with-stack-double-floats ((fnum num)
                                        (fden den))       
             (%double-float/-2! fnum fden result))
         (let* ((numlen (integer-length num))
                (denlen (integer-length den))
                (exp (- numlen denlen))
                (minusp (minusp num)))
           (if (and (<= numlen IEEE-double-float-bias)
                    (<= denlen IEEE-double-float-bias)
                    #|(not (minusp exp))|# 
                    (<= (abs exp) IEEE-double-float-mantissa-width))
             (with-stack-double-floats ((fnum num)
                                            (fden den))
       
               (%double-float/-2! fnum fden result))
             (if (> exp IEEE-double-float-mantissa-width)
               (progn  (%double-float (round num den) result))               
               (if (>= exp 0)
                 ; exp between 0 and 53 and nums big
                 (let* ((shift (- IEEE-double-float-digits exp))
                        (num (if minusp (- num) num))
                        (int (round (ash num shift) den)) ; gaak
                        (intlen (integer-length int))
                        (new-exp (+ intlen (- IEEE-double-float-bias shift))))
                   
                   (when (> intlen IEEE-double-float-digits)
                     (setq shift (1- shift))
                     (setq int (round (ash num shift) den))
                     (setq intlen (integer-length int))
                     (setq new-exp (+ intlen (- IEEE-double-float-bias shift))))
                   (when (> new-exp 2046)
                     (error (make-condition 'floating-point-overflow
                                            :operation 'double-float
                                            :operands (list number))))
		   (make-float-from-fixnums (ldb (byte 25 (- intlen 25)) int)
					    (ldb (byte 28 (max (- intlen 53) 0)) int)
					    new-exp ;(+ intlen (- IEEE-double-float-bias 53))
					    (if minusp -1 1)
					    result))
                 ; den > num - exp negative
                 (progn  
                   (float-rat-neg-exp num den (if minusp -1 1) result)))))))))))


#+32-bit-target
(defun %short-float-ratio (number &optional result)
  (if (not result)(setq result (%make-sfloat)))
  (let* ((num (%numerator number))
         (den (%denominator number)))
    ;; dont error if result is floatable when either top or bottom is
    ;; not.  maybe do usual first, catching error
    (if (not (or (bignump num)(bignump den)))
      (target::with-stack-short-floats ((fnum num)
				       (fden den))       
        (%short-float/-2! fnum fden result))
      (let* ((numlen (integer-length num))
             (denlen (integer-length den))
             (exp (- numlen denlen))
             (minusp (minusp num)))
        (if (and (<= numlen IEEE-single-float-bias)
                 (<= denlen IEEE-single-float-bias)
                 #|(not (minusp exp))|# 
                 (<= (abs exp) IEEE-single-float-mantissa-width))
          (target::with-stack-short-floats ((fnum num)
					   (fden den))
            (%short-float/-2! fnum fden result))
          (if (> exp IEEE-single-float-mantissa-width)
            (progn  (%short-float (round num den) result))               
            (if (>= exp 0)
              ; exp between 0 and 23 and nums big
              (let* ((shift (- IEEE-single-float-digits exp))
                     (num (if minusp (- num) num))
                     (int (round (ash num shift) den)) ; gaak
                     (intlen (integer-length int))
                     (new-exp (+ intlen (- IEEE-single-float-bias shift))))
		(when (> intlen IEEE-single-float-digits)
                  (setq shift (1- shift))
                  (setq int (round (ash num shift) den))
                  (setq intlen (integer-length int))
                  (setq new-exp (+ intlen (- IEEE-single-float-bias shift))))
                (when (> new-exp IEEE-single-float-normal-exponent-max)
                  (error (make-condition 'floating-point-overflow
                                         :operation 'short-float
                                         :operands (list number))))
                (make-short-float-from-fixnums 
                   (ldb (byte IEEE-single-float-digits  (- intlen  IEEE-single-float-digits)) int)
                   new-exp
                   (if minusp -1 1)
                   result))
              ; den > num - exp negative
              (progn  
                (float-rat-neg-exp num den (if minusp -1 1) result t)))))))))

#+64-bit-target
(defun %short-float-ratio (number)
  (let* ((num (%numerator number))
         (den (%denominator number)))
    ;; dont error if result is floatable when either top or bottom is
    ;; not.  maybe do usual first, catching error
    (if (not (or (bignump num)(bignump den)))
      (/ (the short-float (%short-float num))
         (the short-float (%short-float den)))
      (let* ((numlen (integer-length num))
             (denlen (integer-length den))
             (exp (- numlen denlen))
             (minusp (minusp num)))
        (if (and (<= numlen IEEE-single-float-bias)
                 (<= denlen IEEE-single-float-bias)
                 #|(not (minusp exp))|# 
                 (<= (abs exp) IEEE-single-float-mantissa-width))
          (/ (the short-float (%short-float num))
             (the short-float (%short-float den)))
          (if (> exp IEEE-single-float-mantissa-width)
            (progn  (%short-float (round num den)))
            (if (>= exp 0)
              ; exp between 0 and 23 and nums big
              (let* ((shift (- IEEE-single-float-digits exp))
                     (num (if minusp (- num) num))
                     (int (round (ash num shift) den)) ; gaak
                     (intlen (integer-length int))
                     (new-exp (+ intlen (- IEEE-single-float-bias shift))))
		(when (> intlen IEEE-single-float-digits)
                  (setq shift (1- shift))
                  (setq int (round (ash num shift) den))
                  (setq intlen (integer-length int))
                  (setq new-exp (+ intlen (- IEEE-single-float-bias shift))))
                (when (> new-exp IEEE-single-float-normal-exponent-max)
                  (error (make-condition 'floating-point-overflow
                                         :operation 'short-float
                                         :operands (list number))))
                (make-short-float-from-fixnums 
                   (ldb (byte IEEE-single-float-digits  (- intlen  IEEE-single-float-digits)) int)
                   new-exp
                   (if minusp -1 1)))
              ; den > num - exp negative
              (progn  
                (float-rat-neg-exp num den (if minusp -1 1) nil t)))))))))


#+32-bit-target
(defun %short-float (number &optional result)
  (number-case number
    (short-float
     (if result (%copy-short-float number result) number))
    (double-float
     (%double-float->short-float number (or result (%make-sfloat))))
    (fixnum
     (%fixnum-sfloat number (or result (%make-sfloat))))
    (bignum
     (%bignum-sfloat number (or result (%make-sfloat))))
    (ratio
     (%short-float-ratio number result))))

#+64-bit-target
(defun %short-float (number)
  (number-case number
    (short-float number)
    (double-float (%double-float->short-float number))
    (fixnum (%fixnum-sfloat number))
    (bignum (%bignum-sfloat number))
    (ratio (%short-float-ratio number))))


(defun float-rat-neg-exp (integer divisor sign &optional result short)
  (if (minusp sign)(setq integer (- integer)))       
  (let* ((integer-length (integer-length integer))
         ;; make sure we will have enough bits in the quotient
         ;; (and a couple extra for rounding)
         (shift-factor (+ (- (integer-length divisor) integer-length) (if short 28 60))) ; fix
         (scaled-integer integer))
    (if (plusp shift-factor)
      (setq scaled-integer (ash integer shift-factor))
      (setq divisor (ash divisor (- shift-factor)))  ; assume div > num
      )
    ;(pprint (list shift-factor scaled-integer divisor))
    (multiple-value-bind (quotient remainder)(floor scaled-integer divisor)
      (unless (zerop remainder) ; whats this - tells us there's junk below
        (setq quotient (logior quotient 1)))
      ;; why do it return 2 values?
      (values (float-and-scale-and-round sign quotient (- shift-factor)  short result)))))



;;; when is (negate-bignum (bignum-ashift-right big)) ; can't negate
;;; in place cause may get bigger cheaper than (negate-bignum big) - 6
;;; 0r 8 digits ; 8 longs so win if digits > 7 or negate it on the
;;; stack

(defun %bignum-dfloat (big &optional result)  
  (let* ((minusp (bignum-minusp big)))
    (flet 
      ((doit (new-big)
         (let* ((int-len (bignum-integer-length new-big)))
           (when (>= int-len (- 2047 IEEE-double-float-bias)) ; args?
             (error (make-condition 'floating-point-overflow 
                                    :operation 'float :operands (list big))))
           (if (> int-len 53)
             (let* ((hi (ldb (byte 25  (- int-len  25)) new-big))
                    (lo (ldb (byte 28 (- int-len 53)) new-big)))
               ;(print (list new-big hi lo))
               (when (and (logbitp (- int-len 54) new-big)  ; round bit
                          (or (%ilogbitp 0 lo)    ; oddp
                              ;; or more bits below round
                              (%i< (one-bignum-factor-of-two new-big) (- int-len 54))))
                 (if (eq lo #xfffffff)
                   (setq hi (1+ hi) lo 0)
                   (setq lo (1+ lo)))
                 (when (%ilogbitp 25 hi) ; got bigger
                   (setq int-len (1+ int-len))
                   (let ((bit (%ilogbitp 0 hi)))
                     (setq hi (%ilsr 1 hi))
                     (setq lo (%ilsr 1 lo))
                     (if bit (setq lo (%ilogior #x8000000 lo))))))
               (make-float-from-fixnums hi lo (+ IEEE-double-float-bias int-len)(if minusp -1 1) result))
             (let* ((hi (ldb (byte 25  (- int-len  25)) new-big))
                    (lobits (min (- int-len 25) 28))
                    (lo (ldb (byte lobits (- int-len (+ lobits 25))) new-big)))
               (if (< lobits 28) (setq lo (ash lo (- 28 lobits))))
               (make-float-from-fixnums hi lo (+ IEEE-double-float-bias int-len) (if minusp -1 1) result))))))
      (declare (dynamic-extent #'doit))
      (with-one-negated-bignum-buffer big doit))))

#+32-bit-target
(defun %bignum-sfloat (big &optional result)  
  (let* ((minusp (bignum-minusp big)))
    (flet 
      ((doit (new-big)
         (let* ((int-len (bignum-integer-length new-big)))
           (when (>= int-len (- 255 IEEE-single-float-bias)) ; args?
             (error (make-condition 'floating-point-overflow 
                                    :operation 'float :operands (list big 1.0s0))))
           (if t ;(> int-len IEEE-single-float-digits) ; always true
             (let* ((lo (ldb (byte IEEE-single-float-digits  (- int-len  IEEE-single-float-digits)) new-big)))
               (when (and (logbitp (- int-len 25) new-big)  ; round bit
                          (or (%ilogbitp 0 lo)    ; oddp
                              ; or more bits below round
                              (%i< (one-bignum-factor-of-two new-big) (- int-len 25))))
                 (setq lo (1+ lo))
                 (when (%ilogbitp 24 lo) ; got bigger
                   (setq int-len (1+ int-len))
                   (setq lo (%ilsr 1 lo))))
               (make-short-float-from-fixnums  lo (+ IEEE-single-float-bias int-len)(if minusp -1 1) result))
             ))))
      (declare (dynamic-extent #'doit))
      (with-one-negated-bignum-buffer big doit))))


#+64-bit-target
(defun %bignum-sfloat (big)  
  (let* ((minusp (bignum-minusp big)))
    (flet 
      ((doit (new-big)
         (let* ((int-len (bignum-integer-length new-big)))
           (when (>= int-len (- 255 IEEE-single-float-bias)) ; args?
             (error (make-condition 'floating-point-overflow 
                                    :operation 'float :operands (list big 1.0s0))))
           (if t ;(> int-len IEEE-single-float-digits) ; always true
             (let* ((lo (ldb (byte IEEE-single-float-digits  (- int-len  IEEE-single-float-digits)) new-big)))
               (when (and (logbitp (- int-len 25) new-big)  ; round bit
                          (or (%ilogbitp 0 lo)    ; oddp
                              ; or more bits below round
                              (%i< (one-bignum-factor-of-two new-big) (- int-len 25))))
                 (setq lo (1+ lo))
                 (when (%ilogbitp 24 lo) ; got bigger
                   (setq int-len (1+ int-len))
                   (setq lo (%ilsr 1 lo))))
               (make-short-float-from-fixnums  lo (+ IEEE-single-float-bias int-len)(if minusp -1 1)))
             ))))
      (declare (dynamic-extent #'doit))
      (with-one-negated-bignum-buffer big doit))))




(defun %fixnum-dfloat (fix &optional result)  
  (if (eq 0 fix) 
    (if result (%copy-double-float 0.0d0 result) 0.0d0)
    (progn
      (when (not result)(setq result (%make-dfloat)))
      ; it better return result
      (%int-to-dfloat fix result))))


#+32-bit-target
(defun %fixnum-sfloat (fix &optional result)
  (if (eq 0 fix)
    (if result (%copy-short-float 0.0s0 result) 0.0s0)
    (%int-to-sfloat! fix (or result (%make-sfloat)))))

#+64-bit-target
(defun %fixnum-sfloat (fix)
  (if (eq 0 fix)
    0.0s0
    (%int-to-sfloat fix)))

;;; Transcendental functions.
(defun sin (x)
  "Return the sine of NUMBER."
  (cond ((complexp x)
         (let* ((r (realpart x))
                (i (imagpart x)))
           (complex (* (sin r) (cosh i))
                    (* (cos r) (sinh i)))))
        ((or (typep x 'ratio)
             (> (abs x) two^23))
         (if (typep x 'double-float)
           (imagpart (%extended-cis x))
           (%short-float (imagpart (%extended-cis x)))))
        ((typep x 'double-float)
         (%double-float-sin! x (%make-dfloat)))
        (t
         #+32-bit-target
         (target::with-stack-short-floats ((sx x))
           (%single-float-sin! sx (%make-sfloat)))
         #+64-bit-target
         (%single-float-sin (%short-float x)))))

(defun cos (x)
  "Return the cosine of NUMBER."
  (cond ((complexp x)
         (let* ((r (realpart x))
                (i (imagpart x)))
           (complex (* (cos r) (cosh i))
                    (- (* (sin r) (sinh i))))))
        ((or (typep x 'ratio)
             (> (abs x) two^23))
         (if (typep x 'double-float)
           (realpart (%extended-cis x))
           (%short-float (realpart (%extended-cis x)))))
        ((typep x 'double-float)
         (%double-float-cos! x (%make-dfloat)))
        (t
         #+32-bit-target
         (target::with-stack-short-floats ((sx x))
           (%single-float-cos! sx (%make-sfloat)))
         #+64-bit-target
         (%single-float-cos (%short-float x)))))

(defun tan (x)
  "Return the tangent of NUMBER."
  (cond ((complexp x)
         (let ((r (realpart x))
               (i (imagpart x)))
           (if (zerop i)
             (complex (tan r) i)
             (let* ((tx (tan r))
                    (ty (tanh i))
                    (tx2 (* tx tx))
                    (d (1+ (* tx2 (* ty ty))))
                    (c (if (> (abs i) 20)
                         (* 2 (exp (- (abs i))))
                         (/ (cosh i)))))
               (complex (/ (* (* c c) tx) d)
                        (/ (* ty (1+ tx2)) d))))))
        ((or (typep x 'ratio)
             (> (abs x) two^23))
         (let ((c (%extended-cis x)))
           (if (typep x 'double-float)
             (/ (imagpart c) (realpart c))
             (%short-float (/ (imagpart c) (realpart c))))))
        ((typep x 'double-float)
         (%double-float-tan! x (%make-dfloat)))
        (t
         #+32-bit-target
         (target::with-stack-short-floats ((sx x))
           (%single-float-tan! sx (%make-sfloat)))
         #+64-bit-target
         (%single-float-tan (%short-float x))
         )))


;;; Helper function for sin/cos/tan for ratio or large arguments
;;; (Will become inaccurate for ridiculously large arguments though)
;;; Does not assume that float library returns accurate values for large arguments
;;; (many don't)

;;; hexdecimal representations of pi at various precisions
(defconstant pi-vector
  #(#x3243F6A8885A308D313198A2E0
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D008
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B5470
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310B
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D9
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D95748F728EB658718BCD588215
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D95748F728EB658718BCD5882154AEE7B54A41DC25A59B59C30D
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D95748F728EB658718BCD5882154AEE7B54A41DC25A59B59C30D5392AF26013C5D1B023286085
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D95748F728EB658718BCD5882154AEE7B54A41DC25A59B59C30D5392AF26013C5D1B023286085F0CA417918B8DB38EF8E79DCB
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D95748F728EB658718BCD5882154AEE7B54A41DC25A59B59C30D5392AF26013C5D1B023286085F0CA417918B8DB38EF8E79DCB0603A180E6C9E0E8BB01E8A3E
    #x3243F6A8885A308D313198A2E03707344A4093822299F31D0082EFA98EC4E6C89452821E638D01377BE5466CF34E90C6CC0AC29B7C97C50DD3F84D5B5B54709179216D5D98979FB1BD1310BA698DFB5AC2FFD72DBD01ADFB7B8E1AFED6A267E96BA7C9045F12C7F9924A19947B3916CF70801F2E2858EFC16636920D871574E69A458FEA3F4933D7E0D95748F728EB658718BCD5882154AEE7B54A41DC25A59B59C30D5392AF26013C5D1B023286085F0CA417918B8DB38EF8E79DCB0603A180E6C9E0E8BB01E8A3ED71577C1BD314B2778AF2FDA5
    ))

(defun %extended-cis (x)
  (let (size pi-size)
    (typecase x
      (integer (setq size (1- (integer-length (abs x)))))
      (ratio (setq size (- (integer-length (abs (numerator x)))
                           (integer-length (denominator x)))))
      (float (multiple-value-bind (mantissa exponent sign)
                                  (integer-decode-float x)
               (setq size (+ (1- (integer-length mantissa)) exponent))
               (setq x (* sign mantissa (expt 2 exponent))))))
    (setq pi-size (ceiling (+ size 64) 100))
    (cond ((< pi-size 1) (setq pi-size 1))
          ((> pi-size 17) (setq pi-size 17)))
    (let* ((2pi-approx (/ (aref pi-vector (1- pi-size))
                          (ash 1 (1- (* 100 pi-size)))))
           (reduced-x (rem x 2pi-approx))
           (x0 (float reduced-x 1.0d0))
           (x1 (- reduced-x (rational x0))))
      (* (cis x0) (cis (float x1 1.0d0))))))


;;; Multiply by i (with additional optional scale factor)
;;; Does the "right thing" with minus zeroes (see CLTL2)
(defun i* (number &optional (scale 1))
  (complex (* (- scale) (imagpart number))
           (* scale (realpart number))))

;;; complex atanh
(defun %complex-atanh (z)
  (let* ((rx (realpart z))
         (ix (imagpart z))
         (sign (typecase rx
                 (double-float (%double-float-sign rx))
                 (short-float (%short-float-sign rx))
                 (t (minusp rx))))
         (x rx)
         (y ix)
         (y1 (abs y))
         ra ia)
    ;; following code requires non-negative x
    (when sign
      (setf x (- x))
      (setf y (- y)))
    (cond ((> (max x y1) 1.8014399e+16)
           ;; large value escape
           (setq ra (if (> x y1)
                      (let ((r (/ y x)))
                        (/ (/ x) (1+ (* r r))))
                      (let ((r (/ x y)))
                        (/ (/ r y) (1+ (* r r))))))
           (setq ia (typecase y
                      (double-float (float-sign y double-float-half-pi))
                      (single-float (float-sign y single-float-half-pi))
                      (t (if (minusp y) #.(- single-float-half-pi) single-float-half-pi)))))
          ((= x 1)
           (cond ((< y1 1e-9)
                  (setq ra (/ (- (if (typep y 'double-float) double-float-log2 single-float-log2)
                                 (log-e y1))
                              2))
                  (setq ia (/ (if (minusp y) (atan -2 y) (atan 2 (- y))) 2)))
                 (t
                  (setq ra (/ (log1+ (/ 4 (* y y))) 4))
                  (setq ia (/ (atan (/ 2 y) -1) 2)))))
          ((and (< y1 1)
                (< 0.5 x 2))
           (let ((x-1 (- x 1))
                 (x+1 (+ x 1))
                 (y2 (* y y)))
             (setq ra (/ (log-e (/ (+ (* x-1 x-1) y2) (+ (* x+1 x+1) y2))) -4))
             (setq ia (/ (atan (* 2 y) (- 1 (+ (* x x) y2))) 2))))
           (t
           (let ((r2 (+ (* x x) (* y y))))
             (setq ra (/ (log1+ (/ (* -4 x) (1+ (+ (* 2 x) r2)))) -4))
             (setq ia (/ (atan (* 2 y) (- 1 r2)) 2)))))
    ;; fixup signs, with special case for real arguments
    (cond (sign
           (setq ra (- ra))
           (when (typep z 'complex)
             (setq ia (- ia))))
          (t
           (unless (typep z 'complex)
             (setq ia (- ia)))))
    (complex ra ia)))

(defun atan (y &optional (x nil x-p))
  "Return the arc tangent of Y if X is omitted or Y/X if X is supplied."
  (cond (x-p
         (cond ((or (typep x 'double-float)
                    (typep y 'double-float))
                (with-stack-double-floats ((dy y)
                                           (dx x))
                  (%df-atan2 dy dx)))
               (t
                (when (and (rationalp x) (rationalp y))
                  ;; rescale arguments so that the maximum absolute value is 1
                  (let ((x1 (abs x)) (y1 (abs y)))
                    (cond ((> y1 x1)
                           (setf x (/ x y1))
                           (setf y (signum y)))
                          ((not (zerop x))
                           (setf y (/ y x1))
                           (setf x (signum x))))))
                #+32-bit-target
                (target::with-stack-short-floats ((sy y)
                                                  (sx x))
                  (%sf-atan2! sy sx))
                #+64-bit-target
                (%sf-atan2 (%short-float y) (%short-float x)))))
        ((typep y 'double-float)
         (%double-float-atan! y (%make-dfloat)))
        ((typep y 'single-float)
         #+32-bit-target
         (%single-float-atan! y (%make-sfloat))
         #+64-bit-target
         (%single-float-atan y))
        ((typep y 'rational)
         (cond ((<= (abs y) most-positive-short-float)
                #+32-bit-target
                (target::with-stack-short-floats ((sy y))
                  (%single-float-atan! sy (%make-sfloat)))
                #+64-bit-target
                (%single-float-atan (%short-float y)))
               ((minusp y)
                #.(- single-float-half-pi))
               (t
                single-float-half-pi)))
        (t
         (let ((r (realpart y))
               (i (imagpart y)))
           (if (zerop i)
             (complex (atan r) i)
             (i* (%complex-atanh (complex (- i) r)) -1))))))



(defun log (x &optional (b nil b-p))
  "Return the logarithm of NUMBER in the base BASE, which defaults to e."
  (if b-p
    (if (zerop b)
      (if (zerop x)
        (report-bad-arg x '(not (satisfies zerop) ))
        ;; ** CORRECT THE CONTAGION for complex args **
        (+ 0 (* x b)))
      ;; do the float/rational contagion before the division
      ;; but take care with negative zeroes
      (let ((x1 (realpart x))
            (b1 (realpart b)))
        (if (and (typep x1 'float)
                 (typep b1 'float))
          (/ (log-e (* x (float 1.0 b1)))
             (log-e (* b (float 1.0 x1))))
          (let ((r (/ (cond ((typep x 'rational)
                             (%rational-log x 1.0d0))
                            ((typep x1 'rational)
                             (%rational-complex-log x 1.0d0))
                            (t
                             (log-e (* x 1.0d0))))
                      (cond ((typep b 'rational)
                             (%rational-log b 1.0d0))
                            ((typep b1 'rational)
                             (%rational-complex-log b 1.0d0))
                            (t
                             (log-e (* b 1.0d0)))))))
            (cond ((or (typep x1 'double-float)
                       (typep b1 'double-float))
                   r)
                  ((complexp r)
                   (complex (%short-float (realpart r))
                            (%short-float (imagpart r))))
                  (t
                   (%short-float r)))))))
    (log-e x)))



(defun log-e (x)
   (cond
     ((typep x 'double-float)
      (if (%double-float-sign x)
        (with-stack-double-floats ((dx x))
          (complex (%double-float-log! (%%double-float-abs! dx dx) (%make-dfloat)) pi))
        (%double-float-log! x (%make-dfloat))))
    ((typep x 'short-float)
     #+32-bit-target
     (if (%short-float-sign x)
       (target::with-stack-short-floats ((sx x))
         (complex (%single-float-log! (%%short-float-abs! sx sx) (%make-sfloat))
                  single-float-pi))
       (%single-float-log! x (%make-sfloat)))
     #+64-bit-target
     (if (%short-float-sign x)
       (complex (%single-float-log (%short-float-abs (%short-float x)))
                single-float-pi)
       (%single-float-log (%short-float x))))
    ((typep x 'complex)
     (if (typep (realpart x) 'rational)
       (%rational-complex-log x 1.0s0)
       ;; take care that intermediate results do not overflow/underflow:
       ;; pre-scale argument by an appropriate power of two;
       ;; we only need to scale for very large and very small values -
       ;;  hence the various 'magic' numbers (values may not be too
       ;;  critical but do depend on the sqrt update to fix abs's operation)
       (let ((m (max (abs (realpart x))
                     (abs (imagpart x))))
             (log-s 0)
             (s 1))
         (if (typep m 'short-float)
           (let ((expon (- (%short-float-exp m) IEEE-single-float-bias)))
             (cond ((> expon 126)
                    (setq log-s double-float-log2^23)
                    (setq s #.(ash 1 23)))
                   ((< expon -124)
                    (setq log-s #.(- double-float-log2^23))
                    (setq s #.(/ 1.0s0 (ash 1 23))))))
           (let ((expon (- (%double-float-exp m) IEEE-double-float-bias)))
             (cond ((> expon 1022)
                    (setq log-s double-float-log2^23)
                    (setq s #.(ash 1 23)))
                   ((< expon -1020)
                    (setq log-s #.(- double-float-log2^23))
                    (setq s #.(/ 1.0d0 (ash 1 23)))))))
         (if (eql s 1)
           (complex (log-abs x) (phase x))
           (let ((temp (log-abs (/ x s))))
             (complex (float (+ log-s temp) temp) (phase x)))))))
    (t
     (%rational-log x 1.0s0))))

;;; helpers for rational log
(defun %rational-log (x prototype)
  (cond ((minusp x)
         (complex (%rational-log (- x) prototype)
                  (if (typep prototype 'short-float)
                    single-float-pi
                    pi)))
        ((bignump x)
         ;(let* ((base1 3)
         ;       (guess (floor (1- (integer-length x))
         ;                     (log base1 2)))
         ;       (guess1 (* guess (log-e base1))))
         ;  (+ guess1 (log-e (/ x (expt base1 guess)))))
         ; Using base1 = 2 is *much* faster. Was there a reason for 3?
         (let* ((guess (1- (integer-length x)))
                (guess1 (* guess double-float-log2)))
           (float (+ guess1 (log-e (float (/ x (ash 1 guess)) 1.0d0))) prototype)))
        ((and (ratiop x)
              ;; Rational arguments near +1 can be specified with great precision: don't lose it
              (cond ((< 0.5 x 3)
                     (log1+ (float (- x 1) prototype)))
                    (
                     ;; Escape out small values as well as large
                     (or (> x most-positive-short-float)
                         (< x least-positive-normalized-short-float))
                     ;; avoid loss of precision due to subtracting logs of numerator and denominator
                     (let* ((n (%numerator x))
                            (d (%denominator x))
                            (sn (1- (integer-length n)))
                            (sd (1- (integer-length d))))
                       (float (+ (* (- sn sd) double-float-log2)
                                 (- (log1+ (float (1- (/ n (ash 1 sn))) 1.0d0))
                                    (log1+ (float (1- (/ d (ash 1 sd))) 1.0d0))))
                              prototype))))))
        ((typep prototype 'short-float)
         #+32-bit-target
         (target::with-stack-short-floats ((sx x))
           (%single-float-log! sx (%make-sfloat)))
         #+64-bit-target
         (%single-float-log (%short-float x)))
        (t
         (with-stack-double-floats ((dx x))
           (%double-float-log! dx (%make-dfloat))))))

;;; (log1+ x) = (log (1+ x))
;;; but is much more precise near x = 0
(defun log1+ (x)
  ;;(cond ((typep x 'complex)
  ;;      (let ((r (realpart x))
  ;;            (i (imagpart x)))
  ;;        (if (and (< (abs r) 0.5)
  ;;                 (< (abs i) 3))
  ;;          (let* ((n (+ (* r (+ 2 r)) (* i i)))
  ;;                 (d (1+ (sqrt (1+ n)))))
  ;;            (complex (log1+ (/ n d)) (atan i (1+ r))))
  ;;         (log (1+ x)))))
  ;;     (t
  (if (and (typep x 'ratio)
           (< -0.5 x 2))
    (setq x (%short-float x)))
  (let ((y (1+ x)))
    (if (eql y x)
      (log-e y)
      (let ((e (1- y)))
        (if (zerop e)
          (* x 1.0)
          (- (log-e y) (/ (- e x) y)))))))

;;; helper for complex log
;;; uses more careful approach when (abs x) is near 1
(defun log-abs (x)
  (let ((a (abs x)))
    (if (< 0.5 a 3)
      (let* ((r (realpart x))
             (i (imagpart x))
             (n (if (> (abs r) (abs i))
                  (+ (* (1+ r) (1- r)) (* i i))
                  (+ (* r r) (* (1+ i) (1- i))))))
        (log1+ (/ n (1+ a))))
      (log-e a))))

(defun %rational-complex-log (x prototype &aux ra ia)
  (let* ((rx (realpart x))
         (ix (imagpart x))
         (x (abs rx))
         (y (abs ix)))
    (if (> y x)
      (let ((r (float (/ rx y) 1.0d0)))
        (setq ra (+ (%rational-log y 1.0d0)
                    (/ (log1+ (* r r)) 2)))
        (setq ia (atan (if (minusp ix) -1.0d0 1.0d0) r)))
      (let ((r (float (/ ix x) 1.0d0)))
        (setq ra (+ (%rational-log x 1.0d0)
                    (/ (log1+ (* r r)) 2)))
        (setq ia (atan r (if (minusp rx) -1.0d0 1.0d0)))))
    (if (typep prototype 'short-float)
      (complex (%short-float ra) (%short-float ia))
      (complex ra ia))))

(defun exp (x)
  "Return e raised to the power NUMBER."
  (typecase x
    (complex (* (exp (realpart x)) (cis (imagpart x))))
    (double-float (%double-float-exp! x (%make-dfloat)))
    (t
     (if (and (typep x 'rational)
              (< x -104))
       0.0s0
       #+32-bit-target
       (target::with-stack-short-floats ((sx x))
         (%single-float-exp! sx (%make-sfloat)))
       #+64-bit-target
       (%single-float-exp (%short-float x))))))


(defun positive-realpart-p (n)
  (> (realpart n) 0))

;;; (It may be possible to do something with rational exponents, e.g. so that
;;;       (expt x 1/2) == (sqrt x)
;;;       (expt x 3/2) == (expt (sqrt x) 3)      ** NOT (sqrt (expt x 3)) !! **
;;;                      or (* x (sqrt x))
;;;       (expt x 1/8) == (sqrt (sqrt (sqrt x)))
;;;    even, in the case of rational x, returning a rational result if possible.)
;;;
(defun expt (b e)
  "Return BASE raised to the POWER."
  (cond ((zerop e) (1+ (* b e)))
	((integerp e)
         (if (minusp e) (/ 1 (%integer-power b (- e))) (%integer-power b e)))
        ((zerop b)
         (if (plusp (realpart e)) (* b e) (report-bad-arg e '(satisfies plusp))))
        ((and (realp b) (plusp b) (realp e)
              ; escape out very small or very large rationals
              ; - avoid problems converting to float
              (typecase b
                (bignum (<= b most-positive-short-float))
                (ratio (cond ((< b 0.5)
                              (>= b least-positive-normalized-short-float))
                             ((> b 3)
                              (<= b most-positive-short-float))))
                (t t)))
         ;; assumes that the library routines are accurate
         ;; (if not, just excise this whole clause)
         (if (or (typep b 'double-float)
                 (typep e 'double-float))
           (with-stack-double-floats ((b1 b)
                                      (e1 e))
             (%double-float-expt! b1 e1 (%make-dfloat)))
           #+32-bit-target
           (target::with-stack-short-floats ((b1 b)
                                             (e1 e))
             (%single-float-expt! b1 e1 (%make-sfloat)))
           #+64-bit-target
           (%single-float-expt (%short-float b) (%short-float e))
           ))
        ((typep b 'rational)
         (let ((r (exp (* e (%rational-log b 1.0d0)))))
           (cond ((typep (realpart e) 'double-float)
                  r)
                 ((typep r 'complex)
                  (complex (%short-float (realpart r)) (%short-float (imagpart r))))
                 (t
                  (%short-float r)))))
        ((typep (realpart b) 'rational)
         (let ((r (exp (* e (%rational-complex-log b 1.0d0)))))
           (if (typep (realpart e) 'double-float)
             r
             (complex (%short-float (realpart r)) (%short-float (imagpart r))))))
        (t
         ;; type upgrade b without losing -0.0 ...
         (let ((r (exp (* e (log-e (* b 1.0d0))))))
           (cond ((or (typep (realpart b) 'double-float)
                      (typep (realpart e) 'double-float))
                  r)
                 ((typep r 'complex)
                  (complex (%short-float (realpart r)) (%short-float (imagpart r))))
                 (t
                  (%short-float r)))))))


        
(defun sqrt (x &aux a b)
  "Return the square root of NUMBER."
  (cond ((zerop x) x)
        ((complexp x)
         (let ((rx (realpart x))
               (ix (imagpart x)))
           (cond ((rationalp rx)
                  (if (zerop rx)
                    (let ((s (sqrt (/ (abs ix) 2))))
                      (complex s (if (minusp ix) (- s) s)))
                    (let* ((s (+ (* rx rx) (* ix ix)))
                           (d (if (ratiop s)
                                (/ (isqrt (%numerator s))
                                   (isqrt (%denominator s)))
                                (isqrt s))))
                      (unless (eql s (* d d))
                        (setf d (%double-float-hypot (%double-float rx)
                                                     (%double-float ix))))
                      (cond ((minusp rx)
                             (setq b (sqrt (/ (- d rx) 2)))
                             (when (minusp ix)
                               (setq b (- b)))
                             (setq a (/ ix (+ b b))))
                            (t
                             (setq a (sqrt (/ (+ rx d) 2)))
                             (setq b (/ ix (+ a a)))))
                      (if (rationalp a)
                        (complex a b)
                        (complex (%short-float a) (%short-float b))))))
                 ((minusp rx)
                  (if (zerop ix)
                    (complex 0 (float-sign ix (sqrt (- rx))))
                    (let ((shift (cond ((< rx -1) -3)
                                       ((and (> rx -5.9604645E-8) (< (abs ix) 5.9604645E-8)) 25)
                                       (t -1))))
                      (setq rx (scale-float rx shift))
                      (let ((s (fsqrt (- (abs (complex rx (scale-float ix shift))) rx))))
                        (setq b (scale-float s (ash (- -1 shift) -1)))
                        (when (minusp ix)
                          (setq b (- b)))
                        (setq a (/ ix (scale-float b 1)))
                        (complex a b)))))
                 (t
                  (if (zerop ix)
                    (complex (sqrt rx) ix)
                    (let ((shift (cond ((> rx 1) -3)
                                       ((and (< rx 5.9604645E-8) (< (abs ix) 5.9604645E-8)) 25)
                                       (t -1))))
                      (setq rx (scale-float rx shift))
                      (let ((s (fsqrt (+ rx (abs (complex rx (scale-float ix shift)))))))
                        (setq a (scale-float s (ash (- -1 shift) -1)))
                        (setq b (/ ix (scale-float a 1)))
                        (complex a b))))))))
        ((minusp x) (complex 0 (sqrt (- x))))
        ((floatp x)
         (fsqrt x))
        ((and (integerp x) (eql x (* (setq a (isqrt x)) a))) a)
        ((and (ratiop x)
              (let ((n (numerator x))
                    d)
                (and (eql n (* (setq a (isqrt n)) a))
                     (eql (setq d (denominator x))
                          (* (setq b (isqrt d)) b)))))
         (/ a b))          
        (t
         (float (fsqrt (float x 0.0d0)) 1.0s0))))



(defun asin (x)
  "Return the arc sine of NUMBER."
  (cond ((and (typep x 'double-float)
              (locally (declare (type double-float x))
                (and (<= -1.0d0 x)
                     (<= x 1.0d0))))
         (%double-float-asin! x (%make-dfloat)))
        ((and (typep x 'single-float)
              (locally (declare (type single-float x))
                (and (<= -1.0s0 x)
                     (<= x 1.0s0))))
         #+32-bit-target
         (%single-float-asin! x (%make-sfloat))
         #+64-bit-target
         (%single-float-asin x))
        ((and (typep x 'rational)
              (<= (abs x) 1))
         (if (integerp x)
           (case x
             (0 0.0s0)                          ; or simply 0 ??
             (1 single-float-half-pi)
             (-1 #.(- single-float-half-pi)))
           (atan x (sqrt (- 1 (* x x))))))
        (t
         (%complex-asin/acos x nil))
        ))


(defun acos (x)
  "Return the arc cosine of NUMBER."
  (cond ((and (typep x 'double-float)
              (locally (declare (type double-float x))
                (and (<= -1.0d0 x)
                     (<= x 1.0d0))))
         (%double-float-acos! x (%make-dfloat)))
        ((and (typep x 'short-float)
              (locally (declare (type short-float x))
                (and (<= -1.0s0 x)
                     (<= x 1.0s0))))
         #+32-bit-target
         (%single-float-acos! x (%make-sfloat))
         #+64-bit-target
         (%single-float-acos x))
        ((and (typep x 'rational)
              (<= (abs x) 1))
         (if (integerp x)
           (case x
             (0 single-float-half-pi)
             (1 0.0s0)                          ; or simply 0 ??
             (-1 single-float-pi))
           (atan (sqrt (- 1 (* x x))) x)))
        (t
         (%complex-asin/acos x t))
        ))

;;; combined complex asin/acos routine
;;; argument acos is true for acos(z); false for asin(z)
;;;
;;; based on Hull, Fairgrieve & Tang, ACM TMS 23, 3, 299-335 (Sept. 1997)
(defun %complex-asin/acos (z acos)
  (let* ((rx (realpart z))
         (ix (imagpart z))
         (x (abs rx))
         (y (abs ix))
         (m (max x y)))
    (if (> m 1.8014399E+16)
      ;; Large argument escape
      (let ((log-s 0))
        (if (typep m 'double-float)
          (if (> m #.(/ most-positive-double-float 2))
            (setq log-s double-float-log2)
            (setq z (* 2 z)))
          (if (> m #.(/ most-positive-short-float 2))
            (setq log-s single-float-log2)
            (setq z (* 2 z))))
        (if acos
          (i* (+ log-s (log-e z))
              (if (minusp ix) +1 -1))
          (if (minusp ix)
            (i* (+ log-s (log-e (i* z 1))) -1)
            (i* (+ log-s (log-e (i* z -1))) 1))))
      (let ((qrx rx)
            (qx x)
            x-1 y2 s)
        (cond ((rationalp rx)
               (setq x-1 (float (abs (- x 1))))
               (setq rx (float rx))
               (setq x (abs rx))
               (setq y (float y))
               (setq y2 (* y y))
               (setq s (cond ((zerop x-1)
                              y)
                             ((> y x-1)
                              (let ((c (/ x-1 y)))
                                (* y (sqrt (1+ (* c c))))))
                             (t
                              (let ((c (/ y x-1)))
                                (* x-1 (sqrt (1+ (* c c)))))))))
              (t
               (setq x-1 (abs (- x 1)))
               (setq y2 (* y y))
               (setq s (if (zerop x-1)
                         y
                         (sqrt (+ (* x-1 x-1) y2))))))
        (let* ((x+1 (+ x 1))
               (r (sqrt (+ (* x+1 x+1) y2)))
               (a (/ (+ r s) 2))
               (b (/ rx a))
               (ra (if (<= (abs b) 0.6417)
                     (if acos (acos b) (asin b))
                     (let* ((r+x+1 (+ r x+1))
                            (s+x-1 (+ s x-1))
                            (a+x (+ a x))
                            (ry (if (<= qx 1)
                                  (let ((aa (+ (/ y2 r+x+1) s+x-1)))
                                    (sqrt (/ (* a+x aa) 2)))
                                  (let ((aa (+ (/ a+x r+x+1) (/ a+x s+x-1))))
                                    (* y (sqrt (/ aa 2)))))))
                       (if acos (atan ry rx) (atan rx ry)))))
               (ia (if (<= a 1.5)
                     (let* ((r+x+1 (+ r x+1))
                            (s+x-1 (+ s x-1))
                            (ll (if (< qx 1)
                                  (let* ((aa (/ (+ (/ 1 r+x+1) (/ 1 s+x-1)) 2)))
                                    (+ (* aa y2) (* y (sqrt (* aa (1+ a))))))
                                  (let* ((a-1 (/ (+ (/ y2 r+x+1) s+x-1) 2)))
                                    (+ a-1 (sqrt (* a-1 (1+ a))))))))
                       (log1+ ll))
                     (log (+ a (sqrt (1- (* a a))))))))
          ;; final fixup of signs
          (if acos
            (if (complexp z)
              (if (typep ix 'float)
                (setq ia (float-sign (- ix) ia))
                (if (plusp ix)
                  (setq ia (- ia))))
              (if (< qrx -1)
                (setq ia (- ia))))
            (if (complexp z)
              (if (typep ix 'float)
                (setq ia (float-sign ix ia))
                (if (minusp ix)
                  (setq ia (- ia))))
              (if (> qrx 1)
                (setq ia (- ia)))))
          (complex ra ia))))))


(defun fsqrt (x)
  (etypecase x
    (double-float (%double-float-sqrt! x (%make-dfloat)))
    (single-float
     #+32-bit-target
     (%single-float-sqrt! x (%make-sfloat))
     #+64-bit-target
     (%single-float-sqrt x))))



(defun %df-atan2 (y x)
  (if (zerop x)
    (if (zerop y)
      (if (plusp (float-sign x))
        (if (eql y -0.0d0)
          -0.0d0
          0.0d0)
        (float-sign y pi))
      (float-sign y double-float-half-pi))
    (%double-float-atan2! y x (%make-dfloat))))

#+32-bit-target
(defun %sf-atan2! (y x)
  (if (zerop x)
    (if (zerop y)
      (if (plusp (float-sign x))
        ;; Don't return Y (which may be stack-consed) here.
        ;; We know that (ZEROP Y) is true, so:
        (if (eql y -0.0s0)
          -0.0s0
          0.0s0)
        (float-sign y single-float-pi))
      (float-sign y single-float-half-pi))
    (%single-float-atan2! y x (%make-sfloat))))

#+64-bit-target
(defun %sf-atan2 (y x)
  (if (zerop x)
    (if (zerop y)
      (if (plusp (float-sign x))
        y
        (float-sign y single-float-pi))
      (float-sign y single-float-half-pi))
    (%single-float-atan2 y x)))

#+64-bit-target
(defun %short-float-exp (n)
  (let* ((bits (single-float-bits n)))
    (declare (type (unsigned-byte 32) bits))
    (ldb (byte IEEE-single-float-exponent-width IEEE-single-float-exponent-offset) bits)))


#+64-bit-target
(defun set-%short-float-exp (float exp)
  (host-single-float-from-unsigned-byte-32
   (dpb exp
        (byte IEEE-single-float-exponent-width
              IEEE-single-float-exponent-offset)
        (the (unsigned-byte 32) (single-float-bits float)))))

#+64-bit-target
(defun %%scale-sfloat (float int)
  (* (the single-float float)
     (the single-float (host-single-float-from-unsigned-byte-32
                        (dpb int
                             (byte IEEE-single-float-exponent-width
                                   IEEE-single-float-exponent-offset)
                             0)))))

#+64-bit-target
(defun %double-float-exp (n)
  (let* ((highword (double-float-bits n)))
    (declare (fixnum highword))
    (logand (1- (ash 1 IEEE-double-float-exponent-width))
            (ash highword (- (- IEEE-double-float-exponent-offset 32))))))

#+64-bit-target
(defun set-%double-float-exp (float exp)
  (let* ((highword (double-float-bits float)))
    (declare (fixnum highword))
    (setf (uvref float target::double-float.val-high-cell)
          (dpb exp
               (byte IEEE-double-float-exponent-width
                     (- IEEE-double-float-exponent-offset 32))
               highword))
    exp))

#+64-bit-target
(defun %integer-decode-double-float (f)
  (multiple-value-bind (hiword loword) (double-float-bits f)
    (declare (type (unsigned-byte 32) hiword loword))
    (let* ((exp (ldb (byte IEEE-double-float-exponent-width
                           (- IEEE-double-float-exponent-offset 32))
                     hiword))
           (mantissa (logior
                      (the fixnum
                        (dpb (ldb (byte (- IEEE-double-float-mantissa-width 32)
                                        IEEE-double-float-mantissa-offset)
                                  hiword)
                             (byte (- IEEE-double-float-mantissa-width 32)
                                   32)
                             loword))
                      (if (zerop exp)
                        0
                        (ash 1 IEEE-double-float-hidden-bit))))
           (sign (if (logbitp 31 hiword) -1 1)))
      (declare (fixnum exp mantissa sign))
      (values (ldb (byte 25 28) mantissa)
              (ldb (byte 28 0) mantissa)
              exp
              sign))))

;;; end of l0-float.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-float.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-pred.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

;; Non-portable type-predicates & such.


;; bootstrapping defs - real ones in l1-typesys, l1-clos, sysutils

(defun find-builtin-cell (type &optional create)
  (declare (ignore create))
  (cons type nil))


(defun builtin-typep (form cell)
  (typep form (class-cell-name cell)))

(defun class-cell-typep (arg class-cell)
  (typep arg (class-cell-name class-cell)))

(defun class-cell-find-class (class-cell errorp)
  (declare (ignore errorp)) ; AARGH can't be right
  ;(dbg-paws #x100)
  (let ((class (and class-cell (class-cell-class class-cell))))
    (or class 
        (if  (fboundp 'find-class)
          (find-class (class-cell-name class-cell) nil)))))

(defun %require-type-builtin (form foo)
  (declare (ignore foo))
  form)

(defun %require-type-class-cell (form cell)
  (declare (ignore cell))
  form)
  
(defun non-nil-symbol-p (x)
  (if (symbolp x) x))

(defun pathnamep (thing)
  (or (istruct-typep thing 'pathname) (istruct-typep thing 'logical-pathname)))

(defun compiled-function-p (form)
  "Return true if OBJECT is a COMPILED-FUNCTION, and NIL otherwise."
  (and (functionp form)
       (not (logbitp $lfbits-trampoline-bit (the fixnum (lfun-bits form))))))

;;; all characters are base-chars.
(defun extended-char-p (c)
  (declare (ignore c)))


;;; Some of these things are probably open-coded.
;;; The functions have to exist SOMEWHERE ...
(defun fixnump (x)
  (= (the fixnum (lisptag x)) target::tag-fixnum))

(defun bignump (x)
  (= (the fixnum (typecode x)) target::subtag-bignum))

(defun integerp (x)
  "Return true if OBJECT is an INTEGER, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode target::tag-fixnum)
        (= typecode target::subtag-bignum))))

(defun ratiop (x)
  (= (the fixnum (typecode x)) target::subtag-ratio))


(defun rationalp (x)
  "Return true if OBJECT is a RATIONAL, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (and (< typecode (- target::nbits-in-word target::fixnumshift))
         (logbitp (the (integer 0 (#.(- target::nbits-in-word target::fixnumshift)))
                    typecode)
                  (logior (ash 1 target::tag-fixnum)
                          (ash 1 target::subtag-bignum)
                          (ash 1 target::subtag-ratio))))))

(defun short-float-p (x)
  (= (the fixnum (typecode x)) target::subtag-single-float))


(defun double-float-p (x)
  (= (the fixnum (typecode x)) target::subtag-double-float))

(defun floatp (x)
  "Return true if OBJECT is a FLOAT, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-single-float)
        (= typecode target::subtag-double-float))))

(defun realp (x)
  "Return true if OBJECT is a REAL, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (and (< typecode (- target::nbits-in-word target::fixnumshift))
         (logbitp (the (integer 0 (#.(- target::nbits-in-word target::fixnumshift)))
                    typecode)
                  (logior (ash 1 target::tag-fixnum)
                          (ash 1 target::subtag-single-float)
                          (ash 1 target::subtag-double-float)
                          (ash 1 target::subtag-bignum)
                          (ash 1 target::subtag-ratio))))))


(defun complexp (x)
  "Return true if OBJECT is a COMPLEX, and NIL otherwise."
  (let* ((code (typecode x)))
    (declare (type (unsigned-byte 8) code))
    (or 
     (= code target::subtag-complex)
     (= code target::subtag-complex-single-float)
     (= code target::subtag-complex-double-float))))

(defun complex-single-float-p (x)
  (eql (typecode x) target::subtag-complex-single-float))

(defun complex-double-float-p (x)
  (eql (typecode x) target::subtag-complex-double-float))

(defun numberp (x)
  "Return true if OBJECT is a NUMBER, and NIL otherwise."
  (or (realp x) (complexp x)))

(defun arrayp (x)
  "Return true if OBJECT is an ARRAY, and NIL otherwise."
  (arrayp x))

(defun vectorp (x)
  "Return true if OBJECT is a VECTOR, and NIL otherwise."
  (vectorp x))


(defun stringp (x)
  "Return true if OBJECT is a STRING, and NIL otherwise."
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-vectorH)
      (setq typecode (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref x target::arrayH.flags-cell)))))
    (= typecode target::subtag-simple-base-string)))


(defun simple-base-string-p (x)
  (= (the fixnum (typecode x)) target::subtag-simple-base-string))

(defun simple-string-p (x)
  "Return true if OBJECT is a SIMPLE-STRING, and NIL otherwise."
  (= (the fixnum (typecode x)) target::subtag-simple-base-string))

(defun complex-array-p (x)
  (let* ((typecode (typecode x)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
            (= typecode target::subtag-vectorH))
      (not (%array-header-simple-p x)))))

(defun simple-array-p (thing)
  "Returns T if the object is a simple array, else returns NIL.
   That's why it's called SIMPLE-ARRAY-P.  Get it ?
   A simple-array may have no fill-pointer, may not be displaced,
   and may not be adjustable."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
            (= typecode target::subtag-vectorH))
      (%array-header-simple-p thing)
      (or (= typecode target::subtag-simple-vector)
          (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
              target::min-cl-ivector-subtag)))))

(defun macptrp (x)
  (= (the fixnum (typecode x)) target::subtag-macptr))

(defun dead-macptr-p (x)
  (= (the fixnum (typecode x)) target::subtag-dead-macptr))


;;; Note that this is true of symbols and functions and many other
;;; things that it wasn't true of on the 68K.
(defun gvectorp (x)
  #+(or ppc32-target x8632-target arm-target)
  (= (the fixnum (logand (the fixnum (typecode x)) target::fulltagmask)) target::fulltag-nodeheader)
  #+ppc64-target
  (= (the fixnum (logand (the fixnum (typecode x)) ppc64::lowtagmask)) ppc64::lowtag-nodeheader)
  #+x8664-target
  (let* ((fulltag (fulltag x)))
    (declare (fixnum fulltag))
    (when (= fulltag x8664::fulltag-misc)
      (setq fulltag (logand (the (unsigned-byte 8) (typecode x)) x8664::fulltagmask))
      (or (= fulltag x8664::fulltag-nodeheader-0)
          (= fulltag x8664::fulltag-nodeheader-1))))
  )


(setf (type-predicate 'gvector) 'gvectorp)

(defun ivectorp (x)
  #+(or ppc32-target x8632-target arm-target)
  (= (the fixnum (logand (the fixnum (typecode x)) target::fulltagmask))
     target::fulltag-immheader)
  #+ppc64-target
  (= (the fixnum (logand (the fixnum (typecode x)) ppc64::lowtagmask)) ppc64::lowtag-immheader)
  #+x8664-target
  (let* ((fulltag (logand (the fixnum (typecode x)) x8664::fulltagmask)))
    (declare (fixnum fulltag))
    (or (= fulltag x8664::fulltag-immheader-0)
        (= fulltag x8664::fulltag-immheader-1)
        (= fulltag x8664::fulltag-immheader-2)))
  )

(setf (type-predicate 'ivector) 'ivectorp)

(defun miscobjp (x)
  #+(or ppc32-target x8632-target x8664-target arm-target)
  (= (the fixnum (lisptag x)) target::tag-misc)
  #+ppc64-target
  (= (the fixnum (fulltag x)) ppc64::fulltag-misc)
  )

(defun simple-vector-p (x)
  "Return true if OBJECT is a SIMPLE-VECTOR, and NIL otherwise."
  (= (the fixnum (typecode x)) target::subtag-simple-vector))

(defun base-string-p (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-simple-base-string)
        (and (= typecode target::subtag-vectorh)
             (= (the fixnum 
                  (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing target::arrayH.flags-cell))))
                target::subtag-simple-base-string)))))

(defun simple-bit-vector-p (form)
  "Return true if OBJECT is a SIMPLE-BIT-VECTOR, and NIL otherwise."
  (= (the fixnum (typecode form)) target::subtag-bit-vector))

(defun bit-vector-p (thing)
  "Return true if OBJECT is a BIT-VECTOR, and NIL otherwise."
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (or (= typecode target::subtag-bit-vector)
        (and (= typecode target::subtag-vectorh)
             (= (the fixnum 
                  (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref thing target::arrayH.flags-cell))))
                target::subtag-bit-vector)))))

(defun displaced-array-p (array)
  (if (%array-is-header array)
    (do* ((disp (%svref array target::arrayH.displacement-cell)
		(+ disp (the fixnum (%svref target target::arrayH.displacement-cell))))
	  (target (%svref array target::arrayH.data-vector-cell)
		  (%svref target target::arrayH.data-vector-cell)))
	 ((not (%array-is-header target))
	  (values target disp)))
    (values nil 0)))



(defun eq (x y)
  "Return T if OBJ1 and OBJ2 are the same object, otherwise NIL."
  (eq x y))


(defun cons-equal (x y)
  (declare (cons x y))
  (if (equal (car x) (car y))
    (equal (cdr x) (cdr y))))

(defun %pathname-equal (x y logical-p)
  (if (or (not logical-p)
          (and (equalp (%logical-pathname-host x) (%logical-pathname-host y))
               (eql (%logical-pathname-version x) (%logical-pathname-version y))))
    (cond (*case-sensitive-filesystem*
           (and (equal (%pathname-name x) (%pathname-name y))
                (equal (%pathname-type x) (%pathname-type y))
                (do* ((xdir (%pathname-directory x) (cdr xdir))
                      (ydir (%pathname-directory y) (cdr ydir)))
                     ((null xdir) (null ydir))
                  (unless (equal (car xdir) (car ydir))
                    (return)))))
          (t
           (and (equalp (%pathname-name x) (%pathname-name y))
                (equalp (%pathname-type x) (%pathname-type y))
                (do* ((xdir (%pathname-directory x) (cdr xdir))
                      (ydir (%pathname-directory y) (cdr ydir)))
                     ((null xdir) (null ydir))
                  (unless (equalp (car xdir) (car ydir))
                    (return))))))))
           
(defun hairy-equal (x y)
  (declare (optimize (speed 3)))
  ;; X and Y are not EQL, and are both of tag target::fulltag-misc.
  (let* ((x-type (typecode x))
	 (y-type (typecode y)))
    (declare (fixnum x-type y-type))
    (if (and (or (>= (the (unsigned-byte 8)
                       (gvector-typecode-p x-type))
                     target::subtag-vectorH)
                 (>= (the (unsigned-byte 8)
                       (ivector-typecode-p x-type))
                     target::min-cl-ivector-subtag))
	     (or (>= (the (unsigned-byte 8)
                       (gvector-typecode-p y-type))
                     target::subtag-vectorH)
                 (>= (the (unsigned-byte 8)
                       (ivector-typecode-p y-type))
                     target::min-cl-ivector-subtag)))
      (let* ((x-simple (if (= x-type target::subtag-vectorH)
                         (ldb target::arrayH.flags-cell-subtag-byte 
                              (the fixnum (%svref x target::arrayH.flags-cell)))
                         x-type))
             (y-simple (if (= y-type target::subtag-vectorH)
                         (ldb target::arrayH.flags-cell-subtag-byte 
                              (the fixnum (%svref y target::arrayH.flags-cell)))
                         y-type)))
        (declare (fixnum x-simple y-simple))
        (if (= x-simple target::subtag-simple-base-string)
          (if (= y-simple target::subtag-simple-base-string)
            (locally
                (declare (optimize (speed 3) (safety 0)))
              (let* ((x-len (if (= x-type target::subtag-vectorH) 
                              (%svref x target::vectorH.logsize-cell)
                              (uvsize x)))
                     (x-pos 0)
                     (y-len (if (= y-type target::subtag-vectorH) 
                              (%svref y target::vectorH.logsize-cell)
                              (uvsize y)))
                     (y-pos 0))
                (declare (fixnum x-len x-pos y-len y-pos))
                (when (= x-type target::subtag-vectorH)
                  (multiple-value-setq (x x-pos) (array-data-and-offset x)))
                (when (= y-type target::subtag-vectorH)
                  (multiple-value-setq (y y-pos) (array-data-and-offset y)))
                (%simple-string= x y x-pos y-pos (the fixnum (+ x-pos x-len)) (the fixnum (+ y-pos y-len))))))
          ;;Bit-vector case or fail.
          (and (= x-simple target::subtag-bit-vector)
               (= y-simple target::subtag-bit-vector)
               (locally
                   (declare (optimize (speed 3) (safety 0)))
                 (let* ((x-len (if (= x-type target::subtag-vectorH) 
                                 (%svref x target::vectorH.logsize-cell)
                                 (uvsize x)))
                        (x-pos 0)
                        (y-len (if (= y-type target::subtag-vectorH) 
                                 (%svref y target::vectorH.logsize-cell)
                                 (uvsize y)))
                        (y-pos 0))
                   (declare (fixnum x-len x-pos y-len y-pos))
                   (when (= x-len y-len)
                     (when (= x-type target::subtag-vectorH)
                       (multiple-value-setq (x x-pos) (array-data-and-offset x)))
                     (when (= y-type target::subtag-vectorH)
                       (multiple-value-setq (y y-pos) (array-data-and-offset y)))
                     (do* ((i 0 (1+ i)))
                          ((= i x-len) t)
                       (declare (fixnum i))
                       (unless (= (the bit (sbit x x-pos)) (the bit (sbit y y-pos)))
                         (return))
                       (incf x-pos)
                       (incf y-pos))))))))
      (if (= x-type y-type)
        (if (= x-type target::subtag-istruct)
          (let* ((structname (istruct-cell-name (%svref x 0))))
            (if (eq structname (istruct-cell-name (%svref y 0)))
              (if (eq structname 'pathname)
                (%pathname-equal x y nil)
                (if (eq structname 'logical-pathname)
                  (%pathname-equal x y t))))))))))

#+(or ppc32-target arm-target)
(progn
(defparameter *nodeheader-types*
  #(#+arm-target pseudofunction #+ppc32-target bogus ; 0
    ratio                               ; 1
    bogus                               ; 2
    complex                             ; 3
    catch-frame                         ; 4
    function                            ; 5
    basic-stream                         ; 6
    symbol                              ; 7
    lock                                ; 8
    hash-table-vector                   ; 9
    pool                                ; 10
    population                          ; 11
    package                             ; 12
    slot-vector				; 13
    standard-instance                   ; 14
    structure                           ; 15
    internal-structure                  ; 16
    value-cell                          ; 17
    xfunction                           ; 18
    bogus                               ; 19
    bogus                               ; 20
    bogus                               ; 21
    bogus                               ; 22
    bogus                               ; 23
    bogus                               ; 24
    bogus                               ; 25
    bogus                               ; 26
    bogus                               ; 27
    bogus                               ; 28
    array-header                        ; 29
    vector-header                       ; 30
    simple-vector                       ; 31
    ))


(defparameter *immheader-types*
  #(bignum                              ; 0
    short-float                         ; 1
    double-float                        ; 2
    macptr                              ; 3
    dead-macptr                         ; 4
    code-vector                         ; 5
    creole-object                       ; 6
    ;; some are unused
    xcode-vector                        ; 7
    (complex single-float)              ; 8
    (complex double-float)              ; 9
    bogus                               ; 10
    bogus                               ; 11
    bogus                               ; 12
    bogus                               ; 13
    bogus                               ; 14
    bogus                               ; 15
    bogus                               ; 16
    bogus                               ; 17
    bogus                               ; 18
    simple-short-float-vector           ; 19
    simple-unsigned-long-vector         ; 20
    simple-signed-long-vector           ; 21
    simple-fixnum-vector                ; 22
    simple-base-string                  ; 23
    simple-unsigned-byte-vector         ; 24
    simple-signed-byte-vector           ; 25
    simple-unsigned-word-vector         ; 26
    simple-signed-word-vector           ; 27
    simple-double-float-vector          ; 28
    simple-complex-single-float-vector  ; 29
    simple-complex-double-float-vector  ; 30
    simple-bit-vector                   ; 31
    ))

(defun %type-of (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (= typecode target::tag-fixnum)
      'fixnum
      (if (= typecode target::tag-list)
        (if thing 'cons 'null)
        (if (= typecode target::tag-imm)
          (if (base-char-p thing)
            'base-char
            'immediate)
	  (if (= typecode target::subtag-macptr)
	    (if (classp thing)
	      (class-name thing)
	      'macptr)
	    (let* ((tag-type (logand typecode target::full-tag-mask))
		   (tag-val (ash typecode (- target::ntagbits))))
	      (declare (fixnum tag-type tag-val))
	      (if (/= tag-type target::fulltag-nodeheader)
		(%svref *immheader-types* tag-val)
		(let ((type (%svref *nodeheader-types* tag-val)))
		  (if (eq type 'function)
		    (let ((bits (lfun-bits thing)))
		      (declare (fixnum bits))
		      (if (logbitp $lfbits-trampoline-bit bits)
			(let ((inner-fn (closure-function thing)))
                          (if (neq inner-fn thing)
                            (let ((inner-bits (lfun-bits inner-fn)))
                              (if (logbitp $lfbits-method-bit inner-bits)
                                'compiled-lexical-closure
                                (if (logbitp $lfbits-gfn-bit inner-bits)
                                  'standard-generic-function ; not precisely - see class-of
                                  (if (logbitp  $lfbits-cm-bit inner-bits)
                                    'combined-method
                                    'compiled-lexical-closure))))
                            'compiled-lexical-closure))
                        (if (logbitp  $lfbits-method-bit bits)
                          'method-function          
                          'compiled-function)))
		    (if (eq type 'lock)
		      (or (uvref thing target::lock.kind-cell)
			  type)
		      type)))))))))))

);#+(or ppc32-target arm-target)

#+ppc64-target
(progn
(defparameter *immheader-types*
  #(bogus
    bogus
    code-vector
    bogus
    bogus
    bogus
    xcode-vector
    macptr
    bogus
    bogus
    bignum
    dead-macptr
    bogus
    bogus
    double-float
    bogus
    bogus
    bogus
    complex-single-float
    bogus
    bogus
    bogus
    complex-double-float
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-signed-byte-vector
    simple-signed-word-vector
    simple-signed-long-vector
    simple-signed-doubleword-vector
    simple-unsigned-byte-vector
    simple-unsigned-word-vector
    simple-unsigned-long-vector
    simple-unsigned-doubleword-vector
    bogus
    simple-complex-double-float-vector
    simple-short-float-vector
    simple-fixnum-vector
    bogus
    bogus
    bogus
    simple-double-float-vector
    bogus
    bogus
    simple-base-string
    simple-complex-single-float-vector
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-bit-vector
    bogus
    bogus))

(defparameter *nodeheader-types*
    #(function
      catch-frame
      slot-vector
      ratio
      symbol
      basic-stream
      standard-instance
      complex
      bogus
      lock
      structure
      bogus
      bogus
      hash-vector
      internal-structure
      bogus
      bogus
      pool
      value-cell
      bogus
      bogus
      population
      xfunction
      bogus
      bogus
      package
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      array-header
      vector-header
      simple-vector
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      bogus
      )
  )


(defun %type-of (thing)
  (if (null thing)
    'null
    (let* ((typecode (typecode thing)))
      (declare (fixnum typecode))
      (cond ((= typecode ppc64::tag-fixnum) 'fixnum)
            ((= typecode ppc64::fulltag-cons) 'cons)
            ((= typecode ppc64::subtag-character) 'character)
            ((= typecode ppc64::subtag-single-float) 'short-float)
            (t (let* ((lowtag (logand typecode ppc64::lowtagmask)))
                 (declare (fixnum lowtag))
                 (cond ((= lowtag ppc64::lowtag-immheader)
                        (%svref *immheader-types* (ash typecode -2)))
                       ((= lowtag ppc64::lowtag-nodeheader)
                        (let* ((type (%svref *nodeheader-types*
                                             (ash typecode -2))))
                          (cond ((eq type 'function)
                                 (let ((bits (lfun-bits thing)))
                                   (declare (fixnum bits))
                                   (if (logbitp $lfbits-trampoline-bit bits)
                                     (let ((inner-fn (closure-function thing)))
                                         (if (neq inner-fn thing)
                                           (let ((inner-bits (lfun-bits inner-fn)))
                                             (if (logbitp $lfbits-method-bit inner-bits)
                                               'compiled-lexical-closure
                                               (if (logbitp $lfbits-gfn-bit inner-bits)
                                                 'standard-generic-function ; not precisely - see class-of
                                                 (if (logbitp  $lfbits-cm-bit inner-bits)
                                                   'combined-method
                                                   'compiled-lexical-closure))))
                                           'compiled-lexical-closure))
                                     (if (logbitp  $lfbits-method-bit bits)
                                       'method-function          
                                       'compiled-function))))
                                ((eq type 'lock)
                                 (or (uvref thing ppc64::lock.kind-cell)
                                     type))
                                (t type))))
                       (t 'immediate))))))))
);#+ppc64-target


#+x8632-target
(progn
(defparameter *nodeheader-types*
  #(bogus                               ; 0
    ratio                               ; 1
    bogus                               ; 2
    complex                             ; 3
    catch-frame                         ; 4
    function                            ; 5
    basic-stream			; 6
    symbol                              ; 7
    lock                                ; 8
    hash-table-vector                   ; 9
    pool                                ; 10
    population                          ; 11 (weak?)
    package                             ; 12
    slot-vector				; 13
    standard-instance                   ; 14
    structure                           ; 15
    internal-structure                  ; 16
    value-cell                          ; 17
    xfunction                           ; 18
    nil                                 ; 19
    nil                                 ; 20
    nil                                 ; 21
    bogus                               ; 22
    bogus                               ; 23
    bogus                               ; 24
    bogus                               ; 25
    bogus                               ; 26
    bogus                               ; 27
    bogus                               ; 28
    array-header                        ; 29
    vector-header                       ; 30
    simple-vector                       ; 31
    ))


(defparameter *immheader-types*
  #(bignum                              ; 0
    short-float                         ; 1
    double-float                        ; 2
    macptr                              ; 3
    dead-macptr                         ; 4
    code-vector                         ; 5
    creole-object                       ; 6
    xcode-vector                        ; 7
    complex-single-float                ; 8
    complex-double-float                ; 9
    bogus                               ; 10
    bogus                               ; 11
    bogus                               ; 12
    bogus                               ; 13
    bogus                               ; 14
    bogus                               ; 15
    bogus                               ; 16
    bogus                               ; 17
    bogus                               ; 18
    simple-short-float-vector           ; 19
    simple-unsigned-long-vector         ; 20
    simple-signed-long-vector           ; 21
    simple-fixnum-vector                ; 22
    simple-base-string                  ; 23
    simple-unsigned-byte-vector         ; 24
    simple-signed-byte-vector           ; 25
    simple-unsigned-word-vector         ; 26
    simple-signed-word-vector           ; 27
    simple-double-float-vector          ; 28
    simple-complex-single-float-vector  ; 29
    simple-complex-double-float-vector  ; 30
    simple-bit-vector                   ; 31
    ))

(defun %type-of (thing)
  (let* ((typecode (typecode thing)))
    (declare (fixnum typecode))
    (if (= typecode x8632::tag-fixnum)
      'fixnum
      (if (= typecode x8632::tag-list)	;a misnomer on x8632...
	(if (= (fulltag thing) x8632::fulltag-cons)
	  (if thing 'cons 'null)
	  'tagged-return-address)
        (if (= typecode x8632::tag-imm)
          (if (base-char-p thing)
            'base-char
            'immediate)
	  (if (= typecode x8632::subtag-macptr)
	    (if (classp thing)
	      (class-name thing)
	      'macptr)
	    (let* ((tag-type (logand typecode x8632::fulltagmask))
		   (tag-val (ash typecode (- x8632::ntagbits))))
	      (declare (fixnum tag-type tag-val))
	      (if (/= tag-type x8632::fulltag-nodeheader)
		(%svref *immheader-types* tag-val)
		(let ((type (%svref *nodeheader-types* tag-val)))
		  (if (eq type 'function)
		    (let ((bits (lfun-bits thing)))
		      (declare (fixnum bits))
		      (if (logbitp $lfbits-trampoline-bit bits)
			(let ((inner-fn (closure-function thing)))
                          (if (neq inner-fn thing)
                            (let ((inner-bits (lfun-bits inner-fn)))
                              (if (logbitp $lfbits-method-bit inner-bits)
                                'compiled-lexical-closure
                                (if (logbitp $lfbits-gfn-bit inner-bits)
                                  'standard-generic-function ; not precisely - see class-of
                                  (if (logbitp  $lfbits-cm-bit inner-bits)
                                    'combined-method
                                    'compiled-lexical-closure))))
                            'compiled-lexical-closure))
                        (if (logbitp  $lfbits-method-bit bits)
                          'method-function          
                          'compiled-function)))
		    (if (eq type 'lock)
		      (or (uvref thing x8632::lock.kind-cell)
			  type)
		      type)))))))))))

) ;x8632-target

#+x8664-target
(progn
(defparameter *nodeheader-0-types*
  #(bogus
    symbol-vector
    catch-frame
    hash-vector
    pool
    population
    package
    slot-vector
    basic-stream
    function-vector                                        ;8
    array-header
    bogus
    bogus
    bogus
    bogus
    bogus
    ))

(defparameter *nodeheader-1-types*
  #(bogus
    ratio
    complex
    structure
    internal-structure
    value-cell
    xfunction
    lock
    instance
    bogus
    vector-header
    simple-vector
    bogus
    bogus
    bogus
    bogus
    ))

(defparameter *immheader-0-types*
  #(bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-complex-double-float-vector
    simple-signed-word-vector
    simple-unsigned-word-vector
    bogus
    simple-signed-byte-vector
    simple-unsigned-byte-vector
    bit-vector))

(defparameter *immheader-1-types*
  #(bogus
    bignum
    double-float
    xcode-vector
    complex-single-float
    complex-double-float
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-base-string
    simple-signed-long-vector
    simple-unsigned-long-vector
    single-float-vector))

(defparameter *immheader-2-types*
  #(bogus
    macptr
    dead-macptr
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    bogus
    simple-complex-single-float-vector
    simple-fixnum-vector
    simple-signed-doubleword-vector
    simple-unsigned-doubleword-vector
    double-float-vector))


(defparameter *x8664-%type-of-functions* nil)

(let* ((fixnum (lambda (x) (declare (ignore x)) 'fixnum))
       (tra (lambda (x) (declare (ignore x)) 'tagged-return-address))
       (bogus (lambda (x) (declare (ignore x)) 'bogus)))
  (setq *x8664-%type-of-functions*
        (vector
         fixnum                         ;0
         (lambda (x) (declare (ignore x)) 'short-float) ;1
         (lambda (x) (if (characterp x) 'character 'immediate)) ;2
         (lambda (x) (declare (ignore x)) 'cons) ;3
         tra                            ;4
         bogus                          ;5
         bogus                          ;6
         bogus                          ;7
         fixnum                         ;8
         bogus                          ;9
         bogus                          ;10
         (lambda (x) (declare (ignore x)) 'null) ;11
         tra                            ;12
         (lambda (x) (let* ((typecode (typecode x)) 
                            (low4 (logand typecode x8664::fulltagmask))
                            (high4 (ash typecode (- x8664::ntagbits))))
                       (declare (type (unsigned-byte 8) typecode)
                                (type (unsigned-byte 4) low4 high4))
                       (let* ((name
                               (cond ((= low4 x8664::fulltag-immheader-0)
                                      (%svref *immheader-0-types* high4))
                                     ((= low4 x8664::fulltag-immheader-1)
                                      (%svref *immheader-1-types* high4))
                                     ((= low4 x8664::fulltag-immheader-2)
                                      (%svref *immheader-2-types* high4))
                                     ((= low4 x8664::fulltag-nodeheader-0)
                                      (%svref *nodeheader-0-types* high4))
                                     ((= low4 x8664::fulltag-nodeheader-1)
                                      (%svref *nodeheader-1-types* high4))
                                     (t 'bogus))))
                         (or (and (eq name 'lock)
                                  (uvref x x8664::lock.kind-cell))
                             name)))) ;13
         (lambda (x) (declare (ignore x)) 'symbol) ;14
         (lambda (thing)
           (let ((bits (lfun-bits thing)))
             (declare (fixnum bits))
             (if (logbitp $lfbits-trampoline-bit bits)
               (let ((inner-fn (closure-function thing)))
                 (if (neq inner-fn thing)
                   (let ((inner-bits (lfun-bits inner-fn)))
                     (if (logbitp $lfbits-method-bit inner-bits)
                       'compiled-lexical-closure
                       (if (logbitp $lfbits-gfn-bit inner-bits)
                         'standard-generic-function ; not precisely - see class-of
                         (if (logbitp  $lfbits-cm-bit inner-bits)
                           'combined-method
                           'compiled-lexical-closure))))
                   'compiled-lexical-closure))
               (if (logbitp  $lfbits-method-bit bits)
                 'method-function          
                 'compiled-function))))))) ;15
                                      
       


  
(defun %type-of (thing)
  (let* ((f (fulltag thing)))
    (funcall (%svref *x8664-%type-of-functions* f) thing)))

        

);#+x8664-target
      

;;; real machine specific huh
(defun consp (x)
  "Return true if OBJECT is a CONS, and NIL otherwise."
  (consp x))

(defun characterp (arg)
  "Return true if OBJECT is a CHARACTER, and NIL otherwise."
  (characterp arg))

(defun base-char-p (c)
  (base-char-p c))




(defun structurep (form)
  "True if the given object is a named structure, Nil otherwise."
  (= (the fixnum (typecode form)) target::subtag-struct))

(defun istructp (form)
  (= (the fixnum (typecode form)) target::subtag-istruct))


;;; Not to be conused with STRUCTURE-TYPE-P, defined in ccl:lib;pprint.lisp.
;;; (If you've ever been "conused", I'm sure you know just how painful
;;; that can be.)
(defun structure-typep (thing type)
  (if (= (the fixnum (typecode thing)) target::subtag-struct)
    (dolist (x (%svref thing 0))
      (when (eq x type)
        (return t)))))

(defun require-structure-type (arg token)
  (or(and (= (the fixnum (typecode arg)) target::subtag-struct)
           (dolist (x (%svref arg 0))
             (declare (optimize (speed 3) (safety 0)))
             (when (eq x token) (return arg))))
    (%kernel-restart $xwrongtype arg (if (typep token 'class-cell) (class-cell-name token) token))))

(defun istruct-typep (thing type)
  (if (= (the fixnum (typecode thing)) target::subtag-istruct)
    (eq (istruct-cell-name (%svref thing 0)) type)))

(defun istruct-type-name (thing)
  (if (= (the fixnum (typecode thing)) target::subtag-istruct)
    (istruct-cell-name (%svref thing 0))))


;;; This is actually set to an alist in the xloader.
(defparameter *istruct-cells* nil)

;;; This should only ever push anything on the list in the cold
;;; load (e.g., when running single-threaded.)
(defun register-istruct-cell (name)
  (or (assq name *istruct-cells*)
      (let* ((pair (cons name nil)))
        (push pair *istruct-cells*)
        pair)))

(defun set-istruct-cell-info (cell info)
  (etypecase cell
    (cons (%rplacd cell info)))
  info)


(defun symbolp (thing)
  "Return true if OBJECT is a SYMBOL, and NIL otherwise."
  #+(or ppc32-target x8632-target arm-target)
  (if thing
    (= (the fixnum (typecode thing)) target::subtag-symbol)
    t)
  #+ppc64-target
  (= (the fixnum (typecode thing)) ppc64::subtag-symbol)
  #+x8664-target
  (if thing
    (= (the fixnum (lisptag thing)) x8664::tag-symbol)
    t)
  )
      
(defun packagep (thing)
  (= (the fixnum (typecode thing)) target::subtag-package))

;;; 1 if by land, 2 if by sea.
(defun sequence-type (x)
  (let* ((typecode (typecode x)))
    (declare (type (unsigned-byte 8) typecode))
    (unless (or (= typecode target::subtag-vectorH)
                (= typecode target::subtag-simple-vector)
                (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                    target::min-cl-ivector-subtag))
      (or (listp x)
          (report-bad-arg x 'sequence)))))

(defun uvectorp (x)
  (= (the fixnum (fulltag x)) target::fulltag-misc))

(setf (type-predicate 'uvector) 'uvectorp)

(defun listp (x)
  (listp x))

(defparameter *type-cells* nil)



(defparameter *type-cells-lock* nil)


;;; The weird handling to the special variables here has to do with
;;; xload issues.
(defun register-type-cell (specifier)
  (with-lock-grabbed ((or *type-cells-lock*
                         (setq *type-cells-lock* (make-lock))))
    (unless *type-cells*
      (setq *type-cells* (make-hash-table :test 'equal)))
    (or (values (gethash specifier *type-cells*))
        (setf (gethash specifier *type-cells*)
              (make-type-cell specifier)))))


(defvar %find-classes% nil)

(setq %find-classes% (make-hash-table :test 'eq))


(defun find-class-cell (name create?)
  (unless %find-classes%
    (dbg name))
  (let ((cell (gethash name %find-classes%)))
    (or cell
        (and create?
             (setf (gethash name %find-classes%) (make-class-cell name))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-pred.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-array.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;; -*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")




; Return T if array or vector header, NIL if (simple-array * *), else
; error.

(defun %array-is-header (array)
  (if (typep array 'array)
    (let* ((typecode (typecode array)))
      (declare (fixnum typecode))
      (or (= typecode target::subtag-arrayH)
          (= typecode target::subtag-vectorH)))
    (report-bad-arg array 'array)))

(defun %set-fill-pointer (vectorh new)
  (setf (%svref vectorh target::vectorh.logsize-cell) new))

(defun %array-header-subtype (header)
  (the fixnum 
    (ldb target::arrayH.flags-cell-subtag-byte (the fixnum (%svref header target::arrayH.flags-cell)))))

(defun array-element-subtype (array)
  (if (%array-is-header array)
    (%array-header-subtype array)
    (typecode array)))
  
#+ppc32-target
(defconstant ppc32::*immheader-array-types*
  '#(short-float
     (unsigned-byte 32)
     (signed-byte 32)
     fixnum
     character
     (unsigned-byte 8)
     (signed-byte 8)
     (unsigned-byte 16)
     (signed-byte 16)
     double-float
     (complex single-float)
     (complex double-float)
     bit))

#+ppc64-target
(defconstant ppc64::*immheader-array-types*
  '#(unused
     unused
     unused
     unused
     (signed-byte 8)
     (signed-byte 16)
     (signed-byte 32)
     (signed-byte 64)
     (unsigned-byte 8)
     (unsigned-byte 16)
     (unsigned-byte 32)
     (unsigned-byte 64)
     unused
     (complex double-float)
     short-float
     fixnum
     unused
     unused
     unused
     double-float
     unused
     unused
     character
     (complex single-float)
     unused
     unused
     unused
     unused
     unused
     bit
     unused
     unused))

#+x8632-target
(defconstant x8632::*immheader-array-types*
  '#(short-float
     (unsigned-byte 32)
     (signed-byte 32)
     fixnum
     character
     (unsigned-byte 8)
     (signed-byte 8)
     (unsigned-byte 16)
     (signed-byte 16)
     double-float
     (complex single-float)
     (complex double-float)
     bit))

#+x8664-target
(progn
(defconstant x8664::*immheader-0-array-types*
  ;; ivector-class-other-bit
  #(unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    (complex double-float)
    (signed-byte 16)
    (unsigned-byte 16)
    character
    (signed-byte 8)
    (unsigned-byte 8)
    bit
    ))

(defconstant x8664::*immheader-1-array-types*
    ;; ivector-class-32-bit
  #(
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    character
    (signed-byte 32)
    (unsigned-byte 32)
    single-float))

(defconstant x8664::*immheader-2-array-types*
  ;; ivector-class-64-bit
  #(
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    unused
    (complex single-float)
    fixnum
    (signed-byte 64)
    (unsigned-byte 64)
    double-float))
    
)

#+arm-target
(defconstant arm::*immheader-array-types*
  '#(short-float
     (unsigned-byte 32)
     (signed-byte 32)
     fixnum
     character
     (unsigned-byte 8)
     (signed-byte 8)
     (unsigned-byte 16)
     (signed-byte 16)
     double-float
     (complex single-float)
     (complex double-float)
     bit))


(defun array-element-type (array)
  "Return the type of the elements of the array"
  (let* ((subtag (if (%array-is-header array)
                   (%array-header-subtype array)
                   (typecode array))))
    (declare (fixnum subtag))
    (if (= subtag target::subtag-simple-vector)
      t                                 ; only node CL array type
      #+ppc-target
      (svref target::*immheader-array-types*
             #+ppc32-target
             (ash (the fixnum (- subtag ppc32::min-cl-ivector-subtag)) -3)
             #+ppc64-target
             (ash (the fixnum (logand subtag #x7f)) (- ppc64::nlowtagbits)))
      #+x8632-target
      (svref x8632::*immheader-array-types*
	     (ash (the fixnum (- subtag x8632::min-cl-ivector-subtag))
		  (- x8632::ntagbits)))
      #+x8664-target
      (let* ((class (logand subtag x8664::fulltagmask))
             (idx (ash subtag (- x8664::ntagbits))))
        (declare (fixnum class idx))
        (cond ((= class x8664::ivector-class-64-bit)
               (%svref x8664::*immheader-2-array-types* idx))
              ((= class x8664::ivector-class-32-bit)
               (%svref x8664::*immheader-1-array-types* idx))
              (t
               (%svref x8664::*immheader-0-array-types* idx))))
      #+arm-target
      (svref arm::*immheader-array-types*
             (ash (the fixnum (- subtag arm::min-cl-ivector-subtag)) -3))
      )))



(defun adjustable-array-p (array)
  "Return T if (ADJUST-ARRAY ARRAY...) would return an array identical
   to the argument, this happens for complex arrays."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (>= (the (unsigned-byte 8) (gvector-typecode-p typecode))
                target::subtag-arrayH)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
      (if (or (= typecode target::subtag-arrayH)
              (= typecode target::subtag-vectorH))
        (logbitp $arh_adjp_bit (the fixnum (%svref array target::arrayH.flags-cell))))
      (report-bad-arg array 'array))))

(defun array-displacement (array)
  "Return the values of :DISPLACED-TO and :DISPLACED-INDEX-offset
   options to MAKE-ARRAY, or NIL and 0 if not a displaced array."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (and (or (= typecode target::subtag-arrayH)
                 (= typecode target::subtag-vectorH))
             (logbitp $arh_exp_disp_bit
                      (the fixnum (%svref array target::arrayH.flags-cell))))
      (values (%svref array target::arrayH.data-vector-cell)
              (%svref array target::arrayH.displacement-cell))
      (if (array-typecode-p typecode)
        (values nil 0)
        (report-bad-arg array 'array)))))

(defun array-data-and-offset (array)
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
            (= typecode target::subtag-vectorH))
      (%array-header-data-and-offset array)
      (if (or (= typecode target::subtag-simple-vector)
              (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                  target::min-cl-ivector-subtag))
        (values array 0)
        (report-bad-arg array 'array)))))

(defun array-data-offset-subtype (array)
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-vectorH)
            (= typecode target::subtag-arrayH))
      (do* ((header array data)
            (offset (%svref header target::arrayH.displacement-cell)
                    (+ offset 
                       (the fixnum 
                         (%svref header target::arrayH.displacement-cell))))
            (data (%svref header target::arrayH.data-vector-cell)
                  (%svref header target::arrayH.data-vector-cell)))
           ((> (the fixnum (typecode data)) target::subtag-vectorH)
            (values data offset (typecode data)))
        (declare (fixnum offset)))
      (if (or (= typecode target::subtag-simple-vector)
              (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                  target::min-cl-ivector-subtag))
        (values array 0 typecode)
        (report-bad-arg array 'array)))))
  

(defun array-has-fill-pointer-p (array)
  "Return T if the given ARRAY has a fill pointer, or NIL otherwise."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-vectorH)
      (logbitp $arh_fill_bit (the fixnum (%svref array target::vectorH.flags-cell)))
      (unless (array-typecode-p typecode)
        (report-bad-arg array 'array)))))


(defun fill-pointer (array)
  "Return the FILL-POINTER of the given VECTOR."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (and (= typecode target::subtag-vectorH)
             (logbitp $arh_fill_bit (the fixnum (%svref array target::vectorH.flags-cell))))
      (%svref array target::vectorH.logsize-cell)
      (report-bad-arg array '(and array (satisfies array-has-fill-pointer-p))))))

(defun set-fill-pointer (array value)
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (and (= typecode target::subtag-vectorH)
             (logbitp $arh_fill_bit (the fixnum (%svref array target::vectorH.flags-cell))))
      (let* ((vlen (%svref array target::vectorH.physsize-cell)))
        (declare (fixnum vlen))
        (if (eq value t)
          (setq value vlen)
          (unless (and (fixnump value)
                     (>= (the fixnum value) 0)
                     (<= (the fixnum value) vlen))
            (%err-disp $XARROOB value array)))
        (setf (%svref array target::vectorH.logsize-cell) value))
      (%err-disp $XNOFILLPTR array))))

(eval-when (:compile-toplevel)
  (assert (eql target::vectorH.physsize-cell target::arrayH.physsize-cell)))

(defun array-total-size (array)
  "Return the total number of elements in the Array."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (or (= typecode target::subtag-arrayH)
              (= typecode target::subtag-vectorH))
        (%svref array target::vectorH.physsize-cell)
      (if (or (= typecode target::subtag-simple-vector)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
        (uvsize array)
        (report-bad-arg array 'array)))))

      

(defun array-dimension (array axis-number)
  "Return the length of dimension AXIS-NUMBER of ARRAY."
  (unless (typep axis-number 'fixnum) (report-bad-arg axis-number 'fixnum))
  (locally
    (declare (fixnum axis-number))
    (let* ((typecode (typecode array)))
      (declare (fixnum typecode))
      (if (array-typecode-p typecode)
        (if (= typecode target::subtag-arrayH)
          (let* ((rank (%svref array target::arrayH.rank-cell)))
            (declare (fixnum rank))
            (unless (and (>= axis-number 0)
                         (< axis-number rank))
              (%err-disp $XNDIMS array axis-number))
            (%svref array (the fixnum (+ target::arrayH.dim0-cell axis-number))))
          (if (neq axis-number 0)
            (%err-disp $XNDIMS array axis-number)
            (if (= typecode target::subtag-vectorH)
              (%svref array target::vectorH.physsize-cell)
              (uvsize array))))
        (report-bad-arg array 'array)))))

(defun array-dimensions (array)
  "Return a list whose elements are the dimensions of the array"
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (array-typecode-p typecode)
      (if (= typecode target::subtag-arrayH)
        (let* ((rank (%svref array target::arrayH.rank-cell))
               (dims ()))
          (declare (fixnum rank))        
          (do* ((i (1- rank) (1- i)))
               ((< i 0) dims)
            (declare (fixnum i))
            (push (%svref array (the fixnum (+ target::arrayH.dim0-cell i))) dims)))
        (list (if (= typecode target::subtag-vectorH)
                (%svref array target::vectorH.physsize-cell)
                (uvsize array))))
      (report-bad-arg array 'array))))


(defun array-rank (array)
  "Return the number of dimensions of ARRAY."
  (let* ((typecode (typecode array)))
    (declare (fixnum typecode))
    (if (array-typecode-p typecode)
      (if (= typecode target::subtag-arrayH)
        (%svref array target::arrayH.rank-cell)
        1)
      (report-bad-arg array 'array))))

(defun vector-push (elt vector)
  "Attempt to set the element of ARRAY designated by its fill pointer
   to NEW-EL, and increment the fill pointer by one. If the fill pointer is
   too large, NIL is returned, otherwise the index of the pushed element is
   returned."
  (let* ((fill (fill-pointer vector))
         (len (%svref vector target::vectorH.physsize-cell)))
    (declare (fixnum fill len))
    (when (< fill len)
      (multiple-value-bind (data offset) (%array-header-data-and-offset vector)
        (declare (fixnum offset))
        (setf (%svref vector target::vectorH.logsize-cell) (the fixnum (1+ fill))
              (uvref data (the fixnum (+ fill offset))) elt)
        fill))))

;;; Implement some of the guts of REPLACE, where the source and target
;;; sequence have the same type (and we might be able to BLT things
;;; around more quickly because of that.)
;;; Both TARGET and SOURCE are (SIMPLE-ARRAY (*) *), and all of the
;;; indices are fixnums and in bounds.
;;; (Actually, we allow some internal uvector types as well as CL vectors.)
(defun %uvector-replace (target target-start source source-start n typecode)
  (declare (fixnum target-start n source-start n typecode)
           (optimize (speed 3) (safety 0)))
  (if (gvectorp target)
    (if (and (eq source target)
             (> target-start source-start))
      (do* ((i 0 (1+ i))
            (source-pos (1- (the fixnum (+ source-start n)))
                        (1- source-pos))
            (target-pos (1- (the fixnum (+ target-start n)))
                        (1- target-pos)))
           ((= i n))
        (declare (fixnum i source-pos target-pos))
        (setf (%svref target target-pos) (%svref source source-pos)))
      (dotimes (i n)
        (setf (%svref target target-start) (%svref source source-start))
        (incf target-start)
        (incf source-start)))
    (ecase typecode
      (#.target::subtag-bit-vector
       (if (and (eq source target)
                (> target-start source-start))
         (do* ((i 0 (1+ i))
               (source-pos (1- (the fixnum (+ source-start n)))
                           (1- source-pos))
               (target-pos (1- (the fixnum (+ target-start n)))
                           (1- target-pos)))
              ((= i n))
           (declare (fixnum i source-pos target-pos))
           (setf (sbit target target-pos) (sbit source source-pos)))
         (dotimes (i n)
           (setf (sbit target target-start) (sbit source source-start))
           (incf target-start)
           (incf source-start))))
      ;; All other cases can be handled with %COPY-IVECTOR-TO-IVECTOR,
      ;; which knows how to handle overlap
      ((#.target::subtag-s8-vector
        #.target::subtag-u8-vector)
       (%copy-ivector-to-ivector source
                                 source-start
                                 target
                                 target-start
                                 n))
      ((#.target::subtag-s16-vector
        #.target::subtag-u16-vector)
       (%copy-ivector-to-ivector source
                                 (the fixnum (* source-start 2))
                                 target
                                 (the fixnum (* target-start 2))
                                 (the fixnum (* n 2))))
      ((#.target::subtag-s32-vector
        #.target::subtag-u32-vector
        #.target::subtag-single-float-vector
        #.target::subtag-simple-base-string
        #.target::subtag-bignum
        #.target::subtag-single-float
        #.target::subtag-double-float
        #+32-bit-target #.target::subtag-fixnum-vector)
       (%copy-ivector-to-ivector source
                                 (the fixnum (* source-start 4))
                                 target
                                 (the fixnum (* target-start 4))
                                 (the fixnum (* n 4))))
      ((#.target::subtag-double-float-vector
        #+64-bit-target #.target::subtag-s64-vector
        #+64-bit-target #.target::subtag-u64-vector
        #+64-bit-target #.target::subtag-fixnum-vector
        #.target::subtag-complex-single-float-vector)
       (%copy-ivector-to-ivector source
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* source-start 8))))
                                 target
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* target-start 8))))
                                 (the fixnum (* n 8))))
      (#.target::subtag-complex-double-float-vector
        (%copy-ivector-to-ivector source
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* source-start 16))))
                                 target
                                 (the fixnum
                                   (+ (the fixnum (- target::misc-dfloat-offset
                                                     target::misc-data-offset))
                                      (the fixnum (* target-start 16))))
                                 (the fixnum (* n 16))))))
  target)

(defun vector-push-extend (elt vector &optional (extension nil extp))
  "Attempt to set the element of VECTOR designated by its fill pointer
to ELT, and increment the fill pointer by one. If the fill pointer is
too large, VECTOR is extended using adjust-array.  EXTENSION is the
minimum number of elements to add if it must be extended."
  (when extp
    (unless (and (typep extension 'fixnum)
                 (> (the fixnum extension) 0))
      (setq extension (require-type extension 'unsigned-byte))))
  (let* ((fill (fill-pointer vector))
         (len (%svref vector target::vectorH.physsize-cell)))
    (declare (fixnum fill len))
    (multiple-value-bind (data offset) (%array-header-data-and-offset vector)
      (declare (fixnum offset))
      (if (= fill len)
        (let* ((flags (%svref vector target::arrayH.flags-cell)))
          (declare (fixnum flags))
          (unless (logbitp $arh_adjp_bit flags)
            (%err-disp $XMALADJUST vector))
          (let* ((new-size (max
                            (+ len (the fixnum (or extension
                                                  len)))
                            4))
                 (typecode (typecode data))
                 (new-vector (%alloc-misc new-size typecode)))
            (%uvector-replace new-vector 0 data offset fill typecode)
            (setf (%svref vector target::vectorH.data-vector-cell) new-vector
                  (%svref vector target::vectorH.displacement-cell) 0
                  (%svref vector target::vectorH.physsize-cell) new-size
                  (%svref vector target::vectorH.flags-cell) (bitclr $arh_exp_disp_bit flags)
                  (uvref new-vector fill) elt)))
        (setf (uvref data (the fixnum (+ offset fill))) elt))
      (setf (%svref vector target::vectorH.logsize-cell) (the fixnum (1+ fill))))
    fill))

;;; Could avoid potential memoization somehow
(defun vector (&lexpr vals)
  "Construct a SIMPLE-VECTOR from the given objects."
  (let* ((n (%lexpr-count vals))
         (v (allocate-typed-vector :simple-vector n)))
    (declare (fixnum n))
    (dotimes (i n v) (setf (%svref v i) (%lexpr-ref vals n i)))))

;;; CALL-ARGUMENTS-LIMIT.
(defun list-to-vector (elts)
  (let* ((n (length elts)))
    (declare (fixnum n))
    (if (< n (floor #x8000 target::node-size))
      (apply #'vector elts)
      (make-array n :initial-contents elts))))

             
    
(defun %gvector (subtag &lexpr vals)
  (let* ((n (%lexpr-count vals))
         (v (%alloc-misc n subtag)))
    (declare (fixnum n))
    (dotimes (i n v) (setf (%svref v i) (%lexpr-ref vals n i)))))

(defun %aref1 (v i)
  (let* ((typecode (typecode v)))
    (declare (type (unsigned-byte 8)  typecode))
    (if (or (= typecode target::subtag-simple-vector)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
      (uvref v i)
      (if (= typecode target::subtag-vectorH)
        (multiple-value-bind (data offset)
                             (%array-header-data-and-offset v)
          (unless (typep i 'fixnum)
            (report-bad-arg i 'fixnum))
          (unless (and (typep i 'fixnum)
                       (>= (the fixnum i) 0)
                       (< (the fixnum i) (the fixnum (%svref v target::vectorH.physsize-cell))))
            (if (not (typep i 'fixnum))
              (report-bad-arg i 'fixnum)
              (%err-disp $XARROOB i v)))
          (uvref data (+ offset i)))
        (if (= typecode target::subtag-arrayH)
          (%err-disp $XNDIMS v 1)
          (report-bad-arg v 'array))))))

(defun %aset1 (v i new)
  (let* ((typecode (typecode v)))
    (declare (type (unsigned-byte 8) typecode))
    (if (or (= typecode target::subtag-simple-vector)
            (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                target::min-cl-ivector-subtag))
      (setf (uvref v i) new)
      (if (= typecode target::subtag-vectorH)
        (multiple-value-bind (data offset)
                             (%array-header-data-and-offset v)
          (unless (and (typep i 'fixnum)
                       (>= (the fixnum i) 0)
                       (< (the fixnum i) (the fixnum (%svref v target::vectorH.physsize-cell))))
            (if (not (typep i 'fixnum))
              (report-bad-arg i 'fixnum)
              (%err-disp $XARROOB i v)))
          (setf (uvref data (+ offset i)) new))
        (if (= typecode target::subtag-arrayH)
          (%err-disp $XNDIMS v 1)
          (report-bad-arg v 'array))))))

;;; Validate the N indices in the lexpr L against the
;;; array-dimensions of L.  If anything's out-of-bounds,
;;; error out (unless NO-ERROR is true, in which case
;;; return NIL.)
;;; If everything's OK, return the "row-major-index" of the array.
;;; We know that A's an array-header of rank N.

(defun %array-index (a l n &optional no-error)
  (declare (fixnum n))
  (let* ((count (%lexpr-count l)))
    (declare (fixnum count))
    (do* ((axis (1- n) (1- axis))
          (chunk-size 1)
          (result 0))
         ((< axis 0) result)
      (declare (fixnum result axis chunk-size))
      (let* ((index (%lexpr-ref l count axis))
             (dim (%svref a (the fixnum (+ target::arrayH.dim0-cell axis)))))
        (declare (fixnum dim))
        (unless (and (typep index 'fixnum)
                     (>= (the fixnum index) 0)
                     (< (the fixnum index) dim))
          (if no-error
            (return-from %array-index nil)
            (error "Index value ~d is out of bounds for axis ~d of ~s."
                   index axis a)))
        (incf result (the fixnum (* chunk-size (the fixnum index))))
        (setq chunk-size (* chunk-size dim))))))

(defun aref (a &lexpr subs)
  "Return the element of the ARRAY specified by the SUBSCRIPTS."
  (let* ((n (%lexpr-count subs)))
    (declare (fixnum n))
    (if (= n 1)
      (%aref1 a (%lexpr-ref subs n 0))
      (if (= n 2)
        (%aref2 a (%lexpr-ref subs n 0) (%lexpr-ref subs n 1))
        (if (= n 3)
          (%aref3 a (%lexpr-ref subs n 0) (%lexpr-ref subs n 1) (%lexpr-ref subs n 2))
          (let* ((typecode (typecode a)))
            (declare (fixnum typecode))
            (if (or (>= (the (unsigned-byte 8) (gvector-typecode-p typecode))
                        target::subtag-vectorH)
                    (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                        target::min-cl-ivector-subtag))
              (%err-disp $XNDIMS a n)
              (if (/= typecode target::subtag-arrayH)
                (report-bad-arg a 'array)
                ;;  This typecode is Just Right ...
                (progn
                  (unless (= (the fixnum (%svref a target::arrayH.rank-cell)) n)
                    (%err-disp $XNDIMS a n))
                  (let* ((rmi (%array-index a subs n)))
                    (declare (fixnum rmi))
                    (multiple-value-bind (data offset) (%array-header-data-and-offset a)
                      (declare (fixnum offset))
                      (uvref data (the fixnum (+ offset rmi))))))))))))))





(defun aset (a &lexpr subs&val)
  (let* ((count (%lexpr-count subs&val))
         (nsubs (1- count)))
    (declare (fixnum nsubs count))
    (if (eql count 0)
      (%err-disp $xneinps)
      (let* ((val (%lexpr-ref subs&val count nsubs)))
        (if (= nsubs 1)
          (%aset1 a (%lexpr-ref subs&val count 0) val)
          (if (= nsubs 2)
            (%aset2 a (%lexpr-ref subs&val count 0) (%lexpr-ref subs&val count 1) val)
            (if (= nsubs 3)
              (%aset3 a (%lexpr-ref subs&val count 0) (%lexpr-ref subs&val count 1) (%lexpr-ref subs&val count 2) val)
              (let* ((typecode (typecode a)))
                (declare (fixnum typecode))
                (if (or (>= (the (unsigned-byte 8) (gvector-typecode-p typecode))
                        target::subtag-vectorH)
                    (>= (the (unsigned-byte 8) (ivector-typecode-p typecode))
                        target::min-cl-ivector-subtag))
                  (%err-disp $XNDIMS a nsubs)
                  (if (/= typecode target::subtag-arrayH)
                    (report-bad-arg a 'array)
                    ;;  This typecode is Just Right ...
                    (progn
                      (unless (= (the fixnum (%svref a target::arrayH.rank-cell)) nsubs)
                        (%err-disp $XNDIMS a nsubs))
                      (let* ((rmi (%array-index a subs&val nsubs)))
                        (declare (fixnum rmi))
                        (multiple-value-bind (data offset) (%array-header-data-and-offset a)
                          (setf (uvref data (the fixnum (+ offset rmi))) val))))))))))))))


(defun schar (s i)
  "SCHAR returns the character object at an indexed position in a string
   just as CHAR does, except the string must be a simple-string."
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (aref (the simple-string s) i)
      (report-bad-arg s 'simple-string))))

(defun %scharcode (s i)
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (locally (declare (optimize (speed 3) (safety 0)))
        (aref (the (simple-array (unsigned-byte 32) (*)) s) i))
      (report-bad-arg s 'simple-string))))

(defun set-schar (s i v)
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (setf (aref (the simple-string s) i) v)
      (report-bad-arg s 'simple-string))))

(defun %set-scharcode (s i v)
  (let* ((typecode (typecode s)))
    (declare (fixnum typecode))
    (if (= typecode target::subtag-simple-base-string)
      (locally (declare (optimize (speed 3) (safety 0)))
        (setf (aref (the simple-string s) i) v))
      (report-bad-arg s 'simple-string))))

;;; Strings are simple-strings, start & end values are sane.
(defun %simple-string= (str1 str2 start1 start2 end1 end2)
  (declare (fixnum start1 start2 end1 end2))
  (when (= (the fixnum (- end1 start1))
           (the fixnum (- end2 start2)))
    (locally (declare (type simple-base-string str1 str2))
      (do* ((i1 start1 (1+ i1))
	    (i2 start2 (1+ i2)))
	   ((= i1 end1) t)
	(declare (fixnum i1 i2))
	(unless (eq (schar str1 i1) (schar str2 i2))
	  (return))))))

(defun copy-uvector (src)
  (%extend-vector 0 src (uvsize src)))

#+(or ppc32-target arm-target)
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= #.target::fulltag-immheader (logand subtag #.target::fulltagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((element-bit-shift
          (if (<= subtag target::max-32-bit-ivector-subtag)
            5
            (if (<= subtag target::max-8-bit-ivector-subtag)
              3
              (if (<= subtag target::max-16-bit-ivector-subtag)
                4
                (if (= subtag target::subtag-double-float-vector)
                  6
                  (if (= subtag target::subtag-complex-double-float-vector)
                      (return-from subtag-bytes
                        ;; There's a 32-bit pad at the beginning of the vector.
                        (+ 4 (ash element-count 4)))
                      (if (= subtag target::subtag-complex-single-float-vector)
                          (return-from subtag-bytes
                            ;; There's a 32-bit pad at the beginning of the vector.
                            (+ 4 (ash element-count 3)))
                          0)))))))
         (total-bits (ash element-count element-bit-shift)))
    (ash (+ 7 total-bits) -3)))

#+ppc64-target
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= ppc64::lowtag-immheader (logand subtag ppc64::lowtagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((ivector-class (logand subtag ppc64::fulltagmask))
         (element-bit-shift
          (if (= ivector-class ppc64::ivector-class-32-bit)
            5
            (if (= ivector-class ppc64::ivector-class-8-bit)
              3
              (if (= ivector-class ppc64::ivector-class-64-bit)
                6
                (if (= subtag ppc64::subtag-bit-vector)
                  0
                  4)))))
         (total-bits (ash element-count element-bit-shift)))
    (declare (fixnum ivector-class element-bit-shift total-bits))
    (ash (the fixnum (+ 7 total-bits)) -3)))

#+x8632-target
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (= #.x8632::fulltag-immheader (logand subtag #.x8632::fulltagmask))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((element-bit-shift
          (if (<= subtag x8632::max-32-bit-ivector-subtag)
            5
            (if (<= subtag x8632::max-8-bit-ivector-subtag)
              3
              (if (<= subtag x8632::max-16-bit-ivector-subtag)
                4
                (if (= subtag x8632::subtag-double-float-vector)
                  6
                  (if (= subtag x8632::subtag-complex-double-float-vector)
                      (return-from subtag-bytes
                        ;; There's a 32-bit pad at the beginning of the vector.
                        (+ 4 (ash element-count 4)))
                      (if (= subtag x8632::subtag-complex-single-float-vector)
                          (return-from subtag-bytes
                            ;; There's a 32-bit pad at the beginning of the vector.
                            (+ 4 (ash element-count 3)))
                          0)))))))
         (total-bits (ash element-count element-bit-shift)))
    (ash (+ 7 total-bits) -3)))

#+x8664-target
(defun subtag-bytes (subtag element-count)
  (declare (fixnum subtag element-count))
  (unless (logbitp (the (mod 16) (logand subtag x8664::fulltagmask))
                   (logior (ash 1 x8664::fulltag-immheader-0)
                           (ash 1 x8664::fulltag-immheader-1)
                           (ash 1 x8664::fulltag-immheader-2)))
    (error "Not an ivector subtag: ~s" subtag))
  (let* ((ivector-class (logand subtag x8664::fulltagmask))
         (element-bit-shift
          (if (= ivector-class x8664::ivector-class-32-bit)
            5
            (if (= ivector-class x8664::ivector-class-64-bit)
                6
                (if (= subtag x8664::subtag-bit-vector)
                  0
                  (if (= subtag x8664::subtag-complex-double-float-vector)
                      (return-from subtag-bytes
                        ;; There's a 64-bit pad at the beginning of the vector.
                        (+ 8 (ash element-count 4)))
                      (if (>= subtag x8664::min-8-bit-ivector-subtag)
                          3
                          4))))))
         (total-bits (ash element-count element-bit-shift)))
    (declare (fixnum ivector-class element-bit-shift total-bits))
    (ash (the fixnum (+ 7 total-bits)) -3)))

(defun element-type-subtype (type)
  "Convert element type specifier to internal array subtype code"
  (ctype-subtype (specifier-type type)))

(defun ctype-subtype (ctype)
  (typecase ctype
    (class-ctype
     (if (or (eq (class-ctype-class ctype) *character-class*)
	     (eq (class-ctype-class ctype) *base-char-class*)
             (eq (class-ctype-class ctype) *standard-char-class*))
       target::subtag-simple-base-string
       target::subtag-simple-vector))
    (numeric-ctype
     (if (eq (numeric-ctype-complexp ctype) :complex)
       (case (numeric-ctype-format ctype)
         (single-float target::subtag-complex-single-float-vector)
         (double-float target::subtag-complex-double-float-vector)
         (t target::subtag-simple-vector))
       (case (numeric-ctype-class ctype)
	 (integer
	  (let* ((low (numeric-ctype-low ctype))
		 (high (numeric-ctype-high ctype)))
	    (cond ((or (null low) (null high)) target::subtag-simple-vector)
		  ((and (>= low 0) (<= high 1)) target::subtag-bit-vector)
		  ((and (>= low 0) (<= high 255))
                   target::subtag-u8-vector)
		  ((and (>= low 0) (<= high 65535))
                   target::subtag-u16-vector)
		  ((and (>= low 0) (<= high #xffffffff))
                   target::subtag-u32-vector)
		  ((and (>= low -128) (<= high 127)) target::subtag-s8-vector)
		  ((and (>= low -32768) (<= high 32767)) target::subtag-s16-vector)
                  #+32-bit-target
                  ((and (>= low target::target-most-negative-fixnum)
                        (<= high target::target-most-positive-fixnum))
                   target::subtag-fixnum-vector)
		  ((and (>= low (ash -1 31)) (<= high (1- (ash 1 31))))
		   target::subtag-s32-vector)
                  #+64-bit-target
                  ((and (>= low target::target-most-negative-fixnum)
                        (<= high target::target-most-positive-fixnum))
                   target::subtag-fixnum-vector)                  
                  #+64-bit-target
                  ((and (>= low 0) (<= high (1- (ash 1 64))))
                   target::subtag-u64-vector)
                  #+64-bit-target
                  ((and (>= low (ash -1 63)) (<= high (1- (ash 1 63))))
                   target::subtag-s64-vector)
		  (t target::subtag-simple-vector))))
	 (float
	  (case (numeric-ctype-format ctype)
	    ((double-float long-float) target::subtag-double-float-vector)
	    ((single-float short-float) target::subtag-single-float-vector)
	    (t target::subtag-simple-vector)))
	 (t target::subtag-simple-vector))))
    (named-ctype ; *, T, etc.
     target::subtag-simple-vector)
    (t
     (harder-ctype-subtype ctype))))

(defun %set-simple-array-p (array)
  (setf (%svref array  target::arrayh.flags-cell)
        (bitset  $arh_simple_bit (%svref array target::arrayh.flags-cell))))

(defun  %array-header-simple-p (array)
  (logbitp $arh_simple_bit (%svref array target::arrayh.flags-cell)))

(defun %misc-ref (v i)
  (%misc-ref v i))

(defun %misc-set (v i new)
  (%misc-set v i new))

#-ppc-target
(defun %extend-vector (start oldv newsize)
  (declare (fixnum start))
  (let* ((typecode (typecode oldv))
         (new (%alloc-misc newsize typecode))
         (oldsize (uvsize oldv)))
    (declare (fixnum oldsize) (type (unsigned-byte 8) typecode))
    (%uvector-replace  new start oldv 0 oldsize typecode)))



; end of l0-array.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-array.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-misc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")


;;; Bootstrapping for futexes
#+(and linux-target no (or x86-target arm-target))
(eval-when (:compile-toplevel :execute)
  (pushnew :futex *features*))

#+futex
(eval-when (:compile-toplevel :execute)
  ;; We only need a few constants from <linux/futex.h>, which may
  ;; not have been included in the :libc .cdb files.
  (defconstant FUTEX-WAIT 0)
  (defconstant FUTEX-WAKE 1)
  (defconstant futex-avail 0)
  (defconstant futex-locked 1)
  (defconstant futex-contended 2)
  (declaim (inline %lock-futex %unlock-futex)))

;;; Miscellany.

(defun memq (item list)
  (do* ((tail list (%cdr tail)))
       ((null tail))
    (if (eq item (car tail))
      (return tail))))

(defun %copy-u8-to-string (u8-vector source-idx string dest-idx n)
  (declare (optimize (speed 3) (safety 0))
           (fixnum source-idx dest-idx n)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (simple-base-string string))
  (do* ((i 0 (1+ i)))
       ((= i n) string)
    (declare (fixnum i))
    (setf (%scharcode string dest-idx) (aref u8-vector source-idx))
    (incf source-idx)
    (incf dest-idx)))

(defun %copy-string-to-u8 (string source-idx u8-vector dest-idx n)
  (declare (optimize (speed 3) (safety 0))
           (fixnum source-idx dest-idx n)
           (type (simple-array (unsigned-byte 8) (*)) u8-vector)
           (simple-base-string string))
  (do* ((i 0 (1+ i)))
       ((= i n) u8-vector)
    (declare (fixnum i))
    (let* ((code (%scharcode string source-idx)))
      (declare (type (mod #x11000) code))
      (if (> code #xff)
        (setq code (char-code #\Sub)))
      (setf (aref u8-vector dest-idx) code)
      (incf source-idx)
      (incf dest-idx))))
    
        


(defun append-2 (y z)
  (if (null y)
    z
    (let* ((new (cons (car y) nil))
           (tail new))
      (declare (list new tail))
      (dolist (head (cdr y))
        (setq tail (cdr (rplacd tail (cons head nil)))))
      (rplacd tail z)
      new)))









(defun dbg (&optional arg)
  (dbg arg))


; This takes a simple-base-string and passes a C string into
; the kernel "Bug" routine.  Not too fancy, but neither is #_DebugStr,
; and there's a better chance that users would see this message.
(defun bug (arg)
  (if (typep arg 'simple-base-string)
    #+x86-target
    (debug-trap-with-string arg)
    #-x86-target
    (let* ((len (length arg)))
      (%stack-block ((buf (1+ len)))
        (%cstr-pointer arg buf)
        (ff-call 
         (%kernel-import target::kernel-import-lisp-bug)
         :address buf
         :void)))
    (bug "Bug called with non-simple-base-string.")))

(defun total-bytes-allocated ()
  (%heap-bytes-allocated)
  #+not-any-more
  (+ (unsignedwide->integer *total-bytes-freed*)
     (%heap-bytes-allocated)))

(defun %freebytes ()
  (with-macptrs (p)
    (%setf-macptr-to-object p
                            (%fixnum-ref (%get-kernel-global 'all-areas)
                                         target::area.succ))
    (- (%get-natural p target::area.high)
       (%get-natural p target::area.active))))

(defun %reservedbytes ()
  (with-macptrs (p)
    (%setf-macptr-to-object p (%get-kernel-global 'all-areas))
    (- #+32-bit-target
       (%get-unsigned-long p target::area.high)
       #+64-bit-target
       (%%get-unsigned-longlong p target::area.high)
       #+32-bit-target
       (%get-unsigned-long p target::area.low)
       #+64-bit-target
       (%%get-unsigned-longlong p target::area.low))))

(defun object-in-application-heap-p (address)
  (declare (ignore address))
  t)

(defun frozen-space-dnodes ()
  "Returns the current size of the frozen area."
  (%fixnum-ref-natural (%get-kernel-global 'tenured-area)
                       target::area.static-dnodes))
(defun %usedbytes ()
  (with-lock-grabbed (*kernel-exception-lock*)
    (with-lock-grabbed (*kernel-tcr-area-lock*)
      (%normalize-areas)
      (let ((static 0)
            (dynamic 0)
            (library 0))
        (do-consing-areas (area)
          (let* ((active (%fixnum-ref area target::area.active))
                 (bytes (ash (- active
                                (%fixnum-ref area target::area.low))
                             target::fixnumshift))
                 (code (%fixnum-ref area target::area.code)))
            (when (object-in-application-heap-p active)
              (if (eql code area-dynamic)
                (incf dynamic bytes)
                (if (eql code area-managed-static)
                  (incf library bytes)
                  (incf static bytes))))))
        (let* ((frozen-size (ash (frozen-space-dnodes) target::dnode-shift)))
          (decf dynamic frozen-size)
          (values dynamic static library frozen-size))))))



(defun %stack-space ()
  (%normalize-areas)
  (let ((free 0)
        (used 0))
    (with-macptrs (p)
      (do-gc-areas (area)
	(when (member (%fixnum-ref area target::area.code)
		      '(#.area-vstack
			#.area-cstack
                      #.area-tstack))
	  (%setf-macptr-to-object p area)
	  (let ((active
                 #+32-bit-target
                  (%get-unsigned-long p target::area.active)
                  #+64-bit-target
                  (%%get-unsigned-longlong p target::area.active))
		(high
                 #+32-bit-target
                  (%get-unsigned-long p target::area.high)
                  #+64-bit-target
                  (%%get-unsigned-longlong p target::area.high))
		(low
                 #+32-bit-target
                 (%get-unsigned-long p target::area.low)
                 #+64-bit-target
                 (%%get-unsigned-longlong p target::area.low)))
	    (incf used (- high active))
	    (incf free (- active low))))))
    (values (+ free used) used free)))



; Returns an alist of the form:
; ((thread cstack-free cstack-used vstack-free vstack-used tstack-free tstack-used)
;  ...)
(defun %stack-space-by-lisp-thread ()
  (let* ((res nil))
    (without-interrupts
     (dolist (p (all-processes))
       (let* ((thread (process-thread p)))
         (when thread
           (push (cons thread (multiple-value-list (%thread-stack-space thread))) res)))))
    res))



;;; Returns six values on most platforms, 4 on ARM.
;;;   sp free
;;;   sp used
;;;   vsp free
;;;   vsp used
;;;   tsp free  (not on ARM)
;;;   tsp used  (not on ARM)
(defun %thread-stack-space (&optional (thread *current-lisp-thread*))
  (when (eq thread *current-lisp-thread*)
    (%normalize-areas))
  (labels ((free-and-used (area)
	     (with-macptrs (p)
	       (%setf-macptr-to-object p area)
	       (let* ((low
                       #+32-bit-target
                       (%get-unsigned-long p target::area.low)
                       #+64-bit-target
                       (%%get-unsigned-longlong p target::area.low))
		      (high
                       #+32-bit-target
                        (%get-unsigned-long p target::area.high)
                        #+64-bit-target
                        (%%get-unsigned-longlong p target::area.high))
		      (active
                       #+32-bit-target
                       (%get-unsigned-long p target::area.active)
                       #+64-bit-target
                       (%%get-unsigned-longlong p target::area.active))
		      (free (- active low))
		      (used (- high active)))
		 (loop
		     (setq area (%fixnum-ref area target::area.older))
		     (when (eql area 0) (return))
		   (%setf-macptr-to-object p area)
		   (let ((low
                          #+32-bit-target
                           (%get-unsigned-long p target::area.low)
                           #+64-bit-target
                           (%%get-unsigned-longlong p target::area.low))
			 (high
                          #+32-bit-target
                           (%get-unsigned-long p target::area.high)
                           #+64-bit-target
                           (%%get-unsigned-longlong p target::area.high)))
		     (declare (fixnum low high))
		     (incf used (- high low))))
		 (values free used)))))
    (let* ((tcr (lisp-thread.tcr thread))
	   (cs-area #+(and windows-target x8632-target)
		    (%fixnum-ref (%fixnum-ref tcr (- target::tcr.aux
						     target::tcr-bias))
				 target::tcr-aux.cs-area)
		    #-(and windows-target x8632-target)
		    (%fixnum-ref tcr target::tcr.cs-area)))
      (if (or (null tcr)
	      (zerop (%fixnum-ref cs-area)))
	(values 0 0 0 0 0 0)
	(multiple-value-bind (cf cu) (free-and-used cs-area)
	  (multiple-value-bind (vf vu)
	      (free-and-used (%fixnum-ref tcr (- target::tcr.vs-area
						 target::tcr-bias)))
            #+arm-target
            (values cf cu vf vu)
            #-arm-target
	    (multiple-value-bind (tf tu)
		(free-and-used (%fixnum-ref tcr (- target::tcr.ts-area
						   target::tcr-bias)))
	      (values cf cu vf vu tf tu))))))))


(defun room (&optional (verbose :default))
  "Print to *STANDARD-OUTPUT* information about the state of internal
  storage and its management. The optional argument controls the
  verbosity of output. If it is T, ROOM prints out a maximal amount of
  information. If it is NIL, ROOM prints out a minimal amount of
  information. If it is :DEFAULT or it is not supplied, ROOM prints out
  an intermediate amount of information."
  (let* ((freebytes nil)
         (usedbytes nil)
         (static-used nil)
         (staticlib-used nil)
         (frozen-space-size nil)
         (lispheap nil)
         (reserved nil)
         (static nil)
         (stack-total)
         (stack-used)
         (stack-free)
         (static-cons-reserved nil)
         (stack-used-by-thread nil))
    (progn
      (progn
        (setq freebytes (%freebytes))
        (when verbose
          (multiple-value-setq (usedbytes static-used staticlib-used frozen-space-size)
            (%usedbytes))
          (setq lispheap (+ freebytes usedbytes)
                reserved (%reservedbytes)
                static (+ static-used staticlib-used frozen-space-size))
          (multiple-value-setq (stack-total stack-used stack-free)
            (%stack-space))
          (unless (eq verbose :default)
            (setq stack-used-by-thread (%stack-space-by-lisp-thread))))))
    (format t "~&Approximately ~:D bytes of memory can be allocated ~%before the next full GC is triggered. ~%" freebytes)
    (when verbose
      (flet ((k (n) (round n 1024)))
        (princ "
                   Total Size             Free                 Used")
        (format t "~&Lisp Heap:~15t~10D (~DK)~35t~10D (~DK)~55t~10D (~DK)"
                lispheap (k lispheap)
                freebytes (k freebytes)
                usedbytes (k usedbytes))
        (format t "~&Stacks:~15t~10D (~DK)~35t~10D (~DK)~55t~10D (~DK)"
                stack-total (k stack-total)
                stack-free (k stack-free)
                stack-used (k stack-used))
        (format t "~&Static:~15t~10D (~DK)~35t~10D (~DK)~55t~10D (~DK)"
                static (k static)
                0 0
                static (k static))
        (when (and frozen-space-size (not (zerop frozen-space-size)))
          (setq static-cons-reserved (ash (reserved-static-conses) target::dnode-shift)
                frozen-space-size (- frozen-space-size static-cons-reserved))
          (unless (zerop static-cons-reserved)
            (format t "~&~,3f MB of reserved static conses (~d free, ~d reserved)"
                    (/ static-cons-reserved (float (ash 1 20)))
                    (free-static-conses)
                    (reserved-static-conses)))

          (unless (zerop frozen-space-size)
                  (format t "~&~,3f MB of static memory is \"frozen\" dynamic memory"
                          (/ frozen-space-size (float (ash 1 20))))))
        (format t "~&~,3f MB reserved for heap expansion."
                (/ reserved (float (ash 1 20))))
        (unless (eq verbose :default)
          (terpri)
          (let* ((processes (all-processes)))
            (dolist (thread-info stack-used-by-thread)
              (destructuring-bind (thread sp-free sp-used vsp-free vsp-used #-arm-target tsp-free #-arm-target tsp-used)
                  thread-info
                (let* ((process (dolist (p processes)
                                  (when (eq (process-thread p) thread)
                                    (return p)))))
                  (when process
                    (let ((sp-total (+ sp-used sp-free))
                          (vsp-total (+ vsp-used vsp-free))
                          #-arm-target
                          (tsp-total (+ tsp-used tsp-free)))
                      (format t "~%~a(~d)~%  cstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)~
                               ~%  vstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)"
                              (process-name process)
                              (process-serial-number process)
                              sp-total (k sp-total) sp-free (k sp-free) sp-used (k sp-used)
                              vsp-total (k vsp-total) vsp-free (k vsp-free) vsp-used  (k vsp-used))
                      #-arm-target
                      (format t
                               "~%  tstack:~12T~10D (~DK)  ~33T~10D (~DK)  ~54T~10D (~DK)"

                              tsp-total (k tsp-total) tsp-free (k tsp-free) tsp-used (k tsp-used)))))))))))))


(defun list-length (l)
  "Return the length of the given LIST, or NIL if the LIST is circular."
  (do* ((n 0 (+ n 2))
        (fast l (cddr fast))
        (slow l (cdr slow)))
       ((null fast) n)
    (declare (fixnum n))
    (if (null (cdr fast))
      (return (the fixnum (1+ n)))
      (if (and (eq fast slow)
               (> n 0))
        (return nil)))))

(defun proper-list-p (l)
  (and (typep l 'list)
       (do* ((n 0 (+ n 2))
             (fast l (if (and (listp fast) (listp (cdr fast)))
                       (cddr fast)
                       (return-from proper-list-p nil)))
             (slow l (cdr slow)))
            ((null fast) n)
         (declare (fixnum n))
         (if (atom fast)
           (return nil)
           (if (null (cdr fast))
             (return t)
             (if (and (eq fast slow)
                      (> n 0))
               (return nil)))))))

(defun proper-sequence-p (x)
  (cond ((typep x 'vector))
	((typep x 'list) (not (null (list-length x))))))


(defun length (seq)
  "Return an integer that is the length of SEQUENCE."
  (seq-dispatch
   seq
   (or (list-length seq)
       (%err-disp $XIMPROPERLIST seq))
   (if (= (the fixnum (typecode seq)) target::subtag-vectorH)
     (%svref seq target::vectorH.logsize-cell)
     (uvsize seq))))

(defun %str-from-ptr (pointer len &optional (dest (make-string len)))
  (declare (fixnum len)
           (optimize (speed 3) (safety 0)))
  (dotimes (i len dest)
    (setf (%scharcode dest i) (%get-unsigned-byte pointer i))))

(defun %get-cstring (pointer)
  (do* ((end 0 (1+ end)))
       ((zerop (the (unsigned-byte 8) (%get-unsigned-byte pointer end)))
        (%str-from-ptr pointer end))
    (declare (fixnum end))))

(defun %get-utf-8-cstring (pointer)
  (do* ((end 0 (1+ end)))
       ((zerop (the (unsigned-byte 8) (%get-unsigned-byte pointer end)))
        (let* ((len (utf-8-length-of-memory-encoding pointer end 0))
               (string (make-string len)))
          (utf-8-memory-decode pointer end 0 string)
          string))
    (declare (fixnum end))))

;;; Assumes that pointer is terminated by a 0-valued 16-bit word
;;; and that it points to a valid utf-16 string with native endianness.
(defun %get-native-utf-16-cstring (pointer)
  (do* ((nchars 0 (1+ nchars))
        (i 0 (+ i 2))
        (code (%get-unsigned-word pointer i) (%get-unsigned-word pointer i)))
       ((zerop code)
        (do* ((string (make-string nchars))
              (out 0 (1+ out))
              (i 0 (+ i 2)))
             ((= out nchars) string)
          (declare (fixnum i out))
          (let* ((code (%get-unsigned-word pointer i)))
            (declare (type (unsigned-byte 16) code))
            (cond ((and (>= code #xd800)
                        (< code #xdc00))
                   (incf i 2)
                   (let* ((code2 (%get-unsigned-word pointer i)))
                     (declare (type (unsigned-byte 16) code2))
                     (setf (schar string out)
                           (utf-16-combine-surrogate-pairs code code2))))
                  (t (setf (schar string out) (code-char code)))))))
    (when (and (>= code #xd800) (< code #xdc00))
      (incf i 2))))


;;; This is mostly here so we can bootstrap shared libs without
;;; having to bootstrap #_strcmp.
;;; Return true if the cstrings are equal, false otherwise.
(defun %cstrcmp (x y)
  (do* ((i 0 (1+ i))
	(bx (%get-byte x i) (%get-byte x i))
	(by (%get-byte y i) (%get-byte y i)))
       ((not (= bx by)))
    (declare (fixnum i bx by))
    (when (zerop bx)
      (return t))))

(defun %cnstrcmp (x y n)
  (declare (fixnum n))
  (do* ((i 0 (1+ i))
	(bx (%get-byte x i) (%get-byte x i))
	(by (%get-byte y i) (%get-byte y i)))
       ((= i n) t)
    (declare (fixnum i bx by))
    (unless (= bx by)
      (return))))

(defvar %documentation nil)

(defvar %documentation-lock% nil)

(setq %documentation
  (make-hash-table :weak t :size 100 :test 'eq :rehash-threshold .95)
  %documentation-lock% (make-lock))

(defun %put-documentation (thing doc-id doc)
  (with-lock-grabbed (%documentation-lock%)
    (let* ((info (gethash thing %documentation))
	   (pair (assoc doc-id info)))
      (if doc
        (progn
          (unless (typep doc 'string)
            (report-bad-arg doc 'string))
          (if pair
            (setf (cdr pair) doc)
            (setf (gethash thing %documentation) (cons (cons doc-id doc) info))))
	(when pair
	  (if (setq info (nremove pair info))
	    (setf (gethash thing %documentation) info)
	    (remhash thing %documentation))))))
  doc)

(defun %get-documentation (object doc-id)
  (cdr (assoc doc-id (gethash object %documentation))))

;;; This pretends to be (SETF DOCUMENTATION), until that generic function
;;; is defined.  It handles a few common cases.
(defun %set-documentation (thing doc-id doc-string)
  (case doc-id
    (function 
     (if (typep thing 'function)
       (%put-documentation thing t doc-string)
       (if (typep thing 'symbol)
         (let* ((def (fboundp thing)))
           (if def
             (%put-documentation def t doc-string)))
         (if (setf-function-name-p thing)
           (%set-documentation
            (setf-function-name thing) doc-id doc-string)))))
    (variable
     (if (typep thing 'symbol)
       (%put-documentation thing doc-id doc-string)))
    (t (%put-documentation thing doc-id doc-string)))
  doc-string)


(%fhave 'set-documentation #'%set-documentation)



;;; This is intended for use by debugging tools.  It's a horrible thing
;;; to do otherwise.  The caller really needs to hold the heap-segment
;;; lock; this grabs the tcr queue lock as well.


(defparameter *spin-lock-tries* 1)
(defparameter *spin-lock-timeouts* 0)

#+(and (not futex) (not x86-target))
(defun %get-spin-lock (p)
  (let* ((self (%current-tcr))
         (n *spin-lock-tries*))
    (declare (fixnum n))
    (loop
      (dotimes (i n)
        (when (eql 0 (%ptr-store-fixnum-conditional p 0 self))
          (return-from %get-spin-lock t)))
      (%atomic-incf-node 1 '*spin-lock-timeouts* target::symbol.vcell)
      (yield))))

(eval-when (:compile-toplevel :execute)
  (declaim (inline note-lock-wait note-lock-held note-lock-released)))




(eval-when (:compile-toplevel)
  (declaim (inline %lock-recursive-lock-ptr %unlock-recursive-lock-ptr)))

#-futex
(defun %lock-recursive-lock-ptr (ptr lock flag)
  (with-macptrs ((p)
                 (owner (%get-ptr ptr target::lockptr.owner))
                 (signal (%get-ptr ptr target::lockptr.signal))
                 (spin (%inc-ptr ptr target::lockptr.spinlock)))
    (%setf-macptr-to-object p (%current-tcr))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (loop
      (without-interrupts
       (when (eql p owner)
         (incf (%get-natural ptr target::lockptr.count))
         (when flag
           (setf (lock-acquisition.status flag) t))
         (return t))
       (%get-spin-lock spin)
       (when (eql 1 (incf (%get-natural ptr target::lockptr.avail)))
         (setf (%get-ptr ptr target::lockptr.owner) p
               (%get-natural ptr target::lockptr.count) 1)
         (setf (%get-natural spin 0) 0)
         (if flag
           (setf (lock-acquisition.status flag) t))
         (return t))
       (setf (%get-natural spin 0) 0))
      (%process-wait-on-semaphore-ptr signal 1 0 (recursive-lock-whostate lock)))))

#+futex
(defun %lock-recursive-lock-ptr (ptr lock flag)
  (if (istruct-typep flag 'lock-acquisition)
    (setf (lock-acquisition.status flag) nil)
    (if flag (report-bad-arg flag 'lock-acquisition)))
  (let* ((self (%current-tcr))
         (level *interrupt-level*))
    (declare (fixnum self))
    (without-interrupts
     (cond ((eql self (%get-object ptr target::lockptr.owner))
            (incf (%get-natural ptr target::lockptr.count)))
           (t (%lock-futex ptr level lock #'recursive-lock-whostate)
              (%set-object ptr target::lockptr.owner self)
              (setf (%get-natural ptr target::lockptr.count) 1)))
     (when flag
       (setf (lock-acquisition.status flag) t))
     t)))
  

(defun %lock-recursive-lock-object (lock &optional flag)
  (%lock-recursive-lock-ptr (recursive-lock-ptr lock) lock flag))




#+futex
(progn
  #-monitor-futex-wait
  (defun futex-wait (p val whostate)
    (with-process-whostate (whostate)
      (int-errno-ffcall
       (%kernel-import target::kernel-import-lisp-futex)
       :address p :int FUTEX-WAIT :int val :address (%null-ptr) :address (%null-ptr) :int 0 :int)))
  #+monitor-futex-wait
  (progn
    (defparameter *total-futex-wait-calls* 0)
    (defparameter *total-futex-wait-times* 0)
    (defun futex-wait (p val whostate)
      (with-process-whostate (whostate)
        (let* ((start (get-internal-real-time)))
          (incf *total-futex-wait-calls*)
          (int-errno-ffcall
           (%kernel-import target::kernel-import-lisp-futex)
           :address p :int FUTEX-WAIT :int val :address (%null-ptr) :address (%null-ptr) :int 0 :int)
          (incf *total-futex-wait-times* (- (get-internal-real-time) start)))))))
    



#+futex
(defun futex-wake (p n)
  (int-errno-ffcall (%kernel-import target::kernel-import-lisp-futex)
                    :address p :int FUTEX-WAKE :int n :address (%null-ptr) :address (%null-ptr) :int 0 :int))

#+futex
(defun %lock-futex (p wait-level lock fwhostate)
  (let* ((val (%ptr-store-conditional p futex-avail futex-locked)))
    (declare (fixnum val))
    (or (eql val futex-avail)
        (loop
          (if (eql val futex-contended)
            (let* ((*interrupt-level* wait-level))
              (futex-wait p val (if fwhostate (funcall fwhostate lock) "futex wait")))
            (setq val futex-contended))
          (when (eql futex-avail (xchgl val p))
            (return t))))))

#+futex
(defun %unlock-futex (p)
  (unless (eql futex-avail (%atomic-decf-ptr p))
    (setf (%get-natural p target::lockptr.avail) futex-avail)
    (futex-wake p #$INT_MAX)))








          




#-futex
(defun %try-recursive-lock-object (lock &optional flag)
  (let* ((ptr (recursive-lock-ptr lock)))
    (with-macptrs ((p)
                   (owner (%get-ptr ptr target::lockptr.owner))
                   (spin (%inc-ptr ptr target::lockptr.spinlock)))
      (%setf-macptr-to-object p (%current-tcr))
      (if flag
        (if (istruct-typep flag 'lock-acquisition)
          (setf (lock-acquisition.status flag) nil)
          (report-bad-arg flag 'lock-acquisition)))
      (without-interrupts
       (cond ((eql p owner)
              (incf (%get-natural ptr target::lockptr.count))
              (if flag (setf (lock-acquisition.status flag) t))
              t)
             (t
              (let* ((win nil))
                (%get-spin-lock spin)
                (when (setq win (eql 1 (incf (%get-natural ptr target::lockptr.avail))))
                  (setf (%get-ptr ptr target::lockptr.owner) p
                        (%get-natural ptr target::lockptr.count) 1)
                  (if flag (setf (lock-acquisition.status flag) t)))
                (setf (%get-ptr spin) (%null-ptr))
                win)))))))



#+futex
(defun %try-recursive-lock-object (lock &optional flag)
  (let* ((self (%current-tcr))
         (ptr (recursive-lock-ptr lock)))
    (declare (fixnum self))
    (if flag
      (if (istruct-typep flag 'lock-acquisition)
        (setf (lock-acquisition.status flag) nil)
        (report-bad-arg flag 'lock-acquisition)))
    (without-interrupts
     (cond ((eql (%get-object ptr target::lockptr.owner) self)
            (incf (%get-natural ptr target::lockptr.count))
            (if flag (setf (lock-acquisition.status flag) t))
            t)
           (t
            (when (eql 0 (%ptr-store-conditional ptr futex-avail futex-locked))
              (%set-object ptr target::lockptr.owner self)
              (setf (%get-natural ptr target::lockptr.count) 1)
              (if flag (setf (lock-acquisition.status flag) t))
              t))))))





#-futex
(defun %unlock-recursive-lock-ptr (ptr lock)
  (with-macptrs ((signal (%get-ptr ptr target::lockptr.signal))
                 (spin (%inc-ptr ptr target::lockptr.spinlock)))
    (unless (eql (%get-object ptr target::lockptr.owner) (%current-tcr))
      (error 'not-lock-owner :lock lock))
    (without-interrupts
     (when (eql 0 (decf (the fixnum
                          (%get-natural ptr target::lockptr.count))))
       (%get-spin-lock spin)
       (setf (%get-ptr ptr target::lockptr.owner) (%null-ptr))
       (let* ((pending (+ (the fixnum
                            (1- (the fixnum (%get-fixnum ptr target::lockptr.avail))))
                          (the fixnum (%get-fixnum ptr target::lockptr.waiting)))))
         (declare (fixnum pending))
         (setf (%get-natural ptr target::lockptr.avail) 0
               (%get-natural ptr target::lockptr.waiting) 0)
         (setf (%get-ptr spin) (%null-ptr))
         (dotimes (i pending)
           (%signal-semaphore-ptr signal)))))
    nil))




#+futex
(defun %unlock-recursive-lock-ptr (ptr lock)
  (unless (eql (%get-object ptr target::lockptr.owner) (%current-tcr))
    (error 'not-lock-owner :lock lock))
  (without-interrupts
   (when (eql 0 (decf (the fixnum
                        (%get-natural ptr target::lockptr.count))))
     (setf (%get-natural ptr target::lockptr.owner) 0)
     (%unlock-futex ptr)))
  nil)

(defun %unlock-recursive-lock-object (lock)
  (%unlock-recursive-lock-ptr (%svref lock target::lock._value-cell) lock))




(defun %%lock-owner (lock)
  "Intended for debugging only; ownership may change while this code
   is running."
  (let* ((tcr (%get-object (recursive-lock-ptr lock) target::lockptr.owner)))
    (unless (zerop tcr)
      (tcr->process tcr))))

 
  




(defun %rplaca-conditional (cons-cell old new)
  (%store-node-conditional target::cons.car cons-cell old new))

(defun %rplacd-conditional (cons-cell old new)
  (%store-node-conditional target::cons.cdr cons-cell old new))

;;; Atomically push NEW onto the list in the I'th cell of uvector V.

(defun atomic-push-uvector-cell (v i new)
  (let* ((cell (cons new nil))
         (offset (+ target::misc-data-offset (ash i target::word-shift))))
    (loop
      (let* ((old (%svref v i)))
        (rplacd cell old)
        (when (%store-node-conditional offset v old cell)
          (return cell))))))

(defun atomic-pop-uvector-cell (v i)
  (let* ((offset (+ target::misc-data-offset (ash i target::word-shift))))
    (loop
      (let* ((old (%svref v i)))
        (if (null old)
          (return (values nil nil))
          (let* ((tail (cdr old)))
            (when (%store-node-conditional offset v old tail)
              (return (values (car old) t)))))))))


(defun store-gvector-conditional (index gvector old new)
  (declare (index index))
  (%store-node-conditional (the fixnum
                             (+ target::misc-data-offset
                                (the fixnum (ash index target::word-shift))))
			   gvector
			   old
			   new))

(defun %atomic-incf-car (cell &optional (by 1))
  (%atomic-incf-node (require-type by 'fixnum)
		     (require-type cell 'cons)
		     target::cons.car))

(defun %atomic-incf-cdr (cell &optional (by 1))
  (%atomic-incf-node (require-type by 'fixnum)
		     (require-type cell 'cons)
		     target::cons.cdr))

(defun %atomic-incf-gvector (v i &optional (by 1))
  (setq v (require-type v 'gvector))
  (setq i (require-type i 'fixnum))
  (%atomic-incf-node by v (+ target::misc-data-offset (ash i target::word-shift))))

(defun %atomic-incf-symbol-value (s &optional (by 1))
  (setq s (require-type s 'symbol))
  (multiple-value-bind (base offset) (%symbol-binding-address s)
    (%atomic-incf-node by base offset)))

;;; What happens if there are some pending readers and another writer,
;;; and we abort out of the semaphore wait ?  If the writer semaphore is
;;; signaled before we abandon interest in it
#-futex
(defun %write-lock-rwlock-ptr (ptr lock &optional flag)
  (with-macptrs ((write-signal (%get-ptr ptr target::rwlock.writer-signal)) )
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%get-spin-lock ptr)               ;(%get-spin-lock (%inc-ptr ptr target::rwlock.spin))
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (incf (%get-signed-natural ptr target::rwlock.state))
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (if flag
             (setf (lock-acquisition.status flag) t))
           t)
         (do* ()
              ((eql 0 (%get-signed-natural ptr target::rwlock.state))
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state) 1
                     (%get-natural ptr target::rwlock.spin) 0)
               (%set-object ptr target::rwlock.writer tcr)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (incf (%get-natural ptr target::rwlock.blocked-writers))
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (let* ((*interrupt-level* level))
                  (%process-wait-on-semaphore-ptr write-signal 1 0 (rwlock-write-whostate lock)))
           (%get-spin-lock ptr)))))))
#+futex
(defun %write-lock-rwlock-ptr (ptr lock &optional flag)
  (with-macptrs ((write-signal (%INC-ptr ptr target::rwlock.writer-signal)) )
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%lock-futex ptr level lock nil)
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (incf (%get-signed-natural ptr target::rwlock.state))
           (%unlock-futex ptr)
           (if flag
             (setf (lock-acquisition.status flag) t))
           t)
         (do* ()
              ((eql 0 (%get-signed-natural ptr target::rwlock.state))
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state) 1)
               (setf (%get-signed-long write-signal) -1)
               (%unlock-futex ptr)
               (%set-object ptr target::rwlock.writer tcr)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (incf (%get-natural ptr target::rwlock.blocked-writers))
           (let* ((waitval -1))
             (%unlock-futex ptr)
             (with-process-whostate ((rwlock-write-whostate lock))
               (let* ((*interrupt-level* level))
                 (futex-wait write-signal waitval (rwlock-write-whostate lock)))))
           (%lock-futex ptr level lock nil)
           (decf (%get-natural ptr target::rwlock.blocked-writers))))))))



(defun write-lock-rwlock (lock &optional flag)
  (%write-lock-rwlock-ptr (read-write-lock-ptr lock) lock flag))

#-futex
(defun %read-lock-rwlock-ptr (ptr lock &optional flag)
  (with-macptrs ((read-signal (%get-ptr ptr target::rwlock.reader-signal)))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%get-spin-lock ptr)             ;(%get-spin-lock (%inc-ptr ptr target::rwlock.spin))
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (error 'deadlock :lock lock))
         (do* ((state
                (%get-signed-natural ptr target::rwlock.state)
                (%get-signed-natural ptr target::rwlock.state)))
              ((<= state 0)
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state)
                     (the fixnum (1- state))
                     (%get-natural ptr target::rwlock.spin) 0)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (declare (fixnum state))
           (incf (%get-natural ptr target::rwlock.blocked-readers))
           (setf (%get-natural ptr target::rwlock.spin) 0)
           (let* ((*interrupt-level* level))
             (%process-wait-on-semaphore-ptr read-signal 1 0 (rwlock-read-whostate lock)))
           (%get-spin-lock ptr)))))))

#+futex
(defun %read-lock-rwlock-ptr (ptr lock &optional flag) 
  (with-macptrs ((reader-signal (%INC-ptr ptr target::rwlock.reader-signal)))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (declare (fixnum tcr))
      (without-interrupts
       (%lock-futex ptr level lock nil)
       (if (eq (%get-object ptr target::rwlock.writer) tcr)
         (progn
           (%unlock-futex ptr)
           (error 'deadlock :lock lock))
         (do* ((state
                (%get-signed-natural ptr target::rwlock.state)
                (%get-signed-natural ptr target::rwlock.state)))
              ((<= state 0)
               ;; That wasn't so bad, was it ?  We have the spinlock now.
               (setf (%get-signed-natural ptr target::rwlock.state)
                     (the fixnum (1- state)))
               (setf (%get-signed-long reader-signal) -1) ; can happen multiple times, but that's harmless
               (%unlock-futex ptr)
               (if flag
                 (setf (lock-acquisition.status flag) t))
               t)
           (declare (fixnum state))
           (incf (%get-natural ptr target::rwlock.blocked-readers))
           (let* ((waitval -1))
             (%unlock-futex ptr)
             (let* ((*interrupt-level* level))
               (futex-wait reader-signal waitval (rwlock-read-whostate lock))))
           (%lock-futex ptr level lock nil)
           (decf (%get-natural ptr target::rwlock.blocked-readers))))))))



(defun read-lock-rwlock (lock &optional flag)
  (%read-lock-rwlock-ptr (read-write-lock-ptr lock) lock flag))



#-futex
(defun %unlock-rwlock-ptr (ptr lock)
  (with-macptrs ((reader-signal (%get-ptr ptr target::rwlock.reader-signal))
                 (writer-signal (%get-ptr ptr target::rwlock.writer-signal)))
    (without-interrupts
     (%get-spin-lock ptr)
     (let* ((state (%get-signed-natural ptr target::rwlock.state))
            (tcr (%current-tcr)))
       (declare (fixnum state tcr))
       (cond ((> state 0)
              (unless (eql tcr (%get-object ptr target::rwlock.writer))
                (setf (%get-natural ptr target::rwlock.spin) 0)
                (error 'not-lock-owner :lock lock))
              (decf state))
             ((< state 0) (incf state))
             (t (setf (%get-natural ptr target::rwlock.spin) 0)
                (error 'not-locked :lock lock)))
       (setf (%get-signed-natural ptr target::rwlock.state) state)
       (when (zerop state)
         ;; We want any thread waiting for a lock semaphore to
         ;; be able to wait interruptibly.  When a thread waits,
         ;; it increments either the "blocked-readers" or "blocked-writers"
         ;; field, but since it may get interrupted before obtaining
         ;; the semaphore that's more of "an expression of interest"
         ;; in taking the lock than it is "a firm commitment to take it."
         ;; It's generally (much) better to signal the semaphore(s)
         ;; too often than it would be to not signal them often
         ;; enough; spurious wakeups are better than deadlock.
         ;; So: if there are blocked writers, the writer-signal
         ;; is raised once for each apparent blocked writer.  (At most
         ;; one writer will actually succeed in taking the lock.)
         ;; If there are blocked readers, the reader-signal is raised
         ;; once for each of them.  (It's possible for both the
         ;; reader and writer semaphores to be raised on the same
         ;; unlock; the writer semaphore is raised first, so in that
         ;; sense, writers still have priority but it's not guaranteed.)
         ;; Both the "blocked-writers" and "blocked-readers" fields
         ;; are cleared here (they can't be changed from another thread
         ;; until this thread releases the spinlock.)
         (setf (%get-signed-natural ptr target::rwlock.writer) 0)
         (let* ((nwriters (%get-natural ptr target::rwlock.blocked-writers))
                (nreaders (%get-natural ptr target::rwlock.blocked-readers)))
           (declare (fixnum nreaders nwriters))
           (when (> nwriters 0)
             (setf (%get-natural ptr target::rwlock.blocked-writers) 0)
             (dotimes (i nwriters)
               (%signal-semaphore-ptr writer-signal)))
           (when (> nreaders 0)
             (setf (%get-natural ptr target::rwlock.blocked-readers) 0)
             (dotimes (i nreaders)
               (%signal-semaphore-ptr reader-signal)))))
       (setf (%get-natural ptr target::rwlock.spin) 0)
       t))))

#+futex
(defun %unlock-rwlock-ptr (ptr lock)
  (with-macptrs ((reader-signal (%INC-ptr ptr target::rwlock.reader-signal))
                 (writer-signal (%INC-ptr ptr target::rwlock.writer-signal)))
    (let* ((signal nil)
           (wakeup 0))
    (without-interrupts
     (%lock-futex ptr -1 lock nil)
     (let* ((state (%get-signed-natural ptr target::rwlock.state))
            (tcr (%current-tcr)))
       (declare (fixnum state tcr))
       (cond ((> state 0)
              (unless (eql tcr (%get-object ptr target::rwlock.writer))
                (%unlock-futex ptr)
                (error 'not-lock-owner :lock lock))
              (decf state))
             ((< state 0) (incf state))
             (t (%unlock-futex ptr)
                (error 'not-locked :lock lock)))
       (setf (%get-signed-natural ptr target::rwlock.state) state)
       (when (zerop state)
         (setf (%get-signed-natural ptr target::rwlock.writer) 0)
         (let* ((nwriters (%get-natural ptr target::rwlock.blocked-writers))
                (nreaders (%get-natural ptr target::rwlock.blocked-readers)))
           (declare (fixnum nreaders nwriters))
           (if (> nwriters 0)
             (setq signal writer-signal wakeup 1)
             (if (> nreaders 0)
               (setq signal reader-signal wakeup #$INT_MAX)))))
       (when signal (setf (%get-signed-long signal) 0))
       (%unlock-futex ptr)
       (when signal (futex-wake signal wakeup))
       t)))))


(defun unlock-rwlock (lock)
  (%unlock-rwlock-ptr (read-write-lock-ptr lock) lock))

;;; There are all kinds of ways to lose here.
;;; The caller must have read access to the lock exactly once,
;;; or have write access.
;;; there's currently no way to detect whether the caller has
;;; read access at all.
;;; If we have to block and get interrupted, cleanup code may
;;; try to unlock a lock that we don't hold. (It might be possible
;;; to circumvent that if we use the same notifcation object here
;;; that controls that cleanup process.)

(defun %promote-rwlock (lock &optional flag)
  (let* ((ptr (read-write-lock-ptr lock)))
    (if (istruct-typep flag 'lock-acquisition)
      (setf (lock-acquisition.status flag) nil)
      (if flag (report-bad-arg flag 'lock-acquisition)))
    (let* ((level *interrupt-level*)
           (tcr (%current-tcr)))
      (without-interrupts
       #+futex
       (%lock-futex ptr level lock nil)
       #-futex
       (%get-spin-lock ptr)
       (let* ((state (%get-signed-natural ptr target::rwlock.state)))
         (declare (fixnum state))
         (cond ((> state 0)
                (unless (eql (%get-object ptr target::rwlock.writer) tcr)
                  #+futex
                  (%unlock-futex ptr)
                  #-futex
                  (setf (%get-natural ptr target::rwlock.spin) 0)
                  (error :not-lock-owner :lock lock)))
               ((= state 0)
                #+futex (%unlock-futex ptr)
                #-futex (setf (%get-natural ptr target::rwlock.spin) 0)
                (error :not-locked :lock lock))
               (t
                (if (= state -1)
                  (progn
                    (setf (%get-signed-natural ptr target::rwlock.state) 1)
                    (%set-object ptr target::rwlock.writer tcr)
                    #+futex
                    (%unlock-futex ptr)
                    #-futex
                    (setf (%get-natural ptr target::rwlock.spin) 0)
                    (if flag
                      (setf (lock-acquisition.status flag) t))
                    t)
                  (progn                    
                    #+futex
                    (%unlock-futex ptr)
                    #-futex
                    (setf (%get-natural ptr target::rwlock.spin) 0)
                    (%unlock-rwlock-ptr ptr lock)
                    (let* ((*interrupt-level* level))
                      (%write-lock-rwlock-ptr ptr lock flag)))))))))))
                      


(defun safe-get-ptr (p &optional dest)
  (if (null dest)
    (setq dest (%null-ptr))
    (unless (typep dest 'macptr)
      (check-type dest macptr)))
  (without-interrupts                   ;reentrancy
   (%safe-get-ptr p dest)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-misc.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-init.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*-Mode: LISP; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

(defconstant array-total-size-limit
  #.(expt 2 (- target::nbits-in-word target::num-subtag-bits))
  "the exclusive upper bound on the total number of elements in an array")


;Features for #+/- conditionalization:
(defparameter *features*
  '(:common-lisp
    :openmcl
    :ccl
    :ccl-1.2
    :ccl-1.3
    :ccl-1.4
    :ccl-1.5
    :ccl-1.6
    :ccl-1.7
    :ccl-1.8
    :ccl-1.9
    :ccl-1.10
    :ccl-1.11
    :ccl-1.12
    :clozure
    :clozure-common-lisp
    :ansi-cl
    #-windows-target :unix
    :openmcl-unicode-strings
    :ipv6
    ;; Threads and MOP stuff is pretty redundant.
    :openmcl-native-threads
    :openmcl-partial-mop
    :mcl-common-mop-subset
    :openmcl-mop-2
    ;; Thread-private hash-tables were introduced in version 1.0
    :openmcl-private-hash-tables
    ;; Hash-consing support (special primitives for allocating
    ;; and managing statically allocated CONS cells) will be
    ;; added in 1.1
    ;; Was dropped in 1.2
    #+(and x86-target 64-bit-target)
    :static-conses-should-work-with-egc-in-ccl 
    ;; :openmcl-hash-consing
    #+eabi-target :eabi-target
    #+ppc-target :powerpc
    #+ppc-target :ppc-target
    #+ppc-target :ppc-clos              ; used in encapsulate
    #+ppc32-target :ppc32-target
    #+ppc32-target :ppc32-host
    #+ppc64-target :ppc64-target
    #+ppc64-target :ppc64-host
    #+x8632-target :x8632-target
    #+x8632-target :x8632-host
    #+x8664-target :x86-64
    #+x8664-target :x86_64
    #+x8632-target :x86
    #+x86-target :x86-target
    #+x86-target :x86-host
    #+x8664-target :x8664-target
    #+x8664-target :x8664-host
    #+arm-target :arm
    #+arm-target :arm-target
    #+linux-target :linux-host
    #+linux-target :linux-target
    #+linuxppc-target :linuxppc-target
    #+linuxppc-target :linuxppc-host
    #+linuxx86-target :linuxx86-target
    #+linuxx8664-target :linuxx8664-target
    #+linuxx8664-target :linuxx8664-host
    #+linuxx8632-target :linuxx8632-target
    #+linuxx8632-target :linuxx8632-host
    #+linuxarm-target :linuxarm-target
    #+linuxarm-target :linuxarm-host
    #+darwinarm-target :darwinarm-target
    #+darwinarm-target :darwinarm-host
    #+darwinppc-target :darwinppc-target
    #+darwinppc-target :darwinppc-host
    #+darwinppc-target :darwin-target
    #+freebsd-target :freebsd-host
    #+freebsd-target :freebsd-target
    #+freebsdx86-target :freebsdx86-target
    #+freebsdx8664-target :freebsdx8664-target
    #+freebsdx8664-target :freebsdx8664-host
    #+freebsdx8632-target :freebsdx8632-target
    #+freebsdx8632-target :freebsdx8632-host
    #+darwin-target :darwin-host
    #+darwin-target :darwin-target
    #+darwinx86-target :darwinx86-target
    #+darwinx8632-target :darwinx8632-target
    #+darwinx8632-target :darwinx8632-host
    #+darwinx8664-target :darwinx8664-target
    #+darwinx8664-target :darwinx8664-host
    #+windows-target :windows-host
    #+windows-target :windows-target
    #+win64-target :win64-target
    #+win64-target :win64-host
    #+win32-target :win32-target
    #+win32-target :win32-host
    #+solaris-target :solaris-host
    #+solaris-target :solaris-target
    #+solarisx86-target :solarisx86-target
    #+solarisx8664-target :solarisx8664-target
    #+solarisx8664-target :solarisx8664-host
    #+solarisx8632-target :solarisx8632-target
    #+solarisx8632-target :solarisx8632-host
    #+android-target :android-host
    #+android-target :android-target
    #+androidarm-target :androidarm-target
    #+androidarm-target :androidarm-host
    #+(and ppc-target poweropen-target) :poweropen-target
    #+64-bit-target :64-bit-target
    #+64-bit-target :64-bit-host
    #+32-bit-target :32-bit-target
    #+32-bit-target :32-bit-host
    #+darwin-target :darwin
    #+linux-target :linux
    #+freebsd-target :freebsd
    #+solaris-target :solaris
    #+windows-target :windows
    #+android-target :android
    #+little-endian-target :little-endian-target
    #+little-endian-target :little-endian-host
    #+big-endian-target :big-endian-target
    #+big-endian-target :big-endian-host
    )
  "a list of symbols that describe features provided by the
   implementation")

(defparameter *optional-features* () "Set by build process")

(defparameter *load-verbose* nil
  "the default for the :VERBOSE argument to LOAD")

;All Lisp package variables... Dunno if this still matters, but it
;used to happen in the kernel...
(dolist (x '(* ** *** *APPLYHOOK* *DEBUG-IO*
             *DEFAULT-PATHNAME-DEFAULTS* *ERROR-OUTPUT* *EVALHOOK*
             *FEATURES* *LOAD-VERBOSE* *MACROEXPAND-HOOK* *MODULES*
             *PACKAGE* *PRINT-ARRAY* *PRINT-BASE* *PRINT-CASE* *PRINT-CIRCLE*
             *PRINT-ESCAPE* *PRINT-GENSYM* *PRINT-LENGTH* *PRINT-LEVEL*
             *PRINT-PRETTY* *PRINT-RADIX* *QUERY-IO* *RANDOM-STATE* *READ-BASE*
             *READ-DEFAULT-FLOAT-FORMAT* *READ-SUPPRESS* *READTABLE*
             *STANDARD-INPUT* *STANDARD-OUTPUT* *TERMINAL-IO* *TRACE-OUTPUT*
             + ++ +++ - / // /// ARRAY-DIMENSION-LIMIT ARRAY-RANK-LIMIT
             ARRAY-TOTAL-SIZE-LIMIT BOOLE-1 BOOLE-2 BOOLE-AND BOOLE-ANDC1
             BOOLE-ANDC2 BOOLE-C1 BOOLE-C2 BOOLE-CLR BOOLE-EQV BOOLE-IOR
             BOOLE-NAND BOOLE-NOR BOOLE-ORC1 BOOLE-ORC2 BOOLE-SET BOOLE-XOR
             CALL-ARGUMENTS-LIMIT CHAR-CODE-LIMIT
             DOUBLE-FLOAT-EPSILON DOUBLE-FLOAT-NEGATIVE-EPSILON
             INTERNAL-TIME-UNITS-PER-SECOND LAMBDA-LIST-KEYWORDS
             LAMBDA-PARAMETERS-LIMIT LEAST-NEGATIVE-DOUBLE-FLOAT
             LEAST-NEGATIVE-LONG-FLOAT LEAST-NEGATIVE-SHORT-FLOAT
             LEAST-NEGATIVE-SINGLE-FLOAT LEAST-POSITIVE-DOUBLE-FLOAT
             LEAST-POSITIVE-LONG-FLOAT LEAST-POSITIVE-SHORT-FLOAT
             LEAST-POSITIVE-SINGLE-FLOAT LONG-FLOAT-EPSILON
             LONG-FLOAT-NEGATIVE-EPSILON MOST-NEGATIVE-DOUBLE-FLOAT
             MOST-NEGATIVE-FIXNUM MOST-NEGATIVE-LONG-FLOAT
             MOST-NEGATIVE-SHORT-FLOAT MOST-NEGATIVE-SINGLE-FLOAT
             MOST-POSITIVE-DOUBLE-FLOAT MOST-POSITIVE-FIXNUM
             MOST-POSITIVE-LONG-FLOAT MOST-POSITIVE-SHORT-FLOAT
             MOST-POSITIVE-SINGLE-FLOAT MULTIPLE-VALUES-LIMIT PI
             SHORT-FLOAT-EPSILON SHORT-FLOAT-NEGATIVE-EPSILON
             SINGLE-FLOAT-EPSILON SINGLE-FLOAT-NEGATIVE-EPSILON))
  (%symbol-bits x (%ilogior2 (%symbol-bits x) (ash 1 $sym_bit_special))))

(defparameter *loading-file-source-file* nil)
(defparameter *loading-toplevel-location* nil)

(defvar *nx-speed* 1)
(defvar *nx-space* 1)
(defvar *nx-safety* 1)
(defvar *nx-cspeed* 1)
(defvar *nx-debug* 1)

(defparameter *case-sensitive-filesystem* t)

;;; end
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-init.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n











\;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;1
;;;; BEGIN FILE ./reference/ccl/level-0/l0-bignum64.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2
;;;-*- Mode: Lisp; Package: CCL -*-
;;;
;;; Copyright 1994-2009 Clozure Associates
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;     http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(in-package "CCL")

#+64-bit-target
(eval-when (:compile-toplevel :execute)
  (require "ARCH")
  (require "NUMBER-MACROS")
  (require "NUMBER-CASE-MACRO")

  (defsetf bignum-ref bignum-set)
  
  (defconstant digit-size 32)
  (defconstant half-digit-size (/ digit-size 2))
  
  (defconstant maximum-bignum-length (1- (ash 1 56)))
  (defconstant all-ones-digit #xffffffff)
  (deftype bignum-index () `(integer 0 (,maximum-bignum-length)))
  (deftype bignum-element-type () `(unsigned-byte ,digit-size))
  (deftype bignum-half-element-type () `(unsigned-byte ,half-digit-size))
  (deftype bignum-type () 'bignum)
  (defmacro %normalize-bignum-macro (big)
    `(%normalize-bignum-2 t ,big))

  (defmacro %mostly-normalize-bignum-macro (big)
    `(%normalize-bignum-2 nil ,big))
  (defmacro %lognot (x)
    `(logand #xffffffff (lognot (the fixnum ,x))))
  (defmacro %logior (x y)
    `(logior (the fixnum ,x) (the fixnum ,y)))
  (defmacro %logxor (x y)
    `(logand #xffffffff (logxor (the fixnum ,x) (the fixnum ,y))))
  
  ;;; BIGNUM-REPLACE -- Internal.
  ;;;
  (defmacro bignum-replace (dest src &key (start1 '0) end1 (start2 '0) end2
                                 from-end)
    (once-only ((n-dest dest)
                (n-src src))
               (if (and (eq start1 0)(eq start2 0)(null end1)(null end2)(null from-end))
                 ;; this is all true for some uses today <<
                 `(%copy-ivector-to-ivector ,n-src 0 ,n-dest 0 (%ilsl 2 (min (the fixnum (%bignum-length ,n-src))
                                                                         (the fixnum (%bignum-length ,n-dest)))))
                 (let* ((n-start1 (gensym))
                        (n-end1 (gensym))
                        (n-start2 (gensym))
                        (n-end2 (gensym)))
                   `(let ((,n-start1 ,start1)
                          (,n-start2 ,start2)
                          (,n-end1 ,(or end1 `(%bignum-length ,n-dest)))
                          (,n-end2 ,(or end2 `(%bignum-length ,n-src))))
                     ,(if (null from-end)            
                          `(%copy-ivector-to-ivector
                            ,n-src (%i* 4 ,n-start2) 
                            ,n-dest (%i* 4 ,n-start1)
                            (%i* 4 (min (%i- ,n-end2 ,n-start2) 
                                    (%i- ,n-end1 ,n-start1))))
                          `(let ((nwds (min (%i- ,n-end2 ,n-start2)
                                            (%i- ,n-end1 ,n-start1))))
                            (%copy-ivector-to-ivector
                             ,n-src (%ilsl 2 (%i- ,n-end2 nwds))
                             ,n-dest (%ilsl 2 (%i- ,n-end1 nwds))
                             (%i* 4 nwds))))))))) 
  

  ;;;; Shifting.
  
  (defconstant all-ones-half-digit #xFFFF)  
  

;;; %ALLOCATE-BIGNUM must zero all elements.
;;;
  (defmacro %allocate-bignum (ndigits)
    `(%alloc-misc ,ndigits target::subtag-bignum))

  (declaim (inline  %bignum-length))

;;; This macro is used by BIGNUM-ASHIFT-RIGHT,
;;; BIGNUM-BUFFER-ASHIFT-RIGHT, and BIGNUM-LDB-BIGNUM-RES. They supply
;;; a termination form that references locals established by this
;;; form. Source is the source bignum. Start-digit is the first digit
;;; in source from which we pull bits. Start-pos is the first bit we
;;; want. Res-len-form is the form that computes the length of the
;;; resulting bignum. Termination is a DO termination form with a test
;;; and body. When result is supplied, it is the variable to which
;;; this binds a newly allocated bignum.
;;;
;;; Given start-pos, 1-31 inclusively, of shift, we form the j'th resulting
;;; digit from high bits of the i'th source digit and the start-pos number of
;;; bits from the i+1'th source digit.
  (defmacro shift-right-unaligned (source
                                   start-digit
                                   start-pos
                                   res-len-form
                                   termination
                                   &optional result)
    `(let* ((high-bits-in-first-digit (- digit-size ,start-pos))
            (res-len ,res-len-form)
            (res-len-1 (1- res-len))
            ,@(if result `((,result (%allocate-bignum res-len)))))
      (declare (type bignum-index res-len res-len-1))
      (do ((i ,start-digit i+1)
           (i+1 (1+ ,start-digit) (1+ i+1))
           (j 0 (1+ j)))
          ,termination
        (declare (type bignum-index i i+1 j))
        (setf (bignum-ref ,(if result result source) j)
              (%logior (%digit-logical-shift-right (bignum-ref ,source i)
                                                   ,start-pos)
                       (%ashl (bignum-ref ,source i+1)
                              high-bits-in-first-digit))))))


  )


#+64-bit-target
(progn

;;; Extract the length of the bignum.
;;; 
(defun %bignum-length (bignum)
  (uvsize bignum)) 



;;; We can probably do better than UVREF here, but
;;; a) it's not -that- bad
;;; b) it does some bounds/sanity checking, which isn't a bad idea.

(defmacro bignum-ref (b i)
  `(%typed-miscref :bignum ,b ,i))

(defmacro bignum-set (b i val)
  `(%typed-miscset :bignum ,b ,i ,val))


(defun bignum-plusp (b)
  (not (logbitp (1- digit-size) (the bignum-element-type (bignum-ref b (1- (%bignum-length b)))))))

;;; Return T if digit is positive, or NIL if negative.
(defun %digit-0-or-plusp (digit)
  (declare (type bignum-element-type digit))
  (not (logbitp (1- digit-size) digit)))

(defun %bignum-0-or-plusp (bignum len)
  (declare (type bignum-type bignum)
	   (type bignum-index len))
  (%digit-0-or-plusp (bignum-ref bignum (1- len))))

(defun bignum-minusp (b)
  (logbitp 31 (the fixnum (bignum-ref b (1- (%bignum-length b))))))

(defun %sign-digit (b i)
  (%ashr (bignum-ref b (1- i)) (1- digit-size)))

;;; Return the sign of bignum (0 or -1) as a fixnum
(defun %bignum-sign (b)
  (if (logbitp 31 (the fixnum (bignum-ref b (1- (%bignum-length b)))))
    -1
    0))

         
(defun %add-with-carry (a-digit b-digit carry-in)
  (declare (fixnum a-digit b-digit carry-in))
  (setq a-digit (logand all-ones-digit a-digit)
        b-digit (logand all-ones-digit b-digit))
  (let* ((sum (+ carry-in (the fixnum (+ a-digit b-digit)))))
    (declare (fixnum sum))
    (values (logand all-ones-digit sum) (logand 1 (the fixnum (ash sum -32))))))

(defun %subtract-with-borrow (a-digit b-digit borrow-in)
  (declare (fixnum a-digit b-digit borrow-in))
  (setq a-digit (logand all-ones-digit a-digit)
        b-digit (logand all-ones-digit b-digit))
  (let* ((diff (- (the fixnum (- a-digit b-digit))
                  (the fixnum (- 1 borrow-in)))))
    (declare (fixnum diff))
    (values (logand all-ones-digit diff)
            (- 1 (logand (the fixnum (ash diff -32)) 1)))))



(defun %compare-digits (bignum-a bignum-b idx)
  (let* ((a (bignum-ref bignum-a idx))
         (b (bignum-ref bignum-b idx)))
    (declare (fixnum a b))
    (if (= a b)
      0
      (if (> a b)
        1
        -1))))


;;;; Addition.
(defun add-bignums (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b)))
    (declare (bignum-index len-a len-b)
             (optimize (speed 3) (safety 0)))
    (when (> len-b len-a)
      (rotatef a b)
      (rotatef len-a len-b))
    (let* ((len-res (1+ len-a))
	   (res (%allocate-bignum len-res))
	   (carry 0)
	   (sign-b (%bignum-sign b)))
	(dotimes (i len-b)
          (let* ((sum (+
                       (the fixnum (+ (the bignum-element-type (bignum-ref a i))
                                      (the bignum-element-type (bignum-ref b i))))
                       carry)))
            (declare (fixnum sum))
            (setf (bignum-ref res i) sum)
            (setq carry (logand 1 (the fixnum (ash sum -32))))))
	(if (/= len-a len-b)
	  (finish-bignum-add  res carry a sign-b len-b len-a)
          (setf (bignum-ref res len-a)
                (+ (the fixnum carry)
                   (the fixnum (+ (the bignum-element-type (%bignum-sign a))
                                  sign-b)))))
	(%normalize-bignum-macro res))))

(defun add-bignum-and-fixnum (bignum fixnum)
  (declare (bignum-type bignum)
           (fixnum fixnum)
           (optimize (speed 3) (safety 0)))
  (let* ((len-bignum (%bignum-length bignum))
         (len-res (1+ len-bignum))
         (res (%allocate-bignum len-res))
         (low (logand all-ones-digit fixnum))
         (high (logand all-ones-digit (the fixnum (ash fixnum -32)))))
    (declare (bignum-index len-bignum)
             (bignum-type res)
             (bignum-element-type low high))
    (let* ((sum0 (+ (the bignum-element-type (bignum-ref bignum 0)) low))
           (sum1 (+ (the fixnum (+ (the bignum-element-type (bignum-ref bignum 1))
                                   high))
                    (the fixnum (logand 1 (ash sum0 -32)))))
           (carry (logand 1 (ash sum1 -32))))
      (declare (fixnum sum0 sum1) (bignum-element-type carry))
      (setf (bignum-ref res 0) sum0
            (bignum-ref res 1) sum1)
      (if (> len-bignum 2)
        (finish-bignum-add  res carry bignum (ash fixnum (- (- target::nbits-in-word target::fixnumshift))) 2 len-bignum)
        (setf (bignum-ref res 2)
              (+ (the fixnum carry)
                 (the fixnum (+ (the bignum-element-type (%bignum-sign bignum))
                                (the fixnum (ash fixnum (- (- target::nbits-in-word target::fixnumshift)))))))))
      (%normalize-bignum-macro res))))





;;; B was shorter than A; keep adding B's sign digit to each remaining
;;; digit of A, propagating the carry.
(defun finish-bignum-add (result carry a sign-b start end)
  (declare (type bignum-index start end)
           (bignum-element-type sign-b carry)
           (optimize (speed 3) (safety 0)))
  (do* ((i start (1+ i))
        (sign-b (logand all-ones-digit sign-b)))
       ((= i end)
        (setf (bignum-ref result end)
              (the fixnum (+
                           (the fixnum (+ (the fixnum
                                            (logand all-ones-digit
                                                    (the fixnum
                                                      (%sign-digit a end))))
                                          sign-b))
                           carry))))
    (declare (fixnum i) (bignum-element-type sign-b))
    (let* ((sum (the fixnum (+ (the fixnum (+ (bignum-ref a i)
                                              sign-b))
                               carry))))
      (declare (fixnum sum))
      (setf (bignum-ref result i) sum)
      (setq carry (logand 1 (the fixnum (ash sum -32)))))))




;;;; Subtraction.
(defun subtract-bignum (a b)
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (len-res (1+ (max len-a len-b)))
	 (res (%allocate-bignum len-res)))
    (declare (bignum-index len-a len-b len-res))
    (bignum-subtract-loop a len-a b len-b res)
    (%normalize-bignum-macro res)))

(defun bignum-subtract-loop (a len-a b len-b res)
  (declare (bignum-index len-a len-b )
           (optimize (speed 3) (safety 0)))
  (let* ((len-res (%bignum-length res)))
    (declare (bignum-index len-res))
    (let* ((borrow 1)
	   (sign-a (%bignum-sign a))
	   (sign-b (%bignum-sign b)))
      (declare (bignum-element-type borrow sign-a sign-b))
      (dotimes (i (the bignum-index len-res))
        (multiple-value-bind (result-digit borrow-out)
            (%subtract-with-borrow
             (if (< i len-a)
               (bignum-ref a i)
               sign-a)
             (if (< i len-b)
               (bignum-ref b i)
               sign-b)
             borrow)
          (setf (bignum-ref res i) result-digit
                borrow borrow-out))))))


;;;; Multiplication.

#||
;;; These parameters match GMP's.
(defvar *sqr-basecase-threshold* 5)
(defvar *sqr-karatsuba-threshold* 22)
(defvar *mul-karatsuba-threshold* 10)

;;; Squaring is often simpler than multiplication.  This should never
;;; be called with (>= N *sqr-karatsuba-threshold*).
(defun mpn-sqr-basecase (prodp up n)
  (declare (fixnum prodp up n))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (umulppm up up prodp)
  (when (> n 1)
    (%stack-block ((tarr (* 4 (* 2 *sqr-karatsuba-threshold*))))
      (let* ((tp (macptr->fixnum tarr)))
	(mpn-mul-1 tp
		   (the fixnum (1+ up))
		   (the fixnum (1- n))
		   up
		   (the fixnum (+ tp (the fixnum (1- n)))))
	(do* ((i 2 (1+ i)))
	     ((= i n))
	  (declare (fixnum i))
	  (mpn-addmul-1 (the fixnum (- (the fixnum (+ tp (the fixnum (+ i i))))
				       2))
			(the fixnum (+ up i))
			(the fixnum (- n i))
			(the fixnum (+ up (the fixnum (1- i))))
			(the fixnum (+ tp (the fixnum (+ n (the fixnum (- i 2))))))))
	(do* ((i 1 (1+ i))
	      (ul (1+ up) (1+ ul)))
	     ((= i n))
	  (declare (fixnum i ul))
	  (umulppm ul ul (the fixnum (+ prodp (the fixnum (+ i i))))))
	(let* ((2n-2 (- (the fixnum (+ n n)) 2))
	       (carry (mpn-lshift-1 tp tp 2n-2)))
	  (declare (fixnum 2n-2 carry))
	  (incf carry (the fixnum (mpn-add-n (the fixnum (1+ prodp))
					     (the fixnum (1+ prodp))
					     tp
					     2n-2)))
	  (add-fixnum-to-limb carry (the fixnum (+ prodp
						   (the fixnum (1-
								(the fixnum
								  (+ n n))))))))))))

;;; For large enough values of N, squaring via Karatsuba-style
;;; divide&conquer is faster than in the base case.
(defun mpn-kara-sqr-n (p a n ws)
  (declare (fixnum p a n ws))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%stack-block ((limbs 16))
    (let* ((w (macptr->fixnum limbs))
	   (w0 (1+ w))
	   (w1 (1+ w0))
	   (xx (1+ w1))
	   (n2 (ash n -1))
	   (x 0)
	   (y 0)
	   (i 0))
      (declare (fixnum w w0 w1 xx n2 x y i))
      (cond ((logbitp 0 n)
	     ;; Odd length
	     (let* ((n3 (- n n2))
		    (n1 0)
		    (nm1 0))
	       (declare (fixnum n3 n1 nm1))
	       (copy-limb (the fixnum (+ a n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum
		    (- (the fixnum (mpn-sub-n p a (the fixnum (+ a n3)) n2))))
		  w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ a i)) w0)
		     (copy-limb (the fixnum (+ a (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (= i 0))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ a n3)
			   y a)
		     (setq y (+ a n3)
			   x a))
		   (mpn-sub-n p x y n2)))
	       (copy-limb w (the fixnum (+ p n2)))
	       (setq n1 (1+ n))
	       (cond ((< n3 *sqr-basecase-threshold*)
		      (mpn-mul-basecase ws p n3 p n3)
		      (mpn-mul-basecase p a n3 a n3))
		     ((< n3 *sqr-karatsuba-threshold*)
		      (mpn-sqr-basecase ws p n3)
		      (mpn-sqr-basecase p a n3))
		     (t
		      (mpn-kara-sqr-n ws p n3 (the fixnum (+ ws n1)))
		      (mpn-kara-sqr-n p  a n3 (the fixnum (+ ws n1)))))
	       (cond ((< n2 *sqr-basecase-threshold*)
		      (mpn-mul-basecase (the fixnum (+ p n1))
					(the fixnum (+ a n3))
					n2
					(the fixnum (+ a n3))
					n2))
		     ((< n2 *sqr-karatsuba-threshold*)
		      (mpn-sqr-basecase (the fixnum (+ p n1))
					(the fixnum (+ a n3))
					n2))
		     (t
		      (mpn-kara-sqr-n (the fixnum (+ p n1))
				      (the fixnum (+ a n3))
				      n2
				      (the fixnum (+ ws n1)))))
	       (mpn-sub-n ws p ws n1)
	       (setq nm1 (1- n))
	       (unless (zerop (the fixnum
				(mpn-add-n ws
					   (the fixnum (+ p n1))
					   ws
					   nm1)))
		 (copy-limb (the fixnum (+ ws nm1)) xx)
		 (add-fixnum-to-limb 1 xx)
		 (copy-limb xx (the fixnum (+ ws nm1)))
		 (if (limb-zerop xx)
		   (add-fixnum-to-limb 1 (the fixnum (+ ws n)))))
	       (unless (zerop
			(the fixnum
			  (mpn-add-n (the fixnum (+ p n3))
				     (the fixnum (+ p n3))
				     ws
				     n1)))
		 (mpn-incr-u (the fixnum (+ p (the fixnum (+ n1 n3))))
			     1))))
	    (t ; N is even
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ a i)) w0)
	       (copy-limb (the fixnum (+ a (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (= i 0))
		 (return)))
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ a n2)
		     y a)
	       (setq y (+ a n2)
		     x a))
	     (mpn-sub-n p x y n2)
	     (cond ((< n2 *sqr-basecase-threshold*)
		    (mpn-mul-basecase ws p n2 p n2)
		    (mpn-mul-basecase p a n2 a n2)
		    (mpn-mul-basecase (the fixnum (+ p n))
				      (the fixnum (+ a n2))
				      n2
				      (the fixnum (+ a n2))
				      n2))
		   ((< n2 *sqr-karatsuba-threshold*)
		    (mpn-sqr-basecase ws p n2)
		    (mpn-sqr-basecase p a n2)
		    (mpn-sqr-basecase (the fixnum (+ p n))
				      (the fixnum (+ a n2))
				      n2))
		   (t
		    (mpn-kara-sqr-n ws p n2 (the fixnum (+ ws n)))
		    (mpn-kara-sqr-n p  a n2 (the fixnum (+ ws n)))
		    (mpn-kara-sqr-n (the fixnum (+ p n))
				    (the fixnum (+ a n2))
				    n2
				    (the fixnum (+ ws n)))))
	     (let* ((ww (- (the fixnum (mpn-sub-n ws p ws n)))))
	       (declare (fixnum ww))
	       (setq ww (+ ww (mpn-add-n ws (the fixnum (+ p n)) ws n)))
	       (setq ww (+ ww (mpn-add-n (the fixnum (+ p n2))
                                         (the fixnum (+ p n2))
                                         ws
                                         n)))
	       (mpn-incr-u (the fixnum (+ p (the fixnum (+ n2 n)))) ww)))))))

;;; Karatsuba subroutine: multiply A and B, store result at P, use WS
;;; as scrach space.  Treats A and B as if they were both of size N;
;;; if that's not true, caller must fuss around the edges.
(defun mpn-kara-mul-n (p a b n ws)
  (declare (fixnum p a b n ws))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (%stack-block ((limbs 16))
    (let* ((w (macptr->fixnum limbs))
	   (w0 (1+ w))
	   (w1 (1+ w0))
	   (xx (1+ w1))
	   (x 0)
	   (y 0)
	   (i 0)
	   (n2 (ash n -1))
	   (sign 0))
      (declare (fixnum w w0 w1 xx x y i n2 sign))
      (cond ((logbitp 0 n)
	     (let* ((n1 0)
		    (n3 (- n n2))
		    (nm1 0))
	       (declare (fixnum n1 n3 nm1))
	       (copy-limb (the fixnum (+ a n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum (- (mpn-sub-n p a (the fixnum (+ a n3)) n2))) w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ a i)) w0)
		     (copy-limb (the fixnum (+ a (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (zerop i))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ a n3)
			   y a
			   sign -1)
		     (setq x a
			   y (+ a n3)))
		   (mpn-sub-n p x y n2)))
	       (copy-limb w (the fixnum (+ p n2)))
	       (copy-limb (the fixnum (+ b n2)) w)
	       (if (not (limb-zerop w))
		 (add-fixnum-to-limb
		  (the fixnum (- (the fixnum (mpn-sub-n (the fixnum (+ p n3))
							b
							(the fixnum (+ b n3))
							n2))))
		  w)
		 (progn
		   (setq i n2)
		   (loop
		     (decf i)
		     (copy-limb (the fixnum (+ b i)) w0)
		     (copy-limb (the fixnum (+ b (the fixnum (+ n3 i)))) w1)
		     (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
			     (zerop i))
		       (return)))
		   (if (< (the fixnum (compare-limbs w0 w1)) 0)
		     (setq x (+ b n3)
			   y b
			   sign (lognot sign))
		     (setq x b
			   y (+ b n3)))
		   (mpn-sub-n (the fixnum (+ p n3)) x y n2)))
	       (copy-limb w (the fixnum (+ p n)))
	       (setq n1 (1+ n))
	       (cond
		 ((< n2 *mul-karatsuba-threshold*)
		  (cond
		    ((< n3 *mul-karatsuba-threshold*)
		     (mpn-mul-basecase ws p n3 (the fixnum (+ p n3)) n3)
		     (mpn-mul-basecase p a n3 b n3))
		    (t
		     (mpn-kara-mul-n ws p (the fixnum (+ p n3)) n3 (the fixnum (+ ws n1)))
		     (mpn-kara-mul-n p a b n3 (the fixnum (+ ws n1)))))
		  (mpn-mul-basecase (the fixnum (+ p n1))
				    (the fixnum (+ a n3))
				    n2
				    (the fixnum (+ b n3))
				    n2))
		 (t
		  (mpn-kara-mul-n ws p (the fixnum (+ p n3)) n3 (the fixnum (+ ws n1)))
		  (mpn-kara-mul-n p a b n3 (the fixnum (+ ws n1)))
		  (mpn-kara-mul-n (the fixnum (+ p n1))
				  (the fixnum (+ a n3))
				  (the fixnum (+ b n3))
				  n2
				  (the fixnum (+ ws n1)))))
	       (if (not (zerop sign))
		 (mpn-add-n ws p ws n1)
		 (mpn-sub-n ws p ws n1))
	       (setq nm1 (1- n))
	       (unless (zerop (the fixnum (mpn-add-n ws
						     (the fixnum (+ p n1))
						     ws
						     nm1)))
		 (copy-limb (the fixnum (+ ws nm1)) xx)
		 (add-fixnum-to-limb 1 xx)
		 (copy-limb xx (the fixnum (+ ws nm1)))
		 (if (limb-zerop xx)
		   (add-fixnum-to-limb 1 (the fixnum (+ ws n)))))
	       (unless (zerop (the fixnum
				(mpn-add-n (the fixnum (+ p n3))
					   (the fixnum (+ p n3))
					   ws
					   n1)))
		 (mpn-incr-u (the fixnum
			       (+ p (the fixnum (+ n1 n3)))) 1))))
	    (t				; even length
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ a i)) w0)
	       (copy-limb (the fixnum (+ a (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (zerop i))
		 (return)))
	     (setq sign 0)
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ a n2)
		     y a
		     sign -1)
	       (setq x a
		     y (+ a n2)))
	     (mpn-sub-n p x y n2)
	     (setq i n2)
	     (loop
	       (decf i)
	       (copy-limb (the fixnum (+ b i)) w0)
	       (copy-limb (the fixnum (+ b (the fixnum (+ n2 i)))) w1)
	       (if (or (not (zerop (the fixnum (compare-limbs w0 w1))))
		       (zerop i))
		 (return)))	      
	     (if (< (the fixnum (compare-limbs w0 w1)) 0)
	       (setq x (+ b n2)
		     y b
		     sign (lognot sign))
	       (setq x b
		     y (+ b n2)))
	     (mpn-sub-n (the fixnum (+ p n2)) x y n2)
	     (cond
	       ((< n2 *mul-karatsuba-threshold*)
		(mpn-mul-basecase ws p n2 (the fixnum (+ p n2)) n2)
		(mpn-mul-basecase p a n2 b n2)
		(mpn-mul-basecase (the fixnum (+ p n))
				  (the fixnum (+ a n2))
				  n2
				  (the fixnum (+ b n2))
				  n2))
	       (t
		(mpn-kara-mul-n ws p (the fixnum (+ p n2)) n2
				(the fixnum (+ ws n)))
		(mpn-kara-mul-n p a b n2 (the fixnum (+ ws n)))
		(mpn-kara-mul-n (the fixnum (+ p n))
				(the fixnum (+ a n2))
				(the fixnum (+ b n2))
				n2
				(the fixnum (+ ws n)))))
	     (let* ((ww (if (not (zerop sign))
			  (mpn-add-n ws p ws n)
			  (- (the fixnum (mpn-sub-n ws p ws n))))))
	       (declare (fixnum ww))
	       (setq ww (+ ww (mpn-add-n ws (the fixnum (+ p n)) ws n)))
	       (setq ww (+ ww (mpn-add-n (the fixnum (+ p n2))
                                         (the fixnum (+ p n2))
                                         ws
                                         n)))
	       (mpn-incr-u (the fixnum (+ p (the fixnum (+ n2 n)))) ww)))))))

;;; Square UP, of length UN.  I wonder if a Karatsuba multiply might be
;;; faster than a basecase square.
(defun mpn-sqr-n (prodp up un)
  (declare (fixnum prodp up un))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  (if (< un *sqr-basecase-threshold*)
    (mpn-mul-basecase prodp up un up un)
    (if (< un *sqr-karatsuba-threshold*)
      (mpn-sqr-basecase prodp up un)
      (%stack-block ((wsptr (mpn-kara-sqr-n-tsize un)))
	(mpn-kara-sqr-n prodp up un (macptr->fixnum wsptr))))))

;;; Subroutine: store AxB at P.  Assumes A & B to be of length N
(defun mpn-mul-n (p a b n)
  (declare (fixnum p a b n))
  (declare (optimize (speed 3) (safety 0) (space 0)))  
  (if (< n *mul-karatsuba-threshold*)
    (mpn-mul-basecase p a n b n)
    (%stack-block ((wsptr (mpn-kara-mul-n-tsize n)))
      (mpn-kara-mul-n p a b n (macptr->fixnum wsptr)))))


;;; Multiply [UP,UN] by [VP,VN].  UN must not be less than VN.
;;; This does Karatsuba if operands are big enough; if they are
;;; and they differ in size, this computes the product of the
;;; smaller-size slices, then fixes up the resut.
(defun mpn-mul (prodp up un vp vn)
  (declare (fixnum prodp up un vp vn))
  (declare (optimize (speed 3) (safety 0) (space 0)))
  ;(assert (>= un vn 1))
  (if (and (= up vp) (= un vn))
    (mpn-sqr-n prodp up un)
    (if (< vn *mul-karatsuba-threshold*)
      (mpn-mul-basecase prodp up un vp vn)
      (let* ((l vn))
	(declare (fixnum l))
	(mpn-mul-n prodp up vp vn)
	(unless (= un vn)
	  (incf prodp vn)
	  (incf up vn)
	  (decf un vn)
	  (if (< un vn)
	    (psetq un vn vn un up vp vp up))
	  (%stack-block ((wsptr
			  (the fixnum
			    (+ 8
			       (the fixnum
				 (* 4
				    (the fixnum
				      (+ vn
					 (if (>= vn *mul-karatsuba-threshold*)
					   vn
					   un)))))))))
	    (setf (%get-unsigned-long wsptr 0) 0
		  (%get-unsigned-long wsptr 4) 0)
	    (let* ((tt (macptr->fixnum wsptr))
		   (c (1+ tt))
		   (ws (1+ c)))
	      (declare (fixnum tt c ws ))
	      (do* ()
		   ((< vn *mul-karatsuba-threshold*))
		(mpn-mul-n ws up vp vn)
		(cond ((<= l (the fixnum (+ vn vn)))
		       (add-fixnum-to-limb (mpn-add-n prodp prodp ws l) tt)
		       (unless (= l (the fixnum (+ vn vn)))
			 (copy-fixnum-to-limb
			  (mpn-add-1 (the fixnum (+ prodp l))
				     (the fixnum (+ ws l))
				     (the fixnum (- (the fixnum (+ vn vn)) l))
				     tt)
			  tt)
			 (setq l (the fixnum (+ vn vn)))))
		      (t
		       (copy-fixnum-to-limb
			(mpn-add-n prodp prodp ws (the fixnum (+ vn vn))) c)
		       (add-fixnum-to-limb
			(mpn-add-1 (the fixnum (+ prodp (the fixnum (+ vn vn))))
				   (the fixnum (+ prodp (the fixnum (+ vn vn))))
				   (the fixnum (- l (the fixnum (+ vn vn))))
				   c)
			tt)))
		(incf prodp vn)
		(decf l vn)
		(incf up vn)
		(decf un vn)
		(if (< un vn)
		  (psetq up vp vp up un vn vn un)))
	      (unless (zerop vn)
		(mpn-mul-basecase ws up un vp vn)
		(cond ((<= l (the fixnum (+ un vn)))
		       (add-fixnum-to-limb
			(mpn-add-n prodp prodp ws l)
			tt)
		       (unless (= l (the fixnum (+ un vn)))
			 (copy-fixnum-to-limb
			  (mpn-add-1 (the fixnum (+ prodp l))
				     (the fixnum (+ ws l))
				     (the fixnum (- (the fixnum (+ un vn)) l))
				     tt)
			  tt)))
		      (t
		       (copy-fixnum-to-limb
			(mpn-add-n prodp prodp ws (the fixnum (+ un vn)))
			c)
		       (add-fixnum-to-limb
			(mpn-add-1
			 (the fixnum (+ prodp (the fixnum (+ un vn))))
			 (the fixnum (+ prodp (the fixnum (+ un vn))))
			 (the fixnum (- (the fixnum (- l un)) vn))
			 c)
			tt)))))))))))
||#

(defun multiply-bignums (a b)
  (let* ((signs-differ (not (eq (bignum-minusp a) (bignum-minusp b)))))
    (flet ((multiply-unsigned-bignums64 (a b)
	     (let* ((len-a (ceiling (%bignum-length a) 2))
		    (len-b (ceiling (%bignum-length b) 2))
		    (len-res (+ len-a len-b))
		    (res (%allocate-bignum (+ len-res len-res))))
	       (declare (bignum-index len-a len-b len-res))
	       (dotimes (i len-a)
		 (declare (type bignum-index i))
		 (%multiply-and-add-loop64 a b res i len-b))
	       res)))
      (let* ((res (with-negated-bignum-buffers a b
					       multiply-unsigned-bignums64)))
	(if signs-differ (negate-bignum-in-place res))
	(%normalize-bignum-macro res)))))

#+old
(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum-type bignum) (fixnum fixnum))
  (if (eql fixnum 1)
    bignum
    (with-small-bignum-buffers ((big-fix fixnum))
      (multiply-bignums bignum big-fix))))

(defun multiply-bignum-and-fixnum (bignum fixnum)
  (declare (type bignum-type bignum) (fixnum fixnum))
  (if (eql fixnum 1)
    bignum
    (let* ((bignum-len (%bignum-length bignum))
           (bignum-plus-p (bignum-plusp bignum))
           (fixnum-plus-p (not (minusp fixnum)))
           (negate-res (neq bignum-plus-p fixnum-plus-p)))
      (declare (type bignum-type bignum)
               (type bignum-index bignum-len))
      (flet ((do-it (bignum fixnum  negate-res)
               (let* ((bignum-len (%bignum-length bignum))
                      (result (%allocate-bignum (the fixnum (+ bignum-len 2))))
                      (len64 (ash (1+ bignum-len) -1)))
                 (declare (type bignum-type bignum)
                          (type bignum-index bignum-len len64))
                 (%multiply-and-add-fixnum-loop len64 bignum fixnum result)
                 (when negate-res
                   (negate-bignum-in-place result))
                 (%normalize-bignum-macro result ))))
        (declare (dynamic-extent #'do-it))
        (if bignum-plus-p
          (do-it bignum (if fixnum-plus-p fixnum (- fixnum))  negate-res)
          (with-bignum-buffers ((b1 (the fixnum (1+ bignum-len))))
            (negate-bignum bignum nil b1)
            (do-it b1 (if fixnum-plus-p fixnum (- fixnum))  negate-res)))))))


;; assume we already know result won't fit in a fixnum
;; only caller is fixnum-*-2
;;

(defun multiply-fixnums (a b)
  (declare (fixnum a b))
  (* a b))


;;;; GCD.


;;; Both args are > 0.
(defun bignum-fixnum-gcd (bignum fixnum)
  (let* ((rem (bignum-truncate-by-fixnum-no-quo bignum fixnum)))
    (declare (fixnum rem))
    (if (zerop rem)
      fixnum
      (%fixnum-gcd rem fixnum))))



;;; NEGATE-BIGNUM -- Public.
;;;
;;; Fully-normalize is an internal optional.  It cause this to always return
;;; a bignum, without any extraneous digits, and it never returns a fixnum.
;;;
(defun negate-bignum (x &optional (fully-normalize t) res)
  (declare (type bignum-type x))
  (let* ((len-x (%bignum-length x))
	 (len-res (1+ len-x))
         (minusp (bignum-minusp x))
	 (res (or res (%allocate-bignum len-res))))
    (declare (type bignum-index len-x len-res)) ;Test len-res for range?
    (let ((carry (bignum-negate-loop-really x len-x res)))
      (declare (fixnum carry))
      (if (zerop carry)
        (setf (bignum-ref res len-x) (if minusp 0 all-ones-digit))
        (setf (bignum-ref res len-x) (if minusp 1 0))))
    (if fully-normalize
      (%normalize-bignum-macro res)
      (%mostly-normalize-bignum-macro res))))

;;; NEGATE-BIGNUM-IN-PLACE -- Internal.
;;;
;;; This assumes bignum is positive; that is, the result of negating it will
;;; stay in the provided allocated bignum.
;;;
(defun negate-bignum-in-place (bignum)
  (bignum-negate-loop-really bignum (%bignum-length bignum) bignum)
  bignum)


  

(defun copy-bignum (bignum)
  (let ((res (%allocate-bignum (%bignum-length bignum))))
    (bignum-replace res bignum)
    res))



;;; BIGNUM-ASHIFT-RIGHT -- Public.
;;;
;;; First compute the number of whole digits to shift, shifting them by
;;; skipping them when we start to pick up bits, and the number of bits to
;;; shift the remaining digits into place.  If the number of digits is greater
;;; than the length of the bignum, then the result is either 0 or -1.  If we
;;; shift on a digit boundary (that is, n-bits is zero), then we just copy
;;; digits.  The last branch handles the general case which uses a macro that a
;;; couple other routines use.  The fifth argument to the macro references
;;; locals established by the macro.
;;;


(defun bignum-ashift-right (bignum x)
  (declare (type bignum-type bignum)
           (fixnum x)
           (optimize (speed 3) (safety 0)))
  (let ((bignum-len (%bignum-length bignum)))
    (declare (type bignum-index bignum-len))
    (multiple-value-bind (digits n-bits) (truncate x digit-size)
      (declare (type bignum-index digits)(fixnum n-bits))
      (cond
       ((>= digits bignum-len)
        (if (bignum-plusp bignum) 0 -1))
       ((eql 0 n-bits)
        (bignum-ashift-right-digits bignum digits))
       (t
        (shift-right-unaligned bignum digits n-bits (- bignum-len digits)
				      ((= j res-len-1)
				       (setf (bignum-ref res j)
					     (%ashr (bignum-ref bignum i) n-bits))
				       (%normalize-bignum-macro res))
				      res))))))

			       



;;; BIGNUM-ASHIFT-RIGHT-DIGITS -- Internal.
;;;
(defun bignum-ashift-right-digits (bignum digits)
  (declare (type bignum-type bignum)
	   (type bignum-index digits))
  (let* ((res-len (- (%bignum-length bignum) digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len)
	     (type bignum-type res))
    (bignum-replace res bignum :start2 digits)
    (%normalize-bignum-macro res)))


;;; BIGNUM-BUFFER-ASHIFT-RIGHT -- Internal.
;;;
;;; GCD uses this for an in-place shifting operation.  This is different enough
;;; from BIGNUM-ASHIFT-RIGHT that it isn't worth folding the bodies into a
;;; macro, but they share the basic algorithm.  This routine foregoes a first
;;; test for digits being greater than or equal to bignum-len since that will
;;; never happen for its uses in GCD.  We did fold the last branch into a macro
;;; since it was duplicated a few times, and the fifth argument to it
;;; references locals established by the macro.
;;;
 

;;; BIGNUM-ASHIFT-LEFT -- Public.
;;;
;;; This handles shifting a bignum buffer to provide fresh bignum data for some
;;; internal routines.  We know bignum is safe when called with bignum-len.
;;; First we compute the number of whole digits to shift, shifting them
;;; starting to store farther along the result bignum.  If we shift on a digit
;;; boundary (that is, n-bits is zero), then we just copy digits.  The last
;;; branch handles the general case.
;;;
(defun bignum-ashift-left (bignum x &optional bignum-len)
  (declare (type bignum-type bignum)
	   (fixnum x)
	   (type (or null bignum-index) bignum-len))
  (multiple-value-bind (digits n-bits)
		       (truncate x digit-size)
    (declare (fixnum digits n-bits))
    (let* ((bignum-len (or bignum-len (%bignum-length bignum)))
	   (res-len (+ digits bignum-len 1)))
      (declare (fixnum bignum-len res-len))
      (when (> res-len maximum-bignum-length)
	(error "Can't represent result of left shift."))
      (if (zerop n-bits)
        (bignum-ashift-left-digits bignum bignum-len digits)
        (bignum-ashift-left-unaligned bignum digits n-bits res-len)))))

;;; BIGNUM-ASHIFT-LEFT-DIGITS -- Internal.
;;;
(defun bignum-ashift-left-digits (bignum bignum-len digits)
  (declare (type bignum-index bignum-len digits))
  (let* ((res-len (+ bignum-len digits))
	 (res (%allocate-bignum res-len)))
    (declare (type bignum-index res-len))
    (bignum-replace res bignum :start1 digits :end1 res-len :end2 bignum-len
		    :from-end t)
    res))



;;; BIGNUM-ASHIFT-LEFT-UNALIGNED -- Internal.
;;;
;;; BIGNUM-TRUNCATE uses this to store into a bignum buffer by supplying res.
;;; When res comes in non-nil, then this foregoes allocating a result, and it
;;; normalizes the buffer instead of the would-be allocated result.
;;;
;;; We start storing into one digit higher than digits, storing a whole result
;;; digit from parts of two contiguous digits from bignum.  When the loop
;;; finishes, we store the remaining bits from bignum's first digit in the
;;; first non-zero result digit, digits.  We also grab some left over high
;;; bits from the last digit of bignum.
;;;

(defun bignum-ashift-left-unaligned (bignum digits n-bits res-len
                                            &optional (res nil resp))
  (declare (type bignum-index digits res-len)
	   (type (mod #.digit-size) n-bits))
  (let* ((remaining-bits (- digit-size n-bits))
	 (res-len-1 (1- res-len))
	 (res (or res (%allocate-bignum res-len))))
    (declare (type bignum-index res-len res-len-1)
             (optimize (speed 3) (safety 0)))
    (do ((i 0 i+1)
	 (i+1 1 (1+ i+1))
	 (j (1+ digits) (1+ j)))
	((= j res-len-1)
	 (setf (bignum-ref res digits)
	       (%ashl (bignum-ref bignum 0) n-bits))
	 (setf (bignum-ref res j)
	       (%ashr (bignum-ref bignum i) remaining-bits))
	 (if resp
           (%zero-trailing-sign-digits res res-len)
           (%mostly-normalize-bignum-macro res)))
      (declare (type bignum-index i i+1 j))
      (setf (bignum-ref res j)
	    (%logior (%digit-logical-shift-right (bignum-ref bignum i)
						 remaining-bits)
		     (%ashl (bignum-ref bignum i+1) n-bits))))))







;;;; Relational operators.



;;; BIGNUM-COMPARE -- Public.
;;;
;;; This compares two bignums returning -1, 0, or 1, depending on whether a
;;; is less than, equal to, or greater than b.
;;;
;(proclaim '(function bignum-compare (bignum bignum) (integer -1 1)))
(defun bignum-compare (a b)
  (declare (type bignum-type a b))
  (let* ((a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (if (eq a-plusp b-plusp)
      (let* ((len-a (%bignum-length a))
	     (len-b (%bignum-length b)))
	(declare (type bignum-index len-a len-b))
	(cond ((= len-a len-b)
	       (do* ((i (1- len-a) (1- i)))
		    ((zerop i) (%compare-digits a b 0))
		 (declare (fixnum i))
		 (let* ((signum (%compare-digits a b i)))
		   (declare (fixnum signum))
		   (unless (zerop signum)
		     (return signum)))))
	      ((> len-a len-b)
	       (if a-plusp 1 -1))
	      (t (if a-plusp -1 1))))
      (if a-plusp 1 -1))))






;;;; Integer length and logcount


(defun bignum-integer-length (big)
  (the fixnum (- (the fixnum (ash (the fixnum (%bignum-length big)) 5))
		 (the fixnum (%bignum-sign-bits big)))))

; (not (zerop (logand integer1 integer2)

(defun bignum-logtest (num1 num2)
  (let* ((length1 (%bignum-length num1))
         (length2 (%bignum-length num2))
         (n1-minusp (bignum-minusp num1))
         (n2-minusp (bignum-minusp num2)))
    (declare (fixnum length1 length2))
    (if (and n1-minusp n2-minusp) ; both neg, get out quick
      T        
      (or (dotimes (i (min length1 length2))
            (unless (zerop (the fixnum
                             (logand (the fixnum (bignum-ref num1 i))
                                     (the fixnum (bignum-ref num2 i)))))
              (return t)))
          (if (< length1 length2)
            n1-minusp
            (if (< length2 length1)
              n2-minusp))))))

(defun logtest-fix-big (fix big)
  (declare (fixnum fix))
  (unless (zerop fix)
    (if (plusp fix)
      (or
       (not (eql 0 (the fixnum (logand (the fixnum (bignum-ref big 0)) fix))))
       (and (> (%bignum-length big) 1)
            (not (eql 0 (the fixnum (logand (the fixnum (bignum-ref big 1))
                                            (the fixnum (ash fix -32))))))))
      t)))


(defun bignum-logcount (bignum)
  (declare (type bignum-type bignum))
  (let* ((length (%bignum-length bignum))
	 (plusp (bignum-plusp bignum))
	 (result 0))
    (declare (type bignum-index length)
	     (fixnum result))
    (if plusp
      (dotimes (index length result)
	(incf result (the fixnum (%logcount bignum index))))
      (dotimes (index length result)
	(incf result (the fixnum (%logcount-complement bignum index)))))))


;;;; Logical operations.

;;; NOT.
;;;

;;; BIGNUM-LOGICAL-NOT -- Public.
;;;
(defun bignum-logical-not (a)
  (declare (type bignum-type a))
  (let* ((len (%bignum-length a))
	 (res (%allocate-bignum len)))
    (declare (type bignum-index len))
    (dotimes (i len res)
      (bignum-set res i (%lognot (the fixnum (bignum-ref a i)))))))




;;; AND.
;;;

;;; BIGNUM-LOGICAL-AND -- Public.
;;;
(defun bignum-logical-and (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
         (shorter a)
         (longer b)
         (shorter-len len-a)
         (longer-len len-b)
	 (shorter-positive (bignum-plusp a)))
    (declare (type bignum-index len-a len-b shorter-len longer-len))
    (when (< len-b len-a)
      (setq shorter b
            longer a
            shorter-len len-b
            longer-len len-a
            shorter-positive (bignum-plusp b)))
    (let* ((result (%allocate-bignum longer-len)))
      (%bignum-logand shorter-len shorter longer result)
      (unless shorter-positive
        (bignum-replace result longer :start1 shorter-len :start2 shorter-len :end1 longer-len :end2 longer-len))
      (%normalize-bignum-macro result))))


;;;
;;;
;;; bignum-logandc2

(defun bignum-logandc2 (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
	 (a-plusp (bignum-plusp a))
	 (b-plusp (bignum-plusp b)))
    (declare (type bignum-index len-a len-b))
    (cond
     ((< len-a len-b)
      (logandc2-shorter-any a len-a b len-b (if a-plusp (%allocate-bignum len-a) (%allocate-bignum len-b))))
     ((< len-b len-a) ; b shorter 
      (logandc1-shorter-any b len-b a len-a (if b-plusp (%allocate-bignum len-a)(%allocate-bignum len-b))))
     (t (logandc2-shorter-any a len-a b len-b (%allocate-bignum len-a))))))

(defun logandc2-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logand (the fixnum (bignum-ref a i))
                  (the fixnum (%lognot (the fixnum (bignum-ref b i)))))))
  (if (bignum-minusp a)
    (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
      (setf (bignum-ref res i)
            (%lognot (the fixnum (bignum-ref b i))))))
  (%normalize-bignum-macro res))



(defun logandc1-shorter-any (a len-a b len-b res)
  (declare (type bignum-type a b res)
           (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (logand
           (the fixnum (%lognot (the fixnum (bignum-ref a i))))
           (the fixnum (bignum-ref b i)))))
  (when (bignum-plusp a)
    (unless (= len-a len-b)
      (bignum-replace res b :start1 len-a :start2 len-a :end1 len-b :end2 len-b)))
  (%normalize-bignum-macro res))



(defun fix-big-logand (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logand fix big res)))
      (if res
        (progn
          (bignum-replace res big :start1 2 :start2 2 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))


(defun fix-big-logandc2 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (< fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc2 fix big res)))
      (if res
        (progn
          (do ((i 2 (1+ i)))
              ((= i len-b))
            (declare (type bignum-index i))
            (setf (bignum-ref res i)
                  (%lognot (bignum-ref big i))))
          (%normalize-bignum-macro res))
        val))))

(defun fix-big-logandc1 (fix big)
  (let* ((len-b (%bignum-length big))
         (res (if (>= fix 0)(%allocate-bignum len-b))))
    (declare (fixnum fix len-b))        
    (let ((val (fix-digit-logandc1 fix big res)))
      (if res
        (progn  
          (bignum-replace res big :start1 2 :start2 2 :end1 len-b :end2 len-b)
          (%normalize-bignum-macro res))
        val))))


;;; IOR.
;;;

;;; BIGNUM-LOGICAL-IOR -- Public.
;;;
(defun bignum-logical-ior (a b)
  (declare (type bignum-type a b))
  (let* ((len-a (%bignum-length a))
	 (len-b (%bignum-length b))
         (longer-len len-b)
         (shorter-len len-a)
         (shorter a)
         (longer b)
         (shorter-positive (bignum-plusp a)))
    (declare (type bignum-index len-a len-b longer-len shorter-len))
    (when (< len-b len-a)
      (setq shorter b
            longer a
            shorter-len len-b
            longer-len len-a
            shorter-positive (bignum-plusp b)))
    (let* ((result (%allocate-bignum longer-len)))
      (%bignum-logior shorter-len shorter longer result)
      (unless (= shorter-len longer-len)
        (if shorter-positive
          (bignum-replace result longer :start1 shorter-len :start2 shorter-len :end1 longer-len :end2 longer-len)
          (do* ((i shorter-len (1+ i)))
               ((= i longer-len))
            (declare (type bignum-index i))
            (setf (bignum-ref result i) #xffffffff))))
      (%normalize-bignum-macro result))))



;;; XOR.
;;;

;;; BIGNUM-LOGICAL-XOR -- Public.
;;;
(defun bignum-logical-xor (a b)
  (declare (type bignum-type a b))
  (let ((len-a (%bignum-length a))
	(len-b (%bignum-length b)))
    (declare (type bignum-index len-a len-b))
    (if (< len-a len-b)
	(bignum-logical-xor-aux a len-a b len-b (%allocate-bignum len-b))
	(bignum-logical-xor-aux b len-b a len-a (%allocate-bignum len-a)))))

;;; BIGNUM-LOGICAL-XOR-AUX -- Internal.
;;;
;;; This takes the the shorter of two bignums in a and len-a.  Res is len-b
;;; long.  Do the XOR.
;;;
(defun bignum-logical-xor-aux (a len-a b len-b res)
  (declare (type bignum-type a b res)
	   (type bignum-index len-a len-b))
  (dotimes (i len-a)
    (setf (bignum-ref res i)
          (%logxor (the fixnum (bignum-ref a i))
                  (the fixnum (bignum-ref b i)))))
  (unless (= len-a len-b)
    (let ((sign (if (bignum-minusp a) all-ones-digit 0)))
      (declare (fixnum sign))
      (do ((i len-a (1+ i)))
          ((= i len-b))
        (declare (type bignum-index i))
        (setf (bignum-ref res i)
              (%logxor (bignum-ref b i) sign)))))
  (%normalize-bignum-macro res))


;;;; TRUNCATE

;;; Divide X by Y when Y is a single bignum digit. BIGNUM-TRUNCATE
;;; fixes up the quotient and remainder with respect to sign and
;;; normalization.
;;;
;;; We don't have to worry about shifting Y to make its most
;;; significant digit sufficiently large for %FLOOR to return
;;; digit-size quantities for the q-digit and r-digit. If Y is
;;; a single digit bignum, it is already large enough for
;;; %FLOOR. That is, it has some bits on pretty high in the
;;; digit.

(defun bignum-truncate-single-digit (x len-x y)
  (declare (type bignum-index len-x))
  (let ((q (%allocate-bignum len-x))
        (r 0)
        (y (bignum-ref y 0)))
    (declare (type bignum-element-type r y))
    (do ((i (1- len-x) (1- i)))
        ((minusp i))
      (multiple-value-bind (q-digit r-digit)
          (%floor r (bignum-ref x i) y)
        (declare (type bignum-element-type q-digit r-digit))
        (setf (bignum-ref q i) q-digit)
        (setf r r-digit)))
    (let ((rem (%allocate-bignum 1)))
      (setf (bignum-ref rem 0) r)
      (values q rem))))

;;; This returns a guess for the next division step. Y1 is the
;;; highest y digit, and y2 is the second to highest y
;;; digit. The x... variables are the three highest x digits
;;; for the next division step.
;;;
;;; From Knuth, our guess is either all ones or x-i and x-i-1
;;; divided by y1, depending on whether x-i and y1 are the
;;; same. We test this guess by determining whether guess*y2
;;; is greater than the three high digits of x minus guess*y1
;;; shifted left one digit:
;;;    ------------------------------
;;;   |    x-i    |   x-i-1  | x-i-2 |
;;;    ------------------------------
;;;    ------------------------------
;;; - | g*y1 high | g*y1 low |   0   |
;;;    ------------------------------
;;;		...		  <   guess*y2     ???	 
;;; If guess*y2 is greater, then we decrement our guess by one
;;; and try again.  This returns a guess that is either
;;; correct or one too large.
(defun bignum-truncate-guess (y1 y2 x-i x-i-1 x-i-2)
  (declare (type bignum-element-type y1 y2 x-i x-i-1 x-i-2))
  (let ((guess (if (= x-i y1)
                 all-ones-digit
                 (%floor x-i x-i-1 y1))))
    (declare (type bignum-element-type guess))
    (loop
      (multiple-value-bind (high-guess*y1 low-guess*y1)
          (%multiply guess y1)
        (declare (type bignum-element-type low-guess*y1
                       high-guess*y1))
        (multiple-value-bind (high-guess*y2 low-guess*y2)
            (%multiply guess y2)
          (declare (type bignum-element-type high-guess*y2
                         low-guess*y2))
          (multiple-value-bind (middle-digit borrow)
              (%subtract-with-borrow x-i-1 low-guess*y1 1)
            (declare (type bignum-element-type middle-digit)
                     (fixnum borrow))
            ;; Supplying borrow of 1 means there was no
            ;; borrow, and we know x-i-2 minus 0 requires
            ;; no borrow.
            (let ((high-digit (%subtract-with-borrow x-i
                                                     high-guess*y1
                                                     borrow)))
              (declare (type bignum-element-type high-digit))
              (if (and (= high-digit 0)
                       (or (> high-guess*y2
                              middle-digit)
                           (and (= middle-digit
                                   high-guess*y2)
                                (> low-guess*y2
                                   x-i-2))))
                (setf guess (%subtract-with-borrow guess 1 1))
                (return guess)))))))))


;;; This returns the amount to shift y to place a one in the
;;; second highest bit. Y must be positive. If the last digit
;;; of y is zero, then y has a one in the previous digit's
;;; sign bit, so we know it will take one less than digit-size
;;; to get a one where we want. Otherwise, we count how many
;;; right shifts it takes to get zero; subtracting this value
;;; from digit-size tells us how many high zeros there are
;;; which is one more than the shift amount sought.
;;;
;;; Note: This is exactly the same as one less than the
;;; integer-length of the last digit subtracted from the
;;; digit-size.
;;;
;;; We shift y to make it sufficiently large that doing the
;;; 2*digit-size by digit-size %FLOOR calls ensures the quotient and
;;; remainder fit in digit-size.
(defun shift-y-for-truncate (y)
  (the fixnum (1- (the fixnum (%bignum-sign-bits y)))))

;;; Stores two bignums into the truncation bignum buffers,
;;; shifting them on the way in. This assumes x and y are
;;; positive and at least two in length, and it assumes
;;; truncate-x and truncate-y are one digit longer than x and
;;; y.
(defun shift-and-store-truncate-buffers (truncate-x truncate-y x len-x y len-y shift)
  (declare (type bignum-index len-x len-y)
           (type (integer 0 (#.digit-size)) shift))
  (cond ((zerop shift)
         (bignum-replace truncate-x x :end1 len-x)
         (bignum-replace truncate-y y :end1 len-y))
        (t
         (bignum-ashift-left-unaligned x 0 shift (1+ len-x)
                                       truncate-x)
         (bignum-ashift-left-unaligned y 0 shift (1+ len-y)
                                       truncate-y))))

;;; Divide TRUNCATE-X by TRUNCATE-Y, returning the quotient
;;; and destructively modifying TRUNCATE-X so that it holds
;;; the remainder.
;;;
;;; LEN-X and LEN-Y tell us how much of the buffers we care about.
;;;
;;; TRUNCATE-X definitely has at least three digits, and it has one
;;; more than TRUNCATE-Y. This keeps i, i-1, i-2, and low-x-digit
;;; happy. Thanks to SHIFT-AND-STORE-TRUNCATE-BUFFERS.

(defun do-truncate (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
         ;; Add one for extra sign digit in case high bit is on.
         (q (%allocate-bignum (1+ len-q)))
         (k (1- len-q))
         (y1 (bignum-ref truncate-y (1- len-y)))
         (y2 (bignum-ref truncate-y (- len-y 2)))
         (i (1- len-x))
         (i-1 (1- i))
         (i-2 (1- i-1))
         (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i i-1 i-2 low-x-digit)
             (type bignum-element-type y1 y2))
    (loop
      (setf (bignum-ref q k)
            (try-bignum-truncate-guess
             truncate-x truncate-y
             ;; This modifies TRUNCATE-X. Must access
             ;; elements each pass.
             (bignum-truncate-guess y1 y2
                                    (bignum-ref truncate-x i)
                                    (bignum-ref truncate-x i-1)
                                    (bignum-ref truncate-x i-2))
             len-y low-x-digit))
      (cond ((zerop k) (return))
            (t (decf k)
               (decf low-x-digit)
               (shiftf i i-1 i-2 (1- i-2)))))
    q))

#+notyet
(defun do-truncate-no-quo (truncate-x truncate-y len-x len-y)
  (declare (type bignum-index len-x len-y))
  (let* ((len-q (- len-x len-y))
	 (k (1- len-q))
	 (i (1- len-x))
	 (low-x-digit (- i len-y)))
    (declare (type bignum-index len-q k i  low-x-digit))
    (loop
      (let* ((guess (bignum-truncate-guess truncate-x i truncate-y (the fixnum (1- len-y)))                                 
        (try-bignum-truncate-guess guess len-y low-x-digit)
        (cond ((zerop k) (return))
              (t (decf k)
                 (decf low-x-digit)
                 (setq i (1- i))))))
    nil))))

;;; This takes a digit guess, multiplies it by TRUNCATE-Y for a
;;; result one greater in length than LEN-Y, and subtracts this result
;;; from TRUNCATE-X. LOW-X-DIGIT is the first digit of X to start
;;; the subtraction, and we know X is long enough to subtract a LEN-Y
;;; plus one length bignum from it. Next we check the result of the
;;; subtraction, and if the high digit in X became negative, then our
;;; guess was one too big. In this case, return one less than GUESS
;;; passed in, and add one value of Y back into X to account for
;;; subtracting one too many. Knuth shows that the guess is wrong on
;;; the order of 3/b, where b is the base (2 to the digit-size power)
;;; -- pretty rarely.

(defun try-bignum-truncate-guess (truncate-x truncate-y guess len-y low-x-digit)
  (declare (type bignum-index low-x-digit len-y)
           (type bignum-element-type guess))
  (let ((carry-digit 0)
        (borrow 1)
        (i low-x-digit))
    (declare (type bignum-element-type carry-digit)
             (type bignum-index i)
             (fixnum borrow))
    ;; Multiply guess and divisor, subtracting from dividend
    ;; simultaneously.
    (dotimes (j len-y)
      (multiple-value-bind (high-digit low-digit)
          (%multiply-and-add3 guess
                              (bignum-ref truncate-y j)
                              carry-digit)
        (declare (type bignum-element-type high-digit low-digit))
        (setf carry-digit high-digit)
        (multiple-value-bind (x temp-borrow)
            (%subtract-with-borrow (bignum-ref truncate-x i)
                                   low-digit
                                   borrow)
          (declare (type bignum-element-type x)
                   (fixnum temp-borrow))
          (setf (bignum-ref truncate-x i) x)
          (setf borrow temp-borrow)))
      (incf i))
    (setf (bignum-ref truncate-x i)
          (%subtract-with-borrow (bignum-ref truncate-x i)
                                 carry-digit borrow))
    ;; See whether guess is off by one, adding one
    ;; Y back in if necessary.
    (cond ((%digit-0-or-plusp (bignum-ref truncate-x i))
           guess)
          (t
           ;; If subtraction has negative result, add one
           ;; divisor value back in. The guess was one too
           ;; large in magnitude.
           (let ((i low-x-digit)
                 (carry 0))
             (dotimes (j len-y)
               (multiple-value-bind (v k)
                   (%add-with-carry (bignum-ref truncate-y j)
                                    (bignum-ref truncate-x i)
                                    carry)
                 (declare (type bignum-element-type v))
                 (setf (bignum-ref truncate-x i) v)
                 (setf carry k))
               (incf i))
             (setf (bignum-ref truncate-x i)
                   (%add-with-carry (bignum-ref truncate-x i)
                                    0 carry)))
           (%subtract-with-borrow guess 1 1)))))

;;; Someone (from the original CMUCL or SPICE Lisp project, perhaps)
;;; is the "I" who implemented the original version of this.

;;; This is the original sketch of the algorithm from which I implemented this
;;; TRUNCATE, assuming both operands are bignums. I should modify this to work
;;; with the documentation on my functions, as a general introduction. I've
;;; left this here just in case someone needs it in the future. Don't look at
;;; this unless reading the functions' comments leaves you at a loss. Remember
;;; this comes from Knuth, so the book might give you the right general
;;; overview.
;;;
;;; (truncate x y):
;;;
;;; If X's magnitude is less than Y's, then result is 0 with remainder X.
;;;
;;; Make x and y positive, copying x if it is already positive.
;;;
;;; Shift y left until there's a 1 in the 30'th bit (most significant, non-sign
;;;       digit)
;;;    Just do most sig digit to determine how much to shift whole number.
;;; Shift x this much too.
;;; Remember this initial shift count.
;;;
;;; Allocate q to be len-x minus len-y quantity plus 1.
;;;
;;; i = last digit of x.
;;; k = last digit of q.
;;;
;;; LOOP
;;;
;;; j = last digit of y.
;;;
;;; compute guess.
;;; if x[i] = y[j] then g = (1- (ash 1 digit-size))
;;; else g = x[i]x[i-1]/y[j].
;;;
;;; check guess.
;;; %UNSIGNED-MULTIPLY returns b and c defined below.
;;;    a = x[i-1] - (logand (* g y[j]) #xFFFFFFFF).
;;;       Use %UNSIGNED-MULTIPLY taking low-order result.
;;;    b = (logand (ash (* g y[j-1]) (- digit-size)) (1- (ash 1 digit-size))).
;;;    c = (logand (* g y[j-1]) (1- (ash 1 digit-size))).
;;; if a < b, okay.
;;; if a > b, guess is too high
;;;    g = g - 1; go back to "check guess".
;;; if a = b and c > x[i-2], guess is too high
;;;    g = g - 1; go back to "check guess".
;;; GUESS IS 32-BIT NUMBER, SO USE THING TO KEEP IN SPECIAL REGISTER
;;; SAME FOR A, B, AND C.
;;;
;;; Subtract g * y from x[i - len-y+1]..x[i]. See paper for doing this in step.
;;; If x[i] < 0, guess is screwed up.
;;;    negative g, then add 1
;;;    zero or positive g, then subtract 1
;;; AND add y back into x[len-y+1..i].
;;;
;;; q[k] = g.
;;; i = i - 1.
;;; k = k - 1.
;;;
;;; If k>=0, goto LOOP.
;;;
;;; Now quotient is good, but remainder is not.
;;; Shift x right by saved initial left shifting count.
;;;
;;; Check quotient and remainder signs.
;;; x pos y pos --> q pos r pos
;;; x pos y neg --> q neg r pos
;;; x neg y pos --> q neg r neg
;;; x neg y neg --> q pos r neg
;;;
;;; Normalize quotient and remainder. Cons result if necessary.


(defun bignum-truncate (x y &optional no-rem)
  (declare (type bignum-type x y))
  (DECLARE (IGNORE NO-REM))
  ;; Divide X by Y returning the quotient and remainder. In the
  ;; general case, we shift Y to set up for the algorithm, and we
  ;; use two buffers to save consing intermediate values. X gets
  ;; destructively modified to become the remainder, and we have
  ;; to shift it to account for the initial Y shift. After we
  ;; multiple bind q and r, we first fix up the signs and then
  ;; return the normalized results.
  (let* ((x-plusp (%bignum-0-or-plusp x (%bignum-length x)))
         (y-plusp (%bignum-0-or-plusp y (%bignum-length y)))
         (x (if x-plusp x (negate-bignum x nil)))
         (y (if y-plusp y (negate-bignum y nil)))
         (len-x (%bignum-length x))
         (len-y (%bignum-length y)))
    (multiple-value-bind (q r)
        (cond ((< len-y 2)
               (bignum-truncate-single-digit x len-x y))
              ((plusp (bignum-compare y x))
               (let ((res (%allocate-bignum len-x)))
                 (dotimes (i len-x)
                   (setf (bignum-ref res i) (bignum-ref x i)))
                 (values 0 res)))
              (t
               (let ((len-x+1 (1+ len-x)))
                 (with-bignum-buffers ((truncate-x len-x+1)
                                       (truncate-y (1+ len-y)))
                   (let ((y-shift (shift-y-for-truncate y)))
                     (shift-and-store-truncate-buffers truncate-x
                                                       truncate-y
                                                       x len-x
                                                       y len-y
                                                       y-shift)
                     (values
                      (do-truncate truncate-x
                        truncate-y
                        len-x+1
                        len-y)
                      ;; Now DO-TRUNCATE has executed, we just
                      ;; tidy up the remainder (in TRUNCATE-X)
                      ;; and return it.
                      (cond
                        ((zerop y-shift)
                         (let ((res (%allocate-bignum len-y)))
                           (declare (type bignum-type res))
                           (bignum-replace res truncate-x :end2 len-y)
                           (%normalize-bignum-macro res)))
                        (t
                         (shift-right-unaligned
                          truncate-x 0 y-shift len-y
                          ((= j res-len-1)
                           (setf (bignum-ref res j)
                                 (%ashr (bignum-ref truncate-x i)
                                        y-shift))
                           (%normalize-bignum-macro res))
                          res)))))))))
      (let ((quotient (cond ((eq x-plusp y-plusp) q)
                            ((typep q 'fixnum) (the fixnum (- q)))
                            (t (negate-bignum-in-place q))))
            (rem (cond (x-plusp r)
                       ((typep r 'fixnum) (the fixnum (- r)))
                       (t (negate-bignum-in-place r)))))
        (values (if (typep quotient 'fixnum)
                  quotient
                  (%normalize-bignum-macro quotient))
                (if (typep rem 'fixnum)
                  rem
                  (%normalize-bignum-macro rem)))))))

(defun bignum-truncate-by-fixnum (bignum fixnum)
  (with-small-bignum-buffers ((y fixnum))
    (bignum-truncate bignum y)))

(defun bignum-truncate-by-fixnum-no-quo (bignum fixnum)
  (nth-value 1 (bignum-truncate-by-fixnum bignum fixnum)))

;;; This may do unnecessary computation in some cases.
(defun bignum-rem (x y)
  (nth-value 1 (bignum-truncate x y)))



;;;; General utilities.

(defun %zero-trailing-sign-digits (bignum len)
  (declare (fixnum len))
  (unless (<= len 1)
    (do ((next (bignum-ref bignum (the fixnum (- len 2)))
               (bignum-ref bignum (the fixnum (- len 2))))
         (sign (bignum-ref bignum (the fixnum (- len 1)))
               next))
        ((not (zerop (the fixnum (%logxor sign (%ashr next 31))))))
      (decf len)
      (setf (bignum-ref bignum len) 0)
      ;; Return, unless we've already done so (having found significant
      ;; digits earlier.)
      (when (= len 1)
        (return))))
  len)


(defun %normalize-bignum-2 (return-fixnum-p bignum)
  (let* ((len (%bignum-length bignum))
         (newlen (%zero-trailing-sign-digits bignum len)))
    (declare (fixnum len newlen))
    (unless (= len newlen)
      (%set-bignum-length newlen bignum))
    (or (and return-fixnum-p
             (%maybe-fixnum-from-one-or-two-digit-bignum bignum))
        bignum)))
           
    
;;; %MOSTLY-NORMALIZE-BIGNUM -- Internal.
;;;
;;; This drops the last digit if it is unnecessary sign information.  It
;;; repeats this as needed, possibly ending with a fixnum magnitude but never
;;; returning a fixnum.
;;;

(defun %mostly-normalize-bignum (res &optional len)
  (declare (ignore len))
  (%normalize-bignum-2 nil res))





(defun load-byte (size position integer)
  (if (and (bignump integer)
           (<= size (- 63 target::fixnumshift))
           (fixnump position))
    (%ldb-fixnum-from-bignum integer size position)
    (let ((mask (byte-mask size)))
      (if (and (fixnump mask) (fixnump integer)(fixnump position))
        (%ilogand mask (%iasr position integer))
        (logand mask (ash integer (- position)))))))


#+safe-but-slow
;;; This is basically the same algorithm as the "destructive"
;;; version below; while it may be more readable, it's often
;;; slower and conses too much to be at all viable.
(defun %bignum-bignum-gcd (u v)
  (setq u (abs u) v (abs v))
  (do* ((g 1 (ash g 1)))
       ((or (oddp u) (oddp v))
	(do* ()
	     ((zerop u) (* g v))
	  (cond ((evenp u) (setq u (ash u -1)))
		((evenp v) (setq v (ash v -1)))
		(t (let* ((temp (ash (abs (- u v)) -1)))
		     (if (< u v)
		       (setq v temp)
		       (setq u temp)))))))
    (setq u (ash u -1) v (ash v -1))))




#-safe-but-slow
(progn
(defun %positive-bignum-bignum-gcd (u0 v0)
  (let* ((u-len (%bignum-length u0))
	 (v-len (%bignum-length v0)))
    (declare (fixnum u-len v-len))
    (if (or (< u-len v-len)
	    (and (= u-len v-len)
		 (< (bignum-compare u0 v0) 0)))
      (psetq u0 v0 v0 u0 u-len v-len v-len u-len))
    (with-bignum-buffers ((u u-len)
			  (u2 u-len)
			  (v v-len)
			  (v2 v-len))
      (bignum-replace u u0)
      (bignum-replace v v0)
      (let* ((u-trailing-0-bits (%bignum-count-trailing-zero-bits u))
	     (u-trailing-0-digits (ash u-trailing-0-bits -5))
	     (v-trailing-0-bits (%bignum-count-trailing-zero-bits v))
	     (v-trailing-0-digits (ash v-trailing-0-bits -5)))
	(declare (fixnum u-trailing-0-bits v-trailing-0-bits))
	(unless (zerop u-trailing-0-bits)
	  (bignum-shift-right-loop-1
	   (logand u-trailing-0-bits 31)
	   u2
	   u
	   (the fixnum (1- (the fixnum (- u-len u-trailing-0-digits ))))
	   u-trailing-0-digits)
	  (rotatef u u2)
	  (%mostly-normalize-bignum-macro u)
	  (setq u-len (%bignum-length u)))
	(unless (zerop v-trailing-0-bits)
	  (bignum-shift-right-loop-1
	   (logand v-trailing-0-bits 31)
	   v2
	   v
	   (the fixnum (1- (the fixnum (- v-len v-trailing-0-digits))))
	   v-trailing-0-digits)
	  (rotatef v v2)
	  (%mostly-normalize-bignum-macro v)
	  (setq v-len (%bignum-length v)))
	(let* ((shift (min u-trailing-0-bits
			   v-trailing-0-bits)))
	  (loop
	      (let* ((fix-u (and (<= u-len 2)
                                 (%maybe-fixnum-from-one-or-two-digit-bignum u)))
		     (fix-v (and (<= v-len 2)
                                 (%maybe-fixnum-from-one-or-two-digit-bignum v))))
		(if fix-v
		  (if fix-u
		    (return (ash (%fixnum-gcd fix-u fix-v) shift))
		    (return (ash (bignum-fixnum-gcd u fix-v) shift)))
		  (if fix-u
		    (return (ash (bignum-fixnum-gcd v fix-u) shift)))))
	      (let* ((signum (if (> u-len v-len)
			       1
			       (if (< u-len v-len)
				 -1
				 (bignum-compare u v)))))
		(declare (fixnum signum))
		(case signum
		  (0			; (= u v)
		   (if (zerop shift)
		     (let* ((copy (%allocate-bignum u-len)))
		       (bignum-replace copy u)
		       (return copy))
		     (return (ash u shift))))
		  (1			; (> u v)
		   (bignum-subtract-loop u u-len v v-len u)
		   (%mostly-normalize-bignum-macro u)
		   (setq u-len (%bignum-length u))
		   (setq u-trailing-0-bits
			 (%bignum-count-trailing-zero-bits u)
			 u-trailing-0-digits
			 (ash u-trailing-0-bits -5))
                   (unless (zerop u-trailing-0-bits)
                     (%init-misc 0 u2)
                     (bignum-shift-right-loop-1
                      (logand u-trailing-0-bits 31)
                      u2
                      u
                      (the fixnum (1- (the fixnum (- u-len
                                                     u-trailing-0-digits))))
                      u-trailing-0-digits)
                     (rotatef u u2)
                     (%mostly-normalize-bignum-macro u)
                     (setq u-len (%bignum-length u))))
		  (t			; (> v u)
		   (bignum-subtract-loop v v-len u u-len v)
		   (%mostly-normalize-bignum-macro v)
		   (setq v-len (%bignum-length v))
		   (setq v-trailing-0-bits
			 (%bignum-count-trailing-zero-bits v)
			 v-trailing-0-digits
			 (ash v-trailing-0-bits -5))
                   (unless (zerop v-trailing-0-bits)
                     (%init-misc 0 v2)
                     (bignum-shift-right-loop-1
                      (logand v-trailing-0-bits 31)
                      v2
                      v
                      (the fixnum (1- (the fixnum (- v-len v-trailing-0-digits))))
                      v-trailing-0-digits)
                     (rotatef v v2)
                     (%mostly-normalize-bignum-macro v)
                     (setq v-len (%bignum-length v))))))))))))

(defun %bignum-bignum-gcd (u v)
  (with-negated-bignum-buffers u v %positive-bignum-bignum-gcd))
)


(defun bignum-shift-right-loop-1 (nbits result source len idx)
  (declare (type bignum-type result source)
           (type (mod 32) nbits)
           (type bignum-index idx len))
  (let* ((rbits (- 32 nbits)))
    (declare (type (mod 33) rbits))
    (dotimes (j len)
      (let* ((x (bignum-ref source idx)))
        (declare (type bignum-element-type x))
        (setq x (%ilsr nbits x))
        (incf idx)
        (let* ((y (bignum-ref source idx)))
          (declare (type bignum-element-type y))
          (setq y (%ashl y rbits))
          (setf (bignum-ref result j)
                (%logior x y)))))
    (setf (bignum-ref result len)
          (%ilsr nbits (bignum-ref source idx)))
    idx))
    

(defun %logcount (bignum idx)
  (%ilogcount (bignum-ref bignum idx)))

(defun %logcount-complement (bignum idx)
  (- 32 (the fixnum (%ilogcount (bignum-ref bignum idx)))))

(defun %bignum-evenp (bignum)
  (not (logbitp 0 (the fixnum (bignum-ref bignum 0)))))

(defun %bignum-oddp (bignum)
  (logbitp 0 (the fixnum (bignum-ref bignum 0))))

(defun %ldb-fixnum-from-bignum (bignum size position)
  (declare (fixnum size position))
  (let* ((low-idx (ash position -5))
         (low-bit (logand position 31))
         (hi-bit (+ low-bit size))
         (len (%bignum-length bignum))
         (minusp (bignum-minusp bignum)))
    (declare (fixnum size position low-bit hi-bit low-idx len))
    (if (>= low-idx len)
      (if minusp (1- (ash 1 size)) 0)
      (flet ((ldb32 (digit size pos)
               (declare (fixnum digit size pos))
               (logand (the fixnum (1- (ash 1 size)))
                       (the fixnum (ash digit (the fixnum (- pos)))))))
        (let* ((low-digit (bignum-ref bignum low-idx))
               (chunk-lo (ldb32 low-digit (min size (%i- 32 low-bit)) low-bit)))
          (if (< hi-bit 32) 
            chunk-lo
            (let* ((have (- 32 low-bit))
                   (remain (- size have)))
              (declare (fixnum have remain))
              (setq low-idx (1+ low-idx))
              (when (> remain 32)
                (setq chunk-lo
                      (logior (ash (if (< low-idx len)
                                     (bignum-ref bignum low-idx)
                                     (if minusp all-ones-digit 0))
                                   have)
                              chunk-lo))
                (incf have 32)
                (decf remain 32)
                (incf low-idx))
              (let* ((high-digit
                      (if (>= low-idx len)
                        (if minusp all-ones-digit 0)
                        (bignum-ref bignum low-idx)))
                     (chunk-hi (ldb32 high-digit remain 0)))
                (%ilogior (ash chunk-hi have) chunk-lo)))))))))



(defun bignum-negate-loop-really (big len res)
  (declare (fixnum len))
  (let* ((carry 1))
    (dotimes (i len carry)
      (multiple-value-bind (result-digit carry-out)
          (%add-with-carry (%lognot (bignum-ref big i)) 0 carry)
        (setf (bignum-ref res i) result-digit
              carry carry-out)))))

(defun bignum-negate-to-pointer (big len res)
  (declare (fixnum len))
  (let* ((carry 1))
    (do* ((i 0 (1+ i))
          (j 0 (+ j 4)))
         ((= i len) carry)
      (declare (fixnum i))
      (multiple-value-bind (result-digit carry-out)
          (%add-with-carry (%lognot (bignum-ref big i)) 0 carry)
        (setf (%get-unsigned-long res j) result-digit
              carry carry-out)))))
  

(defun %bignum-count-trailing-zero-bits (bignum)
  (let* ((count 0))
    (dotimes (i (%bignum-length bignum))
      (let* ((digit (bignum-ref bignum i)))
        (declare (type bignum-element-type digit))
        (if (zerop digit)
          (incf count 32)
          (progn
            (dotimes (bit 32)
              (declare (type (mod 32) bit))
              (if (logbitp bit digit)
                (return)
                (incf count)))
            (return)))))
    count))
                  

(defun one-bignum-factor-of-two (a)  
  (declare (type bignum-type a))
  (let ((len (%bignum-length a)))
    (declare (fixnum len))
    (dotimes (i len)
      (let* ((x (bignum-ref a i)))
        (declare (fixnum x))
        (unless (zerop x)
          (return (+ (ash i 5)
                     (dotimes (j 32)
                       (if (logbitp j x)
                         (return j))))))))))


(defun %bignum-random (number state)
  (let* ((ndigits (%bignum-length number))
         (sign-index (1- ndigits)))
    (declare (fixnum ndigits sign-index))
    (with-bignum-buffers ((bignum ndigits))
      (dotimes (i sign-index)
        (setf (bignum-ref bignum i) (random (expt 2 digit-size) state)))
      (setf (bignum-ref bignum sign-index)
            (logand #x7fffffff (the (unsigned-byte 32)
                                 (random (expt 2 (1- digit-size)) state))))
      (let* ((result (mod (%normalize-bignum-macro bignum) number)))
        (if (eq result bignum)
          (copy-bignum bignum)
          result)))))



(defun logbitp (index integer)
  "Predicate returns T if bit index of integer is a 1."
  (number-case index
    (fixnum
     (if (minusp (the fixnum index))(report-bad-arg index '(integer 0))))
    (bignum
     ;; assuming bignum cant have more than most-positive-fixnum bits
     ;; (2 expt 24 longs)
     (if (bignum-minusp index)(report-bad-arg index '(integer 0)))
     ;; should error if integer isn't
     (return-from logbitp (minusp (require-type integer 'integer)))))
  (number-case integer
    (fixnum
     (if (%i< index (- target::nbits-in-word target::fixnumshift))
       (%ilogbitp index integer)
       (minusp (the fixnum integer))))
    (bignum
     (let ((bidx (%iasr 5 index))
           (bbit (%ilogand index 31)))
       (declare (fixnum bidx bbit))
       (if (>= bidx (%bignum-length integer))
         (bignum-minusp integer)
         (logbitp bbit (bignum-ref integer bidx)))))))

) ; #+64-bit-target
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;3
;;;; END FILE ./reference/ccl/level-0/l0-bignum64.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;4n









