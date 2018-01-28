;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           number routines

(in-package "SYSTEM")

(defconstant imag-one #C(0.0 1.0))

(defun isqrt (i)
  "Args: (integer)
Returns the integer square root of INTEGER."
       (unless (and (integerp i) (>= i 0))
               (error "~S is not a non-negative integer." i))
       (if (zerop i)
           0
           (let ((n (integer-length i)))
                (do ((x (ash 1 (ceiling n 2)))
                     (y))
                    (nil)
                    (setq y (floor i x))
                    (when (<= x y)
                          (return x))
                    (setq x (floor (+ x y) 2))))))

(defun abs (z)
  "Args: (number)
Returns the absolute value of NUMBER."
  (if (complexp z)
      ;; Compute sqrt(x*x + y*y) carefully to prevent overflow.
      ;; Assume |x| >= |y|. Then sqrt(x*x + y*y) = |x|*sqrt(1 +(y/x)^2).
      (let* ((x (abs (realpart z)))
	     (y (abs (imagpart z))))
	;; Swap x and y so that |x| >= |y|.
	(if (< x y)
	    (rotatef x y))
	(if (zerop x)
	    x
	    (let ((r (/ y x)))
	      (* x (sqrt (+ 1 (* r r)))))))
      (if (minusp z)
	  (- z)
	  z)))

(defun phase (x)
  "Args: (number)
Returns the angle part (in radians) of the polar representation of NUMBER.
Returns zero for non-complex numbers."
       (atan (imagpart x) (realpart x)))

(defun signum (x)
  "Args: (number)
Returns a number that represents the sign of NUMBER.  Returns NUMBER If it is
zero.  Otherwise, returns the value of (/ NUMBER (ABS NUMBER))"
  (if (zerop x) x (/ x (abs x))))

(defun cis (x)
  "Args: (radians)
Returns a complex number whose realpart and imagpart are the values of (COS
RADIANS) and (SIN RADIANS) respectively."
  (exp (* imag-one x)))

(defun asin (x)
  "Args: (number)
Returns the arc sine of NUMBER."
  ;; (* #C(0.0 -1.0) (log (+ (* imag-one x) (sqrt (- 1.0 (* x x))))))
  (let ((c (log (+ (* imag-one x)
		   (sqrt (- 1.0 (* x x)))))))
    (if (and (complexp c) (zerop (realpart c)))
	(imagpart c)
	(* #C(0.0 -1.0) c))))

(defun acos (x)
  "Args: (number)
Returns the arc cosine of NUMBER."
  ;; (* #C(0.0 -1.0) (log (+ x (* imag-one (sqrt (- 1.0 (* x x)))))))
  (let ((c (log (+ x (* imag-one
			(sqrt (- 1.0 (* x x))))))))
    (if (and (complexp c) (zerop (realpart c)))
	(imagpart c)
	(* #C(0.0 -1.0) c))))

;;; (defun sinh (x) (/ (- (exp x) (exp (- x))) 2.0))
;;; version by Raymond Toy <toy@rtp.ericsson.se>
#+nil
(defun sinh (z)
  (if (complexp z)
      ;; For complex Z, compute the real and imaginary parts
      ;; separately to get better precision.
      (let* ((x (realpart z))
	     (y (imagpart z)))
	(complex (* (sinh x) (cos y))
		 (* (cosh x) (sin y))))
      (let ((limit #.(expt (* double-float-epsilon 45/2) 1/5)))
	(if (< (- limit) z limit)
	    ;; For this region, write use the fact that sinh z =
	    ;; z*exp(z)*[(1 - exp(-2z))/(2z)].  Then use the first
	    ;; 4 terms in the Taylor series expansion of
	    ;; (1-exp(-2z))/2/z.  series expansion of (1 -
	    ;; exp(2*x)).  This is needed because there is severe
	    ;; roundoff error calculating (1 - exp(-2z)) for z near
	    ;; 0.
	    (* z (exp z)
	       (- 1 (* z
		       (- 1 (* z
			       (- 2/3 (* z
					 (- 1/3 (* 2/15 z)))))))))
	    (let ((e (exp z)))
	      (/ (- e (/ e)) 2.0))))))

;;; (defun cosh (x) (/ (+ (exp x) (exp (- x))) 2.0))
;;; version by Raymond Toy <toy@rtp.ericsson.se>
#+nil
(defun cosh (z)
  (if (complexp z)
      ;; For complex Z, compute the real and imaginary parts
      ;; separately to get better precision.
      (let* ((x (realpart z))
	     (y (imagpart z)))
	(complex (* (cosh x) (cos y))
		 (* (sinh x) (sin y))))
      ;; For real Z, there's no chance of round-off error, so
      ;; direct evaluation is ok.
      (let ((e (exp z)))
	(/ (+ e (/ e)) 2.0))))

#+nil
(defun tanh (x) (/ (sinh x) (cosh x)))

(defun asinh (x)
  "Args: (number)
Returns the hyperbolic arc sine of NUMBER."
  (log (+ x (sqrt (+ 1.0 (* x x))))))

(defun acosh (x)
  "Args: (number)
Returns the hyperbolic arc cosine of NUMBER."
  ;; CLtL1: (log (+ x (* (1+ x) (sqrt (/ (1- x) (1+ x))))))
  (* 2 (log (+ (sqrt (/ (1+ x) 2)) (sqrt (/ (1- x) 2))))))

(defun atanh (x)
  "Args: (number)
Returns the hyperbolic arc tangent of NUMBER."
  (/ (- (log (1+ x)) (log (- 1 x))) 2))	; CLtL2

(defun rational (x)
  "Args: (real)
Converts REAL into rational accurately and returns the result."
  (etypecase x
    (FLOAT	  
      (multiple-value-bind (i e s) (integer-decode-float x)
			   (if (>= s 0)
			       (* i (expt (float-radix x) e))
			     (- (* i (expt (float-radix x) e))))))
    (RATIONAL x)))

(defun rationalize (x)
  "Args: (real)
Converts REAL into rational approximately and returns the result."
  (etypecase x
    (FLOAT	  
      (multiple-value-bind (i e s) (integer-decode-float x)
			   (if (>= s 0)
			       (* i (expt (float-radix x) e))
			     (- (* i (expt (float-radix x) e))))))
    (RATIONAL x)))

(defun ffloor (x &optional (y 1.0s0))
  "Args: (number &optional (divisor 1))
Same as FLOOR, but returns a float as the first value."
       (multiple-value-bind (i r) (floor (float x) (float y))
        (values (float i r) r)))

(defun fceiling (x &optional (y 1.0s0))
  "Args: (number &optional (divisor 1))
Same as CEILING, but returns a float as the first value."
       (multiple-value-bind (i r) (ceiling (float x) (float y))
        (values (float i r) r)))

(defun ftruncate (x &optional (y 1.0s0))
  "Args: (number &optional (divisor 1))
Same as TRUNCATE, but returns a float as the first value."
       (multiple-value-bind (i r) (truncate (float x) (float y))
        (values (float i r) r)))

(defun fround (x &optional (y 1.0s0))
  "Args: (number &optional (divisor 1))
Same as ROUND, but returns a float as the first value."
       (multiple-value-bind (i r) (round (float x) (float y))
        (values (float i r) r)))

(defun logtest (x y)
  "Args: (integer1 integer2)
Equivalent to (NOT (ZEROP (LOGAND INTEGER1 INTEGER2)))."
  (not (zerop (logand x y))))


(defun byte (size position)
  "Args: (size position)
Returns a byte specifier of integers.  The value specifies the SIZE-bits byte
starting the least-significant-bit but POSITION bits of integers.  In ECL, a
byte specifier is represented by a dotted pair (SIZE . POSITION)."
  (cons size position))

(defun byte-size (bytespec)
  "Args: (byte)
Returns the size part (in ECL, the car part) of the byte specifier BYTE."
  (car bytespec))

(defun byte-position (bytespec)
  "Args: (byte)
Returns the position part (in ECL, the cdr part) of the byte specifier BYTE."
  (cdr bytespec))

(defun ldb (bytespec integer)
  "Args: (bytespec integer)
Extracts a byte from INTEGER at the specified byte position, right-justifies
the byte, and returns the result as an integer."
  (logandc2 (ash integer (- (byte-position bytespec)))
            (- (ash 1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  "Args: (bytespec integer)
Returns T if at least one bit of the specified byte is 1; NIL otherwise."
  (not (zerop (ldb bytespec integer))))

(defun mask-field (bytespec integer)
  "Args: (bytespec integer)
Extracts the specified byte from INTEGER and returns the result as an integer."
  (ash (ldb bytespec integer) (byte-position bytespec)))

(defun dpb (newbyte bytespec integer)
  "Args: (newbyte bytespec integer)
Replaces the specified byte of INTEGER with NEWBYTE (an integer) and returns
the result."
  (logxor integer
          (mask-field bytespec integer)
          (ash (logandc2 newbyte
                         (- (ash 1 (byte-size bytespec))))
               (byte-position bytespec))))

(defun deposit-field (newbyte bytespec integer)
  "Args: (integer1 bytespec integer2)
Returns an integer represented by the bit sequence obtained by replacing the
specified bits of INTEGER2 with the specified bits of INTEGER1."
  (dpb (ash newbyte (- (byte-position bytespec))) bytespec integer))
