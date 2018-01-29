;;;
;;;     CLX -- ecldep.cl
;;;

;;;;  Copyright (c) 1994, Giuseppe Attardi.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2
;;;;    of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package :xlib)

;;; from file depdefs.l

(deftype buffer-bytes () 'string)

(defCbody little-endian-p () boolean
	  "Cnil;
    { int whichbyte = 1;
      if (*(char *) &whichbyte) x=Ct;}")

#|
(definline index+ (fixnum fixnum) fixnum	"(#0) + (#1)")
(definline index-logand (fixnum fixnum) fixnum	"(#0) & (#1)")
(definline index-logior (fixnum fixnum) fixnum	"(#0) | (#1)")
(definline index- (fixnum fixnum) fixnum	"(#0) - (#1)")
(definline index* (fixnum fixnum) fixnum	"(#0) * (#1)")
(definline index1+ (fixnum) fixnum		"(#0)+1")
(definline index1- (fixnum) fixnum		"(#0)-1")

(defmacro index-incf (place &optional (delta 1))
   `(setf ,place (the array-index (+ ,place ,delta))))

(defmacro index-decf (place &optional (delta 1))
   `(setf ,place (the array-index (- ,place ,delta))))

(definline index-min (fixnum fixnum) fixnum	"(#0) < (#1) ? (#0) : (#1)")
(definline index-max (fixnum fixnum) fixnum	"(#0) > (#1) ? (#0) : (#1)")

(defmacro index-floor (number &optional divisor) ; never used
   `(the array-index
	 (values (floor (the array-index ,number)
			,@(when divisor `((the array-index ,divisor)))))))
(defmacro index-ceiling (number &optional divisor) ; rarely used
   `(the array-index
	 (values (ceiling (the array-index ,number)
			  ,@(when divisor `((the array-index ,divisor)))))))
(defmacro index-truncate (number &optional divisor) ; never used
   `(the array-index
	 (values (truncate (the array-index ,number)
			   ,@(when divisor `((the array-index ,divisor)))))))

(definline index-mod (fixnum fixnum) fixnum	"(#0) % (#1)")

(definline index-ash (fixnum fixnum) fixnum
	    "(#1) > 0 ? ((#0) << (#1)) : ((#0) >> -(#1))")
(definline index-plusp (fixnum) compiler::boolean	"(#0) > 0")
(definline index-zerop (fixnum) compiler::boolean	"(#0) == 0")
(definline index-evenp (fixnum) compiler::boolean	"!((#0) & 1)")
(definline index-oddp (fixnum) compiler::boolean	"((#0) & 1)")
(definline index> (fixnum fixnum) compiler::boolean	"(#0) > (#1)")
(definline index= (fixnum fixnum) compiler::boolean	"(#0) == (#1)")
(definline index< (fixnum fixnum) compiler::boolean	"(#0) < (#1)")
(definline index>= (fixnum fixnum) compiler::boolean	"(#0) >= (#1)")
(definline index<= (fixnum fixnum) compiler::boolean	"(#0) <= (#1)")
|#

;;; from file dependent.l

(definline card8->int8 (fixnum) fixnum
  "(#0) & 0x80 ? ((#0)-0x100) : (#0)")

(definline int8->card8 (fixnum) fixnum
  "(#0)")

(definline card16->int16 (fixnum) fixnum
  "((short)(#0))")

(definline int16->card16 (fixnum) fixnum
  "((unsigned short)(#0))")

(definline card32->int32 (fixnum) fixnum
  "(#0)")

(definline int32->card32 (fixnum) fixnum
  "(#0)")

(definline aref-card8 (string fixnum) fixnum
  "(#0)->ust.ust_self[#1]")

(definline aset-card8 (fixnum string fixnum) fixnum
        "((#1)->ust.ust_self[#2]=(#0))")
;; alternatively:
;(defmacro aref-card8 (s i) `(schar ,s ,i))
;(defmacro aset-card8 (v s i) `(setf (schar ,s ,i) (int-char ,v)))

(definline aref-int8 (string fixnum) fixnum
  "(#0)->st.st_self[#1]")

(definline aset-int8 (fixnum string fixnum) fixnum
        "((#1)->st.st_self[#2]=(#0))")

(definline aref-card16 (string fixnum) fixnum
  "(*(unsigned short *)((#0)->ust.ust_self+(#1)))")

(definline aset-card16 (fixnum string fixnum) fixnum
  "((*(unsigned short *)((#1)->ust.ust_self+(#2)))=#0)")

(definline aref-int16 (string fixnum) fixnum
  "(*(short *)((#0)->st.st_self+(#1)))")

(definline aset-int16 (fixnum string fixnum) fixnum
  "((*(short *)((#1)->st.st_self+(#2)))=#0)")

(definline aref-card32 (string fixnum) fixnum
  "(*(unsigned long *)((#0)->ust.ust_self+(#1)))")

(definline aset-card32 (fixnum string fixnum) fixnum
  "((*(unsigned long *)((#1)->ust.ust_self+(#2)))=#0)")

(definline aref-int32 (string fixnum) fixnum
  "(*(long *)((#0)->ust.ust_self+(#1)))")

(definline aset-int32 (fixnum string fixnum) fixnum
  "((*(long *)((#1)->ust.ust_self+(#2)))=#0)")

(definline aref-card29 (string fixnum) fixnum
  "((*(unsigned long *)((#0)->ust.ust_self+(#1))) & 0x1fffffff)")

(definline aset-card29 (fixnum string fixnum) fixnum
  "((*(unsigned long *)((#1)->ust.ust_self+(#2)))=#0)")
