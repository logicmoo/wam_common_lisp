;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Sun Aug 22 18:09:49 2004
;;;; Contains: Tests of the ~< ~> directive





(def-pprint-test format.justify.1
  (format nil "~<~>")
  "")

(def-pprint-test format.justify.2
  (loop for i from 1 to 20
        for s1 = (make-string i :initial-element #\x)
        for s2 = (format nil "~<~A~>" s1)
        unless (string= s1 s2)
        collect (list i s1 s2))
  nil)

(def-pprint-test format.justify.3
  (loop for i from 1 to 20
        for s1 = (make-string i :initial-element #\x)
        for s2 = (format nil "~<~A~;~A~>" s1 s1)
        unless (string= s2 (concatenate 'string s1 s1))
        collect (list i s1 s2))
  nil)

(def-pprint-test format.justify.4
  (loop for i from 1 to 20
        for s1 = (make-string i :initial-element #\x)
        for expected = (concatenate 'string s1 " " s1)
        for s2 = (format nil "~,,1<~A~;~A~>" s1 s1)
        unless (string= s2 expected)
        collect (list i expected s2))
  nil)

(def-pprint-test format.justify.5
  (loop for i from 1 to 20
        for s1 = (make-string i :initial-element #\x)
        for expected = (concatenate 'string s1 "," s1)
        for s2 = (format nil "~,,1,',<~A~;~A~>" s1 s1)
        unless (string= s2 expected)
        collect (list i expected s2))
  nil)

(def-pprint-test format.justify.6
  (loop for i from 1 to 20
        for s1 = (make-string i :initial-element #\x)
        for expected = (concatenate 'string s1 "  " s1)
        for s2 = (format nil "~,,2<~A~;~A~>" s1 s1)
        unless (string= s2 expected)
        collect (list i expected s2))
  nil)

(def-pprint-test format.justify.7
  (loop for mincol = (random 50)
        for len = (random 50)
        for s1 = (make-string len :initial-element #\x)
        for s2 = (format nil "~v<~A~>" mincol s1)
        for expected = (if (< len mincol)
                           (concatenate 'string
                                        (make-string (- mincol len) :initial-element #\Space)
                                        s1)
                         s1)
        repeat 100
        unless (string= s2 expected)
        collect (list mincol len s1 s2 expected))
  nil)

(def-pprint-test format.justify.8
  (loop for mincol = (random 50)
        for minpad = (random 10)
        for len = (random 50)
        for s1 = (make-string len :initial-element #\x)
        for s2 = (format nil "~v,,v<~A~>" mincol minpad s1)
        for expected = (if (< len mincol)
                           (concatenate 'string
                                        (make-string (- mincol len) :initial-element #\Space)
                                        s1)
                         s1)
        repeat 100
        unless (string= s2 expected)
        collect (list mincol minpad len s1 s2 expected))
  nil)

(def-pprint-test format.justify.9
  (loop for mincol = (random 50)
        for padchar = (random-from-seq +standard-chars+)
        for len = (random 50)
        for s1 = (make-string len :initial-element #\x)
        for s2 = (format nil "~v,,,v<~A~>" mincol padchar s1)
        for expected = (if (< len mincol)
                           (concatenate 'string
                                        (make-string (- mincol len) :initial-element padchar)
                                        s1)
                         s1)
        repeat 100
        unless (string= s2 expected)
        collect (list mincol padchar len s1 s2 expected))
  nil)

(def-pprint-test format.justify.10
  (loop for mincol = (random 50)
        for padchar = (random-from-seq +standard-chars+)
        for len = (random 50)
        for s1 = (make-string len :initial-element #\x)
        for s2 = (format nil (format nil "~~~d,,,'~c<~~A~~>" mincol padchar) s1)
        for expected = (if (< len mincol)
                           (concatenate 'string
                                        (make-string (- mincol len) :initial-element padchar)
                                        s1)
                         s1)
        repeat 500
        unless (string= s2 expected)
        collect (list mincol padchar len s1 s2 expected))
  nil)

(def-pprint-test format.justify.11
  (loop for i = (1+ (random 20))
        for colinc = (1+ (random 10))
        for s1 = (make-string i :initial-element #\x)
        for s2 = (format nil "~,v<~A~>" colinc s1)
        for expected-len = (* colinc (ceiling i colinc))
        for expected = (concatenate 'string
                                    (make-string (- expected-len i) :initial-element #\Space)
                                    s1)
        repeat 10
        unless (string= expected s2)
        collect (list i colinc expected s2))
  nil)

(def-pprint-test format.justify.12
  (format nil "~<XXXXXX~^~>")
  "")

(def-pprint-test format.justify.13
  (format nil "~<XXXXXX~;YYYYYYY~^~>")
  "XXXXXX")

(def-pprint-test format.justify.13a
  (format nil "~<~<XXXXXX~;YYYYYYY~^~>~>")
  "XXXXXX")

(def-pprint-test format.justify.14
  (format nil "~<XXXXXX~;YYYYYYY~^~;ZZZZZ~>")
  "XXXXXX")

(def-pprint-test format.justify.15
  (format nil "~13,,2<aaa~;bbb~;ccc~>")
  "aaa  bbb  ccc")

(def-pprint-test format.justify.16
  (format nil "~10@<abcdef~>")
  "abcdef    ")

(def-pprint-test format.justify.17
  (format nil "~10:@<abcdef~>")
  "  abcdef  ")

(def-pprint-test format.justify.18
  (format nil "~10:<abcdef~>")
  "    abcdef")

(def-pprint-test format.justify.19
  (format nil "~4@<~>")
  "    ")

(def-pprint-test format.justify.20
  (format nil "~5:@<~>")
  "     ")

(def-pprint-test format.justify.21
  (format nil "~6:<~>")
  "      ")

(def-pprint-test format.justify.22
  (format nil "~v<~A~>" nil "XYZ")
  "XYZ")

(def-pprint-test format.justify.23
  (format nil "~,v<~A~;~A~>" nil "ABC" "DEF")
  "ABCDEF")

(def-pprint-test format.justify.24
  (format nil "~,,v<~A~;~A~>" nil "ABC" "DEF")
  "ABCDEF")

(def-pprint-test format.justify.25
  (format nil "~,,1,v<~A~;~A~>" nil "ABC" "DEF")
  "ABC DEF")

(def-pprint-test format.justify.26
  (format nil "~,,1,v<~A~;~A~>" #\, "ABC" "DEF")
  "ABC,DEF")

(def-pprint-test format.justify.27
  (format nil "~6<abc~;def~^~>")
  "   abc")

(def-pprint-test format.justify.28
  (format nil "~6@<abc~;def~^~>")
  "abc   ")

;;; ~:; tests

(def-pprint-test format.justify.29
  (format nil "~%X ~,,1<~%X ~:;AAA~;BBB~;CCC~>")
  "
X AAA BBB CCC")

(def-pprint-test format.justify.30
  (format nil "~%X ~<~%X ~0,3:;AAA~>~<~%X ~0,3:;BBB~>~<~%X ~0,3:;CCC~>")
  "
X 
X AAA
X BBB
X CCC")

(def-pprint-test format.justify.31
  (format nil "~%X ~<~%X ~0,30:;AAA~>~<~%X ~0,30:;BBB~>~<~%X ~0,30:;CCC~>")
  "
X AAABBBCCC")

(def-pprint-test format.justify.32
  (format nil "~%X ~<~%X ~0,3:;AAA~>,~<~%X ~0,3:;BBB~>,~<~%X ~0,3:;CCC~>")
  "
X 
X AAA,
X BBB,
X CCC")

;;; Interaction with ~T and ~@T. Unlike ~:T, this should not signal an error.

;; ~<...~>: Three segments of 4 characters to justify: "AA  ", "BBBB", "CCCC"
(def-pprint-test format.justify.33
  ;; ~T, no padding
  (format nil "~12,,,'*<AA~4T~;BBBB~;CCCC~>")
  "AA  BBBBCCCC")

(def-pprint-test format.justify.34
  ;; ~T, one padding character per segment
  (format nil "~15,,,'*<AA~4T~;BBBB~;CCCC~>")
  "AA  *BBBB**CCCC")

(def-pprint-test format.justify.35
  ;; ~@T, no padding
  (format nil "~12,,,'*<AA~1,2@T~;BBBB~;CCCC~>")
  "AA  BBBBCCCC")

(def-pprint-test format.justify.36
  ;; ~@T, one padding character per segment
  (format nil "~15,,,'*<AA~1,2@T~;BBBB~;CCCC~>")
  "AA  *BBBB**CCCC")

;; ~<...~:;...~>: First output "AA ", then justify "CCCC" and
;; "DDDD". Optionally output newline and "BBBB" beforehand if the
;; output doesn't fit in the line width.
(def-pprint-test format.justify.37
  ;; no padding, output fits
  (format nil "AA~4T~8,,,'*<~%BBBB~,12:;CCCC~;DDDD~>")
  "AA  CCCCDDDD")

(def-pprint-test format.justify.38
  ;; no padding, output doesn't fit
  (format nil "AA~4T~8,,,'*<~%BBBB~,11:;CCCC~;DDDD~>")
  "AA  
BBBBCCCCDDDD")

(def-pprint-test format.justify.39
  ;; one padding character per segment, output fits
  (format nil "AA~4T~10,,,'*<~%BBBB~,14:;CCCC~;DDDD~>")
  "AA  CCCC**DDDD")

(def-pprint-test format.justify.40
  ;; one padding character per segment, output doesn't fit
  (format nil "AA~4T~10,,,'*<~%BBBB~,13:;CCCC~;DDDD~>")
  "AA  
BBBBCCCC**DDDD")

(def-pprint-test format.justify.41
  ;; Same with ~@T
  (format nil "AA~1,2@T~8,,,'*<~%BBBB~,12:;CCCC~;DDDD~>")
  "AA  CCCCDDDD")

(def-pprint-test format.justify.42
  (format nil "AA~1,2@T~8,,,'*<~%BBBB~,11:;CCCC~;DDDD~>")
  "AA  
BBBBCCCCDDDD")

(def-pprint-test format.justify.43
  (format nil "AA~1,2@T~10,,,'*<~%BBBB~,14:;CCCC~;DDDD~>")
  "AA  CCCC**DDDD")

(def-pprint-test format.justify.44
  (format nil "AA~1,2@T~10,,,'*<~%BBBB~,13:;CCCC~;DDDD~>")
  "AA  
BBBBCCCC**DDDD")

;;; Error cases

;;; See 22.3.5.2

;;; Interaction with ~W

(deftest format.justify.error.w.1
  (signals-error-always (format nil "~< ~W ~>" nil) error)
  t t)

(deftest format.justify.error.w.2
  (signals-error-always (format nil "~<X~:;Y~>~W" nil) error)
  t t)

(deftest format.justify.error.w.3
  (signals-error-always (format nil "~w~<X~:;Y~>" nil) error)
  t t)

;;; Interaction with ~_

(deftest format.justify.error._.1
  (signals-error-always (format nil "~< ~_ ~>") error)
  t t)

(deftest format.justify.error._.2
  (signals-error-always (format nil "~<X~:;Y~>~_") error)
  t t)

(deftest format.justify.error._.3
  (signals-error-always (format nil "~_~<X~:;Y~>") error)
  t t)

;;; Interaction with ~I

(deftest format.justify.error.i.1
  (signals-error-always (format nil "~< ~i ~>") error)
  t t)

(deftest format.justify.error.i.2
  (signals-error-always (format nil "~<X~:;Y~>~I") error)
  t t)

(deftest format.justify.error.i.3
  (signals-error-always (format nil "~i~<X~:;Y~>") error)
  t t)

;;; Interaction with ~:T

(deftest format.justify.error.\:t.1
  (signals-error-always (format nil "~<XXX~1,1:TYYY~>") error)
  t t)

(deftest format.justify.error.\:t.2
  (signals-error-always (format nil "~<XXX~:;YYY~>ZZZ~4,5:tWWW") error)
  t t)

(deftest format.justify.error.\:t.3
  (signals-error-always (format nil "AAAA~1,1:TBBB~<XXX~:;YYY~>ZZZ") error)
  t t)

;;; Interaction with ~<...~:>

(deftest format.justify.error.logical-block.1
  (signals-error-always (format nil "~<~:;~>~<~:>" nil nil nil) error)
  t t)

(deftest format.justify.error.logical-block.2
  (signals-error-always (format nil "~<~:>~<~:;~>" nil nil nil) error)
  t t)
