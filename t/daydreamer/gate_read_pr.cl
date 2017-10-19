;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; Pretty printer and reader for obs.
;
; 11/6/85: Original version written
; 1/24/86: Added fread and major-type
; 1/30/86: Changed readers to disallow literal atoms if there is
;          no such ob. This is too error prone.
; 9/22/86: Added pprulesize, tabulation, etc.
; 9/24/86: Got rid of flavors
;
;*******************************************************************************

(setq *unknown-ob* (ty$create 'UNKNOWN nil '(prop (obj) ())))
(setq *notype-ob*
      (ty$create 'NOTYPE nil '(nil (actor from to obj strength) ())))

(defun ob$read (stream)
  (ob$readlist (read stream)))

(defun ob$fread (stream)
  (ob$freadlist (read stream)))

(defun ob$readlist (ppob)
 (let ((ob (ob$create-empty)))
  (if (not (symbol? (car ppob)))
      (error "bad ob list format: head of ~A not a symbol" ppob))
      ; Todo: should punt to freadlist in such a case, so that
      ; something like ('(?Type) actor (HA)) could be handled.
  (let ((type (ob$name->ob (car ppob)))
        (slot-names nil)
        (ppformat nil))
    (if (null? type)
        (progn
         (setq type nil)
         (ndbg-roman-nl *gate-dbg* ob-warn
                 "Warning: bad ob list format: head of ~A not a type~%" ppob)
         (if (null? type)
             (setq type *unknown-ob*))))
    (ob$add ob 'type type)
    (setq ppformat (ob$get type 'ppformat))
    (if (null? ppformat)
        (error "bad ob list format: ~A not a readable type" ppob))
    (setq slot-names (cadr ppformat))
    (yloop
      (initial (rest (cdr ppob)) (elem nil) (ob-elem nil)
               (explicit-slot-name nil))
      (ywhile rest)
          (ydo
; Todo: The below is semantically wrong wrt ppformat: it enables the
; last element of the cadr to be implicit even if this is supposed
; to be prevented by the caddr. But this was done to allow obname
; to work.
           (if (and (memq? (car slot-names) (caddr ppformat))
                    (cdr slot-names))
               (setq slot-names (cdr slot-names))
;           (setq slot-names (stuck-cdr slot-names))
           (progn
            (setq elem (car rest))
            (cond
             ((or (var? elem) (special? elem))
              (ob$add ob (or explicit-slot-name (car slot-names)) elem)
              (if (null? explicit-slot-name)
                  (setq slot-names (stuck-cdr slot-names))))
             ((string? elem)
              (ob$add ob (or explicit-slot-name (car slot-names)) elem)
              (if (null? explicit-slot-name)
                  (setq slot-names (stuck-cdr slot-names))))
             ((pair? elem)
              (if (eq? (car elem) 'quote)
                  (ob$add ob
                          (or explicit-slot-name (car slot-names))
                          (cadr elem))
                  (ob$add ob (or explicit-slot-name (car slot-names))
                          (ob$readlist elem)))
              (if (null? explicit-slot-name)
                  (setq slot-names (stuck-cdr slot-names))))
             ((or (symbol? elem) (number? elem))
              (cond
               ((eq? elem 'obname)
                (setq explicit-slot-name 'obname))
               ((eq? elem '--)
                (setq slot-names (stuck-cdr slot-names)))
;               ((var? elem)
;                (ob$add ob (or explicit-slot-name (car slot-names))
;                 elem)
;                (if (null? explicit-slot-name)
;                    (setq slot-names (stuck-cdr slot-names))))
               ((memq? elem slot-names)
                (setq explicit-slot-name elem))
               ((setq ob-elem (ob$name->ob elem))
                (ob$add ob (or explicit-slot-name (car slot-names)) ob-elem)
                (if (null? explicit-slot-name)
                    (setq slot-names (stuck-cdr slot-names))))
               (else
                (if (and (not (number? elem))
                         (not (string? elem))
                         (not (nil? elem))
                         (neq? explicit-slot-name 'obname))
                    (progn
                     (ndbg-roman-nl *gate-dbg* ob-warn
                             "Warning: No such ob name ~A, assume it is atom~%"
                             elem)))
                (ob$add ob (or explicit-slot-name (car slot-names))
                        elem)
                (if (null? explicit-slot-name)
                    (setq slot-names (stuck-cdr slot-names))))))
             ((ob? elem)
              (ob$add ob (or explicit-slot-name (car slot-names))
                      elem)
              (if (null? explicit-slot-name)
                  (setq slot-names (stuck-cdr slot-names))))
             (else (error "bad ob list format: unknown elem ~A in ~A"
                          elem ppob)))
         (setq rest (cdr rest))))))
       (if (eq? *notype-ob* (ob$ty ob))
           (ob$removes ob 'type))
       ob)))

(defun stuck-cdr (x)
  (or (cdr x) x))

;
; ob$freadlist is a fast version of ob$readlist handling only single
; slot values explicitly specified (i.e., no propositional notation).
; Handles NOTYPE and Atom slot names which refer to obs.
; Fdefine-ob1 is designed to be fast for use at runtime.
; Therefore, error checking has been sacrificed.
; If type is quoted, it gets used literally.
;

(defun ob$freadlist (ppob)
  (let ((ob (ob$create-empty)))
       (if (and (pair? (car ppob))
                (eq? 'quote (caar ppob)))
           (ob$add ob 'type (cadar ppob))
           (ob$add ob 'type (ob$name->ob (car ppob))))
       (yloop
        (initial (rest (cdr ppob))
                 (elem nil)
                 (temp nil))
        (ywhile rest)
        (ydo
         (setq elem (cadr rest))
    (if t ; Uri needs nil to be OK. (not (nil? elem))
         (ob$add ob (car rest)
                  (cond
                   ((nil? elem) nil)
                   ((and (eq? (car rest) 'obname)
                         (symbol? elem))
                    elem)
                   ((and (symbol? elem) (setq temp (ob$name->ob elem)))
                    temp)
                   ((symbol? elem)
                     (ndbg-roman-nl *gate-dbg* ob-warn
                             "Warning: No such ob name ~A, assume it is atom"
                             elem)
                    elem)
                   ((or (ob? elem) (string? elem) (number? elem))
                    elem)
                   ((pair? elem)
                    (if (eq? (car elem) 'quote)
                        (cadr elem)
                        (ob$freadlist elem)))
                   (else (error "ob$freadlist: unknown elem ~A in ~A"
                                elem ppob))))
        (ndbg-roman-nl *gate-dbg* ob-warn
                       "Warning: in ob$freadlist, value of slot ~A is nil"
                       (car rest)))
         (setq rest (cddr rest))))
       (if (eq? *notype-ob* (ob$ty ob))
           (ob$removes ob 'type))
       ob))

;
; Top-level printing functions
;

; Quicky ob print function
(defun po (ob)
  (if (ob? ob)
      (progn
       (ob$pr ob *gate-output* *ob-print-options*)
       (newline *gate-output*))
      (format *gate-output* "~A~%" ob))
  *repl-wont-print*)

(defun pom (obs)
  (map 'list (lambda (ob) (po ob))
        obs))

(defun px (ob)
  (pretty-print (ob$pairs ob) *gate-output*)
  (newline *gate-output*)
  *repl-wont-print*)

(defun pox (self stream)
  (begin-regular-font stream)
  (format stream "[")
  (end-font stream)
  (format stream "~A" (ob->string self))
  (begin-regular-font stream)
  (format stream ": ")
  (end-font stream)
  (ob$sprint self stream)
  (begin-regular-font stream)
  (format stream "]")
  (end-font stream))

;
; Print ob in short form
;
(setq *ob-sprint-options*
  '(no-newline short parens never-prop))
 
(defun ob$sprint (ob stream)
  (ob$pr ob stream *ob-sprint-options*))

;
; Print ob with specified options
;
(defun ob$pr (ob stream print-options)
  (if (memq? 'fast print-options)
      (ob$fprint ob stream)
      (let ((old *ob-print-options*))
           (unwind-protect
            (progn
             (if (neq? print-options 'default)
                 (setq *ob-print-options* print-options))
             (ob$print1 ob stream 0)
             *repl-wont-print*)
            (setq *ob-print-options* old)))))

;
; Print ob with current default options
;

(setq *typeset?* nil)

(if *typeset?*
    (progn
     (setq *ob-print-options* '(parens never-prop typeset))
     (setq *ob-sprint-options* '(no-newline short parens never-prop typeset)))
    (progn
     (setq *ob-print-options* '(parens never-prop))
     (setq *ob-sprint-options* '(no-newline short parens never-prop))))

(setq *ob-print-options* '(parens no-newline))
(setq *ob-sprint-options* '(parens no-newline ))

;
; never-prop - override normal printing format and never print things in
;              propositional form
;
(defun ob$print (ob stream)
  (ob$print1 ob stream 0))

;
; Fast ob printer (rarely used; the short option is better)
;
(defun ob$fprint (ob stream)
  (if (> (length (ob$names ob)) 2)
      (format stream "~A" (ob->string ob))
  (let ((type (ob$ty ob))
        (obj (ob$get ob 'obj)))
    (if (ty? type)
        (if obj
            (progn
             (format stream "(~A obj " (type->string type))
             (ob$fprint obj stream)
             (format stream " ..LDOTZ..)"))                 
            (format stream "(~A ..LDOTA..)"
                    (type->string type)))
        (format stream "()")))))

(setq *visited-obs* nil)

(defun ob$print1 (ob stream column)
  (if *typeset?*
      (if (memq? 'short *ob-print-options*)
          nil ; (format stream "{\\small")
          (format stream "~%\\end{flushleft}~%\\begin{tabbing}~%")))
  ; took out 'small' above because I don't think it works...
  (setq *visited-obs* nil)
  (ob$print2 ob stream column t)
  (if *typeset?*
      (if (memq? 'short *ob-print-options*)
          nil ; (format stream "}")
          (format stream "~%\\end{tabbing}~%\\begin{flushleft}~%"))))

(defun ob$print2 (ob stream column top?)
 (cond
  ((var? ob)
   (print-variable-item stream ob column))
   ((memq? ob *visited-obs*)
       (let ((str (format nil "^~A" (ob$name ob))))
         (princ str stream)
	  (setq column (+ (length str) column))))
	   
  (else 
     (ob$print4 ob stream column top?))))


(defun ob$print3 (ob stream column top?)
    (progn (let ((str (format nil "#{~A: "  
       (ob$name ob))))
         (princ str stream)
	 (setq column (+ (length str) column)))
           (ob$print4 ob stream column top?)
	   (format stream "}") (setq column (1+ column))))
  
(defun ob$print4 (ob stream column top?)
   (setq *visited-obs* (cons ob *visited-obs*))

   (let* ((head (or (ob$get ob 'head) (ob$get ob 'type)))
        (head-string nil)
        (slot-column nil)
        (first? t)
        (ppformat nil))
   (if (memq? 'parens *ob-print-options*)
       (progn
        (begin-regular-font stream)
        (format stream "(")
        (end-font stream)
        (setq column (+ 1 column))))
   (if head
       (progn
        (setq head-string
             (cond
              ((ob? head)
               (symbol->string (ob$name head)))
              ((symbol? head)
               (symbol->string head))
              (else "??????")))
        (begin-head-font stream)
        (format stream "~A" head-string)
        (end-font stream)
        (setq slot-column
             (+ 1 column (string-length head-string)))
        (setq column slot-column))
       (progn
        (setq head *unknown-ob*)
        (setq slot-column column)))
   (setq ppformat (if (ty? head)
                     (ob$get head 'ppformat)
                     (ob$get *unknown-ob* 'ppformat)))
   (if (null? ppformat) (error "ob$print: null ppformat for type ~A ob ~A"
                               head (ob->string ob)))
   (yloop
    (initial (short? (memq? 'short *ob-print-options*))
             (slot-values nil) (slot-name-str nil)
             (separate-lines? nil) (emb-ob? nil) (slot-value-column nil)
             (not-first? nil) (previous-multiple-and-no-newline? nil)
             (previous-embedded-ob? nil))
    (yfor slot-name in (cadr ppformat))
    (yuntil (and short?
                (> column *ob-short-length*)
                (progn
                 (do-ldots stream) t)))
  (ydo (if (not (memq? slot-name (caddr ppformat)))
       (progn
        (setq slot-values
             (if (memq? 'omit-links *ob-print-options*)
                 (ob$gets-omit-links ob slot-name)
                 (ob$gets ob slot-name)))
        (if slot-values
            (progn
             (if (and head first?)
                 (progn
                  (setq first? nil)
                  (if *typeset?*
                      (format stream "\\ ")
                      (format stream " "))
                  (if (and *typeset?*
                           (not (memq? 'no-newline *ob-print-options*)))
                      (format stream "\\=\\+"))))
             (if previous-embedded-ob?
                 (if (memq? 'no-newline *ob-print-options*)
                     (progn
                      ; I don't see why this would be.
                      ; It prints a space after objects below.
                      nil
;                      (format stream " ")
;                      (setq column (+ 1 column))
                      )
                     (progn
                      (do-newline stream)
                      (if (not *typeset?*)
                          (print-spaces stream slot-column))
                      (setq column slot-column))))
             (setq emb-ob? (any? (lambda (x) (embedded-ob? x)) slot-values))
             (setq separate-lines? (and (cdr slot-values) emb-ob?))
             (if (and (not (memq? 'no-newline *ob-print-options*))
                      (or (and (> column slot-column)
                               (> column *print-length*))
                          (and emb-ob?
                               (> column slot-column)
                               (not (memq? 'no-embedded-ob-newline
                                      *ob-print-options*)))))
                 (progn
                  (do-newline stream)
                  (if (not *typeset?*)
                      (print-spaces stream slot-column))
                  (setq column slot-column))
                 (if (> column slot-column)
                     (progn (if *typeset?*
                                (format stream "\\ ")
                                (format stream " "))
                            (setq column (+ 1 column)))))
             (if (or (neq? (car ppformat) 'prop)
                     (memq? 'never-prop *ob-print-options*)
                     (memq? slot-name (caddr ppformat))
                     previous-multiple-and-no-newline?)
                 (progn
                  (begin-slot-name-font stream)
                  (setq slot-name-str
                       (string-downcase! (symbol->string slot-name)))
                  (format stream "~A " slot-name-str)
                  (end-font stream)
                  (setq column (+ column 1 (string-length slot-name-str)))))
             (setq slot-value-column column)
             (if (and *typeset?*
                      (not (memq? 'no-newline *ob-print-options*)))
                 (format stream "\\=\\+"))
             (setq not-first? nil)
             (yloop (initial (rest slot-values))
                   (ywhile rest)
                   (yuntil (and short?
                               (> column *ob-short-length*)
                               (progn
                                (do-ldots stream) t)))
                   (ydo (if not-first?
                           (progn
                            (if *typeset?*
                                (format stream "\\ ")
                                (format stream " "))
                            (setq column (+ 1 column))))
                       (setq not-first? t)
                       (setq column (print-item stream (car rest) column))
                       (if (and (cdr rest)
                                separate-lines?)
                           (progn
                            (if (memq? 'no-newline *ob-print-options*)
                                (progn
                                 (format stream " ")
                                 (setq column (+ 1 column)))
                                (progn
                                 (do-newline stream)
                                 (if (not *typeset?*)
                                     (print-spaces stream slot-value-column))
                                 (setq column slot-value-column)))
                            (setq not-first? nil)))
                       (setq rest (cdr rest))))
             (if (and *typeset?*
                      (not (memq? 'no-newline *ob-print-options*)))
                 (format stream "\\-"))
             (setq previous-embedded-ob? t) ; was emb-ob?
                 ; It looks bad to have multiple slots per line.
             (setq previous-multiple-and-no-newline?
                  (and (cdr slot-values) (not separate-lines?)))))))))
   (if (memq? 'parens *ob-print-options*)
       (progn
        (begin-regular-font stream)
        (format stream ")")
        (end-font stream)))
   (if (and *typeset?*
            (not top?)
            (not (memq? 'no-newline *ob-print-options*)))
       (format stream "\\-"))
   (if first? column (- column 1))))

(defun do-newline (stream)
 (cond
  (*typeset?*
   (format stream "\\\\~%"))
  (else (newline stream))))

(defun do-ldots (stream)
 (cond
  (*typeset?*
   (format stream "\\ldots{}"))
  (else (format stream "..."))))

(defun begin-head-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\bf{}"))))

(defun begin-regular-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\rm{}"))))

(defun begin-slanted-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\sl{}"))))

(defun begin-slot-name-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\sf{}"))))

(defun end-font (stream)
 (cond
  (*typeset?*
   (format stream "}"))))

(defun begin-large-roman-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\large\\rm{}"))))

(defun begin-large-bold-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\large\\rm{}"))))

(defun begin-roman-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\rm{}"))))

(defun begin-typewriter-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\tt{}"))))

(defun begin-bold-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\bf{}"))))

(defun begin-italic-font (stream)
 (cond
  (*typeset?*
   (format stream "{\\em{}"))))

;(setq *switch-out-string* "")
;
;(defun switch-in-flushleft (stream)
;  (if *typeset?*
;      (progn
;       (format stream "\\begin{flushleft}~%")
;       (setq *switch-out-string* "\\end{flushleft}~%"))))
;
;(defun switch-out (stream)
;  (if (not (string-empty? *switch-out-string*))
;      (write *switch-out-string* stream)))

(setq *print-length* 60)
(setq *print-length* 600)

(setq *ob-short-length* 26)
(setq *ob-short-length* 127)

(defun type->string (type)
  (if *typeset?*
      (concatenate 'string "{\\bf{}" (symbol->string (ob$name type))
                      "}")
      (symbol->string (ob$name type))))

(defun ob->string (ob)
  (let ((str (symbol->string (ob$name ob))))
    (string-downcase! (chdr str))
    (if *typeset?*
        (setq str (concatenate 'string "{\\sl{}" str "}")))
    str))

(defun ob->raw-string (ob)
  (let ((str (symbol->string (ob$name ob))))
    (string-downcase! (chdr str))
    str))

(defun print-spaces (stream number)
  (yloop (initial (count 1))
        (ywhile (<= count number))
        (ydo (if *typeset?*
                     (format stream "\\ ")
                     (format stream " "))
             (increment-me count))))

(defun embedded-ob? (x)
  (or (pair? x)
      (and (ob? x)
           (not (var? x))
           (not (ob$literal? x))
           (not (has-interesting-obname? x)))))

(defun print-item (stream item column)
 (cond
  ((nil? item)
   (print-symbol stream 'nil column))
  ((symbol? item)
   (print-symbol stream item column))
  ((string? item)
   (print-regular stream (concatenate 'string "\"" item "\"") column))
  ((number? item)
   (print-typewriter stream (format nil "~A" item) column))
  ((pair? item)
   (print-pair stream item column))
  ((var? item)
   (print-variable-item stream item column))
  ((ob? item)
     (if (or (ob$literal? item)
             (has-interesting-obname? item))
         (print-slanted stream
                        (capitalize! (symbol->string (ob$name item)))
                        column)
         (ob$print2 item stream column nil)))
  (else
;   (format *gate-output* "Unknown item.~%")
   (print-regular stream "??????" column))))

; I hope result column doesn't matter because we always do
; a newline after printing a pair.
(defun print-pair (stream pair column)
  (if (memq? 'no-newline *ob-print-options*)
      (progn
       (begin-typewriter-font stream)
       (format stream "(QUOTE ~A)" pair)
       (end-font stream)
       column)
      (let ((str (with-output-to-string (stream1)
                      (pretty-print (list 'quote pair)
                                    stream1)))
            (result nil))
        (walk-string
         (lambda (c)
          (cond
           ((eq? c #\SPACE)
            (if *typeset?*
                (setq result (append! result (list #\\ #\SPACE)))
                (setq result (append! result (list #\SPACE)))))
           ((eq? c #\NEWLINE)
            (if *typeset?*
                (setq result (append! result (list #\\ #\\ #\NEWLINE)))
                (setq result (append! result (list #\NEWLINE
                                     (n-spaces column))))))
           (else
            (setq result (append! result (list c))))))
         str)
        (begin-typewriter-font stream)
        (format stream "~A" (list->string result))
        (end-font stream)
        column)))

(defun n-spaces (n)
  (yloop (initial (result nil))
        (ywhile (> n 0))
        (ydo (setq result (cons #\SPACE result))
            (setq n (- n 1)))
        (yresult result)))

(defun print-variable-item (stream item column)
  (let ((name (variable-name item))
        (typ (variable-type item)))
   (cond
    ((and name typ)
     (let ((type-string nil)
           (name-string nil)
           (len nil)
           (last-char nil))
      (cond
       ((eq? name 'self)
        (print-slanted stream "?Self" column))
       ((eq? name 'other)
        (print-slanted stream "?Other" column))
       ((and (setq type-string
                  (symbol->string (ob$name typ)))
             (setq name-string (symbol->string name))
             (string-equal? type-string name-string))
        (print-slanted stream
                       (concatenate 'string "?" (capitalize! name-string))
                       column))
       ((and (setq len (string-length name-string))
             (setq last-char (nthchar name-string (- len 1)))
             (digit? last-char 10)
             (string-equal? type-string
                           (string-slice name-string 0 (- len 1))))
        (print-slanted stream
                       (concatenate 'string "?" (capitalize! name-string))
                       column))
       (else
        (print-slanted stream
                       (concatenate 'string "?"
                                       (capitalize! name-string)
                                       ":"
                                       type-string)
                       ; Todo: make the type-string bold? Also NOTYPE below.
                       column)))))
    (name
     (print-slanted stream
                    (concatenate 'string "?" (capitalize! (symbol->string name))
                                   ":NOTYPE")
                    column))
    (typ
     (print-slanted stream
                    (concatenate 'string "?:"
                     (symbol->string (ob$name typ)))
                    column))
    (else
     (print-slanted stream "??" column)))))
 
(defun print-symbol (stream symbol column)
  (let ((string (symbol->string symbol)))
   (cond
        ((string-equal? string "!LDOTS")
         (begin-regular-font stream)
         (format stream "\\ldots")
         (end-font stream)
         (+ column 3))
        ((string-equal? string "!OR-BAR")
         (begin-regular-font stream)
         (format stream "$\\mid$")
         (end-font stream)
         (+ column 1))
        ((and (string-equal? (string-slice string 0 (min (- (string-length
                                                             string) 1) 7))
                             "!ITALIC")
              (setq result (string-posq #\: string)))
         (format stream "{\\em{}~A}"
                 (string-downcase (nthchdr string (+ result 1))))
         (+ column (- (string-length string) result)))
        (else
     (begin-typewriter-font stream)
     (format stream "'~A" string)
     (end-font stream)
     (+ column (string-length string))))))

(defun print-slanted (stream string column)
  (begin-slanted-font stream)
  (format stream "~A" string)
  (end-font stream)
  (+ column (string-length string)))

(defun print-regular (stream string column)
  (begin-slanted-font stream)
  (format stream "~A" string)
  (end-font stream)
  (+ column (string-length string)))

(defun print-typewriter (stream string column)
  (begin-typewriter-font stream)
  (format stream "~A" string)
  (end-font stream)
  (+ column (string-length string)))

(defun capitalize! (string)
  (string-downcase! (chdr string))
  string)
                
(defun interesting-obname? (obname-str)
  (not (string-equal? (string-slice obname-str
                                    0
                                    (min (- (string-length obname-str) 1) 2))
                      "OB")))

(defun has-interesting-obname? (ob)
  (interesting-obname? (symbol->string (ob$name ob))))

(defun ob->list (ob)
  (cond
   ((ob? ob)
    (yloop
     (initial (result (list (ob$name (ob$get ob 'type)))))
     (yfor slot-name in (ob$slot-names ob))
     (ydo
      (if (neq? slot-name 'type)
          (setq result
           (append result
             (cons slot-name
              (map 'list 'ob->list1 (ob$gets ob slot-name)))))))
     (yresult result)))
   ((atom ob) ob)
   (else (map 'list 'ob->list1 ob))))

(defun ob->list1 (ob)
  (cond
   ((ob? ob) (ob$name ob))
   ((atom ob) ob)
   (else (map 'list 'ob->list ob))))

; End of file.
