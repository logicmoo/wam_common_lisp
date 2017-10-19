;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; 10/13/84: A few utility functions and other initialization
;   1/6/86: Added new variable syntax
;  9/27/86: Removed flavors
;
;*******************************************************************************

(defun make-typed-var (name)
  (cond
   ((eq? name *question-mark-atom*)
    (make-var nil nil))
   ((eq? name 'self)
    (make-var name *person-ob*))
   ((eq? name 'other)
    (make-var name *person-ob*))
   (else (let* ((str (symbol->string name))
                (len (string-length str))
                (last-char (nthchar str (- len 1))))
               (if (digit? last-char 10)
               (make-var name 
                         (ob$name->ob
                          (string->symbol
                           (string-slice str 0 (- len 1)))))
                   (make-var name (ob$name->ob name)))))))

(setq *bell-char* (ascii->char 7))
(setq *esc-char* (ascii->char 27))
(setq *del-char* (ascii->char 127))
(setq *cr-char* (ascii->char 13))
(setq *cntrl-z-char* (ascii->char 26))
(setq *cntrl-rb-char* (ascii->char 29))

(defun string-truncate (str len)
  (string-slice str 0 (min (string-length str) len)))

;
; NDBG: New Debugging Mechanism
;
; For use in the program:
;
; (ndbg-begin) - Start a new indentation level
; (ndbg-add-item rule) - Add item to list of current items
; (ndbg *dbg-stream* keyname "Message~%") - Print a debugging message
; (ndbg-remove-item rule) - Remove item to list of current items
; (ndbg-end) - End indentation level
;
; For use at debugging time:
;
; (interest 'keyname . items) - Show debugging info for keyname
;   when any item is present in current items, or if an item
;   is 'all, always
; (disinterest 'keyname . items) - Stop debugging info for keyname
;   and items
; (interests) - Show current interests
; (ndbg-reset) - Reset indenting level back to zero
;

(setq *ndbg-interests* nil)
(setq *ndbg-level* 0)
(setq *ndbg-items* nil)
(setq *ndbg-indentation* 1)
(setq *ndbg-max-indentation* 50)

(defun ndbg-add-item (item)
 (setq *ndbg-items* (cons item *ndbg-items*)))

(defun ndbg-remove-item (item)
 (setq *ndbg-items* (delq! item *ndbg-items*)))

(defun ndbg-indentation (stream)
   (yloop (initial (cnt (min (* *ndbg-level* *ndbg-indentation*)
                            *ndbg-max-indentation*)))
         (ywhile (> cnt 0))
         (ydo (format stream " ")
             (setq cnt (- cnt 1)))))

(defun ndbg-begin ()
  (setq *ndbg-level* (+ *ndbg-level* 1)))

(defun ndbg-end ()
  (setq *ndbg-level* (- *ndbg-level* 1)))

(defun ndbg-reset () (setq *ndbg-level* 0))

; Use (interest 'unify ^rule) and (interest 'show ^rule)
; to get full debugging info for a rule. (And use disinterest
; to turn off).

(defun interest (key &rest items)
  (let ((found (assq key *ndbg-interests*)))
    (if found
        (yloop (yfor item in items)
              (ydo (if (memq? item (cdr found))
                       (format *gate-output*
                               "Item ~A key ~A already an interest~%"
                               item key)
                       (setf (cdr found) (cons item (cdr found))))))
        (setq *ndbg-interests* (cons (cons key items) *ndbg-interests*)))
    (interests)))

(defun disinterest (key &rest items)
  (let ((found (assq key *ndbg-interests*)))
    (if found
        (yloop (yfor item in items)
               (ydo (if (not (memq? item (cdr found)))
                        (format *gate-output*
                                "Item ~A key ~A not an interest~%"
                                item key)
                        (setf (cdr found) (delq! item (cdr found))))))
        (format *gate-output* "Key ~A not found at all~%" key))
    (interests)))

(defun interests () *ndbg-interests*)
(defun items () *ndbg-items*)

(defun write-comments (comments stream)
  (let ((max-length (+ 2 (apply 'max
                                (map 'list string-length comments)))))
   (write-dashes-stream max-length stream)
   (yloop (yfor comment1 in comments)
         (ydo (dbg stream " ~A~%" comment1)))
   (write-dashes-stream max-length stream)))

(defun write-dashes-stream (number stream)
  (yloop (initial (count 1))
        (ywhile (<= count number))
        (ydo (format stream "-")
            (increment-me count)))
  (newline stream))

(defun new-filename (atm)
 (let* ((name (string-downcase! (symbol->string (gen-id atm))))
        (filename (string-append "tmp." name)))
   (yloop
    (ywhile (file-exists? filename))
    (ydo
     (dbg *gate-warn-dbg* "-")
     (setq filename (string-append filename "a")))
    (yresult filename))))

(set-macro-character #\?
   (lambda (stream ch)
     (let ((read-in (read stream t nil t))
           (colon-pos nil)
           (str nil))
        (setq str (symbol->string read-in))
        (cond
         ((setq colon-pos (string-posq #\+ str))
          (ob$fcreate
           `(UAND
             obj (UPROC
                  proc (QUOTE ,(string->symbol
                         (string-append (nthchdr str (1+ colon-pos)) "?"))))
             obj ,(make-typed-var
                   (string->symbol (substring str 0 colon-pos))))))
         ((setq colon-pos (string-posq #\: str))
          (if (= colon-pos 0)
              (make-var nil
               (ob$name->ob (string->symbol
                 (nthchdr str (1+ colon-pos))))) ; e.g. for ?:person
              (make-var (string->symbol (substring str 0 colon-pos))
                        (ob$name->ob
                         (string->symbol (nthchdr str (1+ colon-pos)))))))
         (else (make-typed-var read-in)))))
  t)

(set-macro-character #\^ 
  (lambda (stream ch)
    (let ((name (read stream t nil t))
          (ob nil))
     (setq ob (ob$name->ob name))
     (if ob
         (list 'quote ob)
         (progn
          (format t "No such ob ^~A~%" name)
          (list 'quote *repl-wont-print*)))))
  t)

(set-macro-character #\!
         (lambda (stream ch)
                 (let ((name (read stream t nil t))
                       (ob nil))
                      (setq ob (ob$name->ob name))
                      (if ob
                          (progn
                           (po ob)
                           (list 'quote *repl-wont-print*))
                          (progn (format t "No such ob ^~A~%" name)
                         (list 'quote *repl-wont-print*)))))
  t)

(defun interrogate (string)
  (format (standard-output) string)
  (let ((response (read (standard-input))))
       (read-line (standard-input))
       (cond ((or (eq? 'y response)
                  (eq? 'yes response)) t)
             ((or (eq? 'n response)
                  (eq? 'no response)) nil)
             (else (format (standard-output)
                           "Please type 'y' or 'n' as a response.~%")
                   (interrogate string)))))

(defun arg-value (arg-name init-plist default)
   (let ((found (assq arg-name init-plist)))
     (if (and found (neq? (cadr found) 'none))
               (cadr found)
               (if (eq? default 'required)
                   (error "Required make-instance argument ~A not supplied"
                          arg-name)
                   default))))

(defun walk-append (proc lst)
  (yloop (initial (result nil))
        (ywhile lst)
        (ydo (setq result (append! result (funcall proc (car lst))))
            (setq lst (cdr lst)))
        (yresult result)))

(defun with-default (val default)
   (if val val default))

(defun random-integer (from to)
  (cond ((= to from) to)
        ((< to from) (random-integer to from))
        (else (+ from (random (1+ (- to from)))))))

(defun random-element (x)
  (nth-elem x (random-integer 0 (-1+ (length x)))))

(defun randomize (x)
  (yloop (initial (result nil)
                 (elem nil))
        (ywhile x)
        (ydo (setq elem (random-element x))
            (setq x (delq elem x))
            (setq result (cons elem result)))
        (yresult result)))

(defun force-flonum (x)
  (if (flonum? x) x (fixnum->flonum x)))

(defun random-real (from to)
  (setq *large-integer* (random-integer 4 20))
  (cond ((= to from) to)
        ((< to from) (random-real to from))
        (else
         (+ from
            (* (- to from)
               (/ (force-flonum (random-integer 0 *large-integer*))
                  (force-flonum *large-integer*)))))))

; End of file.
