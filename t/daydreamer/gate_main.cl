;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; OB slot-filler objects
;
; 10/13/84: Original version written
;  1/24/86: Added path functions, got rid of weblists
;  1/28/86: Changed add to use append-end instead of cons
;  9/25/86: Converted to be independent of flavors
; 11/02/86: Added add-unique-name
;
;*******************************************************************************

(defun is-var? (x) (var? x))

; Global list of obs
(setq *obs* nil)

; Global list of obnames
(setq *obnames* nil)

;
; OBR
;

(defun print-ob (ob stream depth)
 (declare (ignore depth))
 (ob$print-self ob stream))

(defstruct (obr (:print-function print-ob))
      "OB representation structure"
      (obnames nil)    ; list of symbols which may be used to name the ob
      (slots nil)      ; list of (slot-name slot-value)
      (literal nil)    ; whether the ob is a literal ob
)

(defun ob$print-self (self stream)
  (cond
   ((ty? self)
    (format stream "#{~A}" (car (obr-obnames self))))
   ((var? self) (format stream "#{~A: ?~A:~A}"
                        (car (obr-obnames self))
                        (variable-name self)
                        (if (variable-type self)
                            (car (obr-obnames (variable-type self)))
                            nil)))
   (else
    (format stream "#{~A: " (car (obr-obnames self)))
    (ob$sprint self stream)
    (format stream "}"))))

(setq *hidden-default* t)

(setq *next-ob-number* 1)

;
; ob$create-named-empty: create an empty ob with the specified name
;
(defun ob$create-named-empty (name)
  (let ((self (make-obr)))
       (setq *obs* (cons self *obs*))
       (if name
           (ob$add-name self name)
           (ob$add-unique-name
            self
            (string->symbol
             (string-append "OB."
                            (prog1
                             (fixnum->string *next-ob-number*)
                             (increment-me *next-ob-number*))))))
       self))

(defun ob$destroy (self)
  (if (obr-node self)
      (error "Sure enough, node of ~A isn't nil!" self))
  (ob$remove-all self)
  (yloop (yfor obname in (obr-obnames self))
        (ydo (ob$remove-name self obname)))
  (setq *obs* (delq! self *obs*)))

;
; Inverse slots
;

(setq *inverse-slot-list* nil)

;
; A primary slot is any that does not have an inverse or one that
; was explicitly defined as a primary slot in a primary/secondary
; declaration.
;
(defun primary-slot? (slot-name)
  (or (not (not (assq slot-name *inverse-slot-list*)))
      (yloop (initial (rest *inverse-slot-list*)
                     (result nil))
            (ywhile rest)
            (yuntil result)
            (ydo (if (eq? slot-name (cadr (car rest)))
                    (setq result (car (car rest))))
                (setq rest (cdr rest)))
            (yresult (null? result)))))

(defun inverse-slot (slot-name)
  (let ((found (assq slot-name *inverse-slot-list*)))
    (if found
        (cadr found)
        (yloop (initial (rest *inverse-slot-list*)
                       (result nil))
              (ywhile rest)
              (yuntil result)
              (ydo (if (eq? slot-name (cadr (car rest)))
                      (setq result (car (car rest))))
                  (setq rest (cdr rest)))
              (yresult result)))))

(defun ob$decl-has-inverse (primary-slot-name)
  (ob$decl-inverses primary-slot-name
                          (string->symbol
                           (string-append
                            (symbol->string primary-slot-name)
                            "-OF"))))

(defun used-as-primary? (primary)
  (and (primary-slot? primary)
       (any? (lambda (ob)
              (any? (lambda (slot) (eq? primary (slots-name slot)))
                    (ob$pairs ob)))
             *obs*)))

;
; (ob$decl-inverses primary-slot-name secondary-slot-name):
;
; Declare a primary-slot/secondary-slot pair. Simply warns if the
; declaration has already been performed.
;
(defun ob$decl-inverses (primary secondary)
 (if (not (used-as-primary? secondary))
  (if (and (primary-slot? primary)
           (eq? (inverse-slot primary) secondary))
      (ndbg *gate-dbg* ob-warn
       "Warning: Duplicate primary/secondary declaration ~A ~A~%"
       primary secondary)
      (cond
       ((inverse-slot primary)
        (error "~A already has an inverse of ~A." primary
                                                  (inverse-slot primary)))
       ((inverse-slot secondary)
        (error "~A already has an inverse of ~A." primary
                                                  (inverse-slot secondary)))
       (else (setq *inverse-slot-list* (cons (list primary secondary)
                                            *inverse-slot-list*)) t)))
  (progn
   (format *gate-output*
           "~&~A has already been used as a primary slot name.~%"
           secondary)
   (format *gate-output* "Declaration not performed.~%"))))

(defun decl-primary-secondaries (lst)
 (map 'list
  (lambda (pair) (ob$decl-inverses (car pair) (cadr pair)))
  lst))

;
; This function is not perfect. If one desires duplicate pairs, this
; will not create them.
;
(defun enforce-inverses ()
 (map 'list
  (lambda (ob)
    (if t
        (map 'list
         (lambda (slot)
           (let ((inv (inverse-slot (slots-name slot))))
             (if (and inv
                      (ob? (slots-value slot))
                      (null? (memq? ob (ob$gets
                                         (slots-value slot) inv))))
                 (ob$basic-add
                  (slots-value slot) inv ob))))
         (ob$pairs ob))))
  *obs*))

;
; (ob$add-name ob obname):
;
; Associate another obname with the ob. This new obname may be
; used to refer to the ob, as may any obnames previously defined.
;
; Todo: a separate *obnames* for non "OB." names would speed things up.
; Alternatively, use hash tables.
;
(defun ob$add-name (self obname)
  (if (not (memq? obname (obr-obnames self)))
      (progn
       (if (ob? obname)
           (progn
            (setq obname (ob$name obname))
            (ndbg *gate-dbg* ob-warn
                  "Warning: Probable obname redefinition.~%")))
       (if (not (symbol? obname))
           (setq obname (error "ob$add-name: ~A not symbol" obname)))
       (yloop
        (ywhile (assq obname *obnames*))
        (ydo
         (let ((new-obname
                (string->symbol
                 (string-append
                  (symbol->string obname) "X"))))
              (ndbg *gate-dbg* ob-warn
                   "Warning: Obname ~A already used--using ~A instead.~%"
                   obname new-obname)
              (setq obname new-obname))))
       (setq *obnames* (cons (list obname self) *obnames*))
       (set-obr-obnames self (cons obname (obr-obnames self)))
       obname)
      (progn
      (ndbg *gate-dbg* ob-warn
             "Warning: Obname ~A already in effect for specified ob.~%"
             obname)
       obname)))

; This assumes obname is already determined to be unique. We assume that
; we are able to generate unique "OB." names above. This assumes the
; user does not create such names also.
(defun ob$add-unique-name (self obname)
  (setq *obnames* (cons (list obname self) *obnames*))
  (set-obr-obnames self (cons obname (obr-obnames self)))
  obname)

(defun ob$remove-name (self obname)
  (set-obr-obnames self (delq! obname (obr-obnames self)))
  (setq *obnames* (del! (lambda (x y) (eq? x (car y))) obname *obnames*))
  (if (null? (obr-obnames self))
      (ob$add-unique-name
       self
       (string->symbol
        (string-append "OB."
                       (prog1
                        (fixnum->string *next-ob-number*)
                        (increment-me *next-ob-number*)))))))

;
; (ob$name->ob obname):
;
; Return the ob referred to by a obname. If there is no ob associated
; with the given obname, nil is returned.
;
(defun ob$name->ob (obname)
  (let ((found (assq obname *obnames*)))
    (if found (cadr found) nil)))

;
; (ob$name ob):
;
; Return a (actually, the most recently defined) obname for an ob.
;
(defun ob$name (self)
  (car (obr-obnames self)))                      

;
; (ob$names ob):
;
(defun ob$names (self)
  (obr-obnames self))

;
; The automatic setting of inverses can be disabled. Currently,
; inverse setting is turned off only during load of a ob dump.
;

(setq *inverse-setting?* t)

(defun inverse-setting-on ()
  (let ((previous *inverse-setting?*))
    (setq *inverse-setting?* t)
    previous))

(defun inverse-setting-off ()
  (let ((previous *inverse-setting?*))
    (setq *inverse-setting?* nil)
    previous))

(defun restore-inverse-setting (val)
  (setq *inverse-setting?* val))

;
; (ob$add ob slot-name slot-value):
;
; Add a slot value to an ob. If slot-value is an ob, the inverse slot addition
; is performed.
;

(defun ob$add (self slot-name slot-value)
  (enforce-ob self "ob$add")
  (ob$add1 self slot-name slot-value)
  slot-value)

;
; (ob$padd ob slot-path slot-value):
;
; Allows a path to be used.
;

(defun ob$padd (self slot-path slot-value)
  (enforce-ob self "ob$padd")
  (if (pair? slot-path)
      (ob$add1
                (path->ob self slot-path)
                (path->slot-name slot-path)
                slot-value)
      (ob$add1 self slot-path slot-value))
  slot-value)

(defun path->ob (ob path)
  (if (cdr path)
      (path->ob (ob$get ob (car path))
                 (cdr path))
      ob))

;
; (ob$remove ob slot-name slot-value):
;
; Remove the specified value from the specified slot. If slot-value is a
; ob, the inverse slot removal is performed.
;

(defun ob$remove (self slot-name slot-value)
  (enforce-ob self "ob$remove")
  (ob$remove1 self slot-name slot-value)
  slot-value)

(defun ob$premove (self slot-path slot-value)
  (enforce-ob self "ob$premove")
  (if (pair? slot-path)
      (ob$remove1
                  (path->ob self slot-path)
                  (path->slot-name slot-path))
      (ob$add1 self slot-path slot-value))
  slot-value)

;
; (ob$gets ob slot-name):
;
; Return all values of a slot.
;

(defun ob$gets (self slot-name)
  (enforce-ob self "ob$gets")
 (if (eq? slot-name 'obname)
     (obr-obnames self)
     (yloop (initial (result nil)
                    (rest (obr-slots self)))
           (ywhile rest)
           (ydo (if (eq? slot-name (slots-name (car rest)))
                   (setq result
                        (append! result (list (slots-value (car rest))))))
               (setq rest (cdr rest)))
           (yresult result))))

;
; (ob$get-many ob slot-names):
;
; Return values of several slots.
;

(defun ob$get-many (self slot-names)
  (enforce-ob self "ob$get-many")
  (yloop (initial (result nil)
                 (rest (obr-slots self)))
        (ywhile rest)
        (ydo (if (memq? (slots-name (car rest)) slot-names)
                (setq result (append! result (list (slots-value (car rest))))))
            (setq rest (cdr rest)))
        (yresult result)))  

(defun ob$concatenate (&rest obs)
  (yloop
    (initial (result (ob$create-empty)))
    (yfor ob in obs)
    (ydo
     (yloop
      (yfor sv in (ob$pairs ob))
      (ydo (ob$add1 result (slots-name sv) (slots-value sv)))))
   (yresult result)))

(defun ob$concatenate! (&rest obs)
  (yloop
    (initial (result (car obs)))
    (yfor ob in (cdr obs))
    (ydo
     (yloop
      (yfor sv in (ob$pairs ob))
      (ydo (ob$add1 result (slots-name sv) (slots-value sv)))))
   (yresult result)))

;
; ob$pairs: get all the slot-name slot-value pairs of an ob
;
(defun ob$pairs (self) (obr-slots self))

(defun ob$slot-names (self)
  (yloop
   (initial (result nil))
   (yfor pair in (ob$pairs self))
   (ydo
    (if (not (memq? (car pair) result))
        (setq result (append result (list (car pair))))))
   (yresult result)))

(defun make-into-obname (obj) (if (ob? obj) (ob$name obj) obj))

(defun ob$basic-add (self slot-name slot-value)
  (if (eq? slot-name 'obname)
      (ob$add-name self slot-value)
      (set-obr-slots self (append! (obr-slots self)
                                   (list (list slot-name slot-value))))))

(defun ob$literal? (self) (obr-literal self))

(defun ob$set-literal (self val)
  (set-obr-literal self val))

(defun ob$add1 (self slot-name slot-value)
  (ob$basic-add self slot-name slot-value)
  (if *inverse-setting?*
   (let ((inv (inverse-slot slot-name)))
     (if (and inv (ob? slot-value))
         ; If slot-name has an inverse and slot-value is a ob,
         ; perform inverse setting
         (ob$basic-add slot-value inv self)))))

;
; (ob$get ob slot-name):
;
; Return a unique value of a slot. If there is more than one value for the
; slot, an arbitrary one is returned.
;

(defun ob$get (self slot-name)
 (enforce-ob self "ob$get")
 (if (eq? slot-name 'obname)
  (car (obr-obnames self))
  (let ((found (assq slot-name (obr-slots self))))
    (if found (slots-value found) nil))))

(defun ob$pget (self slot-path)
  (enforce-ob self "ob$pget")
  (if (pair? slot-path)
      (ob$get (path->ob self slot-path)
            (path->slot-name slot-path))
      (if (eq? slot-path 'obname)
          (car (obr-obnames self))
          (let ((found (assq slot-path (obr-slots self))))
               (if found (slots-value found) nil)))))

;
; (ob$set ob slot-name slot-value):
;
; If it is desired to restrict slot values to a unique entry, this
; method removes all slot values from a slot, then sets the unique entry.
; Inverses are affected similarly.
;

(defun ob$set (self slot-name slot-value)
  (enforce-ob self "ob$set")
  (if (eq? slot-name 'obname)
      (progn 
       (ob$add-name self slot-value)
       slot-value)
      (yloop
       (initial (values (ob$gets self slot-name)))
       (ywhile values)
       (ydo
        (ob$remove1 self slot-name (car values))
        (setq values (cdr values)))
       (yresult
        (ob$add1 self slot-name slot-value)
        slot-value))))

(defun ob$pset (self slot-path slot-value)
  (enforce-ob self "ob$pset")
  (if (pair? slot-path)
      (ob$set (path->ob self slot-path)
            (path->slot-name slot-path)
            slot-value)
      (yloop (initial (values (ob$gets self slot-path)))
            (ywhile values)
            (ydo (ob$remove1 self slot-path (car values))
                 (setq values (cdr values)))
            (yresult
             (ob$add1 self slot-path slot-value))))
  slot-value)

; This function is never used?
(defun ob$set1 (self slot-name slot-value)
  (yloop (initial (values (ob$gets self slot-name)))
        (ywhile values)
        (ydo (ob$remove1 self slot-name (car values))
            (setq values (cdr values)))
        (yresult (ob$add1 self slot-name slot-value)))
  slot-value)

(defun ob$removes (self slot-name) (ob$set self slot-name nil) self)
#|
(defun ob$removes (self slot-name)
  (map 'list
   (lambda (slot)
    (if (eq? (slots-name slot) slot-name)
        (ob$remove1 self (slots-name slot)
                   (slots-value slot))))
   (obr-slots self))
  self)
|#

(defun ob$remove-all (self)
  (map 'list
   (lambda (slot)
    (ob$remove1 self (slots-name slot)
               (slots-value slot)))
   (obr-slots self))
  self)

(defun ob$remove1 (self slot-name slot-value)
  (ob$basic-remove self slot-name slot-value)
  (if *inverse-setting?*
   (let ((inv (inverse-slot slot-name)))
    (if (and inv (ob? slot-value))
        ; If slot-name has an inverse and slot-value is an ob,
        ; perform inverse removal.
        (ob$basic-remove slot-value inv self)))))

(defun ob$basic-remove (self slot-name slot-value)
 (if (eq? slot-name 'obname)
  (ob$remove-name self slot-value)
  (yloop (initial (rest (obr-slots self)) (found nil))
        (ywhile rest)
        (yuntil found)
        (ydo (if (and (eq? slot-name (slots-name (car rest)))
                      (eq? slot-value (slots-value (car rest))))
                (progn
                 (setq found t)
                 (set-obr-slots self (delq! (car rest) (obr-slots self)))))
            (setq rest (cdr rest)))
        (yresult (if (null? found)
                    (progn
                     (error "~A slot of ~A has no ~A value."
                            slot-name self slot-value)
                     nil))))))

; End of file.
