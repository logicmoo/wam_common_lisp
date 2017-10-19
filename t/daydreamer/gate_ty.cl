;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains the type mechanism with simple inheritance
;
; 6/29/85: Original version written
; 1/24/86: Added major types
; 9/23/86: Rewrote to be flavorless
;
;*******************************************************************************

(ob$decl-inverses 'isa 'isa-of)
;(ob$decl-inverses 'type 'type-of)

(defun ty$instance? (self type-name)
  (if (not (ob? self))
      (progn (error "ty$instance?: ~A not ob" self)
             (ndbg-roman-nl *gate-dbg* ob-warn
                            "Warning: ty$instance?: ~A not ob" self)
             nil)
      (and (ty? (ob$get self 'type))
           (or (eq? type-name (ob$get self 'type))
               (any? (lambda (x) (memq? type-name (ob$names x)))
                     (ty$supertypes* (ob$get self 'type)))))))

(defun ty$instance-of? (self type)
  (and (ty? (ob$get self 'type))
       (memq? type (ty$supertypes* (ob$get self 'type)))))

;
; ppformat = (<prop | nil> <slotnames> <optional-slotnames>)
;

(defun ty$create (name parent-names ppformat)
  (let* ((temp nil)
         (parents
          (map 'list (lambda (x)
                      (setq temp (ob$name->ob x))
                      (if (null? temp)
                          (error "ty$create ~A: ~A not defined yet.~%" name x)
                          temp))
               parent-names))
         (type
          (ty$new name parents)))
        (cond
         (ppformat (ob$set type 'ppformat ppformat))
         (parents (ob$set type 'ppformat (ob$get (car parents) 'ppformat)))
         (t (ob$set type 'ppformat
                       '(prop (actor from to obj) (actor from to obj)))))
        type))

(defun ty$fcreate (name parent-names slots)
  (ty$create name parent-names (list nil slots nil)))

(defun ty$new (name supertypes)
  (let ((type (ob$create-named-empty name))
        (temp nil))
       (ob$set type 'type *ty-ob*)
       (setq *types* (cons type *types*))
       (yloop
        (yfor supertype in supertypes)
        (ydo (ob$add type 'isa supertype)))
       ; Exemplars are used by the DAYDREAMER generator.
       (setq temp (ob$create-empty))
       (ob$add temp 'type type)
       (ob$set-literal type t)
       (ob$add type 'exemplar temp)
       ; Return new type
       type))

; This is way recursive!
(setq *ty-ob* (ob$create-named-empty 'ty))
(ob$set *ty-ob* 'ppformat '(nil (exemplar)))
(ob$set-literal *ty-ob* t)

(setq *types* (list *ty-ob*))
(ob$set *ty-ob* 'type *ty-ob*)

(defun ty$major-type (type-name)
  (ob$set (ob$name->ob type-name) 'major-type? t))

(defun ty$display ()
  (yloop (yfor type in *types*)
         (ydo (ob$unhide type))))

(defun ty$supertypes (self)
  (ob$gets self 'isa))

(defun ty$subtypes (self)
  (ob$gets self 'isa-of))

(defun ty$supertypes* (self)
  (yloop (initial (result nil)
                  (x nil))
        (yfor type in (ob$gets self 'isa))
        (ydo (setq x (ty$supertypes* type))
             (if (not (null? x))
                 (setq result (union result x))))
        (yresult (cons self result))))

(defun ty$supertype-of? (self type)
  (memq? type (ty$supertypes* self)))

(defun ty$subtypes* (self)
  (yloop (initial (result nil))
        (yfor type in (ob$gets self 'isa-of))
        (ydo (setq result (append result (ty$subtypes* type))))
        (yresult (cons self result))))

(defun ty$subtype-of? (self type)
  (memq? type (ty$subtypes* self)))

(defun ty$least-common-supertype (type1 type2)
  (yloop (initial (supertypes*2 (ty$supertypes* type2))
                 (result nil))
        (yfor supertype1 in (ty$supertypes* type1))
        (yuntil result)
        (ydo (if (memq? supertype1 supertypes*2)
                (setq result supertype1)))
        (yresult result)))

(defun ty$basic-type-distance (ancestor type)
  (let ((position (position ancestor (ty$supertypes* type))))
       (if position (+ 1 position) *max-fixnum*)))

(defun ty$distance (type1 type2)
  (let ((lcs (ty$least-common-supertype type1 type2)))
       (if lcs
           (min (ty$basic-type-distance lcs type1)
                (ty$basic-type-distance lcs type2))
           *max-fixnum*)))

(defun ty$get-major-type (self)
  (yloop (initial (result nil))
         (yfor type in (ty$supertypes* self))
         (ywhile (not result))
         (ydo (setq result (if (ob$get type 'major-type?)
                                type
                                nil)))
         (yresult (if result result self))))

; End of file.
