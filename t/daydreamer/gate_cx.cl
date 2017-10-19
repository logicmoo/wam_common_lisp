;*******************************************************************************
;
; GATE
; Version 2.3
;
; Copyright 1984, 1985, 1986, 1987, 1988, 1999, 2004 Erik T. Mueller.
; All Rights Reserved.
;
; This file contains:
; Context mechanism for obs
;
; 6/22/85: Original version written
; 6/30/85: Added rule comments
;  1/6/86: Removed old rules and truth maintenance
; 1/24/86: Added inheriting of mutations-tried?
; 1/25/86: Added pseudo-sprouts
; 1/27/86: Added and tested type hashing
; 7/19/86: Added touched-facts
; 9/24/86: Rewrote code to be flavorless
;
; Todo: must enforce first arg being a context.
;
;*******************************************************************************


(setq *cx-ob* (ty$create 'CX nil nil))

(setq *next-cx-number* 1)

(defun cx$create ()
  (cx$sprout nil))

(defun cx$sprout (parent)
  (let ((self (ob$fcreate '(CX))))
       (ob$add-unique-name self (string->symbol
         (string-append "CX." (prog1 (fixnum->string *next-cx-number*)
                                      (increment-me *next-cx-number*)))))
       (cond
        ((cx? parent)
         (ob$set self 'parent parent) (cx$add-child parent self)
         (ob$set self 'mutations-tried? (ob$get parent 'mutations-tried?))
         (if (ob$get parent 'timeout)
             (ob$set self 'timeout (- (ob$get parent 'timeout) 1)))
         (ob$set self 'pseudo-sprout? (ob$get parent 'pseudo-sprout?))
         (ob$set self 'ancestors (cons parent (ob$get parent 'ancestors)))
         (ob$set self 'all-obs (copy-list (ob$get parent 'all-obs)))
         (ob$set self 'type-hashing (map 'list (lambda (x) (copy-list x))
                                         (ob$get parent 'type-hashing)))
         (ob$set self 'gen-switches (ob$get parent 'gen-switches))
         (ob$set self 'touched-facts (ob$get parent 'touched-facts)))
        ((null? parent))
        (else (error "cx$sprout: parent is not a context or NIL.")))
       (if parent
           (ndbg *gate-dbg* context "~A --> ~A~%" parent self))
       self))

;
; Getters
;

(defun cx$parent (self)
  (ob$get self 'parent))

(defun cx$ancestors (self)
  (ob$get self 'ancestors))

(defun cx$children (self)
  (ob$get self 'children))

(defun cx$set-last-sprout-con (self val)
  (ndbg *gate-dbg* rule "setting last sprout concept = ~A in ~A~%" val self)
  (if val
      (let ((parent (ob$get self 'parent)))
       (ob$set self 'last-sprout-con val)
       (if (and parent
                (null? (cx$last-sprout-con parent)))
           (cx$set-last-sprout-con parent val)))))

(defun cx$last-sprout-con (self)
  (ob$get self 'last-sprout-con))

; Pseudo sprouts don't inherit info from their parent (or ancestors).
; In particular, the top-context of an ob (an optimization) is no
; longer guaranteed to be an ancestor of a context in which that
; ob is asserted. In accordance, the top-context is never used for
; a pseudo-sprout context. Pseudo-sprouts are effectively root contexts
; but think they are sprouts of the specified context as far as &parent
; and &ancestor related calls go. By the way, all descendents of a
; pseudo sprout are pseudo sprouts.
(defun cx$pseudo-sprout-of (self context)
  (if (ob$get self 'parent)
      (error "Cannot make a pseudo sprout out of a context with a parent")
      (progn
       (ob$set self 'parent context)
       (ob$set self 'ancestors (cons context (ob$get context 'ancestors)))
       (cx$add-child context self)
       (ob$set self 'pseudo-sprout? t))))

(defun cx$touch-fact (self fact)
  (if (and (touchable-fact? fact)
           (not (memq? fact (ob$get self 'touched-facts))))
      (ob$set self 'touched-facts (cons fact (ob$get self 'touched-facts)))))

(defun cx$sorted-all-obs (self)
  (if (ob$get self 'pseudo-sprout?)
      (progn
       (format *gate-output* "GATE bug warning: Can't sort a pseudo-sprout~%")
       (ob$get self 'all-obs))
      (yloop (initial (result nil)
                      (rest (ob$get self 'all-obs)))
             (yfor context in (cons self (ob$get self 'ancestors)))
             (ydo (yloop (yfor ob in rest)
                         (ydo (if (memq? context (ob$gets ob 'top-context))
                                  (progn
                                   (setq result (cons ob result))
                                   (setq rest (delq! ob rest)))))))
             (yresult result))))

(defun cx$add-child (self child)
  (ob$set self 'children (cons child (ob$get self 'children))))

(defun cx$most-recent-child (self)
  (if (ob$get self 'children)
      (car (ob$get self 'children))
      nil))

(defun cx$leaf-descendants (self)
  (if (null? (ob$get self 'children))
      (list self)
      (yloop (initial (result nil))
             (yfor child in (ob$get self 'children))
             (ydo (setq result (append result (cx$leaf-descendants child))))
             (yresult result))))

(defun cx$descendants (self)
  (if (null? (ob$get self 'children))
      (list self)
      (yloop (initial (result (list self)))
             (yfor child in (ob$get self 'children))
             (ydo (setq result (append result (cx$descendants child))))
             (yresult result))))

(defun cx$tree-print (self)
  (map 'list (lambda (x) (cx$print x))
        (cx$descendants self)))

(defun cx$root (self)
  (if (ob$get self 'ancestors)
      (tlast (ob$get self 'ancestors))
      self))

(setq *disallow-non-leaf?* nil)
; Note: The above should be set to t if you are using truth
; maintenance!

(setq *hashing?* t)
; Hashing flag is global, not on a context-by-context basis. So,
; it is recommended not to change this flag during a session.
; Actually, you will be OK if after you change it, you never access
; any contexts used before the change.

; RESTRICTION if you are using hashing:
; Asserted obs cannot change type. If you want to change the type of
; an ob, first retract it from all the contexts in which it is
; asserted, change the type, then reassert it in all the contexts.

; Otherwise, hashing is recommended for faster operation.
; Typical speedups (in min:sec)
; 5:27 no hashing (almost all compiled)
; 3:31 hashing with no use of cx$get-all-ty (almost all compiled)
; 3:12 hashing with use of cx$get-all-ty (almost all compiled)
; 3:02 hashing with use of cx$get-all-ty (all compiled)
; 4:05 hashing with use of cx$get-all-ty and no unify reverse (all compiled)
; 3:39 hashing with use of cx$get-all-ty and no unify reverse
;      and add slot reversal (all compiled but add-slot-val)
; 3:06 hashing with use of cx$get-all-ty and no unify reverse
;      and add slot reversal (all compiled)
; --- below not run in fresh GATE (garbage and other factors will cause
;     increase)
; 5:10 no hashing with use of cx$get-all-ty (all compiled)
; 3:54 hashing with use of cx$get-all-ty and no unify reverse (all compiled)
; 5:22 no hashing with use of cx$get-all-ty and no unify reverse (all compiled)

; Very strange: why is it apparently slower with no unify reverse??!!!
; Because more predictive slots (e.g., type) are last.
; Changed add-slot-value to prepend.

; We could also hash on the type of the OBJ, and other common ob
; attributes.

(defun cx$assert-hash (self ob)
  (let ((type (ob$ty ob)))
    (if (ty? type)
        (let ((found (assq type (ob$get self 'type-hashing))))
          (if found
              (setf (cdr found) (cons ob (cdr found)))
              (ob$set self 'type-hashing
                      (cons (list type ob) (ob$get self 'type-hashing))))))))

(defun cx$retract-unhash (self ob)
  (let ((type (ob$ty ob)))
    (if (ty? type)
        (let ((found (assq type (ob$get self 'type-hashing))))
          (if found
              (progn
               (if (not (memq? ob (cdr found)))
                   (error "cx$retract-unhash: I can't unhash ~A" ob))
               (setf (cdr found) (delq! ob (cdr found))))
              (error "cx$retract-unhash: Strange, I can't seem to unhash ~A"
                     ob))))))

(defun cx$retrieve-hash (self ob)
  (let ((type (ob$ty ob)))
    (if (and (ty? type)
             (not (ty$instance? ob 'USPECIAL))
             (not (ty$instance? ob 'UVAR)))
        (let ((found (assq type (ob$get self 'type-hashing))))
          (if found
              (cdr found)
              nil))
        (ob$get self 'all-obs))))

(defun cx$retrieve-hash-type (self type)
  (let ((found (assq type (ob$get self 'type-hashing))))
     (if found
         (cdr found)
         nil)))

(defun cx$walk (self proc)
  (yloop (yfor ob in (ob$get self 'all-obs))
        (ydo (funcall proc ob))))

(defun cx$walk-type (self proc type)
  (if *hashing?*
      (yloop (yfor ob in (cx$retrieve-hash-type self type))
            (ydo (funcall proc ob)))
      (yloop (yfor ob in (ob$get self 'all-obs))
            (ydo (if (eq? type (ob$ty ob))
                    (funcall proc ob))))))

(defun cx$get-all (self)
  (ob$get self 'all-obs))

;
; Use of this will artificially slow down non-hashing, but hashing has
; already been proven without this.
;
(defun cx$get-all-ty (self type)
  (if *hashing?*
      (let ((found (assq type (ob$get self 'type-hashing))))
         (if found
             (cdr found)
             nil))
      (yloop (initial (result nil))
             (yfor ob in (ob$get self 'all-obs))
             (ydo (if (eq? type (ob$ty ob))
                      (setq result (cons ob result))))
             (yresult result))))

(defun cx$stats (self)
  (format *gate-output* "Hash stats for ~A~%" self)
  (yloop (initial (total (length (ob$get self 'all-obs)))
                  (count 0)
                  (len nil))
         (yfor elem in (ob$get self 'type-hashing))
         (ydo (setq len (length (cdr elem)))
              (setq count (+ len count))
              (format *gate-output* "~A has ~A entries (~A percent)~%"
                      (car elem) len (flonum->fixnum 
                                      (fl* (fl/ (fixnum->flonum len)
                                               (fixnum->flonum total)) 100.0))))
         (yresult
          (format *gate-output*
                  "There are ~A non typed-hashed entries (~A percent)~%"
                  (- total count)
                  (flonum->fixnum (fl* (fl/ (fixnum->flonum (- total count))
                            (fixnum->flonum total)) 100.0)))
          (format *gate-output* "Total of ~A entries~%" total))))

(defun cx$assert (self ob)
  (if (and *disallow-non-leaf?*
           (ob$get self 'children))
      (error "Cannot assert ~A in non-leaf context ~A." ob self))
  (assert-dbg ob self)
  (if (cx$true? self ob)
      (ndbg *gate-dbg* context "Assert: ~A already true in ~A~%" ob self)
      (progn
       (if *gen-stream*
            (generate ob
                      self
                      (append *global-switches*
                              (ob$get self 'gen-switches))))
       (if *hashing?* (cx$assert-hash self ob))
       (ob$set self 'add-obs (cons ob (ob$get self 'add-obs)))
       (ob$set self 'all-obs (cons ob (ob$get self 'all-obs)))
       (if (touchable-fact? ob)
           (ob$set self 'touched-facts (cons ob (ob$get self 'touched-facts))))
       (ob$add ob 'top-context self)))
  ob)

(defun assert-dbg (ob self)
  (ndbg-roman-nl *gate-dbg* rule "Assert ~A in ~A"
   ob (ob->string self))
;  (ob$pr ob *gate-dbg* *ob-print-options*)
;  (do-newline *gate-dbg*)
)

(defun retract-dbg (ob self)
  (ndbg-roman-nl *gate-dbg* rule "Retract ~A in ~A"
   ob (ob->string self))
;  (ob$pr ob *gate-dbg* *ob-print-options*)
;  (do-newline *gate-dbg*)
)

(setq *ctxt-unify-semantics?* nil)

(defun cx$retract (self ob)
  (if (and *disallow-non-leaf?*
           (ob$get self 'children))
      (error "Cannot retract ~A in non-leaf context ~A." ob self))
  (if (not (cx$true? self ob))
      (ndbg *gate-dbg* context "Retract: ~A already false in ~A~%"
            ob self)
      (let ((found nil))
        (retract-dbg ob self)
        (if (memq? self (ob$gets ob 'top-context))
            (ob$remove ob 'top-context self))
        (if *ctxt-unify-semantics?*
            (setq found (or (memq ob (ob$get self 'add-obs))
                           (mem-empty-unify ob (ob$get self 'add-obs) self)))
            (setq found (memq ob (ob$get self 'add-obs))))
        (if found
            (progn
             (if *hashing?* (cx$retract-unhash self (car found)))
             (ob$set self 'touched-facts (delq! (car found)
                                                (ob$get self 'touched-facts)))
             (ob$set self 'add-obs (delq! (car found) (ob$get self 'add-obs)))
             (ob$set self 'all-obs (delq! (car found)
                                           (ob$get self 'all-obs))))
            (progn
             (if *ctxt-unify-semantics?*
                 (setq found (or (memq ob (ob$get self 'all-obs))
                                (mem-empty-unify ob (ob$get self 'all-obs)
                                                 self)))
                 (setq found (memq ob (ob$get self 'all-obs))))
             (if found
                 (progn
                  (if *hashing?* (cx$retract-unhash self (car found)))
                  (ob$set self 'remove-obs (cons (car found)
                                                  (ob$get self 'remove-obs)))
                  ; If touched-facts inheritence is disabled, the line
                  ; below is unnecessary, because a touched-fact would
                  ; have to be in add-obs.
                  (ob$set self 'touched-facts (delq! (car found)
                                                     (ob$get self
                                                             'touched-facts)))
                  (if *ctxt-unify-semantics?*
                      (ob$set self 'all-obs (del-unify! (car found)
                                                         (ob$get self 'all-obs)
                                                         self))
                      (ob$set self 'all-obs (delq! (car found)
                                                    (ob$get self 'all-obs))))
                  ; Todo: For unify semantics, I still don't see why we
                  ; have to unify here. Won't we get an inconsistent state
                  ; if more than one one element matches? Besides, we've
                  ; already done the unify.
;                  (yloop (yfor justificand in
;                             (cx$justificands self (car found)))
;                         (ydo (ndbg *gate-dbg* context
;                            "Retracting dependent assertion ~A~%" justificand)
;                             (cx$retract self justificand)))
                  )
                 (error "Retract: cannot find ~A." ob))))))
  ob)

(defun cx$copy (self parent)
  (yloop (initial (new-context (cx$sprout parent))
                  (new-ob nil))
         (yfor ob in (ob$get self 'all-obs))
         (ydo (setq new-ob ob) ; was (ob$copy ob) but this would kill all links!
;             (ob$set new-ob 'top-context new-context)
;             (ob$removes new-ob 'top-context)
             (cx$assert new-context new-ob))
         (yresult new-context)))

(setq *retrieve-ignore-slots* nil)

; Returns ( <bd: (<retrieved-ob> (<var> <value>)...)> ...)
(defun cx$retrieve (self pattern)
  (yloop (initial (result nil)
                  (bindings nil))
         (yfor ob in (if *hashing?*
                         (cx$retrieve-hash self pattern)
                         (ob$get self 'all-obs)))
         (ydo (setq bindings (ob$unify-cx1 pattern ob *empty-bd*
                              *retrieve-ignore-slots* self))
              (if bindings
                  (setq result (cons (cons ob (cdr bindings)) result))))
         (yresult result)))

(defun cx$retrieve-bd (self pattern bd)
  (yloop (initial (result nil)
                  (bindings nil))
         (yfor ob in (if *hashing?*
                         (cx$retrieve-hash self pattern)
                         (ob$get self 'all-obs)))
         (ydo (setq bindings (ob$unify-cx1 pattern ob bd
                              *retrieve-ignore-slots* self))
              (if bindings
                  (setq result (cons (cons ob (cdr bindings)) result))))
         (yresult result)))

(defun cx$retrieve-number (self pattern number)
  (yloop (initial (result nil)
                  (bindings nil))
         (yfor ob in (if *hashing?
                         (cx$retrieve-hash self pattern)
                         (ob$get self 'all-obs)))
         (ywhile (< (length result) number))
         (ydo (setq bindings (ob$unify-cx pattern ob *empty-bd* self))
              (if bindings
                  (setq result (cons (cons ob (cdr bindings)) result))))
         (yresult result)))

(defun cx$leaf? (self)
  (null? (ob$get self 'children)))

;(defun cx$justificands (self ob)
;  (yloop (initial (result nil))
;         (yfor justificand in (ob$gets ob 'justifies))
;         (ydo (if (cx$true? self justificand)
;                  (setq result (cons justificand result))))
;         (yresult result)))

(defun cx$true? (self ob)
 (if *ctxt-unify-semantics?*
  (let ((top-contexts nil)
        (obs nil))
    (if (and (not (ob$get self 'pseudo-sprout?))
             (setq top-contexts (ob$gets ob 'top-context)))
        (yloop
         (initial (result nil) (found? nil))
         (yfor top-context in top-contexts)
         (yuntil (or result found?))
         (ydo
          (cond
           ((eq? top-context self) (setq result t))
           ((memq? top-context (ob$get self 'ancestors))
            (setq found? t) (setq result t)
            (yloop
             (yfor context in (cdr
                              (memq top-context
                                    (reverse (cons self
                                                   (ob$get self 'ancestors))))))
                  (ydo (cond ((memq? ob (ob$get context 'add-obs))
                             (setq result t))
                            ((memq? ob (ob$get context 'remove-obs))
                             (setq result nil))))))))
         (yresult result))
        (progn
         (setq obs (if *hashing?* (cx$retrieve-hash self ob)
                       (ob$get self 'all-obs)))
         (if *ctxt-unify-semantics?*
             (or (memq? ob obs) (mem-empty-unify? ob obs self))
             (memq? ob obs)))))
  (memq? ob (if *hashing?* (cx$retrieve-hash self ob)
                            (ob$get self 'all-obs)))))

; (if (memq? ob &remove-obs) nil <else>) for above?

(defun cx$hypothesize (self retracts asserts)
  (let ((new-context (cx$sprout self)))
    (yloop (yfor assertion in retracts)
           (ydo (cx$retract new-context assertion)))
    (yloop (yfor assertion in asserts)
           (ydo (cx$assert new-context assertion)))
    new-context))

(defun cx$generate (self)
  (format *gen-stream* "~%-----~%Contents (generate) of ~A:~%" self)
  (yloop (yfor ob in (cx$sorted-all-obs self))
         (ydo (generate ob nil self)))
  (format *gen-stream* "~&-----~%"))

(defun cx$print (self)
  (format *gate-output* "~&-----~%Contents of ~A:~%" self)
  (yloop (yfor ob in (cx$sorted-all-obs self))
         (ydo (progn (format t "#{~A: " (ob$name ob)) (ob$print ob *gate-output*) (format t "}"))
             (newline *gate-output*)))
  (format *gate-output* "-----~%")
  nil)

(defun cx$print-actions (self)
  (format *gate-output* "~&-----~%Contents (actions) of ~A:~%" self)
  (yloop (yfor ob in (cx$sorted-all-obs self))
         (ydo (if (ty$instance? ob 'action)
                  (progn
                   (ob$print ob *gate-output*)
                   (newline *gate-output*)))))
  (format *gate-output* "-----~%")
  nil)

(defun cx$print-diffs (self)
  (format *gate-output* "~&-----~%Differential contents of ~A:~%" self)
  (format *gate-output* "~&Additions:~%")
  (yloop (yfor ob in (ob$get self 'add-obs))
        (ydo (ob$print ob *gate-output*)
             (newline *gate-output*)))
  (format *gate-output* "~&Removals:~%")
  (yloop (yfor ob in (ob$get self 'remove-obs))
        (ydo (ob$print ob *gate-output*)
             (newline *gate-output*)))
  nil)

(defun cx$print-ancestors (self)
  (yloop (yfor context in (reverse (cons self (ob$get self 'ancestors))))
         (ydo (cx$print-diffs context))))

(defun cx$show-descendants (self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$show-descendants1 ob))))

(defun cx$show-descendants1 (self)
  (ob$unhide self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$show-descendants1 ob))))

(defun cx$unshow-descendants (self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$unshow-descendants1 ob))))

(defun cx$unshow-descendants1 (self)
  (ob$hide self)
  (yloop (yfor ob in (ob$get self 'children))
         (ydo (cx$unshow-descendants1 ob))))

;
; Context sensitive links
;

(ob$decl-inverses 'linked-to 'linked-to-of)
(ob$decl-inverses 'linked-from 'linked-from-of)

(defun ol-get (ob link-type dir context)
  (let ((links (ob$gets ob (if (eq? dir 'backward)
                                'linked-to-of
                                'linked-from-of)))
        (other-dir (if (eq? dir 'backward) 'linked-from
                                           'linked-to)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo (if (and (cx$true? context link)
                         (ty$instance-of? link link-type))
                    (setq result (append (ob$gets link other-dir) result))))
           (yresult result))))

(defun ol-set (from-ob link-type to-ob context)
  (cx$assert context (ob$fcreate `((quote ,link-type)
                                   linked-from ,from-ob
                                   linked-to ,to-ob))))

(defun has-link? (ob direction type context)
  (let ((links (ob$gets ob direction)))
    (any? (lambda (x) (and (cx$true? context x)
                           (ty$instance-of? x type))) links)))

; Returns forward linked obs (e.g., results)
(defun get-links (ob link-type context)
  (let ((links (ob$gets ob 'linked-from-of)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo (if (and (cx$true? context link)
                         (ty$instance-of? link link-type))
                    (setq result (append! result (list link)))))
           (yresult result))))

; Returns backward linked obs (e.g., causes)
(defun get-links-from (ob link-type context)
  (let ((links (ob$gets ob 'linked-to-of)))
    (yloop (initial (result nil))
           (yfor link in links)
           (ydo (if (and (cx$true? context link)
                         (ty$instance-of? link link-type))
                    (setq result (append! result (list link)))))
           (yresult result))))

(defun ol-path (ob1 ob2 link-type dir
                      context predicate bindings)
  (yloop (initial
         (result nil)
         (count 0)
         (next-obs (ol-get ob1 link-type dir context)))
        (yuntil
         (or result
             (if (> count *max-breadth*)
                 (progn
                  (ndbg *gate-dbg* ob-warn
                        "Exceeded max breadth in ob-link-path.~%")
                  t)
                 nil)))
        (ywhile next-obs)
        (ydo
         (yloop (yfor next-ob in next-obs)
               (yuntil result)
               (ydo (setq result
                        (if (procedure? predicate)
                            (funcall predicate ob2 next-ob)
                            (ob$unify-cx ob2 next-ob bindings context)))
                   (if result (setq result (cons next-ob (cdr result))))))
         (if (null? result)
             (setq next-obs
                  (walk-append
                   (lambda (ob) (ol-get ob link-type dir context))
                   next-obs)))
         (increment-me count))
        (yresult result)))

; End of file.
