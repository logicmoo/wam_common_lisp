;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Packagw: BABYLON -*-

;;; change the root babylon directory namestring

(defvar *babylon-root-directory* 
  #-:MCL "/home/jdaugherty/work/babylon/build-2015/"   ;;; <--- change the pathname string here!!!!
  #+:MCL (namestring
          (make-pathname :directory (pathname-directory *loading-file-source-file*))))

#+:AKCL(progn
         (load "clII/extens.cl")
         (load "clII/loop.cl")
         (load "clII/defpackage.cl")
         (use-package :defpackage :common-lisp-user))

;;;

;;;#-:CMU(declaim (optimize (speed 3)(space 1)(safety 1)(compilation-speed 0)(debug 0)))


;;;(declaim (optimize (speed 0)(space 0)(safety 3)(compilation-speed 0)(debug 3)))

;;;

(defpackage "FMCS"
  (:use #+:AKCL "LISP" #-:AKCL "COMMON-LISP" #+:MCL "CCL" #+:EXCL "EXCL" #+:SBCL "SBCL")
  (:shadow "DEFCLASS" "DEFMETHOD" "MAKE-INSTANCE" "SLOT-VALUE"
           "STANDARD-OBJECT" "STANDARD-CLASS" "SELF" "CALL-NEXT-METHOD")
  (:export "*REDEFINE-WARNINGS*" "SELF" "$SLOT" 
           "DEF$FLAVOR" "DEF$METHOD" "UNDEF$METHOD" 
           "DEF$FRAME" "DEF$BEHAVIOR"
           "TRACE$METHOD" "UNTRACE$METHOD" "IS-TRACED$METHOD"
           "COMPILE-$FLAVOR-$METHODS" 
           "DEFWHOPPER" "CONTINUE-WHOPPER" 
           "$SEND" "LEXPR-$SEND"
           "FLAVORP" "FLAVOR-INSTANCEP" "FLAVOR-TYPEP" "FLAVOR-TYPE-OF"
           "GET-FLAVOR-INSTANCE-SLOTS" "SYMBOL-VALUE-IN-$INSTANCE" 
           "MAKE-$INSTANCE" "MAKE-WINDOW-OR-INSTANCE"
           "MCS-TRACE" "MCS-UNTRACE" "MCS-IS-TRACED"))

;;;

(defpackage "BABYLON" 
  (:nicknames "BABY")
  (:use #+:AKCL "LISP" #-:AKCL "COMMON-LISP" "FMCS" #+:MCL "CCL" #+:AKCL "DEFPACKAGE") 
  ;;; COMMON-LISP       - /= < <= = > >= AND OR NOT ATOM INTEGER READ FORMAT WRITE
  ;;; CCL               CUT TRUE FALSE
  (:EXPORT 
   ;;; symbols
   "YES" "NO" "UNKNOWN" "HELP" 
   
   ;;;  knowledge base
   
   "*BABYLON-VERSION*" "*CURRENT-KNOWLEDGE-BASE*"
   "SEND-KB" "SEND-CURRENT-KNOWLEDGE-BASE"
   "SELF" "$SEND" "LEXPR-$SEND" "CC-LOAD"
   "GERMAN" "ENGLISH"
   
   "DEF-KB-CONFIGURATION" "DEF-KB-INSTANCE" "INSTRUCTIONS"
   "META-PROCESSOR-CORE" "KB-PROCESSOR-CORE"
   "LISP-MIXIN" "FREE-TEXT-MIXIN"
   
   ;;; input/output interface
   
   "BASIC-INTERFACE-MIXIN" "MINI-INTERFACE-MIXIN" "NORMAL-INTERFACE-MIXIN"
   "SAY"
   
   ;;; frame processor
   
   "BASIC-FRAME-MIXIN" "MINI-FRAME-MIXIN" "NORMAL-FRAME-MIXIN"
   "DEFFRAME" "DEFINSTANCE" "DEFBEHAVIOR" "<--" "<-" "$VALUE" "$EVAL"
   "DEFINE-POSSIBLE-VALUES-BEHAVIOR"
   "SUPERS" "SLOTS" "ACTIVE-VALUE" "WITH" "OF"
   "-" "=" ">" ">=" "<" "<=" 
   "ONE-OF" "ALL-OF" "BETWEEN"
   "IS-FRAME" "IS-INSTANCE" 
   "GET-INSTANCE" "GET-INSTANCE-LIST" "GET-ALL-INSTANCES" "SET-INSTANCE-POINTER" 
   "GET-SUPERS" "GET-ALL-SUPERS" "GET-SUBFRAMES" "GET-ALL-SUBFRAMES"
   ;;; 
   
   ;;; rule processor
   
   "BASIC-RULE-MIXIN" "MINI-RULE-MIXIN" "NORMAL-RULE-MIXIN"
   "DEFRULE-SET" "SEND-RULE"
   "$AND" "?AND" "AND" "$OR" "?OR" "OR"
   "$TRUE" "$FALSE" "$CONCLUDE" "$EXECUTE" "$ASK"
   "FIND-IMPLICATIONS"
   ;;;
   
   ;;; prolog in lisp
   
   "BASIC-PROLOG-MIXIN" "MINI-PROLOG-MIXIN" "NORMAL-PROLOG-MIXIN"
   "DEFRELATIONS" "DEFCLAUSES" "DEFAXIOM-SET"
   "CUT" "TRUE" "FALSE" "CALL" "CALLPRED" 
   ;;; AND OR already exported
   "NOT" "ONCE" "REPEAT" "BAGOF"
   "ASSERTA" "ASSERTZ" "CLAUSE" "RETRACT" "ABOLISH"
   ;;; = < > >=
   "/=" "==" "/==" "=.=" "=/=" "=<"
   "VAR" "ATOM" "INTEGER" "ATOMIC" 
   "READ" "WRITE" "FORMAT"
   "LISP" "IS"
   "FORM" "VARS" "BOUND" "STATUS" 
   "FRAME" "FRAME-DEF" "HAS-SUPER" "INSTANCE" "INSTANCE-DEF" "HAS-SLOT" "HAS-PROPERTY"
   "RULE-SET" "RULE-SET-DEF" "HAS-RULE" 
   ;;; "SATISFIED-P"
   "FREE-TEXT" 
   ;;;
   
   ;;; constraint processor
   
   "BASIC-CONSTRAINT-MIXIN" "MINI-CONSTRAINT-MIXIN" "NORMAL-CONSTRAINT-MIXIN"
   "DEFCONSTRAINT" "DEFRESTRICTION" "PRIMITIVE" "COMPOUND"
   "SATISFIED-P" "CONSTRAINED-P" "UNCONSTRAINED"
   "SATISFY"
   ;;;
   
   ))

#+:CLISP(setf *COMPILE-WARNINGS* t
	      *COMPILE-VERBOSE* t)

#+(and :MCL (not :CCL-3))(load (concatenate 'string *babylon-root-directory*  "make-mcl2.cl"))
#+(and :MCL :CCL-3)(load (concatenate 'string *babylon-root-directory*  "make-mcl3.cl"))

;;; for SUN or UNIX like platforms
#-:MCL(load (concatenate 'string *babylon-root-directory*  "make-sun.cl"))

;;; eof

