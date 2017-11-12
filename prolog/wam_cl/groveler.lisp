
(defun docinfo ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (format t "package_nickname('~a','~a').~%" pn pn)
   (dolist (nn (package-nicknames p))
     (format t "package_nickname('~a','~a').~%" pn (string-downcase nn)))
   (dolist (nn (package-use-list p))
     (format t "package_use_list('~a','~a').~%" pn (string-downcase (package-name nn))))
   (dolist (nn (package-shadowing-symbols p))
     (format t "package_shadowing_symbols('~a','~a').~%" pn nn))

 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)   
    (let (( sn (string-downcase (symbol-name sym))))
       (let ((doc (documentation sym 'function))) (when doc (format t "doc_string('~a','~a',function,~a).~%" sn pn (write-to-string doc))))
       (let ((doc (documentation sym 'variable))) (when doc (format t "doc_string('~a','~a',variable,~a).~%" sn pn (write-to-string doc))))
     (when (special-operator-p sym)
         (format t "special('~a','~a').~%" sn pn))

))))))

(docinfo)


(defun get-arglists ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (do-all-symbols (sym)    
    (when (eq (symbol-package sym) p)   
        (when (fboundp sym) 
	   (let* (( sn (string-downcase (symbol-name sym)))(sf (symbol-function sym)) 
	     (sfll (if (SYSTEM::MACROP sf) (SYSTEM::MACRO-LAMBDA-LIST sf)  (FUNCTION-LAMBDA-EXPRESSION sf))))
	  
	   (when sfll (format t "arglistss('~a','~a','~a').~%" sn pn sfll)))))
))))
(get-arglists)


(setq *print-readably* t)


#-sbcl
(defun psyminfo ()
 (dolist (p (list-all-packages))
  (when (and (not (string= "sb-c" pn))(not (string= "sb-vm" pn))(not (string= "java" pn)))
   (package-info p))))

(defun package-info (ps)
 (let ((p (find-package ps)))
  (let (( pn (string-downcase (package-name p))))
   (format t "package_nickname('~a','~a').~%" pn pn)
   (dolist (nn (package-nicknames p))
     (format t "package_nickname('~a','~a').~%" pn (string-downcase nn)))
   (dolist (nn (package-use-list p))
     (format t "package_use_list('~a','~a').~%" pn (string-downcase (package-name nn))))
   (dolist (nn (package-shadowing-symbols p))
     (format t "package_shadowing_symbols('~a','~a').~%" pn nn)
     (symbol-values nn))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)
     (symbol-values sym))))))
       
(defun symbol-info (sym)
  (setq *print-readably* t)

    (let* ((p (symbol-package sym))
          ( pn (string-downcase (package-name p)))
          ( sn (string-downcase (symbol-name sym))))
     
     '(format t "symbol_info('~a','~a',package,~(~a~)).~%" sn pn (second (multiple-value-list  (find-symbol (symbol-name sym) p))))
    (when (not (string= "keyword" pn))
      (when (boundp sym) 
       
        (if (constantp sym)
         (print `(defconstant ,sym ,(symbol-value sym)))
	 (print `(defparameter ,sym ,(symbol-value sym))))
       (handler-case  
        (let ((doc (documentation sym 'variable))) (when doc (format t "doc_string('~a','~a',variable,\"~a\").~%" sn pn (princ-to-string doc))))))
     '(when (fboundp sym) 
      (let* ((sf (symbol-function sym)) (sft (string-downcase (symbol-name (type-of sf)))))
       (when (special-operator-p sym)
         (setq sft (concatenate 'string "special-" sft)))
       (format t "symbol_info('~a','~a',function_type,'~(~a~)').~%" sn pn sft)
       (let 
         ((doc (documentation sym 'function))) 
	 (when doc (format t "doc_string('~a','~a',function,\"~a\").~%" sn pn (princ-to-string doc))))))

     '(when (symbol-plist sym) (format t "symbol_plist('~a','~a').~%" sn  (symbol-plist sym))))))

 write-to-string

(defun allpackagevalues () (dolist (p (list :cl :user :sys :EXT :clos)) (package-values p)))
(defun maybe-pprint (o) (handler-case (pprint o) (t (a)  (let ((*print-readably* nil)) (pprint o)))))
(defun package-values (ps)
  (let
   ((p (find-package ps))(kwp (or (find-package "TMP") (make-package "TMP" :use ()))))
    (do-all-symbols (sym)  
     (when (and (eq (symbol-package sym) p) (boundp sym))
      (let((*package* kwp)
           (*print-escape* t)
	   (*PRINT-LINES* nil)(*PRINT-ARRAY* t)(*PRINT-GENSYM* t)(*PRINT-CIRCLE* t)
	   (*print-readably* t))
        (fresh-line)(fresh-line)
       
         (with-output-to-string (s str) (maybe-pprint `(,(if(constantp sym) :defconstant :defparameter) ,sym ,(symbol-value sym))))
	 
(allpackagevalues)
	 
(package-values :CL)


(psyminfo)




(defun allpackagevalues () (dolist (p (list :cl :user :sys :EXT :clos)) (package-values p)))
(defun maybe-pprint (o) (handler-case (write-to-string o) (t (a)  (let ((*print-readably* nil)) (write-to-string o)))))
(defun package-values (ps)
  (let ((p (find-package ps))(kwp (or (find-package "TMP") (make-package "TMP" :use ()))))
    (do-all-symbols (sym)  
     (when (and (eq (symbol-package sym) p) (boundp sym))
      (let((*package* kwp) (*print-escape* t) (*PRINT-LINES* nil)(*PRINT-ARRAY* t)(*PRINT-GENSYM* t)(*PRINT-CIRCLE* t)
	   (*print-readably* t))(fresh-line)(fresh-line)       
         (princ (maybe-pprint `(,(if(constantp sym) :defconstant :defparameter) ,sym ,(symbol-value sym)))))))))
(allpackagevalues)




(defun symbol-values (sym)
  (setq *print-readably* t)

       (if (boundp sym)
        (if (constantp sym)
         (print `(defconstant ,sym ,(symbol-value sym)))
	 (print `(defparameter ,sym ,(symbol-value sym))))))


(symbol-values '*features*)




#+clisp
(defun get-arglists ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (do-all-symbols (sym)    
    (when (eq (symbol-package sym) p)   
        (when (fboundp sym) 
	   (let* (( sn (string-downcase (symbol-name sym)))(sf (symbol-function sym)) 
	     (sfll (if (SYSTEM::MACROP sf) (SYSTEM::MACRO-LAMBDA-LIST sf)  (FUNCTION-LAMBDA-EXPRESSION sf))))
	   (unless sfll  (setq sfll (arglist sf)))
	   (when sfll (format t "arglistss('~a','~a','~a').~%" sn pn sfll)))))
))))
#+abcl
(defun get-arglists ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (do-all-symbols (sym)    
    (when (eq (symbol-package sym) p)          
        (when (fboundp sym) 
	   (handler-case 
	   (let* (( sn (string-downcase (symbol-name sym)))(sf (symbol-function sym)) 
	     (sfll (FUNCTION-LAMBDA-EXPRESSION sf)))
	   (unless sfll  (setq sfll (arglist sf)))
	   (when sfll (format t "arglistss('~a','~a','~a').~%" sn pn sfll)))
	     (t ))))))))

(get-arglists)


(arglist #'rest)


(defun method-info (m)(list (METHOD-QUALIFIERS m)(MOP:METHOD-LAMBDA-LIST m)(MOP:METHOD-SPECIALIZERS m)(MOP:METHOD-FUNCTION m)))
(defun print_whatnot (b)(princ b))
(defun print-trip (str a b)(unless (eq a b)(princ "mop_direct('")(princ a)(princ "','")(princ str)(princ "','")(princ b )(princ "').")(terpri)))
(defun print-subclasses (root &optional pre-print)
(let ((class (typecase root (class root) (symbol (find-class root)) (t (class-of root)))))
(dolist (item (mapcar #'MOP:slot-definition-name (MOP:class-direct-slots class)))(print-trip "slot" (class-name class) item))
(dolist (item (mapcar #'class-name (MOP:class-direct-superclasses class)))(print-trip "subclass" item  (class-name class)))
(print-trip "precedance" (class-name class) (mapcar #'class-name (cdr (MOP:class-precedence-list class))))
(when pre-print (print-trip "subclass"  (class-name pre-print) (class-name class)))
(dolist (item (mapcar #'method-info (MOP:class-direct-methods class)))  (print-trip "method"  (class-name class) item ))
(dolist (item (MOP:class-direct-subclasses class))
(print-subclasses item class))))
(print-subclasses t)


(defun method-info (m)
 (list 
  (METHOD-QUALIFIERS m)
  (CLOS:METHOD-LAMBDA-LIST m)
  (CLOS:METHOD-SPECIALIZERS m)
  (CLOS:METHOD-FUNCTION m)
  (CLOS:ACCESSOR-METHOD-SLOT-DEFINITION m)
  (DOCUMENTATION m)))

(defun print_whatnot (b)(princ b))
(defun print-trip (str a b)(unless (eq a b)(princ "mop_direct('")(princ a)(princ "','")(princ str)(princ "','")(princ b )(princ "').")(terpri)))
(defun print-subclasses (root &optional pre-print)
(let ((class (typecase root
(class root) (symbol (find-class root)) (t (class-of root)))))
(dolist (item (mapcar #'sb-mop:slot-definition-name (sb-mop:class-direct-slots class)))
  (print-trip "slot" (class-name class) item))
(dolist (item (mapcar #'sb-mop:class-name (sb-mop:class-direct-superclasses class)))
  (print-trip "subclass" item  (class-name class)))
(print-trip "precedance" (class-name class) (mapcar #'sb-mop:class-name (cdr (sb-mop:compute-class-precedence-list class))))
(when pre-print (print-trip "subclass"  (class-name pre-print) (class-name class)))
; (dolist (item (mapcar #'method-info (sb-mop:class-direct-methods class)))  (print-trip "method"  (class-name class) item ))
(dolist (item (sb-mop:class-direct-subclasses class))
(print-subclasses item class))))

(print-subclasses t)



root@gitlab:/home/dmiles/logicmoo_workspace/packs_usr/armedbear_abcl# ./abcl
Armed Bear Common Lisp 1.6.0-dev
Java 1.8.0_144 Oracle Corporation
Java HotSpot(TM) 64-Bit Server VM
Low-level initialization completed in 0.28 seconds.
Startup completed in 1.826 seconds.
Loading /root/.abclrc completed in 0.003 seconds.
Type ":help" for a list of available commands.
CL-USER(1): 

(defun get-arglists ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (do-all-symbols (sym)
    (when (eq (symbol-package sym) p)
        (when (fboundp sym)
           (handler-case
           (let* (( sn (string-downcase (symbol-name sym)))(sf (symbol-function sym))
             (sfll (function-lambda-expression sf)))
           (handler-case (unless sfll  (setq sfll (arglist sf))) (t ))
           (when sfll (format t "arglistss('~a','~a','~a').~%" sn pn sfll)))
             (t ))))))))

(get-arglists)
GET-ARGLISTS
CL-USER(2): CL-USER(2): 


arglistss('%check-generic-sequence-bounds','sequence','(SEQ START END)').
arglistss('seq-dispatch','sequence','(SEQUENCE LIST-FORM ARRAY-FORM &OPTIONAL OTHER-FORM)').
arglistss('sequence-bounding-indices-bad-error','sequence','(SEQUENCE START END)').
arglistss('%set-elt','sequence','(SEQUENCE INDEX VALUE)').
arglistss('maybe-fold-let*','precompiler','(FORM)').
arglistss('precompile-let/let*-vars','precompiler','(VARS)').
arglistss('precompile-psetf','precompiler','(FORM)').
arglistss('precompile-threads-synchronized-on','precompiler','(FORM)').
arglistss('precompile-function','precompiler','(FORM)').
arglistss('find-use','precompiler','(NAME EXPRESSION)').
arglistss('precompile-defun','precompiler','(FORM)').
arglistss('precompile-case','precompiler','(FORM)').
arglistss('precompile-cons','precompiler','(FORM)').
arglistss('precompile-let*','precompiler','(FORM)').
arglistss('precompile-function-call','precompiler','(FORM)').
arglistss('precompile-the','precompiler','(FORM)').
arglistss('precompile-do/do*-vars','precompiler','(VARLIST)').
arglistss('precompile-block','precompiler','(FORM)').
arglistss('precompile-unwind-protect','precompiler','(FORM)').
arglistss('precompile-multiple-value-list','precompiler','(FORM)').
arglistss('precompile-locally','precompiler','(FORM)').
arglistss('extract-lambda-vars','precompiler','(LAMBDA-LIST)').
arglistss('precompile-when','precompiler','(FORM)').
arglistss('precompile-do-symbols','precompiler','(FORM)').
arglistss('precompile-tagbody','precompiler','(FORM)').
arglistss('precompile-function-position-lambda','precompiler','(LAMBDA ARGS)').
arglistss('precompile1','precompiler','(FORM)').
arglistss('precompile-unless','precompiler','(FORM)').
arglistss('precompile-return-from','precompiler','(FORM)').
arglistss('precompile-return','precompiler','(FORM)').
arglistss('precompile-local-function-def','precompiler','(DEF)').
arglistss('precompile-flet/labels','precompiler','(FORM)').
arglistss('precompile-lambda-list','precompiler','(FORM)').
arglistss('precompile-multiple-value-bind','precompiler','(FORM)').
arglistss('precompile-cond','precompiler','(FORM)').
arglistss('precompile-load-time-value','precompiler','(FORM)').
arglistss('precompile-if','precompiler','(FORM)').
arglistss('precompile-symbol-macrolet','precompiler','(FORM)').
arglistss('precompile-cond-clause','precompiler','(CLAUSE)').
arglistss('precompile-nth-value','precompiler','(FORM)').
arglistss('precompile-case-clause','precompiler','(CLAUSE)').
arglistss('precompile-eval-when','precompiler','(FORM)').
arglistss('precompile-setq','precompiler','(FORM)').
arglistss('precompile-local-functions','precompiler','(DEFS)').
arglistss('precompile-setf','precompiler','(FORM)').
arglistss('precompile-dolist','precompiler','(FORM)').
arglistss('precompile-lambda','precompiler','(FORM)').
arglistss('precompile-progv','precompiler','(FORM)').
arglistss('install-handler','precompiler','(SYMBOL &OPTIONAL HANDLER)').
arglistss('precompile-progn','precompiler','(FORM)').
arglistss('precompile-macrolet','precompiler','(FORM)').
arglistss('precompile-named-lambda','precompiler','(FORM)').
arglistss('precompile-truly-the','precompiler','(FORM)').
arglistss('precompile-let','precompiler','(FORM)').
arglistss('precompile-do/do*-end-form','precompiler','(END-FORM)').
arglistss('precompile-psetq','precompiler','(FORM)').
arglistss('define-function-position-lambda-transform','precompiler','(BODY-FUNCTION-NAME (ARGLIST FORM ARGS) &BODY BODY)').
arglistss('expand-macro','precompiler','(FORM)').
arglistss('precompile-dotimes','precompiler','(FORM)').
arglistss('precompile-identity','precompiler','(FORM)').
arglistss('precompile-do/do*','precompiler','(FORM)').
arglistss('precompile-form','precompiler','(FORM IN-JVM-COMPILE &OPTIONAL PRECOMPILE-ENV)').
arglistss('write-string++','xp','(STRING XP START END)').
arglistss('qkind','xp','(XP INDEX)').
arglistss('specifier-category','xp','(SPEC)').
arglistss('pretty-non-vector','xp','(XP ARRAY)').
arglistss('output-pretty-object','xp','(OBJECT STREAM)').
arglistss('function-print','xp','(XP LIST)').
arglistss('setq-print','xp','(XP OBJ)').
arglistss('bp<-tp','xp','(XP PTR)').
arglistss('maybe-initiate-xp-printing','xp','(OBJECT FN STREAM &REST ARGS)').
arglistss('up-print','xp','(XP LIST)').
arglistss('xp-print','xp','(FN STREAM ARGS)').
arglistss('section-start-line','xp','(XP)').
arglistss('defsetf-print','xp','(XP LIST)').
arglistss('section-start','xp','(XP)').
arglistss('set-prefix','xp','(XP PREFIX-STRING)').
arglistss('write-char++','xp','(CHAR XP)').
arglistss('non-pretty-print','xp','(OBJECT S)').
arglistss('output-width','xp','(&OPTIONAL (S *STANDARD-OUTPUT*))').
arglistss('qtype','xp','(XP INDEX)').
arglistss('mvb-print','xp','(XP LIST)').
arglistss('bind-list','xp','(XP LIST &REST ARGS)').
arglistss('qoffset','xp','(XP INDEX)').
arglistss('force-some-output','xp','(XP)').
arglistss('priority->','xp','(X Y)').
arglistss('output-line','xp','(XP QENTRY)').
arglistss('print-fancy-fn-call','xp','(XP LIST TEMPLATE)').
arglistss('defun-like','xp','(XP LIST &REST ARGS)').
arglistss('write+','xp','(OBJECT XP)').
arglistss('token-type','xp','(TOKEN &AUX STRING)').
arglistss('pretty-array','xp','(XP ARRAY)').
arglistss('convert-body','xp','(SPEC)').
arglistss('cond-print','xp','(XP OBJ)').
arglistss('pprint-tab+','xp','(KIND COLNUM COLINC XP)').
arglistss('write-char+','xp','(CHAR XP)').
arglistss('do-xp-printing','xp','(FN STREAM ARGS)').
arglistss('check-block-abbreviation','xp','(XP ARGS CIRCLE-CHECK?)').
arglistss('misering?','xp','(XP)').
arglistss('lp<-bp','xp','(XP &OPTIONAL (PTR NIL))').
arglistss('specifier-fn','xp','(SPEC)').
arglistss('bp<-lp','xp','(XP PTR)').
arglistss('dmm-print','xp','(XP LIST)').
arglistss('flush','xp','(XP)').
arglistss('always-true','xp','(X)').
arglistss('write-string+++','xp','(STRING XP START END)').
arglistss('adjust-counts','xp','(TABLE PRIORITY DELTA)').
arglistss('pprint-logical-block+','xp','((VAR ARGS PREFIX SUFFIX PER-LINE? CIRCLE-CHECK? ATSIGN?) &BODY BODY)').
arglistss('pprint-indent+','xp','(KIND N XP)').
arglistss('non-blank-prefix-ptr','xp','(XP)').
arglistss('lp<-tp','xp','(XP PTR)').
arglistss('do-print','xp','(XP OBJ)').
arglistss('reverse-string-in-place','xp','(STRING START END)').
arglistss('maybe-print-fast','xp','(OBJECT XP)').
arglistss('quote-print','xp','(XP LIST)').
arglistss('start-block','xp','(XP PREFIX ON-EACH-LINE? SUFFIX)').
arglistss('maybelab','xp','(XP ITEM &REST ARGS)').
arglistss('qend','xp','(XP INDEX)').
arglistss('push-block-stack','xp','(XP)').
arglistss('qdepth','xp','(XP INDEX)').
arglistss('fn-call','xp','(XP LIST)').
arglistss('let-print','xp','(XP OBJ)').
arglistss('suffix-ptr','xp','(XP)').
arglistss('make-pprint-dispatch-table','xp','(&KEY ((CONSES-WITH-CARS G210138) (MAKE-HASH-TABLE TEST (FUNCTION EQ))) ((STRUCTURES G210139) (MAKE-HASH-TABLE TEST (FUNCTION EQ))) ((OTHERS G210140) NIL))').
arglistss('prog-print','xp','(XP LIST)').
arglistss('xp-structure-p','xp','(OBJECT)').
arglistss('attempt-to-output','xp','(XP FORCE-NEWLINES? FLUSH-OUT?)').
arglistss('pretty-loop','xp','(XP LOOP)').
arglistss('check-size','xp','(XP VECT PTR)').
arglistss('maybe-too-large','xp','(XP QENTRY)').
arglistss('array-readably-printable-p','xp','(ARRAY)').
arglistss('fits','xp','(OBJ ENTRY)').
arglistss('pop-block-stack','xp','(XP)').
arglistss('alternative-fn-call','xp','(XP LIST)').
arglistss('make-entry','xp','(&KEY ((TEST G210560) NIL) ((FN G210561) NIL) ((FULL-SPEC G210562) NIL))').
arglistss('set-pprint-dispatch+','xp','(TYPE-SPECIFIER FUNCTION PRIORITY TABLE)').
arglistss('pprint-dispatch-table-p','xp','(OBJECT)').
arglistss('copy-entry','xp','(structure)').
arglistss('entry-p','xp','(OBJECT)').
arglistss('push-prefix-stack','xp','(XP)').
arglistss('function-call-p','xp','(X)').
arglistss('qpos','xp','(XP INDEX)').
arglistss('make-xp-structure','xp','(&KEY ((BASE-STREAM G211882) NIL) ((LINE-LENGTH G211883) NIL) ((LINE-LIMIT G211884) NIL) ((LINE-NO G211885) NIL) ((DEPTH-IN-BLOCKS G211886) NIL) ((BLOCK-STACK G211887) (MAKE-ARRAY 35)) ((BLOCK-STACK-PTR G211888) NIL) ((BUFFER G211889) (MAKE-ARRAY 256 ELEMENT-TYPE (QUOTE CHARACTER))) ((CHARPOS G211890) NIL) ((BUFFER-PTR G211891) NIL) ((BUFFER-OFFSET G211892) NIL) ((QUEUE G211893) (MAKE-ARRAY 525)) ((QLEFT G211894) NIL) ((QRIGHT G211895) NIL) ((PREFIX G211896) (MAKE-ARRAY 256 ELEMENT-TYPE (QUOTE CHARACTER))) ((PREFIX-STACK G211897) (MAKE-ARRAY 150)) ((PREFIX-STACK-PTR G211898) NIL) ((SUFFIX G211899) (MAKE-ARRAY 256 ELEMENT-TYPE (QUOTE CHARACTER))))').
arglistss('qarg','xp','(XP INDEX)').
arglistss('block-like','xp','(XP LIST &REST ARGS)').
arglistss('qnext','xp','(INDEX)').
arglistss('tagbody-print','xp','(XP LIST)').
arglistss('pop-prefix-stack','xp','(XP)').
arglistss('flet-print','xp','(XP OBJ)').
arglistss('write-string+','xp','(STRING XP START END)').
arglistss('setup-for-next-line','xp','(XP QENTRY)').
arglistss('copy-xp-structure','xp','(structure)').
arglistss('set-indentation-prefix','xp','(XP NEW-POSITION)').
arglistss('pprint-dispatch-print','xp','(XP TABLE)').
arglistss('initial-prefix-ptr','xp','(XP)').
arglistss('set-suffix','xp','(XP SUFFIX-STRING)').
arglistss('enqueue','xp','(XP TYPE KIND &OPTIONAL ARG)').
arglistss('get-printer','xp','(OBJECT TABLE)').
arglistss('initialize-xp','xp','(XP STREAM)').
arglistss('tp<-bp','xp','(XP)').
arglistss('end-block','xp','(XP SUFFIX)').
arglistss('print-fixnum','xp','(XP FIXNUM)').
arglistss('prefix-ptr','xp','(XP)').
arglistss('structure-type-p','xp','(X)').
arglistss('pprint-newline+','xp','(KIND XP)').
arglistss('pprint-pop+','xp','(ARGS XP)').
arglistss('pretty-vector','xp','(XP V)').
arglistss('pprint-pop-check+','xp','(ARGS XP)').
arglistss('format-dollars','format','(STREAM NUMBER D N W PAD COLON ATSIGN)').
arglistss(')-format-directive-interpreter','format','(STREAM G231394 G231395 ORIG-ARGS ARGS)').
arglistss('t-format-directive-expander','format','(G225096 G225095)').
arglistss('expand-format-integer','format','(BASE COLONP ATSIGNP PARAMS)').
arglistss('format-justification','format','(STREAM NEWLINE-PREFIX EXTRA-SPACE LINE-LEN STRINGS PAD-LEFT PAD-RIGHT MINCOL COLINC MINPAD PADCHAR)').
arglistss('expand-format-justification','format','(SEGMENTS COLONP ATSIGNP FIRST-SEMI PARAMS)').
arglistss('d-format-directive-interpreter','format','(STREAM G228147 G228146 ORIG-ARGS ARGS)').
arglistss('format-fixed-aux','format','(STREAM NUMBER W D K OVF PAD ATSIGN)').
arglistss('format-exponential','format','(STREAM NUMBER W D E K OVF PAD MARKER ATSIGN)').
arglistss('s-format-directive-expander','format','(G223651 G223650)').
arglistss('format-absolute-tab','format','(STREAM COLNUM COLINC)').
arglistss('output-spaces','format','(STREAM N)').
arglistss('make-format-directive','format','(&KEY ((STRING G221768) (MISSING-ARG)) ((START G221769) (MISSING-ARG)) ((END G221770) (MISSING-ARG)) ((CHARACTER G221771) (MISSING-ARG)) ((COLONP G221772) NIL) ((ATSIGNP G221773) NIL) ((PARAMS G221774) NIL))').
arglistss('add-fill-style-newlines','format','(LIST STRING OFFSET &OPTIONAL LAST-DIRECTIVE)').
arglistss('format-exp-aux','format','(STREAM NUMBER W D E K OVF PAD MARKER ATSIGN)').
arglistss('expand-format-logical-block','format','(PREFIX PER-LINE-P INSIDES SUFFIX ATSIGNP)').
arglistss('r-format-directive-expander','format','(G224094 G224093)').
arglistss('_-format-directive-interpreter','format','(STREAM G230944 G230943 ORIG-ARGS ARGS)').
arglistss('{-format-directive-interpreter','format','(STREAM G231726 DIRECTIVES ORIG-ARGS ARGS)').
arglistss('expand-bind-defaults','format','(SPECS PARAMS &BODY BODY)').
arglistss('e-format-directive-interpreter','format','(STREAM G229462 G229461 ORIG-ARGS ARGS)').
arglistss('def-format-directive','format','(CHAR LAMBDA-LIST &BODY BODY)').
arglistss('<-format-directive-expander','format','(G226361 DIRECTIVES)').
arglistss('copy-format-directive','format','(structure)').
arglistss('w-format-directive-expander','format','(G223810 G223809)').
arglistss('find-directive','format','(DIRECTIVES KIND STOP-AT-SEMI)').
arglistss('x-format-directive-interpreter','format','(STREAM G228549 G228548 ORIG-ARGS ARGS)').
arglistss('>-format-directive-expander','format','(G226436 G226437)').
arglistss('expand-directive-list','format','(DIRECTIVES)').
arglistss('interpret-format-integer','format','(BASE)').
arglistss('illegal-inside-justification-p','format','(DIRECTIVE)').
arglistss('%compiler-walk-format-string','format','(STRING ARGS)').
arglistss('format-exponent-marker','format','(NUMBER)').
arglistss('format-princ','format','(STREAM ARG COLONP ATSIGNP MINCOL COLINC MINPAD PADCHAR)').
arglistss('f-format-directive-interpreter','format','(STREAM G229208 G229207 ORIG-ARGS ARGS)').
arglistss('i-format-directive-interpreter','format','(STREAM G231000 G230999 ORIG-ARGS ARGS)').
arglistss('format-print-roman','format','(STREAM N)').
arglistss('w-format-directive-interpreter','format','(STREAM G227972 G227971 ORIG-ARGS ARGS)').
arglistss('format-print-old-roman','format','(STREAM N)').
arglistss('%format','format','(STREAM STRING-OR-FUN ORIG-ARGS &OPTIONAL (ARGS ORIG-ARGS))').
arglistss('s-format-directive-interpreter','format','(STREAM G227745 G227744 ORIG-ARGS ARGS)').
arglistss('o-format-directive-expander','format','(G224024 G224023)').
arglistss('extract-user-fun-name','format','(STRING START END)').
arglistss('a-format-directive-expander','format','(G223545 G223544)').
arglistss('b-format-directive-interpreter','format','(STREAM G228281 G228280 ORIG-ARGS ARGS)').
arglistss('^-format-directive-interpreter','format','(STREAM G231605 G231604 ORIG-ARGS ARGS)').
arglistss('format-print-small-cardinal','format','(STREAM N)').
arglistss('format-prin1','format','(STREAM ARG COLONP ATSIGNP MINCOL COLINC MINPAD PADCHAR)').
arglistss('interpret-directive-list','format','(STREAM DIRECTIVES ORIG-ARGS ARGS)').
arglistss('format-print-ordinal','format','(STREAM N)').
arglistss('/-format-directive-interpreter','format','(STREAM G232478 G232477 ORIG-ARGS ARGS)').
arglistss('o-format-directive-interpreter','format','(STREAM G228415 G228414 ORIG-ARGS ARGS)').
arglistss('format-print-integer','format','(STREAM NUMBER PRINT-COMMAS-P PRINT-SIGN-P RADIX MINCOL PADCHAR COMMACHAR COMMAINTERVAL)').
arglistss('/-format-directive-expander','format','(G226944 G226943)').
arglistss('}-format-directive-expander','format','(G226290 G226291)').
arglistss('<-format-directive-interpreter','format','(STREAM G231966 DIRECTIVES ORIG-ARGS ARGS)').
arglistss('d-format-directive-expander','format','(G223954 G223953)').
arglistss('parse-conditional-directive','format','(DIRECTIVES)').
arglistss('$-format-directive-expander','format','(G224655 G224654)').
arglistss('interpret-format-logical-block','format','(STREAM ORIG-ARGS ARGS PREFIX PER-LINE-P INSIDES SUFFIX ATSIGNP)').
arglistss('expander-pprint-next-arg','format','(STRING OFFSET)').
arglistss('expander-next-arg','format','(STRING OFFSET)').
arglistss(']-format-directive-expander','format','(G225951 G225952)').
arglistss('format-directive-p','format','(OBJECT)').
arglistss('f-format-directive-expander','format','(G224287 G224286)').
arglistss('format-print-cardinal-aux','format','(STREAM N PERIOD ERR)').
arglistss('*-format-directive-expander','format','(G225356 G225355)').
arglistss('~-format-directive-expander','format','(G224973 G224972)').
arglistss('tokenize-control-string','format','(STRING)').
arglistss('expand-true-false-conditional','format','(TRUE FALSE)').
arglistss('g-format-directive-expander','format','(G224526 G224525)').
arglistss('a-format-directive-interpreter','format','(STREAM G227590 G227589 ORIG-ARGS ARGS)').
arglistss('(-format-directive-expander','format','(G225557 DIRECTIVES)').
arglistss('i-format-directive-expander','format','(G225284 G225283)').
arglistss('&-format-directive-interpreter','format','(STREAM G230383 G230382 ORIG-ARGS ARGS)').
arglistss('parse-directive','format','(STRING START)').
arglistss(')-format-directive-expander','format','(G225623 G225624)').
arglistss('|-format-directive-expander','format','(G224901 G224900)').
arglistss('%-format-directive-interpreter','format','(STREAM G230295 G230294 ORIG-ARGS ARGS)').
arglistss('interpret-format-justification','format','(STREAM ORIG-ARGS ARGS SEGMENTS COLONP ATSIGNP FIRST-SEMI PARAMS)').
arglistss('[-format-directive-interpreter','format','(STREAM G231424 DIRECTIVES ORIG-ARGS ARGS)').
arglistss('}-format-directive-interpreter','format','(STREAM G231936 G231937 ORIG-ARGS ARGS)').
arglistss('c-format-directive-expander','format','(G223757 G223756)').
arglistss('newline-format-directive-expander','format','(G225044 DIRECTIVES)').
arglistss('%formatter','format','(CONTROL-STRING)').
arglistss('^-format-directive-expander','format','(G225982 G225981)').
arglistss('~-format-directive-interpreter','format','(STREAM G230562 G230561 ORIG-ARGS ARGS)').
arglistss('def-complex-format-directive','format','(CHAR LAMBDA-LIST &BODY BODY)').
arglistss('format-general','format','(STREAM NUMBER W D E K OVF PAD MARKER ATSIGN)').
arglistss('e-format-directive-expander','format','(G224396 G224395)').
arglistss('format-add-commas','format','(STRING COMMACHAR COMMAINTERVAL)').
arglistss('g-format-directive-interpreter','format','(STREAM G229833 G229832 ORIG-ARGS ARGS)').
arglistss('%-format-directive-expander','format','(G224754 G224753)').
arglistss('$-format-directive-interpreter','format','(STREAM G230098 G230097 ORIG-ARGS ARGS)').
arglistss('format-write-field','format','(STREAM STRING MINCOL COLINC MINPAD PADCHAR PADLEFT)').
arglistss('p-format-directive-interpreter','format','(STREAM G229107 G229106 ORIG-ARGS ARGS)').
arglistss('|-format-directive-interpreter','format','(STREAM G230472 G230471 ORIG-ARGS ARGS)').
arglistss('?-format-directive-interpreter','format','(STREAM G231230 G231229 ORIG-ARGS ARGS)').
arglistss('expand-directive','format','(DIRECTIVE MORE-DIRECTIVES)').
arglistss('[-format-directive-expander','format','(G225653 DIRECTIVES)').
arglistss('?-format-directive-expander','format','(G225486 G225485)').
arglistss('parse-format-logical-block','format','(SEGMENTS COLONP FIRST-SEMI CLOSE PARAMS STRING END)').
arglistss('newline-format-directive-interpreter','format','(STREAM G230650 DIRECTIVES ORIG-ARGS ARGS)').
arglistss(']-format-directive-interpreter','format','(STREAM G231574 G231575 ORIG-ARGS ARGS)').
arglistss('t-format-directive-interpreter','format','(STREAM G230705 G230704 ORIG-ARGS ARGS)').
arglistss('format-print-cardinal','format','(STREAM N)').
arglistss('p-format-directive-expander','format','(G224215 G224214)').
arglistss('c-format-directive-interpreter','format','(STREAM G227883 G227882 ORIG-ARGS ARGS)').
arglistss('format-relative-tab','format','(STREAM COLREL COLINC)').
arglistss('format-print-named-character','format','(CHAR STREAM)').
arglistss('%set-format-directive-expander','format','(CHAR FN)').
arglistss('x-format-directive-expander','format','(G224059 G224058)').
arglistss('format-fixed','format','(STREAM NUMBER W D K OVF PAD ATSIGN)').
arglistss('_-format-directive-expander','format','(G225229 G225228)').
arglistss('%set-format-directive-interpreter','format','(CHAR FN)').
arglistss('*-format-directive-interpreter','format','(STREAM G231074 G231073 ORIG-ARGS ARGS)').
arglistss('parse-format-justification','format','(DIRECTIVES)').
arglistss('add-fill-style-newlines-aux','format','(LITERAL STRING OFFSET)').
arglistss('b-format-directive-expander','format','(G223989 G223988)').
arglistss('&-format-directive-expander','format','(G224826 G224825)').
arglistss(';-format-directive-interpreter','format','(STREAM G231544 G231545 ORIG-ARGS ARGS)').
arglistss('(-format-directive-interpreter','format','(STREAM G231325 DIRECTIVES ORIG-ARGS ARGS)').
arglistss('expand-maybe-conditional','format','(SUBLIST)').
arglistss('{-format-directive-expander','format','(G226085 DIRECTIVES)').
arglistss(';-format-directive-expander','format','(G225921 G225922)').
arglistss('%print-format-error','format','(CONDITION STREAM)').
arglistss('r-format-directive-interpreter','format','(STREAM G228683 G228682 ORIG-ARGS ARGS)').
arglistss('expand-control-string','format','(STRING)').
arglistss('decimal-string','format','(N)').
arglistss('format-general-aux','format','(STREAM NUMBER W D E K OVF PAD MARKER ATSIGN)').
arglistss('expand-next-arg','format','(&OPTIONAL OFFSET)').
arglistss('object-wait','threads','(object &optional timeout)').
arglistss('object-notify-all','threads','(object)').
arglistss('thread-join','threads','(thread)').
arglistss('object-notify','threads','(object)').
arglistss('interrupt-thread','threads','(thread function &rest args)').
arglistss('make-thread','threads','(function &key name)').
arglistss('thread-alive-p','threads','(thread)').
arglistss('compiler-let','lisp','(BINDINGS &BODY FORMS &ENVIRONMENT ENV)').
arglistss('%find-java-class','java','(class-name-or-class)').
arglistss('%add-to-classpath','java','(jar-or-jars &optional (classloader (get-current-classloader)))').
arglistss('%register-java-class','java','(jclass class-metaobject)').
arglistss('ensure-java-object','java','(obj)').
arglistss('make-classloader','java','(&optional parent)').
arglistss('make-immediate-object','java','(object &optional type)').
arglistss('describe-java-object','java','(object stream)').
arglistss('jarray-ref-raw','java','(java-array &rest indices)').
arglistss('jcoerce','java','(object intended-class)').
arglistss('unregister-java-exception','java','(exception-name)').
arglistss('jstatic','java','(method class &rest args)').
arglistss('jnew','java','(constructor &rest args)').
arglistss('jclass','java','(name-or-class-ref &optional class-loader)').
arglistss('jconstructor','java','(class-ref &rest parameter-class-refs)').
arglistss('jmethod','java','(class-ref method-name &rest parameter-class-refs)').
arglistss('jarray-set','java','(java-array new-value &rest indices)').
arglistss('jcall-raw','java','(method-ref instance &rest args)').
arglistss('jobject-lisp-value','java','(java-object)').
arglistss('jarray-ref','java','(java-array &rest indices)').
arglistss('jfield-raw','java','(class-ref-or-field field-or-instance &optional instance value)').
arglistss('jnull-ref-p','java','(object)').
arglistss('dump-classpath','java','(&optional classloader)').
arglistss('jnew-array','java','(element-type &rest dimensions)').
arglistss('jstatic-raw','java','(method class &rest args)').
arglistss('jcall','java','(method-ref instance &rest args)').
arglistss('jrun-exception-protected','java','(closure)').
arglistss('jfield','java','(class-ref-or-field field-or-instance &optional instance value)').
arglistss('register-java-exception','java','(exception-name condition-symbol)').
arglistss('jresolve-method','java','(method-name instance &rest args)').
arglistss('java-object-p','java','(object)').
arglistss('precompile','extensions','(NAME &OPTIONAL DEFINITION)').
arglistss('collect-normal-expander','extensions','(N-VALUE FUN FORMS)').
arglistss('set-url-pathname-authority','extensions','(P V)').
arglistss('set-url-pathname-scheme','extensions','(P V)').
arglistss('collect-list-expander','extensions','(N-VALUE N-TAIL FORMS)').
arglistss('set-url-pathname-fragment','extensions','(P V)').
arglistss('set-url-pathname-query','extensions','(P V)').
arglistss('%invalidate-namestring','extensions','(pathname)').
arglistss('set-floating-point-modes','extensions','(&key traps)').
arglistss('resolve','extensions','(symbol)').
arglistss('autoload-ref-p','extensions','(symbol)').
arglistss('%cdr','extensions','(X)').
arglistss('url-pathname-scheme','extensions','(P)').
arglistss('url-pathname-query','extensions','(P)').
arglistss('memql','extensions','(item list)').
arglistss('getenv','extensions','(variable)').
arglistss('adjoin-eql','extensions','(item list)').
arglistss('charpos','extensions','(STREAM)').
arglistss('file-directory-p','extensions','(pathspec &key (wild-error-p t))').
arglistss('string-find','extensions','(char string)').
arglistss('arglist','extensions','(extended-function-designator)').
arglistss('make-temp-file','extensions','(&key prefix suffix)').
arglistss('add-package-local-nickname','extensions','(LOCAL-NICKNAME ACTUAL-PACKAGE &OPTIONAL (PACKAGE-DESIGNATOR *PACKAGE*))').
arglistss('remove-package-local-nickname','extensions','(old-nickname &optional package-designator)').
arglistss('url-pathname-authority','extensions','(P)').
arglistss('show-restarts','extensions','(RESTARTS STREAM)').
arglistss('%car','extensions','(X)').
arglistss('neq','extensions','(obj1 obj2)').
arglistss('package-local-nicknames','extensions','(package)').
arglistss('autoload-setf-expander','extensions','(symbol-or-symbols filename)').
arglistss('quit','extensions','(&key status)').
arglistss('collect','extensions','(COLLECTIONS &BODY BODY)').
arglistss('featurep','extensions','(FORM)').
arglistss('autoload-setf-function','extensions','(symbol-or-symbols filename)').
arglistss('autoload','extensions','(symbol-or-symbols &optional filename)').
arglistss('%caddr','extensions','(X)').
arglistss('macroexpand-all','extensions','(FORM &OPTIONAL ENV)').
arglistss('cancel-finalization','extensions','(object)').
arglistss('source-pathname','extensions','(symbol)').
arglistss('probe-directory','extensions','(pathspec)').
arglistss('package-locally-nicknamed-by-list','extensions','(package)').
arglistss('exit','extensions','(&key status)').
arglistss('%cadr','extensions','(X)').
arglistss('precompile','extensions','(NAME &OPTIONAL DEFINITION)').
arglistss('getenv-all','extensions','(variable)').
arglistss('pathname-url-p','extensions','(pathname)').
arglistss('memq','extensions','(item list)').
arglistss('finalize','extensions','(object function)').
arglistss('url-pathname-fragment','extensions','(P)').
arglistss('autoloadp','extensions','(symbol)').
arglistss('style-warn','extensions','(FORMAT-CONTROL &REST FORMAT-ARGUMENTS)').
arglistss('precompile','extensions','(NAME &OPTIONAL DEFINITION)').
arglistss('process-cmd','top-level','(FORM)').
arglistss('print-frame','top-level','(FRAME STREAM &KEY PREFIX)').
arglistss('peek-char-non-whitespace','top-level','(STREAM)').
arglistss('entry-abbreviation','top-level','(ENTRY)').
arglistss('cload-command','top-level','(ARGS)').
arglistss('pad','top-level','(STRING WIDTH)').
arglistss('frame-command','top-level','(ARGS)').
arglistss('ls-command','top-level','(ARGS)').
arglistss('entry-command','top-level','(ENTRY)').
arglistss('entry-name','top-level','(ENTRY)').
arglistss('tokenize','top-level','(STRING)').
arglistss('pwd-command','top-level','(IGNORED)').
arglistss('ld-command','top-level','(ARGS)').
arglistss('cd-command','top-level','(ARGS)').
arglistss('error-command','top-level','(IGNORED)').
arglistss('repl-prompt-fun','top-level','(STREAM)').
arglistss('help-command','top-level','(&OPTIONAL IGNORED)').
arglistss('repl','top-level','(&OPTIONAL (IN *STANDARD-INPUT*) (OUT *STANDARD-OUTPUT*))').
arglistss('find-command','top-level','(STRING)').
arglistss('reset-command','top-level','(IGNORED)').
arglistss('istep-command','top-level','(ARGS)').
arglistss('repl-read-form-fun','top-level','(IN OUT)').
arglistss('apropos-command','top-level','(ARGS)').
arglistss('cf-command','top-level','(ARGS)').
arglistss('describe-command','top-level','(ARGS)').
arglistss('rq-command','top-level','(ARGS)').
arglistss('inspect-command','top-level','(ARGS)').
arglistss('trace-command','top-level','(ARGS)').
arglistss('continue-command','top-level','(ARGS)').
arglistss('untrace-command','top-level','(ARGS)').
arglistss('entry-help','top-level','(ENTRY)').
arglistss('backtrace-command','top-level','(ARGS)').
arglistss('package-command','top-level','(ARGS)').
arglistss('exit-command','top-level','(IGNORED)').
arglistss('read-cmd','top-level','(STREAM)').
arglistss('macroexpand-command','top-level','(ARGS)').
arglistss('%help-command','top-level','(PREFIX)').
arglistss('std-class-p','mop','(CLASS)').
arglistss('around-method-p','mop','(METHOD)').
arglistss('required-portion','mop','(GF ARGS)').
arglistss('make-direct-slot-definition','mop','(CLASS &REST ARGS)').
arglistss('compute-method-function','mop','(LAMBDA-EXPRESSION)').
arglistss('check-initargs','mop','(GF-LIST ARGS INSTANCE SHARED-INITIALIZE-PARAM INITARGS CACHE CALL-SITE)').
arglistss('calculate-allowable-initargs','mop','(GF-LIST ARGS INSTANCE SHARED-INITIALIZE-PARAM INITARGS)').
arglistss('canonicalize-direct-slot','mop','(SPEC)').
arglistss('std-compute-effective-slot-definition','mop','(CLASS NAME DIRECT-SLOTS)').
arglistss('extract-optional-part','mop','(LAMBDA-LIST)').
arglistss('long-method-combination-generic-function-symbol','mop','(METHOD-COMBINATION)').
arglistss('define-long-form-method-combination','mop','(NAME LAMBDA-LIST METHOD-GROUP-SPECS &REST ARGS)').
arglistss('slow-method-lookup','mop','(GF ARGS)').
arglistss('std-find-method-combination','mop','(GF NAME OPTIONS)').
arglistss('allow-other-keys','mop','(LAMBDA-LIST)').
arglistss('%make-long-method-combination','mop','(&KEY NAME DOCUMENTATION LAMBDA-LIST METHOD-GROUP-SPECS ARGS-LAMBDA-LIST GENERIC-FUNCTION-SYMBOL FUNCTION ARGUMENTS DECLARATIONS FORMS)').
arglistss('canonicalize-method-group-spec','mop','(SPEC)').
arglistss('funcallable-instance-function','mop','(funcallable-instance)').
arglistss('declarationp','mop','(EXPR)').
arglistss('with-method-groups','mop','(METHOD-GROUP-SPECS METHODS-FORM &BODY FORMS)').
arglistss('expand-short-defcombin','mop','(WHOLE)').
arglistss('compute-method-fast-function','mop','(LAMBDA-EXPRESSION)').
arglistss('%set-slot-value','mop','(OBJECT SLOT-NAME NEW-VALUE)').
arglistss('check-method-lambda-list','mop','(NAME METHOD-LAMBDA-LIST GF-LAMBDA-LIST)').
arglistss('std-compute-slots','mop','(CLASS)').
arglistss('init-slot-definition','mop','(SLOT &KEY NAME (INITARGS NIL) (INITFORM NIL) (INITFUNCTION NIL) (READERS NIL) (WRITERS NIL) (ALLOCATION INSTANCE) (ALLOCATION-CLASS NIL) (TYPE T) (DOCUMENTATION NIL))').
arglistss('%method-function','mop','(METHOD)').
arglistss('extract-specified-part','mop','(KEY LAMBDA-LIST)').
arglistss('define-class->%class-forwarder','mop','(NAME)').
arglistss('canonicalize-direct-superclass-list','mop','(CLASS DIRECT-SUPERCLASSES)').
arglistss('canonicalize-defgeneric-options','mop','(OPTIONS)').
arglistss('std-finalize-inheritance','mop','(CLASS)').
arglistss('after-method-p','mop','(METHOD)').
arglistss('make-initfunction','mop','(INITFORM)').
arglistss('std-method-generic-function','mop','(METHOD)').
arglistss('topological-sort','mop','(ELEMENTS CONSTRAINTS TIE-BREAKER)').
arglistss('slot-definition-dispatch','mop','(SLOT-DEFINITION STD-FORM GENERIC-FORM)').
arglistss('short-method-combination-identity-with-one-argument','mop','(METHOD-COMBINATION)').
arglistss('mapappend','mop','(FUN &REST ARGS)').
arglistss('long-method-combination-lambda-list','mop','(METHOD-COMBINATION)').
arglistss('define-funcallable-primordial-class','mop','(NAME SUPERCLASSES DIRECT-SLOTS)').
arglistss('instance-slot-location','mop','(INSTANCE SLOT-NAME)').
arglistss('extract-lambda-list-keywords','mop','(LAMBDA-LIST)').
arglistss('parse-defmethod','mop','(ARGS)').
arglistss('canonicalize-specializers','mop','(SPECIALIZERS)').
arglistss('sub-specializer-p','mop','(C1 C2 C-ARG)').
arglistss('maybe-finalize-class-subtree','mop','(CLASS)').
arglistss('extract-required-part','mop','(LAMBDA-LIST)').
arglistss('long-form-method-combination-args','mop','(ARGS)').
arglistss('long-method-combination-function','mop','(METHOD-COMBINATION)').
arglistss('local-precedence-ordering','mop','(CLASS)').
arglistss('slot-location','mop','(CLASS SLOT-NAME)').
arglistss('primary-method-p','mop','(METHOD)').
arglistss('long-method-combination-forms','mop','(METHOD-COMBINATION)').
arglistss('long-method-combination-declarations','mop','(METHOD-COMBINATION)').
arglistss('%method-generic-function','mop','(METHOD)').
arglistss('canonicalize-defclass-option','mop','(OPTION)').
arglistss('wrap-emfun-for-keyword-args-check','mop','(GF EMFUN NON-KEYWORD-ARGS APPLICABLE-KEYWORDS)').
arglistss('std-method-fast-function','mop','(METHOD)').
arglistss('check-duplicate-slots','mop','(SLOTS)').
arglistss('sort-methods','mop','(METHODS GF REQUIRED-CLASSES)').
arglistss('atomic-defgeneric','mop','(FUNCTION-NAME &REST REST)').
arglistss('long-method-combination-arguments','mop','(METHOD-COMBINATION)').
arglistss('mapplist','mop','(FUN X)').
arglistss('find-slot-definition','mop','(CLASS SLOT-NAME)').
arglistss('std-compute-effective-method','mop','(GF METHOD-COMBINATION METHODS)').
arglistss('long-method-combination-method-group-specs','mop','(METHOD-COMBINATION)').
arglistss('assert-unambiguous-method-sorting','mop','(GROUP-NAME METHODS)').
arglistss('method-applicable-p','mop','(METHOD ARGS)').
arglistss('method-combination-name','mop','(METHOD-COMBINATION)').
arglistss('std-generic-function-p','mop','(GF)').
arglistss('std-method-more-specific-p','mop','(METHOD1 METHOD2 REQUIRED-CLASSES ARGUMENT-PRECEDENCE-ORDER)').
arglistss('allocate-funcallable-instance','mop','(CLASS)').
arglistss('push-on-end','mop','(VALUE LOCATION)').
arglistss('canonicalize-direct-slots','mop','(DIRECT-SLOTS)').
arglistss('collect-superclasses*','mop','(CLASS)').
arglistss('std-compute-discriminating-function','mop','(GF)').
arglistss('get-keyword-from-arg','mop','(ARG)').
arglistss('std-method-function','mop','(METHOD)').
arglistss('make-instance-standard-generic-function','mop','(GENERIC-FUNCTION-CLASS &KEY NAME LAMBDA-LIST (METHOD-CLASS +THE-STANDARD-METHOD-CLASS+) (METHOD-COMBINATION +THE-STANDARD-METHOD-COMBINATION+) ARGUMENT-PRECEDENCE-ORDER DECLARATIONS DOCUMENTATION)').
arglistss('method-combination-documentation','mop','(METHOD-COMBINATION)').
arglistss('merge-initargs-sets','mop','(LIST1 LIST2)').
arglistss('collect-eql-specializer-objects','mop','(GENERIC-FUNCTION)').
arglistss('argument-precedence-order-indices','mop','(APO REQ)').
arglistss('before-method-p','mop','(METHOD)').
arglistss('make-instance-standard-accessor-method','mop','(METHOD-CLASS &KEY LAMBDA-LIST QUALIFIERS SPECIALIZERS DOCUMENTATION FUNCTION FAST-FUNCTION SLOT-DEFINITION)').
arglistss('canonicalize-specializer','mop','(SPECIALIZER)').
arglistss('compute-applicable-keywords','mop','(GF APPLICABLE-METHODS)').
arglistss('parse-define-method-combination-args-lambda-list','mop','(LAMBDA-LIST)').
arglistss('finalize-standard-generic-function','mop','(GF)').
arglistss('method-combination-type-lambda-with-args-emf','mop','(&KEY ARGS-LAMBDA-LIST GENERIC-FUNCTION-SYMBOL FORMS &ALLOW-OTHER-KEYS)').
arglistss('method-applicable-using-classes-p','mop','(METHOD CLASSES)').
arglistss('canonicalize-defclass-options','mop','(OPTIONS)').
arglistss('std-accessor-method-slot-definition','mop','(ACCESSOR-METHOD)').
arglistss('%find-method','mop','(GF QUALIFIERS SPECIALIZERS &OPTIONAL (ERRORP T))').
arglistss('find-generic-function','mop','(NAME &OPTIONAL (ERRORP T))').
arglistss('std-slot-exists-p','mop','(INSTANCE SLOT-NAME)').
arglistss('wrap-with-call-method-macro','mop','(GF ARGS-VAR EMF-FORM)').
arglistss('augment-initargs-with-defaults','mop','(CLASS INITARGS)').
arglistss('std-after-reinitialization-for-classes','mop','(CLASS &REST ALL-KEYS &KEY (DIRECT-SUPERCLASSES NIL DIRECT-SUPERCLASSES-P) (DIRECT-SLOTS NIL DIRECT-SLOTS-P) (DIRECT-DEFAULT-INITARGS NIL DIRECT-DEFAULT-INITARGS-P) &ALLOW-OTHER-KEYS)').
arglistss('std-after-initialization-for-classes','mop','(CLASS &KEY DIRECT-SUPERCLASSES DIRECT-SLOTS DIRECT-DEFAULT-INITARGS &ALLOW-OTHER-KEYS)').
arglistss('add-writer-method','mop','(CLASS FUNCTION-NAME SLOT-DEFINITION)').
arglistss('check-applicable-method-keyword-args','mop','(GF ARGS KEYWORD-ARGS APPLICABLE-KEYWORDS)').
arglistss('std-function-keywords','mop','(METHOD)').
arglistss('long-method-combination-args-lambda-list','mop','(METHOD-COMBINATION)').
arglistss('define-primordial-class','mop','(NAME SUPERCLASSES DIRECT-SLOTS)').
arglistss('make-instance-standard-method','mop','(GF &KEY LAMBDA-LIST QUALIFIERS SPECIALIZERS DOCUMENTATION FUNCTION FAST-FUNCTION)').
arglistss('check-variable-name','mop','(NAME)').
arglistss('std-shared-initialize','mop','(INSTANCE SLOT-NAMES ALL-KEYS)').
arglistss('check-argument-precedence-order','mop','(LAMBDA-LIST ARGUMENT-PRECEDENCE-ORDER)').
arglistss('method-documentation','mop','(METHOD)').
arglistss('std-method-specializers','mop','(METHOD)').
arglistss('compute-primary-emfun','mop','(METHODS)').
arglistss('fast-callable-p','mop','(GF)').
arglistss('std-allocate-instance','mop','(CLASS)').
arglistss('make-or-find-instance-funcallable-standard-class','mop','(METACLASS &REST INITARGS &KEY NAME DIRECT-SUPERCLASSES DIRECT-SLOTS DIRECT-DEFAULT-INITARGS DOCUMENTATION)').
arglistss('maybe-note-name-defined','mop','(NAME)').
arglistss('std-slot-makunbound','mop','(INSTANCE SLOT-NAME)').
arglistss('ensure-method','mop','(NAME &REST ALL-KEYS)').
arglistss('method-group-p','mop','(SELECTER QUALIFIERS)').
arglistss('std-tie-breaker-rule','mop','(MINIMAL-ELEMENTS CPL-SO-FAR)').
arglistss('add-reader-method','mop','(CLASS FUNCTION-NAME SLOT-DEFINITION)').
arglistss('make-instance-standard-class','mop','(METACLASS &REST INITARGS &KEY NAME DIRECT-SUPERCLASSES DIRECT-SLOTS DIRECT-DEFAULT-INITARGS DOCUMENTATION)').
arglistss('std-compute-class-precedence-list','mop','(CLASS)').
arglistss('process-next-method-list','mop','(NEXT-METHOD-LIST)').
arglistss('short-method-combination-operator','mop','(METHOD-COMBINATION)').
arglistss('instance-slot-p','mop','(SLOT)').
arglistss('redefine-class-forwarder','mop','(NAME SLOT &OPTIONAL BODY-ALIST)').
arglistss('std-method-qualifiers','mop','(METHOD)').
arglistss('walk-form','mop','(FORM)').
arglistss('check-duplicate-default-initargs','mop','(INITARGS)').
arglistss('expand-long-defcombin','mop','(NAME ARGS)').
arglistss('make-effective-slot-definition','mop','(CLASS &REST ARGS)').
arglistss('std-compute-applicable-methods','mop','(GF ARGS)').
arglistss('analyze-lambda-list','mop','(LAMBDA-LIST)').
arglistss('generate-emf-lambda','mop','(METHOD-FUNCTION NEXT-EMFUN)').
arglistss('canonicalize-defgeneric-option','mop','(OPTION)').
arglistss('std-add-method','mop','(GF METHOD)').
arglistss('method-combination-type-lambda','mop','(&REST ALL-ARGS &KEY NAME LAMBDA-LIST ARGS-LAMBDA-LIST GENERIC-FUNCTION-SYMBOL METHOD-GROUP-SPECS DECLARATIONS FORMS &ALLOW-OTHER-KEYS)').
arglistss('lambda-lists-congruent-p','mop','(LAMBDA-LIST1 LAMBDA-LIST2)').
arglistss('std-compute-default-initargs','mop','(CLASS)').
arglistss('std-remove-method','mop','(GF METHOD)').
arglistss('intern-eql-specializer','mop','(OBJECT)').
arglistss('ensure-class','mop','(NAME &REST ALL-KEYS &KEY &ALLOW-OTHER-KEYS)').
arglistss('extract-specializer-names','mop','(SPECIALIZED-LAMBDA-LIST)').
arglistss('set-funcallable-instance-function','mop','(funcallable-instance function)').
arglistss('extract-lambda-list','mop','(SPECIALIZED-LAMBDA-LIST)').
arglistss('eql-specializer-object','mop','(EQL-SPECIALIZER)').
arglistss('%defgeneric','mop','(FUNCTION-NAME &REST ALL-KEYS)').
arglistss('funcallable-standard-instance-access','mop','(INSTANCE LOCATION)').
arglistss('canonicalize-direct-superclasses','mop','(DIRECT-SUPERCLASSES)').
arglistss('standard-instance-access','system','(instance location)').
arglistss('comma-macro','system','(STREAM IGNORE)').
arglistss('%set-subseq','system','(SEQUENCE START &REST REST)').
arglistss('sharp-illegal','system','(stream sub-char numarg)').
arglistss('with-keyword-pairs','system','((NAMES EXPRESSION &OPTIONAL KEYWORDS-VAR) &BODY FORMS)').
arglistss('%set-caaar','system','(X V)').
arglistss('%terpri','system','(output-stream)').
arglistss('fasl-sharp-dot','system','(stream sub-char numarg)').
arglistss('read-quote','system','(stream character)').
arglistss('keyword-supplied-p','system','(KEYWORD KEY-LIST)').
arglistss('sharp-b','system','(stream sub-char numarg)').
arglistss('sharp-a','system','(stream sub-char numarg)').
arglistss('parse-defmacro','system','(LAMBDA-LIST ARG-LIST-NAME BODY NAME CONTEXT &KEY (ANONYMOUSP NIL) (DOC-STRING-ALLOWED T) ((ENVIRONMENT ENV-ARG-NAME)) (ERROR-FUN (QUOTE ERROR)) (WRAP-BLOCK T))').
arglistss('list-locater-macro','system','(SEQUENCE BODY-FORM RETURN-TYPE)').
arglistss('%set-seventh','system','(X V)').
arglistss('handle-circularity','system','(MARKER STREAM)').
arglistss('defstruct-default-constructor','system','(ARG)').
arglistss('fasl-read-dispatch-char','system','(stream character)').
arglistss('%ldb','system','(SIZE POSITION INTEGER)').
arglistss('define-boa-constructor','system','(CONSTRUCTOR)').
arglistss('read-dispatch-char','system','(stream character)').
arglistss('list-position','system','(ITEM SEQUENCE)').
arglistss('set-function-definition','system','(NAME NEW OLD)').
arglistss('declaration-error','system','(NAME)').
arglistss('mumble-remove-from-end','system','(PRED)').
arglistss('designated-package-name','system','(DESIGNATOR)').
arglistss('munge-restart-case-expression','system','(EXPRESSION ENV)').
arglistss('%set-cadadr','system','(X V)').
arglistss('quoted-form-p','system','(FORM)').
arglistss('incf-complex','system','(G240633 &OPTIONAL (DELTA 1) &ENVIRONMENT G240632)').
arglistss('quicksort','system','(SEQUENCE PREDICATE KEY)').
arglistss('sharp-backslash','system','(stream sub-char numarg)').
arglistss('ansi-loop','system','(EXPS)').
arglistss('%set-cadaar','system','(X V)').
arglistss('dd-print-function','system','(X)').
arglistss('mumble-delete-from-end','system','(PRED)').
arglistss('steve-splice','system','(SOURCE DESTINATION)').
arglistss('dd-print-object','system','(X)').
arglistss('vector-locater-if-macro','system','(TEST SEQUENCE RETURN-TYPE SENSE)').
arglistss('%set-logical-pathname-translations','system','(HOST TRANSLATIONS)').
arglistss('define-reader','system','(SLOT)').
arglistss('make-structure-writer','system','(INDEX STRUCTURE-TYPE)').
arglistss('dd-inherited-accessors','system','(X)').
arglistss('%set-caadr','system','(X V)').
arglistss('%write-string','system','(string output-stream start end)').
arglistss('%set-cddar','system','(X V)').
arglistss('%clear-output','system','(output-stream)').
arglistss('make-defstruct-description','system','(&KEY NAME CONC-NAME DEFAULT-CONSTRUCTOR CONSTRUCTORS COPIER INCLUDE TYPE NAMED INITIAL-OFFSET PREDICATE PRINT-FUNCTION PRINT-OBJECT DIRECT-SLOTS SLOTS INHERITED-ACCESSORS)').
arglistss('dd-direct-slots','system','(X)').
arglistss('vector-locater','system','(ITEM SEQUENCE RETURN-TYPE)').
arglistss('istep','system','(ARGS)').
arglistss('csubtypep','system','(CTYPE1 CTYPE2)').
arglistss('list-position-if','system','(TEST SEQUENCE)').
arglistss('%define-setf-macro','system','(NAME EXPANDER INVERSE DOC)').
arglistss('module-provide-system','system','(MODULE)').
arglistss('list-remove-from-end','system','(PRED)').
arglistss('get-fasl-function','system','(loader function-number)').
arglistss('%set-ninth','system','(X V)').
arglistss('last-cons-of','system','(LIST)').
arglistss('make-defstruct-slot-description','system','(&KEY NAME INDEX READER INITFORM (TYPE T) READ-ONLY)').
arglistss('dd-constructors','system','(X)').
arglistss('mkdir','system','(pathname)').
arglistss('read-comment','system','(stream character)').
arglistss('%elt','system','(sequence index)').
arglistss('map1','system','(FUNCTION ORIGINAL-ARGLISTS ACCUMULATE TAKE-CAR)').
arglistss('print-label','system','(MARKER STREAM)').
arglistss('sharp-c','system','(stream sub-char numarg)').
arglistss('sharp-r','system','(stream sub-char numarg)').
arglistss('sharp-p','system','(stream sub-char numarg)').
arglistss('sharp-o','system','(stream sub-char numarg)').
arglistss('sharp-s','system','(stream sub-char numarg)').
arglistss('list-position*','system','(ITEM SEQUENCE FROM-END TEST TEST-NOT START END KEY)').
arglistss('ctype','system','(TYPE)').
arglistss('sharp-left-paren','system','(stream sub-char numarg)').
arglistss('sharp-x','system','(stream sub-char numarg)').
arglistss('hash-table-entries','system','(hash-table)').
arglistss('ensure-available-symbols','system','(IMPORTS)').
arglistss('predicate-for-type','system','(TYPE)').
arglistss('match-wild-jar-pathname','system','(wild-jar-pathname)').
arglistss('translate-jar-device','system','(SOURCE FROM TO &OPTIONAL CASE)').
arglistss('directory-match-p','system','(THING WILD IGNORE-CASE)').
arglistss('make-vector-writer','system','(INDEX)').
arglistss('merge-sort-body','system','(TYPE REF MPREDICATE MKEY MSEQUENCE MSTART MEND)').
arglistss('vector-find','system','(ITEM SEQUENCE)').
arglistss('fasl-sharp-colon','system','(stream sub-char numarg)').
arglistss('round-up','system','(STRING)').
arglistss('list-find-if-not','system','(TEST SEQUENCE)').
arglistss('%fresh-line','system','(output-stream)').
arglistss('transform-keywords','system','(&KEY REPORT INTERACTIVE TEST)').
arglistss('concatenate-to-string','system','(SEQUENCES)').
arglistss('%set-cdaaar','system','(X V)').
arglistss('circle-subst','system','(OLD-NEW-ALIST TREE)').
arglistss('%set-cdar','system','(X V)').
arglistss('lookup-keyword','system','(KEYWORD KEY-LIST)').
arglistss('%set-cdddar','system','(X V)').
arglistss('%set-symbol-function','system','(symbol function)').
arglistss('locater-if-test','system','(TEST SEQUENCE SEQ-TYPE RETURN-TYPE SENSE)').
arglistss('%set-stream-external-format','system','(stream external-format)').
arglistss('list-remove-duplicates','system','(LIST TEST TEST-NOT START END KEY FROM-END)').
arglistss('%length','system','(sequence)').
arglistss('sharp-equal','system','(STREAM LABEL READTABLE)').
arglistss('make-vector-reader','system','(INDEX)').
arglistss('dd-named','system','(X)').
arglistss('%parse-namestring','system','(namestring host default-pathname)').
arglistss('sharp-colon','system','(stream sub-char numarg)').
arglistss('%set-cddr','system','(X V)').
arglistss('list-find','system','(ITEM SEQUENCE)').
arglistss('sharp-vertical-bar','system','(stream sub-char numarg)').
arglistss('defmacro-error','system','(PROBLEM NAME)').
arglistss('%set-cdaadr','system','(X V)').
arglistss('proclaim-ftype','system','(FTYPE &REST NAMES)').
arglistss('check-disjoint','system','(&REST ARGS)').
arglistss('find-restart-or-control-error','system','(IDENTIFIER &OPTIONAL CONDITION)').
arglistss('parse-keyword-pairs','system','(LIST KEYS)').
arglistss('get-setf-method-inverse','system','(FORM INVERSE SETF-FUNCTION)').
arglistss('translate-directory','system','(SOURCE FROM TO CASE)').
arglistss('punt-print-if-too-long','system','(INDEX STREAM)').
arglistss('merge-lists-no-key','system','(LIST1 LIST2 PRED)').
arglistss('%format','system','(destination control-string &rest args)').
arglistss('%set-cddddr','system','(X V)').
arglistss('merge-sort-vectors','system','(SEQUENCE PREDICATE KEY)').
arglistss('proclaim-type','system','(TYPE &REST NAMES)').
arglistss('fasl-sharp-star','system','(stream sub-char numarg)').
arglistss('read-string','system','(stream character)').
arglistss('%subseq','system','(sequence start &optional end)').
arglistss('sharp-quote','system','(stream sub-char numarg)').
arglistss('list-delete-duplicates*','system','(LIST TEST TEST-NOT KEY FROM-END START END)').
arglistss('vector-locater-if','system','(TEST SEQUENCE RETURN-TYPE)').
arglistss('%reverse','system','(sequence)').
arglistss('%set-fifth','system','(X V)').
arglistss('display-object','system','(OBJ)').
arglistss('run-hook','system','(HOOK &REST ARGS)').
arglistss('csubtypep-function','system','(CT1 CT2)').
arglistss('%nreverse','system','(sequence)').
arglistss('dd-slots','system','(X)').
arglistss('dd-conc-name','system','(X)').
arglistss('mumble-remove-macro','system','(BUMP LEFT BEGIN FINISH RIGHT PRED)').
arglistss('dd-initial-offset','system','(X)').
arglistss('canonicalize-logical-pathname-translations','system','(TRANSLATIONS HOST)').
arglistss('make-gensym-list','system','(N)').
arglistss('fasl-sharp-quote','system','(stream sub-char numarg)').
arglistss('list-delete','system','(PRED)').
arglistss('set-source-transform','system','(NAME TRANSFORM)').
arglistss('list-locater','system','(ITEM SEQUENCE RETURN-TYPE)').
arglistss('vector-delete-duplicates*','system','(VECTOR TEST TEST-NOT KEY FROM-END START END &OPTIONAL (LENGTH (LENGTH VECTOR)))').
arglistss('%set-caaaar','system','(X V)').
arglistss('read-list','system','(stream character)').
arglistss('backq-list','system','(&REST ARGS)').
arglistss('%set-cdaar','system','(X V)').
arglistss('%typep','system','(OBJECT TYPE)').
arglistss('make-list-writer','system','(INDEX)').
arglistss('%compile','system','(NAME DEFINITION)').
arglistss('array-readably-printable-p','system','(ARRAY)').
arglistss('%subtypep','system','(TYPE1 TYPE2 &OPTIONAL ENVIRONMENT)').
arglistss('dot-length','system','(CONS)').
arglistss('leader','system','(NAME)').
arglistss('trace-redefined-update','system','(&REST ARGS)').
arglistss('known-type-p','system','(TYPE)').
arglistss('backq-list*','system','(&REST ARGS)').
arglistss('verify-keywords','system','(KEY-LIST VALID-KEYS ALLOW-OTHER-KEYS)').
arglistss('mumble-delete','system','(PRED)').
arglistss('%set-caaadr','system','(X V)').
arglistss('%set-caddar','system','(X V)').
arglistss('%eval','system','(form)').
arglistss('make-sequence-of-type','system','(TYPE LENGTH)').
arglistss('list-reduce','system','(FUNCTION SEQUENCE START END INITIAL-VALUE IVP KEY)').
arglistss('make-ctype','system','(SUPER TYPE)').
arglistss('check-redefinition','system','(NAME)').
arglistss('list-reduce-from-end','system','(FUNCTION SEQUENCE START END INITIAL-VALUE IVP KEY)').
arglistss('output-terse-array','system','(ARRAY STREAM)').
arglistss('dsd-index','system','(X)').
arglistss('csubtypep-array','system','(CT1 CT2)').
arglistss('push-let-binding','system','(VARIABLE PATH SYSTEMP &OPTIONAL CONDITION (INIT-FORM NIL))').
arglistss('%set-cdadr','system','(X V)').
arglistss('restart-report','system','(RESTART STREAM)').
arglistss('vector-locater-macro','system','(SEQUENCE BODY-FORM RETURN-TYPE)').
arglistss('print-reference','system','(MARKER STREAM)').
arglistss('translate-component','system','(SOURCE FROM TO &OPTIONAL CASE)').
arglistss('float-denormalized-p','system','(X)').
arglistss('%set-eighth','system','(X V)').
arglistss('arg-count-error','system','(ERROR-KIND NAME ARG LAMBDA-LIST MINIMUM MAXIMUM)').
arglistss('output-integer','system','(INTEGER STREAM)').
arglistss('proclaim-ftype-1','system','(FTYPE NAME)').
arglistss('proper-list-of-length-p','system','(X MIN &OPTIONAL (MAX MIN))').
arglistss('output-list','system','(LIST STREAM)').
arglistss('%add-package-local-nickname','system','(local-nickname package &optional package-designator)').
arglistss('sharp-sharp','system','(STREAM IGNORE LABEL)').
arglistss('vector-find-if-not','system','(TEST SEQUENCE)').
arglistss('dd-type','system','(X)').
arglistss('fasl-sharp-backslash','system','(stream sub-char numarg)').
arglistss('list-locater-if','system','(TEST SEQUENCE RETURN-TYPE)').
arglistss('list-locater-if-macro','system','(TEST SEQUENCE RETURN-TYPE SENSE)').
arglistss('%set-symbol-macro','system','(symbol symbol-macro)').
arglistss('%defsetf','system','(ORIG-ACCESS-FORM NUM-STORE-VARS EXPANDER)').
arglistss('fasl-read-string','system','(stream character)').
arglistss('wild-p','system','(COMPONENT)').
arglistss('%set-cadddr','system','(X V)').
arglistss('define-writer','system','(SLOT)').
arglistss('simple-list-remove-duplicates','system','(list)').
arglistss('%set-fdefinition','system','(NAME FUNCTION)').
arglistss('uniquely-identified-by-print-p','system','(X)').
arglistss('%set-sixth','system','(X V)').
arglistss('vector-locater-if-not','system','(TEST SEQUENCE RETURN-TYPE)').
arglistss('%print-object','system','(OBJECT STREAM)').
arglistss('read-conditional','system','(STREAM SUBCHAR INT)').
arglistss('function-class-bytes','system','(function)').
arglistss('once-only','system','(SPECS &BODY BODY)').
arglistss('real-count','system','(COUNT)').
arglistss('fasl-read-quote','system','(stream character)').
arglistss('system-stream-p','system','(LAMBDA (OBJECT) (BLOCK SYSTEM-STREAM-P (SIMPLE-TYPEP OBJECT (QUOTE SYSTEM-STREAM))))').
arglistss('sort-list','system','(LIST PRED KEY)').
arglistss('locater-test-not','system','(ITEM SEQUENCE SEQ-TYPE RETURN-TYPE)').
arglistss('%set-tenth','system','(X V)').
arglistss('component-match-wild-p','system','(THING WILD IGNORE-CASE)').
arglistss('dd-include','system','(X)').
arglistss('dsd-name','system','(X)').
arglistss('type-specifier-atom','system','(TYPE)').
arglistss('check-for-circularity','system','(OBJECT &OPTIONAL ASSIGN)').
arglistss('backq-append','system','(&REST ARGS)').
arglistss('invoke-debugger-report-condition','system','(CONDITION)').
arglistss('backq-vector','system','(LIST)').
arglistss('named-let','system','(NAME BINDS &BODY BODY)').
arglistss('keywordify','system','(SYMBOL)').
arglistss('%set-cdadar','system','(X V)').
arglistss('match-dimensions','system','(DIM PAT)').
arglistss('push-optional-binding','system','(VALUE-VAR INIT-FORM SUPPLIED-VAR CONDITION PATH NAME ERROR-KIND ERROR-FUN)').
arglistss('vector-find*','system','(ITEM SEQUENCE FROM-END TEST TEST-NOT START END KEY)').
arglistss('dsd-read-only','system','(X)').
arglistss('%set-cdaddr','system','(X V)').
arglistss('vector-remove-duplicates','system','(VECTOR TEST TEST-NOT START END KEY FROM-END &OPTIONAL (LENGTH (LENGTH VECTOR)))').
arglistss('compiler-macroexpand-1','system','(FORM &OPTIONAL ENV)').
arglistss('sharp-star','system','(stream sub-char numarg)').
arglistss('precompile-package','system','(PKG &KEY (VERBOSE *COMPILE-VERBOSE*))').
arglistss('dd-default-constructor','system','(X)').
arglistss('backq-cons','system','(&REST ARGS)').
arglistss('%set-caddr','system','(X V)').
arglistss('%set-readtable-case','system','(readtable new-mode)').
arglistss('ctype-type','system','(CTYPE)').
arglistss('dsd-reader','system','(X)').
arglistss('backquotify-1','system','(FLAG THING)').
arglistss('%set-cadar','system','(X V)').
arglistss('ctype-super','system','(CTYPE)').
arglistss('make-sequence-like','system','(SEQUENCE LENGTH)').
arglistss('dimension-subtypep','system','(DIM1 DIM2)').
arglistss('dd-predicate','system','(X)').
arglistss('vector-position','system','(ITEM SEQUENCE)').
arglistss('fasl-sharp-a','system','(stream sub-char numarg)').
arglistss('fasl-sharp-c','system','(stream sub-char numarg)').
arglistss('fasl-sharp-b','system','(stream sub-char numarg)').
arglistss('backtrace-as-list','system','(&OPTIONAL (N 0))').
arglistss('directory-match-components','system','(THING WILD IGNORE-CASE)').
arglistss('fasl-sharp-x','system','(stream sub-char numarg)').
arglistss('fasl-sharp-o','system','(stream sub-char numarg)').
arglistss('fasl-sharp-p','system','(stream sub-char numarg)').
arglistss('fasl-sharp-s','system','(stream sub-char numarg)').
arglistss('print-restart','system','(RESTART STREAM)').
arglistss('fasl-sharp-r','system','(stream sub-char numarg)').
arglistss('interactive-restart-arguments','system','(REAL-RESTART)').
arglistss('read-feature','system','(STREAM)').
arglistss('sub-interval-p','system','(I1 I2)').
arglistss('dd-copier','system','(X)').
arglistss('parse-name-and-options','system','(NAME-AND-OPTIONS)').
arglistss('expand-source-transform-1','system','(FORM)').
arglistss('list-position-if-not','system','(TEST SEQUENCE)').
arglistss('translate-directory-components-aux','system','(SRC FROM TO CASE)').
arglistss('mumble-remove','system','(PRED)').
arglistss('define-keyword-constructor','system','(CONSTRUCTOR)').
arglistss('map-restarts','system','(FN CONDITION CALL-TEST-P)').
arglistss('list-delete-from-end','system','(PRED)').
arglistss('%finish-output','system','(output-stream)').
arglistss('%force-output','system','(output-stream)').
arglistss('symbolicate','system','(&REST THINGS)').
arglistss('flonum-to-string','system','(X &OPTIONAL WIDTH FDIGITS SCALE FMIN)').
arglistss('list-remove','system','(PRED)').
arglistss('find-dsd','system','(NAME)').
arglistss('backq-nconc','system','(&REST ARGS)').
arglistss('output-vector','system','(VECTOR STREAM)').
arglistss('backquote-macro','system','(STREAM IGNORE)').
arglistss('translate-directory-components','system','(SRC FROM TO CASE)').
arglistss('make-structure-reader','system','(INDEX STRUCTURE-TYPE)').
arglistss('quick-sort','system','(SEQ START END PRED KEY)').
arglistss('%write-char','system','(character output-stream)').
arglistss('%reader-error','system','(STREAM CONTROL &REST ARGS)').
arglistss('%set-caadar','system','(X V)').
arglistss('parse-defmacro-lambda-list','system','(LAMBDA-LIST ARG-LIST-NAME NAME ERROR-KIND ERROR-FUN &OPTIONAL TOP-LEVEL ENV-ILLEGAL)').
arglistss('comma','system','(CODE)').
arglistss('quicksort-body','system','(TYPE REF MPREDICATE MKEY SEQUENCE MSTART MEND)').
arglistss('dsd-type','system','(X)').
arglistss('compound-object-p','system','(X)').
arglistss('get-slot','system','(NAME)').
arglistss('list-find-if','system','(TEST SEQUENCE)').
arglistss('%set-caar','system','(X V)').
arglistss('scale-exponent','system','(ORIGINAL-X)').
arglistss('make-list-reader','system','(INDEX)').
arglistss('properly-named-class-p','system','(THING ENVIRONMENT)').
arglistss('lambda-list-broken-key-list-error','system','(&KEY KIND NAME PROBLEM INFO)').
arglistss('apply-key','system','(KEY ELEMENT)').
arglistss('vector-position*','system','(ITEM SEQUENCE FROM-END TEST TEST-NOT START END KEY)').
arglistss('simple-subtypep','system','(TYPE1 TYPE2)').
arglistss('vector-position-if','system','(TEST SEQUENCE)').
arglistss('casify','system','(THING CASE)').
arglistss('merge-vectors-body','system','(TYPE REF A START-A END-A B START-B END-B AUX START-AUX PREDICATE &OPTIONAL KEY)').
arglistss('stringify-names','system','(NAMES)').
arglistss('stream-line-number','system','(stream)').
arglistss('parse-1-option','system','(OPTION)').
arglistss('fasl-sharp-left-paren','system','(stream sub-char numarg)').
arglistss('%set-cadr','system','(X V)').
arglistss('vector-position-if-not','system','(TEST SEQUENCE)').
arglistss('output-ugly-object','system','(OBJECT STREAM)').
arglistss('sharp-dot','system','(stream sub-char numarg)').
arglistss('in-interval-p','system','(X INTERVAL)').
arglistss('%set-cddaar','system','(X V)').
arglistss('read-right-paren','system','(stream character)').
arglistss('stream-offset','system','(stream)').
arglistss('%set-caaddr','system','(X V)').
arglistss('decf-complex','system','(G240688 &OPTIONAL (DELTA 1) &ENVIRONMENT G240687)').
arglistss('%load','system','(filespec verbose print if-does-not-exist external-format)').
arglistss('%set-cdddr','system','(X V)').
arglistss('fasl-sharp-question-mark','system','(stream sub-char numarg)').
arglistss('simple-array-p','system','(OBJECT)').
arglistss('restart-p','system','(OBJECT)').
arglistss('%set-cddadr','system','(X V)').
arglistss('merge-lists','system','(LIST1 LIST2 PRED KEY)').
arglistss('backquotify','system','(STREAM CODE)').
arglistss('expandable-backq-expression-p','system','(OBJECT)').
arglistss('make-fasl-class-loader','system','(base-name)').
arglistss('dsd-initform','system','(X)').
arglistss('dd-name','system','(X)').
arglistss('component-match-p','system','(THING WILD IGNORE-CASE)').
arglistss('safe-length','system','(X)').
arglistss('list-remove-macro','system','(PRED REVERSE-P)').
arglistss('bogus-sublist-error','system','(&KEY KIND NAME OBJECT LAMBDA-LIST)').
arglistss('make-restart','system','(&KEY ((NAME G275170) NIL) ((FUNCTION G275171) NIL) ((REPORT-FUNCTION G275172) NIL) ((INTERACTIVE-FUNCTION G275173) NIL) ((TEST-FUNCTION G275174) (FUNCTION (LAMBDA (C) (DECLARE (IGNORE C)) T))))').
arglistss('vector-find-if','system','(TEST SEQUENCE)').
arglistss('expand-or-get-setf-inverse','system','(FORM ENVIRONMENT)').
arglistss('push-sub-list-binding','system','(VARIABLE PATH OBJECT NAME ERROR-KIND ERROR-FUN)').
arglistss('fasl-read-list','system','(stream character)').
arglistss('with-set-keys','system','(FUNCALL)').
arglistss('list-locater-if-not','system','(TEST SEQUENCE RETURN-TYPE)').
arglistss('%check-object','system','(OBJECT STREAM)').
arglistss('copy-restart','system','(structure)').
arglistss('set-inline-expansion','system','(NAME EXPANSION)').
arglistss('csubtypep-complex','system','(CT1 CT2)').
arglistss('list-find*','system','(ITEM SEQUENCE FROM-END TEST TEST-NOT START END KEY)').
arglistss('%load-returning-last-result','system','(filespec verbose print if-does-not-exist external-format)').
arglistss('%documentation','system','(object doc-type)').
arglistss('check-sequence-bounds','system','(SEQUENCE START END)').
arglistss('setf-function-name-p','system','(thing)').
arglistss('layout-slot-location','system','(layout slot-name)').
arglistss('write-vector-unsigned-byte-8','system','(vector stream start end)').
arglistss('swap-slots','system','(instance-1 instance-2)').
arglistss('float-overflow-mode','system','(&optional boolean)').
arglistss('%make-instances-obsolete','system','(class)').
arglistss('parse-body','system','(BODY &OPTIONAL (DOC-STRING-ALLOWED T))').
arglistss('fdefinition-block-name','system','(function-name)').
arglistss('delete-eql','system','(item sequence)').
arglistss('make-macro-expander','system','(DEFINITION)').
arglistss('subclassp','system','(class)').
arglistss('list-delete-eq','system','(item list)').
arglistss('%reinit-emf-cache','system','(generic-function eql-specializer-objects-list)').
arglistss('%allocate-funcallable-instance','system','(class)').
arglistss('%set-class-layout','system','(class layout)').
arglistss('vector-delete-eq','system','(item vector)').
arglistss('float-underflow-mode','system','(&optional boolean)').
arglistss('out-synonym-of','system','(stream-designator)').
arglistss('std-slot-value','system','(instance slot-name)').
arglistss('structure-length','system','(instance)').
arglistss('notinline-p','system','(NAME)').
arglistss('psxhash','system','(object)').
arglistss('remove-zip-cache-entry','system','(pathname)').
arglistss('puthash','system','(key hash-table new-value &optional default)').
arglistss('%class-slots','system','(class)').
arglistss('environment-add-macro-definition','system','(environment name expander)').
arglistss('list-directory','system','(directory &optional (resolve-symlinks t))').
arglistss('make-keyword','system','(symbol)').
arglistss('logical-pathname-p','system','(object)').
arglistss('%make-logical-pathname','system','(namestring)').
arglistss('output-object','system','(OBJECT STREAM)').
arglistss('coerce-to-condition','system','(DATUM ARGUMENTS DEFAULT-TYPE FUN-NAME)').
arglistss('record-source-information','system','(NAME &OPTIONAL SOURCE-PATHNAME SOURCE-POSITION)').
arglistss('layout-class','system','(layout)').
arglistss('macro-function-p','system','(value)').
arglistss('undefined-function-called','system','(NAME ARGUMENTS)').
arglistss('expand-inline','system','(FORM EXPANSION)').
arglistss('ensure-input-stream','system','(pathname)').
arglistss('%float-bits','system','(integer)').
arglistss('environment-all-variables','system','(environment)').
arglistss('gethash1','system','(key hash-table)').
arglistss('%stream-write-char','system','(character output-stream)').
arglistss('%stream-terpri','system','(output-stream)').
arglistss('environment-add-function-definition','system','(environment name lambda-expression)').
arglistss('%set-standard-instance-access','system','(instance location new-value)').
arglistss('note-name-defined','system','(NAME)').
arglistss('%type-error','system','(DATUM EXPECTED-TYPE)').
arglistss('defconst','system','(NAME VALUE)').
arglistss('hash-table-weakness','system','(hash-table)').
arglistss('read-8-bits','system','(stream &optional eof-error-p eof-value)').
arglistss('init-fasl','system','(&key version)').
arglistss('get-cached-emf','system','(generic-function args)').
arglistss('double-float-high-bits','system','(float)').
arglistss('compiler-defstruct','system','(NAME &KEY CONC-NAME DEFAULT-CONSTRUCTOR CONSTRUCTORS COPIER INCLUDE TYPE NAMED INITIAL-OFFSET PREDICATE PRINT-FUNCTION PRINT-OBJECT DIRECT-SLOTS SLOTS INHERITED-ACCESSORS DOCUMENTATION)').
arglistss('%defun','system','(name definition)').
arglistss('normalize-type','system','(TYPE)').
arglistss('inline-p','system','(NAME)').
arglistss('require-type','system','(ARG TYPE)').
arglistss('environment-all-functions','system','(environment)').
arglistss('environment-variables','system','(environment)').
arglistss('proclaimed-ftype','system','(NAME)').
arglistss('write-8-bits','system','(byte stream)').
arglistss('make-double-float','system','(bits)').
arglistss('layout-length','system','(layout)').
arglistss('get-function-info-value','system','(name indicator)').
arglistss('make-layout','system','(class instance-slots class-slots)').
arglistss('vector-delete-eql','system','(item vector)').
arglistss('%set-class-slots','system','(class slot-definitions)').
arglistss('%class-layout','system','(class)').
arglistss('%putf','system','(plist indicator new-value)').
arglistss('std-slot-boundp','system','(instance slot-name)').
arglistss('%wild-pathname-p','system','(pathname keyword)').
arglistss('make-single-float','system','(bits)').
arglistss('list-delete-eql','system','(item list)').
arglistss('make-environment','system','(&optional parent-environment)').
arglistss('ftype-result-type','system','(FTYPE)').
arglistss('%make-slot-definition','system','(slot-class)').
arglistss('single-float-bits','system','(float)').
arglistss('expand-source-transform','system','(FORM)').
arglistss('frame-to-list','system','(frame)').
arglistss('logical-host-p','system','(CANONICAL-HOST)').
arglistss('compiler-macroexpand','system','(FORM &OPTIONAL ENV)').
arglistss('structure-set','system','(instance index new-value)').
arglistss('structure-object-p','system','(object)').
arglistss('make-symbol-macro','system','(expansion)').
arglistss('set-function-info-value','system','(name indicator value)').
arglistss('delete-eq','system','(item sequence)').
arglistss('standard-instance-access','system','(instance location)').
arglistss('make-macro','system','(name expansion-function)').
arglistss('untraced-function','system','(NAME)').
arglistss('process-optimization-declarations','system','(FORMS)').
arglistss('%set-class-direct-subclasses','system','(class direct-subclasses)').
arglistss('cache-emf','system','(generic-function args emf)').
arglistss('standard-object-p','system','(object)').
arglistss('named-lambda','system','(NAME LAMBDA-LIST &REST BODY)').
arglistss('structure-ref','system','(instance index)').
arglistss('load-compiled-function','system','(source)').
arglistss('aset','system','(array subscripts new-element)').
arglistss('double-float-low-bits','system','(float)').
arglistss('canonicalize-logical-host','system','(host)').
arglistss('%class-name','system','(class)').
arglistss('%make-structure','system','(name slot-values)').
arglistss('source-transform','system','(NAME)').
arglistss('record-source-information-for-type','system','(NAME TYPE &OPTIONAL SOURCE-PATHNAME SOURCE-POSITION)').
arglistss('symbol-macro-p','system','(value)').
arglistss('empty-environment-p','system','(environment)').
arglistss('svset','system','(simple-vector index new-value)').
arglistss('simple-format','system','(DESTINATION CONTROL-STRING &REST FORMAT-ARGUMENTS)').
arglistss('frame-to-string','system','(frame)').
arglistss('compiled-lisp-function-p','system','(object)').
arglistss('set-schar','system','(string index character)').
arglistss('set-std-slot-value','system','(instance slot-name new-value)').
arglistss('define-source-transform','system','(NAME LAMBDA-LIST &REST BODY)').
arglistss('read-vector-unsigned-byte-8','system','(vector stream start end)').
arglistss('%std-allocate-instance','system','(class)').
arglistss('fset','system','(NAME FUNCTION &OPTIONAL SOURCE-POSITION ARGLIST DOCUMENTATION)').
arglistss('shrink-vector','system','(vector new-size)').
arglistss('%set-documentation','system','(object doc-type documentation)').
arglistss('inline-expansion','system','(NAME)').
arglistss('proclaimed-type','system','(NAME)').
arglistss('check-declaration-type','system','(NAME)').
arglistss('environment-add-symbol-binding','system','(environment symbol value)').
arglistss('set-char','system','(string index character)').
arglistss('get-arglists','common-lisp-user','(LAMBDA NIL (BLOCK GET-ARGLISTS (DOLIST (P (LIST-ALL-PACKAGES)) (LET ((PN (STRING-DOWNCASE (PACKAGE-NAME P)))) (BLOCK NIL (FLET ((DO-SYMBOLS-44 (SYM) (TAGBODY (WHEN (EQ (SYMBOL-PACKAGE SYM) P) (WHEN (FBOUNDP SYM) (BLOCK G45 (LET ((G46 NIL)) (DECLARE (IGNORABLE G46)) (TAGBODY (LET ((*HANDLER-CLUSTERS* (CONS (LIST (CONS (QUOTE T) (LAMBDA (TEMP) (DECLARE (IGNORE TEMP)) (GO G47)))) *HANDLER-CLUSTERS*))) (JRUN-EXCEPTION-PROTECTED (LAMBDA NIL (RETURN-FROM G45 (LET* ((SN (STRING-DOWNCASE (SYMBOL-NAME SYM))) (SF (SYMBOL-FUNCTION SYM)) (SFLL (FUNCTION-LAMBDA-EXPRESSION SF))) (UNLESS SFLL (SETQ SFLL (ARGLIST SF))) (WHEN SFLL (FORMAT T arglistss('~a','~a','~a').~% SN PN SFLL))))))) G47 (RETURN-FROM G45 (LOCALLY)))))))))) (MAPC (FUNCTION (LAMBDA (PACKAGE) (FLET ((ITERATE-OVER-SYMBOLS (SYMBOLS) (MAPC (FUNCTION DO-SYMBOLS-44) SYMBOLS))) (ITERATE-OVER-SYMBOLS (PACKAGE-INTERNAL-SYMBOLS PACKAGE)) (ITERATE-OVER-SYMBOLS (PACKAGE-EXTERNAL-SYMBOLS PACKAGE))))) (LIST-ALL-PACKAGES))) (LET ((SYM NIL)) (DECLARE (IGNORABLE SYM)) NIL))))))').
arglistss('vector-count-if','common-lisp','(NOT-P FROM-END-P PREDICATE SEQUENCE)').
arglistss('list-count-if','common-lisp','(NOT-P FROM-END-P PREDICATE SEQUENCE)').
arglistss('tenth','common-lisp','(LIST)').
arglistss('restart-bind','common-lisp','(BINDINGS &BODY FORMS)').
arglistss('mapcan','common-lisp','(FUNCTION LIST &REST MORE-LISTS)').
arglistss('mapcar','common-lisp','(function &rest lists)').
arglistss('cdaar','common-lisp','(list)').
arglistss('translate-pathname','common-lisp','(SOURCE FROM-WILDCARD TO-WILDCARD &KEY)').
arglistss('get-macro-character','common-lisp','(char &optional readtable)').
arglistss('name-char','common-lisp','(name)').
arglistss('>=','common-lisp','(&rest numbers)').
arglistss('pathname-match-p','common-lisp','(PATHNAME WILDCARD)').
arglistss('nbutlast','common-lisp','(LIST &OPTIONAL (N 1))').
arglistss('string-capitalize','common-lisp','(STRING &KEY (START 0) END)').
arglistss('caaaar','common-lisp','(LIST)').
arglistss('notany','common-lisp','(PREDICATE SEQUENCE &REST MORE-SEQUENCES)').
arglistss('translate-logical-pathname','common-lisp','(PATHNAME &KEY)').
arglistss('boundp','common-lisp','(symbol)').
arglistss('packagep','common-lisp','(object)').
arglistss('float-radix','common-lisp','(float)').
arglistss('svref','common-lisp','(simple-vector index)').
arglistss('<=','common-lisp','(&rest numbers)').
arglistss('byte-size','common-lisp','(BYTESPEC)').
arglistss('read-line','common-lisp','(&optional input-stream eof-error-p eof-value recursive-p)').
arglistss('maplist','common-lisp','(FUNCTION LIST &REST MORE-LISTS)').
arglistss('random','common-lisp','(limit &optional random-state)').
arglistss('find-symbol','common-lisp','(string &optional package)').
arglistss('char-not-greaterp','common-lisp','(&rest characters)').
arglistss('vector-push','common-lisp','(new-element vector)').
arglistss('cdadr','common-lisp','(list)').
arglistss('values','common-lisp','(&rest object)').
arglistss('import','common-lisp','(SYMBOLS &OPTIONAL (PACKAGE *PACKAGE* PACKAGE-SUPPLIED-P))').
arglistss('nth','common-lisp','(n list)').
arglistss('push','common-lisp','(&ENVIRONMENT ENV ITEM PLACE)').
arglistss('cons','common-lisp','(object-1 object-2)').
arglistss('listp','common-lisp','(object)').
arglistss('gethash','common-lisp','(key hash-table &optional default)').
arglistss('list*','common-lisp','(&rest objects)').
arglistss('define-compiler-macro','common-lisp','(NAME LAMBDA-LIST &REST BODY)').
arglistss('row-major-aref','common-lisp','(array index)').
arglistss('clrhash','common-lisp','(hash-table)').
arglistss('find-restart','common-lisp','(NAME &OPTIONAL CONDITION)').
arglistss('princ','common-lisp','(OBJECT &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('compiler-macro-function','common-lisp','(NAME &OPTIONAL ENVIRONMENT)').
arglistss('prin1','common-lisp','(OBJECT &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('pprint-indent','common-lisp','(RELATIVE-TO N &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('restart-case','common-lisp','(EXPRESSION &BODY CLAUSES &ENVIRONMENT ENV)').
arglistss('pprint-dispatch','common-lisp','(OBJECT &OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*))').
arglistss('defun','common-lisp','(NAME LAMBDA-LIST &BODY BODY &ENVIRONMENT ENV)').
arglistss('make-string','common-lisp','(SIZE &KEY INITIAL-ELEMENT ELEMENT-TYPE)').
arglistss('cddddr','common-lisp','(LIST)').
arglistss('return','common-lisp','(&OPTIONAL RESULT)').
arglistss('realp','common-lisp','(object)').
arglistss('nunion','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('rename-package','common-lisp','(package new-name &optional new-nicknames)').
arglistss('count-if','common-lisp','(TEST SEQUENCE &REST ARGS &KEY FROM-END (START 0) END KEY)').
arglistss('reduce','common-lisp','(FUNCTION SEQUENCE &REST ARGS &KEY FROM-END (START 0) END (INITIAL-VALUE NIL IVP) KEY)').
arglistss('cdaadr','common-lisp','(LIST)').
arglistss('type-of','common-lisp','(object)').
arglistss('invoke-restart','common-lisp','(RESTART &REST VALUES)').
arglistss('intersection','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('mapl','common-lisp','(FUNCTION LIST &REST MORE-LISTS)').
arglistss('not','common-lisp','(x)').
arglistss('mapc','common-lisp','(function &rest lists)').
arglistss('arrayp','common-lisp','(object)').
arglistss('cdddar','common-lisp','(LIST)').
arglistss('hash-table-test','common-lisp','(hash-table)').
arglistss('method-combination-error','common-lisp','(FORMAT-CONTROL &REST ARGS)').
arglistss('schar','common-lisp','(string index)').
arglistss('symbol-value','common-lisp','(symbol)').
arglistss('makunbound','common-lisp','(symbol)').
arglistss('count-if-not','common-lisp','(TEST SEQUENCE &REST ARGS &KEY FROM-END (START 0) END KEY)').
arglistss('probe-file','common-lisp','(pathspec)').
arglistss('make-condition','common-lisp','(TYPE &REST INITARGS)').
arglistss('loop','common-lisp','(&REST EXPS)').
arglistss('declaim','common-lisp','(&REST DECLS)').
arglistss('complement','common-lisp','(F)').
arglistss('string','common-lisp','(x)').
arglistss('symbol-plist','common-lisp','(symbol)').
arglistss('streamp','common-lisp','(object)').
arglistss('fifth','common-lisp','(LIST)').
arglistss('clear-output','common-lisp','(&OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('nstring-capitalize','common-lisp','(STRING &KEY (START 0) END)').
arglistss('ash','common-lisp','(integer count)').
arglistss('floatp','common-lisp','(object)').
arglistss('integerp','common-lisp','(object)').
arglistss('delete','common-lisp','(ITEM SEQUENCE &REST ARGS &KEY FROM-END (TEST (FUNCTION EQL)) TEST-NOT (START 0) END COUNT KEY)').
arglistss('rationalp','common-lisp','(object)').
arglistss('random-state-p','common-lisp','(object)').
arglistss('type-error-expected-type','common-lisp','(condition)').
arglistss('destructuring-bind','common-lisp','(LAMBDA-LIST ARG-LIST &REST BODY)').
arglistss('alpha-char-p','common-lisp','(character)').
arglistss('defparameter','common-lisp','(NAME INITIAL-VALUE &OPTIONAL DOCSTRING)').
arglistss('pop','common-lisp','(&ENVIRONMENT ENV PLACE)').
arglistss('identity','common-lisp','(object)').
arglistss('array-in-bounds-p','common-lisp','(array &rest subscripts)').
arglistss('1-','common-lisp','(number)').
arglistss('1+','common-lisp','(number)').
arglistss('close','common-lisp','(stream &key abort)').
arglistss('digit-char','common-lisp','(weight &optional radix)').
arglistss('cdaaar','common-lisp','(LIST)').
arglistss('slot-exists-p','common-lisp','(OBJECT SLOT-NAME)').
arglistss('sixth','common-lisp','(LIST)').
arglistss('two-way-stream-output-stream','common-lisp','(two-way-stream)').
arglistss('print','common-lisp','(OBJECT &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('nintersection','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('realpart','common-lisp','(number)').
arglistss('fourth','common-lisp','(list)').
arglistss('/=','common-lisp','(&rest numbers)').
arglistss('slot-boundp','common-lisp','(OBJECT SLOT-NAME)').
arglistss('cdaddr','common-lisp','(LIST)').
arglistss('invalid-method-error','common-lisp','(METHOD FORMAT-CONTROL &REST ARGS)').
arglistss('char-lessp','common-lisp','(&rest characters)').
arglistss('simple-vector-p','common-lisp','(object)').
arglistss('ldb','common-lisp','(BYTESPEC INTEGER)').
arglistss('cerror','common-lisp','(CONTINUE-STRING DATUM &REST ARGUMENTS)').
arglistss('constantp','common-lisp','(form &optional environment)').
arglistss('sort','common-lisp','(SEQUENCE PREDICATE &REST ARGS &KEY KEY)').
arglistss('defgeneric','common-lisp','(FUNCTION-NAME LAMBDA-LIST &REST OPTIONS-AND-METHOD-DESCRIPTIONS)').
arglistss('mapcon','common-lisp','(FUNCTION LIST &REST MORE-LISTS)').
arglistss('defclass','common-lisp','(&WHOLE FORM NAME DIRECT-SUPERCLASSES DIRECT-SLOTS &REST OPTIONS)').
arglistss('make-pathname','common-lisp','(&key host device directory name type version defaults case)').
arglistss('terpri','common-lisp','(&OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('alphanumericp','common-lisp','(character)').
arglistss('null','common-lisp','(object)').
arglistss('find','common-lisp','(ITEM SEQUENCE &REST ARGS &KEY FROM-END (TEST (FUNCTION EQL)) TEST-NOT (START 0) END KEY)').
arglistss('pushnew','common-lisp','(&ENVIRONMENT ENV ITEM PLACE &REST KEYS)').
arglistss('find-package','common-lisp','(name)').
arglistss('minusp','common-lisp','(real)').
arglistss('truename','common-lisp','(filespec)').
arglistss('string-equal','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('class-of','common-lisp','(object)').
arglistss('remove-if','common-lisp','(PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0) END COUNT KEY)').
arglistss('vectorp','common-lisp','(object)').
arglistss('pprint-newline','common-lisp','(KIND &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('pathname-name','common-lisp','(PATHNAME &KEY (CASE LOCAL))').
arglistss('shadowing-import','common-lisp','(symbols &optional package)').
arglistss('butlast','common-lisp','(LIST &OPTIONAL (N 1))').
arglistss('digit-char-p','common-lisp','(char &optional radix)').
arglistss('nset-exclusive-or','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT (FUNCTION EQL) NOTP))').
arglistss('integer-length','common-lisp','(integer)').
arglistss('both-case-p','common-lisp','(character)').
arglistss('write-string','common-lisp','(STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY (START 0) END)').
arglistss('cdadar','common-lisp','(LIST)').
arglistss('load-logical-pathname-translations','common-lisp','(HOST)').
arglistss('readtablep','common-lisp','(object)').
arglistss('defmethod','common-lisp','(&REST ARGS &ENVIRONMENT ENV)').
arglistss('unexport','common-lisp','(symbols &optional package)').
arglistss('string-lessp','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('copy-list','common-lisp','(list)').
arglistss('eql','common-lisp','(x y)').
arglistss('set-difference','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('logical-pathname-translations','common-lisp','(HOST)').
arglistss('export','common-lisp','(symbols &optional package)').
arglistss('rename-file','common-lisp','(filespec new-name)').
arglistss('pprint-linear','common-lisp','(S LIST &OPTIONAL (COLON? T) ATSIGN?)').
arglistss('handler-bind','common-lisp','(BINDINGS &BODY FORMS)').
arglistss('stringp','common-lisp','(object)').
arglistss('make-two-way-stream','common-lisp','(input-stream output-stream)').
arglistss('string>','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('string=','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('byte-position','common-lisp','(BYTESPEC)').
arglistss('char-int','common-lisp','(character)').
arglistss('consp','common-lisp','(object)').
arglistss('string<','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('equal','common-lisp','(x y)').
arglistss('array-rank','common-lisp','(array)').
arglistss('fdefinition','common-lisp','(NAME)').
arglistss('set-macro-character','common-lisp','(char new-function &optional non-terminating-p readtable)').
arglistss('caaar','common-lisp','(list)').
arglistss('slot-makunbound','common-lisp','(OBJECT SLOT-NAME)').
arglistss('bit-vector-p','common-lisp','(object)').
arglistss('some','common-lisp','(PREDICATE SEQUENCE &REST MORE-SEQUENCES)').
arglistss('delete-if','common-lisp','(PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0) KEY END COUNT)').
arglistss('defstruct','common-lisp','(NAME-AND-OPTIONS &REST SLOTS)').
arglistss('caadr','common-lisp','(list)').
arglistss('write-line','common-lisp','(STRING &OPTIONAL (STREAM *STANDARD-OUTPUT*) &KEY (START 0) END)').
arglistss('decf','common-lisp','(PLACE &OPTIONAL (DELTA 1))').
arglistss('cddr','common-lisp','(list)').
arglistss('namestring','common-lisp','(pathname)').
arglistss('cddar','common-lisp','(list)').
arglistss('char-not-lessp','common-lisp','(&rest characters)').
arglistss('typep','common-lisp','(OBJECT TYPE &OPTIONAL ENVIRONMENT)').
arglistss('char-name','common-lisp','(character)').
arglistss('prog1','common-lisp','(FIRST-FORM &REST FORMS)').
arglistss('float-digits','common-lisp','(float)').
arglistss('make-random-state','common-lisp','(&optional state)').
arglistss('shadow','common-lisp','(symbol-names &optional package)').
arglistss('constantly','common-lisp','(X)').
arglistss('stable-sort','common-lisp','(SEQUENCE PREDICATE &REST ARGS &KEY KEY)').
arglistss('prog2','common-lisp','(FIRST-FORM SECOND-FORM &REST FORMS)').
arglistss('elt','common-lisp','(LAMBDA (SEQUENCE INDEX) (BLOCK ELT (%ELT SEQUENCE INDEX)))').
arglistss('array-row-major-index','common-lisp','(ARRAY &REST SUBSCRIPTS)').
arglistss('pprint-logical-block','common-lisp','((STREAM-SYMBOL OBJECT &KEY (PREFIX  PREFIX-P) (PER-LINE-PREFIX  PER-LINE-PREFIX-P) (SUFFIX )) &BODY BODY)').
arglistss('fresh-line','common-lisp','(&OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('macroexpand-1','common-lisp','(form &optional env)').
arglistss('char<','common-lisp','(&rest characters)').
arglistss('char=','common-lisp','(&rest characters)').
arglistss('pathname','common-lisp','(pathspec)').
arglistss('in-package','common-lisp','(NAME)').
arglistss('gensym','common-lisp','(&optional x)').
arglistss('incf','common-lisp','(PLACE &OPTIONAL (DELTA 1))').
arglistss('delete-package','common-lisp','(PACKAGE)').
arglistss('pprint-tabular','common-lisp','(STREAM LIST &OPTIONAL (COLON-P T) AT-SIGN-P (TABSIZE NIL))').
arglistss('file-namestring','common-lisp','(pathname)').
arglistss('cadddr','common-lisp','(LIST)').
arglistss('position-if','common-lisp','(TEST SEQUENCE &REST ARGS &KEY FROM-END (START 0) KEY END)').
arglistss('array-element-type','common-lisp','(array)').
arglistss('fmakunbound','common-lisp','(name)').
arglistss('eighth','common-lisp','(LIST)').
arglistss('denominator','common-lisp','(rational)').
arglistss('nthcdr','common-lisp','(n list)').
arglistss('imagpart','common-lisp','(number)').
arglistss('abs','common-lisp','(number)').
arglistss('cdar','common-lisp','(list)').
arglistss('wild-pathname-p','common-lisp','(PATHNAME &OPTIONAL FIELD-KEY)').
arglistss('set-exclusive-or','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('read-preserving-whitespace','common-lisp','(&optional input-stream eof-error-p eof-value recursive-p)').
arglistss('delete-if-not','common-lisp','(PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0) END KEY COUNT)').
arglistss('logical-pathname','common-lisp','(PATHSPEC)').
arglistss('array-displacement','common-lisp','(array)').
arglistss('psetq','common-lisp','(&ENVIRONMENT ENV &REST ARGS)').
arglistss('cdr','common-lisp','(list)').
arglistss('caaadr','common-lisp','(LIST)').
arglistss('simple-bit-vector-p','common-lisp','(object)').
arglistss('pprint-tab','common-lisp','(KIND COLNUM COLINC &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('nset-difference','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('macroexpand','common-lisp','(form &optional env)').
arglistss('caddar','common-lisp','(LIST)').
arglistss('nconc','common-lisp','(&rest lists)').
arglistss('cdddr','common-lisp','(list)').
arglistss('handler-case','common-lisp','(FORM &REST CASES)').
arglistss('rest','common-lisp','(list)').
arglistss('getf','common-lisp','(plist indicator &optional default)').
arglistss('vector','common-lisp','(&rest objects)').
arglistss('simple-string-p','common-lisp','(object)').
arglistss('complex','common-lisp','(realpart &optional imagpart)').
arglistss('type-error-datum','common-lisp','(condition)').
arglistss('ensure-generic-function','common-lisp','(FUNCTION-NAME &REST ALL-KEYS &KEY LAMBDA-LIST GENERIC-FUNCTION-CLASS METHOD-CLASS METHOD-COMBINATION ARGUMENT-PRECEDENCE-ORDER DECLARATIONS DOCUMENTATION &ALLOW-OTHER-KEYS)').
arglistss('integer-decode-float','common-lisp','(float)').
arglistss('package-shadowing-symbols','common-lisp','(package)').
arglistss('hash-table-p','common-lisp','(object)').
arglistss('ninth','common-lisp','(LIST)').
arglistss('copy-readtable','common-lisp','(&optional from-readtable to-readtable)').
arglistss('find-class','common-lisp','(symbol &optional errorp environment)').
arglistss('zerop','common-lisp','(number)').
arglistss('macro-function','common-lisp','(symbol &optional environment)').
arglistss('scale-float','common-lisp','(float integer)').
arglistss('car','common-lisp','(list)').
arglistss('unread-char','common-lisp','(character &optional input-stream)').
arglistss('rplacd','common-lisp','(cons object)').
arglistss('rplaca','common-lisp','(cons object)').
arglistss('adjustable-array-p','common-lisp','(array)').
arglistss('array-dimension','common-lisp','(array axis-number)').
arglistss('concatenate','common-lisp','(RESULT-TYPE &REST SEQUENCES)').
arglistss('string-right-trim','common-lisp','(CHAR-BAG STRING &AUX END)').
arglistss('cddadr','common-lisp','(LIST)').
arglistss('set-syntax-from-char','common-lisp','(to-char from-char &optional to-readtable from-readtable)').
arglistss('sbit','common-lisp','(SIMPLE-BIT-ARRAY &REST SUBSCRIPTS)').
arglistss('read-char','common-lisp','(&optional input-stream eof-error-p eof-value recursive-p)').
arglistss('invoke-restart-interactively','common-lisp','(RESTART)').
arglistss('vector-push-extend','common-lisp','(new-element vector &optional extension)').
arglistss('cddaar','common-lisp','(LIST)').
arglistss('string-left-trim','common-lisp','(CHAR-BAG STRING &AUX END)').
arglistss('muffle-warning','common-lisp','(&OPTIONAL CONDITION)').
arglistss('signal','common-lisp','(DATUM &REST ARGUMENTS)').
arglistss('remove','common-lisp','(ITEM SEQUENCE &REST ARGS &KEY FROM-END (TEST (FUNCTION EQL)) TEST-NOT (START 0) END COUNT KEY)').
arglistss('symbolp','common-lisp','(object)').
arglistss('remhash','common-lisp','(key hash-table)').
arglistss('hash-table-rehash-threshold','common-lisp','(hash-table)').
arglistss('write','common-lisp','(OBJECT &KEY ((STREAM STREAM) *STANDARD-OUTPUT*) ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*) ((RADIX *PRINT-RADIX*) *PRINT-RADIX*) ((BASE *PRINT-BASE*) *PRINT-BASE*) ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*) ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*) ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*) ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*) ((CASE *PRINT-CASE*) *PRINT-CASE*) ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*) ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*) ((READABLY *PRINT-READABLY*) *PRINT-READABLY*) ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*) *PRINT-RIGHT-MARGIN*) ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*) ((LINES *PRINT-LINES*) *PRINT-LINES*) ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*) *PRINT-PPRINT-DISPATCH*))').
arglistss('defsetf','common-lisp','(ACCESS-FN &REST REST)').
arglistss('char','common-lisp','(string index)').
arglistss('caaddr','common-lisp','(LIST)').
arglistss('length','common-lisp','(LAMBDA (SEQUENCE) (BLOCK LENGTH (%LENGTH SEQUENCE)))').
arglistss('hash-table-count','common-lisp','(hash-table)').
arglistss('char<=','common-lisp','(&rest characters)').
arglistss('upgraded-array-element-type','common-lisp','(typespec &optional environment)').
arglistss('count','common-lisp','(ITEM SEQUENCE &REST ARGS &KEY FROM-END (TEST (FUNCTION EQL) TEST-P) (TEST-NOT NIL TEST-NOT-P) (START 0) END KEY)').
arglistss('get','common-lisp','(symbol indicator &optional default)').
arglistss('hash-table-size','common-lisp','(hash-table)').
arglistss('graphic-char-p','common-lisp','(char)').
arglistss('complexp','common-lisp','(object)').
arglistss('function-lambda-expression','common-lisp','(function)').
arglistss('upper-case-p','common-lisp','(character)').
arglistss('nstring-upcase','common-lisp','(STRING &KEY (START 0) END)').
arglistss('break','common-lisp','(&OPTIONAL (FORMAT-CONTROL BREAK called) &REST FORMAT-ARGUMENTS)').
arglistss('proclaim','common-lisp','(DECLARATION-SPECIFIER)').
arglistss('slot-value','common-lisp','(OBJECT SLOT-NAME)').
arglistss('get-setf-expansion','common-lisp','(FORM &OPTIONAL ENVIRONMENT)').
arglistss('standard-char-p','common-lisp','(character)').
arglistss('caadar','common-lisp','(LIST)').
arglistss('defvar','common-lisp','(VAR &OPTIONAL (VAL NIL VALP) (DOC NIL DOCP))').
arglistss('clear-input','common-lisp','(&optional input-stream)').
arglistss('string<=','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('package-name','common-lisp','(package)').
arglistss('symbol-package','common-lisp','(symbol)').
arglistss('time','common-lisp','(FORM)').
arglistss('copy-structure','common-lisp','(structure)').
arglistss('provide','common-lisp','(MODULE-NAME)').
arglistss('string-upcase','common-lisp','(STRING &KEY (START 0) END)').
arglistss('endp','common-lisp','(list)').
arglistss('code-char','common-lisp','(code)').
arglistss('array-dimensions','common-lisp','(array)').
arglistss('functionp','common-lisp','(object)').
arglistss('string-not-greaterp','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('invoke-debugger','common-lisp','(CONDITION)').
arglistss('compute-restarts','common-lisp','(&OPTIONAL CONDITION)').
arglistss('finish-output','common-lisp','(&OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('compiled-function-p','common-lisp','(object)').
arglistss('char-greaterp','common-lisp','(&rest characters)').
arglistss('symbol-function','common-lisp','(symbol)').
arglistss('string-greaterp','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('formatter','common-lisp','(CONTROL-STRING)').
arglistss('pprint-fill','common-lisp','(STREAM OBJECT &OPTIONAL (COLON-P T) AT-SIGN-P)').
arglistss('use-package','common-lisp','(packages-to-use &optional package)').
arglistss('last','common-lisp','(list &optional n)').
arglistss('lambda','common-lisp','(LAMBDA-LIST &REST BODY)').
arglistss('warn','common-lisp','(DATUM &REST ARGUMENTS)').
arglistss('set-pprint-dispatch','common-lisp','(TYPE-SPECIFIER FUNCTION &OPTIONAL (PRIORITY 0) (TABLE *PRINT-PPRINT-DISPATCH*))').
arglistss('reverse','common-lisp','(LAMBDA (SEQUENCE) (BLOCK REVERSE (%REVERSE SEQUENCE)))').
arglistss('read-delimited-list','common-lisp','(char &optional input-stream recursive-p)').
arglistss('defconstant','common-lisp','(NAME INITIAL-VALUE &OPTIONAL DOCSTRING)').
arglistss('inspect','common-lisp','(OBJ)').
arglistss('union','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('pathname-host','common-lisp','(PATHNAME &KEY (CASE LOCAL))').
arglistss('string-not-equal','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('keywordp','common-lisp','(object)').
arglistss('stream-external-format','common-lisp','(stream)').
arglistss('sxhash','common-lisp','(object)').
arglistss('fboundp','common-lisp','(name)').
arglistss('seventh','common-lisp','(LIST)').
arglistss('directory-namestring','common-lisp','(pathname)').
arglistss('pathnamep','common-lisp','(object)').
arglistss('find-if','common-lisp','(TEST SEQUENCE &REST ARGS &KEY FROM-END (START 0) END KEY)').
arglistss('file-position','common-lisp','(stream &optional position-spec)').
arglistss('string>=','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('min','common-lisp','(&rest reals)').
arglistss('pathname-directory','common-lisp','(PATHNAME &KEY (CASE LOCAL))').
arglistss('error','common-lisp','(DATUM &REST ARGUMENTS)').
arglistss('intern','common-lisp','(string &optional package)').
arglistss('do-all-symbols','common-lisp','((VAR &OPTIONAL RESULT-FORM) &BODY BODY)').
arglistss('position','common-lisp','(ITEM SEQUENCE &REST ARGS &KEY FROM-END (TEST (FUNCTION EQL)) TEST-NOT (START 0) END KEY)').
arglistss('symbol-name','common-lisp','(symbol)').
arglistss('unintern','common-lisp','(symbol &optional package)').
arglistss('lower-case-p','common-lisp','(character)').
arglistss('second','common-lisp','(list)').
arglistss('define-setf-expander','common-lisp','(ACCESS-FN LAMBDA-LIST &BODY BODY)').
arglistss('ldb-test','common-lisp','(BYTESPEC INTEGER)').
arglistss('position-if-not','common-lisp','(TEST SEQUENCE &REST ARGS &KEY FROM-END (START 0) KEY END)').
arglistss('subtypep','common-lisp','(TYPE1 TYPE2 &OPTIONAL ENVIRONMENT)').
arglistss('equalp','common-lisp','(x y)').
arglistss('pathname-type','common-lisp','(PATHNAME &KEY (CASE LOCAL))').
arglistss('member','common-lisp','(ITEM LIST &KEY KEY TEST TEST-NOT)').
arglistss('nstring-downcase','common-lisp','(STRING &KEY (START 0) END)').
arglistss('float','common-lisp','(number &optional prototype)').
arglistss('read-char-no-hang','common-lisp','(&optional input-stream eof-error-p eof-value recursive-p)').
arglistss('values-list','common-lisp','(list)').
arglistss('write-to-string','common-lisp','(OBJECT &KEY ((ESCAPE *PRINT-ESCAPE*) *PRINT-ESCAPE*) ((RADIX *PRINT-RADIX*) *PRINT-RADIX*) ((BASE *PRINT-BASE*) *PRINT-BASE*) ((CIRCLE *PRINT-CIRCLE*) *PRINT-CIRCLE*) ((PRETTY *PRINT-PRETTY*) *PRINT-PRETTY*) ((LEVEL *PRINT-LEVEL*) *PRINT-LEVEL*) ((LENGTH *PRINT-LENGTH*) *PRINT-LENGTH*) ((CASE *PRINT-CASE*) *PRINT-CASE*) ((ARRAY *PRINT-ARRAY*) *PRINT-ARRAY*) ((GENSYM *PRINT-GENSYM*) *PRINT-GENSYM*) ((READABLY *PRINT-READABLY*) *PRINT-READABLY*) ((RIGHT-MARGIN *PRINT-RIGHT-MARGIN*) *PRINT-RIGHT-MARGIN*) ((MISER-WIDTH *PRINT-MISER-WIDTH*) *PRINT-MISER-WIDTH*) ((LINES *PRINT-LINES*) *PRINT-LINES*) ((PPRINT-DISPATCH *PRINT-PPRINT-DISPATCH*) *PRINT-PPRINT-DISPATCH*))').
arglistss('ignore-errors','common-lisp','(&REST FORMS)').
arglistss('two-way-stream-input-stream','common-lisp','(two-way-stream)').
arglistss('read','common-lisp','(&optional input-stream eof-error-p eof-value recursive-p)').
arglistss('numberp','common-lisp','(object)').
arglistss('special-operator-p','common-lisp','(symbol)').
arglistss('prin1-to-string','common-lisp','(OBJECT)').
arglistss('cadar','common-lisp','(list)').
arglistss('make-list','common-lisp','(SIZE &KEY INITIAL-ELEMENT)').
arglistss('find-if-not','common-lisp','(TEST SEQUENCE &REST ARGS &KEY FROM-END (START 0) END KEY)').
arglistss('package-used-by-list','common-lisp','(package)').
arglistss('third','common-lisp','(list)').
arglistss('array-has-fill-pointer-p','common-lisp','(array)').
arglistss('evenp','common-lisp','(integer)').
arglistss('plusp','common-lisp','(real)').
arglistss('caddr','common-lisp','(list)').
arglistss('make-hash-table','common-lisp','(&KEY (TEST (QUOTE EQL)) (SIZE 11) (REHASH-SIZE 1.5) (REHASH-THRESHOLD 0.75) (WEAKNESS NIL))').
arglistss('pathname-device','common-lisp','(PATHNAME &KEY (CASE LOCAL))').
arglistss('cadr','common-lisp','(list)').
arglistss('user-homedir-pathname','common-lisp','(&optional host)').
arglistss('define-method-combination','common-lisp','(&WHOLE FORM NAME &REST ARGS)').
arglistss('first','common-lisp','(list)').
arglistss('readtable-case','common-lisp','(readtable)').
arglistss('require','common-lisp','(MODULE-NAME &OPTIONAL PATHNAMES)').
arglistss('write-char','common-lisp','(CHAR &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('string-downcase','common-lisp','(STRING &KEY (START 0) END)').
arglistss('atom','common-lisp','(object)').
arglistss('max','common-lisp','(&rest reals)').
arglistss('characterp','common-lisp','(object)').
arglistss('remprop','common-lisp','(symbol indicator)').
arglistss('define-condition','common-lisp','(NAME (&REST PARENT-TYPES) (&REST SLOT-SPECS) &BODY OPTIONS)').
arglistss('rational','common-lisp','(number)').
arglistss('dpb','common-lisp','(NEWBYTE BYTESPEC INTEGER)').
arglistss('pprint','common-lisp','(OBJECT &OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('cadadr','common-lisp','(LIST)').
arglistss('subsetp','common-lisp','(LIST1 LIST2 &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('byte','common-lisp','(SIZE POSITION)').
arglistss('caar','common-lisp','(list)').
arglistss('every','common-lisp','(PREDICATE SEQUENCE &REST MORE-SEQUENCES)').
arglistss('string-trim','common-lisp','(CHAR-BAG STRING &AUX END)').
arglistss('set-dispatch-macro-character','common-lisp','(disp-char sub-char new-function &optional readtable)').
arglistss('use-value','common-lisp','(VALUE &OPTIONAL CONDITION)').
arglistss('oddp','common-lisp','(integer)').
arglistss('adjoin','common-lisp','(ITEM LIST &KEY KEY (TEST (FUNCTION EQL) TESTP) (TEST-NOT NIL NOTP))').
arglistss('make-package','common-lisp','(NAME &KEY NICKNAMES USE)').
arglistss('maphash','common-lisp','(function hash-table)').
arglistss('apply','common-lisp','(function &rest args)').
arglistss('char-downcase','common-lisp','(character)').
arglistss('package-nicknames','common-lisp','(package)').
arglistss('bit','common-lisp','(BIT-ARRAY &REST SUBSCRIPTS)').
arglistss('delete-duplicates','common-lisp','(SEQUENCE &REST ARGS &KEY (TEST (FUNCTION EQL)) TEST-NOT (START 0) FROM-END END KEY)').
arglistss('pathname-version','common-lisp','(pathname)').
arglistss('notevery','common-lisp','(PREDICATE SEQUENCE &REST MORE-SEQUENCES)').
arglistss('package-use-list','common-lisp','(package)').
arglistss('cadaar','common-lisp','(LIST)').
arglistss('char-equal','common-lisp','(&rest characters)').
arglistss('list','common-lisp','(&rest objects)').
arglistss('eval','common-lisp','(LAMBDA (FORM) (BLOCK EVAL (%EVAL FORM)))').
arglistss('fill-pointer','common-lisp','(vector)').
arglistss('define-modify-macro','common-lisp','(NAME LAMBDA-LIST FUNCTION &OPTIONAL DOC-STRING)').
arglistss('with-simple-restart','common-lisp','((RESTART-NAME FORMAT-STRING &REST FORMAT-ARGUMENTS) &BODY FORMS)').
arglistss('get-dispatch-macro-character','common-lisp','(disp-char sub-char &optional readtable)').
arglistss('host-namestring','common-lisp','(pathname)').
arglistss('with-open-stream','common-lisp','(&REST ARGS)').
arglistss('eq','common-lisp','(x y)').
arglistss('char-upcase','common-lisp','(character)').
arglistss('vector-pop','common-lisp','(vector)').
arglistss('adjust-array','common-lisp','(ARRAY NEW-DIMENSIONS &KEY (ELEMENT-TYPE (ARRAY-ELEMENT-TYPE ARRAY)) (INITIAL-ELEMENT NIL INITIAL-ELEMENT-P) (INITIAL-CONTENTS NIL INITIAL-CONTENTS-P) FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)').
arglistss('abort','common-lisp','(&OPTIONAL CONDITION)').
arglistss('string-not-lessp','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('unuse-package','common-lisp','(packages-to-unuse &optional package)').
arglistss('make-array','common-lisp','(DIMENSIONS &KEY (ELEMENT-TYPE T) (INITIAL-ELEMENT NIL INITIAL-ELEMENT-P) INITIAL-CONTENTS ADJUSTABLE FILL-POINTER DISPLACED-TO DISPLACED-INDEX-OFFSET)').
arglistss('remove-if-not','common-lisp','(PREDICATE SEQUENCE &REST ARGS &KEY FROM-END (START 0) END COUNT KEY)').
arglistss('defpackage','common-lisp','(PACKAGE &REST OPTIONS)').
arglistss('copy-pprint-dispatch','common-lisp','(&OPTIONAL (TABLE *PRINT-PPRINT-DISPATCH*))').
arglistss('aref','common-lisp','(array &rest subscripts)').
arglistss('store-value','common-lisp','(VALUE &OPTIONAL CONDITION)').
arglistss('subseq','common-lisp','(LAMBDA (SEQUENCE START &OPTIONAL END) (BLOCK SUBSEQ (%SUBSEQ SEQUENCE START END)))').
arglistss('continue','common-lisp','(&OPTIONAL CONDITION)').
arglistss('append','common-lisp','(&rest lists)').
arglistss('array-total-size','common-lisp','(array)').
arglistss('setf','common-lisp','(&REST ARGS &ENVIRONMENT ENVIRONMENT)').
arglistss('merge-pathnames','common-lisp','(pathname &optional default-pathname default-version)').
arglistss('make-dispatch-macro-character','common-lisp','(char &optional non-terminating-p readtable)').
arglistss('nreconc','common-lisp','(list tail)').
arglistss('with-condition-restarts','common-lisp','(CONDITION-FORM RESTARTS-FORM &BODY BODY)').
arglistss('character','common-lisp','(character)').
arglistss('numerator','common-lisp','(rational)').
arglistss('funcall','common-lisp','(function &rest args)').
arglistss('merge','common-lisp','(RESULT-TYPE SEQUENCE1 SEQUENCE2 PREDICATE &KEY KEY &AUX (L1 (LENGTH SEQUENCE1)) (L2 (LENGTH SEQUENCE2)))').
arglistss('set','common-lisp','(symbol value)').
arglistss('nreverse','common-lisp','(LAMBDA (SEQUENCE) (BLOCK NREVERSE (%NREVERSE SEQUENCE)))').
arglistss('hash-table-rehash-size','common-lisp','(hash-table)').
arglistss('>','common-lisp','(&rest numbers)').
arglistss('char-code','common-lisp','(character)').
arglistss('=','common-lisp','(&rest numbers)').
arglistss('princ-to-string','common-lisp','(OBJECT)').
arglistss('<','common-lisp','(&rest numbers)').
arglistss('/','common-lisp','(numerator &rest denominators)').
arglistss('copy-tree','common-lisp','(object)').
arglistss('-','common-lisp','(minuend &rest subtrahends)').
arglistss('+','common-lisp','(&rest numbers)').
arglistss('*','common-lisp','(&rest numbers)').
arglistss('remove-duplicates','common-lisp','(SEQUENCE &REST ARGS &KEY (TEST (FUNCTION EQL)) TEST-NOT (START 0) FROM-END END KEY)').
arglistss('force-output','common-lisp','(&OPTIONAL (STREAM *STANDARD-OUTPUT*))').
arglistss('string/=','common-lisp','(STRING1 STRING2 &KEY (START1 0) END1 (START2 0) END2)').
arglistss('make-symbol','common-lisp','(name)').
arglistss('format','common-lisp','(DESTINATION CONTROL-STRING &REST FORMAT-ARGUMENTS)').
arglistss('parse-namestring','common-lisp','(THING &OPTIONAL HOST (DEFAULT-PATHNAME *DEFAULT-PATHNAME-DEFAULTS*) &KEY (START 0) END JUNK-ALLOWED)').




  i i i i i i i       ooooo    o        ooooooo   ooooo   ooooo
  I I I I I I I      8     8   8           8     8     o  8    8
  I  \ `+' /  I      8         8           8     8        8    8
   \  `-+-'  /       8         8           8      ooooo   8oooo
    `-__|__-'        8         8           8           8  8
        |            8     o   8           8     o     8  8
  ------+------       ooooo    8oooooo  ooo8ooo   ooooo   8

Welcome to GNU CLISP 2.49 (2010-07-07) <http://clisp.cons.org/>

Copyright (c) Bruno Haible, Michael Stoll 1992, 1993
Copyright (c) Bruno Haible, Marcus Daniels 1994-1997
Copyright (c) Bruno Haible, Pierpaolo Bernardi, Sam Steingold 1998
Copyright (c) Bruno Haible, Sam Steingold 1999-2000
Copyright (c) Sam Steingold, Bruno Haible 2001-2010

Type :h and hit Enter for context help.

[1]> (defun allpackagevalues () (dolist (p (list :cl :user :sys :EXT :clos)) (package-values p)))
ALLPACKAGEVALUES
[2]> (defun maybe-pprint (o) (handler-case (write-to-string o) (t (a)  (let ((*print-readably* nil)) (write-to-string o)))))
MAYBE-PPRINT
[3]> (defun package-values (ps)
  (let ((p (find-package ps))(kwp (or (find-package "TMP") (make-package "TMP" :use ()))))
    (do-all-symbols (sym)
     (when (and (eq (symbol-package sym) p) (boundp sym))
      (let((*package* kwp)
           (*print-escape* t)
You are in the top-level Read-Eval-Print loop.
Help (abbreviated :h) = this list
Use the usual editing capabilities.
(quit) or (exit) leaves CLISP.
   (*PRINT-LINES* nil)(*PRINT-ARRAY* t)(*PRINT-GENSYM* t)(*PRINT-CIRCLE* t)
You are in the top-level Read-Eval-Print loop.
Help (abbreviated :h) = this list
Use the usual editing capabilities.
(quit) or (exit) leaves CLISP.
   (*print-readably* t))(fresh-line)(fresh-line)
         (princ (maybe-pprint `(,(if(constantp sym) :defconstant :defparameter) ,sym ,(symbol-value sym)))))))))
PACKAGE-VALUES
[4]> (allpackagevalues)
(:|DEFPARAMETER| |COMMON-LISP|::|MOST-POSITIVE-LONG-FLOAT| 8.8080652584198167656L646456992)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-LINES*| |COMMON-LISP|::|NIL|)
(:DEFPARAMETER COMMON-LISP:*ERROR-OUTPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-XOR| 6.)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-FIXNUM| 281474976710655.)
(:DEFPARAMETER COMMON-LISP:*READTABLE* #<READTABLE #x000334A73DD8>)
(:|DEFPARAMETER| |COMMON-LISP|::|*| |COMMON-LISP-USER|::|PACKAGE-VALUES|)
(:|DEFCONSTANT| |COMMON-LISP|::|DOUBLE-FLOAT-EPSILON| 1.1102230246251568d-16)
(:|DEFPARAMETER| |COMMON-LISP|::|+|
 (|COMMON-LISP|::|DEFUN| |COMMON-LISP-USER|::|PACKAGE-VALUES| (|COMMON-LISP-USER|::|PS|)
  (|COMMON-LISP|::|LET|
   ((|COMMON-LISP-USER|::|P| (|COMMON-LISP|::|FIND-PACKAGE| |COMMON-LISP-USER|::|PS|))
    (|COMMON-LISP-USER|::|KWP|
     (|COMMON-LISP|::|OR| (|COMMON-LISP|::|FIND-PACKAGE| "TMP") (|COMMON-LISP|::|MAKE-PACKAGE| "TMP" :|USE| |COMMON-LISP|::|NIL|))))
   (|COMMON-LISP|::|DO-ALL-SYMBOLS| (|COMMON-LISP-USER|::|SYM|)
    (|COMMON-LISP|::|WHEN|
     (|COMMON-LISP|::|AND| (|COMMON-LISP|::|EQ| (|COMMON-LISP|::|SYMBOL-PACKAGE| |COMMON-LISP-USER|::|SYM|) |COMMON-LISP-USER|::|P|)
      (|COMMON-LISP|::|BOUNDP| |COMMON-LISP-USER|::|SYM|))
     (|COMMON-LISP|::|LET|
      ((|COMMON-LISP|::|*PACKAGE*| |COMMON-LISP-USER|::|KWP|) (|COMMON-LISP|::|*PRINT-ESCAPE*| |COMMON-LISP|::|T|)
       (|COMMON-LISP|::|*PRINT-LINES*| |COMMON-LISP|::|NIL|) (|COMMON-LISP|::|*PRINT-ARRAY*| |COMMON-LISP|::|T|)
       (|COMMON-LISP|::|*PRINT-GENSYM*| |COMMON-LISP|::|T|) (|COMMON-LISP|::|*PRINT-CIRCLE*| |COMMON-LISP|::|T|)
       (|COMMON-LISP|::|*PRINT-READABLY*| |COMMON-LISP|::|T|))
      (|COMMON-LISP|::|FRESH-LINE|) (|COMMON-LISP|::|FRESH-LINE|)
      (|COMMON-LISP|::|PRINC|
       (|COMMON-LISP-USER|::|MAYBE-PPRINT|
        `(,(|COMMON-LISP|::|IF| (|COMMON-LISP|::|CONSTANTP| |COMMON-LISP-USER|::|SYM|) :|DEFCONSTANT| :|DEFPARAMETER|) ,|COMMON-LISP-USER|::|SYM|
          ,(|COMMON-LISP|::|SYMBOL-VALUE| |COMMON-LISP-USER|::|SYM|))))))))))
(:|DEFPARAMETER| |COMMON-LISP|::|-| (|COMMON-LISP-USER|::|ALLPACKAGEVALUES|))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-IOR| 14.)
(:|DEFPARAMETER| |COMMON-LISP|::|/| (|COMMON-LISP-USER|::|PACKAGE-VALUES|))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-SET| 15.)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-EVAL*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-LEVEL*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|//| (|COMMON-LISP-USER|::|MAYBE-PPRINT|))
(:|DEFPARAMETER| |COMMON-LISP|::|LONG-FLOAT-EPSILON| 5.4210108624275221706L-20)
(:DEFPARAMETER COMMON-LISP:*STANDARD-OUTPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:DEFPARAMETER COMMON-LISP:*QUERY-IO* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-FILE-TRUENAME*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|SHORT-FLOAT-NEGATIVE-EPSILON| 3.81476s-6)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-LONG-FLOAT| 5.676615526003731344L-646456994)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-SUPPRESS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-READABLY*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LAMBDA-PARAMETERS-LIMIT| 4096.)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT| -1.1754944f-38)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-2| 12.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-1| 10.)
(:|DEFCONSTANT| |COMMON-LISP|::|CALL-ARGUMENTS-LIMIT| 4096.)
(:|DEFPARAMETER| |COMMON-LISP|::|*DEBUGGER-HOOK*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|MOST-NEGATIVE-LONG-FLOAT| -8.8080652584198167656L646456992)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT| 2.2250738585072014d-308)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-VERBOSE*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-CLR| 0.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "TMP"))
(:DEFPARAMETER COMMON-LISP:*TRACE-OUTPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-ESCAPE*| |COMMON-LISP|::|T|)
(:DEFPARAMETER COMMON-LISP:*DEBUG-IO* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-C2| 3.)
(:|DEFCONSTANT| |COMMON-LISP|::|LAMBDA-LIST-KEYWORDS|
 (|COMMON-LISP|::|&OPTIONAL| |COMMON-LISP|::|&REST| |COMMON-LISP|::|&KEY| |COMMON-LISP|::|&ALLOW-OTHER-KEYS| |COMMON-LISP|::|&AUX|
  |COMMON-LISP|::|&BODY| |COMMON-LISP|::|&WHOLE| |COMMON-LISP|::|&ENVIRONMENT|))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-C1| 5.)
(:DEFPARAMETER COMMON-LISP:*STANDARD-INPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFCONSTANT| |COMMON-LISP|::|CHAR-CODE-LIMIT| 1114112.)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-PRINT*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-SHORT-FLOAT| 1.1755s-38)
(:|DEFPARAMETER| |COMMON-LISP|::|+++|
 (|COMMON-LISP|::|DEFUN| |COMMON-LISP-USER|::|ALLPACKAGEVALUES| |COMMON-LISP|::|NIL|
  (|COMMON-LISP|::|DOLIST| (|COMMON-LISP-USER|::|P| (|COMMON-LISP|::|LIST| :|CL| :|USER| :|SYS| :|EXT| :|CLOS|))
   (|COMMON-LISP-USER|::|PACKAGE-VALUES| |COMMON-LISP-USER|::|P|))))
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-CASE*| :|UPCASE|)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-POSITIVE-LONG-FLOAT| 5.676615526003731344L-646456994)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-NEGATIVE-LONG-FLOAT| -5.676615526003731344L-646456994)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-GENSYM*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-BASE*| 10.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-PRETTY*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|ARRAY-TOTAL-SIZE-LIMIT| 4294967296.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-RADIX*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|SINGLE-FLOAT-NEGATIVE-EPSILON| 2.9802326f-8)
(:|DEFPARAMETER| |COMMON-LISP|::|LONG-FLOAT-NEGATIVE-EPSILON| 2.7105054312137610853L-20)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-PPRINT-DISPATCH*| (|COMMON-LISP|::|*PRINT-PPRINT-DISPATCH*|))
(:|DEFPARAMETER| |COMMON-LISP|::|*GENSYM-COUNTER*| 3450.)
(:|DEFCONSTANT| |COMMON-LISP|::|INTERNAL-TIME-UNITS-PER-SECOND| 1000000.)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-SHORT-FLOAT| 3.4028s38)
(:|DEFPARAMETER| |COMMON-LISP|::|///| (|COMMON-LISP-USER|::|ALLPACKAGEVALUES|))
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-FILE-PATHNAME*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-SINGLE-FLOAT| 3.4028235f38)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-EQV| 9.)
(:|DEFCONSTANT| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT| -5.676615526003731344L-646456994)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-FIXNUM| -281474976710656.)
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-VERBOSE*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-DOUBLE-FLOAT| 1.7976931348623157d308)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-DOUBLE-FLOAT| 2.2250738585072014d-308)
(:|DEFCONSTANT| |COMMON-LISP|::|ARRAY-DIMENSION-LIMIT| 4294967296.)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-SINGLE-FLOAT| 1.1754944f-38)
(:|DEFPARAMETER| |COMMON-LISP|::|*BREAK-ON-SIGNALS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|SHORT-FLOAT-EPSILON| 7.6295s-6)
(:|DEFCONSTANT| |COMMON-LISP|::|SINGLE-FLOAT-EPSILON| 5.960465f-8)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-ARRAY*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT| -1.1755s-38)
(:|DEFPARAMETER| |COMMON-LISP|::|*MACROEXPAND-HOOK*| #.(|SYSTEM|::|%FIND-SUBR| '|COMMON-LISP|::|FUNCALL|))
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-DEFAULT-FLOAT-FORMAT*| |COMMON-LISP|::|SINGLE-FLOAT|)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-BASE*| 10.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-RIGHT-MARGIN*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ORC1| 13.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ORC2| 11.)
(:|DEFPARAMETER| |COMMON-LISP|::|*RANDOM-STATE*|
 #S(|COMMON-LISP|::|RANDOM-STATE| #*1011011101101001011011110100000110001010000110011100000010110111))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-NOR| 1.)
(:|DEFPARAMETER| |COMMON-LISP|::|*MODULES*| ("readline" "regexp" "syscalls" "i18n"))
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-DOUBLE-FLOAT| -2.2250738585072014d-308)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT| -2.2250738585072014d-308)
(:DEFPARAMETER COMMON-LISP:*TERMINAL-IO* #<IO TERMINAL-STREAM>)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-LENGTH*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-NAND| 7.)
(:|DEFPARAMETER| |COMMON-LISP|::|*DEFAULT-PATHNAME-DEFAULTS*|
 #-CLISP #P""
 #+CLISP
 #S(|COMMON-LISP|::|PATHNAME| :|HOST| |COMMON-LISP|::|NIL| :|DEVICE| |COMMON-LISP|::|NIL| :|DIRECTORY| |COMMON-LISP|::|NIL|
    :|NAME| |COMMON-LISP|::|NIL| :|TYPE| |COMMON-LISP|::|NIL| :|VERSION| |COMMON-LISP|::|NIL|))
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-SINGLE-FLOAT| -1.1754944f-38)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-SHORT-FLOAT| -1.1755s-38)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ANDC2| 2.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ANDC1| 4.)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-PATHNAME*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|PI| 3.1415926535897932385L0)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-DOUBLE-FLOAT| -1.7976931348623157d308)
(:|DEFCONSTANT| |COMMON-LISP|::|ARRAY-RANK-LIMIT| 4096.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-AND| 8.)
(:|DEFCONSTANT| |COMMON-LISP|::|MULTIPLE-VALUES-LIMIT| 128.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-MISER-WIDTH*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|***| |COMMON-LISP-USER|::|ALLPACKAGEVALUES|)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-SINGLE-FLOAT| -3.4028235f38)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT| 1.1754944f-38)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-SHORT-FLOAT| -3.4028s38)
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-PRINT*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|++|
 (|COMMON-LISP|::|DEFUN| |COMMON-LISP-USER|::|MAYBE-PPRINT| (|COMMON-LISP-USER|::|O|)
  (|COMMON-LISP|::|HANDLER-CASE| (|COMMON-LISP|::|WRITE-TO-STRING| |COMMON-LISP-USER|::|O|)
   (|COMMON-LISP|::|T| (|COMMON-LISP-USER|::|A|)
    (|COMMON-LISP|::|LET| ((|COMMON-LISP|::|*PRINT-READABLY*| |COMMON-LISP|::|NIL|))
     (|COMMON-LISP|::|WRITE-TO-STRING| |COMMON-LISP-USER|::|O|))))))
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-CIRCLE*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT| 1.1755s-38)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-TRUENAME*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|DOUBLE-FLOAT-NEGATIVE-EPSILON| 5.551115123125784d-17)
(:|DEFPARAMETER| |COMMON-LISP|::|*FEATURES*|
 (:|READLINE| :|REGEXP| :|SYSCALLS| :|I18N| :|LOOP| :|COMPILER| :|CLOS| :|MOP| :|CLISP| :|ANSI-CL| :|COMMON-LISP| :|LISP=CL| :|INTERPRETER|
  :|SOCKETS| :|GENERIC-STREAMS| :|LOGICAL-PATHNAMES| :|SCREEN| :|FFI| :|GETTEXT| :|UNICODE| :|BASE-CHAR=CHARACTER| :|WORD-SIZE=64| :|PC386|
  :|UNIX|))
(:|DEFPARAMETER| |COMMON-LISP|::|**| |COMMON-LISP-USER|::|MAYBE-PPRINT|)
(:|DEFPARAMETER| |COMMON-LISP|::|MOST-POSITIVE-LONG-FLOAT| 8.8080652584198167656L646456992)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-LINES*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-XOR| 6.)
(:DEFPARAMETER COMMON-LISP:*ERROR-OUTPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-FIXNUM| 281474976710655.)
(:DEFPARAMETER COMMON-LISP:*READTABLE* #<READTABLE #x000334A73DD8>)
(:|DEFPARAMETER| |COMMON-LISP|::|*| |COMMON-LISP-USER|::|PACKAGE-VALUES|)
(:|DEFCONSTANT| |COMMON-LISP|::|DOUBLE-FLOAT-EPSILON| 1.1102230246251568d-16)
(:|DEFPARAMETER| |COMMON-LISP|::|+|
 (|COMMON-LISP|::|DEFUN| |COMMON-LISP-USER|::|PACKAGE-VALUES| (|COMMON-LISP-USER|::|PS|)
  (|COMMON-LISP|::|LET|
   ((|COMMON-LISP-USER|::|P| (|COMMON-LISP|::|FIND-PACKAGE| |COMMON-LISP-USER|::|PS|))
    (|COMMON-LISP-USER|::|KWP|
     (|COMMON-LISP|::|OR| (|COMMON-LISP|::|FIND-PACKAGE| "TMP") (|COMMON-LISP|::|MAKE-PACKAGE| "TMP" :|USE| |COMMON-LISP|::|NIL|))))
   (|COMMON-LISP|::|DO-ALL-SYMBOLS| (|COMMON-LISP-USER|::|SYM|)
    (|COMMON-LISP|::|WHEN|
     (|COMMON-LISP|::|AND| (|COMMON-LISP|::|EQ| (|COMMON-LISP|::|SYMBOL-PACKAGE| |COMMON-LISP-USER|::|SYM|) |COMMON-LISP-USER|::|P|)
      (|COMMON-LISP|::|BOUNDP| |COMMON-LISP-USER|::|SYM|))
     (|COMMON-LISP|::|LET|
      ((|COMMON-LISP|::|*PACKAGE*| |COMMON-LISP-USER|::|KWP|) (|COMMON-LISP|::|*PRINT-ESCAPE*| |COMMON-LISP|::|T|)
       (|COMMON-LISP|::|*PRINT-LINES*| |COMMON-LISP|::|NIL|) (|COMMON-LISP|::|*PRINT-ARRAY*| |COMMON-LISP|::|T|)
       (|COMMON-LISP|::|*PRINT-GENSYM*| |COMMON-LISP|::|T|) (|COMMON-LISP|::|*PRINT-CIRCLE*| |COMMON-LISP|::|T|)
       (|COMMON-LISP|::|*PRINT-READABLY*| |COMMON-LISP|::|T|))
      (|COMMON-LISP|::|FRESH-LINE|) (|COMMON-LISP|::|FRESH-LINE|)
      (|COMMON-LISP|::|PRINC|
       (|COMMON-LISP-USER|::|MAYBE-PPRINT|
        `(,(|COMMON-LISP|::|IF| (|COMMON-LISP|::|CONSTANTP| |COMMON-LISP-USER|::|SYM|) :|DEFCONSTANT| :|DEFPARAMETER|) ,|COMMON-LISP-USER|::|SYM|
          ,(|COMMON-LISP|::|SYMBOL-VALUE| |COMMON-LISP-USER|::|SYM|))))))))))
(:|DEFPARAMETER| |COMMON-LISP|::|-| (|COMMON-LISP-USER|::|ALLPACKAGEVALUES|))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-IOR| 14.)
(:|DEFPARAMETER| |COMMON-LISP|::|/| (|COMMON-LISP-USER|::|PACKAGE-VALUES|))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-SET| 15.)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-EVAL*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-LEVEL*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|//| (|COMMON-LISP-USER|::|MAYBE-PPRINT|))
(:|DEFPARAMETER| |COMMON-LISP|::|LONG-FLOAT-EPSILON| 5.4210108624275221706L-20)
(:DEFPARAMETER COMMON-LISP:*STANDARD-OUTPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:DEFPARAMETER COMMON-LISP:*QUERY-IO* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-FILE-TRUENAME*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|SHORT-FLOAT-NEGATIVE-EPSILON| 3.81476s-6)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-LONG-FLOAT| 5.676615526003731344L-646456994)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-SUPPRESS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-READABLY*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LAMBDA-PARAMETERS-LIMIT| 4096.)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT| -1.1754944f-38)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-2| 12.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-1| 10.)
(:|DEFCONSTANT| |COMMON-LISP|::|CALL-ARGUMENTS-LIMIT| 4096.)
(:|DEFPARAMETER| |COMMON-LISP|::|*DEBUGGER-HOOK*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|MOST-NEGATIVE-LONG-FLOAT| -8.8080652584198167656L646456992)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT| 2.2250738585072014d-308)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-VERBOSE*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-CLR| 0.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "TMP"))
(:DEFPARAMETER COMMON-LISP:*TRACE-OUTPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-ESCAPE*| |COMMON-LISP|::|T|)
(:DEFPARAMETER COMMON-LISP:*DEBUG-IO* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-C2| 3.)
(:|DEFCONSTANT| |COMMON-LISP|::|LAMBDA-LIST-KEYWORDS|
 (|COMMON-LISP|::|&OPTIONAL| |COMMON-LISP|::|&REST| |COMMON-LISP|::|&KEY| |COMMON-LISP|::|&ALLOW-OTHER-KEYS| |COMMON-LISP|::|&AUX|
  |COMMON-LISP|::|&BODY| |COMMON-LISP|::|&WHOLE| |COMMON-LISP|::|&ENVIRONMENT|))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-C1| 5.)
(:DEFPARAMETER COMMON-LISP:*STANDARD-INPUT* #<IO SYNONYM-STREAM COMMON-LISP:*TERMINAL-IO*>)
(:|DEFCONSTANT| |COMMON-LISP|::|CHAR-CODE-LIMIT| 1114112.)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-PRINT*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-SHORT-FLOAT| 1.1755s-38)
(:|DEFPARAMETER| |COMMON-LISP|::|+++|
 (|COMMON-LISP|::|DEFUN| |COMMON-LISP-USER|::|ALLPACKAGEVALUES| |COMMON-LISP|::|NIL|
  (|COMMON-LISP|::|DOLIST| (|COMMON-LISP-USER|::|P| (|COMMON-LISP|::|LIST| :|CL| :|USER| :|SYS| :|EXT| :|CLOS|))
   (|COMMON-LISP-USER|::|PACKAGE-VALUES| |COMMON-LISP-USER|::|P|))))
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-CASE*| :|UPCASE|)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-POSITIVE-LONG-FLOAT| 5.676615526003731344L-646456994)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-NEGATIVE-LONG-FLOAT| -5.676615526003731344L-646456994)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-GENSYM*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-BASE*| 10.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-PRETTY*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|ARRAY-TOTAL-SIZE-LIMIT| 4294967296.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-RADIX*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|LONG-FLOAT-NEGATIVE-EPSILON| 2.7105054312137610853L-20)
(:|DEFCONSTANT| |COMMON-LISP|::|SINGLE-FLOAT-NEGATIVE-EPSILON| 2.9802326f-8)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-PPRINT-DISPATCH*| (|COMMON-LISP|::|*PRINT-PPRINT-DISPATCH*|))
(:|DEFPARAMETER| |COMMON-LISP|::|*GENSYM-COUNTER*| 3914.)
(:|DEFCONSTANT| |COMMON-LISP|::|INTERNAL-TIME-UNITS-PER-SECOND| 1000000.)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-SHORT-FLOAT| 3.4028s38)
(:|DEFPARAMETER| |COMMON-LISP|::|///| (|COMMON-LISP-USER|::|ALLPACKAGEVALUES|))
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-FILE-PATHNAME*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-SINGLE-FLOAT| 3.4028235f38)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-EQV| 9.)
(:|DEFCONSTANT| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT| -5.676615526003731344L-646456994)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-FIXNUM| -281474976710656.)
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-VERBOSE*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-DOUBLE-FLOAT| 2.2250738585072014d-308)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-POSITIVE-DOUBLE-FLOAT| 1.7976931348623157d308)
(:|DEFCONSTANT| |COMMON-LISP|::|ARRAY-DIMENSION-LIMIT| 4294967296.)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-SINGLE-FLOAT| 1.1754944f-38)
(:|DEFPARAMETER| |COMMON-LISP|::|*BREAK-ON-SIGNALS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|SHORT-FLOAT-EPSILON| 7.6295s-6)
(:|DEFCONSTANT| |COMMON-LISP|::|SINGLE-FLOAT-EPSILON| 5.960465f-8)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-ARRAY*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT| -1.1755s-38)
(:|DEFPARAMETER| |COMMON-LISP|::|*READ-DEFAULT-FLOAT-FORMAT*| |COMMON-LISP|::|SINGLE-FLOAT|)
(:|DEFPARAMETER| |COMMON-LISP|::|*MACROEXPAND-HOOK*| #.(|SYSTEM|::|%FIND-SUBR| '|COMMON-LISP|::|FUNCALL|))
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-BASE*| 10.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-RIGHT-MARGIN*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ORC1| 13.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ORC2| 11.)
(:|DEFPARAMETER| |COMMON-LISP|::|*RANDOM-STATE*|
 #S(|COMMON-LISP|::|RANDOM-STATE| #*1011011101101001011011110100000110001010000110011100000010110111))
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-NOR| 1.)
(:|DEFPARAMETER| |COMMON-LISP|::|*MODULES*| ("readline" "regexp" "syscalls" "i18n"))
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-DOUBLE-FLOAT| -2.2250738585072014d-308)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT| -2.2250738585072014d-308)
(:DEFPARAMETER COMMON-LISP:*TERMINAL-IO* #<IO TERMINAL-STREAM>)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-LENGTH*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-NAND| 7.)
(:|DEFPARAMETER| |COMMON-LISP|::|*DEFAULT-PATHNAME-DEFAULTS*|
 #-CLISP #P""
 #+CLISP
 #S(|COMMON-LISP|::|PATHNAME| :|HOST| |COMMON-LISP|::|NIL| :|DEVICE| |COMMON-LISP|::|NIL| :|DIRECTORY| |COMMON-LISP|::|NIL|
    :|NAME| |COMMON-LISP|::|NIL| :|TYPE| |COMMON-LISP|::|NIL| :|VERSION| |COMMON-LISP|::|NIL|))
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-SINGLE-FLOAT| -1.1754944f-38)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-NEGATIVE-SHORT-FLOAT| -1.1755s-38)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ANDC2| 2.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-ANDC1| 4.)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-PATHNAME*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|PI| 3.1415926535897932385L0)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-DOUBLE-FLOAT| -1.7976931348623157d308)
(:|DEFCONSTANT| |COMMON-LISP|::|ARRAY-RANK-LIMIT| 4096.)
(:|DEFCONSTANT| |COMMON-LISP|::|BOOLE-AND| 8.)
(:|DEFCONSTANT| |COMMON-LISP|::|MULTIPLE-VALUES-LIMIT| 128.)
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-MISER-WIDTH*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|***| |COMMON-LISP-USER|::|ALLPACKAGEVALUES|)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-SINGLE-FLOAT| -3.4028235f38)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT| 1.1754944f-38)
(:|DEFCONSTANT| |COMMON-LISP|::|MOST-NEGATIVE-SHORT-FLOAT| -3.4028s38)
(:|DEFPARAMETER| |COMMON-LISP|::|*COMPILE-PRINT*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |COMMON-LISP|::|++|
 (|COMMON-LISP|::|DEFUN| |COMMON-LISP-USER|::|MAYBE-PPRINT| (|COMMON-LISP-USER|::|O|)
  (|COMMON-LISP|::|HANDLER-CASE| (|COMMON-LISP|::|WRITE-TO-STRING| |COMMON-LISP-USER|::|O|)
   (|COMMON-LISP|::|T| (|COMMON-LISP-USER|::|A|)
    (|COMMON-LISP|::|LET| ((|COMMON-LISP|::|*PRINT-READABLY*| |COMMON-LISP|::|NIL|))
     (|COMMON-LISP|::|WRITE-TO-STRING| |COMMON-LISP-USER|::|O|))))))
(:|DEFPARAMETER| |COMMON-LISP|::|*PRINT-CIRCLE*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |COMMON-LISP|::|LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT| 1.1755s-38)
(:|DEFPARAMETER| |COMMON-LISP|::|*LOAD-TRUENAME*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |COMMON-LISP|::|DOUBLE-FLOAT-NEGATIVE-EPSILON| 5.551115123125784d-17)
(:|DEFPARAMETER| |COMMON-LISP|::|*FEATURES*|
 (:|READLINE| :|REGEXP| :|SYSCALLS| :|I18N| :|LOOP| :|COMPILER| :|CLOS| :|MOP| :|CLISP| :|ANSI-CL| :|COMMON-LISP| :|LISP=CL| :|INTERPRETER|
  :|SOCKETS| :|GENERIC-STREAMS| :|LOGICAL-PATHNAMES| :|SCREEN| :|FFI| :|GETTEXT| :|UNICODE| :|BASE-CHAR=CHARACTER| :|WORD-SIZE=64| :|PC386|
  :|UNIX|))
(:|DEFPARAMETER| |COMMON-LISP|::|**| |COMMON-LISP-USER|::|MAYBE-PPRINT|)
(:|DEFCONSTANT| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-TYPE-LOCATION*| 0.)
(:|DEFCONSTANT| |SYSTEM|::|*SECLASS-PURE*| (|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |SYSTEM|::|*UNKNOWN-FREE-VARS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*FRAME-LIMIT-UP*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*PRIN-LINELENGTH*| 145.)
(:|DEFCONSTANT| |SYSTEM|::|FUNCTION-CODES|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|
    (|COMMON-LISP|::|LOGEQV| . 577.) (|COMMON-LISP|::|LOGAND| . 576.) (|COMMON-LISP|::|LOGXOR| . 575.) (|COMMON-LISP|::|LOGIOR| . 574.)
    (|COMMON-LISP|::|LCM| . 573.) (|EXT|::|XGCD| . 572.) (|COMMON-LISP|::|GCD| . 571.) (|COMMON-LISP|::|/| . 570.) (|COMMON-LISP|::|*| . 569.)
    (|COMMON-LISP|::|-| . 568.) (|COMMON-LISP|::|+| . 567.) (|COMMON-LISP|::|MIN| . 566.) (|COMMON-LISP|::|MAX| . 565.)
    (|COMMON-LISP|::|>=| . 564.) (|COMMON-LISP|::|<=| . 563.) (|COMMON-LISP|::|>| . 562.) (|COMMON-LISP|::|<| . 561.)
    (|COMMON-LISP|::|/=| . 560.) (|COMMON-LISP|::|=| . 559.) (|COMMON-LISP|::|MAKE-CONCATENATED-STREAM| . 558.)
    (|COMMON-LISP|::|MAKE-BROADCAST-STREAM| . 557.) (|COMMON-LISP|::|NOTEVERY| . 556.) (|COMMON-LISP|::|NOTANY| . 555.)
    (|COMMON-LISP|::|EVERY| . 554.) (|COMMON-LISP|::|SOME| . 553.) (|COMMON-LISP|::|MAP-INTO| . 552.) (|COMMON-LISP|::|MAP| . 551.)
    (|COMMON-LISP|::|CONCATENATE| . 550.) (|CLOS|::|%ALLOCATE-INSTANCE| . 549.) (|COMMON-LISP|::|NCONC| . 548.) (|COMMON-LISP|::|APPEND| . 547.)
    (|COMMON-LISP|::|LIST*| . 546.) (|COMMON-LISP|::|LIST| . 545.) (|CLOS|::|CLASS-TUPLE-GETHASH| . 544.) (|SYSTEM|::|ERROR-OF-TYPE| . 543.)
    (|COMMON-LISP|::|ERROR| . 542.) (|COMMON-LISP|::|VALUES| . 541.) (|COMMON-LISP|::|MAPCON| . 540.) (|EXT|::|MAPCAP| . 539.)
    (|COMMON-LISP|::|MAPCAN| . 538.) (|COMMON-LISP|::|MAPL| . 537.) (|COMMON-LISP|::|MAPC| . 536.) (|COMMON-LISP|::|MAPLIST| . 535.)
    (|COMMON-LISP|::|MAPCAR| . 534.) (|COMMON-LISP|::|FUNCALL| . 533.) (|COMMON-LISP|::|APPLY| . 532.) (|EXT|::|STRING-CONCAT| . 531.)
    (|COMMON-LISP|::|CHAR-NOT-LESSP| . 530.) (|COMMON-LISP|::|CHAR-NOT-GREATERP| . 529.) (|COMMON-LISP|::|CHAR-GREATERP| . 528.)
    (|COMMON-LISP|::|CHAR-LESSP| . 527.) (|COMMON-LISP|::|CHAR-NOT-EQUAL| . 526.) (|COMMON-LISP|::|CHAR-EQUAL| . 525.)
    (|COMMON-LISP|::|CHAR>=| . 524.) (|COMMON-LISP|::|CHAR<=| . 523.) (|COMMON-LISP|::|CHAR>| . 522.) (|COMMON-LISP|::|CHAR<| . 521.)
    (|COMMON-LISP|::|CHAR/=| . 520.) (|COMMON-LISP|::|CHAR=| . 519.) (|COMMON-LISP|::|SBIT| . 518.) (|COMMON-LISP|::|BIT| . 517.)
    (|COMMON-LISP|::|ARRAY-ROW-MAJOR-INDEX| . 516.) (|COMMON-LISP|::|ARRAY-IN-BOUNDS-P| . 515.) (|SYSTEM|::|STORE| . 514.)
    (|COMMON-LISP|::|AREF| . 513.) (|COMMON-LISP|::|VECTOR| . 512.) (|SYSTEM|::|ENCODINGP| . 511.) (|SYSTEM|::|LOG10| . 510.)
    (|SYSTEM|::|LOG2| . 509.) (|SYSTEM|::|(SETF LONG-FLOAT-DIGITS)| . 508.) (|EXT|::|LONG-FLOAT-DIGITS| . 507.) (|EXT|::|EXQUO| . 506.)
    (|EXT|::|!| . 505.) (|COMMON-LISP|::|MAKE-RANDOM-STATE| . 504.) (|COMMON-LISP|::|RANDOM| . 503.) (|COMMON-LISP|::|DEPOSIT-FIELD| . 502.)
    (|COMMON-LISP|::|DPB| . 501.) (|COMMON-LISP|::|MASK-FIELD| . 500.) (|COMMON-LISP|::|LDB-TEST| . 499.) (|COMMON-LISP|::|LDB| . 498.)
    (|COMMON-LISP|::|BYTE-POSITION| . 497.) (|COMMON-LISP|::|BYTE-SIZE| . 496.) (|COMMON-LISP|::|BYTE| . 495.)
    (|COMMON-LISP|::|INTEGER-LENGTH| . 494.) (|COMMON-LISP|::|LOGCOUNT| . 493.) (|COMMON-LISP|::|ASH| . 492.) (|COMMON-LISP|::|LOGBITP| . 491.)
    (|COMMON-LISP|::|LOGTEST| . 490.) (|COMMON-LISP|::|LOGNOT| . 489.) (|COMMON-LISP|::|BOOLE| . 488.) (|COMMON-LISP|::|LOGORC2| . 487.)
    (|COMMON-LISP|::|LOGORC1| . 486.) (|COMMON-LISP|::|LOGANDC2| . 485.) (|COMMON-LISP|::|LOGANDC1| . 484.) (|COMMON-LISP|::|LOGNOR| . 483.)
    (|COMMON-LISP|::|LOGNAND| . 482.) (|COMMON-LISP|::|IMAGPART| . 481.) (|COMMON-LISP|::|REALPART| . 480.) (|COMMON-LISP|::|COMPLEX| . 479.)
    (|COMMON-LISP|::|INTEGER-DECODE-FLOAT| . 478.) (|COMMON-LISP|::|FLOAT-PRECISION| . 477.) (|COMMON-LISP|::|FLOAT-DIGITS| . 476.)
    (|COMMON-LISP|::|FLOAT-SIGN| . 475.) (|COMMON-LISP|::|FLOAT-RADIX| . 474.) (|COMMON-LISP|::|SCALE-FLOAT| . 473.)
    (|COMMON-LISP|::|DECODE-FLOAT| . 472.) (|COMMON-LISP|::|FROUND| . 471.) (|COMMON-LISP|::|FTRUNCATE| . 470.)
    (|COMMON-LISP|::|FCEILING| . 469.) (|COMMON-LISP|::|FFLOOR| . 468.) (|COMMON-LISP|::|REM| . 467.) (|COMMON-LISP|::|MOD| . 466.)
    (|COMMON-LISP|::|ROUND| . 465.) (|COMMON-LISP|::|TRUNCATE| . 464.) (|COMMON-LISP|::|CEILING| . 463.) (|COMMON-LISP|::|FLOOR| . 462.)
    (|COMMON-LISP|::|DENOMINATOR| . 461.) (|COMMON-LISP|::|NUMERATOR| . 460.) (|COMMON-LISP|::|RATIONALIZE| . 459.)
    (|COMMON-LISP|::|RATIONAL| . 458.) (|COMMON-LISP|::|FLOAT| . 457.) (|COMMON-LISP|::|ATANH| . 456.) (|COMMON-LISP|::|ACOSH| . 455.)
    (|COMMON-LISP|::|ASINH| . 454.) (|COMMON-LISP|::|TANH| . 453.) (|COMMON-LISP|::|COSH| . 452.) (|COMMON-LISP|::|SINH| . 451.)
    (|COMMON-LISP|::|ATAN| . 450.) (|COMMON-LISP|::|ACOS| . 449.) (|COMMON-LISP|::|ASIN| . 448.) (|COMMON-LISP|::|CIS| . 447.)
    (|COMMON-LISP|::|TAN| . 446.) (|COMMON-LISP|::|COS| . 445.) (|COMMON-LISP|::|SIN| . 444.) (|COMMON-LISP|::|SIGNUM| . 443.)
    (|COMMON-LISP|::|PHASE| . 442.) (|COMMON-LISP|::|ABS| . 441.) (|COMMON-LISP|::|ISQRT| . 440.) (|COMMON-LISP|::|SQRT| . 439.)
    (|COMMON-LISP|::|LOG| . 438.) (|COMMON-LISP|::|EXPT| . 437.) (|COMMON-LISP|::|EXP| . 436.) (|COMMON-LISP|::|CONJUGATE| . 435.)
    (|COMMON-LISP|::|1-| . 434.) (|COMMON-LISP|::|1+| . 433.) (|COMMON-LISP|::|EVENP| . 432.) (|COMMON-LISP|::|ODDP| . 431.)
    (|COMMON-LISP|::|MINUSP| . 430.) (|COMMON-LISP|::|PLUSP| . 429.) (|COMMON-LISP|::|ZEROP| . 428.) (|SYSTEM|::|DECIMAL-STRING| . 427.)
    (|COMMON-LISP|::|GENSYM| . 426.) (|COMMON-LISP|::|KEYWORDP| . 424.) (|CS-COMMON-LISP|::|symbol-name| . 423.)
    (|COMMON-LISP|::|SYMBOL-NAME| . 422.) (|COMMON-LISP|::|SYMBOL-PLIST| . 421.) (|COMMON-LISP|::|SYMBOL-PACKAGE| . 420.)
    (|COMMON-LISP|::|REMPROP| . 419.) (|SYSTEM|::|%PUT| . 418.) (|SYSTEM|::|%PUTPLIST| . 417.) (|COMMON-LISP|::|GET-PROPERTIES| . 416.)
    (|COMMON-LISP|::|GETF| . 415.) (|COMMON-LISP|::|GET| . 414.) (|SYSTEM|::|%PROCLAIM-CONSTANT| . 413.) (|SYSTEM|::|%PUTD| . 412.)
    (|COMMON-LISP|::|FILE-LENGTH| . 411.) (|COMMON-LISP|::|FILE-POSITION| . 410.) (|COMMON-LISP|::|WRITE-BYTE| . 409.)
    (|COMMON-LISP|::|READ-BYTE| . 408.) (|SYSTEM|::|BUILT-IN-STREAM-CLOSE| . 407.) (|COMMON-LISP|::|STREAM-EXTERNAL-FORMAT| . 406.)
    (|SYSTEM|::|BUILT-IN-STREAM-ELEMENT-TYPE| . 405.) (|COMMON-LISP|::|OUTPUT-STREAM-P| . 404.) (|COMMON-LISP|::|INPUT-STREAM-P| . 403.)
    (|SYSTEM|::|STRING-STREAM-P| . 402.) (|SYSTEM|::|MAKE-STRING-PUSH-STREAM| . 401.) (|COMMON-LISP|::|GET-OUTPUT-STREAM-STRING| . 400.)
    (|COMMON-LISP|::|MAKE-STRING-OUTPUT-STREAM| . 399.) (|SYSTEM|::|STRING-INPUT-STREAM-INDEX| . 398.)
    (|COMMON-LISP|::|MAKE-STRING-INPUT-STREAM| . 397.) (|SYSTEM|::|ECHO-STREAM-P| . 396.) (|COMMON-LISP|::|MAKE-ECHO-STREAM| . 395.)
    (|SYSTEM|::|TWO-WAY-STREAM-P| . 394.) (|COMMON-LISP|::|MAKE-TWO-WAY-STREAM| . 393.) (|SYSTEM|::|CONCATENATED-STREAM-P| . 392.)
    (|SYSTEM|::|BROADCAST-STREAM-P| . 391.) (|SYSTEM|::|SYNONYM-STREAM-P| . 390.) (|COMMON-LISP|::|MAKE-SYNONYM-STREAM| . 389.)
    (|SYSTEM|::|FILE-STREAM-P| . 388.) (|COMMON-LISP|::|MERGE| . 387.) (|COMMON-LISP|::|SORT| . 386.) (|COMMON-LISP|::|SEARCH| . 385.)
    (|COMMON-LISP|::|MISMATCH| . 384.) (|COMMON-LISP|::|COUNT-IF-NOT| . 383.) (|COMMON-LISP|::|COUNT-IF| . 382.) (|COMMON-LISP|::|COUNT| . 381.)
    (|COMMON-LISP|::|POSITION-IF-NOT| . 380.) (|COMMON-LISP|::|POSITION-IF| . 379.) (|COMMON-LISP|::|POSITION| . 378.)
    (|COMMON-LISP|::|FIND-IF-NOT| . 377.) (|COMMON-LISP|::|FIND-IF| . 376.) (|COMMON-LISP|::|FIND| . 375.)
    (|COMMON-LISP|::|NSUBSTITUTE-IF-NOT| . 374.) (|COMMON-LISP|::|NSUBSTITUTE-IF| . 373.) (|COMMON-LISP|::|NSUBSTITUTE| . 372.)
    (|COMMON-LISP|::|SUBSTITUTE-IF-NOT| . 371.) (|COMMON-LISP|::|SUBSTITUTE-IF| . 370.) (|COMMON-LISP|::|SUBSTITUTE| . 369.)
    (|COMMON-LISP|::|DELETE-DUPLICATES| . 368.) (|COMMON-LISP|::|REMOVE-DUPLICATES| . 367.) (|COMMON-LISP|::|DELETE-IF-NOT| . 366.)
    (|COMMON-LISP|::|DELETE-IF| . 365.) (|COMMON-LISP|::|DELETE| . 364.) (|COMMON-LISP|::|REMOVE-IF-NOT| . 363.)
    (|COMMON-LISP|::|REMOVE-IF| . 362.) (|COMMON-LISP|::|REMOVE| . 361.) (|COMMON-LISP|::|REPLACE| . 360.) (|COMMON-LISP|::|FILL| . 359.)
    (|COMMON-LISP|::|REDUCE| . 358.) (|COMMON-LISP|::|MAKE-SEQUENCE| . 357.) (|COMMON-LISP|::|NREVERSE| . 356.) (|COMMON-LISP|::|REVERSE| . 355.)
    (|COMMON-LISP|::|LENGTH| . 354.) (|COMMON-LISP|::|COPY-SEQ| . 353.) (|COMMON-LISP|::|SUBSEQ| . 352.) (|SYSTEM|::|(SETF ELT)| . 351.)
    (|COMMON-LISP|::|ELT| . 350.) (|SYSTEM|::|SEQUENCEP| . 349.) (|CLOS|::|(SETF STANDARD-INSTANCE-ACCESS)| . 348.)
    (|CLOS|::|STANDARD-INSTANCE-ACCESS| . 347.) (|EXT|::|SYMBOL-MACRO-EXPAND| . 346.) (|SYSTEM|::|SYMBOL-MACRO-P| . 345.)
    (|SYSTEM|::|MACRO-EXPANDER| . 344.) (|SYSTEM|::|MACROP| . 343.) (|CLOS|::|SLOT-EXISTS-P| . 342.) (|CLOS|::|SLOT-MAKUNBOUND| . 341.)
    (|CLOS|::|SLOT-BOUNDP| . 340.) (|CLOS|::|SET-SLOT-VALUE| . 339.) (|CLOS|::|SLOT-VALUE| . 338.) (|CLOS|::|STD-INSTANCE-P| . 337.)
    (|CLOS|::|STRUCTURE-OBJECT-P| . 336.) (|SYSTEM|::|FUNCTION-MACRO-FUNCTION| . 335.) (|SYSTEM|::|MAKE-LOAD-TIME-EVAL| . 334.)
    (|SYSTEM|::|%COPY-GENERIC-FUNCTION| . 333.) (|SYSTEM|::|MAKE-MACRO| . 332.) (|SYSTEM|::|MAKE-CLOSURE| . 331.)
    (|SYSTEM|::|CLOSURE-CONSTS| . 330.) (|SYSTEM|::|CLOSURE-CODEVEC| . 329.) (|SYSTEM|::|CLOSURE-NAME| . 328.)
    (|SYSTEM|::|%STRUCTURE-TYPE-P| . 327.) (|COMMON-LISP|::|COPY-STRUCTURE| . 326.) (|SYSTEM|::|%MAKE-STRUCTURE| . 325.)
    (|SYSTEM|::|%STRUCTURE-STORE| . 324.) (|SYSTEM|::|%STRUCTURE-REF| . 323.) (|SYSTEM|::|%RECORD-LENGTH| . 322.)
    (|SYSTEM|::|%RECORD-STORE| . 321.) (|SYSTEM|::|%RECORD-REF| . 320.) (|SYSTEM|::|%COMPILED-FUNCTION-P| . 319.) (|EXT|::|PROPER-LIST-P| . 318.)
    (|CLOS|::|DEFINED-CLASS-P| . 317.) (|CLOS|::|TYPEP-CLASS| . 316.) (|COMMON-LISP|::|COERCE| . 315.) (|CLOS|::|FIND-CLASS| . 314.)
    (|CLOS|::|CLASS-OF| . 313.) (|COMMON-LISP|::|TYPE-OF| . 312.) (|COMMON-LISP|::|SIMPLE-BIT-VECTOR-P| . 311.)
    (|COMMON-LISP|::|SIMPLE-STRING-P| . 310.) (|COMMON-LISP|::|SIMPLE-VECTOR-P| . 309.) (|COMMON-LISP|::|VECTORP| . 308.)
    (|COMMON-LISP|::|BIT-VECTOR-P| . 307.) (|SYSTEM|::|SIMPLE-ARRAY-P| . 306.) (|COMMON-LISP|::|ARRAYP| . 305.)
    (|COMMON-LISP|::|PACKAGEP| . 304.) (|COMMON-LISP|::|FUNCTIONP| . 303.) (|COMMON-LISP|::|CHARACTERP| . 302.)
    (|SYSTEM|::|LOGICAL-PATHNAME-P| . 301.) (|COMMON-LISP|::|PATHNAMEP| . 300.) (|COMMON-LISP|::|HASH-TABLE-P| . 299.)
    (|COMMON-LISP|::|READTABLEP| . 298.) (|COMMON-LISP|::|RANDOM-STATE-P| . 297.) (|COMMON-LISP|::|STREAMP| . 296.)
    (|COMMON-LISP|::|COMPLEXP| . 295.) (|COMMON-LISP|::|REALP| . 294.) (|SYSTEM|::|LONG-FLOAT-P| . 293.) (|SYSTEM|::|DOUBLE-FLOAT-P| . 292.)
    (|SYSTEM|::|SINGLE-FLOAT-P| . 291.) (|SYSTEM|::|SHORT-FLOAT-P| . 290.) (|COMMON-LISP|::|FLOATP| . 289.) (|COMMON-LISP|::|RATIONALP| . 288.)
    (|SYSTEM|::|FIXNUMP| . 287.) (|COMMON-LISP|::|INTEGERP| . 286.) (|COMMON-LISP|::|LISTP| . 285.) (|SYSTEM|::|CLOSUREP| . 284.)
    (|COMMON-LISP|::|COMPILED-FUNCTION-P| . 283.) (|COMMON-LISP|::|NUMBERP| . 282.) (|COMMON-LISP|::|STRINGP| . 281.)
    (|COMMON-LISP|::|SYMBOLP| . 280.) (|COMMON-LISP|::|ATOM| . 279.) (|COMMON-LISP|::|CONSP| . 278.) (|COMMON-LISP|::|EQUALP| . 277.)
    (|COMMON-LISP|::|EQUAL| . 276.) (|COMMON-LISP|::|EQL| . 275.) (|SYSTEM|::|SAVEMEM| . 274.) (|COMMON-LISP|::|FILE-AUTHOR| . 273.)
    (|COMMON-LISP|::|FILE-WRITE-DATE| . 272.) (|EXT|::|DELETE-DIRECTORY| . 271.) (|EXT|::|MAKE-DIRECTORY| . 270.) (|EXT|::|CD| . 269.)
    (|COMMON-LISP|::|DIRECTORY| . 268.) (|COMMON-LISP|::|OPEN| . 267.) (|COMMON-LISP|::|RENAME-FILE| . 266.)
    (|COMMON-LISP|::|DELETE-FILE| . 265.) (|COMMON-LISP|::|PROBE-FILE| . 264.) (|COMMON-LISP|::|TRUENAME| . 263.)
    (|COMMON-LISP|::|NAMESTRING| . 262.) (|COMMON-LISP|::|MAKE-PATHNAME| . 261.) (|COMMON-LISP|::|ENOUGH-NAMESTRING| . 260.)
    (|COMMON-LISP|::|MERGE-PATHNAMES| . 259.) (|COMMON-LISP|::|HOST-NAMESTRING| . 258.) (|COMMON-LISP|::|DIRECTORY-NAMESTRING| . 257.)
    (|COMMON-LISP|::|FILE-NAMESTRING| . 256.) (|COMMON-LISP|::|PATHNAME-VERSION| . 255.) (|COMMON-LISP|::|PATHNAME-TYPE| . 254.)
    (|COMMON-LISP|::|PATHNAME-NAME| . 253.) (|COMMON-LISP|::|PATHNAME-DIRECTORY| . 252.) (|COMMON-LISP|::|PATHNAME-DEVICE| . 251.)
    (|COMMON-LISP|::|PATHNAME-HOST| . 250.) (|COMMON-LISP|::|PATHNAME| . 249.) (|COMMON-LISP|::|PARSE-NAMESTRING| . 248.)
    (|EXT|::|RE-EXPORT| . 247.) (|SYSTEM|::|%FIND-PACKAGE| . 246.) (|SYSTEM|::|MAP-ALL-SYMBOLS| . 245.) (|SYSTEM|::|MAP-EXTERNAL-SYMBOLS| . 244.)
    (|SYSTEM|::|MAP-SYMBOLS| . 243.) (|CS-COMMON-LISP|::|find-all-symbols| . 242.) (|COMMON-LISP|::|FIND-ALL-SYMBOLS| . 241.)
    (|SYSTEM|::|%IN-PACKAGE| . 240.) (|CS-COMMON-LISP|::|make-package| . 239.) (|COMMON-LISP|::|MAKE-PACKAGE| . 238.)
    (|COMMON-LISP|::|UNUSE-PACKAGE| . 237.) (|COMMON-LISP|::|USE-PACKAGE| . 236.) (|CS-COMMON-LISP|::|shadow| . 235.)
    (|COMMON-LISP|::|SHADOW| . 234.) (|COMMON-LISP|::|SHADOWING-IMPORT| . 233.) (|COMMON-LISP|::|IMPORT| . 232.)
    (|COMMON-LISP|::|UNEXPORT| . 231.) (|COMMON-LISP|::|EXPORT| . 230.) (|COMMON-LISP|::|UNINTERN| . 229.)
    (|CS-COMMON-LISP|::|find-symbol| . 228.) (|COMMON-LISP|::|FIND-SYMBOL| . 227.) (|CS-COMMON-LISP|::|intern| . 226.)
    (|COMMON-LISP|::|INTERN| . 225.) (|COMMON-LISP|::|LIST-ALL-PACKAGES| . 224.) (|COMMON-LISP|::|PACKAGE-SHADOWING-SYMBOLS| . 223.)
    (|COMMON-LISP|::|PACKAGE-USED-BY-LIST| . 222.) (|COMMON-LISP|::|PACKAGE-USE-LIST| . 221.) (|COMMON-LISP|::|RENAME-PACKAGE| . 220.)
    (|COMMON-LISP|::|PACKAGE-NICKNAMES| . 219.) (|COMMON-LISP|::|PACKAGE-NAME| . 218.) (|COMMON-LISP|::|FIND-PACKAGE| . 217.)
    (|COMMON-LISP|::|MAKE-SYMBOL| . 216.) (|SYSTEM|::|%%TIME| . 215.) (|SYSTEM|::|%SLEEP| . 214.)
    (|COMMON-LISP|::|GET-INTERNAL-REAL-TIME| . 213.) (|COMMON-LISP|::|GET-INTERNAL-RUN-TIME| . 212.) (|COMMON-LISP|::|GET-UNIVERSAL-TIME| . 211.)
    (|COMMON-LISP|::|IDENTITY| . 210.) (|COMMON-LISP|::|SOFTWARE-VERSION| . 209.) (|COMMON-LISP|::|SOFTWARE-TYPE| . 208.)
    (|COMMON-LISP|::|LISP-IMPLEMENTATION-VERSION| . 207.) (|COMMON-LISP|::|LISP-IMPLEMENTATION-TYPE| . 206.)
    (|COMMON-LISP|::|RASSOC-IF-NOT| . 205.) (|COMMON-LISP|::|RASSOC-IF| . 204.) (|COMMON-LISP|::|RASSOC| . 203.)
    (|COMMON-LISP|::|ASSOC-IF-NOT| . 202.) (|COMMON-LISP|::|ASSOC-IF| . 201.) (|COMMON-LISP|::|ASSOC| . 200.) (|COMMON-LISP|::|PAIRLIS| . 199.)
    (|COMMON-LISP|::|ACONS| . 198.) (|COMMON-LISP|::|ADJOIN| . 197.) (|COMMON-LISP|::|TAILP| . 196.) (|COMMON-LISP|::|MEMBER-IF-NOT| . 195.)
    (|COMMON-LISP|::|MEMBER-IF| . 194.) (|COMMON-LISP|::|MEMBER| . 193.) (|COMMON-LISP|::|NSUBLIS| . 192.) (|COMMON-LISP|::|SUBLIS| . 191.)
    (|COMMON-LISP|::|NSUBST-IF-NOT| . 190.) (|COMMON-LISP|::|NSUBST-IF| . 189.) (|COMMON-LISP|::|NSUBST| . 188.)
    (|COMMON-LISP|::|SUBST-IF-NOT| . 187.) (|COMMON-LISP|::|SUBST-IF| . 186.) (|COMMON-LISP|::|SUBST| . 185.) (|SYSTEM|::|%RPLACD| . 184.)
    (|COMMON-LISP|::|RPLACD| . 183.) (|SYSTEM|::|%RPLACA| . 182.) (|COMMON-LISP|::|RPLACA| . 181.) (|COMMON-LISP|::|LDIFF| . 180.)
    (|COMMON-LISP|::|NBUTLAST| . 179.) (|COMMON-LISP|::|BUTLAST| . 178.) (|SYSTEM|::|LIST-NREVERSE| . 177.) (|COMMON-LISP|::|NRECONC| . 176.)
    (|COMMON-LISP|::|REVAPPEND| . 175.) (|COMMON-LISP|::|COPY-TREE| . 174.) (|SYSTEM|::|MEMQ| . 173.) (|COMMON-LISP|::|COPY-ALIST| . 172.)
    (|COMMON-LISP|::|COPY-LIST| . 171.) (|COMMON-LISP|::|MAKE-LIST| . 170.) (|COMMON-LISP|::|LAST| . 169.) (|COMMON-LISP|::|NTHCDR| . 168.)
    (|COMMON-LISP|::|TENTH| . 167.) (|COMMON-LISP|::|NINTH| . 166.) (|COMMON-LISP|::|EIGHTH| . 165.) (|COMMON-LISP|::|SEVENTH| . 164.)
    (|COMMON-LISP|::|SIXTH| . 163.) (|COMMON-LISP|::|FIFTH| . 162.) (|COMMON-LISP|::|NTH| . 161.) (|COMMON-LISP|::|LIST-LENGTH| . 160.)
    (|COMMON-LISP|::|ENDP| . 159.) (|COMMON-LISP|::|TREE-EQUAL| . 158.) (|SYSTEM|::|LINE-POSITION| . 157.) (|COMMON-LISP|::|CLEAR-OUTPUT| . 156.)
    (|COMMON-LISP|::|FORCE-OUTPUT| . 155.) (|COMMON-LISP|::|FINISH-OUTPUT| . 154.) (|EXT|::|ELASTIC-NEWLINE| . 153.)
    (|COMMON-LISP|::|FRESH-LINE| . 152.) (|COMMON-LISP|::|TERPRI| . 151.) (|COMMON-LISP|::|WRITE-LINE| . 150.)
    (|COMMON-LISP|::|WRITE-STRING| . 149.) (|COMMON-LISP|::|WRITE-CHAR| . 148.) (|COMMON-LISP|::|PRINC-TO-STRING| . 147.)
    (|COMMON-LISP|::|PRIN1-TO-STRING| . 146.) (|COMMON-LISP|::|WRITE-TO-STRING| . 145.) (|COMMON-LISP|::|PRINC| . 144.)
    (|COMMON-LISP|::|PPRINT| . 143.) (|COMMON-LISP|::|PRINT| . 142.) (|COMMON-LISP|::|PRIN1| . 141.) (|COMMON-LISP|::|WRITE| . 140.)
    (|SYSTEM|::|WHITESPACEP| . 139.) (|COMMON-LISP|::|PARSE-INTEGER| . 138.) (|COMMON-LISP|::|READ-FROM-STRING| . 137.)
    (|COMMON-LISP|::|CLEAR-INPUT| . 136.) (|COMMON-LISP|::|READ-CHAR-NO-HANG| . 135.) (|COMMON-LISP|::|LISTEN| . 134.)
    (|COMMON-LISP|::|PEEK-CHAR| . 133.) (|COMMON-LISP|::|UNREAD-CHAR| . 132.) (|COMMON-LISP|::|READ-CHAR| . 131.)
    (|COMMON-LISP|::|READ-LINE| . 130.) (|COMMON-LISP|::|READ-DELIMITED-LIST| . 129.) (|COMMON-LISP|::|READ-PRESERVING-WHITESPACE| . 128.)
    (|COMMON-LISP|::|READ| . 127.) (|COMMON-LISP|::|GET-DISPATCH-MACRO-CHARACTER| . 126.) (|COMMON-LISP|::|SET-DISPATCH-MACRO-CHARACTER| . 125.)
    (|COMMON-LISP|::|MAKE-DISPATCH-MACRO-CHARACTER| . 124.) (|COMMON-LISP|::|GET-MACRO-CHARACTER| . 123.)
    (|COMMON-LISP|::|SET-MACRO-CHARACTER| . 122.) (|COMMON-LISP|::|SET-SYNTAX-FROM-CHAR| . 121.) (|COMMON-LISP|::|COPY-READTABLE| . 120.)
    (|COMMON-LISP|::|SXHASH| . 119.) (|CLOS|::|CLASS-GETHASH| . 118.) (|SYSTEM|::|HASH-TABLE-ITERATE| . 117.)
    (|SYSTEM|::|HASH-TABLE-ITERATOR| . 116.) (|COMMON-LISP|::|HASH-TABLE-COUNT| . 115.) (|COMMON-LISP|::|CLRHASH| . 114.)
    (|COMMON-LISP|::|MAPHASH| . 113.) (|COMMON-LISP|::|REMHASH| . 112.) (|SYSTEM|::|PUTHASH| . 111.) (|COMMON-LISP|::|GETHASH| . 110.)
    (|COMMON-LISP|::|MAKE-HASH-TABLE| . 109.) (|COMMON-LISP|::|INVOKE-DEBUGGER| . 108.) (|SYSTEM|::|CHECK-FUNCTION-NAME| . 107.)
    (|SYSTEM|::|KEYWORD-TEST| . 106.) (|SYSTEM|::|PARSE-BODY| . 105.) (|SYSTEM|::|FUNCTION-NAME-P| . 104.)
    (|SYSTEM|::|FUNCTION-SIDE-EFFECT| . 103.) (|COMMON-LISP|::|CONSTANTP| . 102.) (|EXT|::|APPLYHOOK| . 101.) (|EXT|::|EVALHOOK| . 100.)
    (|COMMON-LISP|::|EVAL| . 99.) (|COMMON-LISP|::|PROCLAIM| . 98.) (|COMMON-LISP|::|MACROEXPAND-1| . 97.) (|COMMON-LISP|::|MACROEXPAND| . 96.)
    (|COMMON-LISP|::|MACRO-FUNCTION| . 95.) (|SYSTEM|::|UNWIND-TO-DRIVER| . 94.) (|SYSTEM|::|DRIVER| . 93.) (|COMMON-LISP|::|FMAKUNBOUND| . 92.)
    (|COMMON-LISP|::|MAKUNBOUND| . 91.) (|SYSTEM|::|SET-SYMBOL-VALUE| . 90.) (|COMMON-LISP|::|SPECIAL-OPERATOR-P| . 89.)
    (|COMMON-LISP|::|FBOUNDP| . 88.) (|COMMON-LISP|::|BOUNDP| . 87.) (|COMMON-LISP|::|FDEFINITION| . 86.) (|COMMON-LISP|::|SYMBOL-VALUE| . 85.)
    (|EXT|::|SUBSTRING| . 84.) (|COMMON-LISP|::|NAME-CHAR| . 83.) (|CS-COMMON-LISP|::|string| . 82.) (|COMMON-LISP|::|STRING| . 81.)
    (|COMMON-LISP|::|STRING-CAPITALIZE| . 80.) (|COMMON-LISP|::|NSTRING-CAPITALIZE| . 79.) (|COMMON-LISP|::|STRING-DOWNCASE| . 78.)
    (|COMMON-LISP|::|NSTRING-DOWNCASE| . 77.) (|COMMON-LISP|::|STRING-UPCASE| . 76.) (|COMMON-LISP|::|NSTRING-UPCASE| . 75.)
    (|SYSTEM|::|STRING-BOTH-TRIM| . 74.) (|COMMON-LISP|::|MAKE-STRING| . 73.) (|SYSTEM|::|SEARCH-STRING-EQUAL| . 72.)
    (|SYSTEM|::|SEARCH-STRING=| . 71.) (|COMMON-LISP|::|STRING-NOT-LESSP| . 70.) (|COMMON-LISP|::|STRING-NOT-GREATERP| . 69.)
    (|COMMON-LISP|::|STRING-GREATERP| . 68.) (|COMMON-LISP|::|STRING-LESSP| . 67.) (|COMMON-LISP|::|STRING-NOT-EQUAL| . 66.)
    (|COMMON-LISP|::|STRING-EQUAL| . 65.) (|CS-COMMON-LISP|::|string>=| . 64.) (|COMMON-LISP|::|STRING>=| . 63.)
    (|CS-COMMON-LISP|::|string<=| . 62.) (|COMMON-LISP|::|STRING<=| . 61.) (|CS-COMMON-LISP|::|string>| . 60.) (|COMMON-LISP|::|STRING>| . 59.)
    (|CS-COMMON-LISP|::|string<| . 58.) (|COMMON-LISP|::|STRING<| . 57.) (|CS-COMMON-LISP|::|string/=| . 56.) (|COMMON-LISP|::|STRING/=| . 55.)
    (|CS-COMMON-LISP|::|string=| . 54.) (|COMMON-LISP|::|STRING=| . 53.) (|SYSTEM|::|STORE-SCHAR| . 52.) (|SYSTEM|::|STORE-CHAR| . 51.)
    (|COMMON-LISP|::|SCHAR| . 50.) (|COMMON-LISP|::|CHAR| . 49.) (|COMMON-LISP|::|CHAR-NAME| . 48.) (|EXT|::|INT-CHAR| . 47.)
    (|COMMON-LISP|::|CHAR-INT| . 46.) (|COMMON-LISP|::|DIGIT-CHAR| . 45.) (|COMMON-LISP|::|CHAR-DOWNCASE| . 44.)
    (|COMMON-LISP|::|CHAR-UPCASE| . 43.) (|COMMON-LISP|::|CHARACTER| . 42.) (|COMMON-LISP|::|CODE-CHAR| . 41.) (|COMMON-LISP|::|CHAR-CODE| . 40.)
    (|COMMON-LISP|::|ALPHANUMERICP| . 39.) (|COMMON-LISP|::|DIGIT-CHAR-P| . 38.) (|COMMON-LISP|::|BOTH-CASE-P| . 37.)
    (|COMMON-LISP|::|LOWER-CASE-P| . 36.) (|COMMON-LISP|::|UPPER-CASE-P| . 35.) (|COMMON-LISP|::|ALPHA-CHAR-P| . 34.)
    (|EXT|::|STRING-CHAR-P| . 33.) (|COMMON-LISP|::|GRAPHIC-CHAR-P| . 32.) (|COMMON-LISP|::|STANDARD-CHAR-P| . 31.)
    (|COMMON-LISP|::|ADJUST-ARRAY| . 30.) (|COMMON-LISP|::|MAKE-ARRAY| . 29.) (|COMMON-LISP|::|VECTOR-PUSH-EXTEND| . 28.)
    (|COMMON-LISP|::|VECTOR-POP| . 27.) (|COMMON-LISP|::|VECTOR-PUSH| . 26.) (|SYSTEM|::|SET-FILL-POINTER| . 25.)
    (|COMMON-LISP|::|FILL-POINTER| . 24.) (|COMMON-LISP|::|ARRAY-HAS-FILL-POINTER-P| . 23.) (|COMMON-LISP|::|BIT-NOT| . 22.)
    (|COMMON-LISP|::|BIT-ORC2| . 21.) (|COMMON-LISP|::|BIT-ORC1| . 20.) (|COMMON-LISP|::|BIT-ANDC2| . 19.) (|COMMON-LISP|::|BIT-ANDC1| . 18.)
    (|COMMON-LISP|::|BIT-NOR| . 17.) (|COMMON-LISP|::|BIT-NAND| . 16.) (|COMMON-LISP|::|BIT-EQV| . 15.) (|COMMON-LISP|::|BIT-XOR| . 14.)
    (|COMMON-LISP|::|BIT-IOR| . 13.) (|COMMON-LISP|::|BIT-AND| . 12.) (|COMMON-LISP|::|ADJUSTABLE-ARRAY-P| . 11.)
    (|COMMON-LISP|::|ARRAY-TOTAL-SIZE| . 10.) (|COMMON-LISP|::|ARRAY-DIMENSIONS| . 9.) (|COMMON-LISP|::|ARRAY-DIMENSION| . 8.)
    (|COMMON-LISP|::|ARRAY-RANK| . 7.) (|COMMON-LISP|::|ARRAY-ELEMENT-TYPE| . 6.) (|SYSTEM|::|ROW-MAJOR-STORE| . 5.)
    (|COMMON-LISP|::|ROW-MAJOR-AREF| . 4.) (|SYSTEM|::|%COPY-SIMPLE-VECTOR| . 3.) (|EXT|::|SPECIAL-VARIABLE-P| . 2.) (|SYSTEM|::|SUBR-INFO| . 1.)
    (|SYSTEM|::|%FUNTABREF| . 0.)))
(:|DEFPARAMETER| |SYSTEM|::|*SAVED-DEBUG-PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "COMMON-LISP-USER"))
(:|DEFPARAMETER| |SYSTEM|::|*COMPILER-UNLOCKED-PACKAGES*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-KCONSTRUCTOR-LOCATION*| 2.)
(:|DEFPARAMETER| |SYSTEM|::|*KNOWN-FUNCTIONS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-PREDICATE-LOCATION*| 7.)
(:|DEFPARAMETER| |SYSTEM|::|*CURRENT-SOURCE-FILE*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*RECURSE-COUNT-STANDARD-OUTPUT*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*COUTPUT-FILE*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*BIG-ENDIAN*| |COMMON-LISP|::|NIL|)
(:DEFPARAMETER SYSTEM::*SAVED-DEBUG-READTABLE* #<READTABLE #x000334C011C0>)
(:|DEFCONSTANT| |SYSTEM|::|*SECLASS-FOLDABLE*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*COMMON-LISP-USER-PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "COMMON-LISP-USER"))
(:|DEFCONSTANT| |SYSTEM|::|INSTRUCTION-TABLE-K|
 #(|COMMON-LISP|::|LOAD| |SYSTEM|::|LOAD&PUSH| |SYSTEM|::|CONST| |SYSTEM|::|CONST&PUSH| |SYSTEM|::|STORE|))
(:|DEFPARAMETER| |SYSTEM|::|*INHIBIT-FLOATING-POINT-UNDERFLOW*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*NORC*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*DEBUG-PRINT-FRAME-LIMIT*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|SHORT-CODE-OPSIZE| #(15. 25. 21. 30. 8.))
(:|DEFPARAMETER| |SYSTEM|::|*INSPECT-DEBUG*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*STEP-QUIT*| 281474976710655.)
(:|DEFPARAMETER| |SYSTEM|::|*LIBOUTPUT-STREAM*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*FORMAT-UP-AND-OUT*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*DECLARATION-TYPES*|
 (|COMMON-LISP|::|SPECIAL| |COMMON-LISP|::|TYPE| |COMMON-LISP|::|FTYPE| |COMMON-LISP|::|FUNCTION| |COMMON-LISP|::|INLINE|
  |COMMON-LISP|::|NOTINLINE| |COMMON-LISP|::|IGNORE| |COMMON-LISP|::|OPTIMIZE| |COMMON-LISP|::|DYNAMIC-EXTENT| |COMMON-LISP|::|DECLARATION|
  |COMMON-LISP|::|ARRAY| |COMMON-LISP|::|ATOM| |COMMON-LISP|::|BASE-CHAR| |COMMON-LISP|::|BASE-STRING| |COMMON-LISP|::|BIGNUM|
  |COMMON-LISP|::|BIT| |COMMON-LISP|::|BIT-VECTOR| |COMMON-LISP|::|BOOLEAN| |COMMON-LISP|::|CHARACTER| |COMMON-LISP|::|COMPILED-FUNCTION|
  |COMMON-LISP|::|COMPLEX| |COMMON-LISP|::|CONS| |COMMON-LISP|::|DOUBLE-FLOAT| |COMMON-LISP|::|EXTENDED-CHAR| |COMMON-LISP|::|FIXNUM|
  |COMMON-LISP|::|FLOAT| |COMMON-LISP|::|FUNCTION| |COMMON-LISP|::|HASH-TABLE| |COMMON-LISP|::|INTEGER| |COMMON-LISP|::|KEYWORD|
  |COMMON-LISP|::|LIST| |COMMON-LISP|::|LONG-FLOAT| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NULL| |COMMON-LISP|::|NUMBER| |COMMON-LISP|::|PACKAGE|
  |COMMON-LISP|::|PATHNAME| |COMMON-LISP|::|RANDOM-STATE| |COMMON-LISP|::|RATIO| |COMMON-LISP|::|RATIONAL| |COMMON-LISP|::|READTABLE|
  |COMMON-LISP|::|REAL| |COMMON-LISP|::|SEQUENCE| |COMMON-LISP|::|SHORT-FLOAT| |COMMON-LISP|::|SIMPLE-ARRAY| |COMMON-LISP|::|SIMPLE-BASE-STRING|
  |COMMON-LISP|::|SIMPLE-BIT-VECTOR| |COMMON-LISP|::|SIMPLE-STRING| |COMMON-LISP|::|SIMPLE-VECTOR| |COMMON-LISP|::|SINGLE-FLOAT|
  |COMMON-LISP|::|STANDARD-CHAR| |COMMON-LISP|::|STREAM| |COMMON-LISP|::|STRING| |CS-COMMON-LISP|::|string| |EXT|::|STRING-CHAR|
  |COMMON-LISP|::|SYMBOL| |COMMON-LISP|::|T| |COMMON-LISP|::|VECTOR| |COMMON-LISP|::|COMPILE| |SYSTEM|::|SOURCE| |SYSTEM|::|IN-DEFUN|
  |COMMON-LISP|::|IGNORABLE| |SYSTEM|::|READ-ONLY|))
(:|DEFPARAMETER| |SYSTEM|::|*LOCALIZED-RECURSION*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*LISP-PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "COMMON-LISP"))
(:DEFPARAMETER SYSTEM::*HTTP-ENCODING* #<ENCODING CHARSET:UTF-8 :DOS>)
(:|DEFCONSTANT| |SYSTEM|::|FOR-VALUE-TABLE|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|
    (|SYSTEM|::|UNWIND-PROTECT-NORMAL-EXIT| . |SYSTEM|::|ALL|) (|COMMON-LISP|::|THROW| . |SYSTEM|::|ALL|)
    (|SYSTEM|::|RETURN-FROM-I| . |SYSTEM|::|ALL|) (|COMMON-LISP|::|RETURN-FROM| . |SYSTEM|::|ALL|) (|SYSTEM|::|MV-TO-LIST| . |SYSTEM|::|ALL|)
    (|SYSTEM|::|NV-TO-STACK| . |SYSTEM|::|ALL|) (|SYSTEM|::|MV-TO-STACK| . |SYSTEM|::|ALL|) (|COMMON-LISP|::|LIST*| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|SVSET| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|SVREF| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|SYMBOL-FUNCTION| . |SYSTEM|::|ONE|)
    (|COMMON-LISP|::|CONSP| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|ATOM| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|CONS| . |SYSTEM|::|ONE|)
    (|COMMON-LISP|::|CDR| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|CAR| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|EQ| . |SYSTEM|::|ONE|)
    (|COMMON-LISP|::|NOT| . |SYSTEM|::|ONE|) (|SYSTEM|::|CATCH-OPEN| . |SYSTEM|::|ONE|) (|SYSTEM|::|MVCALLP| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|LIST-TO-MV| . |SYSTEM|::|ONE|) (|SYSTEM|::|VALUES1| . |SYSTEM|::|ONE|) (|SYSTEM|::|UNLIST*| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|UNLIST| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|APPLY| . |SYSTEM|::|ONE|) (|SYSTEM|::|CALLCKEY| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|CALLC| . |SYSTEM|::|ONE|) (|SYSTEM|::|CALL2| . |SYSTEM|::|ONE|) (|SYSTEM|::|CALL1| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|MAKE-VECTOR1&PUSH| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|PUSH| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|PROGV| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|BIND| . |SYSTEM|::|ONE|) (|SYSTEM|::|SETVALUE| . |SYSTEM|::|ONE|) (|SYSTEM|::|STOREIC| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|STOREV| . |SYSTEM|::|ONE|) (|SYSTEM|::|STOREC| . |SYSTEM|::|ONE|) (|SYSTEM|::|STOREI| . |SYSTEM|::|ONE|)
    (|SYSTEM|::|STORE| . |SYSTEM|::|ONE|) (|COMMON-LISP|::|LIST| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|HANDLER-BEGIN| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|HANDLER-OPEN| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|UNWIND-PROTECT-CLOSE| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|UNWIND-PROTECT-OPEN| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|GO-I| . |COMMON-LISP|::|NIL|)
    (|COMMON-LISP|::|GO| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|TAGBODY-CLOSE-NIL| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|TAGBODY-OPEN| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|BLOCK-OPEN| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|MVCALL| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|STACK-TO-MV| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|VALUES0| . |COMMON-LISP|::|NIL|)
    (|COMMON-LISP|::|BOUNDP| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|JMPIFBOUNDP| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|PUSH-UNBOUND| . |COMMON-LISP|::|NIL|) (|COMMON-LISP|::|FUNCALL| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|CALLSR| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|CALLS2| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|CALLS1| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|CALL0| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|CALL| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|COPY-CLOSURE| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|VENV| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|BARRIER| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|JMPTAIL| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|JSR| . |COMMON-LISP|::|NIL|) (|COMMON-LISP|::|POP| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|GETVALUE| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|LOADIC| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|LOADV| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|LOADC| . |COMMON-LISP|::|NIL|)
    (|SYSTEM|::|LOADI| . |COMMON-LISP|::|NIL|) (|COMMON-LISP|::|LOAD| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|CONST| . |COMMON-LISP|::|NIL|)
    (|COMMON-LISP|::|T| . |COMMON-LISP|::|NIL|) (|SYSTEM|::|PUSH-NIL| . |COMMON-LISP|::|NIL|) (|COMMON-LISP|::|NIL| . |COMMON-LISP|::|NIL|)))
(:|DEFPARAMETER| |SYSTEM|::|*INTERNAL-COMPILED-FILE-TYPE*| "fas")
(:|DEFPARAMETER| |SYSTEM|::|*SQUEEZE-STRING-SECTION*| 10.)
(:|DEFPARAMETER| |SYSTEM|::|*NOTINLINE-FUNCTIONS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*COMPILED-MODULES*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*FFI-MODULE*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*STEP-WATCH*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*RECURSE-COUNT-DEBUG-IO*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*COMPILE-FILE-LINENO2*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*QUIET*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*OPTIMIZE*| #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ|))
(:|DEFPARAMETER| |SYSTEM|::|*COMPILE-FILE-LINENO1*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*BACKQUOTE-OPTIMIZE-VECTOR*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |SYSTEM|::|*FASOUTPUT-STREAM*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*NOTINLINE-CONSTANTS*| |COMMON-LISP|::|NIL|)
(:DEFPARAMETER SYSTEM::*ACTIVE-RESTARTS*
 (#S(COMMON-LISP:RESTART :NAME COMMON-LISP:ABORT :TEST #<COMPILED-FUNCTION SYSTEM::DEFAULT-RESTART-TEST> :INVOKE-TAG COMMON-LISP:NIL
     :INVOKE-FUNCTION #<COMPILED-FUNCTION SYSTEM::MAIN-LOOP-1-1> :REPORT #<COMPILED-FUNCTION SYSTEM::MAIN-LOOP-1-2>
     :INTERACTIVE #<COMPILED-FUNCTION SYSTEM::DEFAULT-RESTART-INTERACTIVE> :MEANINGFULP COMMON-LISP:T)))
(:|DEFPARAMETER| |SYSTEM|::|*USER-DECLARATION-TYPES*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*FRAME-LIMIT-DOWN*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*IMAGE-DOC*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*BACKQUOTE-OPTIMIZE-NCONC*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |SYSTEM|::|*BACKQUOTE-OPTIMIZE-LIST*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |SYSTEM|::|FORMAT-ORDINAL-ONES|
 #(|COMMON-LISP|::|NIL| "first" "second" "third" "fourth" "fifth" "sixth" "seventh" "eighth" "ninth" "tenth" "eleventh" "twelfth" "thirteenth"
   "fourteenth" "fifteenth" "sixteenth" "seventeenth" "eighteenth" "nineteenth"))
(:|DEFPARAMETER| |SYSTEM|::|*SCRIPT*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |SYSTEM|::|*SECLASS-DIRTY*| (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
(:|DEFPARAMETER| |SYSTEM|::|*STEP-LEVEL*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*INLINE-CONSTANTS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*BREAK-COUNT*| 0.)
(:|DEFCONSTANT| |SYSTEM|::|*KEYWORD-PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "KEYWORD"))
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-SLOTS-LOCATION*| 3.)
(:|DEFPARAMETER| |SYSTEM|::|*CONDITION-RESTARTS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|FUNCTIONS-RETURNING-STRING|
 (|COMMON-LISP|::|STRING| |COMMON-LISP|::|SYMBOL-NAME| |COMMON-LISP|::|CHAR-NAME| |COMMON-LISP|::|NAMESTRING| |COMMON-LISP|::|ENOUGH-NAMESTRING|
  |COMMON-LISP|::|PRINC-TO-STRING| |COMMON-LISP|::|PRIN1-TO-STRING| |COMMON-LISP|::|WRITE-TO-STRING| |COMMON-LISP|::|WITH-OUTPUT-TO-STRING|
  |COMMON-LISP|::|GET-OUTPUT-STREAM-STRING|))
(:|DEFCONSTANT| |SYSTEM|::|INSTRUCTION-CODES|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|
    (|SYSTEM|::|FUNCALL&SKIP&RETGF| . 156.) (|SYSTEM|::|APPLY&SKIP&RET| . 155.) (|SYSTEM|::|CONST&SYMBOL-FUNCTION&STORE| . 154.)
    (|SYSTEM|::|CONST&SYMBOL-FUNCTION&PUSH| . 153.) (|SYSTEM|::|CONST&SYMBOL-FUNCTION| . 152.) (|SYSTEM|::|LOAD&DEC&PUSH| . 151.)
    (|SYSTEM|::|LOAD&INC&PUSH| . 150.) (|SYSTEM|::|LOAD&CDR&PUSH| . 149.) (|SYSTEM|::|LOAD&CAR&PUSH| . 148.) (|SYSTEM|::|LOAD&JMPIFNOT| . 147.)
    (|SYSTEM|::|LOAD&JMPIF| . 146.) (|SYSTEM|::|CALLSR&JMPIFNOT| . 145.) (|SYSTEM|::|CALLSR&JMPIF| . 144.) (|SYSTEM|::|CALLS2&JMPIFNOT| . 143.)
    (|SYSTEM|::|CALLS2&JMPIF| . 142.) (|SYSTEM|::|CALLS1&JMPIFNOT| . 141.) (|SYSTEM|::|CALLS1&JMPIF| . 140.) (|SYSTEM|::|CALL2&JMPIFNOT| . 139.)
    (|SYSTEM|::|CALL2&JMPIF| . 138.) (|SYSTEM|::|CALL1&JMPIFNOT| . 137.) (|SYSTEM|::|CALL1&JMPIF| . 136.) (|SYSTEM|::|LOAD&CAR&STORE| . 135.)
    (|SYSTEM|::|LOAD&DEC&STORE| . 134.) (|SYSTEM|::|LOAD&INC&STORE| . 133.) (|SYSTEM|::|LOAD&CONS&STORE| . 132.)
    (|SYSTEM|::|LOAD&CDR&STORE| . 131.) (|SYSTEM|::|CALLSR&STORE| . 130.) (|SYSTEM|::|CALLS2&STORE| . 129.) (|SYSTEM|::|CALLS1&STORE| . 128.)
    (|SYSTEM|::|LOAD&STOREC| . 127.) (|SYSTEM|::|T&STORE| . 126.) (|SYSTEM|::|NIL&STORE| . 125.) (|SYSTEM|::|LIST*&PUSH| . 124.)
    (|SYSTEM|::|LIST&PUSH| . 123.) (|SYSTEM|::|CONS&PUSH| . 122.) (|SYSTEM|::|CDR&PUSH| . 121.) (|SYSTEM|::|CAR&PUSH| . 120.)
    (|SYSTEM|::|APPLY&PUSH| . 119.) (|SYSTEM|::|FUNCALL&PUSH| . 118.) (|SYSTEM|::|CALLCKEY&PUSH| . 117.) (|SYSTEM|::|CALLC&PUSH| . 116.)
    (|SYSTEM|::|CALLSR&PUSH| . 115.) (|SYSTEM|::|CALLS2&PUSH| . 114.) (|SYSTEM|::|CALLS1&PUSH| . 113.) (|SYSTEM|::|CALL2&PUSH| . 112.)
    (|SYSTEM|::|CALL1&PUSH| . 111.) (|SYSTEM|::|CALL&PUSH| . 110.) (|SYSTEM|::|COPY-CLOSURE&PUSH| . 109.) (|SYSTEM|::|JSR&PUSH| . 108.)
    (|SYSTEM|::|GETVALUE&PUSH| . 107.) (|SYSTEM|::|POP&STORE| . 106.) (|SYSTEM|::|LOADV&PUSH| . 105.) (|SYSTEM|::|LOADC&PUSH| . 104.)
    (|SYSTEM|::|LOADI&PUSH| . 103.) (|SYSTEM|::|LOAD&PUSH| . 102.) (|SYSTEM|::|CONST&PUSH| . 101.) (|SYSTEM|::|T&PUSH| . 100.)
    (|SYSTEM|::|NIL&PUSH| . 99.) (|COMMON-LISP|::|LIST*| . 98.) (|COMMON-LISP|::|LIST| . 97.) (|SYSTEM|::|SVSET| . 96.)
    (|COMMON-LISP|::|SVREF| . 95.) (|COMMON-LISP|::|SYMBOL-FUNCTION| . 94.) (|COMMON-LISP|::|CONS| . 93.) (|COMMON-LISP|::|CDR| . 92.)
    (|COMMON-LISP|::|CAR| . 91.) (|COMMON-LISP|::|EQ| . 90.) (|COMMON-LISP|::|NOT| . 89.) (|SYSTEM|::|HANDLER-BEGIN&PUSH| . 88.)
    (|SYSTEM|::|HANDLER-OPEN| . 87.) (|SYSTEM|::|UNWIND-PROTECT-CLEANUP| . 86.) (|SYSTEM|::|UNWIND-PROTECT-CLOSE| . 85.)
    (|SYSTEM|::|UNWIND-PROTECT-NORMAL-EXIT| . 84.) (|SYSTEM|::|UNWIND-PROTECT-OPEN| . 83.) (|COMMON-LISP|::|THROW| . 82.)
    (|SYSTEM|::|CATCH-CLOSE| . 81.) (|SYSTEM|::|CATCH-OPEN| . 80.) (|SYSTEM|::|GO-I| . 79.) (|COMMON-LISP|::|GO| . 78.)
    (|SYSTEM|::|TAGBODY-CLOSE| . 77.) (|SYSTEM|::|TAGBODY-CLOSE-NIL| . 76.) (|SYSTEM|::|TAGBODY-OPEN| . 75.) (|SYSTEM|::|RETURN-FROM-I| . 74.)
    (|COMMON-LISP|::|RETURN-FROM| . 73.) (|SYSTEM|::|BLOCK-CLOSE| . 72.) (|SYSTEM|::|BLOCK-OPEN| . 71.) (|SYSTEM|::|MVCALL| . 70.)
    (|SYSTEM|::|MVCALLP| . 69.) (|SYSTEM|::|LIST-TO-MV| . 68.) (|SYSTEM|::|MV-TO-LIST| . 67.) (|SYSTEM|::|NV-TO-STACK| . 66.)
    (|SYSTEM|::|MV-TO-STACK| . 65.) (|SYSTEM|::|STACK-TO-MV| . 64.) (|SYSTEM|::|VALUES1| . 63.) (|SYSTEM|::|VALUES0| . 62.)
    (|SYSTEM|::|UNBOUND->NIL| . 61.) (|COMMON-LISP|::|BOUNDP| . 60.) (|SYSTEM|::|JMPIFBOUNDP| . 59.) (|SYSTEM|::|UNLIST*| . 58.)
    (|SYSTEM|::|UNLIST| . 57.) (|SYSTEM|::|PUSH-UNBOUND| . 56.) (|COMMON-LISP|::|APPLY| . 55.) (|COMMON-LISP|::|FUNCALL| . 54.)
    (|SYSTEM|::|CALLCKEY| . 53.) (|SYSTEM|::|CALLC| . 52.) (|SYSTEM|::|CALLSR| . 51.) (|SYSTEM|::|CALLS2| . 50.) (|SYSTEM|::|CALLS1| . 49.)
    (|SYSTEM|::|CALL2| . 48.) (|SYSTEM|::|CALL1| . 47.) (|SYSTEM|::|CALL0| . 46.) (|SYSTEM|::|CALL| . 45.) (|SYSTEM|::|COPY-CLOSURE| . 44.)
    (|SYSTEM|::|MAKE-VECTOR1&PUSH| . 43.) (|SYSTEM|::|VENV| . 42.) (|SYSTEM|::|JMPTAIL| . 41.) (|SYSTEM|::|JSR| . 40.)
    (|SYSTEM|::|JMPHASHV| . 39.) (|SYSTEM|::|JMPHASH| . 38.) (|SYSTEM|::|JMPIFNOTEQTO| . 37.) (|SYSTEM|::|JMPIFEQTO| . 36.)
    (|SYSTEM|::|JMPIFNOTEQ| . 35.) (|SYSTEM|::|JMPIFEQ| . 34.) (|SYSTEM|::|JMPIFCONSP| . 33.) (|SYSTEM|::|JMPIFATOM| . 32.)
    (|SYSTEM|::|JMPIFNOT1| . 31.) (|SYSTEM|::|JMPIF1| . 30.) (|SYSTEM|::|JMPIFNOT| . 29.) (|SYSTEM|::|JMPIF| . 28.) (|SYSTEM|::|JMP| . 27.)
    (|SYSTEM|::|SKIP&RETGF| . 26.) (|SYSTEM|::|SKIP&RET| . 25.) (|SYSTEM|::|SKIPSP| . 24.) (|SYSTEM|::|SKIPI| . 23.) (|SYSTEM|::|SKIP| . 22.)
    (|COMMON-LISP|::|POP| . 21.) (|COMMON-LISP|::|PUSH| . 20.) (|COMMON-LISP|::|PROGV| . 19.) (|SYSTEM|::|UNBIND| . 18.)
    (|SYSTEM|::|UNBIND1| . 17.) (|SYSTEM|::|BIND| . 16.) (|SYSTEM|::|SETVALUE| . 15.) (|SYSTEM|::|GETVALUE| . 14.) (|SYSTEM|::|STOREIC| . 13.)
    (|SYSTEM|::|STOREV| . 12.) (|SYSTEM|::|STOREC| . 11.) (|SYSTEM|::|STOREI| . 10.) (|SYSTEM|::|STORE| . 9.) (|SYSTEM|::|LOADIC| . 8.)
    (|SYSTEM|::|LOADV| . 7.) (|SYSTEM|::|LOADC| . 6.) (|SYSTEM|::|LOADI| . 5.) (|COMMON-LISP|::|LOAD| . 4.) (|SYSTEM|::|CONST| . 3.)
    (|COMMON-LISP|::|T| . 2.) (|SYSTEM|::|PUSH-NIL| . 1.) (|COMMON-LISP|::|NIL| . 0.)))
(:|DEFPARAMETER| |SYSTEM|::|*PACKAGE-TASKS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-BOA-CONSTRUCTORS-LOCATION*| 5.)
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-SIZE-LOCATION*| 1.)
(:|DEFPARAMETER| |SYSTEM|::|*RECURSE-COUNT-ERROR-OUTPUT*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*GC-STATISTICS*| -1.)
(:|DEFCONSTANT| |SYSTEM|::|STRING-DIMENSION-LIMIT| 67108864.)
(:|DEFPARAMETER| |SYSTEM|::|*HTML-CHARS*| ((#\< . "&lt;") (#\> . "&gt;") (#\& . "&amp;")))
(:|DEFCONSTANT| |SYSTEM|::|*HTTP-PORT*| 80.)
(:|DEFPARAMETER| |SYSTEM|::|*KEY-BINDINGS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*TOPLEVEL-DENV*| ((|COMMON-LISP|::|DECLARATION| |COMMON-LISP|::|OPTIMIZE| |COMMON-LISP|::|DECLARATION|)))
(:|DEFPARAMETER| |SYSTEM|::|*LOAD-LEVEL*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*BACKQUOTE-OPTIMIZE-CONS*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |SYSTEM|::|CHAR-INT-LIMIT| 285212672.)
(:|DEFCONSTANT| |SYSTEM|::|SHORT-CODE-OPS| #(157. 172. 197. 218. 248.))
(:|DEFPARAMETER| |SYSTEM|::|*TYPE-CATEGORIES*|
 (|COMMON-LISP|::|ARRAY| |COMMON-LISP|::|COMPLEX| |COMMON-LISP|::|CONS| |COMMON-LISP|::|REAL| |COMMON-LISP|::|CHARACTER|
  |COMMON-LISP|::|PATHNAME| |EXT|::|ENCODING| |COMMON-LISP|::|HASH-TABLE| |COMMON-LISP|::|PACKAGE| |COMMON-LISP|::|RANDOM-STATE|
  |COMMON-LISP|::|READTABLE| |COMMON-LISP|::|SYMBOL| |COMMON-LISP|::|BYTE| |EXT|::|SPECIAL-OPERATOR| |EXT|::|LOAD-TIME-EVAL|
  |EXT|::|SYMBOL-MACRO| |EXT|::|FOREIGN-POINTER| |FFI|::|FOREIGN-ADDRESS| |FFI|::|FOREIGN-VARIABLE| |EXT|::|WEAK-POINTER| |SYSTEM|::|READ-LABEL|
  |SYSTEM|::|FRAME-POINTER| |SYSTEM|::|SYSTEM-INTERNAL| |COMMON-LISP|::|STRUCTURE-OBJECT| |COMMON-LISP|::|STANDARD-OBJECT|))
(:|DEFCONSTANT| |SYSTEM|::|FORMAT-CARDINAL-TENS|
 #(|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| "twenty" "thirty" "forty" "fifty" "sixty" "seventy" "eighty" "ninety"))
(:|DEFPARAMETER| |SYSTEM|::|*LOAD-INPUT-STREAM*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*BACKQUOTE-OPTIMIZE-APPEND*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |SYSTEM|::|*CONSTANT-SPECIAL-VARS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*COUTPUT-STREAM*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|INSTRUCTION-TABLE|
 #((|COMMON-LISP|::|NIL| |SYSTEM|::|O|) (|SYSTEM|::|PUSH-NIL| |SYSTEM|::|N|) (|COMMON-LISP|::|T| |SYSTEM|::|O|) (|SYSTEM|::|CONST| |SYSTEM|::|K|)
   (|COMMON-LISP|::|LOAD| |SYSTEM|::|K|) (|SYSTEM|::|LOADI| |SYSTEM|::|NNN|) (|SYSTEM|::|LOADC| |SYSTEM|::|NN|)
   (|SYSTEM|::|LOADV| |SYSTEM|::|NN|) (|SYSTEM|::|LOADIC| |SYSTEM|::|NNNN|) (|SYSTEM|::|STORE| |SYSTEM|::|K|)
   (|SYSTEM|::|STOREI| |SYSTEM|::|NNN|) (|SYSTEM|::|STOREC| |SYSTEM|::|NN|) (|SYSTEM|::|STOREV| |SYSTEM|::|NN|)
   (|SYSTEM|::|STOREIC| |SYSTEM|::|NNNN|) (|SYSTEM|::|GETVALUE| |SYSTEM|::|N|) (|SYSTEM|::|SETVALUE| |SYSTEM|::|N|)
   (|SYSTEM|::|BIND| |SYSTEM|::|N|) (|SYSTEM|::|UNBIND1| |SYSTEM|::|O|) (|SYSTEM|::|UNBIND| |SYSTEM|::|N|) (|COMMON-LISP|::|PROGV| |SYSTEM|::|O|)
   (|COMMON-LISP|::|PUSH| |SYSTEM|::|O|) (|COMMON-LISP|::|POP| |SYSTEM|::|O|) (|SYSTEM|::|SKIP| |SYSTEM|::|N|)
   (|SYSTEM|::|SKIPI| |SYSTEM|::|NNN|) (|SYSTEM|::|SKIPSP| |SYSTEM|::|NN|) (|SYSTEM|::|SKIP&RET| |SYSTEM|::|N|)
   (|SYSTEM|::|SKIP&RETGF| |SYSTEM|::|N|) (|SYSTEM|::|JMP| |SYSTEM|::|L|) (|SYSTEM|::|JMPIF| |SYSTEM|::|L|) (|SYSTEM|::|JMPIFNOT| |SYSTEM|::|L|)
   (|SYSTEM|::|JMPIF1| |SYSTEM|::|L|) (|SYSTEM|::|JMPIFNOT1| |SYSTEM|::|L|) (|SYSTEM|::|JMPIFATOM| |SYSTEM|::|L|)
   (|SYSTEM|::|JMPIFCONSP| |SYSTEM|::|L|) (|SYSTEM|::|JMPIFEQ| |SYSTEM|::|L|) (|SYSTEM|::|JMPIFNOTEQ| |SYSTEM|::|L|)
   (|SYSTEM|::|JMPIFEQTO| |SYSTEM|::|NL|) (|SYSTEM|::|JMPIFNOTEQTO| |SYSTEM|::|NL|) (|SYSTEM|::|JMPHASH| |SYSTEM|::|NHL|)
   (|SYSTEM|::|JMPHASHV| |SYSTEM|::|NHL|) (|SYSTEM|::|JSR| |SYSTEM|::|L|) (|SYSTEM|::|JMPTAIL| |SYSTEM|::|NNL|) (|SYSTEM|::|VENV| |SYSTEM|::|O|)
   (|SYSTEM|::|MAKE-VECTOR1&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|COPY-CLOSURE| |SYSTEM|::|NN|) (|SYSTEM|::|CALL| |SYSTEM|::|NN|)
   (|SYSTEM|::|CALL0| |SYSTEM|::|N|) (|SYSTEM|::|CALL1| |SYSTEM|::|N|) (|SYSTEM|::|CALL2| |SYSTEM|::|N|) (|SYSTEM|::|CALLS1| |SYSTEM|::|B|)
   (|SYSTEM|::|CALLS2| |SYSTEM|::|B|) (|SYSTEM|::|CALLSR| |SYSTEM|::|NB|) (|SYSTEM|::|CALLC| |SYSTEM|::|O|) (|SYSTEM|::|CALLCKEY| |SYSTEM|::|O|)
   (|COMMON-LISP|::|FUNCALL| |SYSTEM|::|N|) (|COMMON-LISP|::|APPLY| |SYSTEM|::|N|) (|SYSTEM|::|PUSH-UNBOUND| |SYSTEM|::|N|)
   (|SYSTEM|::|UNLIST| |SYSTEM|::|NN|) (|SYSTEM|::|UNLIST*| |SYSTEM|::|NN|) (|SYSTEM|::|JMPIFBOUNDP| |SYSTEM|::|NL|)
   (|COMMON-LISP|::|BOUNDP| |SYSTEM|::|N|) (|SYSTEM|::|UNBOUND->NIL| |SYSTEM|::|N|) (|SYSTEM|::|VALUES0| |SYSTEM|::|O|)
   (|SYSTEM|::|VALUES1| |SYSTEM|::|O|) (|SYSTEM|::|STACK-TO-MV| |SYSTEM|::|N|) (|SYSTEM|::|MV-TO-STACK| |SYSTEM|::|O|)
   (|SYSTEM|::|NV-TO-STACK| |SYSTEM|::|N|) (|SYSTEM|::|MV-TO-LIST| |SYSTEM|::|O|) (|SYSTEM|::|LIST-TO-MV| |SYSTEM|::|O|)
   (|SYSTEM|::|MVCALLP| |SYSTEM|::|O|) (|SYSTEM|::|MVCALL| |SYSTEM|::|O|) (|SYSTEM|::|BLOCK-OPEN| |SYSTEM|::|NL|)
   (|SYSTEM|::|BLOCK-CLOSE| |SYSTEM|::|O|) (|COMMON-LISP|::|RETURN-FROM| |SYSTEM|::|N|) (|SYSTEM|::|RETURN-FROM-I| |SYSTEM|::|NNN|)
   (|SYSTEM|::|TAGBODY-OPEN| |SYSTEM|::|NLX|) (|SYSTEM|::|TAGBODY-CLOSE-NIL| |SYSTEM|::|O|) (|SYSTEM|::|TAGBODY-CLOSE| |SYSTEM|::|O|)
   (|COMMON-LISP|::|GO| |SYSTEM|::|NN|) (|SYSTEM|::|GO-I| |SYSTEM|::|NNNN|) (|SYSTEM|::|CATCH-OPEN| |SYSTEM|::|L|)
   (|SYSTEM|::|CATCH-CLOSE| |SYSTEM|::|O|) (|COMMON-LISP|::|THROW| |SYSTEM|::|O|) (|SYSTEM|::|UNWIND-PROTECT-OPEN| |SYSTEM|::|L|)
   (|SYSTEM|::|UNWIND-PROTECT-NORMAL-EXIT| |SYSTEM|::|O|) (|SYSTEM|::|UNWIND-PROTECT-CLOSE| |SYSTEM|::|O|)
   (|SYSTEM|::|UNWIND-PROTECT-CLEANUP| |SYSTEM|::|O|) (|SYSTEM|::|HANDLER-OPEN| |SYSTEM|::|NC|) (|SYSTEM|::|HANDLER-BEGIN&PUSH| |SYSTEM|::|O|)
   (|COMMON-LISP|::|NOT| |SYSTEM|::|O|) (|COMMON-LISP|::|EQ| |SYSTEM|::|O|) (|COMMON-LISP|::|CAR| |SYSTEM|::|O|)
   (|COMMON-LISP|::|CDR| |SYSTEM|::|O|) (|COMMON-LISP|::|CONS| |SYSTEM|::|O|) (|COMMON-LISP|::|SYMBOL-FUNCTION| |SYSTEM|::|O|)
   (|COMMON-LISP|::|SVREF| |SYSTEM|::|O|) (|SYSTEM|::|SVSET| |SYSTEM|::|O|) (|COMMON-LISP|::|LIST| |SYSTEM|::|N|)
   (|COMMON-LISP|::|LIST*| |SYSTEM|::|N|) (|SYSTEM|::|NIL&PUSH| |SYSTEM|::|O|) (|SYSTEM|::|T&PUSH| |SYSTEM|::|O|)
   (|SYSTEM|::|CONST&PUSH| |SYSTEM|::|K|) (|SYSTEM|::|LOAD&PUSH| |SYSTEM|::|K|) (|SYSTEM|::|LOADI&PUSH| |SYSTEM|::|NNN|)
   (|SYSTEM|::|LOADC&PUSH| |SYSTEM|::|NN|) (|SYSTEM|::|LOADV&PUSH| |SYSTEM|::|NN|) (|SYSTEM|::|POP&STORE| |SYSTEM|::|N|)
   (|SYSTEM|::|GETVALUE&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|JSR&PUSH| |SYSTEM|::|L|) (|SYSTEM|::|COPY-CLOSURE&PUSH| |SYSTEM|::|NN|)
   (|SYSTEM|::|CALL&PUSH| |SYSTEM|::|NN|) (|SYSTEM|::|CALL1&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|CALL2&PUSH| |SYSTEM|::|N|)
   (|SYSTEM|::|CALLS1&PUSH| |SYSTEM|::|B|) (|SYSTEM|::|CALLS2&PUSH| |SYSTEM|::|B|) (|SYSTEM|::|CALLSR&PUSH| |SYSTEM|::|NB|)
   (|SYSTEM|::|CALLC&PUSH| |SYSTEM|::|O|) (|SYSTEM|::|CALLCKEY&PUSH| |SYSTEM|::|O|) (|SYSTEM|::|FUNCALL&PUSH| |SYSTEM|::|N|)
   (|SYSTEM|::|APPLY&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|CAR&PUSH| |SYSTEM|::|O|) (|SYSTEM|::|CDR&PUSH| |SYSTEM|::|O|)
   (|SYSTEM|::|CONS&PUSH| |SYSTEM|::|O|) (|SYSTEM|::|LIST&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|LIST*&PUSH| |SYSTEM|::|N|)
   (|SYSTEM|::|NIL&STORE| |SYSTEM|::|N|) (|SYSTEM|::|T&STORE| |SYSTEM|::|N|) (|SYSTEM|::|LOAD&STOREC| |SYSTEM|::|NNN|)
   (|SYSTEM|::|CALLS1&STORE| |SYSTEM|::|BN|) (|SYSTEM|::|CALLS2&STORE| |SYSTEM|::|BN|) (|SYSTEM|::|CALLSR&STORE| |SYSTEM|::|NBN|)
   (|SYSTEM|::|LOAD&CDR&STORE| |SYSTEM|::|N|) (|SYSTEM|::|LOAD&CONS&STORE| |SYSTEM|::|N|) (|SYSTEM|::|LOAD&INC&STORE| |SYSTEM|::|N|)
   (|SYSTEM|::|LOAD&DEC&STORE| |SYSTEM|::|N|) (|SYSTEM|::|LOAD&CAR&STORE| |SYSTEM|::|NN|) (|SYSTEM|::|CALL1&JMPIF| |SYSTEM|::|NL|)
   (|SYSTEM|::|CALL1&JMPIFNOT| |SYSTEM|::|NL|) (|SYSTEM|::|CALL2&JMPIF| |SYSTEM|::|NL|) (|SYSTEM|::|CALL2&JMPIFNOT| |SYSTEM|::|NL|)
   (|SYSTEM|::|CALLS1&JMPIF| |SYSTEM|::|BL|) (|SYSTEM|::|CALLS1&JMPIFNOT| |SYSTEM|::|BL|) (|SYSTEM|::|CALLS2&JMPIF| |SYSTEM|::|BL|)
   (|SYSTEM|::|CALLS2&JMPIFNOT| |SYSTEM|::|BL|) (|SYSTEM|::|CALLSR&JMPIF| |SYSTEM|::|NBL|) (|SYSTEM|::|CALLSR&JMPIFNOT| |SYSTEM|::|NBL|)
   (|SYSTEM|::|LOAD&JMPIF| |SYSTEM|::|NL|) (|SYSTEM|::|LOAD&JMPIFNOT| |SYSTEM|::|NL|) (|SYSTEM|::|LOAD&CAR&PUSH| |SYSTEM|::|N|)
   (|SYSTEM|::|LOAD&CDR&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|LOAD&INC&PUSH| |SYSTEM|::|N|) (|SYSTEM|::|LOAD&DEC&PUSH| |SYSTEM|::|N|)
   (|SYSTEM|::|CONST&SYMBOL-FUNCTION| |SYSTEM|::|N|) (|SYSTEM|::|CONST&SYMBOL-FUNCTION&PUSH| |SYSTEM|::|N|)
   (|SYSTEM|::|CONST&SYMBOL-FUNCTION&STORE| |SYSTEM|::|NN|) (|SYSTEM|::|APPLY&SKIP&RET| |SYSTEM|::|NN|)
   (|SYSTEM|::|FUNCALL&SKIP&RETGF| |SYSTEM|::|NN|)))
(:|DEFPARAMETER| |SYSTEM|::|*READING-ARRAY*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*SECLASS-READ*| (|COMMON-LISP|::|T| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |SYSTEM|::|*STACK-MODE*| 4.)
(:|DEFPARAMETER| |SYSTEM|::|*ALL-LANGUAGES*|
 ((|I18N|::|ENGLISH|) (|I18N|::|DANSK|) (|I18N|::|DEUTSCH|) (|I18N|::|FRANÇAIS|) (|I18N|::|ESPAÑOL|) (|I18N|::|NEDERLANDS|) (|I18N|::|???????|)))
(:|DEFCONSTANT| |SYSTEM|::|FUNTABR-INDEX| 512.)
(:|DEFPARAMETER| |SYSTEM|::|*INLINE-DEFINITIONS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*DEPRECATED-FUNCTIONS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*COMPILE-FILE-RESOLVED-PATHNAME*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*RECURSIVE-ERROR-COUNT*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*KNOWN-SPECIAL-VARS*| |COMMON-LISP|::|NIL|)
(:DEFPARAMETER SYSTEM::SPECDECL #<SPECIAL REFERENCE>)
(:|DEFPARAMETER| |SYSTEM|::|*TRACE-LEVEL*| 0.)
(:|DEFPARAMETER| |SYSTEM|::|*SQUEEZE-STRING-MAX*| 100.)
(:|DEFPARAMETER| |SYSTEM|::|*HOME-PACKAGE*| #.(|SYSTEM|::|%FIND-PACKAGE| "COMMON-LISP-USER"))
(:|DEFPARAMETER| |SYSTEM|::|*UNKNOWN-FUNCTIONS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*INSPECT-ALL*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*RECURSE-COUNT-GC-STATISTICS*| 0.)
(:|DEFCONSTANT| |SYSTEM|::|ONE-VALUE-OPS|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|
    (|COMMON-LISP|::|LIST*| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|LIST| . |COMMON-LISP|::|T|) (|SYSTEM|::|SVSET| . |COMMON-LISP|::|T|)
    (|COMMON-LISP|::|SVREF| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|SYMBOL-FUNCTION| . |COMMON-LISP|::|T|)
    (|COMMON-LISP|::|CONSP| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|ATOM| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|CONS| . |COMMON-LISP|::|T|)
    (|COMMON-LISP|::|CDR| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|CAR| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|EQ| . |COMMON-LISP|::|T|)
    (|COMMON-LISP|::|NOT| . |COMMON-LISP|::|T|) (|SYSTEM|::|TAGBODY-CLOSE-NIL| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|MV-TO-LIST| . |COMMON-LISP|::|T|) (|SYSTEM|::|VALUES1| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|BOUNDP| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|COPY-CLOSURE| . |COMMON-LISP|::|T|) (|SYSTEM|::|VENV| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|POP| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|SETVALUE| . |COMMON-LISP|::|T|) (|SYSTEM|::|GETVALUE| . |COMMON-LISP|::|T|) (|SYSTEM|::|STOREIC| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|STOREV| . |COMMON-LISP|::|T|) (|SYSTEM|::|STOREC| . |COMMON-LISP|::|T|) (|SYSTEM|::|STOREI| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|STORE| . |COMMON-LISP|::|T|) (|SYSTEM|::|LOADIC| . |COMMON-LISP|::|T|) (|SYSTEM|::|LOADV| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|LOADC| . |COMMON-LISP|::|T|) (|SYSTEM|::|LOADI| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|LOAD| . |COMMON-LISP|::|T|)
    (|SYSTEM|::|CONST| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|T| . |COMMON-LISP|::|T|) (|COMMON-LISP|::|NIL| . |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |SYSTEM|::|*READING-STRUCT*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-COPIER-LOCATION*| 6.)
(:|DEFPARAMETER| |SYSTEM|::|*LOGICAL-PATHNAME-TRANSLATIONS*|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |COMMON-LISP|::|EQUALP|
    ("SYS" .
     ((#S(|COMMON-LISP|::|LOGICAL-PATHNAME| :|HOST| "SYS" :|DEVICE| :|UNSPECIFIC| :|DIRECTORY| (:|RELATIVE|) :|NAME| :|WILD| :|TYPE| "LISP"
          :|VERSION| |COMMON-LISP|::|NIL|)
       "*.lisp")
      (#S(|COMMON-LISP|::|LOGICAL-PATHNAME| :|HOST| "SYS" :|DEVICE| :|UNSPECIFIC| :|DIRECTORY| (:|RELATIVE|) :|NAME| :|WILD|
          :|TYPE| |COMMON-LISP|::|NIL| :|VERSION| |COMMON-LISP|::|NIL|)
       "*")
      (#S(|COMMON-LISP|::|LOGICAL-PATHNAME| :|HOST| "SYS" :|DEVICE| :|UNSPECIFIC| :|DIRECTORY| (:|ABSOLUTE|) :|NAME| :|WILD|
          :|TYPE| |COMMON-LISP|::|NIL| :|VERSION| |COMMON-LISP|::|NIL|)
       "/*")))))
(:|DEFPARAMETER| |SYSTEM|::|*USE-CLCS*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |SYSTEM|::|*CURRENT-SOURCE-LINE-1*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|FORMAT-CARDINAL-ONES|
 #(|COMMON-LISP|::|NIL| "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen" "fourteen" "fifteen"
   "sixteen" "seventeen" "eighteen" "nineteen"))
(:|DEFPARAMETER| |SYSTEM|::|*LOAD-FORMS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*FUNCTIONS-WITH-ERRORS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |SYSTEM|::|*JMPBUF-SIZE*| 25.)
(:|DEFCONSTANT| |SYSTEM|::|C-FORM-TABLE|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|
    (|COMMON-LISP|::|DEPOSIT-FIELD| . |SYSTEM|::|C-DEPOSIT-FIELD|) (|COMMON-LISP|::|DPB| . |SYSTEM|::|C-DPB|)
    (|COMMON-LISP|::|MASK-FIELD| . |SYSTEM|::|C-MASK-FIELD|) (|COMMON-LISP|::|LDB-TEST| . |SYSTEM|::|C-LDB-TEST|)
    (|COMMON-LISP|::|LDB| . |SYSTEM|::|C-LDB|) (|COMMON-LISP|::|CONCATENATE| . |SYSTEM|::|C-CONCATENATE|)
    (|COMMON-LISP|::|REMOVE-DUPLICATES| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|NINTERSECTION| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|UNION| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|REMOVE| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|MISMATCH| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|TREE-EQUAL| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|RASSOC| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|MEMBER| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|SUBSTITUTE| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|POSITION| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|INTERSECTION| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|SUBST| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|NUNION| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|FIND| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|SUBSETP| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|NSUBSTITUTE| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|DELETE-DUPLICATES| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|SUBLIS| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|NSUBST| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|DELETE| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|SET-EXCLUSIVE-OR| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|NSUBLIS| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|COUNT| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|SET-DIFFERENCE| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|NSET-EXCLUSIVE-OR| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|ASSOC| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|SEARCH| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|NSET-DIFFERENCE| . |SYSTEM|::|C-TEST/TEST-NOT|)
    (|COMMON-LISP|::|ADJOIN| . |SYSTEM|::|C-TEST/TEST-NOT|) (|COMMON-LISP|::|RASSOC-IF-NOT| . |SYSTEM|::|C-RASSOC-IF-NOT|)
    (|COMMON-LISP|::|RASSOC-IF| . |SYSTEM|::|C-RASSOC-IF|) (|COMMON-LISP|::|ASSOC-IF-NOT| . |SYSTEM|::|C-ASSOC-IF-NOT|)
    (|COMMON-LISP|::|ASSOC-IF| . |SYSTEM|::|C-ASSOC-IF|) (|COMMON-LISP|::|MEMBER-IF-NOT| . |SYSTEM|::|C-MEMBER-IF-NOT|)
    (|COMMON-LISP|::|MEMBER-IF| . |SYSTEM|::|C-MEMBER-IF|) (|COMMON-LISP|::|NSUBST-IF-NOT| . |SYSTEM|::|C-NSUBST-IF-NOT|)
    (|COMMON-LISP|::|NSUBST-IF| . |SYSTEM|::|C-NSUBST-IF|) (|COMMON-LISP|::|SUBST-IF-NOT| . |SYSTEM|::|C-SUBST-IF-NOT|)
    (|COMMON-LISP|::|SUBST-IF| . |SYSTEM|::|C-SUBST-IF|) (|COMMON-LISP|::|COUNT-IF-NOT| . |SYSTEM|::|C-COUNT-IF-NOT|)
    (|COMMON-LISP|::|COUNT-IF| . |SYSTEM|::|C-COUNT-IF|) (|COMMON-LISP|::|POSITION-IF-NOT| . |SYSTEM|::|C-POSITION-IF-NOT|)
    (|COMMON-LISP|::|POSITION-IF| . |SYSTEM|::|C-POSITION-IF|) (|COMMON-LISP|::|FIND-IF-NOT| . |SYSTEM|::|C-FIND-IF-NOT|)
    (|COMMON-LISP|::|FIND-IF| . |SYSTEM|::|C-FIND-IF|) (|COMMON-LISP|::|NSUBSTITUTE-IF-NOT| . |SYSTEM|::|C-NSUBSTITUTE-IF-NOT|)
    (|COMMON-LISP|::|NSUBSTITUTE-IF| . |SYSTEM|::|C-NSUBSTITUTE-IF|) (|COMMON-LISP|::|SUBSTITUTE-IF-NOT| . |SYSTEM|::|C-SUBSTITUTE-IF-NOT|)
    (|COMMON-LISP|::|SUBSTITUTE-IF| . |SYSTEM|::|C-SUBSTITUTE-IF|) (|COMMON-LISP|::|DELETE-IF-NOT| . |SYSTEM|::|C-DELETE-IF-NOT|)
    (|COMMON-LISP|::|DELETE-IF| . |SYSTEM|::|C-DELETE-IF|) (|COMMON-LISP|::|REMOVE-IF-NOT| . |SYSTEM|::|C-REMOVE-IF-NOT|)
    (|COMMON-LISP|::|REMOVE-IF| . |SYSTEM|::|C-REMOVE-IF|) (|COMMON-LISP|::|NOTEVERY| . |SYSTEM|::|C-NOTEVERY|)
    (|COMMON-LISP|::|NOTANY| . |SYSTEM|::|C-NOTANY|) (|COMMON-LISP|::|EVERY| . |SYSTEM|::|C-EVERY|) (|COMMON-LISP|::|SOME| . |SYSTEM|::|C-SOME|)
    (|COMMON-LISP|::|MAP-INTO| . |SYSTEM|::|C-MAP-INTO|) (|COMMON-LISP|::|MAP| . |SYSTEM|::|C-MAP|) (|SYSTEM|::|%SETNTH| . |SYSTEM|::|C-SETNTH|)
    (|COMMON-LISP|::|NTH| . |SYSTEM|::|C-NTH|) (|COMMON-LISP|::|STABLE-SORT| . |SYSTEM|::|C-SORT|) (|COMMON-LISP|::|SORT| . |SYSTEM|::|C-SORT|)
    (|COMMON-LISP|::|FORMAT| . |SYSTEM|::|C-FORMAT|) (|COMMON-LISP|::|TYPEP| . |SYSTEM|::|C-TYPEP|) (|EXT|::|MAPLAP| . |SYSTEM|::|C-MAPLAP|)
    (|EXT|::|MAPCAP| . |SYSTEM|::|C-MAPCAP|) (|COMMON-LISP|::|MAPCON| . |SYSTEM|::|C-MAPCON|) (|COMMON-LISP|::|MAPCAN| . |SYSTEM|::|C-MAPCAN|)
    (|COMMON-LISP|::|MAPL| . |SYSTEM|::|C-MAPL|) (|COMMON-LISP|::|MAPC| . |SYSTEM|::|C-MAPC|) (|COMMON-LISP|::|MAPLIST| . |SYSTEM|::|C-MAPLIST|)
    (|COMMON-LISP|::|MAPCAR| . |SYSTEM|::|C-MAPCAR|) (|COMMON-LISP|::|EQUAL| . |SYSTEM|::|C-EQUAL|) (|COMMON-LISP|::|EQL| . |SYSTEM|::|C-EQL|)
    (|COMMON-LISP|::|EQ| . |SYSTEM|::|C-EQ|) (|SYSTEM|::|SVSTORE| . |SYSTEM|::|C-SVSTORE|) (|COMMON-LISP|::|>=| . |SYSTEM|::|C-COMPARE-NUMBERS|)
    (|COMMON-LISP|::|>| . |SYSTEM|::|C-COMPARE-NUMBERS|) (|COMMON-LISP|::|<=| . |SYSTEM|::|C-COMPARE-NUMBERS|)
    (|COMMON-LISP|::|<| . |SYSTEM|::|C-COMPARE-NUMBERS|) (|COMMON-LISP|::|/=| . |SYSTEM|::|C-COMPARE-NUMBERS|)
    (|COMMON-LISP|::|=| . |SYSTEM|::|C-COMPARE-NUMBERS|) (|COMMON-LISP|::|/| . |SYSTEM|::|C-SLASH|) (|COMMON-LISP|::|-| . |SYSTEM|::|C-MINUS|)
    (|COMMON-LISP|::|*| . |SYSTEM|::|C-STAR|) (|COMMON-LISP|::|+| . |SYSTEM|::|C-PLUS|) (|COMMON-LISP|::|APPLY| . |SYSTEM|::|C-APPLY|)
    (|COMMON-LISP|::|FUNCALL| . |SYSTEM|::|C-FUNCALL|) (|EXT|::|WITHOUT-PACKAGE-LOCK| . |SYSTEM|::|C-WITHOUT-PACKAGE-LOCK|)
    (|SYSTEM|::|CONSTANT-EQL| . |SYSTEM|::|C-CONSTANT-EQL|) (|SYSTEM|::|%HANDLER-BIND| . |SYSTEM|::|C-HANDLER-BIND|)
    (|CLOS|::|GENERIC-LABELS| . |SYSTEM|::|C-GENERIC-LABELS|) (|CLOS|::|GENERIC-FLET| . |SYSTEM|::|C-GENERIC-FLET|)
    (|SYSTEM|::|%OPTIMIZE-FUNCTION-LAMBDA| . |SYSTEM|::|C-%OPTIMIZE-FUNCTION-LAMBDA|)
    (|SYSTEM|::|%GENERIC-FUNCTION-LAMBDA| . |SYSTEM|::|C-%GENERIC-FUNCTION-LAMBDA|) (|COMMON-LISP|::|LOCALLY| . |SYSTEM|::|C-LOCALLY|)
    (|EXT|::|COMPILE-TIME-VALUE| . |SYSTEM|::|C-COMPILE-TIME-VALUE|) (|COMMON-LISP|::|LOAD-TIME-VALUE| . |SYSTEM|::|C-LOAD-TIME-VALUE|)
    (|COMMON-LISP|::|DECLARE| . |SYSTEM|::|C-DECLARE|) (|COMMON-LISP|::|EVAL-WHEN| . |SYSTEM|::|C-EVAL-WHEN|)
    (|EXT|::|COMPILER-LET| . |SYSTEM|::|C-COMPILER-LET|) (|COMMON-LISP|::|SYMBOL-MACROLET| . |SYSTEM|::|C-SYMBOL-MACROLET|)
    (|SYSTEM|::|FUNCTION-MACRO-LET| . |SYSTEM|::|C-FUNCTION-MACRO-LET|) (|COMMON-LISP|::|MACROLET| . |SYSTEM|::|C-MACROLET|)
    (|COMMON-LISP|::|LABELS| . |SYSTEM|::|C-LABELS|) (|COMMON-LISP|::|FLET| . |SYSTEM|::|C-FLET|)
    (|COMMON-LISP|::|MULTIPLE-VALUE-PROG1| . |SYSTEM|::|C-MULTIPLE-VALUE-PROG1|)
    (|COMMON-LISP|::|MULTIPLE-VALUE-LIST| . |SYSTEM|::|C-MULTIPLE-VALUE-LIST|) (|COMMON-LISP|::|PROGV| . |SYSTEM|::|C-PROGV|)
    (|COMMON-LISP|::|UNWIND-PROTECT| . |SYSTEM|::|C-UNWIND-PROTECT|) (|COMMON-LISP|::|THROW| . |SYSTEM|::|C-THROW|)
    (|COMMON-LISP|::|CATCH| . |SYSTEM|::|C-CATCH|) (|COMMON-LISP|::|THE| . |SYSTEM|::|C-THE|) (|COMMON-LISP|::|PROG2| . |SYSTEM|::|C-PROG2|)
    (|COMMON-LISP|::|PROG1| . |SYSTEM|::|C-PROG1|) (|COMMON-LISP|::|MULTIPLE-VALUE-CALL| . |SYSTEM|::|C-MULTIPLE-VALUE-CALL|)
    (|COMMON-LISP|::|PSETQ| . |SYSTEM|::|C-PSETQ|) (|EXT|::|FCASE| . |SYSTEM|::|C-CASE|) (|COMMON-LISP|::|CASE| . |SYSTEM|::|C-CASE|)
    (|COMMON-LISP|::|COND| . |SYSTEM|::|C-COND|) (|COMMON-LISP|::|UNLESS| . |SYSTEM|::|C-UNLESS|) (|COMMON-LISP|::|WHEN| . |SYSTEM|::|C-WHEN|)
    (|COMMON-LISP|::|OR| . |SYSTEM|::|C-OR|) (|COMMON-LISP|::|AND| . |SYSTEM|::|C-AND|)
    (|COMMON-LISP|::|MULTIPLE-VALUE-SETQ| . |SYSTEM|::|C-MULTIPLE-VALUE-SETQ|)
    (|COMMON-LISP|::|MULTIPLE-VALUE-BIND| . |SYSTEM|::|C-MULTIPLE-VALUE-BIND|) (|COMMON-LISP|::|FUNCTION| . |SYSTEM|::|C-FUNCTION|)
    (|COMMON-LISP|::|GO| . |SYSTEM|::|C-GO|) (|COMMON-LISP|::|TAGBODY| . |SYSTEM|::|C-TAGBODY|)
    (|COMMON-LISP|::|RETURN-FROM| . |SYSTEM|::|C-RETURN-FROM|) (|COMMON-LISP|::|BLOCK| . |SYSTEM|::|C-BLOCK|)
    (|COMMON-LISP|::|SETQ| . |SYSTEM|::|C-SETQ|) (|COMMON-LISP|::|IF| . |SYSTEM|::|C-IF|)
    (|COMMON-LISP|::|LET*| .
     #Y(#:|2105 2263 (DEFCONSTANT C-FORM-TABLE (LET # # ...))-172-2| #15Y(00 00 00 00 00 00 00 00 00 01 64 2F 00 19 01) (|SYSTEM|::|C-LET/LET*|)
        (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
    (|COMMON-LISP|::|LET| .
     #Y(#:|2105 2263 (DEFCONSTANT C-FORM-TABLE (LET # # ...))-172-1| #15Y(00 00 00 00 00 00 00 00 00 01 63 2F 00 19 01) (|SYSTEM|::|C-LET/LET*|)
        (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
    (|COMMON-LISP|::|PROGN| . |SYSTEM|::|C-PROGN|) (|COMMON-LISP|::|QUOTE| . |SYSTEM|::|C-QUOTE|)))
(:|DEFPARAMETER| |SYSTEM|::|*COMPILING*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*COMPILE-FILE-DIRECTORY*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|C-TYPEP-ALIST3|
 ((|COMMON-LISP|::|ARRAY| .
   #Y(#:|440 457 (DEF-COMPOUND-TYPE ARRAY (&OPTIONAL # #) ...)-87-2-1|
      #109Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA AE 24 00 80 4B AE 8E 1A 27 AE 63 AD 8C 9F 3F 87 01 00 14 24 00 33 AC 8F
            1E 0A AC 8E AE 06 E1 AD 90 01 32 25 E2 6F 03 DE AE 2D 03 05 CB 14 00 52 AE 8F 1E 0A AE 8E AE 06 DB AF 90 01 32 12 DC 6F 03 DE B0 2D
            03 05 CB 1B 64 83 01 1B FF BD 16 02 E3 B0 B0 B3 2D 04 0A 19 05)
      (|COMMON-LISP|::|*| 4096. "~S: rank ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|ARRAY| |SYSTEM|::|C-WARN| |SYSTEM|::|C-TYPEP|
       4294967296. "~S: dimension ~S is invalid" |COMMON-LISP|::|ARRAYP| |SYSTEM|::|C-TYPEP-ARRAY|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SIMPLE-ARRAY| .
   #Y(#:|458 475 (DEF-COMPOUND-TYPE SIMPLE-ARRAY (&OPTIONAL # #) ...)-88-2-1|
      #109Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA AE 24 00 80 4B AE 8E 1A 27 AE 63 AD 8C 9F 3F 87 01 00 14 24 00 33 AC 8F
            1E 0A AC 8E AE 06 E1 AD 90 01 32 25 E2 6F 03 DE AE 2D 03 05 CB 14 00 52 AE 8F 1E 0A AE 8E AE 06 DB AF 90 01 32 12 DC 6F 03 DE B0 2D
            03 05 CB 1B 64 83 01 1B FF BD 16 02 E3 B0 B0 B3 2D 04 0A 19 05)
      (|COMMON-LISP|::|*| 4096. "~S: rank ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|SIMPLE-ARRAY| |SYSTEM|::|C-WARN| |SYSTEM|::|C-TYPEP|
       4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|SIMPLE-ARRAY-P| |SYSTEM|::|C-TYPEP-ARRAY|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|VECTOR| .
   #Y(#:|476 494 (DEF-COMPOUND-TYPE VECTOR (&OPTIONAL # #) ...)-89-2-1|
      #106Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 3D AE 8E AE 39 DB AF 91 01 32 33 E1 E2 B2 7B 02 B1
            24 00 35 E3 E4 B4 7B 02 E5 B4 6F 0C 7B 02 7B 03 61 01 14 B1 24 00 24 E7 E8 B5 D4 5D 7A E5 B4 7B 02 7B 03 61 01 14 33 02 23 5D 5D 19
            05 DC 6F 03 DE B0 2D 03 05 E0 00 52 00 1B 57 00 1B 67)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|VECTOR| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|AND| |COMMON-LISP|::|VECTORP| |COMMON-LISP|::|EQUAL| |COMMON-LISP|::|ARRAY-ELEMENT-TYPE|
       |COMMON-LISP|::|QUOTE| |COMMON-LISP|::|UPGRADED-ARRAY-ELEMENT-TYPE| |COMMON-LISP|::|EQL| |COMMON-LISP|::|ARRAY-DIMENSION| (0.))
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SIMPLE-VECTOR| .
   #Y(#:|495 501 (DEF-COMPOUND-TYPE SIMPLE-VECTOR (&OPTIONAL #) ...)-90-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|SIMPLE-VECTOR| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|SIMPLE-VECTOR-P| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|COMPLEX| .
   #Y(#:|502 515 (DEF-COMPOUND-TYPE COMPLEX (&OPTIONAL # #) ...)-91-2-1|
      #78Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 A0 FA DB DC B2 7B 02 B1 24 00 2B DD DE B4 7B 02 DF B4 6F 06 7B 02 7B 03 61 01 14
           B1 24 00 1A DD E1 B5 7B 02 DF B4 6F 06 7B 02 7B 03 61 01 14 33 02 23 5D 5D 19 05 00 1B 61 00 1B 72)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|AND| |COMMON-LISP|::|COMPLEXP| |COMMON-LISP|::|TYPEP| |COMMON-LISP|::|REALPART| |COMMON-LISP|::|QUOTE|
       |COMMON-LISP|::|UPGRADED-COMPLEX-PART-TYPE| |COMMON-LISP|::|IMAGPART|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|INTEGER| .
   #Y(#:|516 520 (DEF-COMPOUND-TYPE INTEGER (&OPTIONAL # #) ...)-92-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|INTEGER| |COMMON-LISP|::|INTEGERP| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|MOD| .
   #Y(#:|521 528 (DEF-COMPOUND-TYPE MOD (N) ...)-93-2-1|
      #46Y(00 00 00 00 02 00 00 00 01 18 AE 8F 1E 15 DF E0 B1 7B 02 E1 E2 B3 7B 02 7B 02 E3 B3 B3 7B 03 61 04 19 04 DA 6F 01 DC B0 2D 03 03 DE 00
           52)
      ("~S: argument to MOD must be an integer: ~S" |SYSTEM|::|TEXT| |COMMON-LISP|::|TYPEP| |SYSTEM|::|C-WARN| |SYSTEM|::|C-TYPEP|
       |COMMON-LISP|::|AND| |COMMON-LISP|::|INTEGERP| |COMMON-LISP|::|NOT| |COMMON-LISP|::|MINUSP| |COMMON-LISP|::|<|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SIGNED-BYTE| .
   #Y(#:|529 537 (DEF-COMPOUND-TYPE SIGNED-BYTE (&OPTIONAL #) ...)-94-2-1|
      #61Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 04 AE 8F 1E 17 E1 E2 B1 7B 02 B0 24 00 1A E3 E4 B3 7B 02 B2 7B 03 61 01 5D 5D 19
           04 DB 6F 02 DD DE B1 2D 04 05 E0 00 52 00 1B 6D)
      (|COMMON-LISP|::|*| "~S: argument to ~S must be an integer or * : ~S" |SYSTEM|::|TEXT| |COMMON-LISP|::|TYPEP| |COMMON-LISP|::|SIGNED-BYTE|
       |SYSTEM|::|C-WARN| |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|AND| |COMMON-LISP|::|INTEGERP| |COMMON-LISP|::|<| |COMMON-LISP|::|INTEGER-LENGTH|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|UNSIGNED-BYTE| .
   #Y(#:|538 549 (DEF-COMPOUND-TYPE UNSIGNED-BYTE (&OPTIONAL #) ...)-95-2-1|
      #69Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 04 AE 8F 1E 1F E1 E2 B1 7B 02 E3 E4 B3 7B 02 7B 02 B1 24 00 1B E5 E6 B4 7B 02 B3
           7B 03 61 01 5D 5D 5D 19 04 DB 6F 02 DD DE B1 2D 04 05 E0 00 52 00 1B 6C)
      (|COMMON-LISP|::|*| "~S: argument to ~S must be an integer or * : ~S" |SYSTEM|::|TEXT| |COMMON-LISP|::|TYPEP|
       |COMMON-LISP|::|UNSIGNED-BYTE| |SYSTEM|::|C-WARN| |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|AND| |COMMON-LISP|::|INTEGERP| |COMMON-LISP|::|NOT|
       |COMMON-LISP|::|MINUSP| |COMMON-LISP|::|<=| |COMMON-LISP|::|INTEGER-LENGTH|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|REAL| .
   #Y(#:|550 554 (DEF-COMPOUND-TYPE REAL (&OPTIONAL # #) ...)-96-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|REAL| |COMMON-LISP|::|REALP| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|RATIONAL| .
   #Y(#:|555 559 (DEF-COMPOUND-TYPE RATIONAL (&OPTIONAL # #) ...)-97-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|RATIONAL| |COMMON-LISP|::|RATIONALP| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|FLOAT| .
   #Y(#:|560 564 (DEF-COMPOUND-TYPE FLOAT (&OPTIONAL # #) ...)-98-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|FLOAT| |COMMON-LISP|::|FLOATP| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SHORT-FLOAT| .
   #Y(#:|565 569 (DEF-COMPOUND-TYPE SHORT-FLOAT (&OPTIONAL # #) ...)-99-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|SHORT-FLOAT| |SYSTEM|::|SHORT-FLOAT-P| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SINGLE-FLOAT| .
   #Y(#:|570 574 (DEF-COMPOUND-TYPE SINGLE-FLOAT (&OPTIONAL # #) ...)-100-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|SINGLE-FLOAT| |SYSTEM|::|SINGLE-FLOAT-P| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|DOUBLE-FLOAT| .
   #Y(#:|575 579 (DEF-COMPOUND-TYPE DOUBLE-FLOAT (&OPTIONAL # #) ...)-101-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|DOUBLE-FLOAT| |SYSTEM|::|DOUBLE-FLOAT-P| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|LONG-FLOAT| .
   #Y(#:|580 584 (DEF-COMPOUND-TYPE LONG-FLOAT (&OPTIONAL # #) ...)-102-2-1|
      #30Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B1 B1 B4 2D 05 03 19 05)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|LONG-FLOAT| |SYSTEM|::|LONG-FLOAT-P| |SYSTEM|::|C-TYPEP-NUMBER|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|STRING| .
   #Y(#:|585 591 (DEF-COMPOUND-TYPE STRING (&OPTIONAL #) ...)-103-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|STRING| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|STRINGP| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SIMPLE-STRING| .
   #Y(#:|592 598 (DEF-COMPOUND-TYPE SIMPLE-STRING (&OPTIONAL #) ...)-104-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|SIMPLE-STRING| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|SIMPLE-STRING-P| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|BASE-STRING| .
   #Y(#:|599 605 (DEF-COMPOUND-TYPE BASE-STRING (&OPTIONAL #) ...)-105-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|BASE-STRING| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|STRINGP| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SIMPLE-BASE-STRING| .
   #Y(#:|606 612 (DEF-COMPOUND-TYPE SIMPLE-BASE-STRING (&OPTIONAL #) ...)-106-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|SIMPLE-BASE-STRING| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|SIMPLE-STRING-P| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|BIT-VECTOR| .
   #Y(#:|613 619 (DEF-COMPOUND-TYPE BIT-VECTOR (&OPTIONAL #) ...)-107-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|BIT-VECTOR| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|BIT-VECTOR-P| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|SIMPLE-BIT-VECTOR| .
   #Y(#:|620 626 (DEF-COMPOUND-TYPE SIMPLE-BIT-VECTOR (&OPTIONAL #) ...)-108-2-1|
      #52Y(00 00 00 00 01 00 01 00 01 00 3B 02 02 C5 FA AE 24 00 0E AE 8F 1E 12 AE 8E AE 0E DB AF 91 01 32 08 E1 AF B1 2D 03 08 19 04 DC 6F 03 DE
           B0 2D 03 05 E0 00 52)
      (|COMMON-LISP|::|*| 4294967296. "~S: dimension ~S is invalid" |SYSTEM|::|TEXT| |COMMON-LISP|::|SIMPLE-BIT-VECTOR| |SYSTEM|::|C-WARN|
       |SYSTEM|::|C-TYPEP| |COMMON-LISP|::|SIMPLE-BIT-VECTOR-P| |SYSTEM|::|C-TYPEP-VECTOR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|CONS| .
   #Y(#:|627 637 (DEF-COMPOUND-TYPE CONS (&OPTIONAL # #) ...)-109-2-1|
      #74Y(00 00 00 00 01 00 02 00 01 00 3B 03 02 C5 FB 3B 02 02 C5 FA DB DC B2 7B 02 B1 24 00 27 DD DE B4 7B 02 DF B4 7B 02 7B 03 61 01 14 B1 24
           00 18 DD E0 B5 7B 02 DF B4 7B 02 7B 03 61 01 14 33 02 23 5D 5D 19 05 00 1B 63 00 1B 72)
      (|COMMON-LISP|::|*| |COMMON-LISP|::|AND| |COMMON-LISP|::|CONSP| |COMMON-LISP|::|TYPEP| |COMMON-LISP|::|CAR| |COMMON-LISP|::|QUOTE|
       |COMMON-LISP|::|CDR|)
      (|COMMON-LISP|::|T| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|)))))
(:|DEFPARAMETER| |SYSTEM|::|C-TYPEP-ALIST2|
 ((|COMMON-LISP|::|BASE-STRING| (|SYSTEM|::|X|)
   (|COMMON-LISP|::|AND| (|COMMON-LISP|::|STRINGP| |SYSTEM|::|X|)
    (|COMMON-LISP|::|EQ| (|COMMON-LISP|::|ARRAY-ELEMENT-TYPE| |SYSTEM|::|X|) '|COMMON-LISP|::|CHARACTER|)))
  (|COMMON-LISP|::|BIGNUM| (|SYSTEM|::|X|)
   (|COMMON-LISP|::|AND| (|COMMON-LISP|::|INTEGERP| |SYSTEM|::|X|) (|COMMON-LISP|::|NOT| (|SYSTEM|::|FIXNUMP| |SYSTEM|::|X|))))
  (|COMMON-LISP|::|BIT| (|SYSTEM|::|X|) (|COMMON-LISP|::|OR| (|COMMON-LISP|::|EQL| |SYSTEM|::|X| 0.) (|COMMON-LISP|::|EQL| |SYSTEM|::|X| 1.)))
  (|COMMON-LISP|::|BOOLEAN| (|SYSTEM|::|X|)
   (|COMMON-LISP|::|OR| (|COMMON-LISP|::|EQ| |SYSTEM|::|X| '|COMMON-LISP|::|NIL|) (|COMMON-LISP|::|EQ| |SYSTEM|::|X| '|COMMON-LISP|::|T|)))
  (|COMMON-LISP|::|EXTENDED-CHAR| (|SYSTEM|::|X|) (|COMMON-LISP|::|DECLARE| (|COMMON-LISP|::|IGNORE| |SYSTEM|::|X|)) |COMMON-LISP|::|NIL|)
  (|COMMON-LISP|::|NIL| (|SYSTEM|::|X|) (|COMMON-LISP|::|DECLARE| (|COMMON-LISP|::|IGNORE| |SYSTEM|::|X|)) |COMMON-LISP|::|NIL|)
  (|COMMON-LISP|::|RATIO| (|SYSTEM|::|X|)
   (|COMMON-LISP|::|AND| (|COMMON-LISP|::|RATIONALP| |SYSTEM|::|X|) (|COMMON-LISP|::|NOT| (|COMMON-LISP|::|INTEGERP| |SYSTEM|::|X|))))
  (|COMMON-LISP|::|SIMPLE-BASE-STRING| (|SYSTEM|::|X|)
   (|COMMON-LISP|::|AND| (|COMMON-LISP|::|SIMPLE-STRING-P| |SYSTEM|::|X|)
    (|COMMON-LISP|::|EQ| (|COMMON-LISP|::|ARRAY-ELEMENT-TYPE| |SYSTEM|::|X|) '|COMMON-LISP|::|CHARACTER|)))
  (|COMMON-LISP|::|T| (|SYSTEM|::|X|) (|COMMON-LISP|::|DECLARE| (|COMMON-LISP|::|IGNORE| |SYSTEM|::|X|)) |COMMON-LISP|::|T|)
  (|FFI|::|FOREIGN-FUNCTION| (|SYSTEM|::|X|) (|COMMON-LISP|::|EQ| '|FFI|::|FOREIGN-FUNCTION| (|COMMON-LISP|::|TYPE-OF| |SYSTEM|::|X|)))
  (|FFI|::|FOREIGN-VARIABLE| (|SYSTEM|::|X|) (|COMMON-LISP|::|EQ| '|FFI|::|FOREIGN-VARIABLE| (|COMMON-LISP|::|TYPE-OF| |SYSTEM|::|X|)))
  (|FFI|::|FOREIGN-ADDRESS| (|SYSTEM|::|X|) (|COMMON-LISP|::|EQ| '|FFI|::|FOREIGN-ADDRESS| (|COMMON-LISP|::|TYPE-OF| |SYSTEM|::|X|)))
  (|EXT|::|FOREIGN-POINTER| (|SYSTEM|::|X|) (|COMMON-LISP|::|EQ| '|EXT|::|FOREIGN-POINTER| (|COMMON-LISP|::|TYPE-OF| |SYSTEM|::|X|)))
  (|SYSTEM|::|PLIST| (|SYSTEM|::|X|)
   (|COMMON-LISP|::|MULTIPLE-VALUE-BIND| (|COMMON-LISP|::|LENGTH| |SYSTEM|::|TAIL|) (|EXT|::|LIST-LENGTH-DOTTED| |SYSTEM|::|X|)
    (|COMMON-LISP|::|AND| (|COMMON-LISP|::|NULL| |SYSTEM|::|TAIL|) (|COMMON-LISP|::|EVENP| |COMMON-LISP|::|LENGTH|))))))
(:|DEFPARAMETER| |SYSTEM|::|C-TYPEP-ALIST1|
 ((|COMMON-LISP|::|ARRAY| . |COMMON-LISP|::|ARRAYP|) (|COMMON-LISP|::|ATOM| . |COMMON-LISP|::|ATOM|)
  (|COMMON-LISP|::|BASE-CHAR| . |COMMON-LISP|::|CHARACTERP|) (|COMMON-LISP|::|BIT-VECTOR| . |COMMON-LISP|::|BIT-VECTOR-P|)
  (|COMMON-LISP|::|BYTE| . |SYSTEM|::|BYTEP|) (|COMMON-LISP|::|CHARACTER| . |COMMON-LISP|::|CHARACTERP|)
  (|COMMON-LISP|::|COMPILED-FUNCTION| . |COMMON-LISP|::|COMPILED-FUNCTION-P|) (|COMMON-LISP|::|COMPLEX| . |COMMON-LISP|::|COMPLEXP|)
  (|COMMON-LISP|::|CONS| . |COMMON-LISP|::|CONSP|) (|COMMON-LISP|::|DOUBLE-FLOAT| . |SYSTEM|::|DOUBLE-FLOAT-P|)
  (|EXT|::|ENCODING| . |SYSTEM|::|ENCODINGP|) (|COMMON-LISP|::|FIXNUM| . |SYSTEM|::|FIXNUMP|) (|COMMON-LISP|::|FLOAT| . |COMMON-LISP|::|FLOATP|)
  (|COMMON-LISP|::|FUNCTION| . |COMMON-LISP|::|FUNCTIONP|) (|COMMON-LISP|::|HASH-TABLE| . |COMMON-LISP|::|HASH-TABLE-P|)
  (|COMMON-LISP|::|INTEGER| . |COMMON-LISP|::|INTEGERP|) (|COMMON-LISP|::|KEYWORD| . |COMMON-LISP|::|KEYWORDP|)
  (|COMMON-LISP|::|LIST| . |COMMON-LISP|::|LISTP|) (|COMMON-LISP|::|LOGICAL-PATHNAME| . |SYSTEM|::|LOGICAL-PATHNAME-P|)
  (|COMMON-LISP|::|LONG-FLOAT| . |SYSTEM|::|LONG-FLOAT-P|) (|COMMON-LISP|::|NULL| . |COMMON-LISP|::|NULL|)
  (|COMMON-LISP|::|NUMBER| . |COMMON-LISP|::|NUMBERP|) (|COMMON-LISP|::|PACKAGE| . |COMMON-LISP|::|PACKAGEP|)
  (|COMMON-LISP|::|PATHNAME| . |COMMON-LISP|::|PATHNAMEP|) (|COMMON-LISP|::|RANDOM-STATE| . |COMMON-LISP|::|RANDOM-STATE-P|)
  (|COMMON-LISP|::|RATIONAL| . |COMMON-LISP|::|RATIONALP|) (|COMMON-LISP|::|READTABLE| . |COMMON-LISP|::|READTABLEP|)
  (|COMMON-LISP|::|REAL| . |COMMON-LISP|::|REALP|) (|COMMON-LISP|::|SEQUENCE| . |SYSTEM|::|SEQUENCEP|)
  (|COMMON-LISP|::|SHORT-FLOAT| . |SYSTEM|::|SHORT-FLOAT-P|) (|COMMON-LISP|::|SIMPLE-ARRAY| . |SYSTEM|::|SIMPLE-ARRAY-P|)
  (|COMMON-LISP|::|SIMPLE-BIT-VECTOR| . |COMMON-LISP|::|SIMPLE-BIT-VECTOR-P|) (|COMMON-LISP|::|SIMPLE-STRING| . |COMMON-LISP|::|SIMPLE-STRING-P|)
  (|COMMON-LISP|::|SIMPLE-VECTOR| . |COMMON-LISP|::|SIMPLE-VECTOR-P|) (|COMMON-LISP|::|SINGLE-FLOAT| . |SYSTEM|::|SINGLE-FLOAT-P|)
  (|COMMON-LISP|::|STANDARD-CHAR| . |SYSTEM|::|%STANDARD-CHAR-P|) (|COMMON-LISP|::|STANDARD-OBJECT| . |CLOS|::|STD-INSTANCE-P|)
  (|COMMON-LISP|::|STREAM| . |COMMON-LISP|::|STREAMP|) (|COMMON-LISP|::|FILE-STREAM| . |SYSTEM|::|FILE-STREAM-P|)
  (|COMMON-LISP|::|SYNONYM-STREAM| . |SYSTEM|::|SYNONYM-STREAM-P|) (|COMMON-LISP|::|BROADCAST-STREAM| . |SYSTEM|::|BROADCAST-STREAM-P|)
  (|COMMON-LISP|::|CONCATENATED-STREAM| . |SYSTEM|::|CONCATENATED-STREAM-P|) (|COMMON-LISP|::|TWO-WAY-STREAM| . |SYSTEM|::|TWO-WAY-STREAM-P|)
  (|COMMON-LISP|::|ECHO-STREAM| . |SYSTEM|::|ECHO-STREAM-P|) (|COMMON-LISP|::|STRING-STREAM| . |SYSTEM|::|STRING-STREAM-P|)
  (|COMMON-LISP|::|STRING| . |COMMON-LISP|::|STRINGP|) (|EXT|::|STRING-CHAR| . |COMMON-LISP|::|CHARACTERP|)
  (|COMMON-LISP|::|STRUCTURE-OBJECT| . |CLOS|::|STRUCTURE-OBJECT-P|) (|COMMON-LISP|::|SYMBOL| . |COMMON-LISP|::|SYMBOLP|)
  (|COMMON-LISP|::|VECTOR| . |COMMON-LISP|::|VECTORP|) (|CLOS|::|POTENTIAL-CLASS| . |CLOS|::|POTENTIAL-CLASS-P|)
  (|CLOS|::|DEFINED-CLASS| . |CLOS|::|DEFINED-CLASS-P|) (|CLOS|::|BUILT-IN-CLASS| . |CLOS|::|BUILT-IN-CLASS-P|)
  (|CLOS|::|STRUCTURE-CLASS| . |CLOS|::|STRUCTURE-CLASS-P|) (|CLOS|::|STANDARD-CLASS| . |CLOS|::|STANDARD-CLASS-P|)))
(:|DEFCONSTANT| |SYSTEM|::|*DEFSTRUCT-DESCRIPTION-DIRECT-SLOTS-LOCATION*| 4.)
(:|DEFPARAMETER| |SYSTEM|::|*TRACED-FUNCTIONS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*DEPRECATED-FUNCTIONS-ALIST*|
 ((|SOCKET|::|SOCKET-SERVICE-PORT| #1="Use ~S instead." |POSIX|::|SERVICE|) (|EXT|::|RENAME-DIR| #1# |EXT|::|RENAME-DIRECTORY|)
  (|EXT|::|MAKE-DIR| #1# |EXT|::|MAKE-DIRECTORY|) (|EXT|::|DELETE-DIR| #1# |EXT|::|DELETE-DIRECTORY|)
  (|FFI|::|FOREIGN-ADDRESS-NULL| "The FFI now returns C NULL pointers as Lisp NIL. Use the function ~S instead." |COMMON-LISP|::|NULL|)
  (|EXT|::|DEFINE-SETF-METHOD| #1# |COMMON-LISP|::|DEFINE-SETF-EXPANDER|)
  (|EXT|::|GET-SETF-METHOD-MULTIPLE-VALUE| #1# |COMMON-LISP|::|GET-SETF-EXPANSION|)
  (|EXT|::|SPECIAL-FORM-P| #1# |COMMON-LISP|::|SPECIAL-OPERATOR-P|) (|EXT|::|TYPE-EXPAND-1| #1# |EXT|::|TYPE-EXPAND|)
  (|COMMON-LISP|::|GENTEMP| "This function creates symbols that cannot be garbage-collected. Use ~S instead." |COMMON-LISP|::|GENSYM|)
  (|COMMON-LISP|::|SET| "This function name is anachronistic. Use ~S ~S instead." |COMMON-LISP|::|SETF| |COMMON-LISP|::|SYMBOL-VALUE|)))
(:|DEFPARAMETER| |SYSTEM|::|*TOPLEVEL-ENVIRONMENT*|
 #(|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
   ((|COMMON-LISP|::|DECLARATION| |COMMON-LISP|::|OPTIMIZE| |COMMON-LISP|::|DECLARATION|))))
(:|DEFPARAMETER| |SYSTEM|::|*INLINE-FUNCTIONS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*CURRENT-SOURCE-LINE-2*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*DISASSEMBLE-USE-LIVE-PROCESS*| |COMMON-LISP|::|T|)
(:|DEFCONSTANT| |SYSTEM|::|SHORT-CODE-BASE| 157.)
(:|DEFPARAMETER| |SYSTEM|::|*COUTPUT-STREAM*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*COUTPUT-FILE*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |SYSTEM|::|*FFI-MODULE*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |EXT|::|*COMMAND-INDEX*| 4.)
(:|DEFCONSTANT| |EXT|::|CHAR-SUPER-BIT| 4.)
(:|DEFPARAMETER| |EXT|::|*KEYBOARD-INPUT*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |EXT|::|*BREAK-DRIVER*|
 #Y(|SYSTEM|::|BREAK-LOOP|
    #613Y(01 00 01 00 01 00 02 00 00 0D 00 2B 03 7F 04 00 00 3B 03 01 00 0B 00 01 3B 02 80 50 7D 02 06 00 00 1D 80 4D 1B 80 61 15 1B 80 5D CF 0F
          0C DE 31 5B 11 1B 80 8D E7 6B 0B 38 02 31 95 E8 6F 0F 6B 0B 30 10 6B 0B 31 97 1B 80 8A CF 0F 12 ED 6F 0F 6B 0B 38 02 31 95 1B 80 9B 03
          15 1B 80 92 6B 04 31 98 F2 6F 0F 6B 04 30 10 1B 80 AA 06 00 00 1C 17 06 00 01 1D 12 DA 68 01 01 70 01 93 00 07 DC AD DD 8E 43 FF A1 16
          01 0B 00 02 6B 04 6F 05 63 93 04 81 38 68 02 01 E0 38 02 72 3A 8B 07 81 2C 6B 08 32 B1 10 08 6B 08 E3 91 01 32 15 CF 0F 08 E5 31 5B 6B
          0C 32 B1 10 0C 6B 0C E3 90 01 32 FF 6A 11 6B 0B 31 97 11 06 02 02 1C FF 68 EB 6B 0B 38 02 31 95 6B 12 32 B1 10 12 6B 12 E3 90 01 32 FF
          68 68 05 01 6B 0B EE 06 08 02 1C FF 6B 03 16 14 2D 04 17 11 06 02 02 1D 80 D1 06 02 00 1C 80 C7 6B 04 31 98 93 01 07 F3 6F 0F 6B 04 30
          10 68 02 02 6B 04 31 90 6B 04 31 99 06 02 01 1D 80 CE 1B 80 AE 03 1B 1B 80 BD 93 03 0D 6B 04 31 98 AC 6B 04 30 10 6B 04 31 99 E4 AE 63
          1B 80 72 87 01 00 A4 2B 01 F7 85 04 14 72 AB 73 02 13 DC AF 65 1E 72 43 71 51 38 02 72 8F B0 71 93 AE AE 31 90 AD 01 02 65 1F 63 2D 05
          20 AF AE 31 90 AD 01 02 65 21 63 2D 05 20 AC AE 31 90 16 01 AC 81 90 00 7F 04 03 00 AF 6D 22 01 93 0B 0D 6B 04 31 98 AD 6B 04 30 10 6B
          04 31 99 65 23 71 51 AE 73 02 13 84 0A AF 9E 7A 84 0A AE 9E 7A 84 0A 16 05 83 01 AD 8D 9F FF 89 16 02 65 23 71 51 AE 73 02 13 B0 32 64
          5D FB 16 03 1B 26 92 01 FE 92 06 02 01 1D 1D 68 02 02 68 03 01 6F 1A 38 07 72 69 06 03 02 1C FF 40 03 1C 14 6F 0F 92 01 FF 3C 16 02 6B
          04 31 9B 4B 24 80 6C 80 66 80 61 65 25 31 5B 65 26 31 5B 6B 04 31 88 A5 2B 02 6B 27 32 B1 10 27 DE 72 85 9D 10 28 A0 10 29 2E 2A 14 2E
          2B 14 2E 2C 14 2E 2D 14 33 04 13 0B 0A 00 65 2E 2F 2F 10 30 2E 31 10 32 0E 33 10 33 6B 30 6B 33 30 34 10 35 0E 36 10 36 0E 37 10 37 68
          25 02 66 24 30 38 0B 1C 01 66 1C 66 20 6D 39 02 31 5D 00 1B 01 02 14 31 5E 4C 19 07)
    (|COMMON-LISP|::|CONTINUE| |COMMON-LISP|::|FIND-RESTART| |COMMON-LISP|::|RESTART| 7. |COMMON-LISP|::|*DEBUG-IO*|
     |COMMON-LISP|::|INTERACTIVE-STREAM-P| |COMMON-LISP|::|CONDITION| |COMMON-LISP|::|TYPEP| |SYSTEM|::|*RECURSE-COUNT-ERROR-OUTPUT*| 3. 0.
     |COMMON-LISP|::|*ERROR-OUTPUT*| |SYSTEM|::|*RECURSE-COUNT-DEBUG-IO*| "** - " "Continuable Error" |SYSTEM|::|TEXT| |SYSTEM|::|SAFE-WR-ST|
     "*** - " |SYSTEM|::|*RECURSIVE-ERROR-COUNT*| "Unprintable error message" :|TEXT-INDENT| 5. 6. |SYSTEM|::|PRETTY-PRINT-CONDITION|
     "You can continue (by typing 'continue')." "If you continue (by typing 'continue'): " |COMMON-LISP|::|COMPUTE-RESTARTS|
     "The following restarts are also available:" "The following restarts are available:" ":R" 1. 15. |SYSTEM|::|FORMAT-TABULATE| 24.
     #Y(|SYSTEM|::|BREAK-LOOP-1| #17Y(00 00 00 00 01 00 00 00 00 02 69 00 01 2F 01 19 02)
        (|COMMON-LISP|::|NIL| |COMMON-LISP|::|INVOKE-RESTART-INTERACTIVELY|) (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
     #\Newline #(|EXT|::|QUIT| |SYSTEM|::|ABORT-TO-TOP| |SYSTEM|::|UNWIND|) |SYSTEM|::|*TERMINAL-READ-STREAM*|
     |SYSTEM|::|*TERMINAL-READ-OPEN-OBJECT*| |SYSTEM|::|*BREAK-COUNT*| |COMMON-LISP|::|*STANDARD-INPUT*| |COMMON-LISP|::|*STANDARD-OUTPUT*|
     |SYSTEM|::|PROMPT-START| |SYSTEM|::|PROMPT-BREAK| |SYSTEM|::|PROMPT-BODY| |SYSTEM|::|PROMPT-FINISH| 13. |SYSTEM|::|FRAME-LIMIT-DOWN|
     |SYSTEM|::|*FRAME-LIMIT-DOWN*| |SYSTEM|::|FRAME-LIMIT-UP| |SYSTEM|::|*FRAME-LIMIT-UP*| |SYSTEM|::|*STACK-MODE*| |SYSTEM|::|FRAME-UP-DOWN|
     |SYSTEM|::|*DEBUG-FRAME*| |SYSTEM|::|*SAVED-DEBUG-PACKAGE*| |SYSTEM|::|*SAVED-DEBUG-READTABLE*| |SYSTEM|::|COMMANDS|
     #Y(|SYSTEM|::|BREAK-LOOP-2|
        #104Y(03 00 02 00 00 00 00 00 00 01 C7 50 29 2A 2B 01 00 0B 00 00 4B 03 22 DE DF E0 B3 B2 6D 07 02 E2 E3 6E 06 0A 0E 0B 5D 10 0B 6B 0C B4
              6D 0D 01 30 0E 11 4D 16 01 51 26 10 2C 06 04 00 39 00 00 DC D4 52 69 01 02 2F 11 19 01 69 01 02 2F 12 19 01 4E 01 02 4E 01 01 4E 01
              00 07 01 01 1C 78 69 01 03 2F 13 19 01 00 19 01)
        (|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|DEBUG| #(|COMMON-LISP|::|NIL|) :|NAME| |COMMON-LISP|::|ABORT|
         :|INVOKE-FUNCTION|
         #Y(|SYSTEM|::|BREAK-LOOP-2-1| #17Y(00 00 00 00 00 00 00 00 01 16 9E 0C 00 01 4E 01 00) (|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|)
            (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
         :|REPORT|
         #Y(|SYSTEM|::|BREAK-LOOP-2-2| #20Y(00 00 00 00 01 00 00 00 00 02 DA 6F 01 AE 38 02 31 95 19 02) ("Abort debug loop" |SYSTEM|::|TEXT|)
            (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
         |SYSTEM|::|MAKE-RESTART| |SYSTEM|::|*ACTIVE-RESTARTS*| |SYSTEM|::|*DEBUG-FRAME*|
         #Y(|SYSTEM|::|BREAK-LOOP-2-3| #32Y(00 00 00 00 00 00 00 00 00 01 69 01 01 69 01 02 30 01 1F 0A DC 07 02 03 1C 02 C9 52 C8 52 19 01)
            (|COMMON-LISP|::|NIL| |SYSTEM|::|READ-EVAL-PRINT| |COMMON-LISP|::|DEBUG| |EXT|::|QUIT| |SYSTEM|::|UNWIND|)
            (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
         |SYSTEM|::|SAME-ENV-AS| |COMMON-LISP|::|CONTINUE|
         #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| (|SYSTEM|::|PRINT-ERROR| . 10.) (|SYSTEM|::|INSPECT-ERROR| . 17.)
            (|SYSTEM|::|UNWIND| . 24.) (|SYSTEM|::|ABORT-TO-TOP| . 27.) (|EXT|::|QUIT| . 33.))
         |SYSTEM|::|PRINT-ERROR| |COMMON-LISP|::|INSPECT| |COMMON-LISP|::|INVOKE-RESTART-INTERACTIVELY|)
        (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
    (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
(:|DEFCONSTANT| |EXT|::|CHAR-CONTROL-BIT| 1.)
(:|DEFCONSTANT| |EXT|::|CHAR-BITS-LIMIT| 16.)
(:|DEFPARAMETER| |EXT|::|*DRIVER*|
 #Y(|SYSTEM|::|MAIN-LOOP| #35Y(02 00 01 00 00 00 01 00 00 07 00 2B 01 3B 02 01 02 0B 00 00 47 00 0A C6 0F 02 AF AF 6D 03 02 31 5D 19 03)
    (|SYSTEM|::|MAIN-LOOP| 0. |SYSTEM|::|*BREAK-COUNT*|
     #Y(|SYSTEM|::|MAIN-LOOP-1|
        #133Y(05 00 03 00 00 00 00 00 00 01 C7 50 80 75 2A 2B 01 00 0B 00 00 4B 03 80 55 DE DF E0 B3 B2 6D 07 02 E2 E3 6E 06 0A 0E 0B 5D 10 0B 2E
              0C 14 2E 0D 14 2E 0E 14 73 03 13 2E 0F 14 30 10 1F 3E 07 00 01 1C 18 D7 0F 13 4B 14 2F AE 6D 15 01 DB 6D 16 01 57 17 B0 36 00 16 06
              4D 1B 21 2E 11 1B 1D 58 67 00 00 01 76 00 AD 36 01 18 05 03 19 02 06 04 00 39 00 00 DC 03 19 52 F2 2F 11 4D 11 4D 16 01 51 19 01 19
              01)
        (|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|DEBUG| #(|COMMON-LISP|::|NIL|) :|NAME| |COMMON-LISP|::|ABORT|
         :|INVOKE-FUNCTION|
         #Y(|SYSTEM|::|MAIN-LOOP-1-1| #17Y(00 00 00 00 00 00 00 00 01 16 9E 0C 00 01 4E 01 00) (|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|)
            (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
         :|REPORT|
         #Y(|SYSTEM|::|MAIN-LOOP-1-2| #20Y(00 00 00 00 01 00 00 00 00 02 DA 6F 01 AE 38 02 31 95 19 02) ("Abort main loop" |SYSTEM|::|TEXT|)
            (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
         |SYSTEM|::|MAKE-RESTART| |SYSTEM|::|*ACTIVE-RESTARTS*| |SYSTEM|::|PROMPT-START| |SYSTEM|::|PROMPT-BODY| |SYSTEM|::|PROMPT-FINISH|
         |SYSTEM|::|COMMANDS0| |SYSTEM|::|READ-EVAL-PRINT| |EXT|::|EXIT| 0. |EXT|::|*COMMAND-INDEX*| #(|COMMON-LISP|::|NIL|)
         #Y(|SYSTEM|::|MAIN-LOOP-1-3| #16Y(00 00 00 00 00 00 00 00 00 01 DA 2C 01 01 19 01)
            (|COMMON-LISP|::|NIL|
             #Y(|SYSTEM|::|MAIN-LOOP-1-3-1| #13Y(00 00 00 00 01 00 00 00 00 02 4E 00 00) (|COMMON-LISP|::|NIL|)
                (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
            (|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
         #Y(|SYSTEM|::|MAIN-LOOP-1-4| #13Y(00 00 00 00 00 00 00 00 00 01 00 49 00) (|COMMON-LISP|::|NIL|)
            (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|))
         (#(|COMMON-LISP|::|ERROR| 85.) 4. . 3.) 1. |COMMON-LISP|::|CONTINUE|)
        (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
    (|COMMON-LISP|::|T| |COMMON-LISP|::|T| |COMMON-LISP|::|T|)))
(:|DEFCONSTANT| |EXT|::|CHAR-META-BIT| 2.)
(:|DEFCONSTANT| |EXT|::|CHAR-FONT-LIMIT| 16.)
(:|DEFCONSTANT| |EXT|::|CHAR-HYPER-BIT| 8.)
(:|DEFPARAMETER| |EXT|::|*ARGS*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |EXT|::|BASE-CHAR-CODE-LIMIT| 1114112.)
(:DEFPARAMETER CLOS::|#'generic-function-argorder| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS::GENERIC-FUNCTION-ARGORDER>)
(:|DEFPARAMETER| |CLOS|::|*ALLOW-MIXING-METACLASSES*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |CLOS|::|<RATIO>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|RATIO|))
(:|DEFCONSTANT| |CLOS|::|*<FUNCALLABLE-STANDARD-CLASS>-INSTANCE-SIZE*| 28.)
(:|DEFPARAMETER| |CLOS|::|<STANDARD-ACCESSOR-METHOD>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-ACCESSOR-METHOD|))
(:|DEFPARAMETER| |CLOS|::|<FLOAT>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|FLOAT|))
(:|DEFPARAMETER| |CLOS|::|<PATHNAME>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|PATHNAME|))
(:|DEFPARAMETER| |CLOS|::|<STRING-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|STRING-STREAM|))
(:|DEFPARAMETER| |CLOS|::|*<FUNCALLABLE-STANDARD-CLASS>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|FUNCALLABLE-STANDARD-CLASS|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:DEFPARAMETER CLOS::|#'reinitialize-instance| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:REINITIALIZE-INSTANCE>)
(:|DEFPARAMETER| |CLOS|::|*METHOD-COMBINATION*| |COMMON-LISP|::|NIL|)
(:DEFPARAMETER CLOS::|#'initialize-instance| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:INITIALIZE-INSTANCE>)
(:|DEFPARAMETER| |CLOS|::|*<INHERITABLE-SLOT-DEFINITION-DOC>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|INHERITABLE-SLOT-DEFINITION-DOC| |COMMON-LISP|::|NIL|
  ((|CLOS|::|DOCUMENTATION| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|NULL| |COMMON-LISP|::|STRING|) :|INITARG| :|DOCUMENTATION|))
  (:|METACLASS| |CLOS|::|STRUCTURE-CLASS|)))
(:|DEFPARAMETER| |CLOS|::|*ALLOW-MAKING-GENERIC*| |COMMON-LISP|::|NIL|)
(:DEFPARAMETER CLOS::|#'slot-value-using-class| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-VALUE-USING-CLASS>)
(:|DEFPARAMETER| |CLOS|::|*<EFFECTIVE-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|EFFECTIVE-SLOT-DEFINITION| (|CLOS|::|SLOT-DEFINITION|)
  ((|CLOS|::|$LOCATION| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|NULL| |COMMON-LISP|::|INTEGER| |COMMON-LISP|::|CONS|) :|INITARG|
    |CLOS|::|LOCATION|)
   (|CLOS|::|$EFM-SVUC| :|TYPE| |COMMON-LISP|::|FUNCTION|) (|CLOS|::|$EFM-SSVUC| :|TYPE| |COMMON-LISP|::|FUNCTION|)
   (|CLOS|::|$EFM-SBUC| :|TYPE| |COMMON-LISP|::|FUNCTION|) (|CLOS|::|$EFM-SMUC| :|TYPE| |COMMON-LISP|::|FUNCTION|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|<METHOD-COMBINATION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|METHOD-COMBINATION|))
(:|DEFPARAMETER| |CLOS|::|<VECTOR>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|VECTOR|))
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-CLASS>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-CLASS|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|<READTABLE>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|READTABLE|))
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-FUNCALLABLEP-LOCATION*| 22.)
(:|DEFCONSTANT| |CLOS|::|*<EFFECTIVE-SLOT-DEFINITION>-LOCATION-LOCATION*| 7.)
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-NAMES-LOCATION*| 21.)
(:DEFPARAMETER CLOS::|#'compute-discriminating-function| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:COMPUTE-DISCRIMINATING-FUNCTION>)
(:|DEFPARAMETER| |CLOS|::|<STANDARD-EFFECTIVE-SLOT-DEFINITION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-EFFECTIVE-SLOT-DEFINITION|))
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-KCONSTRUCTOR-LOCATION*| 22.)
(:|DEFCONSTANT| |CLOS|::|*<POTENTIAL-CLASS>-DIRECT-SUBCLASSES-LOCATION*| 4.)
(:|DEFPARAMETER| |CLOS|::|<STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|STREAM|))
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-INSTANCE-SIZE*| 27.)
(:|DEFPARAMETER| |CLOS|::|<STANDARD-READER-METHOD>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-READER-METHOD|))
(:|DEFPARAMETER| |CLOS|::|<CONS>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|CONS|))
(:|DEFPARAMETER| |CLOS|::|<BROADCAST-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|BROADCAST-STREAM|))
(:|DEFCONSTANT| |CLOS|::|*<STANDARD-CLASS>-INSTANCE-SIZE*| 28.)
(:DEFPARAMETER CLOS::|#'compute-effective-method| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:COMPUTE-EFFECTIVE-METHOD>)
(:|DEFPARAMETER| |CLOS|::|<CONCATENATED-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|CONCATENATED-STREAM|))
(:|DEFPARAMETER| |CLOS|::|*COMPILE-ACCESSOR-FUNCTIONS*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |CLOS|::|*MOP-STANDARDIZED-PACKAGES*| (#.(|SYSTEM|::|%FIND-PACKAGE| "COMMON-LISP") #.(|SYSTEM|::|%FIND-PACKAGE| "CLOS")))
(:|DEFPARAMETER| |CLOS|::|<PACKAGE>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|PACKAGE|))
(:|DEFPARAMETER| |CLOS|::|<DIRECT-SLOT-DEFINITION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|DIRECT-SLOT-DEFINITION|))
(:|DEFCONSTANT| |CLOS|::|*<SLOT-DEFINITION>-INITARGS-LOCATION*| 2.)
(:|DEFPARAMETER| |CLOS|::|<STANDARD-DIRECT-SLOT-DEFINITION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-DIRECT-SLOT-DEFINITION|))
(:|DEFCONSTANT| |CLOS|::|*<SLOT-DEFINITION>-INHERITABLE-DOC-LOCATION*| 6.)
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-DEFAULT-INITARGS-LOCATION*| 12.)
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-DIRECT-SUPERCLASSES-LOCATION*| 5.)
(:|DEFCONSTANT| |CLOS|::|*<STANDARD-STABLEHASH>-HASHCODE-LOCATION*| 1.)
(:|DEFCONSTANT| |CLOS|::|*<EFFECTIVE-SLOT-DEFINITION>-EFM-SBUC-LOCATION*| 10.)
(:|DEFCONSTANT| |CLOS|::|*<SLOT-DEFINITION>-NAME-LOCATION*| 1.)
(:DEFPARAMETER CLOS::|#'method-qualifiers| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:METHOD-QUALIFIERS>)
(:DEFPARAMETER CLOS::|#'compute-applicable-methods-using-classes|
 #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:COMPUTE-APPLICABLE-METHODS-USING-CLASSES>)
(:|DEFPARAMETER| |CLOS|::|*<STRUCTURE-EFFECTIVE-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STRUCTURE-EFFECTIVE-SLOT-DEFINITION| (|CLOS|::|EFFECTIVE-SLOT-DEFINITION|)
  ((|CLOS|::|$EFM-SVUC| :|TYPE| |COMMON-LISP|::|FUNCTION| :|INITFORM| #'|CLOS|::|%SLOT-VALUE-USING-CLASS|)
   (|CLOS|::|$EFM-SSVUC| :|TYPE| |COMMON-LISP|::|FUNCTION| :|INITFORM| #'|CLOS|::|%SET-SLOT-VALUE-USING-CLASS|)
   (|CLOS|::|$EFM-SBUC| :|TYPE| |COMMON-LISP|::|FUNCTION| :|INITFORM| #'|CLOS|::|%SLOT-BOUNDP-USING-CLASS|)
   (|CLOS|::|$EFM-SMUC| :|TYPE| |COMMON-LISP|::|FUNCTION| :|INITFORM| #'|CLOS|::|%SLOT-MAKUNBOUND-USING-CLASS|)
   (|CLOS|::|$READONLY| :|TYPE| |COMMON-LISP|::|BOOLEAN| :|INITARG| |CLOS|::|READONLY|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-EFFECTIVE-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STANDARD-EFFECTIVE-SLOT-DEFINITION| (|CLOS|::|EFFECTIVE-SLOT-DEFINITION| |CLOS|::|STANDARD-SLOT-DEFINITION|)
  |COMMON-LISP|::|NIL| (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-DIRECT-DEFAULT-INITARGS-LOCATION*| 11.)
(:|DEFCONSTANT| |CLOS|::|*<SLOTTED-CLASS>-DIRECT-ACCESSORS-LOCATION*| 18.)
(:|DEFCONSTANT| |CLOS|::|*<EFFECTIVE-SLOT-DEFINITION>-EFM-SMUC-LOCATION*| 11.)
(:|DEFPARAMETER| |CLOS|::|<NUMBER>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|NUMBER|))
(:|DEFPARAMETER| |CLOS|::|*<DEFINED-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|DEFINED-CLASS| (|CLOS|::|POTENTIAL-CLASS|)
  ((|CLOS|::|$DIRECT-SUPERCLASSES| :|TYPE| |COMMON-LISP|::|LIST| :|INITARG| :|DIRECT-SUPERCLASSES|)
   (|CLOS|::|$ALL-SUPERCLASSES| :|TYPE| |COMMON-LISP|::|HASH-TABLE|) (|CLOS|::|$PRECEDENCE-LIST| :|TYPE| |COMMON-LISP|::|LIST|)
   (|CLOS|::|$DIRECT-SLOTS| :|TYPE| |COMMON-LISP|::|LIST| :|INITARG| :|DIRECT-SLOTS|) (|CLOS|::|$SLOTS| :|TYPE| |COMMON-LISP|::|LIST|)
   (|CLOS|::|$SLOT-LOCATION-TABLE| :|TYPE| |COMMON-LISP|::|HASH-TABLE| :|INITFORM| |CLOS|::|EMPTY-HT|)
   (|CLOS|::|$DIRECT-DEFAULT-INITARGS| :|TYPE| |COMMON-LISP|::|LIST| :|INITARG| :|DIRECT-DEFAULT-INITARGS|) (|CLOS|::|$DEFAULT-INITARGS|)
   (|CLOS|::|$DOCUMENTATION| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|STRING| |COMMON-LISP|::|NULL|) :|INITARG| :|DOCUMENTATION|)
   (|CLOS|::|$LISTENERS| :|TYPE| |COMMON-LISP|::|LIST| :|INITFORM| |COMMON-LISP|::|NIL|)
   (|CLOS|::|$INITIALIZED| :|TYPE| (|COMMON-LISP|::|INTEGER| 0. 6.) :|INITFORM| 0.))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*MAKE-INSTANCES-OBSOLETE-CALLER*| |CLOS|::|MAKE-INSTANCES-OBSOLETE|)
(:|DEFCONSTANT| |CLOS|::|*<EFFECTIVE-SLOT-DEFINITION>-EFM-SSVUC-LOCATION*| 9.)
(:|DEFPARAMETER| |CLOS|::|*<METAOBJECT>-DEFCLASS*| (|CLOS|::|DEFCLASS| |CLOS|::|METAOBJECT| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|<STANDARD-WRITER-METHOD>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-WRITER-METHOD|))
(:|DEFPARAMETER| |CLOS|::|*<INHERITABLE-SLOT-DEFINITION-INITER>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|INHERITABLE-SLOT-DEFINITION-INITER| |COMMON-LISP|::|NIL|
  ((|CLOS|::|INITFORM| :|TYPE| |COMMON-LISP|::|T| :|INITARG| :|INITFORM|)
   (|CLOS|::|INITFUNCTION| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|NULL| |COMMON-LISP|::|FUNCTION|) :|INITARG| :|INITFUNCTION|))
  (:|METACLASS| |CLOS|::|STRUCTURE-CLASS|)))
(:|DEFPARAMETER| |CLOS|::|*<FORWARD-REFERENCE-TO-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|FORWARD-REFERENCE-TO-CLASS| (|CLOS|::|SUPER-CLASS|) |COMMON-LISP|::|NIL|
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|NIL|)))
(:|DEFPARAMETER| |CLOS|::|<POTENTIAL-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|POTENTIAL-CLASS|))
(:|DEFPARAMETER| |CLOS|::|<FUNCALLABLE-STANDARD-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|FUNCALLABLE-STANDARD-CLASS|))
(:|DEFPARAMETER| |CLOS|::|*<SUPER-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|SUPER-CLASS| (|CLOS|::|STANDARD-STABLEHASH| |CLOS|::|METAOBJECT|)
  ((|CLOS|::|$CLASSNAME| :|TYPE| |COMMON-LISP|::|SYMBOL| :|INITARG| :|NAME|)
   (|CLOS|::|$DIRECT-SUBCLASSES| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|HASH-TABLE| |CLOS|::|WEAK-LIST| |COMMON-LISP|::|NULL|) :|INITFORM|
    |COMMON-LISP|::|NIL|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|NIL|)))
(:|DEFPARAMETER| |CLOS|::|*CLASSES-FINISHED*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |CLOS|::|*<BUILT-IN-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|BUILT-IN-CLASS| (|CLOS|::|DEFINED-CLASS|) ((|CLOS|::|$PROTOTYPE| :|TYPE| |COMMON-LISP|::|T|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|<FORWARD-REFERENCE-TO-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|FORWARD-REFERENCED-CLASS|))
(:|DEFPARAMETER| |CLOS|::|<STRUCTURE-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STRUCTURE-CLASS|))
(:|DEFPARAMETER| |CLOS|::|<LOGICAL-PATHNAME>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|LOGICAL-PATHNAME|))
(:|DEFPARAMETER| |CLOS|::|*ENABLE-CLOS-WARNINGS*| |COMMON-LISP|::|T|)
(:|DEFPARAMETER| |CLOS|::|<STANDARD-OBJECT>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|STANDARD-OBJECT|))
(:|DEFPARAMETER| |CLOS|::|*<FORWARD-REFERENCED-CLASS>-UNDER-<CLASS>*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |CLOS|::|*<BUILT-IN-CLASS>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|BUILT-IN-CLASS|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:DEFPARAMETER CLOS::|#'compute-applicable-methods| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:COMPUTE-APPLICABLE-METHODS>)
(:|DEFPARAMETER| |CLOS|::|*STRICT-MOP*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |CLOS|::|<FILE-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|FILE-STREAM|))
(:DEFPARAMETER CLOS::|#'generic-function-signature| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS::GENERIC-FUNCTION-SIGNATURE>)
(:DEFPARAMETER CLOS::*EXTENDED-METHOD-CHECK-METHOD*
 #1=#<CLOS:STANDARD-METHOD ((COMMON-LISP:EQL COMMON-LISP:NIL) #2=#<CLOS:BUILT-IN-CLASS COMMON-LISP:T> #2#)>)
(:DEFPARAMETER CLOS::|#'update-instance-for-redefined-class| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:UPDATE-INSTANCE-FOR-REDEFINED-CLASS>)
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-DIRECT-SLOT-DEFINITION>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-DIRECT-SLOT-DEFINITION|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFCONSTANT| |CLOS|::|*<FUNCALLABLE-STANDARD-CLASS>-VALID-INITIALIZATION-KEYWORDS*|
 (:|NAME| :|DIRECT-SUPERCLASSES| :|DIRECT-SLOTS| :|DIRECT-DEFAULT-INITARGS| :|DOCUMENTATION| :|GENERIC-ACCESSORS| :|FIXED-SLOT-LOCATIONS|))
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STANDARD-SLOT-DEFINITION| (|CLOS|::|SLOT-DEFINITION|) |COMMON-LISP|::|NIL|
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|<BIT-VECTOR>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|BIT-VECTOR|))
(:DEFPARAMETER CLOS::|#'generic-function-declarations| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-DECLARATIONS>)
(:DEFPARAMETER CLOS::*MAKE-INSTANCE-TABLE*
 #S(COMMON-LISP:HASH-TABLE :TEST EXT:STABLEHASH-EQ :WARN-IF-NEEDS-REHASH-AFTER-GC COMMON-LISP:T
    (#1=#<CLOS:STANDARD-CLASS SYSTEM::SIMPLE-PRINT-NOT-READABLE> .
     #((:OBJECT :FORMAT-CONTROL :FORMAT-ARGUMENTS) #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<SYSTEM-FUNCTION CLOS::%INITIALIZE-INSTANCE>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#2=#<CLOS:STANDARD-CLASS EXT:FILL-STREAM> .
     #((:STREAM :SEXP-INDENT :TEXT-INDENT) #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<SYSTEM-FUNCTION CLOS::%INITIALIZE-INSTANCE>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#3=#<CLOS:STANDARD-CLASS COMMON-LISP:SIMPLE-WARNING> .
     #((:FORMAT-CONTROL :FORMAT-ARGUMENTS) #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<SYSTEM-FUNCTION CLOS::%INITIALIZE-INSTANCE>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#4=#<CLOS:STANDARD-CLASS COMMON-LISP:SIMPLE-ERROR> .
     #((:FORMAT-CONTROL :FORMAT-ARGUMENTS) #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<SYSTEM-FUNCTION CLOS::%INITIALIZE-INSTANCE>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#5=#<CLOS:STANDARD-CLASS CLOS:STRUCTURE-CLASS> .
     #(COMMON-LISP:T #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<COMPILED-FUNCTION CLOS::INITIALIZE-INSTANCE-<EMF-3>-1>
       #6=#<COMPILED-FUNCTION #:|39 56 (DEFMETHOD SHARED-INITIALIZE (# SITUATION &REST ...) ...)-6-1-1|>))
    (#7=#<CLOS:STANDARD-CLASS CLOS::STRUCTURE-DIRECT-SLOT-DEFINITION> .
     #((:NAME :INITARGS :TYPE :ALLOCATION CLOS::INHERITABLE-INITER CLOS::INHERITABLE-DOC :READERS :WRITERS :INITFORM :INITFUNCTION :DOCUMENTATION
        CLOS::DEFCLASS-FORM)
       #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #8=#<COMPILED-FUNCTION #:|62 65 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS) ...)-8-1-1|>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#9=#<CLOS:STANDARD-CLASS CLOS::STRUCTURE-EFFECTIVE-SLOT-DEFINITION> .
     #((:NAME :INITARGS :TYPE :ALLOCATION CLOS::INHERITABLE-INITER CLOS::INHERITABLE-DOC CLOS::LOCATION CLOS::READONLY :INITFORM :INITFUNCTION
        :DOCUMENTATION)
       #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #10=#<COMPILED-FUNCTION #:|73 74 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS) ...)-11-1-1|>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#11=#<CLOS:STANDARD-CLASS CLOS:STANDARD-READER-METHOD> .
     #(COMMON-LISP:T #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE>
       #12=#<COMPILED-FUNCTION #:|38 42 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS ...) ...)-4-1-1|>
       #13=#<COMPILED-FUNCTION #:|8 14 (DEFMETHOD SHARED-INITIALIZE (# SITUATION &REST ...) ...)-2-1-1|>))
    (#14=#<CLOS:STANDARD-CLASS CLOS:STANDARD-EFFECTIVE-SLOT-DEFINITION> .
     #((:NAME :INITARGS :TYPE :ALLOCATION CLOS::INHERITABLE-INITER CLOS::INHERITABLE-DOC CLOS::LOCATION :INITFORM :INITFUNCTION :DOCUMENTATION)
       #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #15=#<COMPILED-FUNCTION #:|57 60 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS) ...)-7-1-1|>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#16=#<CLOS:STANDARD-CLASS CLOS:STANDARD-DIRECT-SLOT-DEFINITION> .
     #((:NAME :INITARGS :TYPE :ALLOCATION CLOS::INHERITABLE-INITER CLOS::INHERITABLE-DOC :READERS :WRITERS :INITFORM :INITFUNCTION :DOCUMENTATION
        CLOS::DEFCLASS-FORM)
       #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #17=#<COMPILED-FUNCTION #:|48 55 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS) ...)-6-1-1|>
       #<SYSTEM-FUNCTION CLOS::%SHARED-INITIALIZE>))
    (#18=#<CLOS:STANDARD-CLASS CLOS:STANDARD-CLASS> .
     #((:NAME :DIRECT-SUPERCLASSES :DIRECT-SLOTS :DIRECT-DEFAULT-INITARGS :DOCUMENTATION :FIXED-SLOT-LOCATIONS :GENERIC-ACCESSORS)
       #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<COMPILED-FUNCTION CLOS::INITIALIZE-INSTANCE-<EMF-2>-1>
       #19=#<COMPILED-FUNCTION #:|58 68 (DEFMETHOD SHARED-INITIALIZE (# SITUATION &REST ...) ...)-7-1-1|>))
    (#20=#<CLOS:STANDARD-CLASS CLOS:EQL-SPECIALIZER> .
     #((CLOS::SINGLETON) #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE> #<SYSTEM-FUNCTION CLOS::%INITIALIZE-INSTANCE>
       #21=#<COMPILED-FUNCTION #:|18 23 (DEFMETHOD SHARED-INITIALIZE (# SITUATION &REST ...) ...)-4-1-1|>))
    (#22=#<CLOS:FUNCALLABLE-STANDARD-CLASS CLOS:STANDARD-GENERIC-FUNCTION> .
     #((:NAME :LAMBDA-LIST :ARGUMENT-PRECEDENCE-ORDER :METHOD-CLASS :METHOD-COMBINATION :DOCUMENTATION :DECLARATIONS :DECLARE CLOS::METHODS)
       #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE>
       #23=#<COMPILED-FUNCTION #:|33 46 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS ...) ...)-4-1-1|>
       #24=#<COMPILED-FUNCTION #:|20 31 (DEFMETHOD SHARED-INITIALIZE (# SITUATION &REST ...) ...)-3-1-1|>))
    (#25=#<CLOS:STANDARD-CLASS CLOS:STANDARD-METHOD> .
     #(COMMON-LISP:T #<SYSTEM-FUNCTION CLOS::%ALLOCATE-INSTANCE>
       #26=#<COMPILED-FUNCTION #:|20 36 (DEFMETHOD INITIALIZE-INSTANCE (# &REST ARGS ...) ...)-3-1-1|> #13#))))
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-SLOT-LOCATION-TABLE-LOCATION*| 10.)
(:DEFPARAMETER CLOS::*EQ-SPECIALIZER-TABLE*
 #<COMMON-LISP:HASH-TABLE :TEST EXT:STABLEHASH-EQ :WEAK :KEY (:HTTP . #1=#<CLOS:EQL-SPECIALIZER :HTTP>) (:TTY . #2=#<CLOS:EQL-SPECIALIZER :TTY>)
   (EXT:CLHS . #3=#<CLOS:EQL-SPECIALIZER EXT:CLHS>) (SYSTEM::IMPNOTES . #4=#<CLOS:EQL-SPECIALIZER SYSTEM::IMPNOTES>)
   (#5=#<CLOS:BUILT-IN-CLASS COMMON-LISP:STREAM> . #6=#<CLOS:EQL-SPECIALIZER #5#>)
   (CLOS:METHOD-COMBINATION . #7=#<CLOS:EQL-SPECIALIZER CLOS:METHOD-COMBINATION>) (CLOS:CLASS . #8=#<CLOS:EQL-SPECIALIZER CLOS:CLASS>)
   (COMMON-LISP:STRUCTURE . #9=#<CLOS:EQL-SPECIALIZER COMMON-LISP:STRUCTURE>) (COMMON-LISP:TYPE . #10=#<CLOS:EQL-SPECIALIZER COMMON-LISP:TYPE>)
   (COMMON-LISP:COMPILER-MACRO . #11=#<CLOS:EQL-SPECIALIZER COMMON-LISP:COMPILER-MACRO>)
   (COMMON-LISP:FUNCTION . #12=#<CLOS:EQL-SPECIALIZER COMMON-LISP:FUNCTION>) (COMMON-LISP:T . #13=#<CLOS:EQL-SPECIALIZER COMMON-LISP:T>)
   (#14=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:FIND-METHOD-COMBINATION> . #15=#<CLOS:EQL-SPECIALIZER #14#>)
   (#16=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:FINALIZE-INHERITANCE> . #17=#<CLOS:EQL-SPECIALIZER #16#>)
   (#18=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-FINALIZED-P> . #19=#<CLOS:EQL-SPECIALIZER #18#>)
   (#20=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-PROTOTYPE> . #21=#<CLOS:EQL-SPECIALIZER #20#>)
   (#22=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-DEFAULT-INITARGS> . #23=#<CLOS:EQL-SPECIALIZER #22#>)
   (#24=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-DIRECT-DEFAULT-INITARGS> . #25=#<CLOS:EQL-SPECIALIZER #24#>)
   (#26=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-SLOTS> . #27=#<CLOS:EQL-SPECIALIZER #26#>)
   (#28=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-DIRECT-SLOTS> . #29=#<CLOS:EQL-SPECIALIZER #28#>)
   (#30=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-PRECEDENCE-LIST> . #31=#<CLOS:EQL-SPECIALIZER #30#>)
   (#32=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:CLASS-DIRECT-SUPERCLASSES> . #33=#<CLOS:EQL-SPECIALIZER #32#>)
   (#34=#<CLOS:STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF CLOS:CLASS-NAME)> . #35=#<CLOS:EQL-SPECIALIZER #34#>)
   (#36=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-LOCATION> . #37=#<CLOS:EQL-SPECIALIZER #36#>)
   (#38=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-WRITERS> . #39=#<CLOS:EQL-SPECIALIZER #38#>)
   (#40=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-READERS> . #41=#<CLOS:EQL-SPECIALIZER #40#>)
   (#42=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-ALLOCATION> . #43=#<CLOS:EQL-SPECIALIZER #42#>)
   (#44=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-TYPE> . #45=#<CLOS:EQL-SPECIALIZER #44#>)
   (#46=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-INITARGS> . #47=#<CLOS:EQL-SPECIALIZER #46#>)
   (#48=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-INITFUNCTION> . #49=#<CLOS:EQL-SPECIALIZER #48#>)
   (#50=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-INITFORM> . #51=#<CLOS:EQL-SPECIALIZER #50#>)
   (#52=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-DEFINITION-NAME> . #53=#<CLOS:EQL-SPECIALIZER #52#>)
   (#54=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:ACCESSOR-METHOD-SLOT-DEFINITION> . #55=#<CLOS:EQL-SPECIALIZER #54#>)
   (#56=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:METHOD-GENERIC-FUNCTION> . #57=#<CLOS:EQL-SPECIALIZER #56#>)
   (#58=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:METHOD-SPECIALIZERS> . #59=#<CLOS:EQL-SPECIALIZER #58#>)
   (#60=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:METHOD-LAMBDA-LIST> . #61=#<CLOS:EQL-SPECIALIZER #60#>)
   (#62=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:METHOD-FUNCTION> . #63=#<CLOS:EQL-SPECIALIZER #62#>)
   (#64=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-DECLARATIONS> . #65=#<CLOS:EQL-SPECIALIZER #64#>)
   (#66=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER> . #67=#<CLOS:EQL-SPECIALIZER #66#>)
   (#68=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-METHOD-COMBINATION> . #69=#<CLOS:EQL-SPECIALIZER #68#>)
   (#70=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-LAMBDA-LIST> . #71=#<CLOS:EQL-SPECIALIZER #70#>)
   (#72=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-METHOD-CLASS> . #73=#<CLOS:EQL-SPECIALIZER #72#>)
   (#74=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-METHODS> . #75=#<CLOS:EQL-SPECIALIZER #74#>)
   (#76=#<CLOS:STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF CLOS:GENERIC-FUNCTION-NAME)> . #77=#<CLOS:EQL-SPECIALIZER #76#>)
   (#78=#<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-NAME> . #79=#<CLOS:EQL-SPECIALIZER #78#>)
   (COMMON-LISP:NIL . #80=#<CLOS:EQL-SPECIALIZER COMMON-LISP:NIL>)>)
(:|DEFPARAMETER| |CLOS|::|<REAL>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|REAL|))
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-ALL-SUPERCLASSES-LOCATION*| 6.)
(:|DEFCONSTANT| |CLOS|::|*<SLOTTED-CLASS>-SUBCLASS-OF-STABLEHASH-P-LOCATION*| 16.)
(:|DEFCONSTANT| |CLOS|::|*<STANDARD-CLASS>-DEFAULT-INITARGS*| (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|<SYMBOL>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|SYMBOL|))
(:|DEFPARAMETER| |CLOS|::|<STANDARD-METHOD>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-METHOD|))
(:DEFPARAMETER CLOS::|#'generic-function-undeterminedp| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS::GENERIC-FUNCTION-UNDETERMINEDP>)
(:|DEFCONSTANT| |CLOS|::|*<EQL-SPECIALIZER>-SINGLETON-LOCATION*| 3.)
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-LISTENERS-LOCATION*| 14.)
(:|DEFCONSTANT| |CLOS|::|*<SLOT-DEFINITION>-TYPE-LOCATION*| 3.)
(:|DEFPARAMETER| |CLOS|::|<STRUCTURE-OBJECT>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|STRUCTURE-OBJECT|))
(:|DEFPARAMETER| |CLOS|::|*<SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|SLOT-DEFINITION| (|CLOS|::|METAOBJECT|)
  ((|CLOS|::|$NAME| :|TYPE| |COMMON-LISP|::|SYMBOL| :|INITARG| :|NAME|)
   (|CLOS|::|$INITARGS| :|TYPE| |COMMON-LISP|::|LIST| :|INITARG| :|INITARGS|) (|CLOS|::|$TYPE| :|TYPE| |COMMON-LISP|::|T| :|INITARG| :|TYPE|)
   (|CLOS|::|$ALLOCATION| :|TYPE| |COMMON-LISP|::|SYMBOL| :|INITARG| :|ALLOCATION|)
   (|CLOS|::|$INHERITABLE-INITER| :|TYPE| |COMMON-LISP|::|CONS| :|INITARG| |CLOS|::|INHERITABLE-INITER|)
   (|CLOS|::|$INHERITABLE-DOC| :|TYPE| |COMMON-LISP|::|CONS| :|INITARG| |CLOS|::|INHERITABLE-DOC|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|<COMPLEX>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|COMPLEX|))
(:|DEFPARAMETER| |CLOS|::|<METAOBJECT>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|METAOBJECT|))
(:|DEFPARAMETER| |CLOS|::|*<DIRECT-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|DIRECT-SLOT-DEFINITION| (|CLOS|::|SLOT-DEFINITION|)
  ((|CLOS|::|$READERS| :|TYPE| |COMMON-LISP|::|LIST| :|INITARG| :|READERS|)
   (|CLOS|::|$WRITERS| :|TYPE| |COMMON-LISP|::|LIST| :|INITARG| :|WRITERS|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-CURRENT-VERSION-LOCATION*| 21.)
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-DIRECT-INSTANCE-SPECIALIZERS-LOCATION*| 25.)
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-SLOTS-LOCATION*| 9.)
(:|DEFPARAMETER| |CLOS|::|<BUILT-IN-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|BUILT-IN-CLASS|))
(:|DEFPARAMETER| |CLOS|::|*<EQL-SPECIALIZER>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|EQL-SPECIALIZER| (|CLOS|::|SPECIALIZER|) ((|CLOS|::|$SINGLETON| :|INITARG| |CLOS|::|SINGLETON|))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*<SEMI-STANDARD-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|SEMI-STANDARD-CLASS| (|CLOS|::|SLOTTED-CLASS|)
  ((|CLOS|::|$CURRENT-VERSION| :|TYPE| |COMMON-LISP|::|SIMPLE-VECTOR|) (|CLOS|::|$FUNCALLABLEP| :|TYPE| |COMMON-LISP|::|BOOLEAN|)
   (|CLOS|::|$FIXED-SLOT-LOCATIONS| :|INITARG| :|FIXED-SLOT-LOCATIONS|)
   (|CLOS|::|$INSTANTIATED| :|TYPE| |COMMON-LISP|::|BOOLEAN| :|INITFORM| |COMMON-LISP|::|NIL|)
   (|CLOS|::|$DIRECT-INSTANCE-SPECIALIZERS| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|HASH-TABLE| |CLOS|::|WEAK-LIST| |COMMON-LISP|::|NULL|)
    :|INITFORM| |COMMON-LISP|::|NIL|)
   (|CLOS|::|$FINALIZED-DIRECT-SUBCLASSES| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|HASH-TABLE| |CLOS|::|WEAK-LIST| |COMMON-LISP|::|NULL|)
    :|INITFORM| '|COMMON-LISP|::|NIL|)
   (|CLOS|::|$PROTOTYPE| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|STANDARD-OBJECT| |COMMON-LISP|::|NULL|)))
  (:|DEFAULT-INITARGS| :|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|NIL|) (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*<MISDESIGNED-FORWARD-REFERENCED-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|MISDESIGNED-FORWARD-REFERENCED-CLASS| (|CLOS|::|FORWARD-REFERENCE-TO-CLASS| |CLOS|::|POTENTIAL-CLASS|)
  |COMMON-LISP|::|NIL| (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|NIL|)))
(:|DEFPARAMETER| |CLOS|::|*<SLOTTED-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|SLOTTED-CLASS| (|CLOS|::|DEFINED-CLASS|)
  ((|CLOS|::|$SUBCLASS-OF-STABLEHASH-P| :|TYPE| |COMMON-LISP|::|BOOLEAN|) (|CLOS|::|$GENERIC-ACCESSORS| :|INITFORM| |COMMON-LISP|::|T|)
   (|CLOS|::|$DIRECT-ACCESSORS| :|TYPE| |COMMON-LISP|::|LIST| :|INITFORM| '|COMMON-LISP|::|NIL|)
   (|CLOS|::|$VALID-INITARGS-FROM-SLOTS| :|TYPE| |COMMON-LISP|::|LIST|)
   (|CLOS|::|$INSTANCE-SIZE| :|TYPE| (|COMMON-LISP|::|INTEGER| 1. |COMMON-LISP|::|*|)))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|<STANDARD-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-CLASS|))
(:|DEFPARAMETER| |CLOS|::|<LIST>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|LIST|))
(:|DEFPARAMETER| |CLOS|::|<INTEGER>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|INTEGER|))
(:|DEFPARAMETER| |CLOS|::|*UPDATE-INSTANCE-FOR-REDEFINED-CLASS-TABLE*|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|))
(:|DEFPARAMETER| |CLOS|::|<HASH-TABLE>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|HASH-TABLE|))
(:|DEFCONSTANT| |CLOS|::|*<BUILT-IN-CLASS>-INSTANCE-SIZE*| 17.)
(:|DEFPARAMETER| |CLOS|::|<ECHO-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|ECHO-STREAM|))
(:|DEFPARAMETER| |CLOS|::|*<STRUCTURE-CLASS>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STRUCTURE-CLASS|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-EFFECTIVE-SLOT-DEFINITION>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-EFFECTIVE-SLOT-DEFINITION|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|<TWO-WAY-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|TWO-WAY-STREAM|))
(:|DEFCONSTANT| |CLOS|::|*<STANDARD-CLASS>-VALID-INITIALIZATION-KEYWORDS*|
 (:|NAME| :|DIRECT-SUPERCLASSES| :|DIRECT-SLOTS| :|DIRECT-DEFAULT-INITARGS| :|DOCUMENTATION| :|GENERIC-ACCESSORS| :|FIXED-SLOT-LOCATIONS|))
(:|DEFPARAMETER| |CLOS|::|<STRUCTURE-DIRECT-SLOT-DEFINITION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STRUCTURE-DIRECT-SLOT-DEFINITION|))
(:|DEFPARAMETER| |CLOS|::|*<EQL-SPECIALIZER>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|EQL-SPECIALIZER|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|*EQL-SPECIALIZER-TABLE*|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|FASTHASH-EQL| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|))
(:|DEFPARAMETER| |CLOS|::|*DUMMY-CLASS*|
 #(|COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
   |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|
   |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|T| |COMMON-LISP|::|NIL|
   |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|*DYNAMICALLY-MODIFIABLE-GENERIC-FUNCTION-NAMES*|
 (|CLOS|::|ADD-METHOD| |CLOS|::|ALLOCATE-INSTANCE| |CLOS|::|CHANGE-CLASS| |CLOS|::|CLASS-NAME| (|COMMON-LISP|::|SETF| |CLOS|::|CLASS-NAME|)
  |CLOS|::|COMPUTE-APPLICABLE-METHODS| |CLOS|::|DESCRIBE-OBJECT| |CLOS|::|DOCUMENTATION| (|COMMON-LISP|::|SETF| |CLOS|::|DOCUMENTATION|)
  |CLOS|::|FIND-METHOD| |CLOS|::|FUNCTION-KEYWORDS| |CLOS|::|INITIALIZE-INSTANCE| |CLOS|::|MAKE-INSTANCE| |CLOS|::|MAKE-INSTANCES-OBSOLETE|
  |CLOS|::|MAKE-LOAD-FORM| |CLOS|::|METHOD-QUALIFIERS| |CLOS|::|NO-APPLICABLE-METHOD| |CLOS|::|NO-NEXT-METHOD| |CLOS|::|PRINT-OBJECT|
  |CLOS|::|REINITIALIZE-INSTANCE| |CLOS|::|REMOVE-METHOD| |CLOS|::|SHARED-INITIALIZE| |CLOS|::|SLOT-MISSING| |CLOS|::|SLOT-UNBOUND|
  |CLOS|::|UPDATE-INSTANCE-FOR-DIFFERENT-CLASS| |CLOS|::|UPDATE-INSTANCE-FOR-REDEFINED-CLASS| |CLOS|::|ADD-DEPENDENT| |CLOS|::|REMOVE-DEPENDENT|
  |CLOS|::|MAP-DEPENDENTS| |CLOS|::|ADD-DIRECT-METHOD| |CLOS|::|REMOVE-DIRECT-METHOD| |CLOS|::|SPECIALIZER-DIRECT-GENERIC-FUNCTIONS|
  |CLOS|::|SPECIALIZER-DIRECT-METHODS| |CLOS|::|ADD-DIRECT-SUBCLASS| |CLOS|::|REMOVE-DIRECT-SUBCLASS| |CLOS|::|CLASS-DIRECT-SUBCLASSES|
  |CLOS|::|COMPUTE-APPLICABLE-METHODS-USING-CLASSES| |CLOS|::|COMPUTE-CLASS-PRECEDENCE-LIST| |CLOS|::|COMPUTE-DEFAULT-INITARGS|
  |CLOS|::|COMPUTE-DIRECT-SLOT-DEFINITION-INITARGS| |CLOS|::|COMPUTE-DISCRIMINATING-FUNCTION| |CLOS|::|COMPUTE-EFFECTIVE-METHOD|
  |CLOS|::|COMPUTE-EFFECTIVE-SLOT-DEFINITION| |CLOS|::|COMPUTE-EFFECTIVE-SLOT-DEFINITION-INITARGS| |CLOS|::|COMPUTE-SLOTS|
  |CLOS|::|DIRECT-SLOT-DEFINITION-CLASS| |CLOS|::|EFFECTIVE-SLOT-DEFINITION-CLASS| |CLOS|::|ENSURE-CLASS-USING-CLASS|
  |CLOS|::|ENSURE-GENERIC-FUNCTION-USING-CLASS| |CLOS|::|READER-METHOD-CLASS| |CLOS|::|SLOT-VALUE-USING-CLASS|
  (|COMMON-LISP|::|SETF| |CLOS|::|SLOT-VALUE-USING-CLASS|) |CLOS|::|SLOT-BOUNDP-USING-CLASS| |CLOS|::|SLOT-MAKUNBOUND-USING-CLASS|
  |CLOS|::|VALIDATE-SUPERCLASS| |CLOS|::|WRITER-METHOD-CLASS| (|COMMON-LISP|::|SETF| |CLOS|::|METHOD-GENERIC-FUNCTION|)
  |CLOS|::|NO-PRIMARY-METHOD|))
(:DEFPARAMETER CLOS::|#'(setf slot-value-using-class)| #<CLOS:STANDARD-GENERIC-FUNCTION (COMMON-LISP:SETF CLOS:SLOT-VALUE-USING-CLASS)>)
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-INSTANTIATED-LOCATION*| 24.)
(:|DEFPARAMETER| |CLOS|::|<T>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|T|))
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-INITIALIZED-LOCATION*| 15.)
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-FINALIZED-DIRECT-SUBCLASSES-LOCATION*| 26.)
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-DOCUMENTATION-LOCATION*| 13.)
(:DEFPARAMETER CLOS::|#'generic-function-method-combination| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-METHOD-COMBINATION>)
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-FIXED-SLOT-LOCATIONS-LOCATION*| 23.)
(:|DEFPARAMETER| |CLOS|::|<FUNCALLABLE-STANDARD-OBJECT>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|FUNCALLABLE-STANDARD-OBJECT|))
(:|DEFPARAMETER| |CLOS|::|<METHOD>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|METHOD|))
(:|DEFPARAMETER| |CLOS|::|<FUNCTION>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|FUNCTION|))
(:DEFPARAMETER CLOS::|#'shared-initialize| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SHARED-INITIALIZE>)
(:DEFPARAMETER CLOS::|#'allocate-instance| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:ALLOCATE-INSTANCE>)
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-PREDICATE-LOCATION*| 25.)
(:DEFPARAMETER CLOS::|#'method-specializers| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:METHOD-SPECIALIZERS>)
(:|DEFPARAMETER| |CLOS|::|<STRUCTURE-EFFECTIVE-SLOT-DEFINITION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STRUCTURE-EFFECTIVE-SLOT-DEFINITION|))
(:|DEFCONSTANT| |CLOS|::|*<SLOTTED-CLASS>-INSTANCE-SIZE-LOCATION*| 20.)
(:|DEFCONSTANT| |CLOS|::|*<SLOT-DEFINITION>-INHERITABLE-INITER-LOCATION*| 5.)
(:|DEFCONSTANT| |CLOS|::|*<POTENTIAL-CLASS>-CLASSNAME-LOCATION*| 3.)
(:|DEFPARAMETER| |CLOS|::|*METHOD-COMBINATION-GENERIC-FUNCTION*| |COMMON-LISP|::|NIL|)
(:|DEFCONSTANT| |CLOS|::|*<SPECIALIZER>-DIRECT-METHODS-LOCATION*| 2.)
(:|DEFPARAMETER| |CLOS|::|<STRING>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|STRING|))
(:|DEFCONSTANT| |CLOS|::|*<SLOTTED-CLASS>-VALID-INITARGS-FROM-SLOTS-LOCATION*| 19.)
(:|DEFPARAMETER| |CLOS|::|*<POTENTIAL-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|POTENTIAL-CLASS| (|CLOS|::|SPECIALIZER| |CLOS|::|SUPER-CLASS|) |COMMON-LISP|::|NIL|
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STANDARD-CLASS| (|CLOS|::|SEMI-STANDARD-CLASS|) |COMMON-LISP|::|NIL| (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|<ARRAY>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|ARRAY|))
(:|DEFPARAMETER| |CLOS|::|<SYNONYM-STREAM>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|SYNONYM-STREAM|))
(:|DEFPARAMETER| |CLOS|::|<RATIONAL>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|RATIONAL|))
(:|DEFPARAMETER| |CLOS|::|<RANDOM-STATE>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|RANDOM-STATE|))
(:|DEFCONSTANT| |CLOS|::|*<EQL-SPECIALIZER>-INSTANCE-SIZE*| 4.)
(:DEFPARAMETER CLOS::|#'slot-boundp-using-class| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-BOUNDP-USING-CLASS>)
(:|DEFPARAMETER| |CLOS|::|*UPDATE-INSTANCE-FOR-DIFFERENT-CLASS-TABLE*|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQUAL| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|))
(:|DEFPARAMETER| |CLOS|::|*<STRUCTURE-EFFECTIVE-SLOT-DEFINITION>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STRUCTURE-EFFECTIVE-SLOT-DEFINITION|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:|DEFPARAMETER| |CLOS|::|*<STRUCTURE-DIRECT-SLOT-DEFINITION>-CLASS-VERSION*|
 #1=#(#2=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STRUCTURE-DIRECT-SLOT-DEFINITION|) #2# |COMMON-LISP|::|NIL| 0. |COMMON-LISP|::|NIL|
      |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL| |COMMON-LISP|::|NIL|))
(:DEFPARAMETER CLOS::|#'generic-function-methods| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-METHODS>)
(:DEFPARAMETER CLOS::|#'slot-makunbound-using-class| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:SLOT-MAKUNBOUND-USING-CLASS>)
(:|DEFPARAMETER| |CLOS|::|<MISDESIGNED-FORWARD-REFERENCED-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|MISDESIGNED-FORWARD-REFERENCED-CLASS|))
(:DEFPARAMETER CLOS::|#'update-instance-for-different-class| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:UPDATE-INSTANCE-FOR-DIFFERENT-CLASS>)
(:|DEFPARAMETER| |CLOS|::|<DEFINED-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|CLASS|))
(:|DEFPARAMETER| |CLOS|::|<STANDARD-GENERIC-FUNCTION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|STANDARD-GENERIC-FUNCTION|))
(:|DEFCONSTANT| |CLOS|::|*<SLOT-DEFINITION>-ALLOCATION-LOCATION*| 4.)
(:|DEFPARAMETER| |CLOS|::|<NULL>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|NULL|))
(:|DEFPARAMETER| |CLOS|::|<GENERIC-FUNCTION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|GENERIC-FUNCTION|))
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-COPIER-LOCATION*| 24.)
(:|DEFPARAMETER| |CLOS|::|<SEQUENCE>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|SEQUENCE|))
(:|DEFCONSTANT| |CLOS|::|EMPTY-HT|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|FASTHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|))
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-PROTOTYPE-LOCATION*| 26.)
(:|DEFCONSTANT| |CLOS|::|*<SEMI-STANDARD-CLASS>-PROTOTYPE-LOCATION*| 27.)
(:|DEFCONSTANT| |CLOS|::|*<DIRECT-SLOT-DEFINITION>-WRITERS-LOCATION*| 8.)
(:|DEFPARAMETER| |CLOS|::|<EQL-SPECIALIZER>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|EQL-SPECIALIZER|))
(:|DEFCONSTANT| |CLOS|::|*<SLOTTED-CLASS>-GENERIC-ACCESSORS-LOCATION*| 17.)
(:|DEFPARAMETER| |CLOS|::|<EFFECTIVE-SLOT-DEFINITION>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|EFFECTIVE-SLOT-DEFINITION|))
(:|DEFPARAMETER| |CLOS|::|<CHARACTER>| #1=#.(|CLOS|::|FIND-CLASS| '|COMMON-LISP|::|CHARACTER|))
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-PRECEDENCE-LIST-LOCATION*| 7.)
(:|DEFCONSTANT| |CLOS|::|*<BUILT-IN-CLASS>-PROTOTYPE-LOCATION*| 16.)
(:|DEFCONSTANT| |CLOS|::|*<DIRECT-SLOT-DEFINITION>-READERS-LOCATION*| 7.)
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-STABLEHASH>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STANDARD-STABLEHASH| |COMMON-LISP|::|NIL| ((|CLOS|::|$HASHCODE| :|INITFORM| (|SYSTEM|::|RANDOM-POSFIXNUM|)))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFCONSTANT| |CLOS|::|*<STRUCTURE-CLASS>-BOA-CONSTRUCTORS-LOCATION*| 23.)
(:|DEFPARAMETER| |CLOS|::|*METHOD-COMBINATION-ARGUMENTS*| |COMMON-LISP|::|NIL|)
(:|DEFPARAMETER| |CLOS|::|*<STRUCTURE-DIRECT-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STRUCTURE-DIRECT-SLOT-DEFINITION| (|CLOS|::|DIRECT-SLOT-DEFINITION|) |COMMON-LISP|::|NIL|
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*<STANDARD-DIRECT-SLOT-DEFINITION>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STANDARD-DIRECT-SLOT-DEFINITION| (|CLOS|::|DIRECT-SLOT-DEFINITION| |CLOS|::|STANDARD-SLOT-DEFINITION|)
  |COMMON-LISP|::|NIL| (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFPARAMETER| |CLOS|::|*<SPECIALIZER>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|SPECIALIZER| (|CLOS|::|STANDARD-STABLEHASH| |CLOS|::|METAOBJECT|)
  ((|CLOS|::|$DIRECT-METHODS| :|INITFORM| |COMMON-LISP|::|NIL|)) (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:|DEFCONSTANT| |CLOS|::|*<EFFECTIVE-SLOT-DEFINITION>-EFM-SVUC-LOCATION*| 8.)
(:|DEFPARAMETER| |CLOS|::|<SEMI-STANDARD-CLASS>| #1=#.(|CLOS|::|FIND-CLASS| '|CLOS|::|SEMI-STANDARD-CLASS|))
(:|DEFCONSTANT| |CLOS|::|*<DEFINED-CLASS>-DIRECT-SLOTS-LOCATION*| 8.)
(:|DEFPARAMETER| |CLOS|::|*<STRUCTURE-CLASS>-DEFCLASS*|
 (|CLOS|::|DEFCLASS| |CLOS|::|STRUCTURE-CLASS| (|CLOS|::|SLOTTED-CLASS|)
  ((|CLOS|::|$NAMES| :|TYPE| |COMMON-LISP|::|CONS|) (|CLOS|::|$KCONSTRUCTOR| :|TYPE| |COMMON-LISP|::|SYMBOL|)
   (|CLOS|::|$BOA-CONSTRUCTORS| :|TYPE| |COMMON-LISP|::|LIST|) (|CLOS|::|$COPIER| :|TYPE| |COMMON-LISP|::|SYMBOL|)
   (|CLOS|::|$PREDICATE| :|TYPE| |COMMON-LISP|::|SYMBOL|)
   (|CLOS|::|$PROTOTYPE| :|TYPE| (|COMMON-LISP|::|OR| |COMMON-LISP|::|STRUCTURE-OBJECT| |COMMON-LISP|::|NULL|)))
  (:|FIXED-SLOT-LOCATIONS| |COMMON-LISP|::|T|)))
(:DEFPARAMETER CLOS::|#'generic-function-method-class| #<CLOS:STANDARD-GENERIC-FUNCTION CLOS:GENERIC-FUNCTION-METHOD-CLASS>)
(:|DEFPARAMETER| |CLOS|::|*REINITIALIZE-INSTANCE-TABLE*|
 #S(|COMMON-LISP|::|HASH-TABLE| :|TEST| |EXT|::|STABLEHASH-EQ| :|WARN-IF-NEEDS-REHASH-AFTER-GC| |COMMON-LISP|::|T|))


Armed Bear Common Lisp 1.6.0-dev
Java 1.8.0_144 Oracle Corporation
Java HotSpot(TM) 64-Bit Server VM
Low-level initialization completed in 0.295 seconds.
Startup completed in 1.798 seconds.
Loading /root/.abclrc completed in 0.002 seconds.
Type ":help" for a list of available commands.
CL-USER(1):
(defun allpackagevalues () (dolist (p (list :cl :user :sys :EXT :clos)) (package-values p)))
CL-USER(1): (defun maybe-pprint (o) (handler-case (write-to-string o) (t (a)  (let ((*print-readably* nil)) (write-to-string o)))))
(defun package-values (ps)
  (let ((p (find-package ps))(kwp (or (find-package "TMP") (make-package "TMP" :use ()))))
    (do-all-symbols (sym)
ALLPACKAGEVALUES
     (when (and (eq (symbol-package sym) p) (boundp sym))
CL-USER(2):       (let((*package* kwp)
           (*print-escape* t)
           (*PRINT-LINES* nil)(*PRINT-ARRAY* t)(*PRINT-GENSYM* t)(*PRINT-CIRCLE* t)
MAYBE-PPRINT
CL-USER(3):        (*print-readably* t))(fresh-line)(fresh-line)
         (princ (maybe-pprint `(,(if(constantp sym) :defconstant :defparameter) ,sym ,(symbol-value sym)))))))))
(allpackagevalues)

PACKAGE-VALUES
CL-USER(4): (:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-SINGLE-FLOAT -1.4f-45)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT 1.17549435f-38)
(:DEFPARAMETER COMMON-LISP:*PRINT-PPRINT-DISPATCH* #S(XP::PPRINT-DISPATCH-TABLE :CONSES-WITH-CARS #<EQ HASH-TABLE 57 entries, 114 buckets {5438921F}> :STRUCTURES #<EQ HASH-TABLE 1 entry, 32 buckets {4F66E4EF}> :OTHERS (#S(XP::ENTRY :TEST (COMMON-LISP:LAMBDA (XP::X) (COMMON-LISP:FUNCALL (COMMON-LISP:FUNCTION XP::FUNCTION-CALL-P) XP::X)) :FN XP::FN-CALL :FULL-SPEC ((-5) (COMMON-LISP:SATISFIES XP::FUNCTION-CALL-P))) #S(XP::ENTRY :TEST #<COMMON-LISP:COMPILED-FUNCTION #<CONSP {77813CB2}> {77813CB2}> :FN COMMON-LISP:PPRINT-FILL :FULL-SPEC ((-10) COMMON-LISP:CONS)))))
(:DEFPARAMETER COMMON-LISP:*STANDARD-INPUT* #S(SYSTEM::SYSTEM-STREAM))
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-LONG-FLOAT 4.9d-324)
(:DEFPARAMETER COMMON-LISP:*ERROR-OUTPUT* #S(SYSTEM::SYSTEM-STREAM))
(:DEFPARAMETER COMMON-LISP:*LOAD-TRUENAME* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT 2.2250738585072014d-308)
(:DEFPARAMETER COMMON-LISP:*COMPILE-FILE-TRUENAME* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-DOUBLE-FLOAT -4.9d-324)
(:DEFCONSTANT COMMON-LISP:BOOLE-AND 6)
(:DEFPARAMETER COMMON-LISP:*LOAD-PRINT* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:CHAR-CODE-LIMIT 65536)
(:DEFPARAMETER COMMON-LISP:+++ (COMMON-LISP:DEFUN COMMON-LISP-USER::ALLPACKAGEVALUES COMMON-LISP:NIL (COMMON-LISP:DOLIST (COMMON-LISP-USER::P (COMMON-LISP:LIST :CL :USER :SYS :EXT :CLOS)) (COMMON-LISP-USER::PACKAGE-VALUES COMMON-LISP-USER::P))))
(:DEFCONSTANT COMMON-LISP:BOOLE-XOR 8)
(:DEFPARAMETER COMMON-LISP:*PRINT-ESCAPE* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-DOUBLE-FLOAT 1.7976931348623157d308)
(:DEFCONSTANT COMMON-LISP:LAMBDA-PARAMETERS-LIMIT 50)
(:DEFPARAMETER COMMON-LISP:*BREAK-ON-SIGNALS* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-FIXNUM 2147483647)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-SHORT-FLOAT -1.4f-45)
(:DEFCONSTANT COMMON-LISP:BOOLE-CLR 0)
(:DEFPARAMETER COMMON-LISP:*RANDOM-STATE* #<RANDOM-STATE {76231CD6}>)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-LONG-FLOAT -1.7976931348623157d308)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-FIXNUM -2147483648)
(:DEFPARAMETER COMMON-LISP:/// (COMMON-LISP-USER::ALLPACKAGEVALUES))
(:DEFCONSTANT COMMON-LISP:BOOLE-EQV 9)
(:DEFPARAMETER COMMON-LISP:*MACROEXPAND-HOOK* COMMON-LISP:FUNCALL)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT -2.2250738585072014d-308)
(:DEFCONSTANT COMMON-LISP:LONG-FLOAT-NEGATIVE-EPSILON 5.551115123125784d-17)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-DOUBLE-FLOAT 4.9d-324)
(:DEFPARAMETER COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS* #P"/home/dmiles/logicmoo_workspace/packs_usr/armedbear_abcl/")
(:DEFCONSTANT COMMON-LISP:SINGLE-FLOAT-NEGATIVE-EPSILON 2.9802326f-8)
(:DEFPARAMETER COMMON-LISP:*PRINT-PRETTY* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:NIL COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:// (COMMON-LISP-USER::MAYBE-PPRINT))
(:DEFCONSTANT COMMON-LISP:BOOLE-NAND 10)
(:DEFPARAMETER COMMON-LISP:*PRINT-LINES* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*READ-BASE* 10)
(:DEFCONSTANT COMMON-LISP:ARRAY-RANK-LIMIT 8)
(:DEFPARAMETER COMMON-LISP:++ (COMMON-LISP:DEFUN COMMON-LISP-USER::MAYBE-PPRINT (COMMON-LISP-USER::O) (COMMON-LISP:HANDLER-CASE (COMMON-LISP:WRITE-TO-STRING COMMON-LISP-USER::O) (COMMON-LISP:T (COMMON-LISP-USER::A) (COMMON-LISP:LET ((COMMON-LISP:*PRINT-READABLY* COMMON-LISP:NIL)) (COMMON-LISP:WRITE-TO-STRING COMMON-LISP-USER::O))))))
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT -2.2250738585072014d-308)
(:DEFCONSTANT COMMON-LISP:ARRAY-DIMENSION-LIMIT 2147483647)
(:DEFPARAMETER COMMON-LISP:** COMMON-LISP-USER::MAYBE-PPRINT)
(:DEFPARAMETER COMMON-LISP:*PRINT-RIGHT-MARGIN* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*READTABLE* #<org.armedbear.lisp.Readtable@5261e09d>)
(:DEFPARAMETER COMMON-LISP:*COMPILE-PRINT* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:BOOLE-IOR 7)
(:DEFCONSTANT COMMON-LISP:INTERNAL-TIME-UNITS-PER-SECOND 1000)
(:DEFPARAMETER COMMON-LISP:*READ-DEFAULT-FLOAT-FORMAT* COMMON-LISP:SINGLE-FLOAT)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-SHORT-FLOAT 3.4028235f38)
(:DEFPARAMETER COMMON-LISP:*COMPILE-FILE-PATHNAME* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*PACKAGE* #<PACKAGE TMP>)
(:DEFPARAMETER COMMON-LISP:*TERMINAL-IO* #<TWO-WAY-STREAM {48911A69}>)
(:DEFCONSTANT COMMON-LISP:CALL-ARGUMENTS-LIMIT 50)
(:DEFCONSTANT COMMON-LISP:DOUBLE-FLOAT-EPSILON 1.1102230246251568d-16)
(:DEFCONSTANT COMMON-LISP:BOOLE-1 2)
(:DEFCONSTANT COMMON-LISP:SINGLE-FLOAT-EPSILON 5.960465f-8)
(:DEFPARAMETER COMMON-LISP:*PRINT-LENGTH* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-LONG-FLOAT 2.2250738585072014d-308)
(:DEFPARAMETER COMMON-LISP:*FEATURES* (:X86-64 :JAVA-1.8 :UNIX :LINUX :ARMEDBEAR :ABCL :COMMON-LISP :ANSI-CL :CDR6 :MOP :PACKAGE-LOCAL-NICKNAMES))
(:DEFCONSTANT COMMON-LISP:SHORT-FLOAT-EPSILON 5.960465f-8)
(:DEFPARAMETER COMMON-LISP:*PRINT-RADIX* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LONG-FLOAT-EPSILON 1.1102230246251568d-16)
(:DEFCONSTANT COMMON-LISP:BOOLE-2 3)
(:DEFCONSTANT COMMON-LISP:BOOLE-C1 4)
(:DEFCONSTANT COMMON-LISP:BOOLE-C2 5)
(:DEFPARAMETER COMMON-LISP:*** COMMON-LISP-USER::ALLPACKAGEVALUES)
(:DEFCONSTANT COMMON-LISP:BOOLE-SET 1)
(:DEFPARAMETER COMMON-LISP:*DEBUGGER-HOOK* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:BOOLE-ANDC2 13)
(:DEFCONSTANT COMMON-LISP:BOOLE-ANDC1 12)
(:DEFPARAMETER COMMON-LISP:*PRINT-MISER-WIDTH* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*PRINT-GENSYM* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-SINGLE-FLOAT -3.4028235f38)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-LONG-FLOAT -4.9d-324)
(:DEFPARAMETER COMMON-LISP:*PRINT-CIRCLE* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*QUERY-IO* #<TWO-WAY-STREAM {26BA7A01}>)
(:DEFPARAMETER COMMON-LISP:*READ-EVAL* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-SHORT-FLOAT 1.4f-45)
(:DEFPARAMETER COMMON-LISP:*LOAD-VERBOSE* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:MULTIPLE-VALUES-LIMIT 32)
(:DEFCONSTANT COMMON-LISP:BOOLE-ORC2 15)
(:DEFCONSTANT COMMON-LISP:BOOLE-ORC1 14)
(:DEFPARAMETER COMMON-LISP:*MODULES* ("INSPECT" "FORMAT" "PRINT-OBJECT" "CLOS" "MOP" "DELETE" "COLLECT" "PPRINT-DISPATCH" "PPRINT" "PRINT" "EXTENSIBLE-SEQUENCES-BASE"))
(:DEFPARAMETER COMMON-LISP:*DEBUG-IO* #<TWO-WAY-STREAM {7B820343}>)
(:DEFCONSTANT COMMON-LISP:PI 3.141592653589793d0)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT 1.17549435f-38)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-SINGLE-FLOAT 3.4028235f38)
(:DEFPARAMETER COMMON-LISP:*TRACE-OUTPUT* #S(SYSTEM::SYSTEM-STREAM))
(:DEFPARAMETER COMMON-LISP:*PRINT-LEVEL* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT -1.17549435f-38)
(:DEFPARAMETER COMMON-LISP:*STANDARD-OUTPUT* #S(SYSTEM::SYSTEM-STREAM))
(:DEFPARAMETER COMMON-LISP:*PRINT-BASE* 10)
(:DEFPARAMETER COMMON-LISP:*PRINT-CASE* :UPCASE)
(:DEFPARAMETER COMMON-LISP:*PRINT-READABLY* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:ARRAY-TOTAL-SIZE-LIMIT 2147483647)
(:DEFPARAMETER COMMON-LISP:*GENSYM-COUNTER* 51)
(:DEFCONSTANT COMMON-LISP:BOOLE-NOR 11)
(:DEFCONSTANT COMMON-LISP:SHORT-FLOAT-NEGATIVE-EPSILON 2.9802326f-8)
(:DEFPARAMETER COMMON-LISP:*COMPILE-VERBOSE* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*PRINT-ARRAY* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-LONG-FLOAT 1.7976931348623157d308)
(:DEFCONSTANT COMMON-LISP:DOUBLE-FLOAT-NEGATIVE-EPSILON 5.551115123125784d-17)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-SHORT-FLOAT -3.4028235f38)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-DOUBLE-FLOAT -1.7976931348623157d308)
(:DEFCONSTANT COMMON-LISP:LAMBDA-LIST-KEYWORDS (COMMON-LISP:&OPTIONAL COMMON-LISP:&REST COMMON-LISP:&KEY COMMON-LISP:&AUX COMMON-LISP:&BODY COMMON-LISP:&WHOLE COMMON-LISP:&ALLOW-OTHER-KEYS COMMON-LISP:&ENVIRONMENT))
(:DEFPARAMETER COMMON-LISP:*READ-SUPPRESS* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:T COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT -1.17549435f-38)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-SINGLE-FLOAT 1.4f-45)
(:DEFPARAMETER COMMON-LISP:/ (COMMON-LISP-USER::PACKAGE-VALUES))
(:DEFPARAMETER COMMON-LISP:- (COMMON-LISP-USER::ALLPACKAGEVALUES))
(:DEFPARAMETER COMMON-LISP:+ (COMMON-LISP:DEFUN COMMON-LISP-USER::PACKAGE-VALUES (COMMON-LISP-USER::PS) (COMMON-LISP:LET ((COMMON-LISP-USER::P (COMMON-LISP:FIND-PACKAGE COMMON-LISP-USER::PS)) (COMMON-LISP-USER::KWP (COMMON-LISP:OR (COMMON-LISP:FIND-PACKAGE "TMP") (COMMON-LISP:MAKE-PACKAGE "TMP" :USE COMMON-LISP:NIL)))) (COMMON-LISP:DO-ALL-SYMBOLS (COMMON-LISP-USER::SYM) (COMMON-LISP:WHEN (COMMON-LISP:AND (COMMON-LISP:EQ (COMMON-LISP:SYMBOL-PACKAGE COMMON-LISP-USER::SYM) COMMON-LISP-USER::P) (COMMON-LISP:BOUNDP COMMON-LISP-USER::SYM)) (COMMON-LISP:LET ((COMMON-LISP:*PACKAGE* COMMON-LISP-USER::KWP) (COMMON-LISP:*PRINT-ESCAPE* COMMON-LISP:T) (COMMON-LISP:*PRINT-LINES* COMMON-LISP:NIL) (COMMON-LISP:*PRINT-ARRAY* COMMON-LISP:T) (COMMON-LISP:*PRINT-GENSYM* COMMON-LISP:T) (COMMON-LISP:*PRINT-CIRCLE* COMMON-LISP:T) (COMMON-LISP:*PRINT-READABLY* COMMON-LISP:T)) (COMMON-LISP:FRESH-LINE) (COMMON-LISP:FRESH-LINE) (COMMON-LISP:PRINC (COMMON-LISP-USER::MAYBE-PPRINT (SYSTEM::BACKQ-LIST (COMMON-LISP:IF (COMMON-LISP:CONSTANTP COMMON-LISP-USER::SYM) :DEFCONSTANT :DEFPARAMETER) COMMON-LISP-USER::SYM (COMMON-LISP:SYMBOL-VALUE COMMON-LISP-USER::SYM))))))))))
(:DEFPARAMETER COMMON-LISP:* COMMON-LISP-USER::PACKAGE-VALUES)
(:DEFPARAMETER COMMON-LISP:*LOAD-PATHNAME* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*ENV-VAR* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*SHARP-SHARP-ALIST* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*SYSTEM-LETS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*SHARP-EQUAL-ALIST* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*INSPECT-BREAK* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*CURRENT-ERROR-DEPTH* 0)
(:DEFPARAMETER SYSTEM::*COMPILER-MACROS* #<EQUAL HASH-TABLE 15 entries, 22 buckets {1B4B370A}>)
(:DEFCONSTANT SYSTEM::+STANDARD-READTABLE+ #<org.armedbear.lisp.Readtable@5bc92458>)
(:DEFPARAMETER SYSTEM::*FSET-HOOKS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*LOAD-STREAM* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*MAXIMUM-ERROR-DEPTH* 10)
(:DEFPARAMETER SYSTEM::*INSPECTED-OBJECT* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*IGNORABLE-VARS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*AUTOLOADING-CACHE* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*BACKQUOTE-COUNT* 0)
(:DEFPARAMETER SYSTEM::*DECLARATION-TYPES* #<EQ HASH-TABLE 0 entries, 11 buckets {2ED62752}>)
(:DEFPARAMETER SYSTEM::*CIRCULARITY-HASH-TABLE* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*KNOWN-TYPES* #<EQ HASH-TABLE 81 entries, 176 buckets {45A5B1D0}>)
(:DEFCONSTANT SYSTEM::FORM-FEED-CHAR-CODE 12)
(:DEFPARAMETER SYSTEM::*MODULE-PROVIDER-FUNCTIONS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*DOUBLE-COLON-PACKAGE-SEPARATORS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*BACKQ-TOKENS* (SYSTEM::BACKQ-COMMA SYSTEM::BACKQ-COMMA-AT SYSTEM::BACKQ-COMMA-DOT SYSTEM::BACKQ-LIST SYSTEM::BACKQ-LIST* SYSTEM::BACKQ-APPEND SYSTEM::BACKQ-NCONC SYSTEM::BACKQ-CONS SYSTEM::BACKQ-VECTOR))
(:DEFPARAMETER SYSTEM::*RESTART-CLUSTERS* ((#<COMMON-LISP:RESTART TOP-LEVEL::TOP-LEVEL {7142D09}>)))
(:DEFPARAMETER SYSTEM::*HANDLER-CLUSTERS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*CIRCULARITY-COUNTER* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*INVOKE-DEBUGGER-HOOK* COMMON-LISP:NIL)
(:DEFCONSTANT SYSTEM::DOUBLE-FLOAT-EXPONENT-BYTE (11 . 20))
(:DEFPARAMETER SYSTEM::*BQ-DOT-FLAG* (SYSTEM::|,.|))
(:DEFPARAMETER SYSTEM::*BQ-VECTOR-FLAG* (#:|bqv|))
(:DEFPARAMETER SYSTEM::*USER-LETS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*FASL-UNINTERNED-SYMBOLS* COMMON-LISP:NIL)
(:DEFCONSTANT SYSTEM::*PROCLAIMED-FTYPES* #<EQUAL HASH-TABLE 0 entries, 11 buckets {186AE00F}>)
(:DEFPARAMETER SYSTEM::*INSPECTED-OBJECT-STACK* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*LOAD-DEPTH* 0)
(:DEFPARAMETER SYSTEM::*BQ-COMMA-FLAG* (SYSTEM::|,|))
(:DEFPARAMETER SYSTEM::*CONDITION-RESTARTS* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*BQ-AT-FLAG* (SYSTEM::|,@|))
(:DEFPARAMETER SYSTEM::*PRINT-FASL* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM::*DD-DEFAULT-SLOT-TYPE* COMMON-LISP:T)
(:DEFPARAMETER SYSTEM::*ARG-TESTS* COMMON-LISP:NIL)
(:DEFCONSTANT SYSTEM::*FASL-EXTERNAL-FORMAT* "UTF-8")
(:DEFCONSTANT SYSTEM:CALL-REGISTERS-LIMIT 8)
(:DEFCONSTANT SYSTEM:+KEYWORD-PACKAGE+ #<PACKAGE KEYWORD>)
(:DEFCONSTANT SYSTEM:+CL-PACKAGE+ #<PACKAGE COMMON-LISP>)
(:DEFPARAMETER SYSTEM:*SPACE* 1)
(:DEFPARAMETER SYSTEM:*INLINE-DECLARATIONS* (((COMMON-LISP:SETF MOP:SLOT-DEFINITION-DOCUMENTATION) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-TYPE) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP::SLOT-DEFINITION-LOCATION-CLASS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP::SLOT-DEFINITION-ALLOCATION-CLASS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-WRITERS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-READERS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-NAME) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-INITFUNCTION) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-INITFORM) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-INITARGS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:SLOT-DEFINITION-ALLOCATION) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-DIRECT-DEFAULT-INITARGS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-DEFAULT-INITARGS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-FINALIZED-P) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-PRECEDENCE-LIST) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-DIRECT-METHODS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-DIRECT-SUBCLASSES) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-DIRECT-SUPERCLASSES) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP::CLASS-LAYOUT) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-DIRECT-SLOTS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF MOP:CLASS-SLOTS) . COMMON-LISP:NOTINLINE) ((COMMON-LISP:SETF COMMON-LISP:CLASS-NAME) . COMMON-LISP:NOTINLINE)))
(:DEFCONSTANT SYSTEM:*FASL-VERSION* 43)
(:DEFPARAMETER SYSTEM:*SOURCE* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM:*CURRENT-PRINT-LEVEL* 0)
(:DEFPARAMETER SYSTEM:*TRACED-NAMES* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM:*COMPILE-FILE-ZIP* COMMON-LISP:T)
(:DEFPARAMETER SYSTEM:*EXPLAIN* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM:*DEBUG* 1)
(:DEFPARAMETER SYSTEM:*LOGICAL-PATHNAME-TRANSLATIONS* #<EQUAL HASH-TABLE 1 entry, 64 buckets {5B7F0136}>)
(:DEFPARAMETER SYSTEM:*FASL-LOADER* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM:*SPEED* 1)
(:DEFPARAMETER SYSTEM:*COMPILE-FILE-CLASS-EXTENSION* "cls")
(:DEFPARAMETER SYSTEM:*SAFETY* 1)
(:DEFPARAMETER SYSTEM:*CURRENT-PRINT-LENGTH* 0)
(:DEFPARAMETER SYSTEM:*SOURCE-POSITION* COMMON-LISP:NIL)
(:DEFCONSTANT SYSTEM:+SLOT-UNBOUND+ #<UNBOUND>)
(:DEFPARAMETER SYSTEM:*COMPILE-FILE-ENVIRONMENT* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM:*NOINFORM* COMMON-LISP:NIL)
(:DEFPARAMETER SYSTEM:*COMPILE-FILE-TYPE* "abcl")
(:DEFPARAMETER EXTENSIONS:*PRINT-STRUCTURE* COMMON-LISP:T)
(:DEFPARAMETER EXTENSIONS:*REQUIRE-STACK-FRAME* COMMON-LISP:NIL)
(:DEFPARAMETER EXTENSIONS:*ENABLE-INLINE-EXPANSION* COMMON-LISP:T)
(:DEFPARAMETER EXTENSIONS:*DEBUG-CONDITION* COMMON-LISP:NIL)
(:DEFPARAMETER EXTENSIONS:*DEBUG-LEVEL* 0)
(:DEFPARAMETER EXTENSIONS:*LOAD-TRUENAME-FASL* COMMON-LISP:NIL)
(:DEFPARAMETER EXTENSIONS:*WARN-ON-REDEFINITION* COMMON-LISP:T)
(:DEFCONSTANT EXTENSIONS:SINGLE-FLOAT-NEGATIVE-INFINITY #.EXTENSIONS:SINGLE-FLOAT-NEGATIVE-INFINITY)
(:DEFPARAMETER EXTENSIONS:*AUTOLOAD-VERBOSE* COMMON-LISP:NIL)
(:DEFPARAMETER EXTENSIONS:*DISASSEMBLER* "jad -a -p")
(:DEFPARAMETER EXTENSIONS:*SUPPRESS-COMPILER-WARNINGS* COMMON-LISP:NIL)
(:DEFCONSTANT EXTENSIONS:MOST-NEGATIVE-JAVA-LONG -9223372036854775808)
(:DEFPARAMETER EXTENSIONS:*LISP-HOME* #P"jar:file:/home/dmiles/logicmoo_workspace/packs_usr/armedbear_abcl/dist/abcl.jar!/org/armedbear/lisp/")
(:DEFCONSTANT EXTENSIONS:DOUBLE-FLOAT-POSITIVE-INFINITY #.EXTENSIONS:DOUBLE-FLOAT-POSITIVE-INFINITY)
(:DEFPARAMETER EXTENSIONS:*ED-FUNCTIONS* (SYSTEM::DEFAULT-ED-FUNCTION))
(:DEFPARAMETER EXTENSIONS:*COMMAND-LINE-ARGUMENT-LIST* COMMON-LISP:NIL)
(:DEFCONSTANT EXTENSIONS:SINGLE-FLOAT-POSITIVE-INFINITY #.EXTENSIONS:SINGLE-FLOAT-POSITIVE-INFINITY)
(:DEFCONSTANT EXTENSIONS:DOUBLE-FLOAT-NEGATIVE-INFINITY #.EXTENSIONS:DOUBLE-FLOAT-NEGATIVE-INFINITY)
(:DEFPARAMETER EXTENSIONS:*SAVED-BACKTRACE* COMMON-LISP:NIL)
(:DEFCONSTANT EXTENSIONS:MOST-POSITIVE-JAVA-LONG 9223372036854775807)
(:DEFPARAMETER EXTENSIONS:*BATCH-MODE* COMMON-LISP:NIL)
(:DEFPARAMETER EXTENSIONS:*INSPECTOR-HOOK* COMMON-LISP:NIL)


This is SBCL 1.3.1.debian, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (defun allpackagevalues () (dolist (p (list :cl :user :sys :EXT :clos)) (package-values p)))
(defun maybe-pprint (o) (handler-case (write-to-string o) (t (a)  (let ((*print-readably* nil)) (write-to-string o)))))
(defun package-values (ps)
  (let ((p (find-package ps))(kwp (or (find-package "TMP") (make-package "TMP" :use ()))))
    (do-all-symbols (sym)
     (when (and (eq (symbol-package sym) p) (boundp sym))
      (let((*package* kwp)
           (*print-escape* t)
           (*PRINT-LINES* nil)(*PRINT-ARRAY* t)(*PRINT-GENSYM* t)(*PRINT-CIRCLE* t)
           (*print-readably* t))(fresh-line)(fresh-line)
         (princ (maybe-pprint `(,(if(constantp sym) :defconstant :defparameter) ,sym ,(symbol-value sym)))))))))
(allpackagevalues)

; in: DEFUN ALLPACKAGEVALUES
;     (PACKAGE-VALUES P)
;
; caught STYLE-WARNING:
;   undefined function: PACKAGE-VALUES
;
; compilation unit finished
;   Undefined function:
;     PACKAGE-VALUES
;   caught 1 STYLE-WARNING condition

ALLPACKAGEVALUES
* ; in: DEFUN MAYBE-PPRINT
;     (HANDLER-CASE (WRITE-TO-STRING O)
;                   (T (A)
;                    (LET ((*PRINT-READABLY* NIL))
;                      (WRITE-TO-STRING O))))
; --> SB-INT:DX-FLET
; ==>
;   (FLET ((#:FORM-FUN-4 ()
;            (WRITE-TO-STRING O))
;          (#:FUN1 (A)
;            (LET (#)
;              (WRITE-TO-STRING O))))
;     (DECLARE
;      (SB-INT:TRULY-DYNAMIC-EXTENT (FUNCTION #:FORM-FUN-4) (FUNCTION #:FUN1)))
;     (DECLARE (OPTIMIZE (SB-C::CHECK-TAG-EXISTENCE 0)))
;     (BLOCK #:BLOCK2
;       (SB-INT:DX-LET ((#:CELL3 #))
;         (DECLARE (IGNORABLE #:CELL3))
;         (TAGBODY
;           (SB-IMPL::%HANDLER-BIND # #)
;          #:TAG0
;           (RETURN-FROM #:BLOCK2 #)))))
;
; caught STYLE-WARNING:
;   The variable A is defined but never used.
;
; compilation unit finished
;   caught 1 STYLE-WARNING condition

MAYBE-PPRINT
*
PACKAGE-VALUES
*
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-SHORT-FLOAT 3.4028235e38)
(:DEFPARAMETER COMMON-LISP:** COMMON-LISP-USER::MAYBE-PPRINT)
(:DEFPARAMETER COMMON-LISP:*RANDOM-STATE*
 #S(COMMON-LISP:RANDOM-STATE :STATE #.(COMMON-LISP:MAKE-ARRAY 627 :ELEMENT-TYPE
                                                              '(COMMON-LISP:UNSIGNED-BYTE
                                                                32)
                                                              :INITIAL-CONTENTS
                                                              '(0 2567483615
                                                                624 5489
                                                                1301868182
                                                                2938499221
                                                                2950281878
                                                                1875628136
                                                                751856242
                                                                944701696
                                                                2243192071
                                                                694061057
                                                                219885934
                                                                2066767472
                                                                3182869408
                                                                485472502
                                                                2336857883
                                                                1071588843
                                                                3418470598
                                                                951210697
                                                                3693558366
                                                                2923482051
                                                                1793174584
                                                                2982310801
                                                                1586906132
                                                                1951078751
                                                                1808158765
                                                                1733897588
                                                                431328322
                                                                4202539044
                                                                530658942
                                                                1714810322
                                                                3025256284
                                                                3342585396
                                                                1937033938
                                                                2640572511
                                                                1654299090
                                                                3692403553
                                                                4233871309
                                                                3497650794
                                                                862629010
                                                                2943236032
                                                                2426458545
                                                                1603307207
                                                                1133453895
                                                                3099196360
                                                                2208657629
                                                                2747653927
                                                                931059398
                                                                761573964
                                                                3157853227
                                                                785880413
                                                                730313442
                                                                124945756
                                                                2937117055
                                                                3295982469
                                                                1724353043
                                                                3021675344
                                                                3884886417
                                                                4010150098
                                                                4056961966
                                                                699635835
                                                                2681338818
                                                                1339167484
                                                                720757518
                                                                2800161476
                                                                2376097373
                                                                1532957371
                                                                3902664099
                                                                1238982754
                                                                3725394514
                                                                3449176889
                                                                3570962471
                                                                4287636090
                                                                4087307012
                                                                3603343627
                                                                202242161
                                                                2995682783
                                                                1620962684
                                                                3704723357
                                                                371613603
                                                                2814834333
                                                                2111005706
                                                                624778151
                                                                2094172212
                                                                4284947003
                                                                1211977835
                                                                991917094
                                                                1570449747
                                                                2962370480
                                                                1259410321
                                                                170182696
                                                                146300961
                                                                2836829791
                                                                619452428
                                                                2723670296
                                                                1881399711
                                                                1161269684
                                                                1675188680
                                                                4132175277
                                                                780088327
                                                                3409462821
                                                                1036518241
                                                                1834958505
                                                                3048448173
                                                                161811569
                                                                618488316
                                                                44795092
                                                                3918322701
                                                                1924681712
                                                                3239478144
                                                                383254043
                                                                4042306580
                                                                2146983041
                                                                3992780527
                                                                3518029708
                                                                3545545436
                                                                3901231469
                                                                1896136409
                                                                2028528556
                                                                2339662006
                                                                501326714
                                                                2060962201
                                                                2502746480
                                                                561575027
                                                                581893337
                                                                3393774360
                                                                1778912547
                                                                3626131687
                                                                2175155826
                                                                319853231
                                                                986875531
                                                                819755096
                                                                2915734330
                                                                2688355739
                                                                3482074849
                                                                2736559
                                                                2296975761
                                                                1029741190
                                                                2876812646
                                                                690154749
                                                                579200347
                                                                4027461746
                                                                1285330465
                                                                2701024045
                                                                4117700889
                                                                759495121
                                                                3332270341
                                                                2313004527
                                                                2277067795
                                                                4131855432
                                                                2722057515
                                                                1264804546
                                                                3848622725
                                                                2211267957
                                                                4100593547
                                                                959123777
                                                                2130745407
                                                                3194437393
                                                                486673947
                                                                1377371204
                                                                17472727
                                                                352317554
                                                                3955548058
                                                                159652094
                                                                1232063192
                                                                3835177280
                                                                49423123
                                                                3083993636
                                                                733092
                                                                2120519771
                                                                2573409834
                                                                1112952433
                                                                3239502554
                                                                761045320
                                                                1087580692
                                                                2540165110
                                                                641058802
                                                                1792435497
                                                                2261799288
                                                                1579184083
                                                                627146892
                                                                2165744623
                                                                2200142389
                                                                2167590760
                                                                2381418376
                                                                1793358889
                                                                3081659520
                                                                1663384067
                                                                2009658756
                                                                2689600308
                                                                739136266
                                                                2304581039
                                                                3529067263
                                                                591360555
                                                                525209271
                                                                3131882996
                                                                294230224
                                                                2076220115
                                                                3113580446
                                                                1245621585
                                                                1386885462
                                                                3203270426
                                                                123512128
                                                                12350217
                                                                354956375
                                                                4282398238
                                                                3356876605
                                                                3888857667
                                                                157639694
                                                                2616064085
                                                                1563068963
                                                                2762125883
                                                                4045394511
                                                                4180452559
                                                                3294769488
                                                                1684529556
                                                                1002945951
                                                                3181438866
                                                                22506664
                                                                691783457
                                                                2685221343
                                                                171579916
                                                                3878728600
                                                                2475806724
                                                                2030324028
                                                                3331164912
                                                                1708711359
                                                                1970023127
                                                                2859691344
                                                                2588476477
                                                                2748146879
                                                                136111222
                                                                2967685492
                                                                909517429
                                                                2835297809
                                                                3206906216
                                                                3186870716
                                                                341264097
                                                                2542035121
                                                                3353277068
                                                                548223577
                                                                3170936588
                                                                1678403446
                                                                297435620
                                                                2337555430
                                                                466603495
                                                                1132321815
                                                                1208589219
                                                                696392160
                                                                894244439
                                                                2562678859
                                                                470224582
                                                                3306867480
                                                                201364898
                                                                2075966438
                                                                1767227936
                                                                2929737987
                                                                3674877796
                                                                2654196643
                                                                3692734598
                                                                3528895099
                                                                2796780123
                                                                3048728353
                                                                842329300
                                                                191554730
                                                                2922459673
                                                                3489020079
                                                                3979110629
                                                                1022523848
                                                                2202932467
                                                                3583655201
                                                                3565113719
                                                                587085778
                                                                4176046313
                                                                3013713762
                                                                950944241
                                                                396426791
                                                                3784844662
                                                                3477431613
                                                                3594592395
                                                                2782043838
                                                                3392093507
                                                                3106564952
                                                                2829419931
                                                                1358665591
                                                                2206918825
                                                                3170783123
                                                                31522386
                                                                2988194168
                                                                1782249537
                                                                1105080928
                                                                843500134
                                                                1225290080
                                                                1521001832
                                                                3605886097
                                                                2802786495
                                                                2728923319
                                                                3996284304
                                                                903417639
                                                                1171249804
                                                                1020374987
                                                                2824535874
                                                                423621996
                                                                1988534473
                                                                2493544470
                                                                1008604435
                                                                1756003503
                                                                1488867287
                                                                1386808992
                                                                732088248
                                                                1780630732
                                                                2482101014
                                                                976561178
                                                                1543448953
                                                                2602866064
                                                                2021139923
                                                                1952599828
                                                                2360242564
                                                                2117959962
                                                                2753061860
                                                                2388623612
                                                                4138193781
                                                                2962920654
                                                                2284970429
                                                                766920861
                                                                3457264692
                                                                2879611383
                                                                815055854
                                                                2332929068
                                                                1254853997
                                                                3740375268
                                                                3799380844
                                                                4091048725
                                                                2006331129
                                                                1982546212
                                                                686850534
                                                                1907447564
                                                                2682801776
                                                                2780821066
                                                                998290361
                                                                1342433871
                                                                4195430425
                                                                607905174
                                                                3902331779
                                                                2454067926
                                                                1708133115
                                                                1170874362
                                                                2008609376
                                                                3260320415
                                                                2211196135
                                                                433538229
                                                                2728786374
                                                                2189520818
                                                                262554063
                                                                1182318347
                                                                3710237267
                                                                1221022450
                                                                715966018
                                                                2417068910
                                                                2591870721
                                                                2870691989
                                                                3418190842
                                                                4238214053
                                                                1540704231
                                                                1575580968
                                                                2095917976
                                                                4078310857
                                                                2313532447
                                                                2110690783
                                                                4056346629
                                                                4061784526
                                                                1123218514
                                                                551538993
                                                                597148360
                                                                4120175196
                                                                3581618160
                                                                3181170517
                                                                422862282
                                                                3227524138
                                                                1713114790
                                                                662317149
                                                                1230418732
                                                                928171837
                                                                1324564878
                                                                1928816105
                                                                1786535431
                                                                2878099422
                                                                3290185549
                                                                539474248
                                                                1657512683
                                                                552370646
                                                                1671741683
                                                                3655312128
                                                                1552739510
                                                                2605208763
                                                                1441755014
                                                                181878989
                                                                3124053868
                                                                1447103986
                                                                3183906156
                                                                1728556020
                                                                3502241336
                                                                3055466967
                                                                1013272474
                                                                818402132
                                                                1715099063
                                                                2900113506
                                                                397254517
                                                                4194863039
                                                                1009068739
                                                                232864647
                                                                2540223708
                                                                2608288560
                                                                2415367765
                                                                478404847
                                                                3455100648
                                                                3182600021
                                                                2115988978
                                                                434269567
                                                                4117179324
                                                                3461774077
                                                                887256537
                                                                3545801025
                                                                286388911
                                                                3451742129
                                                                1981164769
                                                                786667016
                                                                3310123729
                                                                3097811076
                                                                2224235657
                                                                2959658883
                                                                3370969234
                                                                2514770915
                                                                3345656436
                                                                2677010851
                                                                2206236470
                                                                271648054
                                                                2342188545
                                                                4292848611
                                                                3646533909
                                                                3754009956
                                                                3803931226
                                                                4160647125
                                                                1477814055
                                                                4043852216
                                                                1876372354
                                                                3133294443
                                                                3871104810
                                                                3177020907
                                                                2074304428
                                                                3479393793
                                                                759562891
                                                                164128153
                                                                1839069216
                                                                2114162633
                                                                3989947309
                                                                3611054956
                                                                1333547922
                                                                835429831
                                                                494987340
                                                                171987910
                                                                1252001001
                                                                370809172
                                                                3508925425
                                                                2535703112
                                                                1276855041
                                                                1922855120
                                                                835673414
                                                                3030664304
                                                                613287117
                                                                171219893
                                                                3423096126
                                                                3376881639
                                                                2287770315
                                                                1658692645
                                                                1262815245
                                                                3957234326
                                                                1168096164
                                                                2968737525
                                                                2655813712
                                                                2132313144
                                                                3976047964
                                                                326516571
                                                                353088456
                                                                3679188938
                                                                3205649712
                                                                2654036126
                                                                1249024881
                                                                880166166
                                                                691800469
                                                                2229503665
                                                                1673458056
                                                                4032208375
                                                                1851778863
                                                                2563757330
                                                                376742205
                                                                1794655231
                                                                340247333
                                                                1505873033
                                                                396524441
                                                                879666767
                                                                3335579166
                                                                3260764261
                                                                3335999539
                                                                506221798
                                                                4214658741
                                                                975887814
                                                                2080536343
                                                                3360539560
                                                                571586418
                                                                138896374
                                                                4234352651
                                                                2737620262
                                                                3928362291
                                                                1516365296
                                                                38056726
                                                                3599462320
                                                                3585007266
                                                                3850961033
                                                                471667319
                                                                1536883193
                                                                2310166751
                                                                1861637689
                                                                2530999841
                                                                4139843801
                                                                2710569485
                                                                827578615
                                                                2012334720
                                                                2907369459
                                                                3029312804
                                                                2820112398
                                                                1965028045
                                                                35518606
                                                                2478379033
                                                                643747771
                                                                1924139484
                                                                4123405127
                                                                3811735531
                                                                3429660832
                                                                3285177704
                                                                1948416081
                                                                1311525291
                                                                1183517742
                                                                1739192232
                                                                3979815115
                                                                2567840007
                                                                4116821529
                                                                213304419
                                                                4125718577
                                                                1473064925
                                                                2442436592
                                                                1893310111
                                                                4195361916
                                                                3747569474
                                                                828465101
                                                                2991227658
                                                                750582866
                                                                1205170309
                                                                1409813056
                                                                678418130
                                                                1171531016
                                                                3821236156
                                                                354504587
                                                                4202874632
                                                                3882511497
                                                                1893248677
                                                                1903078632
                                                                26340130
                                                                2069166240
                                                                3657122492
                                                                3725758099
                                                                831344905
                                                                811453383
                                                                3447711422
                                                                2434543565
                                                                4166886888
                                                                3358210805
                                                                4142984013
                                                                2988152326
                                                                3527824853
                                                                982082992
                                                                2809155763
                                                                190157081
                                                                3340214818
                                                                2365432395
                                                                2548636180
                                                                2894533366
                                                                3474657421
                                                                2372634704
                                                                2845748389
                                                                43024175
                                                                2774226648
                                                                1987702864
                                                                3186502468
                                                                453610222
                                                                4204736567
                                                                1392892630
                                                                2471323686
                                                                2470534280
                                                                3541393095
                                                                4269885866
                                                                3909911300
                                                                759132955
                                                                1482612480
                                                                667715263
                                                                1795580598
                                                                2337923983
                                                                3390586366
                                                                581426223
                                                                1515718634
                                                                476374295
                                                                705213300
                                                                363062054
                                                                2084697697
                                                                2407503428
                                                                2292957699
                                                                2426213835
                                                                2199989172
                                                                1987356470
                                                                4026755612
                                                                2147252133
                                                                270400031
                                                                1367820199
                                                                2369854699
                                                                2844269403
                                                                79981964))))
(:DEFPARAMETER COMMON-LISP:*PRINT-LINES* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*QUERY-IO*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL COMMON-LISP:*TERMINAL-IO* {10001C8053}>)
(:DEFPARAMETER COMMON-LISP:*MODULES* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:SHORT-FLOAT-NEGATIVE-EPSILON 2.9802326e-8)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT -1.1754944e-38)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-SINGLE-FLOAT 3.4028235e38)
(:DEFCONSTANT COMMON-LISP:DOUBLE-FLOAT-NEGATIVE-EPSILON 5.551115123125784d-17)
(:DEFCONSTANT COMMON-LISP:BOOLE-AND 6)
(:DEFCONSTANT COMMON-LISP:BOOLE-CLR 0)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT -1.1754944e-38)
(:DEFPARAMETER COMMON-LISP:*STANDARD-OUTPUT*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL SB-SYS:*STDOUT* {10001D0A53}>)
(:DEFCONSTANT COMMON-LISP:LAMBDA-LIST-KEYWORDS
 (COMMON-LISP:&ALLOW-OTHER-KEYS COMMON-LISP:&AUX COMMON-LISP:&BODY
  COMMON-LISP:&ENVIRONMENT COMMON-LISP:&KEY SB-INT:&MORE COMMON-LISP:&OPTIONAL
  COMMON-LISP:&REST COMMON-LISP:&WHOLE))
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-DOUBLE-FLOAT -1.7976931348623157d308)
(:DEFPARAMETER COMMON-LISP:- (COMMON-LISP-USER::ALLPACKAGEVALUES))
(:DEFPARAMETER COMMON-LISP:*LOAD-PATHNAME* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*READ-SUPPRESS* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:CALL-ARGUMENTS-LIMIT 4611686018427387903)
(:DEFPARAMETER COMMON-LISP:*PRINT-RIGHT-MARGIN* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*ERROR-OUTPUT*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL SB-SYS:*STDERR* {1000156B73}>)
(:DEFCONSTANT COMMON-LISP:BOOLE-ANDC2 13)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-DOUBLE-FLOAT -4.9406564584124654d-324)
(:DEFCONSTANT COMMON-LISP:BOOLE-2 3)
(:DEFPARAMETER COMMON-LISP:*GENSYM-COUNTER* 390)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-SHORT-FLOAT 1.4012985e-45)
(:DEFPARAMETER COMMON-LISP:*READ-BASE* 10)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT
 -2.2250738585072014d-308)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT 1.1754944e-38)
(:DEFPARAMETER COMMON-LISP:*PRINT-ESCAPE* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:SINGLE-FLOAT-EPSILON 5.960465e-8)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT 1.1754944e-38)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT
 -2.2250738585072014d-308)
(:DEFCONSTANT COMMON-LISP:BOOLE-1 2)
(:DEFPARAMETER COMMON-LISP:*PRINT-CIRCLE* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*READ-EVAL* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:BOOLE-ORC1 14)
(:DEFCONSTANT COMMON-LISP:LAMBDA-PARAMETERS-LIMIT 4611686018427387903)
(:DEFPARAMETER COMMON-LISP:*TRACE-OUTPUT*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL SB-SYS:*STDOUT* {10001D0A53}>)
(:DEFPARAMETER COMMON-LISP:*LOAD-VERBOSE* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*TERMINAL-IO*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL SB-SYS:*TTY* {10001D06C3}>)
(:DEFPARAMETER COMMON-LISP:*MACROEXPAND-HOOK* COMMON-LISP:FUNCALL)
(:DEFPARAMETER COMMON-LISP:*PRINT-MISER-WIDTH* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*COMPILE-VERBOSE* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-SINGLE-FLOAT 1.4012985e-45)
(:DEFPARAMETER COMMON-LISP:*READTABLE* #<COMMON-LISP:READTABLE {10001CF9B3}>)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-FIXNUM -4611686018427387904)
(:DEFPARAMETER COMMON-LISP:*PRINT-BASE* 10)
(:DEFPARAMETER COMMON-LISP:*STANDARD-INPUT*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL SB-SYS:*STDIN* {10001CEF83}>)
(:DEFPARAMETER COMMON-LISP:++
 (COMMON-LISP:DEFUN COMMON-LISP-USER::MAYBE-PPRINT (COMMON-LISP-USER::O)
   (COMMON-LISP:HANDLER-CASE (COMMON-LISP:WRITE-TO-STRING COMMON-LISP-USER::O)
                             (COMMON-LISP:T (COMMON-LISP-USER::A)
                              (COMMON-LISP:LET ((COMMON-LISP:*PRINT-READABLY*
                                                 COMMON-LISP:NIL))
                                (COMMON-LISP:WRITE-TO-STRING
                                 COMMON-LISP-USER::O))))))
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-DOUBLE-FLOAT 1.7976931348623157d308)
(:DEFCONSTANT COMMON-LISP:BOOLE-ORC2 15)
(:DEFPARAMETER COMMON-LISP:*PRINT-GENSYM* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:LONG-FLOAT-EPSILON 1.1102230246251568d-16)
(:DEFPARAMETER COMMON-LISP:*BREAK-ON-SIGNALS* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:BOOLE-NAND 10)
(:DEFPARAMETER COMMON-LISP:*** COMMON-LISP-USER::ALLPACKAGEVALUES)
(:DEFCONSTANT COMMON-LISP:BOOLE-ANDC1 12)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-SHORT-FLOAT -1.4012985e-45)
(:DEFCONSTANT COMMON-LISP:BOOLE-C2 5)
(:DEFPARAMETER COMMON-LISP:* COMMON-LISP-USER::PACKAGE-VALUES)
(:DEFCONSTANT COMMON-LISP:BOOLE-XOR 8)
(:DEFPARAMETER COMMON-LISP:*PRINT-PRETTY* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:BOOLE-IOR 7)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-LONG-FLOAT 1.7976931348623157d308)
(:DEFPARAMETER COMMON-LISP:*PRINT-PPRINT-DISPATCH*
 #<SB-PRETTY:PPRINT-DISPATCH-TABLE {10001CBC63}>)
(:DEFCONSTANT COMMON-LISP:BOOLE-NOR 11)
(:DEFPARAMETER COMMON-LISP:*PRINT-CASE* :UPCASE)
(:DEFPARAMETER COMMON-LISP:*PRINT-READABLY* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*DEBUGGER-HOOK* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:INTERNAL-TIME-UNITS-PER-SECOND 1000)
(:DEFPARAMETER COMMON-LISP:+
 (COMMON-LISP:DEFUN COMMON-LISP-USER::PACKAGE-VALUES (COMMON-LISP-USER::PS)
   (COMMON-LISP:LET ((COMMON-LISP-USER::P
                      (COMMON-LISP:FIND-PACKAGE COMMON-LISP-USER::PS))
                     (COMMON-LISP-USER::KWP
                      (COMMON-LISP:OR (COMMON-LISP:FIND-PACKAGE "TMP")
                                      (COMMON-LISP:MAKE-PACKAGE "TMP" :USE
                                                                COMMON-LISP:NIL))))
     (COMMON-LISP:DO-ALL-SYMBOLS (COMMON-LISP-USER::SYM)
       (COMMON-LISP:WHEN
           (COMMON-LISP:AND
            (COMMON-LISP:EQ (COMMON-LISP:SYMBOL-PACKAGE COMMON-LISP-USER::SYM)
                            COMMON-LISP-USER::P)
            (COMMON-LISP:BOUNDP COMMON-LISP-USER::SYM))
         (COMMON-LISP:LET ((COMMON-LISP:*PACKAGE* COMMON-LISP-USER::KWP)
                           (COMMON-LISP:*PRINT-ESCAPE* COMMON-LISP:T)
                           (COMMON-LISP:*PRINT-LINES* COMMON-LISP:NIL)
                           (COMMON-LISP:*PRINT-ARRAY* COMMON-LISP:T)
                           (COMMON-LISP:*PRINT-GENSYM* COMMON-LISP:T)
                           (COMMON-LISP:*PRINT-CIRCLE* COMMON-LISP:T)
                           (COMMON-LISP:*PRINT-READABLY* COMMON-LISP:T))
           (COMMON-LISP:FRESH-LINE)
           (COMMON-LISP:FRESH-LINE)
           (COMMON-LISP:PRINC
            (COMMON-LISP-USER::MAYBE-PPRINT
             `(,(COMMON-LISP:IF (COMMON-LISP:CONSTANTP COMMON-LISP-USER::SYM)
                                :DEFCONSTANT
                                :DEFPARAMETER)
               ,COMMON-LISP-USER::SYM
               ,(COMMON-LISP:SYMBOL-VALUE COMMON-LISP-USER::SYM))))))))))
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-DOUBLE-FLOAT 4.9406564584124654d-324)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-LONG-FLOAT -1.7976931348623157d308)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT
 2.2250738585072014d-308)
(:DEFCONSTANT COMMON-LISP:DOUBLE-FLOAT-EPSILON 1.1102230246251568d-16)
(:DEFCONSTANT COMMON-LISP:SHORT-FLOAT-EPSILON 5.960465e-8)
(:DEFPARAMETER COMMON-LISP:*COMPILE-PRINT* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*PACKAGE* #<COMMON-LISP:PACKAGE "TMP">)
(:DEFCONSTANT COMMON-LISP:NIL COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*LOAD-PRINT* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*COMPILE-FILE-TRUENAME* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:PI 3.141592653589793d0)
(:DEFPARAMETER COMMON-LISP:*LOAD-TRUENAME* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:SINGLE-FLOAT-NEGATIVE-EPSILON 2.9802326e-8)
(:DEFCONSTANT COMMON-LISP:ARRAY-DIMENSION-LIMIT 4611686018427387901)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-SINGLE-FLOAT -1.4012985e-45)
(:DEFPARAMETER COMMON-LISP:*PRINT-LEVEL* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*DEBUG-IO*
 #<COMMON-LISP:SYNONYM-STREAM :SYMBOL COMMON-LISP:*TERMINAL-IO* {10001C8053}>)
(:DEFCONSTANT COMMON-LISP:ARRAY-TOTAL-SIZE-LIMIT 4611686018427387901)
(:DEFPARAMETER COMMON-LISP:*COMPILE-FILE-PATHNAME* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*PRINT-RADIX* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-LONG-FLOAT
 2.2250738585072014d-308)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-FIXNUM 4611686018427387903)
(:DEFPARAMETER COMMON-LISP:*PRINT-ARRAY* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:T COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS*
 #P"/home/dmiles/logicmoo_workspace/packs_usr/armedbear_abcl/")
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-LONG-FLOAT 4.9406564584124654d-324)
(:DEFCONSTANT COMMON-LISP:ARRAY-RANK-LIMIT 65529)
(:DEFCONSTANT COMMON-LISP:BOOLE-C1 4)
(:DEFPARAMETER COMMON-LISP:*FEATURES*
 (:64-BIT :64-BIT-REGISTERS :ALIEN-CALLBACKS :ANSI-CL :ASH-RIGHT-VOPS
  :C-STACK-IS-CONTROL-STACK :COMMON-LISP :COMPARE-AND-SWAP-VOPS
  :COMPLEX-FLOAT-VOPS :CYCLE-COUNTER :ELF :FLOAT-EQL-VOPS
  :FP-AND-PC-STANDARD-SAVE :GENCGC :IEEE-FLOATING-POINT :INLINE-CONSTANTS
  :INTEGER-EQL-VOP :INTERLEAVED-RAW-SLOTS :LARGEFILE :LINKAGE-TABLE :LINUX
  :LITTLE-ENDIAN :MEMORY-BARRIER-VOPS :MULTIPLY-HIGH-VOPS
  :OS-PROVIDES-BLKSIZE-T :OS-PROVIDES-DLADDR :OS-PROVIDES-DLOPEN
  :OS-PROVIDES-GETPROTOBY-R :OS-PROVIDES-POLL :OS-PROVIDES-PUTWC
  :OS-PROVIDES-SUSECONDS-T :PACKAGE-LOCAL-NICKNAMES :PRECISE-ARG-COUNT-ERROR
  :RAW-INSTANCE-INIT-VOPS :SB-AFTER-XC-CORE :SB-CORE-COMPRESSION :SB-DOC
  :SB-EVAL :SB-FUTEX :SB-LDB :SB-PACKAGE-LOCKS :SB-SIMD-PACK
  :SB-SOURCE-LOCATIONS :SB-TEST :SB-THREAD :SB-UNICODE :SB-XREF-FOR-INTERNALS
  :SBCL :STACK-ALLOCATABLE-CLOSURES :STACK-ALLOCATABLE-FIXED-OBJECTS
  :STACK-ALLOCATABLE-LISTS :STACK-ALLOCATABLE-VECTORS
  :STACK-GROWS-DOWNWARD-NOT-UPWARD :SYMBOL-INFO-VOPS :UNIX
  :UNWIND-TO-FRAME-AND-CALL-VOP :X86-64))
(:DEFCONSTANT COMMON-LISP:LONG-FLOAT-NEGATIVE-EPSILON 5.551115123125784d-17)
(:DEFCONSTANT COMMON-LISP:BOOLE-SET 1)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-SHORT-FLOAT -3.4028235e38)
(:DEFPARAMETER COMMON-LISP:/// (COMMON-LISP-USER::ALLPACKAGEVALUES))
(:DEFPARAMETER COMMON-LISP:*READ-DEFAULT-FLOAT-FORMAT* COMMON-LISP:SINGLE-FLOAT)
(:DEFPARAMETER COMMON-LISP:*PRINT-LENGTH* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:/ (COMMON-LISP-USER::PACKAGE-VALUES))
(:DEFPARAMETER COMMON-LISP:+++
 (COMMON-LISP:DEFUN COMMON-LISP-USER::ALLPACKAGEVALUES ()
   (COMMON-LISP:DOLIST
       (COMMON-LISP-USER::P (COMMON-LISP:LIST :CL :USER :SYS :EXT :CLOS))
     (COMMON-LISP-USER::PACKAGE-VALUES COMMON-LISP-USER::P))))
(:DEFCONSTANT COMMON-LISP:CHAR-CODE-LIMIT 1114112)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-LONG-FLOAT -4.9406564584124654d-324)
(:DEFPARAMETER COMMON-LISP:// (COMMON-LISP-USER::MAYBE-PPRINT))
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-SINGLE-FLOAT -3.4028235e38)
(:DEFCONSTANT COMMON-LISP:MULTIPLE-VALUES-LIMIT 4611686018427387903)
(:DEFCONSTANT COMMON-LISP:BOOLE-EQV 9)
(:DEFPARAMETER COMMON-LISP:* COMMON-LISP-USER::PACKAGE-VALUES)
(:DEFCONSTANT COMMON-LISP:T COMMON-LISP:T)


Welcome to Clozure Common Lisp Version 1.11-r16635  (LinuxX8664)!

CCL is developed and maintained by Clozure Associates. For more information
about CCL visit http://ccl.clozure.com.  To enquire about Clozure's Common Lisp
consulting services e-mail info@clozure.com or visit http://www.clozure.com.

? (defun allpackagevalues () (dolist (p (list :cl :user :sys :EXT :clos)) (package-values p)))
(defun maybe-pprint (o) (handler-case (write-to-string o) (t (a)  (let ((*print-readably* nil)) (write-to-string o)))))
(defun package-values (ps)
  (let ((p (find-package ps))(kwp (or (find-package "TMP") (make-package "TMP" :use ()))))
    (do-all-symbols (sym)
     (when (and (eq (symbol-package sym) p) (boundp sym))
      (let((*package* kwp) (*print-escape* t) (*PRINT-LINES* nil)(*PRINT-ARRAY* t)(*PRINT-GENSYM* t)(*PRINT-CIRCLE* t)
           (*print-readably* t))(fresh-line)(fresh-line)
         (princ (maybe-pprint `(,(if(constantp sym) :defconstant :defparameter) ,sym ,(symbol-value sym)))))))))
(allpackagevalues)
;Compiler warnings :
;   In ALLPACKAGEVALUES: Undefined function PACKAGE-VALUES
ALLPACKAGEVALUES
? ;Compiler warnings :
;   In MAYBE-PPRINT: Unused lexical variable A
MAYBE-PPRINT
? PACKAGE-VALUES
? (:DEFCONSTANT COMMON-LISP:T COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:NIL COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*LOAD-TRUENAME* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:+ (COMMON-LISP:DEFUN COMMON-LISP-USER::PACKAGE-VALUES (COMMON-LISP-USER::PS) (COMMON-LISP:LET ((COMMON-LISP-USER::P (COMMON-LISP:FIND-PACKAGE COMMON-LISP-USER::PS)) (COMMON-LISP-USER::KWP (COMMON-LISP:OR (COMMON-LISP:FIND-PACKAGE "TMP") (COMMON-LISP:MAKE-PACKAGE "TMP" :USE COMMON-LISP:NIL)))) (COMMON-LISP:DO-ALL-SYMBOLS (COMMON-LISP-USER::SYM) (COMMON-LISP:WHEN (COMMON-LISP:AND (COMMON-LISP:EQ (COMMON-LISP:SYMBOL-PACKAGE COMMON-LISP-USER::SYM) COMMON-LISP-USER::P) (COMMON-LISP:BOUNDP COMMON-LISP-USER::SYM)) (COMMON-LISP:LET ((COMMON-LISP:*PACKAGE* COMMON-LISP-USER::KWP) (COMMON-LISP:*PRINT-ESCAPE* COMMON-LISP:T) (COMMON-LISP:*PRINT-LINES* COMMON-LISP:NIL) (COMMON-LISP:*PRINT-ARRAY* COMMON-LISP:T) (COMMON-LISP:*PRINT-GENSYM* COMMON-LISP:T) (COMMON-LISP:*PRINT-CIRCLE* COMMON-LISP:T) (COMMON-LISP:*PRINT-READABLY* COMMON-LISP:T)) (COMMON-LISP:FRESH-LINE) (COMMON-LISP:FRESH-LINE) (COMMON-LISP:PRINC (COMMON-LISP-USER::MAYBE-PPRINT (COMMON-LISP:LIST* (COMMON-LISP:IF (COMMON-LISP:CONSTANTP COMMON-LISP-USER::SYM) :DEFCONSTANT :DEFPARAMETER) (COMMON-LISP:LIST* COMMON-LISP-USER::SYM (COMMON-LISP:LIST (COMMON-LISP:SYMBOL-VALUE COMMON-LISP-USER::SYM))))))))))))
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-SINGLE-FLOAT -3.4028235E+38)
(:DEFPARAMETER COMMON-LISP:*LOAD-VERBOSE* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-LONG-FLOAT 5.0D-324)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-FIXNUM 1152921504606846975)
(:DEFCONSTANT COMMON-LISP:BOOLE-ANDC1 12)
(:DEFCONSTANT COMMON-LISP:LONG-FLOAT-NEGATIVE-EPSILON 5.551115123125784D-17)
(:DEFCONSTANT COMMON-LISP:T COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*** COMMON-LISP-USER::ALLPACKAGEVALUES)
(:DEFCONSTANT COMMON-LISP:LAMBDA-PARAMETERS-LIMIT 4096)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT 1.1754945E-38)
(:DEFCONSTANT COMMON-LISP:SHORT-FLOAT-EPSILON 5.960465E-8)
(:DEFPARAMETER COMMON-LISP:*PRINT-LINES* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*COMPILE-FILE-PATHNAME* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:CHAR-CODE-LIMIT 1114112)
(:DEFCONSTANT COMMON-LISP:BOOLE-SET 1)
(:DEFPARAMETER COMMON-LISP:*COMPILE-FILE-TRUENAME* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*READ-SUPPRESS* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT 1.1754945E-38)
(:DEFPARAMETER COMMON-LISP:*PRINT-CASE* :UPCASE)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-LONG-FLOAT 1.7976931348623157D+308)
(:DEFPARAMETER COMMON-LISP:*READ-DEFAULT-FLOAT-FORMAT* COMMON-LISP:SINGLE-FLOAT)
(:DEFCONSTANT COMMON-LISP:BOOLE-ANDC2 13)
(:DEFPARAMETER COMMON-LISP:*PRINT-BASE* 10)
(:DEFCONSTANT COMMON-LISP:DOUBLE-FLOAT-NEGATIVE-EPSILON 5.551115123125784D-17)
(:DEFPARAMETER COMMON-LISP:*COMPILE-PRINT* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:SINGLE-FLOAT-EPSILON 5.960465E-8)
(:DEFCONSTANT COMMON-LISP:SHORT-FLOAT-NEGATIVE-EPSILON 2.9802326E-8)
(:DEFCONSTANT COMMON-LISP:BOOLE-CLR 0)
(:DEFPARAMETER COMMON-LISP:- (COMMON-LISP-USER::ALLPACKAGEVALUES))
(:DEFPARAMETER COMMON-LISP:*PRINT-ARRAY* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:*LOAD-PRINT* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:BOOLE-C1 4)
(:DEFPARAMETER COMMON-LISP:*PRINT-PPRINT-DISPATCH* #<PPRINT-DISPATCH-TABLE #x3020004BFF7D>)
(:DEFPARAMETER COMMON-LISP:*GENSYM-COUNTER* 20)
(:DEFPARAMETER COMMON-LISP:*LOAD-PATHNAME* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:ARRAY-TOTAL-SIZE-LIMIT 72057594037927936)
(:DEFPARAMETER COMMON-LISP:*PRINT-PRETTY* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*COMPILE-VERBOSE* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:* COMMON-LISP-USER::PACKAGE-VALUES)
(:DEFPARAMETER COMMON-LISP:*DEBUGGER-HOOK* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-DOUBLE-FLOAT -1.7976931348623157D+308)
(:DEFCONSTANT COMMON-LISP:LAMBDA-LIST-KEYWORDS (COMMON-LISP:&OPTIONAL COMMON-LISP:&REST COMMON-LISP:&AUX COMMON-LISP:&KEY COMMON-LISP:&ALLOW-OTHER-KEYS COMMON-LISP:&BODY COMMON-LISP:&ENVIRONMENT COMMON-LISP:&WHOLE))
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT -1.1754945E-38)
(:DEFPARAMETER COMMON-LISP:+++ (COMMON-LISP:DEFUN COMMON-LISP-USER::ALLPACKAGEVALUES COMMON-LISP:NIL (COMMON-LISP:DOLIST (COMMON-LISP-USER::P (COMMON-LISP:LIST :CL :USER :SYS :EXT :CLOS)) (COMMON-LISP-USER::PACKAGE-VALUES COMMON-LISP-USER::P))))
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-SINGLE-FLOAT 3.4028235E+38)
(:DEFCONSTANT COMMON-LISP:BOOLE-AND 6)
(:DEFPARAMETER COMMON-LISP:*TRACE-OUTPUT* #<SYNONYM-STREAM to COMMON-LISP:*TERMINAL-IO* #x30200049B4CD>)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-LONG-FLOAT -5.0D-324)
(:DEFPARAMETER COMMON-LISP:/ (COMMON-LISP-USER::PACKAGE-VALUES))
(:DEFPARAMETER COMMON-LISP:*PACKAGE* #<Package "TMP">)
(:DEFCONSTANT COMMON-LISP:BOOLE-ORC2 15)
(:DEFPARAMETER COMMON-LISP:++ (COMMON-LISP:DEFUN COMMON-LISP-USER::MAYBE-PPRINT (COMMON-LISP-USER::O) (COMMON-LISP:HANDLER-CASE (COMMON-LISP:WRITE-TO-STRING COMMON-LISP-USER::O) (COMMON-LISP:T (COMMON-LISP-USER::A) (COMMON-LISP:LET ((COMMON-LISP:*PRINT-READABLY* COMMON-LISP:NIL)) (COMMON-LISP:WRITE-TO-STRING COMMON-LISP-USER::O))))))
(:DEFPARAMETER COMMON-LISP:*PRINT-MISER-WIDTH* 40)
(:DEFCONSTANT COMMON-LISP:MULTIPLE-VALUES-LIMIT 200)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-SHORT-FLOAT -3.4028235E+38)
(:DEFPARAMETER COMMON-LISP:// (COMMON-LISP-USER::MAYBE-PPRINT))
(:DEFPARAMETER COMMON-LISP:*TERMINAL-IO* #<ECHOING-TWO-WAY-STREAM input #<BASIC-CHARACTER-INPUT-STREAM UTF-8 (TTY/0) #x30200049DC8D>, output #<BASIC-CHARACTER-OUTPUT-STREAM UTF-8 (TTY/1) #x30200049D2FD> #x3020004BF71D>)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-SINGLE-FLOAT -1.4012985E-45)
(:DEFPARAMETER COMMON-LISP:*PRINT-RADIX* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:BOOLE-NAND 10)
(:DEFCONSTANT COMMON-LISP:BOOLE-NOR 11)
(:DEFPARAMETER COMMON-LISP:*PRINT-CIRCLE* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:ARRAY-DIMENSION-LIMIT 72057594037927936)
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-SHORT-FLOAT 3.4028235E+38)
(:DEFCONSTANT COMMON-LISP:CALL-ARGUMENTS-LIMIT 65536)
(:DEFPARAMETER COMMON-LISP:*STANDARD-OUTPUT* #<SYNONYM-STREAM to COMMON-LISP:*TERMINAL-IO* #x30200049B4CD>)
(:DEFPARAMETER COMMON-LISP:*STANDARD-INPUT* #<SYNONYM-STREAM to COMMON-LISP:*TERMINAL-IO* #x30200049B87D>)
(:DEFPARAMETER COMMON-LISP:*QUERY-IO* #<SYNONYM-STREAM to COMMON-LISP:*TERMINAL-IO* #x30200049B3FD>)
(:DEFPARAMETER COMMON-LISP:*PRINT-LEVEL* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:BOOLE-ORC1 14)
(:DEFCONSTANT COMMON-LISP:PI 3.141592653589793D0)
(:DEFCONSTANT COMMON-LISP:BOOLE-2 3)
(:DEFPARAMETER COMMON-LISP:*FEATURES* (:PRIMARY-CLASSES :COMMON-LISP :OPENMCL :CCL :CCL-1.2 :CCL-1.3 :CCL-1.4 :CCL-1.5 :CCL-1.6 :CCL-1.7 :CCL-1.8 :CCL-1.9 :CCL-1.10 :CCL-1.11 :CLOZURE :CLOZURE-COMMON-LISP :ANSI-CL :UNIX :OPENMCL-UNICODE-STRINGS :IPV6 :OPENMCL-NATIVE-THREADS :OPENMCL-PARTIAL-MOP :MCL-COMMON-MOP-SUBSET :OPENMCL-MOP-2 :OPENMCL-PRIVATE-HASH-TABLES :STATIC-CONSES-SHOULD-WORK-WITH-EGC-IN-CCL :X86-64 :X86_64 :X86-TARGET :X86-HOST :X8664-TARGET :X8664-HOST :LINUX-HOST :LINUX-TARGET :LINUXX86-TARGET :LINUXX8664-TARGET :LINUXX8664-HOST :64-BIT-TARGET :64-BIT-HOST :LINUX :LITTLE-ENDIAN-TARGET :LITTLE-ENDIAN-HOST))
(:DEFPARAMETER COMMON-LISP:*PRINT-READABLY* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:ARRAY-RANK-LIMIT 4096)
(:DEFPARAMETER COMMON-LISP:*BREAK-ON-SIGNALS* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT -1.1754945E-38)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-FIXNUM -1152921504606846976)
(:DEFPARAMETER COMMON-LISP:*ERROR-OUTPUT* #<SYNONYM-STREAM to COMMON-LISP:*TERMINAL-IO* #x30200049B43D>)
(:DEFPARAMETER COMMON-LISP:*READTABLE* #<COMMON-LISP:READTABLE #x302000305D5D>)
(:DEFCONSTANT COMMON-LISP:INTERNAL-TIME-UNITS-PER-SECOND 1000000)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-SHORT-FLOAT 1.4012985E-45)
(:DEFPARAMETER COMMON-LISP:*RANDOM-STATE* #.(CCL::INITIALIZE-MRG31K3P-STATE 314159 42 1776 271828 6021023 1066))
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT 2.2250738585072014D-308)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-DOUBLE-FLOAT 5.0D-324)
(:DEFCONSTANT COMMON-LISP:SINGLE-FLOAT-NEGATIVE-EPSILON 2.9802326E-8)
(:DEFPARAMETER COMMON-LISP:*MACROEXPAND-HOOK* COMMON-LISP:FUNCALL)
(:DEFCONSTANT COMMON-LISP:BOOLE-EQV 9)
(:DEFPARAMETER COMMON-LISP:*DEFAULT-PATHNAME-DEFAULTS* #P"")
(:DEFCONSTANT COMMON-LISP:MOST-POSITIVE-DOUBLE-FLOAT 1.7976931348623157D+308)
(:DEFPARAMETER COMMON-LISP:*PRINT-GENSYM* COMMON-LISP:T)
(:DEFCONSTANT COMMON-LISP:DOUBLE-FLOAT-EPSILON 1.1102230246251568D-16)
(:DEFPARAMETER COMMON-LISP:*PRINT-RIGHT-MARGIN* COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*PRINT-LENGTH* COMMON-LISP:NIL)
(:DEFCONSTANT COMMON-LISP:MOST-NEGATIVE-LONG-FLOAT -1.7976931348623157D+308)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT -2.2250738585072014D-308)
(:DEFPARAMETER COMMON-LISP:*READ-BASE* 10)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-NORMALIZED-LONG-FLOAT 2.2250738585072014D-308)
(:DEFPARAMETER COMMON-LISP:/// (COMMON-LISP-USER::ALLPACKAGEVALUES))
(:DEFCONSTANT COMMON-LISP:BOOLE-C2 5)
(:DEFPARAMETER COMMON-LISP:*PRINT-ESCAPE* COMMON-LISP:T)
(:DEFPARAMETER COMMON-LISP:** COMMON-LISP-USER::MAYBE-PPRINT)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-SHORT-FLOAT -1.4012985E-45)
(:DEFCONSTANT COMMON-LISP:LONG-FLOAT-EPSILON 1.1102230246251568D-16)
(:DEFPARAMETER COMMON-LISP:*DEBUG-IO* #<SYNONYM-STREAM to COMMON-LISP:*TERMINAL-IO* #x30200049B3FD>)
(:DEFCONSTANT COMMON-LISP:BOOLE-XOR 8)
(:DEFPARAMETER COMMON-LISP:*MODULES* ("PREPARE-MCL-ENVIRONMENT" "SOCKETS" "LISPEQU" "CN-ENCODE" "JP-ENCODE" "VERSION" "CCL-EXPORT-SYMS" "LOOP" "MCL-COMPAT" "REMOTE-LISP" "SWANK-LOADER" "DOMINANCE" "CORE-FILES" "LEAKS" "COVER" "SWINK" "DESCRIBE" "EDIT-CALLERS" "ARGLIST" "COMPILE-CCL" "SYSTEMS" "TIME" "PATHNAMES" "DUMPLISP" "PPRINT" "MISC" "METHOD-COMBINATION" "ENCAPSULATE" "CASE-ERROR" "DB-IO" "FFI-LINUXX8664" "FOREIGN-TYPES" "X86-LAPMACROS" "X86-DISASSEMBLE" "NXENV" "SOURCE-FILES" "APROPOS" "ARRAYS-FRY" "READ" "BACKTRACE" "X86-BACKTRACE" "BACKTRACE-LDS" "BACKQUOTE" "NFCOMP" "DEFSTRUCT-LDS" "DEFSTRUCT-MACROS" "OPTIMIZERS" "STREAMS" "FORMAT" "SETF-RUNTIME" "SETF" "MACROS" "LEVEL-2" "X86-BACKEND" "X8664-BACKEND" "X8664-VINSNS" "NX" "HASH" "ACODE-REWRITE" "X862" "NX2" "REG" "VINSN" "BACKEND" "VREG" "X8664-ARCH" "X8632-ARCH" "X86-LAP" "X86-ASM" "X86-ARCH" "ARCH" "SUBPRIMS" "NUMBERS" "SORT" "SEQUENCES" "CHARS" "DLL-NODE" "DEFSTRUCT" "LISTS"))
(:DEFCONSTANT COMMON-LISP:BOOLE-1 2)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-DOUBLE-FLOAT -5.0D-324)
(:DEFCONSTANT COMMON-LISP:LEAST-POSITIVE-SINGLE-FLOAT 1.4012985E-45)
(:DEFCONSTANT COMMON-LISP:BOOLE-IOR 7)
(:DEFCONSTANT COMMON-LISP:LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT -2.2250738585072014D-308)
(:DEFCONSTANT COMMON-LISP:NIL COMMON-LISP:NIL)
(:DEFPARAMETER COMMON-LISP:*READ-EVAL* COMMON-LISP:T)

