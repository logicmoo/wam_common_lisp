
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


#-sbcl
(defun psyminfo ()
 (dolist (p (list-all-packages))
  (let (( pn (string-downcase (package-name p))))
   (format t "package_nickname('~a','~a').~%" pn pn)
   (dolist (nn (package-nicknames p))
     (format t "package_nickname('~a','~a').~%" pn (string-downcase nn)))
   (dolist (nn (package-use-list p))
     (format t "package_use_list('~a','~a').~%" pn (string-downcase (package-name nn))))
   (dolist (nn (package-shadowing-symbols p))
     (format t "package_shadowing_symbols('~a','~a').~%" pn nn))
 (when (and (not (string= "sb-c" pn))(not (string= "sb-vm" pn))(not (string= "java" pn)))
 (do-all-symbols (sym)    
  (when (eq (symbol-package sym) p)   
    (let (( sn (string-downcase (symbol-name sym))))
     
     (format t "symbol_info('~a','~a',package,~(~a~)).~%" sn pn (second (multiple-value-list  (find-symbol (symbol-name sym) p))))
    (when (not (string= "keyword" pn))
      (when (boundp sym) 
       (handler-case  
        (progn (format t "symbol_info('~a','~a',value,'~a',~a).~%" sn pn (symbol-value sym) (if (constantp sym) "constant" "variable"))
      (let ((doc (documentation sym 'variable))) (when doc (format t "doc_string('~a','~a',variable,\"~a\").~%" sn pn (princ-to-string doc)))))))
     (when (fboundp sym) 
      (let* ((sf (symbol-function sym)) (sft (string-downcase (symbol-name (type-of sf)))))
       (when (special-operator-p sym)
         (setq sft (concatenate 'string "special-" sft)))
       (format t "symbol_info('~a','~a',function_type,'~(~a~)').~%" sn pn sft)
       (let ((doc (documentation sym 'function))) (when doc (format t "doc_string('~a','~a',function,\"~a\").~%" sn pn (princ-to-string doc))))))
     '(when (symbol-plist sym) (format t "symbol_plist('~a','~a').~%" sn  (symbol-plist sym)))

))))))))

(psyminfo)



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
