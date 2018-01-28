#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/defstruct-0" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 04:47:47 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
(in-package "SYSTEM")


;;; The DEFSTRUCT macro.

*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp:0 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','#:system'])
/*
% macroexpand:-[in_package,system3].
*/
/*
% into:-[eval_when,[kw_compile_toplevel,kw_load_toplevel,kw_execute],[sys_select_package,'$ARRAY'([*],claz_base_character,"SYSTEM")]].
*/
:- do_when([kw_compile_toplevel, kw_load_toplevel, kw_execute],
	   f_sys_select_package('$ARRAY'([*], claz_base_character, "SYSTEM"),
				_Ignored),
	   _Ignored).
/*
;; The DEFSTRUCT macro.
*/
/*
(defmacro defstruct (name &rest slots)
  "Syntax: (defstruct
         {name | (name {:conc-name | (:conc-name prefix-string) |
                        :constructor | (:constructor symbol [lambda-list]) |
                        :copier | (:copier symbol) |
                        :predicate | (:predicate symbol) |
                        (:include symbol) |
                        (:print-function function) |
                        (:type {vector | (vector type) | list}) |
                        :named |
                        (:initial-offset number)}*)}
         [doc]
         {slot-name |
          (slot-name [default-value-form] {:type type | :read-only flag}*) }*
         )
Defines a structure named by NAME.  The doc-string DOC, if supplied, is saved
as a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."
  (let*((slot-descriptions slots)
	;;#+clos
	local-slot-descriptions
        options
        conc-name
        constructors default-constructor no-constructor
        copier
        predicate predicate-specified
        include
        print-function type named initial-offset
        offset name-offset
        documentation)

    (when (consp name)
          ;; The defstruct options are supplied.
          (setq options (cdr name))
          (setq name (car name)))

    ;; The default conc-name.
    (setq conc-name (string-concatenate (string name) "-"))

    ;; The default constructor.
    (setq default-constructor
          (intern (string-concatenate "MAKE-" (string name))))

    ;; The default copier and predicate.
    (setq copier
          (intern (string-concatenate "COPY-" (string name)))
          predicate
          (intern (string-concatenate (string name) "-P")))

    ;; Parse the defstruct options.
    (do ((os options (cdr os)) (o) (v))
        ((endp os))
      (cond ((and (consp (car os)) (not (endp (cdar os))))
             (setq o (caar os) v (cadar os))
             (case o
               (:CONC-NAME
                (if (null v)
                    (setq conc-name nil)
                    (setq conc-name v)))
               (:CONSTRUCTOR
                (if (null v)
                    (setq no-constructor t)
                    (if (endp (cddar os))
                        (setq constructors (cons v constructors))
                        (setq constructors (cons (cdar os) constructors)))))
               (:COPIER (setq copier v))
               (:PREDICATE
                (setq predicate v)
                (setq predicate-specified t))
               (:INCLUDE
                (setq include (cdar os))
                (unless (get-sysprop v 'IS-A-STRUCTURE)
                        (error ""(defmacro defstruct (name &rest slots)\n  \"Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure).\"\n  (let*((slot-descriptions slots)\n\t;;#+clos\n\tlocal-slot-descriptions\n        options\n        conc-name\n        constructors default-constructor no-constructor\n        copier\n        predicate predicate-specified\n        include\n        print-function type named initial-offset\n        offset name-offset\n        documentation)\n\n    (when (consp name)\n          ;; The defstruct options are supplied.\n          (setq options (cdr name))\n          (setq name (car name)))\n\n    ;; The default conc-name.\n    (setq conc-name (string-concatenate (string name) \"-\"))\n\n    ;; The default constructor.\n    (setq default-constructor\n          (intern (string-concatenate \"MAKE-\" (string name))))\n\n    ;; The default copier and predicate.\n    (setq copier\n          (intern (string-concatenate \"COPY-\" (string name)))\n          predicate\n          (intern (string-concatenate (string name) \"-P\")))\n\n    ;; Parse the defstruct options.\n    (do ((os options (cdr os)) (o) (v))\n        ((endp os))\n      (cond ((and (consp (car os)) (not (endp (cdar os))))\n             (setq o (caar os) v (cadar os))\n             (case o\n               (:CONC-NAME\n                (if (null v)\n                    (setq conc-name nil)\n                    (setq conc-name v)))\n               (:CONSTRUCTOR\n                (if (null v)\n                    (setq no-constructor t)\n                    (if (endp (cddar os))\n                        (setq constructors (cons v constructors))\n                        (setq constructors (cons (cdar os) constructors)))))\n               (:COPIER (setq copier v))\n               (:PREDICATE\n                (setq predicate v)\n                (setq predicate-specified t))\n               (:INCLUDE\n                (setq include (cdar os))\n                (unless (get-sysprop v 'IS-A-STRUCTURE)\n                        (error \"~S is an illegal included structure.\" v)))\n               (:PRINT-FUNCTION (setq print-function v))\n               (:TYPE (setq type v))\n               (:INITIAL-OFFSET (setq initial-offset v))\n               (t (error \"~S is an illegal defstruct option.\" o))))\n            (t\n             (if (consp (car os))\n                 (setq o (caar os))\n                 (setq o (car os)))\n             (case o\n               (:CONSTRUCTOR\n                (setq constructors\n                      (cons default-constructor constructors)))\n               ((:CONC-NAME :COPIER :PREDICATE :PRINT-FUNCTION))\n               (:NAMED (setq named t))\n               (t (error \"~S is an illegal defstruct option.\" o))))))\n\n    (setq conc-name (intern (string conc-name)))\n\n    ;; Skip the documentation string.\n    (when (and (not (endp slot-descriptions))\n               (stringp (car slot-descriptions)))\n          (setq documentation (car slot-descriptions))\n          (setq slot-descriptions (cdr slot-descriptions)))\n    \n    ;; Check the include option.\n    (when include\n          (unless (equal type (get-sysprop (car include) 'STRUCTURE-TYPE))\n                  (error \"~S is an illegal structure include.\"\n                         (car include))))\n\n    ;; Set OFFSET.\n    (setq offset (if include\n\t\t     (get-sysprop (car include) 'STRUCTURE-OFFSET)\n\t\t     0))\n\n    ;; Increment OFFSET.\n    (when (and type initial-of; Increment OFFSET.\n    (when (and type initial-offset)\n          (setq offset (+ offset initial-offset)))\n    (when (and type named)\n          (setq name-offset offset)\n          (setq offset (1+ offset)))\n\n    ;; Parse slot-descriptions, incrementing OFFSET for each one.\n    (do ((ds slot-descriptions (cdr ds))\n         (sds nil))\n        ((endp ds)\n         (setq slot-descriptions (nreverse sds)))\n      (push (parse-slot-description (car ds) offset) sds)\n      (setq offset (1+ offset)))\n\n    ;; If TYPE is non-NIL and structure is named,\n    ;;  add the slot for the structure-name to the slot-descriptions.\n    (when (and type named)\n          (setq slot-descriptions\n                (cons (list nil name) slot-descriptions)))\n\n    ;; Pad the slot-descriptions with the initial-offset number of NILs.\n    (when (and type initial-offset)\n          (setq slot-descriptions\n                (append (make-list initial-offset) slot-descriptions)))\n\n    ;;#+clos\n    (setq local-slot-descriptions slot-descriptions)\n\n    ;; Append the slot-descriptions of the included structure.\n    ;; The slot-descriptions in the include option are also counted.\n    (cond ((null include))\n          ((endp (cdr include))\n           (setq slot-descriptions\n                 (append (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS)\n                         slot-descriptions)))\n          (t\n           (setq slot-descriptions\n                 (append (overwrite-slot-descriptions\n                          (mapcar #'(lambda (sd)\n                                      (parse-slot-description sd 0))\n                                  (cdr include))\n                          (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS))\n                         slot-descriptions))))\n\n    (cond (no-constructor\n           ;; If a constructor option is NIL,\n           ;;  no constructor should have been specified.\n           (when constructors\n                 (error \"Contradictory constructor options.\")))\n          ((null constructors)\n           ;; If no constructor is specified,\n           ;;  the default-constructor is made.\n           (setq constructors (list default-constructor))))\n\n    ;; Check the named option and set the predicate.\n    (when (and type (not named))\n          (when predicate-specified\n                (error \"~S is an illegal structure predicate.\"\n                       predicate))\n          (setq predicate nil))\n\n    (when include (setq include (car include)))\n\n    ;; Check the print-function.\n    (when (and print-function type)\n          (error \"An print function is supplied to a typed structure.\"))\n\n    (if (or type (not (member ':CLOS *features*)))\n\t`(eval-when (compile load eval)\n\n\t  (define-structure ',name ',conc-name ',type ',named ',slots\n\t\t\t    ',slot-descriptions ',copier ',include\n\t\t\t    ',print-function ',constructors ',offset\n\t\t\t    ',documentation)\n\t  ,@(mapcar #'(lambda (constructor)\n\t\t\t(make-constructor name constructor type named\n\t\t\t\t\t  slot-descriptions))\n\t     constructors)\n\t  ,@(when predicate\n\t      (list `(fset ',predicate\n\t\t      (make-predicate ',name ',type ',named ',name-offset))))\n\t  ',name)\n\n      ;; else (and (not type) (member :CLOS *features*))\n\n      `(eval-when (compile load eval)\n\n\t(defclass ,name (,(or include 'STRUCTURE-OBJECT))\n\t  ,(mapcar\n\t    #'(lambda (sd)\n\t\t(if sd\n\t\t    (list* (first sd)\n\t\t\t   :initform (second sd)\n\t\t\t   :initarg \n\t\t\t   (intern (symbol-name (first sd))\n\t\t\t\t   (find-package 'KEYWORD))\n\t\t\t   (when (third sd) (list :type (third sd))))\n\t\t    nil))\t\t; for initial offset slots\n\t    local-slot-descriptions)\n\t  (:metaclass structure-class))\n\n#|\t   (with-slots (defstruct-form slot-descriptions initial-offset\n\t\t\t constructors documentation copier predicate\n\t\t\t print-function)\n\t\t       (find-class ',name)\n              (setq defstruct-form '(defstruct ,name ,@slots))\n\t      (setq slot-descriptions ',slot-descriptions)\n\t      (setq initial-offset ',structure-offset)\n\t      (setq constructors ',constructors)\n\t      (setq documentation ,documentation)\n\t      (setq copier ,copier)\n\t      (setq pre\t   (with-slots (defstruct-form slot-descriptions initial-offset\n\t\t\t constructors documentation copier predicate\n\t\t\t print-function)\n\t\t       (find-class ',name)\n              (setq defstruct-form '(defstruct ,name ,@slots))\n\t      (setq slot-descriptions ',slot-descriptions)\n\t      (setq initial-offset ',structure-offset)\n\t      (setq constructors ',constructors)\n\t      (setq documentation ,documentation)\n\t      (setq copier ,copier)\n\t      (setq predicate ,predicate)\n\t      (setq print-function ,print-function))\n|#\n\n\t,@(if print-function\n\t      `((defmethod print-object\n\t\t    ((obj ,name) stream)\n\t\t  (,print-function obj stream *print-level*))))\n\n\t(define-structure ',name ',conc-name ',type ',named ',slots\n\t\t\t  ',slot-descriptions ',copier ',include\n\t\t\t  ',print-function ',constructors ',offset\n\t\t\t  ',documentation)\n\t,@(mapcar #'(lambda (constructor)\n\t\t      (make-constructor name constructor type named\n\t\t\t\t\tslot-descriptions))\n\t   constructors)\n\t,@(when predicate\n\t    (list `(fset ',predicate\n\t\t    (make-predicate ',name ',type ',named ',name-offset))))\n\t',name))))\n\n\n;;; The #S reader.\n\n(defun sharp-s-reader (stream subchar arg)\n  (declare (ignore subchar))\n  (when (and arg (null *read-suppress*))\n        (error \"An extra argument was supplied for the #S readmacro.\"))\n  (let ((l (read stream)))\n    (unless (get-sysprop (car l) 'IS-A-STRUCTURE)\n            (error \"~S is not a structure.\" (car l)))\n    ;; Intern keywords in the keyword package.\n    (do ((ll (cdr l) (cddr ll)))\n        ((endp ll)\n         ;; Find an appropriate construtor.\n         (do ((cs (get-sysprop (car l) 'STRUCTURE-CONSTRUCTORS) (cdr cs)))\n             ((endp cs)\n              (error \"The structure ~S has no structure constructor.\"\n                     (car l)))\n           (when (symbolp (car cs))\n                 (return (apply (car cs) (cdr l))))))\n      (rplaca ll (intern (string (car ll)) 'KEYWORD)))))\n\n\n\n(defun make-constructor (name constructor type named slot-descriptions)\n  (declare (ignore named))\n  (let ((slot-names\n         ;; Collect the slot-names.\n         (mapcar #'(lambda (x)\n                     (cond ((null x)\n                            ;; If the slot-description is NIL,\n                            ;;  it is in the padding of initial-offset.\n                            nil)\n                           ((null (car x))\n                            ;; If the slot name is NIL,\n                            ;;  it is the structure name.\n                            ;;  This is for typed structures with names.\n                            (list 'QUOTE (cadr x)))\n                           (t (car x))))\n                 slot-descriptions))\n        (keys\n         ;; Make the keyword parameters.\n         (mapcan #'(lambda (x)\n                     (cond ((null x) nil)\n                           ((null (car x)) nil)\n                           ((null (cadr x)) (list (car x)))\n                           (t (list (list  (car x) (cadr x))))))\n                 slot-descriptions)))\n    (cond ((consp constructor)\n           ;; The case for a BOA constructor.\n           ;; Dirty code!!\n           ;; We must add an initial value for an optional parameter,\n           ;;  if the default value is not specified\n           ;;  in the given parameter list and yet the initial value\n           ;;  is supplied in the slot description.\n           (do ((a (cadr constructor) (cdr a)) (l nil) (vs nil))\n               ((endp a)\n                ;; Add those options that do not appear in the parameter list\n                ;;  as auxiliary paramters.\n                ;; The parameters are accumulated in the variable VS.\n                (setq keys\n                      (nreconc (cons '&aux l)\n                               (mapcan #'(lambda (k)\n                                           (if (member (if (atom k) k (car k))\n                                                       vs)\n                                               nil\n                                               (list k)))\n                                       keys))))\n             ;; Skip until &OPTIONAL ap; Skip until &OPTIONAL appears.\n             (cond ((eq (car a) '&optional)\n                    (setq l (cons '&optional l))\n                    (do ((aa (cdr a) (cdr aa)) (ov) (y))\n                        ((endp aa)\n                         ;; Add those options that do not appear in the\n                         ;;  parameter list.\n                         (setq keys\n                               (nreconc (cons '&aux l)\n                                        (mapcan #'(lambda (k)\n                                                    (if (member (if (atom k)\n                                                                    k\n                                                                    (car k))\n                                                                vs)\n                                                        nil\n                                                        (list k)))\n                                                keys)))\n                         (return nil))\n                      (when (member (car aa) lambda-list-keywords)\n                            (when (eq (car aa) '&rest)\n                                  ;; &REST is found.\n                                  (setq l (cons '&rest l))\n                                  (setq aa (cdr aa))\n                                  (unless (and (not (endp aa))\n                                               (symbolp (car aa)))\n                                          (illegal-boa))\n                                  (setq vs (cons (car aa) vs))\n                                  (setq l (cons (car aa) l))\n                                  (setq aa (cdr aa))\n                                  (when (endp aa)\n                                        (setq keys\n                                              (nreconc\n                                               (cons '&aux l)\n                                               (mapcan\n                                                #'(lambda (k)\n                                                    (if (member (if (atom k)\n                                                                    k\n                                                                    (car k))\n                                                                vs)\n                                                        nil\n                                                        (list k)))\n                                                keys)))\n                                        (return nil)))\n                            ;; &AUX should follow.\n                            (unless (eq (car aa) '&aux)\n                                    (illegal-boa))\n                            (setq l (cons '&aux l))\n                            (do ((aaa (cdr aa) (cdr aaa)))\n                                ((endp aaa))\n                              (setq l (cons (car aaa) l))\n                              (cond ((and (atom (car aaa))\n                                          (symbolp (car aaa)))\n                                     (setq vs (cons (car aaa) vs)))\n                                    ((and (symbolp (caar aaa))\n                                          (or (endp (cdar aaa))\n                                              (endp (cddar aaa))))\n                                     (setq vs (cons (caar aaa) vs)))\n                                    (t (illegal-boa))))\n                            ;; End of the parameter list.\n                            (setq keys\n                                  (nreconc l\n                                           (mapcan\n                                            #'(lambda (k)\n                                                (if (member (if (atom k)\n                                                                k\n                                                                (car k))\n                                                            vs)\n                                                    nil\n                                                    (list k)))\n                                            keys)))\n; End of the parameter list.\n                            (setq keys\n                                  (nreconc l\n                                           (mapcan\n                                            #'(lambda (k)\n                                                (if (member (if (atom k)\n                                                                k\n                                                                (car k))\n                                                            vs)\n                                                    nil\n                                                    (list k)))\n                                            keys)))\n                            (return nil))\n                      ;; Checks if the optional paramter without a default\n                      ;;  value has a default value in the slot-description.\n                      (if (and (cond ((atom (car aa)) (setq ov (car aa)) t)\n                                     ((endp (cdar aa)) (setq ov (caar aa)) t)\n                                     (t nil))\n                               (setq y (member ov\n                                               keys\n                                               :key\n                                               #'(lambda (x)\n                                                   (if (consp x)\n                                                       ;; With default value.\n                                                       (car x))))))\n                          ;; If no default value is supplied for\n                          ;;  the optional parameter and yet appears\n                          ;;  in KEYS with a default value,\n                          ;;  then cons the pair to L,\n                          (setq l (cons (car y) l))\n                          ;;  otherwise cons just the parameter to L.\n                          (setq l (cons (car aa) l)))\n                      ;; Checks the form of the optional parameter.\n                      (cond ((atom (car aa))\n       ".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp:51 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defstruct,[name,'&rest',slots],'$STRING'("Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."),['let*',[['slot-descriptions',slots],'local-slot-descriptions',options,'conc-name',constructors,'default-constructor','no-constructor',copier,predicate,'predicate-specified',include,'print-function',type,named,'initial-offset',offset,'name-offset',documentation],[when,[consp,name],[setq,options,[cdr,name]],[setq,name,[car,name]]],[setq,'conc-name',['string-concatenate',[string,name],'$STRING'("-")]],[setq,'default-constructor',[intern,['string-concatenate','$STRING'("MAKE-"),[string,name]]]],[setq,copier,[intern,['string-concatenate','$STRING'("COPY-"),[string,name]]],predicate,[intern,['string-concatenate',[string,name],'$STRING'("-P")]]],[do,[[os,options,[cdr,os]],[o],[v]],[[endp,os]],[cond,[[and,[consp,[car,os]],[not,[endp,[cdar,os]]]],[setq,o,[caar,os],v,[cadar,os]],[case,o,[':CONC-NAME',[if,[null,v],[setq,'conc-name',[]],[setq,'conc-name',v]]],[':CONSTRUCTOR',[if,[null,v],[setq,'no-constructor',t],[if,[endp,[cddar,os]],[setq,constructors,[cons,v,constructors]],[setq,constructors,[cons,[cdar,os],constructors]]]]],[':COPIER',[setq,copier,v]],[':PREDICATE',[setq,predicate,v],[setq,'predicate-specified',t]],[':INCLUDE',[setq,include,[cdar,os]],[unless,['get-sysprop',v,[quote,'IS-A-STRUCTURE']],[error,'$STRING'("~S is an illegal included structure."),v]]],[':PRINT-FUNCTION',[setq,'print-function',v]],[':TYPE',[setq,type,v]],[':INITIAL-OFFSET',[setq,'initial-offset',v]],[t,[error,'$STRING'("~S is an illegal defstruct option."),o]]]],[t,[if,[consp,[car,os]],[setq,o,[caar,os]],[setq,o,[car,os]]],[case,o,[':CONSTRUCTOR',[setq,constructors,[cons,'default-constructor',constructors]]],[[':CONC-NAME',':COPIER',':PREDICATE',':PRINT-FUNCTION']],[':NAMED',[setq,named,t]],[t,[error,'$STRING'("~S is an illegal defstruct option."),o]]]]]],[setq,'conc-name',[intern,[string,'conc-name']]],[when,[and,[not,[endp,'slot-descriptions']],[stringp,[car,'slot-descriptions']]],[setq,documentation,[car,'slot-descriptions']],[setq,'slot-descriptions',[cdr,'slot-descriptions']]],[when,include,[unless,[equal,type,['get-sysprop',[car,include],[quote,'STRUCTURE-TYPE']]],[error,'$STRING'("~S is an illegal structure include."),[car,include]]]],[setq,offset,[if,include,['get-sysprop',[car,include],[quote,'STRUCTURE-OFFSET']],0]],[when,[and,type,'initial-of',[when,[and,type,'initial-offset'],[setq,offset,[+,offset,'initial-offset']]],[when,[and,type,named],[setq,'name-offset',offset],[setq,offset,['1+',offset]]],[do,[[ds,'slot-descriptions',[cdr,ds]],[sds,[]]],[[endp,ds],[setq,'slot-descriptions',[nreverse,sds]]],[push,['parse-slot-description',[car,ds],offset],sds],[setq,offset,['1+',offset]]],[when,[and,type,named],[setq,'slot-descriptions',[cons,[list,[],name],'slot-descriptions']]],[when,[and,type,'initial-offset'],[setq,'slot-descriptions',[append,['make-list','initial-offset'],'slot-descriptions']]],[setq,'local-slot-descriptions','slot-descriptions'],[cond,[[null,include]],[[endp,[cdr,include]],[setq,'slot-descriptions',[append,['get-sysprop',[car,include],[quote,'STRUCTURE-SLOT-DESCRIPTIONS']],'slot-descriptions']]],[t,[setq,'slot-descriptions',[append,['overwrite-slot-descriptions',[mapcar,function([lambda,[sd],['parse-slot-description',sd,0]]),[cdr,include]],['get-sysprop',[car,include],[quote,'STRUCTURE-SLOT-DESCRIPTIONS']]],'slot-descriptions']]]],[cond,['no-constructor',[when,constructors,[error,'$STRING'("Contradictory constructor options.")]]],[[null,constructors],[setq,constructors,[list,'default-constructor']]]],[when,[and,type,[not,named]],[when,'predicate-specified',[error,'$STRING'("~S is an illegal structure predicate."),predicate]],[setq,predicate,[]]],[when,include,[setq,include,[car,include]]],[when,[and,'print-function',type],[error,'$STRING'("An print function is supplied to a typed structure.")]],[if,[or,type,[not,[member,[quote,':CLOS'],'*features*']]],['#BQ',['eval-when',[compile,load,eval],['define-structure',[quote,['#COMMA',name]],[quote,['#COMMA','conc-name']],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA',slots]],[quote,['#COMMA','slot-descriptions']],[quote,['#COMMA',copier]],[quote,['#COMMA',include]],[quote,['#COMMA','print-function']],[quote,['#COMMA',constructors]],[quote,['#COMMA',offset]],[quote,['#COMMA',documentation]]],['#BQ-COMMA-ELIPSE',[mapcar,function([lambda,[constructor],['make-constructor',name,constructor,type,named,'slot-descriptions']]),constructors]],['#BQ-COMMA-ELIPSE',[when,predicate,[list,['#BQ',[fset,[quote,['#COMMA',predicate]],['make-predicate',[quote,['#COMMA',name]],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA','name-offset']]]]]]]],[quote,['#COMMA',name]]]],['#BQ',['eval-when',[compile,load,eval],[defclass,['#COMMA',name],[['#COMMA',[or,include,[quote,'STRUCTURE-OBJECT']]]],['#COMMA',[mapcar,function([lambda,[sd],[if,sd,['list*',[first,sd],':initform',[second,sd],':initarg',[intern,['symbol-name',[first,sd]],['find-package',[quote,'KEYWORD']]],[when,[third,sd],[list,':type',[third,sd]]]],[]]]),'local-slot-descriptions']],[':metaclass','structure-class']],['#BQ-COMMA-ELIPSE',[if,'print-function',['#BQ',[[defmethod,'print-object',[[obj,['#COMMA',name]],stream],[['#COMMA','print-function'],obj,stream,'*print-level*']]]]]],['define-structure',[quote,['#COMMA',name]],[quote,['#COMMA','conc-name']],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA',slots]],[quote,['#COMMA','slot-descriptions']],[quote,['#COMMA',copier]],[quote,['#COMMA',include]],[quote,['#COMMA','print-function']],[quote,['#COMMA',constructors]],[quote,['#COMMA',offset]],[quote,['#COMMA',documentation]]],['#BQ-COMMA-ELIPSE',[mapcar,function([lambda,[constructor],['make-constructor',name,constructor,type,named,'slot-descriptions']]),constructors]],['#BQ-COMMA-ELIPSE',[when,predicate,[list,['#BQ',[fset,[quote,['#COMMA',predicate]],['make-predicate',[quote,['#COMMA',name]],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA','name-offset']]]]]]]],[quote,['#COMMA',name]]]]]]],[defun,'sharp-s-reader',[stream,subchar,arg],[declare,[ignore,subchar]],[when,[and,arg,[null,'*read-suppress*']],[error,'$STRING'("An extra argument was supplied for the #S readmacro.")]],[let,[[l,[read,stream]]],[unless,['get-sysprop',[car,l],[quote,'IS-A-STRUCTURE']],[error,'$STRING'("~S is not a structure."),[car,l]]],[do,[[ll,[cdr,l],[cddr,ll]]],[[endp,ll],[do,[[cs,['get-sysprop',[car,l],[quote,'STRUCTURE-CONSTRUCTORS']],[cdr,cs]]],[[endp,cs],[error,'$STRING'("The structure ~S has no structure constructor."),[car,l]]],[when,[symbolp,[car,cs]],[return,[apply,[car,cs],[cdr,l]]]]]],[rplaca,ll,[intern,[string,[car,ll]],[quote,'KEYWORD']]]]]],[defun,'make-constructor',[name,constructor,type,named,'slot-descriptions'],[declare,[ignore,named]],[let,[['slot-names',[mapcar,function([lambda,[x],[cond,[[null,x],[]],[[null,[car,x]],[list,[quote,'QUOTE'],[cadr,x]]],[t,[car,x]]]]),'slot-descriptions']],[keys,[mapcan,function([lambda,[x],[cond,[[null,x],[]],[[null,[car,x]],[]],[[null,[cadr,x]],[list,[car,x]]],[t,[list,[list,[car,x],[cadr,x]]]]]]),'slot-descriptions']]],[cond,[[consp,constructor],[do,[[a,[cadr,constructor],[cdr,a]],[l,[]],[vs,[]]],[[endp,a],[setq,keys,[nreconc,[cons,[quote,'&aux'],l],[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]]],[cond,[[eq,[car,a],[quote,'&optional']],[setq,l,[cons,[quote,'&optional'],l]],[do,[[aa,[cdr,a],[cdr,aa]],[ov],[y]],[[endp,aa],[setq,keys,[nreconc,[cons,[quote,'&aux'],l],[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[return,[]]],[when,[member,[car,aa],'lambda-list-keywords'],[when,[eq,[car,aa],[quote,'&rest']],[setq,l,[cons,[quote,'&rest'],l]],[setq,aa,[cdr,aa]],[unless,[and,[not,[endp,aa]],[symbolp,[car,aa]]],['illegal-boa']],[setq,vs,[cons,[car,aa],vs]],[setq,l,[cons,[car,aa],l]],[setq,aa,[cdr,aa]],[when,[endp,aa],[setq,keys,[nreconc,[cons,[quote,'&aux'],l],[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[return,[]]]],[unless,[eq,[car,aa],[quote,'&aux']],['illegal-boa']],[setq,l,[cons,[quote,'&aux'],l]],[do,[[aaa,[cdr,aa],[cdr,aaa]]],[[endp,aaa]],[setq,l,[cons,[car,aaa],l]],[cond,[[and,[atom,[car,aaa]],[symbolp,[car,aaa]]],[setq,vs,[cons,[car,aaa],vs]]],[[and,[symbolp,[caar,aaa]],[or,[endp,[cdar,aaa]],[endp,[cddar,aaa]]]],[setq,vs,[cons,[caar,aaa],vs]]],[t,['illegal-boa']]]],[setq,keys,[nreconc,l,[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[setq,keys,[nreconc,l,[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[return,[]]],[if,[and,[cond,[[atom,[car,aa]],[setq,ov,[car,aa]],t],[[endp,[cdar,aa]],[setq,ov,[caar,aa]],t],[t,[]]],[setq,y,[member,ov,keys,':key',function([lambda,[x],[if,[consp,x],[car,x]]])]]],[setq,l,[cons,[car,y],l]],[setq,l,[cons,[car,aa],l]]],[cond,[[atom,[car,aa]],[unless,[symbolp,[car,aa]],['illegal-boa']],[setq,vs,[cons,[car,aa],vs]]],[[not,[symbolp,[caar,aa]]],['illegal-boa']],[[or,[endp,[cdar,aa]],[endp,[cddar,aa]]],[setq,vs,[cons,[caar,aa],vs]]],[[not,[symbolp,[caddar,aa]]],['illegal-boa']],[[not,[endp,[cdddar,aa]]],['illegal-boa']],[t,[setq,vs,[cons,[caar,aa],vs]],[setq,vs,[cons,[caddar,aa],vs]]]]],[return,[]]],[t,[unless,[symbolp,[car,a]],['illegal-boa']],[setq,l,[cons,[car,a],l]],[setq,vs,[cons,[car,a],vs]]]]],[setq,constructor,[car,constructor]]],[t,[setq,keys,[cons,[quote,'&key'],keys]]]],[cond,[[null,type],['#BQ',[defun,['#COMMA',constructor],['#COMMA',keys],['sys:make-structure',[quote,['#COMMA',name]],['#BQ-COMMA-ELIPSE','slot-names']]]]],[[or,[eq,type,[quote,'VECTOR']],[and,[consp,type],[eq,[car,type],[quote,vector]]]],['#BQ',[defun,['#COMMA',constructor],['#COMMA',keys],[vector,['#BQ-COMMA-ELIPSE','slot-names']]]]],[[eq,type,[quote,'LIST']],['#BQ',[defun,['#COMMA',constructor],['#COMMA',keys],[list,['#BQ-COMMA-ELIPSE','slot-names']]]]],[[error,'$STRING'("~S is an illegal structure type"),type]]]]],[setq,keys,[cons,[quote,'&key'],keys]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_string_concatenate,
					       kw_function,
					       f_sys_string_concatenate)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_string_concatenate,
					       kw_function,
					       f_sys_string_concatenate)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_string_concatenate,
					       kw_function,
					       f_sys_string_concatenate)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_string_concatenate,
					       kw_function,
					       f_sys_string_concatenate)).
*/
/*
% case:-[[kw_conc_name,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]],[kw_constructor,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]],[kw_copier,[setq,sys_copier,sys_v]],[kw_predicate,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]],[kw_include,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]],[kw_print_function,[setq,sys_print_function,sys_v]],[kw_type,[setq,type,sys_v]],[kw_initial_offset,[setq,sys_initial_offset,sys_v]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_544128,[quote,kw_conc_name]],[progn,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]]],[[eq,_544128,[quote,kw_constructor]],[progn,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]]],[[eq,_544128,[quote,kw_copier]],[progn,[setq,sys_copier,sys_v]]],[[eq,_544128,[quote,kw_predicate]],[progn,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]]],[[eq,_544128,[quote,kw_include]],[progn,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]]],[[eq,_544128,[quote,kw_print_function]],[progn,[setq,sys_print_function,sys_v]]],[[eq,_544128,[quote,kw_type]],[progn,[setq,type,sys_v]]],[[eq,_544128,[quote,kw_initial_offset]],[progn,[setq,sys_initial_offset,sys_v]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
% case:-[[kw_constructor,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]],[[kw_conc_name,kw_copier,kw_predicate,kw_print_function]],[kw_named,[setq,sys_named,t]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_917220,[quote,kw_constructor]],[progn,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]]],[[sys_memq,_917220,[quote,[kw_conc_name,kw_copier,kw_predicate,kw_print_function]]],[progn]],[[eq,_917220,[quote,kw_named]],[progn,[setq,sys_named,t]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
% case:-[[kw_conc_name,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]],[kw_constructor,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]],[kw_copier,[setq,sys_copier,sys_v]],[kw_predicate,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]],[kw_include,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]],[kw_print_function,[setq,sys_print_function,sys_v]],[kw_type,[setq,type,sys_v]],[kw_initial_offset,[setq,sys_initial_offset,sys_v]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_709572,[quote,kw_conc_name]],[progn,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]]],[[eq,_709572,[quote,kw_constructor]],[progn,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]]],[[eq,_709572,[quote,kw_copier]],[progn,[setq,sys_copier,sys_v]]],[[eq,_709572,[quote,kw_predicate]],[progn,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]]],[[eq,_709572,[quote,kw_include]],[progn,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]]],[[eq,_709572,[quote,kw_print_function]],[progn,[setq,sys_print_function,sys_v]]],[[eq,_709572,[quote,kw_type]],[progn,[setq,type,sys_v]]],[[eq,_709572,[quote,kw_initial_offset]],[progn,[setq,sys_initial_offset,sys_v]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
% case:-[[kw_constructor,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]],[[kw_conc_name,kw_copier,kw_predicate,kw_print_function]],[kw_named,[setq,sys_named,t]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_905762,[quote,kw_constructor]],[progn,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]]],[[sys_memq,_905762,[quote,[kw_conc_name,kw_copier,kw_predicate,kw_print_function]]],[progn]],[[eq,_905762,[quote,kw_named]],[progn,[setq,sys_named,t]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       defstruct,
					       kw_special,
					       sf_defstruct)).
*/
doc: doc_string(defstruct,
	      _32856,
	      function,
	      "Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure).").

wl:lambda_def(defmacro, defstruct, mf_defstruct, [sys_name, c38_rest, sys_slots], [[let_xx, [[sys_slot_descriptions, sys_slots], sys_local_slot_descriptions, sys_options, sys_conc_name, sys_constructors, sys_default_constructor, sys_no_constructor, sys_copier, sys_predicate, sys_predicate_specified, sys_include, sys_print_function, type, sys_named, sys_initial_offset, sys_offset, sys_name_offset, documentation], [when, [consp, sys_name], [setq, sys_options, [cdr, sys_name]], [setq, sys_name, [car, sys_name]]], [setq, sys_conc_name, [sys_string_concatenate, [string, sys_name], '$ARRAY'([*], claz_base_character, "-")]], [setq, sys_default_constructor, [intern, [sys_string_concatenate, '$ARRAY'([*], claz_base_character, "MAKE-"), [string, sys_name]]]], [setq, sys_copier, [intern, [sys_string_concatenate, '$ARRAY'([*], claz_base_character, "COPY-"), [string, sys_name]]], sys_predicate, [intern, [sys_string_concatenate, [string, sys_name], '$ARRAY'([*], claz_base_character, "-P")]]], [do, [[sys_os, sys_options, [cdr, sys_os]], [sys_o], [sys_v]], [[endp, sys_os]], [cond, [[and, [consp, [car, sys_os]], [not, [endp, [cdar, sys_os]]]], [setq, sys_o, [caar, sys_os], sys_v, [cadar, sys_os]], [case, sys_o, [kw_conc_name, [if, [null, sys_v], [setq, sys_conc_name, []], [setq, sys_conc_name, sys_v]]], [kw_constructor, [if, [null, sys_v], [setq, sys_no_constructor, t], [if, [endp, [cddar, sys_os]], [setq, sys_constructors, [cons, sys_v, sys_constructors]], [setq, sys_constructors, [cons, [cdar, sys_os], sys_constructors]]]]], [kw_copier, [setq, sys_copier, sys_v]], [kw_predicate, [setq, sys_predicate, sys_v], [setq, sys_predicate_specified, t]], [kw_include, [setq, sys_include, [cdar, sys_os]], [unless, [sys_get_sysprop, sys_v, [quote, sys_is_a_structure]], [error, '$ARRAY'([*], claz_base_character, "~S is an illegal included structure."), sys_v]]], [kw_print_function, [setq, sys_print_function, sys_v]], [kw_type, [setq, type, sys_v]], [kw_initial_offset, [setq, sys_initial_offset, sys_v]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), sys_o]]]], [t, [if, [consp, [car, sys_os]], [setq, sys_o, [caar, sys_os]], [setq, sys_o, [car, sys_os]]], [case, sys_o, [kw_constructor, [setq, sys_constructors, [cons, sys_default_constructor, sys_constructors]]], [[kw_conc_name, kw_copier, kw_predicate, kw_print_function]], [kw_named, [setq, sys_named, t]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), sys_o]]]]]], [setq, sys_conc_name, [intern, [string, sys_conc_name]]], [when, [and, [not, [endp, sys_slot_descriptions]], [stringp, [car, sys_slot_descriptions]]], [setq, documentation, [car, sys_slot_descriptions]], [setq, sys_slot_descriptions, [cdr, sys_slot_descriptions]]], [when, sys_include, [unless, [equal, type, [sys_get_sysprop, [car, sys_include], [quote, sys_structure_type]]], [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure include."), [car, sys_include]]]], [setq, sys_offset, [if, sys_include, [sys_get_sysprop, [car, sys_include], [quote, sys_structure_offset]], 0]], [when, [and, type, sys_initial_of, [when, [and, type, sys_initial_offset], [setq, sys_offset, [+, sys_offset, sys_initial_offset]]], [when, [and, type, sys_named], [setq, sys_name_offset, sys_offset], [setq, sys_offset, ['1+', sys_offset]]], [do, [[sys_ds, sys_slot_descriptions, [cdr, sys_ds]], [sys_sds, []]], [[endp, sys_ds], [setq, sys_slot_descriptions, [nreverse, sys_sds]]], [push, [sys_parse_slot_description, [car, sys_ds], sys_offset], sys_sds], [setq, sys_offset, ['1+', sys_offset]]], [when, [and, type, sys_named], [setq, sys_slot_descriptions, [cons, [list, [], sys_name], sys_slot_descriptions]]], [when, [and, type, sys_initial_offset], [setq, sys_slot_descriptions, [append, [make_list, sys_initial_offset], sys_slot_descriptions]]], [setq, sys_local_slot_descriptions, sys_slot_descriptions], [cond, [[null, sys_include]], [[endp, [cdr, sys_include]], [setq, sys_slot_descriptions, [append, [sys_get_sysprop, [car, sys_include], [quote, sys_structure_slot_descriptions]], sys_slot_descriptions]]], [t, [setq, sys_slot_descriptions, [append, [sys_overwrite_slot_descriptions, [mapcar, function([lambda, [sys_sd], [sys_parse_slot_description, sys_sd, 0]]), [cdr, sys_include]], [sys_get_sysprop, [car, sys_include], [quote, sys_structure_slot_descriptions]]], sys_slot_descriptions]]]], [cond, [sys_no_constructor, [when, sys_constructors, [error, '$ARRAY'([*], claz_base_character, "Contradictory constructor options.")]]], [[null, sys_constructors], [setq, sys_constructors, [list, sys_default_constructor]]]], [when, [and, type, [not, sys_named]], [when, sys_predicate_specified, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure predicate."), sys_predicate]], [setq, sys_predicate, []]], [when, sys_include, [setq, sys_include, [car, sys_include]]], [when, [and, sys_print_function, type], [error, '$ARRAY'([*], claz_base_character, "An print function is supplied to a typed structure.")]], [if, [or, type, [not, [member, [quote, kw_clos], xx_features_xx]]], ['#BQ', [eval_when, [compile, load, eval], [sys_define_structure, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', sys_conc_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_slots]], [quote, ['#COMMA', sys_slot_descriptions]], [quote, ['#COMMA', sys_copier]], [quote, ['#COMMA', sys_include]], [quote, ['#COMMA', sys_print_function]], [quote, ['#COMMA', sys_constructors]], [quote, ['#COMMA', sys_offset]], [quote, ['#COMMA', documentation]]], ['#BQ-COMMA-ELIPSE', [mapcar, function([lambda, [sys_constructor], [sys_make_constructor, sys_name, sys_constructor, type, sys_named, sys_slot_descriptions]]), sys_constructors]], ['#BQ-COMMA-ELIPSE', [when, sys_predicate, [list, ['#BQ', [sys_fset, [quote, ['#COMMA', sys_predicate]], [sys_make_predicate, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_name_offset]]]]]]]], [quote, ['#COMMA', sys_name]]]], ['#BQ', [eval_when, [compile, load, eval], [defclass, ['#COMMA', sys_name], [['#COMMA', [or, sys_include, [quote, structure_object]]]], ['#COMMA', [mapcar, function([lambda, [sys_sd], [if, sys_sd, [list_xx, [first, sys_sd], kw_initform, [second, sys_sd], kw_initarg, [intern, [symbol_name, [first, sys_sd]], [find_package, [quote, keyword]]], [when, [third, sys_sd], [list, kw_type, [third, sys_sd]]]], []]]), sys_local_slot_descriptions]], [kw_metaclass, structure_class]], ['#BQ-COMMA-ELIPSE', [if, sys_print_function, ['#BQ', [[defmethod, print_object, [[sys_obj, ['#COMMA', sys_name]], stream], [['#COMMA', sys_print_function], sys_obj, stream, xx_print_level_xx]]]]]], [sys_define_structure, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', sys_conc_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_slots]], [quote, ['#COMMA', sys_slot_descriptions]], [quote, ['#COMMA', sys_copier]], [quote, ['#COMMA', sys_include]], [quote, ['#COMMA', sys_print_function]], [quote, ['#COMMA', sys_constructors]], [quote, ['#COMMA', sys_offset]], [quote, ['#COMMA', documentation]]], ['#BQ-COMMA-ELIPSE', [mapcar, function([lambda, [sys_constructor], [sys_make_constructor, sys_name, sys_constructor, type, sys_named, sys_slot_descriptions]]), sys_constructors]], ['#BQ-COMMA-ELIPSE', [when, sys_predicate, [list, ['#BQ', [sys_fset, [quote, ['#COMMA', sys_predicate]], [sys_make_predicate, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_name_offset]]]]]]]], [quote, ['#COMMA', sys_name]]]]]]], [defun, sys_sharp_s_reader, [stream, subchar, arg], [declare, [ignore, subchar]], [when, [and, arg, [null, xx_read_suppress_xx]], [error, '$ARRAY'([*], claz_base_character, "An extra argument was supplied for the #S readmacro.")]], [let, [[sys_l, [read, stream]]], [unless, [sys_get_sysprop, [car, sys_l], [quote, sys_is_a_structure]], [error, '$ARRAY'([*], claz_base_character, "~S is not a structure."), [car, sys_l]]], [do, [[sys_ll, [cdr, sys_l], [cddr, sys_ll]]], [[endp, sys_ll], [do, [[sys_cs, [sys_get_sysprop, [car, sys_l], [quote, sys_structure_constructors]], [cdr, sys_cs]]], [[endp, sys_cs], [error, '$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), [car, sys_l]]], [when, [symbolp, [car, sys_cs]], [return, [apply, [car, sys_cs], [cdr, sys_l]]]]]], [rplaca, sys_ll, [intern, [string, [car, sys_ll]], [quote, keyword]]]]]], [defun, sys_make_constructor, [sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], [declare, [ignore, sys_named]], [let, [[sys_slot_names, [mapcar, function([lambda, [x], [cond, [[null, x], []], [[null, [car, x]], [list, [quote, quote], [cadr, x]]], [t, [car, x]]]]), sys_slot_descriptions]], [sys_keys, [mapcan, function([lambda, [x], [cond, [[null, x], []], [[null, [car, x]], []], [[null, [cadr, x]], [list, [car, x]]], [t, [list, [list, [car, x], [cadr, x]]]]]]), sys_slot_descriptions]]], [cond, [[consp, sys_constructor], [do, [[a, [cadr, sys_constructor], [cdr, a]], [sys_l, []], [sys_vs, []]], [[endp, a], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]]], [cond, [[eq, [car, a], [quote, c38_optional]], [setq, sys_l, [cons, [quote, c38_optional], sys_l]], [do, [[sys_aa, [cdr, a], [cdr, sys_aa]], [sys_ov], [sys_y]], [[endp, sys_aa], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]], [when, [member, [car, sys_aa], lambda_list_keywords], [when, [eq, [car, sys_aa], [quote, c38_rest]], [setq, sys_l, [cons, [quote, c38_rest], sys_l]], [setq, sys_aa, [cdr, sys_aa]], [unless, [and, [not, [endp, sys_aa]], [symbolp, [car, sys_aa]]], [sys_illegal_boa]], [setq, sys_vs, [cons, [car, sys_aa], sys_vs]], [setq, sys_l, [cons, [car, sys_aa], sys_l]], [setq, sys_aa, [cdr, sys_aa]], [when, [endp, sys_aa], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]]], [unless, [eq, [car, sys_aa], [quote, c38_aux]], [sys_illegal_boa]], [setq, sys_l, [cons, [quote, c38_aux], sys_l]], [do, [[sys_aaa, [cdr, sys_aa], [cdr, sys_aaa]]], [[endp, sys_aaa]], [setq, sys_l, [cons, [car, sys_aaa], sys_l]], [cond, [[and, [atom, [car, sys_aaa]], [symbolp, [car, sys_aaa]]], [setq, sys_vs, [cons, [car, sys_aaa], sys_vs]]], [[and, [symbolp, [caar, sys_aaa]], [or, [endp, [cdar, sys_aaa]], [endp, [cddar, sys_aaa]]]], [setq, sys_vs, [cons, [caar, sys_aaa], sys_vs]]], [t, [sys_illegal_boa]]]], [setq, sys_keys, [nreconc, sys_l, [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [setq, sys_keys, [nreconc, sys_l, [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]], [if, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [x], [if, [consp, x], [car, x]]])]]], [setq, sys_l, [cons, [car, sys_y], sys_l]], [setq, sys_l, [cons, [car, sys_aa], sys_l]]], [cond, [[atom, [car, sys_aa]], [unless, [symbolp, [car, sys_aa]], [sys_illegal_boa]], [setq, sys_vs, [cons, [car, sys_aa], sys_vs]]], [[not, [symbolp, [caar, sys_aa]]], [sys_illegal_boa]], [[or, [endp, [cdar, sys_aa]], [endp, [cddar, sys_aa]]], [setq, sys_vs, [cons, [caar, sys_aa], sys_vs]]], [[not, [symbolp, [caddar, sys_aa]]], [sys_illegal_boa]], [[not, [endp, [cdddar, sys_aa]]], [sys_illegal_boa]], [t, [setq, sys_vs, [cons, [caar, sys_aa], sys_vs]], [setq, sys_vs, [cons, [caddar, sys_aa], sys_vs]]]]], [return, []]], [t, [unless, [symbolp, [car, a]], [sys_illegal_boa]], [setq, sys_l, [cons, [car, a], sys_l]], [setq, sys_vs, [cons, [car, a], sys_vs]]]]], [setq, sys_constructor, [car, sys_constructor]]], [t, [setq, sys_keys, [cons, [quote, c38_key], sys_keys]]]], [cond, [[null, type], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [sys_make_structure, [quote, ['#COMMA', sys_name]], ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[or, [eq, type, [quote, vector]], [and, [consp, type], [eq, [car, type], [quote, vector]]]], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [vector, ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[eq, type, [quote, list]], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [list, ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure type"), type]]]]], [setq, sys_keys, [cons, [quote, c38_key], sys_keys]]]]).
wl:arglist_info(defstruct, mf_defstruct, [sys_name, c38_rest, sys_slots], arginfo{all:[sys_name], allow_other_keys:0, aux:0, body:0, complex:[rest], env:0, key:0, names:[sys_name, sys_slots], opt:0, req:[sys_name], rest:[sys_slots], sublists:0, whole:0}).
wl: init_args(1, mf_defstruct).

/*

### Compiled Macro Operator: `CL:DEFSTRUCT` 
*/
sf_defstruct(MacroEnv, Name_In, RestNKeys, FResult) :-
	mf_defstruct([defstruct, Name_In|RestNKeys], MacroEnv, MFResult),
	f_sys_env_eval(MacroEnv, MFResult, FResult).
/*

### Compiled Macro Function: `CL:DEFSTRUCT` 
*/
mf_defstruct([defstruct, Name_In|RestNKeys], MacroEnv, MFResult) :-
	nop(defmacro),
	GEnv=[bv(sys_name, Name_In), bv(sys_slots, RestNKeys)],
	catch(( ( get_var(GEnv, sys_slots, Slots_Get),
		  LEnv=[bv(sys_slot_descriptions, Slots_Get)|GEnv],
		  LEnv13=[bv(sys_local_slot_descriptions, [])|LEnv],
		  LEnv16=[bv(sys_options, [])|LEnv13],
		  LEnv19=[bv(sys_conc_name, [])|LEnv16],
		  LEnv22=[bv(sys_constructors, [])|LEnv19],
		  LEnv25=[bv(sys_default_constructor, [])|LEnv22],
		  LEnv28=[bv(sys_no_constructor, [])|LEnv25],
		  LEnv31=[bv(sys_copier, [])|LEnv28],
		  LEnv34=[bv(sys_predicate, [])|LEnv31],
		  LEnv37=[bv(sys_predicate_specified, [])|LEnv34],
		  LEnv40=[bv(sys_include, [])|LEnv37],
		  LEnv43=[bv(sys_print_function, [])|LEnv40],
		  LEnv46=[bv(type, [])|LEnv43],
		  LEnv49=[bv(sys_named, [])|LEnv46],
		  LEnv52=[bv(sys_initial_offset, [])|LEnv49],
		  LEnv55=[bv(sys_offset, [])|LEnv52],
		  LEnv58=[bv(sys_name_offset, [])|LEnv55],
		  AEnv=[bv(documentation, [])|LEnv58],
		  get_var(AEnv, sys_name, Name_Get),
		  (   c0nz:is_consp(Name_Get)
		  ->  get_var(AEnv, sys_name, Name_Get67),
		      f_cdr(Name_Get67, Options),
		      set_var(AEnv, sys_options, Options),
		      get_var(AEnv, sys_name, Name_Get68),
		      f_car(Name_Get68, TrueResult),
		      set_var(AEnv, sys_name, TrueResult),
		      _34430=TrueResult
		  ;   _34430=[]
		  ),
		  get_var(AEnv, sys_name, Name_Get70),
		  f_string(Name_Get70, String_concatenate_Param),
		  f_sys_string_concatenate(String_concatenate_Param,
					   '$ARRAY'([*],
						    claz_base_character,
						    "-"),
					   Conc_name),
		  set_var(AEnv, sys_conc_name, Conc_name),
		  get_var(AEnv, sys_name, Name_Get71),
		  f_string(Name_Get71, String_Ret),
		  f_sys_string_concatenate('$ARRAY'([*],
						    claz_base_character,
						    "MAKE-"),
					   String_Ret,
					   Intern_Param),
		  f_intern(Intern_Param, Default_constructor),
		  set_var(AEnv, sys_default_constructor, Default_constructor),
		  get_var(AEnv, sys_name, Name_Get72),
		  f_string(Name_Get72, String_Ret2364),
		  f_sys_string_concatenate('$ARRAY'([*],
						    claz_base_character,
						    "COPY-"),
					   String_Ret2364,
					   Intern_Param2245),
		  f_intern(Intern_Param2245, Copier),
		  set_var(AEnv, sys_copier, Copier),
		  get_var(AEnv, sys_name, Name_Get73),
		  f_string(Name_Get73, String_concatenate_Param2246),
		  f_sys_string_concatenate(String_concatenate_Param2246,
					   '$ARRAY'([*],
						    claz_base_character,
						    "-P"),
					   Intern_Param2247),
		  f_intern(Intern_Param2247, Predicate),
		  set_var(AEnv, sys_predicate, Predicate),
		  get_var(AEnv, sys_options, Options_Get),
		  AEnv=[bv(sys_os, Options_Get), bv([sys_o], []), bv([sys_v], [])|AEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_15), get_var(AEnv, sys_os, Os_Get196), (s3q:is_endp(Os_Get196)->throw(block_exit([], [])), _TBResult=ThrowResult200;get_var(AEnv, sys_os, Os_Get205), f_car(Os_Get205, PredArgResult207), (c0nz:is_consp(PredArgResult207)->get_var(AEnv, sys_os, Os_Get208), f_cdar(Os_Get208, Endp_Param), f_endp(Endp_Param, Not_Param), f_not(Not_Param, TrueResult209), IFTEST202=TrueResult209;IFTEST202=[]), (IFTEST202\==[]->get_var(AEnv, sys_os, Os_Get211), f_caar(Os_Get211, O), set_var(AEnv, sys_o, O), get_var(AEnv, sys_os, Os_Get212), f_cadar(Os_Get212, V), set_var(AEnv, sys_v, V), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_conc_name)->get_var(AEnv, sys_v, IFTEST218), (IFTEST218==[]->set_var(AEnv, sys_conc_name, []), TrueResult276=[];get_var(AEnv, sys_v, V_Get221), set_var(AEnv, sys_conc_name, V_Get221), TrueResult276=V_Get221), TrueResult302=TrueResult276;(is_eq(Key, kw_constructor)->get_var(AEnv, sys_v, IFTEST225), (IFTEST225==[]->set_var(AEnv, sys_no_constructor, t), TrueResult274=t;get_var(AEnv, sys_os, Os_Get229), f_cddar(Os_Get229, PredArgResult231), (s3q:is_endp(PredArgResult231)->get_var(AEnv, sys_constructors, Constructors_Get233), get_var(AEnv, sys_v, V_Get232), TrueResult236=[V_Get232|Constructors_Get233], set_var(AEnv, sys_constructors, TrueResult236), ElseResult238=TrueResult236;get_var(AEnv, sys_os, Os_Get234), f_cdar(Os_Get234, Cdar_Ret), get_var(AEnv, sys_constructors, Constructors_Get235), ElseResult237=[Cdar_Ret|Constructors_Get235], set_var(AEnv, sys_constructors, ElseResult237), ElseResult238=ElseResult237), TrueResult274=ElseResult238), ElseResult277=TrueResult274;(is_eq(Key, kw_copier)->get_var(AEnv, sys_v, V_Get241), set_var(AEnv, sys_copier, V_Get241), ElseResult275=V_Get241;(is_eq(Key, kw_predicate)->get_var(AEnv, sys_v, V_Get244), set_var(AEnv, sys_predicate, V_Get244), set_var(AEnv, sys_predicate_specified, t), ElseResult273=t;(is_eq(Key, kw_include)->get_var(AEnv, sys_os, Os_Get247), f_cdar(Os_Get247, Include), set_var(AEnv, sys_include, Include), get_var(AEnv, sys_v, V_Get250), f_sys_get_sysprop(V_Get250, sys_is_a_structure, [], IFTEST248), (IFTEST248\==[]->TrueResult269=[];get_var(AEnv, sys_v, V_Get251), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal included structure."), V_Get251], ElseResult252), TrueResult269=ElseResult252), ElseResult271=TrueResult269;(is_eq(Key, kw_print_function)->get_var(AEnv, sys_v, V_Get255), set_var(AEnv, sys_print_function, V_Get255), ElseResult270=V_Get255;(is_eq(Key, kw_type)->get_var(AEnv, sys_v, V_Get258), set_var(AEnv, type, V_Get258), ElseResult268=V_Get258;(is_eq(Key, kw_initial_offset)->get_var(AEnv, sys_v, V_Get261), set_var(AEnv, sys_initial_offset, V_Get261), ElseResult266=V_Get261;get_var(AEnv, sys_o, O_Get262), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get262], ElseResult264), ElseResult266=ElseResult264), ElseResult268=ElseResult266), ElseResult270=ElseResult268), ElseResult271=ElseResult270), ElseResult273=ElseResult271), ElseResult275=ElseResult273), ElseResult277=ElseResult275), TrueResult302=ElseResult277), _37724=TrueResult302;get_var(AEnv, sys_os, Os_Get279), f_car(Os_Get279, PredArgResult281), (c0nz:is_consp(PredArgResult281)->get_var(AEnv, sys_os, Os_Get282), f_caar(Os_Get282, TrueResult284), set_var(AEnv, sys_o, TrueResult284), _39564=TrueResult284;get_var(AEnv, sys_os, Os_Get283), f_car(Os_Get283, ElseResult285), set_var(AEnv, sys_o, ElseResult285), _39564=ElseResult285), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_constructor)->get_var(AEnv, sys_constructors, Constructors_Get292), get_var(AEnv, sys_default_constructor, Default_constructor_Get291), TrueResult300=[Default_constructor_Get291|Constructors_Get292], set_var(AEnv, sys_constructors, TrueResult300), ElseResult303=TrueResult300;f_sys_memq(Key, [kw_conc_name, kw_copier, kw_predicate, kw_print_function], IFTEST293), (IFTEST293\==[]->ElseResult301=[];(is_eq(Key, kw_named)->set_var(AEnv, sys_named, t), ElseResult299=t;get_var(AEnv, sys_o, O_Get297), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get297], ElseResult298), ElseResult299=ElseResult298), ElseResult301=ElseResult299), ElseResult303=ElseResult301), _37724=ElseResult303), get_var(AEnv, sys_os, Os_Get304), f_cdr(Os_Get304, Os), set_var(AEnv, sys_os, Os), goto(do_label_15, AEnv), _TBResult=_GORES305)),
					  
					  [ addr(addr_tagbody_15_do_label_15,
						 do_label_15,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_os, Os_Get), (s3q:is_endp(Os_Get)->throw(block_exit([], [])), _40330=ThrowResult;get_var(AEnv, sys_os, Os_Get90), f_car(Os_Get90, PredArgResult92), (c0nz:is_consp(PredArgResult92)->get_var(AEnv, sys_os, Os_Get93), f_cdar(Os_Get93, Endp_Param2250), f_endp(Endp_Param2250, Not_Param2251), f_not(Not_Param2251, TrueResult94), IFTEST87=TrueResult94;IFTEST87=[]), (IFTEST87\==[]->get_var(AEnv, sys_os, Os_Get96), f_caar(Os_Get96, Caar_Ret), set_var(AEnv, sys_o, Caar_Ret), get_var(AEnv, sys_os, Os_Get97), f_cadar(Os_Get97, Cadar_Ret), set_var(AEnv, sys_v, Cadar_Ret), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_conc_name)->get_var(AEnv, sys_v, IFTEST103), (IFTEST103==[]->set_var(AEnv, sys_conc_name, []), TrueResult161=[];get_var(AEnv, sys_v, V_Get106), set_var(AEnv, sys_conc_name, V_Get106), TrueResult161=V_Get106), TrueResult187=TrueResult161;(is_eq(Key, kw_constructor)->get_var(AEnv, sys_v, IFTEST110), (IFTEST110==[]->set_var(AEnv, sys_no_constructor, t), TrueResult159=t;get_var(AEnv, sys_os, Os_Get114), f_cddar(Os_Get114, PredArgResult116), (s3q:is_endp(PredArgResult116)->get_var(AEnv, sys_constructors, Get_var_Ret), get_var(AEnv, sys_v, V_Get117), TrueResult121=[V_Get117|Get_var_Ret], set_var(AEnv, sys_constructors, TrueResult121), ElseResult123=TrueResult121;get_var(AEnv, sys_os, Os_Get119), f_cdar(Os_Get119, Cdar_Ret2369), get_var(AEnv, sys_constructors, Constructors_Get120), ElseResult122=[Cdar_Ret2369|Constructors_Get120], set_var(AEnv, sys_constructors, ElseResult122), ElseResult123=ElseResult122), TrueResult159=ElseResult123), ElseResult162=TrueResult159;(is_eq(Key, kw_copier)->get_var(AEnv, sys_v, V_Get126), set_var(AEnv, sys_copier, V_Get126), ElseResult160=V_Get126;(is_eq(Key, kw_predicate)->get_var(AEnv, sys_v, V_Get129), set_var(AEnv, sys_predicate, V_Get129), set_var(AEnv, sys_predicate_specified, t), ElseResult158=t;(is_eq(Key, kw_include)->get_var(AEnv, sys_os, Os_Get132), f_cdar(Os_Get132, Cdar_Ret2370), set_var(AEnv, sys_include, Cdar_Ret2370), get_var(AEnv, sys_v, V_Get135), f_sys_get_sysprop(V_Get135, sys_is_a_structure, [], IFTEST133), (IFTEST133\==[]->TrueResult154=[];get_var(AEnv, sys_v, V_Get136), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal included structure."), V_Get136], ElseResult137), TrueResult154=ElseResult137), ElseResult156=TrueResult154;(is_eq(Key, kw_print_function)->get_var(AEnv, sys_v, V_Get140), set_var(AEnv, sys_print_function, V_Get140), ElseResult155=V_Get140;(is_eq(Key, kw_type)->get_var(AEnv, sys_v, V_Get143), set_var(AEnv, type, V_Get143), ElseResult153=V_Get143;(is_eq(Key, kw_initial_offset)->get_var(AEnv, sys_v, V_Get146), set_var(AEnv, sys_initial_offset, V_Get146), ElseResult151=V_Get146;get_var(AEnv, sys_o, O_Get147), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get147], ElseResult149), ElseResult151=ElseResult149), ElseResult153=ElseResult151), ElseResult155=ElseResult153), ElseResult156=ElseResult155), ElseResult158=ElseResult156), ElseResult160=ElseResult158), ElseResult162=ElseResult160), TrueResult187=ElseResult162), _40948=TrueResult187;get_var(AEnv, sys_os, Os_Get164), f_car(Os_Get164, PredArgResult166), (c0nz:is_consp(PredArgResult166)->get_var(AEnv, sys_os, Os_Get167), f_caar(Os_Get167, TrueResult169), set_var(AEnv, sys_o, TrueResult169), _41006=TrueResult169;get_var(AEnv, sys_os, Os_Get168), f_car(Os_Get168, ElseResult170), set_var(AEnv, sys_o, ElseResult170), _41006=ElseResult170), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_constructor)->get_var(AEnv, sys_constructors, Constructors_Get177), get_var(AEnv, sys_default_constructor, Get_var_Ret2371), TrueResult185=[Get_var_Ret2371|Constructors_Get177], set_var(AEnv, sys_constructors, TrueResult185), ElseResult188=TrueResult185;f_sys_memq(Key, [kw_conc_name, kw_copier, kw_predicate, kw_print_function], IFTEST178), (IFTEST178\==[]->ElseResult186=[];(is_eq(Key, kw_named)->set_var(AEnv, sys_named, t), ElseResult184=t;get_var(AEnv, sys_o, O_Get182), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get182], ElseResult183), ElseResult184=ElseResult183), ElseResult186=ElseResult184), ElseResult188=ElseResult186), _40948=ElseResult188), get_var(AEnv, sys_os, Os_Get189), f_cdr(Os_Get189, Cdr_Ret), set_var(AEnv, sys_os, Cdr_Ret), goto(do_label_15, AEnv), _40330=_GORES)))
					  ]),
			  []=LetResult75
			),
			block_exit([], LetResult75),
			true),
		  get_var(AEnv, sys_conc_name, Conc_name_Get),
		  f_string(Conc_name_Get, Intern_Param2252),
		  f_intern(Intern_Param2252, Conc_name2212),
		  set_var(AEnv, sys_conc_name, Conc_name2212),
		  get_var(AEnv, sys_slot_descriptions, Slot_descriptions_Get),
		  f_endp(Slot_descriptions_Get, PredArgResult315),
		  (   PredArgResult315==[]
		  ->  get_var(AEnv,
			      sys_slot_descriptions,
			      Slot_descriptions_Get316),
		      f_car(Slot_descriptions_Get316, Stringp_Param),
		      f_stringp(Stringp_Param, TrueResult317),
		      IFTEST310=TrueResult317
		  ;   IFTEST310=[]
		  ),
		  (   IFTEST310\==[]
		  ->  get_var(AEnv,
			      sys_slot_descriptions,
			      Slot_descriptions_Get318),
		      f_car(Slot_descriptions_Get318, Documentation),
		      set_var(AEnv, documentation, Documentation),
		      get_var(AEnv,
			      sys_slot_descriptions,
			      Slot_descriptions_Get319),
		      f_cdr(Slot_descriptions_Get319, TrueResult320),
		      set_var(AEnv, sys_slot_descriptions, TrueResult320),
		      _41214=TrueResult320
		  ;   _41214=[]
		  ),
		  get_var(AEnv, sys_include, IFTEST321),
		  (   IFTEST321\==[]
		  ->  get_var(AEnv, sys_include, Include_Get326),
		      get_var(AEnv, type, Type_Get),
		      f_car(Include_Get326, Get_sysprop_Param),
		      f_sys_get_sysprop(Get_sysprop_Param,
					sys_structure_type,
					[],
					PredArg2Result),
		      (   is_equal(Type_Get, PredArg2Result)
		      ->  TrueResult332=[]
		      ;   get_var(AEnv, sys_include, Include_Get330),
			  f_car(Include_Get330, Car_Ret),
			  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "~S is an illegal structure include."),
				    Car_Ret
				  ],
				  ElseResult331),
			  TrueResult332=ElseResult331
		      ),
		      _41488=TrueResult332
		  ;   _41488=[]
		  ),
		  get_var(AEnv, sys_include, IFTEST333),
		  (   IFTEST333\==[]
		  ->  get_var(AEnv, sys_include, Include_Get336),
		      f_car(Include_Get336, Get_sysprop_Param2255),
		      f_sys_get_sysprop(Get_sysprop_Param2255,
					sys_structure_offset,
					[],
					TrueResult337),
		      Offset=TrueResult337
		  ;   Offset=0
		  ),
		  set_var(AEnv, sys_offset, Offset),
		  get_var(AEnv, type, IFTEST340),
		  (   IFTEST340\==[]
		  ->  get_var(AEnv, sys_initial_of, IFTEST343),
		      (   IFTEST343\==[]
		      ->  get_var(AEnv, type, IFTEST350),
			  (   IFTEST350\==[]
			  ->  get_var(AEnv,
				      sys_initial_offset,
				      Initial_offset_Get),
			      IFTEST348=Initial_offset_Get
			  ;   IFTEST348=[]
			  ),
			  (   IFTEST348\==[]
			  ->  get_var(AEnv,
				      sys_initial_offset,
				      Initial_offset_Get356),
			      get_var(AEnv, sys_offset, Offset_Get),
			      'f_+'(Offset_Get,
				    Initial_offset_Get356,
				    TrueResult357),
			      set_var(AEnv, sys_offset, TrueResult357),
			      IFTEST346=TrueResult357
			  ;   IFTEST346=[]
			  ),
			  (   IFTEST346\==[]
			  ->  get_var(AEnv, type, IFTEST362),
			      (   IFTEST362\==[]
			      ->  get_var(AEnv, sys_named, Named_Get),
				  IFTEST360=Named_Get
			      ;   IFTEST360=[]
			      ),
			      (   IFTEST360\==[]
			      ->  get_var(AEnv, sys_offset, Offset_Get367),
				  set_var(AEnv, sys_name_offset, Offset_Get367),
				  get_var(AEnv, sys_offset, Offset_Get368),
				  'f_1+'(Offset_Get368, TrueResult369),
				  set_var(AEnv, sys_offset, TrueResult369),
				  IFTEST358=TrueResult369
			      ;   IFTEST358=[]
			      ),
			      (   IFTEST358\==[]
			      ->  get_var(AEnv,
					  sys_slot_descriptions,
					  Slot_descriptions_Get375),
				  BlockExitEnv=[bv(sys_ds, Slot_descriptions_Get375), bv(sys_sds, [])|AEnv],
				  catch(( call_addr_block(BlockExitEnv,
							  (push_label(do_label_16), get_var(BlockExitEnv, sys_ds, Ds_Get396), (s3q:is_endp(Ds_Get396)->get_var(BlockExitEnv, sys_sds, Sds_Get402), f_nreverse(Sds_Get402, RetResult399), set_var(BlockExitEnv, sys_slot_descriptions, RetResult399), throw(block_exit([], RetResult399)), _TBResult377=ThrowResult400;sf_push(BlockExitEnv, [sys_parse_slot_description, [car, sys_ds], sys_offset], sys_sds, Sds), get_var(BlockExitEnv, sys_offset, Offset_Get404), 'f_1+'(Offset_Get404, Offset2216), set_var(BlockExitEnv, sys_offset, Offset2216), get_var(BlockExitEnv, sys_ds, Ds_Get405), f_cdr(Ds_Get405, Ds), set_var(BlockExitEnv, sys_ds, Ds), goto(do_label_16, BlockExitEnv), _TBResult377=_GORES406)),
							  
							  [ addr(addr_tagbody_16_do_label_16,
								 do_label_16,
								 '$unused',
								 AEnv,
								 (get_var(AEnv, sys_ds, Ds_Get), (s3q:is_endp(Ds_Get)->get_var(AEnv, sys_sds, Nreverse_Param), f_nreverse(Nreverse_Param, RetResult382), set_var(AEnv, sys_slot_descriptions, RetResult382), throw(block_exit([], RetResult382)), _TBResult377=ThrowResult383;sf_push(AEnv, [sys_parse_slot_description, [car, sys_ds], sys_offset], sys_sds, Sf_push_Ret), get_var(AEnv, sys_offset, Offset_Get388), 'f_1+'(Offset_Get388, Set_var_Ret), set_var(AEnv, sys_offset, Set_var_Ret), get_var(AEnv, sys_ds, Ds_Get389), f_cdr(Ds_Get389, Cdr_Ret2376), set_var(AEnv, sys_ds, Cdr_Ret2376), goto(do_label_16, AEnv), _TBResult377=_GORES390)))
							  ]),
					  []=IFTEST370
					),
					block_exit([], IFTEST370),
					true),
				  (   IFTEST370\==[]
				  ->  get_var(AEnv, type, IFTEST414),
				      (   IFTEST414\==[]
				      ->  get_var(AEnv, sys_named, Named_Get417),
					  IFTEST412=Named_Get417
				      ;   IFTEST412=[]
				      ),
				      (   IFTEST412\==[]
				      ->  get_var(AEnv, sys_name, Name_Get419),
					  CAR=[[], Name_Get419],
					  get_var(AEnv,
						  sys_slot_descriptions,
						  Slot_descriptions_Get420),
					  TrueResult421=[CAR|Slot_descriptions_Get420],
					  set_var(AEnv,
						  sys_slot_descriptions,
						  TrueResult421),
					  IFTEST410=TrueResult421
				      ;   IFTEST410=[]
				      ),
				      (   IFTEST410\==[]
				      ->  get_var(AEnv, type, IFTEST426),
					  (   IFTEST426\==[]
					  ->  get_var(AEnv,
						      sys_initial_offset,
						      Initial_offset_Get429),
					      IFTEST424=Initial_offset_Get429
					  ;   IFTEST424=[]
					  ),
					  (   IFTEST424\==[]
					  ->  get_var(AEnv,
						      sys_initial_offset,
						      Initial_offset_Get431),
					      f_make_list(Initial_offset_Get431,
							  Append_Param),
					      get_var(AEnv,
						      sys_slot_descriptions,
						      Slot_descriptions_Get432),
					      f_append(Append_Param,
						       Slot_descriptions_Get432,
						       TrueResult433),
					      set_var(AEnv,
						      sys_slot_descriptions,
						      TrueResult433),
					      IFTEST422=TrueResult433
					  ;   IFTEST422=[]
					  ),
					  (   IFTEST422\==[]
					  ->  get_var(AEnv,
						      sys_slot_descriptions,
						      IFTEST434),
					      set_var(AEnv,
						      sys_local_slot_descriptions,
						      IFTEST434),
					      (   IFTEST434\==[]
					      ->  get_var(AEnv,
							  sys_include,
							  IFTEST439),
						  (   IFTEST439==[]
						  ->  IFTEST437=[]
						  ;   get_var(AEnv,
							      sys_include,
							      Include_Get443),
						      f_cdr(Include_Get443,
							    PredArgResult445),
						      (   s3q:is_endp(PredArgResult445)
						      ->  get_var(AEnv,
								  sys_include,
								  Include_Get446),
							  f_car(Include_Get446,
								Get_sysprop_Param2258),
							  f_sys_get_sysprop(Get_sysprop_Param2258,
									    sys_structure_slot_descriptions,
									    [],
									    Append_Param2259),
							  get_var(AEnv,
								  sys_slot_descriptions,
								  Slot_descriptions_Get447),
							  f_append(Append_Param2259,
								   Slot_descriptions_Get447,
								   TrueResult456),
							  set_var(AEnv,
								  sys_slot_descriptions,
								  TrueResult456),
							  ElseResult458=TrueResult456
						      ;   get_var(AEnv,
								  sys_include,
								  Include_Get453),
							  f_cdr(Include_Get453,
								Cdr_Ret2378),
							  f_mapcar(closure(kw_function,
									   
									   [ ClosureEnvironment
									   | AEnv
									   ],
									   Whole,
									   LResult,
									   [sys_sd],
									   (get_var(ClosureEnvironment, sys_sd, Sd_Get), f_sys_parse_slot_description(Sd_Get, 0, LResult)),
									   
									   [ lambda,
									     [sys_sd],
									     
									     [ sys_parse_slot_description,
									       sys_sd,
									       0
									     ]
									   ]),
								   [Cdr_Ret2378],
								   Slot_descriptions_Param),
							  get_var(AEnv,
								  sys_include,
								  Include_Get454),
							  f_car(Include_Get454,
								Get_sysprop_Param2260),
							  f_sys_get_sysprop(Get_sysprop_Param2260,
									    sys_structure_slot_descriptions,
									    [],
									    Get_sysprop_Ret),
							  f_sys_overwrite_slot_descriptions(Slot_descriptions_Param,
											    Get_sysprop_Ret,
											    Append_Param2262),
							  get_var(AEnv,
								  sys_slot_descriptions,
								  Slot_descriptions_Get455),
							  f_append(Append_Param2262,
								   Slot_descriptions_Get455,
								   ElseResult457),
							  set_var(AEnv,
								  sys_slot_descriptions,
								  ElseResult457),
							  ElseResult458=ElseResult457
						      ),
						      IFTEST437=ElseResult458
						  ),
						  (   IFTEST437\==[]
						  ->  get_var(AEnv,
							      sys_no_constructor,
							      IFTEST461),
						      (   IFTEST461\==[]
						      ->  get_var(AEnv,
								  sys_constructors,
								  IFTEST464),
							  (   IFTEST464\==[]
							  ->  f_error(
								      [ '$ARRAY'([*],
										 claz_base_character,
										 "Contradictory constructor options.")
								      ],
								      TrueResult467),
							      TrueResult474=TrueResult467
							  ;   TrueResult474=[]
							  ),
							  IFTEST459=TrueResult474
						      ;   get_var(AEnv,
								  sys_constructors,
								  IFTEST468),
							  (   IFTEST468==[]
							  ->  get_var(AEnv,
								      sys_default_constructor,
								      Default_constructor_Get471),
							      TrueResult472=[Default_constructor_Get471],
							      set_var(AEnv,
								      sys_constructors,
								      TrueResult472),
							      ElseResult475=TrueResult472
							  ;   ElseResult473=[],
							      ElseResult475=ElseResult473
							  ),
							  IFTEST459=ElseResult475
						      ),
						      (   IFTEST459\==[]
						      ->  get_var(AEnv,
								  type,
								  IFTEST480),
							  (   IFTEST480\==[]
							  ->  get_var(AEnv,
								      sys_named,
								      Named_Get483),
							      f_not(Named_Get483,
								    TrueResult484),
							      IFTEST478=TrueResult484
							  ;   IFTEST478=[]
							  ),
							  (   IFTEST478\==[]
							  ->  get_var(AEnv,
								      sys_predicate_specified,
								      IFTEST485),
							      (   IFTEST485\==[]
							      ->  get_var(AEnv,
									  sys_predicate,
									  Predicate_Get),
								  f_error(
									  [ '$ARRAY'([*],
										     claz_base_character,
										     "~S is an illegal structure predicate."),
									    Predicate_Get
									  ],
									  TrueResult489),
								  _45474=TrueResult489
							      ;   _45474=[]
							      ),
							      set_var(AEnv,
								      sys_predicate,
								      []),
							      IFTEST476=[]
							  ;   IFTEST476=[]
							  ),
							  (   IFTEST476\==[]
							  ->  get_var(AEnv,
								      sys_include,
								      IFTEST492),
							      (   IFTEST492\==[]
							      ->  get_var(AEnv,
									  sys_include,
									  Include_Get495),
								  f_car(Include_Get495,
									TrueResult496),
								  set_var(AEnv,
									  sys_include,
									  TrueResult496),
								  IFTEST490=TrueResult496
							      ;   IFTEST490=[]
							      ),
							      (   IFTEST490\==[]
							      ->  get_var(AEnv,
									  sys_print_function,
									  IFTEST501),
								  (   IFTEST501\==[]
								  ->  get_var(AEnv,
									      type,
									      Type_Get504),
								      IFTEST499=Type_Get504
								  ;   IFTEST499=[]
								  ),
								  (   IFTEST499\==[]
								  ->  f_error(
									      [ '$ARRAY'([*],
											 claz_base_character,
											 "An print function is supplied to a typed structure.")
									      ],
									      TrueResult506),
								      IFTEST497=TrueResult506
								  ;   IFTEST497=[]
								  ),
								  (   IFTEST497\==[]
								  ->  (   get_var(AEnv,
										  type,
										  Type_Get509),
									  Type_Get509\==[],
									  IFTEST507=Type_Get509
								      ->  true
								      ;   get_var(AEnv,
										  xx_features_xx,
										  Xx_features_xx_Get),
									  f_member(kw_clos,
										   Xx_features_xx_Get,
										   [],
										   Not_Param2263),
									  f_not(Not_Param2263,
										Not_Ret),
									  IFTEST507=Not_Ret
								      ),
								      (   IFTEST507\==[]
								      ->  get_var(AEnv,
										  documentation,
										  Documentation_Get),
									  get_var(AEnv,
										  sys_conc_name,
										  Conc_name_Get513),
									  get_var(AEnv,
										  sys_constructors,
										  Constructors_Get521),
									  get_var(AEnv,
										  sys_copier,
										  Copier_Get),
									  get_var(AEnv,
										  sys_include,
										  Include_Get519),
									  get_var(AEnv,
										  sys_name,
										  Name_Get512),
									  get_var(AEnv,
										  sys_named,
										  Named_Get515),
									  get_var(AEnv,
										  sys_offset,
										  Offset_Get522),
									  get_var(AEnv,
										  sys_print_function,
										  Print_function_Get520),
									  get_var(AEnv,
										  sys_slot_descriptions,
										  Slot_descriptions_Get517),
									  get_var(AEnv,
										  sys_slots,
										  Slots_Get516),
									  get_var(AEnv,
										  type,
										  Type_Get514),
									  f_mapcar(closure(kw_function,
											   
											   [ ClosureEnvironment531
											   | AEnv
											   ],
											   Whole532,
											   LResult529,
											   [sys_constructor],
											   (get_var(ClosureEnvironment531, sys_constructor, Constructor_Get), get_var(ClosureEnvironment531, sys_name, Name_Get524), get_var(ClosureEnvironment531, sys_named, Named_Get527), get_var(ClosureEnvironment531, sys_slot_descriptions, Slot_descriptions_Get528), get_var(ClosureEnvironment531, type, Type_Get526), f_sys_make_constructor(Name_Get524, Constructor_Get, Type_Get526, Named_Get527, Slot_descriptions_Get528, LResult529)),
											   
											   [ lambda,
											     [sys_constructor],
											     
											     [ sys_make_constructor,
											       sys_name,
											       sys_constructor,
											       type,
											       sys_named,
											       sys_slot_descriptions
											     ]
											   ]),
										   
										   [ Constructors_Get521
										   ],
										   Mapcar_Ret),
									  get_var(AEnv,
										  sys_predicate,
										  IFTEST534),
									  (   IFTEST534\==[]
									  ->  get_var(AEnv,
										      sys_name,
										      Name_Get538),
									      get_var(AEnv,
										      sys_name_offset,
										      Name_offset_Get),
									      get_var(AEnv,
										      sys_named,
										      Named_Get540),
									      get_var(AEnv,
										      sys_predicate,
										      Predicate_Get537),
									      get_var(AEnv,
										      type,
										      Type_Get539),
									      TrueResult542=[[sys_fset, [quote, Predicate_Get537], [sys_make_predicate, [quote, Name_Get538], [quote, Type_Get539], [quote, Named_Get540], [quote, Name_offset_Get]]]],
									      Bq_append_Param=TrueResult542
									  ;   Bq_append_Param=[]
									  ),
									  get_var(AEnv,
										  sys_name,
										  Name_Get543),
									  bq_append(Bq_append_Param,
										    
										    [ [quote, Name_Get543]
										    ],
										    Bq_append_Ret),
									  bq_append(
										    [ 
										      [ sys_define_structure,
											[quote, Name_Get512],
											
											[ quote,
											  Conc_name_Get513
											],
											[quote, Type_Get514],
											
											[ quote,
											  Named_Get515
											],
											
											[ quote,
											  Slots_Get516
											],
											
											[ quote,
											  Slot_descriptions_Get517
											],
											[quote, Copier_Get],
											
											[ quote,
											  Include_Get519
											],
											
											[ quote,
											  Print_function_Get520
											],
											
											[ quote,
											  Constructors_Get521
											],
											
											[ quote,
											  Offset_Get522
											],
											
											[ quote,
											  Documentation_Get
											]
										      ]
										    | Mapcar_Ret
										    ],
										    Bq_append_Ret,
										    Bq_append_Ret2383),
									  TrueResult601=[eval_when, [compile, load, eval]|Bq_append_Ret2383]
								      ;   get_var(AEnv,
										  sys_name,
										  Name_Get544),
									  (   get_var(AEnv,
										      sys_include,
										      Include_Get545),
									      Include_Get545\==[],
									      CAR2392=Include_Get545
									  ->  true
									  ;   CAR2392=structure_object
									  ),
									  get_var(AEnv,
										  sys_local_slot_descriptions,
										  Local_slot_descriptions_Get),
									  f_mapcar(closure(kw_function,
											   
											   [ ClosureEnvironment561
											   | AEnv
											   ],
											   Whole562,
											   LResult559,
											   [sys_sd],
											   (get_var(ClosureEnvironment561, sys_sd, IFTEST547), (IFTEST547\==[]->get_var(ClosureEnvironment561, sys_sd, Sd_Get550), f_car(Sd_Get550, List_xx_Param), get_var(ClosureEnvironment561, sys_sd, Sd_Get551), f_second(Sd_Get551, Initform), get_var(ClosureEnvironment561, sys_sd, Sd_Get552), f_car(Sd_Get552, Symbol_name_Param), f_symbol_name(Symbol_name_Param, Intern_Param2266), f_find_package(keyword, Find_package_Ret), f_intern(Intern_Param2266, Find_package_Ret, Intern_Ret), get_var(ClosureEnvironment561, sys_sd, Sd_Get555), f_third(Sd_Get555, IFTEST553), (IFTEST553\==[]->get_var(ClosureEnvironment561, sys_sd, Sd_Get556), f_third(Sd_Get556, Third_Ret), TrueResult557=[kw_type, Third_Ret], _47556=TrueResult557;_47556=[]), f_list_xx(List_xx_Param, kw_initform, Initform, kw_initarg, Intern_Ret, _47556, TrueResult558), LResult559=TrueResult558;LResult559=[])),
											   
											   [ lambda,
											     [sys_sd],
											     
											     [ if,
											       sys_sd,
											       
											       [ list_xx,
												 [first, sys_sd],
												 kw_initform,
												 [second, sys_sd],
												 kw_initarg,
												 
												 [ intern,
												   
												   [ symbol_name,
												     [first, sys_sd]
												   ],
												   
												   [ find_package,
												     [quote, keyword]
												   ]
												 ],
												 
												 [ when,
												   [third, sys_sd],
												   
												   [ list,
												     kw_type,
												     [third, sys_sd]
												   ]
												 ]
											       ],
											       []
											     ]
											   ]),
										   
										   [ Local_slot_descriptions_Get
										   ],
										   Mapcar_Ret2384),
									  get_var(AEnv,
										  sys_print_function,
										  IFTEST564),
									  (   IFTEST564\==[]
									  ->  get_var(AEnv,
										      sys_name,
										      Name_Get567),
									      get_var(AEnv,
										      sys_print_function,
										      Print_function_Get568),
									      CDR=[[defmethod, print_object, [[sys_obj, Name_Get567], stream], [Print_function_Get568, sys_obj, stream, xx_print_level_xx]]]
									  ;   CDR=[]
									  ),
									  get_var(AEnv,
										  documentation,
										  Documentation_Get580),
									  get_var(AEnv,
										  sys_conc_name,
										  Conc_name_Get570),
									  get_var(AEnv,
										  sys_constructors,
										  Constructors_Get578),
									  get_var(AEnv,
										  sys_copier,
										  Copier_Get575),
									  get_var(AEnv,
										  sys_include,
										  Include_Get576),
									  get_var(AEnv,
										  sys_name,
										  Name_Get569),
									  get_var(AEnv,
										  sys_named,
										  Named_Get572),
									  get_var(AEnv,
										  sys_offset,
										  Offset_Get579),
									  get_var(AEnv,
										  sys_print_function,
										  Print_function_Get577),
									  get_var(AEnv,
										  sys_slot_descriptions,
										  Slot_descriptions_Get574),
									  get_var(AEnv,
										  sys_slots,
										  Slots_Get573),
									  get_var(AEnv,
										  type,
										  Type_Get571),
									  f_mapcar(closure(kw_function,
											   
											   [ ClosureEnvironment588
											   | AEnv
											   ],
											   Whole589,
											   LResult586,
											   [sys_constructor],
											   (get_var(ClosureEnvironment588, sys_constructor, Constructor_Get582), get_var(ClosureEnvironment588, sys_name, Name_Get581), get_var(ClosureEnvironment588, sys_named, Named_Get584), get_var(ClosureEnvironment588, sys_slot_descriptions, Slot_descriptions_Get585), get_var(ClosureEnvironment588, type, Type_Get583), f_sys_make_constructor(Name_Get581, Constructor_Get582, Type_Get583, Named_Get584, Slot_descriptions_Get585, LResult586)),
											   
											   [ lambda,
											     [sys_constructor],
											     
											     [ sys_make_constructor,
											       sys_name,
											       sys_constructor,
											       type,
											       sys_named,
											       sys_slot_descriptions
											     ]
											   ]),
										   
										   [ Constructors_Get578
										   ],
										   Mapcar_Ret2388),
									  get_var(AEnv,
										  sys_predicate,
										  IFTEST591),
									  (   IFTEST591\==[]
									  ->  get_var(AEnv,
										      sys_name,
										      Name_Get595),
									      get_var(AEnv,
										      sys_name_offset,
										      Name_offset_Get598),
									      get_var(AEnv,
										      sys_named,
										      Named_Get597),
									      get_var(AEnv,
										      sys_predicate,
										      Predicate_Get594),
									      get_var(AEnv,
										      type,
										      Type_Get596),
									      TrueResult599=[[sys_fset, [quote, Predicate_Get594], [sys_make_predicate, [quote, Name_Get595], [quote, Type_Get596], [quote, Named_Get597], [quote, Name_offset_Get598]]]],
									      Bq_append_Param2268=TrueResult599
									  ;   Bq_append_Param2268=[]
									  ),
									  get_var(AEnv,
										  sys_name,
										  Name_Get600),
									  bq_append(Bq_append_Param2268,
										    
										    [ [quote, Name_Get600]
										    ],
										    Bq_append_Ret2389),
									  bq_append(
										    [ 
										      [ sys_define_structure,
											[quote, Name_Get569],
											
											[ quote,
											  Conc_name_Get570
											],
											[quote, Type_Get571],
											
											[ quote,
											  Named_Get572
											],
											
											[ quote,
											  Slots_Get573
											],
											
											[ quote,
											  Slot_descriptions_Get574
											],
											
											[ quote,
											  Copier_Get575
											],
											
											[ quote,
											  Include_Get576
											],
											
											[ quote,
											  Print_function_Get577
											],
											
											[ quote,
											  Constructors_Get578
											],
											
											[ quote,
											  Offset_Get579
											],
											
											[ quote,
											  Documentation_Get580
											]
										      ]
										    | Mapcar_Ret2388
										    ],
										    Bq_append_Ret2389,
										    Bq_append_Ret2390),
									  bq_append(
										    [ 
										      [ defclass,
											Name_Get544,
											[CAR2392],
											Mapcar_Ret2384,
											
											[ kw_metaclass,
											  structure_class
											]
										      ]
										    | CDR
										    ],
										    Bq_append_Ret2390,
										    Bq_append_Ret2391),
									  TrueResult601=[eval_when, [compile, load, eval]|Bq_append_Ret2391]
								      ),
								      TrueResult602=TrueResult601
								  ;   TrueResult602=[]
								  ),
								  TrueResult603=TrueResult602
							      ;   TrueResult603=[]
							      ),
							      TrueResult604=TrueResult603
							  ;   TrueResult604=[]
							  ),
							  TrueResult605=TrueResult604
						      ;   TrueResult605=[]
						      ),
						      TrueResult606=TrueResult605
						  ;   TrueResult606=[]
						  ),
						  TrueResult607=TrueResult606
					      ;   TrueResult607=[]
					      ),
					      TrueResult608=TrueResult607
					  ;   TrueResult608=[]
					  ),
					  TrueResult609=TrueResult608
				      ;   TrueResult609=[]
				      ),
				      TrueResult610=TrueResult609
				  ;   TrueResult610=[]
				  ),
				  TrueResult611=TrueResult610
			      ;   TrueResult611=[]
			      ),
			      TrueResult612=TrueResult611
			  ;   TrueResult612=[]
			  ),
			  TrueResult613=TrueResult612
		      ;   TrueResult613=[]
		      ),
		      IFTEST338=TrueResult613
		  ;   IFTEST338=[]
		  ),
		  (   IFTEST338\==[]
		  ->  _41890=[]
		  ;   _41890=[]
		  ),
		  assert_lsp(sys_sharp_s_reader,
			     wl:lambda_def(defun, sys_sharp_s_reader, f_sys_sharp_s_reader, [stream, subchar, arg], [[declare, [ignore, subchar]], [when, [and, arg, [null, xx_read_suppress_xx]], [error, '$ARRAY'([*], claz_base_character, "An extra argument was supplied for the #S readmacro.")]], [let, [[sys_l, [read, stream]]], [unless, [sys_get_sysprop, [car, sys_l], [quote, sys_is_a_structure]], [error, '$ARRAY'([*], claz_base_character, "~S is not a structure."), [car, sys_l]]], [do, [[sys_ll, [cdr, sys_l], [cddr, sys_ll]]], [[endp, sys_ll], [do, [[sys_cs, [sys_get_sysprop, [car, sys_l], [quote, sys_structure_constructors]], [cdr, sys_cs]]], [[endp, sys_cs], [error, '$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), [car, sys_l]]], [when, [symbolp, [car, sys_cs]], [return, [apply, [car, sys_cs], [cdr, sys_l]]]]]], [rplaca, sys_ll, [intern, [string, [car, sys_ll]], [quote, keyword]]]]]])),
		  assert_lsp(sys_sharp_s_reader,
			     wl:arglist_info(sys_sharp_s_reader, f_sys_sharp_s_reader, [stream, subchar, arg], arginfo{all:[stream, subchar, arg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, subchar, arg], opt:0, req:[stream, subchar, arg], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_sharp_s_reader,
			     wl:init_args(x, f_sys_sharp_s_reader)),
		  assert_lsp(sys_sharp_s_reader,
			     (f_sys_sharp_s_reader(Stream_In, Subchar_In, Arg_In, FnResult):-GEnv2219=[bv(stream, Stream_In), bv(subchar, Subchar_In), bv(arg, Arg_In)], catch(((sf_declare(GEnv2219, [ignore, subchar], Sf_declare_Ret), get_var(GEnv2219, arg, IFTEST622), (IFTEST622\==[]->get_var(GEnv2219, xx_read_suppress_xx, Xx_read_suppress_xx_Get), f_null(Xx_read_suppress_xx_Get, TrueResult626), IFTEST620=TrueResult626;IFTEST620=[]), (IFTEST620\==[]->f_error(['$ARRAY'([*], claz_base_character, "An extra argument was supplied for the #S readmacro.")], TrueResult627), _49756=TrueResult627;_49756=[]), get_var(GEnv2219, stream, Stream_Get), f_read(Stream_Get, L_Init), LEnv630=[bv(sys_l, L_Init)|GEnv2219], get_var(LEnv630, sys_l, L_Get), f_car(L_Get, Get_sysprop_Param2269), f_sys_get_sysprop(Get_sysprop_Param2269, sys_is_a_structure, [], IFTEST633), (IFTEST633\==[]->_50042=[];get_var(LEnv630, sys_l, L_Get636), f_car(L_Get636, Car_Ret2395), f_error(['$ARRAY'([*], claz_base_character, "~S is not a structure."), Car_Ret2395], ElseResult637), _50042=ElseResult637), get_var(LEnv630, sys_l, L_Get641), f_cdr(L_Get641, Ll_Init), AEnv=[bv(sys_ll, Ll_Init)|LEnv630], catch((call_addr_block(AEnv,  (push_label(do_label_17), get_var(AEnv, sys_ll, Ll_Get715), (s3q:is_endp(Ll_Get715)->get_var(AEnv, sys_l, L_Get723), f_car(L_Get723, Get_sysprop_Param2270), f_sys_get_sysprop(Get_sysprop_Param2270, sys_structure_constructors, [], Cs_Init724), AEnv=[bv(sys_cs, Cs_Init724)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_19), get_var(AEnv, sys_cs, Cs_Get752), (s3q:is_endp(Cs_Get752)->get_var(AEnv, sys_l, L_Get757), f_car(L_Get757, Car_Ret2396), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret2396], RetResult755), throw(block_exit([], RetResult755)), _TBResult725=ThrowResult756;get_var(AEnv, sys_cs, Cs_Get760), f_car(Cs_Get760, PredArgResult762), (is_symbolp(PredArgResult762)->get_var(AEnv, sys_cs, Cs_Get765), f_car(Cs_Get765, Apply_Param), get_var(AEnv, sys_l, L_Get766), f_cdr(L_Get766, Cdr_Ret2397), f_apply(Apply_Param, Cdr_Ret2397, RetResult763), throw(block_exit([], RetResult763)), _52982=ThrowResult764;_52982=[]), get_var(AEnv, sys_cs, Cs_Get769), f_cdr(Cs_Get769, Cs), set_var(AEnv, sys_cs, Cs), goto(do_label_19, AEnv), _TBResult725=_GORES770)), [addr(addr_tagbody_19_do_label_19, do_label_19, '$unused', AEnv,  (get_var(AEnv, sys_cs, Cs_Get727), (s3q:is_endp(Cs_Get727)->get_var(AEnv, sys_l, L_Get732), f_car(L_Get732, Car_Ret2398), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret2398], RetResult730), throw(block_exit([], RetResult730)), _TBResult725=ThrowResult731;get_var(AEnv, sys_cs, Cs_Get735), f_car(Cs_Get735, PredArgResult737), (is_symbolp(PredArgResult737)->get_var(AEnv, sys_cs, Cs_Get740), f_car(Cs_Get740, Apply_Param2272), get_var(AEnv, sys_l, L_Get741), f_cdr(L_Get741, Cdr_Ret2399), f_apply(Apply_Param2272, Cdr_Ret2399, RetResult738), throw(block_exit([], RetResult738)), _53482=ThrowResult739;_53482=[]), get_var(AEnv, sys_cs, Cs_Get745), f_cdr(Cs_Get745, Cdr_Ret2400), set_var(AEnv, sys_cs, Cdr_Ret2400), goto(do_label_19, AEnv), _TBResult725=_GORES746)))]), []=RetResult718), block_exit([], RetResult718), true), throw(block_exit([], RetResult718)), _TBResult643=ThrowResult719;get_var(AEnv, sys_ll, Ll_Get775), f_car(Ll_Get775, String_Param), f_string(String_Param, Intern_Param2274), f_intern(Intern_Param2274, keyword, Keyword), f_rplaca(Ll_Get775, Keyword, Rplaca_Ret), get_var(AEnv, sys_ll, Ll_Get778), f_cddr(Ll_Get778, Ll), set_var(AEnv, sys_ll, Ll), goto(do_label_17, AEnv), _TBResult643=_GORES779)), [addr(addr_tagbody_17_do_label_17, do_label_17, '$unused', AEnv,  (get_var(AEnv, sys_ll, Ll_Get), (s3q:is_endp(Ll_Get)->get_var(AEnv, sys_l, L_Get653), f_car(L_Get653, Get_sysprop_Param2275), f_sys_get_sysprop(Get_sysprop_Param2275, sys_structure_constructors, [], Get_sysprop_Ret2402), AEnv=[bv(sys_cs, Get_sysprop_Ret2402)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_18), get_var(AEnv, sys_cs, Cs_Get682), (s3q:is_endp(Cs_Get682)->get_var(AEnv, sys_l, L_Get687), f_car(L_Get687, Car_Ret2403), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret2403], RetResult685), throw(block_exit([], RetResult685)), _TBResult655=ThrowResult686;get_var(AEnv, sys_cs, Cs_Get690), f_car(Cs_Get690, PredArgResult692), (is_symbolp(PredArgResult692)->get_var(AEnv, sys_cs, Cs_Get695), f_car(Cs_Get695, Apply_Param2276), get_var(AEnv, sys_l, L_Get696), f_cdr(L_Get696, Cdr_Ret2404), f_apply(Apply_Param2276, Cdr_Ret2404, RetResult693), throw(block_exit([], RetResult693)), _53924=ThrowResult694;_53924=[]), get_var(AEnv, sys_cs, Cs_Get699), f_cdr(Cs_Get699, Cdr_Ret2405), set_var(AEnv, sys_cs, Cdr_Ret2405), goto(do_label_18, AEnv), _TBResult655=_GORES700)), [addr(addr_tagbody_18_do_label_18, do_label_18, '$unused', AEnv,  (get_var(AEnv, sys_cs, Cs_Get), (s3q:is_endp(Cs_Get)->get_var(AEnv, sys_l, L_Get662), f_car(L_Get662, Car_Ret2406), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret2406], RetResult660), throw(block_exit([], RetResult660)), _TBResult655=ThrowResult661;get_var(AEnv, sys_cs, Cs_Get665), f_car(Cs_Get665, PredArgResult667), (is_symbolp(PredArgResult667)->get_var(AEnv, sys_cs, Cs_Get670), f_car(Cs_Get670, Apply_Param2277), get_var(AEnv, sys_l, L_Get671), f_cdr(L_Get671, Cdr_Ret2407), f_apply(Apply_Param2277, Cdr_Ret2407, RetResult668), throw(block_exit([], RetResult668)), _54130=ThrowResult669;_54130=[]), get_var(AEnv, sys_cs, Cs_Get675), f_cdr(Cs_Get675, Cdr_Ret2408), set_var(AEnv, sys_cs, Cdr_Ret2408), goto(do_label_18, AEnv), _TBResult655=_GORES676)))]), []=RetResult648), block_exit([], RetResult648), true), throw(block_exit([], RetResult648)), _TBResult643=ThrowResult649;get_var(AEnv, sys_ll, Ll_Get705), f_car(Ll_Get705, String_Param2278), f_string(String_Param2278, Intern_Param2279), f_intern(Intern_Param2279, keyword, Intern_Ret2409), f_rplaca(Ll_Get705, Intern_Ret2409, Rplaca_Ret2410), get_var(AEnv, sys_ll, Ll_Get708), f_cddr(Ll_Get708, Cddr_Ret), set_var(AEnv, sys_ll, Cddr_Ret), goto(do_label_17, AEnv), _TBResult643=_GORES709)))]), []=LetResult629), block_exit([], LetResult629), true)), LetResult629=FnResult), block_exit(sys_sharp_s_reader, FnResult), true))),
		  set_opv(sys_sharp_s_reader,
			  symbol_function,
			  f_sys_sharp_s_reader),
		  DefunResult=sys_sharp_s_reader,
		  assert_lsp(sys_make_constructor,
			     wl:lambda_def(defun, sys_make_constructor, f_sys_make_constructor, [sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], [[declare, [ignore, sys_named]], [let, [[sys_slot_names, [mapcar, function([lambda, [x], [cond, [[null, x], []], [[null, [car, x]], [list, [quote, quote], [cadr, x]]], [t, [car, x]]]]), sys_slot_descriptions]], [sys_keys, [mapcan, function([lambda, [x], [cond, [[null, x], []], [[null, [car, x]], []], [[null, [cadr, x]], [list, [car, x]]], [t, [list, [list, [car, x], [cadr, x]]]]]]), sys_slot_descriptions]]], [cond, [[consp, sys_constructor], [do, [[a, [cadr, sys_constructor], [cdr, a]], [sys_l, []], [sys_vs, []]], [[endp, a], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]]], [cond, [[eq, [car, a], [quote, c38_optional]], [setq, sys_l, [cons, [quote, c38_optional], sys_l]], [do, [[sys_aa, [cdr, a], [cdr, sys_aa]], [sys_ov], [sys_y]], [[endp, sys_aa], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]], [when, [member, [car, sys_aa], lambda_list_keywords], [when, [eq, [car, sys_aa], [quote, c38_rest]], [setq, sys_l, [cons, [quote, c38_rest], sys_l]], [setq, sys_aa, [cdr, sys_aa]], [unless, [and, [not, [endp, sys_aa]], [symbolp, [car, sys_aa]]], [sys_illegal_boa]], [setq, sys_vs, [cons, [car, sys_aa], sys_vs]], [setq, sys_l, [cons, [car, sys_aa], sys_l]], [setq, sys_aa, [cdr, sys_aa]], [when, [endp, sys_aa], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]]], [unless, [eq, [car, sys_aa], [quote, c38_aux]], [sys_illegal_boa]], [setq, sys_l, [cons, [quote, c38_aux], sys_l]], [do, [[sys_aaa, [cdr, sys_aa], [cdr, sys_aaa]]], [[endp, sys_aaa]], [setq, sys_l, [cons, [car, sys_aaa], sys_l]], [cond, [[and, [atom, [car, sys_aaa]], [symbolp, [car, sys_aaa]]], [setq, sys_vs, [cons, [car, sys_aaa], sys_vs]]], [[and, [symbolp, [caar, sys_aaa]], [or, [endp, [cdar, sys_aaa]], [endp, [cddar, sys_aaa]]]], [setq, sys_vs, [cons, [caar, sys_aaa], sys_vs]]], [t, [sys_illegal_boa]]]], [setq, sys_keys, [nreconc, sys_l, [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [setq, sys_keys, [nreconc, sys_l, [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]], [if, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [x], [if, [consp, x], [car, x]]])]]], [setq, sys_l, [cons, [car, sys_y], sys_l]], [setq, sys_l, [cons, [car, sys_aa], sys_l]]], [cond, [[atom, [car, sys_aa]], [unless, [symbolp, [car, sys_aa]], [sys_illegal_boa]], [setq, sys_vs, [cons, [car, sys_aa], sys_vs]]], [[not, [symbolp, [caar, sys_aa]]], [sys_illegal_boa]], [[or, [endp, [cdar, sys_aa]], [endp, [cddar, sys_aa]]], [setq, sys_vs, [cons, [caar, sys_aa], sys_vs]]], [[not, [symbolp, [caddar, sys_aa]]], [sys_illegal_boa]], [[not, [endp, [cdddar, sys_aa]]], [sys_illegal_boa]], [t, [setq, sys_vs, [cons, [caar, sys_aa], sys_vs]], [setq, sys_vs, [cons, [caddar, sys_aa], sys_vs]]]]], [return, []]], [t, [unless, [symbolp, [car, a]], [sys_illegal_boa]], [setq, sys_l, [cons, [car, a], sys_l]], [setq, sys_vs, [cons, [car, a], sys_vs]]]]], [setq, sys_constructor, [car, sys_constructor]]], [t, [setq, sys_keys, [cons, [quote, c38_key], sys_keys]]]], [cond, [[null, type], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [sys_make_structure, [quote, ['#COMMA', sys_name]], ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[or, [eq, type, [quote, vector]], [and, [consp, type], [eq, [car, type], [quote, vector]]]], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [vector, ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[eq, type, [quote, list]], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [list, ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure type"), type]]]]])),
		  assert_lsp(sys_make_constructor,
			     wl:arglist_info(sys_make_constructor, f_sys_make_constructor, [sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], arginfo{all:[sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], opt:0, req:[sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], rest:0, sublists:0, whole:0})),
		  assert_lsp(sys_make_constructor,
			     wl:init_args(x, f_sys_make_constructor)),
		  assert_lsp(sys_make_constructor,
			     (f_sys_make_constructor(Name_In787, Constructor_In, Type_In, Named_In, Slot_descriptions_In, FnResult785):-GEnv2223=[bv(sys_name, Name_In787), bv(sys_constructor, Constructor_In), bv(type, Type_In), bv(sys_named, Named_In), bv(sys_slot_descriptions, Slot_descriptions_In)], catch(((sf_declare(GEnv2223, [ignore, sys_named], Sf_declare_Ret2412), get_var(GEnv2223, sys_slot_descriptions, Slot_descriptions_Get810), f_mapcar(closure(kw_function, [ClosureEnvironment808|GEnv2223], Whole809, LResult806, [x],  (get_var(ClosureEnvironment808, x, IFTEST795), (IFTEST795==[]->LResult806=[];get_var(ClosureEnvironment808, x, X_Get800), f_car(X_Get800, IFTEST798), (IFTEST798==[]->get_var(ClosureEnvironment808, x, X_Get801), f_cadr(X_Get801, Cadr_Ret), TrueResult803=[quote, Cadr_Ret], ElseResult805=TrueResult803;get_var(ClosureEnvironment808, x, X_Get802), f_car(X_Get802, ElseResult804), ElseResult805=ElseResult804), LResult806=ElseResult805)), [lambda, [x], [cond, [[null, x], []], [[null, [car, x]], [list, [quote, quote], [cadr, x]]], [t, [car, x]]]]), [Slot_descriptions_Get810], Slot_names_Init), get_var(GEnv2223, sys_slot_descriptions, Slot_descriptions_Get831), f_mapcan(closure(kw_function, [ClosureEnvironment829|GEnv2223], Whole830, LResult827, [x],  (get_var(ClosureEnvironment829, x, IFTEST811), (IFTEST811==[]->LResult827=[];get_var(ClosureEnvironment829, x, X_Get816), f_car(X_Get816, IFTEST814), (IFTEST814==[]->ElseResult826=[];get_var(ClosureEnvironment829, x, X_Get819), f_cadr(X_Get819, IFTEST817), (IFTEST817==[]->get_var(ClosureEnvironment829, x, X_Get820), f_car(X_Get820, Car_Ret2414), TrueResult823=[Car_Ret2414], ElseResult825=TrueResult823;get_var(ClosureEnvironment829, x, X_Get821), f_car(X_Get821, Car_Ret2415), get_var(ClosureEnvironment829, x, X_Get822), f_cadr(X_Get822, Cadr_Ret2416), CAR2417=[Car_Ret2415, Cadr_Ret2416], ElseResult824=[CAR2417], ElseResult825=ElseResult824), ElseResult826=ElseResult825), LResult827=ElseResult826)), [lambda, [x], [cond, [[null, x], []], [[null, [car, x]], []], [[null, [cadr, x]], [list, [car, x]]], [t, [list, [list, [car, x], [cadr, x]]]]]]), Slot_descriptions_Get831, Keys_Init), LEnv794=[bv(sys_slot_names, Slot_names_Init), bv(sys_keys, Keys_Init)|GEnv2223], get_var(LEnv794, sys_constructor, Constructor_Get835), (c0nz:is_consp(Constructor_Get835)->get_var(LEnv794, sys_constructor, Constructor_Get841), f_cadr(Constructor_Get841, A_Init), BlockExitEnv=[bv(a, A_Init), bv(sys_l, []), bv(sys_vs, [])|LEnv794], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_20), get_var(BlockExitEnv, a, A_Get1503), (s3q:is_endp(A_Get1503)->get_var(BlockExitEnv, sys_l, L_Get1509), Nreconc_Param=[c38_aux|L_Get1509], get_var(BlockExitEnv, sys_keys, Keys_Get1527), f_mapcan(closure(kw_function, [ClosureEnvironment1525|BlockExitEnv], Whole1526, LResult1523, [sys_k],  (get_var(ClosureEnvironment1525, sys_k, K_Get1513), (K_Get1513\=[CAR2419|CDR2420]->get_var(ClosureEnvironment1525, sys_k, K_Get1516), Member_Param=K_Get1516;get_var(ClosureEnvironment1525, sys_k, K_Get1517), f_car(K_Get1517, ElseResult1519), Member_Param=ElseResult1519), get_var(ClosureEnvironment1525, sys_vs, Vs_Get1520), f_member(Member_Param, Vs_Get1520, [], IFTEST1510), (IFTEST1510\==[]->LResult1523=[];get_var(ClosureEnvironment1525, sys_k, K_Get1521), ElseResult1522=[K_Get1521], LResult1523=ElseResult1522)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1527, Mapcan_Ret), f_nreconc(Nreconc_Param, Mapcan_Ret, RetResult1506), set_var(BlockExitEnv, sys_keys, RetResult1506), throw(block_exit([], RetResult1506)), _TBResult843=ThrowResult1507;get_var(BlockExitEnv, a, A_Get1530), f_car(A_Get1530, PredArg1Result1532), (is_eq(PredArg1Result1532, c38_optional)->get_var(BlockExitEnv, sys_l, L_Get1533), L=[c38_optional|L_Get1533], set_var(BlockExitEnv, sys_l, L), get_var(BlockExitEnv, a, A_Get1537), f_cdr(A_Get1537, Aa_Init1538), BlockExitEnv=[bv(sys_aa, Aa_Init1538), bv([sys_ov], []), bv([sys_y], [])|BlockExitEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_24), get_var(BlockExitEnv, sys_aa, Aa_Get1842), (s3q:is_endp(Aa_Get1842)->get_var(BlockExitEnv, sys_l, L_Get1848), Nreconc_Param2283=[c38_aux|L_Get1848], get_var(BlockExitEnv, sys_keys, Keys_Get1866), f_mapcan(closure(kw_function, [ClosureEnvironment1864|BlockExitEnv], Whole1865, LResult1862, [sys_k],  (get_var(ClosureEnvironment1864, sys_k, K_Get1852), (K_Get1852\=[CAR2422|CDR2423]->get_var(ClosureEnvironment1864, sys_k, K_Get1855), Member_Param2282=K_Get1855;get_var(ClosureEnvironment1864, sys_k, K_Get1856), f_car(K_Get1856, ElseResult1858), Member_Param2282=ElseResult1858), get_var(ClosureEnvironment1864, sys_vs, Vs_Get1859), f_member(Member_Param2282, Vs_Get1859, [], IFTEST1849), (IFTEST1849\==[]->LResult1862=[];get_var(ClosureEnvironment1864, sys_k, K_Get1860), ElseResult1861=[K_Get1860], LResult1862=ElseResult1861)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1866, Mapcan_Ret2421), f_nreconc(Nreconc_Param2283, Mapcan_Ret2421, Keys), set_var(BlockExitEnv, sys_keys, Keys), throw(block_exit([], [])), throw(block_exit([], RetResult1845)), _TBResult1539=ThrowResult1846;get_var(BlockExitEnv, sys_aa, Aa_Get1872), f_car(Aa_Get1872, Member_Param2284), get_var(BlockExitEnv, lambda_list_keywords, Lambda_list_keywords_Get1873), f_member(Member_Param2284, Lambda_list_keywords_Get1873, [], IFTEST1870), (IFTEST1870\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1875), f_car(Aa_Get1875, PredArg1Result1877), (is_eq(PredArg1Result1877, c38_rest)->get_var(BlockExitEnv, sys_l, L_Get1878), L2226=[c38_rest|L_Get1878], set_var(BlockExitEnv, sys_l, L2226), get_var(BlockExitEnv, sys_aa, Aa_Get1879), f_cdr(Aa_Get1879, Aa), set_var(BlockExitEnv, sys_aa, Aa), get_var(BlockExitEnv, sys_aa, Aa_Get1883), f_endp(Aa_Get1883, PredArgResult1885), (PredArgResult1885==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1886), f_car(Aa_Get1886, Symbolp_Param), f_symbolp(Symbolp_Param, TrueResult1887), IFTEST1880=TrueResult1887;IFTEST1880=[]), (IFTEST1880\==[]->_85576=[];f_sys_illegal_boa(ElseResult1888), _85576=ElseResult1888), get_var(BlockExitEnv, sys_aa, Aa_Get1889), f_car(Aa_Get1889, Car_Ret2424), get_var(BlockExitEnv, sys_vs, Vs_Get1890), Vs=[Car_Ret2424|Vs_Get1890], set_var(BlockExitEnv, sys_vs, Vs), get_var(BlockExitEnv, sys_aa, Aa_Get1891), f_car(Aa_Get1891, Car_Ret2425), get_var(BlockExitEnv, sys_l, L_Get1892), L2229=[Car_Ret2425|L_Get1892], set_var(BlockExitEnv, sys_l, L2229), get_var(BlockExitEnv, sys_aa, Aa_Get1893), f_cdr(Aa_Get1893, Aa2230), set_var(BlockExitEnv, sys_aa, Aa2230), get_var(BlockExitEnv, sys_aa, Aa_Get1895), (s3q:is_endp(Aa_Get1895)->get_var(BlockExitEnv, sys_l, L_Get1898), Nreconc_Param2287=[c38_aux|L_Get1898], get_var(BlockExitEnv, sys_keys, Keys_Get1916), f_mapcan(closure(kw_function, [ClosureEnvironment1914|BlockExitEnv], Whole1915, LResult1912, [sys_k],  (get_var(ClosureEnvironment1914, sys_k, K_Get1902), (K_Get1902\=[CAR2427|CDR2428]->get_var(ClosureEnvironment1914, sys_k, K_Get1905), Member_Param2286=K_Get1905;get_var(ClosureEnvironment1914, sys_k, K_Get1906), f_car(K_Get1906, ElseResult1908), Member_Param2286=ElseResult1908), get_var(ClosureEnvironment1914, sys_vs, Vs_Get1909), f_member(Member_Param2286, Vs_Get1909, [], IFTEST1899), (IFTEST1899\==[]->LResult1912=[];get_var(ClosureEnvironment1914, sys_k, K_Get1910), ElseResult1911=[K_Get1910], LResult1912=ElseResult1911)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1916, Mapcan_Ret2426), f_nreconc(Nreconc_Param2287, Mapcan_Ret2426, Keys2231), set_var(BlockExitEnv, sys_keys, Keys2231), throw(block_exit([], [])), TrueResult1920=ThrowResult1918;TrueResult1920=[]), _85414=TrueResult1920;_85414=[]), get_var(BlockExitEnv, sys_aa, Aa_Get1922), f_car(Aa_Get1922, PredArg1Result1924), (is_eq(PredArg1Result1924, c38_aux)->_86834=[];f_sys_illegal_boa(ElseResult1925), _86834=ElseResult1925), get_var(BlockExitEnv, sys_l, L_Get1926), L2232=[c38_aux|L_Get1926], set_var(BlockExitEnv, sys_l, L2232), get_var(BlockExitEnv, sys_aa, Aa_Get1930), f_cdr(Aa_Get1930, Aaa_Init1931), AEnv=[bv(sys_aaa, Aaa_Init1931)|BlockExitEnv], catch((call_addr_block(AEnv,  (push_label(do_label_26), get_var(AEnv, sys_aaa, Aaa_Get1976), (s3q:is_endp(Aaa_Get1976)->throw(block_exit([], [])), _TBResult1932=ThrowResult1980;get_var(AEnv, sys_aaa, Aaa_Get1983), f_car(Aaa_Get1983, Car_Ret2429), get_var(AEnv, sys_l, L_Get1984), L2233=[Car_Ret2429|L_Get1984], set_var(AEnv, sys_l, L2233), get_var(AEnv, sys_aaa, Aaa_Get1988), f_car(Aaa_Get1988, PredArgResult1990), (PredArgResult1990\=[CAR2430|CDR2431]->get_var(AEnv, sys_aaa, Aaa_Get1991), f_car(Aaa_Get1991, Symbolp_Param2288), f_symbolp(Symbolp_Param2288, TrueResult1992), IFTEST1985=TrueResult1992;IFTEST1985=[]), (IFTEST1985\==[]->get_var(AEnv, sys_aaa, Aaa_Get1993), f_car(Aaa_Get1993, Car_Ret2432), get_var(AEnv, sys_vs, Vs_Get1994), TrueResult2009=[Car_Ret2432|Vs_Get1994], set_var(AEnv, sys_vs, TrueResult2009), _88352=TrueResult2009;get_var(AEnv, sys_aaa, Aaa_Get1998), f_caar(Aaa_Get1998, PredArgResult2000), (is_symbolp(PredArgResult2000)->(get_var(AEnv, sys_aaa, Aaa_Get2001), f_cdar(Aaa_Get2001, Endp_Param2289), f_endp(Endp_Param2289, FORM1_Res2003), FORM1_Res2003\==[], TrueResult2004=FORM1_Res2003->true;get_var(AEnv, sys_aaa, Aaa_Get2002), f_cddar(Aaa_Get2002, Endp_Param2290), f_endp(Endp_Param2290, Endp_Ret), TrueResult2004=Endp_Ret), IFTEST1995=TrueResult2004;IFTEST1995=[]), (IFTEST1995\==[]->get_var(AEnv, sys_aaa, Aaa_Get2005), f_caar(Aaa_Get2005, Caar_Ret2434), get_var(AEnv, sys_vs, Vs_Get2006), TrueResult2007=[Caar_Ret2434|Vs_Get2006], set_var(AEnv, sys_vs, TrueResult2007), ElseResult2010=TrueResult2007;f_sys_illegal_boa(ElseResult2008), ElseResult2010=ElseResult2008), _88352=ElseResult2010), get_var(AEnv, sys_aaa, Aaa_Get2011), f_cdr(Aaa_Get2011, Aaa), set_var(AEnv, sys_aaa, Aaa), goto(do_label_26, AEnv), _TBResult1932=_GORES2012)), [addr(addr_tagbody_26_do_label_26, do_label_26, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get1934), (s3q:is_endp(Aaa_Get1934)->throw(block_exit([], [])), _TBResult1932=ThrowResult1938;get_var(AEnv, sys_aaa, Aaa_Get1941), f_car(Aaa_Get1941, Car_Ret2435), get_var(AEnv, sys_l, L_Get1942), Set_var_Ret2436=[Car_Ret2435|L_Get1942], set_var(AEnv, sys_l, Set_var_Ret2436), get_var(AEnv, sys_aaa, Aaa_Get1946), f_car(Aaa_Get1946, PredArgResult1948), (PredArgResult1948\=[CAR2437|CDR2438]->get_var(AEnv, sys_aaa, Aaa_Get1949), f_car(Aaa_Get1949, Symbolp_Param2291), f_symbolp(Symbolp_Param2291, TrueResult1950), IFTEST1943=TrueResult1950;IFTEST1943=[]), (IFTEST1943\==[]->get_var(AEnv, sys_aaa, Aaa_Get1951), f_car(Aaa_Get1951, Car_Ret2439), get_var(AEnv, sys_vs, Vs_Get1952), TrueResult1967=[Car_Ret2439|Vs_Get1952], set_var(AEnv, sys_vs, TrueResult1967), _89420=TrueResult1967;get_var(AEnv, sys_aaa, Aaa_Get1956), f_caar(Aaa_Get1956, PredArgResult1958), (is_symbolp(PredArgResult1958)->(get_var(AEnv, sys_aaa, Aaa_Get1959), f_cdar(Aaa_Get1959, Endp_Param2292), f_endp(Endp_Param2292, FORM1_Res1961), FORM1_Res1961\==[], TrueResult1962=FORM1_Res1961->true;get_var(AEnv, sys_aaa, Aaa_Get1960), f_cddar(Aaa_Get1960, Endp_Param2293), f_endp(Endp_Param2293, Endp_Ret2440), TrueResult1962=Endp_Ret2440), IFTEST1953=TrueResult1962;IFTEST1953=[]), (IFTEST1953\==[]->get_var(AEnv, sys_aaa, Aaa_Get1963), f_caar(Aaa_Get1963, Caar_Ret2441), get_var(AEnv, sys_vs, Vs_Get1964), TrueResult1965=[Caar_Ret2441|Vs_Get1964], set_var(AEnv, sys_vs, TrueResult1965), ElseResult1968=TrueResult1965;f_sys_illegal_boa(ElseResult1966), ElseResult1968=ElseResult1966), _89420=ElseResult1968), get_var(AEnv, sys_aaa, Aaa_Get1969), f_cdr(Aaa_Get1969, Cdr_Ret2442), set_var(AEnv, sys_aaa, Cdr_Ret2442), goto(do_label_26, AEnv), _TBResult1932=_GORES1970)))]), []=LetResult1928), block_exit([], LetResult1928), true), get_var(BlockExitEnv, sys_keys, Keys_Get2034), get_var(BlockExitEnv, sys_l, L_Get2016), f_mapcan(closure(kw_function, [ClosureEnvironment2032|BlockExitEnv], Whole2033, LResult2030, [sys_k],  (get_var(ClosureEnvironment2032, sys_k, K_Get2020), (K_Get2020\=[CAR2444|CDR2445]->get_var(ClosureEnvironment2032, sys_k, K_Get2023), Member_Param2294=K_Get2023;get_var(ClosureEnvironment2032, sys_k, K_Get2024), f_car(K_Get2024, ElseResult2026), Member_Param2294=ElseResult2026), get_var(ClosureEnvironment2032, sys_vs, Vs_Get2027), f_member(Member_Param2294, Vs_Get2027, [], IFTEST2017), (IFTEST2017\==[]->LResult2030=[];get_var(ClosureEnvironment2032, sys_k, K_Get2028), ElseResult2029=[K_Get2028], LResult2030=ElseResult2029)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get2034, Mapcan_Ret2443), f_nreconc(L_Get2016, Mapcan_Ret2443, Keys2235), set_var(BlockExitEnv, sys_keys, Keys2235), get_var(BlockExitEnv, sys_keys, Keys_Get2053), get_var(BlockExitEnv, sys_l, L_Get2035), f_mapcan(closure(kw_function, [ClosureEnvironment2051|BlockExitEnv], Whole2052, LResult2049, [sys_k],  (get_var(ClosureEnvironment2051, sys_k, K_Get2039), (K_Get2039\=[CAR2447|CDR2448]->get_var(ClosureEnvironment2051, sys_k, K_Get2042), Member_Param2295=K_Get2042;get_var(ClosureEnvironment2051, sys_k, K_Get2043), f_car(K_Get2043, ElseResult2045), Member_Param2295=ElseResult2045), get_var(ClosureEnvironment2051, sys_vs, Vs_Get2046), f_member(Member_Param2295, Vs_Get2046, [], IFTEST2036), (IFTEST2036\==[]->LResult2049=[];get_var(ClosureEnvironment2051, sys_k, K_Get2047), ElseResult2048=[K_Get2047], LResult2049=ElseResult2048)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get2053, Mapcan_Ret2446), f_nreconc(L_Get2035, Mapcan_Ret2446, Keys2236), set_var(BlockExitEnv, sys_keys, Keys2236), throw(block_exit([], [])), _85312=ThrowResult2055;_85312=[]), get_var(BlockExitEnv, sys_aa, Aa_Get2062), f_car(Aa_Get2062, PredArgResult2064), (PredArgResult2064\=[CAR2449|CDR2450]->get_var(BlockExitEnv, sys_aa, Aa_Get2065), f_car(Aa_Get2065, Ov), set_var(BlockExitEnv, sys_ov, Ov), IFTEST2059=t;get_var(BlockExitEnv, sys_aa, Aa_Get2067), f_cdar(Aa_Get2067, PredArgResult2069), (s3q:is_endp(PredArgResult2069)->get_var(BlockExitEnv, sys_aa, Aa_Get2070), f_caar(Aa_Get2070, Ov2238), set_var(BlockExitEnv, sys_ov, Ov2238), ElseResult2071=t;ElseResult2071=[]), IFTEST2059=ElseResult2071), (IFTEST2059\==[]->get_var(BlockExitEnv, sys_keys, Keys_Get2073), get_var(BlockExitEnv, sys_ov, Ov_Get2072), f_member(Ov_Get2072, Keys_Get2073, [kw_key, closure(kw_function, [ClosureEnvironment2082|BlockExitEnv], Whole2083, LResult2080, [x],  (get_var(ClosureEnvironment2082, x, X_Get2075), (c0nz:is_consp(X_Get2075)->get_var(ClosureEnvironment2082, x, X_Get2078), f_car(X_Get2078, TrueResult2079), LResult2080=TrueResult2079;LResult2080=[])), [lambda, [x], [if, [consp, x], [car, x]]])], TrueResult2084), set_var(BlockExitEnv, sys_y, TrueResult2084), IFTEST2057=TrueResult2084;IFTEST2057=[]), (IFTEST2057\==[]->get_var(BlockExitEnv, sys_y, Y_Get2085), f_car(Y_Get2085, Car_Ret2451), get_var(BlockExitEnv, sys_l, L_Get2086), TrueResult2089=[Car_Ret2451|L_Get2086], set_var(BlockExitEnv, sys_l, TrueResult2089), _91092=TrueResult2089;get_var(BlockExitEnv, sys_aa, Aa_Get2087), f_car(Aa_Get2087, Car_Ret2452), get_var(BlockExitEnv, sys_l, L_Get2088), ElseResult2090=[Car_Ret2452|L_Get2088], set_var(BlockExitEnv, sys_l, ElseResult2090), _91092=ElseResult2090), get_var(BlockExitEnv, sys_aa, Aa_Get2092), f_car(Aa_Get2092, PredArgResult2094), (PredArgResult2094\=[CAR2453|CDR2454]->get_var(BlockExitEnv, sys_aa, Aa_Get2096), f_car(Aa_Get2096, PredArgResult2098), (is_symbolp(PredArgResult2098)->_92174=[];f_sys_illegal_boa(ElseResult2099), _92174=ElseResult2099), get_var(BlockExitEnv, sys_aa, Aa_Get2100), f_car(Aa_Get2100, Car_Ret2455), get_var(BlockExitEnv, sys_vs, Vs_Get2101), TrueResult2133=[Car_Ret2455|Vs_Get2101], set_var(BlockExitEnv, sys_vs, TrueResult2133), _85310=TrueResult2133;get_var(BlockExitEnv, sys_aa, Aa_Get2103), f_caar(Aa_Get2103, Symbolp_Param2296), f_symbolp(Symbolp_Param2296, PredArgResult2105), (PredArgResult2105==[]->f_sys_illegal_boa(TrueResult2131), ElseResult2134=TrueResult2131;(get_var(BlockExitEnv, sys_aa, Aa_Get2108), f_cdar(Aa_Get2108, Endp_Param2297), f_endp(Endp_Param2297, FORM1_Res2110), FORM1_Res2110\==[], IFTEST2106=FORM1_Res2110->true;get_var(BlockExitEnv, sys_aa, Aa_Get2109), f_cddar(Aa_Get2109, Endp_Param2298), f_endp(Endp_Param2298, Endp_Ret2456), IFTEST2106=Endp_Ret2456), (IFTEST2106\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get2111), f_caar(Aa_Get2111, Caar_Ret2457), get_var(BlockExitEnv, sys_vs, Vs_Get2112), TrueResult2129=[Caar_Ret2457|Vs_Get2112], set_var(BlockExitEnv, sys_vs, TrueResult2129), ElseResult2132=TrueResult2129;get_var(BlockExitEnv, sys_aa, Aa_Get2114), f_caddar(Aa_Get2114, Symbolp_Param2299), f_symbolp(Symbolp_Param2299, PredArgResult2116), (PredArgResult2116==[]->f_sys_illegal_boa(TrueResult2127), ElseResult2130=TrueResult2127;get_var(BlockExitEnv, sys_aa, Aa_Get2118), f_cdddar(Aa_Get2118, Endp_Param2300), f_endp(Endp_Param2300, PredArgResult2120), (PredArgResult2120==[]->f_sys_illegal_boa(TrueResult2125), ElseResult2128=TrueResult2125;get_var(BlockExitEnv, sys_aa, Aa_Get2121), f_caar(Aa_Get2121, Caar_Ret2458), get_var(BlockExitEnv, sys_vs, Vs_Get2122), Vs2239=[Caar_Ret2458|Vs_Get2122], set_var(BlockExitEnv, sys_vs, Vs2239), get_var(BlockExitEnv, sys_aa, Aa_Get2123), f_caddar(Aa_Get2123, Caddar_Ret), get_var(BlockExitEnv, sys_vs, Vs_Get2124), ElseResult2126=[Caddar_Ret|Vs_Get2124], set_var(BlockExitEnv, sys_vs, ElseResult2126), ElseResult2128=ElseResult2126), ElseResult2130=ElseResult2128), ElseResult2132=ElseResult2130), ElseResult2134=ElseResult2132), _85310=ElseResult2134), get_var(BlockExitEnv, sys_aa, Aa_Get2135), f_cdr(Aa_Get2135, Aa2240), set_var(BlockExitEnv, sys_aa, Aa2240), goto(do_label_24, BlockExitEnv), _TBResult1539=_GORES2136)), [addr(addr_tagbody_24_do_label_24, do_label_24, '$unused', AEnv,  (get_var(AEnv, sys_aa, Aa_Get1541), (s3q:is_endp(Aa_Get1541)->get_var(AEnv, sys_l, L_Get1547), Nreconc_Param2302=[c38_aux|L_Get1547], get_var(AEnv, sys_keys, Keys_Get1565), f_mapcan(closure(kw_function, [ClosureEnvironment1563|AEnv], Whole1564, LResult1561, [sys_k],  (get_var(ClosureEnvironment1563, sys_k, K_Get1551), (K_Get1551\=[CAR2461|CDR2462]->get_var(ClosureEnvironment1563, sys_k, K_Get1554), Member_Param2301=K_Get1554;get_var(ClosureEnvironment1563, sys_k, K_Get1555), f_car(K_Get1555, ElseResult1557), Member_Param2301=ElseResult1557), get_var(ClosureEnvironment1563, sys_vs, Vs_Get1558), f_member(Member_Param2301, Vs_Get1558, [], IFTEST1548), (IFTEST1548\==[]->LResult1561=[];get_var(ClosureEnvironment1563, sys_k, K_Get1559), ElseResult1560=[K_Get1559], LResult1561=ElseResult1560)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1565, Mapcan_Ret2460), f_nreconc(Nreconc_Param2302, Mapcan_Ret2460, Nreconc_Ret), set_var(AEnv, sys_keys, Nreconc_Ret), throw(block_exit([], [])), throw(block_exit([], RetResult1544)), _TBResult1539=ThrowResult1545;get_var(AEnv, sys_aa, Aa_Get1571), f_car(Aa_Get1571, Member_Param2303), get_var(AEnv, lambda_list_keywords, Lambda_list_keywords_Get1572), f_member(Member_Param2303, Lambda_list_keywords_Get1572, [], IFTEST1569), (IFTEST1569\==[]->get_var(AEnv, sys_aa, Aa_Get1574), f_car(Aa_Get1574, PredArg1Result1576), (is_eq(PredArg1Result1576, c38_rest)->get_var(AEnv, sys_l, L_Get1578), Set_var_Ret2464=[c38_rest|L_Get1578], set_var(AEnv, sys_l, Set_var_Ret2464), get_var(AEnv, sys_aa, Aa_Get1579), f_cdr(Aa_Get1579, Cdr_Ret2465), set_var(AEnv, sys_aa, Cdr_Ret2465), get_var(AEnv, sys_aa, Aa_Get1583), f_endp(Aa_Get1583, PredArgResult1585), (PredArgResult1585==[]->get_var(AEnv, sys_aa, Aa_Get1586), f_car(Aa_Get1586, Symbolp_Param2304), f_symbolp(Symbolp_Param2304, TrueResult1587), IFTEST1580=TrueResult1587;IFTEST1580=[]), (IFTEST1580\==[]->_93782=[];f_sys_illegal_boa(ElseResult1588), _93782=ElseResult1588), get_var(AEnv, sys_aa, Aa_Get1589), f_car(Aa_Get1589, Car_Ret2466), get_var(AEnv, sys_vs, Vs_Get1590), Set_var_Ret2467=[Car_Ret2466|Vs_Get1590], set_var(AEnv, sys_vs, Set_var_Ret2467), get_var(AEnv, sys_aa, Aa_Get1591), f_car(Aa_Get1591, Car_Ret2468), get_var(AEnv, sys_l, L_Get1592), Set_var_Ret2469=[Car_Ret2468|L_Get1592], set_var(AEnv, sys_l, Set_var_Ret2469), get_var(AEnv, sys_aa, Aa_Get1593), f_cdr(Aa_Get1593, Cdr_Ret2470), set_var(AEnv, sys_aa, Cdr_Ret2470), get_var(AEnv, sys_aa, Aa_Get1595), (s3q:is_endp(Aa_Get1595)->get_var(AEnv, sys_l, L_Get1598), Nreconc_Param2306=[c38_aux|L_Get1598], get_var(AEnv, sys_keys, Keys_Get1616), f_mapcan(closure(kw_function, [ClosureEnvironment1614|AEnv], Whole1615, LResult1612, [sys_k],  (get_var(ClosureEnvironment1614, sys_k, K_Get1602), (K_Get1602\=[CAR2472|CDR2473]->get_var(ClosureEnvironment1614, sys_k, K_Get1605), Member_Param2305=K_Get1605;get_var(ClosureEnvironment1614, sys_k, K_Get1606), f_car(K_Get1606, ElseResult1608), Member_Param2305=ElseResult1608), get_var(ClosureEnvironment1614, sys_vs, Vs_Get1609), f_member(Member_Param2305, Vs_Get1609, [], IFTEST1599), (IFTEST1599\==[]->LResult1612=[];get_var(ClosureEnvironment1614, sys_k, K_Get1610), ElseResult1611=[K_Get1610], LResult1612=ElseResult1611)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1616, Mapcan_Ret2471), f_nreconc(Nreconc_Param2306, Mapcan_Ret2471, Nreconc_Ret2474), set_var(AEnv, sys_keys, Nreconc_Ret2474), throw(block_exit([], [])), TrueResult1620=ThrowResult1618;TrueResult1620=[]), _94114=TrueResult1620;_94114=[]), get_var(AEnv, sys_aa, Aa_Get1622), f_car(Aa_Get1622, PredArg1Result1624), (is_eq(PredArg1Result1624, c38_aux)->_94144=[];f_sys_illegal_boa(ElseResult1625), _94144=ElseResult1625), get_var(AEnv, sys_l, L_Get1626), Set_var_Ret2475=[c38_aux|L_Get1626], set_var(AEnv, sys_l, Set_var_Ret2475), get_var(AEnv, sys_aa, Aa_Get1630), f_cdr(Aa_Get1630, Aaa_Init1631), AEnv=[bv(sys_aaa, Aaa_Init1631)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_25), get_var(AEnv, sys_aaa, Aaa_Get1676), (s3q:is_endp(Aaa_Get1676)->throw(block_exit([], [])), _TBResult1632=ThrowResult1680;get_var(AEnv, sys_aaa, Aaa_Get1683), f_car(Aaa_Get1683, Car_Ret2476), get_var(AEnv, sys_l, L_Get1684), Set_var_Ret2477=[Car_Ret2476|L_Get1684], set_var(AEnv, sys_l, Set_var_Ret2477), get_var(AEnv, sys_aaa, Aaa_Get1688), f_car(Aaa_Get1688, PredArgResult1690), (PredArgResult1690\=[CAR2478|CDR2479]->get_var(AEnv, sys_aaa, Aaa_Get1691), f_car(Aaa_Get1691, Symbolp_Param2307), f_symbolp(Symbolp_Param2307, TrueResult1692), IFTEST1685=TrueResult1692;IFTEST1685=[]), (IFTEST1685\==[]->get_var(AEnv, sys_aaa, Aaa_Get1693), f_car(Aaa_Get1693, Car_Ret2480), get_var(AEnv, sys_vs, Vs_Get1694), TrueResult1709=[Car_Ret2480|Vs_Get1694], set_var(AEnv, sys_vs, TrueResult1709), _94412=TrueResult1709;get_var(AEnv, sys_aaa, Aaa_Get1698), f_caar(Aaa_Get1698, PredArgResult1700), (is_symbolp(PredArgResult1700)->(get_var(AEnv, sys_aaa, Aaa_Get1701), f_cdar(Aaa_Get1701, Endp_Param2308), f_endp(Endp_Param2308, FORM1_Res1703), FORM1_Res1703\==[], TrueResult1704=FORM1_Res1703->true;get_var(AEnv, sys_aaa, Aaa_Get1702), f_cddar(Aaa_Get1702, Endp_Param2309), f_endp(Endp_Param2309, Endp_Ret2481), TrueResult1704=Endp_Ret2481), IFTEST1695=TrueResult1704;IFTEST1695=[]), (IFTEST1695\==[]->get_var(AEnv, sys_aaa, Aaa_Get1705), f_caar(Aaa_Get1705, Caar_Ret2482), get_var(AEnv, sys_vs, Vs_Get1706), TrueResult1707=[Caar_Ret2482|Vs_Get1706], set_var(AEnv, sys_vs, TrueResult1707), ElseResult1710=TrueResult1707;f_sys_illegal_boa(ElseResult1708), ElseResult1710=ElseResult1708), _94412=ElseResult1710), get_var(AEnv, sys_aaa, Aaa_Get1711), f_cdr(Aaa_Get1711, Cdr_Ret2483), set_var(AEnv, sys_aaa, Cdr_Ret2483), goto(do_label_25, AEnv), _TBResult1632=_GORES1712)), [addr(addr_tagbody_25_do_label_25, do_label_25, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get1634), (s3q:is_endp(Aaa_Get1634)->throw(block_exit([], [])), _TBResult1632=ThrowResult1638;get_var(AEnv, sys_aaa, Aaa_Get1641), f_car(Aaa_Get1641, Car_Ret2484), get_var(AEnv, sys_l, L_Get1642), Set_var_Ret2485=[Car_Ret2484|L_Get1642], set_var(AEnv, sys_l, Set_var_Ret2485), get_var(AEnv, sys_aaa, Aaa_Get1646), f_car(Aaa_Get1646, PredArgResult1648), (PredArgResult1648\=[CAR2486|CDR2487]->get_var(AEnv, sys_aaa, Aaa_Get1649), f_car(Aaa_Get1649, Symbolp_Param2310), f_symbolp(Symbolp_Param2310, TrueResult1650), IFTEST1643=TrueResult1650;IFTEST1643=[]), (IFTEST1643\==[]->get_var(AEnv, sys_aaa, Aaa_Get1651), f_car(Aaa_Get1651, Car_Ret2488), get_var(AEnv, sys_vs, Vs_Get1652), TrueResult1667=[Car_Ret2488|Vs_Get1652], set_var(AEnv, sys_vs, TrueResult1667), _94828=TrueResult1667;get_var(AEnv, sys_aaa, Aaa_Get1656), f_caar(Aaa_Get1656, PredArgResult1658), (is_symbolp(PredArgResult1658)->(get_var(AEnv, sys_aaa, Aaa_Get1659), f_cdar(Aaa_Get1659, Endp_Param2311), f_endp(Endp_Param2311, FORM1_Res1661), FORM1_Res1661\==[], TrueResult1662=FORM1_Res1661->true;get_var(AEnv, sys_aaa, Aaa_Get1660), f_cddar(Aaa_Get1660, Endp_Param2312), f_endp(Endp_Param2312, Endp_Ret2489), TrueResult1662=Endp_Ret2489), IFTEST1653=TrueResult1662;IFTEST1653=[]), (IFTEST1653\==[]->get_var(AEnv, sys_aaa, Aaa_Get1663), f_caar(Aaa_Get1663, Caar_Ret2490), get_var(AEnv, sys_vs, Vs_Get1664), TrueResult1665=[Caar_Ret2490|Vs_Get1664], set_var(AEnv, sys_vs, TrueResult1665), ElseResult1668=TrueResult1665;f_sys_illegal_boa(ElseResult1666), ElseResult1668=ElseResult1666), _94828=ElseResult1668), get_var(AEnv, sys_aaa, Aaa_Get1669), f_cdr(Aaa_Get1669, Cdr_Ret2491), set_var(AEnv, sys_aaa, Cdr_Ret2491), goto(do_label_25, AEnv), _TBResult1632=_GORES1670)))]), []=LetResult1628), block_exit([], LetResult1628), true), get_var(AEnv, sys_keys, Keys_Get1734), get_var(AEnv, sys_l, L_Get1716), f_mapcan(closure(kw_function, [ClosureEnvironment1732|AEnv], Whole1733, LResult1730, [sys_k],  (get_var(ClosureEnvironment1732, sys_k, K_Get1720), (K_Get1720\=[CAR2493|CDR2494]->get_var(ClosureEnvironment1732, sys_k, K_Get1723), Member_Param2313=K_Get1723;get_var(ClosureEnvironment1732, sys_k, K_Get1724), f_car(K_Get1724, ElseResult1726), Member_Param2313=ElseResult1726), get_var(ClosureEnvironment1732, sys_vs, Vs_Get1727), f_member(Member_Param2313, Vs_Get1727, [], IFTEST1717), (IFTEST1717\==[]->LResult1730=[];get_var(ClosureEnvironment1732, sys_k, K_Get1728), ElseResult1729=[K_Get1728], LResult1730=ElseResult1729)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1734, Mapcan_Ret2492), f_nreconc(L_Get1716, Mapcan_Ret2492, Nreconc_Ret2495), set_var(AEnv, sys_keys, Nreconc_Ret2495), get_var(AEnv, sys_keys, Keys_Get1753), get_var(AEnv, sys_l, L_Get1735), f_mapcan(closure(kw_function, [ClosureEnvironment1751|AEnv], Whole1752, LResult1749, [sys_k],  (get_var(ClosureEnvironment1751, sys_k, K_Get1739), (K_Get1739\=[CAR2497|CDR2498]->get_var(ClosureEnvironment1751, sys_k, K_Get1742), Member_Param2314=K_Get1742;get_var(ClosureEnvironment1751, sys_k, K_Get1743), f_car(K_Get1743, ElseResult1745), Member_Param2314=ElseResult1745), get_var(ClosureEnvironment1751, sys_vs, Vs_Get1746), f_member(Member_Param2314, Vs_Get1746, [], IFTEST1736), (IFTEST1736\==[]->LResult1749=[];get_var(ClosureEnvironment1751, sys_k, K_Get1747), ElseResult1748=[K_Get1747], LResult1749=ElseResult1748)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1753, Mapcan_Ret2496), f_nreconc(L_Get1735, Mapcan_Ret2496, Nreconc_Ret2499), set_var(AEnv, sys_keys, Nreconc_Ret2499), throw(block_exit([], [])), _95434=ThrowResult1755;_95434=[]), get_var(AEnv, sys_aa, Aa_Get1762), f_car(Aa_Get1762, PredArgResult1764), (PredArgResult1764\=[CAR2500|CDR2501]->get_var(AEnv, sys_aa, Aa_Get1765), f_car(Aa_Get1765, Car_Ret2502), set_var(AEnv, sys_ov, Car_Ret2502), IFTEST1759=t;get_var(AEnv, sys_aa, Aa_Get1767), f_cdar(Aa_Get1767, PredArgResult1769), (s3q:is_endp(PredArgResult1769)->get_var(AEnv, sys_aa, Aa_Get1770), f_caar(Aa_Get1770, Caar_Ret2503), set_var(AEnv, sys_ov, Caar_Ret2503), ElseResult1771=t;ElseResult1771=[]), IFTEST1759=ElseResult1771), (IFTEST1759\==[]->get_var(AEnv, sys_keys, Keys_Get1773), get_var(AEnv, sys_ov, Ov_Get1772), f_member(Ov_Get1772, Keys_Get1773, [kw_key, closure(kw_function, [ClosureEnvironment1782|AEnv], Whole1783, LResult1780, [x],  (get_var(ClosureEnvironment1782, x, X_Get1775), (c0nz:is_consp(X_Get1775)->get_var(ClosureEnvironment1782, x, X_Get1778), f_car(X_Get1778, TrueResult1779), LResult1780=TrueResult1779;LResult1780=[])), [lambda, [x], [if, [consp, x], [car, x]]])], TrueResult1784), set_var(AEnv, sys_y, TrueResult1784), IFTEST1757=TrueResult1784;IFTEST1757=[]), (IFTEST1757\==[]->get_var(AEnv, sys_y, Y_Get1785), f_car(Y_Get1785, Car_Ret2504), get_var(AEnv, sys_l, L_Get1786), TrueResult1789=[Car_Ret2504|L_Get1786], set_var(AEnv, sys_l, TrueResult1789), _95754=TrueResult1789;get_var(AEnv, sys_aa, Aa_Get1787), f_car(Aa_Get1787, Car_Ret2505), get_var(AEnv, sys_l, L_Get1788), ElseResult1790=[Car_Ret2505|L_Get1788], set_var(AEnv, sys_l, ElseResult1790), _95754=ElseResult1790), get_var(AEnv, sys_aa, Aa_Get1792), f_car(Aa_Get1792, PredArgResult1794), (PredArgResult1794\=[CAR2506|CDR2507]->get_var(AEnv, sys_aa, Aa_Get1796), f_car(Aa_Get1796, PredArgResult1798), (is_symbolp(PredArgResult1798)->_95860=[];f_sys_illegal_boa(ElseResult1799), _95860=ElseResult1799), get_var(AEnv, sys_aa, Aa_Get1800), f_car(Aa_Get1800, Car_Ret2508), get_var(AEnv, sys_vs, Vs_Get1801), TrueResult1833=[Car_Ret2508|Vs_Get1801], set_var(AEnv, sys_vs, TrueResult1833), _95920=TrueResult1833;get_var(AEnv, sys_aa, Aa_Get1803), f_caar(Aa_Get1803, Symbolp_Param2315), f_symbolp(Symbolp_Param2315, PredArgResult1805), (PredArgResult1805==[]->f_sys_illegal_boa(TrueResult1831), ElseResult1834=TrueResult1831;(get_var(AEnv, sys_aa, Aa_Get1808), f_cdar(Aa_Get1808, Endp_Param2316), f_endp(Endp_Param2316, FORM1_Res1810), FORM1_Res1810\==[], IFTEST1806=FORM1_Res1810->true;get_var(AEnv, sys_aa, Aa_Get1809), f_cddar(Aa_Get1809, Endp_Param2317), f_endp(Endp_Param2317, Endp_Ret2509), IFTEST1806=Endp_Ret2509), (IFTEST1806\==[]->get_var(AEnv, sys_aa, Aa_Get1811), f_caar(Aa_Get1811, Caar_Ret2510), get_var(AEnv, sys_vs, Vs_Get1812), TrueResult1829=[Caar_Ret2510|Vs_Get1812], set_var(AEnv, sys_vs, TrueResult1829), ElseResult1832=TrueResult1829;get_var(AEnv, sys_aa, Aa_Get1814), f_caddar(Aa_Get1814, Symbolp_Param2318), f_symbolp(Symbolp_Param2318, PredArgResult1816), (PredArgResult1816==[]->f_sys_illegal_boa(TrueResult1827), ElseResult1830=TrueResult1827;get_var(AEnv, sys_aa, Aa_Get1818), f_cdddar(Aa_Get1818, Endp_Param2319), f_endp(Endp_Param2319, PredArgResult1820), (PredArgResult1820==[]->f_sys_illegal_boa(TrueResult1825), ElseResult1828=TrueResult1825;get_var(AEnv, sys_aa, Aa_Get1821), f_caar(Aa_Get1821, Caar_Ret2511), get_var(AEnv, sys_vs, Vs_Get1822), Set_var_Ret2512=[Caar_Ret2511|Vs_Get1822], set_var(AEnv, sys_vs, Set_var_Ret2512), get_var(AEnv, sys_aa, Aa_Get1823), f_caddar(Aa_Get1823, Caddar_Ret2513), get_var(AEnv, sys_vs, Vs_Get1824), ElseResult1826=[Caddar_Ret2513|Vs_Get1824], set_var(AEnv, sys_vs, ElseResult1826), ElseResult1828=ElseResult1826), ElseResult1830=ElseResult1828), ElseResult1832=ElseResult1830), ElseResult1834=ElseResult1832), _95920=ElseResult1834), get_var(AEnv, sys_aa, Aa_Get1835), f_cdr(Aa_Get1835, Cdr_Ret2514), set_var(AEnv, sys_aa, Cdr_Ret2514), goto(do_label_24, AEnv), _TBResult1539=_GORES1836)))]), []=LetResult1535), block_exit([], LetResult1535), true), throw(block_exit([], [])), _75608=ThrowResult2141;get_var(BlockExitEnv, a, A_Get2143), f_car(A_Get2143, PredArgResult2145), (is_symbolp(PredArgResult2145)->_96368=[];f_sys_illegal_boa(ElseResult2146), _96368=ElseResult2146), get_var(BlockExitEnv, a, A_Get2147), f_car(A_Get2147, Car_Ret2515), get_var(BlockExitEnv, sys_l, L_Get2148), L2241=[Car_Ret2515|L_Get2148], set_var(BlockExitEnv, sys_l, L2241), get_var(BlockExitEnv, a, A_Get2149), f_car(A_Get2149, Car_Ret2516), get_var(BlockExitEnv, sys_vs, Vs_Get2150), ElseResult2152=[Car_Ret2516|Vs_Get2150], set_var(BlockExitEnv, sys_vs, ElseResult2152), _75608=ElseResult2152), get_var(BlockExitEnv, a, A_Get2153), f_cdr(A_Get2153, A), set_var(BlockExitEnv, a, A), goto(do_label_20, BlockExitEnv), _TBResult843=_GORES2154)), [addr(addr_tagbody_20_do_label_20, do_label_20, '$unused', AEnv,  (get_var(AEnv, a, A_Get), (s3q:is_endp(A_Get)->get_var(AEnv, sys_l, L_Get851), Nreconc_Param2321=[c38_aux|L_Get851], get_var(AEnv, sys_keys, Get_var_Ret2517), f_mapcan(closure(kw_function, [ClosureEnvironment867|AEnv], Whole868, LResult865, [sys_k],  (get_var(ClosureEnvironment867, sys_k, K_Get), (K_Get\=[CAR2519|CDR2520]->get_var(ClosureEnvironment867, sys_k, K_Get858), Member_Param2320=K_Get858;get_var(ClosureEnvironment867, sys_k, K_Get859), f_car(K_Get859, ElseResult861), Member_Param2320=ElseResult861), get_var(ClosureEnvironment867, sys_vs, Get_var_Ret2521), f_member(Member_Param2320, Get_var_Ret2521, [], IFTEST852), (IFTEST852\==[]->LResult865=[];get_var(ClosureEnvironment867, sys_k, K_Get863), ElseResult864=[K_Get863], LResult865=ElseResult864)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Get_var_Ret2517, Mapcan_Ret2518), f_nreconc(Nreconc_Param2321, Mapcan_Ret2518, RetResult848), set_var(AEnv, sys_keys, RetResult848), throw(block_exit([], RetResult848)), _TBResult843=ThrowResult849;get_var(AEnv, a, A_Get872), f_car(A_Get872, PredArg1Result874), (is_eq(PredArg1Result874, c38_optional)->get_var(AEnv, sys_l, L_Get876), Set_var_Ret2522=[c38_optional|L_Get876], set_var(AEnv, sys_l, Set_var_Ret2522), get_var(AEnv, a, A_Get880), f_cdr(A_Get880, Cdr_Ret2523), BlockExitEnv=[bv(sys_aa, Cdr_Ret2523), bv([sys_ov], []), bv([sys_y], [])|AEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_21), get_var(BlockExitEnv, sys_aa, Aa_Get1185), (s3q:is_endp(Aa_Get1185)->get_var(BlockExitEnv, sys_l, L_Get1191), Nreconc_Param2323=[c38_aux|L_Get1191], get_var(BlockExitEnv, sys_keys, Keys_Get1209), f_mapcan(closure(kw_function, [ClosureEnvironment1207|BlockExitEnv], Whole1208, LResult1205, [sys_k],  (get_var(ClosureEnvironment1207, sys_k, K_Get1195), (K_Get1195\=[CAR2525|CDR2526]->get_var(ClosureEnvironment1207, sys_k, K_Get1198), Member_Param2322=K_Get1198;get_var(ClosureEnvironment1207, sys_k, K_Get1199), f_car(K_Get1199, ElseResult1201), Member_Param2322=ElseResult1201), get_var(ClosureEnvironment1207, sys_vs, Vs_Get1202), f_member(Member_Param2322, Vs_Get1202, [], IFTEST1192), (IFTEST1192\==[]->LResult1205=[];get_var(ClosureEnvironment1207, sys_k, K_Get1203), ElseResult1204=[K_Get1203], LResult1205=ElseResult1204)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1209, Mapcan_Ret2524), f_nreconc(Nreconc_Param2323, Mapcan_Ret2524, Nreconc_Ret2527), set_var(BlockExitEnv, sys_keys, Nreconc_Ret2527), throw(block_exit([], [])), throw(block_exit([], RetResult1188)), _TBResult882=ThrowResult1189;get_var(BlockExitEnv, sys_aa, Aa_Get1215), f_car(Aa_Get1215, Member_Param2324), get_var(BlockExitEnv, lambda_list_keywords, Lambda_list_keywords_Get1216), f_member(Member_Param2324, Lambda_list_keywords_Get1216, [], IFTEST1213), (IFTEST1213\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1218), f_car(Aa_Get1218, PredArg1Result1220), (is_eq(PredArg1Result1220, c38_rest)->get_var(BlockExitEnv, sys_l, L_Get1221), Set_var_Ret2528=[c38_rest|L_Get1221], set_var(BlockExitEnv, sys_l, Set_var_Ret2528), get_var(BlockExitEnv, sys_aa, Aa_Get1222), f_cdr(Aa_Get1222, Cdr_Ret2529), set_var(BlockExitEnv, sys_aa, Cdr_Ret2529), get_var(BlockExitEnv, sys_aa, Aa_Get1226), f_endp(Aa_Get1226, PredArgResult1228), (PredArgResult1228==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1229), f_car(Aa_Get1229, Symbolp_Param2325), f_symbolp(Symbolp_Param2325, TrueResult1230), IFTEST1223=TrueResult1230;IFTEST1223=[]), (IFTEST1223\==[]->_97488=[];f_sys_illegal_boa(ElseResult1231), _97488=ElseResult1231), get_var(BlockExitEnv, sys_aa, Aa_Get1232), f_car(Aa_Get1232, Car_Ret2530), get_var(BlockExitEnv, sys_vs, Vs_Get1233), Set_var_Ret2531=[Car_Ret2530|Vs_Get1233], set_var(BlockExitEnv, sys_vs, Set_var_Ret2531), get_var(BlockExitEnv, sys_aa, Aa_Get1234), f_car(Aa_Get1234, Car_Ret2532), get_var(BlockExitEnv, sys_l, L_Get1235), Set_var_Ret2533=[Car_Ret2532|L_Get1235], set_var(BlockExitEnv, sys_l, Set_var_Ret2533), get_var(BlockExitEnv, sys_aa, Aa_Get1236), f_cdr(Aa_Get1236, Cdr_Ret2534), set_var(BlockExitEnv, sys_aa, Cdr_Ret2534), get_var(BlockExitEnv, sys_aa, Aa_Get1238), (s3q:is_endp(Aa_Get1238)->get_var(BlockExitEnv, sys_l, L_Get1241), Nreconc_Param2327=[c38_aux|L_Get1241], get_var(BlockExitEnv, sys_keys, Keys_Get1259), f_mapcan(closure(kw_function, [ClosureEnvironment1257|BlockExitEnv], Whole1258, LResult1255, [sys_k],  (get_var(ClosureEnvironment1257, sys_k, K_Get1245), (K_Get1245\=[CAR2536|CDR2537]->get_var(ClosureEnvironment1257, sys_k, K_Get1248), Member_Param2326=K_Get1248;get_var(ClosureEnvironment1257, sys_k, K_Get1249), f_car(K_Get1249, ElseResult1251), Member_Param2326=ElseResult1251), get_var(ClosureEnvironment1257, sys_vs, Vs_Get1252), f_member(Member_Param2326, Vs_Get1252, [], IFTEST1242), (IFTEST1242\==[]->LResult1255=[];get_var(ClosureEnvironment1257, sys_k, K_Get1253), ElseResult1254=[K_Get1253], LResult1255=ElseResult1254)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1259, Mapcan_Ret2535), f_nreconc(Nreconc_Param2327, Mapcan_Ret2535, Nreconc_Ret2538), set_var(BlockExitEnv, sys_keys, Nreconc_Ret2538), throw(block_exit([], [])), TrueResult1263=ThrowResult1261;TrueResult1263=[]), _97820=TrueResult1263;_97820=[]), get_var(BlockExitEnv, sys_aa, Aa_Get1265), f_car(Aa_Get1265, PredArg1Result1267), (is_eq(PredArg1Result1267, c38_aux)->_97850=[];f_sys_illegal_boa(ElseResult1268), _97850=ElseResult1268), get_var(BlockExitEnv, sys_l, L_Get1269), Set_var_Ret2539=[c38_aux|L_Get1269], set_var(BlockExitEnv, sys_l, Set_var_Ret2539), get_var(BlockExitEnv, sys_aa, Aa_Get1273), f_cdr(Aa_Get1273, Aaa_Init1274), AEnv=[bv(sys_aaa, Aaa_Init1274)|BlockExitEnv], catch((call_addr_block(AEnv,  (push_label(do_label_23), get_var(AEnv, sys_aaa, Aaa_Get1319), (s3q:is_endp(Aaa_Get1319)->throw(block_exit([], [])), _TBResult1275=ThrowResult1323;get_var(AEnv, sys_aaa, Aaa_Get1326), f_car(Aaa_Get1326, Car_Ret2540), get_var(AEnv, sys_l, L_Get1327), Set_var_Ret2541=[Car_Ret2540|L_Get1327], set_var(AEnv, sys_l, Set_var_Ret2541), get_var(AEnv, sys_aaa, Aaa_Get1331), f_car(Aaa_Get1331, PredArgResult1333), (PredArgResult1333\=[CAR2542|CDR2543]->get_var(AEnv, sys_aaa, Aaa_Get1334), f_car(Aaa_Get1334, Symbolp_Param2328), f_symbolp(Symbolp_Param2328, TrueResult1335), IFTEST1328=TrueResult1335;IFTEST1328=[]), (IFTEST1328\==[]->get_var(AEnv, sys_aaa, Aaa_Get1336), f_car(Aaa_Get1336, Car_Ret2544), get_var(AEnv, sys_vs, Vs_Get1337), TrueResult1352=[Car_Ret2544|Vs_Get1337], set_var(AEnv, sys_vs, TrueResult1352), _98118=TrueResult1352;get_var(AEnv, sys_aaa, Aaa_Get1341), f_caar(Aaa_Get1341, PredArgResult1343), (is_symbolp(PredArgResult1343)->(get_var(AEnv, sys_aaa, Aaa_Get1344), f_cdar(Aaa_Get1344, Endp_Param2329), f_endp(Endp_Param2329, FORM1_Res1346), FORM1_Res1346\==[], TrueResult1347=FORM1_Res1346->true;get_var(AEnv, sys_aaa, Aaa_Get1345), f_cddar(Aaa_Get1345, Endp_Param2330), f_endp(Endp_Param2330, Endp_Ret2545), TrueResult1347=Endp_Ret2545), IFTEST1338=TrueResult1347;IFTEST1338=[]), (IFTEST1338\==[]->get_var(AEnv, sys_aaa, Aaa_Get1348), f_caar(Aaa_Get1348, Caar_Ret2546), get_var(AEnv, sys_vs, Vs_Get1349), TrueResult1350=[Caar_Ret2546|Vs_Get1349], set_var(AEnv, sys_vs, TrueResult1350), ElseResult1353=TrueResult1350;f_sys_illegal_boa(ElseResult1351), ElseResult1353=ElseResult1351), _98118=ElseResult1353), get_var(AEnv, sys_aaa, Aaa_Get1354), f_cdr(Aaa_Get1354, Cdr_Ret2547), set_var(AEnv, sys_aaa, Cdr_Ret2547), goto(do_label_23, AEnv), _TBResult1275=_GORES1355)), [addr(addr_tagbody_23_do_label_23, do_label_23, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get1277), (s3q:is_endp(Aaa_Get1277)->throw(block_exit([], [])), _TBResult1275=ThrowResult1281;get_var(AEnv, sys_aaa, Aaa_Get1284), f_car(Aaa_Get1284, Car_Ret2548), get_var(AEnv, sys_l, L_Get1285), Set_var_Ret2549=[Car_Ret2548|L_Get1285], set_var(AEnv, sys_l, Set_var_Ret2549), get_var(AEnv, sys_aaa, Aaa_Get1289), f_car(Aaa_Get1289, PredArgResult1291), (PredArgResult1291\=[CAR2550|CDR2551]->get_var(AEnv, sys_aaa, Aaa_Get1292), f_car(Aaa_Get1292, Symbolp_Param2331), f_symbolp(Symbolp_Param2331, TrueResult1293), IFTEST1286=TrueResult1293;IFTEST1286=[]), (IFTEST1286\==[]->get_var(AEnv, sys_aaa, Aaa_Get1294), f_car(Aaa_Get1294, Car_Ret2552), get_var(AEnv, sys_vs, Vs_Get1295), TrueResult1310=[Car_Ret2552|Vs_Get1295], set_var(AEnv, sys_vs, TrueResult1310), _98534=TrueResult1310;get_var(AEnv, sys_aaa, Aaa_Get1299), f_caar(Aaa_Get1299, PredArgResult1301), (is_symbolp(PredArgResult1301)->(get_var(AEnv, sys_aaa, Aaa_Get1302), f_cdar(Aaa_Get1302, Endp_Param2332), f_endp(Endp_Param2332, FORM1_Res1304), FORM1_Res1304\==[], TrueResult1305=FORM1_Res1304->true;get_var(AEnv, sys_aaa, Aaa_Get1303), f_cddar(Aaa_Get1303, Endp_Param2333), f_endp(Endp_Param2333, Endp_Ret2553), TrueResult1305=Endp_Ret2553), IFTEST1296=TrueResult1305;IFTEST1296=[]), (IFTEST1296\==[]->get_var(AEnv, sys_aaa, Aaa_Get1306), f_caar(Aaa_Get1306, Caar_Ret2554), get_var(AEnv, sys_vs, Vs_Get1307), TrueResult1308=[Caar_Ret2554|Vs_Get1307], set_var(AEnv, sys_vs, TrueResult1308), ElseResult1311=TrueResult1308;f_sys_illegal_boa(ElseResult1309), ElseResult1311=ElseResult1309), _98534=ElseResult1311), get_var(AEnv, sys_aaa, Aaa_Get1312), f_cdr(Aaa_Get1312, Cdr_Ret2555), set_var(AEnv, sys_aaa, Cdr_Ret2555), goto(do_label_23, AEnv), _TBResult1275=_GORES1313)))]), []=LetResult1271), block_exit([], LetResult1271), true), get_var(BlockExitEnv, sys_keys, Keys_Get1377), get_var(BlockExitEnv, sys_l, L_Get1359), f_mapcan(closure(kw_function, [ClosureEnvironment1375|BlockExitEnv], Whole1376, LResult1373, [sys_k],  (get_var(ClosureEnvironment1375, sys_k, K_Get1363), (K_Get1363\=[CAR2557|CDR2558]->get_var(ClosureEnvironment1375, sys_k, K_Get1366), Member_Param2334=K_Get1366;get_var(ClosureEnvironment1375, sys_k, K_Get1367), f_car(K_Get1367, ElseResult1369), Member_Param2334=ElseResult1369), get_var(ClosureEnvironment1375, sys_vs, Vs_Get1370), f_member(Member_Param2334, Vs_Get1370, [], IFTEST1360), (IFTEST1360\==[]->LResult1373=[];get_var(ClosureEnvironment1375, sys_k, K_Get1371), ElseResult1372=[K_Get1371], LResult1373=ElseResult1372)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1377, Mapcan_Ret2556), f_nreconc(L_Get1359, Mapcan_Ret2556, Nreconc_Ret2559), set_var(BlockExitEnv, sys_keys, Nreconc_Ret2559), get_var(BlockExitEnv, sys_keys, Keys_Get1396), get_var(BlockExitEnv, sys_l, L_Get1378), f_mapcan(closure(kw_function, [ClosureEnvironment1394|BlockExitEnv], Whole1395, LResult1392, [sys_k],  (get_var(ClosureEnvironment1394, sys_k, K_Get1382), (K_Get1382\=[CAR2561|CDR2562]->get_var(ClosureEnvironment1394, sys_k, K_Get1385), Member_Param2335=K_Get1385;get_var(ClosureEnvironment1394, sys_k, K_Get1386), f_car(K_Get1386, ElseResult1388), Member_Param2335=ElseResult1388), get_var(ClosureEnvironment1394, sys_vs, Vs_Get1389), f_member(Member_Param2335, Vs_Get1389, [], IFTEST1379), (IFTEST1379\==[]->LResult1392=[];get_var(ClosureEnvironment1394, sys_k, K_Get1390), ElseResult1391=[K_Get1390], LResult1392=ElseResult1391)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1396, Mapcan_Ret2560), f_nreconc(L_Get1378, Mapcan_Ret2560, Nreconc_Ret2563), set_var(BlockExitEnv, sys_keys, Nreconc_Ret2563), throw(block_exit([], [])), _99140=ThrowResult1398;_99140=[]), get_var(BlockExitEnv, sys_aa, Aa_Get1405), f_car(Aa_Get1405, PredArgResult1407), (PredArgResult1407\=[CAR2564|CDR2565]->get_var(BlockExitEnv, sys_aa, Aa_Get1408), f_car(Aa_Get1408, Car_Ret2566), set_var(BlockExitEnv, sys_ov, Car_Ret2566), IFTEST1402=t;get_var(BlockExitEnv, sys_aa, Aa_Get1410), f_cdar(Aa_Get1410, PredArgResult1412), (s3q:is_endp(PredArgResult1412)->get_var(BlockExitEnv, sys_aa, Aa_Get1413), f_caar(Aa_Get1413, Caar_Ret2567), set_var(BlockExitEnv, sys_ov, Caar_Ret2567), ElseResult1414=t;ElseResult1414=[]), IFTEST1402=ElseResult1414), (IFTEST1402\==[]->get_var(BlockExitEnv, sys_keys, Keys_Get1416), get_var(BlockExitEnv, sys_ov, Ov_Get1415), f_member(Ov_Get1415, Keys_Get1416, [kw_key, closure(kw_function, [ClosureEnvironment1425|BlockExitEnv], Whole1426, LResult1423, [x],  (get_var(ClosureEnvironment1425, x, X_Get1418), (c0nz:is_consp(X_Get1418)->get_var(ClosureEnvironment1425, x, X_Get1421), f_car(X_Get1421, TrueResult1422), LResult1423=TrueResult1422;LResult1423=[])), [lambda, [x], [if, [consp, x], [car, x]]])], TrueResult1427), set_var(BlockExitEnv, sys_y, TrueResult1427), IFTEST1400=TrueResult1427;IFTEST1400=[]), (IFTEST1400\==[]->get_var(BlockExitEnv, sys_y, Y_Get1428), f_car(Y_Get1428, Car_Ret2568), get_var(BlockExitEnv, sys_l, L_Get1429), TrueResult1432=[Car_Ret2568|L_Get1429], set_var(BlockExitEnv, sys_l, TrueResult1432), _99460=TrueResult1432;get_var(BlockExitEnv, sys_aa, Aa_Get1430), f_car(Aa_Get1430, Car_Ret2569), get_var(BlockExitEnv, sys_l, L_Get1431), ElseResult1433=[Car_Ret2569|L_Get1431], set_var(BlockExitEnv, sys_l, ElseResult1433), _99460=ElseResult1433), get_var(BlockExitEnv, sys_aa, Aa_Get1435), f_car(Aa_Get1435, PredArgResult1437), (PredArgResult1437\=[CAR2570|CDR2571]->get_var(BlockExitEnv, sys_aa, Aa_Get1439), f_car(Aa_Get1439, PredArgResult1441), (is_symbolp(PredArgResult1441)->_99566=[];f_sys_illegal_boa(ElseResult1442), _99566=ElseResult1442), get_var(BlockExitEnv, sys_aa, Aa_Get1443), f_car(Aa_Get1443, Car_Ret2572), get_var(BlockExitEnv, sys_vs, Vs_Get1444), TrueResult1476=[Car_Ret2572|Vs_Get1444], set_var(BlockExitEnv, sys_vs, TrueResult1476), _99626=TrueResult1476;get_var(BlockExitEnv, sys_aa, Aa_Get1446), f_caar(Aa_Get1446, Symbolp_Param2336), f_symbolp(Symbolp_Param2336, PredArgResult1448), (PredArgResult1448==[]->f_sys_illegal_boa(TrueResult1474), ElseResult1477=TrueResult1474;(get_var(BlockExitEnv, sys_aa, Aa_Get1451), f_cdar(Aa_Get1451, Endp_Param2337), f_endp(Endp_Param2337, FORM1_Res1453), FORM1_Res1453\==[], IFTEST1449=FORM1_Res1453->true;get_var(BlockExitEnv, sys_aa, Aa_Get1452), f_cddar(Aa_Get1452, Endp_Param2338), f_endp(Endp_Param2338, Endp_Ret2573), IFTEST1449=Endp_Ret2573), (IFTEST1449\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1454), f_caar(Aa_Get1454, Caar_Ret2574), get_var(BlockExitEnv, sys_vs, Vs_Get1455), TrueResult1472=[Caar_Ret2574|Vs_Get1455], set_var(BlockExitEnv, sys_vs, TrueResult1472), ElseResult1475=TrueResult1472;get_var(BlockExitEnv, sys_aa, Aa_Get1457), f_caddar(Aa_Get1457, Symbolp_Param2339), f_symbolp(Symbolp_Param2339, PredArgResult1459), (PredArgResult1459==[]->f_sys_illegal_boa(TrueResult1470), ElseResult1473=TrueResult1470;get_var(BlockExitEnv, sys_aa, Aa_Get1461), f_cdddar(Aa_Get1461, Endp_Param2340), f_endp(Endp_Param2340, PredArgResult1463), (PredArgResult1463==[]->f_sys_illegal_boa(TrueResult1468), ElseResult1471=TrueResult1468;get_var(BlockExitEnv, sys_aa, Aa_Get1464), f_caar(Aa_Get1464, Caar_Ret2575), get_var(BlockExitEnv, sys_vs, Vs_Get1465), Set_var_Ret2576=[Caar_Ret2575|Vs_Get1465], set_var(BlockExitEnv, sys_vs, Set_var_Ret2576), get_var(BlockExitEnv, sys_aa, Aa_Get1466), f_caddar(Aa_Get1466, Caddar_Ret2577), get_var(BlockExitEnv, sys_vs, Vs_Get1467), ElseResult1469=[Caddar_Ret2577|Vs_Get1467], set_var(BlockExitEnv, sys_vs, ElseResult1469), ElseResult1471=ElseResult1469), ElseResult1473=ElseResult1471), ElseResult1475=ElseResult1473), ElseResult1477=ElseResult1475), _99626=ElseResult1477), get_var(BlockExitEnv, sys_aa, Aa_Get1478), f_cdr(Aa_Get1478, Cdr_Ret2578), set_var(BlockExitEnv, sys_aa, Cdr_Ret2578), goto(do_label_21, BlockExitEnv), _TBResult882=_GORES1479)), [addr(addr_tagbody_21_do_label_21, do_label_21, '$unused', AEnv,  (get_var(AEnv, sys_aa, Aa_Get), (s3q:is_endp(Aa_Get)->get_var(AEnv, sys_l, L_Get890), Nreconc_Param2342=[c38_aux|L_Get890], get_var(AEnv, sys_keys, Keys_Get908), f_mapcan(closure(kw_function, [ClosureEnvironment906|AEnv], Whole907, LResult904, [sys_k],  (get_var(ClosureEnvironment906, sys_k, K_Get894), (K_Get894\=[CAR2580|CDR2581]->get_var(ClosureEnvironment906, sys_k, K_Get897), Member_Param2341=K_Get897;get_var(ClosureEnvironment906, sys_k, K_Get898), f_car(K_Get898, ElseResult900), Member_Param2341=ElseResult900), get_var(ClosureEnvironment906, sys_vs, Vs_Get901), f_member(Member_Param2341, Vs_Get901, [], IFTEST891), (IFTEST891\==[]->LResult904=[];get_var(ClosureEnvironment906, sys_k, K_Get902), ElseResult903=[K_Get902], LResult904=ElseResult903)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get908, Mapcan_Ret2579), f_nreconc(Nreconc_Param2342, Mapcan_Ret2579, Nreconc_Ret2582), set_var(AEnv, sys_keys, Nreconc_Ret2582), throw(block_exit([], [])), throw(block_exit([], RetResult887)), _TBResult882=ThrowResult888;get_var(AEnv, sys_aa, Aa_Get914), f_car(Aa_Get914, Member_Param2343), get_var(AEnv, lambda_list_keywords, Get_var_Ret2583), f_member(Member_Param2343, Get_var_Ret2583, [], IFTEST912), (IFTEST912\==[]->get_var(AEnv, sys_aa, Aa_Get917), f_car(Aa_Get917, PredArg1Result919), (is_eq(PredArg1Result919, c38_rest)->get_var(AEnv, sys_l, L_Get921), Set_var_Ret2584=[c38_rest|L_Get921], set_var(AEnv, sys_l, Set_var_Ret2584), get_var(AEnv, sys_aa, Aa_Get922), f_cdr(Aa_Get922, Cdr_Ret2585), set_var(AEnv, sys_aa, Cdr_Ret2585), get_var(AEnv, sys_aa, Aa_Get926), f_endp(Aa_Get926, PredArgResult928), (PredArgResult928==[]->get_var(AEnv, sys_aa, Aa_Get929), f_car(Aa_Get929, Symbolp_Param2344), f_symbolp(Symbolp_Param2344, TrueResult930), IFTEST923=TrueResult930;IFTEST923=[]), (IFTEST923\==[]->_100456=[];f_sys_illegal_boa(ElseResult931), _100456=ElseResult931), get_var(AEnv, sys_aa, Aa_Get932), f_car(Aa_Get932, Car_Ret2586), get_var(AEnv, sys_vs, Vs_Get933), Set_var_Ret2587=[Car_Ret2586|Vs_Get933], set_var(AEnv, sys_vs, Set_var_Ret2587), get_var(AEnv, sys_aa, Aa_Get934), f_car(Aa_Get934, Car_Ret2588), get_var(AEnv, sys_l, L_Get935), Set_var_Ret2589=[Car_Ret2588|L_Get935], set_var(AEnv, sys_l, Set_var_Ret2589), get_var(AEnv, sys_aa, Aa_Get936), f_cdr(Aa_Get936, Cdr_Ret2590), set_var(AEnv, sys_aa, Cdr_Ret2590), get_var(AEnv, sys_aa, Aa_Get938), (s3q:is_endp(Aa_Get938)->get_var(AEnv, sys_l, L_Get941), Nreconc_Param2346=[c38_aux|L_Get941], get_var(AEnv, sys_keys, Keys_Get959), f_mapcan(closure(kw_function, [ClosureEnvironment957|AEnv], Whole958, LResult955, [sys_k],  (get_var(ClosureEnvironment957, sys_k, K_Get945), (K_Get945\=[CAR2592|CDR2593]->get_var(ClosureEnvironment957, sys_k, K_Get948), Member_Param2345=K_Get948;get_var(ClosureEnvironment957, sys_k, K_Get949), f_car(K_Get949, ElseResult951), Member_Param2345=ElseResult951), get_var(ClosureEnvironment957, sys_vs, Vs_Get952), f_member(Member_Param2345, Vs_Get952, [], IFTEST942), (IFTEST942\==[]->LResult955=[];get_var(ClosureEnvironment957, sys_k, K_Get953), ElseResult954=[K_Get953], LResult955=ElseResult954)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get959, Mapcan_Ret2591), f_nreconc(Nreconc_Param2346, Mapcan_Ret2591, Nreconc_Ret2594), set_var(AEnv, sys_keys, Nreconc_Ret2594), throw(block_exit([], [])), TrueResult963=ThrowResult961;TrueResult963=[]), _100788=TrueResult963;_100788=[]), get_var(AEnv, sys_aa, Aa_Get965), f_car(Aa_Get965, PredArg1Result967), (is_eq(PredArg1Result967, c38_aux)->_100818=[];f_sys_illegal_boa(ElseResult968), _100818=ElseResult968), get_var(AEnv, sys_l, L_Get969), Set_var_Ret2595=[c38_aux|L_Get969], set_var(AEnv, sys_l, Set_var_Ret2595), get_var(AEnv, sys_aa, Aa_Get973), f_cdr(Aa_Get973, Cdr_Ret2596), AEnv=[bv(sys_aaa, Cdr_Ret2596)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_22), get_var(AEnv, sys_aaa, Aaa_Get1019), (s3q:is_endp(Aaa_Get1019)->throw(block_exit([], [])), _TBResult975=ThrowResult1023;get_var(AEnv, sys_aaa, Aaa_Get1026), f_car(Aaa_Get1026, Car_Ret2597), get_var(AEnv, sys_l, L_Get1027), Set_var_Ret2598=[Car_Ret2597|L_Get1027], set_var(AEnv, sys_l, Set_var_Ret2598), get_var(AEnv, sys_aaa, Aaa_Get1031), f_car(Aaa_Get1031, PredArgResult1033), (PredArgResult1033\=[CAR2599|CDR2600]->get_var(AEnv, sys_aaa, Aaa_Get1034), f_car(Aaa_Get1034, Symbolp_Param2347), f_symbolp(Symbolp_Param2347, TrueResult1035), IFTEST1028=TrueResult1035;IFTEST1028=[]), (IFTEST1028\==[]->get_var(AEnv, sys_aaa, Aaa_Get1036), f_car(Aaa_Get1036, Car_Ret2601), get_var(AEnv, sys_vs, Vs_Get1037), TrueResult1052=[Car_Ret2601|Vs_Get1037], set_var(AEnv, sys_vs, TrueResult1052), _101074=TrueResult1052;get_var(AEnv, sys_aaa, Aaa_Get1041), f_caar(Aaa_Get1041, PredArgResult1043), (is_symbolp(PredArgResult1043)->(get_var(AEnv, sys_aaa, Aaa_Get1044), f_cdar(Aaa_Get1044, Endp_Param2348), f_endp(Endp_Param2348, FORM1_Res1046), FORM1_Res1046\==[], TrueResult1047=FORM1_Res1046->true;get_var(AEnv, sys_aaa, Aaa_Get1045), f_cddar(Aaa_Get1045, Endp_Param2349), f_endp(Endp_Param2349, Endp_Ret2602), TrueResult1047=Endp_Ret2602), IFTEST1038=TrueResult1047;IFTEST1038=[]), (IFTEST1038\==[]->get_var(AEnv, sys_aaa, Aaa_Get1048), f_caar(Aaa_Get1048, Caar_Ret2603), get_var(AEnv, sys_vs, Vs_Get1049), TrueResult1050=[Caar_Ret2603|Vs_Get1049], set_var(AEnv, sys_vs, TrueResult1050), ElseResult1053=TrueResult1050;f_sys_illegal_boa(ElseResult1051), ElseResult1053=ElseResult1051), _101074=ElseResult1053), get_var(AEnv, sys_aaa, Aaa_Get1054), f_cdr(Aaa_Get1054, Cdr_Ret2604), set_var(AEnv, sys_aaa, Cdr_Ret2604), goto(do_label_22, AEnv), _TBResult975=_GORES1055)), [addr(addr_tagbody_22_do_label_22, do_label_22, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get), (s3q:is_endp(Aaa_Get)->throw(block_exit([], [])), _TBResult975=ThrowResult981;get_var(AEnv, sys_aaa, Aaa_Get984), f_car(Aaa_Get984, Car_Ret2605), get_var(AEnv, sys_l, L_Get985), Set_var_Ret2606=[Car_Ret2605|L_Get985], set_var(AEnv, sys_l, Set_var_Ret2606), get_var(AEnv, sys_aaa, Aaa_Get989), f_car(Aaa_Get989, PredArgResult991), (PredArgResult991\=[CAR2607|CDR2608]->get_var(AEnv, sys_aaa, Aaa_Get992), f_car(Aaa_Get992, Symbolp_Param2350), f_symbolp(Symbolp_Param2350, TrueResult993), IFTEST986=TrueResult993;IFTEST986=[]), (IFTEST986\==[]->get_var(AEnv, sys_aaa, Aaa_Get994), f_car(Aaa_Get994, Car_Ret2609), get_var(AEnv, sys_vs, Vs_Get995), TrueResult1010=[Car_Ret2609|Vs_Get995], set_var(AEnv, sys_vs, TrueResult1010), _101490=TrueResult1010;get_var(AEnv, sys_aaa, Aaa_Get999), f_caar(Aaa_Get999, PredArgResult1001), (is_symbolp(PredArgResult1001)->(get_var(AEnv, sys_aaa, Aaa_Get1002), f_cdar(Aaa_Get1002, Endp_Param2351), f_endp(Endp_Param2351, FORM1_Res1004), FORM1_Res1004\==[], TrueResult1005=FORM1_Res1004->true;get_var(AEnv, sys_aaa, Aaa_Get1003), f_cddar(Aaa_Get1003, Endp_Param2352), f_endp(Endp_Param2352, Endp_Ret2610), TrueResult1005=Endp_Ret2610), IFTEST996=TrueResult1005;IFTEST996=[]), (IFTEST996\==[]->get_var(AEnv, sys_aaa, Aaa_Get1006), f_caar(Aaa_Get1006, Caar_Ret2611), get_var(AEnv, sys_vs, Vs_Get1007), TrueResult1008=[Caar_Ret2611|Vs_Get1007], set_var(AEnv, sys_vs, TrueResult1008), ElseResult1011=TrueResult1008;f_sys_illegal_boa(ElseResult1009), ElseResult1011=ElseResult1009), _101490=ElseResult1011), get_var(AEnv, sys_aaa, Aaa_Get1012), f_cdr(Aaa_Get1012, Cdr_Ret2612), set_var(AEnv, sys_aaa, Cdr_Ret2612), goto(do_label_22, AEnv), _TBResult975=_GORES1013)))]), []=LetResult971), block_exit([], LetResult971), true), get_var(AEnv, sys_keys, Keys_Get1077), get_var(AEnv, sys_l, L_Get1059), f_mapcan(closure(kw_function, [ClosureEnvironment1075|AEnv], Whole1076, LResult1073, [sys_k],  (get_var(ClosureEnvironment1075, sys_k, K_Get1063), (K_Get1063\=[CAR2614|CDR2615]->get_var(ClosureEnvironment1075, sys_k, K_Get1066), Member_Param2353=K_Get1066;get_var(ClosureEnvironment1075, sys_k, K_Get1067), f_car(K_Get1067, ElseResult1069), Member_Param2353=ElseResult1069), get_var(ClosureEnvironment1075, sys_vs, Vs_Get1070), f_member(Member_Param2353, Vs_Get1070, [], IFTEST1060), (IFTEST1060\==[]->LResult1073=[];get_var(ClosureEnvironment1075, sys_k, K_Get1071), ElseResult1072=[K_Get1071], LResult1073=ElseResult1072)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1077, Mapcan_Ret2613), f_nreconc(L_Get1059, Mapcan_Ret2613, Nreconc_Ret2616), set_var(AEnv, sys_keys, Nreconc_Ret2616), get_var(AEnv, sys_keys, Keys_Get1096), get_var(AEnv, sys_l, L_Get1078), f_mapcan(closure(kw_function, [ClosureEnvironment1094|AEnv], Whole1095, LResult1092, [sys_k],  (get_var(ClosureEnvironment1094, sys_k, K_Get1082), (K_Get1082\=[CAR2618|CDR2619]->get_var(ClosureEnvironment1094, sys_k, K_Get1085), Member_Param2354=K_Get1085;get_var(ClosureEnvironment1094, sys_k, K_Get1086), f_car(K_Get1086, ElseResult1088), Member_Param2354=ElseResult1088), get_var(ClosureEnvironment1094, sys_vs, Vs_Get1089), f_member(Member_Param2354, Vs_Get1089, [], IFTEST1079), (IFTEST1079\==[]->LResult1092=[];get_var(ClosureEnvironment1094, sys_k, K_Get1090), ElseResult1091=[K_Get1090], LResult1092=ElseResult1091)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1096, Mapcan_Ret2617), f_nreconc(L_Get1078, Mapcan_Ret2617, Nreconc_Ret2620), set_var(AEnv, sys_keys, Nreconc_Ret2620), throw(block_exit([], [])), _102096=ThrowResult1098;_102096=[]), get_var(AEnv, sys_aa, Aa_Get1105), f_car(Aa_Get1105, PredArgResult1107), (PredArgResult1107\=[CAR2621|CDR2622]->get_var(AEnv, sys_aa, Aa_Get1108), f_car(Aa_Get1108, Car_Ret2623), set_var(AEnv, sys_ov, Car_Ret2623), IFTEST1102=t;get_var(AEnv, sys_aa, Aa_Get1110), f_cdar(Aa_Get1110, PredArgResult1112), (s3q:is_endp(PredArgResult1112)->get_var(AEnv, sys_aa, Aa_Get1113), f_caar(Aa_Get1113, Caar_Ret2624), set_var(AEnv, sys_ov, Caar_Ret2624), ElseResult1114=t;ElseResult1114=[]), IFTEST1102=ElseResult1114), (IFTEST1102\==[]->get_var(AEnv, sys_keys, Keys_Get1116), get_var(AEnv, sys_ov, Member_Param2355), f_member(Member_Param2355, Keys_Get1116, [kw_key, closure(kw_function, [ClosureEnvironment1125|AEnv], Whole1126, LResult1123, [x],  (get_var(ClosureEnvironment1125, x, X_Get1118), (c0nz:is_consp(X_Get1118)->get_var(ClosureEnvironment1125, x, X_Get1121), f_car(X_Get1121, TrueResult1122), LResult1123=TrueResult1122;LResult1123=[])), [lambda, [x], [if, [consp, x], [car, x]]])], TrueResult1127), set_var(AEnv, sys_y, TrueResult1127), IFTEST1100=TrueResult1127;IFTEST1100=[]), (IFTEST1100\==[]->get_var(AEnv, sys_y, Car_Param), f_car(Car_Param, Car_Ret2625), get_var(AEnv, sys_l, L_Get1129), TrueResult1132=[Car_Ret2625|L_Get1129], set_var(AEnv, sys_l, TrueResult1132), _102392=TrueResult1132;get_var(AEnv, sys_aa, Aa_Get1130), f_car(Aa_Get1130, Car_Ret2626), get_var(AEnv, sys_l, L_Get1131), ElseResult1133=[Car_Ret2626|L_Get1131], set_var(AEnv, sys_l, ElseResult1133), _102392=ElseResult1133), get_var(AEnv, sys_aa, Aa_Get1135), f_car(Aa_Get1135, PredArgResult1137), (PredArgResult1137\=[CAR2627|CDR2628]->get_var(AEnv, sys_aa, Aa_Get1139), f_car(Aa_Get1139, PredArgResult1141), (is_symbolp(PredArgResult1141)->_102498=[];f_sys_illegal_boa(ElseResult1142), _102498=ElseResult1142), get_var(AEnv, sys_aa, Aa_Get1143), f_car(Aa_Get1143, Car_Ret2629), get_var(AEnv, sys_vs, Vs_Get1144), TrueResult1176=[Car_Ret2629|Vs_Get1144], set_var(AEnv, sys_vs, TrueResult1176), _102558=TrueResult1176;get_var(AEnv, sys_aa, Aa_Get1146), f_caar(Aa_Get1146, Symbolp_Param2357), f_symbolp(Symbolp_Param2357, PredArgResult1148), (PredArgResult1148==[]->f_sys_illegal_boa(TrueResult1174), ElseResult1177=TrueResult1174;(get_var(AEnv, sys_aa, Aa_Get1151), f_cdar(Aa_Get1151, Endp_Param2358), f_endp(Endp_Param2358, FORM1_Res1153), FORM1_Res1153\==[], IFTEST1149=FORM1_Res1153->true;get_var(AEnv, sys_aa, Aa_Get1152), f_cddar(Aa_Get1152, Endp_Param2359), f_endp(Endp_Param2359, Endp_Ret2630), IFTEST1149=Endp_Ret2630), (IFTEST1149\==[]->get_var(AEnv, sys_aa, Aa_Get1154), f_caar(Aa_Get1154, Caar_Ret2631), get_var(AEnv, sys_vs, Vs_Get1155), TrueResult1172=[Caar_Ret2631|Vs_Get1155], set_var(AEnv, sys_vs, TrueResult1172), ElseResult1175=TrueResult1172;get_var(AEnv, sys_aa, Aa_Get1157), f_caddar(Aa_Get1157, Symbolp_Param2360), f_symbolp(Symbolp_Param2360, PredArgResult1159), (PredArgResult1159==[]->f_sys_illegal_boa(TrueResult1170), ElseResult1173=TrueResult1170;get_var(AEnv, sys_aa, Aa_Get1161), f_cdddar(Aa_Get1161, Endp_Param2361), f_endp(Endp_Param2361, PredArgResult1163), (PredArgResult1163==[]->f_sys_illegal_boa(TrueResult1168), ElseResult1171=TrueResult1168;get_var(AEnv, sys_aa, Aa_Get1164), f_caar(Aa_Get1164, Caar_Ret2632), get_var(AEnv, sys_vs, Vs_Get1165), Set_var_Ret2633=[Caar_Ret2632|Vs_Get1165], set_var(AEnv, sys_vs, Set_var_Ret2633), get_var(AEnv, sys_aa, Aa_Get1166), f_caddar(Aa_Get1166, Caddar_Ret2634), get_var(AEnv, sys_vs, Vs_Get1167), ElseResult1169=[Caddar_Ret2634|Vs_Get1167], set_var(AEnv, sys_vs, ElseResult1169), ElseResult1171=ElseResult1169), ElseResult1173=ElseResult1171), ElseResult1175=ElseResult1173), ElseResult1177=ElseResult1175), _102558=ElseResult1177), get_var(AEnv, sys_aa, Aa_Get1178), f_cdr(Aa_Get1178, Cdr_Ret2635), set_var(AEnv, sys_aa, Cdr_Ret2635), goto(do_label_21, AEnv), _TBResult882=_GORES1179)))]), []=LetResult878), block_exit([], LetResult878), true), throw(block_exit([], [])), _102974=ThrowResult1484;get_var(AEnv, a, A_Get1486), f_car(A_Get1486, PredArgResult1488), (is_symbolp(PredArgResult1488)->_103018=[];f_sys_illegal_boa(ElseResult1489), _103018=ElseResult1489), get_var(AEnv, a, A_Get1490), f_car(A_Get1490, Car_Ret2636), get_var(AEnv, sys_l, L_Get1491), Set_var_Ret2637=[Car_Ret2636|L_Get1491], set_var(AEnv, sys_l, Set_var_Ret2637), get_var(AEnv, a, A_Get1492), f_car(A_Get1492, Car_Ret2638), get_var(AEnv, sys_vs, Vs_Get1493), ElseResult1495=[Car_Ret2638|Vs_Get1493], set_var(AEnv, sys_vs, ElseResult1495), _102974=ElseResult1495), get_var(AEnv, a, A_Get1496), f_cdr(A_Get1496, Cdr_Ret2639), set_var(AEnv, a, Cdr_Ret2639), goto(do_label_20, AEnv), _TBResult843=_GORES1497)))]), []=LetResult839), block_exit([], LetResult839), true), get_var(LEnv794, sys_constructor, Constructor_Get2159), f_car(Constructor_Get2159, TrueResult2161), set_var(LEnv794, sys_constructor, TrueResult2161), _56066=TrueResult2161;get_var(LEnv794, sys_keys, Keys_Get2160), ElseResult2162=[c38_key|Keys_Get2160], set_var(LEnv794, sys_keys, ElseResult2162), _56066=ElseResult2162), get_var(LEnv794, type, IFTEST2163), (IFTEST2163==[]->get_var(LEnv794, sys_constructor, Constructor_Get2166), get_var(LEnv794, sys_keys, Keys_Get2167), get_var(LEnv794, sys_name, Name_Get2168), get_var(LEnv794, sys_slot_names, Slot_names_Get), LetResult793=[defun, Constructor_Get2166, Keys_Get2167, [sys_make_structure, [quote, Name_Get2168]|Slot_names_Get]];(get_var(LEnv794, type, Type_Get2172), f_eq(Type_Get2172, vector, FORM1_Res2179), FORM1_Res2179\==[], IFTEST2170=FORM1_Res2179->true;get_var(LEnv794, type, Type_Get2174), (c0nz:is_consp(Type_Get2174)->get_var(LEnv794, type, Type_Get2177), f_car(Type_Get2177, Eq_Param), f_eq(Eq_Param, vector, TrueResult2178), _103510=TrueResult2178;_103510=[]), IFTEST2170=_103510), (IFTEST2170\==[]->get_var(LEnv794, sys_constructor, Constructor_Get2180), get_var(LEnv794, sys_keys, Keys_Get2181), get_var(LEnv794, sys_slot_names, Slot_names_Get2182), ElseResult2196=[defun, Constructor_Get2180, Keys_Get2181, [vector|Slot_names_Get2182]];get_var(LEnv794, type, Type_Get2184), (is_eq(Type_Get2184, list)->get_var(LEnv794, sys_constructor, Constructor_Get2187), get_var(LEnv794, sys_keys, Keys_Get2188), get_var(LEnv794, sys_slot_names, Slot_names_Get2189), ElseResult2195=[defun, Constructor_Get2187, Keys_Get2188, [list|Slot_names_Get2189]];get_var(LEnv794, type, Type_Get2192), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal structure type"), Type_Get2192], IFTEST2190), (IFTEST2190\==[]->ElseResult2194=[];ElseResult2193=[], ElseResult2194=ElseResult2193), ElseResult2195=ElseResult2194), ElseResult2196=ElseResult2195), LetResult793=ElseResult2196)), LetResult793=FnResult785), block_exit(sys_make_constructor, FnResult785), true))),
		  set_opv(sys_make_constructor,
			  symbol_function,
			  f_sys_make_constructor),
		  DefunResult2198=sys_make_constructor,
		  get_var(AEnv, sys_keys, Keys_Get2199),
		  LetResult12=[c38_key|Keys_Get2199],
		  set_var(AEnv, sys_keys, LetResult12)
		),
		LetResult12=MFResult
	      ),
	      block_exit(defstruct, MFResult),
	      true).
:- set_opv(mf_defstruct, type_of, sys_macro),
   set_opv(defstruct, symbol_function, mf_defstruct),
   DefMacroResult=defstruct.
/*
:- side_effect(assert_lsp(defstruct,
			  doc_string(defstruct,
				     _32856,
				     function,
				     "Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."))).
*/
/*
:- side_effect(assert_lsp(defstruct,
			  lambda_def(defmacro,
				     defstruct,
				     mf_defstruct,
				     [sys_name, c38_rest, sys_slots],
				     
				     [ 
				       [ let_xx,
					 
					 [ [sys_slot_descriptions, sys_slots],
					   sys_local_slot_descriptions,
					   sys_options,
					   sys_conc_name,
					   sys_constructors,
					   sys_default_constructor,
					   sys_no_constructor,
					   sys_copier,
					   sys_predicate,
					   sys_predicate_specified,
					   sys_include,
					   sys_print_function,
					   type,
					   sys_named,
					   sys_initial_offset,
					   sys_offset,
					   sys_name_offset,
					   documentation
					 ],
					 
					 [ when,
					   [consp, sys_name],
					   [setq, sys_options, [cdr, sys_name]],
					   [setq, sys_name, [car, sys_name]]
					 ],
					 
					 [ setq,
					   sys_conc_name,
					   
					   [ sys_string_concatenate,
					     [string, sys_name],
					     '$ARRAY'([*],
						      claz_base_character,
						      "-")
					   ]
					 ],
					 
					 [ setq,
					   sys_default_constructor,
					   
					   [ intern,
					     
					     [ sys_string_concatenate,
					       '$ARRAY'([*],
							claz_base_character,
							"MAKE-"),
					       [string, sys_name]
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_copier,
					   
					   [ intern,
					     
					     [ sys_string_concatenate,
					       '$ARRAY'([*],
							claz_base_character,
							"COPY-"),
					       [string, sys_name]
					     ]
					   ],
					   sys_predicate,
					   
					   [ intern,
					     
					     [ sys_string_concatenate,
					       [string, sys_name],
					       '$ARRAY'([*],
							claz_base_character,
							"-P")
					     ]
					   ]
					 ],
					 
					 [ do,
					   
					   [ [sys_os, sys_options, [cdr, sys_os]],
					     [sys_o],
					     [sys_v]
					   ],
					   [[endp, sys_os]],
					   
					   [ cond,
					     
					     [ 
					       [ and,
						 [consp, [car, sys_os]],
						 [not, [endp, [cdar, sys_os]]]
					       ],
					       
					       [ setq,
						 sys_o,
						 [caar, sys_os],
						 sys_v,
						 [cadar, sys_os]
					       ],
					       
					       [ case,
						 sys_o,
						 
						 [ kw_conc_name,
						   
						   [ if,
						     [null, sys_v],
						     [setq, sys_conc_name, []],
						     
						     [ setq,
						       sys_conc_name,
						       sys_v
						     ]
						   ]
						 ],
						 
						 [ kw_constructor,
						   
						   [ if,
						     [null, sys_v],
						     
						     [ setq,
						       sys_no_constructor,
						       t
						     ],
						     
						     [ if,
						       [endp, [cddar, sys_os]],
						       
						       [ setq,
							 sys_constructors,
							 
							 [ cons,
							   sys_v,
							   sys_constructors
							 ]
						       ],
						       
						       [ setq,
							 sys_constructors,
							 
							 [ cons,
							   [cdar, sys_os],
							   sys_constructors
							 ]
						       ]
						     ]
						   ]
						 ],
						 
						 [ kw_copier,
						   [setq, sys_copier, sys_v]
						 ],
						 
						 [ kw_predicate,
						   [setq, sys_predicate, sys_v],
						   
						   [ setq,
						     sys_predicate_specified,
						     t
						   ]
						 ],
						 
						 [ kw_include,
						   
						   [ setq,
						     sys_include,
						     [cdar, sys_os]
						   ],
						   
						   [ unless,
						     
						     [ sys_get_sysprop,
						       sys_v,
						       
						       [ quote,
							 sys_is_a_structure
						       ]
						     ],
						     
						     [ error,
						       '$ARRAY'([*],
								claz_base_character,
								"~S is an illegal included structure."),
						       sys_v
						     ]
						   ]
						 ],
						 
						 [ kw_print_function,
						   
						   [ setq,
						     sys_print_function,
						     sys_v
						   ]
						 ],
						 [kw_type, [setq, type, sys_v]],
						 
						 [ kw_initial_offset,
						   
						   [ setq,
						     sys_initial_offset,
						     sys_v
						   ]
						 ],
						 
						 [ t,
						   
						   [ error,
						     '$ARRAY'([*],
							      claz_base_character,
							      "~S is an illegal defstruct option."),
						     sys_o
						   ]
						 ]
					       ]
					     ],
					     
					     [ t,
					       
					       [ if,
						 [consp, [car, sys_os]],
						 [setq, sys_o, [caar, sys_os]],
						 [setq, sys_o, [car, sys_os]]
					       ],
					       
					       [ case,
						 sys_o,
						 
						 [ kw_constructor,
						   
						   [ setq,
						     sys_constructors,
						     
						     [ cons,
						       sys_default_constructor,
						       sys_constructors
						     ]
						   ]
						 ],
						 
						 [ 
						   [ kw_conc_name,
						     kw_copier,
						     kw_predicate,
						     kw_print_function
						   ]
						 ],
						 [kw_named, [setq, sys_named, t]],
						 
						 [ t,
						   
						   [ error,
						     '$ARRAY'([*],
							      claz_base_character,
							      "~S is an illegal defstruct option."),
						     sys_o
						   ]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_conc_name,
					   [intern, [string, sys_conc_name]]
					 ],
					 
					 [ when,
					   
					   [ and,
					     
					     [ not,
					       [endp, sys_slot_descriptions]
					     ],
					     
					     [ stringp,
					       [car, sys_slot_descriptions]
					     ]
					   ],
					   
					   [ setq,
					     documentation,
					     [car, sys_slot_descriptions]
					   ],
					   
					   [ setq,
					     sys_slot_descriptions,
					     [cdr, sys_slot_descriptions]
					   ]
					 ],
					 
					 [ when,
					   sys_include,
					   
					   [ unless,
					     
					     [ equal,
					       type,
					       
					       [ sys_get_sysprop,
						 [car, sys_include],
						 [quote, sys_structure_type]
					       ]
					     ],
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"~S is an illegal structure include."),
					       [car, sys_include]
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_offset,
					   
					   [ if,
					     sys_include,
					     
					     [ sys_get_sysprop,
					       [car, sys_include],
					       [quote, sys_structure_offset]
					     ],
					     0
					   ]
					 ],
					 
					 [ when,
					   
					   [ and,
					     type,
					     sys_initial_of,
					     
					     [ when,
					       [and, type, sys_initial_offset],
					       
					       [ setq,
						 sys_offset,
						 
						 [ (+),
						   sys_offset,
						   sys_initial_offset
						 ]
					       ]
					     ],
					     
					     [ when,
					       [and, type, sys_named],
					       
					       [ setq,
						 sys_name_offset,
						 sys_offset
					       ],
					       
					       [ setq,
						 sys_offset,
						 ['1+', sys_offset]
					       ]
					     ],
					     
					     [ do,
					       
					       [ 
						 [ sys_ds,
						   sys_slot_descriptions,
						   [cdr, sys_ds]
						 ],
						 [sys_sds, []]
					       ],
					       
					       [ [endp, sys_ds],
						 
						 [ setq,
						   sys_slot_descriptions,
						   [nreverse, sys_sds]
						 ]
					       ],
					       
					       [ push,
						 
						 [ sys_parse_slot_description,
						   [car, sys_ds],
						   sys_offset
						 ],
						 sys_sds
					       ],
					       
					       [ setq,
						 sys_offset,
						 ['1+', sys_offset]
					       ]
					     ],
					     
					     [ when,
					       [and, type, sys_named],
					       
					       [ setq,
						 sys_slot_descriptions,
						 
						 [ cons,
						   [list, [], sys_name],
						   sys_slot_descriptions
						 ]
					       ]
					     ],
					     
					     [ when,
					       [and, type, sys_initial_offset],
					       
					       [ setq,
						 sys_slot_descriptions,
						 
						 [ append,
						   
						   [ make_list,
						     sys_initial_offset
						   ],
						   sys_slot_descriptions
						 ]
					       ]
					     ],
					     
					     [ setq,
					       sys_local_slot_descriptions,
					       sys_slot_descriptions
					     ],
					     
					     [ cond,
					       [[null, sys_include]],
					       
					       [ [endp, [cdr, sys_include]],
						 
						 [ setq,
						   sys_slot_descriptions,
						   
						   [ append,
						     
						     [ sys_get_sysprop,
						       [car, sys_include],
						       
						       [ quote,
							 sys_structure_slot_descriptions
						       ]
						     ],
						     sys_slot_descriptions
						   ]
						 ]
					       ],
					       
					       [ t,
						 
						 [ setq,
						   sys_slot_descriptions,
						   
						   [ append,
						     
						     [ sys_overwrite_slot_descriptions,
						       
						       [ mapcar,
							 function(
								  [ lambda,
								    [sys_sd],
								    
								    [ sys_parse_slot_description,
								      sys_sd,
								      0
								    ]
								  ]),
							 [cdr, sys_include]
						       ],
						       
						       [ sys_get_sysprop,
							 [car, sys_include],
							 
							 [ quote,
							   sys_structure_slot_descriptions
							 ]
						       ]
						     ],
						     sys_slot_descriptions
						   ]
						 ]
					       ]
					     ],
					     
					     [ cond,
					       
					       [ sys_no_constructor,
						 
						 [ when,
						   sys_constructors,
						   
						   [ error,
						     '$ARRAY'([*],
							      claz_base_character,
							      "Contradictory constructor options.")
						   ]
						 ]
					       ],
					       
					       [ [null, sys_constructors],
						 
						 [ setq,
						   sys_constructors,
						   
						   [ list,
						     sys_default_constructor
						   ]
						 ]
					       ]
					     ],
					     
					     [ when,
					       [and, type, [not, sys_named]],
					       
					       [ when,
						 sys_predicate_specified,
						 
						 [ error,
						   '$ARRAY'([*],
							    claz_base_character,
							    "~S is an illegal structure predicate."),
						   sys_predicate
						 ]
					       ],
					       [setq, sys_predicate, []]
					     ],
					     
					     [ when,
					       sys_include,
					       
					       [ setq,
						 sys_include,
						 [car, sys_include]
					       ]
					     ],
					     
					     [ when,
					       [and, sys_print_function, type],
					       
					       [ error,
						 '$ARRAY'([*],
							  claz_base_character,
							  "An print function is supplied to a typed structure.")
					       ]
					     ],
					     
					     [ if,
					       
					       [ or,
						 type,
						 
						 [ not,
						   
						   [ member,
						     [quote, kw_clos],
						     xx_features_xx
						   ]
						 ]
					       ],
					       
					       [ '#BQ',
						 
						 [ eval_when,
						   [compile, load, eval],
						   
						   [ sys_define_structure,
						     
						     [ quote,
						       ['#COMMA', sys_name]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_conc_name
						       ]
						     ],
						     [quote, ['#COMMA', type]],
						     
						     [ quote,
						       ['#COMMA', sys_named]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_slots]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_slot_descriptions
						       ]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_copier]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_include]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_print_function
						       ]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_constructors
						       ]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_offset]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 documentation
						       ]
						     ]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ mapcar,
						       function(
								[ lambda,
								  [sys_constructor],
								  
								  [ sys_make_constructor,
								    sys_name,
								    sys_constructor,
								    type,
								    sys_named,
								    sys_slot_descriptions
								  ]
								]),
						       sys_constructors
						     ]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ when,
						       sys_predicate,
						       
						       [ list,
							 
							 [ '#BQ',
							   
							   [ sys_fset,
							     
							     [ quote,
							       
							       [ '#COMMA',
								 sys_predicate
							       ]
							     ],
							     
							     [ sys_make_predicate,
							       
							       [ quote,
								 ['#COMMA', sys_name]
							       ],
							       
							       [ quote,
								 ['#COMMA', type]
							       ],
							       
							       [ quote,
								 
								 [ '#COMMA',
								   sys_named
								 ]
							       ],
							       
							       [ quote,
								 
								 [ '#COMMA',
								   sys_name_offset
								 ]
							       ]
							     ]
							   ]
							 ]
						       ]
						     ]
						   ],
						   [quote, ['#COMMA', sys_name]]
						 ]
					       ],
					       
					       [ '#BQ',
						 
						 [ eval_when,
						   [compile, load, eval],
						   
						   [ defclass,
						     ['#COMMA', sys_name],
						     
						     [ 
						       [ '#COMMA',
							 
							 [ or,
							   sys_include,
							   
							   [ quote,
							     structure_object
							   ]
							 ]
						       ]
						     ],
						     
						     [ '#COMMA',
						       
						       [ mapcar,
							 function(
								  [ lambda,
								    [sys_sd],
								    
								    [ if,
								      sys_sd,
								      
								      [ list_xx,
									[first, sys_sd],
									kw_initform,
									[second, sys_sd],
									kw_initarg,
									
									[ intern,
									  
									  [ symbol_name,
									    [first, sys_sd]
									  ],
									  
									  [ find_package,
									    [quote, keyword]
									  ]
									],
									
									[ when,
									  [third, sys_sd],
									  
									  [ list,
									    kw_type,
									    [third, sys_sd]
									  ]
									]
								      ],
								      []
								    ]
								  ]),
							 sys_local_slot_descriptions
						       ]
						     ],
						     
						     [ kw_metaclass,
						       structure_class
						     ]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ if,
						       sys_print_function,
						       
						       [ '#BQ',
							 
							 [ 
							   [ defmethod,
							     print_object,
							     
							     [ 
							       [ sys_obj,
								 ['#COMMA', sys_name]
							       ],
							       stream
							     ],
							     
							     [ 
							       [ '#COMMA',
								 sys_print_function
							       ],
							       sys_obj,
							       stream,
							       xx_print_level_xx
							     ]
							   ]
							 ]
						       ]
						     ]
						   ],
						   
						   [ sys_define_structure,
						     
						     [ quote,
						       ['#COMMA', sys_name]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_conc_name
						       ]
						     ],
						     [quote, ['#COMMA', type]],
						     
						     [ quote,
						       ['#COMMA', sys_named]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_slots]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_slot_descriptions
						       ]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_copier]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_include]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_print_function
						       ]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 sys_constructors
						       ]
						     ],
						     
						     [ quote,
						       ['#COMMA', sys_offset]
						     ],
						     
						     [ quote,
						       
						       [ '#COMMA',
							 documentation
						       ]
						     ]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ mapcar,
						       function(
								[ lambda,
								  [sys_constructor],
								  
								  [ sys_make_constructor,
								    sys_name,
								    sys_constructor,
								    type,
								    sys_named,
								    sys_slot_descriptions
								  ]
								]),
						       sys_constructors
						     ]
						   ],
						   
						   [ '#BQ-COMMA-ELIPSE',
						     
						     [ when,
						       sys_predicate,
						       
						       [ list,
							 
							 [ '#BQ',
							   
							   [ sys_fset,
							     
							     [ quote,
							       
							       [ '#COMMA',
								 sys_predicate
							       ]
							     ],
							     
							     [ sys_make_predicate,
							       
							       [ quote,
								 ['#COMMA', sys_name]
							       ],
							       
							       [ quote,
								 ['#COMMA', type]
							       ],
							       
							       [ quote,
								 
								 [ '#COMMA',
								   sys_named
								 ]
							       ],
							       
							       [ quote,
								 
								 [ '#COMMA',
								   sys_name_offset
								 ]
							       ]
							     ]
							   ]
							 ]
						       ]
						     ]
						   ],
						   [quote, ['#COMMA', sys_name]]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ defun,
					   sys_sharp_s_reader,
					   [stream, subchar, arg],
					   [declare, [ignore, subchar]],
					   
					   [ when,
					     
					     [ and,
					       arg,
					       [null, xx_read_suppress_xx]
					     ],
					     
					     [ error,
					       '$ARRAY'([*],
							claz_base_character,
							"An extra argument was supplied for the #S readmacro.")
					     ]
					   ],
					   
					   [ let,
					     [[sys_l, [read, stream]]],
					     
					     [ unless,
					       
					       [ sys_get_sysprop,
						 [car, sys_l],
						 [quote, sys_is_a_structure]
					       ],
					       
					       [ error,
						 '$ARRAY'([*],
							  claz_base_character,
							  "~S is not a structure."),
						 [car, sys_l]
					       ]
					     ],
					     
					     [ do,
					       
					       [ 
						 [ sys_ll,
						   [cdr, sys_l],
						   [cddr, sys_ll]
						 ]
					       ],
					       
					       [ [endp, sys_ll],
						 
						 [ do,
						   
						   [ 
						     [ sys_cs,
						       
						       [ sys_get_sysprop,
							 [car, sys_l],
							 
							 [ quote,
							   sys_structure_constructors
							 ]
						       ],
						       [cdr, sys_cs]
						     ]
						   ],
						   
						   [ [endp, sys_cs],
						     
						     [ error,
						       '$ARRAY'([*],
								claz_base_character,
								"The structure ~S has no structure constructor."),
						       [car, sys_l]
						     ]
						   ],
						   
						   [ when,
						     [symbolp, [car, sys_cs]],
						     
						     [ return,
						       
						       [ apply,
							 [car, sys_cs],
							 [cdr, sys_l]
						       ]
						     ]
						   ]
						 ]
					       ],
					       
					       [ rplaca,
						 sys_ll,
						 
						 [ intern,
						   [string, [car, sys_ll]],
						   [quote, keyword]
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ defun,
					   sys_make_constructor,
					   
					   [ sys_name,
					     sys_constructor,
					     type,
					     sys_named,
					     sys_slot_descriptions
					   ],
					   [declare, [ignore, sys_named]],
					   
					   [ let,
					     
					     [ 
					       [ sys_slot_names,
						 
						 [ mapcar,
						   function(
							    [ lambda,
							      [x],
							      
							      [ cond,
								[[null, x], []],
								
								[ [null, [car, x]],
								  
								  [ list,
								    [quote, quote],
								    [cadr, x]
								  ]
								],
								[t, [car, x]]
							      ]
							    ]),
						   sys_slot_descriptions
						 ]
					       ],
					       
					       [ sys_keys,
						 
						 [ mapcan,
						   function(
							    [ lambda,
							      [x],
							      
							      [ cond,
								[[null, x], []],
								[[null, [car, x]], []],
								
								[ [null, [cadr, x]],
								  [list, [car, x]]
								],
								
								[ t,
								  
								  [ list,
								    
								    [ list,
								      [car, x],
								      [cadr, x]
								    ]
								  ]
								]
							      ]
							    ]),
						   sys_slot_descriptions
						 ]
					       ]
					     ],
					     
					     [ cond,
					       
					       [ [consp, sys_constructor],
						 
						 [ do,
						   
						   [ 
						     [ a,
						       [cadr, sys_constructor],
						       [cdr, a]
						     ],
						     [sys_l, []],
						     [sys_vs, []]
						   ],
						   
						   [ [endp, a],
						     
						     [ setq,
						       sys_keys,
						       
						       [ nreconc,
							 
							 [ cons,
							   [quote, c38_aux],
							   sys_l
							 ],
							 
							 [ mapcan,
							   function(
								    [ lambda,
								      [sys_k],
								      
								      [ if,
									
									[ member,
									  
									  [ if,
									    [atom, sys_k],
									    sys_k,
									    [car, sys_k]
									  ],
									  sys_vs
									],
									[],
									[list, sys_k]
								      ]
								    ]),
							   sys_keys
							 ]
						       ]
						     ]
						   ],
						   
						   [ cond,
						     
						     [ 
						       [ eq,
							 [car, a],
							 [quote, c38_optional]
						       ],
						       
						       [ setq,
							 sys_l,
							 
							 [ cons,
							   
							   [ quote,
							     c38_optional
							   ],
							   sys_l
							 ]
						       ],
						       
						       [ do,
							 
							 [ 
							   [ sys_aa,
							     [cdr, a],
							     [cdr, sys_aa]
							   ],
							   [sys_ov],
							   [sys_y]
							 ],
							 
							 [ [endp, sys_aa],
							   
							   [ setq,
							     sys_keys,
							     
							     [ nreconc,
							       
							       [ cons,
								 [quote, c38_aux],
								 sys_l
							       ],
							       
							       [ mapcan,
								 function(
									  [ lambda,
									    [sys_k],
									    
									    [ if,
									      
									      [ member,
										
										[ if,
										  [atom, sys_k],
										  sys_k,
										  [car, sys_k]
										],
										sys_vs
									      ],
									      [],
									      [list, sys_k]
									    ]
									  ]),
								 sys_keys
							       ]
							     ]
							   ],
							   [return, []]
							 ],
							 
							 [ when,
							   
							   [ member,
							     [car, sys_aa],
							     lambda_list_keywords
							   ],
							   
							   [ when,
							     
							     [ eq,
							       [car, sys_aa],
							       [quote, c38_rest]
							     ],
							     
							     [ setq,
							       sys_l,
							       
							       [ cons,
								 [quote, c38_rest],
								 sys_l
							       ]
							     ],
							     
							     [ setq,
							       sys_aa,
							       [cdr, sys_aa]
							     ],
							     
							     [ unless,
							       
							       [ and,
								 [not, [endp, sys_aa]],
								 
								 [ symbolp,
								   [car, sys_aa]
								 ]
							       ],
							       [sys_illegal_boa]
							     ],
							     
							     [ setq,
							       sys_vs,
							       
							       [ cons,
								 [car, sys_aa],
								 sys_vs
							       ]
							     ],
							     
							     [ setq,
							       sys_l,
							       
							       [ cons,
								 [car, sys_aa],
								 sys_l
							       ]
							     ],
							     
							     [ setq,
							       sys_aa,
							       [cdr, sys_aa]
							     ],
							     
							     [ when,
							       [endp, sys_aa],
							       
							       [ setq,
								 sys_keys,
								 
								 [ nreconc,
								   
								   [ cons,
								     [quote, c38_aux],
								     sys_l
								   ],
								   
								   [ mapcan,
								     function(
									      [ lambda,
										[sys_k],
										
										[ if,
										  
										  [ member,
										    
										    [ if,
										      [atom, sys_k],
										      sys_k,
										      [car, sys_k]
										    ],
										    sys_vs
										  ],
										  [],
										  [list, sys_k]
										]
									      ]),
								     sys_keys
								   ]
								 ]
							       ],
							       [return, []]
							     ]
							   ],
							   
							   [ unless,
							     
							     [ eq,
							       [car, sys_aa],
							       [quote, c38_aux]
							     ],
							     [sys_illegal_boa]
							   ],
							   
							   [ setq,
							     sys_l,
							     
							     [ cons,
							       [quote, c38_aux],
							       sys_l
							     ]
							   ],
							   
							   [ do,
							     
							     [ 
							       [ sys_aaa,
								 [cdr, sys_aa],
								 [cdr, sys_aaa]
							       ]
							     ],
							     [[endp, sys_aaa]],
							     
							     [ setq,
							       sys_l,
							       
							       [ cons,
								 [car, sys_aaa],
								 sys_l
							       ]
							     ],
							     
							     [ cond,
							       
							       [ 
								 [ and,
								   
								   [ atom,
								     [car, sys_aaa]
								   ],
								   
								   [ symbolp,
								     [car, sys_aaa]
								   ]
								 ],
								 
								 [ setq,
								   sys_vs,
								   
								   [ cons,
								     [car, sys_aaa],
								     sys_vs
								   ]
								 ]
							       ],
							       
							       [ 
								 [ and,
								   
								   [ symbolp,
								     [caar, sys_aaa]
								   ],
								   
								   [ or,
								     
								     [ endp,
								       [cdar, sys_aaa]
								     ],
								     
								     [ endp,
								       [cddar, sys_aaa]
								     ]
								   ]
								 ],
								 
								 [ setq,
								   sys_vs,
								   
								   [ cons,
								     [caar, sys_aaa],
								     sys_vs
								   ]
								 ]
							       ],
							       
							       [ t,
								 [sys_illegal_boa]
							       ]
							     ]
							   ],
							   
							   [ setq,
							     sys_keys,
							     
							     [ nreconc,
							       sys_l,
							       
							       [ mapcan,
								 function(
									  [ lambda,
									    [sys_k],
									    
									    [ if,
									      
									      [ member,
										
										[ if,
										  [atom, sys_k],
										  sys_k,
										  [car, sys_k]
										],
										sys_vs
									      ],
									      [],
									      [list, sys_k]
									    ]
									  ]),
								 sys_keys
							       ]
							     ]
							   ],
							   
							   [ setq,
							     sys_keys,
							     
							     [ nreconc,
							       sys_l,
							       
							       [ mapcan,
								 function(
									  [ lambda,
									    [sys_k],
									    
									    [ if,
									      
									      [ member,
										
										[ if,
										  [atom, sys_k],
										  sys_k,
										  [car, sys_k]
										],
										sys_vs
									      ],
									      [],
									      [list, sys_k]
									    ]
									  ]),
								 sys_keys
							       ]
							     ]
							   ],
							   [return, []]
							 ],
							 
							 [ if,
							   
							   [ and,
							     
							     [ cond,
							       
							       [ [atom, [car, sys_aa]],
								 
								 [ setq,
								   sys_ov,
								   [car, sys_aa]
								 ],
								 t
							       ],
							       
							       [ 
								 [ endp,
								   [cdar, sys_aa]
								 ],
								 
								 [ setq,
								   sys_ov,
								   [caar, sys_aa]
								 ],
								 t
							       ],
							       [t, []]
							     ],
							     
							     [ setq,
							       sys_y,
							       
							       [ member,
								 sys_ov,
								 sys_keys,
								 kw_key,
								 function(
									  [ lambda,
									    [x],
									    
									    [ if,
									      [consp, x],
									      [car, x]
									    ]
									  ])
							       ]
							     ]
							   ],
							   
							   [ setq,
							     sys_l,
							     
							     [ cons,
							       [car, sys_y],
							       sys_l
							     ]
							   ],
							   
							   [ setq,
							     sys_l,
							     
							     [ cons,
							       [car, sys_aa],
							       sys_l
							     ]
							   ]
							 ],
							 
							 [ cond,
							   
							   [ [atom, [car, sys_aa]],
							     
							     [ unless,
							       
							       [ symbolp,
								 [car, sys_aa]
							       ],
							       [sys_illegal_boa]
							     ],
							     
							     [ setq,
							       sys_vs,
							       
							       [ cons,
								 [car, sys_aa],
								 sys_vs
							       ]
							     ]
							   ],
							   
							   [ 
							     [ not,
							       
							       [ symbolp,
								 [caar, sys_aa]
							       ]
							     ],
							     [sys_illegal_boa]
							   ],
							   
							   [ 
							     [ or,
							       
							       [ endp,
								 [cdar, sys_aa]
							       ],
							       
							       [ endp,
								 [cddar, sys_aa]
							       ]
							     ],
							     
							     [ setq,
							       sys_vs,
							       
							       [ cons,
								 [caar, sys_aa],
								 sys_vs
							       ]
							     ]
							   ],
							   
							   [ 
							     [ not,
							       
							       [ symbolp,
								 [caddar, sys_aa]
							       ]
							     ],
							     [sys_illegal_boa]
							   ],
							   
							   [ 
							     [ not,
							       
							       [ endp,
								 [cdddar, sys_aa]
							       ]
							     ],
							     [sys_illegal_boa]
							   ],
							   
							   [ t,
							     
							     [ setq,
							       sys_vs,
							       
							       [ cons,
								 [caar, sys_aa],
								 sys_vs
							       ]
							     ],
							     
							     [ setq,
							       sys_vs,
							       
							       [ cons,
								 [caddar, sys_aa],
								 sys_vs
							       ]
							     ]
							   ]
							 ]
						       ],
						       [return, []]
						     ],
						     
						     [ t,
						       
						       [ unless,
							 [symbolp, [car, a]],
							 [sys_illegal_boa]
						       ],
						       
						       [ setq,
							 sys_l,
							 [cons, [car, a], sys_l]
						       ],
						       
						       [ setq,
							 sys_vs,
							 [cons, [car, a], sys_vs]
						       ]
						     ]
						   ]
						 ],
						 
						 [ setq,
						   sys_constructor,
						   [car, sys_constructor]
						 ]
					       ],
					       
					       [ t,
						 
						 [ setq,
						   sys_keys,
						   
						   [ cons,
						     [quote, c38_key],
						     sys_keys
						   ]
						 ]
					       ]
					     ],
					     
					     [ cond,
					       
					       [ [null, type],
						 
						 [ '#BQ',
						   
						   [ defun,
						     
						     [ '#COMMA',
						       sys_constructor
						     ],
						     ['#COMMA', sys_keys],
						     
						     [ sys_make_structure,
						       
						       [ quote,
							 ['#COMMA', sys_name]
						       ],
						       
						       [ '#BQ-COMMA-ELIPSE',
							 sys_slot_names
						       ]
						     ]
						   ]
						 ]
					       ],
					       
					       [ 
						 [ or,
						   [eq, type, [quote, vector]],
						   
						   [ and,
						     [consp, type],
						     
						     [ eq,
						       [car, type],
						       [quote, vector]
						     ]
						   ]
						 ],
						 
						 [ '#BQ',
						   
						   [ defun,
						     
						     [ '#COMMA',
						       sys_constructor
						     ],
						     ['#COMMA', sys_keys],
						     
						     [ vector,
						       
						       [ '#BQ-COMMA-ELIPSE',
							 sys_slot_names
						       ]
						     ]
						   ]
						 ]
					       ],
					       
					       [ [eq, type, [quote, list]],
						 
						 [ '#BQ',
						   
						   [ defun,
						     
						     [ '#COMMA',
						       sys_constructor
						     ],
						     ['#COMMA', sys_keys],
						     
						     [ list,
						       
						       [ '#BQ-COMMA-ELIPSE',
							 sys_slot_names
						       ]
						     ]
						   ]
						 ]
					       ],
					       
					       [ 
						 [ error,
						   '$ARRAY'([*],
							    claz_base_character,
							    "~S is an illegal structure type"),
						   type
						 ]
					       ]
					     ]
					   ]
					 ],
					 
					 [ setq,
					   sys_keys,
					   [cons, [quote, c38_key], sys_keys]
					 ]
				       ]
				     ]))).
*/
/*
:- side_effect(assert_lsp(defstruct,
			  arglist_info(defstruct,
				       mf_defstruct,
				       [sys_name, c38_rest, sys_slots],
				       arginfo{ all:[sys_name],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:[rest],
						env:0,
						key:0,
						names:[sys_name, sys_slots],
						opt:0,
						req:[sys_name],
						rest:[sys_slots],
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(defstruct, init_args(1, mf_defstruct))).
*/
/*
;#+clos
*/
/*
; The defstruct options are supplied.
*/
/*
; The default conc-name.
*/
/*
; The default constructor.
*/
/*
; The default copier and predicate.
*/
/*
; Parse the defstruct options.
*/
/*
; Skip the documentation string.
*/
/*
; Check the include option.
*/
/*
; Set OFFSET.
*/
/*
; Increment OFFSET.
*/
/*
 Increment OFFSET.
*/
/*
; Parse slot-descriptions, incrementing OFFSET for each one.
*/
/*
; If TYPE is non-NIL and structure is named,
*/
/*
;  add the slot for the structure-name to the slot-descriptions.
*/
/*
; Pad the slot-descriptions with the initial-offset number of NILs.
*/
/*
;#+clos
*/
/*
; Append the slot-descriptions of the included structure.
*/
/*
; The slot-descriptions in the include option are also counted.
*/
/*
; If a constructor option is NIL,
*/
/*
;  no constructor should have been specified.
*/
/*
; If no constructor is specified,
*/
/*
;  the default-constructor is made.
*/
/*
; Check the named option and set the predicate.
*/
/*
; Check the print-function.
*/
/*
; else (and (not type) (member :CLOS *features*))
*/
/*
 for initial offset slots
*/
/*
	   (with-slots (defstruct-form slot-descriptions initial-offset
			 constructors documentation copier predicate
			 print-function)
		       (find-class ',name)
              (setq defstruct-form '(defstruct ,name ,@slots))
	      (setq slot-descriptions ',slot-descriptions)
	      (setq initial-offset ',structure-offset)
	      (setq constructors ',constructors)
	      (setq documentation ,documentation)
	      (setq copier ,copier)
	      (setq pre	   (with-slots (defstruct-form slot-descriptions initial-offset
			 constructors documentation copier predicate
			 print-function)
		       (find-class ',name)
              (setq defstruct-form '(defstruct ,name ,@slots))
	      (setq slot-descriptions ',slot-descriptions)
	      (setq initial-offset ',structure-offset)
	      (setq constructors ',constructors)
	      (setq documentation ,documentation)
	      (setq copier ,copier)
	      (setq predicate ,predicate)
	      (setq print-function ,print-function))
*/
/*
;; The #S reader.
*/
/*
; Intern keywords in the keyword package.
*/
/*
; Find an appropriate construtor.
*/
/*
; Collect the slot-names.
*/
/*
; If the slot-description is NIL,
*/
/*
;  it is in the padding of initial-offset.
*/
/*
; If the slot name is NIL,
*/
/*
;  it is the structure name.
*/
/*
;  This is for typed structures with names.
*/
/*
; Make the keyword parameters.
*/
/*
; The case for a BOA constructor.
*/
/*
; Dirty code!!
*/
/*
; We must add an initial value for an optional parameter,
*/
/*
;  if the default value is not specified
*/
/*
;  in the given parameter list and yet the initial value
*/
/*
;  is supplied in the slot description.
*/
/*
; Add those options that do not appear in the parameter list
*/
/*
;  as auxiliary paramters.
*/
/*
; The parameters are accumulated in the variable VS.
*/
/*
; Skip until &OPTIONAL ap; Skip until &OPTIONAL appears.
*/
/*
; Add those options that do not appear in the
*/
/*
;  parameter list.
*/
/*
; &REST is found.
*/
/*
; &AUX should follow.
*/
/*
; End of the parameter list.
*/
/*
 End of the parameter list.
*/
/*
; Checks if the optional paramter without a default
*/
/*
;  value has a default value in the slot-description.
*/
/*
; With default value.
*/
/*
; If no default value is supplied for
*/
/*
;  the optional parameter and yet appears
*/
/*
;  in KEYS with a default value,
*/
/*
;  then cons the pair to L,
*/
/*
;  otherwise cons just the parameter to L.
*/
/*
; Checks the form of the optional parameter.
*/
/*
; RETURN from the outside DO.
*/
/*
; If not a BOA constructor, just cons &KEY.
*/
/*
 If not a BOA constructor, just cons &KEY.
*/


%; Total compilation time: 44.73 seconds

