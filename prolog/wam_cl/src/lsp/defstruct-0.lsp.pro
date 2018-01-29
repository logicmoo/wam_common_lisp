#!/usr/bin/env swipl
%; WAM-CL translated Lisp File (see https://github.com/TeamSPoon/wam_common_lisp/tree/master/prolog/wam_cl )
%; File: "lib/lsp/defstruct-0" (/home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp)
%; PWD: /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/
%; Start time: Sun Jan 28 15:54:10 2018

:-style_check(-discontiguous).
:-style_check(-singleton).
:-use_module(library(wamcl_runtime)).

/*
(in-package "SYSTEM")


*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp:0 **********************/
:-lisp_compile_to_prolog(pkg_user,['in-package','$STRING'("SYSTEM")])
/*
% macroexpand:-[in_package,'$ARRAY'([*],claz_base_character,"SYSTEM")].
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
                        (error ""(defmacro defstruct (name &rest slots)\n  \"Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure).\"\n  (let*((slot-descriptions slots)\n\t;;#+clos\n\tlocal-slot-descriptions\n        options\n        conc-name\n        constructors default-constructor no-constructor\n        copier\n        predicate predicate-specified\n        include\n        print-function type named initial-offset\n        offset name-offset\n        documentation)\n\n    (when (consp name)\n          ;; The defstruct options are supplied.\n          (setq options (cdr name))\n          (setq name (car name)))\n\n    ;; The default conc-name.\n    (setq conc-name (string-concatenate (string name) \"-\"))\n\n    ;; The default constructor.\n    (setq default-constructor\n          (intern (string-concatenate \"MAKE-\" (string name))))\n\n    ;; The default copier and predicate.\n    (setq copier\n          (intern (string-concatenate \"COPY-\" (string name)))\n          predicate\n          (intern (string-concatenate (string name) \"-P\")))\n\n    ;; Parse the defstruct options.\n    (do ((os options (cdr os)) (o) (v))\n        ((endp os))\n      (cond ((and (consp (car os)) (not (endp (cdar os))))\n             (setq o (caar os) v (cadar os))\n             (case o\n               (:CONC-NAME\n                (if (null v)\n                    (setq conc-name nil)\n                    (setq conc-name v)))\n               (:CONSTRUCTOR\n                (if (null v)\n                    (setq no-constructor t)\n                    (if (endp (cddar os))\n                        (setq constructors (cons v constructors))\n                        (setq constructors (cons (cdar os) constructors)))))\n               (:COPIER (setq copier v))\n               (:PREDICATE\n                (setq predicate v)\n                (setq predicate-specified t))\n               (:INCLUDE\n                (setq include (cdar os))\n                (unless (get-sysprop v 'IS-A-STRUCTURE)\n                        (error \"~S is an illegal included structure.\" v)))\n               (:PRINT-FUNCTION (setq print-function v))\n               (:TYPE (setq type v))\n               (:INITIAL-OFFSET (setq initial-offset v))\n               (t (error \"~S is an illegal defstruct option.\" o))))\n            (t\n             (if (consp (car os))\n                 (setq o (caar os))\n                 (setq o (car os)))\n             (case o\n               (:CONSTRUCTOR\n                (setq constructors\n                      (cons default-constructor constructors)))\n               ((:CONC-NAME :COPIER :PREDICATE :PRINT-FUNCTION))\n               (:NAMED (setq named t))\n               (t (error \"~S is an illegal defstruct option.\" o))))))\n\n    (setq conc-name (intern (string conc-name)))\n\n    ;; Skip the documentation string.\n    (when (and (not (endp slot-descriptions))\n               (stringp (car slot-descriptions)))\n          (setq documentation (car slot-descriptions))\n          (setq slot-descriptions (cdr slot-descriptions)))\n    \n    ;; Check the include option.\n    (when include\n          (unless (equal type (get-sysprop (car include) 'STRUCTURE-TYPE))\n                  (error \"~S is an illegal structure include.\"\n                         (car include))))\n\n    ;; Set OFFSET.\n    (setq offset (if include\n\t\t     (get-sysprop (car include) 'STRUCTURE-OFFSET)\n\t\t     0))\n\n    ;; Increment OFFSET.\n    (when (and type initial-offset)\n          (setq offset (+ offset initial-offset)))\n    (when (and type named)\n          (setq name-offset offset)\n          (setq offset (1+ offset)))\n\n    ;; Parse slot-descriptions, incrementing OFFSET for each one.\n    (do ((ds slot-descriptions (cdr ds))\n         (sds nil))\n        ((endp ds)\n         (setq slot-descriptions (nreverse sds)))\n      (push (parse-slot-description (car ds) offset) sds)\n      (setq offset (1+ offset)))\n\n    ;; If TYPE is non-NIL and structure is named,\n    ;;  add the slot for the structure-name to the slot-descriptions.\n    (when (and type named)\n          (setq slot-descriptions\n                (cons (list nil name) slot-descriptions)))\n\n    ;; Pad the slot-descriptions with the initial-offset number of NILs.\n    (when (and type initial-offset)\n          (setq slot-descriptions\n                (append (make-list initial-offset) slot-descriptions)))\n\n    ;;#+clos\n    (setq local-slot-descriptions slot-descriptions)\n\n    ;; Append the slot-descriptions of the included structure.\n    ;; The slot-descriptions in the include option are also counted.\n    (cond ((null include))\n          ((endp (cdr include))\n           (setq slot-descriptions\n                 (append (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS)\n                         slot-descriptions)))\n          (t\n           (setq slot-descriptions\n                 (append (overwrite-slot-descriptions\n                          (mapcar #'(lambda (sd)\n                                      (parse-slot-description sd 0))\n                                  (cdr include))\n                          (get-sysprop (car include) 'STRUCTURE-SLOT-DESCRIPTIONS))\n                         slot-descriptions))))\n\n    (cond (no-constructor\n           ;; If a constructor option is NIL,\n           ;;  no constructor should have been specified.\n           (when constructors\n                 (error \"Contradictory constructor options.\")))\n          ((null constructors)\n           ;; If no constructor is specified,\n           ;;  the default-constructor is made.\n           (setq constructors (list default-constructor))))\n\n    ;; Check the named option and set the predicate.\n    (when (and type (not named))\n          (when predicate-specified\n                (error \"~S is an illegal structure predicate.\"\n                       predicate))\n          (setq predicate nil))\n\n    (when include (setq include (car include)))\n\n    ;; Check the print-function.\n    (when (and print-function type)\n          (error \"An print function is supplied to a typed structure.\"))\n\n    (if (or type (not (member ':CLOS *features*)))\n\t`(eval-when (compile load eval)\n\n\t  (define-structure ',name ',conc-name ',type ',named ',slots\n\t\t\t    ',slot-descriptions ',copier ',include\n\t\t\t    ',print-function ',constructors ',offset\n\t\t\t    ',documentation)\n\t  ,@(mapcar #'(lambda (constructor)\n\t\t\t(make-constructor name constructor type named\n\t\t\t\t\t  slot-descriptions))\n\t     constructors)\n\t  ,@(when predicate\n\t      (list `(fset ',predicate\n\t\t      (make-predicate ',name ',type ',named ',name-offset))))\n\t  ',name)\n\n      ;; else (and (not type) (member :CLOS *features*))\n\n      `(eval-when (compile load eval)\n\n\t(defclass ,name (,(or include 'STRUCTURE-OBJECT))\n\t  ,(mapcar\n\t    #'(lambda (sd)\n\t\t(if sd\n\t\t    (list* (first sd)\n\t\t\t   :initform (second sd)\n\t\t\t   :initarg \n\t\t\t   (intern (symbol-name (first sd))\n\t\t\t\t   (find-package 'KEYWORD))\n\t\t\t   (when (third sd) (list :type (third sd))))\n\t\t    nil))\t\t; for initial offset slots\n\t    local-slot-descriptions)\n\t  (:metaclass structure-class))\n\n#|\t   (with-slots (defstruct-form slot-descriptions initial-offset\n\t\t\t constructors documentation copier predicate\n\t\t\t print-function)\n\t\t       (find-class ',name)\n              (setq defstruct-form '(defstruct ,name ,@slots))\n\t      (setq slot-descriptions ',slot-descriptions)\n\t      (setq initial-offset ',structure-offset)\n\t      (setq constructors ',constructors)\n\t      (setq documentation ,documentation)\n\t      (setq copier ,copier)\n\t      (setq predicate ,predicate)\n\t      (setq print-function ,print-function))\n|#\n\n\t,@(if print-function\n\t      `((defmethod print-object\n\t\t    ((obj ,name) stream)\n\t\t  (,print-function obj stream *print-level*))))\n\n\t(define-structure ',name ',conc-name ',type ',named ',slots\n\t\t\t  ',slot-descriptions ',copier ',include\n\t\t\t  ',print-function ',constructors ',offset\n\t\t\t  ',documentation)\n\t,@(mapcar #'(lambda (constructor)\n\t\t      (make-constructor name constructor type named\n\t\t\t\t\tslot-descriptions))\n\t   constructors)\n\t,@(when predicate\n\t    (list `(fset ',predicate\n\t\t    (make-predicate ',name ',type ',named ',name-offset))))\n\t',name))))\n\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp:50 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defmacro,defstruct,[name,'&rest',slots],'$STRING'("Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure)."),['let*',[['slot-descriptions',slots],;,;,options,'conc-name',constructors,'default-constructor','no-constructor',copier,predicate,'predicate-specified',include,'print-function',type,named,'initial-offset',offset,'name-offset',documentation],[when,[consp,name],;,;,'The',defstruct,options,are,'supplied.',[setq,options,[cdr,name]],[setq,name,[car,name]]],;,;,'The',default,'conc-name.',[setq,'conc-name',['string-concatenate',[string,name],'$STRING'("-")]],;,;,'The',default,'constructor.',[setq,'default-constructor',[intern,['string-concatenate','$STRING'("MAKE-"),[string,name]]]],;,;,'The',default,copier,and,'predicate.',[setq,copier,[intern,['string-concatenate','$STRING'("COPY-"),[string,name]]],predicate,[intern,['string-concatenate',[string,name],'$STRING'("-P")]]],;,;,'Parse',the,defstruct,'options.',[do,[[os,options,[cdr,os]],[o],[v]],[[endp,os]],[cond,[[and,[consp,[car,os]],[not,[endp,[cdar,os]]]],[setq,o,[caar,os],v,[cadar,os]],[case,o,[':CONC-NAME',[if,[null,v],[setq,'conc-name',[]],[setq,'conc-name',v]]],[':CONSTRUCTOR',[if,[null,v],[setq,'no-constructor',t],[if,[endp,[cddar,os]],[setq,constructors,[cons,v,constructors]],[setq,constructors,[cons,[cdar,os],constructors]]]]],[':COPIER',[setq,copier,v]],[':PREDICATE',[setq,predicate,v],[setq,'predicate-specified',t]],[':INCLUDE',[setq,include,[cdar,os]],[unless,['get-sysprop',v,[quote,'IS-A-STRUCTURE']],[error,'$STRING'("~S is an illegal included structure."),v]]],[':PRINT-FUNCTION',[setq,'print-function',v]],[':TYPE',[setq,type,v]],[':INITIAL-OFFSET',[setq,'initial-offset',v]],[t,[error,'$STRING'("~S is an illegal defstruct option."),o]]]],[t,[if,[consp,[car,os]],[setq,o,[caar,os]],[setq,o,[car,os]]],[case,o,[':CONSTRUCTOR',[setq,constructors,[cons,'default-constructor',constructors]]],[[':CONC-NAME',':COPIER',':PREDICATE',':PRINT-FUNCTION']],[':NAMED',[setq,named,t]],[t,[error,'$STRING'("~S is an illegal defstruct option."),o]]]]]],[setq,'conc-name',[intern,[string,'conc-name']]],;,;,'Skip',the,documentation,'string.',[when,[and,[not,[endp,'slot-descriptions']],[stringp,[car,'slot-descriptions']]],[setq,documentation,[car,'slot-descriptions']],[setq,'slot-descriptions',[cdr,'slot-descriptions']]],;,;,'Check',the,include,'option.',[when,include,[unless,[equal,type,['get-sysprop',[car,include],[quote,'STRUCTURE-TYPE']]],[error,'$STRING'("~S is an illegal structure include."),[car,include]]]],;,;,'Set','OFFSET.',[setq,offset,[if,include,['get-sysprop',[car,include],[quote,'STRUCTURE-OFFSET']],0]],;,;,'Increment','OFFSET.',[when,[and,type,'initial-offset'],[setq,offset,[+,offset,'initial-offset']]],[when,[and,type,named],[setq,'name-offset',offset],[setq,offset,['1+',offset]]],;,;,'Parse','slot-descriptions,',incrementing,'OFFSET',for,each,'one.',[do,[[ds,'slot-descriptions',[cdr,ds]],[sds,[]]],[[endp,ds],[setq,'slot-descriptions',[nreverse,sds]]],[push,['parse-slot-description',[car,ds],offset],sds],[setq,offset,['1+',offset]]],;,;,'If','TYPE',is,'non-NIL',and,structure,is,'named,',;,;,add,the,slot,for,the,'structure-name',to,the,'slot-descriptions.',[when,[and,type,named],[setq,'slot-descriptions',[cons,[list,[],name],'slot-descriptions']]],;,;,'Pad',the,'slot-descriptions',with,the,'initial-offset',number,of,'NILs.',[when,[and,type,'initial-offset'],[setq,'slot-descriptions',[append,['make-list','initial-offset'],'slot-descriptions']]],;,;,;,;,'Append',the,'slot-descriptions',of,the,included,'structure.',;,;,'The','slot-descriptions',in,the,include,option,are,also,'counted.',[cond,[[null,include]],[[endp,[cdr,include]],[setq,'slot-descriptions',[append,['get-sysprop',[car,include],[quote,'STRUCTURE-SLOT-DESCRIPTIONS']],'slot-descriptions']]],[t,[setq,'slot-descriptions',[append,['overwrite-slot-descriptions',[mapcar,function([lambda,[sd],['parse-slot-description',sd,0]]),[cdr,include]],['get-sysprop',[car,include],[quote,'STRUCTURE-SLOT-DESCRIPTIONS']]],'slot-descriptions']]]],[cond,['no-constructor',;,;,'If',a,constructor,option,is,'NIL,',;,;,no,constructor,should,have,been,'specified.',[when,constructors,[error,'$STRING'("Contradictory constructor options.")]]],[[null,constructors],;,;,'If',no,constructor,is,'specified,',;,;,the,'default-constructor',is,'made.',[setq,constructors,[list,'default-constructor']]]],;,;,'Check',the,named,option,and,set,the,'predicate.',[when,[and,type,[not,named]],[when,'predicate-specified',[error,'$STRING'("~S is an illegal structure predicate."),predicate]],[setq,predicate,[]]],[when,include,[setq,include,[car,include]]],;,;,'Check',the,'print-function.',[when,[and,'print-function',type],[error,'$STRING'("An print function is supplied to a typed structure.")]],[if,[or,type,[not,[member,[quote,':CLOS'],'*features*']]],['#BQ',['eval-when',[compile,load,eval],['define-structure',[quote,['#COMMA',name]],[quote,['#COMMA','conc-name']],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA',slots]],[quote,['#COMMA','slot-descriptions']],[quote,['#COMMA',copier]],[quote,['#COMMA',include]],[quote,['#COMMA','print-function']],[quote,['#COMMA',constructors]],[quote,['#COMMA',offset]],[quote,['#COMMA',documentation]]],['#BQ-COMMA-ELIPSE',[mapcar,function([lambda,[constructor],['make-constructor',name,constructor,type,named,'slot-descriptions']]),constructors]],['#BQ-COMMA-ELIPSE',[when,predicate,[list,['#BQ',[fset,[quote,['#COMMA',predicate]],['make-predicate',[quote,['#COMMA',name]],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA','name-offset']]]]]]]],[quote,['#COMMA',name]]]],;,;,else,[and,[not,type],[member,':CLOS','*features*']],['#BQ',['eval-when',[compile,load,eval],[defclass,['#COMMA',name],[['#COMMA',[or,include,[quote,'STRUCTURE-OBJECT']]]],['#COMMA',[mapcar,function([lambda,[sd],[if,sd,['list*',[first,sd],':initform',[second,sd],':initarg',[intern,['symbol-name',[first,sd]],['find-package',[quote,'KEYWORD']]],[when,[third,sd],[list,':type',[third,sd]]]],[]]]),;,for,initial,offset,slots,'local-slot-descriptions']],[':metaclass','structure-class']],#,'\t   (with-slots (defstruct-form slot-descriptions initial-offset\n\t\t\t constructors documentation copier predicate\n\t\t\t print-function)\n\t\t       (find-class \',name)\n              (setq defstruct-form \'(defstruct ,name ,@slots))\n\t      (setq slot-descriptions \',slot-descriptions)\n\t      (setq initial-offset \',structure-offset)\n\t      (setq constructors \',constructors)\n\t      (setq documentation ,documentation)\n\t      (setq copier ,copier)\n\t      (setq predicate ,predicate)\n\t      (setq print-function ,print-function))\n',#,['#BQ-COMMA-ELIPSE',[if,'print-function',['#BQ',[[defmethod,'print-object',[[obj,['#COMMA',name]],stream],[['#COMMA','print-function'],obj,stream,'*print-level*']]]]]],['define-structure',[quote,['#COMMA',name]],[quote,['#COMMA','conc-name']],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA',slots]],[quote,['#COMMA','slot-descriptions']],[quote,['#COMMA',copier]],[quote,['#COMMA',include]],[quote,['#COMMA','print-function']],[quote,['#COMMA',constructors]],[quote,['#COMMA',offset]],[quote,['#COMMA',documentation]]],['#BQ-COMMA-ELIPSE',[mapcar,function([lambda,[constructor],['make-constructor',name,constructor,type,named,'slot-descriptions']]),constructors]],['#BQ-COMMA-ELIPSE',[when,predicate,[list,['#BQ',[fset,[quote,['#COMMA',predicate]],['make-predicate',[quote,['#COMMA',name]],[quote,['#COMMA',type]],[quote,['#COMMA',named]],[quote,['#COMMA','name-offset']]]]]]]],[quote,['#COMMA',name]]]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       defstruct,
					       kw_macro,
					       mf_defstruct)).
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
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_string_concatenate,
					       kw_function,
					       f_sys_string_concatenate)).
*/
/*
% case:-[[kw_conc_name,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]],[kw_constructor,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]],[kw_copier,[setq,sys_copier,sys_v]],[kw_predicate,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]],[kw_include,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]],[kw_print_function,[setq,sys_print_function,sys_v]],[kw_type,[setq,type,sys_v]],[kw_initial_offset,[setq,sys_initial_offset,sys_v]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_501296,[quote,kw_conc_name]],[progn,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]]],[[eq,_501296,[quote,kw_constructor]],[progn,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]]],[[eq,_501296,[quote,kw_copier]],[progn,[setq,sys_copier,sys_v]]],[[eq,_501296,[quote,kw_predicate]],[progn,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]]],[[eq,_501296,[quote,kw_include]],[progn,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]]],[[eq,_501296,[quote,kw_print_function]],[progn,[setq,sys_print_function,sys_v]]],[[eq,_501296,[quote,kw_type]],[progn,[setq,type,sys_v]]],[[eq,_501296,[quote,kw_initial_offset]],[progn,[setq,sys_initial_offset,sys_v]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
% case:-[[kw_constructor,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]],[[kw_conc_name,kw_copier,kw_predicate,kw_print_function]],[kw_named,[setq,sys_named,t]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_672206,[quote,kw_constructor]],[progn,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]]],[[sys_memq,_672206,[quote,[kw_conc_name,kw_copier,kw_predicate,kw_print_function]]],[progn]],[[eq,_672206,[quote,kw_named]],[progn,[setq,sys_named,t]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
% case:-[[kw_conc_name,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]],[kw_constructor,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]],[kw_copier,[setq,sys_copier,sys_v]],[kw_predicate,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]],[kw_include,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]],[kw_print_function,[setq,sys_print_function,sys_v]],[kw_type,[setq,type,sys_v]],[kw_initial_offset,[setq,sys_initial_offset,sys_v]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_508150,[quote,kw_conc_name]],[progn,[if,[null,sys_v],[setq,sys_conc_name,[]],[setq,sys_conc_name,sys_v]]]],[[eq,_508150,[quote,kw_constructor]],[progn,[if,[null,sys_v],[setq,sys_no_constructor,t],[if,[endp,[cddar,sys_os]],[setq,sys_constructors,[cons,sys_v,sys_constructors]],[setq,sys_constructors,[cons,[cdar,sys_os],sys_constructors]]]]]],[[eq,_508150,[quote,kw_copier]],[progn,[setq,sys_copier,sys_v]]],[[eq,_508150,[quote,kw_predicate]],[progn,[setq,sys_predicate,sys_v],[setq,sys_predicate_specified,t]]],[[eq,_508150,[quote,kw_include]],[progn,[setq,sys_include,[cdar,sys_os]],[unless,[sys_get_sysprop,sys_v,[quote,sys_is_a_structure]],[error,'$ARRAY'([*],claz_base_character,"~S is an illegal included structure."),sys_v]]]],[[eq,_508150,[quote,kw_print_function]],[progn,[setq,sys_print_function,sys_v]]],[[eq,_508150,[quote,kw_type]],[progn,[setq,type,sys_v]]],[[eq,_508150,[quote,kw_initial_offset]],[progn,[setq,sys_initial_offset,sys_v]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
% case:-[[kw_constructor,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]],[[kw_conc_name,kw_copier,kw_predicate,kw_print_function]],[kw_named,[setq,sys_named,t]],[t,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]].
*/
/*
% conds:-[[[eq,_679132,[quote,kw_constructor]],[progn,[setq,sys_constructors,[cons,sys_default_constructor,sys_constructors]]]],[[sys_memq,_679132,[quote,[kw_conc_name,kw_copier,kw_predicate,kw_print_function]]],[progn]],[[eq,_679132,[quote,kw_named]],[progn,[setq,sys_named,t]]],[t,[progn,[error,'$ARRAY'([*],claz_base_character,"~S is an illegal defstruct option."),sys_o]]]].
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_parse_slot_description,
					       kw_function,
					       f_sys_parse_slot_description)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_overwrite_slot_descriptions,
					       kw_function,
					       f_sys_overwrite_slot_descriptions)).
*/
doc: doc_string(defstruct,
	      _19470,
	      function,
	      "Syntax: (defstruct\n         {name | (name {:conc-name | (:conc-name prefix-string) |\n                        :constructor | (:constructor symbol [lambda-list]) |\n                        :copier | (:copier symbol) |\n                        :predicate | (:predicate symbol) |\n                        (:include symbol) |\n                        (:print-function function) |\n                        (:type {vector | (vector type) | list}) |\n                        :named |\n                        (:initial-offset number)}*)}\n         [doc]\n         {slot-name |\n          (slot-name [default-value-form] {:type type | :read-only flag}*) }*\n         )\nDefines a structure named by NAME.  The doc-string DOC, if supplied, is saved\nas a STRUCTURE doc and can be retrieved by (documentation 'NAME 'structure).").

wl:lambda_def(defmacro, defstruct, mf_defstruct, [sys_name, c38_rest, sys_slots], [[let_xx, [[sys_slot_descriptions, sys_slots], ;, ;, sys_options, sys_conc_name, sys_constructors, sys_default_constructor, sys_no_constructor, sys_copier, sys_predicate, sys_predicate_specified, sys_include, sys_print_function, type, sys_named, sys_initial_offset, sys_offset, sys_name_offset, documentation], [when, [consp, sys_name], ;, ;, the, defstruct, sys_options, sys_are, sys_supplied_c46, [setq, sys_options, [cdr, sys_name]], [setq, sys_name, [car, sys_name]]], ;, ;, the, sys_default, sys_conc_name_c46, [setq, sys_conc_name, [sys_string_concatenate, [string, sys_name], '$ARRAY'([*], claz_base_character, "-")]], ;, ;, the, sys_default, sys_constructor_c46, [setq, sys_default_constructor, [intern, [sys_string_concatenate, '$ARRAY'([*], claz_base_character, "MAKE-"), [string, sys_name]]]], ;, ;, the, sys_default, sys_copier, and, sys_predicate_c46, [setq, sys_copier, [intern, [sys_string_concatenate, '$ARRAY'([*], claz_base_character, "COPY-"), [string, sys_name]]], sys_predicate, [intern, [sys_string_concatenate, [string, sys_name], '$ARRAY'([*], claz_base_character, "-P")]]], ;, ;, sys_parse, the, defstruct, sys_options_c46, [do, [[sys_os, sys_options, [cdr, sys_os]], [sys_o], [sys_v]], [[endp, sys_os]], [cond, [[and, [consp, [car, sys_os]], [not, [endp, [cdar, sys_os]]]], [setq, sys_o, [caar, sys_os], sys_v, [cadar, sys_os]], [case, sys_o, [kw_conc_name, [if, [null, sys_v], [setq, sys_conc_name, []], [setq, sys_conc_name, sys_v]]], [kw_constructor, [if, [null, sys_v], [setq, sys_no_constructor, t], [if, [endp, [cddar, sys_os]], [setq, sys_constructors, [cons, sys_v, sys_constructors]], [setq, sys_constructors, [cons, [cdar, sys_os], sys_constructors]]]]], [kw_copier, [setq, sys_copier, sys_v]], [kw_predicate, [setq, sys_predicate, sys_v], [setq, sys_predicate_specified, t]], [kw_include, [setq, sys_include, [cdar, sys_os]], [unless, [sys_get_sysprop, sys_v, [quote, sys_is_a_structure]], [error, '$ARRAY'([*], claz_base_character, "~S is an illegal included structure."), sys_v]]], [kw_print_function, [setq, sys_print_function, sys_v]], [kw_type, [setq, type, sys_v]], [kw_initial_offset, [setq, sys_initial_offset, sys_v]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), sys_o]]]], [t, [if, [consp, [car, sys_os]], [setq, sys_o, [caar, sys_os]], [setq, sys_o, [car, sys_os]]], [case, sys_o, [kw_constructor, [setq, sys_constructors, [cons, sys_default_constructor, sys_constructors]]], [[kw_conc_name, kw_copier, kw_predicate, kw_print_function]], [kw_named, [setq, sys_named, t]], [t, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), sys_o]]]]]], [setq, sys_conc_name, [intern, [string, sys_conc_name]]], ;, ;, sys_skip, the, documentation, sys_string_c46, [when, [and, [not, [endp, sys_slot_descriptions]], [stringp, [car, sys_slot_descriptions]]], [setq, documentation, [car, sys_slot_descriptions]], [setq, sys_slot_descriptions, [cdr, sys_slot_descriptions]]], ;, ;, sys_check, the, sys_include, sys_option_c46, [when, sys_include, [unless, [equal, type, [sys_get_sysprop, [car, sys_include], [quote, sys_structure_type]]], [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure include."), [car, sys_include]]]], ;, ;, set, sys_offset_c46, [setq, sys_offset, [if, sys_include, [sys_get_sysprop, [car, sys_include], [quote, sys_structure_offset]], 0]], ;, ;, sys_increment, sys_offset_c46, [when, [and, type, sys_initial_offset], [setq, sys_offset, [+, sys_offset, sys_initial_offset]]], [when, [and, type, sys_named], [setq, sys_name_offset, sys_offset], [setq, sys_offset, ['1+', sys_offset]]], ;, ;, sys_parse, sys_slot_descriptions_c44, sys_incrementing, sys_offset, sys_for, sys_each, sys_one_c46, [do, [[sys_ds, sys_slot_descriptions, [cdr, sys_ds]], [sys_sds, []]], [[endp, sys_ds], [setq, sys_slot_descriptions, [nreverse, sys_sds]]], [push, [sys_parse_slot_description, [car, sys_ds], sys_offset], sys_sds], [setq, sys_offset, ['1+', sys_offset]]], ;, ;, if, type, sys_is, sys_non_nil, and, structure, sys_is, sys_named_c44, ;, ;, sys_add, the, sys_slot, sys_for, the, sys_structure_name, sys_to, the, sys_slot_descriptions_c46, [when, [and, type, sys_named], [setq, sys_slot_descriptions, [cons, [list, [], sys_name], sys_slot_descriptions]]], ;, ;, sys_pad, the, sys_slot_descriptions, sys_with, the, sys_initial_offset, number, sys_of, sys_nils_c46, [when, [and, type, sys_initial_offset], [setq, sys_slot_descriptions, [append, [make_list, sys_initial_offset], sys_slot_descriptions]]], ;, ;, ;, ;, append, the, sys_slot_descriptions, sys_of, the, sys_included, sys_structure_c46, ;, ;, the, sys_slot_descriptions, sys_in, the, sys_include, sys_option, sys_are, sys_also, sys_counted_c46, [cond, [[null, sys_include]], [[endp, [cdr, sys_include]], [setq, sys_slot_descriptions, [append, [sys_get_sysprop, [car, sys_include], [quote, sys_structure_slot_descriptions]], sys_slot_descriptions]]], [t, [setq, sys_slot_descriptions, [append, [sys_overwrite_slot_descriptions, [mapcar, function([lambda, [sys_sd], [sys_parse_slot_description, sys_sd, 0]]), [cdr, sys_include]], [sys_get_sysprop, [car, sys_include], [quote, sys_structure_slot_descriptions]]], sys_slot_descriptions]]]], [cond, [sys_no_constructor, ;, ;, if, sys_a, sys_constructor, sys_option, sys_is, sys_nil_c44, ;, ;, sys_no, sys_constructor, sys_should, sys_have, sys_been, sys_specified_c46, [when, sys_constructors, [error, '$ARRAY'([*], claz_base_character, "Contradictory constructor options.")]]], [[null, sys_constructors], ;, ;, if, sys_no, sys_constructor, sys_is, sys_specified_c44, ;, ;, the, sys_default_constructor, sys_is, sys_made_c46, [setq, sys_constructors, [list, sys_default_constructor]]]], ;, ;, sys_check, the, sys_named, sys_option, and, set, the, sys_predicate_c46, [when, [and, type, [not, sys_named]], [when, sys_predicate_specified, [error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure predicate."), sys_predicate]], [setq, sys_predicate, []]], [when, sys_include, [setq, sys_include, [car, sys_include]]], ;, ;, sys_check, the, sys_print_function_c46, [when, [and, sys_print_function, type], [error, '$ARRAY'([*], claz_base_character, "An print function is supplied to a typed structure.")]], [if, [or, type, [not, [member, [quote, kw_clos], xx_features_xx]]], ['#BQ', [eval_when, [compile, load, eval], [sys_define_structure, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', sys_conc_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_slots]], [quote, ['#COMMA', sys_slot_descriptions]], [quote, ['#COMMA', sys_copier]], [quote, ['#COMMA', sys_include]], [quote, ['#COMMA', sys_print_function]], [quote, ['#COMMA', sys_constructors]], [quote, ['#COMMA', sys_offset]], [quote, ['#COMMA', documentation]]], ['#BQ-COMMA-ELIPSE', [mapcar, function([lambda, [sys_constructor], [sys_make_constructor, sys_name, sys_constructor, type, sys_named, sys_slot_descriptions]]), sys_constructors]], ['#BQ-COMMA-ELIPSE', [when, sys_predicate, [list, ['#BQ', [sys_fset, [quote, ['#COMMA', sys_predicate]], [sys_make_predicate, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_name_offset]]]]]]]], [quote, ['#COMMA', sys_name]]]], ;, ;, sys_else, [and, [not, type], [member, kw_clos, xx_features_xx]], ['#BQ', [eval_when, [compile, load, eval], [defclass, ['#COMMA', sys_name], [['#COMMA', [or, sys_include, [quote, structure_object]]]], ['#COMMA', [mapcar, function([lambda, [sys_sd], [if, sys_sd, [list_xx, [first, sys_sd], kw_initform, [second, sys_sd], kw_initarg, [intern, [symbol_name, [first, sys_sd]], [find_package, [quote, keyword]]], [when, [third, sys_sd], [list, kw_type, [third, sys_sd]]]], []]]), ;, sys_for, sys_initial, sys_offset, sys_slots, sys_local_slot_descriptions]], [kw_metaclass, structure_class]], #, sys_c9_c32_c32_c32_c40_with_slots_c32_c40_defstruct_form_c32_slot_descriptions_c32_initial_offset_c10_c9_c9_c9_c32_constructors_c32_documentation_c32_copier_c32_predicate_c10_c9_c9_c9_c32_print_function_c41_c10_c9_c9_c32_c32_c32_c32_c32_c32_c32_c40_find_class_c32_c39_c44_name_c41_c10_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c40_setq_c32_defstruct_form_c32_c39_c40_defstruct_c32_c44_name_c32_c44_c64_slots_c41_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_slot_descriptions_c32_c39_c44_slot_descriptions_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_initial_offset_c32_c39_c44_structure_offset_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_constructors_c32_c39_c44_constructors_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_documentation_c32_c44_documentation_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_copier_c32_c44_copier_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_predicate_c32_c44_predicate_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_print_function_c32_c44_print_function_c41_c41_c10, #, ['#BQ-COMMA-ELIPSE', [if, sys_print_function, ['#BQ', [[defmethod, print_object, [[sys_obj, ['#COMMA', sys_name]], stream], [['#COMMA', sys_print_function], sys_obj, stream, xx_print_level_xx]]]]]], [sys_define_structure, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', sys_conc_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_slots]], [quote, ['#COMMA', sys_slot_descriptions]], [quote, ['#COMMA', sys_copier]], [quote, ['#COMMA', sys_include]], [quote, ['#COMMA', sys_print_function]], [quote, ['#COMMA', sys_constructors]], [quote, ['#COMMA', sys_offset]], [quote, ['#COMMA', documentation]]], ['#BQ-COMMA-ELIPSE', [mapcar, function([lambda, [sys_constructor], [sys_make_constructor, sys_name, sys_constructor, type, sys_named, sys_slot_descriptions]]), sys_constructors]], ['#BQ-COMMA-ELIPSE', [when, sys_predicate, [list, ['#BQ', [sys_fset, [quote, ['#COMMA', sys_predicate]], [sys_make_predicate, [quote, ['#COMMA', sys_name]], [quote, ['#COMMA', type]], [quote, ['#COMMA', sys_named]], [quote, ['#COMMA', sys_name_offset]]]]]]]], [quote, ['#COMMA', sys_name]]]]]]]).
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
		  LEnv13=[bv(;, [])|LEnv],
		  LEnv16=[bv(;, [])|LEnv13],
		  LEnv19=[bv(sys_options, [])|LEnv16],
		  LEnv22=[bv(sys_conc_name, [])|LEnv19],
		  LEnv25=[bv(sys_constructors, [])|LEnv22],
		  LEnv28=[bv(sys_default_constructor, [])|LEnv25],
		  LEnv31=[bv(sys_no_constructor, [])|LEnv28],
		  LEnv34=[bv(sys_copier, [])|LEnv31],
		  LEnv37=[bv(sys_predicate, [])|LEnv34],
		  LEnv40=[bv(sys_predicate_specified, [])|LEnv37],
		  LEnv43=[bv(sys_include, [])|LEnv40],
		  LEnv46=[bv(sys_print_function, [])|LEnv43],
		  LEnv49=[bv(type, [])|LEnv46],
		  LEnv52=[bv(sys_named, [])|LEnv49],
		  LEnv55=[bv(sys_initial_offset, [])|LEnv52],
		  LEnv58=[bv(sys_offset, [])|LEnv55],
		  LEnv61=[bv(sys_name_offset, [])|LEnv58],
		  LEnv64=[bv(documentation, [])|LEnv61],
		  get_var(LEnv64, sys_name, Name_Get),
		  (   c0nz:is_consp(Name_Get)
		  ->  get_var(LEnv64, ;, C59_Get70),
		      get_var(LEnv64, defstruct, Defstruct_Get),
		      get_var(LEnv64, sys_are, Are_Get),
		      get_var(LEnv64, sys_name, Name_Get77),
		      get_var(LEnv64, sys_options, Options_Get),
		      get_var(LEnv64, sys_supplied_c46, Supplied_c46_Get),
		      get_var(LEnv64, the, The_Get),
		      f_cdr(Name_Get77, Options),
		      set_var(LEnv64, sys_options, Options),
		      get_var(LEnv64, sys_name, Name_Get78),
		      f_car(Name_Get78, TrueResult),
		      set_var(LEnv64, sys_name, TrueResult),
		      _21128=TrueResult
		  ;   _21128=[]
		  ),
		  get_var(LEnv64, ;, C59_Get80),
		  get_var(LEnv64, sys_conc_name_c46, Conc_name_c46_Get),
		  get_var(LEnv64, sys_default, Default_Get),
		  get_var(LEnv64, sys_name, Name_Get85),
		  get_var(LEnv64, the, The_Get82),
		  f_string(Name_Get85, String_concatenate_Param),
		  f_sys_string_concatenate(String_concatenate_Param,
					   '$ARRAY'([*],
						    claz_base_character,
						    "-"),
					   Conc_name),
		  set_var(LEnv64, sys_conc_name, Conc_name),
		  get_var(LEnv64, ;, C59_Get86),
		  get_var(LEnv64, sys_constructor_c46, Constructor_c46_Get),
		  get_var(LEnv64, sys_default, Default_Get89),
		  get_var(LEnv64, sys_name, Name_Get91),
		  get_var(LEnv64, the, The_Get88),
		  f_string(Name_Get91, String_Ret),
		  f_sys_string_concatenate('$ARRAY'([*],
						    claz_base_character,
						    "MAKE-"),
					   String_Ret,
					   Intern_Param),
		  f_intern(Intern_Param, Default_constructor),
		  set_var(LEnv64, sys_default_constructor, Default_constructor),
		  get_var(LEnv64, ;, C59_Get92),
		  get_var(LEnv64, and, And_Get),
		  get_var(LEnv64, sys_copier, Copier_Get),
		  get_var(LEnv64, sys_default, Default_Get95),
		  get_var(LEnv64, sys_name, Name_Get99),
		  get_var(LEnv64, sys_predicate_c46, Predicate_c46_Get),
		  get_var(LEnv64, the, The_Get94),
		  f_string(Name_Get99, String_Ret675),
		  f_sys_string_concatenate('$ARRAY'([*],
						    claz_base_character,
						    "COPY-"),
					   String_Ret675,
					   Intern_Param656),
		  f_intern(Intern_Param656, Copier),
		  set_var(LEnv64, sys_copier, Copier),
		  get_var(LEnv64, sys_name, Name_Get100),
		  f_string(Name_Get100, String_concatenate_Param657),
		  f_sys_string_concatenate(String_concatenate_Param657,
					   '$ARRAY'([*],
						    claz_base_character,
						    "-P"),
					   Intern_Param658),
		  f_intern(Intern_Param658, Predicate),
		  set_var(LEnv64, sys_predicate, Predicate),
		  get_var(LEnv64, ;, C59_Get101),
		  get_var(LEnv64, defstruct, Defstruct_Get105),
		  get_var(LEnv64, sys_options, Options_Get110),
		  get_var(LEnv64, sys_options_c46, Options_c46_Get),
		  get_var(LEnv64, sys_parse, Parse_Get),
		  get_var(LEnv64, the, The_Get104),
		  AEnv=[bv(sys_os, Options_Get110), bv([sys_o], []), bv([sys_v], [])|LEnv64],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_1), get_var(AEnv, sys_os, Os_Get229), (s3q:is_endp(Os_Get229)->throw(block_exit([], [])), _TBResult=ThrowResult233;get_var(AEnv, sys_os, Os_Get238), f_car(Os_Get238, PredArgResult240), (c0nz:is_consp(PredArgResult240)->get_var(AEnv, sys_os, Os_Get241), f_cdar(Os_Get241, Endp_Param), f_endp(Endp_Param, Not_Param), f_not(Not_Param, TrueResult242), IFTEST235=TrueResult242;IFTEST235=[]), (IFTEST235\==[]->get_var(AEnv, sys_os, Os_Get244), f_caar(Os_Get244, O), set_var(AEnv, sys_o, O), get_var(AEnv, sys_os, Os_Get245), f_cadar(Os_Get245, V), set_var(AEnv, sys_v, V), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_conc_name)->get_var(AEnv, sys_v, IFTEST251), (IFTEST251==[]->set_var(AEnv, sys_conc_name, []), TrueResult309=[];get_var(AEnv, sys_v, V_Get254), set_var(AEnv, sys_conc_name, V_Get254), TrueResult309=V_Get254), TrueResult335=TrueResult309;(is_eq(Key, kw_constructor)->get_var(AEnv, sys_v, IFTEST258), (IFTEST258==[]->set_var(AEnv, sys_no_constructor, t), TrueResult307=t;get_var(AEnv, sys_os, Os_Get262), f_cddar(Os_Get262, PredArgResult264), (s3q:is_endp(PredArgResult264)->get_var(AEnv, sys_constructors, Constructors_Get266), get_var(AEnv, sys_v, V_Get265), TrueResult269=[V_Get265|Constructors_Get266], set_var(AEnv, sys_constructors, TrueResult269), ElseResult271=TrueResult269;get_var(AEnv, sys_os, Os_Get267), f_cdar(Os_Get267, Cdar_Ret), get_var(AEnv, sys_constructors, Constructors_Get268), ElseResult270=[Cdar_Ret|Constructors_Get268], set_var(AEnv, sys_constructors, ElseResult270), ElseResult271=ElseResult270), TrueResult307=ElseResult271), ElseResult310=TrueResult307;(is_eq(Key, kw_copier)->get_var(AEnv, sys_v, V_Get274), set_var(AEnv, sys_copier, V_Get274), ElseResult308=V_Get274;(is_eq(Key, kw_predicate)->get_var(AEnv, sys_v, V_Get277), set_var(AEnv, sys_predicate, V_Get277), set_var(AEnv, sys_predicate_specified, t), ElseResult306=t;(is_eq(Key, kw_include)->get_var(AEnv, sys_os, Os_Get280), f_cdar(Os_Get280, Include), set_var(AEnv, sys_include, Include), get_var(AEnv, sys_v, V_Get283), f_sys_get_sysprop(V_Get283, sys_is_a_structure, [], IFTEST281), (IFTEST281\==[]->TrueResult302=[];get_var(AEnv, sys_v, V_Get284), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal included structure."), V_Get284], ElseResult285), TrueResult302=ElseResult285), ElseResult304=TrueResult302;(is_eq(Key, kw_print_function)->get_var(AEnv, sys_v, V_Get288), set_var(AEnv, sys_print_function, V_Get288), ElseResult303=V_Get288;(is_eq(Key, kw_type)->get_var(AEnv, sys_v, V_Get291), set_var(AEnv, type, V_Get291), ElseResult301=V_Get291;(is_eq(Key, kw_initial_offset)->get_var(AEnv, sys_v, V_Get294), set_var(AEnv, sys_initial_offset, V_Get294), ElseResult299=V_Get294;get_var(AEnv, sys_o, O_Get295), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get295], ElseResult297), ElseResult299=ElseResult297), ElseResult301=ElseResult299), ElseResult303=ElseResult301), ElseResult304=ElseResult303), ElseResult306=ElseResult304), ElseResult308=ElseResult306), ElseResult310=ElseResult308), TrueResult335=ElseResult310), _25066=TrueResult335;get_var(AEnv, sys_os, Os_Get312), f_car(Os_Get312, PredArgResult314), (c0nz:is_consp(PredArgResult314)->get_var(AEnv, sys_os, Os_Get315), f_caar(Os_Get315, TrueResult317), set_var(AEnv, sys_o, TrueResult317), _26906=TrueResult317;get_var(AEnv, sys_os, Os_Get316), f_car(Os_Get316, ElseResult318), set_var(AEnv, sys_o, ElseResult318), _26906=ElseResult318), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_constructor)->get_var(AEnv, sys_constructors, Constructors_Get325), get_var(AEnv, sys_default_constructor, Default_constructor_Get324), TrueResult333=[Default_constructor_Get324|Constructors_Get325], set_var(AEnv, sys_constructors, TrueResult333), ElseResult336=TrueResult333;f_sys_memq(Key, [kw_conc_name, kw_copier, kw_predicate, kw_print_function], IFTEST326), (IFTEST326\==[]->ElseResult334=[];(is_eq(Key, kw_named)->set_var(AEnv, sys_named, t), ElseResult332=t;get_var(AEnv, sys_o, O_Get330), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get330], ElseResult331), ElseResult332=ElseResult331), ElseResult334=ElseResult332), ElseResult336=ElseResult334), _25066=ElseResult336), get_var(AEnv, sys_os, Os_Get337), f_cdr(Os_Get337, Os), set_var(AEnv, sys_os, Os), goto(do_label_1, AEnv), _TBResult=_GORES338)),
					  
					  [ addr(addr_tagbody_1_do_label_1,
						 do_label_1,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_os, Os_Get), (s3q:is_endp(Os_Get)->throw(block_exit([], [])), _27672=ThrowResult;get_var(AEnv, sys_os, Os_Get123), f_car(Os_Get123, PredArgResult125), (c0nz:is_consp(PredArgResult125)->get_var(AEnv, sys_os, Os_Get126), f_cdar(Os_Get126, Endp_Param661), f_endp(Endp_Param661, Not_Param662), f_not(Not_Param662, TrueResult127), IFTEST120=TrueResult127;IFTEST120=[]), (IFTEST120\==[]->get_var(AEnv, sys_os, Os_Get129), f_caar(Os_Get129, Caar_Ret), set_var(AEnv, sys_o, Caar_Ret), get_var(AEnv, sys_os, Os_Get130), f_cadar(Os_Get130, Cadar_Ret), set_var(AEnv, sys_v, Cadar_Ret), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_conc_name)->get_var(AEnv, sys_v, IFTEST136), (IFTEST136==[]->set_var(AEnv, sys_conc_name, []), TrueResult194=[];get_var(AEnv, sys_v, V_Get139), set_var(AEnv, sys_conc_name, V_Get139), TrueResult194=V_Get139), TrueResult220=TrueResult194;(is_eq(Key, kw_constructor)->get_var(AEnv, sys_v, IFTEST143), (IFTEST143==[]->set_var(AEnv, sys_no_constructor, t), TrueResult192=t;get_var(AEnv, sys_os, Os_Get147), f_cddar(Os_Get147, PredArgResult149), (s3q:is_endp(PredArgResult149)->get_var(AEnv, sys_constructors, Get_var_Ret), get_var(AEnv, sys_v, V_Get150), TrueResult154=[V_Get150|Get_var_Ret], set_var(AEnv, sys_constructors, TrueResult154), ElseResult156=TrueResult154;get_var(AEnv, sys_os, Os_Get152), f_cdar(Os_Get152, Cdar_Ret680), get_var(AEnv, sys_constructors, Constructors_Get153), ElseResult155=[Cdar_Ret680|Constructors_Get153], set_var(AEnv, sys_constructors, ElseResult155), ElseResult156=ElseResult155), TrueResult192=ElseResult156), ElseResult195=TrueResult192;(is_eq(Key, kw_copier)->get_var(AEnv, sys_v, V_Get159), set_var(AEnv, sys_copier, V_Get159), ElseResult193=V_Get159;(is_eq(Key, kw_predicate)->get_var(AEnv, sys_v, V_Get162), set_var(AEnv, sys_predicate, V_Get162), set_var(AEnv, sys_predicate_specified, t), ElseResult191=t;(is_eq(Key, kw_include)->get_var(AEnv, sys_os, Os_Get165), f_cdar(Os_Get165, Cdar_Ret681), set_var(AEnv, sys_include, Cdar_Ret681), get_var(AEnv, sys_v, V_Get168), f_sys_get_sysprop(V_Get168, sys_is_a_structure, [], IFTEST166), (IFTEST166\==[]->TrueResult187=[];get_var(AEnv, sys_v, V_Get169), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal included structure."), V_Get169], ElseResult170), TrueResult187=ElseResult170), ElseResult189=TrueResult187;(is_eq(Key, kw_print_function)->get_var(AEnv, sys_v, V_Get173), set_var(AEnv, sys_print_function, V_Get173), ElseResult188=V_Get173;(is_eq(Key, kw_type)->get_var(AEnv, sys_v, V_Get176), set_var(AEnv, type, V_Get176), ElseResult186=V_Get176;(is_eq(Key, kw_initial_offset)->get_var(AEnv, sys_v, V_Get179), set_var(AEnv, sys_initial_offset, V_Get179), ElseResult184=V_Get179;get_var(AEnv, sys_o, O_Get180), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get180], ElseResult182), ElseResult184=ElseResult182), ElseResult186=ElseResult184), ElseResult188=ElseResult186), ElseResult189=ElseResult188), ElseResult191=ElseResult189), ElseResult193=ElseResult191), ElseResult195=ElseResult193), TrueResult220=ElseResult195), _28290=TrueResult220;get_var(AEnv, sys_os, Os_Get197), f_car(Os_Get197, PredArgResult199), (c0nz:is_consp(PredArgResult199)->get_var(AEnv, sys_os, Os_Get200), f_caar(Os_Get200, TrueResult202), set_var(AEnv, sys_o, TrueResult202), _28348=TrueResult202;get_var(AEnv, sys_os, Os_Get201), f_car(Os_Get201, ElseResult203), set_var(AEnv, sys_o, ElseResult203), _28348=ElseResult203), get_var(AEnv, sys_o, Key), (is_eq(Key, kw_constructor)->get_var(AEnv, sys_constructors, Constructors_Get210), get_var(AEnv, sys_default_constructor, Get_var_Ret682), TrueResult218=[Get_var_Ret682|Constructors_Get210], set_var(AEnv, sys_constructors, TrueResult218), ElseResult221=TrueResult218;f_sys_memq(Key, [kw_conc_name, kw_copier, kw_predicate, kw_print_function], IFTEST211), (IFTEST211\==[]->ElseResult219=[];(is_eq(Key, kw_named)->set_var(AEnv, sys_named, t), ElseResult217=t;get_var(AEnv, sys_o, O_Get215), f_error(['$ARRAY'([*], claz_base_character, "~S is an illegal defstruct option."), O_Get215], ElseResult216), ElseResult217=ElseResult216), ElseResult219=ElseResult217), ElseResult221=ElseResult219), _28290=ElseResult221), get_var(AEnv, sys_os, Os_Get222), f_cdr(Os_Get222, Cdr_Ret), set_var(AEnv, sys_os, Cdr_Ret), goto(do_label_1, AEnv), _27672=_GORES)))
					  ]),
			  []=LetResult108
			),
			block_exit([], LetResult108),
			true),
		  get_var(LEnv64, sys_conc_name, Conc_name_Get),
		  f_string(Conc_name_Get, Intern_Param663),
		  f_intern(Intern_Param663, Conc_name648),
		  set_var(LEnv64, sys_conc_name, Conc_name648),
		  get_var(LEnv64, ;, C59_Get343),
		  get_var(LEnv64, documentation, Documentation_Get),
		  get_var(LEnv64, sys_skip, Skip_Get),
		  get_var(LEnv64, sys_slot_descriptions, Slot_descriptions_Get),
		  get_var(LEnv64, sys_string_c46, String_c46_Get),
		  get_var(LEnv64, the, The_Get346),
		  f_endp(Slot_descriptions_Get, PredArgResult354),
		  (   PredArgResult354==[]
		  ->  get_var(LEnv64,
			      sys_slot_descriptions,
			      Slot_descriptions_Get355),
		      f_car(Slot_descriptions_Get355, Stringp_Param),
		      f_stringp(Stringp_Param, TrueResult356),
		      IFTEST349=TrueResult356
		  ;   IFTEST349=[]
		  ),
		  (   IFTEST349\==[]
		  ->  get_var(LEnv64,
			      sys_slot_descriptions,
			      Slot_descriptions_Get357),
		      f_car(Slot_descriptions_Get357, Documentation),
		      set_var(LEnv64, documentation, Documentation),
		      get_var(LEnv64,
			      sys_slot_descriptions,
			      Slot_descriptions_Get358),
		      f_cdr(Slot_descriptions_Get358, TrueResult359),
		      set_var(LEnv64, sys_slot_descriptions, TrueResult359),
		      _28682=TrueResult359
		  ;   _28682=[]
		  ),
		  get_var(LEnv64, ;, C59_Get360),
		  get_var(LEnv64, sys_check, Check_Get),
		  get_var(LEnv64, sys_include, IFTEST366),
		  get_var(LEnv64, sys_option_c46, Option_c46_Get),
		  get_var(LEnv64, the, The_Get363),
		  (   IFTEST366\==[]
		  ->  get_var(LEnv64, sys_include, Include_Get371),
		      get_var(LEnv64, type, Type_Get),
		      f_car(Include_Get371, Get_sysprop_Param),
		      f_sys_get_sysprop(Get_sysprop_Param,
					sys_structure_type,
					[],
					PredArg2Result),
		      (   is_equal(Type_Get, PredArg2Result)
		      ->  TrueResult377=[]
		      ;   get_var(LEnv64, sys_include, Include_Get375),
			  f_car(Include_Get375, Car_Ret),
			  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "~S is an illegal structure include."),
				    Car_Ret
				  ],
				  ElseResult376),
			  TrueResult377=ElseResult376
		      ),
		      _29082=TrueResult377
		  ;   _29082=[]
		  ),
		  get_var(LEnv64, ;, C59_Get378),
		  get_var(LEnv64, set, Set_Get),
		  get_var(LEnv64, sys_include, IFTEST382),
		  get_var(LEnv64, sys_offset_c46, Offset_c46_Get),
		  (   IFTEST382\==[]
		  ->  get_var(LEnv64, sys_include, Include_Get385),
		      f_car(Include_Get385, Get_sysprop_Param666),
		      f_sys_get_sysprop(Get_sysprop_Param666,
					sys_structure_offset,
					[],
					TrueResult386),
		      Offset=TrueResult386
		  ;   Offset=0
		  ),
		  set_var(LEnv64, sys_offset, Offset),
		  get_var(LEnv64, ;, C59_Get387),
		  get_var(LEnv64, sys_increment, Increment_Get),
		  get_var(LEnv64, sys_offset_c46, Offset_c46_Get390),
		  get_var(LEnv64, type, IFTEST393),
		  (   IFTEST393\==[]
		  ->  get_var(LEnv64, sys_initial_offset, Initial_offset_Get),
		      IFTEST391=Initial_offset_Get
		  ;   IFTEST391=[]
		  ),
		  (   IFTEST391\==[]
		  ->  get_var(LEnv64, sys_initial_offset, Initial_offset_Get399),
		      get_var(LEnv64, sys_offset, Offset_Get),
		      'f_+'(Offset_Get, Initial_offset_Get399, TrueResult400),
		      set_var(LEnv64, sys_offset, TrueResult400),
		      _29682=TrueResult400
		  ;   _29682=[]
		  ),
		  get_var(LEnv64, type, IFTEST403),
		  (   IFTEST403\==[]
		  ->  get_var(LEnv64, sys_named, Named_Get),
		      IFTEST401=Named_Get
		  ;   IFTEST401=[]
		  ),
		  (   IFTEST401\==[]
		  ->  get_var(LEnv64, sys_offset, Offset_Get408),
		      set_var(LEnv64, sys_name_offset, Offset_Get408),
		      get_var(LEnv64, sys_offset, Offset_Get409),
		      'f_1+'(Offset_Get409, TrueResult410),
		      set_var(LEnv64, sys_offset, TrueResult410),
		      _29910=TrueResult410
		  ;   _29910=[]
		  ),
		  get_var(LEnv64, ;, C59_Get411),
		  get_var(LEnv64, sys_each, Each_Get),
		  get_var(LEnv64, sys_for, For_Get),
		  get_var(LEnv64, sys_incrementing, Incrementing_Get),
		  get_var(LEnv64, sys_offset, Offset_Get416),
		  get_var(LEnv64, sys_one_c46, One_c46_Get),
		  get_var(LEnv64, sys_parse, Parse_Get413),
		  get_var(LEnv64,
			  sys_slot_descriptions,
			  Slot_descriptions_Get423),
		  get_var(LEnv64,
			  sys_slot_descriptions_c44,
			  Slot_descriptions_c44_Get),
		  BlockExitEnv=[bv(sys_ds, Slot_descriptions_Get423), bv(sys_sds, [])|LEnv64],
		  catch(( call_addr_block(BlockExitEnv,
					  (push_label(do_label_2), get_var(BlockExitEnv, sys_ds, Ds_Get444), (s3q:is_endp(Ds_Get444)->get_var(BlockExitEnv, sys_sds, Sds_Get450), f_nreverse(Sds_Get450, RetResult447), set_var(BlockExitEnv, sys_slot_descriptions, RetResult447), throw(block_exit([], RetResult447)), _TBResult425=ThrowResult448;sf_push(BlockExitEnv, [sys_parse_slot_description, [car, sys_ds], sys_offset], sys_sds, Sds), get_var(BlockExitEnv, sys_offset, Offset_Get452), 'f_1+'(Offset_Get452, Offset652), set_var(BlockExitEnv, sys_offset, Offset652), get_var(BlockExitEnv, sys_ds, Ds_Get453), f_cdr(Ds_Get453, Ds), set_var(BlockExitEnv, sys_ds, Ds), goto(do_label_2, BlockExitEnv), _TBResult425=_GORES454)),
					  
					  [ addr(addr_tagbody_2_do_label_2,
						 do_label_2,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_ds, Ds_Get), (s3q:is_endp(Ds_Get)->get_var(AEnv, sys_sds, Nreverse_Param), f_nreverse(Nreverse_Param, RetResult430), set_var(AEnv, sys_slot_descriptions, RetResult430), throw(block_exit([], RetResult430)), _TBResult425=ThrowResult431;sf_push(AEnv, [sys_parse_slot_description, [car, sys_ds], sys_offset], sys_sds, Sf_push_Ret), get_var(AEnv, sys_offset, Offset_Get436), 'f_1+'(Offset_Get436, Set_var_Ret), set_var(AEnv, sys_offset, Set_var_Ret), get_var(AEnv, sys_ds, Ds_Get437), f_cdr(Ds_Get437, Cdr_Ret687), set_var(AEnv, sys_ds, Cdr_Ret687), goto(do_label_2, AEnv), _TBResult425=_GORES438)))
					  ]),
			  []=LetResult421
			),
			block_exit([], LetResult421),
			true),
		  get_var(LEnv64, ;, C59_Get458),
		  get_var(LEnv64, and, And_Get464),
		  get_var(LEnv64, if, If_Get),
		  get_var(LEnv64, structure, Structure_Get),
		  get_var(LEnv64, sys_add, Add_Get),
		  get_var(LEnv64, sys_for, For_Get473),
		  get_var(LEnv64, sys_is, Is_Get466),
		  get_var(LEnv64, sys_named_c44, Named_c44_Get),
		  get_var(LEnv64, sys_non_nil, Non_nil_Get),
		  get_var(LEnv64, sys_slot, Slot_Get),
		  get_var(LEnv64,
			  sys_slot_descriptions_c46,
			  Slot_descriptions_c46_Get),
		  get_var(LEnv64, sys_structure_name, Structure_name_Get),
		  get_var(LEnv64, sys_to, To_Get),
		  get_var(LEnv64, the, The_Get471),
		  get_var(LEnv64, type, Type_Get461),
		  (   Type_Get461\==[]
		  ->  get_var(LEnv64, sys_named, Named_Get484),
		      IFTEST479=Named_Get484
		  ;   IFTEST479=[]
		  ),
		  (   IFTEST479\==[]
		  ->  get_var(LEnv64, sys_name, Name_Get486),
		      CAR=[[], Name_Get486],
		      get_var(LEnv64,
			      sys_slot_descriptions,
			      Slot_descriptions_Get487),
		      TrueResult488=[CAR|Slot_descriptions_Get487],
		      set_var(LEnv64, sys_slot_descriptions, TrueResult488),
		      _31740=TrueResult488
		  ;   _31740=[]
		  ),
		  get_var(LEnv64, ;, C59_Get489),
		  get_var(LEnv64, number, Number_Get),
		  get_var(LEnv64, sys_initial_offset, Initial_offset_Get496),
		  get_var(LEnv64, sys_nils_c46, Nils_c46_Get),
		  get_var(LEnv64, sys_of, Of_Get),
		  get_var(LEnv64, sys_pad, Pad_Get),
		  get_var(LEnv64,
			  sys_slot_descriptions,
			  Slot_descriptions_Get493),
		  get_var(LEnv64, sys_with, With_Get),
		  get_var(LEnv64, the, The_Get492),
		  get_var(LEnv64, type, IFTEST502),
		  (   IFTEST502\==[]
		  ->  get_var(LEnv64, sys_initial_offset, Initial_offset_Get505),
		      IFTEST500=Initial_offset_Get505
		  ;   IFTEST500=[]
		  ),
		  (   IFTEST500\==[]
		  ->  get_var(LEnv64, sys_initial_offset, Initial_offset_Get507),
		      f_make_list(Initial_offset_Get507, Append_Param),
		      get_var(LEnv64,
			      sys_slot_descriptions,
			      Slot_descriptions_Get508),
		      f_append(Append_Param,
			       Slot_descriptions_Get508,
			       TrueResult509),
		      set_var(LEnv64, sys_slot_descriptions, TrueResult509),
		      _32222=TrueResult509
		  ;   _32222=[]
		  ),
		  get_var(LEnv64, ;, C59_Get510),
		  get_var(LEnv64, append, Append_Get),
		  get_var(LEnv64, sys_also, Also_Get),
		  get_var(LEnv64, sys_are, Are_Get529),
		  get_var(LEnv64, sys_counted_c46, Counted_c46_Get),
		  get_var(LEnv64, sys_in, In_Get),
		  get_var(LEnv64, sys_include, Include_Get527),
		  get_var(LEnv64, sys_included, Included_Get),
		  get_var(LEnv64, sys_of, Of_Get517),
		  get_var(LEnv64, sys_option, Option_Get),
		  get_var(LEnv64,
			  sys_slot_descriptions,
			  Slot_descriptions_Get516),
		  get_var(LEnv64, sys_structure_c46, Structure_c46_Get),
		  get_var(LEnv64, the, The_Get515),
		  (   Include_Get527==[]
		  ->  _32984=[]
		  ;   get_var(LEnv64, sys_include, Include_Get536),
		      f_cdr(Include_Get536, PredArgResult538),
		      (   s3q:is_endp(PredArgResult538)
		      ->  get_var(LEnv64, sys_include, Include_Get539),
			  f_car(Include_Get539, Get_sysprop_Param669),
			  f_sys_get_sysprop(Get_sysprop_Param669,
					    sys_structure_slot_descriptions,
					    [],
					    Append_Param670),
			  get_var(LEnv64,
				  sys_slot_descriptions,
				  Slot_descriptions_Get540),
			  f_append(Append_Param670,
				   Slot_descriptions_Get540,
				   TrueResult549),
			  set_var(LEnv64, sys_slot_descriptions, TrueResult549),
			  ElseResult551=TrueResult549
		      ;   get_var(LEnv64, sys_include, Include_Get546),
			  f_cdr(Include_Get546, Cdr_Ret689),
			  f_mapcar(closure(kw_function,
					   [ClosureEnvironment|LEnv64],
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
				   [Cdr_Ret689],
				   Slot_descriptions_Param),
			  get_var(LEnv64, sys_include, Include_Get547),
			  f_car(Include_Get547, Get_sysprop_Param671),
			  f_sys_get_sysprop(Get_sysprop_Param671,
					    sys_structure_slot_descriptions,
					    [],
					    Get_sysprop_Ret),
			  f_sys_overwrite_slot_descriptions(Slot_descriptions_Param,
							    Get_sysprop_Ret,
							    Append_Param673),
			  get_var(LEnv64,
				  sys_slot_descriptions,
				  Slot_descriptions_Get548),
			  f_append(Append_Param673,
				   Slot_descriptions_Get548,
				   ElseResult550),
			  set_var(LEnv64, sys_slot_descriptions, ElseResult550),
			  ElseResult551=ElseResult550
		      ),
		      _32984=ElseResult551
		  ),
		  get_var(LEnv64, sys_no_constructor, IFTEST552),
		  (   IFTEST552\==[]
		  ->  get_var(LEnv64, ;, C59_Get555),
		      get_var(LEnv64, if, If_Get557),
		      get_var(LEnv64, sys_a, A_Get),
		      get_var(LEnv64, sys_been, Been_Get),
		      get_var(LEnv64, sys_constructor, Constructor_Get566),
		      get_var(LEnv64, sys_constructors, IFTEST571),
		      get_var(LEnv64, sys_have, Have_Get),
		      get_var(LEnv64, sys_is, Is_Get561),
		      get_var(LEnv64, sys_nil_c44, Nil_c44_Get),
		      get_var(LEnv64, sys_no, No_Get),
		      get_var(LEnv64, sys_option, Option_Get560),
		      get_var(LEnv64, sys_should, Should_Get),
		      get_var(LEnv64, sys_specified_c46, Specified_c46_Get),
		      (   IFTEST571\==[]
		      ->  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "Contradictory constructor options.")
				  ],
				  TrueResult574),
			  TrueResult594=TrueResult574
		      ;   TrueResult594=[]
		      ),
		      _33522=TrueResult594
		  ;   get_var(LEnv64, sys_constructors, IFTEST575),
		      (   IFTEST575==[]
		      ->  get_var(LEnv64, ;, C59_Get578),
			  get_var(LEnv64, if, If_Get580),
			  get_var(LEnv64, sys_constructor, Constructor_Get582),
			  get_var(LEnv64,
				  sys_default_constructor,
				  Default_constructor_Get588),
			  get_var(LEnv64, sys_is, Is_Get583),
			  get_var(LEnv64, sys_made_c46, Made_c46_Get),
			  get_var(LEnv64, sys_no, No_Get581),
			  get_var(LEnv64, sys_specified_c44, Specified_c44_Get),
			  get_var(LEnv64, the, The_Get587),
			  TrueResult592=[Default_constructor_Get588],
			  set_var(LEnv64, sys_constructors, TrueResult592),
			  ElseResult595=TrueResult592
		      ;   ElseResult593=[],
			  ElseResult595=ElseResult593
		      ),
		      _33522=ElseResult595
		  ),
		  get_var(LEnv64, ;, C59_Get596),
		  get_var(LEnv64, and, And_Get602),
		  get_var(LEnv64, set, Set_Get603),
		  get_var(LEnv64, sys_check, Check_Get598),
		  get_var(LEnv64, sys_named, Named_Get600),
		  get_var(LEnv64, sys_option, Option_Get601),
		  get_var(LEnv64, sys_predicate_c46, Predicate_c46_Get605),
		  get_var(LEnv64, the, The_Get599),
		  get_var(LEnv64, type, IFTEST608),
		  (   IFTEST608\==[]
		  ->  get_var(LEnv64, sys_named, Named_Get611),
		      f_not(Named_Get611, TrueResult612),
		      IFTEST606=TrueResult612
		  ;   IFTEST606=[]
		  ),
		  (   IFTEST606\==[]
		  ->  get_var(LEnv64, sys_predicate_specified, IFTEST613),
		      (   IFTEST613\==[]
		      ->  get_var(LEnv64, sys_predicate, Predicate_Get),
			  f_error(
				  [ '$ARRAY'([*],
					     claz_base_character,
					     "~S is an illegal structure predicate."),
				    Predicate_Get
				  ],
				  TrueResult617),
			  _35020=TrueResult617
		      ;   _35020=[]
		      ),
		      set_var(LEnv64, sys_predicate, []),
		      _34848=[]
		  ;   _34848=[]
		  ),
		  get_var(LEnv64, sys_include, IFTEST618),
		  (   IFTEST618\==[]
		  ->  get_var(LEnv64, sys_include, Include_Get621),
		      f_car(Include_Get621, TrueResult622),
		      set_var(LEnv64, sys_include, TrueResult622),
		      _35122=TrueResult622
		  ;   _35122=[]
		  ),
		  get_var(LEnv64, ;, C59_Get623),
		  get_var(LEnv64, sys_check, Check_Get625),
		  get_var(LEnv64, sys_print_function, IFTEST630),
		  get_var(LEnv64,
			  sys_print_function_c46,
			  Print_function_c46_Get),
		  get_var(LEnv64, the, The_Get626),
		  (   IFTEST630\==[]
		  ->  get_var(LEnv64, type, Type_Get633),
		      IFTEST628=Type_Get633
		  ;   IFTEST628=[]
		  ),
		  (   IFTEST628\==[]
		  ->  f_error(
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "An print function is supplied to a typed structure.")
			      ],
			      TrueResult635),
		      _35378=TrueResult635
		  ;   _35378=[]
		  ),
		  sf_if(LEnv64,
			
			[ or,
			  type,
			  [not, [member, [quote, kw_clos], xx_features_xx]]
			],
			
			[ '#BQ',
			  
			  [ eval_when,
			    [compile, load, eval],
			    
			    [ sys_define_structure,
			      [quote, ['#COMMA', sys_name]],
			      [quote, ['#COMMA', sys_conc_name]],
			      [quote, ['#COMMA', type]],
			      [quote, ['#COMMA', sys_named]],
			      [quote, ['#COMMA', sys_slots]],
			      [quote, ['#COMMA', sys_slot_descriptions]],
			      [quote, ['#COMMA', sys_copier]],
			      [quote, ['#COMMA', sys_include]],
			      [quote, ['#COMMA', sys_print_function]],
			      [quote, ['#COMMA', sys_constructors]],
			      [quote, ['#COMMA', sys_offset]],
			      [quote, ['#COMMA', documentation]]
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
				      [quote, ['#COMMA', sys_predicate]],
				      
				      [ sys_make_predicate,
					[quote, ['#COMMA', sys_name]],
					[quote, ['#COMMA', type]],
					[quote, ['#COMMA', sys_named]],
					[quote, ['#COMMA', sys_name_offset]]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    [quote, ['#COMMA', sys_name]]
			  ]
			],
			(;),
			(;),
			sys_else,
			[and, [not, type], [member, kw_clos, xx_features_xx]],
			
			[ '#BQ',
			  
			  [ eval_when,
			    [compile, load, eval],
			    
			    [ defclass,
			      ['#COMMA', sys_name],
			      
			      [ 
				[ '#COMMA',
				  [or, sys_include, [quote, structure_object]]
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
				  (;),
				  sys_for,
				  sys_initial,
				  sys_offset,
				  sys_slots,
				  sys_local_slot_descriptions
				]
			      ],
			      [kw_metaclass, structure_class]
			    ],
			    #,
			    sys_c9_c32_c32_c32_c40_with_slots_c32_c40_defstruct_form_c32_slot_descriptions_c32_initial_offset_c10_c9_c9_c9_c32_constructors_c32_documentation_c32_copier_c32_predicate_c10_c9_c9_c9_c32_print_function_c41_c10_c9_c9_c32_c32_c32_c32_c32_c32_c32_c40_find_class_c32_c39_c44_name_c41_c10_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c40_setq_c32_defstruct_form_c32_c39_c40_defstruct_c32_c44_name_c32_c44_c64_slots_c41_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_slot_descriptions_c32_c39_c44_slot_descriptions_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_initial_offset_c32_c39_c44_structure_offset_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_constructors_c32_c39_c44_constructors_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_documentation_c32_c44_documentation_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_copier_c32_c44_copier_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_predicate_c32_c44_predicate_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_print_function_c32_c44_print_function_c41_c41_c10,
			    #,
			    
			    [ '#BQ-COMMA-ELIPSE',
			      
			      [ if,
				sys_print_function,
				
				[ '#BQ',
				  
				  [ 
				    [ defmethod,
				      print_object,
				      [[sys_obj, ['#COMMA', sys_name]], stream],
				      
				      [ ['#COMMA', sys_print_function],
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
			      [quote, ['#COMMA', sys_name]],
			      [quote, ['#COMMA', sys_conc_name]],
			      [quote, ['#COMMA', type]],
			      [quote, ['#COMMA', sys_named]],
			      [quote, ['#COMMA', sys_slots]],
			      [quote, ['#COMMA', sys_slot_descriptions]],
			      [quote, ['#COMMA', sys_copier]],
			      [quote, ['#COMMA', sys_include]],
			      [quote, ['#COMMA', sys_print_function]],
			      [quote, ['#COMMA', sys_constructors]],
			      [quote, ['#COMMA', sys_offset]],
			      [quote, ['#COMMA', documentation]]
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
				      [quote, ['#COMMA', sys_predicate]],
				      
				      [ sys_make_predicate,
					[quote, ['#COMMA', sys_name]],
					[quote, ['#COMMA', type]],
					[quote, ['#COMMA', sys_named]],
					[quote, ['#COMMA', sys_name_offset]]
				      ]
				    ]
				  ]
				]
			      ]
			    ],
			    [quote, ['#COMMA', sys_name]]
			  ]
			],
			LetResult12)
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
				     _19470,
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
					   (;),
					   (;),
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
					   (;),
					   (;),
					   the,
					   defstruct,
					   sys_options,
					   sys_are,
					   sys_supplied_c46,
					   [setq, sys_options, [cdr, sys_name]],
					   [setq, sys_name, [car, sys_name]]
					 ],
					 (;),
					 (;),
					 the,
					 sys_default,
					 sys_conc_name_c46,
					 
					 [ setq,
					   sys_conc_name,
					   
					   [ sys_string_concatenate,
					     [string, sys_name],
					     '$ARRAY'([*],
						      claz_base_character,
						      "-")
					   ]
					 ],
					 (;),
					 (;),
					 the,
					 sys_default,
					 sys_constructor_c46,
					 
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
					 (;),
					 (;),
					 the,
					 sys_default,
					 sys_copier,
					 and,
					 sys_predicate_c46,
					 
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
					 (;),
					 (;),
					 sys_parse,
					 the,
					 defstruct,
					 sys_options_c46,
					 
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
					 (;),
					 (;),
					 sys_skip,
					 the,
					 documentation,
					 sys_string_c46,
					 
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
					 (;),
					 (;),
					 sys_check,
					 the,
					 sys_include,
					 sys_option_c46,
					 
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
					 (;),
					 (;),
					 set,
					 sys_offset_c46,
					 
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
					 (;),
					 (;),
					 sys_increment,
					 sys_offset_c46,
					 
					 [ when,
					   [and, type, sys_initial_offset],
					   
					   [ setq,
					     sys_offset,
					     [+, sys_offset, sys_initial_offset]
					   ]
					 ],
					 
					 [ when,
					   [and, type, sys_named],
					   [setq, sys_name_offset, sys_offset],
					   [setq, sys_offset, ['1+', sys_offset]]
					 ],
					 (;),
					 (;),
					 sys_parse,
					 sys_slot_descriptions_c44,
					 sys_incrementing,
					 sys_offset,
					 sys_for,
					 sys_each,
					 sys_one_c46,
					 
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
					   [setq, sys_offset, ['1+', sys_offset]]
					 ],
					 (;),
					 (;),
					 if,
					 type,
					 sys_is,
					 sys_non_nil,
					 and,
					 structure,
					 sys_is,
					 sys_named_c44,
					 (;),
					 (;),
					 sys_add,
					 the,
					 sys_slot,
					 sys_for,
					 the,
					 sys_structure_name,
					 sys_to,
					 the,
					 sys_slot_descriptions_c46,
					 
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
					 (;),
					 (;),
					 sys_pad,
					 the,
					 sys_slot_descriptions,
					 sys_with,
					 the,
					 sys_initial_offset,
					 number,
					 sys_of,
					 sys_nils_c46,
					 
					 [ when,
					   [and, type, sys_initial_offset],
					   
					   [ setq,
					     sys_slot_descriptions,
					     
					     [ append,
					       [make_list, sys_initial_offset],
					       sys_slot_descriptions
					     ]
					   ]
					 ],
					 (;),
					 (;),
					 (;),
					 (;),
					 append,
					 the,
					 sys_slot_descriptions,
					 sys_of,
					 the,
					 sys_included,
					 sys_structure_c46,
					 (;),
					 (;),
					 the,
					 sys_slot_descriptions,
					 sys_in,
					 the,
					 sys_include,
					 sys_option,
					 sys_are,
					 sys_also,
					 sys_counted_c46,
					 
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
					     (;),
					     (;),
					     if,
					     sys_a,
					     sys_constructor,
					     sys_option,
					     sys_is,
					     sys_nil_c44,
					     (;),
					     (;),
					     sys_no,
					     sys_constructor,
					     sys_should,
					     sys_have,
					     sys_been,
					     sys_specified_c46,
					     
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
					     (;),
					     (;),
					     if,
					     sys_no,
					     sys_constructor,
					     sys_is,
					     sys_specified_c44,
					     (;),
					     (;),
					     the,
					     sys_default_constructor,
					     sys_is,
					     sys_made_c46,
					     
					     [ setq,
					       sys_constructors,
					       [list, sys_default_constructor]
					     ]
					   ]
					 ],
					 (;),
					 (;),
					 sys_check,
					 the,
					 sys_named,
					 sys_option,
					 and,
					 set,
					 the,
					 sys_predicate_c46,
					 
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
					 (;),
					 (;),
					 sys_check,
					 the,
					 sys_print_function_c46,
					 
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
						 [quote, ['#COMMA', sys_name]],
						 
						 [ quote,
						   ['#COMMA', sys_conc_name]
						 ],
						 [quote, ['#COMMA', type]],
						 [quote, ['#COMMA', sys_named]],
						 [quote, ['#COMMA', sys_slots]],
						 
						 [ quote,
						   
						   [ '#COMMA',
						     sys_slot_descriptions
						   ]
						 ],
						 [quote, ['#COMMA', sys_copier]],
						 
						 [ quote,
						   ['#COMMA', sys_include]
						 ],
						 
						 [ quote,
						   
						   [ '#COMMA',
						     sys_print_function
						   ]
						 ],
						 
						 [ quote,
						   ['#COMMA', sys_constructors]
						 ],
						 [quote, ['#COMMA', sys_offset]],
						 
						 [ quote,
						   ['#COMMA', documentation]
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
					   (;),
					   (;),
					   sys_else,
					   
					   [ and,
					     [not, type],
					     [member, kw_clos, xx_features_xx]
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
						     (;),
						     sys_for,
						     sys_initial,
						     sys_offset,
						     sys_slots,
						     sys_local_slot_descriptions
						   ]
						 ],
						 
						 [ kw_metaclass,
						   structure_class
						 ]
					       ],
					       #,
					       sys_c9_c32_c32_c32_c40_with_slots_c32_c40_defstruct_form_c32_slot_descriptions_c32_initial_offset_c10_c9_c9_c9_c32_constructors_c32_documentation_c32_copier_c32_predicate_c10_c9_c9_c9_c32_print_function_c41_c10_c9_c9_c32_c32_c32_c32_c32_c32_c32_c40_find_class_c32_c39_c44_name_c41_c10_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c32_c40_setq_c32_defstruct_form_c32_c39_c40_defstruct_c32_c44_name_c32_c44_c64_slots_c41_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_slot_descriptions_c32_c39_c44_slot_descriptions_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_initial_offset_c32_c39_c44_structure_offset_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_constructors_c32_c39_c44_constructors_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_documentation_c32_c44_documentation_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_copier_c32_c44_copier_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_predicate_c32_c44_predicate_c41_c10_c9_c32_c32_c32_c32_c32_c32_c40_setq_c32_print_function_c32_c44_print_function_c41_c41_c10,
					       #,
					       
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
						 [quote, ['#COMMA', sys_name]],
						 
						 [ quote,
						   ['#COMMA', sys_conc_name]
						 ],
						 [quote, ['#COMMA', type]],
						 [quote, ['#COMMA', sys_named]],
						 [quote, ['#COMMA', sys_slots]],
						 
						 [ quote,
						   
						   [ '#COMMA',
						     sys_slot_descriptions
						   ]
						 ],
						 [quote, ['#COMMA', sys_copier]],
						 
						 [ quote,
						   ['#COMMA', sys_include]
						 ],
						 
						 [ quote,
						   
						   [ '#COMMA',
						     sys_print_function
						   ]
						 ],
						 
						 [ quote,
						   ['#COMMA', sys_constructors]
						 ],
						 [quote, ['#COMMA', sys_offset]],
						 
						 [ quote,
						   ['#COMMA', documentation]
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
;; The #S reader.
*/
/*
(defun sharp-s-reader (stream subchar arg)
  (declare (ignore subchar))
  (when (and arg (null *read-suppress*))
        (error "An extra argument was supplied for the #S readmacro."))
  (let ((l (read stream)))
    (unless (get-sysprop (car l) 'IS-A-STRUCTURE)
            (error ""(defun sharp-s-reader (stream subchar arg)\n  (declare (ignore subchar))\n  (when (and arg (null *read-suppress*))\n        (error \"An extra argument was supplied for the #S readmacro.\"))\n  (let ((l (read stream)))\n    (unless (get-sysprop (car l) 'IS-A-STRUCTURE)\n            (error \"~S is not a structure.\" (car l)))\n    ;; Intern keywords in the keyword package.\n    (do ((ll (cdr l) (cddr ll)))\n        ((endp ll)\n         ;; Find an appropriate construtor.\n         (do ((cs (get-sysprop (car l) 'STRUCTURE-CONSTRUCTORS) (cdr cs)))\n             ((endp cs)\n              (error \"The structure ~S has no structure constructor.\"\n                     (car l)))\n           (when (symbolp (car cs))\n                 (return (apply (car cs) (cdr l))))))\n      (rplaca ll (intern (string (car ll)) 'KEYWORD)))))\n\n\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp:8844 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'sharp-s-reader',[stream,subchar,arg],[declare,[ignore,subchar]],[when,[and,arg,[null,'*read-suppress*']],[error,'$STRING'("An extra argument was supplied for the #S readmacro.")]],[let,[[l,[read,stream]]],[unless,['get-sysprop',[car,l],[quote,'IS-A-STRUCTURE']],[error,'$STRING'("~S is not a structure."),[car,l]]],;,;,'Intern',keywords,in,the,keyword,'package.',[do,[[ll,[cdr,l],[cddr,ll]]],[[endp,ll],;,;,'Find',an,appropriate,'construtor.',[do,[[cs,['get-sysprop',[car,l],[quote,'STRUCTURE-CONSTRUCTORS']],[cdr,cs]]],[[endp,cs],[error,'$STRING'("The structure ~S has no structure constructor."),[car,l]]],[when,[symbolp,[car,cs]],[return,[apply,[car,cs],[cdr,l]]]]]],[rplaca,ll,[intern,[string,[car,ll]],[quote,'KEYWORD']]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_sharp_s_reader,
					       kw_function,
					       f_sys_sharp_s_reader)).
*/
wl:lambda_def(defun, sys_sharp_s_reader, f_sys_sharp_s_reader, [stream, sys_subchar, sys_arg], [[declare, [ignore, sys_subchar]], [when, [and, sys_arg, [null, xx_read_suppress_xx]], [error, '$ARRAY'([*], claz_base_character, "An extra argument was supplied for the #S readmacro.")]], [let, [[sys_l, [read, stream]]], [unless, [sys_get_sysprop, [car, sys_l], [quote, sys_is_a_structure]], [error, '$ARRAY'([*], claz_base_character, "~S is not a structure."), [car, sys_l]]], ;, ;, intern, sys_keywords, sys_in, the, keyword, sys_package_c46, [do, [[sys_ll, [cdr, sys_l], [cddr, sys_ll]]], [[endp, sys_ll], ;, ;, find, sys_an, sys_appropriate, sys_construtor_c46, [do, [[sys_cs, [sys_get_sysprop, [car, sys_l], [quote, sys_structure_constructors]], [cdr, sys_cs]]], [[endp, sys_cs], [error, '$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), [car, sys_l]]], [when, [symbolp, [car, sys_cs]], [return, [apply, [car, sys_cs], [cdr, sys_l]]]]]], [rplaca, sys_ll, [intern, [string, [car, sys_ll]], [quote, keyword]]]]]]).
wl:arglist_info(sys_sharp_s_reader, f_sys_sharp_s_reader, [stream, sys_subchar, sys_arg], arginfo{all:[stream, sys_subchar, sys_arg], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[stream, sys_subchar, sys_arg], opt:0, req:[stream, sys_subchar, sys_arg], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_sharp_s_reader).

/*

### Compiled Function: `SYS::SHARP-S-READER` 
*/
f_sys_sharp_s_reader(Stream_In, Subchar_In, Arg_In, FnResult) :-
	GEnv=[bv(stream, Stream_In), bv(sys_subchar, Subchar_In), bv(sys_arg, Arg_In)],
	catch(( ( sf_declare(GEnv, [ignore, sys_subchar], Sf_declare_Ret),
		  get_var(GEnv, sys_arg, IFTEST9),
		  (   IFTEST9\==[]
		  ->  get_var(GEnv,
			      xx_read_suppress_xx,
			      Xx_read_suppress_xx_Get),
		      f_null(Xx_read_suppress_xx_Get, TrueResult),
		      IFTEST=TrueResult
		  ;   IFTEST=[]
		  ),
		  (   IFTEST\==[]
		  ->  f_error(
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "An extra argument was supplied for the #S readmacro.")
			      ],
			      TrueResult14),
		      _7834=TrueResult14
		  ;   _7834=[]
		  ),
		  get_var(GEnv, stream, Stream_Get),
		  f_read(Stream_Get, L_Init),
		  LEnv=[bv(sys_l, L_Init)|GEnv],
		  get_var(LEnv, sys_l, L_Get),
		  f_car(L_Get, Get_sysprop_Param),
		  f_sys_get_sysprop(Get_sysprop_Param,
				    sys_is_a_structure,
				    [],
				    IFTEST20),
		  (   IFTEST20\==[]
		  ->  _8066=[]
		  ;   get_var(LEnv, sys_l, L_Get23),
		      f_car(L_Get23, Car_Ret),
		      f_error(
			      [ '$ARRAY'([*],
					 claz_base_character,
					 "~S is not a structure."),
				Car_Ret
			      ],
			      ElseResult),
		      _8066=ElseResult
		  ),
		  get_var(LEnv, ;, C59_Get26),
		  get_var(LEnv, intern, Intern_Get),
		  ( get_var(LEnv, keyword, Keyword_Get),
		    get_var(LEnv, sys_in, In_Get)
		  ),
		  get_var(LEnv, sys_keywords, Keywords_Get),
		  get_var(LEnv, sys_l, L_Get36),
		  ( get_var(LEnv, sys_package_c46, Package_c46_Get),
		    get_var(LEnv, the, The_Get)
		  ),
		  f_cdr(L_Get36, Ll_Init),
		  AEnv=[bv(sys_ll, Ll_Init)|LEnv],
		  catch(( call_addr_block(AEnv,
					  (push_label(do_label_3), get_var(AEnv, sys_ll, Ll_Get115), (s3q:is_endp(Ll_Get115)->get_var(AEnv, ;, C59_Get120), get_var(AEnv, find, Find_Get122), get_var(AEnv, sys_an, An_Get123), get_var(AEnv, sys_appropriate, Appropriate_Get124), get_var(AEnv, sys_construtor_c46, Construtor_c46_Get125), get_var(AEnv, sys_l, L_Get129), f_car(L_Get129, Get_sysprop_Param196), f_sys_get_sysprop(Get_sysprop_Param196, sys_structure_constructors, [], Cs_Init130), AEnv=[bv(sys_cs, Cs_Init130)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_5), get_var(AEnv, sys_cs, Cs_Get158), (s3q:is_endp(Cs_Get158)->get_var(AEnv, sys_l, L_Get163), f_car(L_Get163, Car_Ret208), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret208], RetResult161), throw(block_exit([], RetResult161)), _TBResult131=ThrowResult162;get_var(AEnv, sys_cs, Cs_Get166), f_car(Cs_Get166, PredArgResult168), (is_symbolp(PredArgResult168)->get_var(AEnv, sys_cs, Cs_Get171), f_car(Cs_Get171, Apply_Param), get_var(AEnv, sys_l, L_Get172), f_cdr(L_Get172, Cdr_Ret), f_apply(Apply_Param, Cdr_Ret, RetResult169), throw(block_exit([], RetResult169)), _11346=ThrowResult170;_11346=[]), get_var(AEnv, sys_cs, Cs_Get175), f_cdr(Cs_Get175, Cs), set_var(AEnv, sys_cs, Cs), goto(do_label_5, AEnv), _TBResult131=_GORES176)), [addr(addr_tagbody_5_do_label_5, do_label_5, '$unused', AEnv,  (get_var(AEnv, sys_cs, Cs_Get133), (s3q:is_endp(Cs_Get133)->get_var(AEnv, sys_l, L_Get138), f_car(L_Get138, Car_Ret210), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret210], RetResult136), throw(block_exit([], RetResult136)), _TBResult131=ThrowResult137;get_var(AEnv, sys_cs, Cs_Get141), f_car(Cs_Get141, PredArgResult143), (is_symbolp(PredArgResult143)->get_var(AEnv, sys_cs, Cs_Get146), f_car(Cs_Get146, Apply_Param198), get_var(AEnv, sys_l, L_Get147), f_cdr(L_Get147, Cdr_Ret211), f_apply(Apply_Param198, Cdr_Ret211, RetResult144), throw(block_exit([], RetResult144)), _11846=ThrowResult145;_11846=[]), get_var(AEnv, sys_cs, Cs_Get151), f_cdr(Cs_Get151, Cdr_Ret212), set_var(AEnv, sys_cs, Cdr_Ret212), goto(do_label_5, AEnv), _TBResult131=_GORES152)))]), []=RetResult118), block_exit([], RetResult118), true), throw(block_exit([], RetResult118)), _TBResult=ThrowResult119;get_var(AEnv, sys_ll, Ll_Get181), f_car(Ll_Get181, String_Param), f_string(String_Param, Intern_Param), f_intern(Intern_Param, keyword, Keyword), f_rplaca(Ll_Get181, Keyword, Rplaca_Ret), get_var(AEnv, sys_ll, Ll_Get184), f_cddr(Ll_Get184, Ll), set_var(AEnv, sys_ll, Ll), goto(do_label_3, AEnv), _TBResult=_GORES185)),
					  
					  [ addr(addr_tagbody_3_do_label_3,
						 do_label_3,
						 '$unused',
						 AEnv,
						 (get_var(AEnv, sys_ll, Ll_Get), (s3q:is_endp(Ll_Get)->get_var(AEnv, ;, C59_Get45), get_var(AEnv, find, Get_var_Ret), get_var(AEnv, sys_an, Get_var_Ret215), get_var(AEnv, sys_appropriate, Get_var_Ret216), get_var(AEnv, sys_construtor_c46, Get_var_Ret217), get_var(AEnv, sys_l, L_Get54), f_car(L_Get54, Get_sysprop_Param201), f_sys_get_sysprop(Get_sysprop_Param201, sys_structure_constructors, [], Get_sysprop_Ret), AEnv=[bv(sys_cs, Get_sysprop_Ret)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_4), get_var(AEnv, sys_cs, Cs_Get82), (s3q:is_endp(Cs_Get82)->get_var(AEnv, sys_l, L_Get87), f_car(L_Get87, Car_Ret219), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret219], RetResult85), throw(block_exit([], RetResult85)), _TBResult56=ThrowResult86;get_var(AEnv, sys_cs, Cs_Get90), f_car(Cs_Get90, PredArgResult92), (is_symbolp(PredArgResult92)->get_var(AEnv, sys_cs, Cs_Get95), f_car(Cs_Get95, Apply_Param202), get_var(AEnv, sys_l, L_Get96), f_cdr(L_Get96, Cdr_Ret220), f_apply(Apply_Param202, Cdr_Ret220, RetResult93), throw(block_exit([], RetResult93)), _12312=ThrowResult94;_12312=[]), get_var(AEnv, sys_cs, Cs_Get99), f_cdr(Cs_Get99, Cdr_Ret221), set_var(AEnv, sys_cs, Cdr_Ret221), goto(do_label_4, AEnv), _TBResult56=_GORES100)), [addr(addr_tagbody_4_do_label_4, do_label_4, '$unused', AEnv,  (get_var(AEnv, sys_cs, Cs_Get), (s3q:is_endp(Cs_Get)->get_var(AEnv, sys_l, L_Get63), f_car(L_Get63, Car_Ret222), f_error(['$ARRAY'([*], claz_base_character, "The structure ~S has no structure constructor."), Car_Ret222], RetResult61), throw(block_exit([], RetResult61)), _TBResult56=ThrowResult62;get_var(AEnv, sys_cs, Cs_Get66), f_car(Cs_Get66, PredArgResult68), (is_symbolp(PredArgResult68)->get_var(AEnv, sys_cs, Cs_Get71), f_car(Cs_Get71, Apply_Param203), get_var(AEnv, sys_l, L_Get72), f_cdr(L_Get72, Cdr_Ret223), f_apply(Apply_Param203, Cdr_Ret223, RetResult69), throw(block_exit([], RetResult69)), _12518=ThrowResult70;_12518=[]), get_var(AEnv, sys_cs, Cs_Get75), f_cdr(Cs_Get75, Cdr_Ret224), set_var(AEnv, sys_cs, Cdr_Ret224), goto(do_label_4, AEnv), _TBResult56=_GORES)))]), []=LetResult52), block_exit([], LetResult52), true), throw(block_exit([], LetResult52)), _12578=ThrowResult;get_var(AEnv, sys_ll, Ll_Get105), f_car(Ll_Get105, String_Param204), f_string(String_Param204, Intern_Param205), f_intern(Intern_Param205, keyword, Intern_Ret), f_rplaca(Ll_Get105, Intern_Ret, Rplaca_Ret226), get_var(AEnv, sys_ll, Ll_Get108), f_cddr(Ll_Get108, Cddr_Ret), set_var(AEnv, sys_ll, Cddr_Ret), goto(do_label_3, AEnv), _12578=_GORES109)))
					  ]),
			  []=LetResult34
			),
			block_exit([], LetResult34),
			true)
		),
		LetResult34=FnResult
	      ),
	      block_exit(sys_sharp_s_reader, FnResult),
	      true).
:- set_opv(sys_sharp_s_reader, symbol_function, f_sys_sharp_s_reader),
   DefunResult=sys_sharp_s_reader.
/*
:- side_effect(assert_lsp(sys_sharp_s_reader,
			  lambda_def(defun,
				     sys_sharp_s_reader,
				     f_sys_sharp_s_reader,
				     [stream, sys_subchar, sys_arg],
				     
				     [ [declare, [ignore, sys_subchar]],
				       
				       [ when,
					 
					 [ and,
					   sys_arg,
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
					 (;),
					 (;),
					 intern,
					 sys_keywords,
					 sys_in,
					 the,
					 keyword,
					 sys_package_c46,
					 
					 [ do,
					   
					   [ 
					     [ sys_ll,
					       [cdr, sys_l],
					       [cddr, sys_ll]
					     ]
					   ],
					   
					   [ [endp, sys_ll],
					     (;),
					     (;),
					     find,
					     sys_an,
					     sys_appropriate,
					     sys_construtor_c46,
					     
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
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_sharp_s_reader,
			  arglist_info(sys_sharp_s_reader,
				       f_sys_sharp_s_reader,
				       [stream, sys_subchar, sys_arg],
				       arginfo{ all:
						    [ stream,
						      sys_subchar,
						      sys_arg
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ stream,
							sys_subchar,
							sys_arg
						      ],
						opt:0,
						req:
						    [ stream,
						      sys_subchar,
						      sys_arg
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_sharp_s_reader,
			  init_args(x, f_sys_sharp_s_reader))).
*/
/*
(defun make-constructor (name constructor type named slot-descriptions)
  (declare (ignore named))
  (let ((slot-names
         ;; Collect the slot-names.
         (mapcar #'(lambda (x)
                     (cond ((null x)
                            ;; If the slot-description is NIL,
                            ;;  it is in the padding of initial-offset.
                            nil)
                           ((null (car x))
                            ;; If the slot name is NIL,
                            ;;  it is the structure name.
                            ;;  This is for typed structures with names.
                            (list 'QUOTE (cadr x)))
                           (t (car x))))
                 slot-descriptions))
        (keys
         ;; Make the keyword parameters.
         (mapcan #'(lambda (x)
                     (cond ((null x) nil)
                           ((null (car x)) nil)
                           ((null (cadr x)) (list (car x)))
                           (t (list (list  (car x) (cadr x))))))
                 slot-descriptions)))
    (cond ((consp constructor)
           ;; The case for a BOA constructor.
           ;; Dirty code!!
           ;; We must add an initial value for an optional parameter,
           ;;  if the default value is not specified
           ;;  in the given parameter list and yet the initial value
           ;;  is supplied in the slot description.
           (do ((a (cadr constructor) (cdr a)) (l nil) (vs nil))
               ((endp a)
                ;; Add those options that do not appear in the parameter list
                ;;  as auxiliary paramters.
                ;; The parameters are accumulated in the variable VS.
                (setq keys
                      (nreconc (cons '&aux l)
                               (mapcan #'(lambda (k)
                                           (if (member (if (atom k) k (car k))
                                                       vs)
                                               nil
                                               (list k)))
                                       keys))))
             ;; Skip until &OPTIONAL appears.
             (cond ((eq (car a) '&optional)
                    (setq l (cons '&optional l))
                    (do ((aa (cdr a) (cdr aa)) (ov) (y))
                        ((endp aa)
                         ;; Add those options that do not appear in the
                         ;;  parameter list.
                         (setq keys
                               (nreconc (cons '&aux l)
                                        (mapcan #'(lambda (k)
                                                    (if (member (if (atom k)
                                                                    k
                                                                    (car k))
                                                                vs)
                                                        nil
                                                        (list k)))
                                                keys)))
                         (return nil))
                      (when (member (car aa) lambda-list-keywords)
                            (when (eq (car aa) '&rest)
                                  ;; &REST is found.
                                  (setq l (cons '&rest l))
                                  (setq aa (cdr aa))
                                  (unless (and (not (endp aa))
                                               (symbolp (car aa)))
                                          (illegal-boa))
                                  (setq vs (cons (car aa) vs))
                                  (setq l (cons (car aa) l))
                                  (setq aa (cdr aa))
                                  (when (endp aa)
                                        (setq keys
                                              (nreconc
                                               (cons '&aux l)
                                               (mapcan
                                                #'(lambda (k)
                                                    (if (member (if (atom k)
                                                                    k
                                                                    (car k))
                                                                vs)
                                                        nil
                                                        (list k)))
                                                keys)))
                                        (return nil)))
                            ;; &AUX should follow.
                            (unless (eq (car aa) '&aux)
                                    (illegal-boa))
                            (setq l (cons '&aux l))
                            (do ((aaa (cdr aa) (cdr aaa)))
                                ((endp aaa))
                              (setq l (cons (car aaa) l))
                              (cond ((and (atom (car aaa))
                                          (symbolp (car aaa)))
                                     (setq vs (cons (car aaa) vs)))
                                    ((and (symbolp (caar aaa))
                                          (or (endp (cdar aaa))
                                              (endp (cddar aaa))))
                                     (setq vs (cons (caar aaa) vs)))
                                    (t (illegal-boa))))
                            ;; End of the parameter list.
                            (setq keys
                                  (nreconc l
                                           (mapcan
                                            #'(lambda (k)
                                                (if (member (if (atom k)
                                                                k
                                                                (car k))
                                                            vs)
                                                    nil
                                                    (list k)))
                                            keys)))
                            (return nil))
                      ;; Checks if the optional paramter without a default
                      ;;  value has a default value in the slot-description.
                      (if (and (cond ((atom (car aa)) (setq ov (car aa)) t)
                                     ((endp (cdar aa)) (setq ov (caar aa)) t)
                                     (t nil))
                               (setq y (member ov
                                               keys
                                               :key
                                               #'(lambda (x)
                                                   (if (consp x)
                                                       ;; With default value.
                                                       (car x))))))
                          ;; If no default value is supplied for
                          ;;  the optional parameter and yet appears
                          ;;  in KEYS with a default value,
                          ;;  then cons the pair to L,
                          (setq l (cons (car y) l))
                          ;;  otherwise cons just the parameter to L.
                          (setq l (cons (car aa) l)))
                      ;; Checks the form of the optional parameter.
                      (cond ((atom (car aa))
                             (unless (symbolp (car aa))
                                     (illegal-boa))
                             (setq vs (cons (car aa) vs)))
                            ((not (symbolp (caar aa)))
                             (illegal-boa))
                            ((or (endp (cdar aa)) (endp (cddar aa)))
                             (setq vs (cons (caar aa) vs)))
                            ((not (symbolp (caddar aa)))
                             (illegal-boa))
                            ((not (endp (cdddar aa)))
                             (illegal-boa))
                            (t
                             (setq vs (cons (caar aa) vs))
                             (setq vs (cons (caddar aa) vs)))))
                    ;; RETURN from the outside DO.
                    (return nil))
                   (t
                    (unless (symbolp (car a))
                            (illegal-boa))
                    (setq l (cons (car a) l))
                    (setq vs (cons (car a) vs)))))
           (setq constructor (car constructor)))
          (t
           ;; If not a BOA constructor, just cons &KEY.
           (setq keys (cons '&key keys))))
    (cond ((null type)
           `(defun ,constructor ,keys
	      #-CLOS
              (sys:make-structure ',name ,@slot-names)
	      #+CLOS
	      (sys:make-structure (find-class ',name) ,@slot-names)))
          ((or (eq type 'VECTOR)
               (and (consp type) (eq (car type) 'vector)))
           `(defun ,constructor ,keys
              (vector ,@slot-names)))
          ((eq type 'LIST)
           `(defun ,constructor ,keys
              (list ,@slot-names)))
          ((error ""(defun make-constructor (name constructor type named slot-descriptions)\n  (declare (ignore named))\n  (let ((slot-names\n         ;; Collect the slot-names.\n         (mapcar #'(lambda (x)\n                     (cond ((null x)\n                            ;; If the slot-description is NIL,\n                            ;;  it is in the padding of initial-offset.\n                            nil)\n                           ((null (car x))\n                            ;; If the slot name is NIL,\n                            ;;  it is the structure name.\n                            ;;  This is for typed structures with names.\n                            (list 'QUOTE (cadr x)))\n                           (t (car x))))\n                 slot-descriptions))\n        (keys\n         ;; Make the keyword parameters.\n         (mapcan #'(lambda (x)\n                     (cond ((null x) nil)\n                           ((null (car x)) nil)\n                           ((null (cadr x)) (list (car x)))\n                           (t (list (list  (car x) (cadr x))))))\n                 slot-descriptions)))\n    (cond ((consp constructor)\n           ;; The case for a BOA constructor.\n           ;; Dirty code!!\n           ;; We must add an initial value for an optional parameter,\n           ;;  if the default value is not specified\n           ;;  in the given parameter list and yet the initial value\n           ;;  is supplied in the slot description.\n           (do ((a (cadr constructor) (cdr a)) (l nil) (vs nil))\n               ((endp a)\n                ;; Add those options that do not appear in the parameter list\n                ;;  as auxiliary paramters.\n                ;; The parameters are accumulated in the variable VS.\n                (setq keys\n                      (nreconc (cons '&aux l)\n                               (mapcan #'(lambda (k)\n                                           (if (member (if (atom k) k (car k))\n                                                       vs)\n                                               nil\n                                               (list k)))\n                                       keys))))\n             ;; Skip until &OPTIONAL appears.\n             (cond ((eq (car a) '&optional)\n                    (setq l (cons '&optional l))\n                    (do ((aa (cdr a) (cdr aa)) (ov) (y))\n                        ((endp aa)\n                         ;; Add those options that do not appear in the\n                         ;;  parameter list.\n                         (setq keys\n                               (nreconc (cons '&aux l)\n                                        (mapcan #'(lambda (k)\n                                                    (if (member (if (atom k)\n                                                                    k\n                                                                    (car k))\n                                                                vs)\n                                                        nil\n                                                        (list k)))\n                                                keys)))\n                         (return nil))\n                      (when (member (car aa) lambda-list-keywords)\n                            (when (eq (car aa) '&rest)\n                                  ;; &REST is found.\n                                  (setq l (cons '&rest l))\n                                  (setq aa (cdr aa))\n                                  (unless (and (not (endp aa))\n                                               (symbolp (car aa)))\n                                          (illegal-boa))\n                                  (setq vs (cons (car aa) vs))\n                                  (setq l (cons (car aa) l))\n                                  (setq aa (cdr aa))\n                                  (when (endp aa)\n                                        (setq keys\n                                              (nreconc\n                                               (cons '&aux l)\n                                               (mapcan\n                                                #'(lambda (k)\n                                                    (if (member (if (atom k)\n                                                                    k\n                                                                    (car k))\n                                                                vs)\n                                                        nil\n                                                        (list k)))\n                                                keys)))\n                                        (return nil)))\n                            ;; &AUX should follow.\n                            (unless (eq (car aa) '&aux)\n                                    (illegal-boa))\n                            (setq l (cons '&aux l))\n                            (do ((aaa (cdr aa) (cdr aaa)))\n                                ((endp aaa))\n                              (setq l (cons (car aaa) l))\n                              (cond ((and (atom (car aaa))\n                                          (symbolp (car aaa)))\n                                     (setq vs (cons (car aaa) vs)))\n                                    ((and (symbolp (caar aaa))\n                                          (or (endp (cdar aaa))\n                                              (endp (cddar aaa))))\n                                     (setq vs (cons (caar aaa) vs)))\n                                    (t (illegal-boa))))\n                            ;; End of the parameter list.\n                            (setq keys\n                                  (nreconc l\n                                           (mapcan\n                                            #'(lambda (k)\n                                                (if (member (if (atom k)\n                                                                k\n                                                                (car k))\n                                                            vs)\n                                                    nil\n                                                    (list k)))\n                                            keys)))\n                            (return nil))\n                      ;; Checks if the optional paramter without a default\n                      ;;  value has a default value in the slot-description.\n                      (if (and (cond ((atom (car aa)) (setq ov (car aa)) t)\n                                     ((endp (cdar aa)) (setq ov (caar aa)) t)\n                                     (t nil))\n                               (setq y (member ov\n                                               keys\n                                               :key\n                                               #'(lambda (x)\n                                                   (if (consp x)\n                                                       ;; With default value.\n                                                       (car x))))))\n                          ;; If no default value is supplied for\n                          ;;  the optional parameter and yet appears\n                          ;;  in KEYS with a default value,\n                          ;;  then cons the pair to L,\n                          (setq l (cons (car y) l))\n                          ;;  otherwise cons just the parameter to L.\n                          (setq l (cons (car aa) l)))\n                      ;; Checks the form of the optional parameter.\n                      (cond ((atom (car aa))\n                             (unless (symbolp (car aa))\n                                     (illegal-boa))\n                             (setq vs (cons (car aa) vs)))\n                            ((not (symbolp (caar aa)))\n                             (illegal-boa))\n                            ((or (endp (cdar aa)) (endp (cddar aa)))\n                             (setq vs (cons (caar aa) vs)))\n                            ((not (symbolp (caddar aa)))\n                             (illegal-boa))\n                            ((not (endp (cdddar aa)))\n                             (illegal-boa))\n                            (t\n                             (setq vs (cons (caar aa) vs))\n                             (setq vs (cons (caddar aa) vs)))))\n                    ;; RETURN from the outside DO.\n                    (return nil))\n                   (t\n                    (unless (symbolp (car a))\n                            (illegal-boa))\n                    (setq l (cons (car a) l))\n                    (setq vs (cons (car a) vs)))))\n           (setq constructor (car constructor)))\n          (t\n           ;; If not a BOA constructor, just cons &KEY.\n           (setq keys (cons '&key keys))))\n    (cond ((null type)\n           `(defun ,constructor ,keys\n\t      #-CLOS\n              (sys:make-structure ',name ,@slot-names)\n\t      #+CLOS\n\t      (sys:make-structure (find-class ',name) ,@slot-names)))\n          ((or (eq type 'VECTOR)\n               (and (consp type) (eq (car type) 'vector)))\n           `(defun ,constructor ,keys\n              (vector ,@slot-names)))\n          ((eq type 'LIST)\n           `(defun ,constructor ,keys\n              (list ,@slot-names)))\n          ((error \"~S is an illegal structure type\" type)))))\n\n".
*/

/*********** /home/dmiles/logicmoo_workspace/packs_usr/wam_common_lisp/prolog/wam_cl/lib/lsp/defstruct-0.lsp:9654 **********************/
:-lisp_compile_to_prolog(pkg_sys,[defun,'make-constructor',[name,constructor,type,named,'slot-descriptions'],[declare,[ignore,named]],[let,[['slot-names',;,;,'Collect',the,'slot-names.',[mapcar,function([lambda,[x],[cond,[[null,x],;,;,'If',the,'slot-description',is,'NIL,',;,;,it,is,in,the,padding,of,'initial-offset.',[]],[[null,[car,x]],;,;,'If',the,slot,name,is,'NIL,',;,;,it,is,the,structure,'name.',;,;,'This',is,for,typed,structures,with,'names.',[list,[quote,'QUOTE'],[cadr,x]]],[t,[car,x]]]]),'slot-descriptions']],[keys,;,;,'Make',the,keyword,'parameters.',[mapcan,function([lambda,[x],[cond,[[null,x],[]],[[null,[car,x]],[]],[[null,[cadr,x]],[list,[car,x]]],[t,[list,[list,[car,x],[cadr,x]]]]]]),'slot-descriptions']]],[cond,[[consp,constructor],;,;,'The',case,for,a,'BOA','constructor.',;,;,'Dirty','code!!',;,;,'We',must,add,an,initial,value,for,an,optional,'parameter,',;,;,if,the,default,value,is,not,specified,;,;,in,the,given,parameter,list,and,yet,the,initial,value,;,;,is,supplied,in,the,slot,'description.',[do,[[a,[cadr,constructor],[cdr,a]],[l,[]],[vs,[]]],[[endp,a],;,;,'Add',those,options,that,do,not,appear,in,the,parameter,list,;,;,as,auxiliary,'paramters.',;,;,'The',parameters,are,accumulated,in,the,variable,'VS.',[setq,keys,[nreconc,[cons,[quote,'&aux'],l],[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]]],;,;,'Skip',until,'&OPTIONAL','appears.',[cond,[[eq,[car,a],[quote,'&optional']],[setq,l,[cons,[quote,'&optional'],l]],[do,[[aa,[cdr,a],[cdr,aa]],[ov],[y]],[[endp,aa],;,;,'Add',those,options,that,do,not,appear,in,the,;,;,parameter,'list.',[setq,keys,[nreconc,[cons,[quote,'&aux'],l],[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[return,[]]],[when,[member,[car,aa],'lambda-list-keywords'],[when,[eq,[car,aa],[quote,'&rest']],;,;,'&REST',is,'found.',[setq,l,[cons,[quote,'&rest'],l]],[setq,aa,[cdr,aa]],[unless,[and,[not,[endp,aa]],[symbolp,[car,aa]]],['illegal-boa']],[setq,vs,[cons,[car,aa],vs]],[setq,l,[cons,[car,aa],l]],[setq,aa,[cdr,aa]],[when,[endp,aa],[setq,keys,[nreconc,[cons,[quote,'&aux'],l],[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[return,[]]]],;,;,'&AUX',should,'follow.',[unless,[eq,[car,aa],[quote,'&aux']],['illegal-boa']],[setq,l,[cons,[quote,'&aux'],l]],[do,[[aaa,[cdr,aa],[cdr,aaa]]],[[endp,aaa]],[setq,l,[cons,[car,aaa],l]],[cond,[[and,[atom,[car,aaa]],[symbolp,[car,aaa]]],[setq,vs,[cons,[car,aaa],vs]]],[[and,[symbolp,[caar,aaa]],[or,[endp,[cdar,aaa]],[endp,[cddar,aaa]]]],[setq,vs,[cons,[caar,aaa],vs]]],[t,['illegal-boa']]]],;,;,'End',of,the,parameter,'list.',[setq,keys,[nreconc,l,[mapcan,function([lambda,[k],[if,[member,[if,[atom,k],k,[car,k]],vs],[],[list,k]]]),keys]]],[return,[]]],;,;,'Checks',if,the,optional,paramter,without,a,default,;,;,value,has,a,default,value,in,the,'slot-description.',[if,[and,[cond,[[atom,[car,aa]],[setq,ov,[car,aa]],t],[[endp,[cdar,aa]],[setq,ov,[caar,aa]],t],[t,[]]],[setq,y,[member,ov,keys,':key',function([lambda,[x],[if,[consp,x],;,;,'With',default,'value.',[car,x]]])]]],;,;,'If',no,default,value,is,supplied,for,;,;,the,optional,parameter,and,yet,appears,;,;,in,'KEYS',with,a,default,'value,',;,;,then,cons,the,pair,to,'L,',[setq,l,[cons,[car,y],l]],;,;,otherwise,cons,just,the,parameter,to,'L.',[setq,l,[cons,[car,aa],l]]],;,;,'Checks',the,form,of,the,optional,'parameter.',[cond,[[atom,[car,aa]],[unless,[symbolp,[car,aa]],['illegal-boa']],[setq,vs,[cons,[car,aa],vs]]],[[not,[symbolp,[caar,aa]]],['illegal-boa']],[[or,[endp,[cdar,aa]],[endp,[cddar,aa]]],[setq,vs,[cons,[caar,aa],vs]]],[[not,[symbolp,[caddar,aa]]],['illegal-boa']],[[not,[endp,[cdddar,aa]]],['illegal-boa']],[t,[setq,vs,[cons,[caar,aa],vs]],[setq,vs,[cons,[caddar,aa],vs]]]]],;,;,'RETURN',from,the,outside,'DO.',[return,[]]],[t,[unless,[symbolp,[car,a]],['illegal-boa']],[setq,l,[cons,[car,a],l]],[setq,vs,[cons,[car,a],vs]]]]],[setq,constructor,[car,constructor]]],[t,;,;,'If',not,a,'BOA','constructor,',just,cons,'&KEY.',[setq,keys,[cons,[quote,'&key'],keys]]]],[cond,[[null,type],['#BQ',[defun,['#COMMA',constructor],['#COMMA',keys],['sys:make-structure',[quote,['#COMMA',name]],['#BQ-COMMA-ELIPSE','slot-names']]]]],[[or,[eq,type,[quote,'VECTOR']],[and,[consp,type],[eq,[car,type],[quote,vector]]]],['#BQ',[defun,['#COMMA',constructor],['#COMMA',keys],[vector,['#BQ-COMMA-ELIPSE','slot-names']]]]],[[eq,type,[quote,'LIST']],['#BQ',[defun,['#COMMA',constructor],['#COMMA',keys],[list,['#BQ-COMMA-ELIPSE','slot-names']]]]],[[error,'$STRING'("~S is an illegal structure type"),type]]]]])
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_make_constructor,
					       kw_function,
					       f_sys_make_constructor)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
/*
:- side_effect(generate_function_or_macro_name([name='GLOBAL', environ=env_1],
					       sys_illegal_boa,
					       kw_function,
					       f_sys_illegal_boa)).
*/
wl:lambda_def(defun, sys_make_constructor, f_sys_make_constructor, [sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], [[declare, [ignore, sys_named]], [let, [[sys_slot_names, ;, ;, sys_collect, the, sys_slot_names_c46, [mapcar, function([lambda, [sys_x], [cond, [[null, sys_x], ;, ;, if, the, sys_slot_description, sys_is, sys_nil_c44, ;, ;, sys_it, sys_is, sys_in, the, sys_padding, sys_of, sys_initial_offset_c46, []], [[null, [car, sys_x]], ;, ;, if, the, sys_slot, sys_name, sys_is, sys_nil_c44, ;, ;, sys_it, sys_is, the, structure, sys_name_c46, ;, ;, sys_this, sys_is, sys_for, sys_typed, sys_structures, sys_with, sys_names_c46, [list, [quote, quote], [cadr, sys_x]]], [t, [car, sys_x]]]]), sys_slot_descriptions]], [sys_keys, ;, ;, sys_make, the, keyword, sys_parameters_c46, [mapcan, function([lambda, [sys_x], [cond, [[null, sys_x], []], [[null, [car, sys_x]], []], [[null, [cadr, sys_x]], [list, [car, sys_x]]], [t, [list, [list, [car, sys_x], [cadr, sys_x]]]]]]), sys_slot_descriptions]]], [cond, [[consp, sys_constructor], ;, ;, the, case, sys_for, sys_a, sys_boa, sys_constructor_c46, ;, ;, sys_dirty, sys_code_c33_c33, ;, ;, sys_we, sys_must, sys_add, sys_an, sys_initial, sys_value, sys_for, sys_an, sys_optional, sys_parameter_c44, ;, ;, if, the, sys_default, sys_value, sys_is, not, sys_specified, ;, ;, sys_in, the, sys_given, sys_parameter, list, and, sys_yet, the, sys_initial, sys_value, ;, ;, sys_is, sys_supplied, sys_in, the, sys_slot, sys_description_c46, [do, [[sys_a, [cadr, sys_constructor], [cdr, sys_a]], [sys_l, []], [sys_vs, []]], [[endp, sys_a], ;, ;, sys_add, sys_those, sys_options, sys_that, do, not, sys_appear, sys_in, the, sys_parameter, list, ;, ;, sys_as, sys_auxiliary, sys_paramters_c46, ;, ;, the, sys_parameters, sys_are, sys_accumulated, sys_in, the, variable, sys_vs_c46, [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]]], ;, ;, sys_skip, sys_until, c38_optional, sys_appears_c46, [cond, [[eq, [car, sys_a], [quote, c38_optional]], [setq, sys_l, [cons, [quote, c38_optional], sys_l]], [do, [[sys_aa, [cdr, sys_a], [cdr, sys_aa]], [sys_ov], [sys_y]], [[endp, sys_aa], ;, ;, sys_add, sys_those, sys_options, sys_that, do, not, sys_appear, sys_in, the, ;, ;, sys_parameter, sys_list_c46, [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]], [when, [member, [car, sys_aa], lambda_list_keywords], [when, [eq, [car, sys_aa], [quote, c38_rest]], ;, ;, c38_rest, sys_is, sys_found_c46, [setq, sys_l, [cons, [quote, c38_rest], sys_l]], [setq, sys_aa, [cdr, sys_aa]], [unless, [and, [not, [endp, sys_aa]], [symbolp, [car, sys_aa]]], [sys_illegal_boa]], [setq, sys_vs, [cons, [car, sys_aa], sys_vs]], [setq, sys_l, [cons, [car, sys_aa], sys_l]], [setq, sys_aa, [cdr, sys_aa]], [when, [endp, sys_aa], [setq, sys_keys, [nreconc, [cons, [quote, c38_aux], sys_l], [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]]], ;, ;, c38_aux, sys_should, sys_follow_c46, [unless, [eq, [car, sys_aa], [quote, c38_aux]], [sys_illegal_boa]], [setq, sys_l, [cons, [quote, c38_aux], sys_l]], [do, [[sys_aaa, [cdr, sys_aa], [cdr, sys_aaa]]], [[endp, sys_aaa]], [setq, sys_l, [cons, [car, sys_aaa], sys_l]], [cond, [[and, [atom, [car, sys_aaa]], [symbolp, [car, sys_aaa]]], [setq, sys_vs, [cons, [car, sys_aaa], sys_vs]]], [[and, [symbolp, [caar, sys_aaa]], [or, [endp, [cdar, sys_aaa]], [endp, [cddar, sys_aaa]]]], [setq, sys_vs, [cons, [caar, sys_aaa], sys_vs]]], [t, [sys_illegal_boa]]]], ;, ;, sys_end, sys_of, the, sys_parameter, sys_list_c46, [setq, sys_keys, [nreconc, sys_l, [mapcan, function([lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), sys_keys]]], [return, []]], ;, ;, sys_checks, if, the, sys_optional, sys_paramter, sys_without, sys_a, sys_default, ;, ;, sys_value, sys_has, sys_a, sys_default, sys_value, sys_in, the, sys_slot_description_c46, [if, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [sys_x], [if, [consp, sys_x], ;, ;, sys_with, sys_default, sys_value_c46, [car, sys_x]]])]]], ;, ;, if, sys_no, sys_default, sys_value, sys_is, sys_supplied, sys_for, ;, ;, the, sys_optional, sys_parameter, and, sys_yet, sys_appears, ;, ;, sys_in, sys_keys, sys_with, sys_a, sys_default, sys_value_c44, ;, ;, sys_then, cons, the, sys_pair, sys_to, sys_l_c44, [setq, sys_l, [cons, [car, sys_y], sys_l]], ;, ;, otherwise, cons, sys_just, the, sys_parameter, sys_to, sys_l_c46, [setq, sys_l, [cons, [car, sys_aa], sys_l]]], ;, ;, sys_checks, the, sys_form, sys_of, the, sys_optional, sys_parameter_c46, [cond, [[atom, [car, sys_aa]], [unless, [symbolp, [car, sys_aa]], [sys_illegal_boa]], [setq, sys_vs, [cons, [car, sys_aa], sys_vs]]], [[not, [symbolp, [caar, sys_aa]]], [sys_illegal_boa]], [[or, [endp, [cdar, sys_aa]], [endp, [cddar, sys_aa]]], [setq, sys_vs, [cons, [caar, sys_aa], sys_vs]]], [[not, [symbolp, [caddar, sys_aa]]], [sys_illegal_boa]], [[not, [endp, [cdddar, sys_aa]]], [sys_illegal_boa]], [t, [setq, sys_vs, [cons, [caar, sys_aa], sys_vs]], [setq, sys_vs, [cons, [caddar, sys_aa], sys_vs]]]]], ;, ;, return, sys_from, the, sys_outside, sys_do_c46, [return, []]], [t, [unless, [symbolp, [car, sys_a]], [sys_illegal_boa]], [setq, sys_l, [cons, [car, sys_a], sys_l]], [setq, sys_vs, [cons, [car, sys_a], sys_vs]]]]], [setq, sys_constructor, [car, sys_constructor]]], [t, ;, ;, if, not, sys_a, sys_boa, sys_constructor_c44, sys_just, cons, sys_c38_key_c46, [setq, sys_keys, [cons, [quote, c38_key], sys_keys]]]], [cond, [[null, type], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [sys_make_structure, [quote, ['#COMMA', sys_name]], ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[or, [eq, type, [quote, vector]], [and, [consp, type], [eq, [car, type], [quote, vector]]]], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [vector, ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[eq, type, [quote, list]], ['#BQ', [defun, ['#COMMA', sys_constructor], ['#COMMA', sys_keys], [list, ['#BQ-COMMA-ELIPSE', sys_slot_names]]]]], [[error, '$ARRAY'([*], claz_base_character, "~S is an illegal structure type"), type]]]]]).
wl:arglist_info(sys_make_constructor, f_sys_make_constructor, [sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], arginfo{all:[sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], allow_other_keys:0, aux:0, body:0, complex:0, env:0, key:0, names:[sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], opt:0, req:[sys_name, sys_constructor, type, sys_named, sys_slot_descriptions], rest:0, sublists:0, whole:0}).
wl: init_args(x, f_sys_make_constructor).

/*

### Compiled Function: `SYS::MAKE-CONSTRUCTOR` 
*/
f_sys_make_constructor(Name_In, Constructor_In, Type_In, Named_In, Slot_descriptions_In, FnResult) :-
	Sf_declare_Param=[bv(sys_name, Name_In), bv(sys_constructor, Constructor_In), bv(type, Type_In), bv(sys_named, Named_In), bv(sys_slot_descriptions, Slot_descriptions_In)],
	catch(( ( sf_declare(Sf_declare_Param,
			     [ignore, sys_named],
			     Sf_declare_Ret),
		  LEnv=[bv([sys_slot_names, ;, ;, sys_collect, the, sys_slot_names_c46, [mapcar, function([lambda, [sys_x], [cond, [[null, sys_x], ;, ;, if, the, sys_slot_description, sys_is, sys_nil_c44, ;, ;, sys_it, sys_is, sys_in, the, sys_padding, sys_of, sys_initial_offset_c46, []], [[null, [car, sys_x]], ;, ;, if, the, sys_slot, sys_name, sys_is, sys_nil_c44, ;, ;, sys_it, sys_is, the, structure, sys_name_c46, ;, ;, sys_this, sys_is, sys_for, sys_typed, sys_structures, sys_with, sys_names_c46, [list, [quote, quote], [cadr, sys_x]]], [t, [car, sys_x]]]]), sys_slot_descriptions]], []), bv([sys_keys, ;, ;, sys_make, the, keyword, sys_parameters_c46, [mapcan, function([lambda, [sys_x], [cond, [[null, sys_x], []], [[null, [car, sys_x]], []], [[null, [cadr, sys_x]], [list, [car, sys_x]]], [t, [list, [list, [car, sys_x], [cadr, sys_x]]]]]]), sys_slot_descriptions]], [])|Sf_declare_Param],
		  get_var(LEnv, sys_constructor, Constructor_Get),
		  (   c0nz:is_consp(Constructor_Get)
		  ->  get_var(LEnv, ;, C59_Get17),
		      get_var(LEnv, case, Case_Get),
		      get_var(LEnv, sys_a, A_Get),
		      get_var(LEnv, sys_boa, Boa_Get),
		      ( get_var(LEnv, ;, C59_Get24),
			get_var(LEnv, the, The_Get)
		      ),
		      ( get_var(LEnv, ;, C59_Get25),
			get_var(LEnv, sys_for, For_Get)
		      ),
		      get_var(LEnv, sys_code_c33_c33, Code_c33_c33_Get),
		      ( get_var(LEnv, ;, C59_Get28),
			get_var(LEnv, sys_constructor_c46, Constructor_c46_Get)
		      ),
		      ( get_var(LEnv, sys_add, Add_Get),
			get_var(LEnv, sys_dirty, Dirty_Get)
		      ),
		      ( get_var(LEnv, sys_an, An_Get),
			get_var(LEnv, sys_must, Must_Get)
		      ),
		      ( get_var(LEnv, sys_for, For_Get36),
			get_var(LEnv, sys_we, We_Get)
		      ),
		      get_var(LEnv, sys_an, An_Get37),
		      ( get_var(LEnv, ;, C59_Get40),
			get_var(LEnv, sys_initial, Initial_Get)
		      ),
		      ( get_var(LEnv, ;, C59_Get41),
			get_var(LEnv, sys_value, Value_Get)
		      ),
		      ( get_var(LEnv, if, If_Get),
			get_var(LEnv, sys_optional, Optional_Get)
		      ),
		      ( get_var(LEnv, sys_default, Default_Get),
			get_var(LEnv, sys_parameter_c44, Parameter_c44_Get)
		      ),
		      ( get_var(LEnv, not, Not_Get),
			get_var(LEnv, sys_is, Is_Get)
		      ),
		      ( get_var(LEnv, ;, C59_Get49),
			get_var(LEnv, the, The_Get43)
		      ),
		      ( get_var(LEnv, ;, C59_Get50),
			get_var(LEnv, sys_value, Value_Get45)
		      ),
		      get_var(LEnv, sys_given, Given_Get),
		      ( get_var(LEnv, list, List_Get),
			get_var(LEnv, sys_specified, Specified_Get)
		      ),
		      ( get_var(LEnv, and, And_Get),
			get_var(LEnv, sys_in, In_Get)
		      ),
		      ( get_var(LEnv, sys_initial, Initial_Get59),
			get_var(LEnv, the, The_Get52)
		      ),
		      ( get_var(LEnv, ;, C59_Get61),
			get_var(LEnv, sys_parameter, Parameter_Get)
		      ),
		      ( get_var(LEnv, ;, C59_Get62),
			get_var(LEnv, sys_yet, Yet_Get)
		      ),
		      ( get_var(LEnv, sys_in, In_Get65),
			get_var(LEnv, the, The_Get58)
		      ),
		      ( get_var(LEnv, sys_is, Is_Get63),
			get_var(LEnv, sys_value, Value_Get60)
		      ),
		      ( get_var(LEnv, sys_constructor, Constructor_Get72),
			get_var(LEnv, sys_slot, Slot_Get)
		      ),
		      ( get_var(LEnv, sys_description_c46, Description_c46_Get),
			get_var(LEnv, sys_supplied, Supplied_Get)
		      ),
		      get_var(LEnv, the, The_Get66),
		      f_cadr(Constructor_Get72, A_Init),
		      BlockExitEnv=[bv(sys_a, A_Init), bv(sys_l, []), bv(sys_vs, [])|LEnv],
		      catch(( call_addr_block(BlockExitEnv,
					      (push_label(do_label_6), get_var(BlockExitEnv, sys_a, A_Get790), (s3q:is_endp(A_Get790)->get_var(BlockExitEnv, ;, C59_Get795), get_var(BlockExitEnv, sys_add, Add_Get797), (get_var(BlockExitEnv, do, Do_Get801), get_var(BlockExitEnv, sys_options, Options_Get799)), (get_var(BlockExitEnv, not, Not_Get802), get_var(BlockExitEnv, sys_those, Those_Get798)), (get_var(BlockExitEnv, sys_appear, Appear_Get803), get_var(BlockExitEnv, sys_that, That_Get800)), get_var(BlockExitEnv, sys_in, In_Get804), (get_var(BlockExitEnv, ;, C59_Get808), get_var(BlockExitEnv, sys_parameter, Parameter_Get806)), (get_var(BlockExitEnv, ;, C59_Get809), get_var(BlockExitEnv, the, The_Get805)), get_var(BlockExitEnv, list, List_Get807), (get_var(BlockExitEnv, ;, C59_Get813), get_var(BlockExitEnv, sys_as, As_Get810)), (get_var(BlockExitEnv, ;, C59_Get814), get_var(BlockExitEnv, sys_auxiliary, Auxiliary_Get811)), (get_var(BlockExitEnv, sys_parameters, Parameters_Get816), get_var(BlockExitEnv, sys_paramters_c46, Paramters_c46_Get812)), get_var(BlockExitEnv, sys_accumulated, Accumulated_Get818), (get_var(BlockExitEnv, sys_are, Are_Get817), get_var(BlockExitEnv, the, The_Get815)), get_var(BlockExitEnv, sys_in, In_Get819), get_var(BlockExitEnv, sys_vs_c46, Vs_c46_Get822), (get_var(BlockExitEnv, sys_l, L_Get824), get_var(BlockExitEnv, the, The_Get820)), get_var(BlockExitEnv, variable, Variable_Get821), Nreconc_Param=[c38_aux|L_Get824], get_var(BlockExitEnv, sys_keys, Keys_Get842), f_mapcan(closure(kw_function, [ClosureEnvironment840|BlockExitEnv], Whole841, LResult838, [sys_k],  (get_var(ClosureEnvironment840, sys_k, K_Get828), (K_Get828\=[CAR|CDR]->get_var(ClosureEnvironment840, sys_k, K_Get831), Member_Param=K_Get831;get_var(ClosureEnvironment840, sys_k, K_Get832), f_car(K_Get832, ElseResult834), Member_Param=ElseResult834), get_var(ClosureEnvironment840, sys_vs, Vs_Get835), f_member(Member_Param, Vs_Get835, [], IFTEST825), (IFTEST825\==[]->LResult838=[];get_var(ClosureEnvironment840, sys_k, K_Get836), ElseResult837=[K_Get836], LResult838=ElseResult837)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get842, Mapcan_Ret), f_nreconc(Nreconc_Param, Mapcan_Ret, RetResult793), set_var(BlockExitEnv, sys_keys, RetResult793), throw(block_exit([], RetResult793)), _TBResult=ThrowResult794;get_var(BlockExitEnv, ;, C59_Get844), get_var(BlockExitEnv, c38_optional, C38_optional_Get848), get_var(BlockExitEnv, sys_a, A_Get851), (get_var(BlockExitEnv, sys_appears_c46, Appears_c46_Get849), get_var(BlockExitEnv, sys_skip, Skip_Get846)), get_var(BlockExitEnv, sys_until, Until_Get847), f_car(A_Get851, PredArg1Result853), (is_eq(PredArg1Result853, c38_optional)->get_var(BlockExitEnv, sys_l, L_Get854), L=[c38_optional|L_Get854], set_var(BlockExitEnv, sys_l, L), get_var(BlockExitEnv, sys_a, A_Get858), f_cdr(A_Get858, Aa_Init859), BlockExitEnv=[bv(sys_aa, Aa_Init859), bv([sys_ov], []), bv([sys_y], [])|BlockExitEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_10), get_var(BlockExitEnv, sys_aa, Aa_Get1171), (s3q:is_endp(Aa_Get1171)->get_var(BlockExitEnv, ;, C59_Get1176), get_var(BlockExitEnv, sys_add, Add_Get1178), (get_var(BlockExitEnv, do, Do_Get1182), get_var(BlockExitEnv, sys_options, Options_Get1180)), (get_var(BlockExitEnv, not, Not_Get1183), get_var(BlockExitEnv, sys_those, Those_Get1179)), (get_var(BlockExitEnv, sys_appear, Appear_Get1184), get_var(BlockExitEnv, sys_that, That_Get1181)), get_var(BlockExitEnv, ;, C59_Get1187), (get_var(BlockExitEnv, ;, C59_Get1188), get_var(BlockExitEnv, sys_in, In_Get1185)), (get_var(BlockExitEnv, sys_list_c46, List_c46_Get1190), get_var(BlockExitEnv, the, The_Get1186)), get_var(BlockExitEnv, sys_l, L_Get1192), get_var(BlockExitEnv, sys_parameter, Parameter_Get1189), Nreconc_Param1573=[c38_aux|L_Get1192], get_var(BlockExitEnv, sys_keys, Keys_Get1210), f_mapcan(closure(kw_function, [ClosureEnvironment1208|BlockExitEnv], Whole1209, LResult1206, [sys_k],  (get_var(ClosureEnvironment1208, sys_k, K_Get1196), (K_Get1196\=[CAR1654|CDR1655]->get_var(ClosureEnvironment1208, sys_k, K_Get1199), Member_Param1572=K_Get1199;get_var(ClosureEnvironment1208, sys_k, K_Get1200), f_car(K_Get1200, ElseResult1202), Member_Param1572=ElseResult1202), get_var(ClosureEnvironment1208, sys_vs, Vs_Get1203), f_member(Member_Param1572, Vs_Get1203, [], IFTEST1193), (IFTEST1193\==[]->LResult1206=[];get_var(ClosureEnvironment1208, sys_k, K_Get1204), ElseResult1205=[K_Get1204], LResult1206=ElseResult1205)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1210, Mapcan_Ret1653), f_nreconc(Nreconc_Param1573, Mapcan_Ret1653, Keys), set_var(BlockExitEnv, sys_keys, Keys), throw(block_exit([], [])), throw(block_exit([], RetResult1174)), _TBResult860=ThrowResult1175;get_var(BlockExitEnv, sys_aa, Aa_Get1216), f_car(Aa_Get1216, Member_Param1574), get_var(BlockExitEnv, lambda_list_keywords, Lambda_list_keywords_Get1217), f_member(Member_Param1574, Lambda_list_keywords_Get1217, [], IFTEST1214), (IFTEST1214\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1219), f_car(Aa_Get1219, PredArg1Result1221), (is_eq(PredArg1Result1221, c38_rest)->get_var(BlockExitEnv, ;, C59_Get1222), get_var(BlockExitEnv, c38_rest, C38_rest_Get1224), get_var(BlockExitEnv, sys_found_c46, Found_c46_Get1226), get_var(BlockExitEnv, sys_is, Is_Get1225), get_var(BlockExitEnv, sys_l, L_Get1227), L1555=[c38_rest|L_Get1227], set_var(BlockExitEnv, sys_l, L1555), get_var(BlockExitEnv, sys_aa, Aa_Get1228), f_cdr(Aa_Get1228, Aa), set_var(BlockExitEnv, sys_aa, Aa), get_var(BlockExitEnv, sys_aa, Aa_Get1232), f_endp(Aa_Get1232, PredArgResult1234), (PredArgResult1234==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1235), f_car(Aa_Get1235, Symbolp_Param), f_symbolp(Symbolp_Param, TrueResult1236), IFTEST1229=TrueResult1236;IFTEST1229=[]), (IFTEST1229\==[]->_65014=[];f_sys_illegal_boa(ElseResult1237), _65014=ElseResult1237), get_var(BlockExitEnv, sys_aa, Aa_Get1238), f_car(Aa_Get1238, Car_Ret), get_var(BlockExitEnv, sys_vs, Vs_Get1239), Vs=[Car_Ret|Vs_Get1239], set_var(BlockExitEnv, sys_vs, Vs), get_var(BlockExitEnv, sys_aa, Aa_Get1240), f_car(Aa_Get1240, Car_Ret1657), get_var(BlockExitEnv, sys_l, L_Get1241), L1558=[Car_Ret1657|L_Get1241], set_var(BlockExitEnv, sys_l, L1558), get_var(BlockExitEnv, sys_aa, Aa_Get1242), f_cdr(Aa_Get1242, Aa1559), set_var(BlockExitEnv, sys_aa, Aa1559), get_var(BlockExitEnv, sys_aa, Aa_Get1244), (s3q:is_endp(Aa_Get1244)->get_var(BlockExitEnv, sys_l, L_Get1247), Nreconc_Param1577=[c38_aux|L_Get1247], get_var(BlockExitEnv, sys_keys, Keys_Get1265), f_mapcan(closure(kw_function, [ClosureEnvironment1263|BlockExitEnv], Whole1264, LResult1261, [sys_k],  (get_var(ClosureEnvironment1263, sys_k, K_Get1251), (K_Get1251\=[CAR1659|CDR1660]->get_var(ClosureEnvironment1263, sys_k, K_Get1254), Member_Param1576=K_Get1254;get_var(ClosureEnvironment1263, sys_k, K_Get1255), f_car(K_Get1255, ElseResult1257), Member_Param1576=ElseResult1257), get_var(ClosureEnvironment1263, sys_vs, Vs_Get1258), f_member(Member_Param1576, Vs_Get1258, [], IFTEST1248), (IFTEST1248\==[]->LResult1261=[];get_var(ClosureEnvironment1263, sys_k, K_Get1259), ElseResult1260=[K_Get1259], LResult1261=ElseResult1260)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1265, Mapcan_Ret1658), f_nreconc(Nreconc_Param1577, Mapcan_Ret1658, Keys1560), set_var(BlockExitEnv, sys_keys, Keys1560), throw(block_exit([], [])), TrueResult1269=ThrowResult1267;TrueResult1269=[]), _64712=TrueResult1269;_64712=[]), get_var(BlockExitEnv, ;, C59_Get1270), get_var(BlockExitEnv, c38_aux, C38_aux_Get1272), get_var(BlockExitEnv, sys_aa, Aa_Get1276), get_var(BlockExitEnv, sys_follow_c46, Follow_c46_Get1274), get_var(BlockExitEnv, sys_should, Should_Get1273), f_car(Aa_Get1276, PredArg1Result1278), (is_eq(PredArg1Result1278, c38_aux)->_66414=[];f_sys_illegal_boa(ElseResult1279), _66414=ElseResult1279), get_var(BlockExitEnv, sys_l, L_Get1280), L1561=[c38_aux|L_Get1280], set_var(BlockExitEnv, sys_l, L1561), get_var(BlockExitEnv, sys_aa, Aa_Get1284), f_cdr(Aa_Get1284, Aaa_Init1285), AEnv=[bv(sys_aaa, Aaa_Init1285)|BlockExitEnv], catch((call_addr_block(AEnv,  (push_label(do_label_12), get_var(AEnv, sys_aaa, Aaa_Get1330), (s3q:is_endp(Aaa_Get1330)->throw(block_exit([], [])), _TBResult1286=ThrowResult1334;get_var(AEnv, sys_aaa, Aaa_Get1337), f_car(Aaa_Get1337, Car_Ret1661), get_var(AEnv, sys_l, L_Get1338), L1562=[Car_Ret1661|L_Get1338], set_var(AEnv, sys_l, L1562), get_var(AEnv, sys_aaa, Aaa_Get1342), f_car(Aaa_Get1342, PredArgResult1344), (PredArgResult1344\=[CAR1662|CDR1663]->get_var(AEnv, sys_aaa, Aaa_Get1345), f_car(Aaa_Get1345, Symbolp_Param1578), f_symbolp(Symbolp_Param1578, TrueResult1346), IFTEST1339=TrueResult1346;IFTEST1339=[]), (IFTEST1339\==[]->get_var(AEnv, sys_aaa, Aaa_Get1347), f_car(Aaa_Get1347, Car_Ret1664), get_var(AEnv, sys_vs, Vs_Get1348), TrueResult1363=[Car_Ret1664|Vs_Get1348], set_var(AEnv, sys_vs, TrueResult1363), _67932=TrueResult1363;get_var(AEnv, sys_aaa, Aaa_Get1352), f_caar(Aaa_Get1352, PredArgResult1354), (is_symbolp(PredArgResult1354)->(get_var(AEnv, sys_aaa, Aaa_Get1355), f_cdar(Aaa_Get1355, Endp_Param), f_endp(Endp_Param, FORM1_Res1357), FORM1_Res1357\==[], TrueResult1358=FORM1_Res1357->true;get_var(AEnv, sys_aaa, Aaa_Get1356), f_cddar(Aaa_Get1356, Endp_Param1580), f_endp(Endp_Param1580, Endp_Ret), TrueResult1358=Endp_Ret), IFTEST1349=TrueResult1358;IFTEST1349=[]), (IFTEST1349\==[]->get_var(AEnv, sys_aaa, Aaa_Get1359), f_caar(Aaa_Get1359, Caar_Ret), get_var(AEnv, sys_vs, Vs_Get1360), TrueResult1361=[Caar_Ret|Vs_Get1360], set_var(AEnv, sys_vs, TrueResult1361), ElseResult1364=TrueResult1361;f_sys_illegal_boa(ElseResult1362), ElseResult1364=ElseResult1362), _67932=ElseResult1364), get_var(AEnv, sys_aaa, Aaa_Get1365), f_cdr(Aaa_Get1365, Aaa), set_var(AEnv, sys_aaa, Aaa), goto(do_label_12, AEnv), _TBResult1286=_GORES1366)), [addr(addr_tagbody_12_do_label_12, do_label_12, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get1288), (s3q:is_endp(Aaa_Get1288)->throw(block_exit([], [])), _TBResult1286=ThrowResult1292;get_var(AEnv, sys_aaa, Aaa_Get1295), f_car(Aaa_Get1295, Car_Ret1667), get_var(AEnv, sys_l, L_Get1296), Set_var_Ret=[Car_Ret1667|L_Get1296], set_var(AEnv, sys_l, Set_var_Ret), get_var(AEnv, sys_aaa, Aaa_Get1300), f_car(Aaa_Get1300, PredArgResult1302), (PredArgResult1302\=[CAR1669|CDR1670]->get_var(AEnv, sys_aaa, Aaa_Get1303), f_car(Aaa_Get1303, Symbolp_Param1581), f_symbolp(Symbolp_Param1581, TrueResult1304), IFTEST1297=TrueResult1304;IFTEST1297=[]), (IFTEST1297\==[]->get_var(AEnv, sys_aaa, Aaa_Get1305), f_car(Aaa_Get1305, Car_Ret1671), get_var(AEnv, sys_vs, Vs_Get1306), TrueResult1321=[Car_Ret1671|Vs_Get1306], set_var(AEnv, sys_vs, TrueResult1321), _69000=TrueResult1321;get_var(AEnv, sys_aaa, Aaa_Get1310), f_caar(Aaa_Get1310, PredArgResult1312), (is_symbolp(PredArgResult1312)->(get_var(AEnv, sys_aaa, Aaa_Get1313), f_cdar(Aaa_Get1313, Endp_Param1582), f_endp(Endp_Param1582, FORM1_Res1315), FORM1_Res1315\==[], TrueResult1316=FORM1_Res1315->true;get_var(AEnv, sys_aaa, Aaa_Get1314), f_cddar(Aaa_Get1314, Endp_Param1583), f_endp(Endp_Param1583, Endp_Ret1672), TrueResult1316=Endp_Ret1672), IFTEST1307=TrueResult1316;IFTEST1307=[]), (IFTEST1307\==[]->get_var(AEnv, sys_aaa, Aaa_Get1317), f_caar(Aaa_Get1317, Caar_Ret1673), get_var(AEnv, sys_vs, Vs_Get1318), TrueResult1319=[Caar_Ret1673|Vs_Get1318], set_var(AEnv, sys_vs, TrueResult1319), ElseResult1322=TrueResult1319;f_sys_illegal_boa(ElseResult1320), ElseResult1322=ElseResult1320), _69000=ElseResult1322), get_var(AEnv, sys_aaa, Aaa_Get1323), f_cdr(Aaa_Get1323, Cdr_Ret), set_var(AEnv, sys_aaa, Cdr_Ret), goto(do_label_12, AEnv), _TBResult1286=_GORES1324)))]), []=LetResult1282), block_exit([], LetResult1282), true), get_var(BlockExitEnv, ;, C59_Get1370), get_var(BlockExitEnv, sys_end, End_Get1372), get_var(BlockExitEnv, sys_l, L_Get1377), (get_var(BlockExitEnv, sys_keys, Keys_Get1395), get_var(BlockExitEnv, sys_of, Of_Get1373)), (get_var(BlockExitEnv, sys_list_c46, List_c46_Get1376), get_var(BlockExitEnv, sys_parameter, Parameter_Get1375)), get_var(BlockExitEnv, the, The_Get1374), f_mapcan(closure(kw_function, [ClosureEnvironment1393|BlockExitEnv], Whole1394, LResult1391, [sys_k],  (get_var(ClosureEnvironment1393, sys_k, K_Get1381), (K_Get1381\=[CAR1676|CDR1677]->get_var(ClosureEnvironment1393, sys_k, K_Get1384), Member_Param1584=K_Get1384;get_var(ClosureEnvironment1393, sys_k, K_Get1385), f_car(K_Get1385, ElseResult1387), Member_Param1584=ElseResult1387), get_var(ClosureEnvironment1393, sys_vs, Vs_Get1388), f_member(Member_Param1584, Vs_Get1388, [], IFTEST1378), (IFTEST1378\==[]->LResult1391=[];get_var(ClosureEnvironment1393, sys_k, K_Get1389), ElseResult1390=[K_Get1389], LResult1391=ElseResult1390)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1395, Mapcan_Ret1675), f_nreconc(L_Get1377, Mapcan_Ret1675, Keys1564), set_var(BlockExitEnv, sys_keys, Keys1564), throw(block_exit([], [])), _64610=ThrowResult1397;_64610=[]), get_var(BlockExitEnv, ;, C59_Get1399), get_var(BlockExitEnv, if, If_Get1402), get_var(BlockExitEnv, sys_checks, Checks_Get1401), get_var(BlockExitEnv, sys_optional, Optional_Get1404), (get_var(BlockExitEnv, sys_a, A_Get1407), get_var(BlockExitEnv, sys_paramter, Paramter_Get1405)), (get_var(BlockExitEnv, ;, C59_Get1409), get_var(BlockExitEnv, the, The_Get1403)), get_var(BlockExitEnv, ;, C59_Get1410), (get_var(BlockExitEnv, sys_default, Default_Get1408), get_var(BlockExitEnv, sys_without, Without_Get1406)), get_var(BlockExitEnv, sys_a, A_Get1413), (get_var(BlockExitEnv, sys_default, Default_Get1414), get_var(BlockExitEnv, sys_has, Has_Get1412)), (get_var(BlockExitEnv, sys_in, In_Get1416), get_var(BlockExitEnv, sys_value, Value_Get1411)), get_var(BlockExitEnv, sys_slot_description_c46, Slot_description_c46_Get1418), get_var(BlockExitEnv, sys_value, Value_Get1415), get_var(BlockExitEnv, the, The_Get1417), sf_if(BlockExitEnv, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [sys_x], [if, [consp, sys_x], ;, ;, sys_with, sys_default, sys_value_c46, [car, sys_x]]])]]], ;, ;, if, sys_no, sys_default, sys_value, sys_is, sys_supplied, sys_for, ;, ;, the, sys_optional, sys_parameter, and, sys_yet, sys_appears, ;, ;, sys_in, sys_keys, sys_with, sys_a, sys_default, sys_value_c44, ;, ;, sys_then, cons, the, sys_pair, sys_to, sys_l_c44, [setq, sys_l, [cons, [car, sys_y], sys_l]], ;, ;, otherwise, cons, sys_just, the, sys_parameter, sys_to, sys_l_c46, [setq, sys_l, [cons, [car, sys_aa], sys_l]], Sf_if_Ret), get_var(BlockExitEnv, ;, C59_Get1419), get_var(BlockExitEnv, sys_checks, Checks_Get1421), get_var(BlockExitEnv, sys_form, Form_Get1423), get_var(BlockExitEnv, sys_of, Of_Get1424), get_var(BlockExitEnv, sys_optional, Optional_Get1426), (get_var(BlockExitEnv, sys_aa, Aa_Get1429), get_var(BlockExitEnv, the, The_Get1422)), get_var(BlockExitEnv, sys_parameter_c46, Parameter_c46_Get1427), get_var(BlockExitEnv, the, The_Get1425), f_car(Aa_Get1429, PredArgResult1431), (PredArgResult1431\=[CAR1679|CDR1680]->get_var(BlockExitEnv, sys_aa, Aa_Get1433), f_car(Aa_Get1433, PredArgResult1435), (is_symbolp(PredArgResult1435)->_71094=[];f_sys_illegal_boa(ElseResult1436), _71094=ElseResult1436), get_var(BlockExitEnv, sys_aa, Aa_Get1437), f_car(Aa_Get1437, Car_Ret1681), get_var(BlockExitEnv, sys_vs, Vs_Get1438), TrueResult1470=[Car_Ret1681|Vs_Get1438], set_var(BlockExitEnv, sys_vs, TrueResult1470), _64608=TrueResult1470;get_var(BlockExitEnv, sys_aa, Aa_Get1440), f_caar(Aa_Get1440, Symbolp_Param1585), f_symbolp(Symbolp_Param1585, PredArgResult1442), (PredArgResult1442==[]->f_sys_illegal_boa(TrueResult1468), ElseResult1471=TrueResult1468;(get_var(BlockExitEnv, sys_aa, Aa_Get1445), f_cdar(Aa_Get1445, Endp_Param1586), f_endp(Endp_Param1586, FORM1_Res1447), FORM1_Res1447\==[], IFTEST1443=FORM1_Res1447->true;get_var(BlockExitEnv, sys_aa, Aa_Get1446), f_cddar(Aa_Get1446, Endp_Param1587), f_endp(Endp_Param1587, Endp_Ret1682), IFTEST1443=Endp_Ret1682), (IFTEST1443\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get1448), f_caar(Aa_Get1448, Caar_Ret1683), get_var(BlockExitEnv, sys_vs, Vs_Get1449), TrueResult1466=[Caar_Ret1683|Vs_Get1449], set_var(BlockExitEnv, sys_vs, TrueResult1466), ElseResult1469=TrueResult1466;get_var(BlockExitEnv, sys_aa, Aa_Get1451), f_caddar(Aa_Get1451, Symbolp_Param1588), f_symbolp(Symbolp_Param1588, PredArgResult1453), (PredArgResult1453==[]->f_sys_illegal_boa(TrueResult1464), ElseResult1467=TrueResult1464;get_var(BlockExitEnv, sys_aa, Aa_Get1455), f_cdddar(Aa_Get1455, Endp_Param1589), f_endp(Endp_Param1589, PredArgResult1457), (PredArgResult1457==[]->f_sys_illegal_boa(TrueResult1462), ElseResult1465=TrueResult1462;get_var(BlockExitEnv, sys_aa, Aa_Get1458), f_caar(Aa_Get1458, Caar_Ret1684), get_var(BlockExitEnv, sys_vs, Vs_Get1459), Vs1565=[Caar_Ret1684|Vs_Get1459], set_var(BlockExitEnv, sys_vs, Vs1565), get_var(BlockExitEnv, sys_aa, Aa_Get1460), f_caddar(Aa_Get1460, Caddar_Ret), get_var(BlockExitEnv, sys_vs, Vs_Get1461), ElseResult1463=[Caddar_Ret|Vs_Get1461], set_var(BlockExitEnv, sys_vs, ElseResult1463), ElseResult1465=ElseResult1463), ElseResult1467=ElseResult1465), ElseResult1469=ElseResult1467), ElseResult1471=ElseResult1469), _64608=ElseResult1471), get_var(BlockExitEnv, sys_aa, Aa_Get1472), f_cdr(Aa_Get1472, Aa1566), set_var(BlockExitEnv, sys_aa, Aa1566), goto(do_label_10, BlockExitEnv), _TBResult860=_GORES1473)), [addr(addr_tagbody_10_do_label_10, do_label_10, '$unused', AEnv,  (get_var(AEnv, sys_aa, Aa_Get862), (s3q:is_endp(Aa_Get862)->get_var(AEnv, ;, C59_Get867), get_var(AEnv, sys_add, Add_Get869), (get_var(AEnv, do, Do_Get873), get_var(AEnv, sys_options, Options_Get871)), (get_var(AEnv, not, Not_Get874), get_var(AEnv, sys_those, Those_Get870)), (get_var(AEnv, sys_appear, Appear_Get875), get_var(AEnv, sys_that, That_Get872)), get_var(AEnv, ;, C59_Get878), (get_var(AEnv, ;, C59_Get879), get_var(AEnv, sys_in, In_Get876)), (get_var(AEnv, sys_list_c46, List_c46_Get881), get_var(AEnv, the, The_Get877)), get_var(AEnv, sys_l, L_Get883), get_var(AEnv, sys_parameter, Parameter_Get880), Nreconc_Param1591=[c38_aux|L_Get883], get_var(AEnv, sys_keys, Keys_Get901), f_mapcan(closure(kw_function, [ClosureEnvironment899|AEnv], Whole900, LResult897, [sys_k],  (get_var(ClosureEnvironment899, sys_k, K_Get887), (K_Get887\=[CAR1687|CDR1688]->get_var(ClosureEnvironment899, sys_k, K_Get890), Member_Param1590=K_Get890;get_var(ClosureEnvironment899, sys_k, K_Get891), f_car(K_Get891, ElseResult893), Member_Param1590=ElseResult893), get_var(ClosureEnvironment899, sys_vs, Vs_Get894), f_member(Member_Param1590, Vs_Get894, [], IFTEST884), (IFTEST884\==[]->LResult897=[];get_var(ClosureEnvironment899, sys_k, K_Get895), ElseResult896=[K_Get895], LResult897=ElseResult896)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get901, Mapcan_Ret1686), f_nreconc(Nreconc_Param1591, Mapcan_Ret1686, Nreconc_Ret), set_var(AEnv, sys_keys, Nreconc_Ret), throw(block_exit([], [])), throw(block_exit([], RetResult865)), _TBResult860=ThrowResult866;get_var(AEnv, sys_aa, Aa_Get907), f_car(Aa_Get907, Member_Param1592), get_var(AEnv, lambda_list_keywords, Lambda_list_keywords_Get908), f_member(Member_Param1592, Lambda_list_keywords_Get908, [], IFTEST905), (IFTEST905\==[]->get_var(AEnv, sys_aa, Aa_Get910), f_car(Aa_Get910, PredArg1Result912), (is_eq(PredArg1Result912, c38_rest)->get_var(AEnv, ;, C59_Get913), get_var(AEnv, c38_rest, C38_rest_Get915), get_var(AEnv, sys_found_c46, Found_c46_Get917), get_var(AEnv, sys_is, Is_Get916), get_var(AEnv, sys_l, L_Get919), Set_var_Ret1690=[c38_rest|L_Get919], set_var(AEnv, sys_l, Set_var_Ret1690), get_var(AEnv, sys_aa, Aa_Get920), f_cdr(Aa_Get920, Cdr_Ret1691), set_var(AEnv, sys_aa, Cdr_Ret1691), get_var(AEnv, sys_aa, Aa_Get924), f_endp(Aa_Get924, PredArgResult926), (PredArgResult926==[]->get_var(AEnv, sys_aa, Aa_Get927), f_car(Aa_Get927, Symbolp_Param1593), f_symbolp(Symbolp_Param1593, TrueResult928), IFTEST921=TrueResult928;IFTEST921=[]), (IFTEST921\==[]->_72958=[];f_sys_illegal_boa(ElseResult929), _72958=ElseResult929), get_var(AEnv, sys_aa, Aa_Get930), f_car(Aa_Get930, Car_Ret1692), get_var(AEnv, sys_vs, Vs_Get931), Set_var_Ret1693=[Car_Ret1692|Vs_Get931], set_var(AEnv, sys_vs, Set_var_Ret1693), get_var(AEnv, sys_aa, Aa_Get932), f_car(Aa_Get932, Car_Ret1694), get_var(AEnv, sys_l, L_Get933), Set_var_Ret1695=[Car_Ret1694|L_Get933], set_var(AEnv, sys_l, Set_var_Ret1695), get_var(AEnv, sys_aa, Aa_Get934), f_cdr(Aa_Get934, Cdr_Ret1696), set_var(AEnv, sys_aa, Cdr_Ret1696), get_var(AEnv, sys_aa, Aa_Get936), (s3q:is_endp(Aa_Get936)->get_var(AEnv, sys_l, L_Get939), Nreconc_Param1595=[c38_aux|L_Get939], get_var(AEnv, sys_keys, Keys_Get957), f_mapcan(closure(kw_function, [ClosureEnvironment955|AEnv], Whole956, LResult953, [sys_k],  (get_var(ClosureEnvironment955, sys_k, K_Get943), (K_Get943\=[CAR1698|CDR1699]->get_var(ClosureEnvironment955, sys_k, K_Get946), Member_Param1594=K_Get946;get_var(ClosureEnvironment955, sys_k, K_Get947), f_car(K_Get947, ElseResult949), Member_Param1594=ElseResult949), get_var(ClosureEnvironment955, sys_vs, Vs_Get950), f_member(Member_Param1594, Vs_Get950, [], IFTEST940), (IFTEST940\==[]->LResult953=[];get_var(ClosureEnvironment955, sys_k, K_Get951), ElseResult952=[K_Get951], LResult953=ElseResult952)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get957, Mapcan_Ret1697), f_nreconc(Nreconc_Param1595, Mapcan_Ret1697, Nreconc_Ret1700), set_var(AEnv, sys_keys, Nreconc_Ret1700), throw(block_exit([], [])), TrueResult961=ThrowResult959;TrueResult961=[]), _73290=TrueResult961;_73290=[]), get_var(AEnv, ;, C59_Get962), get_var(AEnv, c38_aux, C38_aux_Get964), get_var(AEnv, sys_aa, Aa_Get968), get_var(AEnv, sys_follow_c46, Follow_c46_Get966), get_var(AEnv, sys_should, Should_Get965), f_car(Aa_Get968, PredArg1Result970), (is_eq(PredArg1Result970, c38_aux)->_73376=[];f_sys_illegal_boa(ElseResult971), _73376=ElseResult971), get_var(AEnv, sys_l, L_Get972), Set_var_Ret1701=[c38_aux|L_Get972], set_var(AEnv, sys_l, Set_var_Ret1701), get_var(AEnv, sys_aa, Aa_Get976), f_cdr(Aa_Get976, Aaa_Init977), AEnv=[bv(sys_aaa, Aaa_Init977)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_11), get_var(AEnv, sys_aaa, Aaa_Get1022), (s3q:is_endp(Aaa_Get1022)->throw(block_exit([], [])), _TBResult978=ThrowResult1026;get_var(AEnv, sys_aaa, Aaa_Get1029), f_car(Aaa_Get1029, Car_Ret1702), get_var(AEnv, sys_l, L_Get1030), Set_var_Ret1703=[Car_Ret1702|L_Get1030], set_var(AEnv, sys_l, Set_var_Ret1703), get_var(AEnv, sys_aaa, Aaa_Get1034), f_car(Aaa_Get1034, PredArgResult1036), (PredArgResult1036\=[CAR1704|CDR1705]->get_var(AEnv, sys_aaa, Aaa_Get1037), f_car(Aaa_Get1037, Symbolp_Param1596), f_symbolp(Symbolp_Param1596, TrueResult1038), IFTEST1031=TrueResult1038;IFTEST1031=[]), (IFTEST1031\==[]->get_var(AEnv, sys_aaa, Aaa_Get1039), f_car(Aaa_Get1039, Car_Ret1706), get_var(AEnv, sys_vs, Vs_Get1040), TrueResult1055=[Car_Ret1706|Vs_Get1040], set_var(AEnv, sys_vs, TrueResult1055), _73644=TrueResult1055;get_var(AEnv, sys_aaa, Aaa_Get1044), f_caar(Aaa_Get1044, PredArgResult1046), (is_symbolp(PredArgResult1046)->(get_var(AEnv, sys_aaa, Aaa_Get1047), f_cdar(Aaa_Get1047, Endp_Param1597), f_endp(Endp_Param1597, FORM1_Res1049), FORM1_Res1049\==[], TrueResult1050=FORM1_Res1049->true;get_var(AEnv, sys_aaa, Aaa_Get1048), f_cddar(Aaa_Get1048, Endp_Param1598), f_endp(Endp_Param1598, Endp_Ret1707), TrueResult1050=Endp_Ret1707), IFTEST1041=TrueResult1050;IFTEST1041=[]), (IFTEST1041\==[]->get_var(AEnv, sys_aaa, Aaa_Get1051), f_caar(Aaa_Get1051, Caar_Ret1708), get_var(AEnv, sys_vs, Vs_Get1052), TrueResult1053=[Caar_Ret1708|Vs_Get1052], set_var(AEnv, sys_vs, TrueResult1053), ElseResult1056=TrueResult1053;f_sys_illegal_boa(ElseResult1054), ElseResult1056=ElseResult1054), _73644=ElseResult1056), get_var(AEnv, sys_aaa, Aaa_Get1057), f_cdr(Aaa_Get1057, Cdr_Ret1709), set_var(AEnv, sys_aaa, Cdr_Ret1709), goto(do_label_11, AEnv), _TBResult978=_GORES1058)), [addr(addr_tagbody_11_do_label_11, do_label_11, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get980), (s3q:is_endp(Aaa_Get980)->throw(block_exit([], [])), _TBResult978=ThrowResult984;get_var(AEnv, sys_aaa, Aaa_Get987), f_car(Aaa_Get987, Car_Ret1710), get_var(AEnv, sys_l, L_Get988), Set_var_Ret1711=[Car_Ret1710|L_Get988], set_var(AEnv, sys_l, Set_var_Ret1711), get_var(AEnv, sys_aaa, Aaa_Get992), f_car(Aaa_Get992, PredArgResult994), (PredArgResult994\=[CAR1712|CDR1713]->get_var(AEnv, sys_aaa, Aaa_Get995), f_car(Aaa_Get995, Symbolp_Param1599), f_symbolp(Symbolp_Param1599, TrueResult996), IFTEST989=TrueResult996;IFTEST989=[]), (IFTEST989\==[]->get_var(AEnv, sys_aaa, Aaa_Get997), f_car(Aaa_Get997, Car_Ret1714), get_var(AEnv, sys_vs, Vs_Get998), TrueResult1013=[Car_Ret1714|Vs_Get998], set_var(AEnv, sys_vs, TrueResult1013), _74060=TrueResult1013;get_var(AEnv, sys_aaa, Aaa_Get1002), f_caar(Aaa_Get1002, PredArgResult1004), (is_symbolp(PredArgResult1004)->(get_var(AEnv, sys_aaa, Aaa_Get1005), f_cdar(Aaa_Get1005, Endp_Param1600), f_endp(Endp_Param1600, FORM1_Res1007), FORM1_Res1007\==[], TrueResult1008=FORM1_Res1007->true;get_var(AEnv, sys_aaa, Aaa_Get1006), f_cddar(Aaa_Get1006, Endp_Param1601), f_endp(Endp_Param1601, Endp_Ret1715), TrueResult1008=Endp_Ret1715), IFTEST999=TrueResult1008;IFTEST999=[]), (IFTEST999\==[]->get_var(AEnv, sys_aaa, Aaa_Get1009), f_caar(Aaa_Get1009, Caar_Ret1716), get_var(AEnv, sys_vs, Vs_Get1010), TrueResult1011=[Caar_Ret1716|Vs_Get1010], set_var(AEnv, sys_vs, TrueResult1011), ElseResult1014=TrueResult1011;f_sys_illegal_boa(ElseResult1012), ElseResult1014=ElseResult1012), _74060=ElseResult1014), get_var(AEnv, sys_aaa, Aaa_Get1015), f_cdr(Aaa_Get1015, Cdr_Ret1717), set_var(AEnv, sys_aaa, Cdr_Ret1717), goto(do_label_11, AEnv), _TBResult978=_GORES1016)))]), []=LetResult974), block_exit([], LetResult974), true), get_var(AEnv, ;, C59_Get1062), get_var(AEnv, sys_end, End_Get1064), get_var(AEnv, sys_l, L_Get1069), (get_var(AEnv, sys_keys, Keys_Get1087), get_var(AEnv, sys_of, Of_Get1065)), (get_var(AEnv, sys_list_c46, List_c46_Get1068), get_var(AEnv, sys_parameter, Parameter_Get1067)), get_var(AEnv, the, The_Get1066), f_mapcan(closure(kw_function, [ClosureEnvironment1085|AEnv], Whole1086, LResult1083, [sys_k],  (get_var(ClosureEnvironment1085, sys_k, K_Get1073), (K_Get1073\=[CAR1719|CDR1720]->get_var(ClosureEnvironment1085, sys_k, K_Get1076), Member_Param1602=K_Get1076;get_var(ClosureEnvironment1085, sys_k, K_Get1077), f_car(K_Get1077, ElseResult1079), Member_Param1602=ElseResult1079), get_var(ClosureEnvironment1085, sys_vs, Vs_Get1080), f_member(Member_Param1602, Vs_Get1080, [], IFTEST1070), (IFTEST1070\==[]->LResult1083=[];get_var(ClosureEnvironment1085, sys_k, K_Get1081), ElseResult1082=[K_Get1081], LResult1083=ElseResult1082)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get1087, Mapcan_Ret1718), f_nreconc(L_Get1069, Mapcan_Ret1718, Nreconc_Ret1721), set_var(AEnv, sys_keys, Nreconc_Ret1721), throw(block_exit([], [])), _74558=ThrowResult1089;_74558=[]), get_var(AEnv, ;, C59_Get1091), get_var(AEnv, if, If_Get1094), get_var(AEnv, sys_checks, Checks_Get1093), get_var(AEnv, sys_optional, Optional_Get1096), (get_var(AEnv, sys_a, A_Get1099), get_var(AEnv, sys_paramter, Paramter_Get1097)), (get_var(AEnv, ;, C59_Get1101), get_var(AEnv, the, The_Get1095)), get_var(AEnv, ;, C59_Get1102), (get_var(AEnv, sys_default, Default_Get1100), get_var(AEnv, sys_without, Without_Get1098)), get_var(AEnv, sys_a, A_Get1105), (get_var(AEnv, sys_default, Default_Get1106), get_var(AEnv, sys_has, Has_Get1104)), (get_var(AEnv, sys_in, In_Get1108), get_var(AEnv, sys_value, Value_Get1103)), get_var(AEnv, sys_slot_description_c46, Slot_description_c46_Get1110), get_var(AEnv, sys_value, Value_Get1107), get_var(AEnv, the, The_Get1109), sf_if(AEnv, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [sys_x], [if, [consp, sys_x], ;, ;, sys_with, sys_default, sys_value_c46, [car, sys_x]]])]]], ;, ;, if, sys_no, sys_default, sys_value, sys_is, sys_supplied, sys_for, ;, ;, the, sys_optional, sys_parameter, and, sys_yet, sys_appears, ;, ;, sys_in, sys_keys, sys_with, sys_a, sys_default, sys_value_c44, ;, ;, sys_then, cons, the, sys_pair, sys_to, sys_l_c44, [setq, sys_l, [cons, [car, sys_y], sys_l]], ;, ;, otherwise, cons, sys_just, the, sys_parameter, sys_to, sys_l_c46, [setq, sys_l, [cons, [car, sys_aa], sys_l]], Sf_if_Ret1722), get_var(AEnv, ;, C59_Get1111), get_var(AEnv, sys_checks, Checks_Get1113), get_var(AEnv, sys_form, Form_Get1115), get_var(AEnv, sys_of, Of_Get1116), get_var(AEnv, sys_optional, Optional_Get1118), (get_var(AEnv, sys_aa, Aa_Get1121), get_var(AEnv, the, The_Get1114)), get_var(AEnv, sys_parameter_c46, Parameter_c46_Get1119), get_var(AEnv, the, The_Get1117), f_car(Aa_Get1121, PredArgResult1123), (PredArgResult1123\=[CAR1723|CDR1724]->get_var(AEnv, sys_aa, Aa_Get1125), f_car(Aa_Get1125, PredArgResult1127), (is_symbolp(PredArgResult1127)->_75014=[];f_sys_illegal_boa(ElseResult1128), _75014=ElseResult1128), get_var(AEnv, sys_aa, Aa_Get1129), f_car(Aa_Get1129, Car_Ret1725), get_var(AEnv, sys_vs, Vs_Get1130), TrueResult1162=[Car_Ret1725|Vs_Get1130], set_var(AEnv, sys_vs, TrueResult1162), _75074=TrueResult1162;get_var(AEnv, sys_aa, Aa_Get1132), f_caar(Aa_Get1132, Symbolp_Param1603), f_symbolp(Symbolp_Param1603, PredArgResult1134), (PredArgResult1134==[]->f_sys_illegal_boa(TrueResult1160), ElseResult1163=TrueResult1160;(get_var(AEnv, sys_aa, Aa_Get1137), f_cdar(Aa_Get1137, Endp_Param1604), f_endp(Endp_Param1604, FORM1_Res1139), FORM1_Res1139\==[], IFTEST1135=FORM1_Res1139->true;get_var(AEnv, sys_aa, Aa_Get1138), f_cddar(Aa_Get1138, Endp_Param1605), f_endp(Endp_Param1605, Endp_Ret1726), IFTEST1135=Endp_Ret1726), (IFTEST1135\==[]->get_var(AEnv, sys_aa, Aa_Get1140), f_caar(Aa_Get1140, Caar_Ret1727), get_var(AEnv, sys_vs, Vs_Get1141), TrueResult1158=[Caar_Ret1727|Vs_Get1141], set_var(AEnv, sys_vs, TrueResult1158), ElseResult1161=TrueResult1158;get_var(AEnv, sys_aa, Aa_Get1143), f_caddar(Aa_Get1143, Symbolp_Param1606), f_symbolp(Symbolp_Param1606, PredArgResult1145), (PredArgResult1145==[]->f_sys_illegal_boa(TrueResult1156), ElseResult1159=TrueResult1156;get_var(AEnv, sys_aa, Aa_Get1147), f_cdddar(Aa_Get1147, Endp_Param1607), f_endp(Endp_Param1607, PredArgResult1149), (PredArgResult1149==[]->f_sys_illegal_boa(TrueResult1154), ElseResult1157=TrueResult1154;get_var(AEnv, sys_aa, Aa_Get1150), f_caar(Aa_Get1150, Caar_Ret1728), get_var(AEnv, sys_vs, Vs_Get1151), Set_var_Ret1729=[Caar_Ret1728|Vs_Get1151], set_var(AEnv, sys_vs, Set_var_Ret1729), get_var(AEnv, sys_aa, Aa_Get1152), f_caddar(Aa_Get1152, Caddar_Ret1730), get_var(AEnv, sys_vs, Vs_Get1153), ElseResult1155=[Caddar_Ret1730|Vs_Get1153], set_var(AEnv, sys_vs, ElseResult1155), ElseResult1157=ElseResult1155), ElseResult1159=ElseResult1157), ElseResult1161=ElseResult1159), ElseResult1163=ElseResult1161), _75074=ElseResult1163), get_var(AEnv, sys_aa, Aa_Get1164), f_cdr(Aa_Get1164, Cdr_Ret1731), set_var(AEnv, sys_aa, Cdr_Ret1731), goto(do_label_10, AEnv), _TBResult860=_GORES1165)))]), []=LetResult856), block_exit([], LetResult856), true), get_var(BlockExitEnv, ;, C59_Get1477), set_var(BlockExitEnv, 'block_ret_[]', []), always('block_exit_[]', BlockExitEnv), _54252=ThrowResult1485;get_var(BlockExitEnv, sys_a, A_Get1487), f_car(A_Get1487, PredArgResult1489), (is_symbolp(PredArgResult1489)->_75734=[];f_sys_illegal_boa(ElseResult1490), _75734=ElseResult1490), get_var(BlockExitEnv, sys_a, A_Get1491), f_car(A_Get1491, Car_Ret1732), get_var(BlockExitEnv, sys_l, L_Get1492), L1567=[Car_Ret1732|L_Get1492], set_var(BlockExitEnv, sys_l, L1567), get_var(BlockExitEnv, sys_a, A_Get1493), f_car(A_Get1493, Car_Ret1733), get_var(BlockExitEnv, sys_vs, Vs_Get1494), ElseResult1496=[Car_Ret1733|Vs_Get1494], set_var(BlockExitEnv, sys_vs, ElseResult1496), _54252=ElseResult1496), get_var(BlockExitEnv, sys_a, A_Get1497), f_cdr(A_Get1497, A), set_var(BlockExitEnv, sys_a, A), goto(do_label_6, BlockExitEnv), _TBResult=_GORES1498)),
					      
					      [ addr(addr_tagbody_6_do_label_6,
						     do_label_6,
						     '$unused',
						     BlockExitEnv,
						     (get_var(BlockExitEnv, sys_a, A_Get76), (s3q:is_endp(A_Get76)->get_var(BlockExitEnv, ;, C59_Get81), get_var(BlockExitEnv, sys_add, Add_Get83), (get_var(BlockExitEnv, do, Get_var_Ret), get_var(BlockExitEnv, sys_options, Get_var_Ret1735)), (get_var(BlockExitEnv, not, Not_Get88), get_var(BlockExitEnv, sys_those, Get_var_Ret1736)), (get_var(BlockExitEnv, sys_appear, Get_var_Ret1737), get_var(BlockExitEnv, sys_that, Get_var_Ret1738)), get_var(BlockExitEnv, sys_in, In_Get90), (get_var(BlockExitEnv, ;, C59_Get94), get_var(BlockExitEnv, sys_parameter, Parameter_Get92)), (get_var(BlockExitEnv, ;, C59_Get95), get_var(BlockExitEnv, the, The_Get91)), get_var(BlockExitEnv, list, List_Get93), (get_var(BlockExitEnv, ;, C59_Get99), get_var(BlockExitEnv, sys_as, Get_var_Ret1739)), (get_var(BlockExitEnv, ;, C59_Get100), get_var(BlockExitEnv, sys_auxiliary, Get_var_Ret1740)), (get_var(BlockExitEnv, sys_parameters, Get_var_Ret1741), get_var(BlockExitEnv, sys_paramters_c46, Get_var_Ret1742)), get_var(BlockExitEnv, sys_accumulated, Get_var_Ret1743), (get_var(BlockExitEnv, sys_are, Get_var_Ret1744), get_var(BlockExitEnv, the, The_Get101)), get_var(BlockExitEnv, sys_in, In_Get105), get_var(BlockExitEnv, sys_vs_c46, Get_var_Ret1745), (get_var(BlockExitEnv, sys_l, Get_var_Ret1746), get_var(BlockExitEnv, the, The_Get106)), get_var(BlockExitEnv, variable, Get_var_Ret1747), Nreconc_Param1610=[c38_aux|Get_var_Ret1746], get_var(BlockExitEnv, sys_keys, Get_var_Ret1748), f_mapcan(closure(kw_function, [Get_var_Param|BlockExitEnv], _76376, _76378, [sys_k],  (get_var(Get_var_Param, sys_k, K_Get), (K_Get\=[CAR1750|CDR1751]->get_var(Get_var_Param, sys_k, K_Get117), Member_Param1609=K_Get117;get_var(Get_var_Param, sys_k, K_Get118), f_car(K_Get118, Car_Ret1752), Member_Param1609=Car_Ret1752), get_var(Get_var_Param, sys_vs, Get_var_Ret1753), f_member(Member_Param1609, Get_var_Ret1753, [], IFTEST111), (IFTEST111\==[]->_76378=[];get_var(Get_var_Param, sys_k, K_Get122), ElseResult123=[K_Get122], _76378=ElseResult123)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Get_var_Ret1748, Mapcan_Ret1749), f_nreconc(Nreconc_Param1610, Mapcan_Ret1749, Nreconc_Ret1754), set_var(BlockExitEnv, sys_keys, Nreconc_Ret1754), throw(block_exit([], Nreconc_Ret1754)), _76478=ThrowResult;get_var(BlockExitEnv, ;, C59_Get130), get_var(BlockExitEnv, c38_optional, Get_var_Ret1755), get_var(BlockExitEnv, sys_a, A_Get137), (get_var(BlockExitEnv, sys_appears_c46, Get_var_Ret1756), get_var(BlockExitEnv, sys_skip, Get_var_Ret1757)), get_var(BlockExitEnv, sys_until, Get_var_Ret1758), f_car(A_Get137, Is_eq_Param), (is_eq(Is_eq_Param, c38_optional)->get_var(BlockExitEnv, sys_l, L_Get140), Set_var_Ret1759=[c38_optional|L_Get140], set_var(BlockExitEnv, sys_l, Set_var_Ret1759), get_var(BlockExitEnv, sys_a, A_Get144), f_cdr(A_Get144, Cdr_Ret1760), BlockExitEnv=[bv(sys_aa, Cdr_Ret1760), bv([sys_ov], []), bv([sys_y], [])|BlockExitEnv], catch((call_addr_block(BlockExitEnv,  (push_label(do_label_7), get_var(BlockExitEnv, sys_aa, Aa_Get457), (s3q:is_endp(Aa_Get457)->get_var(BlockExitEnv, ;, C59_Get462), get_var(BlockExitEnv, sys_add, Add_Get464), (get_var(BlockExitEnv, do, Do_Get468), get_var(BlockExitEnv, sys_options, Options_Get466)), (get_var(BlockExitEnv, not, Not_Get469), get_var(BlockExitEnv, sys_those, Those_Get465)), (get_var(BlockExitEnv, sys_appear, Appear_Get470), get_var(BlockExitEnv, sys_that, That_Get467)), get_var(BlockExitEnv, ;, C59_Get473), (get_var(BlockExitEnv, ;, C59_Get474), get_var(BlockExitEnv, sys_in, In_Get471)), (get_var(BlockExitEnv, sys_list_c46, List_c46_Get476), get_var(BlockExitEnv, the, The_Get472)), get_var(BlockExitEnv, sys_l, L_Get478), get_var(BlockExitEnv, sys_parameter, Parameter_Get475), Nreconc_Param1613=[c38_aux|L_Get478], get_var(BlockExitEnv, sys_keys, Keys_Get496), f_mapcan(closure(kw_function, [ClosureEnvironment494|BlockExitEnv], Whole495, LResult492, [sys_k],  (get_var(ClosureEnvironment494, sys_k, K_Get482), (K_Get482\=[CAR1762|CDR1763]->get_var(ClosureEnvironment494, sys_k, K_Get485), Member_Param1612=K_Get485;get_var(ClosureEnvironment494, sys_k, K_Get486), f_car(K_Get486, ElseResult488), Member_Param1612=ElseResult488), get_var(ClosureEnvironment494, sys_vs, Vs_Get489), f_member(Member_Param1612, Vs_Get489, [], IFTEST479), (IFTEST479\==[]->LResult492=[];get_var(ClosureEnvironment494, sys_k, K_Get490), ElseResult491=[K_Get490], LResult492=ElseResult491)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get496, Mapcan_Ret1761), f_nreconc(Nreconc_Param1613, Mapcan_Ret1761, Nreconc_Ret1764), set_var(BlockExitEnv, sys_keys, Nreconc_Ret1764), throw(block_exit([], [])), throw(block_exit([], RetResult460)), _TBResult146=ThrowResult461;get_var(BlockExitEnv, sys_aa, Aa_Get502), f_car(Aa_Get502, Member_Param1614), get_var(BlockExitEnv, lambda_list_keywords, Lambda_list_keywords_Get503), f_member(Member_Param1614, Lambda_list_keywords_Get503, [], IFTEST500), (IFTEST500\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get505), f_car(Aa_Get505, PredArg1Result507), (is_eq(PredArg1Result507, c38_rest)->get_var(BlockExitEnv, ;, C59_Get508), get_var(BlockExitEnv, c38_rest, C38_rest_Get510), get_var(BlockExitEnv, sys_found_c46, Found_c46_Get512), get_var(BlockExitEnv, sys_is, Is_Get511), get_var(BlockExitEnv, sys_l, L_Get513), Set_var_Ret1765=[c38_rest|L_Get513], set_var(BlockExitEnv, sys_l, Set_var_Ret1765), get_var(BlockExitEnv, sys_aa, Aa_Get514), f_cdr(Aa_Get514, Cdr_Ret1766), set_var(BlockExitEnv, sys_aa, Cdr_Ret1766), get_var(BlockExitEnv, sys_aa, Aa_Get518), f_endp(Aa_Get518, PredArgResult520), (PredArgResult520==[]->get_var(BlockExitEnv, sys_aa, Aa_Get521), f_car(Aa_Get521, Symbolp_Param1615), f_symbolp(Symbolp_Param1615, TrueResult522), IFTEST515=TrueResult522;IFTEST515=[]), (IFTEST515\==[]->_77256=[];f_sys_illegal_boa(ElseResult523), _77256=ElseResult523), get_var(BlockExitEnv, sys_aa, Aa_Get524), f_car(Aa_Get524, Car_Ret1767), get_var(BlockExitEnv, sys_vs, Vs_Get525), Set_var_Ret1768=[Car_Ret1767|Vs_Get525], set_var(BlockExitEnv, sys_vs, Set_var_Ret1768), get_var(BlockExitEnv, sys_aa, Aa_Get526), f_car(Aa_Get526, Car_Ret1769), get_var(BlockExitEnv, sys_l, L_Get527), Set_var_Ret1770=[Car_Ret1769|L_Get527], set_var(BlockExitEnv, sys_l, Set_var_Ret1770), get_var(BlockExitEnv, sys_aa, Aa_Get528), f_cdr(Aa_Get528, Cdr_Ret1771), set_var(BlockExitEnv, sys_aa, Cdr_Ret1771), get_var(BlockExitEnv, sys_aa, Aa_Get530), (s3q:is_endp(Aa_Get530)->get_var(BlockExitEnv, sys_l, L_Get533), Nreconc_Param1617=[c38_aux|L_Get533], get_var(BlockExitEnv, sys_keys, Keys_Get551), f_mapcan(closure(kw_function, [ClosureEnvironment549|BlockExitEnv], Whole550, LResult547, [sys_k],  (get_var(ClosureEnvironment549, sys_k, K_Get537), (K_Get537\=[CAR1773|CDR1774]->get_var(ClosureEnvironment549, sys_k, K_Get540), Member_Param1616=K_Get540;get_var(ClosureEnvironment549, sys_k, K_Get541), f_car(K_Get541, ElseResult543), Member_Param1616=ElseResult543), get_var(ClosureEnvironment549, sys_vs, Vs_Get544), f_member(Member_Param1616, Vs_Get544, [], IFTEST534), (IFTEST534\==[]->LResult547=[];get_var(ClosureEnvironment549, sys_k, K_Get545), ElseResult546=[K_Get545], LResult547=ElseResult546)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get551, Mapcan_Ret1772), f_nreconc(Nreconc_Param1617, Mapcan_Ret1772, Nreconc_Ret1775), set_var(BlockExitEnv, sys_keys, Nreconc_Ret1775), throw(block_exit([], [])), TrueResult555=ThrowResult553;TrueResult555=[]), _77588=TrueResult555;_77588=[]), get_var(BlockExitEnv, ;, C59_Get556), get_var(BlockExitEnv, c38_aux, C38_aux_Get558), get_var(BlockExitEnv, sys_aa, Aa_Get562), get_var(BlockExitEnv, sys_follow_c46, Follow_c46_Get560), get_var(BlockExitEnv, sys_should, Should_Get559), f_car(Aa_Get562, PredArg1Result564), (is_eq(PredArg1Result564, c38_aux)->_77674=[];f_sys_illegal_boa(ElseResult565), _77674=ElseResult565), get_var(BlockExitEnv, sys_l, L_Get566), Set_var_Ret1776=[c38_aux|L_Get566], set_var(BlockExitEnv, sys_l, Set_var_Ret1776), get_var(BlockExitEnv, sys_aa, Aa_Get570), f_cdr(Aa_Get570, Aaa_Init571), AEnv=[bv(sys_aaa, Aaa_Init571)|BlockExitEnv], catch((call_addr_block(AEnv,  (push_label(do_label_9), get_var(AEnv, sys_aaa, Aaa_Get616), (s3q:is_endp(Aaa_Get616)->throw(block_exit([], [])), _TBResult572=ThrowResult620;get_var(AEnv, sys_aaa, Aaa_Get623), f_car(Aaa_Get623, Car_Ret1777), get_var(AEnv, sys_l, L_Get624), Set_var_Ret1778=[Car_Ret1777|L_Get624], set_var(AEnv, sys_l, Set_var_Ret1778), get_var(AEnv, sys_aaa, Aaa_Get628), f_car(Aaa_Get628, PredArgResult630), (PredArgResult630\=[CAR1779|CDR1780]->get_var(AEnv, sys_aaa, Aaa_Get631), f_car(Aaa_Get631, Symbolp_Param1618), f_symbolp(Symbolp_Param1618, TrueResult632), IFTEST625=TrueResult632;IFTEST625=[]), (IFTEST625\==[]->get_var(AEnv, sys_aaa, Aaa_Get633), f_car(Aaa_Get633, Car_Ret1781), get_var(AEnv, sys_vs, Vs_Get634), TrueResult649=[Car_Ret1781|Vs_Get634], set_var(AEnv, sys_vs, TrueResult649), _77942=TrueResult649;get_var(AEnv, sys_aaa, Aaa_Get638), f_caar(Aaa_Get638, PredArgResult640), (is_symbolp(PredArgResult640)->(get_var(AEnv, sys_aaa, Aaa_Get641), f_cdar(Aaa_Get641, Endp_Param1619), f_endp(Endp_Param1619, FORM1_Res643), FORM1_Res643\==[], TrueResult644=FORM1_Res643->true;get_var(AEnv, sys_aaa, Aaa_Get642), f_cddar(Aaa_Get642, Endp_Param1620), f_endp(Endp_Param1620, Endp_Ret1782), TrueResult644=Endp_Ret1782), IFTEST635=TrueResult644;IFTEST635=[]), (IFTEST635\==[]->get_var(AEnv, sys_aaa, Aaa_Get645), f_caar(Aaa_Get645, Caar_Ret1783), get_var(AEnv, sys_vs, Vs_Get646), TrueResult647=[Caar_Ret1783|Vs_Get646], set_var(AEnv, sys_vs, TrueResult647), ElseResult650=TrueResult647;f_sys_illegal_boa(ElseResult648), ElseResult650=ElseResult648), _77942=ElseResult650), get_var(AEnv, sys_aaa, Aaa_Get651), f_cdr(Aaa_Get651, Cdr_Ret1784), set_var(AEnv, sys_aaa, Cdr_Ret1784), goto(do_label_9, AEnv), _TBResult572=_GORES652)), [addr(addr_tagbody_9_do_label_9, do_label_9, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get574), (s3q:is_endp(Aaa_Get574)->throw(block_exit([], [])), _TBResult572=ThrowResult578;get_var(AEnv, sys_aaa, Aaa_Get581), f_car(Aaa_Get581, Car_Ret1785), get_var(AEnv, sys_l, L_Get582), Set_var_Ret1786=[Car_Ret1785|L_Get582], set_var(AEnv, sys_l, Set_var_Ret1786), get_var(AEnv, sys_aaa, Aaa_Get586), f_car(Aaa_Get586, PredArgResult588), (PredArgResult588\=[CAR1787|CDR1788]->get_var(AEnv, sys_aaa, Aaa_Get589), f_car(Aaa_Get589, Symbolp_Param1621), f_symbolp(Symbolp_Param1621, TrueResult590), IFTEST583=TrueResult590;IFTEST583=[]), (IFTEST583\==[]->get_var(AEnv, sys_aaa, Aaa_Get591), f_car(Aaa_Get591, Car_Ret1789), get_var(AEnv, sys_vs, Vs_Get592), TrueResult607=[Car_Ret1789|Vs_Get592], set_var(AEnv, sys_vs, TrueResult607), _78358=TrueResult607;get_var(AEnv, sys_aaa, Aaa_Get596), f_caar(Aaa_Get596, PredArgResult598), (is_symbolp(PredArgResult598)->(get_var(AEnv, sys_aaa, Aaa_Get599), f_cdar(Aaa_Get599, Endp_Param1622), f_endp(Endp_Param1622, FORM1_Res601), FORM1_Res601\==[], TrueResult602=FORM1_Res601->true;get_var(AEnv, sys_aaa, Aaa_Get600), f_cddar(Aaa_Get600, Endp_Param1623), f_endp(Endp_Param1623, Endp_Ret1790), TrueResult602=Endp_Ret1790), IFTEST593=TrueResult602;IFTEST593=[]), (IFTEST593\==[]->get_var(AEnv, sys_aaa, Aaa_Get603), f_caar(Aaa_Get603, Caar_Ret1791), get_var(AEnv, sys_vs, Vs_Get604), TrueResult605=[Caar_Ret1791|Vs_Get604], set_var(AEnv, sys_vs, TrueResult605), ElseResult608=TrueResult605;f_sys_illegal_boa(ElseResult606), ElseResult608=ElseResult606), _78358=ElseResult608), get_var(AEnv, sys_aaa, Aaa_Get609), f_cdr(Aaa_Get609, Cdr_Ret1792), set_var(AEnv, sys_aaa, Cdr_Ret1792), goto(do_label_9, AEnv), _TBResult572=_GORES610)))]), []=LetResult568), block_exit([], LetResult568), true), get_var(BlockExitEnv, ;, C59_Get656), get_var(BlockExitEnv, sys_end, End_Get658), get_var(BlockExitEnv, sys_l, L_Get663), (get_var(BlockExitEnv, sys_keys, Keys_Get681), get_var(BlockExitEnv, sys_of, Of_Get659)), (get_var(BlockExitEnv, sys_list_c46, List_c46_Get662), get_var(BlockExitEnv, sys_parameter, Parameter_Get661)), get_var(BlockExitEnv, the, The_Get660), f_mapcan(closure(kw_function, [ClosureEnvironment679|BlockExitEnv], Whole680, LResult677, [sys_k],  (get_var(ClosureEnvironment679, sys_k, K_Get667), (K_Get667\=[CAR1794|CDR1795]->get_var(ClosureEnvironment679, sys_k, K_Get670), Member_Param1624=K_Get670;get_var(ClosureEnvironment679, sys_k, K_Get671), f_car(K_Get671, ElseResult673), Member_Param1624=ElseResult673), get_var(ClosureEnvironment679, sys_vs, Vs_Get674), f_member(Member_Param1624, Vs_Get674, [], IFTEST664), (IFTEST664\==[]->LResult677=[];get_var(ClosureEnvironment679, sys_k, K_Get675), ElseResult676=[K_Get675], LResult677=ElseResult676)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get681, Mapcan_Ret1793), f_nreconc(L_Get663, Mapcan_Ret1793, Nreconc_Ret1796), set_var(BlockExitEnv, sys_keys, Nreconc_Ret1796), throw(block_exit([], [])), _78856=ThrowResult683;_78856=[]), get_var(BlockExitEnv, ;, C59_Get685), get_var(BlockExitEnv, if, If_Get688), get_var(BlockExitEnv, sys_checks, Checks_Get687), get_var(BlockExitEnv, sys_optional, Optional_Get690), (get_var(BlockExitEnv, sys_a, A_Get693), get_var(BlockExitEnv, sys_paramter, Paramter_Get691)), (get_var(BlockExitEnv, ;, C59_Get695), get_var(BlockExitEnv, the, The_Get689)), get_var(BlockExitEnv, ;, C59_Get696), (get_var(BlockExitEnv, sys_default, Default_Get694), get_var(BlockExitEnv, sys_without, Without_Get692)), get_var(BlockExitEnv, sys_a, A_Get699), (get_var(BlockExitEnv, sys_default, Default_Get700), get_var(BlockExitEnv, sys_has, Has_Get698)), (get_var(BlockExitEnv, sys_in, In_Get702), get_var(BlockExitEnv, sys_value, Value_Get697)), get_var(BlockExitEnv, sys_slot_description_c46, Slot_description_c46_Get704), get_var(BlockExitEnv, sys_value, Value_Get701), get_var(BlockExitEnv, the, The_Get703), sf_if(BlockExitEnv, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [sys_x], [if, [consp, sys_x], ;, ;, sys_with, sys_default, sys_value_c46, [car, sys_x]]])]]], ;, ;, if, sys_no, sys_default, sys_value, sys_is, sys_supplied, sys_for, ;, ;, the, sys_optional, sys_parameter, and, sys_yet, sys_appears, ;, ;, sys_in, sys_keys, sys_with, sys_a, sys_default, sys_value_c44, ;, ;, sys_then, cons, the, sys_pair, sys_to, sys_l_c44, [setq, sys_l, [cons, [car, sys_y], sys_l]], ;, ;, otherwise, cons, sys_just, the, sys_parameter, sys_to, sys_l_c46, [setq, sys_l, [cons, [car, sys_aa], sys_l]], Sf_if_Ret1797), get_var(BlockExitEnv, ;, C59_Get705), get_var(BlockExitEnv, sys_checks, Checks_Get707), get_var(BlockExitEnv, sys_form, Form_Get709), get_var(BlockExitEnv, sys_of, Of_Get710), get_var(BlockExitEnv, sys_optional, Optional_Get712), (get_var(BlockExitEnv, sys_aa, Aa_Get715), get_var(BlockExitEnv, the, The_Get708)), get_var(BlockExitEnv, sys_parameter_c46, Parameter_c46_Get713), get_var(BlockExitEnv, the, The_Get711), f_car(Aa_Get715, PredArgResult717), (PredArgResult717\=[CAR1798|CDR1799]->get_var(BlockExitEnv, sys_aa, Aa_Get719), f_car(Aa_Get719, PredArgResult721), (is_symbolp(PredArgResult721)->_79312=[];f_sys_illegal_boa(ElseResult722), _79312=ElseResult722), get_var(BlockExitEnv, sys_aa, Aa_Get723), f_car(Aa_Get723, Car_Ret1800), get_var(BlockExitEnv, sys_vs, Vs_Get724), TrueResult756=[Car_Ret1800|Vs_Get724], set_var(BlockExitEnv, sys_vs, TrueResult756), _79372=TrueResult756;get_var(BlockExitEnv, sys_aa, Aa_Get726), f_caar(Aa_Get726, Symbolp_Param1625), f_symbolp(Symbolp_Param1625, PredArgResult728), (PredArgResult728==[]->f_sys_illegal_boa(TrueResult754), ElseResult757=TrueResult754;(get_var(BlockExitEnv, sys_aa, Aa_Get731), f_cdar(Aa_Get731, Endp_Param1626), f_endp(Endp_Param1626, FORM1_Res733), FORM1_Res733\==[], IFTEST729=FORM1_Res733->true;get_var(BlockExitEnv, sys_aa, Aa_Get732), f_cddar(Aa_Get732, Endp_Param1627), f_endp(Endp_Param1627, Endp_Ret1801), IFTEST729=Endp_Ret1801), (IFTEST729\==[]->get_var(BlockExitEnv, sys_aa, Aa_Get734), f_caar(Aa_Get734, Caar_Ret1802), get_var(BlockExitEnv, sys_vs, Vs_Get735), TrueResult752=[Caar_Ret1802|Vs_Get735], set_var(BlockExitEnv, sys_vs, TrueResult752), ElseResult755=TrueResult752;get_var(BlockExitEnv, sys_aa, Aa_Get737), f_caddar(Aa_Get737, Symbolp_Param1628), f_symbolp(Symbolp_Param1628, PredArgResult739), (PredArgResult739==[]->f_sys_illegal_boa(TrueResult750), ElseResult753=TrueResult750;get_var(BlockExitEnv, sys_aa, Aa_Get741), f_cdddar(Aa_Get741, Endp_Param1629), f_endp(Endp_Param1629, PredArgResult743), (PredArgResult743==[]->f_sys_illegal_boa(TrueResult748), ElseResult751=TrueResult748;get_var(BlockExitEnv, sys_aa, Aa_Get744), f_caar(Aa_Get744, Caar_Ret1803), get_var(BlockExitEnv, sys_vs, Vs_Get745), Set_var_Ret1804=[Caar_Ret1803|Vs_Get745], set_var(BlockExitEnv, sys_vs, Set_var_Ret1804), get_var(BlockExitEnv, sys_aa, Aa_Get746), f_caddar(Aa_Get746, Caddar_Ret1805), get_var(BlockExitEnv, sys_vs, Vs_Get747), ElseResult749=[Caddar_Ret1805|Vs_Get747], set_var(BlockExitEnv, sys_vs, ElseResult749), ElseResult751=ElseResult749), ElseResult753=ElseResult751), ElseResult755=ElseResult753), ElseResult757=ElseResult755), _79372=ElseResult757), get_var(BlockExitEnv, sys_aa, Aa_Get758), f_cdr(Aa_Get758, Cdr_Ret1806), set_var(BlockExitEnv, sys_aa, Cdr_Ret1806), goto(do_label_7, BlockExitEnv), _TBResult146=_GORES759)), [addr(addr_tagbody_7_do_label_7, do_label_7, '$unused', AEnv,  (get_var(AEnv, sys_aa, Aa_Get), (s3q:is_endp(Aa_Get)->get_var(AEnv, ;, C59_Get153), get_var(AEnv, sys_add, Add_Get155), (get_var(AEnv, do, Do_Get159), get_var(AEnv, sys_options, Options_Get157)), (get_var(AEnv, not, Not_Get160), get_var(AEnv, sys_those, Those_Get156)), (get_var(AEnv, sys_appear, Appear_Get161), get_var(AEnv, sys_that, That_Get158)), get_var(AEnv, ;, C59_Get164), (get_var(AEnv, ;, C59_Get165), get_var(AEnv, sys_in, In_Get162)), (get_var(AEnv, sys_list_c46, Get_var_Ret1807), get_var(AEnv, the, The_Get163)), get_var(AEnv, sys_l, L_Get169), get_var(AEnv, sys_parameter, Parameter_Get166), Nreconc_Param1631=[c38_aux|L_Get169], get_var(AEnv, sys_keys, Keys_Get187), f_mapcan(closure(kw_function, [ClosureEnvironment185|AEnv], Whole186, LResult183, [sys_k],  (get_var(ClosureEnvironment185, sys_k, K_Get173), (K_Get173\=[CAR1809|CDR1810]->get_var(ClosureEnvironment185, sys_k, K_Get176), Member_Param1630=K_Get176;get_var(ClosureEnvironment185, sys_k, K_Get177), f_car(K_Get177, ElseResult179), Member_Param1630=ElseResult179), get_var(ClosureEnvironment185, sys_vs, Vs_Get180), f_member(Member_Param1630, Vs_Get180, [], IFTEST170), (IFTEST170\==[]->LResult183=[];get_var(ClosureEnvironment185, sys_k, K_Get181), ElseResult182=[K_Get181], LResult183=ElseResult182)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get187, Mapcan_Ret1808), f_nreconc(Nreconc_Param1631, Mapcan_Ret1808, Nreconc_Ret1811), set_var(AEnv, sys_keys, Nreconc_Ret1811), throw(block_exit([], [])), throw(block_exit([], RetResult151)), _TBResult146=ThrowResult152;get_var(AEnv, sys_aa, Aa_Get193), f_car(Aa_Get193, Member_Param1632), get_var(AEnv, lambda_list_keywords, Get_var_Ret1812), f_member(Member_Param1632, Get_var_Ret1812, [], IFTEST191), (IFTEST191\==[]->get_var(AEnv, sys_aa, Aa_Get196), f_car(Aa_Get196, PredArg1Result198), (is_eq(PredArg1Result198, c38_rest)->get_var(AEnv, ;, C59_Get199), get_var(AEnv, c38_rest, Get_var_Ret1813), get_var(AEnv, sys_found_c46, Get_var_Ret1814), get_var(AEnv, sys_is, Is_Get202), get_var(AEnv, sys_l, L_Get205), Set_var_Ret1815=[c38_rest|L_Get205], set_var(AEnv, sys_l, Set_var_Ret1815), get_var(AEnv, sys_aa, Aa_Get206), f_cdr(Aa_Get206, Cdr_Ret1816), set_var(AEnv, sys_aa, Cdr_Ret1816), get_var(AEnv, sys_aa, Aa_Get210), f_endp(Aa_Get210, PredArgResult212), (PredArgResult212==[]->get_var(AEnv, sys_aa, Aa_Get213), f_car(Aa_Get213, Symbolp_Param1633), f_symbolp(Symbolp_Param1633, TrueResult214), IFTEST207=TrueResult214;IFTEST207=[]), (IFTEST207\==[]->_80418=[];f_sys_illegal_boa(ElseResult215), _80418=ElseResult215), get_var(AEnv, sys_aa, Aa_Get216), f_car(Aa_Get216, Car_Ret1817), get_var(AEnv, sys_vs, Vs_Get217), Set_var_Ret1818=[Car_Ret1817|Vs_Get217], set_var(AEnv, sys_vs, Set_var_Ret1818), get_var(AEnv, sys_aa, Aa_Get218), f_car(Aa_Get218, Car_Ret1819), get_var(AEnv, sys_l, L_Get219), Set_var_Ret1820=[Car_Ret1819|L_Get219], set_var(AEnv, sys_l, Set_var_Ret1820), get_var(AEnv, sys_aa, Aa_Get220), f_cdr(Aa_Get220, Cdr_Ret1821), set_var(AEnv, sys_aa, Cdr_Ret1821), get_var(AEnv, sys_aa, Aa_Get222), (s3q:is_endp(Aa_Get222)->get_var(AEnv, sys_l, L_Get225), Nreconc_Param1635=[c38_aux|L_Get225], get_var(AEnv, sys_keys, Keys_Get243), f_mapcan(closure(kw_function, [ClosureEnvironment241|AEnv], Whole242, LResult239, [sys_k],  (get_var(ClosureEnvironment241, sys_k, K_Get229), (K_Get229\=[CAR1823|CDR1824]->get_var(ClosureEnvironment241, sys_k, K_Get232), Member_Param1634=K_Get232;get_var(ClosureEnvironment241, sys_k, K_Get233), f_car(K_Get233, ElseResult235), Member_Param1634=ElseResult235), get_var(ClosureEnvironment241, sys_vs, Vs_Get236), f_member(Member_Param1634, Vs_Get236, [], IFTEST226), (IFTEST226\==[]->LResult239=[];get_var(ClosureEnvironment241, sys_k, K_Get237), ElseResult238=[K_Get237], LResult239=ElseResult238)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get243, Mapcan_Ret1822), f_nreconc(Nreconc_Param1635, Mapcan_Ret1822, Nreconc_Ret1825), set_var(AEnv, sys_keys, Nreconc_Ret1825), throw(block_exit([], [])), TrueResult247=ThrowResult245;TrueResult247=[]), _80750=TrueResult247;_80750=[]), get_var(AEnv, ;, C59_Get248), get_var(AEnv, c38_aux, Get_var_Ret1826), get_var(AEnv, sys_aa, Aa_Get254), get_var(AEnv, sys_follow_c46, Get_var_Ret1827), get_var(AEnv, sys_should, Get_var_Ret1828), f_car(Aa_Get254, PredArg1Result256), (is_eq(PredArg1Result256, c38_aux)->_80800=[];f_sys_illegal_boa(ElseResult257), _80800=ElseResult257), get_var(AEnv, sys_l, L_Get258), Set_var_Ret1829=[c38_aux|L_Get258], set_var(AEnv, sys_l, Set_var_Ret1829), get_var(AEnv, sys_aa, Aa_Get262), f_cdr(Aa_Get262, Cdr_Ret1830), AEnv=[bv(sys_aaa, Cdr_Ret1830)|AEnv], catch((call_addr_block(AEnv,  (push_label(do_label_8), get_var(AEnv, sys_aaa, Aaa_Get308), (s3q:is_endp(Aaa_Get308)->throw(block_exit([], [])), _TBResult264=ThrowResult312;get_var(AEnv, sys_aaa, Aaa_Get315), f_car(Aaa_Get315, Car_Ret1831), get_var(AEnv, sys_l, L_Get316), Set_var_Ret1832=[Car_Ret1831|L_Get316], set_var(AEnv, sys_l, Set_var_Ret1832), get_var(AEnv, sys_aaa, Aaa_Get320), f_car(Aaa_Get320, PredArgResult322), (PredArgResult322\=[CAR1833|CDR1834]->get_var(AEnv, sys_aaa, Aaa_Get323), f_car(Aaa_Get323, Symbolp_Param1636), f_symbolp(Symbolp_Param1636, TrueResult324), IFTEST317=TrueResult324;IFTEST317=[]), (IFTEST317\==[]->get_var(AEnv, sys_aaa, Aaa_Get325), f_car(Aaa_Get325, Car_Ret1835), get_var(AEnv, sys_vs, Vs_Get326), TrueResult341=[Car_Ret1835|Vs_Get326], set_var(AEnv, sys_vs, TrueResult341), _81056=TrueResult341;get_var(AEnv, sys_aaa, Aaa_Get330), f_caar(Aaa_Get330, PredArgResult332), (is_symbolp(PredArgResult332)->(get_var(AEnv, sys_aaa, Aaa_Get333), f_cdar(Aaa_Get333, Endp_Param1637), f_endp(Endp_Param1637, FORM1_Res335), FORM1_Res335\==[], TrueResult336=FORM1_Res335->true;get_var(AEnv, sys_aaa, Aaa_Get334), f_cddar(Aaa_Get334, Endp_Param1638), f_endp(Endp_Param1638, Endp_Ret1836), TrueResult336=Endp_Ret1836), IFTEST327=TrueResult336;IFTEST327=[]), (IFTEST327\==[]->get_var(AEnv, sys_aaa, Aaa_Get337), f_caar(Aaa_Get337, Caar_Ret1837), get_var(AEnv, sys_vs, Vs_Get338), TrueResult339=[Caar_Ret1837|Vs_Get338], set_var(AEnv, sys_vs, TrueResult339), ElseResult342=TrueResult339;f_sys_illegal_boa(ElseResult340), ElseResult342=ElseResult340), _81056=ElseResult342), get_var(AEnv, sys_aaa, Aaa_Get343), f_cdr(Aaa_Get343, Cdr_Ret1838), set_var(AEnv, sys_aaa, Cdr_Ret1838), goto(do_label_8, AEnv), _TBResult264=_GORES344)), [addr(addr_tagbody_8_do_label_8, do_label_8, '$unused', AEnv,  (get_var(AEnv, sys_aaa, Aaa_Get), (s3q:is_endp(Aaa_Get)->throw(block_exit([], [])), _TBResult264=ThrowResult270;get_var(AEnv, sys_aaa, Aaa_Get273), f_car(Aaa_Get273, Car_Ret1839), get_var(AEnv, sys_l, L_Get274), Set_var_Ret1840=[Car_Ret1839|L_Get274], set_var(AEnv, sys_l, Set_var_Ret1840), get_var(AEnv, sys_aaa, Aaa_Get278), f_car(Aaa_Get278, PredArgResult280), (PredArgResult280\=[CAR1841|CDR1842]->get_var(AEnv, sys_aaa, Aaa_Get281), f_car(Aaa_Get281, Symbolp_Param1639), f_symbolp(Symbolp_Param1639, TrueResult282), IFTEST275=TrueResult282;IFTEST275=[]), (IFTEST275\==[]->get_var(AEnv, sys_aaa, Aaa_Get283), f_car(Aaa_Get283, Car_Ret1843), get_var(AEnv, sys_vs, Vs_Get284), TrueResult299=[Car_Ret1843|Vs_Get284], set_var(AEnv, sys_vs, TrueResult299), _81472=TrueResult299;get_var(AEnv, sys_aaa, Aaa_Get288), f_caar(Aaa_Get288, PredArgResult290), (is_symbolp(PredArgResult290)->(get_var(AEnv, sys_aaa, Aaa_Get291), f_cdar(Aaa_Get291, Endp_Param1640), f_endp(Endp_Param1640, Endp_Ret1844), Endp_Ret1844\==[], TrueResult294=Endp_Ret1844->true;get_var(AEnv, sys_aaa, Aaa_Get292), f_cddar(Aaa_Get292, Endp_Param1641), f_endp(Endp_Param1641, Endp_Ret1845), TrueResult294=Endp_Ret1845), IFTEST285=TrueResult294;IFTEST285=[]), (IFTEST285\==[]->get_var(AEnv, sys_aaa, Aaa_Get295), f_caar(Aaa_Get295, Caar_Ret1846), get_var(AEnv, sys_vs, Vs_Get296), TrueResult297=[Caar_Ret1846|Vs_Get296], set_var(AEnv, sys_vs, TrueResult297), ElseResult300=TrueResult297;f_sys_illegal_boa(ElseResult298), ElseResult300=ElseResult298), _81472=ElseResult300), get_var(AEnv, sys_aaa, Aaa_Get301), f_cdr(Aaa_Get301, Cdr_Ret1847), set_var(AEnv, sys_aaa, Cdr_Ret1847), goto(do_label_8, AEnv), _TBResult264=_GORES)))]), []=LetResult260), block_exit([], LetResult260), true), get_var(AEnv, ;, C59_Get348), get_var(AEnv, sys_end, Get_var_Ret1848), get_var(AEnv, sys_l, L_Get355), (get_var(AEnv, sys_keys, Keys_Get373), get_var(AEnv, sys_of, Get_var_Ret1849)), (get_var(AEnv, sys_list_c46, List_c46_Get354), get_var(AEnv, sys_parameter, Parameter_Get353)), get_var(AEnv, the, The_Get352), f_mapcan(closure(kw_function, [ClosureEnvironment371|AEnv], Whole372, LResult369, [sys_k],  (get_var(ClosureEnvironment371, sys_k, K_Get359), (K_Get359\=[CAR1851|CDR1852]->get_var(ClosureEnvironment371, sys_k, K_Get362), Member_Param1642=K_Get362;get_var(ClosureEnvironment371, sys_k, K_Get363), f_car(K_Get363, ElseResult365), Member_Param1642=ElseResult365), get_var(ClosureEnvironment371, sys_vs, Vs_Get366), f_member(Member_Param1642, Vs_Get366, [], IFTEST356), (IFTEST356\==[]->LResult369=[];get_var(ClosureEnvironment371, sys_k, K_Get367), ElseResult368=[K_Get367], LResult369=ElseResult368)), [lambda, [sys_k], [if, [member, [if, [atom, sys_k], sys_k, [car, sys_k]], sys_vs], [], [list, sys_k]]]), Keys_Get373, Mapcan_Ret1850), f_nreconc(L_Get355, Mapcan_Ret1850, Nreconc_Ret1853), set_var(AEnv, sys_keys, Nreconc_Ret1853), throw(block_exit([], [])), _81934=ThrowResult375;_81934=[]), get_var(AEnv, ;, C59_Get377), get_var(AEnv, if, If_Get380), get_var(AEnv, sys_checks, Get_var_Ret1854), get_var(AEnv, sys_optional, Optional_Get382), (get_var(AEnv, sys_a, A_Get385), get_var(AEnv, sys_paramter, Get_var_Ret1855)), (get_var(AEnv, ;, C59_Get387), get_var(AEnv, the, The_Get381)), get_var(AEnv, ;, C59_Get388), (get_var(AEnv, sys_default, Default_Get386), get_var(AEnv, sys_without, Get_var_Ret1856)), get_var(AEnv, sys_a, A_Get391), (get_var(AEnv, sys_default, Default_Get392), get_var(AEnv, sys_has, Get_var_Ret1857)), (get_var(AEnv, sys_in, In_Get394), get_var(AEnv, sys_value, Value_Get389)), get_var(AEnv, sys_slot_description_c46, Get_var_Ret1858), get_var(AEnv, sys_value, Value_Get393), get_var(AEnv, the, The_Get395), sf_if(AEnv, [and, [cond, [[atom, [car, sys_aa]], [setq, sys_ov, [car, sys_aa]], t], [[endp, [cdar, sys_aa]], [setq, sys_ov, [caar, sys_aa]], t], [t, []]], [setq, sys_y, [member, sys_ov, sys_keys, kw_key, function([lambda, [sys_x], [if, [consp, sys_x], ;, ;, sys_with, sys_default, sys_value_c46, [car, sys_x]]])]]], ;, ;, if, sys_no, sys_default, sys_value, sys_is, sys_supplied, sys_for, ;, ;, the, sys_optional, sys_parameter, and, sys_yet, sys_appears, ;, ;, sys_in, sys_keys, sys_with, sys_a, sys_default, sys_value_c44, ;, ;, sys_then, cons, the, sys_pair, sys_to, sys_l_c44, [setq, sys_l, [cons, [car, sys_y], sys_l]], ;, ;, otherwise, cons, sys_just, the, sys_parameter, sys_to, sys_l_c46, [setq, sys_l, [cons, [car, sys_aa], sys_l]], Sf_if_Ret1859), get_var(AEnv, ;, C59_Get397), get_var(AEnv, sys_checks, Checks_Get399), get_var(AEnv, sys_form, Get_var_Ret1860), get_var(AEnv, sys_of, Of_Get402), get_var(AEnv, sys_optional, Optional_Get404), (get_var(AEnv, sys_aa, Aa_Get407), get_var(AEnv, the, The_Get400)), get_var(AEnv, sys_parameter_c46, Get_var_Ret1861), get_var(AEnv, the, The_Get403), f_car(Aa_Get407, PredArgResult409), (PredArgResult409\=[CAR1862|CDR1863]->get_var(AEnv, sys_aa, Aa_Get411), f_car(Aa_Get411, PredArgResult413), (is_symbolp(PredArgResult413)->_82306=[];f_sys_illegal_boa(ElseResult414), _82306=ElseResult414), get_var(AEnv, sys_aa, Aa_Get415), f_car(Aa_Get415, Car_Ret1864), get_var(AEnv, sys_vs, Vs_Get416), TrueResult448=[Car_Ret1864|Vs_Get416], set_var(AEnv, sys_vs, TrueResult448), _82366=TrueResult448;get_var(AEnv, sys_aa, Aa_Get418), f_caar(Aa_Get418, Symbolp_Param1643), f_symbolp(Symbolp_Param1643, PredArgResult420), (PredArgResult420==[]->f_sys_illegal_boa(TrueResult446), ElseResult449=TrueResult446;(get_var(AEnv, sys_aa, Aa_Get423), f_cdar(Aa_Get423, Endp_Param1644), f_endp(Endp_Param1644, FORM1_Res425), FORM1_Res425\==[], IFTEST421=FORM1_Res425->true;get_var(AEnv, sys_aa, Aa_Get424), f_cddar(Aa_Get424, Endp_Param1645), f_endp(Endp_Param1645, Endp_Ret1865), IFTEST421=Endp_Ret1865), (IFTEST421\==[]->get_var(AEnv, sys_aa, Aa_Get426), f_caar(Aa_Get426, Caar_Ret1866), get_var(AEnv, sys_vs, Vs_Get427), TrueResult444=[Caar_Ret1866|Vs_Get427], set_var(AEnv, sys_vs, TrueResult444), ElseResult447=TrueResult444;get_var(AEnv, sys_aa, Aa_Get429), f_caddar(Aa_Get429, Symbolp_Param1646), f_symbolp(Symbolp_Param1646, PredArgResult431), (PredArgResult431==[]->f_sys_illegal_boa(TrueResult442), ElseResult445=TrueResult442;get_var(AEnv, sys_aa, Aa_Get433), f_cdddar(Aa_Get433, Endp_Param1647), f_endp(Endp_Param1647, PredArgResult435), (PredArgResult435==[]->f_sys_illegal_boa(TrueResult440), ElseResult443=TrueResult440;get_var(AEnv, sys_aa, Aa_Get436), f_caar(Aa_Get436, Caar_Ret1867), get_var(AEnv, sys_vs, Vs_Get437), Set_var_Ret1868=[Caar_Ret1867|Vs_Get437], set_var(AEnv, sys_vs, Set_var_Ret1868), get_var(AEnv, sys_aa, Aa_Get438), f_caddar(Aa_Get438, Caddar_Ret1869), get_var(AEnv, sys_vs, Vs_Get439), ElseResult441=[Caddar_Ret1869|Vs_Get439], set_var(AEnv, sys_vs, ElseResult441), ElseResult443=ElseResult441), ElseResult445=ElseResult443), ElseResult447=ElseResult445), ElseResult449=ElseResult447), _82366=ElseResult449), get_var(AEnv, sys_aa, Aa_Get450), f_cdr(Aa_Get450, Cdr_Ret1870), set_var(AEnv, sys_aa, Cdr_Ret1870), goto(do_label_7, AEnv), _TBResult146=_GORES451)))]), []=LetResult142), block_exit([], LetResult142), true), get_var(BlockExitEnv, ;, C59_Get763), set_var(BlockExitEnv, 'block_ret_[]', []), always('block_exit_[]', BlockExitEnv), _82796=ThrowResult771;get_var(BlockExitEnv, sys_a, A_Get773), f_car(A_Get773, PredArgResult775), (is_symbolp(PredArgResult775)->_82840=[];f_sys_illegal_boa(ElseResult776), _82840=ElseResult776), get_var(BlockExitEnv, sys_a, A_Get777), f_car(A_Get777, Car_Ret1871), get_var(BlockExitEnv, sys_l, L_Get778), Set_var_Ret1872=[Car_Ret1871|L_Get778], set_var(BlockExitEnv, sys_l, Set_var_Ret1872), get_var(BlockExitEnv, sys_a, A_Get779), f_car(A_Get779, Car_Ret1873), get_var(BlockExitEnv, sys_vs, Vs_Get780), ElseResult782=[Car_Ret1873|Vs_Get780], set_var(BlockExitEnv, sys_vs, ElseResult782), _82796=ElseResult782), get_var(BlockExitEnv, sys_a, A_Get783), f_cdr(A_Get783, Cdr_Ret1874), set_var(BlockExitEnv, sys_a, Cdr_Ret1874), goto(do_label_6, BlockExitEnv), _76478=_GORES784)))
					      ]),
			      []=LetResult70
			    ),
			    block_exit([], LetResult70),
			    true),
		      get_var(LEnv, sys_constructor, Constructor_Get1503),
		      f_car(Constructor_Get1503, TrueResult1515),
		      set_var(LEnv, sys_constructor, TrueResult1515),
		      _32228=TrueResult1515
		  ;   get_var(LEnv, ;, C59_Get1504),
		      get_var(LEnv, if, If_Get1506),
		      get_var(LEnv, not, Not_Get1507),
		      ( get_var(LEnv, cons, Cons_Get),
			get_var(LEnv, sys_a, A_Get1508)
		      ),
		      get_var(LEnv, sys_boa, Boa_Get1509),
		      get_var(LEnv, sys_c38_key_c46, C38_key_c46_Get),
		      get_var(LEnv, sys_constructor_c44, Constructor_c44_Get),
		      get_var(LEnv, sys_just, Just_Get),
		      get_var(LEnv, sys_keys, Keys_Get1514),
		      ElseResult1516=[c38_key|Keys_Get1514],
		      set_var(LEnv, sys_keys, ElseResult1516),
		      _32228=ElseResult1516
		  ),
		  get_var(LEnv, type, IFTEST1517),
		  (   IFTEST1517==[]
		  ->  get_var(LEnv, sys_constructor, Constructor_Get1520),
		      get_var(LEnv, sys_keys, Keys_Get1521),
		      get_var(LEnv, sys_name, Name_Get),
		      get_var(LEnv, sys_slot_names, Slot_names_Get),
		      LetResult=[defun, Constructor_Get1520, Keys_Get1521, [sys_make_structure, [quote, Name_Get]|Slot_names_Get]]
		  ;   (   get_var(LEnv, type, Type_Get1526),
			  f_eq(Type_Get1526, vector, FORM1_Res1533),
			  FORM1_Res1533\==[],
			  IFTEST1524=FORM1_Res1533
		      ->  true
		      ;   get_var(LEnv, type, Type_Get1528),
			  (   c0nz:is_consp(Type_Get1528)
			  ->  get_var(LEnv, type, Type_Get1531),
			      f_car(Type_Get1531, Eq_Param),
			      f_eq(Eq_Param, vector, TrueResult1532),
			      _83544=TrueResult1532
			  ;   _83544=[]
			  ),
			  IFTEST1524=_83544
		      ),
		      (   IFTEST1524\==[]
		      ->  get_var(LEnv, sys_constructor, Constructor_Get1534),
			  get_var(LEnv, sys_keys, Keys_Get1535),
			  get_var(LEnv, sys_slot_names, Slot_names_Get1536),
			  ElseResult1550=[defun, Constructor_Get1534, Keys_Get1535, [vector|Slot_names_Get1536]]
		      ;   get_var(LEnv, type, Type_Get1538),
			  (   is_eq(Type_Get1538, list)
			  ->  get_var(LEnv,
				      sys_constructor,
				      Constructor_Get1541),
			      get_var(LEnv, sys_keys, Keys_Get1542),
			      get_var(LEnv, sys_slot_names, Slot_names_Get1543),
			      ElseResult1549=[defun, Constructor_Get1541, Keys_Get1542, [list|Slot_names_Get1543]]
			  ;   get_var(LEnv, type, Type_Get1546),
			      f_error(
				      [ '$ARRAY'([*],
						 claz_base_character,
						 "~S is an illegal structure type"),
					Type_Get1546
				      ],
				      IFTEST1544),
			      (   IFTEST1544\==[]
			      ->  ElseResult1548=[]
			      ;   ElseResult1547=[],
				  ElseResult1548=ElseResult1547
			      ),
			      ElseResult1549=ElseResult1548
			  ),
			  ElseResult1550=ElseResult1549
		      ),
		      LetResult=ElseResult1550
		  )
		),
		LetResult=FnResult
	      ),
	      block_exit(sys_make_constructor, FnResult),
	      true).
:- set_opv(sys_make_constructor, symbol_function, f_sys_make_constructor),
   DefunResult=sys_make_constructor.
/*
:- side_effect(assert_lsp(sys_make_constructor,
			  lambda_def(defun,
				     sys_make_constructor,
				     f_sys_make_constructor,
				     
				     [ sys_name,
				       sys_constructor,
				       type,
				       sys_named,
				       sys_slot_descriptions
				     ],
				     
				     [ [declare, [ignore, sys_named]],
				       
				       [ let,
					 
					 [ 
					   [ sys_slot_names,
					     (;),
					     (;),
					     sys_collect,
					     the,
					     sys_slot_names_c46,
					     
					     [ mapcar,
					       function(
							[ lambda,
							  [sys_x],
							  
							  [ cond,
							    
							    [ [null, sys_x],
							      (;),
							      (;),
							      if,
							      the,
							      sys_slot_description,
							      sys_is,
							      sys_nil_c44,
							      (;),
							      (;),
							      sys_it,
							      sys_is,
							      sys_in,
							      the,
							      sys_padding,
							      sys_of,
							      sys_initial_offset_c46,
							      []
							    ],
							    
							    [ [null, [car, sys_x]],
							      (;),
							      (;),
							      if,
							      the,
							      sys_slot,
							      sys_name,
							      sys_is,
							      sys_nil_c44,
							      (;),
							      (;),
							      sys_it,
							      sys_is,
							      the,
							      structure,
							      sys_name_c46,
							      (;),
							      (;),
							      sys_this,
							      sys_is,
							      sys_for,
							      sys_typed,
							      sys_structures,
							      sys_with,
							      sys_names_c46,
							      
							      [ list,
								[quote, quote],
								[cadr, sys_x]
							      ]
							    ],
							    [t, [car, sys_x]]
							  ]
							]),
					       sys_slot_descriptions
					     ]
					   ],
					   
					   [ sys_keys,
					     (;),
					     (;),
					     sys_make,
					     the,
					     keyword,
					     sys_parameters_c46,
					     
					     [ mapcan,
					       function(
							[ lambda,
							  [sys_x],
							  
							  [ cond,
							    [[null, sys_x], []],
							    
							    [ [null, [car, sys_x]],
							      []
							    ],
							    
							    [ [null, [cadr, sys_x]],
							      [list, [car, sys_x]]
							    ],
							    
							    [ t,
							      
							      [ list,
								
								[ list,
								  [car, sys_x],
								  [cadr, sys_x]
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
					     (;),
					     (;),
					     the,
					     case,
					     sys_for,
					     sys_a,
					     sys_boa,
					     sys_constructor_c46,
					     (;),
					     (;),
					     sys_dirty,
					     sys_code_c33_c33,
					     (;),
					     (;),
					     sys_we,
					     sys_must,
					     sys_add,
					     sys_an,
					     sys_initial,
					     sys_value,
					     sys_for,
					     sys_an,
					     sys_optional,
					     sys_parameter_c44,
					     (;),
					     (;),
					     if,
					     the,
					     sys_default,
					     sys_value,
					     sys_is,
					     not,
					     sys_specified,
					     (;),
					     (;),
					     sys_in,
					     the,
					     sys_given,
					     sys_parameter,
					     list,
					     and,
					     sys_yet,
					     the,
					     sys_initial,
					     sys_value,
					     (;),
					     (;),
					     sys_is,
					     sys_supplied,
					     sys_in,
					     the,
					     sys_slot,
					     sys_description_c46,
					     
					     [ do,
					       
					       [ 
						 [ sys_a,
						   [cadr, sys_constructor],
						   [cdr, sys_a]
						 ],
						 [sys_l, []],
						 [sys_vs, []]
					       ],
					       
					       [ [endp, sys_a],
						 (;),
						 (;),
						 sys_add,
						 sys_those,
						 sys_options,
						 sys_that,
						 do,
						 not,
						 sys_appear,
						 sys_in,
						 the,
						 sys_parameter,
						 list,
						 (;),
						 (;),
						 sys_as,
						 sys_auxiliary,
						 sys_paramters_c46,
						 (;),
						 (;),
						 the,
						 sys_parameters,
						 sys_are,
						 sys_accumulated,
						 sys_in,
						 the,
						 variable,
						 sys_vs_c46,
						 
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
					       (;),
					       (;),
					       sys_skip,
					       sys_until,
					       c38_optional,
					       sys_appears_c46,
					       
					       [ cond,
						 
						 [ 
						   [ eq,
						     [car, sys_a],
						     [quote, c38_optional]
						   ],
						   
						   [ setq,
						     sys_l,
						     
						     [ cons,
						       [quote, c38_optional],
						       sys_l
						     ]
						   ],
						   
						   [ do,
						     
						     [ 
						       [ sys_aa,
							 [cdr, sys_a],
							 [cdr, sys_aa]
						       ],
						       [sys_ov],
						       [sys_y]
						     ],
						     
						     [ [endp, sys_aa],
						       (;),
						       (;),
						       sys_add,
						       sys_those,
						       sys_options,
						       sys_that,
						       do,
						       not,
						       sys_appear,
						       sys_in,
						       the,
						       (;),
						       (;),
						       sys_parameter,
						       sys_list_c46,
						       
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
							 (;),
							 (;),
							 c38_rest,
							 sys_is,
							 sys_found_c46,
							 
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
						       (;),
						       (;),
						       c38_aux,
						       sys_should,
						       sys_follow_c46,
						       
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
						       (;),
						       (;),
						       sys_end,
						       sys_of,
						       the,
						       sys_parameter,
						       sys_list_c46,
						       
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
						     (;),
						     (;),
						     sys_checks,
						     if,
						     the,
						     sys_optional,
						     sys_paramter,
						     sys_without,
						     sys_a,
						     sys_default,
						     (;),
						     (;),
						     sys_value,
						     sys_has,
						     sys_a,
						     sys_default,
						     sys_value,
						     sys_in,
						     the,
						     sys_slot_description_c46,
						     
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
									[sys_x],
									
									[ if,
									  [consp, sys_x],
									  (;),
									  (;),
									  sys_with,
									  sys_default,
									  sys_value_c46,
									  [car, sys_x]
									]
								      ])
							   ]
							 ]
						       ],
						       (;),
						       (;),
						       if,
						       sys_no,
						       sys_default,
						       sys_value,
						       sys_is,
						       sys_supplied,
						       sys_for,
						       (;),
						       (;),
						       the,
						       sys_optional,
						       sys_parameter,
						       and,
						       sys_yet,
						       sys_appears,
						       (;),
						       (;),
						       sys_in,
						       sys_keys,
						       sys_with,
						       sys_a,
						       sys_default,
						       sys_value_c44,
						       (;),
						       (;),
						       sys_then,
						       cons,
						       the,
						       sys_pair,
						       sys_to,
						       sys_l_c44,
						       
						       [ setq,
							 sys_l,
							 
							 [ cons,
							   [car, sys_y],
							   sys_l
							 ]
						       ],
						       (;),
						       (;),
						       otherwise,
						       cons,
						       sys_just,
						       the,
						       sys_parameter,
						       sys_to,
						       sys_l_c46,
						       
						       [ setq,
							 sys_l,
							 
							 [ cons,
							   [car, sys_aa],
							   sys_l
							 ]
						       ]
						     ],
						     (;),
						     (;),
						     sys_checks,
						     the,
						     sys_form,
						     sys_of,
						     the,
						     sys_optional,
						     sys_parameter_c46,
						     
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
						   (;),
						   (;),
						   return,
						   sys_from,
						   the,
						   sys_outside,
						   sys_do_c46,
						   [return, []]
						 ],
						 
						 [ t,
						   
						   [ unless,
						     [symbolp, [car, sys_a]],
						     [sys_illegal_boa]
						   ],
						   
						   [ setq,
						     sys_l,
						     [cons, [car, sys_a], sys_l]
						   ],
						   
						   [ setq,
						     sys_vs,
						     [cons, [car, sys_a], sys_vs]
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
					     (;),
					     (;),
					     if,
					     not,
					     sys_a,
					     sys_boa,
					     sys_constructor_c44,
					     sys_just,
					     cons,
					     sys_c38_key_c46,
					     
					     [ setq,
					       sys_keys,
					       [cons, [quote, c38_key], sys_keys]
					     ]
					   ]
					 ],
					 
					 [ cond,
					   
					   [ [null, type],
					     
					     [ '#BQ',
					       
					       [ defun,
						 ['#COMMA', sys_constructor],
						 ['#COMMA', sys_keys],
						 
						 [ sys_make_structure,
						   [quote, ['#COMMA', sys_name]],
						   
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
						 ['#COMMA', sys_constructor],
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
						 ['#COMMA', sys_constructor],
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
				     ]))).
*/
/*
:- side_effect(assert_lsp(sys_make_constructor,
			  arglist_info(sys_make_constructor,
				       f_sys_make_constructor,
				       
				       [ sys_name,
					 sys_constructor,
					 type,
					 sys_named,
					 sys_slot_descriptions
				       ],
				       arginfo{ all:
						    [ sys_name,
						      sys_constructor,
						      type,
						      sys_named,
						      sys_slot_descriptions
						    ],
						allow_other_keys:0,
						aux:0,
						body:0,
						complex:0,
						env:0,
						key:0,
						names:
						      [ sys_name,
							sys_constructor,
							type,
							sys_named,
							sys_slot_descriptions
						      ],
						opt:0,
						req:
						    [ sys_name,
						      sys_constructor,
						      type,
						      sys_named,
						      sys_slot_descriptions
						    ],
						rest:0,
						sublists:0,
						whole:0
					      }))).
*/
/*
:- side_effect(assert_lsp(sys_make_constructor,
			  init_args(x, f_sys_make_constructor))).
*/


%; Total compilation time: 41.514 seconds

