;;; CMPSYSFUN   Database for system functions.
;;;
;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;    Copying of this file is authorized to users who have executed the true
;;;    and proper "License Agreement for ECoLisp".

;;;
;;; For each system function it provides:
;;;
;;; 	name		of corresponding C function
;;;	argtypes	(list of types of arguments, * means optionals)
;;;	return-type
;;;	never-change-special-var-p
;;;	predicate
;;;	optimizers
;;;
;;; An optimizer is a property list:
;;;	{ property inline-info }*
;;;
;;; Valid property names are:
;;;  :INLINE-ALWAYS
;;;  :INLINE-SAFE	safe-compile only
;;;  :INLINE-UNSAFE	non-safe-compile only
;;;
;;; An inline-info is:
;;; ( types { type | boolean } side-effect new-object { string | function } ).
;;; string is:
;;; [@i..;]Cexpr(#0, ..., #n)
;;; where #0 indicates the first argument. The optional @i indicates an
;;; argument to be saved into a variable before evaluating Cexpr.

;;; The following flag:
;;;	side-effect-p
;;; which is repeated in each optimizer, could be supplied just once
;;; per function, while
;;;	allocates-new-storage
;;; could be just eliminated since we use a conservative GC.

(in-package 'compiler)

(defun defsysfun (fname cname-string arg-types return-type
                        never-change-special-var-p predicate
			&rest optimizers)
  ;; The value NIL for each parameter except for fname means "not known".
  ;; optimizers is a list of alternating {safety inline-info}* as above.
  (when cname-string (setf (get fname 'Lfun) cname-string))
  (when arg-types
    (setf (get fname 'arg-types)
	  (mapcar #'(lambda (x) (if (eql x '*) '* (type-filter x)))
		  arg-types)))
  (when (and return-type (not (eq 'T return-type)))
    (setf (get fname 'return-type) (type-filter return-type)))
  (when never-change-special-var-p (setf (get fname 'no-sp-change) t))
  (when predicate (setf (get fname 'predicate) t))
  (remprop fname ':inline-always)
  (remprop fname ':inline-safe)
  (remprop fname ':inline-unsafe)
  (do ((scan optimizers (cddr scan))
       (safety) (inline-info))
      ((null scan))
      (setq safety (first scan)
	    inline-info (second scan))
      (push inline-info (get fname safety)))
  )

; file alloc.c
(defsysfun 'si::ALLOC "siLalloc" nil nil nil nil)
(defsysfun 'si::NPAGE "siLnpage" nil nil nil nil)
(defsysfun 'si::MAXPAGE "siLmaxpage" nil nil nil nil)
(defsysfun 'si::ALLOC-CONTPAGE "siLalloc_contpage" nil nil nil nil)
(defsysfun 'si::NCBPAGE "siLncbpage" nil nil nil nil)
(defsysfun 'si::MAXCBPAGE "siLmaxcbpage" nil nil nil nil)
(defsysfun 'si::ALLOC-RELPAGE "siLalloc_relpage" nil nil nil nil)
(defsysfun 'si::NRBPAGE "siLnrbpage" nil nil nil nil)
(defsysfun 'si::GET-HOLE-SIZE "siLget_hole_size" nil nil nil nil)
(defsysfun 'si::SET-HOLE-SIZE "siLset_hole_size" nil nil nil nil)

(defsysfun 'si::RPLACA-NTHCDR "siLrplaca_nthcdr" nil T nil nil)
(defsysfun 'si::LIST-NTH "siLlist_nth" nil T nil nil)
(defsysfun 'si::MAKE-PURE-ARRAY "siLmake_pure_array" nil 'array nil nil)
(defsysfun 'si::MAKE-VECTOR "siLmake_vector" nil 'vector nil nil)
;(defsysfun 'si::MAKE-BITVECTOR "siLmake_bitvector" nil 'bit-vector nil nil)
(DEFSYSFUN 'AREF "Laref" '(array *) T NIL NIL
  ':inline-unsafe '((t t t) t nil t
        "@0;aref(#0,fix(#1)*(#0)->a.a_dims[1]+fix(#2))")
  ':inline-unsafe '(((array t) t t) t nil nil
        "@0;(#0)->a.a_self[fix(#1)*(#0)->a.a_dims[1]+fix(#2)]")
  ':inline-unsafe '(((array t) fixnum fixnum) t nil nil
        "@0;(#0)->a.a_self[#1*(#0)->a.a_dims[1]+#2]")
  ':inline-unsafe '(((array string-char) fixnum fixnum) character nil nil
        "@0;(#0)->ust.ust_self[#1*(#0)->a.a_dims[1]+#2]")
  ':inline-unsafe '(((array long-float) fixnum fixnum) long-float nil nil
        "@0;(#0)->lfa.lfa_self[#1*(#0)->a.a_dims[1]+#2]")
  ':inline-unsafe '(((array short-float) fixnum fixnum) short-float nil nil
        "@0;(#0)->sfa.sfa_self[#1*(#0)->a.a_dims[1]+#2]")
  ':inline-unsafe '(((array fixnum) fixnum fixnum) fixnum nil nil
        "@0;(#0)->fixa.fixa_self[#1*(#0)->a.a_dims[1]+#2]")

  ':inline-always '((t t) t nil t "aref1(#0,fixint(#1))")
  ':inline-always '((t fixnum) t nil t "aref1(#0,#1)")
  ':inline-unsafe '((t t) t nil t "aref1(#0,fix(#1))")
  ':inline-unsafe '((t fixnum) t nil t "aref1(#0,#1)")
  ':inline-unsafe '(((array t) t) t nil nil "(#0)->v.v_self[fix(#1)]")
  ':inline-unsafe '(((array t) fixnum) t nil nil "(#0)->v.v_self[#1]")
  ':inline-unsafe '(((array string-char) fixnum) fixnum nil nil
        "(#0)->ust.ust_self[#1]")
  ':inline-unsafe '(((array string-char) fixnum) character nil nil
        "(#0)->ust.ust_self[#1]")
  ':inline-unsafe '(((array long-float) fixnum) long-float nil nil
        "(#0)->lfa.lfa_self[#1]")
  ':inline-unsafe '(((array short-float) fixnum) short-float nil nil
        "(#0)->sfa.sfa_self[#1]")
  ':inline-unsafe '(((array fixnum) fixnum) fixnum nil nil
        "(#0)->fixa.fixa_self[#1]"))
(DEFSYSFUN 'SI::ASET "siLaset" '(T ARRAY *) NIL NIL NIL
  ':inline-unsafe '((t t t t) t t nil
		   "@0;aset(#1,fix(#2)*(#1)->a.a_dims[1]+fix(#3),#0)")
  ':inline-unsafe '((t (array t) fixnum fixnum) t t nil
		   "@1;(#1)->a.a_self[#2*(#1)->a.a_dims[1]+#3]= #0")
  ':inline-unsafe
  '((character (array string-char) fixnum fixnum) character t nil
    "@1;(#1)->ust.ust_self[#2*(#1)->a.a_dims[1]+#3]= #0")
  ':inline-unsafe 
  '((long-float (array long-float) fixnum fixnum) long-float t nil
    "@1;(#1)->lfa.lfa_self[#2*(#1)->a.a_dims[1]+#3]= #0")
  ':inline-unsafe
  '((short-float (array short-float) fixnum fixnum) short-float t nil
    "@1;(#1)->sfa.sfa_self[#2*(#1)->a.a_dims[1]+#3]= #0")
  ':inline-unsafe
  '((fixnum (array fixnum) fixnum fixnum) fixnum t nil
    "@1;(#1)->fixa.fixa_self[#2*(#1)->a.a_dims[1]+#3]= #0")

  ':inline-always '((t t t) t t nil "aset1(#1,fixint(#2),#0)")
  ':inline-always '((t t fixnum) t t nil "aset1(#1,#2,#0)")

  ':inline-unsafe '((t t t) t t nil "aset1(#1,fix(#2),#0)")
  ':inline-unsafe '((t (array t) fixnum) t t nil "(#1)->v.v_self[#2]= #0")
  ':inline-unsafe '((character (array string-char) fixnum) character t nil
		   "(#1)->ust.ust_self[#2]= #0")
  ':inline-unsafe '((long-float (array long-float) fixnum) long-float t nil
		   "(#1)->lfa.lfa_self[#2]= #0")
  ':inline-unsafe '((short-float (array short-float) fixnum) short-float t nil
		   "(#1)->sfa.sfa_self[#2]= #0")
  ':inline-unsafe '((fixnum (array fixnum) fixnum) fixnum t nil
		   "(#1)->fixa.fixa_self[#2]= #0"))
(defsysfun 'ARRAY-ELEMENT-TYPE "Larray_element_type" '(array) T nil nil)
(defsysfun 'ARRAY-RANK "Larray_rank" '(array) 'fixnum nil nil)
(defsysfun 'ARRAY-DIMENSION "Larray_dimension" '(array fixnum) 'fixnum nil nil)
(defsysfun 'ARRAY-TOTAL-SIZE "Larray_total_size" '(array) T nil nil
	   ':inline-unsafe '((t) fixnum nil nil "((#0)->st.st_dim)"))
(defsysfun 'ADJUSTABLE-ARRAY-P "Ladjustable_array_p" '(array) T nil t)
(defsysfun 'si::DISPLACED-ARRAY-P "siLdisplaced_array_p" '(array) T nil t)
(defsysfun 'SVREF "Lsvref" '(simple-vector fixnum) T nil nil
  ':inline-always '((t t) t nil t "aref1(#0,fixint(#1))")
  ':inline-always '((t fixnum) t nil t "aref1(#0,#1)")
  ':inline-unsafe '((t t) t nil nil "(#0)->v.v_self[fix(#1)]")
  ':inline-unsafe '((t fixnum) t nil nil "(#0)->v.v_self[#1]"))
(defsysfun 'si::SVSET "siLsvset" '(simple-vector fixnum t) T nil nil
  ':inline-always '((t t t) t t nil "aset1(#0,fixint(#1),#2)")
  ':inline-always '((t fixnum t) t t nil "aset1(#0,#1,#2)")
  ':inline-unsafe '((t t t) t t nil "((#0)->v.v_self[fix(#1)]=(#2))")
  ':inline-unsafe '((t fixnum t) t t nil "(#0)->v.v_self[#1]= #2"))
(defsysfun 'ARRAY-HAS-FILL-POINTER-P "Larray_has_fill_pointer_p" nil T nil t)
(defsysfun 'FILL-POINTER "Lfill_pointer" '(vector) 'fixnum nil nil
	   ':inline-unsafe '((t) fixnum nil nil "((#0)->st.st_fillp)"))
(defsysfun 'si::FILL-POINTER-SET "siLfill_pointer_set"
  '(vector fixnum) 'fixnum nil nil
  ':inline-unsafe '((t fixnum) fixnum t nil "((#0)->st.st_fillp)=(#1)"))
(defsysfun 'si::REPLACE-ARRAY "siLreplace_array" nil T nil nil)
;(defsysfun 'si::ASET-BY-CURSOR "siLaset_by_cursor" nil T nil nil)

; file assignment.c
(defsysfun 'SET "Lset" '(symbol t) T nil nil)
(defsysfun 'si::FSET "siLfset" '(symbol t) T nil nil)
(defsysfun 'MAKUNBOUND "Lmakunbound" '(symbol) T nil nil)
(defsysfun 'FMAKUNBOUND "Lfmakunbound" '(symbol) T nil nil)
(defsysfun 'si::CLEAR-COMPILER-PROPERTIES "siLclear_compiler_properties" nil
  T nil nil)
#+clos
(defsysfun 'si::SETF-NAMEP "siLsetf_namep" nil T nil t)

; file catch.c
;#-clcs (DEFSYSFUN 'SI::ERROR-SET "siLerror_set" '(T) '* NIL NIL)

; file cfun.c
(defsysfun 'si::COMPILED-FUNCTION-NAME "siLcompiled_function_name" nil
  T nil nil)

; file character.c
(defsysfun 'STANDARD-CHAR-P "Lstandard_char_p" '(character) T nil t)
(defsysfun 'GRAPHIC-CHAR-P "Lgraphic_char_p" '(character) T nil t)
(defsysfun 'STRING-CHAR-P "Lstring_char_p" '(character) T nil t)
(defsysfun 'ALPHA-CHAR-P "Lalpha_char_p" '(character) T nil t)
(defsysfun 'UPPER-CASE-P "Lupper_case_p" '(character) T nil t)
(defsysfun 'LOWER-CASE-P "Llower_case_p" '(character) T nil t)
(defsysfun 'BOTH-CASE-P "Lboth_case_p" '(character) T nil t)
(defsysfun 'DIGIT-CHAR-P "Ldigit_char_p" '(character *) T nil nil
	   ':inline-always
	   '((character) boolean nil nil "@0; ((#0) <= '9' && (#0) >= '0')"))
(defsysfun 'ALPHANUMERICP "Lalphanumericp" '(character) T nil t)
(DEFSYSFUN 'CHARACTER "Lcharacter" '(T) 'CHARACTER NIL NIL)
(defsysfun 'CHAR= "Lchar_eq" '(character *) T nil t
  ':inline-unsafe '((t t) boolean nil nil "char_code(#0)==char_code(#1)")
  ':inline-always '((character character) boolean nil nil "(#0)==(#1)"))
(defsysfun 'CHAR/= "Lchar_neq" '(character *) T nil t
  ':inline-unsafe '((t t) boolean nil nil "char_code(#0)!=char_code(#1)")
  ':inline-always '((character character) boolean nil nil "(#0)!=(#1)"))
(defsysfun 'CHAR< "Lchar_l" '(character *) T nil t
  ':inline-always '((character character) boolean nil nil "(#0)<(#1)"))
(defsysfun 'CHAR> "Lchar_g" '(character *) T nil t
  ':inline-always '((character character) boolean nil nil "(#0)>(#1)"))
(defsysfun 'CHAR<= "Lchar_le" '(character *) T nil t
  ':inline-always '((character character) boolean nil nil "(#0)<=(#1)"))
(defsysfun 'CHAR>= "Lchar_ge" '(character *) T nil t
  ':inline-always '((character character) boolean nil nil "(#0)>=(#1)"))
(defsysfun 'CHAR-EQUAL "Lchar_equal" '(character *) T nil t)
(defsysfun 'CHAR-NOT-EQUAL "Lchar_not_equal" '(character *) T nil t)
(defsysfun 'CHAR-LESSP "Lchar_lessp" '(character *) T nil t)
(defsysfun 'CHAR-GREATERP "Lchar_greaterp" '(character *) T nil t)
(defsysfun 'CHAR-NOT-GREATERP "Lchar_not_greaterp" '(character *) T nil t)
(defsysfun 'CHAR-NOT-LESSP "Lchar_not_lessp" '(character *) T nil t)
(defsysfun 'CHARACTER "Lcharacter" nil 'character nil nil)
(defsysfun 'CHAR-CODE "Lchar_code" '(character) 'fixnum nil nil
  ':inline-always '((character) fixnum nil nil "#0"))
(defsysfun 'CHAR-BITS "Lchar_bits" '(character) 'fixnum nil nil)
(defsysfun 'CHAR-FONT "Lchar_font" '(character) 'fixnum nil nil)
(defsysfun 'CODE-CHAR "Lcode_char" '(fixnum *) 'character nil nil
  ':inline-always '((fixnum) character nil nil "#0"))
(defsysfun 'MAKE-CHAR "Lmake_char" '(character *) 'character nil nil)
(defsysfun 'CHAR-UPCASE "Lchar_upcase" '(character) 'character nil nil)
(defsysfun 'CHAR-DOWNCASE "Lchar_downcase" '(character) 'character nil nil)
(defsysfun 'DIGIT-CHAR "Ldigit_char" '(fixnum *) 'character nil nil)
(defsysfun 'CHAR-INT "Lchar_int" '(character) 'fixnum nil nil
  ':inline-always '((character) fixnum nil nil "#0"))
(defsysfun 'INT-CHAR "Lint_char" '(fixnum) 'character nil nil
  ':inline-always '((fixnum) character nil nil "#0"))
(defsysfun 'CHAR-NAME "Lchar_name" '(character) 'symbol nil nil)
(defsysfun 'NAME-CHAR "Lname_char" '(symbol) 'character nil nil)
(defsysfun 'CHAR-BIT "Lchar_bit" '(character Y) 'fixnum nil nil)
(defsysfun 'SET-CHAR-BIT "Lset_char_bit" '(character t fixnum) 'character
  nil nil)

; file error.c
#-clcs
(defsysfun 'ERROR "Lerror" '(T *) T nil nil)
#-clcs
(defsysfun 'CERROR "Lcerror" '(T T *) T nil nil)

(defsysfun 'si::IHS-TOP "siLihs_top" nil nil nil nil)
(defsysfun 'si::IHS-FUN "siLihs_fun" nil nil nil nil)
(defsysfun 'si::IHS-ENV "siLihs_env" nil nil nil nil)
(defsysfun 'si::FRS-TOP "siLfrs_top" nil nil nil nil)
(defsysfun 'si::FRS-VS "siLfrs_vs" nil nil nil nil)
(defsysfun 'si::FRS-BDS "siLfrs_bds" nil nil nil nil)
(defsysfun 'si::FRS-CLASS "siLfrs_class" nil nil nil nil)
(defsysfun 'si::FRS-TAG "siLfrs_tag" nil nil nil nil)
(defsysfun 'si::FRS-IHS "siLfrs_ihs" nil nil nil nil)
(defsysfun 'si::BDS-TOP "siLbds_top" nil nil nil nil)
(defsysfun 'si::BDS-VAR "siLbds_var" nil nil nil nil)
(defsysfun 'si::BDS-VAL "siLbds_val" nil nil nil nil)
(defsysfun 'si::VS-TOP "siLvs_top" nil nil nil nil)
(defsysfun 'si::VS "siLvs" nil nil nil nil)
(defsysfun 'si::SCH-FRS-BASE "siLsch_frs_base" nil nil nil nil)
(defsysfun 'si::INTERNAL-SUPER-GO "siLinternal_super_go" nil nil nil nil)
(defsysfun 'si::UNIVERSAL-ERROR-HANDLER "siLuniversal_error_handler" nil
  nil nil nil)

; file eval.c
(defsysfun 'APPLY "Lapply" '(T T *) T nil nil)
(DEFSYSFUN 'FUNCALL "Lfuncall" '(T *) T NIL NIL)
(DEFSYSFUN 'EVAL "Leval" '(T) T NIL NIL)
(DEFSYSFUN 'EVALHOOK "Levalhook" '(T T T *) T NIL NIL)
(DEFSYSFUN 'APPLYHOOK "Lapplyhook" '(T T T T *) T NIL NIL)
(DEFSYSFUN 'CONSTANTP "Lconstantp" '(T) T NIL T)
(defsysfun 'si::UNLINK-SYMBOL "siLunlink_symbol" nil T nil nil)
(defsysfun 'si::LINK-ENABLE "siLlink_enable" nil T nil nil)

; file file.d
(DEFSYSFUN 'MAKE-SYNONYM-STREAM "Lmake_synonym_stream" '(T) T NIL NIL)
(DEFSYSFUN 'MAKE-BROADCAST-STREAM "Lmake_broadcast_stream" '(*) T NIL NIL)
(defsysfun 'MAKE-CONCATENATED-STREAM "Lmake_concatenated_stream" nil T nil nil)
(DEFSYSFUN 'MAKE-TWO-WAY-STREAM "Lmake_two_way_stream" '(T T) T NIL NIL)
(DEFSYSFUN 'MAKE-ECHO-STREAM "Lmake_echo_stream" '(T T) T NIL NIL)
(defsysfun 'MAKE-STRING-INPUT-STREAM "Lmake_string_input_stream" nil T nil nil)
(defsysfun 'MAKE-STRING-OUTPUT-STREAM "Lmake_string_output_stream" nil T
  nil nil)
(defsysfun 'GET-OUTPUT-STREAM-STRING "Lget_output_stream_string" nil T nil nil)
(DEFSYSFUN 'SI::OUTPUT-STREAM-STRING "siLoutput_stream_string" '(T) T NIL NIL)
(DEFSYSFUN 'STREAMP "Lstreamp" '(T) T NIL T)
(DEFSYSFUN 'INPUT-STREAM-P "Linput_stream_p" '(T) T NIL T)
(DEFSYSFUN 'OUTPUT-STREAM-P "Loutput_stream_p" '(T) T NIL T)
(DEFSYSFUN 'STREAM-ELEMENT-TYPE "Lstream_element_type" '(T) T NIL NIL)
(DEFSYSFUN 'CLOSE "Lclose" '(T *) T NIL NIL)
;#-clcs (DEFSYSFUN 'OPEN "Lopen" '(T *) T NIL NIL) 
(DEFSYSFUN 'FILE-POSITION "Lfile_position" '(T *) T NIL NIL)
(DEFSYSFUN 'FILE-LENGTH "Lfile_length" '(T) T NIL NIL)
;#-clcs (DEFSYSFUN 'LOAD "Lload" '(T *) T NIL NIL) 
(defsysfun 'si::GET-STRING-INPUT-STREAM-INDEX
  "siLget_string_input_stream_index" nil T nil nil)
(defsysfun 'si::MAKE-STRING-OUTPUT-STREAM-FROM-STRING
  "siLmake_string_output_stream_from_string" nil T nil nil)

; file gbc.c
(defsysfun 'si::ROOM-REPORT "siLroom_report" nil T nil nil)
(defsysfun 'si::RESET-GBC-COUNT "siLreset_gbc_count" nil T nil nil)
(defsysfun 'GBC "Lgbc" nil T nil nil)

; file unixfsys.c
(DEFSYSFUN 'TRUENAME "Ltruename" '(T) T NIL NIL)
(DEFSYSFUN 'RENAME-FILE "Lrename_file" '(T T) T NIL NIL)
(DEFSYSFUN 'SI::SPECIALP "siLspecialp" '(T) T NIL T)
(DEFSYSFUN 'DELETE-FILE "Ldelete_file" '(T) T NIL NIL)
(DEFSYSFUN 'PROBE-FILE "Lprobe_file" '(T) T NIL NIL)
(DEFSYSFUN 'FILE-WRITE-DATE "Lfile_write_date" '(T) T NIL NIL)
(DEFSYSFUN 'FILE-AUTHOR "Lfile_author" '(T) T NIL NIL)
(DEFSYSFUN 'PATHNAME "Lpathname" '(T) T NIL NIL)
(DEFSYSFUN 'USER-HOMEDIR-PATHNAME "Luser_homedir_pathname" '(*) T NIL NIL)
(DEFSYSFUN 'DIRECTORY "Ldirectory" '(T) T NIL NIL)
(defsysfun 'si::CHDIR "siLchdir" nil T nil nil)

; file unixint.c
(defsysfun 'si::CATCH-BAD-SIGNALS "siLcatch_bad_signals" nil T nil nil)
(defsysfun 'si::UNCATCH-BAD-SIGNALS "siLuncatch_bad_signals" nil T nil nil)

; file format.c
(DEFSYSFUN 'FORMAT "Lformat" '(T string *) T NIL NIL)

; file hash.d
(DEFSYSFUN 'MAKE-HASH-TABLE "Lmake_hash_table" '(*) T NIL NIL)
(DEFSYSFUN 'HASH-TABLE-P "Lhash_table_p" '(T) T NIL T)
(DEFSYSFUN 'VALUES "Lvalues" '(*) '* NIL NIL)
(DEFSYSFUN 'GETHASH "Lgethash" '(T T *) '(VALUES T T) NIL NIL)
(DEFSYSFUN 'REMHASH "Lremhash" '(T T) T NIL NIL)
(DEFSYSFUN 'MAPHASH "Lmaphash" '(T T) T NIL NIL)
(DEFSYSFUN 'CLRHASH "Lclrhash" '(T) T NIL NIL)
(DEFSYSFUN 'HASH-TABLE-COUNT "Lhash_table_count" '(T) T NIL NIL)
(DEFSYSFUN 'SXHASH "Lsxhash" '(T) 'FIXNUM NIL NIL)
(DEFSYSFUN 'SI::HASH-SET "siLhash_set" NIL T NIL NIL)

; file list.d
(DEFSYSFUN 'CAR "Lcar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "car(#0)")
  ':inline-unsafe '((t) t nil nil "CAR(#0)"))
(DEFSYSFUN 'CDR "Lcdr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdr(#0)")
  ':inline-unsafe '((t) t nil nil "CDR(#0)"))
(DEFSYSFUN 'CAAR "Lcaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caar(#0)")
  ':inline-unsafe '((t) t nil nil "CAAR(#0)"))
(DEFSYSFUN 'CADR "Lcadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cadr(#0)")
  ':inline-unsafe '((t) t nil nil "CADR(#0)"))
(DEFSYSFUN 'CDAR "Lcdar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdar(#0)")
  ':inline-unsafe '((t) t nil nil "CDAR(#0)"))
(DEFSYSFUN 'CDDR "Lcddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cddr(#0)")
  ':inline-unsafe '((t) t nil nil "CDDR(#0)"))
(DEFSYSFUN 'CAAAR "Lcaaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caaar(#0)")
  ':inline-unsafe '((t) t nil nil "CAAAR(#0)"))
(DEFSYSFUN 'CAADR "Lcaadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caadr(#0)")
  ':inline-unsafe '((t) t nil nil "CAADR(#0)"))
(DEFSYSFUN 'CADAR "Lcadar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cadar(#0)")
  ':inline-unsafe '((t) t nil nil "CADAR(#0)"))
(DEFSYSFUN 'CADDR "Lcaddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caddr(#0)")
  ':inline-unsafe '((t) t nil nil "CADDR(#0)"))
(DEFSYSFUN 'CDAAR "Lcdaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdaar(#0)")
  ':inline-unsafe '((t) t nil nil "CDAAR(#0)"))
(DEFSYSFUN 'CDADR "Lcdadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdadr(#0)")
  ':inline-unsafe '((t) t nil nil "CDADR(#0)"))
(DEFSYSFUN 'CDDAR "Lcddar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cddar(#0)")
  ':inline-unsafe '((t) t nil nil "CDDAR(#0)"))
(DEFSYSFUN 'CDDDR "Lcdddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdddr(#0)")
  ':inline-unsafe '((t) t nil nil "CDDDR(#0)"))
(DEFSYSFUN 'CAAAAR "Lcaaaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caaaar(#0)")
  ':inline-unsafe '((t) t nil nil "CAAAAR(#0)"))
(DEFSYSFUN 'CAAADR "Lcaaadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caaadr(#0)")
  ':inline-unsafe '((t) t nil nil "CAAADR(#0)"))
(DEFSYSFUN 'CAADAR "Lcaadar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caadar(#0)")
  ':inline-unsafe '((t) t nil nil "CAADAR(#0)"))
(DEFSYSFUN 'CAADDR "Lcaaddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caaddr(#0)")
  ':inline-unsafe '((t) t nil nil "CAADDR(#0)"))
(DEFSYSFUN 'CADAAR "Lcadaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cadaar(#0)")
  ':inline-unsafe '((t) t nil nil "CADAAR(#0)"))
(DEFSYSFUN 'CADADR "Lcadadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cadadr(#0)")
  ':inline-unsafe '((t) t nil nil "CADADR(#0)"))
(DEFSYSFUN 'CADDAR "Lcaddar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "caddar(#0)")
  ':inline-unsafe '((t) t nil nil "CADDAR(#0)"))
(DEFSYSFUN 'CADDDR "Lcadddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cadddr(#0)")
  ':inline-unsafe '((t) t nil nil "CADDDR(#0)"))
(DEFSYSFUN 'CDAAAR "Lcdaaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdaaar(#0)")
  ':inline-unsafe '((t) t nil nil "CDAAAR(#0)"))
(DEFSYSFUN 'CDAADR "Lcdaadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdaadr(#0)")
  ':inline-unsafe '((t) t nil nil "CDAADR(#0)"))
(DEFSYSFUN 'CDADAR "Lcdadar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdadar(#0)")
  ':inline-unsafe '((t) t nil nil "CDADAR(#0)"))
(DEFSYSFUN 'CDADDR "Lcdaddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdaddr(#0)")
  ':inline-unsafe '((t) t nil nil "CDADDR(#0)"))
(DEFSYSFUN 'CDDAAR "Lcddaar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cddaar(#0)")
  ':inline-unsafe '((t) t nil nil "CDDAAR(#0)"))
(DEFSYSFUN 'CDDADR "Lcddadr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cddadr(#0)")
  ':inline-unsafe '((t) t nil nil "CDDADR(#0)"))
(DEFSYSFUN 'CDDDAR "Lcdddar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdddar(#0)")
  ':inline-unsafe '((t) t nil nil "CDDDAR(#0)"))
(DEFSYSFUN 'CDDDDR "Lcddddr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cddddr(#0)")
  ':inline-unsafe '((t) t nil nil "CDDDDR(#0)"))
(DEFSYSFUN 'CONS "Lcons" '(T T) T NIL NIL
  ':inline-always '((t t) t nil t "CONS(#0,#1)"))
(DEFSYSFUN 'TREE-EQUAL "Ltree_equal" '(T T *) T NIL T)
(DEFSYSFUN 'ENDP "Lendp" '(T) T NIL T
  ':inline-safe '((t) boolean nil nil "endp(#0)")
  ':inline-unsafe '((t) boolean nil nil "#0==Cnil"))
(DEFSYSFUN 'LIST-LENGTH "Llist_length" '(T) T NIL NIL)
(DEFSYSFUN 'NTH "Lnth" '(T T) T NIL NIL
  ':inline-always '((t t) t nil nil "nth(fixint(#0),#1)")
  ':inline-always '((fixnum t) t nil nil "nth(#0,#1)")
  ':inline-unsafe '((t t) t nil nil "nth(fix(#0),#1)")
  ':inline-unsafe '((fixnum t) t nil nil "nth(#0,#1)"))
(DEFSYSFUN 'FIRST "Lcar" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "car(#0)")
  ':inline-unsafe '((t) t nil nil "CAR(#0)"))
(defsysfun 'SECOND "Lcadr" '(T) T nil nil
  ':inline-safe '((t) t nil nil "cadr(#0)")
  ':inline-unsafe '((t) t nil nil "CADR(#0)"))
(defsysfun 'THIRD "Lcaddr" '(T) T nil nil
  ':inline-safe '((t) t nil nil "caddr(#0)")
  ':inline-unsafe '((t) t nil nil "CADDR(#0)"))
(defsysfun 'FOURTH "Lcadddr" '(T) T nil nil
  ':inline-safe '((t) t nil nil "cadddr(#0)")
  ':inline-unsafe '((t) t nil nil "CADDDR(#0)"))
(defsysfun 'FIFTH "Lfifth" '(T) T nil nil)
(defsysfun 'SIXTH "Lsixth" '(T) T nil nil)
(defsysfun 'SEVENTH "Lseventh" '(T) T nil nil)
(defsysfun 'EIGHTH "Leighth" '(T) T nil nil)
(defsysfun 'NINTH "Lninth" '(T) T nil nil)
(defsysfun 'TENTH "Ltenth" '(T) T nil nil)
(DEFSYSFUN 'REST "Lcdr" '(T) T NIL NIL
  ':inline-safe '((t) t nil nil "cdr(#0)")
  ':inline-unsafe '((t) t nil nil "CDR(#0)"))
(defsysfun 'NTHCDR "Lnthcdr" '(fixnum t) T nil nil
  ':inline-always '((t t) t nil nil "nthcdr(fixint(#0),#1)")
  ':inline-always '((fixnum t) t nil nil "nthcdr(#0,#1)")
  ':inline-unsafe '((t t) t nil nil "nthcdr(fix(#0),#1)")
  ':inline-unsafe '((fixnum t) t nil nil "nthcdr(#0,#1)"))
(DEFSYSFUN 'LAST "Llast" '(T) T NIL NIL)
(DEFSYSFUN 'LIST "Llist" '(*) T NIL NIL
  ':inline-always '(nil t nil nil "Cnil")
  ':inline-always '((t) t nil t "CONS(#0,Cnil)")
  ':inline-always '((t t) t nil t list-inline)
  ':inline-always '((t t t) t nil t list-inline)
  ':inline-always '((t t t t) t nil t list-inline)
  ':inline-always '((t t t t t) t nil t list-inline)
  ':inline-always '((t t t t t t) t nil t list-inline)
  ':inline-always '((t t t t t t t) t nil t list-inline)
  ':inline-always '((t t t t t t t t) t nil t list-inline)
  ':inline-always '((t t t t t t t t t) t nil t list-inline)
  ':inline-always '((t t t t t t t t t t) t nil t list-inline))
(DEFSYSFUN 'LIST* "LlistA" '(T *) T NIL NIL
  ':inline-always '((t) t nil nil "#0")
  ':inline-always '((t t) t nil t "CONS(#0,#1)")
  ':inline-always '((t t t) t nil t list*-inline)
  ':inline-always '((t t t t) t nil t list*-inline)
  ':inline-always '((t t t t t) t nil t list*-inline)
  ':inline-always '((t t t t t t) t nil t list*-inline)
  ':inline-always '((t t t t t t t) t nil t list*-inline)
  ':inline-always '((t t t t t t t t) t nil t list*-inline)
  ':inline-always '((t t t t t t t t t) t nil t list*-inline)
  ':inline-always '((t t t t t t t t t t) t nil t list*-inline))
(defsysfun 'MAKE-LIST "Lmake_list" '(fixnum *) T nil nil)
(DEFSYSFUN 'APPEND "Lappend" '(*) T NIL NIL
  ':inline-always '((t t) t nil t "append(#0,#1)"))
(DEFSYSFUN 'COPY-LIST "Lcopy_list" '(T) T NIL NIL)
(DEFSYSFUN 'COPY-ALIST "Lcopy_alist" '(T) T NIL NIL)
(DEFSYSFUN 'COPY-TREE "Lcopy_tree" '(T) T NIL NIL)
(DEFSYSFUN 'REVAPPEND "Lrevappend" '(T T) T NIL NIL)
(DEFSYSFUN 'NCONC "Lnconc" '(*) T NIL NIL
  ':inline-always '((t t) t t nil "nconc(#0,#1)"))
(DEFSYSFUN 'NRECONC "Lreconc" '(T T) T NIL NIL)
(DEFSYSFUN 'BUTLAST "Lbutlast" '(T *) T NIL NIL)
(DEFSYSFUN 'NBUTLAST "Lnbutlast" '(T *) T NIL NIL)
(DEFSYSFUN 'LDIFF "Lldiff" '(T T) T NIL NIL)
(defsysfun 'RPLACA "Lrplaca" '(cons T) T nil nil)
(defsysfun 'RPLACD "Lrplacd" '(cons T) T nil nil)
(DEFSYSFUN 'SUBST "Lsubst" '(T T T *) T NIL NIL)
(DEFSYSFUN 'SUBST-IF "Lsubst_if" '(T T T *) T NIL NIL)
(DEFSYSFUN 'SUBST-IF-NOT "Lsubst_if_not" '(T T T *) T NIL NIL)
(DEFSYSFUN 'NSUBST "Lnsubst" '(T T T *) T NIL NIL)
(DEFSYSFUN 'NSUBST-IF "Lnsubst_if" '(T T T *) T NIL NIL)
(DEFSYSFUN 'NSUBST-IF-NOT "Lnsubst_if_not" '(T T T *) T NIL NIL)
(DEFSYSFUN 'SUBLIS "Lsublis" '(T T *) T NIL NIL)
(DEFSYSFUN 'NSUBLIS "Lnsublis" '(T T *) T NIL NIL)
(DEFSYSFUN 'MEMBER "Lmember" '(T T *) T NIL NIL)
(DEFSYSFUN 'MEMBER-IF "Lmember_if" '(T T *) T NIL NIL)
(DEFSYSFUN 'MEMBER-IF-NOT "Lmember_if_not" '(T T *) T NIL NIL)
(defsysfun 'MEMBER1 "Lmember1"'(T T *) T nil nil)
(DEFSYSFUN 'TAILP "Ltailp" '(T T) T NIL T)
(DEFSYSFUN 'ADJOIN "Ladjoin" '(T T *) T NIL NIL)
(DEFSYSFUN 'ACONS "Lacons" '(T T T) T NIL NIL)
(DEFSYSFUN 'PAIRLIS "Lpairlis" '(T T *) T NIL NIL)
(DEFSYSFUN 'ASSOC "Lassoc" '(T T *) T NIL NIL)
(DEFSYSFUN 'ASSOC-IF "Lassoc_if" '(T T) T NIL NIL)
(DEFSYSFUN 'ASSOC-IF-NOT "Lassoc_if_not" '(T T) T NIL NIL)
(DEFSYSFUN 'RASSOC "Lrassoc" '(T T *) T NIL NIL)
(DEFSYSFUN 'RASSOC-IF "Lrassoc_if" '(T T) T NIL NIL)
(DEFSYSFUN 'RASSOC-IF-NOT "Lrassoc_if_not" '(T T) T NIL NIL)
(defsysfun 'si::MEMQ "siLmemq" '(T T T) T nil nil)

; file lwp.c
;to do

; file macros.c
(defsysfun 'si::DEFINE-MACRO "siLdefine_macro" nil T nil nil)
(DEFSYSFUN 'MACROEXPAND "Lmacroexpand" '(T *) '(VALUES T T) NIL NIL)
(DEFSYSFUN 'MACROEXPAND-1 "Lmacroexpand_1" '(T *) '(VALUES T T) NIL NIL)

; file main.c
(defsysfun 'QUIT "Lquit" nil T nil nil)
(DEFSYSFUN 'IDENTITY "Lidentity" '(T) T NIL NIL)
(defsysfun 'si::ARGC "siLargc" nil T nil nil)
(defsysfun 'si::ARGV "siLargv" nil T nil nil)
(defsysfun 'si::GETENV "siLgetenv" nil T nil nil)
(defsysfun 'si::RESET-STACK-LIMITS "siLreset_stack_limits" nil T nil nil)
#-sparc
(defsysfun 'si::INIT-SYSTEM "siLinit_system" nil T nil nil)
(defsysfun 'si::POINTER "siLaddress" nil T nil nil)
(defsysfun 'si::NANI "siLnani" nil T nil nil)

; file mapfun.c
(DEFSYSFUN 'MAPCAR "Lmapcar" '(T T *) T NIL NIL)
(DEFSYSFUN 'MAPLIST "Lmaplist" '(T T *) T NIL NIL)
(DEFSYSFUN 'MAPC "Lmapc" '(T T *) T NIL NIL)
(DEFSYSFUN 'MAPL "Lmapl" '(T T *) T NIL NIL)
(DEFSYSFUN 'MAPCAN "Lmapcan" '(T T *) T NIL NIL)
(DEFSYSFUN 'MAPCON "Lmapcon" '(T T *) T NIL NIL)

; file multival.c
(defsysfun 'VALUES "Lvalues" nil T nil nil)
(DEFSYSFUN 'VALUES-LIST "Lvalues_list" '(T) '* NIL NIL)

; file num_arith.c
(DEFSYSFUN '+ "Lplus" '(*) T NIL NIL
  ':inline-always '((t t) t nil t "number_plus(#0,#1)")
  ':inline-always '((fixnum-float fixnum-float) long-float nil nil
        "(double)(#0)+(double)(#1)")
  ':inline-always '((fixnum-float fixnum-float) short-float nil nil
        "(float)(#0)+(float)(#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil "(#0)+(#1)"))
(DEFSYSFUN '- "Lminus" '(T *) T NIL NIL
  ':inline-always '((t) t nil t "number_negate(#0)")
  ':inline-always '((t t) t nil t "number_minus(#0,#1)")
  ':inline-always '((fixnum-float fixnum-float) long-float nil nil
        "(double)(#0)-(double)(#1)")
  ':inline-always '((fixnum-float fixnum-float) short-float nil nil
        "(float)(#0)-(float)(#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil "(#0)-(#1)")

  ':inline-always '((fixnum-float) long-float nil nil "-(double)(#0)")
  ':inline-always '((fixnum-float) short-float nil nil "-(float)(#0)")
  ':inline-always '((fixnum) fixnum nil nil "-(#0)")) 
(DEFSYSFUN '* "Ltimes" '(*) T NIL NIL
  ':inline-always '((t t) t nil t "number_times(#0,#1)")
  ':inline-always '((fixnum-float fixnum-float) long-float nil nil
        "(double)(#0)*(double)(#1)")
  ':inline-always '((fixnum-float fixnum-float) short-float nil nil
        "(float)(#0)*(float)(#1)")
  ':inline-always '((fixnum fixnum) t nil nil "fixnum_times(#0,#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil "(#0)*(#1)"))
(DEFSYSFUN '/ "Ldivide" '(T *) T NIL NIL
  ':inline-always '((t t) t nil t "number_divide(#0,#1)")
  ':inline-always '((fixnum-float fixnum-float) long-float nil nil
        "(double)(#0)/(double)(#1)")
  ':inline-always '((fixnum-float fixnum-float) short-float nil nil
        "(float)(#0)/(float)(#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil
        "(#0)/(#1)"))
(DEFSYSFUN '1+ "Lone_plus" '(T) T NIL NIL
  ':inline-always '((t) t nil t "one_plus(#0)")
  ':inline-always '((fixnum-float) long-float nil nil "(double)(#0)+1")
  ':inline-always '((fixnum-float) short-float nil nil "(float)(#0)+1")
  ':inline-always '((fixnum) fixnum nil nil "(#0)+1"))
(DEFSYSFUN '1- "Lone_minus" '(T) T NIL NIL
  ':inline-always '((t) t nil t "one_minus(#0)")
  ':inline-always '((fixnum-float) long-float nil nil "(double)(#0)-1")
  ':inline-always '((fixnum-float) short-float nil nil "(float)(#0)-1")
  ':inline-always '((fixnum) fixnum nil nil "(#0)-1"))
(DEFSYSFUN 'CONJUGATE "Lconjugate" '(T) T NIL NIL)
(DEFSYSFUN 'GCD "Lgcd" '(*) T NIL NIL)
(DEFSYSFUN 'LCM "Llcm" '(T *) T NIL NIL)

; file num_co.c
(DEFSYSFUN 'FLOAT "Lfloat" '(T *) T NIL NIL
  ':inline-always '((T) short-float nil nil "(sf((Lfloat(#0),VALUES(0))))")
  ':inline-always '((fixnum-float) long-float nil nil "((double)(#0))")
  ':inline-always '((fixnum-float) short-float nil nil "((float)(#0))"))
(DEFSYSFUN 'NUMERATOR "Lnumerator" '(T) T NIL NIL)
(DEFSYSFUN 'DENOMINATOR "Ldenominator" '(T) T NIL NIL)
(DEFSYSFUN 'FLOOR "Lfloor" '(T *) '(VALUES T T) NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil
		   "@01;(#0>=0&&#1>0?(#0)/(#1):ifloor(#0,#1))"))
(DEFSYSFUN 'CEILING "Lceiling" '(T *) '(VALUES T T) NIL NIL)
(DEFSYSFUN 'TRUNCATE "Ltruncate" '(T *) '(VALUES T T) NIL NIL
  ':inline-always '((fixnum-float) fixnum nil nil "(fixnum)(#0)"))
(DEFSYSFUN 'ROUND "Lround" '(T *) '(VALUES T T) NIL NIL)
(DEFSYSFUN 'MOD "Lmod" '(T T) T NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil
		   "@01;(#0>=0&&#1>0?(#0)%(#1):imod(#0,#1))"))
(DEFSYSFUN 'REM "Lrem" '(T T) T NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil "(#0)%(#1)"))
(DEFSYSFUN 'DECODE-FLOAT "Ldecode_float" '(T) '(VALUES T T T) NIL NIL)
(DEFSYSFUN 'SCALE-FLOAT "Lscale_float" '(T T) T NIL NIL)
(DEFSYSFUN 'FLOAT-RADIX "Lfloat_radix" '(T) 'FIXNUM NIL NIL)
(DEFSYSFUN 'FLOAT-SIGN "Lfloat_sign" '(T *) T NIL NIL)
(DEFSYSFUN 'FLOAT-DIGITS "Lfloat_digits" '(T) 'FIXNUM NIL NIL)
(DEFSYSFUN 'FLOAT-PRECISION "Lfloat_precision" '(T) 'FIXNUM NIL NIL)
(DEFSYSFUN 'INTEGER-DECODE-FLOAT "Linteger_decode_float" '(T)
    '(VALUES T T T) NIL NIL)
(DEFSYSFUN 'COMPLEX "Lcomplex" '(T *) T NIL NIL)
(DEFSYSFUN 'REALPART "Lrealpart" '(T) T NIL NIL)
(DEFSYSFUN 'IMAGPART "Limagpart" '(T) T NIL NIL)
(DEFSYSFUN '= "Lall_the_same" '(T *) T NIL T
  ':inline-always '((t t) boolean nil nil "number_compare(#0,#1)==0")
  ':inline-always '((fixnum-float fixnum-float) boolean nil nil "(#0)==(#1)"))
(defsysfun '/= "Lall_different" '(T *) T nil t
  ':inline-always '((t t) boolean nil nil "number_compare(#0,#1)!=0")
  ':inline-always '((fixnum-float fixnum-float) boolean nil nil "(#0)!=(#1)"))
(defsysfun '< "Lmonotonically_increasing" '(T *) T nil t
  ':inline-always '((t t) boolean nil nil "number_compare(#0,#1)<0")
  ':inline-always '((fixnum-float fixnum-float) boolean nil nil "(#0)<(#1)"))
(defsysfun '> "Lmonotonically_decreasing" '(T *) T nil t
  ':inline-always '((t t) boolean nil nil "number_compare(#0,#1)>0")
  ':inline-always '((fixnum-float fixnum-float) boolean nil nil "(#0)>(#1)"))
(defsysfun '<= "Lmonotonically_nondecreasing" '(T *) T nil t
  ':inline-always '((t t) boolean nil nil "number_compare(#0,#1)<=0")
  ':inline-always '((fixnum-float fixnum-float) boolean nil nil "(#0)<=(#1)"))
(defsysfun '>= "Lmonotonically_nonincreasing" '(T *) T nil t
  ':inline-always '((t t) boolean nil nil "number_compare(#0,#1)>=0")
  ':inline-always '((fixnum-float fixnum-float) boolean nil nil "(#0)>=(#1)"))
(DEFSYSFUN 'MAX "Lmax" '(T *) T NIL NIL
  ':inline-always '((t t) t nil nil "@01;(number_compare(#0,#1)>=0?#0:#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil "@01;(#0)>=(#1)?#0:#1"))
(DEFSYSFUN 'MIN "Lmin" '(T *) T NIL NIL
  ':inline-always '((t t) t nil nil "@01;(number_compare(#0,#1)<=0?#0:#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil "@01;(#0)<=(#1)?#0:#1"))
(DEFSYSFUN 'LOGIOR "Llogior" '(*) T NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil "((#0) | (#1))"))
(DEFSYSFUN 'LOGXOR "Llogxor" '(*) T NIL NIL)
(DEFSYSFUN 'LOGAND "Llogand" '(*) T NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil "((#0) & (#1))"))
(DEFSYSFUN 'LOGEQV "Llogeqv" '(*) T NIL NIL)
(DEFSYSFUN 'BOOLE "Lboole" '(T T T) T NIL NIL)
(DEFSYSFUN 'LOGBITP "Llogbitp" '(T T) T NIL T
  ':inline-always '((fixnum fixnum) boolean nil nil "(#1 >> #0) & 1"))
(DEFSYSFUN 'ASH "Lash" '(T T) T NIL NIL)
(DEFSYSFUN 'LOGCOUNT "Llogcount" '(T) T NIL NIL)
(DEFSYSFUN 'INTEGER-LENGTH "Linteger_length" '(T) 'FIXNUM NIL NIL)
(defsysfun 'si::BIT-ARRAY-OP "siLbit_array_op" nil T nil nil)
(DEFSYSFUN 'ZEROP "Lzerop" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)==0")
  ':inline-always '((fixnum-float) boolean nil nil "(#0)==0"))
(DEFSYSFUN 'PLUSP "Lplusp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)<0")
  ':inline-always '((fixnum-float) boolean nil nil "(#0)>0"))
(DEFSYSFUN 'MINUSP "Lminusp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)>0")
  ':inline-always '((fixnum-float) boolean nil nil "(#0)<0"))
(DEFSYSFUN 'ODDP "Loddp" '(T) T NIL T
  ':inline-always '((fixnum fixnum) boolean nil nil "(#0) & 1"))
(DEFSYSFUN 'EVENP "Levenp" '(T) T NIL T
  ':inline-always '((fixnum fixnum) boolean nil nil "~(#0) & 1"))
(DEFSYSFUN 'RANDOM "Lrandom" '(T *) T NIL NIL)
(DEFSYSFUN 'MAKE-RANDOM-STATE "Lmake_random_state" '(*) T NIL NIL)
(DEFSYSFUN 'RANDOM-STATE-P "Lrandom_state_p" '(T) T NIL T)
(DEFSYSFUN 'EXP "Lexp" '(T) T NIL NIL
  ':inline-always '((number) t nil t "number_exp(#0)"))
(DEFSYSFUN 'EXPT "Lexpt" '(T T) T NIL NIL
  ':inline-always '((t t) t nil t "number_expt(#0,#1)")
  ':inline-always '((fixnum fixnum) fixnum nil nil
        (lambda (loc1 loc2)
	    (if (and (consp loc1) (eq (car loc1) 'fixnum)
		     (consp (cadr loc1)) (eq (caadr loc1) 'fixnum-value)
		     (eq (cadr (cadr loc1)) 2))
		(progn (wt1 "(1<<(") (wt1 loc2) (wt1 "))"))
		(progn (wt1 "fixnum_expt(") (wt1 loc1) (wt1 #\,) (wt1 loc2)
		       (wt1 #\) ))))))
(DEFSYSFUN 'LOG "Llog" '(T *) T NIL NIL
  ':inline-always '((fixnum-float) long-float nil t "log((double)(#0))")
  ':inline-always '((fixnum-float) short-float nil nil
		   "(float)log((double)(#0))"))
(DEFSYSFUN 'SQRT "Lsqrt" '(T) T NIL NIL
  ':inline-always '((fixnum-float) long-float nil t "sqrt((double)(#0))")
  ':inline-always '((fixnum-float) short-float nil nil
		   "(float)sqrt((double)(#0))"))
(DEFSYSFUN 'SIN "Lsin" '(T) T NIL NIL
  ':inline-always '((fixnum-float) long-float nil nil "sin((double)(#0))")
  ':inline-always '((fixnum-float) short-float nil nil
		   "(float)sin((double)(#0))"))
(DEFSYSFUN 'COS "Lcos" '(T) T NIL NIL
  ':inline-always '((fixnum-float) long-float nil nil "cos((double)(#0))")
  ':inline-always '((fixnum-float) short-float nil nil
		   "(float)cos((double)(#0))"))
(defsysfun 'tan "Ltan" '(number) 'number nil nil
  ':inline-always '((fixnum-float) long-float nil nil "tan((double)(#0))")
  ':inline-always '((fixnum-float) short-float nil nil
		   "(float)cos((tan)(#0))"))
(DEFSYSFUN 'ATAN "Latan" '(T *) T NIL NIL)

; file package.d
(DEFSYSFUN 'MAKE-PACKAGE "Lmake_package" '(T *) T NIL NIL)
(DEFSYSFUN 'IN-PACKAGE "Lin_package" '(T *) T NIL NIL)
(DEFSYSFUN 'FIND-PACKAGE "Lfind_package" '(T) T NIL NIL)
(DEFSYSFUN 'PACKAGE-NAME "Lpackage_name" '(T) T NIL NIL)
(DEFSYSFUN 'PACKAGE-NICKNAMES "Lpackage_nicknames" '(T) T NIL NIL)
(DEFSYSFUN 'RENAME-PACKAGE "Lrename_package" '(T T *) T NIL NIL)
(DEFSYSFUN 'PACKAGE-USE-LIST "Lpackage_use_list" '(T) T NIL NIL)
(DEFSYSFUN 'PACKAGE-USED-BY-LIST "Lpackage_used_by_list" '(T) T NIL NIL)
(defsysfun 'PACKAGE-SHADOWING-SYMBOLS "Lpackage_shadowing_symbols" '(T) T
 nil nil)
(DEFSYSFUN 'LIST-ALL-PACKAGES "Llist_all_packages" 'NIL T NIL NIL)
(DEFSYSFUN 'INTERN "Lintern" '(string *) '(VALUES T T) NIL NIL)
(DEFSYSFUN 'FIND-SYMBOL "Lfind_symbol" '(string *) '(VALUES T T) NIL NIL)
(defsysfun 'UNINTERN "Lunintern" '(symbol t) T nil nil)
(DEFSYSFUN 'EXPORT "Lexport" '(T *) T NIL NIL)
(DEFSYSFUN 'UNEXPORT "Lunexport" '(T *) T NIL NIL)
(DEFSYSFUN 'IMPORT "Limport" '(T *) T NIL NIL)
(DEFSYSFUN 'SHADOWING-IMPORT "Lshadowing_import" '(T *) T NIL NIL)
(DEFSYSFUN 'SHADOW "Lshadow" '(T *) T NIL NIL)
(DEFSYSFUN 'USE-PACKAGE "Luse_package" '(T *) T NIL NIL)
(DEFSYSFUN 'UNUSE-PACKAGE "Lunuse_package" '(T *) T NIL NIL)
(defsysfun 'si::PACKAGE-INTERNAL "siLpackage_internal" nil T nil nil)
(defsysfun 'si::PACKAGE-EXTERNAL "siLpackage_external" nil T nil nil)
(defsysfun 'PATHNAME "Lpathname" '(T) T nil nil)
(DEFSYSFUN 'PARSE-NAMESTRING "Lparse_namestring" '(T *) T NIL NIL)
(DEFSYSFUN 'MERGE-PATHNAMES "Lmerge_pathnames" '(T *) T NIL NIL)
(DEFSYSFUN 'MAKE-PATHNAME "Lmake_pathname" '(*) T NIL NIL)
(DEFSYSFUN 'PATHNAMEP "Lpathnamep" '(T) T NIL T)
(DEFSYSFUN 'PATHNAME-HOST "Lpathname_host" '(T) T NIL NIL)
(DEFSYSFUN 'PATHNAME-DEVICE "Lpathname_device" '(T) T NIL NIL)
(DEFSYSFUN 'PATHNAME-DIRECTORY "Lpathname_directory" '(T) T NIL NIL)
(DEFSYSFUN 'PATHNAME-NAME "Lpathname_name" '(T) T NIL NIL)
(DEFSYSFUN 'PATHNAME-TYPE "Lpathname_type" '(T) T NIL NIL)
(DEFSYSFUN 'PATHNAME-VERSION "Lpathname_version" '(T) T NIL NIL)
(DEFSYSFUN 'NAMESTRING "Lnamestring" '(T) 'string NIL NIL
  ':inline-always '((t) t nil t "coerce_to_namestring(#0)"))
(DEFSYSFUN 'FILE-NAMESTRING "Lfile_namestring" '(T) 'STRING NIL NIL)
(DEFSYSFUN 'DIRECTORY-NAMESTRING "Ldirectory_namestring" '(T) 'STRING
    NIL NIL)
(DEFSYSFUN 'HOST-NAMESTRING "Lhost_namestring" '(T) 'STRING NIL NIL)
(DEFSYSFUN 'ENOUGH-NAMESTRING "Lenough_namestring" '(T *) 'STRING NIL NIL)
(DEFSYSFUN 'NULL "Lnull" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "#0==Cnil"))
(DEFSYSFUN 'SYMBOLP "Lsymbolp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "type_of(#0)==t_symbol"))
(DEFSYSFUN 'ATOM "Latom" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "type_of(#0)!=t_cons"))
(DEFSYSFUN 'CONSP "Lconsp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "type_of(#0)==t_cons"))
(DEFSYSFUN 'LISTP "Llistp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "@0;type_of(#0)==t_cons||#0==Cnil"))
(DEFSYSFUN 'NUMBERP "Lnumberp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "numberp(#0)"))
(DEFSYSFUN 'INTEGERP "Lintegerp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil
        "@0;type_of(#0)==t_fixnum||type_of(#0)==t_bignum"))
(defsysfun 'RATIONAL "Lrationalp" '(T) T nil t)
(DEFSYSFUN 'FLOATP "Lfloatp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil
		   "@0;type_of(#0)==t_shortfloat||type_of(#0)==t_longfloat"))
(DEFSYSFUN 'COMPLEXP "Lcomplexp" '(T) T NIL T)
(DEFSYSFUN 'CHARACTERP "Lcharacterp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "CHARACTERP(#0)"))
(DEFSYSFUN 'STRINGP "Lstringp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "type_of(#0)==t_string"))
(DEFSYSFUN 'BIT-VECTOR-P "Lbit_vector_p" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "(type_of(#0)==t_bitvector)"))
(DEFSYSFUN 'VECTORP "Lvectorp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil
		   "@0;type_of(#0)==t_vector||
type_of(#0)==t_string||
type_of(#0)==t_bitvector"))
(DEFSYSFUN 'SIMPLE-STRING-P "Lsimple_string_p" '(T) T NIL T)
(DEFSYSFUN 'SIMPLE-BIT-VECTOR-P "Lsimple_bit_vector_p" '(T) T NIL T)
(DEFSYSFUN 'SIMPLE-VECTOR-P "Lsimple_vector_p" '(T) T NIL T)
(DEFSYSFUN 'ARRAYP "Larrayp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil
		   "@0;ARRAYP(#0)"))
(DEFSYSFUN 'PACKAGEP "Lpackagep" '(T) T NIL T)
(DEFSYSFUN 'FUNCTIONP "Lfunctionp" '(T) T NIL T)
(DEFSYSFUN 'COMPILED-FUNCTION-P "Lcompiled_function_p" '(T) T NIL T)
#-ansi
(DEFSYSFUN 'COMMONP "Lcommonp" '(T) T NIL T)
(DEFSYSFUN 'EQ "Leq" '(T T) T NIL T
  ':inline-always '((t t) boolean nil nil "(#0)==(#1)")
  ':inline-always '((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(DEFSYSFUN 'EQL "Leql" '(T T) T NIL T
  ':inline-always '((t t) boolean nil nil "eql(#0,#1)")
  ':inline-always '((character t) boolean nil nil	; Beppe
		   "(CHARACTERP(#1) && (#0)==char_code(#1))")
  ':inline-always '((t character) boolean nil nil	; Beppe
		   "(CHARACTERP(#0) && char_code(#0)==(#1))")
  ':inline-always '((character character) boolean nil nil "(#0)==(#1)")
  ':inline-always '((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(defsysfun 'EQUAL "Lequal" '(T T) T nil t
  ':inline-always '((t t) boolean nil nil "equal(#0,#1)")
  ':inline-always '((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(DEFSYSFUN 'EQUALP "Lequalp" '(T T) T NIL T
  ':inline-always '((t t) boolean nil nil "equalp(#0,#1)")
  ':inline-always '((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(DEFSYSFUN 'NOT "Lnull" '(T) T NIL T
  ':inline-always '((t) boolean nil nil "(#0)==Cnil"))

; file print.d
(DEFSYSFUN 'CLEAR-OUTPUT "Lclear_output" '(*) T NIL NIL)
(DEFSYSFUN 'FINISH-OUTPUT "Lfinish_output" '(*) T NIL NIL)
(DEFSYSFUN 'FORCE-OUTPUT "Lforce_output" '(*) T NIL NIL)
(DEFSYSFUN 'FRESH-LINE "Lfresh_line" '(*) T NIL NIL)
(DEFSYSFUN 'LISTEN "Llisten" '(*) T NIL NIL)
(DEFSYSFUN 'PEEK-CHAR "Lpeek_char" '(*) T NIL NIL)
(DEFSYSFUN 'PPRINT "Lpprint" '(T *) T NIL NIL)
(DEFSYSFUN 'PRIN1 "Lprin1" '(T *) T NIL NIL
  ':inline-always '((t t) t t nil "prin1(#0,#1)")
  ':inline-always '((t) t t nil "prin1(#0,Cnil)"))
(DEFSYSFUN 'PRINC "Lprinc" '(T *) T NIL NIL
  ':inline-always '((t t) t t nil "princ(#0,#1)")
  ':inline-always '((t) t t nil "princ(#0,Cnil)"))
(DEFSYSFUN 'PRINT "Lprint" '(T *) T NIL NIL
  ':inline-always '((t t) t t nil "print(#0,#1)")
  ':inline-always '((t) t t nil "print(#0,Cnil)"))
(DEFSYSFUN 'PROBE-FILE "Lprobe_file" '(T) T NIL T
	   ':inline-always
	   '((t) boolean nil nil "(file_exists(#0))"))
(DEFSYSFUN 'UNREAD-CHAR "Lunread_char" '(T *) T NIL NIL)
(DEFSYSFUN 'READ "Lread" '(*) T NIL NIL)
(DEFSYSFUN 'READ-CHAR "Lread_char" '(*) T NIL NIL)
(DEFSYSFUN 'READ-DELIMITED-LIST "Lread_delimited_list" '(T *) T NIL NIL)
(DEFSYSFUN 'READ-LINE "Lread_line" '(*) '(VALUES T T) NIL NIL)
(defsysfun 'READ-PRESERVING-WHITESPACE "Lread_preserving_whitespace" nil T
  nil nil)
(DEFSYSFUN 'TERPRI "Lterpri" '(*) T NIL T
  ':inline-always '((t) t t nil "terpri(#0)")
  ':inline-always '(nil t t nil "terpri(Cnil)"))
(DEFSYSFUN 'WRITE "Lwrite" '(T *) T NIL NIL)
(defsysfun 'WRITE-BYTE "Lwrite_byte" '(fixnum stream) T nil nil)
(defsysfun 'si::WRITE-BYTES "Lwrite_bytes"
  '(stream string fixnum fixnum) T nil nil)
(DEFSYSFUN 'WRITE-CHAR "Lwrite_char" '(T *) T NIL NIL
  ':inline-unsafe '((t) t t nil "@0;(princ_char(char_code(#0),Cnil),(#0))"))
(DEFSYSFUN 'WRITE-LINE "Lwrite_line" '(T *) T NIL NIL)
(DEFSYSFUN 'WRITE-STRING "Lwrite_string" '(T *) T NIL NIL)
(DEFSYSFUN 'READ-CHAR-NO-HANG "Lread_char_no_hang" '(*) T NIL NIL)
(DEFSYSFUN 'CLEAR-INPUT "Lclear_input" '(*) T NIL NIL)
(DEFSYSFUN 'PARSE-INTEGER "Lparse_integer" '(T *) T NIL NIL)
(DEFSYSFUN 'READ-BYTE "Lread_byte" '(T *) T NIL NIL)
(defsysfun 'si::READ-BYTES "Lread_bytes" '(stream string fixnum fixnum) T
  nil nil)
(DEFSYSFUN 'COPY-READTABLE "Lcopy_readtable" '(*) T NIL NIL
  ':inline-always '((null null) t nil nil "standard_readtable"))
(DEFSYSFUN 'READTABLEP "Lreadtablep" '(T) T NIL T)
(DEFSYSFUN 'SET-SYNTAX-FROM-CHAR "Lset_syntax_from_char" '(T T *) T NIL NIL)
(DEFSYSFUN 'SET-MACRO-CHARACTER "Lset_macro_character" '(T T *) T NIL NIL)
(DEFSYSFUN 'GET-MACRO-CHARACTER "Lget_macro_character" '(T *) T NIL NIL)
(defsysfun 'MAKE-DISPATCH-MACRO-CHARACTER "Lmake_dispatch_macro_character" nil
  T nil nil)
(defsysfun 'SET-DISPATCH-MACRO-CHARACTER "Lset_dispatch_macro_character" nil
  T nil nil)
(defsysfun 'GET-DISPATCH-MACRO-CHARACTER "Lget_dispatch_macro_character" nil
  T nil nil)
(DEFSYSFUN 'SI::STRING-TO-OBJECT "siLstring_to_object" '(T) T NIL NIL)
(defsysfun 'si::STANDARD-READTABLE "siLstandard_readtable" '(T) T nil nil)
(DEFSYSFUN 'SYMBOL-FUNCTION "Lsymbol_function" '(T) T NIL NIL
  ':inline-always '((t) t nil t "symbol_function(#0)"))
(defsysfun 'FBOUNDP "Lfboundp" '(symbol) T nil t)
(defsysfun 'SYMBOL-VALUE "Lsymbol_value" '(symbol) T nil nil)
(defsysfun 'BOUNDP "Lboundp" '(symbol) T nil t
  ':inline-unsafe '((t) boolean nil nil "(#0)->s.s_dbind!=OBJNULL"))
(defsysfun 'MACRO-FUNCTION "Lmacro_function" '(symbol) T nil nil)
(defsysfun 'SPECIAL-FORM-P "Lspecial_form_p" '(symbol) T nil t)

; file unixsave.c
(DEFSYSFUN 'SAVE "Lsave" '(T) T NIL NIL)

; file unixsys.c
(defsysfun 'SYSTEM "Lsystem" nil T nil nil)

; file sequence.d
(defsysfun 'ELT "Lelt" '(sequence fixnum) T nil nil
  ':inline-always '((t t) t nil t "elt(#0,fixint(#1))")
  ':inline-always '((t fixnum) t nil t "elt(#0,#1)")
  ':inline-unsafe '((t t) t nil t "elt(#0,fix(#1))")
  ':inline-unsafe '((t fixnum) t nil t "elt(#0,#1)"))
(defsysfun 'si::ELT-SET "siLelt_set" '(sequence fixnum t) T nil nil
  ':inline-always '((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
  ':inline-always '((t fixnum t) t t nil "elt_set(#0,#1,#2)")
  ':inline-unsafe '((t t t) t t nil "elt_set(#0,fix(#1),#2)"))
(defsysfun 'SUBSEQ "Lsubseq" '(sequence fixnum *) 'sequence nil nil)
(defsysfun 'COPY-SEQ "Lcopy_seq" '(sequence) 'sequence nil nil)
(defsysfun 'LENGTH "Llength" '(sequence) 'fixnum t nil
  ':inline-always '((t) fixnum nil nil "length(#0)")
  ':inline-unsafe '(((array t)) fixnum nil nil "(#0)->v.v_fillp")
  ':inline-unsafe '((string) fixnum nil nil "(#0)->v.v_fillp"))
(defsysfun 'REVERSE "Lreverse" '(sequence) 'sequence nil nil
  ':inline-always '((t) t nil t "reverse(#0)"))
(defsysfun 'NREVERSE "Lnreverse" '(sequence) 'sequence nil nil
  ':inline-always '((t) t t t "nreverse(#0)"))

; file character.d
(defsysfun 'CHAR "Lchar" '(string fixnum) 'character nil nil
  ':inline-always '((t t) t nil t "elt(#0,fixint(#1))")
  ':inline-always '((t fixnum) t nil t "elt(#0,#1)")
  ':inline-unsafe '((t t) t nil nil "code_char((#0)->ust.ust_self[fix(#1)])")
  ':inline-unsafe '((t fixnum) fixnum nil nil "(#0)->ust.ust_self[#1]")
  ':inline-unsafe '((t fixnum) character nil nil "(#0)->ust.ust_self[#1]"))
(defsysfun 'si::CHAR-SET "siLchar_set"
  '(string fixnum character) 'character nil nil
  ':inline-always '((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
  ':inline-always '((t fixnum t) t t nil "elt_set(#0,#1,#2)")
  ':inline-unsafe '((t t t) t t nil
		   "@2;((#0)->ust.ust_self[fix(#1)]=char_code(#2),(#2))")
  ':inline-unsafe '((t fixnum character) character t nil
		   "(#0)->ust.ust_self[#1]= #2"))
(defsysfun 'SCHAR "Lchar" '(string fixnum) 'character nil nil
  ':inline-always '((t t) t nil t "elt(#0,fixint(#1))")
  ':inline-always '((t fixnum) t nil t "elt(#0,#1)")
  ':inline-unsafe '((t t) t nil nil "code_char((#0)->ust.ust_self[fix(#1)])")
  ':inline-unsafe '((t t) fixnum nil nil "(#0)->ust.ust_self[fix(#1)]")
  ':inline-unsafe '((t fixnum) fixnum nil nil "(#0)->ust.ust_self[#1]")
  ':inline-unsafe '((t fixnum) character nil nil "(#0)->ust.ust_self[#1]"))
(defsysfun 'si::SCHAR-SET "siLchar_set"
  '(string fixnum character) 'character nil nil
  ':inline-always '((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
  ':inline-always '((t fixnum t) t t nil "elt_set(#0,#1,#2)")
  ':inline-unsafe '((t t t) t t nil
		   "@2;((#0)->ust.ust_self[fix(#1)]=char_code(#2),(#2))")
  ':inline-unsafe '((t fixnum character) character t nil
		   "(#0)->ust.ust_self[#1]= #2"))
(defsysfun 'STRING= "Lstring_eq" '(string string *) T nil t
  ':inline-always '((string  string) boolean nil nil "string_eq(#0,#1)"))
(defsysfun 'STRING-EQUAL "Lstring_equal" '(string string *) T nil t
  ':inline-always '((string  string) boolean nil nil "string_equal(#0,#1)"))
(defsysfun 'STRING< "Lstring_l" '(string string *) T nil t)
(defsysfun 'STRING> "Lstring_g" '(string string *) T nil t)
(defsysfun 'STRING<= "Lstring_le" '(string string *) T nil t)
(defsysfun 'STRING>= "Lstring_ge" '(string string *) T nil t)
(defsysfun 'STRING/= "Lstring_neq" '(string string *) T nil t)
(defsysfun 'STRING-LESSP "Lstring_lessp" '(string string *) T nil t)
(defsysfun 'STRING-GREATERP "Lstring_greaterp" '(string string *) T nil t)
(defsysfun 'STRING-NOT-LESSP "Lstring_not_lessp" '(string string *) T nil t)
(defsysfun 'STRING-NOT-GREATERP "Lstring_not_greaterp" '(string string *) T
  nil t)
(defsysfun 'STRING-NOT-EQUAL "Lstring_not_equal" '(string string *) T nil t)
(defsysfun 'MAKE-STRING "Lmake_string" '(fixnum *) 'string nil nil)
(defsysfun 'STRING-TRIM "Lstring_trim" '(t string) 'string nil nil)
(defsysfun 'STRING-LEFT-TRIM "Lstring_left_trim" '(t string) 'string nil nil)
(defsysfun 'STRING-RIGHT-TRIM "Lstring_right_trim" '(t string) 'string nil nil)
(defsysfun 'STRING-UPCASE "Lstring_upcase" '(string *) 'string nil nil)
(defsysfun 'STRING-DOWNCASE "Lstring_downcase" '(string *) 'string nil nil)
(defsysfun 'STRING-CAPITALIZE "Lstring_capitalize" '(string *) 'string nil nil)
(defsysfun 'NSTRING-UPCASE "Lnstring_upcase" '(string *) 'string nil nil)
(defsysfun 'NSTRING-DOWNCASE "Lnstring_downcase" '(string *) 'string nil nil)
(defsysfun 'NSTRING-CAPITALIZE "Lnstring_capitalize" '(string *) 'string
  nil nil)
(defsysfun 'STRING "Lstring" '(T) 'string nil t
  ':inline-always '((t) t nil nil "coerce_to_string(#0)"))
(defsysfun 'STRING-CONCATENATE "siLstring_concatenate" '(T) 'string nil nil)

; file structure.d
(defsysfun 'si::MAKE-STRUCTURE "siLmake_structure" '(T *) T nil nil)
(defsysfun 'si::COPY-STRUCTURE "siLcopy_structure" '(T T) T nil nil)
(DEFSYSFUN 'SI::STRUCTURE-NAME "siLstructure_name" '(T) 'SYMBOL NIL NIL
  ':inline-always '((structure) symbol nil nil "SNAME(#0)"))
(defsysfun 'si::STRUCTURE-REF "siLstructure_ref" '(t t fixnum) T nil nil
   ':inline-always '((t t fixnum) t nil nil "structure_ref(#0,#1,#2)"))
(defsysfun 'si::STRUCTURE-SET "siLstructure_set" '(t t fixnum t) T nil nil
   ':inline-always '((t t fixnum t) t T nil "structure_set(#0,#1,#2,#3)"))
(DEFSYSFUN 'SI::STRUCTUREP "siLstructurep" '(T) T NIL T
   ':inline-always '((t) boolean nil nil "type_of(#0)==t_structure"))
(DEFSYSFUN 'SI::STRUCTURE-SUBTYPE-P "siLstructure_subtype_p" '(T T) T NIL T)
(defsysfun 'si::RPLACA-NTHCDR "siLrplaca_nthcdr" '(T T T) nil T nil t)
(defsysfun 'si::LIST-NTH "siLlist_nth" '(T T) T nil t)

; file toplevel.c
(defsysfun 'si::*MAKE-SPECIAL "siLAmake_special" nil T nil nil)
(defsysfun 'si::*MAKE-CONSTANT "siLAmake_constant" nil T nil nil)

; file symbol.d
(defsysfun 'GET "Lget" '(symbol t *) T nil nil
  ':inline-always '((t t t) t nil nil "get(#0,#1,#2)")
  ':inline-always '((t t) t nil nil "get(#0,#1,Cnil)")
  ':inline-unsafe '((t t t) t nil nil "getf(#0->s.s_plist,#1,#2)")
  ':inline-unsafe '((t t) t nil nil "getf(#0->s.s_plist,#1,Cnil)"))
(defsysfun 'REMPROP "Lremprop" '(symbol t) T nil nil
  ':inline-always '((t t) t t nil "remprop(#0,#1)"))
(defsysfun 'SYMBOL-PLIST "Lsymbol_plist" '(symbol) T nil T
  ':inline-always '((t) t nil nil "((#0)->s.s_plist)"))
(DEFSYSFUN 'GETF "Lgetf" '(T T *) T NIL NIL)
(DEFSYSFUN 'GET-PROPERTIES "Lget_properties" '(T T) '* NIL NIL)
(defsysfun 'SYMBOL-NAME "Lsymbol_name" '(symbol) 'string nil nil
  ':inline-always '((t) t nil t "symbol_name(#0)"))
(defsysfun 'MAKE-SYMBOL "Lmake_symbol" '(string) 'symbol nil nil)
(defsysfun 'COPY-SYMBOL "Lcopy_symbol" '(symbol *) 'symbol nil nil)
(defsysfun 'GENSYM "Lgensym" '(*) 'symbol nil nil)
(defsysfun 'GENTEMP "Lgentemp" '(*) 'symbol nil nil)
(defsysfun 'SYMBOL-PACKAGE "Lsymbol_package" '(symbol) T nil nil)
(DEFSYSFUN 'KEYWORDP "Lkeywordp" '(T) T NIL T
  ':inline-always '((t) boolean nil nil
        "@0;(type_of(#0)==t_symbol&&(#0)->s.s_hpack==keyword_package)"))
(DEFSYSFUN 'SI::PUT-F "siLput_f" NIL '(T T) NIL NIL)
(DEFSYSFUN 'SI::REM-F "siLrem_f" NIL '(T T) NIL NIL)
(defsysfun 'si::SET-SYMBOL-PLIST "siLset_symbol_plist" '(symbol t) T nil nil)
(DEFSYSFUN 'SI::PUTPROP "siLputprop" '(T T T) T NIL NIL
  ':inline-always '((t t t) t t nil "putprop(#0,#1,#2)"))

; file tcp.c
(defsysfun 'si::OPEN-TCP-STREAM "Lopen_tcp_stream" '(T T) T nil nil)

; file unixfasl.c
(defsysfun 'si::READ-EXTERNALS "siLread_externals" nil T nil nil)
(defsysfun 'si::SET-UP-COMBINED "siLset_up_combined" nil T nil nil)
(defsysfun 'si::BUILD-SYMBOL-TABLE "siLbuild_symbol_table" nil T nil nil)
#+bsd
(defsysfun 'si::FASLINK "siLfaslink" nil T nil nil)

; file unixtime.c
(defsysfun 'si::DAYLIGHT-SAVING-TIME-P "Ldaylight_saving_timep" nil T nil t)
(defsysfun 'GET-UNIVERSAL-TIME "Lget_universal_time" nil T nil nil)
(defsysfun 'GET-INTERNAL-RUN-TIME "Lget_internal_run_time" nil T nil nil)
(defsysfun 'GET-INTERNAL-REAL-TIME "Lget_internal_real_time" nil T nil nil)
(defsysfun 'si::GET-LOCAL-TIME-ZONE "Lget_local_time_zone" nil T nil nil)
(defsysfun 'SLEEP "Lsleep" '(fixnum) T nil nil)

(DEFSYSFUN 'TYPE-OF "Ltype_of" '(T) T NIL NIL
  ':inline-always '((t) t nil t "TYPE_OF(#0)"))

;;; Beppe's additions
(defsysfun 'READ-BYTES "Lread_bytes" '(stream string fixnum fixnum) T nil nil)
(defsysfun 'WRITE-BYTES "Lwrite_bytes" '(stream string fixnum fixnum) T
  nil nil)

; file instance.c
#+clos
(progn
(defsysfun 'si::ALLOCATE-INSTANCE "siLallocate_instance" '(t fixnum) T nil nil)
(defsysfun 'si::INSTANCE-REF "siLinstance_ref" '(t fixnum) T nil nil
  ':inline-always '((standard-object fixnum) t nil nil "(#0)->in.in_slots[#1]"))
(defsysfun 'si::INSTANCE-SET "siLinstance_set" '(t fixnum t) T nil nil
  ':inline-always '((standard-object fixnum t) t t nil
		   "(#0)->in.in_slots[#1]=(#2)"))
(defsysfun 'si::INSTANCE-CLASS "siLinstance_class" '(t) T nil nil
  ':inline-always '((standard-object) t nil nil "(#0)->in.in_class"))
(defsysfun 'si::INSTANCE-CLASS-SET "siLinstance_class_set" '(t t) T nil nil)
(defsysfun 'si::INSTANCEP "siLinstancep" '(t) T nil t)
(defsysfun 'si::SL-BOUNDP "siLsl_boundp" '(t) T nil t
  ':inline-always '((t) boolean nil nil "(#0)!=OBJNULL"))
(defsysfun 'si::SL-MAKUNBOUND "siLsl_makunbound" '(t fixnum) T nil t)

; file gfun.c
(defsysfun 'si::ALLOCATE-GFUN "siLallocate_gfun" nil T nil nil)
(defsysfun 'si::GFUN-NAME  "siLgfun_name" nil T nil nil)
(defsysfun 'si::GFUN-NAME-SET "siLgfun_name_set" nil T nil nil)
(defsysfun 'si::GFUN-METHOD-HT "siLgfun_method_ht" nil T nil nil)
(defsysfun 'si::GFUN-METHOD-HT-SET "siLgfun_method_ht_set" nil T nil nil)
(defsysfun 'si::GFUN-SPEC-HOW-REF  "siLgfun_spec_how_ref" nil T nil nil)
(defsysfun 'si::GFUN-SPEC-HOW-SET "siLgfun_spec_how_set" nil T nil nil)
(defsysfun 'si::GFUN-INSTANCE  "siLgfun_instance" nil T nil nil)
(defsysfun 'si::GFUN-INSTANCE-SET "siLgfun_instance_set" nil T nil nil)
(defsysfun 'si::GFUNP "siLgfunp" nil T nil nil)
)

;;; AKCL additions:
(DEFSYSFUN 'SI::COPY-STREAM "siLcopy_stream" '(T T) T NIL NIL)

;; file numlib.lsp:
(DEFSYSFUN 'LOGNOT nil nil nil NIL NIL
  ':inline-always '((fixnum) fixnum nil nil "(~(#0))"))

;;; file cmpfun.lsp:
;;; The following functions are introduced by the compiler in pass 1 

(DEFSYSFUN 'shift>> nil nil nil NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil "((#0) >> (- (#1)))"))
(DEFSYSFUN 'shift<< nil nil nil NIL NIL
  ':inline-always '((fixnum fixnum) fixnum nil nil "((#0) << (#1))"))
(DEFSYSFUN 'symbol-length nil nil nil NIL NIL
  ':inline-always
  '((t) fixnum nil nil
    "@0;(type_of(#0)==t_symbol ? (#0)->s.st_fillp :not_a_variable((#0)))"))

(DEFSYSFUN 'short-float-p nil nil nil T T
  ':inline-always '((t) boolean nil nil "type_of(#0)==t_shortfloat"))
(DEFSYSFUN 'long-float-p nil nil nil T T
  ':inline-always '((t) boolean nil nil "type_of(#0)==t_longfloat"))
(DEFSYSFUN 'si:fixnump nil nil nil T T
  ':inline-always '((t) boolean nil nil "FIXNUMP(#0)")
  ':inline-always '((fixnum) boolean nil nil "1"))
(DEFSYSFUN 'si::put-properties "siLput_properties" '(*) nil T nil)

;;; Prolog:
(defsysfun 'si::trail-mark nil nil nil nil nil
  ':inline-always '(() nil t nil "trail_mark"))
(defsysfun 'si::trail-restore nil nil nil nil nil
  ':inline-always '(() nil t nil "trail_restore"))
(setf (get 'si::trail-restore 'proclaimed-return-type) 'null) ; C2OR optimization
(defsysfun 'si::trail-unmark nil nil nil nil nil
  ':inline-always '(() nil t nil "trail_unmark"))
(defsysfun 'si::get-value nil nil nil nil nil
  ':inline-always '((t t) boolean t nil "get_value(#0, #1)"))
(defsysfun 'si::get-constant nil nil nil nil nil
  ':inline-always '((t t) boolean t nil "get_constant(#0, #1)"))
(defsysfun 'si::get-nil nil nil nil nil nil
  ':inline-always '((t) boolean t nil "get_nil(#0)"))
(defsysfun 'si::get-cons nil nil nil nil nil
  ':inline-always '((t) boolean t nil "get_cons(#0)"))
(defsysfun 'si::unify-slot nil nil nil nil nil
  ':inline-always '(() t t nil "(*slotf)(*slot)"))
(defsysfun 'si::unify-value nil nil nil nil nil
  ':inline-always '((t) boolean t nil "(*slotf)(#0)"))
(defsysfun 'si::unify-constant nil nil nil nil nil
  ':inline-always '((t) boolean t nil "(*slotf)(#0)"))
(defsysfun 'si::unify-nil nil nil nil nil nil
  ':inline-always '(() boolean t nil "(*slotf)(Cnil)"))
