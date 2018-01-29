(load "clisp-fmt")

(put 'ecl 'latexinfo-format 'latexinfo-format-ecl)
(defun latexinfo-format-ecl ()
  (latexinfo-parse-noarg)
  (insert "ECoLisp"))
(put 'clisp 'latexinfo-format 'latexinfo-format-clisp)
(defun latexinfo-format-clisp ()
  (latexinfo-parse-noarg)
  (insert "Common Lisp"))

(put 'deftype  'latexinfo-defun-indexing-property 'latexinfo-index-defun)
(put 'deftypex  'latexinfo-defun-indexing-property 'latexinfo-index-defun)
(put 'deftype 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defun)
(put 'deftypex 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defun)
(put 'deftype 'latexinfo-format 'latexinfo-format-defun)
(put 'deftypex 'latexinfo-format 'latexinfo-format-defunx)
(put 'enddeftype 'latexinfo-format 'latexinfo-end-defun)
(put 'deftype 'latexinfo-defun-type '('defun-type "Type"))
(put 'deftypex 'latexinfo-defun-type '('defun-type "Type"))
(put 'deftype 'latexinfo-defun-index 'latexinfo-vindex)
(put 'deftypex 'latexinfo-defun-index 'latexinfo-vindex)

(put 'defgeneric  'latexinfo-defun-indexing-property 'latexinfo-index-defun)
(put 'defgenericx  'latexinfo-defun-indexing-property 'latexinfo-index-defun)
(put 'defgeneric 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defun)
(put 'defgenericx 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defun)
(put 'defgeneric 'latexinfo-format 'latexinfo-format-defun)
(put 'defgenericx 'latexinfo-format 'latexinfo-format-defunx)
(put 'enddefgeneric 'latexinfo-format 'latexinfo-end-defun)
(put 'defgeneric 'latexinfo-defun-type '('defun-type "Generic Function"))
(put 'defgenericx 'latexinfo-defun-type '('defun-type "Generic Function"))
(put 'defgeneric 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defgenericx 'latexinfo-defun-index 'latexinfo-vindex)

(put 'defmethod  'latexinfo-defun-indexing-property 'latexinfo-index-defun)
(put 'defmethodx  'latexinfo-defun-indexing-property 'latexinfo-index-defun)
(put 'defmethod 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defun)
(put 'defmethodx 'latexinfo-deffn-formatting-property
     'latexinfo-format-specialized-defun)
(put 'defmethod 'latexinfo-format 'latexinfo-format-defun)
(put 'defmethodx 'latexinfo-format 'latexinfo-format-defunx)
(put 'enddefmethod 'latexinfo-format 'latexinfo-end-defun)
(put 'defmethod 'latexinfo-defun-type '('defun-type "Method"))
(put 'defmethodx 'latexinfo-defun-type '('defun-type "Method"))
(put 'defmethod 'latexinfo-defun-index 'latexinfo-vindex)
(put 'defmethodx 'latexinfo-defun-index 'latexinfo-vindex)

(put 'deftyp 'latexinfo-numargs 1)
(put 'deftypx 'latexinfo-numargs 1)
(put 'defgeneric 'latexinfo-numargs 2)
(put 'defgenericx 'latexinfo-numargs 2)
(put 'defmethod 'latexinfo-numargs 2)
(put 'defmethodx 'latexinfo-numargs 2)

(provide 'ecl-fmt)
