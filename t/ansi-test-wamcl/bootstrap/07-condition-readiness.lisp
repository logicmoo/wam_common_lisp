;; Standalone ANSI bootstrap readiness checks for the current blocking layer.
;; These presence/expansion checks intentionally report failures until WAM-CL
;; implements the condition and restart facilities required by ansi-test.
;; Run from the repository root:
;;   .\wam_cl.cmd < t\ansi-test-wamcl\bootstrap\07-condition-readiness.lisp

(in-package :cl-user)

(defparameter *ab-passed* 0)
(defparameter *ab-failed* 0)
(defparameter *ab-total* 17)
(unless (boundp '*ab-all-passed*) (setq *ab-all-passed* 0))
(unless (boundp '*ab-all-failed*) (setq *ab-all-failed* 0))
(unless (boundp '*ab-all-total*) (setq *ab-all-total* 0))

(defun ab-record (name expected actual)
  (if (equal expected actual)
      (progn
        (setq *ab-passed* (+ *ab-passed* 1))
        (print (list :ansi-bootstrap-test name :pass)))
      (progn
        (setq *ab-failed* (+ *ab-failed* 1))
        (print (list :ansi-bootstrap-test name :fail
                     :expected expected :actual actual))))
  nil)

(defun ab-test (name expected actual)
  (ab-record name expected actual))

(defun ab-summary (phase)
  (let ((failed (- *ab-total* *ab-passed*)))
    (setq *ab-all-passed* (+ *ab-all-passed* *ab-passed*))
    (setq *ab-all-failed* (+ *ab-all-failed* failed))
    (setq *ab-all-total* (+ *ab-all-total* *ab-total*))
    (print (list :ansi-bootstrap-summary phase
                 :passed *ab-passed* :failed failed :total *ab-total*))
    (print (list :ansi-bootstrap-total
                 :passed *ab-all-passed*
                 :failed *ab-all-failed*
                 :total *ab-all-total*)))
  nil)

(defun ab-macro-expands-p (form)
  (let ((result
         (ignore-errors
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 form)
             (list :ok expansion expanded)))))
    (and (consp result)
         (eq (car result) :ok)
         (not (null (caddr result))))))

(ab-test :empty-handler-bind
         3
         (handler-bind () (+ 1 2)))

(ab-test :nonempty-handler-bind-expands
         t
         (ab-macro-expands-p
          '(handler-bind ((error #'identity)) 1)))

(ab-test :handler-case-expands
         t
         (ab-macro-expands-p
          '(handler-case 1 (error () 2))))

(ab-test :restart-case-expands
         t
         (ab-macro-expands-p
          '(restart-case 1 (continue () 2))))

(ab-test :handler-bind-behavior
         :handled
         (let ((state nil))
           (ignore-errors
             (handler-bind
                 ((error (lambda (condition)
                           (declare (ignore condition))
                           (setq state :handled))))
               (error "handler-bind")))
           state))

(ab-test :handler-case-behavior
         :handled
         (handler-case
             (error "handler-case")
           (error () :handled)))

(ab-test :restart-case-behavior
         :continued
         (restart-case
             (invoke-restart 'continue)
           (continue () :continued)))

(ab-test :warning-muffling
         :continued
         (let ((state nil))
           (handler-bind
               ((warning (lambda (condition)
                           (muffle-warning condition))))
             (warn "warning")
             (setq state :continued))
           state))

(ab-test :make-condition-present
         t
         (not (null (fboundp 'make-condition))))

(ab-test :signal-present
         t
         (not (null (fboundp 'signal))))

(ab-test :warn-present
         t
         (not (null (fboundp 'warn))))

(ab-test :muffle-warning-present
         t
         (not (null (fboundp 'muffle-warning))))

(ab-test :find-restart-present
         t
         (not (null (fboundp 'find-restart))))

(ab-test :invoke-restart-present
         t
         (not (null (fboundp 'invoke-restart))))

(ab-test :simple-condition-construction
         t
         (not
          (null
           (ignore-errors
             (typep
              (make-condition 'simple-error
                              :format-control "bootstrap"
                              :format-arguments nil)
              'simple-error)))))

(ab-test :signaled-error-is-condition
         t
         (let ((values
                (multiple-value-list
                 (ignore-errors (error "bootstrap")))))
           (not
            (null
             (ignore-errors
               (typep (cadr values) 'error))))))

(ab-test :simple-condition-accessors
         t
         (not
          (null
           (ignore-errors
             (let ((condition
                    (make-condition 'simple-error
                                    :format-control "bootstrap"
                                    :format-arguments '(1 2))))
               (and (string= "bootstrap"
                             (simple-condition-format-control condition))
                    (equal '(1 2)
                           (simple-condition-format-arguments condition))))))))

(ab-summary 'condition-readiness)
