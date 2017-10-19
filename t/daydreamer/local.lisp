


;; (in-package "CYC")

(cl:defun load-cyc () (let ((*package* (find-package "CYC")))(load "init/jrtl-release-init.lisp")  (load "init/port-init.lisp")))

;; (cyc-repl)
;; (load-cyc)


;; #-quicklisp
;; (progn (load "http://beta.quicklisp.org/quicklisp.lisp") (quicklisp-quickstart:install)  (ql:add-to-init-file))

