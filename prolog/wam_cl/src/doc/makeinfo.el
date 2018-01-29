;;(if (null (getenv "LATEXINFO"))
;;  (error "You must define the environment variable LATEXINFO first."))

(load-file "latexinfo")
(find-file "MANUAL.tex")
(latexinfo-format-buffer t)
(save-some-buffers t)
(kill-emacs 0)
