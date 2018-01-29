(load "../util/system")
(load "defsys")
(load "cmpinit")

;(sbt:build-system clx)
;(setq *print-circle* t)
(allocate 'cons 800 t)
(si:ALLOCATE-RELOCATABLE-PAGES 200)
;(setq si:*gc-verbose* nil)
