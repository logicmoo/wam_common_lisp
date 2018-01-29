;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAP  Map functions.

(in-package 'compiler)

(defun c1map-functions (name car-p args &aux funob info)
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'map-function 2 (length args)))
  (setq funob (c1funob (car args)))
  (setq info (copy-info (cadr funob)))
  (list name info funob car-p (c1args (cdr args) info))
  )

(defun c2mapcar (funob car-p args &aux (*inline-blocks* 0))
  (let ((label (next-label*))
        (value-loc (list 'TEMP (next-temp)))
        (handy (list 'lcl (next-lcl)))
        (handies (mapcar #'(lambda (x) (declare (ignore x))
                                   (list 'lcl (next-lcl)))
                         args))
        (save (save-funob funob)))
    (setq args (push-changed-vars (coerce-locs (inline-args args) nil)
				  funob))
       (wt-nl "{object " handy ";")
       (dolist (loc handies)
         (wt-nl "object " loc "= " (car args) ";")
         (pop args))
       (cond (*safe-compile*
              (wt-nl "if(endp(" (car handies) ")")
              (dolist (loc (cdr handies)) (wt "||endp(" loc ")"))
              (wt "){"))
             (t
              (wt-nl "if(" (car handies) "==Cnil")
              (dolist (loc (cdr handies)) (wt "||" loc "==Cnil"))
              (wt "){")))
       (unwind-exit nil 'jump)
       (wt "}")
       (wt-nl value-loc "=" handy "=CONS(Cnil,Cnil);")
       (wt-label label)
       (let* ((*destination* (list 'CAR (cadr handy)))
              (*exit* (next-label))
              (*unwind-exit* (cons *exit* *unwind-exit*)))
	 (c2funcall funob
		    (if car-p
			(mapcar
			 #'(lambda (loc)
			     (list 'LOCATION *info* (list 'CAR (cadr loc))))
			 handies)
			(mapcar #'(lambda (loc) (list 'LOCATION *info* loc))
				handies))
		    save)
	 (wt-label *exit*))
       (cond (*safe-compile*
              (wt-nl "if(endp(" (car handies) "=CDR(" (car handies) "))")
              (dolist (loc (cdr handies))
                        (wt "||endp(" loc "=CDR(" loc "))"))
              (wt "){"))
             (t
              (wt-nl "if((" (car handies) "=CDR(" (car handies) "))==Cnil")
              (dolist (loc (cdr handies))
                        (wt "||(" loc "=CDR(" loc "))==Cnil"))
              (wt "){")))
       (unwind-exit value-loc 'jump)
       (wt "}")
       (wt-nl handy "=CDR(" handy ")=CONS(Cnil,Cnil);")
       (wt-nl) (wt-go label)
       (wt "}")
       (close-inline-blocks)
       )
  )

(defun c2mapc (funob car-p args &aux (*inline-blocks* 0))
  (let ((label (next-label*))
        value-loc
        (handies (mapcar #'(lambda (x) (declare (ignore x))
                                   (list 'lcl (next-lcl)))
                         args))
        save
        )
       (setq save (save-funob funob))
       (setq args (push-changed-vars (coerce-locs (inline-args args) nil)
				     funob))
       (setq value-loc (car args))
       (wt-nl "{")
       (dolist (loc handies)
                 (wt-nl "object " loc "= " (car args) ";")
                 (pop args))
       (cond (*safe-compile*
              (wt-nl "if(endp(" (car handies) ")")
              (dolist (loc (cdr handies)) (wt "||endp(" loc ")"))
              (wt "){"))
             (t
              (wt-nl "if(" (car handies) "==Cnil")
              (dolist (loc (cdr handies)) (wt "||" loc "==Cnil"))
              (wt "){")))
       (unwind-exit nil 'jump)
       (wt "}")
       (wt-label label)
       (let* ((*destination* 'trash)
              (*exit* (next-label))
              (*unwind-exit* (cons *exit* *unwind-exit*)))
             (c2funcall funob
               (if car-p
                   (mapcar
                    #'(lambda (loc)
                              (list 'LOCATION *info* (list 'CAR (cadr loc))))
                    handies)
                   (mapcar #'(lambda (loc) (list 'LOCATION *info* loc))
                           handies))
               save)
             (wt-label *exit*))
       (cond (*safe-compile*
              (wt-nl "if(endp(" (car handies) "=CDR(" (car handies) "))")
              (dolist (loc (cdr handies))
                        (wt "||endp(" loc "=CDR(" loc "))"))
              (wt "){"))
             (t
              (wt-nl "if((" (car handies) "=CDR(" (car handies) "))==Cnil")
              (dolist (loc (cdr handies))
                        (wt "||(" loc "=CDR(" loc "))==Cnil"))
              (wt "){")))
       (unwind-exit value-loc 'jump)
       (wt "}")
       (wt-nl) (wt-go label)
       (wt "}")
       (close-inline-blocks)
       )
  )

(defun c2mapcan (funob car-p args &aux (*inline-blocks* 0))
  (let ((label (next-label*))
        (value-loc (list 'TEMP (next-temp)))
        (handy (list 'lcl (next-lcl)))
        (handies (mapcar #'(lambda (x) (declare (ignore x))
                                   (list 'lcl (next-lcl)))
                         args))
        save
        )
       (setq save (save-funob funob))
       (setq args (push-changed-vars (coerce-locs (inline-args args) nil)
				     funob))
       (wt-nl "{object " handy ";")
       (dolist (loc handies)
                 (wt-nl "object " loc "= " (car args) ";")
                 (pop args))
       (cond (*safe-compile*
              (wt-nl "if(endp(" (car handies) ")")
              (dolist (loc (cdr handies)) (wt "||endp(" loc ")"))
              (wt "){"))
             (t
              (wt-nl "if(" (car handies) "==Cnil")
              (dolist (loc (cdr handies)) (wt "||" loc "==Cnil"))
              (wt "){")))
       (unwind-exit nil 'jump)
       (wt "}")
       (wt-nl value-loc "=" handy "=CONS(Cnil,Cnil);")
       (wt-label label)
       (let* ((*destination* (list 'CDR (cadr handy)))
              (*exit* (next-label))
              (*unwind-exit* (cons *exit* *unwind-exit*))
              )
             (c2funcall funob
               (if car-p
                   (mapcar
                    #'(lambda (loc)
                              (list 'LOCATION *info* (list 'CAR (cadr loc))))
                    handies)
                   (mapcar #'(lambda (loc) (list 'LOCATION *info* loc))
                           handies))
               save)
             (wt-label *exit*))
       (cond
        (*safe-compile*
         (wt-nl "while(!endp(CDR(" handy ")))" handy "=CDR(" handy ");")
         (wt-nl "if(endp(" (car handies) "=CDR(" (car handies) "))")
         (dolist (loc (cdr handies)) (wt "||endp(" loc "=CDR(" loc "))"))
         (wt "){"))
        (t
         (wt-nl "while(CDR(" handy ")!=Cnil)" handy "=CDR(" handy ");")
         (wt-nl "if((" (car handies) "=CDR(" (car handies) "))==Cnil")
         (dolist (loc (cdr handies))
                   (wt "||(" loc "=CDR(" loc "))==Cnil"))
         (wt "){")))
       (wt-nl value-loc "=CDR(" value-loc ");")
       (unwind-exit value-loc 'jump)
       (wt "}")
       (wt-nl) (wt-go label)
       (wt "}")
       (close-inline-blocks)
       )
  )


(defun push-changed-vars (locs funob &aux (locs1 nil) (forms (list funob)))
  (dolist (loc locs (nreverse locs1))
    (if (and (consp loc)
	     (eq (car loc) 'VAR)
	     (var-changed-in-forms (cadr loc) forms))
	(let ((temp (list 'TEMP (next-temp))))
	  (wt-nl temp "= " loc ";")
	  (push temp locs1))
	(push loc locs1))))

;;; ----------------------------------------------------------------------

(setf (get 'mapcar 'c1) 'c1mapcar)
(setf (get 'maplist 'c1) 'c1maplist)
(setf (get 'mapcar 'c2) 'c2mapcar)
(setf (get 'mapc 'c1) 'c1mapc)
(setf (get 'mapl 'c1) 'c1mapl)
(setf (get 'mapc 'c2) 'c2mapc)
(setf (get 'mapcan 'c1) 'c1mapcan)
(setf (get 'mapcon 'c1) 'c1mapcon)
(setf (get 'mapcan 'c2) 'c2mapcan)

(defun c1mapcar (args) (c1map-functions 'mapcar t args))
(defun c1maplist (args) (c1map-functions 'mapcar nil args))
(defun c1mapc (args) (c1map-functions 'mapc t args))
(defun c1mapl (args) (c1map-functions 'mapc nil args))
(defun c1mapcan (args) (c1map-functions 'mapcan t args))
(defun c1mapcon (args) (c1map-functions 'mapcan nil args))
