(in-package 'mp :use '(lisp) )

(export '(lock-owner
	  make-process-lock
	  P
	  V))
	 
(defstruct (lock (:constructor make-process-lock)
		 (:conc-name nil))
  (owner nil)
  (requests nil))

(defun P (lock)
  (si:without-scheduling
    (if (owner lock)
	(wait-in lock)
      ;else
      (setf (owner lock) (current-thread)))))

(defun wait-in (lock)
  (setf (requests lock)
	(nconc (requests lock) (list (current-thread))))
  (deactivate (current-thread)))


(defun V (lock)
  (si:without-scheduling
    (if (requests lock)
      (progn
        (setf (owner lock) (car (requests lock)))
	(reactivate (owner lock))
        (setf (requests lock)
            (delete (car (requests lock)) (requests lock))))
      ;else
      (setf (owner lock) nil))))


(in-package 'xlib)

(import '(owner
	  make-process-lock))

