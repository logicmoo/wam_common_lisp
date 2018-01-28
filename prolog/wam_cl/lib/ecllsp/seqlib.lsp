;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           sequence routines


(in-package "SYSTEM")

(declaim (ftype (function (t) t) seqtype))
(defun seqtype (sequence)
  (declare (si::c-local))
  (cond ((listp sequence) 'list)
        ((stringp sequence) 'string)
        ((bit-vector-p sequence) 'bit-vector)
        ((vectorp sequence) (list 'vector (array-element-type sequence)))
        (t (error "~S is not a sequence." sequence))))

(declaim (ftype (function (t t t t) t) call-test))
(defun call-test (test test-not item keyx)
  (declare (si::c-local))
  (cond (test (funcall test item keyx))
        (test-not (not (funcall test-not item keyx)))
        (t (eql item keyx))))

(declaim (ftype (function () t) test-error))
(defun test-error()
  (declare (si::c-local))
  (error "both test and test are supplied"))

(defun sequence-limits (start end seq)
  (declare (si::c-local))
  (let* (x0 x1 (l (length seq)))
    (declare (fixnum x0 x1 l))
    (unless (and (fixnump start) (>= (setq x0 start) 0))
      (error 'simple-type-error
	     :format-control "~S is not a valid :START for sequence ~S"
	     :format-arguments (list start seq)
	     :datum start
	     :expected-type `(integer 0 ,l)))
    (if end
	(unless (and (fixnump end) (>= (setq x1 end) 0))
	  (error 'simple-type-error
		 :format-control "~S is not a valid :END for sequence ~S"
		 :format-arguments (list end seq)
		 :datum end
		 :expected-type `(or nil (integer 0 ,l))))
	(setq x1 l))
    (unless (<= x0 x1)
      (error ":START = ~S should be smaller or equal to :END = ~S"
	     start end))
    (values x0 x1)))

#+ecl-min
(eval-when (compile eval)
  (defmacro with-start-end (start end seq &body body)
    `(multiple-value-bind (,start ,end)
        (sequence-limits ,start ,end ,seq) 
      (declare (fixnum ,start ,end))
      ,@body)))

(defun reduce (function sequence
               &key from-end
                    (start 0)
                    end
                    key (initial-value nil ivsp))
  (with-start-end start end sequence
     (unless key (setq key #'identity))
     (cond ((not from-end)
	    (when (null ivsp)
                  (when (>= start end)
                        (return-from reduce (funcall function)))
                  (setq initial-value (funcall key (elt sequence start)))
		  (incf start))
	    (do ((x initial-value
		    (funcall function x
			     (prog1 (funcall key (elt sequence start))
				    (incf start)))))
		((>= start end) x)))
          (t
           (when (null ivsp)
                 (when (>= start end)
                       (return-from reduce (funcall function)))
                 (decf end)
                 (setq initial-value (elt sequence end)))
           (do ((x initial-value (funcall function
					  (funcall key (elt sequence end))
					  x)))
               ((>= start end) x)
	     (decf end))))))

(defun fill (sequence item &key (start 0) end)
  (with-start-end start end sequence
		  (do ((i start (1+ i)))
		      ((>= i end) sequence)
		     (declare (fixnum i))
		     (setf (elt sequence i) item))))

(defun replace (sequence1 sequence2 &key (start1 0) end1 (start2 0) end2)
  (with-start-end start1 end1 sequence1
     (with-start-end start2 end2 sequence2		  
    (if (and (eq sequence1 sequence2)
             (> start1 start2))
        (do* ((i 0 (1+ i))
              (l (if (< (the fixnum (- end1 start1))
			(the fixnum (- end2 start2)))
                      (- end1 start1)
                      (- end2 start2)))
              (s1 (+ start1 (the fixnum (1- l))) (the fixnum (1- s1)))
              (s2 (+ start2 (the fixnum (1- l))) (the fixnum (1- s2))))
            ((>= i l) sequence1)
          (declare (fixnum i l s1 s2))
          (setf (elt sequence1 s1) (elt sequence2 s2)))
        (do ((i 0 (1+ i))
             (l (if (< (the fixnum (- end1 start1))
		       (the fixnum (- end2 start2)))
                    (- end1 start1)
                    (- end2 start2)))
             (s1 start1 (1+ s1))
             (s2 start2 (1+ s2)))
            ((>= i l) sequence1)
          (declare (fixnum i l s1 s2))
          (setf (elt sequence1 s1) (elt sequence2 s2)))))))


;;; DEFSEQ macro.
;;; Usage:
;;;
;;;    (DEFSEQ function-name argument-list countp everywherep variantsp body)
;;;
;;; The arguments ITEM and SEQUENCE (PREDICATE and SEQUENCE)
;;;  and the keyword arguments are automatically supplied.
;;; If the function has the :COUNT argument, set COUNTP T.
;;; If VARIANTSP is NIL, the variants -IF and -IF-NOT are not generated. 

(eval-when (eval compile)
(defmacro defseq (f args countp everywherep variantsp normal-form &optional from-end-form)
  `(macrolet
      ((do-defseq (f args countp everywherep)
	 (let* (from-end-form
		normal-form
		(i-in-range '(and (<= start i) (< i end)))
		(x '(elt sequence i))
		(keyx `(funcall key ,x))
		(satisfies-the-test `(call-test test test-not item ,keyx))
		(number-satisfied
		 `(n (internal-count item sequence
		      :from-end from-end
		      :test test :test-not test-not
		      :start start :end end
		      ,@(if countp '(:count count))
		      :key key)))
		(within-count '(< k count))
		(kount-0 '(k 0))
		(kount-up '(setq k (1+  k))))
	   (let* ((iterate-i '(i start (1+ i)))
		  (endp-i '(>= i end))
		  (iterate-i-everywhere '(i 0 (1+ i)))
		  (endp-i-everywhere '(>= i l)))
	     (setq normal-form ,normal-form))
	   (let* ((iterate-i '(i (1- end) (1- i)))
		  (endp-i '(< i start))
		  (iterate-i-everywhere '(i (1- l) (1- i)))
		  (endp-i-everywhere '(< i 0)))
	     (setq from-end-form ,(or from-end-form normal-form)))
           `(defun ,f (,@args item sequence
                       &key test test-not
		       from-end (start 0) end
		       key
		       ,@(if countp '(count))
                       ,@(if everywherep
                             (list '&aux '(l (length sequence)))
                             nil))
	     ,@(if everywherep '((declare (fixnum l))))
	     (unless key (setq key #'identity))
	     (with-start-end start end sequence
			     (let ,@(if countp
					'(((count (cond ((null count)
							 most-positive-fixnum)
							((minusp count)
							 0)
							(t count))))))
				  ,@(if countp '((declare (fixnum count))))
				  nil
				  (and test test-not (test-error))
				  (if from-end ,from-end-form ,normal-form)))))))
    (do-defseq ,f ,args ,countp ,everywherep)
    ,@(if variantsp
	   `((defun ,(intern (si:string-concatenate (string f) "-IF")
			     (symbol-package f))
		 (,@args predicate sequence
			 &key from-end
			 (start 0) end
			 key
			 ,@(if countp '(count)))
	       (,f ,@args predicate sequence
		   :from-end from-end
		   :test #'funcall
		   :start start :end end
		   ,@(if countp '(:count count))
		   :key key))
	     (defun ,(intern (si:string-concatenate (string f) "-IF-NOT")
			     (symbol-package f))
		 (,@args predicate sequence
			 &key from-end (start 0) end
			 key ,@(if countp '(count)))
	       (,f ,@args predicate sequence
		   :from-end from-end
		   :test-not #'funcall
		   :start start :end end
		   ,@(if countp '(:count count))
		   :key key))
	     ',f)
	   `(',f)))))
; eval-when


(defseq remove () t nil t
      ;; Ordinary run
      `(if (listp sequence)
           (let* ((l sequence) (l1 nil))
             (do ((i 0 (1+ i)))
                 ((>= i start))
               (declare (fixnum i))
               (push (car l) l1)
               (pop l))
             (do ((i start (1+ i)) (j 0))
                 ((or (>= i end) (>= j count) (endp l))
                  (nreconc l1 l))
               (declare (fixnum i j))
               (if (call-test test test-not item (funcall key (car l)))
		   (incf j)
		   (push (car l) l1))
	       (pop l)))
           (delete item sequence
                   :from-end from-end
                   :test test :test-not test-not
                   :start start :end end
                   :count count
                   :key key))
      ;; From end run
      `(delete item sequence
               :from-end from-end
               :test test :test-not test-not
               :start start :end end
               :count count
               :key key))


(defseq delete () t t t
      ;; Ordinary run
      `(if (listp sequence)
           (let* ((l0 (cons nil sequence)) (l l0))
             (do ((i 0 (1+ i)))
                 ((>= i start))
               (declare (fixnum i))
               (pop l))
             (do ((i start (1+ i)) (j 0))
                 ((or (>= i end) (>= j count) (endp (cdr l))) (cdr l0))
               (declare (fixnum i j))
               (cond ((call-test test test-not item (funcall key (cadr l)))
                      (incf j)
                      (rplacd l (cddr l)))
                     (t (setq l (cdr l))))))
           (let (,number-satisfied)
             (declare (fixnum n))
             (when (< n count) (setq count n))
             (do ((newseq
                   (make-sequence (seqtype sequence)
                                  (the fixnum (- l count))))
                  ,iterate-i-everywhere
                  (j 0)
                  ,kount-0)
                 (,endp-i-everywhere newseq)
               (declare (fixnum i j k))
               (cond ((and ,i-in-range ,within-count ,satisfies-the-test)
                      ,kount-up)
                     (t (setf (elt newseq j) ,x)
                        (incf j))))))
      ;; From end run
      `(let (,number-satisfied)
         (declare (fixnum n))
         (when (< n count) (setq count n))
         (do ((newseq
               (make-sequence (seqtype sequence) (the fixnum (- l count))))
              ,iterate-i-everywhere
              (j (- (the fixnum (1- l)) n))
              ,kount-0)
             (,endp-i-everywhere newseq)
           (declare (fixnum i j k))
           (cond ((and ,i-in-range ,within-count ,satisfies-the-test)
                  ,kount-up)
                 (t (setf (elt newseq j) ,x)
                    (decf j))))))

(defseq count () nil nil t
  ;; Both runs
  `(do (,iterate-i ,kount-0)
       (,endp-i k)
     (declare (fixnum i k))
     (when (and ,satisfies-the-test)
           ,kount-up)))


(defseq internal-count () t nil nil
  ;; Both runs
  `(do (,iterate-i ,kount-0)
       (,endp-i k)
     (declare (fixnum i k))
     (when (and ,within-count ,satisfies-the-test)
           ,kount-up)))


(defseq substitute (newitem) t t t
  ;; Both runs
  `(do ((newseq (make-sequence (seqtype sequence) l))
        ,iterate-i-everywhere
        ,kount-0)
       (,endp-i-everywhere newseq)
     (declare (fixnum i k))
     (cond ((and ,i-in-range ,within-count ,satisfies-the-test)
            (setf (elt newseq i) newitem)
            ,kount-up)
           (t (setf (elt newseq i) ,x)))))


(defseq nsubstitute (newitem) t nil t
  ;; Both runs
  `(do (,iterate-i ,kount-0)
       (,endp-i sequence)
     (declare (fixnum i k))
     (when (and ,within-count ,satisfies-the-test)
           (setf ,x newitem)
           ,kount-up)))


(defseq find () nil nil t
  ;; Both runs
  `(do (,iterate-i)
       (,endp-i nil)
     (declare (fixnum i))
     (when ,satisfies-the-test (return ,x))))


(defseq position () nil nil t
  ;; Both runs
  `(do (,iterate-i)
       (,endp-i nil)
     (declare (fixnum i))
     (when ,satisfies-the-test (return i))))


(defun remove-duplicates (sequence
                          &key test test-not from-end (start 0) end key)
  "Args: (sequence
       &key key (test '#'eql) test-not
            (start 0) (end (length sequence)) (from-end nil))
Returns a copy of SEQUENCE without duplicated elements."
  (and test test-not (test-error))
  (when (and (listp sequence) (not from-end) (null start) (null end))
        (when (endp sequence) (return-from remove-duplicates nil))
        (do ((l sequence (cdr l)) (l1 nil))
            ((endp (cdr l))
             (return-from remove-duplicates (nreconc l1 l)))
          (unless (member1 (car l) (cdr l) test test-not key)
                  (setq l1 (cons (car l) l1)))))
  (delete-duplicates sequence
                     :from-end from-end
                     :test test :test-not test-not
                     :start start :end end
                     :key key))
       

(defun delete-duplicates (sequence
			  &key test test-not from-end (start 0) end key
                          &aux (l (length sequence)))
  "Args: (sequence &key key
		     (test '#'eql) test-not
                     (start 0) (end (length sequence)) (from-end nil))
Destructive REMOVE-DUPLICATES.  SEQUENCE may be destroyed."
  (declare (fixnum l))
  (and test test-not (test-error))
  (when (and (listp sequence) (not from-end) (null start) (null end))
        (when (endp sequence) (return-from delete-duplicates nil))
        (do ((l sequence))
            ((endp (cdr l))
             (return-from delete-duplicates sequence))
            (cond ((member1 (car l) (cdr l) test test-not key)
                   (rplaca l (cadr l))
                   (rplacd l (cddr l)))
                  (t (setq l (cdr l))))))
  (with-start-end start end sequence
    (unless key (setq key #'identity))
    (if (not from-end)
        (do ((n 0)
             (i start (1+ i)))
            ((>= i end)
             (do ((newseq (make-sequence (seqtype sequence)
                                         (the fixnum (- l n))))
                  (i 0 (1+ i))
                  (j 0))
                 ((>= i l) newseq)
               (declare (fixnum i j))
               (cond ((and (<= start i)
                           (< i end)
                           (position (funcall key (elt sequence i))
                                     sequence
                                     :test test
                                     :test-not test-not
                                     :start (the fixnum (1+ i))
                                     :end end
                                     :key key)))
                     (t
                      (setf (elt newseq j) (elt sequence i))
                      (incf j)))))
          (declare (fixnum n i))
          (when (position (funcall key (elt sequence i))
                          sequence
                          :test test
                          :test-not test-not
                          :start (the fixnum (1+ i))
                          :end end
                          :key key)
                (incf n)))
        (do ((n 0)
             (i (1- end) (1- i)))
            ((< i start)
             (do ((newseq (make-sequence (seqtype sequence)
                                         (the fixnum (- l n))))
                  (i (1- l) (1- i))
                  (j (- (the fixnum (1- l)) n)))
                 ((< i 0) newseq)
               (declare (fixnum i j))
               (cond ((and (<= start i)
                           (< i end)
                           (position (funcall key (elt sequence i))
                                     sequence
                                     :from-end t
                                     :test test
                                     :test-not test-not
                                     :start start
                                     :end i
                                     :key key)))
                     (t
                      (setf (elt newseq j) (elt sequence i))
                      (decf j)))))
          (declare (fixnum n i))
          (when (position (funcall key (elt sequence i))
                          sequence
                          :from-end t
                          :test test
                          :test-not test-not
                          :start start
                          :end i
                          :key key)
                (incf n))))))
       

(defun mismatch (sequence1 sequence2
		 &key from-end test test-not key
		      (start1 0) (start2 0)
		      end1 end2)
  "Args: (sequence1 sequence2
       &key key (test '#'eql) test-not
            (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))
            (from-end nil))
Compares element-wise the specified subsequences of SEQUENCE1 and SEQUENCE2.
Returns NIL if they are of the same length and they have the same elements in
the sense of TEST.  Otherwise, returns the index of SEQUENCE1 to the first
element that does not match."
  (and test test-not (test-error))
  (with-start-end start1 end1 sequence1
   (with-start-end start2 end2 sequence2
    (unless key (setq key #'identity))
    (if (not from-end)
        (do ((i1 start1 (1+ i1))
             (i2 start2 (1+ i2)))
            ((or (>= i1 end1) (>= i2 end2))
             (if (and (>= i1 end1) (>= i2 end2)) nil i1))
          (declare (fixnum i1 i2))
          (unless (call-test test test-not
                             (funcall key (elt sequence1 i1))
                             (funcall key (elt sequence2 i2)))
                  (return i1)))
        (do ((i1 (1- end1) (1- i1))
             (i2 (1- end2)  (1- i2)))
            ((or (< i1 start1) (< i2 start2))
             (if (and (< i1 start1) (< i2 start2)) nil (1+ i1)))
          (declare (fixnum i1 i2))
          (unless (call-test test test-not
                             (funcall key (elt sequence1 i1))
                             (funcall key (elt sequence2 i2)))
                  (return (1+ i1))))))))


(defun search (sequence1 sequence2
               &key from-end test test-not key
		    (start1 0) (start2 0)
		    end1 end2)
  "Args: (sequence1 sequence2
       &key key (test '#'eql) test-not
            (start1 0) (end1 (length sequence1))
            (start2 0) (end2 (length sequence2))
            (from-end nil))
Searches SEQUENCE2 for a subsequence that element-wise matches SEQUENCE1.
Returns the index to the first element of the subsequence if such a
subsequence is found.  Returns NIL otherwise."
  (and test test-not (test-error))
  (with-start-end start1 end1 sequence1
   (with-start-end start2 end2 sequence2  
    (unless key (setq key #'identity))
    (if (not from-end)
        (loop
         (do ((i1 start1 (1+ i1))
              (i2 start2 (1+ i2)))
             ((>= i1 end1) (return-from search start2))
           (declare (fixnum i1 i2))
           (when (>= i2 end2) (return-from search nil))
           (unless (call-test test test-not
                              (funcall key (elt sequence1 i1))
                              (funcall key (elt sequence2 i2)))
                   (return nil)))
         (incf start2))
        (loop
         (do ((i1 (1- end1) (1- i1))
              (i2 (1- end2) (1- i2)))
             ((< i1 start1) (return-from search (the fixnum (1+ i2))))
           (declare (fixnum i1 i2))
           (when (< i2 start2) (return-from search nil))
           (unless (call-test test test-not
                              (funcall key (elt sequence1 i1))
                              (funcall key (elt sequence2 i2)))
                   (return nil)))
         (decf end2))))))


(defun sort (sequence predicate &key key)
  "Args: (sequence test &key key)
Destructively sorts SEQUENCE and returns the result.  TEST should return non-
NIL if its first argument is to precede its second argument.  The order of two
elements X and Y is arbitrary if both
	(FUNCALL TEST X Y)
	(FUNCALL TEST Y X)
evaluates to NIL.  See STABLE-SORT."
  (if (listp sequence)
      (list-merge-sort sequence predicate key)
      (quick-sort sequence 0 (the fixnum (length sequence)) predicate key)))


(defun list-merge-sort (l predicate key)
  (unless key (setq key #'identity))
  (labels
   ((sort (l)
      (prog ((i 0) left right l0 l1 key-left key-right)
        (declare (fixnum i))
        (setq i (length l))
        (cond ((< i 2) (return l))
              ((= i 2)
               (setq key-left (funcall key (car l)))
               (setq key-right (funcall key (cadr l)))
               (cond ((funcall predicate key-left key-right) (return l))
                     ((funcall predicate key-right key-left)
                      (return (nreverse l)))
                     (t (return l)))))
        (setq i (floor i 2))
        (do ((j 1 (1+ j)) (l1 l (cdr l1)))
            ((>= j i)
             (setq left l)
             (setq right (cdr l1))
             (rplacd l1 nil))
          (declare (fixnum j)))
        (setq left (sort left))
        (setq right (sort right))
        (cond ((endp left) (return right))
              ((endp right) (return left)))
        (setq l0 (cons nil nil))
        (setq l1 l0)
        (setq key-left (funcall key (car left)))
        (setq key-right (funcall key (car right)))
      loop
        (cond ((funcall predicate key-left key-right) (go left))
              ((funcall predicate key-right key-left) (go right))
              (t (go left)))
      left
        (rplacd l1 left)
        (setq l1 (cdr l1))
        (setq left (cdr left))
        (when (endp left)
              (rplacd l1 right)
              (return (cdr l0)))
        (setq key-left (funcall key (car left)))
        (go loop)
      right
        (rplacd l1 right)
        (setq l1 (cdr l1))
        (setq right (cdr right))
        (when (endp right)
              (rplacd l1 left)
              (return (cdr l0)))
        (setq key-right (funcall key (car right)))
        (go loop))))
   (sort l)))


(declaim (ftype (function (t fixnum fixnum t t) t) quick-sort))

(defun quick-sort (seq start end pred key)
  (declare (fixnum start end))
  (unless key (setq key #'identity))
  (if (<= end (the fixnum (1+ start)))
      seq
      (let* ((j start) (k end) (d (elt seq start)) (kd (funcall key d)))
            (declare (fixnum j k))
        (block outer-loop
          (loop (loop (decf k)
                      (unless (< j k) (return-from outer-loop))
                      (when (funcall pred (funcall key (elt seq k)) kd)
                            (return)))
                (loop (incf j)
                      (unless (< j k) (return-from outer-loop))
                      (unless (funcall pred (funcall key (elt seq j)) kd)
                              (return)))
                (let ((temp (elt seq j)))
                  (setf (elt seq j) (elt seq k)
                        (elt seq k) temp))))
        (setf (elt seq start) (elt seq j)
              (elt seq j) d)
        (quick-sort seq start j pred key)
        (quick-sort seq (1+ j) end pred key))))


(defun stable-sort (sequence predicate &key key)
  "Args: (sequence test &key key)
Destructively sorts SEQUENCE and returns the result.  TEST should return non-
NIL if its first argument is to precede its second argument.  For two elements
X and Y, if both
	(FUNCALL TEST X Y)
	(FUNCALL TEST Y X)
evaluates to NIL, then the order of X and Y are the same as in the original
SEQUENCE.  See SORT."
  (if (listp sequence)
      (list-merge-sort sequence predicate key)
      (if (or (stringp sequence) (bit-vector-p sequence))
          (sort sequence predicate :key key)
          (coerce (list-merge-sort (coerce sequence 'list)
                                   predicate
                                   key)
                  (seqtype sequence)))))


(defun merge (result-type sequence1 sequence2 predicate &key key
	      &aux (l1 (length sequence1)) (l2 (length sequence2)))
  "Args: (type sequence1 sequence2 test &key key)
Merges two sequences in the way specified by TEST and returns the result as a
sequence of TYPE.  Both SEQUENCEs may be destroyed.  If both SEQUENCE1 and
SEQUENCE2 are sorted in the sense of TEST, then the result is also sorted in
the sense of TEST."
  (declare (fixnum l1 l2))
  (unless key (setq key #'identity))
  (do ((newseq (make-sequence result-type (the fixnum (+ l1 l2))))
       (j 0 (1+ j))
       (i1 0)
       (i2 0))
      ((and (= i1 l1) (= i2 l2)) newseq)
    (declare (fixnum j i1 i2))
    (cond ((and (< i1 l1) (< i2 l2))
	   (cond ((funcall predicate
			   (funcall key (elt sequence1 i1))
			   (funcall key (elt sequence2 i2)))
		  (setf (elt newseq j) (elt sequence1 i1))
		  (incf i1))
		 ((funcall predicate
			   (funcall key (elt sequence2 i2))
			   (funcall key (elt sequence1 i1)))
		  (setf (elt newseq j) (elt sequence2 i2))
		  (incf i2))
		 (t
		  (setf (elt newseq j) (elt sequence1 i1))
		  (incf i1))))
          ((< i1 l1)
	   (setf (elt newseq j) (elt sequence1 i1))
	   (incf i1))
	  (t
	   (setf (elt newseq j) (elt sequence2 i2))
	   (incf i2)))))

(defun complement (f)
  "Args: (f)
Returns a new function which first applies F to its arguments and then negates
the output"
  #'(lambda (&rest x) (not (apply f x))))

(defun constantly (n)
  "Args: (n)
Builds a new function which accepts any number of arguments but always outputs N."
  #'(lambda (&rest x) (declare (ignore x)) n))
