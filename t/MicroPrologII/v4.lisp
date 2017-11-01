; Version 3
; boot.lsp
;
;; (in-package :USER)
  
(declaim (optimize (speed 3) (debug 0) (safety 0)))


(require :sb-posix)
(import '(sb-posix:termios sb-posix:termios-lflag sb-posix:termios-cc
sb-posix:tcgetattr sb-posix:tcsetattr
sb-posix:tcsadrain
sb-posix:icanon sb-posix:echo sb-posix:echoe sb-posix:echok
sb-posix:echonl sb-posix:vmin sb-posix:vtime))

#| What "less" does:
s.c_lflag &= ~(ICANON|ECHO|ECHOE|ECHOK|ECHONL);
s.c_oflag |= (OPOST|ONLCR|TAB3);
s.c_oflag &= ~(OCRNL|ONOCR|ONLRET);
s.c_cc[VMIN] = 1;
s.c_cc[VTIME] = 0;
|#
(defun read-char-no-echo-cbreak (&optional (stream *query-io*))
(let ((old (tcgetattr 0))
(new (tcgetattr 0))
(bits (logior icanon echo echoe echok echonl)))
(unwind-protect
(progn
(setf (termios-lflag new) (logandc2 (termios-lflag old) bits)
(aref (termios-cc new) vmin) 1
(aref (termios-cc new) vtime) 0)
(tcsetattr 0 tcsadrain new)
(read-char stream))
(tcsetattr 0 tcsadrain old))))



(defun mlg3 ()
  (banner)
  (format t "~A~%" (time (load "mlg.Start")))
					;(setq *gc-silence* t)  
  (myloop (read_prompt)))

(defun read_prompt () 
  (terpri)
  (format t "| ?- ")
  (force-output)
					;(gc)
  (read_code_tail))

(defun banner ()
  (dotimes (i 2) (terpri))
  (format t "Micro_Log3 pour vous servir~%")
  (dotimes (i 2) (terpri)))

(defun l ()
  (format t "Back to MicroLog3 top-level~%")
  (myloop (read_prompt)))
; clecteur.lsp
;

(defvar *lvarloc nil)
(defvar *lvarglob nil)
(set-macro-character #\% (get-macro-character #\;))

(defun rch ()
  (do ((ch (read-char) (read-char)))
      ((char/= ch #\Newline) ch)))
(defun rchnsep ()	
  (do ((ch (rch) (rch)))
      ((char/= ch #\space) ch)))

(defun special-plvar (ch) (char= ch '#\_))
(defun alphanum (ch) (or (alphanumericp ch) (special-plvar ch)))
(defun valdigit (ch) (digit-char-p ch)) 
 
(defun read_number (ch) 
  (do ((v (valdigit ch) (+ (* v 10) (valdigit (read-char)))))
      ((not (digit-char-p (peek-char))) v)))

(defun implode (lch) (intern (map 'string #'identity lch)))

(defun read_atom (ch) 
  (do ((lch (list ch) (push (read-char) lch)))
      ((not (alphanum (peek-char))) (implode (reverse lch)))))

(defun read_at (ch)
  (do ((lch (list ch) (push (read-char) lch)))
      ((char= (peek-char) #\') (read-char) (implode (reverse lch)))))

(defun read_string (ch)
  (do ((lch (list (char-int ch)) (push (char-int (read-char)) lch)))
      ((char= (peek-char) #\") (read-char) (do_l (reverse lch)))))

(defun read_var (ch n) 
  (status (read_atom ch) n))

(defun status (nom n)
  (if (= n 1)
      (unless (member nom *lvarglob) (pushnew nom *lvarloc))
    (progn (if (member nom *lvarloc) (setq *lvarloc (delete nom *lvarloc)))
	   (pushnew nom *lvarglob)))
  nom)

(defun read_simple (ch n)
  (cond
   ((or (upper-case-p ch) (special-plvar ch)) (read_var ch n))
   ((digit-char-p ch) (read_number ch))
   ((char= ch #\") (read_string (read-char)))
   ((char= ch #\') (read_at (read-char)))
   (t (read_atom ch))))

(defun read_fct (ch n)
  (let ((fct (read_simple ch n)) (c (rchnsep)))
    (if (char= c #\()
	(let ((la (read_args (rchnsep) (1+ n))))
	  (cons (list fct (length la)) la))
      (progn (unread-char c) fct))))
                
(defun read_args (ch n) 
  (let ((arg (read_term ch n)))
    (if (char= (rchnsep) #\,)
	(cons arg (read_args (rchnsep) n))
      (list arg))))
   
(defun read_list (ch n)
  (if (char= ch #\])
      ()
    (let ((te (read_term ch n)))
      (case (rchnsep)
	    (#\, (list '(\.  2) te (read_list (rchnsep) n)))
	    (#\| (prog1 (list '(\. 2) te (read_term (rchnsep) n)) (rchnsep))) 
	    (#\] (list '(\. 2) te nil))))))

(defun read_term (ch n)
  (if (char= ch #\[) (read_list (rchnsep) (1+ n)) (read_fct ch n))) 

(defun read_tail (ch)	
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
	(list tete)
      (cons tete (read_tail (rchnsep))))))

(defun read_clause (ch) 
  (let ((tete (read_pred ch)))
    (if (char= (rchnsep) #\.)
	(list tete)
      (progn (read-char) (cons tete (read_tail (rchnsep)))))))

(defun c (l)
  (if (atom l)
      (if (member l *lvarloc)
	  (cons 'L (position l *lvarloc))
	(if (member l *lvarglob) (cons 'G (position l *lvarglob)) l))
    (if (eq (car l) '|cUt|)
	(list '|cUt| (length *lvarloc))
      (cons (c (car l)) (c (cdr l))))))
; Version 3
; lecteur.lsp
;

(defun read_code_cl () 
  (let ((*lvarloc ()) (*lvarglob ()))
    (let ((x (read_clause (rchnsep))))
      (maj_locglob (car x) (car (last x)))
      (cons (cons (length *lvarloc) (length *lvarglob)) (c x)))))
              
(defun read_code_tail ()
  (setq *lvarloc () *lvarglob ())
  (let ((x (read_tail (rchnsep))))
    (cons
     (cons (length *lvarloc) (length *lvarglob))
     (append (c x) (list '(|true|))))))

(defun read_pred (ch) 
  (let ((nom (read_atom ch)) (c (rchnsep)))
    (if (char= c #\()
	(cons nom
	      (read_args (rchnsep) (if (member nom '(|dif| |freeze|)) 2 1)))
      (progn (unread-char c) (list nom)))))

(defun unsafe? (x h q) (and (member x q) (not (member x h))))

(defun maj_locglob (h q)
  (mapc #'(lambda (x) 
	    (when (unsafe? x h q)
		  (setq *lvarloc (delete x *lvarloc))
		  (push x *lvarglob)))
	*lvarloc))
; Version 3
; blocs.lsp
;

; I. Registres
;
(defconstant BottomFR 1)
(defconstant BottomG 300000)
(defconstant BottomL 600000)
(defconstant BottomTR 1000000)
(defconstant A 1200000)

;

(defvar Mem (make-array 1205000 :initial-element 0))
(defvar FR) (defvar TR) (defvar L) (defvar G)
(defvar CP) (defvar CL) (defvar Cut_pt) (defvar FRCP)
(defvar BL) (defvar BG)
(defvar PC) (defvar PCE) (defvar PCG) (defvar Duboulot)

(defmacro vset (v i x) `(setf (svref ,v ,i) ,x))

; II. Local Stack
;

;; fibp(20, X)

; WAM notion of environment [CL CP G Cut E]
;
(defmacro CL (b) `(svref Mem ,b))
(defmacro CP (b) `(svref Mem (1+ ,b)))
(defmacro G (b) `(svref Mem (+ ,b 2)))
(defmacro Cut (b) `(svref Mem (+ ,b 3)))
(defmacro E (b) `(+ ,b 4))            

(defmacro push_cont ()           
  `(progn (vset Mem L CL) (vset Mem (1+ L) CP)))
  
(defmacro push_E (n)		
  `(let ((top (+ L 4 ,n)))	
     (if (>= top BottomTR)
	 (throw 'debord (print "Local Stack Overflow")))
     (vset Mem (+ L 3) Cut_pt)
     (dotimes (i ,n top) (vset Mem (decf top) (cons 'LIBRE BottomG)))))
 
(defmacro maj_L (nl) `(incf L (+ 4 ,nl)))


;choice-point : [a1 .. an A FR BCP BCL BG BL BP TR]
;
(defmacro TR (b) `(svref Mem (1- ,b)))
(defmacro BP (b) `(svref Mem (- ,b 2)))
(defmacro BL (b) `(svref Mem (- ,b 3)))
(defmacro BG (b) `(svref Mem (- ,b 4)))
(defmacro BCL (b) `(svref Mem (- ,b 5)))
(defmacro BCP (b) `(svref Mem (- ,b 6)))
(defmacro FR (b) `(svref Mem (- ,b 7)))
(defmacro A (b) `(svref Mem (- ,b 8)))

(defun save_args ()
  (dotimes (i (svref Mem A) (vset Mem (incf L i) i))
	   (vset Mem (+ L i) (svref Mem (+ A i 1)))))

(defun push_choix ()
  (save_args)
  (vset Mem (incf L) FR)
  (vset Mem (incf L) CP)
  (vset Mem (incf L) CL)
  (vset Mem (incf L) G)     
  (vset Mem (incf L) BL)
  (vset Mem (incf L 2) TR) 
  (setq BL (incf L) BG G))
        
(defun push_bpr (reste) (vset Mem (- BL 2) reste))
    
(defmacro size_C (b) `(+ 8 (A ,b)))

(defun pop_choix () 
  (setq L (- BL (size_C BL)) BL (BL BL) BG (if (zerop BL) BottomG (BG BL))))

; III. Global Stack
;

(defmacro push_G (n)
  `(let ((top (+ G ,n)))
     (if (>= top BottomL) (throw 'debord (print "Global Stack Overflow")))
     (dotimes (i ,n (vset Mem (+ L 2) G))
	      (vset Mem (decf top) (cons 'LIBRE BottomG)))))
(defmacro maj_G (n) `(incf G ,n))

;IV. Trail
;
(defmacro fgblock (x) `(cdr (svref Mem ,x)))

(defun pushtrail (x)
  (if (>= TR A) (throw 'debord (print "Trail Overflow")))
  (vset Mem TR x)
  (incf TR))

(defun poptrail (top)	       
  (do () ((= TR top))
      (let ((x (svref Mem (decf TR))))
	(if (numberp x)
	    (vset Mem x (cons 'LIBRE BottomG))
	  (vset Mem (car x) (cons 'LIBRE (cdr x)))))))

; V. Frozen Goals Stack
;
(defmacro FGvar (x) `(svref Mem ,x))
(defmacro FGtail (x) `(svref Mem (1+ ,x)))
(defmacro FGgoal (x) `(svref Mem (+ 2 ,x)))
(defmacro FGenv (x) `(svref Mem (+ 3 ,x)))
(defmacro frozen? (x) `(< (fgblock ,x) BottomG))

(defmacro push_fg (v b eb r)
  `(if (>= (+ FR 3) BottomG)
       (throw 'debord (print "Frozen Goals Stack Overflow"))
     (progn (vset Mem FR ,v)
	    (vset Mem (incf FR) ,r)
	    (vset Mem (incf FR) ,b)
	    (vset Mem (incf FR) ,eb)
	    (incf FR))))
; cutili.lsp
;

(defmacro nloc (c) `(caar ,c))
(defmacro nglob (c) `(cdar ,c))
(defmacro head (c) `(cadr ,c))
(defmacro tail (c) `(cddr ,c))
(defmacro pred (g) `(car ,g))
(defmacro largs (g) `(cdr ,g))

(defmacro functor (des) `(car ,des))
(defmacro arity (des) `(cadr ,des))
(defmacro des (te) `(car ,te))
(defmacro var? (v) `(and (consp ,v) (numberp (cdr ,v))))
(defmacro list? (x) `(eq (functor (des ,x)) '\.))
 
(defmacro user? (g) `(get (pred ,g) 'def))
(defmacro builtin? (g) `(get (pred ,g) 'evaluable))
(defmacro def_of (g) 
  `(get (pred ,g) 
	(if (largs ,g)
	    (nature (car (ultimate (car (largs ,g)) PCE PCG)))
	  'def)))

(defun nature (te)
  (cond 
   ((var? te) 'def)
   ((null te) 'empty)
   ((atom te) 'atom)
   ((list? te) 'list)
   (t 'fonct)))

(defun add_cl (pred c ind)
  (setf (get pred ind) (append (get pred ind) (list c))))

(set-macro-character
 #\$
 #'(lambda (stream char)
     (let* ( (*standard-input* stream) (c (read_code_cl)))
       (add_cl (pred (head c)) c 'def)
       (if (largs (head c)) 
	   (let ((b (nature (car (largs (head c))))))
	     (if (eq b 'def)
		 (mapc 
		  #' (lambda (x) (add_cl (pred (head c)) c x))
		  '(atom empty list fonct))
	       (add_cl (pred (head c)) c b)))))
     (values)))
  
(defun answer ()
  (printvar)
  (if (zerop BL)
      (setq Duboulot nil)
    (if (and (progn (princ "More : ") (force-output) t) (member (read-char-no-echo-cbreak) '(#\o #\y)))
	(backtrack)
      (setq Duboulot nil))))
	
(defun printvar ()
  (if (and (null *lvarloc) (null *lvarglob))
      (format t "Yes ~%")
    (let ((nl -1) (ng -1))
      (mapc 
       #' (lambda (x)
	    (format t "~A = " x)
	    (write1 (ult (cons 'L (incf nl)) (E BottomL))) (terpri))
       *lvarloc)
      (mapc
       #' (lambda (x) 
	    (format t "~A = " x)
	    (write1 (ult (cons 'G (incf ng)) BottomG)) (terpri))
       *lvarglob))))
; Version 3
; unify.lsp
;

(defmacro bind (x sq e xt)
  `(progn (if (or (and (> ,x BottomL) (< ,x BL)) (< ,x BG)) (pushtrail ,xt))
	  (rplaca (svref Mem ,x) ,sq)
	  (rplacd (svref Mem ,x) ,e)))

(defun bindte (x sq e)
  (if (frozen? x)
      (let ((y (fgblock x))) (push y FRCP) (bind x sq e (cons x y)))
    (bind x sq e x)))

;(defun bindf0 (x b eb r)
;   (pushtrail (cons x (fgblock x)))
;   (rplacd (svref Mem x) FR)
;   (push_fg b eb r))

(defun bindfg (x b eb r)
  (bind x 'LIBRE FR (if (frozen? x) (cons x r) x))
  (push_fg x b eb r))

(defun unify_with (largs el eg)
  (catch 'impossible
    (dotimes (i (svref Mem A))
	     (unif
	      (let ((te (svref Mem (+ A 1 i)))) (val (car te) (cdr te)))
	      (ultimate (pop largs) el eg)))))

; cunify.lsp
;
 
(defmacro adr (v e) `(+ (cdr ,v) ,e)) 
(defmacro value (v e) `(svref Mem (adr ,v ,e))) 
     
(defun ult (v e) 
    (let ((te (value v e))) 
      (cond 
       ((eq (car te) 'LIBRE) (cons v e)) 
       ((var? (car te)) (ult (car te) (cdr te))) 
       ( te)))) 
 
(defun val (x e) (if (var? x) (ult x e) (cons x e))) 
 
(defun ultimate (x el eg) 
  (if (var? x)  
      (if (eq (car x) 'L) (ult x el) (ult x eg)) 
    (cons x eg))) 

(defmacro bindv (x ex y ey)
  `(let ((ax (adr ,x ,ex)) (ay (adr ,y ,ey)))
     (if (< ax ay) (bindte ay ,x ,ex) (bindte ax ,y ,ey))))
 
(defun unif (t1 t2)
  (let ((x (car t1)) (ex (cdr t1)) (y (car t2)) (ey (cdr t2)))
    (cond 
     ((var? y)  
      (if (var? x)
	  (if (= (adr x ex) (adr y ey)) t (bindv y ey x ex))
	(bindte (adr y ey) x ex)))
     ((var? x) (bindte (adr x ex) y ey))
     ((and (atom x) (atom y)) (if (eql x y) t (throw 'impossible 'fail)))
     ((or (atom x) (atom y)) (throw 'impossible 'fail))
     ( (let ((dx (pop x)) (dy (pop y)))
	 (if (and (eq (functor dx) (functor dy)) (= (arity dx) (arity dy)))
	     (do () ((null x)) 
		 (unif (val (pop x) ex) (val (pop y) ey)))
	   (throw 'impossible 'fail)))))))
; Version 3
; resol.lsp
;

(defun forward () 
  (do () ((null Duboulot) (format t "no More~%"))      
      (cond ((and (null CP) (null FRCP)) (answer))      
	    ((load_PC)                    
	     (cond                   
	      ((user? PC)  
	       (let ((d (def_of PC))) 
		 (if d (pr2 d) (backtrack)))) 
	      ((builtin? PC)               
	       (if (eq (apply (car PC) (cdr PC)) 'fail) 
		   (backtrack)  
		 (cont_eval)))               
	      ((backtrack))))))) 
 
(defun load_A (largs el eg) 
  (dotimes (i (length largs) (vset Mem A i))  
	   (vset Mem (+ A i 1) (ultimate (pop largs) el eg))))

(defun load_PC () 
  (if FRCP  
      (let ((x ()))   
	(do () ((null FRCP))(setq x (add_fg (pop FRCP) x)))
	(do () ((null x)) (create_block (abs (pop x))))))
  (setq PC (pop CP) PCE (E CL) PCG (G CL) Cut_pt BL))
					;  (if dbg (dbg PC) t))
(defun other_fg (b r)
  (if (< (FGtail b) BottomG) (add_fg (FGtail b) r) r))

(defun add_fg (b r)
  (let ((b1 (if (numberp (FGgoal b)) (FGgoal b) b)))
    (if (eq (pred (FGgoal b1)) '|dif|)
	(insert (- b1) (other_fg b r))
      (let* ((v (svref Mem (FGvar b1)))
	     (te (val (car v) (cdr v))))
	(if (var? (car te))
	    (let ((y (adr (car te) (cdr te))))
	      (bindfg y b1 nil (fgblock y))
	      (other_fg b r))
	  (insert b1 (other_fg b r)))))))

(defun insert (b l)	
  (if (or (null l) (> b (car l)))
      (cons b l) 
    (cons (car l) (insert b (cdr l))))) 

(defmacro dec_goal (x)
`(if (atom ,x) (list ,x) (cons (caar ,x) (cdr ,x))))

(defun create_block (b) 
  (push_cont) 
  (vset Mem (+ L 2) (FGenv b)) 
  (vset Mem (+ L 3) Cut_pt) 
  (setq CP (list (FGgoal b)) CL L) 
  (maj_L 0)) 
 
(defun pr2 (paq) 
  (load_A (largs PC) PCE PCG) 
  (if CP  
      (pr paq) 
    (progn
      (if (<= BL CL) (setq L CL)) 
      (setq CP (CP CL) CL (CL CL)) 
      (pr paq))))
         
(defun cont_eval () 
  (unless CP (if (<= BL CL) (setq L CL)) (setq CP (CP CL) CL (CL CL)))) 
 
(defun pr (paq)                   
  (if (cdr paq)              
      (progn (push_choix) (pr_choice paq))
    (pr_det (car paq))))

(defun pr_det (c) 
  (if (eq (unify_with
	   (largs (head c))
	   (push_E (nloc c))
	   (push_G (nglob c)))  
          'fail) 
      (backtrack) 
    (progn
      (maj_G (nglob c)) 
      (when (tail c)(push_cont) (setq CP (tail c) CL L) (maj_L (nloc c))))))  

(defun pr_choice (paq) 
  (let* ((resu (shallow_backtrack paq)) (c (car resu)) (r (cdr resu)))
    (cond ((null r) (pop_choix) (pr_det c))
	  ( (push_bpr r)                  
	    (maj_G (nglob c))                    
	    (when (tail c) 
		  (push_cont)                         
		  (setq CP (tail c) CL L) 
		  (maj_L (nloc c)))))))      

(defun shallow_backtrack (paq) 
  (if (and (cdr paq) 
	   (eq (unify_with 
		(largs (head (car paq)))
		(push_E (nloc (car paq)))
		(push_G (nglob (car paq))))
	       'fail)) 
      (progn 
	(setq FRCP nil FR (FR BL))    
	(poptrail (TR BL)) 
	(shallow_backtrack (cdr paq)))
    paq))

(defun backtrack ()            
  (if (zerop BL)              
      (setq Duboulot nil)     
    (progn (setq L BL G BG FR (FR L) FRCP nil Cut_pt (BL BL)
		 CP (BCP L) CL (BCL L) Cut_pt (BL BL))
	   (load_A2)            
	   (poptrail (TR BL))                 
	   (pr_choice (BP L)))))
    
(defun load_A2 () 
  (let ((deb (- L (size_C L)))) 
    (dotimes (i (A L) (vset Mem A i)) 
	     (vset Mem (+ A i 1) (svref Mem (+ deb i))))))
 
(defun myloop (c) 
  (setq FR BottomFR G BottomG L BottomL TR BottomTR Cut_pt 0
        CP nil CL 0  BL 0 BG BottomG FRCP nil Duboulot t) 
  (push_cont)
  (push_E (nloc c))
  (push_G (nglob c))  
  (setq CP (cdr c) CL L)
  (maj_L (nloc c))
  (maj_G (nglob c)) (read-char)
  (catch 'debord ( time (forward)))
  (myloop (read_prompt))) 
 
; Version 3
; pred.lsp
;
(defvar Ob_Micro_Log 
      '(|write| |nl| |tab| |read| |get| |get0|
	|var| |nonvar| |atomic| |atom| |number|
	|cUt| |fail| |true|
	|divi| |mod| |plus| |minus| |mult| |le| |lt| 
	|name| |consult| |abolish| |cputime| |statistics|
	|call| |freeze| |dif| |frozen_goals|))
(mapc #'(lambda (x) (setf (get x 'evaluable) t)) Ob_Micro_Log)
 
; !/0
(defun |cUt| (n) 
  (setq BL (Cut CL) BG (if (zerop BL) BottomG (BG BL))
	L (+ CL 4 n)))

; call/1 (+term)

(defun |call| (x)
  (if (var? x)
      (let ((te (ultimate x PCE PCG)))
	(unless CP
	  (if (<= BL CL) (setq L CL))
	  (setq CP (CP CL) CL (CL CL)))
	(push_cont)
	(vset Mem (+ L 2) (cdr te))
	(vset Mem (+ L 3) Cut_pt)
	(setq CP (list (dec_goal (car te))) CL L)
	(maj_L 0))
    (push (dec_goal x) CP)))

; freeze/2 (?var,+term)
(defun |freeze| (x p)
  (let ((xte (ultimate x PCE PCG)))
    (if (var? (car xte)) 
	(let ((y (adr (car xte) (cdr xte))) (pte (ultimate p PCE PCG)))
	  (bindfg y (dec_goal (car pte)) (cdr pte) (fgblock y)))
      (|call| p))))

; dif/2 (?term,?term)
(defun |dif| (x y)
  (let ((BL L) (BG G) (str TR) (FRCP nil))
    (if (eq (uni x y) 'fail)		
	(poptrail str)			
      (if (/= TR str)			
	  (let* ((xv (svref Mem (1- TR))) (v (if (numberp xv) xv (car xv)))) 
	    (poptrail str)		
	    (bindfg v PC PCG (fgblock v)))
	'fail))))

; statistics/0 
(defun |statistics| ()
  (format t " local stack : ~A (~A used)~%" (- BottomTR BottomL) (- L BottomL))
  (format t " global stack : ~A (~A used)~%" (- BottomL BottomG) (- G BottomG))
  (format t " trail : ~A (~A used)~%" (- A BottomTR) (- TR BottomTR))
  (format t " frozen-goals stack : ~A (~A used)~%" BottomG (- FR BottomFR)))

; frozen_goals/0
(defun |frozen_goals| ()
  (do ((i (- FR 4) (- i 4)))
      ((< i 0))
      (if (eq (car (svref Mem (FGvar i))) 'LIBRE)
	  (let ((b (if (numberp (FGgoal i)) (FGgoal i) i)))
	    (writesf (pred (FGgoal b)) (largs (FGgoal b)) (FGenv b))
	    (format t " frozen upon X~A~%" (FGvar i))))))
; cpred.lsp
;

(defmacro value1 (x) `(car (ultimate ,x PCE PCG)))
(defun uni (x y)
  (catch 'impossible
    (unif (ultimate x PCE PCG) (ultimate y PCE PCG))))
      
					;write/1 (?term)
(defun |write| (x)
  (write1 (ultimate x PCE PCG)))

(defun write1 (te)
  (let ((x (car te)) (e (cdr te)))
    (cond 
     ((null x) (format t "[]"))
     ((atom x) (format t "~A" x))
     ((var? x) (format t "X~A" (adr x e)))
     ((list? x) (format t "[")
      (writesl (val (cadr x) e) (val (caddr x) e))
      (format t "]"))
     ((writesf (functor (des x)) (largs x) e)))))

(defun writesl (te r)
  (write1 te)
  (let ((q (car r)) (e (cdr r)))
    (cond
     ((null q))
     ((var? q) (format t "|X~A" (adr q e)))
     (t (format t ",") (writesl (val (cadr q) e) (val (caddr q) e))))))

(defun writesf (fct largs e)
  (format t "~A(" fct)
  (write1 (val (car largs) e))
  (mapc #' (lambda (x) (format t ",") (write1 (val x e))) (cdr largs))
  (format t ")"))

					;nl/0
(defun |nl| () (terpri))
					;tab/1 (+int)
(defun |tab| (x)
  (dotimes (i (value1 x)) (format t " ")))
					;read/1 (?term)
(defun |read| (x) 
  (let ((te (read_terme)))
    (catch 'impossible 
      (unif (ultimate x PCE PCG) (cons (cdr te) (push1_g (car te)))))))

(defun read_terme ()
  (let ((*lvarloc nil) (*lvarglob nil))
    (let ((te (read_term (rchnsep) 2)))
      (rchnsep) (cons (length *lvarglob) (c te)))))

(defun push1_g (n)
  (if (>= (+ G n) BottomL) (throw 'debord (print "Global Stack Overflow")))
  (dotimes (i n (- G n)) (vset Mem G (cons 'LIBRE BottomG)) (incf G)))

					;get/1 (?car)
(defun |get| (x)
  (uni x (char-int (rchnsep))))
					;get0/1 (?car)
(defun |get0| (x)
  (uni x (char-int (read-char))))
					;var/1 (?term)
(defun |var| (x)
  (unless (var? (value1 x)) 'fail))
					;nonvar/1 (?term)
(defun |nonvar| (x)
  (if (var? (value1 x)) 'fail))
					;atomic/1 (?term)
(defun |atomic| (x)
  (if (listp (value1 x)) 'fail))
					;atom/1 (?term)
(defun |atom| (x)
  (unless (symbolp (value1 x)) 'fail))
					;number/1 (?term)
(defun |number| (x)
  (unless (numberp (value1 x)) 'fail))
					;fail/0
(defun |fail| () 'fail)
					;true/0
(defun |true| ())
					;divi/3 (+int,+int,?int)
(defun |divi| (x y z)
  (uni z (floor (value1 x) (value1 y))))
					;mod/3 (+int,+int,?int)
(defun |mod| (x y z)
  (uni z (rem (value1 x) (value1 y))))
					;plus/3 (+int,+int,?int)
(defun |plus| (x y z)
  (uni z (+ (value1 x) (value1 y))))
					;minus/3 (+int,+int,?int)
(defun |minus| (x y z)
  (uni z (- (value1 x) (value1 y))))
					;mult/3 (+int,+int,?int)
(defun |mult| (x y z)
  (uni z (* (value1 x) (value1 y))))
					;le/2 (+int,+int)
(defun |le| (x y)
  (if (> (value1 x) (value1 y)) 'fail))
					;lt/2 (+int,+int)
(defun |lt| (x y)
  (if (>= (value1 x) (value1 y)) 'fail))
					;name/2 (?atom,?list)
(defun |name| (x y)
  (let ((b (value1 x)))
     (if (var? b) 
         (uni x (impl (undo_l (ultimate y PCE PCG))))
         (uni y (do_l (expl b))))))

(defun undo_l (te)
  (let ((x (car te)) (e (cdr te)))
    (if (atom x) 
	x
      (cons (undo_l (val (cadr x) e)) (undo_l (val (caddr x) e))))))
(defun do_l (x)
  (if (atom x) x (list '(\. 2) (car x) (do_l (cdr x)))))
(defun impl (l)
  (intern (map 'string #'int-char l)))
(defun expl (at)
  (map 'list #'char-int (string at)))

					;consult/1 (+atom)
(defun |consult| (f)
  (format t "~A~%" (load (value1 f))))
					; abolish/1
(defun |abolish| (p)
  (mapc  #'(lambda (x) (setf (get p x) nil))
	 '(atom empty list fonct def)))
					; cputime/1
(defun |cputime| (x)
  (uni x (float (/ (get-internal-run-time) internal-time-units-per-second))))


(mlg3)
