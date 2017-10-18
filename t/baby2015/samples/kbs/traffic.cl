;;; -*- Mode: LISP; Base: 10; Syntax: CommonLisp; Package: BABYLON -*-

(in-package "BABYLON")

;; Model of a Crossing
;; 6-20-88, Hans-Werner Guesgen, Joachim Hertzberg


(DEF-KB-INSTANCE traffic trafficc)



;;;******************************** L I S P **************************************************


(DEFUN actual-time () 
   (get-universal-time))



;;;************************ F R A M E S ,  B E H A V I O R S *********************************


(DEFFRAME crossing
  (SLOTS (avenue - :POSSIBLE-VALUES (:INSTANCE-OF way))
	 (street - :POSSIBLE-VALUES (:INSTANCE-OF way))
	 (tl-ave-n green :POSSIBLE-VALUES (:ONE-OF red green))
	 (tl-ave-s green :POSSIBLE-VALUES (:ONE-OF red green))
	 (tl-st-w red :POSSIBLE-VALUES (:ONE-OF red green))
	 (tl-st-e red :POSSIBLE-VALUES (:ONE-OF red green))
	 (last-change - :POSSIBLE-VALUES :NUMBER)))


(DEFBEHAVIOR (crossing :set-green) (traffic-light)
  (<- SELF :PUT 'tl-ave-n '-)
  (<- SELF :PUT 'tl-ave-s '-)
  (<- SELF :PUT 'tl-st-w '-)
  (<- SELF :PUT 'tl-st-e '-)
  (<- SELF :PUT-IF-SATISFIED traffic-light 'green))


(DEFBEHAVIOR (crossing :AFTER :set-green) (traffic-light)
  (declare (ignore traffic-light))
  (<- SELF :PUT 'last-change (actual-time))
  (SEND-KB :BABYLON-FORMAT "~%   **************** PHASE CHANGE ****************~%"))


(DEFBEHAVIOR (crossing :time-since-last-change) (relation value &REST IGNORE)
  (declare (ignore IGNORE))
  (FUNCALL relation (- (actual-time) (<- SELF :GET 'last-change)) value))



(DEFFRAME way)



(DEFFRAME vehicle
  (SLOTS (next-crossing - :POSSIBLE-VALUES (:ONE-OF 5th-ave-32nd-st broadway-deadend))
	 (direction - :POSSIBLE-VALUES (:ONE-OF n s e w))))



;;;*************************** I N S T A N C E S *********************************************


(DEFINSTANCE vw OF vehicle)

(DEFINSTANCE bmw OF vehicle)

(DEFINSTANCE mercedes OF vehicle)


(DEFINSTANCE 5th-ave-32nd-st OF crossing)

(DEFINSTANCE broadway-deadend OF crossing)



;;;******************************* C O N S T R A I N T S ************************************


(DEFCONSTRAINT unequal
  (:TYPE primitive)
  (:INTERFACE c1 c2)
  (:RELATION (:TUPLE (red green))
             (:TUPLE (green red))))


(DEFCONSTRAINT equal
  (:TYPE primitive)
  (:INTERFACE c1 c2)
  (:RELATION (:TUPLE (red red))
             (:TUPLE (green green))))


(DEFRESTRICTION xing-constraints
  (:RESTRICTIONS
    (:FOR-ALL cross = (:ONE-OF 5th-ave-32nd-st broadway-deadend)
	      (unequal (cross tl-ave-n) (cross tl-st-w)))
    (:FOR-ALL cross = (:ONE-OF 5th-ave-32nd-st broadway-deadend)
	      (equal (cross tl-ave-n) (cross tl-ave-s)))
    (:FOR-ALL cross = (:ONE-OF 5th-ave-32nd-st broadway-deadend)
	      (equal (cross tl-st-w) (cross tl-st-e)))))



;;;******************** C L A U S E S ****************************************************


(DEFCLAUSES traffic-clauses

  ((member _x (_x . _l)))

  ((member _x (_ . _l))
   <-
   (member _x _l))

  ((is-initialized _cross)
   <-
   (crossing _cross)
   (not (is - (<- _cross :GET 'last-change))))
  
  ((approaching-car _cross _dir)
   <-
   (crossing _cross)
   (member _dir (n s e w))
   (vehicle _v)
   (_v next-crossing = _cross)
   (_v direction = _dir))
  
  ((is-red _cross _traffic-light)
   <-
   (crossing _cross)
   (member _traffic-light
	   (tl-ave-n tl-ave-s tl-st-w tl-st-e))
   (is red (<- _cross :GET '_traffic-light))))



;;;********************************** R U L E S **********************************************


(DEFRULE-SET :initialize-5th-32nd
	     
  (rule-ini1 (?AND (NOT (is-initialized 5th-ave-32nd-st))
		   (This is a demo))
	     ($EXECUTE (<- 5th-ave-32nd-st :PUT 'last-change (actual-time))))
  
  (rule-ini2 (?AND (is-initialized 5th-ave-32nd-st)
		   (This is a demo))
	     ($EXECUTE (FIND-IMPLICATIONS :control-5th-32nd))))


(DEFRULE-SET :control-5th-32nd
	     
  (rule-con1 ($AND (approaching-car 5th-ave-32nd-st n)
                   (is-red 5th-ave-32nd-st tl-ave-n)
	           (NOT (approaching-car 5th-ave-32nd-st e))
	           (NOT (approaching-car 5th-ave-32nd-st w)))
             ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-ave-n)))
  
  (rule-con2 ($AND (approaching-car 5th-ave-32nd-st s)
		   (is-red 5th-ave-32nd-st tl-ave-s)
		   (NOT (approaching-car 5th-ave-32nd-st e))
		   (NOT (approaching-car 5th-ave-32nd-st w)))
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-ave-s)))
  
  (rule-con3 ($AND (approaching-car 5th-ave-32nd-st e)
		   (is-red 5th-ave-32nd-st tl-st-e)
		   (NOT (approaching-car 5th-ave-32nd-st n))
		   (NOT (approaching-car 5th-ave-32nd-st s)))
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-st-e)))
  
  (rule-con4 ($AND (approaching-car 5th-ave-32nd-st w)
		   (is-red 5th-ave-32nd-st tl-ave-w)
		   (NOT (approaching-car 5th-ave-32nd-st n))
		   (NOT (approaching-car 5th-ave-32nd-st s)))
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-st-w)))
  
  
  (rule-con5 ($AND (approaching-car 5th-ave-32nd-st n)
		   (is-red 5th-ave-32nd-st tl-ave-n)
		   (5th-ave-32nd-st :time-since-last-change '> 30)) 
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-ave-n)))
  
  (rule-con6 ($AND (approaching-car 5th-ave-32nd-st s)
		   (is-red 5th-ave-32nd-st tl-ave-s)
		   (5th-ave-32nd-st :time-since-last-change '> 30)) 
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-st-e)))
  
  (rule-con7 ($AND (approaching-car 5th-ave-32nd-st e)
		   (is-red 5th-ave-32nd-st tl-st-e)
		   (5th-ave-32nd-st :time-since-last-change '> 60)) 
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-st-w)))
  
  (rule-con8 ($AND (approaching-car 5th-ave-32nd-st w)
		   (is-red 5th-ave-32nd-st tl-ave-w)
		   (5th-ave-32nd-st :time-since-last-change '> 60)) 
	     ($EXECUTE (<- 5th-ave-32nd-st :set-green 'tl-st-w))))




;;;************************************ I N S T R U C T I O N S ******************************


(INSTRUCTIONS  (FIND-IMPLICATIONS :initialize-5th-32nd)
	       (SEND-KB :BABYLON-FORMAT
			"~% TL-AVE-N: ~A~% TL-AVE-S: ~A~% TL-ST-W:  ~A~% TL-ST-E:  ~A~%"
			(<- 5th-ave-32nd-st :get 'tl-ave-n)
			(<- 5th-ave-32nd-st :get 'tl-ave-s)
			(<- 5th-ave-32nd-st :get 'tl-st-w)
			(<- 5th-ave-32nd-st :get 'tl-st-e)))


;;; eof

