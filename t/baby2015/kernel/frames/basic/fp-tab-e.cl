;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R


;
;This is the English version of all the strings and menu-item-lists of 
;the  frame processor. 



(defbabylon-table frame-io-table english :size 100)



(defbabylon-entry no-delete-permit-error-fstr frame-io-table english    
  "~%:VALUE property cannot be deleted in instances.~@
     in (~S :DELETE-PROPERTY ~S ~S)")

(defbabylon-entry no-update-permit-error-fstr frame-io-table english
  "~%You are trying to update the ~S ~S of ~S with ~S,~@
     but no update is permitted.")

(defbabylon-entry unknown-frame-error-fstr frame-io-table english     
  "~S is not the name of a known frame of knowledge base ~S.")

(defbabylon-entry unknown-method-error-fstr frame-io-table english       
  "No such behavior for ~S: ~S")

(defbabylon-entry no-kb-error-fstr frame-io-table english              
  "~%Trying to define the frame ~S outside a knowledge-base.")

(defbabylon-entry frame-spec-error-fstr frame-io-table english          
  "~%Wrong frame specification for frame ~S.~@
     Expected definition format is: ~A.")

(defbabylon-entry supers-spec-error-fstr frame-io-table english         
  "~%~S : wrong supers specification in frame ~S.~@
           The expected format is: ~A.")

(defbabylon-entry slot-spec-error-fstr frame-io-table english      
  "~%~S : wrong slot specification in frame ~S.~@
      The expected slot format is: ~A.")

(defbabylon-entry unknown-frame-for-behavier-error-fstr frame-io-table english   
  "while defining BEHAVIOR ~S,~@
           ~S is not a known frame of knowledge base ~S.")

(defbabylon-entry behavior-spec-error-fstr frame-io-table english     
  "~S wrong BEHAVIOR specification.")

(defbabylon-entry unknown-instance-error-fstr frame-io-table english   
  "~S is not the name~@
            of a known instance of knowledge base ~S.")

(defbabylon-entry no-kb-for-instance-error-fstr frame-io-table english    
  "~%Trying to define the instance ~S of frame ~S ~@
            outside a knowledge base.")

(defbabylon-entry of-keyword-expect-error-fstr frame-io-table english    
  "~S: wrong keyword in ~A. The expected keyword is OF.")

(defbabylon-entry slot-initialization-error-fstr frame-io-table english    
  "~S: wrong slot initialization in ~A.~@
       The expected format for instance defintions is: ~A.")

(defbabylon-entry instance-spec-error-fstr frame-io-table english     
  "~S: wrong instance definition.~@
       The expected format for instance defintions i ~A.")

(defbabylon-entry default-value-error-fstr frame-io-table english    
  "~%The default value ~S for slot ~S of frame ~S~@
         does not satisfy the possible values ~S.")

(defbabylon-entry mode-error-fstr frame-io-table english   
  "Wrong mode in compute-slot-message: ~S.")

(defbabylon-entry expect-relation-fstr frame-io-table english   
  "Relation = is expected instead of ~S.")

(defbabylon-entry constraints-spec-error-fstr frame-io-table english   
  "~%==> ~S : Wrong constraints specification~@
              in slot ~S of instance ~S of frame ~S.")



(defbabylon-entry constraints-violation-fstr frame-io-table english   
  "The value ~S does not satisfy the constraints ~S ~@[ ~S~] ~@
   for slot ~S of instance ~S of frame ~S.")


(defbabylon-entry unknown-poss-val-method-fstr frame-io-table english   
  "~S unknown possible value method ~@
   in slot ~S of instance ~S of frame ~S.")


(defbabylon-entry other-value-question-str frame-io-table english   
  "~%Do you want to give another value ? (Y or N) ")

(defbabylon-entry new-value-question-fstr  frame-io-table english    
  "~%New value for ~S ~S: ")

(defbabylon-entry explain-answers-spec-error-fstr frame-io-table english  
  "~S:~%wrong :EXPLAIN-ANSWERS specification~@
           in slot ~S of instance ~S of frame ~S.~@
           ~S is not a possible value.")

(defbabylon-entry wrong-arg-type-error-fstr frame-io-table english   
  "~S: wrong argument type in ~@
           ... ~S :ASK ~S ~S).")

(defbabylon-entry whats-the-value-of-fstr frame-io-table english   
  "~%~3TWhat's the value of ~S ~S ? ")

(defbabylon-entry explain-fstr frame-io-table english   
  "~%To get explanations for: ~{~S ~} or about the context ~@
        enter a value or help ")


(defbabylon-entry explain-answers-fstr frame-io-table english
   "~2&For explanations on possible answers ~
      ~:[enter Space. ~;enter one of the values: ~%  ~{~S ~} ~]")

(defbabylon-entry explain-context-fstr frame-io-table english
	  "~%For context explanations enter ~:C (continue with ~:C) : ")

(defbabylon-entry no-explain-answers-fstr  frame-io-table english
      "~2&There are no explanations on possible answers.")

(defbabylon-entry next-value-fstr frame-io-table english
	  "~%Next value (continue with ~:C) : ")


(defbabylon-entry please-enter-fstr  frame-io-table english   
  "~%~3TPlease enter ~S for ~S ~S: ")

(defbabylon-entry a-slot-of-fstr  frame-io-table english          
  "a slot of ~S ~S")

(defbabylon-entry slot-spec-example-str frame-io-table english          
  "<slot-name> |~@
           ~@T(<slot-name> <default-value> :<prop-name> <prop-value> ... )")

(defbabylon-entry frame-spec-example-str frame-io-table english         
  "(DEFFRAME <frame-name> ~@
   ~5@T(SUPERS <frame-name1> ... <frame-nameN>)~@
   ~5@T(SLOTS (<slot-name> <default-value> :<prop-name> <prop-value> ...)~@
        ~12@T... ))")

(defbabylon-entry supers-spec-example-str frame-io-table english         
  "(SUPERS <frame-name1> ... <frame-nameN>)")

(defbabylon-entry instance-spec-example-str frame-io-table english      
  "(DEFINSTANCE <instance-name> OF <frame-name>~@
      ~4@TWITH <slot-name1> = <slot-specification1>~@
      ~9@T...~@
      ~9@T<slot-nameN> = <slot-specificationN>)~@
      ~@
       ~@T<slot-specification> := ATOM~@
       ~@T<slot-specification> := (<init-value> :<prop-name> <prop-value> ...)")

(defbabylon-entry definstance-spec-fstr frame-io-table english           
  "(DEFINSTANCE ~S ~S ~S ...)")

(defbabylon-entry all-properties-item-str  frame-io-table english         
  " All properties ")

(defbabylon-entry value-property-only-item-str  frame-io-table english	  
  " :VALUE property only ")

(defbabylon-entry which-property-header-str  frame-io-table english	  
  " Describe which slot properties of ~S ")

(defbabylon-entry expect-restricted-value-fstr frame-io-table english    
  "~%The value is expected to satisfy the restriction ~S ~
           ~:[~;with arguments ~S.~] ")

(defbabylon-entry expected-value-not-str frame-io-table english          
  "~%EXPECTED VALUE NOT ~S ")
   
(defbabylon-entry expected-value-str frame-io-table english               
  "~%EXPECTED VALUE ~S ")

(defbabylon-entry expect-no-restricted-value-fstr frame-io-table english   
  "~%There is no restriction for the value.")

(defbabylon-entry no-instance-error-msg-fstr  frame-io-table english    
  "~%====>> ~S is no instance.~%")

(defbabylon-entry object-header-str frame-io-table english              
  "~3%;; ************* O B J E C T S ***********~3%")

(defbabylon-entry frame-header-fstr  frame-io-table english             
  ";; ************* FRAME  ~S ***********~2%")

(defbabylon-entry no-of-frames-fstr  frame-io-table english             
  "~%- Number of FRAMES: ~38T~D")

(defbabylon-entry no-of-instances-fstr  frame-io-table english          
  "~%- Number of INSTANCES: ~38T~D")

(defbabylon-entry describe-which-question-str  frame-io-table english   
  " Describe which ~S ? ")


(defbabylon-entry meta-frame-trace-fstr frame-io-table english            
	  " META -> FRAME ~S  ~S")

(defbabylon-entry meta-frame-trace-one-fstr frame-io-table english	
	  " META -> FRAME ~S")


(defbabylon-entry exit-select-item frame-io-table english
  '("-- exit select --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry toggle-mode-item frame-io-table english 
  '("-- toggle mode --" :value mode
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry unknown-frame-while-defining-fstr frame-io-table english
  "While defining ~A ~S:~%~A is not a known frame of knowledge base ~S")


(defbabylon-entry ask-slot-prop-fstr  frame-io-table english   
  "~%~3TWhat's the value of ~S for ~S of ~S: ")

(defbabylon-entry ask-slot-fstr  frame-io-table english   
  "~%~3TWhat's the value of ~S of ~S: ")

;;; eof

