;;; -*- Mode: Lisp; Base:10; Syntax: Common-Lisp; Package: BABYLON -*-

(in-package "BABYLON")

;;           Copyright   1986, 1985 and 1984    BY
;;           G M D  
;;           Postfach 1240
;;           D-5205 St. Augustin
;;           FRG


;; AUTHOR:   J. W A L T H E R


;
;This is the German version of all the strings and menu-item-lists of 
;the frame processor. 



(defbabylon-table frame-io-table german :size 100)



(defbabylon-entry no-delete-permit-error-fstr frame-io-table german
  "~%:VALUE Property kann in Instanzen nicht geloescht werden.~@
           In (~S :DELETE-PROPERTY ~S ~S)")


(defbabylon-entry no-update-permit-error-fstr frame-io-table german 
  "~%Sie versuchen  ~S ~S von ~S in ~S zu aendern,~@
     aber Aenderungen sind nicht erlaubt.")

(defbabylon-entry unknown-frame-error-fstr frame-io-table german
  "~S ist kein bekannter Name eines Frames der Wissensbasis ~S.")

(defbabylon-entry unknown-method-error-fstr  frame-io-table german
  "Dieses Behavior gibt es nicht in ~S: ~S")

(defbabylon-entry no-kb-error-fstr frame-io-table german
  "~%Sie versuchen den Frame ~S ausserhalb einer Wissensbasis zu definieren.")

(defbabylon-entry frame-spec-error-fstr frame-io-table german
  "~%Inkorrekte Frame-Spezifikation fuer ~S.~@
           Erwartetes Format ist: ~A.")

(defbabylon-entry supers-spec-error-fstr frame-io-table german
  "~%~S : Inkorrekte Supers-Spezifikation in Frame ~S.~@
           Erwartetes Format ist: ~A.")

(defbabylon-entry slot-spec-error-fstr frame-io-table german
  "~%~S : Inkorrekte Slot-Spezifikation in Frame ~S.~@
           Erwartetes Format ist: ~A.")

(defbabylon-entry unknown-frame-for-behavier-error-fstr frame-io-table german
  "In der Definition von BEHAVIOR ~S,~@
           ~S ist unbekannter Frame der Wissensbasis ~S.")

(defbabylon-entry behavior-spec-error-fstr frame-io-table german
  "~S: inkorrekte BEHAVIOR Spezifikation.")

(defbabylon-entry unknown-instance-error-fstr frame-io-table german
  "~S: ist nicht Name~@
           einer bekannten Instanz der Wissensbasis ~S.")

(defbabylon-entry no-kb-for-instance-error-fstr frame-io-table german
  "~%Sie versuchen die Instanz ~S von Frame ~S ~@
         ausserhalb einer Wissensbasis zu definieren.")

(defbabylon-entry of-keyword-expect-error-fstr frame-io-table german
  "~S: Falsches Schluesselwort in ~A. Erwartet wird OF.")

(defbabylon-entry slot-initialization-error-fstr frame-io-table german
  "~S: Inkorrekte Slot Initialisierung in ~A.~@
       Erwartetes Format ist: ~A.")

(defbabylon-entry instance-spec-error-fstr frame-io-table german
  "~S: Inkorrekte Instance Definition.~@
       Erwartetes Format ist: ~A.")

(defbabylon-entry default-value-error-fstr frame-io-table german
  "~%Der Default-Wert ~S des Slot ~S von Frame ~S~@
         erfuellt nicht die Possible Values ~S.")

(defbabylon-entry mode-error-fstr frame-io-table german
  "Inkorrekter Mode in compute-slot-message: ~S.")

(defbabylon-entry expect-relation-fstr frame-io-table german 
  "Relation = wird anstelle von ~S erwartet.")

(defbabylon-entry constraints-spec-error-fstr frame-io-table german
  "~%==> ~S : Inkorrekte Constraints-Spezifikation~@
              in Slot ~S der Instanz ~S von Frame ~S.")


(defbabylon-entry constraints-violation-fstr frame-io-table german 
  "Der Wert ~S verletzt die Constraints ~S ~@[ ~S~] ~@
   fuer  Slot ~S der Instanz  ~S von Frame ~S.")


(defbabylon-entry unknown-poss-val-method-fstr frame-io-table german 
  "~S unbekannte Possible Value Methode ~@
   im Slot ~S der Instanz ~S von Frame ~S.")


(defbabylon-entry other-value-question-str frame-io-table german
  "Moechten Sie einen anderen Wert geben ? (Y or N) ")

(defbabylon-entry new-value-question-fstr  frame-io-table german
  "~%Neuer Wert fuer ~S ~S: ")


(defbabylon-entry explain-answers-spec-error-fstr frame-io-table german
  "~S:~%inkorrekte :EXPLAIN-ANSWERS Specification~@
           in Slot ~S der Instanz ~S von Frame ~S.~@
           ~S ist kein erlaubter Wert.")

(defbabylon-entry wrong-arg-type-error-fstr frame-io-table german
  "~S: Inkorrekter Argument-Typ in~@
           ... ~S :ASK ~S ~S).")

(defbabylon-entry whats-the-value-of-fstr frame-io-table german
  "~%~3TWelchen Wert hat ~S ~S ? ")

(defbabylon-entry explain-fstr frame-io-table german    
  "~%Um Erlaeuterungen fuer: ~{~S ~} oder zum Kontext zu erhalten,~@
        geben Sie einen Wert oder help ein ")


(defbabylon-entry explain-answers-fstr frame-io-table german
  "~2&Fuer Erlaeuterungen zu moeglichen Antworten ~
   ~:[geben Sie Leertaste ein. ~;geben Sie einen der folgenden Werte ein: ~%  ~{~S ~} ~]")

(defbabylon-entry explain-context-fstr frame-io-table german 
  "~%Fuer Erlaeuterungen zum Kontext geben Sie ~:C ein (weiter mit ~:C) : ")

(defbabylon-entry no-explain-answers-fstr  frame-io-table german 
  "~2&Erlaeuterungen zu moeglichen Antworten liegen nicht vor.")

(defbabylon-entry next-value-fstr frame-io-table german 
  "~%Naechster Wert (weiter mit ~:C) : ")


(defbabylon-entry please-enter-fstr  frame-io-table german
  "~%~3TBitte ~S fuer ~S ~S eingeben: ")

(defbabylon-entry a-slot-of-fstr  frame-io-table german
  "ein Slot von ~S ~S")

(defbabylon-entry slot-spec-example-str frame-io-table german
  "<slot-name> |~@
           ~@T(<slot-name> <default-value> :<prop-name> <prop-value> ... )")

(defbabylon-entry frame-spec-example-str frame-io-table german
  "(DEFFRAME <frame-name> ~@
  ~5@T(SUPERS <frame-name1> ... <frame-nameN>)~@
  ~5@T(SLOTS (<slot-name> <default-value> :<prop-name> <prop-value> ...)~@
         ~12@T... ))")

(defbabylon-entry supers-spec-example-str frame-io-table german
  "(SUPERS <frame-name1> ... <frame-nameN>)")

(defbabylon-entry instance-spec-example-str frame-io-table german
  "(DEFINSTANCE <instance-name> OF <frame-name>~@
      ~4@TWITH <slot-name1> = <slot-specification1>~@
      ~9@T...~@
      ~9@T<slot-nameN> = <slot-specificationN>)~@
      ~@
      ~@T<slot-specification> := ATOM~@
      ~@T<slot-specification> := (<init-value> :<prop-name> <prop-value> ...)")

(defbabylon-entry definstance-spec-fstr frame-io-table german
  "(DEFINSTANCE ~S ~S ~S ...)")

(defbabylon-entry all-properties-item-str  frame-io-table german
  " Alle Properties ")

(defbabylon-entry value-property-only-item-str  frame-io-table german
  " nur :VALUE Property ")

(defbabylon-entry which-property-header-str  frame-io-table german
  " Welche Properties von Slots von ~S sollen beschrieben werden ")

(defbabylon-entry expect-restricted-value-fstr frame-io-table german
  "~%Der Wert verletzt die Restriktion ~S ~
           ~:[~;mit den Argumenten ~S.~] ")

(defbabylon-entry expected-value-not-str frame-io-table german
  "~%ERWARTETER WERT NICHT ~S ")

(defbabylon-entry expected-value-str frame-io-table german
  "~%ERWARTETER WERT ~S ")

(defbabylon-entry expect-no-restricted-value-fstr frame-io-table german
  "~%Fuer diesen Wert existiert keine Einschraenkung.")

(defbabylon-entry no-instance-error-msg-fstr  frame-io-table german
  "~%====>> ~S ist keine Instanz.~%")

(defbabylon-entry object-header-str frame-io-table german
  "~3%;; ************* O B J E K T E ***********~3%")

(defbabylon-entry frame-header-fstr  frame-io-table german
  ";; ************* FRAME  ~S ***********~2%")

(defbabylon-entry no-of-frames-fstr  frame-io-table german
  "~%- Anzahl der FRAMES: ~38T~D")

(defbabylon-entry no-of-instances-fstr  frame-io-table german
  "~%- Anzahl der INSTANZEN: ~38T~D")

(defbabylon-entry describe-which-question-str  frame-io-table german
  " Beschreibe ~S ? ")

(defbabylon-entry meta-frame-trace-fstr frame-io-table german
	  " META -> FRAME ~S  ~S")

(defbabylon-entry meta-frame-trace-one-fstr frame-io-table german
	  " META -> FRAME ~S")


(defbabylon-entry exit-select-item frame-io-table german
  '("-- Ende Auswahl --" :value exit
    #+:lispm :font #+:lispm fonts:cptfontb
    ))

(defbabylon-entry toggle-mode-item frame-io-table german
  '("-- Aendere Modus --" :value mode
    #+:lispm :font #+:lispm fonts:cptfontb
    ))


(defbabylon-entry unknown-frame-while-defining-fstr frame-io-table german
  "Bei der Definition von ~A ~S:~%~A ist kein bekannter Frame der Wissensbasis ~S")	


(defbabylon-entry ask-slot-prop-fstr  frame-io-table german
  "~%~3TWelchen Wert hat ~S fuer ~S von ~S: ")

(defbabylon-entry ask-slot-fstr  frame-io-table german 
  "~%~3TWelchen Wert hat ~S von ~S: ")


;;; eof

