;;;  -*-  Mode: Lisp; Syntax: Common-Lisp; Package: BABYLON; Base: 10. -*-

(in-package "BABYLON")

;; ******* knowledge-base  declaration ******


(def-kb-instance urlaub urlaubc)


;; ******* o b j e c t s *********

(defframe institut-frame
  (slots (anzahl-mitarbeiter - :possible-values :number)
         (in-urlaub nil :possible-values :list)))


(defbehavior (institut-frame :ist-in-urlaub) (mitarbeiter-name 
                                              &optional (mode :recall))
  "something instructive."
  (case mode
    (:recall (member mitarbeiter-name ($value 'in-urlaub) :test #'string=))
    (:remember (setf ($value 'in-urlaub)
                     `(,mitarbeiter-name . ,($value 'in-urlaub))))))

(definstance f3 of institut-frame) 

(definstance f2 of institut-frame)

(definstance f1 of institut-frame)

(defframe mitarbeiter-frame
  (slots (name - :possible-values :string)
         (telefon - :possible-values :number)
         (institut - :possible-values (:one-of f3 f2 f1))))

(defbehavior (mitarbeiter-frame :ist-in-urlaub) (&optional (mode :recall))
  "something instructive."
  (<-- ($value 'institut) :ist-in-urlaub ($value 'name) mode))

(definstance urlaubs-vertreter of mitarbeiter-frame)

(defframe antrag-steller-frame
  (supers mitarbeiter-frame)
  (slots (urlaubs-konto 30 :possible-values (:interval 0 30))))

(defbehavior (antrag-steller-frame :reduce-konto) (anzahl-tage 
                                                   &optional (mode :remember))
  "something instructive."
  ;; mode muss spezifiziert werden weil
  ;; die methode in den regeln gebraucht wird
  (case mode
    (:recall t) ;; als praemisse soll nichts tun
    (:remember
     (setf ($value 'urlaubs-konto)
           (- ($value 'urlaubs-konto) anzahl-tage)))))

(definstance franco of antrag-steller-frame with 
  name          = "Franco di Primio"
  telefon       = 2684
  institut      = f3
  urlaubs-konto = 30)

(definstance peter of antrag-steller-frame with 
  name          = "Peter Henne"
  telefon       = 2687
  institut      = f3
  urlaubs-konto = 30)

(definstance karl of antrag-steller-frame with 
  name          = "Karl Wittur"
  telefon       = 2682
  institut      = f3
  urlaubs-konto = 30)

(definstance tom of antrag-steller-frame with 
  name          = "Tom Gordon"
  telefon       = 2678
  institut      = f2
  urlaubs-konto = 30)

(definstance antrag-steller of antrag-steller-frame)

;; mit (active-value <localstate> <getfn> <putfn>) deklariert man einen active value.

(defframe urlaubs-antrag-frame
  (slots (beantragt-von  antrag-steller
                         :possible-values (:instance-of antrag-steller-frame))
         (vertreter urlaubs-vertreter
                    :possible-values  (:instance-of mitarbeiter-frame))
         (anzahl-tage - 
                      :possible-values :number
                      :ask ("~&Wieviele Tage Urlaub sind von ~A beantragt ? "
                            (<- (<- O :get 'beantragt-von) :get 'name))
                      :explain-answers ("~%maximal 30 Tage."))
         (gezeichnet -
                     :possible-values (:one-of ja nein)
                     :ask ("~%Ist der Antrag vom Institutsleiter unterzeichnet worden ? "))
         (status - :possible-values (:one-of genehmigt nicht-genehmigt))
         (status-begruendung nil :possible-values :list)))



(defbehavior (urlaubs-antrag-frame :add-status) (a-string &optional (mode :remember))
  "something instructive."
  (declare (ignore mode))
  (setf ($value 'status-begruendung)
        `(,@($value 'status-begruendung) ,a-string)))

(defbehavior (urlaubs-antrag-frame :erklaere-status) (&optional (mode :remember))
  "something instructive."
  (declare (ignore mode))
  (say "~%Der Urlaub beantragt von ~A mit Vertreter ~A ist ~S."
       (<-- ($value 'beantragt-von) name)
       (<-- ($value 'vertreter) name)
       ($value 'status))
  (cond (($value 'status-begruendung) 
         (dolist (a-string ($value 'status-begruendung) t)
           (say a-string)))))

(definstance urlaubs-antrag of urlaubs-antrag-frame)

(defframe eingangs-frame
  (slots (antrag-schlange nil
                          :possible-values :list
                          :doc "Eine Liste von Paaren (<antragssteller> <urlaubsvertreter>)")
         (laufendes-paar nil :possible-values :list)))

(defbehavior (eingangs-frame :next) (&optional (mode :remember))
  "something instructive."
  (declare (special urlaubs-vertreter antrag-steller))
  (case mode 
    (:remember
     (reset-instance urlaubs-antrag)
     (setf ($value 'laufendes-paar) (first ($value 'antrag-schlange)))
     ($send self :dequeue) 
     (set-instance-pointer antrag-steller
                           (first ($value 'laufendes-paar)))
     (set-instance-pointer urlaubs-vertreter
                           (second ($value 'laufendes-paar)))
     (say "~%----- aktueller Antrag --------~%")
     (say "~%  Antragssteller   =  ~A" (<-- antrag-steller name))
     (say "~%  Urlaubsvertreter =  ~A" (<-- urlaubs-vertreter name))
     t)))

(defbehavior (eingangs-frame :empty) (&optional (mode :recall))
  "something instructive."
  (case mode
    (:recall (if ($value 'antrag-schlange) nil t))))

(defbehavior (eingangs-frame :dequeue) (&optional (mode :remember))
  "something instructive."
  (declare (ignore mode))
  (setf ($value 'antrag-schlange) (rest ($value 'antrag-schlange)))
  t)


(definstance eingangskorb of eingangs-frame with 
  antrag-schlange = (((Franco Peter) (Peter Franco) (Karl Tom))))

(defframe ausgangs-frame
  (slots (ausgang-stack nil :possible-values :list)))

(defbehavior (ausgangs-frame :enqueue) (antrag &optional (mode :remember))
  "something more instructive than this one."
  (declare (ignore mode))
  (setf  ($value 'ausgang-stack) `(,antrag . ,($value 'ausgang-stack)))
  t)

(definstance ausgangskorb of ausgangs-frame)

;; ******* r u l e   s e t s *******

(defrule-set :eingangs-kontrolle
  
  (rule-0 ($and (eingangskorb :empty))
          ($execute (say "~%Keine Antraege mehr zu bearbeiten.")
                    (stop-kb-execution)))
  (rule-1 ($true)
          ($execute (eingangskorb :next)))
  (rule-2 (?and (urlaubs-antrag gezeichnet = ja))
          ($ask (urlaubs-antrag anzahl-tage)))
  (rule-3 ($and (urlaubs-antrag gezeichnet = nein))
          ($execute
           (urlaubs-antrag status = nicht-genehmigt)
           (urlaubs-antrag :add-status "~%Unterschrift des Institutsleiters fehlt.")
           (urlaubs-antrag :add-status "~%Der Antrag muss neu gestellt werden.")
           (ausgangskorb :enqueue ($inst urlaubs-antrag))
           (urlaubs-antrag :erklaere-status)
           (stop-kb-execution))))

(defrule-set :genehmigung
  (rule-1 ($and (urlaubs-vertreter :ist-in-urlaub))
          ($conclude
           (urlaubs-antrag status = nicht-genehmigt)
           (urlaubs-antrag :add-status "~%Urlaubsvertreter selbst in Urlaub.")))
  (rule-2 ($and (urlaubs-antrag anzahl-tage > (antrag-steller urlaubs-konto)))
          ($conclude
           (urlaubs-antrag status = nicht-genehmigt)
           (urlaubs-antrag :add-status "~%beantragte Tage > Urlaubs-Konto.")))
  (rule-3 ($and (not (im-selben-institut urlaubs-vertreter antrag-steller)))
          ($conclude
           (urlaubs-antrag status = nicht-genehmigt)
           (urlaubs-antrag :add-status
                           "~%Urlaubsvertreter arbeitet nicht im selben Institut.")))
  (rule-4 ($and (not (urlaubs-vertreter :ist-in-urlaub))
                (urlaubs-antrag anzahl-tage <= (antrag-steller urlaubs-konto))
                (urlaubs-vertreter institut = (antrag-steller institut)))
          ($conclude
           (urlaubs-antrag status = genehmigt)
           (antrag-steller :reduce-konto (<-- urlaubs-antrag anzahl-tage))
           (antrag-steller :ist-in-urlaub)
           (say "~%Der Antrag von ~A" (<-- antrag-steller name))
           (say " fuer ~S Tage Urlaub ist genehmigt."
                (<-- urlaubs-antrag anzahl-tage))
           (say "~%Rest Urlaub = ~S Tage."
                (<-- antrag-steller urlaubs-konto)))))

(defrule-set :ausgang
  (rule-1 ($and (urlaubs-antrag status = genehmigt))
          ($execute
           (ausgangskorb :enqueue ($inst urlaubs-antrag))
           (say "~%Bearbeitung des Antrages abgeschlossen.")))
  (rule-2 ($and (urlaubs-antrag status = nicht-genehmigt))
          ($execute 
           (urlaubs-antrag :add-status "~%Der Antrag muss neu gestellt werden.")
           (urlaubs-antrag :erklaere-status)
           (ausgangskorb :enqueue urlaubs-antrag))))

;(defrule-set :control
;          (rule-1 ($true)
;		   ($execute (say "~%doing :eingangs-kontrolle")
;			     (find-implications :eingangs-kontrolle :do-all)))
;	   (rule-2 ($true)
;		   ($execute (say "~%doing :genehmigung")
;			     (find-implications :genehmigung :do-all)))
;	   (rule-3 ($true)
;		   ($execute (say "~%doing :ausgang")
;			     (find-implications :ausgang :do-one))))


;; ******* r e l a t i o n s **********

(defrelations urlaubcl
  ((im-selben-institut _mitarbeiter1 _mitarbeiter2) <-
   (institut _mitarbeiter1 _inst)
   (institut _mitarbeiter2 _inst)))

;; ******* i n s t r u c t i o n s ********

(instructions (say "~%doing :Eingangs-Kontrolle")
              ($send $self :find-implications :eingangs-kontrolle :do-all)
              (say "~%doing :Genehmigung")
              ($send $self :find-implications :genehmigung :do-all)
              (say "~%doing :Ausgang")
              ($send $self :find-implications :ausgang :do-one))

;;; eof

