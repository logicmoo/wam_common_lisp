;;; -*- Mode: LISP; Syntax: Common-Lisp; Base:10; Package: BABYLON  -*- 

(in-package "BABYLON")

;;; ------------------------------------------------------------------------------
;;; Knowledge-base ExTraGMD (see section 3.10 of The AI Workbench BABYLON)
;;; ------------------------------------------------------------------------------

(def-kb-instance extra extrac)

(defvar helpvar nil)

(defvar helpvar2 nil)

;;; ------------------------------------------------------------------------------
;;; Frames, Behaviors and Lisp Functions
;;; ------------------------------------------------------------------------------


(defframe SessionFrame
  (slots
   (sessiontime -)))


(defbehavior (SessionFrame :set-old-ask-time) (neuerlokw aktivierw propname my-slotname)
  "fragt bei einigen active values, die einen Zeitstempel benoetigen,
nach der zugehoerigen Zeit (Messzeitpunkt, Operationszeitpunkt etc.)"
  (declare (ignore aktivierw propname)
           (special EXTRA SESSION))
  (cond ((equal (<- self :get my-slotname :old) '-)
         (setq helpvar (list neuerlokw)))
        (t (setq helpvar (append (list neuerlokw) 
                                 (<- self :get my-slotname :old)))))
  (send-kb :babylon-format "~%    at (time; type RETURN for default-time = ~S) --> "
           (<- ExTra :get 'DefaultTime))
  (setq helpvar2 (read-line (send-kb :dialog-stream)))
  (cond ((and (equal helpvar2 "")
              (equal (<- ExTra :get 'DefaultTime) '-))
         (<- self :put my-slotname (time:print-universal-time
                                    (time:parse-universal-time
                                     (<- Session :get 'SessionTime))
                                    nil)
             :deltatime)
         (<- ExTra :put 'DefaultTime (<- Session :get 'SessionTime)))
        ((equal helpvar2 "")
         (<- self :put my-slotname (time:print-universal-time
                                    (time:parse-universal-time
                                     (<- ExTra :get 'DefaultTime))
                                    nil)
             :deltatime))
        (t (<- ExTra :put 'DefaultTime helpvar2)
           (<- self :put my-slotname (time:print-universal-time
                                      (time:parse-universal-time
                                       (<- ExTra :get 'DefaultTime))
                                      nil)
               :deltatime)))
  (setq helpvar (append (list (<- self :get my-slotname :deltatime)) 
                        helpvar))
  (cond ((equal (length helpvar) 2)
         (<- self :put my-slotname 0 :deltatime))
        (t (<- self :put my-slotname (/ (time:time-difference
                                         (time:parse-universal-time
                                          (<- self :get my-slotname :deltatime))
                                         (time:parse-universal-time
                                          (car (<- self :get my-slotname :old))))
                                        3600.0)
               :deltatime)))
  (<- self :put my-slotname helpvar :old )
  neuerlokw)



(defbehavior (SessionFrame :set-old-default-time) (neuerlokw aktivierw propname my-slotname)
  "arbeitet analog zu :set-old-ask-time mit dem Unterschied,
dass nicht eine Zeit abgefragt wird, sondern als Zeitstempel 
die aktuelle SessionTime verwendet wird."
  (declare (ignore AKTIVIERW PROPNAME)
           (special SESSION))
  (cond ((equal (<- self :get my-slotname :old) '-)
         (setq helpvar (list neuerlokw)))
        (t (setq helpvar (append (list neuerlokw) 
                                 (<- self :get my-slotname :old)))))
  (<- self :put my-slotname (<- Session SessionTime) :deltatime)
  (setq helpvar (append (list (<- self :get my-slotname :deltatime)) 
                        helpvar))
  
  (cond ((equal (length helpvar) 2)
         (<- self :put my-slotname 0 :deltatime))
        (t (<- self :put my-slotname (/ (time:time-difference
                                         (time:parse-universal-time
                                          (<- self :get my-slotname :deltatime))
                                         (time:parse-universal-time
                                          (car (<- self :get my-slotname :old))))
                                        3600.0)
               :deltatime)))
  (<- self :put my-slotname helpvar :old )
  neuerlokw)



(defbehavior (SessionFrame :ask-new-timed-values) ()
  "erfragt bei einer neuen Loop oder nach dem Lesen aus einem File
alle Werte und Zeiten neu ab, fuer deren Slot eine Property :variabel 
mit dem Wert '- definiert ist. Vor der Abfrage werden der alte Wert und 
Zeiten angezeigt und bleiben unveraendert, wenn als Eingabe x gegeben wird."
  (dolist (element1 (send-kb :instances))
    (dolist (element2 (<- element1 :slots))
      (cond ((equal (<- element1 :get element2)
                    '-))
            ((and (equal (<- element1 :get element2 :variabel) '-)
                  (not (equal (<- element1 :get element2 :old) '-)))
             (say "~%   Value of ~S is: ~S    (at: ~S)"
                  element2
                  (<- element1 :get element2)
                  (car (<- element1 :get element2 :old)))
             (say "~%   Type X, if this value is still o.k.,")
             (say "~%   otherwise type in the new value.~%")
             (setq helpvar (send-kb :babylon-read))
             (cond ((equal helpvar 'X))
                   (t (<- element1 :put element2 helpvar))))))))


(defbehavior (SessionFrame :recall-timed-values)
             (slot referencetime longer-time-span direction shorter-time-span mode)
  (declare (ignore mode))
  (cond ((not (listp (<- self :get slot :old)))
         (<- self :ask slot)))
  (make-list-of-old-values
   (<- self :get slot :old)
   referencetime
   longer-time-span
   direction
   shorter-time-span))


(defun make-list-of-old-values
       (history referencetime longer-time-span direction shorter-time-span)
  (declare (special LOWERREFERENCETIME THISTIME UPPERREFERENCETIME))
  (cond ((equal direction 'after) 
         (setq lowerreferencetime (+ (time:parse-universal-time referencetime)
                                     (* 3600 shorter-time-span)))
         (setq upperreferencetime (+ (time:parse-universal-time referencetime)
                                     (* 3600 longer-time-span))))
        ((equal direction 'before)
         (setq upperreferencetime (- (time:parse-universal-time referencetime)
                                     (* 3600 shorter-time-span)))
         (setq lowerreferencetime (- (time:parse-universal-time referencetime)
                                     (* 3600 longer-time-span)))))
  (cond ((null history) nil)
        (t (setq thistime (time:parse-universal-time (car history))) ;!?! history kann - sein
           (cond ((and (<= thistime upperreferencetime)
                       (>= thistime lowerreferencetime))
                  (cons (second history) (make-list-of-old-values (cddr history)
                                                                  referencetime
                                                                  longer-time-span
                                                                  direction
                                                                  shorter-time-span)))
                 (t (make-list-of-old-values (cddr history)
                                             referencetime
                                             longer-time-span
                                             direction
                                             shorter-time-span))))))


;;; im folgenden werden einige Funktionen definiert, die fuer die Zeitbehandlung und
;;; fuer die Verarbeitung von Listen erforderlich sind.

;;; max.in liefert das Maximum einer Liste von Zahlen oder NIL

(defun max.in (list)
  (cond ((equal list '-) nil)
        ((equal list 'unknown) nil)
        ((equal list nil) nil)
        ((not (numberp (car list))) nil)
        (t (apply 'max list))))


;;; min.in liefert das Minimum einer Liste von Zahlen oder NIL

(defun min.in (list)
  (cond ((equal list '-) nil)
        ((equal list 'unknown) nil)
        ((equal list nil) nil)
        ((not (numberp (car list))) nil)
        (t (apply 'min list))))


;;; val<, val<= ... sind Vergleichsrelationen, die sich wie <, <= ... verhalten mit
;;; dem Unterschied, dass par1 auch NIL sein darf; in dem Falle wird auch NIL ausgegeben

(defun val< (par1 par2)
  (cond ((null par1) nil)
        (t (< par1 par2))))

(defun val<= (par1 par2)
  (cond ((null par1) nil)
        (t (<= par1 par2))))

(defun val= (par1 par2)
  (cond ((null par1) nil)
        (t (equal par1 par2))))

(defun val> (par1 par2)
  (cond ((null par1) nil)
        (t (> par1 par2))))

(defun val>= (par1 par2)
  (cond ((null par1) nil)
        (t (>= par1 par2))))


;;; val- bildet die Differenz zweier Zahlen. Wenn par1 oder par2 NIL sind, wird nur NIL
;;; zurueckgegeben.

(defun val- (par1 par2)
  (cond ((or (null par1) (null par2))
         nil)
        (t (- par1 par2))))


;;; val.sum summiert die Zahlenwerte einer Liste; wenn die Liste leer ist, ist der Wert 0.

(defun val.sum (list)
  (cond ((null list) 0)
        (t (apply '+ list))))


(define-relation-behavior (SessionFrame oldvalue-1<) (par1 par2)
  (cond ((and (listp par1)
              (> (length par1) 2))
         (< (nth 3 par1) par2))
        (T NIL)))

(define-relation-behavior (SessionFrame oldvalue-1<=) (par1 par2)
  (cond ((and (listp par1)
              (> (length par1) 2))
         (<= (nth 3 par1) par2))
        (T NIL)))

(define-relation-behavior (SessionFrame oldvalue-1=) (par1 par2)
  (cond ((and (listp par1)
              (> (length par1) 2))
         (equal (nth 3 par1) par2))
        (T NIL)))

(define-relation-behavior (SessionFrame oldvalue-1>=) (par1 par2)
  (cond ((and (listp par1)
              (> (length par1) 2))
         (>= (nth 3 par1) par2))
        (T NIL)))

(define-relation-behavior (SessionFrame oldvalue-1>) (par1 par2)
  (cond ((and (listp par1)
              (> (length par1) 2))
         (> (nth 3 par1) par2))
        (T NIL)))


;;; Zeitvergleichs-Behaviors

(defbehavior (SessionFrame :age-of-value) (slot mode)
  "bestimmt, wie alt der aktuelle Wert (letzter Wert) relativ zu SessionTime
ist. Der Zeitstempel des aktuellen Wertes ist das erste Element der Property 
:old des Slots."
  (declare (ignore MODE)
           (special SESSION))
  (cond ((not (listp (<- self :get slot :old))) nil)
        (t (/ (time:time-difference (time:parse-universal-time (<- session :get 'SessionTime))
                                    (time:parse-universal-time (car (<- self :get slot :old))))
              3600.0))))



(defbehavior (SessionFrame :age-of-oldvalue-1) (slot mode)
  "bestimmt, wie alt der vorletzte Wert relativ zu SessionTime ist. Der
Zeitstempel des vorletzten Wertes ist das dritte Element der Property 
:old des Slots."
  (declare (ignore MODE)
           (special SESSION))
  (cond ((not (and (listp (<- self :get slot :old))
                   (> (length (<- self :get slot :old)) 2))) nil)
        (t (/ (time:time-difference (time:parse-universal-time (<- session :get 'SessionTime))
                                    (time:parse-universal-time (caddr (<- self :get slot :old))))
              3600.0))))



(definstance Session of SessionFrame)

(defframe ExTraFrame
  (slots
   (loopend - :possible-values (:one-of end go-on))
   (LastSessionTime - :ask ("~% Date and time of last session"))
   (DefaultTime -)
   (hours.since 0)
   (days.since 0)
   (datasource - :possible-values (:one-of TTY FILE LOOP)
               :ask ("~%   Source of Session Data (TTY or FILE) ? "))
   (Hypotheses.generating - :possible-values (:one-of input from-rules))))

(definstance Extra of ExtraFrame)


(defframe PatientFrame
  (supers SessionFrame)
  (slots
   (Name -)
   (FirstName -)
   (Diagnosis.certain (ACTIVE-VALUE - NIL :set-old-default-time)
                      :deltatime -
                      :old -)
   (Diagnosis.certain.PosEvidence (ACTIVE-VALUE 0 NIL :set-old-default-time)
                                  :deltatime -
                                  :old -)
   (Diagnosis.possible (ACTIVE-VALUE - NIL :set-old-default-time)
                       :deltatime -
                       :old -)
   (Weight (ACTIVE-VALUE - NIL :set-old-ask-time)
           :deltatime -
           :old -
           :variabel -)
   (DryWeight (ACTIVE-VALUE - NIL :set-old-ask-time)
              :deltatime -
              :old -
              :variabel -)))

(defframe AnamnesisFrame
  (supers SessionFrame)
  (slots
   (TimeOfTransplantation -)
   (TimeOfTransplantation.since -)
   (reoperation (ACTIVE-VALUE - NIL :set-old-ask-time)
                :possible-values (:one-of yes no)
                :deltatime -
                :old -
                :variabel -)))

(defframe TherapyFrame
  (supers SessionFrame)
  (slots
   (Dialysis (ACTIVE-VALUE no NIL :set-old-ask-time)
             :deltatime -
             :old -
             :variabel -
             :possible-values (:one-of yes no))
   (AntiLymphocyteSera (ACTIVE-VALUE - NIL :set-old-ask-time)
                       :deltatime -
                       :old -
                       :variabel -)))

(defframe ImmunoSuppressionFrame
  (supers SessionFrame)
  (slots
   (Methylprednisolone (ACTIVE-VALUE - NIL :set-old-ask-time)
                       :deltatime -
                       :old -
                       :variabel -
                       :possible-values :number)))

(defframe ClinicalFindingsFrame
  (supers SessionFrame)
  (slots
   (SpontaneousGraftPain (ACTIVE-VALUE - NIL :set-old-ask-time)
                         :deltatime -
                         :old -
                         :variabel -
                         :possible-values (:one-of yes no))
   (GraftPainByPalpation (ACTIVE-VALUE - NIL :set-old-ask-time)
                         :deltatime -
                         :old -
                         :variabel -
                         :possible-values (:one-of yes no))
   (GraftTenderness (ACTIVE-VALUE - NIL :set-old-ask-time)
                    :deltatime -
                    :old -
                    :variabel -
                    :possible-values (:one-of yes no))
   (EnlargementOfGraft (ACTIVE-VALUE - NIL :set-old-ask-time)
                       :deltatime -
                       :old -
                       :variabel -
                       :possible-values (:one-of yes no))
   (Reanimation - :possible-values (:one-of yes no))
   (Temperature (ACTIVE-VALUE - NIL :set-old-ask-time)
                :deltatime -
                :old -
                :variabel -
                :possible-values (:interval 36.0 42.5))
   (SystolicBloodPressure (ACTIVE-VALUE - NIL :set-old-ask-time)
                          :deltatime -
                          :old -
                          :variabel -
                          :possible-values (:interval 50 150))))

(defframe ImmunologicDataFrame
  (supers SessionFrame)
  (slots
   (ImmunologicRisk -)
   (NumberTransplantation - :possible-values (:interval 1 5))
   (FormerTransplantationCourse - :possible-values
                                (:one-of
                                 chronic.rejection technical.lost
                                 immunologic.lost thrombosis ))
   (AntiBodyLevel (ACTIVE-VALUE - NIL :set-old-ask-time)
                  :deltatime -
                  :old -
                  :variabel -
                  :possible-values (:interval 1 50))))

(defframe LabFindingsFrame
  (supers SessionFrame)
  (slots
   (Diuresis (ACTIVE-VALUE - NIL :set-old-ask-time)
             :deltatime -
             :old -
             :variabel -)
   (RestDiuresis (ACTIVE-VALUE - NIL :set-old-ask-time)
                 :deltatime -
                 :old -
                 :variabel -)
   (CyALevel (ACTIVE-VALUE - NIL :set-old-ask-time)
             :deltatime -
             :old -
             :variabel -
             :possible-values (:interval 100 200))))

(defframe BloodLabFrame
  (supers SessionFrame)
  (slots
   (Haemodialysis - :possible-values (:one-of yes no))
   (Creatinine (ACTIVE-VALUE - NIL :set-old-ask-time)
               :deltatime -
               :old -
               :variabel -
               :possible-values (:interval 1 100))))

(defframe TechnicalInvestigationsFrame
  (supers SessionFrame)
  (slots
   (KidneyGraftBiopsy (ACTIVE-VALUE - NIL :set-old-default-time)
                      :deltatime -
                      :old -
                      :variabel -
                      :possible-values (:one-of atn rejection
                                                acute.humoral.rejection
                                                acute.cellular.rejection))
   (KGB.Diagnosis.PosEvidence (ACTIVE-VALUE - NIL :set-old-default-time)
                              :deltatime -
                              :old -)))

(defframe FirstConclusionsFrame
  (supers SessionFrame)
  (slots
   (ImmunologicRisk -)
   (ImmunologicRisk.calculation - :possible-values (:one-of yes no))
   (ImmunosupressionLevel - :possible-values (:one-of very.low low normal high))
   (Fever - :possible-values (:one-of no low high))
   (Diuresis.h -)
   (Diuresis24 (ACTIVE-VALUE - NIL :set-old-default-time)
               :deltatime -
               :old -
               :possible-values (:interval 0 5000))
   (DiffWeight -)
   (DeltaCreatinine - :possible-values (:interval -100 1000))))

(defframe DonorFrame
  (slots
   (KidneyGraftBiopsy - :possible-values (:one-of yes no))
   (ParallelKidneyDiagnosis - :possible-values
                            (:one-of atn atn.poliuric atn.oliguric
                                     atn.anuric prerenal.thrombosis))))

(defframe HypothesesFrame
  (slots
   (calculation no :possible-values (:one-of yes1 yes no))
   (ATN - :possible-values (:one-of considered accepted))
   (ATN.PosEvidence (ACTIVE-VALUE 0 NIL :put-numeric-evidence))
   (ATN.NegEvidence (ACTIVE-VALUE 0 NIL :put-numeric-evidence))
   (ATN.subclassification - :possible-values (:one-of FirstDaysATN normal))
   (rejection - :possible-values (:one-of considered accepted))
   (rejection.PosEvidence (ACTIVE-VALUE 0 NIL :put-numeric-evidence))
   (rejection.NegEvidence (ACTIVE-VALUE 0 NIL :put-numeric-evidence))
   (rejection.subclassification - :possible-values (:one-of hyperacute acute))))


;;; :put-symbolic-evidence ist ein Vorschlag, wie man zur Evidenzbestimmung mit
;;; symbolischen Werten arbeiten kann; wird in den Regeln noch nicht verwendet.

(defbehavior (HypothesesFrame :put-symbolic-evidence)
             (neuerlokw aktiverw propname my-slotname)
  (declare (ignore AKTIVERW PROPNAME))
  (send-kb :babylon-format "~%~S ~S" my-slotname neuerlokw)
  (cond
   ((and (equal (<- self :get my-slotname) 'impossible)
         (equal neuerlokw 'possible)) 'uncertain)
   (t neuerlokw)))



(defbehavior (HypothesesFrame :put-numeric-evidence)
             (neuerlokw aktiverw propname my-slotname)
  (declare (ignore AKTIVERW PROPNAME))
  (send-kb :babylon-format  "~%~S ~S" my-slotname neuerlokw)
  (cond
   ((numberp (<- self :get my-slotname)) (max (<- self :get my-slotname) neuerlokw))
   (t neuerlokw)))


;;; ------------------------------------------------------------------------------
;;; Build Instances
;;; ------------------------------------------------------------------------------


(defrule-set :build-instances
  
  (build-100 ($and
              (ExTra DataSource = -))
             ($execute 
              (<- ExTra :ask 'DataSource)))
  
  (build-200 ($and (ExTra DataSource = TTY))
             ($execute
              (definstance Session of SessionFrame)
              (<- Session :put 'SessionTime (time:print-current-time nil))
              (say "~%   Date and Time of last Session --> ")
              (<- ExTra :put 'LastSessionTime (time:print-universal-time
                                               (time:parse-universal-time
                                                (read-line (send-kb :dialog-stream)))
                                               nil))
              (definstance Patient of PatientFrame)
              (definstance Anamnesis of AnamnesisFrame)
              (definstance Therapy of TherapyFrame)
              (definstance ImmunoSuppression of ImmunoSuppressionFrame)
              (definstance ClinicalFindings of ClinicalFindingsFrame)
              (definstance ImmunologicData of  ImmunologicDataFrame)
              (definstance LabFindings of LabFindingsFrame)
              (definstance TechnicalInvestigations of TechnicalInvestigationsFrame)
              (definstance BloodLab of BloodLabFrame)
              (definstance FirstConclusions of FirstConclusionsFrame)
              (definstance Donor of DonorFrame)
              (definstance Hypotheses of HypothesesFrame)
              ;	       (loop for element in (send-kb :instances)
              ;			 do (<- element :history-off))
              ))
  
  (build-300 ($and (ExTra DataSource = FILE))
             ($execute
              (load (concatenate 'string (namestring (user-homedir-pathname))
                                 (string-downcase (send-kb :kb-name))
                                 ".kb"))
              (<- ExTra :put 'LastSessionTime (<- Session SessionTime)) 
              (definstance Session of SessionFrame)
              (<- Session :put 'SessionTime (time:print-current-time nil))
              ))
  (build-400 ($or (ExTra DataSource = LOOP)
                  (ExTra DataSource = FILE))
             ($execute
              (definstance Hypotheses of HypothesesFrame)
              (definstance FirstConclusions of FirstConclusionsFrame)
              (Patient Diagnosis.certain = -)
              (Patient Diagnosis.possible = -)
              (ExTra Hypotheses.generating = -)))
  
  )



;;; ------------------------------------------------------------------------------
;;; Questions
;;; ------------------------------------------------------------------------------

(defrule-set :questions
  
  (q-0100 ($and (ExTra DataSource = TTY))
          ($ask
           (Patient Name)
           (Patient FirstName)))
  
  (q-0200 ($and (ExTra DataSource = TTY))
          ($execute
           (say "~%   Time of Transplantation (date) --> ")
           (<- Anamnesis :put 'TimeOfTransplantation
               (time:print-universal-time
                (time:parse-universal-time
                 (read-line (send-kb :dialog-stream)))
                nil))))
  
  (q-0300 ($and (ExTra DataSource = TTY))
          ($ask
           (ClinicalFindings Temperature)
           (ImmunologicData AntiBodyLevel)
           (ImmunologicData FormerTransplantationCourse)
           (ImmunologicData NumberTransplantation)
           (Anamnesis Reoperation)
           (ExTra Hypotheses.generating)
           ))
  
  (q-0400 ($or (ExTra DataSource = FILE)
               (ExTra DataSource = LOOP))
          ($execute
           (<- Session :ask-new-timed-values)
           (ExTra Hypotheses.generating)))
  )

;;; ------------------------------------------------------------------------------
;;; :initial-time
;;; ------------------------------------------------------------------------------

;;; Bestimmung der Tage zwischen  Transplantation und aktueller SessionTime

(defrule-set :initial-time
  (i-t-0100 ($true)
            ($execute
             (<- Anamnesis :put 'TimeOfTransplantation.since
                 (/ (time:time-difference
                     (time:parse-universal-time
                      (<- Session :get 'SessionTime ))
                     (time:parse-universal-time
                      (<- Anamnesis :get 'TimeOfTransplantation)))
                    (* 24 3600.0)))))
  )

;;; ------------------------------------------------------------------------------
;;; Data-Abstraction
;;; ------------------------------------------------------------------------------

;;; erste Ableitungen

(defrule-set :data-abstraction
  
  (d-a-0100 (?and
             (ImmunologicData NumberTransplantation > 1)
             (ImmunologicData FormerTransplantationCourse = immunologic.lost))
            ($conclude
             (FirstConclusions ImmunologicRisk.calculation = yes)))
  
  (d-a-0100.10 ($or
                (FirstConclusions ImmunologicRisk.calculation = yes)
                (ImmunologicData AntiBodyLevel >= 30))
               ($conclude
                (FirstConclusions ImmunologicRisk = high)
                (FirstConclusions ImmunologicRisk.calculation = -)))
  
  (d-a-0200 (?and
             (ImmunologicData AntiBodyLevel <= 30)
             (ImmunologicData FormerTransplantationCourse one-of
                              (chronic.rejection thrombosis technical.lost)))
            ($conclude
             (FirstConclusions ImmunologicRisk = low)))
  
  (d-a-0300 (?and
             (ImmunologicData AntiBodyLevel <= 30)
             (ImmunologicData NumberTransplantation = 1))
            ($conclude
             (FirstConclusions ImmunologicRisk = low)))
  
  
  ;;; rule ClinicalFindings * Fever(Pfeil) * Fever(Pfeil)high
  
  (d-a-0400 (?and
             (ClinicalFindings Temperature >= 39.0))
            ($conclude
             (FirstConclusions Fever = high)))
  
  
  
  ;;; rule ClinicalFindings * Fever(Pfeil) * Fever(Pfeil)low2
  
  (d-a-0600 (?and
             (ClinicalFindings Temperature between (38.0 39.0)))
            ($conclude
             (FirstConclusions Fever = low)))
  
  
  ;;; rule ClinicalFindings * Fever(Pfeil) * Fever(Pfeil)low3
  
  (d-a-0700 (?and
             (ClinicalFindings Temperature between (37.5 38.0))
             (Anamnesis TimeOfTransplantation.since between (1.0 1.99))
             (val< (max.in (<- ClinicalFindings :recall-timed-values
                               'temperature
                               (<- Anamnesis :get 'TimeOfTransplantation)
                               24
                               'after
                               0
                               :recall))
                   37.5))
            ($conclude
             (FirstConclusions Fever = low)))
  
  
  
  ;;; rule ClinicalFindings * Fever(Pfeil) * Fever(Pfeil)no
  
  (d-a-0800 (?and
             (ClinicalFindings Temperature < 38.0))
            ($conclude
             (FirstConclusions Fever = no)))
  
  
  ;;; rule ClinicalFindings * getDiffWeight
  
  (d-a-0900 (?and
             (Patient DryWeight > 0)
             (Patient Weight > 0))
            ($execute
             (<- FirstConclusions :PUT 'DiffWeight (- (<- Patient :GET 'Weight)
                                                      (<- Patient :GET 'DryWeight)))))
  
  
  ;;; rule ClinicalFindings * DetDiuresis/24 * Diuresis/24noRestDiuresis
  
  (d-a-1000 (?and
             (LabFindings Diuresis > 0)
             (LabFindings RestDiuresis <= 500))
            ($execute
             (<- FirstConclusions :put 'Diuresis24
                 (val.sum
                  (<- LabFindings :recall-timed-values
                      'Diuresis
                      (<- Session :get 'SessionTime)
                      24
                      'before
                      0
                      :recall)))))
  
  
  
  
  ;;; rule ClinicalFindings * Diuresis/h(Pfeil) * Diuresis/h(Pfeil)1Value
  
  (d-a-2000 (?and
             (not (LabFindings Diuresis = -)))   ; hier fehlt noch der Zugriff auf alten Wert
            ($execute
             (<- FirstConclusions :PUT 'Diuresis.h (/ (<- LabFindings  :GET 'Diuresis)
                                                      (<- LabFindings  :GET 'Diuresis)))))
  
  
  )


;;; ------------------------------------------------------------------------------
;;; Print Abstraction
;;; ------------------------------------------------------------------------------

(defrule-set :print-abstractions
  (p-a-0100 ($true)
            ($execute
             (say "~% From the questions just answered the following has been concluded: ")
             (say "~% Fever: ~S" (<- FirstConclusions :GET 'Fever))
             (say "~% ImmunologicRisk: ~S" (<- FirstConclusions :GET 'ImmunologicRisk))
             (say "~% DiffWeight: ~S"
                  (<- FirstConclusions :GET 'DiffWeight))
             (say "~% Diuresis.h: ~S"
                  (<- FirstConclusions :GET 'Diuresis.h))
             (say "~% Diuresis24: ~S"
                  (<- FirstConclusions :GET 'Diuresis24))
             (say "~% DeltaCreatinine: ~S"
                  (<- FirstConclusions :GET 'DeltaCreatinine)))))


;;; ------------------------------------------------------------------------------
;;; Generate Hypotheses
;;; ------------------------------------------------------------------------------


(defrule-set :generate-hypotheses
  
  (g-h-0050 ($and (ExTra Hypotheses.generating = input))
            ($ask
             (Hypotheses ATN)
             (Hypotheses rejection)))
  
  (g-h-0200 ($and (ExTra Hypotheses.generating = from-rules)) 
            ($conclude
             (Hypotheses ATN = considered)
             (Hypotheses rejection = considered))))


;;; ------------------------------------------------------------------------------
;;; Print Generated Hypotheses
;;; ------------------------------------------------------------------------------

(defrule-set :print-generated-hypotheses
  
  (p-g-h-0100 ($true)
              ($execute
               (say "~%   The following hypotheses have been concluded: ")
               (say "~%   ATN: ~S" (<- Hypotheses :GET 'ATN))
               (say "~%   rejection: ~S" (<- Hypotheses :GET 'rejection)))))



;;; ------------------------------------------------------------------------------
;;; Verify Hypotheses
;;; ------------------------------------------------------------------------------

;;; Hypothesis * rejection * ImmunosupressionLevel

(defrule-set :verify-hypotheses
  
  (v-h-0300 (?and
             (Hypotheses rejection = considered)
             (FirstConclusions ImmunologicRisk = high)
             (Anamnesis TimeOfTransplantation.since < 7))
            ($conclude
             (Hypotheses calculation = yes)))
  
  (v-h-0300.10 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions ImmunosupressionLevel = very.low))
               ($execute
                (Hypotheses rejection.PosEvidence = .5)))
  
  (v-h-0300.20 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions ImmunosupressionLevel = low))
               ($execute
                (Hypotheses rejection.PosEvidence = .3)))
  
  (v-h-0300.30 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions ImmunosupressionLevel = normal))    
               ($execute
                (Hypotheses rejection.PosEvidence = .2)))
  
  (v-h-0300.40 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions ImmunosupressionLevel = high))
               ($execute
                (Hypotheses rejection.PosEvidence = .1)))
  
  (v-h-0300.50 ($true)
               ($execute
                (Hypotheses calculation = no)))
  
  
  ;;;  ATN-Regeln
  
  ;;; Hypothesis * atn * excludeATN * a
  ;;;
  
  (v-h-1200 (?and
             (Hypotheses atn = considered)
             (FirstConclusions DeltaCreatinine < -1)
             (not (member 'yes (<- Therapy :recall-timed-values
                                   'Dialysis
                                   (<- Session :get 'SessionTime)
                                   48
                                   'before
                                   0
                                   :recall))))
            ($execute
             (Hypotheses atn.NegEvidence = 1.0)))
  
  
  
  
  ;;; Hypothesis * atn * excludeATN * c
  
  (v-h-1310 (?and
             (Hypotheses atn = considered)
             (val> (min.in (<- ClinicalFindings :recall-timed-values
                               'SystolicBloodPressure
                               (<- Session :get 'SessionTime)
                               48
                               'before
                               0
                               :recall))
                   80)
             (not (member 'yes (<- ClinicalFindings  :recall-timed-values
                                   'Reanimation
                                   (<- Session :get 'SessionTime)
                                   48
                                   'before
                                   0
                                   :recall)))
             (val<
              (<- TechnicalInvestigations :age-of-oldvalue-1 'KidneyGraftBiopsy :recall)
              96)
             (TechnicalInvestigations KidneyGraftBiopsy oldvalue-1= atn)
             (TechnicalInvestigations KGB.Diagnosis.PosEvidence oldvalue-1< .2))
            ($execute
             (Hypotheses atn.NegEvidence  = 1.0)))
  
  
  
  ;;; scoring Hypothesis * atn * DeltaCreatinine
  
  (v-h-2000.a (?and
	       (Hypotheses atn = considered)
	       (member 'yes (<- Anamnesis :recall-timed-values
                                'reoperation
                                (<- Session :get 'SessionTime)
                                48
                                'before
                                0
                                :recall)))
	      ($execute
	       (Hypotheses calculation = yes1)))
  
  (v-h-2000.b (?and
	       (Hypotheses atn = considered)
	       (val< (min.in (<- ClinicalFindings :recall-timed-values
                                 'SystolicBloodPressure
                                 (<- Session :get 'SessionTime)
                                 48
                                 'before
                                 0
                                 :recall))
		     80))
	      ($execute
	       (Hypotheses calculation = yes1)))
  
  (v-h-2000.10 (?or
		(Hypotheses calculation = yes1)
		(Anamnesis reoperation :deltatime < 48))
	       ($execute
		(Hypotheses calculation = yes)))
  
  (v-h-2000.10.1 (?and
		  (Hypotheses calculation = yes)
		  (FirstConclusions DeltaCreatinine between (0 .5)))
		 ($execute
		  (Hypotheses atn.PosEvidence = .7)))
  
  (v-h-2000.10.2 (?and
		  (Hypotheses calculation = yes)
		  (FirstConclusions DeltaCreatinine between (.5 1.5)))
		 ($execute
		  (Hypotheses atn.PosEvidence = .5)))
  
  (v-h-2000.10.3 (?and
		  (Hypotheses calculation = yes)
		  (FirstConclusions DeltaCreatinine between (1.5 999)))
		 ($execute
		  (Hypotheses atn.PosEvidence = .3)))
  
  (v-h-2000.20 ($true)
	       ($execute
                (Hypotheses calculation = no)))
  
  
  
  
  ;;; scoring Hypothesis * rejection * After2Days * DeltaCreatinine
  
  (v-h-2200 (?and
             (Hypotheses rejection = considered)
             (Patient Diagnosis.certain oldvalue-1= atn)
             (val< (max.in (<- Patient :recall-timed-values
                               'Diagnosis.certain.PosEvidence
                               (<- Session :get 'SessionTime)
                               36
                               'before
                               12
                               :recall))
		   .6)
             (not (member 'yes (<- Therapy :recall-timed-values
                                   'Dialysis
                                   (<- Session :get 'SessionTime)
                                   48
                                   'before
                                   0
                                   :recall))))
	    ($execute
             (Hypotheses calculation = yes)))
  
  (v-h-2200.10 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions DeltaCreatinine between (0 .1)))
	       ($execute
                (Hypotheses rejection.PosEvidence = .1)))
  
  (v-h-2200.20 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions DeltaCreatinine between (.1 .5)))
	       ($execute
                (Hypotheses rejection.PosEvidence = .3)))
  
  (v-h-2200.30 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions DeltaCreatinine between (.5 1.0)))
	       ($execute
                (Hypotheses rejection.PosEvidence = .5)))
  
  (v-h-2200.40 (?and
                (Hypotheses calculation = yes)
                (FirstConclusions DeltaCreatinine between (1.0 999)))
	       ($execute
                (Hypotheses rejection.PosEvidence = .7)))
  
  (v-h-2200.50 ($true)
	       ($execute
                (Hypotheses calculation = no)))
  
  
  ;;; Hypotheses * Rejection * RejUnderMPTherapy
  
  (v-h-2300 (?and
             (Hypotheses rejection = considered)
             (Patient Diagnosis.certain :old oldvalue-1= rejection)
             (Patient Diagnosis.certain :deltatime between (12 36))
             (Patient Diagnosis.certain.PosEvidence :old oldvalue-1> .6)
             (FirstConclusions DeltaCreatinine > 0)
             (ImmunoSuppression Methylprednisolone > 124)
             (ImmunoSuppression Methylprednisolone :deltatime < 24))
	    ($execute
             (Hypotheses rejection.PosEvidence = .7)))
  
  )


;;; ------------------------------------------------------------------------------
;;; Print Verified Hypotheses
;;; ------------------------------------------------------------------------------

(defrule-set :print-verified-hypotheses
  (p-v-h-0100 ($true)
              ($execute
               (say "~%~%~% ==============================================================")
               (say "~% The verification of the hypotheses ")
               (say "~% leads to the following certainties:")
               (say "~% ATN - positiv: ~S" (<- Hypotheses :GET 'ATN.PosEvidence))
               (say "~% ATN - negativ: ~S" (<- Hypotheses :GET 'ATN.NegEvidence))
               (say "~% ATN - subclassification: ~S"
                    (<- Hypotheses :GET 'ATN.subclassification))
               (say "~% rejection - positiv: ~S" (<- Hypotheses :GET 'rejection.PosEvidence))
               (say "~% rejection - negativ: ~S" (<- Hypotheses :GET 'rejection.NegEvidence))
               (say "~% rejection - subclassification: ~S"
                    (<- Hypotheses :GET 'rejection.subclassification))
               (say "~% =============================================================="))))


;;; ------------------------------------------------------------------------------
;;; Differential Diagnosis
;;; ------------------------------------------------------------------------------

;;; Die Bedingungsteile sind gemaess den Behaviors von HypothesesFrame zu gestalten, wo
;;; zur Zeit nur die Summierung von Evidenzwerten erfolgt.

(defrule-set :differential-diagnosis
  (d-d-0100 ($and
             (Hypotheses ATN.PosEvidence > .5)
             (Hypotheses rejection.PosEvidence < .5))
            ($conclude
             (Patient Diagnosis.certain = ATN)
             (Patient Diagnosis.certain.PosEvidence = (Hypotheses ATN.PosEvidence))))
  
  ;;; ##### Diagnosis.certain.PosEvidence muss im ersten Durchgang mit 'unknown' beantwortet
  ;;; werden !!!
  
  (d-d-0200 ($and
             (Hypotheses ATN.PosEvidence > .5)
             (Hypotheses rejection.NegEvidence > .5))
            ($conclude
             (Patient Diagnosis.certain = ATN)
             (Patient Diagnosis.certain.PosEvidence = (Hypotheses ATN.PosEvidence))))
  
  (d-d-0300 ($and
             (Hypotheses ATN.PosEvidence < .5)
             (Hypotheses rejection.PosEvidence > .5))
            ($conclude
             (Patient Diagnosis.certain = rejection)
             (Patient Diagnosis.certain.PosEvidence = (Hypotheses rejection.PosEvidence))))
  )


;;; ------------------------------------------------------------------------------
;;; Print Differential Diagnosis
;;; ------------------------------------------------------------------------------

(defrule-set :print-differential-diagnosis
  (p-d-d-0100 ($true)
              ($execute
               (say "~% Results of the differential diagnosis:")
               (say "~% Diagnosis.certain: ~S" (<- Patient :GET 'Diagnosis.certain))
               (say "~% Diagnosis.possible: ~S" (<- Patient :GET 'Diagnosis.possible)))))


;;; ------------------------------------------------------------------------------
;;; Ask Loop End
;;; ------------------------------------------------------------------------------

(defrule-set :ask-loop-end
  (i-0050 ($true)
          ($ask
           (ExTra loopend)
           )))



;;; ------------------------------------------------------------------------------
;;; Save Instances
;;; ------------------------------------------------------------------------------

;;; Sichern der aktuellen KB
;;; Der Pfadname fuer den Output-Stream ist entweder zu modifizieren oder
;;; per Default-Pathname zu generieren.

(defrule-set :save-instances
  
  (s-i-100 ($true)                                                       ;File-Output
           ($execute
            (ExTra DataSource = FILE)
            (with-open-file (stream (concatenate 'string 
                                                 (namestring (user-homedir-pathname))
                                                 (string-downcase (send-kb :kb-name))
                                                 ".kb")
                                    :direction :output
                                    :if-exists :overwrite
                                    :if-does-not-exist :create)
              (dolist (element (send-kb :instances))
                (print (send-fp :save-instance element) stream)))))
  )

;;; ------------------------------------------------------------------------------
;;; unparse instances (methods) 
;;; ------------------------------------------------------------------------------

;;; Methoden zur Generierung der Output-Daten beim Sichern der KB (siehe Save-Instances)

;(def$method (frame :unparse-slot)
;	   (slot-name &optional (all-properties t) internal-properties)
;  (let ((header `(,slot-name :value ,($send self :get-value-only slot-name))))
;    (setf internal-properties (or internal-properties
;				  ($send self :internal-properties)))
;    (cond ((null all-properties) header)
;	  (t (append header
;		     (do ((plist (rest1 (get-slot-plist slot-name))
;				 (cddr plist))
;			  (result nil))
;			 ((null plist) (reverse result))
;		       (unless (member (first plist) internal-properties)
;			 (setf result (cons (second plist)
;					    (cons (first plist)
;						  result))))))))))
; 
;(def$method (frame :unparse-instance)
;	   (&optional slot-list (all-properties t) internal-properties)
;  (setf internal-properties (or internal-properties
;				($send self :internal-properties)))
;  (append `(,object-name ,(second (frame-definition (typep self))))
;	  (mapcar #'(lambda (slot)
;		      ($send self :unparse-slot
;				    slot all-properties internal-properties))
;		  (or slot-list slots))))
;

(def$method (normal-frame-processor :save-instance) (instance-name)
  (let ((unparsed-instance
         (<- instance-name
             :unparse-instance nil t '(:possible-values))))
    `(definstance ,(first unparsed-instance) of ,(second unparsed-instance) with 
                  ,@(mapcan #'(lambda (slot-spez)
                                `(,(first slot-spez) = ,(rest (rest slot-spez))))
                            (rest (rest unparsed-instance))))))




;;; ------------------------------------------------------------------------------
;;; Instructions
;;; ------------------------------------------------------------------------------

(instructions
 (let ()
   (declare (special ExTra))
   (loop
     (cond ((equal (<- ExTra :get `loopend) 'end)
            (return)))
     (<- ExTra :put `loopend '-)
     (SAY "~%##### BUILD INSTANCES")
     (send-kb :find-implications :build-instances :do-all)
     (say "~%##### QUESTIONS")
     (send-kb :find-implications :questions :do-all)	
     (say "~%##### INITIAL TIME")
     (send-kb :find-implications :initial-time :do-all)
     (say "~%##### DATA ABSTRACTION")
     (send-kb :find-implications :data-abstraction :do-all)
     (send-kb  :find-implications :print-abstractions :do-all)
     (say "~%##### GENERATE HYPOTHESES")
     (send-kb :obtain 1 '(Hypotheses) :generate-hypotheses)
     (send-kb :find-implications  :print-generated-hypotheses :do-all)
     (say "~%##### VERIFY HYPOTHESES")
     (send-kb :find-implications  :verify-hypotheses  :do-all)
     (send-kb :find-implications  :print-verified-hypotheses :do-all)
     (say "~%##### DIFFERENTIAL DIAGNOSIS")
     (send-kb :obtain :all '(Patient) :differential-diagnosis)
     (send-kb :find-implications  :print-differential-diagnosis :do-all)
     (say "~%##### LOOP END QUESTION")
     (send-kb :find-implications  :ask-loop-end :do-all)
     (<- ExTra :put `DataSource 'LOOP))
   (<- ExTra :put `DataSource 'FILE)
   (<- ExTra :put 'loopend '-)
   (send-kb :find-implications  :save-instances :do-all)
   (<- ExTra :put `DataSource '-)
   (say "~%***** END OF EXTRAGMD-SESSION *****~%")
   t))

;;; eof





