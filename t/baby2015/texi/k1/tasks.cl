;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")
 
;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89

(def-kb-instance tasks&agendas k1dummyc :pkg :ks)

;;; make sure unit-kb is loaded and imported
(do ()
    ((member 'unit-kb *known-knowledge-bases*)
     (progn
       ($send tasks&agendas :make-yourself-current)
       (send-kb :send-if-handles :import-kb unit-kb)
       "unit-kb imported"))
  (if (not (member 'unit-kb *known-knowledge-bases*))
    (cerror "Load << unit-kb >> before proceeding!"
            "Unknown Knowledge Base ~S" 'unit-kb)))

;;; a knowledge-base which realize an AGENDA CONTROL STRUCTURE using
;;; objects called TASKS.
;;; created by Karl Wittur 23.5.86

;; ******************** TRACING STUFF ********************

;; these variables are for tracing and resetting purposes only
;; they should not be accessed by the user

(defvar *ACTUAL-TASK-CALLER* nil)

(defvar *ACTUALLY-CALLED-TASK* nil)

(defvar *THROW-FLAG* nil)


;; ******************** TASK DEFINITION ********************


;; callers is a list of all the tasks which activated this task
;; each element of callers has the form (<caller-spec> <stopping-spec>)
;; where <caller-spec> is of the following form:
;;
;; a) if the task was activated through the current-kb it is the
;;    name of the current-kb
;; b) otherwise it is a list of two elements (<task-name> <activation-number>)
;;    where <activation-number> indicates that the <activation-number>th activation
;;    of <task-name> led to this activation
;;
;; <stopping-spec> has the form
;;
;; a) (<task-name> FINISHED) if this task was finished regularly or
;; b) (<task-name> <activation-number>) if a <activation-number>th activation
;;    of task has led to the stopping of this task.
;;
;; called-tasks are used to record all the tasks this task has activated
;;
;; task-trace-on is a flag for tracing-purposes



(DEFFRAME task
  (SLOTS (callers nil :current-caller nil)
	 (called-tasks nil)
	 (task-trace-on t)))


;; ____________________________________________________________

;; ******************** SOME BASIC OPERATIONS ********************


;(DEFBEHAVIOR (task :before :initialize) (&rest arg)
;
;  "by default no history."
;  
;  (declare (ignore arg))
;  (<- self :history-off))


;; ____________________________________________________________


(DEFBEHAVIOR (task :reset) ()
  
  "resets the basis slots and properties."
  
  (setf ($value 'callers) nil)
  (setf ($value 'callers :current-caller) nil)
  (setf ($value 'called-tasks) nil))

;; ____________________________________________________________


(DEFBEHAVIOR (task :get-current-caller) ()
  
  "gets the current task-spec which called this one or nil if this task is not active."
  
  ($value 'callers :current-caller))

;; ____________________________________________________________



(DEFBEHAVIOR (task :get-name-of-current-caller) ()
  
  "gets the name of the last caller."
  
  (let ((kb-name (if (send-kb :operation-handled-p :it-initial-function)
                   (send-kb :global-kb-name)             ;; for TEX-I-BYLON
                   (send-kb :kb-name))) 
        (current-caller ($value 'callers :current-caller)))
    (if (null current-caller)
      nil
      (if (eq kb-name current-caller)
        kb-name
        (first current-caller)))))


;; ____________________________________________________________



(DEFBEHAVIOR (task :get-last-caller) ()

  "gets the last task-spec which called this one or nil if this task was not yet called."

  (first ($value 'callers)))


;; ____________________________________________________________


(DEFBEHAVIOR (task :get-name-of-last-caller) ()
  
  "gets the name of the last caller."
  
  (let ((kb-name (if (send-kb :operation-handled-p :it-initial-function)
		     (send-kb :global-kb-name)                 ;; for TEX-I-BYLON
		     (send-kb :kb-name)))  
	(last-caller (first (<- self :get-last-caller))))
    (if (eq kb-name last-caller)
	kb-name
	(first last-caller))))

 
;; ____________________________________________________________


(DEFBEHAVIOR (task :get-nth-activation-information) (n)

  "gets the nth activation-info (i.e a list of the form (<caller-spec><reason-spec>)."
  
  (if (zerop n)
      nil
      (let ((the-callers (if (not (<- self :is-active))
			     (reverse ($value 'callers))
			     (reverse `(,(list ($value 'callers :current-caller))
					. ,($value 'callers))))))
	(if (> n (length the-callers))
	    nil
	    (nth (1- n) the-callers)))))

;; ____________________________________________________________


(DEFBEHAVIOR (task :get-caller-of-nth-activation) (n)

  "gets the n-th caller of this task."
  
 (first (<- self :get-nth-activation-information n)))

;; ____________________________________________________________


(DEFBEHAVIOR (task :get-nth-stopping-reason) (n)

  "gets the n-th stopping reason of this task."
  
 (second (<- self :get-nth-activation-information n)))

;; ____________________________________________________________ 


(DEFBEHAVIOR (task :get-activation-path-for-nth-activation) (n)
  
  " USER   gets the nth activation path."
  
  (let ((nth-activator (<- self :get-caller-of-nth-activation n)))
    (if (null nth-activator)
      nil
      (if (eq (if (send-kb :operation-handled-p :it-initial-function)
                (send-kb :global-kb-name)                     ;; for TEX-I-BYLON
                (send-kb :kb-name)) 
              nth-activator)
        `(,nth-activator)
        (append (<- (first nth-activator) :get-activation-path-for-nth-activation (second nth-activator))
                `(,(first nth-activator)))))))
  

;; ____________________________________________________________ 

(DEFBEHAVIOR (task :get-current-activation-path) ()
  
  " USER   gets all the tasks the current activation of this task depends on."

  (if (<- self :is-active)
      (<- self :get-activation-path-for-nth-activation (<- self :get-activation-number))
      nil))

;; ____________________________________________________________


(DEFBEHAVIOR (task :get-last-activation-path) ()
  
  " USER   gets the last activation path for this task.
if task is active the current caller is not considered."
  
  (<-  self :get-activation-path-for-nth-activation (length ($value 'callers))))

 
;; ____________________________________________________________


(DEFBEHAVIOR (task :get-last-stopping-reason) ()

  "accesses the stopping-spec of the last-caller."
  
  (let ((last-caller (<- self :get-last-caller)))
    (if (null last-caller)
	nil
	(second last-caller))))

;; ____________________________________________________________


(DEFBEHAVIOR (task :get-name-of-last-stopping-task) ()

  "selects the name of the last stopping-spec."
  
  (let ((last-stopping-reason (<- self :get-last-stopping-reason)))
    (if (null last-stopping-reason)
	nil
	(first last-stopping-reason))))


;; ____________________________________________________________


(DEFBEHAVIOR (task :get-directly-called-tasks) ()

  " USER   gets all tasks which were called through this task."
  
  ($value 'called-tasks))


;; ____________________________________________________________



(DEFBEHAVIOR (task :get-current-activation-path-from-here) ()

  "gets the path to all direct and indirect currently active tasks."

  (if (<- self :is-active)
      `(,@(rest (member ($send self :object-name) 
                        (<-  *ACTUALLY-CALLED-TASK* :get-current-activation-path)))
	,*ACTUALLY-CALLED-TASK*)
      nil))


;; ____________________________________________________________



(DEFBEHAVIOR (task :get-activated-tasks) (&optional (mode 'all))
  
  " USER   yields all or only the terminal tasks which were
started directly or indirectly from this task."
  
  (let ((result nil))
    (dolist (a-task (<- self  :get-directly-called-tasks) result)
      (append result 
              (if (not (eq mode 'all))
                (if (null (<- a-task :get-directly-called-tasks))
                  `(,a-task)
                  (<- a-task :get-activated-tasks mode))
                (cons a-task (<- a-task :get-activated-tasks mode)))))))

;; ____________________________________________________________


(DEFBEHAVIOR (task :set-current-caller) (a-task-spec)
  
  "sets the current-caller of this task.
it is not possible to call an already active task."

  (let ((current-caller ($value 'callers :current-caller))) 
    (if (null current-caller)
	(setf ($value 'callers :current-caller) a-task-spec)
	(error "~% TASK ~S IS ALREADY ACTIVE" ($send self :object-name)))))

;; ____________________________________________________________


(DEFBEHAVIOR (task :get-activation-number) ()

  "the activation number of a task."
  
  (if (<- self :is-active)
      (1+ (length ($value 'callers)))
      (length ($value 'callers))))


;; ____________________________________________________________


(DEFBEHAVIOR (task :add-to-called-tasks) (a-task)
  
  "behavior which records that a-task is activated through this task."

  (setf ($value 'called-tasks)
	(cons a-task ($value 'called-tasks))))


;; ____________________________________________________________


(DEFBEHAVIOR (task :add-to-callers) (a-task-spec)

  "a-task-spec is added (pushed) to callers."
  
  (setf ($value 'callers)
	(cons a-task-spec ($value 'callers))))


;; ____________________________________________________________


(DEFBEHAVIOR (task :is-active) (&optional (mode :RECALL))

  " USER   t iff this task is active."

  (when (eq mode :RECALL)
    (not (null ($value 'callers :current-caller)))))

;; ____________________________________________________________


(DEFBEHAVIOR (task :before :start) (&optional arg (mode :REMEMBER))
  
  "maintains the property :current-caller of slot caller."
  
  (declare (ignore arg mode))
  (let* ((kb-name (if (send-kb :operation-handled-p :it-initial-function)
                    (send-kb :global-kb-name)                      ;; for TEX-I-BYLON      
                    (send-kb :kb-name)))  
         (the-caller
          (if (eq *ACTUAL-TASK-CALLER* kb-name)
            kb-name
            `(,*ACTUAL-TASK-CALLER* ,(<- *ACTUAL-TASK-CALLER* :get-activation-number)))))
    (<- self :set-current-caller the-caller)
    (if (eq the-caller kb-name)
      nil
      (<- (first the-caller) :add-to-called-tasks ($send self :object-name)))))


;; ____________________________________________________________

(DEFBEHAVIOR (task :start) (&optional arg (mode :REMEMBER))
  
  " USER   each task understands the message :start, which by default does nothing.
Other kinds of behavior can be implemented using specialisations 
that override this start-behavior."
  
  (declare (ignore arg mode))
  nil)

;; ____________________________________________________________


(defwhopper #+:SYMBOLICS(:start ks::task) #-:SYMBOLICS(ks::task :start)
  (&optional arg (mode :REMEMBER))
  
  "the whopper sets for each activation of a task (via :start)
a tag which is the object-name of the instance beeing started.
This tag is "throwable" and enables that every started task can be stopped.
Tracing is done before and after each activation of a task."
  
  (<- self :set-global-variables)
  (if (<- self :is-traced-task)
    (<- self :task-trace 'started *ACTUAL-TASK-CALLER*))
  (let ((aborted t))
    (unwind-protect
      (prog1 
        (catch ($send self :object-name)
          (prog1 (continue-whopper arg mode)
            (<- self :set-finished-normal)))
        (if (<- self :is-active) (<- self :stopped-by *ACTUALLY-CALLED-TASK*))
        (setf aborted nil))
      (if aborted
        (cond (*THROW-FLAG* (<- self :set-stopped-on-the-fly *ACTUALLY-CALLED-TASK*))
              (t (<- self :reset-global-variables)
                 (<- self :reset)))))))

;; ____________________________________________________________


(defbehavior (task :set-global-variables) ()
  (setq *ACTUAL-TASK-CALLER*
        (if (null *ACTUAL-TASK-CALLER*)
          (if (send-kb :operation-handled-p :it-initial-function)
            (send-kb :global-kb-name)                    ;; for TEX-I-BYLON
            (send-kb :kb-name))               
          *ACTUALLY-CALLED-TASK*)
        *ACTUALLY-CALLED-TASK* ($send self :object-name)
        *THROW-FLAG* nil))

(defbehavior (task :reset-global-variables) ()
  (setq *ACTUALLY-CALLED-TASK* (<- self :get-name-of-current-caller))
  (setq *ACTUAL-TASK-CALLER* 
        (if (eq *ACTUALLY-CALLED-TASK*
                (if (send-kb :operation-handled-p :it-initial-function)
                  (send-kb :global-kb-name)             ;; for TEX-I-BYLON
                  (send-kb :kb-name)))          
          nil
          (<- *ACTUALLY-CALLED-TASK* :get-name-of-current-caller))))


(defbehavior (task :set-finished-normal) ()
  (<- self :task-trace 'finished nil)
  (<- self :add-finished-caller)
  ;; the following variables must be set before inactivating this task
  (<- self :reset-global-variables)
  (<- self :inactivate))			


(defbehavior (task :add-finished-caller) ()
  (let* ((old-callers ($value 'callers))
	 (my-caller ($value 'callers :current-caller))
	 (finished-reason `(,($send self :object-name) finished))
	 (new-caller-spec `(,my-caller ,finished-reason)))
    (setf ($value 'callers) `(,new-caller-spec ,@old-callers))))

(defbehavior (task :inactivate) ()
  (setf ($value 'callers :current-caller) nil))


(defbehavior (task :stopped-by) (because)
  (<- self :task-trace 'stopped-direct because)
  (<- self :add-stopped-caller because)
    ;; the following variables must be set before inactivating this task
  (<- self :reset-global-variables)
  (<- self :inactivate))


(defbehavior (task :add-stopped-caller) (because)
  (let* ((old-callers ($value 'callers))
	 (my-caller ($value 'callers :current-caller))
	 (finished-reason `(,because ,(<- because :get-activation-number)))
	 (new-caller-spec `(,my-caller ,finished-reason)))
    (setf ($value 'callers) `(,new-caller-spec ,@old-callers))))


(defbehavior (task :set-stopped-on-the-fly ) (because)
  (<- self :task-trace 'stopped-indirect because)
  (<- self :add-stopped-caller because)
  (<- self :inactivate))



;; ____________________________________________________________



(DEFBEHAVIOR (task :before :stop) (&optional arg (mode :REMEMBER))
  (declare (ignore arg mode))
  (if (not (<- self :is-active))
    (error "TASK ~S IS NOT ACTIVE. YOU CAN ONLY STOP AN ACTIVE TASK" 
           ($send self :object-name))
    (setq *THROW-FLAG* t)))

;; ____________________________________________________________


(DEFBEHAVIOR (task :stop) (&optional arg (mode :REMEMBER))

  " USER   each task can be stopped via this message. 
It performs a throw to that tag which is identical to object-name. 
Stopping means that no more operations of the task after the stop-message will evaluate.
If a task is stopped additional args can be passed to the specified catch. 
These args are passed as results to the catch-form."

  (when (eq mode :REMEMBER)
    (throw ($send self :object-name) arg)))


;; ____________________________________________________________


(DEFBEHAVIOR (task :stop-current-caller) (&optional arg (mode :REMEMBER))
  
  " USER  stops the current-caller if possible."
  
  (cond ((not (<- self :is-active))
         (error "TASK ~S IS NOT ACTIVE. YOU CAN ONLY STOP AN ACTIVE TASK" 
                ($send self :object-name)))
        ((eq (<- self :get-name-of-current-caller)
             (if (send-kb :operation-handled-p :it-initial-function)
               (send-kb :global-kb-name)                    ;; for TEX-I-BYLON
               (send-kb :kb-name))) 
         (error "THE TASK ~S WAS NOT ACTIVATED BY ANY OTHER TASK" 
                ($send self :object-name)))
        (T (<- (<- self :get-name-of-current-caller) :stop arg mode))))


;; ____________________________________________________________
	 
(DEFBEHAVIOR (task :stop-all-tasks) (&optional arg (mode :REMEMBER))

  " USER  stops all currently active tasks."

  (<- (second (<- self :get-current-activation-path)) :stop arg mode))


;; ____________________________________________________________

(DEFBEHAVIOR (task :stop-all-agendas) (&optional arg (mode :REMEMBER))

  " USER   the outermost agenda which contains this task is send a stop message. 
If the task is not contained in an agenda the task itself will be stopped."
  
  (<-  (<- self :get-top-agenda) :stop arg mode))


;; ____________________________________________________________


(DEFBEHAVIOR (task :get-top-agenda) ()
  
  "gets the outermost active agenda which contains this task
or object-name if this task is not contained in an active agenda."
  
  (let ((current-caller (<- self :get-name-of-current-caller)))
    (if (or (null current-caller)
            (eq current-caller
                (if (send-kb :operation-handled-p :it-initial-function)
                  (send-kb :global-kb-name)            ;; for TEX-I-BYLON
                  (send-kb :kb-name)))  
            (not (<- current-caller :type 'agenda)))
      ($send self :object-name)
      (<- current-caller :get-top-agenda))))

;; ____________________________________________________________


(DEFBEHAVIOR (task :stop-current-agenda) (&optional arg (mode :REMEMBER))
  
  " USER  stops my-agenda."
  
  (<- (<- self :get-my-agenda) :stop arg mode))

;; ____________________________________________________________


(DEFBEHAVIOR  (task :get-my-agenda) ()

  "gets the name of the current caller if it is an agenda else object-name."

  (let ((name-of-current-caller (<- self :get-name-of-current-caller)))
    (if (not (null name-of-current-caller))
	(if (<- name-of-current-caller :type 'agenda)
	    name-of-current-caller
	    ($send self :object-name))
	($send self :object-name))))

;; ____________________________________________________________



;; *********************** TRACING STUFF ***********************


(DEFBEHAVIOR (task :set-task-trace) (t-or-nil &optional (mode :REMEMBER))
  
  " USER   behavior for setting the trace-mode."
  
  (declare (ignore mode))  
  (setf ($value 'task-trace-on) t-or-nil))


;; ____________________________________________________________


(DEFBEHAVIOR (task :is-traced-task) (&optional (mode :RECALL))
  
  "predicate for test if trace-mode is set."

  (declare (ignore mode))  
  ($value 'task-trace-on))
 
;; ____________________________________________________________


(DEFBEHAVIOR (task :task-trace) (kind-of-trace reason)
  
  "default trace for starting and stopping a task."
  
  (let* ((vorspann `(:string ,(format nil "  : TASK ")))
         (first-involved-task 
          (<- self :generate-mouse-sensitive-item
              (case kind-of-trace
                ((stopped-indirect  finished) ($send self :object-name))
                ((started stopped-direct) reason)
                (t (error "wrong kind-of-trace ~S in :task-trace" kind-of-trace)))))
         (kind-of-affairs 
          `(:string ,(case kind-of-trace
                       (started " STARTS ")
                       (stopped-indirect " STOPPED INDIRECT ")
                       (stopped-direct " STOPPED ")
                       (finished " FINISHED ")
                       (t (error "wrong kind-of-trace ~S in :task-trace" kind-of-trace)))))
         (second-involved-task 
          (case kind-of-trace
            ((stopped-indirect finished) '(:string ""))
            ((started stopped-direct)
             (if (eq ($send self :object-name) reason)
               `(:string "ITSELF")
               (<- self :generate-mouse-sensitive-item ($send self :object-name))))
            (t (error "wrong kind-of-trace ~S in :task-trace" kind-of-trace))))
         (blanks-to-fill-in 
          (<- self :generate-blank-item 
              first-involved-task kind-of-affairs second-involved-task))
         (short-line 
          `(:string ,(format nil "~S ~S ~A"
                             (if (eq kind-of-trace 'started)
                               reason
                               (<- self :get-name-of-current-caller))
                             (if (eq kind-of-trace 'started)
                               '=>
                               `<=)
                             (if (member  kind-of-trace '(stopped-indirect finished))
                               ""
                               ($send self :object-name)))))) 
    (send-kb :send-system-trace-window  :format
             vorspann
             first-involved-task
             kind-of-affairs
             second-involved-task
             blanks-to-fill-in
             short-line)))


(DEFBEHAVIOR (task :generate-mouse-sensitive-item) (an-instance)
  (if (eq (if (send-kb :operation-handled-p :it-initial-function)
            (send-kb :global-kb-name)                    ;; for TEX-I-BYLON
            (send-kb :kb-name)) 
          an-instance)
    `(:string ,(format nil "~S" an-instance))
    `(:mouse (:mouse-click :eval (,(get-instance an-instance)
                                  :get-task-trace-information)
                           :documentation
                           ,(format nil
                                    "Click here to show some information about the ~S ~S."
                                    (<- an-instance :type)
                                    an-instance))
             :string ,(format nil "~S" an-instance))))


(DEFBEHAVIOR (task :generate-blank-item) 
  (first-involved-task kind-of-affairs second-involved-task)
  (let* ((fit-string (case (first first-involved-task)
                       (:string (second first-involved-task))
                       (t (cadr (member :string first-involved-task)))))
         (koa-string (second kind-of-affairs))
         (sit-string (case (first second-involved-task)
                       (:string (second second-involved-task))
                       (t (cadr (member :string second-involved-task)))))
         (spaces-to-insert (- 60 (+ 10
                                    (length fit-string)
                                    (length koa-string)
                                    (length sit-string))))
         (control-string (if (minusp spaces-to-insert) 
                           "~@T"
                           (format nil "~~~S@T" spaces-to-insert)))) 
    `(:string ,(format nil "~?" control-string nil))))


;; ____________________________________________________________


(DEFBEHAVIOR (task :get-task-trace-information) ()
  
  "pops up a menu for displaying special traced informations."
  
  (send-kb :CHOOSE-FROM-MENU
           `(("Get Last Activation Path" :eval
              (,self :show-list-as-menu :get-last-activation-path
                     ,(format nil "Last Tasks path to ~S" ($send self :object-name))))
             ("Get Current Activation Path" :eval
              (,self :show-list-as-menu :get-current-activation-path
                     ,(format nil "Current Tasks path to ~S" ($send self :object-name))))
             ("Get Activated Tasks" :eval
              (,self :show-list-as-menu :get-activated-tasks
                     ,(format nil "Tasks activated by ~S" ($send self :object-name)))))
           " Select Task Operation: "))

;; ____________________________________________________________


(DEFBEHAVIOR (task :show-list-as-menu) (list-generator &optional label)
  
  "list-generator is interpreted as the name of a behavior 
which is send to self and should result a list. 
this list is shown as a menu in which no item is selectable."
  
  (let ((item-list (mapcar #'(lambda (item)
                               `(,item :no-select t))
                           ($send self list-generator))))
    (if (null item-list)
      (setq item-list '(("No Elements" :no-select t))))
    (send-kb :CHOOSE-FROM-MENU 
             item-list 
             (if (null label)
               (format nil "result of ~S" list-generator)
               label))))

;; ____________________________________________________________

;; ******************** AGENDA DEFINITION ********************


;; an agenda is a special task. Its purpose is to maintain a list of
;; tasks. 


(DEFFRAME agenda
  (supers task)
  (SLOTS (list-of-tasks nil
	  :POSSIBLE-VALUES :LIST-OF-TASKS)))

;; ___________________________________________________________

(DEFINE-POSSIBLE-VALUES-BEHAVIOR (agenda :LIST-OF-TASKS) (slot-value)
  (or (null slot-value)
      (and (listp slot-value)
           (every #'(lambda (x) (and (is-instance x)
                                     (<- x :type 'TASK))) slot-value))))

;; ____________________________________________________________


(DEFBEHAVIOR (agenda :start) (&optional arg (mode :REMEMBER))
  
  " USER   the principle evaluation-loop for a task.
special strategies can be implemented by specialisation of
agenda and overriding of the next-task-message which is by
default :pop."
  
  (when (eq mode :REMEMBER)
    (loop 
      (if (<- self :empty) 
        (return t)
        (<- (<- self :next-task) :start arg mode)))))

;; ____________________________________________________________


;; ********** SOME BASIC OPERATIONS ON AGENDAS **********




(DEFBEHAVIOR (agenda :next-task) ()
  
  " USER  default behavior for selecting the next-task to start."
  
  (let ((top (first ($value 'list-of-tasks))))
    (setf ($value 'list-of-tasks) (rest ($value 'list-of-tasks))) 
    top))


;; ____________________________________________________________


(DEFBEHAVIOR (agenda :forget-next-tasks) (&optional (mode :REMEMBER))
  
  " USER  sets list-of-tasks to nil."

  (when (eq mode :REMEMBER) (<- self :clear)))


;; ____________________________________________________________


(DEFBEHAVIOR (agenda :clear) (&optional (mode :REMEMBER))
  
  "sets list-of-tasks to nil."

  (when (eq mode :REMEMBER)
    (setf ($value 'list-of-tasks) nil)))


;; ____________________________________________________________

 
(DEFBEHAVIOR (agenda :empty) (&optional (mode :RECALL))
  
  "test if agenda is empty."

  (when (eq mode :RECALL)
    (null ($value 'list-of-tasks))))


;; ____________________________________________________________



(DEFBEHAVIOR (agenda :delete-task) (a-task &optional (which 1)(mode :REMEMBER))
  
  " USER  delete the which-th occurance of a-task from the list-of-tasks."
  
  (when (eq mode :REMEMBER)
    (setf ($value 'list-of-tasks)
          (if (eq which 'all)
            (remove a-task ($value 'list-of-tasks) :test 'equal)
            (remove a-task ($value 'list-of-tasks) :test 'equal :count which)))	
    a-task))

;; ____________________________________________________________


(DEFBEHAVIOR (agenda :do-next) (a-task &optional (mode :REMEMBER))
  
  " USER   if a-task is already contained in this agenda
it is activated next, otherwise it is pushed on this agenda."

  (when (eq mode :REMEMBER)
    (<- self :delete-task a-task)
    (<- self :push-task a-task)))


;; ____________________________________________________________


(DEFBEHAVIOR (agenda :do-immediately) (a-task &optional (mode :REMEMBER))
  
  " USER   stops the currently active task and starts a-task next."
  
  (when (eq mode :REMEMBER)
    (when (<- self :is-active)
      (<- self :do-next a-task)
      (<- (first ($value 'called-tasks)) :stop))))
  

;; ____________________________________________________________


(DEFBEHAVIOR (agenda :push-task) (a-task &optional (mode :REMEMBER))
  
  " USER   push a task in this agenda and sets the slot "associated-agendas" 
of that task to this agenda a-task must be an instance of TASK."

  (when (eq mode :REMEMBER)
    (<- self :put 'list-of-tasks `(,a-task . ,($value 'list-of-tasks))))) 


;; ____________________________________________________________


(DEFBEHAVIOR (agenda :add-tasks) (a-task-list &optional (mode :REMEMBER))
  
  " USER   add a list of tasks to agenda (in front)."

  (when (eq mode :REMEMBER)
    (if (not (listp a-task-list))
      (error "~%~S is not a list" a-task-list)
      (dolist (a-task (reverse a-task-list))
        (<- self :push-task a-task)))))


(if (member 'tasks&agendas *known-knowledge-bases*)
  ($send tasks&agendas :send-if-handles :export-kb))


;;; eof

