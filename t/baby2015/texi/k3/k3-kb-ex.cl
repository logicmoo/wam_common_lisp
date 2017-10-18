;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: BABYLON; Base: 10 -*-

(in-package "BABYLON")

;;; adopted to non SYMBOLICS machines for BABYLON 2.1 by Juergen Walther 10.5.89

#|

This document gives a short working instruction to K3 facilities.
It assumes full familiarity with the K3-Specification report, with BABYLON, 
and with CONSAT.

The document is provisional, it is not meant to replace a user manual
or reference manual. The idea is to work through a simple example, experiment with
it, and by the way learning  the semantics and usage of K3 constructs.

An example that is simple, meaningful, and illustrates at least most 
of the facilities, is difficult to find. Another problem is that a 
meaningful example, which is also simple, almost always gives rise to
questions dealing with modeling, e.g. why did you model it in this way,
and not in mine, why did you not define an own region for this phenomenon, etc.
We therefore decided to work with an abstract example that is not meant to convey 
any meaning to its slots, regions, and all this other stuff. As a disatvantage,
you might have some trouble to remember the structure of the components,
what slot belong to what component, which constraints were defined in this region,
etc. We will therefore provide many comments and extensive information about the
internal state of the system during development.

You should first try to understand the internal structure of the following 
frames. They will later be used to define a hierarchy with a node of type
K0-TYPE at top and two nodes of type K1-TYPE and K2-TYPE as its leaves.

|#
;;*********************************************************************************


(def-kb-instance k3-kb-example k3c :pkg :ks)
 
;; the kb uses the definitions of k3-kb knowledge base

(do ()
    ((member 'k3-kb *known-knowledge-bases*)
     (progn
       ($send k3-kb-example :make-yourself-current)
       (send-kb :send-if-handles :import-kb k3-kb)
       "k3-kb imported"))
  (if (not (member 'k3-kb *known-knowledge-bases*))
    (cerror "Load << k3-kb >> before proceeding!"
            "Unknown Knowledge Base ~S" 'k3-kb)))

;;*********************************************************************************


(DEFFRAME k0-type
  (SUPERS physical-unit)
  (SLOTS  
   (k1 -
       :IS-A k1-type)
   (k2 -
       :IS-A k2-type)
   (v1 -)
   (v2 -)
   (constraints -
                :HORIZONTAL (((k1 v3)
                              (k2 v3)))
                :VERTICAL   (((half-and-half v1 
                                             (k1 v1-1)
                                             (k1 v2-1)))
                             ((k3-equal-3 v2
                                          (k2 v1-2)
                                          (k2 v2-2)))))))


(DEFFRAME k1-type
  (SUPERS physical-unit)
  (SLOTS
   (v1-1 -)
   (v2-1 -
         :possible-values (:one-of 0.0 2.0 4.0 8.0 -2.0 -4.0 -8.0))
   (v3 -)
   
   (constraints -
                :LOCAL ((local-1-1 (equal v1-1 v3))
                        (local-2-1 (not-equal v1-1 v3))))
   
   (wr1-1 -
          :REGION (AND (greater-0 v3)
                       (greater-0 v1-1)
                       (greater-0 v2-1))
          :EXCLUDES (local-2-1)
          :INCLUDES ((equal v1-1 v2-1)))
   
   (wr2-1 -
          :REGION (OR 
                   (less-or-equal-0 v3)
                   (less-or-equal-0 v1-1))
          
          :EXCLUDES (local-1-1))))
  
  
  
(DEFFRAME k2-type
  (SUPERS physical-unit)
  (SLOTS
   (v1-2 -)
   (v2-2 -)
   (v3 -)
   
   (CONSTRAINTS -
                :LOCAL ((LOCAL-1-2 (EQUAL V1-2 V3))
                        (LOCAL-2-2 (NOT-EQUAL V1-2 V3))))
   
   (A1-2 -
         :REGION (AND (GREATER-0 V3)
                      (GREATER-0 V1-2))
         :EXCLUDES (LOCAL-2-2))
   
   
   (A2-2 -
         :REGION  (OR 
                   (LESS-OR-EQUAL-0 V3)
                   (LESS-OR-EQUAL-0 V1-2))
         :EXCLUDES (LOCAL-1-2))))
						
;;*********************************************************************************

;These definitions must be completed by specifying the constraints that
;were mentioned. Here are some suitable definitions:

;;*********************************************************************************



(DEFCONSTRAINT equal
  (:TYPE PRIMITIVE)
  (:INTERFACE arg1 arg2)
  (:RELATION  (:PATTERN (arg2 arg2) :IF (constrained-p arg2))
              (:PATTERN (arg1 arg1) :IF (constrained-p arg1)))
  (:CONDITION :OR))

(DEFCONSTRAINT greater-0
  (:TYPE PRIMITIVE)
  (:INTERFACE arg1)
  (:RELATION  (:PATTERN (arg1) :IF (> arg1 0.0)))
  (:CONDITION (constrained-p arg1)))

(DEFCONSTRAINT less-or-equal-0
  (:TYPE PRIMITIVE)
  (:INTERFACE arg1)
  (:RELATION  (:PATTERN (arg1) :IF (<= arg1 0.0)))
  (:CONDITION (constrained-p arg1)))

(DEFCONSTRAINT not-equal
  (:TYPE PRIMITIVE)
  (:INTERFACE arg1 arg2)
  (:RELATION  (:PATTERN (arg1 arg2) :IF (<= arg1 arg2)))
  (:CONDITION (constrained-p arg1 arg2)))

(DEFCONSTRAINT half-and-half
  (:TYPE PRIMITIVE)
  (:INTERFACE arg1 arg2 arg3)
  (:RELATION
   (:PATTERN (arg1 (/ arg1 2.0) (/ arg1 2.0)) :IF (constrained-p arg1))
   (:PATTERN ((* arg2 2.0) arg2 arg2)         :IF (constrained-p arg2))
   (:PATTERN ((* arg3 2.0) arg3 arg3)         :IF (constrained-p arg3)))
  (:CONDITION :OR))


;;*********************************************************************************

;Now you are ready to create a component hierarchy like e.g.

;;*********************************************************************************

(DEFINSTANCE k2-inst OF k2-type)

(DEFINSTANCE k1-inst OF k1-type)

(DEFINSTANCE k0-inst OF k0-type
  WITH 
  k1 = (:NAME k1-inst)
  k2 = (:NAME k2-inst))

#|

;*********************************************************************************

Assume now that you have evaluated the preceding LISP expressions in the context of
a knowledge base provided with K1 and K3 facilities.
As a prerequisite for further working you should call the behavior
"install-constraints" addressed to the root of your component hierarchy:

;**********************************************************************************

(<- k0-inst :install-constraints)

;**********************************************************************************

In the future we will try to find a way to make this method unnecessary. It is 
merely there because of technical problems on implementation level.

You can now  experiment with the definitions as, for example, it is
demonstrated in the following session "simulation" with annotated comments.


You can examine all relevant definitions of constraints and restriction nets 
with the function "print-constraints". Without argument the result is forwarded
to *default-dialog-stream*, otherwise you can find it in the specified file.
At least part of the information provided by "print-constraints" might probably only
be useful to you if you are  interested in the implementation of K3 and/or if you want
to debug your program. If you only want to have a quick look at the
functionality of K3's mechanisms, you should skip over the lenghty result
of (print-constraints "net-out") which is listed below.
Notice that the original output of print-constraints was directed to
a file "...>net-out", which is only reproduced here.

;**********************************************************************************
;******************* result of (print-constraints "net-out") **********************
;**********************************************************************************

Restrictions of #<MINI-CFRLX-CONSTRAINT 266527524>:

Restrictions of Restriction Net CNET-FOR-K3-ROOT (#<RESTRICTION-NET 44104200>):

(K3-K2-TYPE-A1-2 (K2-INST A1-2) (K2-INST V3) (K2-INST V1-2)) 
(K3-K2-TYPE-A2-2 (K2-INST A2-2) (K2-INST V3) (K2-INST V1-2)) 
(K3-EQUAL-K2-INST (K2-INST V1-2) (K2-INST V3)) 
(K3-NOT-EQUAL-K2-INST (K2-INST V1-2) (K2-INST V3)) 
(K3-EQUAL-K1-INST (K1-INST V1-1) (K1-INST V2-1)) 
(K3-K1-TYPE-WR1-1 (K1-INST WR1-1) (K1-INST V3) (K1-INST V1-1) (K1-INST V2-1)) 
(K3-K1-TYPE-WR2-1 (K1-INST WR2-1) (K1-INST V3) (K1-INST V1-1)) 
(K3-EQUAL-K1-INST (K1-INST V1-1) (K1-INST V3)) 
(K3-NOT-EQUAL-K1-INST (K1-INST V1-1) (K1-INST V3)) 
(K3-EQUAL-2 (K1-INST V3) (K2-INST V3)) 
(HALF-AND-HALF (K0-INST V1) (K1-INST V1-1) (K1-INST V2-1)) 
(K3-EQUAL-3 (K0-INST V2) (K2-INST V1-2) (K2-INST V2-2)) 


Primitive Constraints of #<MINI-CFRLX-CONSTRAINT 266527524>:

 
(DEFCONSTRAINT  K3-NOT-EQUAL-K1-INST
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG1 ARG2) :IF (<= ARG1 ARG2)))
  (:CONDITION (AND (CONSTRAINED-P ARG1 ARG2)
                   (OR (EQUAL (GLOBAL-VALUES '(K1-INST WR2-1)
                                             'CNET-FOR-K3-ROOT)
                              '(T))))))

 
(DEFCONSTRAINT  K3-EQUAL-K1-INST
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN (ARG1 ARG1) :IF (CONSTRAINED-P ARG1)))
  (:CONDITION (AND (OR (CONSTRAINED-P ARG2)
                       (CONSTRAINED-P ARG1))
                   (OR (EQUAL (GLOBAL-VALUES '(K1-INST WR1-1)
                                             'CNET-FOR-K3-ROOT)
                              '(T))))))

 
(DEFCONSTRAINT K3C-LESS-OR-EQUAL-0
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 WW)
  (:RELATION (:PATTERN (ARG1 'T)
                       :IF
                       (AND (CONSTRAINED-P ARG1)
                            (<= ARG1 0.0)))
             (:PATTERN (ARG1 'NIL)
                       :IF
                       (AND (CONSTRAINED-P ARG1)
                            (NOT (<= ARG1 0.0)))))
  (:CONDITION (AND (CONSTRAINED-P ARG1)
                   (CONSTRAINED-P ARG1))))

 
(DEFCONSTRAINT K3C-GREATER-0
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 WW)
  (:RELATION (:PATTERN (ARG1 'T)
                       :IF
                       (AND (CONSTRAINED-P ARG1)
                            (> ARG1 0.0)))
             (:PATTERN (ARG1 'NIL)
                       :IF
                       (AND (CONSTRAINED-P ARG1)
                            (NOT (> ARG1 0.0)))))
  (:CONDITION (AND (CONSTRAINED-P ARG1)
                   (CONSTRAINED-P ARG1))))

 
(DEFCONSTRAINT K3-NOT-EQUAL-K2-INST
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG1 ARG2) :IF (<= ARG1 ARG2)))
  (:CONDITION (AND (CONSTRAINED-P ARG1 ARG2)
                   (OR (EQUAL (GLOBAL-VALUES '(K2-INST A2-2)
                                             'CNET-FOR-K3-ROOT)
                              '(T))))))

 
(DEFCONSTRAINT K3-EQUAL-K2-INST
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN (ARG1 ARG1) :IF (CONSTRAINED-P ARG1)))
  (:CONDITION (AND (OR (CONSTRAINED-P ARG2)
                       (CONSTRAINED-P ARG1))
                   (OR (EQUAL (GLOBAL-VALUES '(K2-INST A1-2)
                                             'CNET-FOR-K3-ROOT)
                              '(T))))))

 
(DEFCONSTRAINT HALF-AND-HALF
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 ARG3)
  (:RELATION (:PATTERN (ARG1 (/ ARG1 2.0) (/ ARG1 2.0)) :IF (CONSTRAINED-P ARG1))
             (:PATTERN ((* ARG2 2.0) ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN ((* ARG3 2.0) ARG3 ARG3) :IF (CONSTRAINED-P ARG3)))
  (:CONDITION (OR (CONSTRAINED-P ARG1)
                  (CONSTRAINED-P ARG2)
                  (CONSTRAINED-P ARG3))))

(DEFCONSTRAINT HALF-AND-HALF
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 ARG3)
  (:RELATION (:PATTERN (ARG1 (/ ARG1 2.0) (/ ARG1 2.0)) :IF (CONSTRAINED-P ARG1))
             (:PATTERN ((* ARG2 2.0) ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN ((* ARG3 2.0) ARG3 ARG3) :IF (CONSTRAINED-P ARG3)))
  (:CONDITION :OR))
 
(DEFCONSTRAINT NOT-EQUAL
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG1 ARG2) :IF (<= ARG1 ARG2)))
  (:CONDITION (CONSTRAINED-P ARG1 ARG2)))

 
(DEFCONSTRAINT LESS-OR-EQUAL-0
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1)
  (:RELATION (:PATTERN (ARG1) :IF (<= ARG1 0.0)))
  (:CONDITION (CONSTRAINED-P ARG1)))

 
(DEFCONSTRAINT GREATER-0
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1)
  (:RELATION (:PATTERN (ARG1) :IF (> ARG1 0.0)))
  (:CONDITION (CONSTRAINED-P ARG1)))

 
(DEFCONSTRAINT EQUAL
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN (ARG1 ARG1) :IF (CONSTRAINED-P ARG1)))
  (:CONDITION (OR (CONSTRAINED-P ARG2)
                  (CONSTRAINED-P ARG1))))

 
(DEFCONSTRAINT K3-OR-2
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 RES)
  (:RELATION (:TUPLE (T T T))
             (:TUPLE (T NIL T))
             (:TUPLE (NIL T T))
             (:TUPLE (NIL NIL NIL)))
  (:CONDITION T))

 
(DEFCONSTRAINT K3-AND-2
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 RES)
  (:RELATION (:TUPLE (T T T))
             (:TUPLE (T NIL NIL))
             (:TUPLE (NIL T NIL))
             (:TUPLE (NIL NIL NIL)))
  (:CONDITION T))

 
(DEFCONSTRAINT K3-EQUAL-5
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 ARG3 ARG4 ARG5)
  (:RELATION (:PATTERN (ARG1 ARG1 ARG1 ARG1 ARG1) :IF (CONSTRAINED-P ARG1))
             (:PATTERN (ARG2 ARG2 ARG2 ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN (ARG3 ARG3 ARG3 ARG3 ARG3) :IF (CONSTRAINED-P ARG3))
             (:PATTERN (ARG4 ARG4 ARG4 ARG4 ARG4) :IF (CONSTRAINED-P ARG4))
             (:PATTERN (ARG5 ARG5 ARG5 ARG5 ARG5) :IF (CONSTRAINED-P ARG5)))
  (:CONDITION (OR (CONSTRAINED-P ARG1)
                  (CONSTRAINED-P ARG2)
                  (CONSTRAINED-P ARG3)
                  (CONSTRAINED-P ARG4)
                  (CONSTRAINED-P ARG5))))

 
(DEFCONSTRAINT K3-EQUAL-4
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 ARG3 ARG4)
  (:RELATION (:PATTERN (ARG1 ARG1 ARG1 ARG1) :IF (CONSTRAINED-P ARG1))
             (:PATTERN (ARG2 ARG2 ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN (ARG3 ARG3 ARG3 ARG3) :IF (CONSTRAINED-P ARG3))
             (:PATTERN (ARG4 ARG4 ARG4 ARG4) :IF (CONSTRAINED-P ARG4)))
  (:CONDITION (OR (CONSTRAINED-P ARG1)
                  (CONSTRAINED-P ARG2)
                  (CONSTRAINED-P ARG3)
                  (CONSTRAINED-P ARG4))))

 
(DEFCONSTRAINT K3-EQUAL-3
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2 ARG3)
  (:RELATION (:PATTERN (ARG1 ARG1 ARG1) :IF (CONSTRAINED-P ARG1))
             (:PATTERN (ARG2 ARG2 ARG2) :IF (CONSTRAINED-P ARG2))
             (:PATTERN (ARG3 ARG3 ARG3) :IF (CONSTRAINED-P ARG3)))
  (:CONDITION (OR (CONSTRAINED-P ARG1)
                  (CONSTRAINED-P ARG2)
                  (CONSTRAINED-P ARG3))))


(DEFCONSTRAINT K3-EQUAL-2
  (:TYPE PRIMITIVE)
  (:INTERFACE ARG1 ARG2)
  (:RELATION (:PATTERN (ARG1 ARG1) :IF (CONSTRAINED-P ARG1))
             (:PATTERN (ARG2 ARG2) :IF (CONSTRAINED-P ARG2)))
  (:CONDITION (OR (CONSTRAINED-P ARG1)
                  (CONSTRAINED-P ARG2))))



Compound Constraints of #<MINI-CFRLX-CONSTRAINT 266527524>:


(DEFCONSTRAINT K3-K1-TYPE-WR2-1
  (:TYPE COMPOUND)
  (:INTERFACE WR2-1 K3C-LESS-OR-EQUAL-0-ARG1-0 K3C-LESS-OR-EQUAL-0-ARG1-1)
  (:CONSTRAINT-EXPRESSIONS (K3C-LESS-OR-EQUAL-0 K3C-LESS-OR-EQUAL-0-ARG1-1 WW-1)
                           (K3-OR-2 WW-0 WW-1 WR2-1)
                           (K3C-LESS-OR-EQUAL-0 K3C-LESS-OR-EQUAL-0-ARG1-0 WW-0)))

 
(DEFCONSTRAINT K3-K1-TYPE-WR1-1
  (:TYPE COMPOUND)
  (:INTERFACE WR1-1
              K3C-GREATER-0-ARG1-0
              K3C-GREATER-0-ARG1-1
              K3C-GREATER-0-ARG1-2)
  (:CONSTRAINT-EXPRESSIONS (K3C-GREATER-0 K3C-GREATER-0-ARG1-2 WW-2)
                           (K3-AND-3 WW-0 WW-1 WW-2 WR1-1)
                           (K3C-GREATER-0 K3C-GREATER-0-ARG1-1 WW-1)
                           (K3C-GREATER-0 K3C-GREATER-0-ARG1-0 WW-0)))

 
(DEFCONSTRAINT K3-K2-TYPE-A2-2
  (:TYPE COMPOUND)
  (:INTERFACE A2-2 K3C-LESS-OR-EQUAL-0-ARG1-0 K3C-LESS-OR-EQUAL-0-ARG1-1)
  (:CONSTRAINT-EXPRESSIONS (K3C-LESS-OR-EQUAL-0 K3C-LESS-OR-EQUAL-0-ARG1-1 WW-1)
                           (K3-OR-2 WW-0 WW-1 A2-2)
                           (K3C-LESS-OR-EQUAL-0 K3C-LESS-OR-EQUAL-0-ARG1-0 WW-0)))

 
(DEFCONSTRAINT K3-K2-TYPE-A1-2
  (:TYPE COMPOUND)
  (:INTERFACE A1-2 K3C-GREATER-0-ARG1-0 K3C-GREATER-0-ARG1-1)
  (:CONSTRAINT-EXPRESSIONS (K3C-GREATER-0 K3C-GREATER-0-ARG1-1 WW-1)
                           (K3-AND-2 WW-0 WW-1 A1-2)
                           (K3C-GREATER-0 K3C-GREATER-0-ARG1-0 WW-0)))

 
(DEFCONSTRAINT K3-AND-3
  (:TYPE COMPOUND)
  (:INTERFACE WW-0 WW-1 WW-2 R)
  (:CONSTRAINT-EXPRESSIONS (K3-AND-2 WW-0 WW-1 INTERN-R1)
                           (K3-AND-2 INTERN-R1 WW-2 R)))
 

;********************************************************************************
;********************************************************************************

You can use the function print-net if you want to know the values of all slots
associated with a constraint net. The first parameter of print-net specifies 
the name of the net you want to examine. The current K3-code relies on the
global variable *tex-i-net-name* to contain the name of the restriction
net under construction. *tex-i-net-name* has the name "cnet-for-k3-root"
as default. In a later version, however, the name of the root node of the
component hierarchy will be automatically used as an integral part of the name.
A second optional parameter of print-net is used as the destination stream/file
of print-net's output. NIL or no parameter means *default-dialog-stream*, otherwise
the parameter is interpreted as a file name.

Before assigning any values, (print-net *tex-i-net-name* "net-out" results in

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 320064405>:

Slot  V2-2  of instance  K2-INST  has value  -
Slot  V2  of instance  K0-INST  has value  -
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  -
Slot  V1  of instance  K0-INST  has value  -
Slot  WR1-1  of instance  K1-INST  has value  -
Slot  V3  of instance  K1-INST  has value  -
Slot  V1-1  of instance  K1-INST  has value  -
Slot  V2-1  of instance  K1-INST  has value  -
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  -
Slot  V1-2  of instance  K2-INST  has value  -

;********************************************************************************

All values of slots associated with the constraint net are undetermined (-).
We now assign a value to slot v3 of k1-inst under control of CONSAT.
For this purpose,
we use the new method ":put-if-satisfied" instead of the usual "put"-method.
:put-if-satisfied differs from :put in two important ways:

    1. It propagates the newly assigned value in the constraint net such that
       all other values depending on it will also receive new values.

    2. If during this propagation an inconsistency occurs no value is assigned
       to the slot. Hence, :put-if-satisfied has no effect if assigning a new 
       value would result in an inconsistent state.

:put-if-satisfied returns "T" iff the assignment was successfully executed
(the constraint net being consistent afterwards). A value of "nil" indicates
that the assignment has been refused.

;******************************************************************************** 

(<- k1-inst :put-if-satisfied 'v3 4.0)

;;******************************************************************************** 

;which yields the new assignments 

;******************************************************************************** 

Printing the values of all slots of restriction net #<RESTRICTION-NET 320064405>:

Slot  V2-2  of instance  K2-INST  has value  -
Slot  V2  of instance  K0-INST  has value  -
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  -
Slot  V1  of instance  K0-INST  has value  -
Slot  WR1-1  of instance  K1-INST  has value  -
Slot  V3  of instance  K1-INST  has value  4.0
Slot  V1-1  of instance  K1-INST  has value  -
Slot  V2-1  of instance  K1-INST  has value  -
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  4.0
Slot  V1-2  of instance  K2-INST  has value  -

;******************************************************************************** 

K2-INST.V3 now has the same value as K1-INST.V3. All
regions (K1-INST.WR1-1, K1-INST.WR2-1, K2-INST.A1-2, and K2-INST.A2-2)
are still unknown, which means that they have neither value "T" nor "NIL".
We now set the value of K0-INST.V1 and hope that it propagates to
K1-INST.V1-1 and so on.

;******************************************************************************** 

(<- k0-inst :put-if-satisfied 'v1 8.0)
T

;********************************************************************************

The new slot values are:

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 320064405>:

Slot  V2-2  of instance  K2-INST  has value  -
Slot  V2  of instance  K0-INST  has value  -
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  NIL
Slot  V1  of instance  K0-INST  has value  8.0
Slot  WR1-1  of instance  K1-INST  has value  T
Slot  V3  of instance  K1-INST  has value  4.0
Slot  V1-1  of instance  K1-INST  has value  4.0
Slot  V2-1  of instance  K1-INST  has value  4.0
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  4.0
Slot  V1-2  of instance  K2-INST  has value  -

;********************************************************************************

Notice that every other value of K0-INST.V1 than 8.0 should result in an inconsistency.
Regions K1-INST.WR2-1 and K1-INST.WR1-1 have now become known as NIL and T, 
respectively. As a consequence, constraint local-1-1 and the (redundant) equal-
constraint in the :INCLUDES clause of region WR-1-1 become applicable. The value 4.0
is then propagated to K1-INST.V1-1 and K1-INST.V2-1. Now two of three variables
of the HALF-AND-HALF constraint are known, which lets K0-INST.V1 assign the value 8.0.

Now let us test the correctness of the consistency mechanism by trying to 
produce an inconsistency. For example, assigning 5.0 to K1-INST.V3 

;********************************************************************************

(<- k1-inst :put-if-satisfied 'v3 5.0)
NIL

;********************************************************************************

is refused, as indicated by the result NIL. All slot values remain unchanged as you
will probably convince yourself.

There are still some undetermined values. So, what happens, for example, if we
try

;********************************************************************************

(<- k2-inst :put-if-satisfied 'v1-2 5.0)
T

;********************************************************************************

The result "T" means that we are still in a consistent state that shows to be:

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 320064405>:

Slot  V2-2  of instance  K2-INST  has value  5.0
Slot  V2  of instance  K0-INST  has value  5.0
Slot  A1-2  of instance  K2-INST  has value  T
Slot  WR2-1  of instance  K1-INST  has value  NIL
Slot  V1  of instance  K0-INST  has value  8.0
Slot  WR1-1  of instance  K1-INST  has value  T
Slot  V3  of instance  K1-INST  has value  4.0
Slot  V1-1  of instance  K1-INST  has value  4.0
Slot  V2-1  of instance  K1-INST  has value  4.0
Slot  A2-2  of instance  K2-INST  has value  NIL
Slot  V3  of instance  K2-INST  has value  4.0
Slot  V1-2  of instance  K2-INST  has value  5.0

;********************************************************************************

Let's try another example. For this purpose, you should first reset all constraint
variables to unconstrained and all associated slot values to undetermined. The
function "restart-net" net serves both purposes.

;********************************************************************************

(restart-net *tex-i-net-name*)
((CNET-FOR-K3-ROOT . #<RESTRICTION-NET 266661002>))

;********************************************************************************

All values should now be unknown:

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 266661002>:

Slot  V2-2  of instance  K2-INST  has value  -
Slot  V2  of instance  K0-INST  has value  -
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  -
Slot  V1  of instance  K0-INST  has value  -
Slot  WR1-1  of instance  K1-INST  has value  -
Slot  V3  of instance  K1-INST  has value  -
Slot  V1-1  of instance  K1-INST  has value  -
Slot  V2-1  of instance  K1-INST  has value  -
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  -
Slot  V1-2  of instance  K2-INST  has value  -

;********************************************************************************

Good gracious, it works! Now we start with

;********************************************************************************


(<- k2-inst :put 'v2-2 4.0)
T

;********************************************************************************

According to the description from above, the :put method should not cause an
activation of constraint propagation. Hence, no other slots should be effected.
Indeed,

;;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 266661002>:

Slot  V2-2  of instance  K2-INST  has value  4.0
Slot  V2  of instance  K0-INST  has value  -
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  -
Slot  V1  of instance  K0-INST  has value  -
Slot  WR1-1  of instance  K1-INST  has value  -
Slot  V3  of instance  K1-INST  has value  -
Slot  V1-1  of instance  K1-INST  has value  -
Slot  V2-1  of instance  K1-INST  has value  -
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  -
Slot  V1-2  of instance  K2-INST  has value  -

;********************************************************************************

the result of (print-net *tex-i-net-name* "net-out") is promising.
It shows that the still existing :put facility allows you to 
let some slots receive new  values without intermediate propagation.

If you now set another value, say K2-INST.V1-2, with :put-if-satisfied,
you would probably guess that the value of V2-2 will also be considered
during propagation. You can make a test with

;********************************************************************************

(<- k2-inst :put-if-satisfied 'v1-2 5.0)
NIL

;********************************************************************************

which demonstrates that propagation has been activated with values 4.0
and 5.0 for K1-INST.V2-2 and K1-INST.V1-2, respectively. Due to the equal
constraint defined in K0 in the VERTICAL relationship, assignment was denied.

By the way, you should notice another feature that is illustrated by the
following assignment:

;********************************************************************************

(<- k0-inst :put-if-satisfied 'v1 5.0)
NIL

;********************************************************************************

After all, this refusal of value 5.0 for K0-INST.V1 might be a surprise to you.
The explanation is that K1-INST.V2-1 has a nonempty possible value definition,
and that these values are also considered as the only possible values of the
corresponding constraint net variables. As CONSAT is able to cope with
those sets of values, it has correctly computed that K0-INST.V1 can only 
be assigned values in the list (-16.0 0.0 -8.0 4.0 -4.0 8.0 16.0). This set
was created from the set (-8.0 0.0 -4.0 2.0 -2.0 4.0 8.0) for K1-INST.V2-1
by multiplying each element by 2.

You can inspect the "real" net values with  function "print-rnet".
print-rnet is defined like "print-net", and takes the same arguments.
In our case, (print-rnet *tex-i-net-name* "net-out") results in 

;********************************************************************************


Printing the values of all variables of restriction net #<RESTRICTION-NET 266661002>:


Slot  V2-2  of instance  K2-INST  has value  (4.0)
Slot  V2  of instance  K0-INST  has value  (4.0)
Slot  A1-2  of instance  K2-INST  has value  (T NIL)
Slot  WR2-1  of instance  K1-INST  has value  (T NIL)
Slot  V1  of instance  K0-INST  has value  (-16.0 0.0 -8.0 4.0 -4.0 8.0 16.0)
Slot  WR1-1  of instance  K1-INST  has value  (T NIL)
Slot  V3  of instance  K1-INST  has value  UNCONSTRAINED
Slot  V1-1  of instance  K1-INST  has value  (-8.0 0.0 -4.0 2.0 -2.0 4.0 8.0)
Slot  V2-1  of instance  K1-INST  has value  (-8.0 0.0 -4.0 2.0 -2.0 4.0 8.0)
Slot  A2-2  of instance  K2-INST  has value  (T NIL)
Slot  V3  of instance  K2-INST  has value  UNCONSTRAINED
Slot  V1-2  of instance  K2-INST  has value  (4.0)

;********************************************************************************

As you can also observe, all variables associated with region slots have
T and NIL as possible values. CONSAT computes these values from the definition
of the constraints attached to the region slots. 
Notice that all variables with more than
one value in the net have "-" as value in the corresponding slot. From the
point of view of the constraint net, only K1-INST.V3 and K2-INST.K3 are
UNCONSTRAINED. When debugging your programs this distinction between "undetermined (-)"
and UNCONSTRAINED might become important. Therefore, we shall also
present the result of "print-net" of the same constraint net as above:

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 266661002>:

Slot  V2-2  of instance  K2-INST  has value  4.0
Slot  V2  of instance  K0-INST  has value  4.0
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  -
Slot  V1  of instance  K0-INST  has value  -
Slot  WR1-1  of instance  K1-INST  has value  -
Slot  V3  of instance  K1-INST  has value  -
Slot  V1-1  of instance  K1-INST  has value  -
Slot  V2-1  of instance  K1-INST  has value  -
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  -
Slot  V1-2  of instance  K2-INST  has value  4.0

;********************************************************************************

Finishing this short working introduction into K3 facilities, we want 
to complete the current state of the net in two steps. First, we use :put
to assign a value to K0-INST.V1:

;********************************************************************************

(<- K0-INST :put 'V1 8.0)
T

;********************************************************************************

Remember that the net is only updated with respect to K0-INST.V1:

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 266661002>:

Slot  V2-2  of instance  K2-INST  has value  4.0
Slot  V2  of instance  K0-INST  has value  4.0
Slot  A1-2  of instance  K2-INST  has value  -
Slot  WR2-1  of instance  K1-INST  has value  -
Slot  V1  of instance  K0-INST  has value  8.0
Slot  WR1-1  of instance  K1-INST  has value  -
Slot  V3  of instance  K1-INST  has value  -
Slot  V1-1  of instance  K1-INST  has value  -
Slot  V2-1  of instance  K1-INST  has value  -
Slot  A2-2  of instance  K2-INST  has value  -
Slot  V3  of instance  K2-INST  has value  -
Slot  V1-2  of instance  K2-INST  has value  4.0

;********************************************************************************

We can now come to an end with assigning a value to K1-INST.V3.
As we know the example well, we achieve a consistent final state
with the first try:

;********************************************************************************

(<- K1-INST :put-if-satisfied 'V3 4.0)
T

;********************************************************************************

Printing the values of all slots of restriction net #<RESTRICTION-NET 266661002>:

Slot  V2-2  of instance  K2-INST  has value  4.0
Slot  V2  of instance  K0-INST  has value  4.0
Slot  A1-2  of instance  K2-INST  has value  T
Slot  WR2-1  of instance  K1-INST  has value  NIL
Slot  V1  of instance  K0-INST  has value  8.0
Slot  WR1-1  of instance  K1-INST  has value  T
Slot  V3  of instance  K1-INST  has value  4.0
Slot  V1-1  of instance  K1-INST  has value  4.0
Slot  V2-1  of instance  K1-INST  has value  4.0
Slot  A2-2  of instance  K2-INST  has value  NIL
Slot  V3  of instance  K2-INST  has value  4.0
Slot  V1-2  of instance  K2-INST  has value  4.0

;********************************************************************************

|#

;;  Instructions for normal-Babylon :
;;  you can start it interactiv
;;  if you eval this region
			  
(instructions
  (<- k0-inst :install-constraints)
  (<- k1-inst :put-if-satisfied 'v3 4.0)
  (<- k0-inst :put-if-satisfied 'v1 8.0)
  (<- k1-inst :put-if-satisfied 'v3 5.0)
  (<- k2-inst :put-if-satisfied 'v1-2 5.0)
  (print-net *tex-i-net-name*))


;;;  Task and instruction part for tex-i-bylon :
;;;  you can start this task after PRESET with
;;;
;;;      ($send (get-task 'my-k3-1) :start)


;(CREATE-TASK my-k3-1 normal-fcrplx-task k3-kb 
;	     ((<- k0-inst :install-constraints)
;	      (<- k1-inst :put-if-satisfied 'v3 4.0)
;	      (<- k0-inst :put-if-satisfied 'v1 8.0)
;	      (<- k1-inst :put-if-satisfied 'v3 5.0)
;	      (<- k2-inst :put-if-satisfied 'v1-2 5.0)
;	      (print-net *tex-i-net-name*)))
;
;(PRESET-INFERENCE-TASK (get-task 'my-k3-1))

;;; eof

