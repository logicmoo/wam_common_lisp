;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

;;; *** arrangement as generic relation ***

(DEF-GENERIC-RELATION arrangement
  (SLOTS (domains   (room employee))
         (assumable false)))


;;; *** Instances OF arrangement ***


(DEF-RELATION initial-arrangement OF arrangement           ; computable (arrangement)
  WITH domains    = ((room employee))
       assumable  = false)

(DEF-CLAUSE-SET initial-arrangement
  ((initial-arrangement hall werner-karbach))
  ((initial-arrangement hall alexander-horz))
  ((initial-arrangement hall juergen-walther))
  ((initial-arrangement hall michael-schmitz))
  ((initial-arrangement hall angi-voss))
  ((initial-arrangement hall hans-voss))
  ((initial-arrangement hall monika-wendel))
  ((initial-arrangement hall thomas-christaller)))


;;; eof

