;;; -*- Mode: Lisp; Syntax: Common-Lisp; Base: 10; Package: BABYLON -*-

(in-package "BABYLON")

; ========================================================
; ============= DEFINITION OF METACLASSES ================
; ========================================================


;;; *** definition of the requirements ***

;;; requirements claimed for components,e.g. head-of-group-in-single-room
;;; or heads-of-projects-near-to-head-of-group

(DEF-METACLASS component-specific-requirements
  WITH value = (nil)
       structure = set
       value-element-format = (component-specific-requirement)
       possible-concepts = ((component-specific-requirement)))

(DEF-METACLASS location-specific-requirements
  WITH value = (nil)
       structure = set
       value-element-format = (location-specific-requirement)
       possible-concepts = ((location-specific-requirement)))


;;; *** metaclasses for employees ****

;;; already placed components

(DEF-METACLASS arranged-components
  WITH value = (nil)
       structure = set
       value-element-format = (employee "...")
       possible-concepts = ((employee)))

;;; component to place in next cycle

(DEF-METACLASS component-to-integrate-next
  WITH value = (nil)
       structure = set
       value-element-format = (employee)
       possible-concepts = ((employee)))

;;; all components you can place

(DEF-METACLASS components-to-arrange
  WITH value = (nil)
       structure = set
       value-element-format = (employee "...")
       possible-concepts = ((employee)))

;;; set of conditions issued by evaluation of concerned requirements
;;; for the component to integrate next
  
(DEF-METACLASS conditions-for-component-to-integrate-next
  WITH value = (nil)
       structure = set
       value-element-format = (condition employee "...")
       possible-concepts = ((condition)))

;;; set of conditions for components already placed

(DEF-METACLASS conditions-between-arranged-components
  WITH value = (nil)
       structure = set
       value-element-format = (condition employee "...")
       possible-concepts =((condition)))

;;; union of the condition sets considered before

(DEF-METACLASS conditions-between-concerned-components
  WITH value = (nil)
       structure = set
       value-element-format = (condition employee "...")
       possible-concepts = ((condition)))

;;; *** metaclasses for occupancies ****

;;; possible occupancies after consideration of conditions between
;;; and legal places for all concerned components 

(DEF-METACLASS possible-arrangements
  WITH value = (nil)
       structure = set
       value-element-format = (room employee "...")
       possible-concepts =((room employee)))

;;; occupancies meeting the location specific requirements
;;; like check for provided resources claimed by employees 

(DEF-METACLASS proposed-valid-arrangements
  WITH value = (nil)
       structure = set
       value-element-format = (room employee "...")
       possible-concepts =((room employee)))

;;; occupancies keeped for trace purposes
 
(DEF-METACLASS valid-arrangements
  WITH value = (nil)
       structure = set
       value-element-format = (room employee "...")
       possible-concepts =((room employee)))

;;; places occuring in the valid arrangements for each employee.
;;; legal places for components can be added by the user before integrating
;;; the next component or removed to restrict the proposed arrangements.
  

(DEF-METACLASS legal-places-for-components
  WITH value = (nil)
       structure = set
       value-element-format = (employee room "...")
       possible-concepts =((employee room)))

;;; flag to indicate the failure of arrangement

(DEF-METACLASS components-not-possible-to-integrate
  WITH value = (nil)
       structure = set
       value-element-format = (employee)
       possible-concepts = ((employee)))


;;; eof

