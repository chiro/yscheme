(defclass scm-object () ())



(defclass self-evaluating (scm-object)
  ((val :accessor val :initarg :val)))

;(defclass scm-boolean   (self-evaluating) ())
;(defclass scm-number    (self-evaluating) ())
;(defclass scm-string    (self-evaluating) ())
;(defclass scm-character (scm-self-evaluating) ())



(defclass scm-symbol (scm-object)
  ((name :accessor name :initarg :name)))



(defclass scm-cons (scm-object)
  ((car :accessor car :initarg :car)
   (cdr :accessor cdr :initarg :cdr)))

(defclass definition (scm-cons)
  ())
(defclass syntax-definition (scm-cons)
  ())
(defclass record-type-definition (scm-cons)
  ())

