(in-package :yscheme)

;;; 6.1. Equivlence predicates

(in-package :yscheme)


(defgeneric scm-eqv? (obj1 obj2)
  (:documentation ""))

(defmethod scm-eqv? (obj1 obj2)
  (if (eql obj1 obj2) +true+ +false+))

(defmethod scm-eqv? ((obj1 scm-boolean) (obj2 scm-boolean))
  (if (eql (val obj1) (val obj2)) +true+ +false+))

(defmethod scm-eqv? ((obj1 scm-symbol) (obj2 scm-symbol))
  (if (string= (name obj1) (name obj2)) +true+ +false+))

; number
;(defmethod scm-eqv? ((obj1 scm-) (obj2 scm-))
;  ())

(defmethod scm-eqv? ((obj1 scm-character) (obj2 scm-character))
  (if (char= (val obj1) (val obj2)) +true+ +false+))

(defmethod scm-eqv? ((obj1 scm-nil) (obj2 scm-nil)) +true+)

(defmethod scm-eqv? ((obj1 scm-pair) (obj2 scm-pair))
  (if (eql obj1 obj2) +true+ +false+))

(defmethod scm-eqv? ((obj1 scm-vector) (obj2 scm-vector))
  (if (eql obj1 obj2) +true+ +false+))

(defmethod scm-eqv? ((obj1 scm-string) (obj2 scm-string))
  (if (eql obj1 obj2) +true+ +false+))

(defmethod scm-eqv? ((obj1 procedure) (obj2 procedure))
  (if (eql obj1 obj2) +true+ +false+))



(defgeneric scm-eq? (obj1 obj2)
  (:documentation ""))

(defmethod scm-eq? (obj1 obj2) (scm-eqv? obj1 obj2))



(defgeneric scm-equal? (obj1 obj2)
  (:documentation ""))

(defmethod scm-equal? (obj1 obj2) (eqv? obj1 obj2))

;(defmethod scm-equal? ((obj1 scm-pair) (obj2 scm-pair))
;  )

;(defmethod scm-equal? ((obj1 scm-vector) (obj2 scm-vector))
;  )
