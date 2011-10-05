(defclass scm-object () ())

(defstruct (environment (:constructor make-env)
                        (:conc-name env-)
                        (:copier copy-env))
  (env nil :type list))                                 ; alist

(defclass scm-exp (scm-object) ())




(defclass self-evaluating (scm-exp)
  ((val :accessor val :initarg :val)))

(defclass scm-nil       (self-evaluating) ())           ; val = nil
(defclass scm-boolean   (self-evaluating) ())           ; val = t | nil
(defclass scm-number    (self-evaluating) ())           ; val = <number>
(defclass scm-string    (self-evaluating) ())           ; val = string
(defclass scm-character (self-evaluating) ())           ; val = character



(defclass scm-symbol (scm-exp)
  ((name :accessor name :initarg :name)))               ; string



(defclass cons (scm-exp)
  ((val-car :accessor val-car :initarg :val-car)        ; scm-exp
   (val-cdr :accessor val-cdr :initarg :val-cdr)))      ; scm-exp



(defclass definition (scm-exp)
  ((var :accessor var :initarg :var)))                  ; scm-symbol

(defclass variable-definition (definition)
  ((val :accessor val :initarg :val)))                  ; scm-exp

(defclass function-definition (definition)
  ((parms :accessor parms :initarg :parms)              ; list of scm-symbol
   (body :accessor body :initarg :body)))               ; list of scm-exp

;(defclass syntax-definition (definition)
;  ())
;(defclass record-type-definition (definition)
;  ())


(defclass assignment (scm-exp)
  ((var :accessor var :initarg :var)
   (val :accessor val :initarg :val)))


(defclass quotation (scm-exp)
  ((qexp :accessor qexp :initarg :qexp)))


(defclass begin (scm-exp)
  ((exps :accessor exps :initarg :exps)))


(defclass if-exp (scm-exp)
  ((pred :accessor pred :initarg :pred)
   (then :accessor then :initarg :then)
   (else :accessor else :initarg :else)))


(defclass cond-exp (scm-exp)
  ((clauses :accessor clauses :initarg :clauses)))


(defclass scm-lambda (scm-exp)
  ((parms :accessor parms :initarg :parms)              ; list of scm-symbol
   (body :accessor body :initarg :body)                 ; list of scm-exp
   (env :accessor env :initarg :env)))


(defclass and-exp (scm-exp)
  ((exps :accessor exps :initarg :exps)))

(defclass or-exp (scm-exp)
  ((exps :accessor exps :initarg :exps)))

(defclass let-exp (scm-exp)
  ((var :accessor var :initarg var)
   (binds :accessor binds :initarg binds)
   (body :accessor body :initarg body)))

;(defclass let*-exp (scm-exp)
;  (()))
;(defclass letrec-exp (scm-exp)
;  (()))


(defclass application (scm-exp)
  (()))