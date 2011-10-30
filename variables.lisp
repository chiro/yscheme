(in-package :yscheme)


(defclass scm-object () ())
(defclass scm-form (scm-object) ())
(defclass scm-undefined (scm-object) ())
(defvar +undefined+ (make-instance 'scm-undefined))


(defclass self-evaluating (scm-form)
  ((val :accessor val :initarg :val)))
;; self-evaluating の val
;; scm-number    : initialize-instance の際に val を string-to-number する
;; scm-boolean   : 新たにインスタンスが生成されることはない(全て同一インスタンス)
;; scm-symbol    : name は string
;; scm-character : character (todo: #\alarm)
;; scm-string    : string
;; scm-vector    : vector

(defclass scm-number (self-evaluating)
  ((ex :accessor ex :initarg :ex)))

(defclass scm-complex  (scm-number)   ())
(defclass scm-real     (scm-complex)  ())

(defclass scm-finite   (scm-real)     ())
(defclass scm-rational (scm-finite)   ())
(defclass scm-integer  (scm-rational) ())

(defclass scm-infinity (scm-real) ())
(defclass scm-positive-infinity  (scm-infinity) ())
(defclass scm-negative-infinity  (scm-infinity) ())
(defclass scm-nan      (scm-real) ())

(defclass scm-boolean (self-evaluating) ())             ; val = t | nil
(defvar +true+ (make-instance 'scm-boolean :val t))
(defvar +false+ (make-instance 'scm-boolean :val nil))

(defclass scm-list (scm-form) ())
(defclass scm-nil  (scm-list) ())
(defclass scm-pair (scm-list)
  ((val-car :accessor val-car :initarg :val-car)        ; scm-form
   (val-cdr :accessor val-cdr :initarg :val-cdr)))      ; scm-form

(defclass scm-symbol (scm-form)
  ((name :accessor name :initarg :name)))               ; string

(defclass scm-character  (self-evaluating) ())          ; val = character
(defclass scm-string     (self-evaluating) ())          ; val = string
(defclass scm-vector     (self-evaluating) ())          ; val = vector
(defclass scm-bytevector (self-evaluating) ())          ; val = vector


(defclass definition (scm-form)
  ((sym :accessor sym :initarg :sym)))                  ; scm-symbol

(defclass variable-definition (definition)
  ((val :accessor val :initarg :val)))                  ; scm-form

(defclass function-definition (definition)
  ((parms :accessor parms :initarg :parms)              ; list of scm-symbol
   (body :accessor body :initarg :body)))               ; list of scm-form

;(defclass syntax-definition (definition)
;  ())

;(defclass record-type-definition (definition)
;  ())




;;; 4.1.1. Variable references



;;; 4.1.2. Literal expressions

(defclass quotation (scm-form)
  ((qexp :accessor qexp :initarg :qexp)))


;;; 4.1.3. Procedure calls

(defclass application (scm-form)
  ((proc :accessor proc :initarg :proc)
   (args :accessor args :initarg :args)))


;;; 4.1.4. Procedures

(defclass procedure (scm-form) ())

(defclass primitive-procedure (procedure)
  ((func :accessor func :initarg :func)))               ; cl function

(defclass compound-procedure (procedure)
  ((parms :accessor parms :initarg :parms)              ; list of scm-symbol
   (body :accessor body :initarg :body)
   (env :accessor env :initarg :env)))


;;; 4.1.5. Conditionals

(defclass if-exp (scm-form)
  ((pred :accessor pred :initarg :pred)
   (then :accessor then :initarg :then)
   (else :accessor else :initarg :else)))


;;; 4.1.6. Assignments

(defclass assignment (scm-form)
  ((sym :accessor sym :initarg :sym)
   (val :accessor val :initarg :val)))


;;; 4.2.1. Conditionals

(defclass clause (scm-form)
  ((exps :accessor exps :initarg :exps)))

(defclass cond-clause (clause)
  ((test :accessor test :initarg :test)))

(defclass cond-clause-with-proc (cond-clause) ())

(defclass cond-else-clause (clause) ())

(defclass case-clause (clause)
  ((datums :accessor datum :initarg :datum)))

(defclass case-clause-with-proc (case-clause) ())

(defclass case-else-clause (clause) ())

(defclass case-else-clause-with-proc (else-clause) ())


(defclass cond-exp (scm-form)
  ((clauses :accessor clauses :initarg :clauses)))      ; list of clause

(defclass case-exp (scm-form)
  ((key :accessor key :initarg :key)
   (clauses :accessor clauses :initarg :clauses)))      ; list of clause

(defclass and-exp (scm-form)
  ((exps :accessor exps :initarg :exps)))

(defclass or-exp (scm-form)
  ((exps :accessor exps :initarg :exps)))


;;; 4.2.2. Binding constructs

(defclass binding (scm-form)
  ((sym :accessor sym :initarg :sym)
   (init :accessor init :initarg :init)))

(defclass let-exp (scm-form)
  ((binds :accessor binds :initarg :binds)              ; list of binding
   (body :accessor body :initarg :body)))

(defclass let*-exp (let-exp) ())

(defclass letrec-exp (let-exp) ())

(defclass letrec*-exp (let-exp) ())


;;; 4.2.3. Sequencing

(defclass begin (scm-form)
  ((exps :accessor exps :initarg :exps)))


;;; 4.2.4. Iteration

(defclass step-binding (binding)
  ((next :accessor next :initarg :next)))

(defclass do-end-clause (clause)
  ((test :accessor test :initarg :test)))

(defclass do-exp (scm-form)
  ((binds :accessor binds :initarg :binds)             ; list of step-binding
   (end :accessor end :initarg :end)                   ; do-end-clause
   (body :accessor body :initarg :body)))

(defclass named-let-exp (let-exp)
  ((sym :accessor sym :initarg :sym)))
