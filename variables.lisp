(in-package :yscheme)


(defclass scm-object () ())
(defclass scm-form (scm-object) ())
(defclass scm-undefined (scm-object) ())
(defvar +undefined+ (make-instance 'scm-undefined))


(defclass self-evaluating (scm-form)
  ((val :accessor val :initarg :val :initform (error "value required"))))
;; self-evaluating の val
;; scm-number    : initialize-instance の際に val を string-to-number する
;; scm-boolean   : 新たにインスタンスが生成されることはない(全て同一インスタンス)
;; scm-symbol    : name は string
;; scm-character : character (todo: #\alarm)
;; scm-string    : string
;; scm-vector    : vector

(defclass scm-number (self-evaluating)
  ((ex :accessor ex :initarg :ex :initform nil)))

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



;; 5.2 Definitions

(defclass definition (scm-form)
  ((sym :accessor sym :initarg :sym)))                  ; scm-symbol

(defclass variable-definition (definition)
  ((val :accessor val :initarg :val)))                  ; scm-form

(defclass function-definition (definition)
  ((parms :accessor parms :initarg :parms)              ; list of scm-symbol
   (body :accessor body :initarg :body)))               ; list of scm-form


;; 5.3 Syntax definitions

;(defclass syntax-definition (definition)
;  ())


;; 5.4 Record-type definitions

(defclass record-type (scm-object)
  ((rec-type :accessor rec-type :initarg :rec-type)     ; scm-symbol
   (val :accessor val :initarg :val)))                  ; alist

(defclass record-type-const (scm-form)
  ((sym :accessor sym :initarg :sym)                    ; scm-symbol
   (parms :accessor parms :initarg :parms)))            ; list of scm-symbol

(defclass record-type-field (scm-form)
  ((sym :accessor sym :initarg :sym)                    ; scm-symbol
   (access :accessor access :initarg :access)           ; scm-symbol
   (Modify :accessor modify :initarg :modify)))         ; scm-symbol

(defclass record-type-definition (definition)
  ((rec-type :accessor rec-type :initarg :rec-type)     ; scm-symbol
   (const :accessor const :initarg :const)              ; record-type-const
   (pred :accessor pred :initarg :pred)                 ; scm-symbol
   (fields :accessor fields :initarg :fields)))         ; record-type-field


;; 5.5 Libraries

(defclass rename-pair (scm-form)
  ((from :accessor from :initarg :from)                 ; scm-symbol
   (to :accessor to :initarg :to)))                     ; scm-symbol

(defclass scm-import-set (scm-form) ())

(defclass import-library (scm-import-set)
  ((syms :accessor syms :initarg :syms)))

(defclass import-only (scm-import-set)
  ((im-set :accessor im-set :initarg :im-set)
   (syms :accessor syms :initarg :syms)))

(defclass import-except (scm-import-set)
  ((im-set :accessor im-set :initarg :im-set)
   (syms :accessor syms :initarg :syms)))

(defclass import-prefix (scm-import-set)
  ((im-set :accessor im-set :initarg :im-set)
   (sym :accessor sym :initarg :sym)))

(defclass import-rename (scm-import-set rename-pair)
  ((im-set :accessor im-set :initarg :im-set)))


(defclass cond-expand-clause (clause)
  ((req :accessor req :initarg :req)))

(defclass cond-expand-else-clause (clause) ())

(defclass feature-requirement (scm-form) ())

(defclass required-identifier (feature-requirement)
  ((feature :accessor feature :initarg :feature)))      ; scm-symbol

(defclass required-library (feature-requirement)
  ((syms :accessor syms :initarg :syms)))               ; list of scm-symbol


(defclass library-export (scm-form)
  ((syms :accessor syms :initarg :syms)                 ; list of scm-symbol
   (renames :accessor renames :initarg :renames)))      ; list of rename-pair

(defclass library-import (scm-form)
  ((exps :accessor exps :initarg :exps)))

(defclass library-include (scm-form)
  ((files :accessor files :initarg files)))

(defclass library-include-ci (scm-form)
  ((files :accessor files :initarg files)))

(defclass library-cond-expand (scm-form)
  ((clauses :accessor clauses :initarg :clauses)))


(defclass Library-definition (scm-form)
  ((syms :accessor syms :initarg :syms)
   (exps :accessor exps :initarg :exps)
   (mod-ex :accessor mod-ex :initarg :mod-ex)))

;   (mod-im :accessor mod-im :initarg :mod-im)
;   (mod-ex :accessor mod-ex :initarg :mod-ex)
;   (mod-inc :accessor mod-inc :initarg :mod-inc)
;   (mod-inc-ci :accessor mod-inc-ci :initarg :mod-inc-ci)
;   (mod-cond :accessor mod-cond :initarg :mod-cond)
;   (mod-begin :accessor mod-begin :initarg :mod-begin)))

(defclass library (scm-object)
  ((syms :accessor syms :initarg :syms)
   (env :accessor env :initarg :env)))

(defvar *scm-libraries* nil)
(defvar *scm-features* nil)


;;; 6.7.1. Ports

(defclass scm-port (scm-object)
  ((file :accessor file :initarg :file)
   (direction :accessor direction :initarg :direction)
   (strm :accessor strm :initarg :strm)))

(defclass scm-character-port (scm-port) ())
(defclass scm-binary-port (scm-port) ())

(defclass scm-stdin (scm-object) ())
(defclass scm-stdout (scm-object) ())
(defclass scm-stderr (scm-object) ())

;(defvar +scm-stdin+ (new 'scm-stdin))
;(defvar +scm-stdout+ (new 'scm-stdout))
;(defvar +scm-stderr+ (new 'scm-stderr))


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


;;; 4.1.6. Assignments (set!)

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

(defclass when-exp (scm-form)
  ((test :accessor test :initarg :test)
   (exps :accessor exps :initarg :exps)))

(defclass unless-exp (scm-form)
  ((test :accessor test :initarg :test)
   (exps :accessor exps :initarg :exps)))


;;; 4.2.2. Binding constructs

(defclass binding (scm-form)
  ((sym :accessor sym :initarg :sym)
   (init :accessor init :initarg :init)))

(defclass mv-binding (scm-form)
  ((syms :accessor syms :initarg :syms)
   (init :accessor init :initarg :init)))

(defclass let-exp (scm-form)
  ((binds :accessor binds :initarg :binds)              ; list of binding or mv-binding
   (body :accessor body :initarg :body)))

(defclass let*-exp (let-exp) ())

(defclass letrec-exp (let-exp) ())

(defclass letrec*-exp (let-exp) ())

(defclass let-values-exp (let-exp) ())

(defclass let*-values-exp (let-exp) ())


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


;;; 4.2.5. Delayed evaluation

(defclass scm-promise (scm-form)
  ((done :accessor done :initarg :done)
   (proc :accessor proc :initarg :proc)))

(defclass scm-delay (scm-form)
  ((expr :accessor expr :initarg :exp)))

(defclass scm-lazy (scm-form)
  ((expr :accessor expr :initarg :exp)))

(defclass scm-force (scm-form)
  ((promise :accessor promise :initarg :promise)))


;;; 4.2.6. Dynamic Bindings

(defclass parameterize (scm-form)
  ((parms :accessor parms :initarg :parms)
   (body :accessor body :initarg :body)))


;;; 4.2.7. Exception Handling

;(defclass guard (scm-form)
;  ((
