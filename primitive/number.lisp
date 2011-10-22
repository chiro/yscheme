(in-package :yscheme)

;;; 6.2. Numbers


(defclass scm-number (self-evaluating)
  ((ex :accessor ex :initarg :ex)))

;   (re :accessor re :initarg :re :documentation "real part")
;   (im :accessor im :initarg :im :documentation "imaginary part")
;   (nu :accessor nu :initarg :nu :documentation "numerator")
;   (de :accessor de :initarg :de :documentation "denominator")

(defclass scm-complex  (scm-number)   ())
(defclass scm-real     (scm-complex)  ())

(defclass scm-finite   (scm-real)     ())
(defclass scm-rational (scm-finite)   ())
(defclass scm-integer  (scm-rational) ())

(defclass scm-infinity (scm-real) ())
(defclass scm-positive-infinity  (scm-infinity) ())
(defclass scm-negative-infinity  (scm-infinity) ())
(defclass scm-nan      (scm-real) ())



;;
(defun radix-n-to-number (n num)
  (read-from-string (format nil "#~Ar~A" n num)))

;; 処理系依存の予感
(defun number-class-of (val)
  (cond ((integerp val)      'scm-integer)
        ((zerop (mod val 1)) 'scm-integer)
        ((rationalp val)     'scm-rational)
        ((floatp val)        'scm-rational)
        ((realp val)         'scm-real)
        ((complexp val)      'scm-complex)))

(defmacro define-numerical-operation (name &key op exactness)
  (with-gensyms (parm v1 v2)
    `(progn
       (defgeneric ,name (,parm))
       (defmethod ,name (,parm)
         (multiple-value-bind (,v1 ,v2)
             (apply ,op (mapcar (lambda (num) (val num)) ,parm))
           (if ,v2
               (values (new 'scm-number :val ,v1 :ex t)
                       (new 'scm-number :val ,v2 :ex t))
               (new 'scm-number
                    :val ,v1
                    :ex (aif ,exactness
                             it
                             (reduce (lambda (f s) (and f s))
                                     (mapcar (lambda (num) (ex num)) ,parm))))))))))

(defmethod initialize-instance :after ((num scm-number) &key)
  (let ((val (val num)))
    (change-class num (number-class-of val))))



(define-predicate number? ((obj scm-number)) +false+ +true+)
(define-predicate complex? ((obj scm-complex)) +false+ +true+)
(define-predicate real? ((obj scm-real)) +false+ +true+)
(define-predicate rational? ((obj scm-rational)) +false+ +true+)
(define-predicate integer? ((obj scm-integer)) +false+ +true+)

(define-predicate exact? ((obj scm-number)) +false+
  (if (scm-truep (ex obj)) +true+ +false+))

(define-predicate inexact? ((obj scm-number)) +false+
  (if (not (scm-truep (ex obj))) +true+ +false+))

(define-predicate exact-integer? ((obj scm-integer)) +false+
  (if (scm-truep (ex obj)) +true+ +false+))

(define-predicate infinite? ((obj scm-finite)) +false+ +true+)
(define-predicate infinite? ((obj scm-infinity)) +false+ +true+)
(define-predicate nan? ((obj scm-nan)) +true+)

(define-numerical-operation scm-= :op #'=)
(define-numerical-operation scm-< :op #'<)
(define-numerical-operation scm-> :op #'>)
(define-numerical-operation scm-<= :op #'<=)
(define-numerical-operation scm->= :op #'>=)

(define-predicate zero? ((obj scm-integer)) +false+
  (if (= 0 (val obj)) +true+ +false+))

(define-predicate positive? ((obj scm-number)) +false+
  (if (plusp (val obj)) +true+ +false+))

(define-predicate negative? ((obj scm-number)) +false+
  (if (minusp (val obj)) +true+ +false+))

(define-predicate odd? ((obj scm-number)) +false+
  (if (oddp (val obj)) +true+ +false+))

(define-predicate even? ((obj scm-number)) +false+
  (if (evenp (val obj)) +true+ +false+))

(define-numerical-operation scm-max :op #'max)
(define-numerical-operation scm-min :op #'min)

(define-numerical-operation scm-+ :op #'+)
(define-numerical-operation scm-- :op #'-)
(define-numerical-operation scm-* :op #'*)
(define-numerical-operation scm-/ :op #'/)

(define-numerical-operation scm-abs :op #'abs)

(define-numerical-operation scm-floor/ :op #'floor)
(define-numerical-operation scm-floor-quotient :op #'floor)
(define-numerical-operation scm-floor-remainder :op #'floor)

