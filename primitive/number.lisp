;;; 6.2. Numbers


(defclass scm-number (self-evaluating)
  ((ex :accessor ex :initarg :ex :documentation "exactness")))

;   (re :accessor re :initarg :re :documentation "real part")
;   (im :accessor im :initarg :im :documentation "imaginary part")
;   (nu :accessor nu :initarg :nu :documentation "numerator")
;   (de :accessor de :initarg :de :documentation "denominator")

(defclass scm-complex  (scm-number)   ())
(defclass scm-real     (scm-complex)  ())

(defclass scm-finite   (scm-real)
(defclass scm-rational (scm-finite)   ())
(defclass scm-integer  (scm-rational) ())

(defclass scm-infinity (scm-real) ())
(defclass scm-positive-infinity  (scm-infinity) ())
(defclass scm-negative-infinity  (scm-infinity) ())
(defclass scm-nan      (scm-real) ())



;;
(defun radix-n-to-number (n num)
  (read-from-string (format nil "#~Ar~A" n num)))


(define-predicate number? ((obj scm-number)) +true+)
(define-predicate complex? ((obj scm-complex)) +true+)
(define-predicate real? ((obj scm-real)) +true+)
(define-predicate rational? ((obj scm-rational)) +true+)
(define-predicate integer? ((obj scm-integer)) +true+)

(define-predicate exact? ((obj scm-number))
  (if (scm-truep (exactness obj)) +true+ +false+))

(define-predicate inexact? ((obj scm-number))
  (if (scm-not (exactness obj)) +true+ +false+))

(define-predicate exact-integer? ((obj scm-integer))
  (if (scm-not (exactness obj)) +true+ +false+))

(define-predicate infinite? ((obj scm-finite)) +true+)
(define-predicate infinite? ((obj scm-infinity)) +true+)
(define-predicate nan? ((obj scm-nan)) +true+)


(define-


(define-predicate zero? ((obj scm-integer))
  (if (= 0 (val obj)) +true+ +false+))

(define-predicate positive? ((obj scm-number))
  (if (plusp (val obj)) +true+ +false+))

(define-predicate negative? ((obj scm-number))
  (if (minusp (val obj)) +true+ +false+))

(define-predicate odd? ((obj scm-number))
  (if (oddp (val obj)) +true+ +false+))

(define-predicate even? ((obj scm-number))
  (if (evenp (val obj)) +true+ +false+))

