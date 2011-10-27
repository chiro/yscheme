;;; 6.2. Numbers

(in-package :yscheme)


(defun radix-n-to-number (n num)
  (read-from-string (format nil "#~Ar~A" n num)))

(defun number-class-of (val)
  (cond ((integerp val)      'scm-integer)
        ((zerop (mod val 1)) 'scm-integer)
        ((rationalp val)     'scm-rational)
        ((floatp val)        'scm-rational)
        ((realp val)         'scm-real)
        ((complexp val)      'scm-complex)))

(defmacro define-numerical-operation (name &key op ex (sel :first))
  ;; sel :first :second :multi
  (with-gensyms (parm exact v1 v2)
    `(progn
       (defgeneric ,name (&rest ,parm))
       (defmethod ,name (&rest ,parm)
         (multiple-value-bind (,v1 ,v2)
             (apply ,op (mapcar (lambda (num) (val num)) ,parm))
           (declare (ignorable ,v1 ,v2))
           (let ((,exact (or ,ex
                             (reduce (lambda (f s) (and f s))
                                     (mapcar (lambda (num) (ex num)) ,parm)))))
             ,(case sel
                    (:first `(new 'scm-number :val ,v1 :ex ,exact))
                    (:second `(new 'scm-number :val ,v2 :ex ,exact))
                    (:multi `(values (new 'scm-number :val ,v1 :ex ,exact)
                                     (new 'scm-number :val ,v2 :ex ,exact))))))))))

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

(define-numerical-operation scm-floor/ :op #'floor :sel :multi)
(define-numerical-operation scm-floor-quotient :op #'floor :sel :first)
(define-numerical-operation scm-floor-remainder :op #'floor :sel :second)

(define-numerical-operation scm-ceiling/ :op #'ceiling :sel :multi)
(define-numerical-operation scm-ceiling-quotient :op #'ceiling :sel :first)
(define-numerical-operation scm-ceiling-remainder :op #'ceiling :sel :second)

(define-numerical-operation scm-truncate/ :op #'truncate :sel :multi)
(define-numerical-operation scm-truncate-quotient :op #'truncate :sel :first)
(define-numerical-operation scm-truncate-remainder :op #'truncate :sel :second)

(define-numerical-operation scm-round/ :op #'round :sel :multi)
(define-numerical-operation scm-round-quotient :op #'round :sel :first)
(define-numerical-operation scm-round-remainder :op #'round :sel :second)

(defun euclidean (n1 n2)
  (if (> n2 0) (ceiling n1 n2) (floor n1 n2)))

(define-numerical-operation scm-euclidean/ :op #'euclidean :sel :multi)
(define-numerical-operation scm-euclidean-quotient :op #'euclidean :sel :first)
(define-numerical-operation scm-euclidean-remainder :op #'euclidean :sel :second)

(define-numerical-operation scm-quotient :op #'truncate :sel :first)
(define-numerical-operation scm-remainder :op #'truncate :sel :second)
(define-numerical-operation scm-modulo :op #'mod)

(define-numerical-operation scm-gcd :op #'gcd)
(define-numerical-operation scm-lcm :op #'lcm)

(define-numerical-operation scm-numerator :op #'numerator)
(define-numerical-operation scm-denominator :op #'denominator)

(define-numerical-operation scm-floor :op #'floor)
(define-numerical-operation scm-ceiling :op #'ceiling)
(define-numerical-operation scm-truncate :op #'truncate)
(define-numerical-operation scm-round :op #'round)

(defun scm-rationalize0 (x y)
  (let ((x (rationalize x)))
    (labels ((rec (p q)
               (cond ((< y (abs (- (/ p q) x))) nil)
                     ((zerop p) (list 0 1))
                     ((= 1 q) (list p 1))
                     (t (append (list (list p q)) (rec (1- p) q) (rec p (1- q)))))))
      (apply #'/
             (reduce (lambda (f s) (if (<= (apply #'+ f) (apply #'+ s)) f s))
                     (rec (numerator x) (denominator x)))))))

(define-numerical-operation scm-rationalize :op #'scm-rationalize0)

(define-numerical-operation scm-exp :op #'exp :ex nil)
(define-numerical-operation scm-log :op #'log :ex nil)
(define-numerical-operation scm-sin :op #'sin :ex nil)
(define-numerical-operation scm-cos :op #'cos :ex nil)
(define-numerical-operation scm-tan :op #'tan :ex nil)
(define-numerical-operation scm-asin :op #'asin :ex nil)
(define-numerical-operation scm-acos :op #'acos :ex nil)
(define-numerical-operation scm-atan :op #'atan :ex nil)

(define-numerical-operation scm-sqrt :op #'sqrt :ex nil)

(define-numerical-operation scm-exact-integer-sqrt :op #'sqrt)

(define-numerical-operation scm-expt :op #'expt)

;(define-numerical-operation scm-mkae-rectangular :op #')
;(define-numerical-operation scm-make-polar :op #')
(define-numerical-operation scm-real-part :op #'realpart)
(define-numerical-operation scm-imag-part :op #'imagpart)
(define-numerical-operation scm-magnitude :op #'abs)
(define-numerical-operation scm-angle
    :op #'(lambda (z) (atan (imagpart z) (realpart z))))

(define-numerical-operation scm-exact->inexact :op #'(lambda (z) z) :ex nil)
(define-numerical-operation scm-inexact->exact :op #'(lambda (z) z) :ex t)


;; 数値を表す文字列->数値 : 全射(単射でない) 基数に関する情報は落ちる
;; 数値->数値を表す文字列 : 単射(全射でない)
;(defun number-to-string (num radix)
;  ())

(defgeneric scm-number->string (obj1 &optional obj2))
(defmethod scm-number->string ((num scm-number) &optional radix)
  (number-to-string num radix))


;(defun string-to-number (str radix)
;  ())

;(defun string-of-exact-number-p (str)
;  ())

(defgeneric scm-string->number (obj1 &optional obj2))
(defmethod scm-string->number ((str scm-string) &optional radix)
  (new 'scm-number
       :val (string-to-number str radix)
       :ex (string-of-exact-number str)))
