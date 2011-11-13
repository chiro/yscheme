;;; 6.3.4. Characters

(in-package :yscheme)


(defvar +whitespace+ (list #\Space #\Newline))

(define-predicate scm-char? ((obj scm-character)) +false+ +true+)


(define-compare scm-char=? (objs)
  :test (apply #'char= (mapcar #'val objs)))

(define-compare scm-char<? (objs)
  :test (apply #'char< (mapcar #'val objs)))

(define-compare scm-char>? (objs)
  :test (apply #'char> (mapcar #'val objs)))

(define-compare scm-char<=? (objs)
  :test (apply #'char<= (mapcar #'val objs)))

(define-compare scm-char>=? (objs)
  :test (apply #'char>= (mapcar #'val objs)))


(define-compare scm-char-ci=? (objs)
  :test (apply #'char-equal (mapcar #'val objs)))

(define-compare scm-char-ci<? (objs)
  :test (apply #'char-lessp (mapcar #'val objs)))

(define-compare scm-char-ci>? (objs)
  :test (apply #'char-greaterp (mapcar #'val objs)))

(define-compare scm-char-ci<=? (objs)
  :test (apply #'char-not-greaterp (mapcar #'val objs)))

(define-compare scm-char-ci>=? (objs)
  :test (apply #'char-not-lessp (mapcar #'val objs)))


(defgeneric scm-char-alphabetic? (obj))
(defmethod scm-char-alphabetic? ((char scm-character))
  (char= (char (cl-unicode:general-category (val char)) 0) #\L))

(defgeneric scm-char-numeric? (obj))
(defmethod scm-char-numeric? ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 0) #\N)
      +true+ +false+))

(defgeneric scm-char-whitespace? (obj))
(defmethod scm-char-whitespace? ((char scm-character))
  (if (member (val char) +whitespace+)
      +true+ +false+))

(defgeneric scm-char-upper-case? (obj))
(defmethod scm-char-upper-case? ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 1) #\u)
      +true+ +false+))

(defgeneric scm-char-lower-case? (obj))
(defmethod scm-char-lower-case? ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 1) #\l)
      +true+ +false+))

(defgeneric scm-digit-value (obj))
(defmethod scm-digit-value ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 0) #\N)
      (new 'scm-number
           :val (cl-unicode:numeric-value (val char))
           :ex t)
      +false+))

(defgeneric scm-char->integer (obj))
(defmethod scm-char->integer ((char scm-character))
  (new 'scm-number
       :val (char-code (val char)) :ex t))

(defgeneric scm-integer->char (obj))
(defmethod scm-integer->char ((n scm-number))
  (new 'scm-charater :val (code-char (val n))))

(defgeneric scm-char-upcase (obj))
(defmethod scm-char-upcase ((char scm-character))
  (new 'scm-character
       :val (cl-unicode:uppercase-mapping (val char))))

(defgeneric scm-char-downcase (obj))
(defmethod scm-char-downcase ((char scm-character))
  (new 'scm-character
       :val (cl-unicode:lowercase-mapping (val char) 0)))

(defgeneric scm-char-foldcase (obj))
(defmethod scm-char-foldcase ((char scm-character))
  (cond ((scm-truep (scm-char-upper-case? char)) (char-upcase char))
        ((scm-truep (scm-char-lower-case? char)) (char-downcase char))))

