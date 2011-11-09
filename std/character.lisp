;;; 6.3.4. Characters

(in-package :yscheme)


(defvar +whitespace+ (list #\Space #\Newline))

(define-predicate char? ((obj scm-character)) +false+ +true+)


;; mapcar de yoi

(define-compare char=? (objs)
  :test (apply #'char= (mapcar #'val objs)))

(define-compare char<? (objs)
  :test (apply #'char< (mapcar #'val objs)))

(define-compare char>? (objs)
  :test (apply #'char> (mapcar #'val objs)))

(define-compare char<=? (objs)
  :test (apply #'char<= (mapcar #'val objs)))

(define-compare char>=? (objs)
  :test (apply #'char>= (mapcar #'val objs)))


(define-compare char-ci=? (objs)
  :test (apply #'char-equal (mapcar #'val objs)))

(define-compare char-ci<? (objs)
  :test (apply #'char-lessp (mapcar #'val objs)))

(define-compare char-ci>? (objs)
  :test (apply #'char-greaterp (mapcar #'val objs)))

(define-compare char-ci<=? (objs)
  :test (apply #'char-not-greaterp (mapcar #'val objs)))

(define-compare char-ci>=? (objs)
  :test (apply #'char-not-lessp (mapcar #'val objs)))


(defgeneric character-alphabetic? (obj))
(defmethod character-alphabetic? ((char scm-character))
  (char= (char (cl-unicode:general-category (val char)) 0) #\L))

(defgeneric character-numeric? (obj))
(defmethod character-numeric? ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 0) #\N)
      +true+ +false+))

(defgeneric character-whitespace? (obj))
(defmethod character-whitespace? ((char scm-character))
  (if (member (val char) +whitespace+)
      +true+ +false+))

(defgeneric character-upper-case? (obj))
(defmethod character-upper-case? ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 1) #\u)
      +true+ +false+))

(defgeneric character-lower-case? (obj))
(defmethod character-lower-case? ((char scm-character))
  (if (char= (char (cl-unicode:general-category (val char)) 1) #\l)
      +true+ +false+))

(defgeneric char->integer (obj))
(defmethod char->integer ((char scm-character))
  (new 'scm-number
       :val (char-code (val char)) :ex t))

(defgeneric integer->char (obj))
(defmethod integer->char ((n scm-number))
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
  (cond ((scm-truep (character-upper-case? char)) (char-upcase char))
        ((scm-truep (character-lower-case? char)) (char-downcase char))))

