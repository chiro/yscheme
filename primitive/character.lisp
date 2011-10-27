;;; 6.3.4. Characters

(in-package :yscheme)


(defvar +whitespace+ '(#\Space #\Newline))

(define-predicate char? ((obj scm-character)) +false+ +true+)


(define-equiv char=? ((obj1 obj2 scm-character))
  :test (char= (val char1) (val char2)))

(define-equiv char<? ((obj1 obj2 scm-character))
  :test (char< (val char1) (val char2)))

(define-equiv char>? ((obj1 obj2 scm-character))
  :test (char> (val char1) (val char2)))

(define-equiv char<=? ((obj1 obj2 scm-character))
  :test (char<= (val char1) (val char2)))

(define-equiv char>=? ((obj1 obj2 scm-character))
  :test (char>= (val char1) (val char2)))


(define-equiv char-ci=? ((obj1 obj2 scm-character))
  :test (char-equal (val char1) (val char2)))

(define-equiv char-ci<? ((obj1 obj2 scm-character))
  :test (char-lessp (val char1) (val char2)))

(define-equiv char-ci>? ((obj1 obj2 scm-character))
  :test (char-greaterp (val char1) (val char2)))

(define-equiv char-ci<=? ((obj1 obj2 scm-character))
  :test (char-not-greaterp (val char1) (val char2)))

(define-equiv char-ci>=? ((obj1 obj2 scm-character))
  :test (char-not-lessp (val char1) (val char2)))


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
(defgeneric char->integer ((char scm-character))
  (new 'scm-number
       :val (char-code (val char)) :ex t))

(defgeneric integer->char (obj))
(defmethod integer->char ((n scm-number))
  (new 'scm-charater
       :val (code-char (val scm-number))))

(defgeneric char-upcase (obj))
(defmethod char-upcase ((char scm-character))
  (new 'scm-character
       :val (cl-unicode:uppercase-mapping (val char))))

(defgeneric char-downcase (obj))
(defmethod char-downcase ((char scm-character))
  (new 'scm-character
       :val (cl-unicode:lowercase-mapping (val char) 0)))

(defgeneric char-foldcase (obj))
(defmethod char-foldcase ((char scm-character))
  (cond ((scm-truep (character-upper-case? char)) (char-upcase char))
        ((scm-truep (character-lower-case? char)) (char-downcase char))))
