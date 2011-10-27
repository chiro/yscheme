;;; 6.3.4. Characters

(in-package :yscheme)


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


(defgeneric character-alphabetic (obj))
(defmethod character-alphabetic ((char scm-character))

(defgeneric character-numeric (obj))
(defmethod character-numeric ((char scm-character))
  (if (find (val char)

(defgeneric character-whitespace (obj))
(defmethod character-whitespace ((char scm-character))
  (if (char= (val cahr) " ") +true+ +false+))

(defgeneric character-upper-case (obj))
(defmethod character-upper-case ((char scm-character))

(defgeneric character-lower-case (obj))
(defmethod character-lower-case ((char scm-character))


(defgeneric char->integer (obj))
(defgeneric char->integer ((char scm-character))

(defgeneric integer->char (obj))
(defmethod integer->char ((n scm-number))


(defgeneric char-upcase (obj))
(defmethod char-upcase ((char scm-character))


(defgeneric char-down-case (obj))
(defmethod char-downcase ((char scm-character))

(defgeneric char-foldcase (obj))
(defmethod char-foldcase ((char scm-character))
