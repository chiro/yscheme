;;; 6.3.4. Characters

(in-package :yscheme)


(defvar +whitespace+ '(#\Space #\Newline))

(define-predicate char? ((obj scm-character)) +false+ +true+)


(define-compare char=? (objs)
  :test (reduce (lambda (f s) (and f (char= (val f) (val s)) s)) objs)

(define-compare char<? (objs)
  :test (reduce (lambda (f s) (and f (char< (val f) (val s)) s)) objs)

(define-compare char>? (objs)
  :test (reduce (lambda (f s) (and f (char> (val f) (val s)) s)) objs))

(define-compare char<=? (objs)
  :test (reduce (lambda (f s) (and f (char<= (val f) (val s)) s)) objs))

(define-compare char>=? (objs)
  :test (reduce (lambda (f s) (and f (char>= (val f) (val s)) s)) objs))


(define-compare char-ci=? (objs)
  :test (reduce (lambda (f s) (and f (char-equal (val f) (val s)) s)) objs))

(define-compare char-ci<? (objs)
  :test (reduce (lambda (f s) (and f (char-lessp (val f) (val s)) s)) objs))

(define-compare char-ci>? (objs)
  :test (reduce (lambda (f s) (and f (char-greaterp (val f) (val s)) s)) objs))

(define-compare char-ci<=? (objs)
  :test (reduce (lambda (f s) (and f (char-not-greaterp (val f) (val s)) s)) objs))

(define-compare char-ci>=? (objs)
  :test (reduce (lambda (f s) (and f (char-not-lessp (val f) (val s)) s)) objs))


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
