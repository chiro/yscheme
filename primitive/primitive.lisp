
(in-package :yscheme)

;; define-predicate で書き換えて他のファイルへ


;;; 3.2. type

(defgeneric symbol? (obj))
(defmethod symbol? ((obj scm-object)) +false+)
(defmethod symbol? ((obj scm-symbol)) +true+)

(defgeneric char? (obj))
(defmethod char? ((obj scm-object)) +false+)
(defmethod char? ((obj scm-character)) +true+)

(defgeneric string? (obj))
(defmethod string? ((obj scm-object)) +false+)
(defmethod string? ((obj scm-string)) +true+)

;(defgeneric blob? (obj))
;(defmethod blob? ((obj scm-object)) +false+)
;(defmethod blob? ((obj scm-blob)) +true+)

;(defgeneric port? (obj))
;(defmethod port? ((obj scm-object)) +false+)
;(defmethod port? ((obj scm-port)) +true+)

(defgeneric procedure? (obj))
(defmethod procedure? ((obj scm-object)) +false+)
(defmethod procedure? ((obj procedure)) +true+)

;;; i/o

(defgeneric scm-display (obj))
(defmethod scm-display ((obj scm-object))
  (prin1 obj))

(defgeneric scm-newline (obj))
(defmethod scm-newline ((obj scm-object))
  (prin1 #\Newline))
