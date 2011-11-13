;;; 6.3.3. Symbols

(in-package :yscheme)


(define-predicate scm-symbol? ((obj scm-symbol)) +false+ +true+)

(defgeneric scm-symbol->string (obj))
(defmethod scm-symbol->string ((sym scm-symbol))
  (new 'scm-string :val (name sym)))

(defgeneric scm-string->symbol (obj))
(defmethod scm-string->symbol ((str scm-string))
  (new 'scm-symbol :name (val str)))
