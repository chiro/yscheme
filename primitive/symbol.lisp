;;; 6.3.3. Symbols

(in-package :yscheme)


(define-predicate symbol? ((obj scm-symbol)) +false+ +true+)

(defgeneric symbol->string (obj))
(defmethod symbol->string ((sym scm-symbol))
  (new 'scm-string :val (name sym)))

(defgeneric string->symbol (obj))
(defmethod string->symbol ((str scm-string))
  (new 'scm-symbol :name (val str)))
