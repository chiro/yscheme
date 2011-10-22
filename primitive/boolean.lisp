;;; 6.3. Other data typp
;;; 6.3.1. Booleans

(in-package :yscheme)


(define-predicate scm-not ((obj scm-boolean)) +false+
  (if (val obj) +false+ +true+))

(define-predicate boolean? ((obj scm-boolean)) +false+ +true+)