;;; Load Library

(in-package :yscheme)


(defvar *load-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "load"))
       :env (prepare-environment '(

;"load"
))))