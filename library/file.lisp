;;; File Library

(in-package :yscheme)


(defvar *file-library*
  (new 'library
       :syms (list (new 'scm-symbol :name "scheme")
                   (new 'scm-symbol :name "file"))
       :env (prepare-environment '(

"call-with-input-file"
"call-with-output-file"
;"delete-file"
;"file-exists?"
"open-binary-input-file"
"open-binary-output-file"
"open-input-file"
"open-output-file"
"with-input-from-file"
"with-output-to-file"
))))