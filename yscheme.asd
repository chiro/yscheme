(defpackage :yscheme-system (:use :cl :asdf))
(in-package :yscheme-system)

(defsystem :yscheme
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parser/generic")
               (:file "parser/space")
               (:file "parser/comment")
               (:file "parser/identifier")
               (:file "parser/value")
               (:file "parser/number")
               (:file "parser/datum")
               (:file "parser/parser")
               )
  :depends-on (:esrap)
  )
