(defpackage :yscheme-system (:use :cl :asdf))
(in-package :yscheme-system)

(defsystem :yscheme
  :serial t
  :components ((:file "package")
               (:file "util")

               (:module parser
                        :serial t
                        :components ((:file "generic")
                                     (:file "space")
                                     (:file "comment")
                                     (:file "identifier")
                                     (:file "value")
                                     (:file "number")
                                     (:file "datum")
                                     (:file "expression")
                                     (:file "program")
                                     (:file "parser")))
               )
  :depends-on (:esrap)
  )
