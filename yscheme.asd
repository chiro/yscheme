(defpackage :yscheme-system (:use :cl :asdf))
(in-package :yscheme-system)

(defsystem :yscheme
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "onlisp")

               (:file "variables")

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

               (:module primitive
                        :serial t
                        :components ((:file "equiv")
                                     (:file "number")
                                     (:file "primitive")))
               )
  :depends-on (:esrap)
  )
