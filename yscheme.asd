(defpackage :yscheme-system (:use :cl :asdf))
(in-package :yscheme-system)

(defsystem :yscheme
  :serial t
  :components ((:file "package")
               (:file "variables")
               (:file "util")
               (:file "func")
               (:file "onlisp")



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

               (:file "primitive")
               (:module std
                        :serial t
                        :components ((:file "equiv")
                                      (:file "number")
                                      (:file "boolean")
                                      (:file "list")
                                      ;; (:file "symbol")
                                      ;; (:file "character")
                                      ;; (:file "string")
                                      ;; (:file "vector")
                                      ;; (:file "blobs")
                                     ))

               (:file "env")
               (:file "eval")
               )
  :depends-on (:esrap :cl-unicode)
  )
