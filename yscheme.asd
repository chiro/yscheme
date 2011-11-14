(defpackage :yscheme-system (:use :cl :asdf))
(in-package :yscheme-system)

(defsystem :yscheme
  :serial t
  :components ((:file "package")
               (:file "variables")
               (:file "util")
               (:file "func")
               (:file "onlisp")

               (:module standard
                        :serial t
                        :components ((:file "equiv")
                                     (:file "number")
                                     (:file "boolean")
                                     (:file "list")
                                     (:file "symbol")
                                     (:file "character")
                                     (:file "string")
                                     (:file "vector")
                                     (:file "bytevector")
                                     (:file "control")
                                     (:file "eval")
                                     (:file "env")
                                     (:file "io")))

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

               (:module structure
                        :serial t
                        :components ((:file "def")
                                     ;; (:file "syndef")
                                     (:file "recdef")
                                     (:file "libdef")
                                     ))

               (:module library
                        :serial t
                        :components ((:file "base")
                                     (:file "case-lambda")
                                     (:file "char-normalization")
                                     (:file "char")
                                     (:file "complex")
                                     (:file "division")
                                     (:file "eval")
                                     (:file "file")
                                     (:file "inexact")
                                     (:file "lazy")
                                     (:file "load")
                                     (:file "process-context")
                                     (:file "read")
                                     (:file "repl")
                                     (:file "time")
                                     (:file "write")
                                     ))

               (:file "prepare")
               (:file "repl")
               )
  :depends-on (:esrap :cl-unicode)
  )
