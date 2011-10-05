(defpackage :yscheme-system (:use :cl :asdf))
(in-package :yscheme-system)

(defsystem :yscheme
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "parse"))
  :depends-on (:esrap)
  )
