(in-package :yscheme)


;; eval中のconditionを表すクラス
(defclass scm-condition () ())

(defclass scm-error (scm-condition) ())