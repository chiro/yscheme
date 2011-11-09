(defparameter *env (scheme-report-environment 7))

(defparameter *proc
  (scm-eval (parse-program-from-file "./fact.scm")
            env))
(defparameter *result (scm-eval (parse-program "(fact 10)") *env))


(defparameter *a (scm-eval (parse-program "(define a 100)") *env))
(defparameter *list (scm-eval (parse-program "(list 1 2 3)") *env))
(defparameter *list1 (scm-eval (parse-program "(list 1 a 3)") *env))


(defparameter *list-a (scm-eval (parse-program "(define a (list 1 2 3))") *env))
(defparameter *list-b (scm-eval (parse-program "(define b (list 4 5 6))") *env))
(defparameter *list-c (scm-eval (parse-program "(define c (list 7 8 9))") *env))

(defparameter *list-ra (scm-eval (parse-program "(define r (reverse a))") *env))

(defparameter *list-abc (scm-eval (parse-program "(define abc (append a b c))") *env))