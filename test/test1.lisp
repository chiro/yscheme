;;; 環境

(defvar test-env1 (make-env))

;;; 変数定義

(defparameter test-define1
  (make-instance 'variable-definition
                 :sym (make-instance 'scm-symbol :name "hoge")
                 :val (make-instance 'scm-number :val 1000)))

(scm-eval test-define1 test-env1)
(scm-eval (make-instance 'scm-symbol :name "hoge") test-env1)

;;; プリミティブ手続き

(defparameter test-primitive1
  (make-instance 'application
                 :proc (make-instance 'primitive-procedure :func #'scm-+)
                 :args (list (make-instance 'scm-number :val 10)
                             (make-instance 'scm-number :val 1)
                             (make-instance 'scm-symbol :name "hoge"))))

(val (scm-eval test-primitive1 test-env1))

(defparameter test-primitive2
  (make-instance 'application
                 :proc (make-instance 'primitive-procedure :func #'scm--)
                 :args (list (make-instance 'scm-number :val 10)
                             (make-instance 'scm-number :val 1)
                             (make-instance 'scm-symbol :name "hoge"))))

(val (scm-eval test-primitive2 test-env1))

;;; 関数定義 (fact)

(defparameter test-define2
  (make-instance
   'function-definition
   :sym (make-instance 'scm-symbol :name "fact")
   :parms (list (make-instance 'scm-symbol :name "n"))
   :body (make-instance
          'if-exp
          :pred (make-instance
                 'application
                 :proc (make-instance 'scm-symbol :name "<=")
                 :args (list
                        (make-instance 'scm-symbol :name "n")
                        (make-instance 'scm-number :val 0)))
          :then (make-instance 'scm-number :val 1)
          :else (make-instance
                 'application
                 :proc (make-instance 'scm-symbol :name "*")
                 :args (list
                        (make-instance 'scm-symbol :name "n")
                        (make-instance
                         'application
                         :proc (make-instance 'scm-symbol :name "fact")
                         :args (list
                                (make-instance
                                 'application
                                 :proc (make-instance 'scm-symbol :name "-")
                                 :args (list
                                        (make-instance 'scm-symbol :name "n")
                                        (make-instance 'scm-number :val 1))))))))))

(scm-eval test-define2 *interaction-environment*)
(val (scm-eval (make-instance 'application
                              :proc (make-instance 'scm-symbol :name "fact")
                              :args (list (make-instance 'scm-number :val 3)))
               *interaction-environment*))

(val (scm-eval (make-instance 'application
                              :proc (make-instance 'scm-symbol :name "fact")
                              :args (list (make-instance 'scm-number :val 6)))
               *interaction-environment*))

