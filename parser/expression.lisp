(in-package :yscheme)

(defmacro insert_space (&rest lst)
  `(esrap::and ,@(flatten (insert-every 'intertoken_space
                                       lst))))
(defun insert-every (exps lst)
  (if (null lst) nil
      (cons exps
            (cons (car lst)
                  (insert-every exps (cdr lst))))))

(esrap::defrule yscheme::expression
    (esrap::and intertoken_space
                (esrap::or
                 scm_variable
                 literal
                 procedure_call
                 lambda_expression
                 conditional
                 assignment
                 ;; derived_expression
                 ;; macro_use
                 ;; macro_block
                 )
                intertoken_space
                )
  (:lambda (data) (cadr data))
)

(esrap::defrule literal
    (esrap::or quotation
               self-evaluating))

(esrap::defrule self-evaluating
    (esrap::or scm_boolean
               scm_number
               scm_character
               scm_string))

(esrap::defrule quotation
    (esrap::or (esrap::and intertoken_space
                           "'" intertoken_space
                           datum)
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "quote" intertoken_space
                           datum intertoken_space
                           ")"))
  (:lambda (data)
    (if (= (length data) 4)
        (make-instance 'quotation :qexp (cadddr data))
        (make-instance 'quotation :qexp (caddr (cdddr data)))))
)

(esrap::defrule procedure_call
    (esrap::and intertoken_space
                "(" intertoken_space
                operator intertoken_space
                (esrap::* operand) intertoken_space
                ")")
  (:destructure (s1 p1 s2 ope s3 opl s4 p2)
                (make-instance 'application :proc ope :args opl))
;  (:lambda (lst) (remove-if #'null lst))
)

(esrap::defrule operator
    (esrap::and intertoken_space expression)
  (:destructure (sp ex) ex)
)
(esrap::defrule operand
    (esrap::and intertoken_space expression)
  (:destructure (sp ex) ex)
)

(esrap::defrule lambda_expression
    (esrap::and intertoken_space
                "(" intertoken_space
                "lambda" intertoken_space
                formals intertoken_space
                body intertoken_space
                ")"))

(esrap::defrule formals
    (esrap::or (esrap::and intertoken_space
                           "(" intertoken_space
                           (esrap::* scm_variable) intertoken_space
                           ")")
               (esrap::and intertoken_space scm_variable)
               (esrap::and intertoken_space
                           "(" intertoken_space
                           (esrap::+ scm_variable) intertoken_space
                           "." intertoken_space
                           scm_variable intertoken_space
                           ")")
               )
)

(esrap::defrule body
    (esrap::and (esrap::* syntax_definition) intertoken_space
                (esrap::* definition) intertoken_space
                sequence)
  (:lambda (data) (remove-if #'null data))
)

;; left recursive!!
(esrap::defrule sequence
    (esrap::and (esrap::* command) intertoken_space))


(esrap::defrule command
    (esrap::and intertoken_space
                expression))

(esrap::defrule conditional
    (esrap::and intertoken_space
                "(" intertoken_space
                "if" intertoken_space
                test
                consequent
                alternate intertoken_space
                ")")
  (:destructure (s1 p1 s2 iff s3 tst cosq alt s4 p2)
                (make-instance 'if-exp :pred tst :then cosq :else alt))
)
(esrap::defrule test
    expression)
(esrap::defrule consequent
    expression)
(esrap::defrule alternate
    (esrap::? expression))

(esrap::defrule assignment
    (esrap::and intertoken_space
                "(" intertoken_space
                "set!" intertoken_space
                scm_variable
                expression intertoken_space
                ")")
  (:destructure (s1 p1 s2 sets s3 var exp s4 p2)
                (make-instance 'assignment :sym var :val exp))
  ;; (:lambda (asg)
  ;;   (remove-if #'null asg))
)

