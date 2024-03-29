(in-package :yscheme)

(esrap::defrule yscheme::expression
    (esrap::and intertoken_space
                (esrap::or
                 scm_variable
                 literal
                 procedure_call
                 lambda_expression
                 conditional
                 assignment
                 derived_expression
                 ;; macro_use
                 ;; macro_block
                 empty-sequence
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

(esrap::defrule operator expression)

(esrap::defrule operand expression)

(esrap::defrule lambda_expression
    (esrap::and intertoken_space
                "(" intertoken_space
                "lambda" intertoken_space
                formals intertoken_space
                body intertoken_space
                ")")
  (:destructure (sp1 lp sp2 lbd sp3 frm sp4 bdy sp5 rp)
    (make-instance 'compound-procedure
                   :parms frm :body bdy)))

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
                           ")"))
  (:lambda (data)
    (cond ((= (length data) 6)
           (make-instance 'scm-parameters :syms (fourth data)))
          ((= (length data) 2)
           (make-instance 'scm-parameters :rst (second data)))
          ((= (length data) 10)
           (make-instance 'scm-parameters :syms (fourth data) :rst (eighth data))))))

(esrap::defrule body
    (esrap::and (esrap::* syntax_definition) intertoken_space
                (esrap::* definition) intertoken_space
                sequence)
  (:destructure (sdef s1 def s2 sq) ;;TODO
    (if (null (cdr sq))
        (car sq)
        (make-instance 'begin :exps sq)))
)

;; left recursive!!
(esrap::defrule sequence
    (esrap::and (esrap::* command) intertoken_space)
  (:destructure (clist sp) clist))

(esrap::defrule command expression)

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
)

