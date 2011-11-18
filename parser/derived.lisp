(in-package :yscheme)

(esrap::defrule yscheme::derived_expression
    (esrap::or (esrap::and intertoken_space "(" intertoken_space "cond" intertoken_space
                           (esrap::+ cond_clause) ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "cond" intertoken_space
                           (esrap::* cond_clause) "(" intertoken_space "else"
                           intertoken_space sequence ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "case" expression
                           (esrap::+ case_clause) ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "case" expression
                           (esrap::+ case_clause) "(" intertoken_space "else"
                           intertoken_space sequence ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "case" expression
                           (esrap::+ case_clause) "(" intertoken_space "else"
                           intertoken_space "=>" intertoken_space sequence ")"
                           intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "and" intertoken_space
                           (esrap::* test) ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "or" intertoken_space
                           (esrap::* test) ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "when" intertoken_space
                           test expression sequence intertoken_space intertoken_space
                           ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "unless"
                           intertoken_space test expression sequence intertoken_space
                           intertoken_space ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "let"
                           (esrap::? (esrap::and " " identifier)) intertoken_space
                           "(" intertoken_space (esrap::* binding_spec) intertoken_space
                           ")" intertoken_space body ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "let*"
                           intertoken_space "(" intertoken_space (esrap::* binding_spec)
                           intertoken_space ")" intertoken_space body
                           ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "letrec"
                           intertoken_space "(" intertoken_space (esrap::* binding_spec)
                           intertoken_space ")" intertoken_space body
                           ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "letrec*"
                           intertoken_space "(" intertoken_space (esrap::* binding_spec)
                           intertoken_space ")" intertoken_space body ")"
                           intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "let-values"
                           intertoken_space "(" intertoken_space
                           (esrap::* mv_binding_spec) intertoken_space ")"
                           intertoken_space body ")" intertoken_space)
               (esrap::and intertoken_space "(" intertoken_space "let*-values"
                           intertoken_space "(" intertoken_space
                           (esrap::* mv_binding_spec) intertoken_space ")"
                           intertoken_space body ")" intertoken_space)
               ;;
               )
  (:lambda (data)
    (scase ((fourth data) :test #'string=)
           ("cond" (if (= (length data) 8)
                       (make-instance 'cond-exp :clauses (sixth data))
                       (make-instance 'cond-exp
                                      :clauses (list (sixth data)
                                                     (make-instance
                                                      'cond-else-clause
                                                      :exps (nth 11 data))))))
           ("case" (case (length data)
                     (8  (make-instance 'case-exp :clauses (sixth data)))
                     (13 (make-instance 'case-exp
                                        :clauses (list (sixth data)
                                                       (make-instance
                                                        'cond-else-clause
                                                        :datums (fifth data)
                                                        :exps (nth 11 data)))))
                     (15 (make-instance 'case-exp
                                        :clauses (list (sixth data)
                                                       (make-instance
                                                        'case-else-clause-with-proc
                                                        :datums (fifth data)
                                                        :exps (first (tenth data))))))))
           ("and"             (make-instance 'and-exp :exps (sixth data)))
           ("or"              (make-instance 'or-exp :exps (sixth data)))
           ("when"            (make-instance 'when-exp
                                             :test (sixth data)
                                             :exps (cons (seventh data) (eighth data))))
           ("unless"          (make-instance 'unless-exp
                                             :test (sixth data)
                                             :exps (cons (seventh data) (eighth data))))
           ("let"         (if (= (length (fifth data)) 2)
                              (make-instance 'named-let-exp
                                             :sym (third (fifth data))
                                             :binds (ninth data)
                                             :body (nth 12 data))
                              (make-instance 'let-exp
                                             :binds (ninth data)
                                             :body (nth 12 data))))
           ("let*"            (make-instance 'let*-exp
                                             :binds (eighth data)
                                             :body (nth 11 data)))
           ("letrec"          (make-instance 'letrec-exp
                                             :binds (eighth data)
                                             :body (nth 11 data)))
           ("letrec*"         (make-instance 'letrec*-exp
                                             :binds (eighth data)
                                             :body (nth 11 data)))
           ("let-values"      (make-instance 'let-values-exp
                                             :binds (eighth data)
                                             :body (nth 11 data)))
           ("let*-values"     (make-instance 'let*-values-exp
                                             :binds (eighth data)
                                             :body (nth 11 data)))
)))

(esrap::defrule yscheme::cond_clause
    (esrap::or (esrap::and intertoken_space "(" test sequence ")" intertoken_space)
               test
               (esrap::and test "=>" recipient))
  (:lambda (data)
    (case (length data)
      (6 (make-instance 'cond-clause :test (third data) :exps (fourth data)))
      (1 (make-instance 'cond-clause :test data :exps nil))
      (3 (make-instance 'cond-clause-with-proc
                        :test (first data) :exps (list (third data)))))))

(esrap::defrule yscheme::recipient expression)

(esrap::defrule yscheme::case_clause
    (esrap::or (esrap::and intertoken_space "(" intertoken_space "(" (esrap::* datum)
                           intertoken_space ")"  intertoken_space sequence ")")
               (esrap::and intertoken_space "(" intertoken_space "(" (esrap::* datum)
                           intertoken_space ")" intertoken_space "=>" intertoken_space
                           recipient ")"))
  (:lambda (data)
    (case (length data)
      (10 (make-instance 'case-clause :datums (fifth data) :exps (ninth data)))
      (12 (make-instance 'case-clause-with-proc
                         :datums (fifth data) :exps (first (tenth data)))))))

(esrap::defrule yscheme::binding_spec
    (esrap::and intertoken_space "(" intertoken_space identifier expression ")"
                intertoken_space)
  (:destructure (s1 p1 s2 id ex p2 s3)
    (make-instance 'binding :sym id :init ex)))

(esrap::defrule yscheme::mv_binding_spec
    (esrap::and intertoken_space "(" intertoken_space formals expression ")"
                intertoken_space)
  (:destructure (s1 p1 s2 fm ex p2 s3)
    (make-instance 'mv-binding :syms (syms fm) :init ex)))

(esrap::defrule yscheme::iteration_spec
    (esrap::or (esrap::and intertoken_space "(" identifier intertoken_space init
                           intertoken_space step ")" intertoken_space)
               (esrap::and intertoken_space "(" identifier intertoken_space init ")"
                           intertoken_space))

)

(esrap::defrule yscheme::case-lambda_clause
    (esrap::and intertoken_space "(" formals intertoken_space body ")")
)

(esrap::defrule yscheme::init expression)

(esrap::defrule yscheme::step expression)

(esrap::defrule yscheme::do_result
    (esrap::* sequence)
)

(esrap::defrule yscheme::macro_use
    (esrap::and intertoken_space "(" intertoken_space keyword intertoken_space
                (esrap::* datum) intertoken_space ")" intertoken_space)
)

(esrap::defrule yscheme::keyword identifier)
