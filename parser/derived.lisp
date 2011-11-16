(in-package :yscheme)

(esrap::defrule yscheme::derived_expression
    (esrap::or (esrap::and intertoken_space "(" intertoken_space "cond" intertoken_space
                           (esrap::+ cond_clause) ")")
               (esrap::and intertoken_space "(" intertoken_space "cond" intertoken_space
                           (esrap::* cond_clause)
                           "(" intertoken_space "else" intertoken_space sequence ")")
               (esrap::and intertoken_space "(" intertoken_space "case" intertoken_space
                           expression intertoken_space (esrap::+ case_clause) ")")
               (esrap::and intertoken_space "(" intertoken_space "case" intertoken_space
                           expression intertoken_space (esrap::+ case_clause)
                           "(" intertoken_space "else" intertoken_space sequence ")")
               (esrap::and "(" intertoken_space "and" intertoken_space
                           (esrap::* test) ")")
               (esrap::and "(" intertoken_space "or" intertoken_space
                           (esrap::* test) ")")
               (esrap::and "(" intertoken_space "when" intertoken_space
                           test expression sequence intertoken_space
                           (esrap::* test) intertoken_space ")")
               (esrap::and "(" intertoken_space "let"
                           intertoken_space identifier
                           intertoken_space "(" intertoken_space (esrap::* binding_spec)
                           intertoken_space ")" intertoken_space body ")")
               (esrap::and "(" intertoken_space (esrap::or "let" "let*"
                                                           "letrec" "letrec*"
                                                           "let-values" "let*-values")
                           intertoken_space "(" intertoken_space (esrap::* binding_spec)
                           intertoken_space ")")
               ;;
               )
  (:lambda (data)
    (case (fourth data)
      ("cond"
)


