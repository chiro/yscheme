(in-package :yscheme)

(esrap::defrule transformer_spec
    (esrap::or (esrap::and intertoken_space
                           "(" intertoken_space
                           "syntax-rules" intertoken_space
                           "(" intertoken_space
                           (esrap::* identifier)
                           (esrap::* syntax_rule) intertoken_space
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "syntax-rules" intertoken_space
                           identifier intertoken_space
                           "(" intertoken_space
                           (esrap::* identifier) intertoken_space
                           ")" intertoken_space
                           (esrap::* syntax_rule) intertoken_space
                           ")"))
)

(esrap::defrule syntax_rule
    (esrap::and intertoken_space
                "(" intertoken_space
                pattern intertoken_space
                template intertoken_space
                ")"))

;; incomplete
(esrap::defrule pattern
    (esrap::or pattern_identifier
               underscore
               (esrap::and intertoken_space
                           "(" intertoken_space
                           (esrap::* (esrap::and pattern intertoken_space))
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           (esrap::+ (esrap::and pattern intertoken_space))
                           "." intertoken_space
                           pattern intertoken_space
                           ")")
               pattern_datum))
               
(esrap::defrule pattern_datum
    (esrap::and intertoken_space
                (esrap::or scm_string
                           scm_character
                           scm_boolean
                           scm_number)))

(esrap::defrule template
    (esrap::or pattern_identifier
               ; ...
               )
)

(esrap::defrule pattern_identifier
    (esrap::and (esrap::! "...") identifier))

(esrap::defrule underscore
    (esrap::and "_"))
