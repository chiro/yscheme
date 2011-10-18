(in-package :yscheme)

(esrap::defrule program
    (esrap::and intertoken_space
                (esrap::*
                 (esrap::and command_or_definition
                             intertoken_space)))
)

(esrap::defrule command_or_definition
    (esrap::or command
               definition
               syntax_definition
               ;; (esrap::and intertoken_space
               ;;             "(" intertoken_space
               ;;             "import" intertoken_space
               ;;             (esrap::+ import_set) intertoken_space
               ;;             ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "begin" intertoken_space
                           (esrap::+ command_or_definition)
                           intertoken_space
                           ")")
               )
)

(esrap::defrule definition
    (esrap::or (esrap::and intertoken_space
                           "(" intertoken_space
                           "define" intertoken_space
                           scm_variable intertoken_space
                           expression intertoken_space
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "define" intertoken_space
                           "(" intertoken_space
                           scm_variable intertoken_space
                           def_formals intertoken_space
                           ")" intertoken_space
                           body intertoken_space
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "define-record-type"
                           intertoken_space
                           scm_variable intertoken_space
                           constructor
                           scm_variable intertoken_space
                           (esrap::* field_spac) intertoken_space
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "begin" intertoken_space
                           (esrap::* definition)
                           intertoken_space
                           ")")
               )
)

(esrap::defrule def_formals
    (esrap::or (esrap::* (esrap::and scm_variable intertoken_space))
               (esrap::and (esrap::* scm_variable)
                           intertoken_space
                           "." intertoken_space
                           scm_variable)
               )
)

(esrap::defrule constructor
    (esrap::and intertoken_space
                "(" intertoken_space
                scm_variable intertoken_space
                (esrap::* field_name) intertoken_space
                ")")
)

(esrap::defrule field_spec
    (esrap::or (esrap::and intertoken_space
                           "(" intertoken_space
                           field_name intertoken_space
                           scm_variable intertoken_space
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           field_name intertoken_space
                           scm_variable intertoken_space
                           scm_variable intertoken_space
                           ")")
               )
)

(esrap::defrule field_name
    (esrap::and identifier))

(esrap::defrule syntax_definition
    (esrap::or (esrap::and intertoken_space
                           "(" intertoken_space
                           "define-syntax" intertoken_space
                           keyword intertoken_space
                           transformer_spec intertoken_space
                           ")")
               (esrap::and intertoken_space
                           "(" intertoken_space
                           "begin" i ntertoken_space
                           (esrap::* syntax_definition) intertoken_space
                           ")")
               )
)
