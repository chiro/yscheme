(in-package :yscheme)

(esrap::defrule expression
    (esrap::or variable
               literal
               procedure_call
               lambda_expression
               conditional
               assignment
               derived_expression
               macro_use
               macro_block))

(esrap::defrule literal
    (esrap::or quotation
               self_evaluating))

(esrap::defrule self_evaluating
    (esrap::or scm_boolean
               scm_number
               scm_character
               scm_string))
