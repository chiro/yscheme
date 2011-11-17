(in-package :yscheme)

(esrap::defrule program
    (esrap::and intertoken_space
                (esrap::*
                 (esrap::and command_or_definition
                             intertoken_space)))
  (:lambda (data)
    (mapcar #'car (cadr data)))
)

;; TODO modules
(esrap::defrule command_or_definition
    (esrap::or command
               definition
               syntax_definition
               ;; (esrap::and intertoken_space
               ;;             "(" intertoken_space
               ;;             "import" intertoken_space
               ;;             (esrap::+ import_set) intertoken_space
               ;;             ")")
               begin_cod)
)

(esrap::defrule begin_cod
    (esrap::and intertoken_space
                "(" intertoken_space
                "begin" intertoken_space
                (esrap::* command_or_definition)
                intertoken_space
                ")")
)

(esrap::defrule definition
    (esrap::or def_simple
               def_func
               def_record_type
               begin_def)
)

(esrap::defrule def_simple
    (esrap::and intertoken_space
                "(" intertoken_space
                "define" intertoken_space
                scm_variable intertoken_space
                expression intertoken_space
                ")")
  (:destructure (sf p1 s1 def s2 var s3 exp s4 p2)
;                (list :define var exp))
                (make-instance 'variable-definition :sym var :val exp))
)

(esrap::defrule def_func
    (esrap::and intertoken_space
                "(" intertoken_space
                "define" intertoken_space
                "(" intertoken_space
                scm_variable intertoken_space
                def_formals intertoken_space
                ")" intertoken_space
                body intertoken_space
                ")")
  (:destructure (s1 p1 s2 def s3 p2 s4 var s5 fo s6 p3 s7 b s8 p4)
                (make-instance 'function-definition
                               :sym var :parms fo :body b))
)

(esrap::defrule def_record_type
    (esrap::and intertoken_space
                "(" intertoken_space
                "define-record-type" intertoken_space
                scm_variable intertoken_space
                constructor
                scm_variable intertoken_space
                (esrap::* field_spec) intertoken_space
                ")")
)


(esrap::defrule begin_def
    (esrap::and intertoken_space
                "(" intertoken_space
                "begin" intertoken_space
                (esrap::* definition)
                intertoken_space
                ")")
)

(esrap::defrule def_formals
    (esrap::or (esrap::and (esrap::* (esrap::and scm_variable intertoken_space))
                           intertoken_space
                           "." intertoken_space
                           scm_variable)
               (esrap::* (esrap::and scm_variable intertoken_space))
               )
  (:lambda (data)
    (if (and (= (length data) 5) (string= "." (caddr data)))
        (remove-if #'null data)
        (make-instance 'scm-parameters :syms (mapcar #'car data))))
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
