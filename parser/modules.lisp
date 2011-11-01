(in-package :yscheme)

;; TODO write action

(esrap::defrule module
    (esrap::and "(" intertoken_space
                "module" intertoken_space
                module_name intertoken_space
                (esrap::* (esrap::and module_declaration intertoken_space))
                ")"))

(esrap::defrule module_name
    (esrap::and "(" intertoken_space
                (esrap::+ (esrap::and module_name_part intertoken_space))
                ")"))

(esrap::defrule module_name_part
    (esrap::or identifier
               uinteger10))

(esrap::defrule module_declaration
    (esrap::and m_export
                m_import 
                m_begin
                m_include
                m_include_ci
                m_cond_expand))

(esrap::defrule m_export
    (esrap::and "(" intertoken_space
                "export" intertoken_space
                (esrap::* (esrap::and export_spec intertoken_space))
                ")"))

(esrap::defrule m_import
    (esrap::and "(" intertoken_space
                "import" intertoken_space
                (esrap::* (esrap::and import_set intertoken_space))
                ")"))

(esrap::defrule m_begin
    (esrap::and "(" intertoken_space
                "begin" intertoken_space
                (esrap::* (esrap::and command_or_definition intertoken_space))
                ")"))

(esrap::defrule m_include
    (esrap::and "(" intertoken_space
                "include" intertoken_space
                (esrap::+ (esrap::and scm_string intertoken_space))
                ")"))

(esrap::defrule m_include_ci
    (esrap::and "(" intertoken_space
                "include-ci" intertoken_space
                (esrap::+ (esrap::and scm_string intertoken_space))
                ")"))

(esrap::defrule m_cond_expand
    (esrap::and "(" intertoken_space
                "cond-expand" intertoken_space
                (esrap::* (esrap::and cond_expand_clause intertoken_space))
                (esrap::? (esrap::and "(" intertoken_space
                                      "else" intertoken_space
                                      (esrap::* (esrap::and module_declaration intertoken_space))
                                      ")" intertoken_space))
                ")"))

(esrap::defrule export_spec
    (esrap::or identifier
               rename_export))
(esrap::defrule rename_export
    (esrap::and "(" intertoken_space
                "rename" intertoken_space
                identifier intertoken_space
                identifier intertoken_space
                ")"))

(esrap::defrule import_set
    (esrap::or module_name
               is_only
               is_except
               is_prefix
               is_rename))
(esrap::defrule is_only
    (esrap::and "(" intertoken_space
                "only" intertoken_space
                import_set intertoken_space
                (esrap::+ (esrap::and identifier intertoken_space))
                ")"))
(esrap::defrule is_except
    (esrap::and "(" intertoken_space
                "only" intertoken_space
                import_set intertoken_space
                (esrap::+ (esrap::and identifier intertoken_space))
                ")"))
(esrap::defrule is_prefix
    (esrap::and "(" intertoken_space
                "prefix" intertoken_space
                import_set intertoken_space
                identifier intertoken_space
                ")"))
(esrap::defrule is_rename
    (esrap::and "(" intertoken_space
                "prefix" intertoken_space
                import_set intertoken_space
                (esrap::+ (esrap::and export_spec intertoken_space))
                ")"))

(esrap::defrule cond_expand_clause
    (esrap::and "(" intertoken_space
                feature_requirement intertoken_space
                (esrap::* (esrap::and module_declaration intertoken_space))
                ")"))

(esrap::defrule feature_requirement
    (esrap::or identifier
               module_name
               fr_and
               fr_or
               fr_not))
(esrap::defrule fr_and
    (esrap::and "(" intertoken_space
                "and" intertoken_space
                (esrap::* (esrap::and feature_requirement intertoken_space))
                ")"))
(esrap::defrule fr_or
    (esrap::and "(" intertoken_space
                "or" intertoken_space
                (esrap::* (esrap::and feature_requirement intertoken_space))
                ")"))
(esrap::defrule fr_not
    (esrap::and "(" intertoken_space
                "not" intertoken_space
                feature_requirement intertoken_space
                ")"))
