(in-package :yscheme)

;; TODO write action

(esrap::defrule library
    (esrap::and "(" intertoken_space
                "define-library" intertoken_space
                library_name intertoken_space
                (esrap::* (esrap::and library_declaration intertoken_space))
                ")"))

(esrap::defrule library_name
    (esrap::and "(" intertoken_space
                (esrap::+ (esrap::and library_name_part intertoken_space))
                ")"))

(esrap::defrule library_name_part
    (esrap::or identifier
               uinteger10))

(esrap::defrule library_declaration
    (esrap::and l_export
                l_import
                l_begin
                l_include
                l_include_ci
                l_cond_expand))

(esrap::defrule l_export
    (esrap::and "(" intertoken_space
                "export" intertoken_space
                (esrap::* (esrap::and export_spec intertoken_space))
                ")"))

(esrap::defrule l_import
    (esrap::and "(" intertoken_space
                "import" intertoken_space
                (esrap::* (esrap::and import_set intertoken_space))
                ")"))

(esrap::defrule l_begin
    (esrap::and "(" intertoken_space
                "begin" intertoken_space
                (esrap::* (esrap::and command_or_definition intertoken_space))
                ")"))

(esrap::defrule l_include
    (esrap::and "(" intertoken_space
                "include" intertoken_space
                (esrap::+ (esrap::and scm_string intertoken_space))
                ")"))

(esrap::defrule l_include_ci
    (esrap::and "(" intertoken_space
                "include-ci" intertoken_space
                (esrap::+ (esrap::and scm_string intertoken_space))
                ")"))

(esrap::defrule l_cond_expand
    (esrap::and "(" intertoken_space
                "cond-expand" intertoken_space
                (esrap::* (esrap::and cond_expand_clause intertoken_space))
                (esrap::? (esrap::and "(" intertoken_space
                                      "else" intertoken_space
                                      (esrap::* (esrap::and library_declaration intertoken_space))
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
    (esrap::or library_name
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
                (esrap::* (esrap::and library_declaration intertoken_space))
                ")"))

(esrap::defrule feature_requirement
    (esrap::or identifier
               library_name
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
