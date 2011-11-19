(in-package :yscheme)

;; TODO write action

(esrap::defrule library
    (esrap::and "(" intertoken_space
                "define-library" intertoken_space
                library_name intertoken_space
                (esrap::* library_declaration)
                ")")
  (:destructure (p1 s1 def s2 name s3 decs p2)
    (make-instance 'library-definition :syms name :exps decs)))

(esrap::defrule library_name
    (esrap::and "(" intertoken_space
                (esrap::+ library_name_part)
                ")" intertoken_space)
  (:destructure (p1 s1 np p2 s2) np))

(esrap::defrule library_name_part
    (esrap::and (esrap::or identifier uinteger10)
                intertoken_space)
  (:destructure (np s1)
    (if (listp np)
        (make-instance 'scm-number :val np)
        np)))

(esrap::defrule library_declaration
    (esrap::and intertoken_space
                (esrap::or l_export
                           l_import
                           l_begin
                           l_include
                           l_include_ci
                           l_cond_expand)
                intertoken_space)
  (:destructure (s1 exp s2) exp))

(esrap::defrule l_export
    (esrap::and "(" intertoken_space
                "export" intertoken_space
                (esrap::* export_spec)
                ")")
  (:destructure (p1 s1 ex s2 spcs p2)
    (make-instance 'library-export :exps spcs)))

(esrap::defrule l_import
    (esrap::and "(" intertoken_space
                "import" intertoken_space
                (esrap::* import_set)
                ")")
  (:destructure (p1 s1 im s2 spcs p2)
    (make-instance 'library-import :exps (remove-if #'null spcs))))

(esrap::defrule l_begin
    (esrap::and "(" intertoken_space
                "begin" intertoken_space
                (esrap::* (esrap::and command_or_definition intertoken_space))
                ")")
  (:destructure (p1 s1 im s2 exps p2)
    (make-instance 'begin :exps (mapcar #'car (remove-if #'null exps)))))

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
                (esrap::* cond_expand_clause)
                (esrap::? cond_expand_else_clause)
                ")")
  (:destructure (p1 s1 ce s2 cls el p2)
    (make-instance 'library-cond-expand
                   :clauses (append cls (and el (list el))))))

(esrap::defrule export_spec
    (esrap::and (esrap::or identifier rename_export)
                intertoken_space)
  (:destructure (spc s1) spc))

(esrap::defrule rename_export
    (esrap::and "(" intertoken_space
                "rename" intertoken_space
                identifier intertoken_space
                identifier intertoken_space
                ")")
  (:destructure (p1 s1 rn s2 id1 s3 id2 s4 p2)
     (make-instance 'rename-pair :from id1 :to id2)))

(esrap::defrule import_set
    (esrap::and (esrap::or library_name
                           is_only
                           is_except
                           is_prefix
                           is_rename)
                intertoken_space)
  (:destructure (is s1)
     (if (listp is)
         (make-instance 'import-library :syms is)
         is)))

(esrap::defrule is_only
    (esrap::and "(" intertoken_space
                "only" intertoken_space
                import_set intertoken_space
                (esrap::+ (esrap::and identifier intertoken_space))
                ")")
  (:destructure (p1 s1 nm s2 st s3 ids p2)
    (make-instance 'import-only
                   :im_set st
                   :syms (mapcar #'car (remove-if #'null ids)))))

(esrap::defrule is_except
    (esrap::and "(" intertoken_space
                "only" intertoken_space
                import_set intertoken_space
                (esrap::+ (esrap::and identifier intertoken_space))
                ")")
  (:destructure (p1 s1 nm s2 st s3 ids p2)
    (make-instance 'import-except
                   :im_set st
                   :syms (mapcar #'car (remove-if #'null ids)))))

(esrap::defrule is_prefix
    (esrap::and "(" intertoken_space
                "prefix" intertoken_space
                import_set intertoken_space
                identifier intertoken_space
                ")")
  (:destructure (p1 s1 nm s2 st s3 id s4 p2)
    (make-instance 'import-prefix :im_set st :sym id)))

(esrap::defrule is_rename
    (esrap::and "(" intertoken_space
                "prefix" intertoken_space
                import_set intertoken_space
                (esrap::+ export_spec)
                ")")
  (:destructure (p1 s1 nm s2 st s3 exps p2)
    (make-instance 'import-rename :im_set st :exps exps)))

(esrap::defrule cond_expand_clause
    (esrap::and "(" intertoken_space
                feature_requirement intertoken_space
                (esrap::* library_declaration)
                ")" intertoken_space)
  (:destructure (p1 s1 fr s2 dcs p2)
    (make-instance 'cond-expand-clause :req fr :exps dcs)))

(esrap::defrule cond_expand_else_clause
    (esrap::and "(" intertoken_space
                "else" intertoken_space
                (esrap::* library_declaration)
                ")" intertoken_space)
  (:destructure (p1 s1 el s2 dcs p2 s3)
    (make-instance 'cond-expand-else-clause :exps dcs)))

(esrap::defrule feature_requirement
    (esrap::and (esrap::or identifier
                           library_name
                           fr_and
                           fr_or
                           fr_not)
                intertoken_space)
  (:lambda (data)
    (typecase data
      (scm-symbol (make-instance 'required-identifier :feature data))
      (list (make-instance 'required-library :syms data))
      (t data))))

(esrap::defrule fr_and
    (esrap::and "(" intertoken_space
                "and" intertoken_space
                (esrap::* feature_requirement)
                ")")
  (:destructure (p1 s1 nm s2 frs o2)
    (make-instance 'and-exp :exps frs)))

(esrap::defrule fr_or
    (esrap::and "(" intertoken_space
                "or" intertoken_space
                (esrap::* feature_requirement)
                ")")
  (:destructure (p1 s1 nm s2 frs o2)
    (make-instance 'or-exp :exps frs)))

(esrap::defrule fr_not
    (esrap::and "(" intertoken_space
                "not" intertoken_space
                feature_requirement intertoken_space
                ")")
  (:destructure (p1 s1 nm s2 fr o2)
    (make-instance 'required-not :exps fr)))