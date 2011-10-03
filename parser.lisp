(IN-PACKAGE :METAPEG)
(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0)))
(DEFUN METAPEG::GENERATED-PARSER ()
  (LET ((METAPEG::*CONTEXT* (MAKE-INSTANCE 'METAPEG::CONTEXT :START-INDEX 0)))
    (FUNCALL (METAPEG::|parse_program|) 0)))
(DEFUN |parse_program| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "program"
                                    (METAPEG::SEQ (|parse_comment|)
                                                  (|parse_backslash|)
                                                  (METAPEG::MANY
                                                   (|parse_whitespace|))))))
(DEFUN |parse_comment| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ";")
                                                   (METAPEG::MANY
                                                    (METAPEG::SEQ
                                                     (METAPEG::NEGATE
                                                      (|parse_newline|))
                                                     (METAPEG::MATCH-ANY-CHAR
                                                      'METAPEG::DUMMY))))
                                     (|parse_nested_comment|)))))
(DEFUN |parse_nested_comment| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "nested_comment"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "#|")
                                                  (|parse_comment_text|)
                                                  (METAPEG::MANY
                                                   (|parse_comment_cont|))
                                                  (METAPEG::MATCH-STRING
                                                   "|#")))))
(DEFUN |parse_comment_text| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment_text"
                                    (METAPEG::MANY
                                     (METAPEG::SEQ
                                      (METAPEG::NEGATE
                                       (|parse_comment_text_taboo|))
                                      (METAPEG::MATCH-ANY-CHAR
                                       'METAPEG::DUMMY))))))
(DEFUN |parse_comment_text_taboo| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment_text_taboo"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "#|")
                                     (METAPEG::MATCH-STRING "|#")))))
(DEFUN |parse_comment_cont| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment_cont"
                                    (METAPEG::SEQ (|parse_nested_comment|)
                                                  (|parse_comment_text|)))))
(DEFUN |parse_atmosphere| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "atmosphere"
                                    (METAPEG::EITHER (|parse_whitespace|)
                                                     (|parse_comment|)))))
(DEFUN |parse_intertoken_space| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "intertoken_space"
                                    (METAPEG::MANY (|parse_atmosphere|)))))
(DEFUN |parse_whitespace| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "whitespace"
                                    (METAPEG::EITHER
                                     (|parse_intraline_whitespace|)
                                     (|parse_newline|) (|parse_return|)))))
(DEFUN |parse_newline| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "newline"
                                    (METAPEG::MATCH-CHAR '(#\Newline)))))
(DEFUN |parse_return| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "return" (METAPEG::MATCH-CHAR '(#\r)))))
(DEFUN |parse_intraline_whitespace| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "intraline_whitespace"
                                    (METAPEG::MATCH-CHAR '(#\  #\Tab)))))
(DEFUN |parse_backslash| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "backslash" (METAPEG::MATCH-CHAR '(#\\)))))

 