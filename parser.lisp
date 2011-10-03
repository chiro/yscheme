(IN-PACKAGE :METAPEG)
(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0)))
(DEFUN METAPEG::GENERATED-PARSER ()
  (LET ((METAPEG::*CONTEXT* (MAKE-INSTANCE 'METAPEG::CONTEXT :START-INDEX 0)))
    (FUNCALL (METAPEG::|parse_program|) 0)))
(DEFUN METAPEG::|parse_program| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "program" (METAPEG::|parse_comment|))))
(DEFUN METAPEG::|parse_datum| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "datum"
                                    (METAPEG::SEQ
                                     (METAPEG::|parse_intertoken_space|)
                                     (METAPEG::EITHER
                                      (METAPEG::|parse_simple_datum|)
                                      (METAPEG::|parse_compound_datum|)
                                      (METAPEG::SEQ (METAPEG::|parse_label|)
                                                    (METAPEG::MATCH-STRING "=")
                                                    (METAPEG::|parse_datum|))
                                      (METAPEG::SEQ (METAPEG::|parse_label|)
                                                    (METAPEG::MATCH-STRING
                                                     "#")))
                                     (METAPEG::|parse_intertoken_space|)))))
(DEFUN METAPEG::|parse_simple_datum| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "simple_datum"
                                    (METAPEG::EITHER (METAPEG::|parse_boolean|)
                                                     (METAPEG::|parse_number|)
                                                     (METAPEG::|parse_character|)
                                                     (METAPEG::|parse_string|)
                                                     (METAPEG::|parse_symbol|)
                                                     (METAPEG::|parse_bytevector|)))))
(DEFUN METAPEG::|parse_symbol| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "symbol" (METAPEG::|parse_identifier|))))
(DEFUN METAPEG::|parse_compound_datum| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "compound_datum"
                                    (METAPEG::EITHER (METAPEG::|parse_list|)
                                                     (METAPEG::|parse_vector|)))))
(DEFUN METAPEG::|parse_list| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "list"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "(")
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_datum|))
                                                   (METAPEG::MATCH-STRING ")"))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "(")
                                                   (METAPEG::MANY1
                                                    (METAPEG::|parse_datum|))
                                                   (METAPEG::MATCH-STRING ".")
                                                   (METAPEG::|parse_datum|)
                                                   (METAPEG::MATCH-STRING ")"))
                                     (METAPEG::|parse_abbreviation|)))))
(DEFUN METAPEG::|parse_abbreviation| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "abbreviation"
                                    (METAPEG::SEQ
                                     (METAPEG::|parse_abbrev_prefix|)
                                     (METAPEG::|parse_datum|)))))
(DEFUN METAPEG::|parse_abbrev_prefix| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "abbrev_prefix"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "'")
                                     (METAPEG::MATCH-STRING "`")
                                     (METAPEG::MATCH-STRING ",@")
                                     (METAPEG::MATCH-STRING ",")))))
(DEFUN METAPEG::|parse_vector| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "vector"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "#(")
                                                  (METAPEG::MANY
                                                   (METAPEG::|parse_datum|))
                                                  (METAPEG::MATCH-STRING
                                                   ")")))))
(DEFUN METAPEG::|parse_label| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "label"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "#")
                                                  (METAPEG::MANY1
                                                   (METAPEG::|parse_digit10|))))))
(DEFUN METAPEG::|parse_token| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "token"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_identifier|)
                                      (METAPEG::|parse_delimiter|))
                                     (METAPEG::SEQ (METAPEG::|parse_boolean|)
                                                   (METAPEG::|parse_delimiter|))
                                     (METAPEG::SEQ (METAPEG::|parse_number|)
                                                   (METAPEG::|parse_delimiter|))
                                     (METAPEG::SEQ (METAPEG::|parse_character|)
                                                   (METAPEG::|parse_delimiter|))
                                     (METAPEG::|parse_string|)
                                     (METAPEG::MATCH-STRING "(")
                                     (METAPEG::MATCH-STRING ")")
                                     (METAPEG::MATCH-STRING "#(")
                                     (METAPEG::MATCH-STRING "\\'")
                                     (METAPEG::MATCH-STRING "\\`")
                                     (METAPEG::MATCH-STRING ",@")
                                     (METAPEG::MATCH-STRING ",")
                                     (METAPEG::MATCH-STRING ".")))))
(DEFUN METAPEG::|parse_delimiter| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "delimiter"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_whitespace|)
                                     (METAPEG::MATCH-STRING "(")
                                     (METAPEG::MATCH-STRING ")")
                                     (METAPEG::MATCH-CHAR '(#\"))
                                     (METAPEG::MATCH-STRING ";")))))
(DEFUN METAPEG::|parse_comment| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::MATCH-STRING "\\;")
                                      (METAPEG::MANY
                                       (METAPEG::SEQ
                                        (METAPEG::NEGATE
                                         (METAPEG::|parse_newline|))
                                        (METAPEG::MATCH-ANY-CHAR
                                         'METAPEG::DUMMY))))
                                     (METAPEG::|parse_nested_comment|)))))
(DEFUN METAPEG::|parse_nested_comment| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "nested_comment"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "#|")
                                                  (METAPEG::|parse_comment_text|)
                                                  (METAPEG::MANY
                                                   (METAPEG::|parse_comment_cont|))
                                                  (METAPEG::MATCH-STRING
                                                   "|#")))))
(DEFUN METAPEG::|parse_comment_text| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment_text"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY
                                      (METAPEG::SEQ
                                       (METAPEG::NEGATE
                                        (METAPEG::|parse_comment_text_taboo|))
                                       (METAPEG::MATCH-ANY-CHAR
                                        'METAPEG::DUMMY)))
                                     (LIST 'METAPEG::ACTION NIL
                                           'METAPEG::METAPEG-ACTION199)))))
(DEFUN METAPEG::|parse_comment_text_taboo| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment_text_taboo"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "#|")
                                     (METAPEG::MATCH-STRING "|#")))))
(DEFUN METAPEG::|parse_comment_cont| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "comment_cont"
                                    (METAPEG::SEQ
                                     (METAPEG::|parse_nested_comment|)
                                     (METAPEG::|parse_comment_text|)))))
(DEFUN METAPEG::|parse_atmosphere| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "atmosphere"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_whitespace|)
                                     (METAPEG::|parse_comment|)))))
(DEFUN METAPEG::|parse_intertoken_space| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "intertoken_space"
                                    (METAPEG::MANY
                                     (METAPEG::|parse_atmosphere|)))))
(DEFUN METAPEG::|parse_whitespace| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "whitespace"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_intraline_whitespace|)
                                     (METAPEG::|parse_newline|)
                                     (METAPEG::|parse_return|)))))
(DEFUN METAPEG::|parse_newline| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "newline"
                                    (METAPEG::MATCH-CHAR '(#\Newline)))))
(DEFUN METAPEG::|parse_return| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "return" (METAPEG::MATCH-CHAR '(#\r)))))
(DEFUN METAPEG::|parse_intraline_whitespace| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "intraline_whitespace"
                                    (METAPEG::MATCH-CHAR '(#\  #\Tab)))))
(DEFUN METAPEG::|parse_line_ending| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "line_ending"
                                    (METAPEG::EITHER (METAPEG::|parse_newline|)
                                                     (METAPEG::|parse_return|)))))
(DEFUN METAPEG::|parse_backslash| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "backslash" (METAPEG::MATCH-CHAR '(#\\)))))
(DEFUN METAPEG::|parse_identifier| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "identifier"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_initial|)
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_subsequent|)))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "|")
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_symbol_element|))
                                                   (METAPEG::MATCH-STRING "|"))
                                     (METAPEG::|parse_peculiar_identifier|)))))
(DEFUN METAPEG::|parse_initial| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "initial"
                                    (METAPEG::EITHER (METAPEG::|parse_letter|)
                                                     (METAPEG::|parse_special_initial|)
                                                     (METAPEG::|parse_inline_hex_escape|)))))
(DEFUN METAPEG::|parse_letter| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "letter"
                                    (METAPEG::MATCH-CHAR
                                     '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
                                       #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
                                       #\u #\v #\w #\x #\y #\z #\A #\B #\C #\D
                                       #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N
                                       #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X
                                       #\Y #\Z)))))
(DEFUN METAPEG::|parse_special_initial| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "special_initial"
                                    (METAPEG::MATCH-CHAR
                                     '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\>
                                       #\? #\^ #\_ #\~)))))
(DEFUN METAPEG::|parse_subsequent| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "subsequent"
                                    (METAPEG::EITHER (METAPEG::|parse_initial|)
                                                     (METAPEG::|parse_digit|)
                                                     (METAPEG::|parse_special_subsequent|)))))
(DEFUN METAPEG::|parse_digit| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "digit"
                                    (METAPEG::MATCH-CHAR
                                     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
                                       #\9)))))
(DEFUN METAPEG::|parse_hex_digit| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "hex_digit"
                                    (METAPEG::EITHER (METAPEG::|parse_digit|)
                                                     (METAPEG::MATCH-CHAR
                                                      '(#\a #\b #\c #\d #\e #\f
                                                        #\A #\B #\C #\D #\E
                                                        #\F))))))
(DEFUN METAPEG::|parse_explicit_sign| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "explicit_sign"
                                    (METAPEG::MATCH-CHAR '(#\+ #\-)))))
(DEFUN METAPEG::|parse_special_subsequent| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "special_subsequent"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_explicit_sign|)
                                     (METAPEG::MATCH-STRING ".")
                                     (METAPEG::MATCH-STRING "@")))))
(DEFUN METAPEG::|parse_inline_hex_escape| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "inline_hex_escape"
                                    (METAPEG::SEQ (METAPEG::|parse_backslash|)
                                                  (METAPEG::MATCH-STRING "x")
                                                  (METAPEG::|parse_hex_scalar_value|)
                                                  (METAPEG::MATCH-STRING
                                                   ";")))))
(DEFUN METAPEG::|parse_hex_scalar_value| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "hex_scalar_value"
                                    (METAPEG::MANY1
                                     (METAPEG::|parse_hex_digit|)))))
(DEFUN METAPEG::|parse_symbol_element| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "symbol_element"
                                    (METAPEG::SEQ
                                     (METAPEG::NEGATE
                                      (METAPEG::MATCH-STRING "|"))
                                     (METAPEG::NEGATE
                                      (METAPEG::|parse_backslash|))
                                     (METAPEG::MATCH-ANY-CHAR
                                      'METAPEG::DUMMY)))))
(DEFUN METAPEG::|parse_non-digit| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "non-digit"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_dot_subsequent|)
                                     (METAPEG::|parse_explicit_sign|)))))
(DEFUN METAPEG::|parse_dot_subsequent| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "dot_subsequent"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_sign_subsequent|)
                                     (METAPEG::MATCH-STRING ".")))))
(DEFUN METAPEG::|parse_sign_subsequent| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "sign_subsequent"
                                    (METAPEG::EITHER (METAPEG::|parse_initial|)
                                                     (METAPEG::|parse_explicit_sign|)
                                                     (METAPEG::MATCH-STRING
                                                      "@")))))
(DEFUN METAPEG::|parse_peculiar_identifier| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "peculiar_identifier"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_explicit_sign|)
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_explicit_sign|)
                                      (METAPEG::|parse_sign_subsequent|)
                                      (METAPEG::MANY
                                       (METAPEG::|parse_subsequent|)))
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_explicit_sign|)
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::|parse_dot_subsequent|)
                                      (METAPEG::MANY
                                       (METAPEG::|parse_subsequent|)))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (METAPEG::|parse_non-digit|)
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_subsequent|)))))))
(DEFUN METAPEG::|parse_syntactic_keyword| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "syntactic_keyword"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_expression_keyword|)
                                     (METAPEG::MATCH-STRING "else")
                                     (METAPEG::MATCH-STRING "=>")
                                     (METAPEG::MATCH-STRING "define")
                                     (METAPEG::MATCH-STRING "unquote")
                                     (METAPEG::MATCH-STRING
                                      "unquote-splicing")))))
(DEFUN METAPEG::|parse_expression_keyword| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "expression_keyword"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "quote")
                                     (METAPEG::MATCH-STRING "lambda")
                                     (METAPEG::MATCH-STRING "if")
                                     (METAPEG::MATCH-STRING "set!")
                                     (METAPEG::MATCH-STRING "begin")
                                     (METAPEG::MATCH-STRING "cond")
                                     (METAPEG::MATCH-STRING "and")
                                     (METAPEG::MATCH-STRING "or")
                                     (METAPEG::MATCH-STRING "case")
                                     (METAPEG::MATCH-STRING "let")
                                     (METAPEG::MATCH-STRING "let*")
                                     (METAPEG::MATCH-STRING "letrec")
                                     (METAPEG::MATCH-STRING "do")
                                     (METAPEG::MATCH-STRING "delay")
                                     (METAPEG::MATCH-STRING "quasiquote")))))
(DEFUN METAPEG::|parse_variable| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "variable"
                                    (METAPEG::SEQ
                                     (METAPEG::NEGATE
                                      (METAPEG::|parse_syntactic_keyword|))
                                     (METAPEG::|parse_identifier|)))))
(DEFUN METAPEG::|parse_boolean| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "boolean"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "#t")
                                     (METAPEG::MATCH-STRING "#f")))))
(DEFUN METAPEG::|parse_character| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "character"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::MATCH-STRING "#\\\\")
                                      (METAPEG::EITHER
                                       (METAPEG::|parse_character_name|)
                                       (METAPEG::MATCH-ANY-CHAR
                                        'METAPEG::DUMMY)))
                                     (METAPEG::SEQ
                                      (METAPEG::MATCH-STRING "#\\\\x")
                                      (METAPEG::|parse_hex_scalar_value|))))))
(DEFUN METAPEG::|parse_character_name| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "character_name"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "null")
                                     (METAPEG::MATCH-STRING "alarm")
                                     (METAPEG::MATCH-STRING "backspace")
                                     (METAPEG::MATCH-STRING "tab")
                                     (METAPEG::MATCH-STRING "newline")
                                     (METAPEG::MATCH-STRING "return")
                                     (METAPEG::MATCH-STRING "escape")
                                     (METAPEG::MATCH-STRING "space")
                                     (METAPEG::MATCH-STRING "delete")))))
(DEFUN METAPEG::|parse_string| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "string"
                                    (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\"))
                                                  (METAPEG::MANY
                                                   (METAPEG::|parse_string_element|))
                                                  (METAPEG::MATCH-CHAR
                                                   '(#\"))))))
(DEFUN METAPEG::|parse_string_element| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "string_element"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::NEGATE
                                       (METAPEG::MATCH-CHAR '(#\" #\\)))
                                      (METAPEG::MATCH-ANY-CHAR
                                       'METAPEG::DUMMY))
                                     (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\\))
                                                   (METAPEG::MATCH-CHAR
                                                    '(#\a #\b #\t #\n #\r #\"
                                                      #\\)))
                                     (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\\))
                                                   (METAPEG::|parse_intraline_whitespace|)
                                                   (METAPEG::|parse_line_ending|)
                                                   (METAPEG::|parse_intraline_whitespace|))
                                     (METAPEG::|parse_inline_hex_escape|)))))
(DEFUN METAPEG::|parse_bytevector| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "bytevector"
                                    (METAPEG::SEQ
                                     (METAPEG::MATCH-STRING "#u8(")
                                     (METAPEG::MANY
                                      (METAPEG::|parse_intertoken_space|))
                                     (METAPEG::MANY
                                      (METAPEG::SEQ (METAPEG::|parse_byte|)
                                                    (METAPEG::MANY
                                                     (METAPEG::|parse_intertoken_space|))))
                                     (METAPEG::MATCH-STRING ")")))))
(DEFUN METAPEG::|parse_byte| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "byte"
                                    (METAPEG::MANY1 (METAPEG::|parse_digit|)))))
(DEFUN METAPEG::|parse_number| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "number"
                                    (METAPEG::EITHER (METAPEG::|parse_num2|)
                                                     (METAPEG::|parse_num8|)
                                                     (METAPEG::|parse_num10|)
                                                     (METAPEG::|parse_num16|)))))
(DEFUN METAPEG::|parse_num2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "num2"
                                    (METAPEG::SEQ (METAPEG::|parse_prefix2|)
                                                  (METAPEG::|parse_complex2|)))))
(DEFUN METAPEG::|parse_complex2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "complex2"
                                    (METAPEG::EITHER (METAPEG::|parse_real2|)
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real2|)
                                                      (METAPEG::MATCH-STRING
                                                       "@")
                                                      (METAPEG::|parse_real2|))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real2|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal2|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real2|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal2|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))))))
(DEFUN METAPEG::|parse_real2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "real2"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_sign|)
                                                   (METAPEG::|parse_ureal2|))
                                     (METAPEG::|parse_infinity|)))))
(DEFUN METAPEG::|parse_ureal2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ureal2"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_uinteger2|)
                                     (METAPEG::SEQ (METAPEG::|parse_uinteger2|)
                                                   (METAPEG::MATCH-STRING "/")
                                                   (METAPEG::|parse_uinteger2|))
                                     (METAPEG::|parse_decimal2|)))))
(DEFUN METAPEG::|parse_decimal2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "decimal2"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_uinteger2|)
                                                   (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (METAPEG::MANY1
                                                    (METAPEG::|parse_digit2|))
                                                   (METAPEG::MANY
                                                    (METAPEG::MATCH-STRING
                                                     "#"))
                                                   (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit2|))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY (METAPEG::|parse_digit2|))
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit2|))
                                      (METAPEG::MANY1
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))))))
(DEFUN METAPEG::|parse_uinteger2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "uinteger2"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY1 (METAPEG::|parse_digit2|))
                                     (METAPEG::MANY
                                      (METAPEG::MATCH-STRING "#"))))))
(DEFUN METAPEG::|parse_prefix2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "prefix2"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_radix2|)
                                                   (METAPEG::|parse_exactness|))
                                     (METAPEG::SEQ (METAPEG::|parse_exactness|)
                                                   (METAPEG::|parse_radix2|))))))
(DEFUN METAPEG::|parse_num8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "num8"
                                    (METAPEG::SEQ (METAPEG::|parse_prefix8|)
                                                  (METAPEG::|parse_complex8|)))))
(DEFUN METAPEG::|parse_complex8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "complex8"
                                    (METAPEG::EITHER (METAPEG::|parse_real8|)
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real8|)
                                                      (METAPEG::MATCH-STRING
                                                       "@")
                                                      (METAPEG::|parse_real8|))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real8|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal8|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real8|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal8|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))))))
(DEFUN METAPEG::|parse_real8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "real8"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_sign|)
                                                   (METAPEG::|parse_ureal8|))
                                     (METAPEG::|parse_infinity|)))))
(DEFUN METAPEG::|parse_ureal8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ureal8"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_uinteger8|)
                                     (METAPEG::SEQ (METAPEG::|parse_uinteger8|)
                                                   (METAPEG::MATCH-STRING "/")
                                                   (METAPEG::|parse_uinteger8|))
                                     (METAPEG::|parse_decimal8|)))))
(DEFUN METAPEG::|parse_decimal8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "decimal8"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_uinteger8|)
                                                   (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (METAPEG::MANY1
                                                    (METAPEG::|parse_digit8|))
                                                   (METAPEG::MANY
                                                    (METAPEG::MATCH-STRING
                                                     "#"))
                                                   (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit8|))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY (METAPEG::|parse_digit8|))
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit8|))
                                      (METAPEG::MANY1
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))))))
(DEFUN METAPEG::|parse_uinteger8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "uinteger8"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY1 (METAPEG::|parse_digit8|))
                                     (METAPEG::MANY
                                      (METAPEG::MATCH-STRING "#"))))))
(DEFUN METAPEG::|parse_prefix8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "prefix8"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_radix8|)
                                                   (METAPEG::|parse_exactness|))
                                     (METAPEG::SEQ (METAPEG::|parse_exactness|)
                                                   (METAPEG::|parse_radix8|))))))
(DEFUN METAPEG::|parse_num10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "num10"
                                    (METAPEG::SEQ (METAPEG::|parse_prefix10|)
                                                  (METAPEG::|parse_complex10|)))))
(DEFUN METAPEG::|parse_complex10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "complex10"
                                    (METAPEG::EITHER (METAPEG::|parse_real10|)
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real10|)
                                                      (METAPEG::MATCH-STRING
                                                       "@")
                                                      (METAPEG::|parse_real10|))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real10|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal10|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real10|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal10|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))))))
(DEFUN METAPEG::|parse_real10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "real10"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_sign|)
                                                   (METAPEG::|parse_ureal10|))
                                     (METAPEG::|parse_infinity|)))))
(DEFUN METAPEG::|parse_ureal10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ureal10"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_uinteger10|)
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_uinteger10|)
                                      (METAPEG::MATCH-STRING "/")
                                      (METAPEG::|parse_uinteger10|))
                                     (METAPEG::|parse_decimal10|)))))
(DEFUN METAPEG::|parse_decimal10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "decimal10"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_uinteger10|)
                                      (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (METAPEG::MANY1
                                                    (METAPEG::|parse_digit10|))
                                                   (METAPEG::MANY
                                                    (METAPEG::MATCH-STRING
                                                     "#"))
                                                   (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit10|))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY
                                       (METAPEG::|parse_digit10|))
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit10|))
                                      (METAPEG::MANY1
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))))))
(DEFUN METAPEG::|parse_uinteger10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "uinteger10"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY1
                                      (METAPEG::|parse_digit10|))
                                     (METAPEG::MANY
                                      (METAPEG::MATCH-STRING "#"))))))
(DEFUN METAPEG::|parse_prefix10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "prefix10"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_radix10|)
                                                   (METAPEG::|parse_exactness|))
                                     (METAPEG::SEQ (METAPEG::|parse_exactness|)
                                                   (METAPEG::|parse_radix10|))))))
(DEFUN METAPEG::|parse_num16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "num16"
                                    (METAPEG::SEQ (METAPEG::|parse_prefix16|)
                                                  (METAPEG::|parse_complex16|)))))
(DEFUN METAPEG::|parse_complex16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "complex16"
                                    (METAPEG::EITHER (METAPEG::|parse_real16|)
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real16|)
                                                      (METAPEG::MATCH-STRING
                                                       "@")
                                                      (METAPEG::|parse_real16|))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real16|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal16|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::|parse_real16|)
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::|parse_ureal16|)
                                                      (METAPEG::MATCH-STRING
                                                       "i"))
                                                     (METAPEG::SEQ
                                                      (METAPEG::EITHER
                                                       (METAPEG::MATCH-STRING
                                                        "+")
                                                       (METAPEG::MATCH-STRING
                                                        "-"))
                                                      (METAPEG::MATCH-STRING
                                                       "i"))))))
(DEFUN METAPEG::|parse_real16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "real16"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_sign|)
                                                   (METAPEG::|parse_ureal16|))
                                     (METAPEG::|parse_infinity|)))))
(DEFUN METAPEG::|parse_ureal16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ureal16"
                                    (METAPEG::EITHER
                                     (METAPEG::|parse_uinteger16|)
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_uinteger16|)
                                      (METAPEG::MATCH-STRING "/")
                                      (METAPEG::|parse_uinteger16|))
                                     (METAPEG::|parse_decimal16|)))))
(DEFUN METAPEG::|parse_decimal16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "decimal16"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_uinteger16|)
                                      (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (METAPEG::MANY1
                                                    (METAPEG::|parse_digit16|))
                                                   (METAPEG::MANY
                                                    (METAPEG::MATCH-STRING
                                                     "#"))
                                                   (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit16|))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY
                                       (METAPEG::|parse_digit16|))
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))
                                     (METAPEG::SEQ
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit16|))
                                      (METAPEG::MANY1
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::MATCH-STRING ".")
                                      (METAPEG::MANY
                                       (METAPEG::MATCH-STRING "#"))
                                      (METAPEG::|parse_suffix|))))))
(DEFUN METAPEG::|parse_uinteger16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "uinteger16"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY1
                                      (METAPEG::|parse_digit16|))
                                     (METAPEG::MANY
                                      (METAPEG::MATCH-STRING "#"))))))
(DEFUN METAPEG::|parse_prefix16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "prefix16"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_radix16|)
                                                   (METAPEG::|parse_exactness|))
                                     (METAPEG::SEQ (METAPEG::|parse_exactness|)
                                                   (METAPEG::|parse_radix16|))))))
(DEFUN METAPEG::|parse_infinity| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "infinity"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "+inf.0")
                                     (METAPEG::MATCH-STRING "-inf.0")
                                     (METAPEG::MATCH-STRING "+nan.0")))))
(DEFUN METAPEG::|parse_suffix| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "suffix"
                                    (METAPEG::OPTIONAL
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_exponent_marker|)
                                      (METAPEG::|parse_sign|)
                                      (METAPEG::MANY1
                                       (METAPEG::|parse_digit10|)))))))
(DEFUN METAPEG::|parse_exponent_marker| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "exponent_marker"
                                    (METAPEG::MATCH-CHAR
                                     '(#\e #\s #\f #\d #\l)))))
(DEFUN METAPEG::|parse_sign| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "sign"
                                    (METAPEG::OPTIONAL
                                     (METAPEG::EITHER
                                      (METAPEG::MATCH-STRING "+")
                                      (METAPEG::MATCH-STRING "-"))))))
(DEFUN METAPEG::|parse_exactness| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "exactness"
                                    (METAPEG::OPTIONAL
                                     (METAPEG::EITHER
                                      (METAPEG::MATCH-STRING "#i")
                                      (METAPEG::MATCH-STRING "#e"))))))
(DEFUN METAPEG::|parse_radix2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "radix2" (METAPEG::MATCH-STRING "#b"))))
(DEFUN METAPEG::|parse_radix8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "radix8" (METAPEG::MATCH-STRING "#o"))))
(DEFUN METAPEG::|parse_radix10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "radix10"
                                    (METAPEG::OPTIONAL
                                     (METAPEG::MATCH-STRING "#d")))))
(DEFUN METAPEG::|parse_radix16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "radix16" (METAPEG::MATCH-STRING "#x"))))
(DEFUN METAPEG::|parse_digit2| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "digit2"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "0")
                                     (METAPEG::MATCH-STRING "1")))))
(DEFUN METAPEG::|parse_digit8| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "digit8"
                                    (METAPEG::MATCH-CHAR
                                     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)))))
(DEFUN METAPEG::|parse_digit10| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "digit10" (METAPEG::|parse_digit|))))
(DEFUN METAPEG::|parse_digit16| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "digit16"
                                    (METAPEG::MATCH-CHAR
                                     '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
                                       #\a #\b #\c #\d #\e #\f)))))

 
(defun METAPEG::METAPEG-ACTION199 (data)  (charl-to-str (mapcar #'cadr (first data)))  )