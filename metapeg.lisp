(IN-PACKAGE :METAPEG)
(DECLAIM (OPTIMIZE (SPEED 3) (SAFETY 0) (DEBUG 0)))
(DEFUN METAPEG::GENERATED-PARSER ()
  (LET ((METAPEG::*CONTEXT* (MAKE-INSTANCE 'METAPEG::CONTEXT :START-INDEX 0)))
    (FUNCALL (METAPEG::|parse_program|) 0)))
(DEFUN METAPEG::|parse_program| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "program"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY
                                      (METAPEG::|parse_ws_or_nl|))
                                     (METAPEG::MANY1 (METAPEG::|parse_rule|))
                                     (LIST 'METAPEG::ACTION NIL
                                           'METAPEG::METAPEG-ACTION69)))))
(DEFUN METAPEG::|parse_rule| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "rule"
                                    (METAPEG::SEQ (METAPEG::|parse_id|)
                                                  (METAPEG::MANY
                                                   (METAPEG::|parse_ws|))
                                                  (METAPEG::MATCH-STRING "<-")
                                                  (METAPEG::MANY
                                                   (METAPEG::|parse_ws|))
                                                  (METAPEG::|parse_ordered-expr-list|)
                                                  (METAPEG::MANY
                                                   (METAPEG::|parse_ws_or_nl|))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        'METAPEG::METAPEG-ACTION70)))))
(DEFUN METAPEG::|parse_ordered-expr-list| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ordered-expr-list"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_expr-list|)
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_ws|))
                                                   (METAPEG::MATCH-STRING "/")
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_ws|))
                                                   (METAPEG::|parse_ordered-expr-list|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION71))
                                     (METAPEG::SEQ (METAPEG::|parse_expr-list|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION72))))))
(DEFUN METAPEG::|parse_expr-list| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "expr-list"
                                    (METAPEG::SEQ (METAPEG::|parse_expr|)
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::MANY1
                                                     (METAPEG::|parse_ws|))
                                                    (METAPEG::|parse_expr-list|)))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        'METAPEG::METAPEG-ACTION73)))))
(DEFUN METAPEG::|parse_expr| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "expr"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_simple-expr|)
                                      (METAPEG::MATCH-STRING "*")
                                      (LIST 'METAPEG::ACTION NIL
                                            'METAPEG::METAPEG-ACTION74))
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_simple-expr|)
                                      (METAPEG::MATCH-STRING "+")
                                      (LIST 'METAPEG::ACTION NIL
                                            'METAPEG::METAPEG-ACTION75))
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_simple-expr|)
                                      (METAPEG::MATCH-STRING "?")
                                      (LIST 'METAPEG::ACTION NIL
                                            'METAPEG::METAPEG-ACTION76))
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_simple-expr|)
                                      (LIST 'METAPEG::ACTION NIL
                                            'METAPEG::METAPEG-ACTION77))))))
(DEFUN METAPEG::|parse_simple-expr| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "simple-expr"
                                    (METAPEG::EITHER
                                     (METAPEG::SEQ (METAPEG::|parse_string|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION78))
                                     (METAPEG::|parse_action|)
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "&")
                                                   (METAPEG::|parse_simple-expr|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION79))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "@")
                                                   (METAPEG::|parse_id|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION80))
                                     (METAPEG::SEQ (METAPEG::|parse_id|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION81))
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_bracketed-rule|)
                                      (LIST 'METAPEG::ACTION NIL
                                            'METAPEG::METAPEG-ACTION82))
                                     (METAPEG::MATCH-STRING "!.")
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "!")
                                                   (METAPEG::|parse_expr|)
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION83))
                                     (METAPEG::SEQ
                                      (METAPEG::|parse_character-class|)
                                      (LIST 'METAPEG::ACTION NIL
                                            'METAPEG::METAPEG-ACTION84))
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING ".")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION85))))))
(DEFUN METAPEG::|parse_bracketed-rule| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "bracketed-rule"
                                    (METAPEG::EITHER
                                     (METAPEG::MATCH-STRING "()")
                                     (METAPEG::SEQ (METAPEG::MATCH-STRING "(")
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_ws|))
                                                   (METAPEG::|parse_ordered-expr-list|)
                                                   (METAPEG::MANY
                                                    (METAPEG::|parse_ws|))
                                                   (METAPEG::MATCH-STRING ")")
                                                   (LIST 'METAPEG::ACTION NIL
                                                         'METAPEG::METAPEG-ACTION86))))))
(DEFUN METAPEG::|parse_id| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "id"
                                    (METAPEG::SEQ
                                     (METAPEG::MANY1
                                      (METAPEG::MATCH-CHAR
                                       '(#\A #\B #\C #\D #\E #\F #\G #\H #\I
                                         #\J #\K #\L #\M #\N #\O #\P #\Q #\R
                                         #\S #\T #\U #\V #\W #\X #\Y #\Z #\a
                                         #\b #\c #\d #\e #\f #\g #\h #\i #\j
                                         #\k #\l #\m #\n #\o #\p #\q #\r #\s
                                         #\t #\u #\v #\w #\x #\y #\z #\- #\_
                                         #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8
                                         #\9)))
                                     (LIST 'METAPEG::ACTION NIL
                                           'METAPEG::METAPEG-ACTION87)))))
(DEFUN METAPEG::|parse_character-class| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "character-class"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "[")
                                                  (METAPEG::MANY1
                                                   (METAPEG::SEQ
                                                    (METAPEG::|parse_not_right_bracket|)
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-STRING "]")
                                                  (LIST 'METAPEG::ACTION NIL
                                                        'METAPEG::METAPEG-ACTION88)))))
(DEFUN METAPEG::|parse_string| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "string"
                                    (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\"))
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::NEGATE
                                                     (METAPEG::MATCH-CHAR
                                                      '(#\")))
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-CHAR '(#\"))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        'METAPEG::METAPEG-ACTION89)))))
(DEFUN METAPEG::|parse_action| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "action"
                                    (METAPEG::SEQ (METAPEG::MATCH-CHAR '(#\{))
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::NEGATE
                                                     (METAPEG::MATCH-CHAR
                                                      '(#\})))
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-CHAR '(#\}))
                                                  (LIST 'METAPEG::ACTION NIL
                                                        'METAPEG::METAPEG-ACTION90)))))
(DEFUN METAPEG::|parse_not_right_bracket| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "not_right_bracket"
                                    (METAPEG::NEGATE
                                     (METAPEG::MATCH-STRING "]")))))
(DEFUN METAPEG::|parse_semi_comment| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "semi_comment"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING ";")
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::NEGATE
                                                     (METAPEG::MATCH-CHAR
                                                      '(#\Newline)))
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))))))
(DEFUN METAPEG::|parse_inline_comment| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "inline_comment"
                                    (METAPEG::SEQ (METAPEG::MATCH-STRING "/*")
                                                  (METAPEG::MANY
                                                   (METAPEG::SEQ
                                                    (METAPEG::NEGATE
                                                     (METAPEG::MATCH-STRING
                                                      "*/"))
                                                    (METAPEG::MATCH-ANY-CHAR
                                                     'METAPEG::DUMMY)))
                                                  (METAPEG::MATCH-STRING
                                                   "*/")))))
(DEFUN METAPEG::|parse_raw_ws| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "raw_ws"
                                    (METAPEG::MATCH-CHAR '(#\  #\Tab)))))
(DEFUN METAPEG::|parse_nl| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "nl" (METAPEG::MATCH-CHAR '(#\Newline)))))
(DEFUN METAPEG::|parse_ws| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ws"
                                    (METAPEG::EITHER (METAPEG::|parse_raw_ws|)
                                                     (METAPEG::|parse_inline_comment|)
                                                     (METAPEG::|parse_semi_comment|)))))
(DEFUN METAPEG::|parse_ws_or_nl| ()
  (LAMBDA (METAPEG::OFFSET)
    (METAPEG::BUILD-PARSER-FUNCTION "ws_or_nl"
                                    (METAPEG::EITHER (METAPEG::|parse_ws|)
                                                     (METAPEG::|parse_nl|)))))

 
(defun METAPEG::METAPEG-ACTION90 (data)  
	(let ((action-name (gen-action-name)))
	 (push (list action-name (char-list-to-string (fix-escapes (zip-second (second data))))) *actions*)
	 `(list 'action nil ',action-name))
 )
(defun METAPEG::METAPEG-ACTION89 (data)  `(match-string ,(char-list-to-string (zip-second (second data))))  )
(defun METAPEG::METAPEG-ACTION88 (data)  `(match-char ',(fix-escapes2 (zip-second (second data))))  )
(defun METAPEG::METAPEG-ACTION87 (data)  (char-list-to-string (first data))  )
(defun METAPEG::METAPEG-ACTION86 (data)  (third data)  )
(defun METAPEG::METAPEG-ACTION85 (data)  (declare (ignore data)) `(match-any-char 'dummy)  )
(defun METAPEG::METAPEG-ACTION84 (data)  (first data)  )
(defun METAPEG::METAPEG-ACTION83 (data)  `(negate ,(second data))  )
(defun METAPEG::METAPEG-ACTION82 (data)  (first data)  )
(defun METAPEG::METAPEG-ACTION81 (data)  
	`(,(make-name (first data)))
 )
(defun METAPEG::METAPEG-ACTION80 (data)  `(match ,(second data))  )
(defun METAPEG::METAPEG-ACTION79 (data)  `(follow ,(second data))  )
(defun METAPEG::METAPEG-ACTION78 (data)  (first data)  )
(defun METAPEG::METAPEG-ACTION77 (data)  (first data)  )
(defun METAPEG::METAPEG-ACTION76 (data)  `(optional ,(first data))  )
(defun METAPEG::METAPEG-ACTION75 (data)  `(many1 ,(first data))  )
(defun METAPEG::METAPEG-ACTION74 (data)  `(many ,(first data))  )
(defun METAPEG::METAPEG-ACTION73 (data)  (if (or (equal (second data) "") (null (second data)))
					     (first data)
					     (let ((tail (second (first (second data)))))
						  (if (equal (first tail) 'seq)
		   			              `(seq ,(first data) ,@(rest tail))
		   			              `(seq ,(first data) ,tail))))  )
(defun METAPEG::METAPEG-ACTION72 (data)  (first data)  )
(defun METAPEG::METAPEG-ACTION71 (data)  
(let ((tail (fifth data)))
	(if (equal (first tail) 'either)
	    `(either ,(first data) ,@(rest tail))
	    `(either ,(first data) ,(fifth data))))
 )
(defun METAPEG::METAPEG-ACTION70 (data)  `(defun ,(make-name (first data)) ()
	 (lambda (offset)
	  (build-parser-function ,(first data) ,(fifth data))))  )
(defun METAPEG::METAPEG-ACTION69 (data) 
`((in-package :metapeg)	
 (declaim (optimize (speed 3) (safety 0) (debug 0)))
  (defun generated-parser ()
	(let ((*context* (make-instance 'context :start-index 0)))
	   (funcall (|parse_program|) 0)))
	,@(second data))
 )