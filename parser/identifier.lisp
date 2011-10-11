(in-package :yscheme)

;; semanticis function
(defun alpha-p (char)
  (or (and (<= (char-code char) 122) (>= (char-code char) 97))
      (and (<= (char-code char) 90) (>= (char-code char) 65))))

(defparameter *special-init* "!$%&*/:<=>?^_~")
(defun special-init-p (char)
  (find char *special-init*))

(defparameter *hex-digit* "0123456789abcdefABCDEF")
(defun hex-digit-p (char)
  (find char *hex-digit*))

;; not (backslash or |)
(defun se-taboo-p (char)
  (not (or (eql #\\ char)
           (eql #\| char))))

;; syntax rule

(esrap:defrule yscheme::identifier
    (esrap::and intertoken_space
                (esrap::or (esrap::and initial (esrap::* subsequent))
                           (esrap::and "|" (esrap::* symbol_element) "|")
                           peculiar_identifier)
                (esrap::& delimiter)
                intertoken_space)
  (:lambda (x) (list :identifier (cadr x)))
  )

(esrap::defrule yscheme::initial
    (esrap::or letter special_initial inline_hex_escape))

(esrap::defrule yscheme::letter
    (alpha-p character))

(esrap::defrule yscheme::special_initial
    (special-init-p character))

(esrap::defrule yscheme::subsequent
    (esrap::or initial digit special_subsequent))

(esrap::defrule yscheme::digit
    (digit-char-p character))

(esrap::defrule yscheme::hex_digit
    (hex-digit-p character))

(esrap::defrule yscheme::explicit_sign
    (esrap::or "+" "-"))

(esrap::defrule yscheme::special_subsequent
    (esrap::or explicit_sign "." "@"))

(esrap::defrule yscheme::inline_hex_escape
    (esrap::and bslash "x" hex_scalar_value ";"))

(esrap::defrule yscheme::hex_scalar_value
    (esrap::+ hex_digit))

(esrap::defrule yscheme::symbol_element
    (sm-taboo-p character))

(esrap::defrule yscheme::non_digit
    (esrap::or dot_subsequent explicit_sign))

(esrap::defrule yscheme::dot_subsequent
    (esrap::or sign_subsequent "."))

(esrap::defrule yscheme::sign_subsequent
    (esrap::or initial explicit_sign "@"))

(esrap::defrule yscheme::peculiar_identifier
    (esrap::or explicit_sign
               (esrap::and explicit_sign sign_subsequent (esrap::* subsequent))
               (esrap::and explicit_sign "." dot_subsequent (esrap::* subsequent))
               (esrap::and "." non_digit (esrap::* subsequent))))

(esrap::defrule yscheme::syntactic_keyword
    (esrap::or expression_keyword "else" "=>" "define" "unquote-splicing" "unquote"))

(esrap::defrule yscheme::expression_keyword
    (esrap::or "quote" "lambda" "if" "set!" "begin" "cond" "and" "or" "case" "letrec"
               "let*" "let" "do" "delay" "quasiquote"))

(esrap::defrule yscheme::variable
    (esrap::and (esrap::not syntactic_keyword) identifier))
