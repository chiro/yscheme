(in-package :yscheme)

;; boolean
(esrap::defrule yscheme::scm_boolean
    (esrap::and intertoken_space
                (esrap::or "#t" "#f")
                (esrap::& delimiter)
                intertoken_space)
  (:destructure (sp b dl)
                (list :boolean b)))


;; character
(esrap::defrule yscheme::scm_character
    (esrap::and intertoken_space
                (esrap::or (esrap::and bslash
                                       (esrap::or character_name character))
                           (esrap::and bslash "x" hex_scalar_value))
                (esrap:& delimiter)
                intertoken_space)
  (:lambda (sp) sp))

(esrap::defrule yscheme::character_name
    (esrap::and "null" "alarm" "backspace" "tab" "newline" "return" "escape" "space" "delete"))


;; string
(defun not-dq-p (char)
  (not (eql #\" char)))

(defun not-dq-or-bs-p (char)
  (and (not-dq-p char)
       (not (eql #\\ char))))

(esrap::defrule yscheme::scm_string
    (esrap::and intertoken_space
                "\""
                (esrap::* scm_string_element)
                "\""
                intertoken_space)
  (:destructure (isp c1 str c2)
                (list :string str)))

(esrap::defrule yscheme::scm_string_element
    (esrap::or inline_hex_escape
               (esrap::and bslash itlws line_ending itlws)
               (esrap::and bslash (esrap::or "a" "b" "t" "n" "r" "\"" bslash))
               (not-dq-or-bs-p character)))

;; bytevector
(esrap::defrule yscheme::scm_bytevector
    (esrap::and "#u8(" intertoken_space
                (esrap::* (esrap::and scm-byte intertoken_space)) ")"))

(defun byte-p (digs)
  (let ((dp (parse-integer (charl-to-str digs) :junk-allowed t)))
    (not (or (null dp) (< dp 0) (> dp 255)))))

(esrap::defrule yscheme::scm-byte
    (byte-p (esrap::+ digit))
  (:lambda (digs) (parse-integer (charl-to-str digs))))

