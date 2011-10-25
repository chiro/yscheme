(in-package :yscheme)

;; boolean
(esrap::defrule yscheme::scm_boolean
    (esrap::and intertoken_space
                (esrap::or "#t" "#f")
                (esrap::& delimiter)
                intertoken_space)
  (:destructure (sp b dl sp2)
                (if (string= b "#t")
                    +true+
                    +false+)))

;; character
(esrap::defrule yscheme::scm_character
    (esrap::and intertoken_space
                "#"
                (esrap::or
                 (esrap::and bslash hex_escaped_char)
                 (esrap::and bslash character_name)
                 (esrap::and bslash character)
                 )
                (esrap:& delimiter)
                intertoken_space)
  (:destructure (sp1 sharp (bslash ch) del sp2)
                (make-instance 'scm-character :val ch))
)

(esrap::defrule hex_escaped_char
    (esrap::and "x" hex_scalar_value)
  (:destructure (x sv)
                (code-char (parse-integer (cadr sv) :radix 16)))
)

(esrap::defrule yscheme::character_name
    (esrap::or "null" "alarm" "backspace" "tab" "newline" "return" "escape" "space" "delete")
  (:lambda (data)
    (case data
      ("null" #\null)
      ("backspace" #\backspace)
      ("tab" #\tab)
      ("newline" #\newline)
      ("return" #\return)
      ("escape" #\escape)
      ("space" #\space)
      ("delete" #\delete)
      ("alarm" "alarm")))
)


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
  (:destructure (isp c1 str c2 isp2)
                (make-instance 'scm-string :val
                               (reduce (lambda (x y) (concatenate 'string x y))
                                       (mapcar #'string str)
                                       :initial-value "")))
)

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

