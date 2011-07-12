(require 'lispbuilder-regex)
(require 'lispbuilder-lexer)

(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

(let* (;; identifier
       (digit               "[0-9]")
       (letter              "[a-zA-Z]")
       (special-initial     "[\!\$\%\&\*\.\/\:\<\=\>\?\@\^\_\~]")
       (explicit-sign       "(\+|\-)")
       (hex-digit           (mkstr "(" digit "|" "[a-fA-F]" ")"))
       (hex-scalar-value    (mkstr hex-digit "\+"))
       (inline-hex-escape   (mkstr "\\\\x" hex-scalar-value "\;"))
       (initial             (mkstr "(" letter "|" special-initial
                                   "|" inline-hex-escape ")"))
       (special-subsequent  (mkstr "(" explicit-sign "|" "\." "|" "\@" ")"))
       (subsequent          (mkstr "(" initial "|" digit "|" special-subsequent ")"))
       (sign-subsequent     (mkstr "(" initial "|" explicit-sign "|" "\@" ")"))
       (dot-subsequent      (mkstr "(" sign-subsequent "|" "\." ")"))
       (non-digit           (mkstr "(" dot-subsequent "|" explicit-sign ")"))
       (peculiar-identifier (mkstr "(" explicit-sign
                                   "|" explicit-sign sign-subsequent subsequent "\*"
                                   "|" explicit-sign "\." dot-subsequent subsequent "\*"
                                   "|" "\." non-digit subsequent "\*" ")"))
       (symbol-element      "[^\\\\\\|]+")
       (identifier          (mkstr "(" initial subsequent "\*"
                                   "|" "\\|" symbol-element "\*" "\\|"
                                   "|" peculiar-identifier ")"))
       (expression-keyword  (mkstr "(" "quote" "|" "lambda" "|" "if" "|" "set!"
                                   "|" "begin" "|" "cond" "|" "and" "|" "or" "|" "case"
                                   "|" "let" "|" "let\*" "|" "letrec" "|" "do"
                                   "|" "delay" "|" "quasiquote" ")"))
       (syntax-keyword      (mkstr "(" expression-keyword "|" "else" "|" "unquote"
                                   "|" "\=\>" "|" "define" "|" "unquote\-splicing" ")"))
       ;; boolean
       (boolean             "\#t|\#f")
       ;; character
       (character-name      (mkstr "(" "null" "|" "backspace" "|" "tab" "|" "newline"
                                   "|" "return" "|" "escape" "|" "space" "|" "alarm"
                                   "|" "delete" ")"))
       (character           (mkstr "(" "\#\\\\" "." "|" "\#\\\\" character-name
                                   "|" "\#\\\\x" hex-scalar-value ")"))
       ;; string
       (string-element      (mkstr "(" "[^\"\\\\]+" "|" "\\\\a" "|" "\\\\b" "|" "\\\\t"
                                   "|" "\\\\n" "|" "\\\\r" "|" "\\\\"
                                   "|" "\\\\\\\\" "|" inline-hex-escape ")"))
       (string              (mkstr "\"" string-element "\*" "\""))
       ;; number
       (digit2              "(0|1)")
       (digit8              "(0|1|2|3|4|5|6|7)")
       (digit10             digit)
       (digit16             (mkstr "(" digit10 "|" "[a-f]" ")"))
       (radix2              "\#b")
       (radix8              "\#o")
       (radix10             "(|\#d)")
       (radix16             "\#x")
       (exactness           "(|\#i|\#e)")
       (sign                "(|\+|\-)")
       (exponent-marker     "(e|s|f|d|l)")
       (suffix              (mkstr "(|" exponent-marker sign digit10 "\+" ")"))
       (infinity            (mkstr "(" "+inf.0" "|" "-inf.0" "|" "+nan.0" ")"))
       (prefix2             (mkstr "(" radix2 exactness "|" exactness radix2 ")"))
       (prefix8             (mkstr "(" radix8 exactness "|" exactness radix8 ")"))
       (prefix10            (mkstr "(" radix10 exactness "|" exactness radix10 ")"))
       (prefix16            (mkstr "(" radix16 exactness "|" exactness radix16 ")"))
       (uinteger2           (mkstr digit2 "\+" "\#\*"))
       (uinteger8           (mkstr digit8 "\+" "\#\*"))
       (uinteger10          (mkstr digit10 "\+" "\#\*"))
       (uinteger16          (mkstr digit16 "\+" "\#\*"))
       (decimal10           (mkstr "(" uinteger10 suffix
                                   "|" "\." digit10 "\+" "\#\*" suffix
                                   "|" digit10 "\+" "\." digit10 "\#\*" suffix
                                   "|" digit10 "\+" "\#\+" "\." "\#\*" suffix ")"))
       (ureal2              (mkstr "(" uinteger2 "|" uinteger2 "\/" uinteger2 ")"))
       (ureal8              (mkstr "(" uinteger8 "|" uinteger8 "\/" uinteger8 ")"))
       (ureal10             (mkstr "(" uinteger10 "|" uinteger10 "\/" uinteger10
                                   "|" decimal10 ")"))
       (ureal16             (mkstr "(" uinteger16 "|" uinteger16 "\/" uinteger16 ")"))
       (real2               (mkstr "(" sign ureal2 "|" infinity ")"))
       (real8               (mkstr "(" sign ureal8 "|" infinity ")"))
       (real10              (mkstr "(" sign ureal10 "|" infinity ")"))
       (real16              (mkstr "(" sign ureal16 "|" infinity ")"))
       (complex2            (mkstr "(" real2 "|" real2 "\@" real2
                                   "|" real2 "\+" real2 "i" "|" real2 "\-" real2 "i"
                                   "|" real2 "\+" "i" "|" real2 "\-" "i" ")"))
       (complex8            (mkstr "(" real8 "|" real8 "\@" real8
                                   "|" real8 "\+" real8 "i" "|" real8 "\-" real8 "i"
                                   "|" real8 "\+" "i" "|" real8 "\-" "i" ")"))
       (complex10           (mkstr "(" real10 "|" real10 "\@" real10
                                   "|" real10 "\+" real10 "i" "|" real10 "\-" real10 "i"
                                   "|" real10 "\+" "i" "|" real10 "\-" "i" ")"))
       (complex16           (mkstr "(" real16 "|" real16 "\@" real16
                                   "|" real16 "\+" real16 "i" "|" real16 "\-" real16 "i"
                                   "|" real16 "\+" "i" "|" real16 "\-" "i" ")"))
       (num2                (mkstr "(" prefix2 complex2 ")"))
       (num8                (mkstr "(" prefix8 complex8 ")"))
       (num10               (mkstr "(" prefix10 complex10 ")"))
       (num16               (mkstr "(" prefix16 complex16 ")"))
       (number              (mkstr "(" num2 "|" num8 "|" num10 "|" num16 ")"))))

(lispbuilder-lexer:deflexer scheme-lexer
  (identifier;
   (return (values :identifier (scm->cl-identifier lispbuilder-lexer:%0))))
  (boolean;
   (return (values :boolean (scm->cl-boolean lispbuilder-lexer:%0))))
  (character;
   (return (values :character (scm->cl-string lispbuilder-lexer:%0))))
  (string;
   (return (values :string (scm->cl-string lispbuilder-lexer:%0))))
  (number;
   (return (values :number (scm->cl-number lispbuilder-lexer:%0))))

  ;; other tokens
  ("\("   (return (values :left-parenthesis lispbuilder-lexer:%0)))
  ("\)"   (return (values :left-parenthesis lispbuilder-lexer:%0)))
  ("\#\(" (return (values :sharp-left-parenthesis lispbuilder-lexer:%0)))
  ("\'"   (return (values :quotation lispbuilder-lexer:%0)))
  ("\`"   (return (values :back-quotation lispbuilder-lexer:%0)))
  ("\,"   (return (values :comma lispbuilder-lexer:%0)))
  ("\,\@" (return (values :comma-atmark lispbuilder-lexer:%0)))
  ("\."   (return (values :colon lispbuilder-lexer:%0)))

  ;; delimiter
  ("[ \(\)\"\;]+")

  ;; comment
  (";.*#\Newline");;;;;;;
  ("#||#");;;;;;;
  ("#;");;;;;;;
  )


(defun scm->cl-identifier (str) )
(defun scm->cl-boolean (str)
  (cond ((string= "#t" str) t)
        ((string= "#f" str) nil)))
(defun scm->cl-string (str)
(defun scm->cl-character (str) )
(defun scm->cl-number (str) )