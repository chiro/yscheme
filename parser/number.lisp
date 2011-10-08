(in-package :yscheme)

;; number

(esrap::defrule scm_number
    (esrap::and (esrap::or num2 num8 num10 num16)
                (esrap::& delimiter))
  (:destructure (num del) (list :number num))
  )

;; binary numbers
(esrap::defrule num2
    (esrap::and prefix2 complex2))

(esrap::defrule complex2
    (esrap::or real2
               (esrap::and real2 "@" real2)
               (esrap::and real2 "+" ureal2 "i")
               (esrap::and real2 "-" ureal2 "i")
               (esrap::and real2 "+i")
               (esrap::and real2 "-i")
               (esrap::and "+" ureal2 "i")
               (esrap::and "-" ureal2 "i")
               "+i"
               "-i"))

(esrap::defrule real2
    (esrap::or (esrap::and sign ureal2)
               infinity))

(esrap::defrule ureal2
    (esrap::or uinteger2
               (esrap::and uinteger2 "/" uinteger2)
               ))

(esrap::defrule uinteger2
    (esrap::and (esrap:+ digit2) (esrap:* "#")))

(esrap::defrule prefix2
    (esrap::or (esrap::and radix2 exactness)
               (esrap::and exactness radix2)))


;; octet numbers
(esrap::defrule num8
    (esrap::and prefix8 complex8))

(esrap::defrule complex8
    (esrap::or real8
               (esrap::and real8 "@" real8)
               (esrap::and real8 "+" ureal8 "i")
               (esrap::and real8 "-" ureal8 "i")
               (esrap::and real8 "+i")
               (esrap::and real8 "-i")
               (esrap::and "+" ureal8 "i")
               (esrap::and "-" ureal8 "i")
               "+i"
               "-i"))

(esrap::defrule real8
    (esrap::or (esrap::and sign ureal8)
               infinity))

(esrap::defrule ureal8
    (esrap::or uinteger8
               (esrap::and uinteger8 "/" uinteger8)
               ))

(esrap::defrule uinteger8
    (esrap::and (esrap:+ digit8) (esrap:* "#")))

(esrap::defrule prefix8
    (esrap::or (esrap::and radix8 exactness)
              (esrap::and exactness radix8)))

;; decimal numbers
(esrap::defrule num10
    (esrap::and prefix10 complex10)
  (:destructure (pre comp)
             (cond ((and pre comp) (list pre comp))
                   (comp comp)
                   (pre pre)
                   (t nil)))
)

(esrap::defrule complex10
    (esrap::or real10
               (esrap::and real10 "@" real10)
               (esrap::and real10 "+" ureal10 "i")
               (esrap::and real10 "-" ureal10 "i")
               (esrap::and real10 "+i")
               (esrap::and real10 "-i")
               (esrap::and "+" ureal10 "i")
               (esrap::and "-" ureal10 "i")
               "+i"
               "-i"))

(esrap::defrule real10
    (esrap::or (esrap::and sign ureal10)
               infinity)
  (:lambda (data)
    (if (and (listp data) (= 2 (length data)))
        (if (null (car data)) (cadr data))
        data))
)

(esrap::defrule ureal10
    (esrap::or uinteger10
               (esrap::and uinteger10 "/" uinteger10)
               decimal10))

(esrap::defrule uinteger10
    (esrap::and (esrap:+ digit10) (esrap:* "#"))
  (:destructure (digs shs)
                (if (null shs)
                    digs
                    (list digs shs))))

(esrap::defrule prefix10
    (esrap::or (esrap::and radix10 exactness)
               (esrap::and exactness radix10))
  (:destructure (p1 p2)
                (cond ((and p1 p2) (list p1 p2))
                      (p1 p1)
                      (p2 p2)
                      (t nil)))
)

(esrap::defrule decimal10
    (esrap::or (esrap::and uinteger10 suffx)
               (esrap::and "." (esrap:+ digit10) (esrap:* "#") suffix)
               (esrap::and (esrap:+ digit10)
                           (esrap:+ "#") "." (esrap:* "#") suffix)
               ))

;; hex numbers
(esrap::defrule num16
    (esrap::and prefix16 complex16))

(esrap::defrule complex16
    (esrap::or real16
               (esrap::and real16 "@" real16)
               (esrap::and real16 "+" ureal16 "i")
               (esrap::and real16 "-" ureal16 "i")
               (esrap::and real16 "+i")
               (esrap::and real16 "-i")
               (esrap::and "+" ureal16 "i")
               (esrap::and "-" ureal16 "i")
               "+i"
               "-i"))

(esrap::defrule real16
    (esrap::or (esrap::and sign ureal16)
               infinity))

(esrap::defrule ureal16
    (esrap::or uinteger16
               (esrap::and uinteger16 "/" uinteger16)
               decimal16))

(esrap::defrule uinteger16
    (esrap::and (esrap::+ digit16) (esrap::* "#")))

(esrap::defrule prefix16
    (esrap::or (esrap::and radix16 exactness)
               (esrap::and exactness radix16)))


;; ==================================================

(esrap:defrule infinity
    (esrap::or "+inf.0" "-inf.0" "+nan.0"))

(esrap:defrule suffix
    (esrap::? (esrap::or (esrap::and exponent_marker sign (esrap::+ digit10)))))

(esrap:defrule exponent_marker
    (esrap::or "e" "s" "f" "d" "l"))

(esrap:defrule sign
    (esrap::? (esrap::or "+" "-")))

(esrap:defrule exactness
    (esrap::? (esrap::or "#i" "#e")))

(esrap:defrule radix2
    (esrap::and "#b"))
(esrap:defrule radix8
    (esrap::and "#o"))
(esrap:defrule radix10
    (esrap::? "#d"))
(esrap:defrule radix16
    (esrap::and "#x"))

(defparameter *digit2* "01")
(defparameter *digit8* "01234567")
(defparameter *digit10* "0123456789")
(defparameter *digit16* "0123456789abcdefABCDEF")

(defun digit2-p (char)
  (find char *digit2*))
(defun digti8-p (char)
  (find char *digit8*))
(defun digit10-p (char)
  (find char *digit10*))
(defun digit16-p (char)
  (find char *digit16*))

(esrap:defrule digit2
    (digit2-p character))
(esrap:defrule digit8
    (digit8-p character))
(esrap:defrule digit10
    (digit10-p character))
(esrap:defrule digit16
    (digit16-p character))



