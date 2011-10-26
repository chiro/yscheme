(in-package :yscheme)

;; number

(esrap::defrule scm_number
    (esrap::and (esrap::or num2 num8 num10 num16)
                (esrap::& delimiter))
  (:destructure (num del) num)
  )

;; binary numbers
(esrap::defrule num2
    (esrap::and prefix2 complex2)
  (:destructure (pre comp)
                (cond ((and pre comp) (list :num2 pre comp))
                      (t (list :num2 comp))))
)

(esrap::defrule complex2
    (esrap::or
     (esrap::and "+" ureal2 "i")
     (esrap::and "-" ureal2 "i")
     (esrap::and real2 "@" real2)
     (esrap::and real2 "+" ureal2 "i")
     (esrap::and real2 "-" ureal2 "i")
     (esrap::and real2 "+i")
     (esrap::and real2 "-i")
     real2
     "+i"
     "-i"))


(esrap::defrule real2
    (esrap::or (esrap::and sign ureal2)
               infinity)
  (:lambda (data)
    (if (or (not (listp data)) (car data))
        data
        (cadr data)))
)

(esrap::defrule ureal2
    (esrap::or uinteger2
               (esrap::and uinteger2 "/" uinteger2)
               ))

(esrap::defrule uinteger2
    (esrap::and (esrap:+ digit2) (esrap:* "#"))
  (:destructure (digs shs)
                (remove-if #'null (list :integer (charl-to-str digs) shs))))


(esrap::defrule prefix2
    (esrap::or (esrap::and radix2 exactness)
               (esrap::and exactness radix2))
  (:destructure (p1 p2)
                (cond ((and (null p1) (null p2)) nil)
                      (t (remove-if #'null (list :prefix p1 p2)))))
)


;; decimal numbers
(esrap::defrule num10
    (esrap::and prefix10 complex10)
  (:destructure (pre comp)
             (cond ((and pre comp) (list :num10 pre comp))
                   (t comp)))
)

(esrap::defrule complex10
    (esrap::or 
     (esrap::and "+" ureal10 "i")
     (esrap::and "-" ureal10 "i")
     (esrap::and real10 "@" real10)
     (esrap::and real10 "+" ureal10 "i")
     (esrap::and real10 "-" ureal10 "i")
     (esrap::and real10 "+i")
     (esrap::and real10 "-i")
     real10
     "+i"
     "-i")
  (:lambda (data)
    data))

(esrap::defrule real10
    (esrap::or (esrap::and sign ureal10)
               infinity)
  (:lambda (data)
    (if (listp data)
        (if (string= (car data) "-")
            (list :minus (cadr data))
            (cadr data))
        data))
)


(esrap::defrule ureal10
    (esrap::or (esrap::and uinteger10 "/" uinteger10)
               decimal10
               uinteger10))
               

(esrap::defrule uinteger10
    (esrap::and +digit10 (esrap:* "#"))
  (:destructure (digs shs)
                (make-instance 'scm-integer :val (parse-integer digs)))) ;; temporary
                ;; (if (null shs)
                ;;     (list :integer digs)
                ;;     (list :integer digs shs))))

(esrap::defrule prefix10
    (esrap::or (esrap::and radix10 exactness)
               (esrap::and exactness radix10))
  (:destructure (p1 p2)
                (cond ((and p1 p2) (list :prefix p1 p2))
                      (p1 (list :prefix p1))
                      (p2 (list :prefix p2))
                      (t nil)))
)

(esrap::defrule decimal10
    (esrap::or (esrap::and "." +digit10 (esrap:* "#") suffix)
               (esrap::and +digit10 "." *digit10 (esrap:* "#") suffix)
               (esrap::and +digit10
                           (esrap:+ "#") "." (esrap:* "#") suffix)
               (esrap::and uinteger10 suffix)
               )
  (:lambda (data)
    (cond ((= (length data) 2)
           (if (cadr data)
             (cons :decimal data)
             (car data)))
          (t (list :decimal (remove-if #'null data)))))
)

(esrap::defrule +digit10
    (esrap:+ digit10)
  (:lambda (data) (charl-to-str data)))
(esrap::defrule *digit10
    (esrap:* digit10)
  (:lambda (data) (charl-to-str data)))

;; octet numbers
(esrap::defrule num8
    (esrap::and prefix8 complex8)
  (:destructure (pre comp)
                (cond ((and pre comp) (list :num8 pre comp))
                      (t (list :num8 comp))))
)

(esrap::defrule complex8
    (esrap::or
     (esrap::and "+" ureal8 "i")
     (esrap::and "-" ureal8 "i")
     (esrap::and real8 "@" real8)
     (esrap::and real8 "+" ureal8 "i")
     (esrap::and real8 "-" ureal8 "i")
     (esrap::and real8 "+i")
     (esrap::and real8 "-i")
     real8
     "+i"
     "-i"))


(esrap::defrule real8
    (esrap::or (esrap::and sign ureal8)
               infinity)
  (:lambda (data)
    (if (or (not (listp data)) (car data))
        data
        (cadr data)))
)

(esrap::defrule ureal8
    (esrap::or uinteger8
               (esrap::and uinteger8 "/" uinteger8)
               ))

(esrap::defrule uinteger8
    (esrap::and (esrap:+ digit8) (esrap:* "#"))
  (:destructure (digs shs)
                (remove-if #'null (list :integer (charl-to-str digs) shs))))


(esrap::defrule prefix8
    (esrap::or (esrap::and radix8 exactness)
               (esrap::and exactness radix8))
  (:destructure (p1 p8)
                (cond ((and (null p1) (null p8)) nil)
                      (t (remove-if #'null (list :prefix p1 p8)))))
)

;; hex numbers
(esrap::defrule num16
    (esrap::and prefix16 complex16)
  (:destructure (pre comp)
                (cond ((and pre comp) (list :num16 pre comp))
                      (t (list :num16 comp))))
)

(esrap::defrule complex16
    (esrap::or
     (esrap::and "+" ureal16 "i")
     (esrap::and "-" ureal16 "i")
     (esrap::and real16 "@" real16)
     (esrap::and real16 "+" ureal16 "i")
     (esrap::and real16 "-" ureal16 "i")
     (esrap::and real16 "+i")
     (esrap::and real16 "-i")
     real16
     "+i"
     "-i"))


(esrap::defrule real16
    (esrap::or (esrap::and sign ureal16)
               infinity)
  (:lambda (data)
    (if (or (not (listp data)) (car data))
        data
        (cadr data)))
)

(esrap::defrule ureal16
    (esrap::or uinteger16
               (esrap::and uinteger16 "/" uinteger16)
               ))

(esrap::defrule uinteger16
    (esrap::and (esrap:+ digit16) (esrap:* "#"))
  (:destructure (digs shs)
                (remove-if #'null (list :integer (charl-to-str digs) shs))))


(esrap::defrule prefix16
    (esrap::or (esrap::and radix16 exactness)
               (esrap::and exactness radix16))
  (:destructure (p1 p8)
                (cond ((and (null p1) (null p8)) nil)
                      (t (remove-if #'null (list :prefix p1 p8)))))
)


;; ==================================================

(esrap:defrule infinity
    (esrap::or "+inf.0" "-inf.0" "+nan.0")
  (:lambda (inf)
    (case inf
      ("+inf.0" (make-instance 'scm-positive-infinity))
      ("-inf.0" (make-instance 'scm-negative-infinity))
      ("+nan.0" (make-instance 'scm-nan)))))

(esrap:defrule suffix
    (esrap::? (esrap::and exponent_marker sign +digit10))
  (:lambda (data)
            (when data
              (remove-if #'null (list :suffix data))))
)

(esrap:defrule exponent_marker
    (esrap::or "e" "s" "f" "d" "l"))

(esrap:defrule sign
    (esrap::? (esrap::or "+" "-"))
)

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
(defun digit8-p (char)
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

