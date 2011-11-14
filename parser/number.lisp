(in-package :yscheme)

;; number

(esrap::defrule scm_number
    (esrap::and (esrap::or num2 num8 num10 num16)
                (esrap::& delimiter))
  (:destructure (num del)
    (make-instance 'scm-number :val (cadr num))))

;; binary numbers
(esrap::defrule num2
    (esrap::and prefix2 complex2)
  (:destructure (pre comp)
    (list :num2 (num-with-exactness (cadr pre) (cadr comp)))))

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
     "-i")
  (:lambda (data)
    (cond ((and (listp data) (eql (car data) :real2))
           (list :complex2 (cadr data)))
          ((and (stringp data) (string= data "+i"))
           (list :complex2 (complex 0 1)))
          ((and (stringp data) (string= data "-i"))
           (list :complex2 (complex 0 -1)))
          ((and (= (length data) 2) (string= (cadr data) "+i"))
           (list :complex2 (complex (cadr (car data)) 1)))
          ((and (= (length data) 2) (string= (cadr data) "-i"))
           (list :complex2 (complex (cadr (car data)) -1)))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "+"))
           (list :complex2 (complex 0 (cadr (cadr data)))))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "-"))
           (list :complex2 (complex 0 (- (cadr (cadr data))))))
          ((and (= (length data) 3) (stringp (cadr data)) (string= (cadr data) "@"))
           (list :complex2 (complex (* (cadr (car data)) (cos (cadr (caddr data))))
                                    (* (cadr (car data)) (sin (cadr (caddr data)))))))
          ((and (= (length data) 4) (string= (cadr data) "+"))
           (list :complex2 (complex (cadr (car data)) (cadr (caddr data)))))
          ((and (= (length data) 4) (string= (cadr data) "-"))
           (list :complex2 (complex (cadr (car data)) (- (cadr (caddr data)))))))))

(esrap::defrule real2
    (esrap::or (esrap::and sign ureal2)
               infinity)
  (:lambda (data)
    (cond ((= (length data) 2)
           (list :real2 (if (string= (cadr (car data)) "-")
                            (- (cadr (cadr data)))
                            (cadr (cadr data)))))
          ((= (length data) 1)
           (list :read2 (cadr data))))))

(esrap::defrule ureal2
    (esrap::or uinteger2
               (esrap::and uinteger2 "/" uinteger2))
  (:lambda (data)
    (cond ((eql (car data) :uinteger2)
           (list :ureal2 (read-from-string (cadr data))))
          (t
           (list :ureal2 (read-from-string (mkstr (cadr (car data))
                                                  (cadr data)
                                                  (cadr (caddr data)))))))))

(esrap::defrule uinteger2
    (esrap::and (esrap:+ digit2) (esrap:* "#"))
  (:destructure (digs shs)
    (list :uinteger2 (cadr digs))))

(esrap::defrule prefix2
    (esrap::or (esrap::and radix2 exactness)
               (esrap::and exactness radix2))
  (:lambda (data)
    (list :prefix2 (cadr (assoc :exactness data)))))

;; decimal numbers
(esrap::defrule num10
    (esrap::and prefix10 complex10)
  (:destructure (pre comp)
    (list :num10 (num-with-exactness (cadr pre) (cadr comp)))))

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
    (cond ((and (listp data) (eql (car data) :real10))
           (list :complex10 (cadr data)))
          ((and (stringp data) (string= data "+i"))
           (list :complex10 (complex 0 1)))
          ((and (stringp data) (string= data "-i"))
           (list :complex10 (complex 0 -1)))
          ((and (= (length data) 2) (string= (cadr data) "+i"))
           (list :complex10 (complex (cadr (car data)) 1)))
          ((and (= (length data) 2) (string= (cadr data) "-i"))
           (list :complex10 (complex (cadr (car data)) -1)))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "+"))
           (list :complex10 (complex 0 (cadr (cadr data)))))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "-"))
           (list :complex10 (complex 0 (- (cadr (cadr data))))))
          ((and (= (length data) 3) (stringp (cadr data)) (string= (cadr data) "@"))
           (list :complex10 (complex (* (cadr (car data)) (cos (cadr (caddr data))))
                                     (* (cadr (car data)) (sin (cadr (caddr data)))))))
          ((and (= (length data) 4) (string= (cadr data) "+"))
           (list :complex10 (complex (cadr (car data)) (cadr (caddr data)))))
          ((and (= (length data) 4) (string= (cadr data) "-"))
           (list :complex10 (complex (cadr (car data)) (- (cadr (caddr data)))))))))

(esrap::defrule real10
    (esrap::or (esrap::and sign ureal10)
               infinity)
  (:lambda (data)
    (cond ((= (length data) 2)
           (list :real10 (if (string= (cadr (car data)) "-")
                             (- (cadr (cadr data)))
                             (cadr (cadr data)))))
          ((= (length data) 1)
           (list :read10 (cadr data))))))

(esrap::defrule ureal10
    (esrap::or (esrap::and uinteger10 "/" uinteger10)
               decimal10
               uinteger10)
  (:lambda (data)
    (cond ((eql (car data) :decimal10)
           (list :ureal10 (cadr data)))
          ((eql (car data) :uinteger10)
           (list :ureal10 (read-from-string (cadr data))))
          (t
           (list :ureal10 (read-from-string (mkstr (cadr (car data))
                                                   (cadr data)
                                                   (cadr (caddr data)))))))))

(esrap::defrule uinteger10
    (esrap::and +digit10 (esrap:* "#"))
  (:destructure (digs shs)
    (list :uinteger10 (cadr digs))))

(esrap::defrule prefix10
    (esrap::or (esrap::and exactness radix10)
               (esrap::and radix10 exactness))
  (:lambda (data)
    (list :prefix10 (cadr (assoc :exactness data)))))

(esrap::defrule decimal10
    (esrap::or (esrap::and "." +digit10 (esrap:* "#") suffix)
               (esrap::and +digit10 "." *digit10 (esrap:* "#") suffix)
               (esrap::and +digit10 (esrap:+ "#") "." (esrap:* "#") suffix)
               (esrap::and uinteger10 suffix))
  (:lambda (data)
    (cond ((and (= (length data) 2) (cadr data))
           (list :decimal10 (read-from-string (mkstr (cadr (car data))
                                                     (cadr (cadr data))))))
          ((= (length data) 2)
           (list :decimal10 (read-from-string (mkstr (cadr (car data))))))
          ((= (length data) 4)
           (list :decimal10 (read-from-string (mkstr (first data)
                                                     (cadr (second data))
                                                     (cadr (fourth data))))))
          ((and (= (length data) 5) (cadr data))
           (list :decimal10 (read-from-string (mkstr (cadr (first data))
                                                     (second data)
                                                     (cadr (third data))
                                                     (cadr (fifth data))))))
          ((and (= (length data) 5) (cadr data))
           (list :decimal10 (read-from-string (mkstr (cadr (first data))
                                                     (third data)
                                                     (cadr (fifth data)))))))))

(esrap::defrule +digit10
    (esrap:+ digit10)
  (:lambda (data) (list :+digit10 (charl-to-str data))))
(esrap::defrule *digit10
    (esrap:* digit10)
  (:lambda (data) (list :*digit10 (charl-to-str data))))

;; octet numbers
(esrap::defrule num8
    (esrap::and prefix8 complex8)
  (:destructure (pre comp)
    (list :num8 (num-with-exactness (cadr pre) (cadr comp)))))

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
     "-i")
  (:lambda (data)
    (cond ((and (listp data) (eql (car data) :real8))
           (list :complex8 (cadr data)))
          ((and (stringp data) (string= data "+i"))
           (list :complex8 (complex 0 1)))
          ((and (stringp data) (string= data "-i"))
           (list :complex8 (complex 0 -1)))
          ((and (= (length data) 2) (string= (cadr data) "+i"))
           (list :complex8 (complex (cadr (car data)) 1)))
          ((and (= (length data) 2) (string= (cadr data) "-i"))
           (list :complex8 (complex (cadr (car data)) -1)))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "+"))
           (list :complex8 (complex 0 (cadr (cadr data)))))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "-"))
           (list :complex8 (complex 0 (- (cadr (cadr data))))))
          ((and (= (length data) 3) (stringp (cadr data)) (string= (cadr data) "@"))
           (list :complex8 (complex (* (cadr (car data)) (cos (cadr (caddr data))))
                                    (* (cadr (car data)) (sin (cadr (caddr data)))))))
          ((and (= (length data) 4) (string= (cadr data) "+"))
           (list :complex8 (complex (cadr (car data)) (cadr (caddr data)))))
          ((and (= (length data) 4) (string= (cadr data) "-"))
           (list :complex8 (complex (cadr (car data)) (- (cadr (caddr data)))))))))

(esrap::defrule real8
    (esrap::or (esrap::and sign ureal8)
               infinity)
  (:lambda (data)
    (cond ((= (length data) 2)
           (list :real8 (if (string= (cadr (car data)) "-")
                            (- (cadr (cadr data)))
                            (cadr (cadr data)))))
          ((= (length data) 1)
           (list :read8 (cadr data))))))

(esrap::defrule ureal8
    (esrap::or uinteger8
               (esrap::and uinteger8 "/" uinteger8))
  (:lambda (data)
    (cond ((eql (car data) :decimal8)
           (list :ureal8 (cadr data)))
          ((eql (car data) :uinteger8)
           (list :ureal8 (read-from-string (cadr data))))
          (t
           (list :ureal8 (read-from-string (mkstr (cadr (car data))
                                                  (cadr data)
                                                  (cadr (caddr data)))))))))

(esrap::defrule uinteger8
    (esrap::and (esrap:+ digit8) (esrap:* "#"))
  (:destructure (digs shs)
    (list :uinteger8 (cadr digs))))

(esrap::defrule prefix8
    (esrap::or (esrap::and radix8 exactness)
               (esrap::and exactness radix8))
  (:lambda (data)
    (list :prefix8 (cadr (assoc :exactness data)))))

;; hex numbers
(esrap::defrule num16
    (esrap::and prefix16 complex16)
  (:destructure (pre comp)
    (list :num16 (num-with-exactness (cadr pre) (cadr comp)))))

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
     "-i")
  (:lambda (data)
    (cond ((and (listp data) (eql (car data) :real16))
           (list :complex16 (cadr data)))
          ((and (stringp data) (string= data "+i"))
           (list :complex16 (complex 0 1)))
          ((and (stringp data) (string= data "-i"))
           (list :complex16 (complex 0 -1)))
          ((and (= (length data) 2) (string= (cadr data) "+i"))
           (list :complex16 (complex (cadr (car data)) 1)))
          ((and (= (length data) 2) (string= (cadr data) "-i"))
           (list :complex16 (complex (cadr (car data)) -1)))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "+"))
           (list :complex16 (complex 0 (cadr (cadr data)))))
          ((and (= (length data) 3) (stringp (car data)) (string= (car data) "-"))
           (list :complex16 (complex 0 (- (cadr (cadr data))))))
          ((and (= (length data) 3) (stringp (cadr data)) (string= (cadr data) "@"))
           (list :complex16 (complex (* (cadr (car data)) (cos (cadr (caddr data))))
                                     (* (cadr (car data)) (sin (cadr (caddr data)))))))
          ((and (= (length data) 4) (string= (cadr data) "+"))
           (list :complex16 (complex (cadr (car data)) (cadr (caddr data)))))
          ((and (= (length data) 4) (string= (cadr data) "-"))
           (list :complex16 (complex (cadr (car data)) (- (cadr (caddr data)))))))))

(esrap::defrule real16
    (esrap::or (esrap::and sign ureal16)
               infinity)
  (:lambda (data)
    (cond ((= (length data) 2)
           (list :real16 (if (string= (cadr (car data)) "-")
                             (- (cadr (cadr data)))
                             (cadr (cadr data)))))
          ((= (length data) 1)
           (list :read16 (cadr data))))))

(esrap::defrule ureal16
    (esrap::or uinteger16
               (esrap::and uinteger16 "/" uinteger16))
  (:lambda (data)
    (cond ((eql (car data) :decimal16)
           (list :ureal16 (cadr data)))
          ((eql (car data) :uinteger16)
           (list :ureal16 (read-from-string (cadr data))))
          (t
           (list :ureal16 (read-from-string (mkstr (cadr (car data))
                                                   (cadr data)
                                                   (cadr (caddr data)))))))))

(esrap::defrule uinteger16
    (esrap::and (esrap:+ digit16) (esrap:* "#"))
  (:destructure (digs shs)
    (list :uinteger16 (cadr digs))))

(esrap::defrule prefix16
    (esrap::or (esrap::and radix16 exactness)
               (esrap::and exactness radix16))
  (:lambda (data)
    (list :prefix16 (cadr (assoc :exactness data)))))

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
    (list :suffix
          (apply #'concatenate 'string
                 (list (cadr (car data)) (cadr (cadr data)) (cadr (caddr data)))))))

(esrap:defrule exponent_marker
    (esrap::or "e" "s" "f" "d" "l")
  (:lambda (data)
    (list :exponent_marker data)))

(esrap:defrule sign
    (esrap::? (esrap::or "+" "-"))
  (:lambda (data)
    (list :sign (if data data "+"))))

(esrap:defrule exactness
    (esrap::? (esrap::or "#i" "#e"))
  (:lambda (data)
    (when data (list :exactness data))))

(esrap:defrule radix2
    (esrap::and "#b"))
(esrap:defrule radix8
    (esrap::and "#o"))
(esrap:defrule radix10
    (esrap::? "#d")
  (:lambda (data) (list data)))
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

