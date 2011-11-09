;;; 6.3.5. Strings

(in-package :yscheme)


(define-predicate string? ((obj scm-string)) +false+ +true+)


(defgeneric scm-make-string (obj1 &optional obj2))
(defmethod scm-make-string
    ((k scm-integer) &optional (char (new 'scm-character :val #\Null)))
  (new 'scm-string
       :val (make-string (val k) :initial-element (val char))))

(defgeneric scm-string (&rest objs))
(defmethod scm-string (&rest chars)
  (new 'scm-string
       :val (concatenate 'string (mapcar #'val chars))))

(defgeneric scm-string-length (obj))
(defmethod scm-string-length ((str scm-string))
  (new 'scm-number
       :val (length (val str))))

(defgeneric scm-string-ref (obj1 obj2))
(defmethod scm-string-ref ((str scm-string) (k scm-integer))
  (new 'scm-character
       :val (char (val str) (val k))))

(defgeneric scm-string-set! (obj1 obj2 obj3))
(defmethod scm-string-set! ((str scm-string) (k scm-integer) (char scm-character))
  (setf (char (val str) (val k)) (val char)))


(define-compare string=? (strs)
  :test (reduce (lambda (f s) (and f (string= (val f) (val s)) s)) strs))

(define-compare string-ci=? (strs)
  :test (reduce (lambda (f s) (and f (string-equal (val f) (val s)) s)) strs))

;(define-compare string-ni=? (strs)
;  :test (reduce (lambda (f s) (and f (string= (val f) (val s)) s)) strs))


(define-compare string<? (strs)
  :test (reduce (lambda (f s) (and f (string< (val f) (val s)) s)) strs))

(define-compare string-ci<? (strs)
  :test (reduce (lambda (f s) (and f (string-lessp (val f) (val s)) s)) strs))

;(define-compare string-ni<? (strs)
;  :test (reduce (lambda (f s) (and f (string- (val f) (val s)) s)) strs))


(define-compare string>? (strs)
  :test (reduce (lambda (f s) (and f (string> (val f) (val s)) s)) strs))

(define-compare string-ci>? (strs)
  :test (reduce (lambda (f s) (and f (string-greaterp (val f) (val s)) s)) strs))

;(define-compare string-ni>? (strs)
;  :test (reduce (lambda (f s) (and f (string- (val f) (val s)) s)) strs))


(define-compare string<=? (strs)
  :test (reduce (lambda (f s) (and f (string<= (val f) (val s)) s)) strs))

(define-compare string-ci<=? (strs)
  :test (reduce (lambda (f s) (and f (string-not-greaterp (val f) (val s)) s)) strs))

;(define-compare string-ni<=? (strs)
;  :test (reduce (lambda (f s) (and f (string- (val f) (val s)) s)) strs))


(define-compare string>=? (strs)
  :test (reduce (lambda (f s) (and f (string>= (val f) (val s)) s)) strs))

(define-compare string-ci>=? (strs)
  :test (reduce (lambda (f s) (and f (string-not-lessp (val f) (val s)) s)) strs))

;(define-compare string-ni>=? (strs)
;  :test (reduce (lambda (f s) (and f (string- (val f) (val s)) s)) strs))


(defgeneric scm-string-upcase (obj))
(defmethod scm-string-upcase ((str scm-string))
  (new 'scm-string
       :val (concatenate 'string
                         (mapcar #'cl-unicode:uppercase-mapping
                                 (concatenate 'list (val str))))))

(defgeneric scm-string-downcase (obj))
(defmethod scm-string-downcase ((str scm-string))
  (new 'scm-string
       :val (concatenate 'string
                         (mapcar #'cl-unicode:lowercase-mapping
                                 (concatenate 'list (val str))))))

(defgeneric string-foldcase (obj))
(defmethod string-foldcase ((str scm-string))
  (new 'scm-string
       :val (concatenate
             'string
             (mapcar
              (lambda (c)
                (cond ((char= (char (cl-unicode:general-category c) 1) #\u)
                       (cl-unicode:lowercase-mapping c))
                      ((char= (char (cl-unicode:general-category c) 1) #\l)
                       (cl-unicode:uppercase-mapping c))))
              (concatenate 'list (val str))))))

(defgeneric substring (obj1 obj2 obj3))
(defmethod substring ((str scm-string) (start scm-integer) (end scm-integer))
  (new 'scm-string :val (subseq (val str) (val start) (val end))))

(defgeneric string-append (&rest objs))
(defmethod string-append (&rest objs)
  (new 'scm-string :val (concatenate 'string (mapcar #'val objs))))

(defgeneric string->list (obj))
(defmethod string->list ((str scm-string))
  (scm-apply #'scm-list
             (mapcar (lambda (c) (new 'scm-character :val c))
                     (concatenate 'list (val str)))))

(defgeneric list->string (obj))
(defmethod list->string ((list scm-pair))
  (do ((list list (val-cdr list))
       (result "" (concatenate 'string result (list (val (val-car list))))))
      ((scm-truep (null? list))
       (new 'scm-string :val result))))

(defgeneric string-copy (obj))
(defmethod string-copy ((str scm-string))
  (new 'scm-string :val (copy-seq (val str))))

(defgeneric string-fill! (obj1 obj2))
(defmethod string-fill! ((str scm-string) (char scm-character))
  (setf (val str)
        (make-string (length (val str)) :initial-element (val char))))
