;;; 6.3. Other data typp
;;; 6.3.2. Pairs and lists

(in-package :yscheme)


(define-predicate pair? ((obj scm-pair)) +false+ +true+)

(defgeneric scm-cons (obj1 obj2))
(defmethod scm-cons ((obj1 scm-object) (obj2 scm-object))
  (new 'scm-pair :val-car obj1 :val-cdr obj2))

(defgeneric scm-car (obj))
(defmethod scm-car ((obj scm-pair)) (val-car obj))

(defgeneric scm-cdr (obj))
(defmethod scm-car ((obj scm-pair)) (val-cdr obj))

(defgeneric set-car! (obj1 obj2))
(defmethod set-car! ((pair scm-pair) (obj scm-object))
  (setf (val-car pair) obj)
  obj)

(defgeneric set-cdr! (obj1 obj2))
(defmethod set-cdr! ((pair scm-pair) (obj scm-object))
  (setf (val-cdr pair) obj)
  obj)


(define-predicate null? ((obj scm-nil)) +false+ +true+)

(define-predicate list? ((obj scm-pair)) +false+
  (cond ((scm-truep (null? obj)) +true+)
        ((scm-truep (pair? obj)) (list? (cdr obj)))
        (t +false+)))

(defgeneric scm-make-list (obj1 &optional obj2))
(defmethod scm-make-list ((k scm-integer) &optional obj)
  (do ((i (val k) (1- i))
       (result (new 'scheme-nil)
               (new 'scm-pair :val-car obj :val-cdr result)))
      ((>= 0 i) result)))

(defgeneric scm-list (&rest objs))
(defmethod scm-list (&rest objs)
  (do* ((obj nil (pop objs))
        (result (new 'scheme-nil)
                (new 'scm-pair :val-car obj :val-cdr result)))
      ((null objs) result)))

(defgeneric scm-length (obj))
(defmethod scm-length ((list scm-list))
  (do ((i 0 (1+ i))
       (list list (val-cdr list)))
      ((scm-truep (null? list)) (new scm-number :val i :ex t))))

(defgeneric scm-append (obj1 obj2))
(defmethod scm-append ((obj1 scm-list) (obj2 scm-list));
  (labels ((rec (lst1 lst2)
             (if (scm-truep (null? (val-cdr lst1)))
                 lst2
                 (rec (val-cdr lst1) lst2))))
    (rec obj1 obj2)))

(defgeneric scm-reverse (obj))
(defmethod scm-reverse ((list scm-pair))
  (do* ((obj nil (pop list))
        (result (new 'scm-nil)
                (new 'scm-pair :val-car obj :val-cdr result)))
      ((null list) result)))

(defgeneric list-tail (obj1 obj2))
(defmethod list-tail ((list scm-list) (k scm-integer))
  (do* ((i k (1- i))
        (result list (val-cdr list)))
       ((zerop i) result)))

(defgeneric list-ref (obj1 obj2))
(defmethod list-ref ((list scm-list) (k scm-integer))
  (do* ((i k (1- i))
        (result list (val-cdr list)))
       ((zerop i) (val-car result))))

(defgeneric list-set! (obj1 obj2 obj3))
(defmethod list-set! ((list scm-list) (k scm-integer) (obj scm-object))
  (do ((i k (1- i)))
      ((zerop i)
       (setf (val-car list) obj)
       list)))


(defgeneric scm-memq (obj1 obj2))
(defmethod scm-memq ((obj scm-object) (list scm-list))
  (scm-member obj list (new 'primitive-procedure :func #'eq?)))

(defgeneric scm-memv (obj1 obj2))
(defmethod scm-memq ((obj scm-object) (list scm-list))
  (scm-member obj list (new 'primitive-procedure :func #'eqv?)))

(defgeneric scm-member (obj1 obj2 &optional obj3))
(defmethod scm-member ((obj scm-object) (list scm-list)
                   &optional (cmp (new 'primitive-procedure :func #'equal?)))
  (do ((list list (val-cdr list)))
      ((or (scm-truep (scm-apply cmp (list (val-car list) obj)))
           (scm-truep (null? list)))
       (if (scm-truep (null? list))
           +false+
           list))))


(defgeneric scm-assq (obj1 obj2))
(defmethod scm-assq ((obj scm-object) (list scm-list))
  (scm-assoc obj list (new 'primitive-procedure :func #'eq?)))

(defgeneric scm-assv (obj1 obj2))
(defmethod scm-assv ((obj scm-object) (list scm-list))
  (scm-assoc obj list (new 'primitive-procedure :func #'eqv?)))

(defgeneric scm-assoc (obj1 obj2 &optional obj3))
(defmethod scm-assoc ((obj scm-object) (list scm-list)
                      &optional (cmp (new 'primitive-procedure :func #'equal?)))
  (do ((list list (val-cdr list))
       (key-and-value nil (val-car list)))
      ((or (scm-truep (null? list))
           (and (scm-truep (pair? key-and-value))
                (scm-truep (scm-apply cmp (list obj (val-car key-and-value)))))))
       (if (scm-truep (null? list))
           +false+
           key-and-value)))


(defgeneric scm-list-copy (obj))
(defmethod scm-list-copy ((list scm-list))
  (do ((list list (val-cdr list))
       (new-list
        (new 'scm-nil)
        (new 'scm-pair :val-car (val-car list) :val-cdr new-list)))
      ((scm-truep (null? list)) new-list)))

