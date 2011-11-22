;;; 6.3. Other data typp
;;; 6.3.2. Pairs and lists

(in-package :yscheme)


(define-predicate scm-pair? ((obj scm-pair)) +false+ +true+)

(defgeneric scm-cons (obj1 obj2))
(defmethod scm-cons ((obj1 scm-object) (obj2 scm-object))
  (new 'scm-pair :val-car obj1 :val-cdr obj2))

(defgeneric scm-car (obj))
(defmethod scm-car ((obj scm-pair)) (val-car obj))

(defgeneric scm-cdr (obj))
(defmethod scm-cdr ((obj scm-pair)) (val-cdr obj))

(defgeneric scm-set-car! (obj1 obj2))
(defmethod scm-set-car! ((pair scm-pair) (obj scm-object))
  (setf (val-car pair) obj)
  obj)

(defgeneric scm-set-cdr! (obj1 obj2))
(defmethod scm-set-cdr! ((pair scm-pair) (obj scm-object))
  (setf (val-cdr pair) obj)
  obj)


(define-predicate scm-null? ((obj scm-nil)) +false+ +true+)

(define-predicate scm-list? ((obj scm-list)) +false+
  (cond ((scm-truep (scm-null? obj)) +true+)
        ((scm-truep (scm-pair? obj)) (scm-list? (val-cdr obj)))
        (t +false+)))

(defgeneric scm-make-list (obj1 &optional obj2))
(defmethod scm-make-list ((k scm-integer) &optional obj)
  (do ((i (val k) (1- i))
       (result (new 'scm-nil)
               (new 'scm-pair :val-car obj :val-cdr result)))
      ((>= 0 i) result)))

(defun scm-general-list (init objs)
  (do ((objs (reverse objs) (cdr objs))
       (result init (new 'scm-pair :val-car (car objs) :val-cdr result)))
      ((null objs) result)))

(defgeneric scm-list (&rest objs))
(defmethod scm-list (&rest objs)
  (scm-general-list (new 'scm-nil) objs))

(defgeneric scm-length (obj))
(defmethod scm-length ((list scm-list))
  (do ((i 0 (1+ i))
       (list list (val-cdr list)))
      ((scm-truep (scm-null? list)) (new 'scm-number :val i))))

(defun scm-last1 (list)
  (if (scm-truep (scm-pair? list))
      (scm-last1 (val-cdr list))
      list))

(defgeneric scm-append (&rest objs))
(defmethod scm-append (&rest objs)
  (do ((objs (reverse objs) (cdr objs))
       (result (scm-last1 (last1 objs))
               (do ((list (scm-reverse (car objs)) (val-cdr list))
                    (rslt result (new 'scm-pair :val-car (val-car list) :val-cdr rslt)))
                   ((scm-truep (scm-null? list)) rslt))))
      ((null objs) result)))

(defgeneric scm-reverse (obj))
(defmethod scm-reverse ((list scm-pair))
  (do ((list list (val-cdr list))
       (result (new 'scm-nil)
               (new 'scm-pair :val-car (val-car list) :val-cdr result)))
      ((not (scm-truep (scm-pair? list))) result)))

(defgeneric scm-list-tail (obj1 obj2))
(defmethod scm-list-tail ((list scm-list) (k scm-integer))
  (do* ((i k (1- i))
        (result list (val-cdr list)))
       ((zerop i) result)))

(defgeneric scm-list-ref (obj1 obj2))
(defmethod scm-list-ref ((list scm-list) (k scm-integer))
  (do* ((i k (1- i))
        (result list (val-cdr list)))
       ((zerop i) (val-car result))))

(defgeneric scm-list-set! (obj1 obj2 obj3))
(defmethod scm-list-set! ((list scm-list) (k scm-integer) (obj scm-object))
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
           (scm-truep (scm-null? list)))
       (if (scm-truep (scm-null? list))
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
      ((or (scm-truep (scm-null? list))
           (and (scm-truep (scm-pair? key-and-value))
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
      ((scm-truep (scm-null? list)) new-list)))

