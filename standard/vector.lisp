;;; 6.3.6. Vectors

(in-package :yscheme)


(define-predicate scm-vector? ((obj scm-vector)) +false+ +true+)


(defgeneric scm-make-vector (obj1 &optional obj2))
(defmethod scm-make-vector
    ((k scm-integer) &optional (fill (new 'scm-number :val 0)))
  (new 'scm-vector
       :val (make-array (val k) :element-type 'vector :initial-element fill)))

(defgeneric scm-vector (&rest objs))
(defmethod scm-vector (&rest objs)
  (new 'scm-vector :val (concatenate 'vector objs)))

(defgeneric scm-vector-length (obj))
(defmethod scm-vector-length ((vec scm-vector))
  (new 'scm-number :val (length (val vec))))

(defgeneric scm-vector-ref (obj1 obj2))
(defmethod scm-vector-ref ((vec scm-vector) (k scm-integer))
  (svref (val vec) (val k)))

(defgeneric scm-vector-set! (obj1 obj2 obj3))
(defmethod scm-vector-set! ((vec scm-vector) (k scm-number) (obj scm-object))
  (setf (svref (val vec) (val k)) obj))

(defgeneric scm-vector->list (obj))
(defmethod scm-vector->list ((vec scm-vector))
  (apply #'scm-list
         (mapcar (lambda (o) o) (concatenate 'list (val vec)))))

(defgeneric scm-list->vector (obj))
(defmethod scm-list->vector ((list scm-list))
  (do ((list list (val-cdr list))
       (result "" (concatenate 'vector result (list (val (val-car list))))))
      ((scm-truep (scm-null? list))
       (new 'scm-vector :val result))))

(defgeneric scm-vector->string (obj))
(defmethod scm-vector->string ((vec scm-vector))
  (new 'scm-string :val (concatenate 'string (val vec))))

(defgeneric scm-string->vector (obj))
(defmethod scm-string->vector ((str scm-string))
  (new 'scm-vector :val (concatenate 'vector (val str))))

(defgeneric scm-vector-copy (obj))
(defmethod scm-vector-copy ((vec scm-vector))
  (new 'scm-vector :val (copy-seq (val vec))))

(defgeneric scm-vector-fill! (obj))
(defmethod scm-vector-fill! ((vec scm-vector))
  (setf (val vec)
        (make-array (length (val vec))
                    :element-type 'vector :initial-element (val vec))))
