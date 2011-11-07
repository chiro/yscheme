;;; 6.3.7. Bytevectors

(in-package :yscheme)


(define-predicate bytevector? ((obj scm-vector)) +false+ +true+)


(defgeneric scm-make-bytevector (obj1 &optional obj2))
(defmethod scm-make-bytevector
    ((k scm-integer) &optional (byte (new 'scm-number :val 0 :ex t)))
  (new 'scm-bytevecter
       :val (make-array (val k) :element-type 'vector :initial-element byte)))

(defgeneric scm-bytevector-length (obj))
(defmethod scm-bytevector-length ((bytevec scm-bytevector))
  (new 'scm-number :val (length (val bytevec))))

(defgeneric scm-bytevector-u8-ref (obj1 ibj2))
(defmethod scm-bytevector-u8-ref ((bytevec scm-bytevector) (k scm-integer))
  (svref (val bytevec) (val k)))

(defgeneric scm-bytevector-u8-set! (obj1 obj2 obj3))
(defmethod scm-bytevector-u8-set!
    ((bytevec scm-bytevector) (k scm-integer) (byte scm-integer))
  (setf (svref (val bytevec) (val k)) byte))

(defgeneric scm-bytevector-copy (obj))
(defmethod scm-bytevector-copy ((bytevec scm-bytevector))
  (new 'scm-bytevector :val (copy-seq (val bytevec))))

(defgeneric scm-bytevector-copy! (obj1 obj2))
(defmethod scm-bytevector-copy! ((from scm-bytevector) (to scm-bytevector))
  (dotimes (i (length (val from)))
    (setf (svref (val to) i) (svref (val from) i))))

(defgeneric scm-bytevector-copy-partial (obj1 obj2 obj3))
(defmethod scm-bytevector-copy-partial
    ((bytevec scm-bytevector) (start scm-integer) (end scm-integer))
  (new 'scm-bytevector
       :val (subseq (val bytevec) (val start) (val end))))

(defgeneric scm-bytevector-copy-partial! (obj1 obj2 obj3 obj4 obj5))
(defmethod scm-bytevector-copy-partial!
    ((from scm-bytevector) (start scm-integer) (end scm-integer) (to scm-bytevector)
     (at scm-integer))
  (dotimes (i (length (val from)))
    (setf (svref (val to) (+ i (val at))) (svref (val from) i))))
