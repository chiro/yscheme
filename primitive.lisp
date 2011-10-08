

;;; type

(defgeneric boolean? (obj))
(defmethod boolean? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod boolean? ((obj scm-boolean)) (make-instance 'scm-boolean :val t))

(defgeneric pair? (obj))
(defmethod pair? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod pair? ((obj scm-cons)) (make-instance 'scm-boolean :val t))

(defgeneric symbol? (obj))
(defmethod symbol? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod symbol? ((obj scm-symbol)) (make-instance 'scm-boolean :val t))

(defgeneric number? (obj))
(defmethod number? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod number? ((obj scm-number)) (make-instance 'scm-boolean :val t))

(defgeneric char? (obj))
(defmethod char? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod char? ((obj scm-character)) (make-instance 'scm-boolean :val t))

(defgeneric string? (obj))
(defmethod string? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod string? ((obj scm-string)) (make-instance 'scm-boolean :val t))

;(defgeneric blob? (obj))
;(defmethod blob? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
;(defmethod blob? ((obj scm-blob)) (make-instance 'scm-boolean :val t))

;(defgeneric port? (obj))
;(defmethod port? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
;(defmethod port? ((obj scm-port)) (make-instance 'scm-boolean :val t))

(defgeneric procedure? (obj))
(defmethod procedure? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod procedure? ((obj procedure)) (make-instance 'scm-boolean :val t))

(defgeneric null? (obj))
(defmethod null? ((obj scm-object)) (make-instance 'scm-boolean :val nil))
(defmethod null? ((obj scm-nil)) (make-instance 'scm-boolean :val t))



;;; cons-cell, list

(defgeneric scm-cons (obj0 obj1))
(defmethod scm-cons ((obj0 scm-object) (obj1 scm-object))
  (make-instance 'scm-cons :val-car obj0 :val-cdr obj1))

(defgeneric scm-car (obj))
(defmethod scm-car ((obj scm-cons)) (val-car obj))

(defgeneric scm-cdr (obj))
(defmethod scm-car ((obj scm-cons)) (val-cdr obj))

(defgeneric scm-list (&rest objs))
(defmethod scm-list (&rest objs);
  (labels ((rec (objs)
             (if objs
                 (make-instance 'scm-cons
                                :val-car (car objs)
                                :val-cdr (rec (cdr objs)))
                 (make-instance 'scm-nil))))
    (rec objs)))

(defgeneric scm-append (obj0 obj1))
(defmethod scm-append ((obj0 scm-cons) (obj1 scm-cons))
  (labels ((rec (lst0 lst1)
             (if (scm-nullp (val-cdr lst0))
                 lst1
                 (rec (val-cdr lst0) lst1))))
    (if (scm-dotted-list-p obj0)
        (error "invalid list ~A" obj0)
        (rec obj0 obj1))))

(defgeneric scm-reverse (obj))
(defmethod scm-reverse ((obj scm-cons))
  (labels ((iter (rest result)
             (if (scm-pairp (val-cdr rest))
                 (iter (val-cdr rest)
                       (make-instance 'scm-cons
                                      :val-car (val-car rest)
                                      :val-cdr result))
                 result)))
    (iter obj (make-instance 'scm-nil))))



;;; compare

(defgeneric scm-< (num0 num1 &rest more-numbers))
(defmethod scm-< (num0 num1 &rest more-numbers)
  (let ((vals (mapcar #'val (append (list num0 num1) more-numbers))))
    (make-instance 'scm-boolean :val (apply #'< vals))))

(defgeneric scm-> (num0 num1 &rest more-numbers))
(defmethod scm-> (num0 num1 &rest more-numbers)
  (let ((vals (mapcar #'val (append (list num0 num1) more-numbers))))
    (make-instance 'scm-boolean :val (apply #'> vals))))

(defgeneric scm-<= (num0 num1 &rest more-numbers))
(defmethod scm-<= (num0 num1 &rest more-numbers)
  (let ((vals (mapcar #'val (append (list num0 num1) more-numbers))))
    (make-instance 'scm-boolean :val (apply #'<= vals))))

(defgeneric scm->= (num0 num1 &rest more-numbers))
(defmethod scm->= (num0 num1 &rest more-numbers)
  (let ((vals (mapcar #'val (append (list num0 num1) more-numbers))))
    (make-instance 'scm-boolean :val (apply #'>= vals))))



;;; arithmetic
;;; numberは細分化するので試用版

(defgeneric scm-+ (&rest args))
(defmethod scm-+ (&rest args)
  (let ((vals (mapcar #'val args)))
    (make-instance 'scm-number :val (apply #'+ vals))))

(defgeneric scm-- (number &rest more-numbers))
(defmethod scm-- (number &rest more-numbers)
  (let ((vals (mapcar #'val (cons number more-numbers))))
    (make-instance 'scm-number :val (apply #'- vals))))

(defgeneric scm-* (&rest args))
(defmethod scm-* (&rest args)
  (let ((vals (mapcar #'val args)))
    (make-instance 'scm-number :val (apply #'* vals))))

(defgeneric scm-/ (number &rest more-numbers))
(defmethod scm-/ (number &rest more-numbers)
  (let ((vals (mapcar #'val (cons number more-numbers))))
    (make-instance 'scm-number :val (apply #'/ vals))))





;;; i/o

(defgeneric scm-display (obj))
(defmethod scm-display ((obj scm-object))
  (prin1 obj))

(defgeneric scm-newline (obj))
(defmethod scm-newline ((obj scm-object))
  (prin1 #\Newline))


