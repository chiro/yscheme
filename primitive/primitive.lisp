




;;; 3.2. type

(defgeneric boolean? (obj))
(defmethod boolean? ((obj scm-object)) +false+)
(defmethod boolean? ((obj scm-boolean)) +true+)

(defgeneric pair? (obj))
(defmethod pair? ((obj scm-object)) +false+)
(defmethod pair? ((obj scm-pair)) +true+)

(defgeneric symbol? (obj))
(defmethod symbol? ((obj scm-object)) +false+)
(defmethod symbol? ((obj scm-symbol)) +true+)

(defgeneric char? (obj))
(defmethod char? ((obj scm-object)) +false+)
(defmethod char? ((obj scm-character)) +true+)

(defgeneric string? (obj))
(defmethod string? ((obj scm-object)) +false+)
(defmethod string? ((obj scm-string)) +true+)

;(defgeneric blob? (obj))
;(defmethod blob? ((obj scm-object)) +false+)
;(defmethod blob? ((obj scm-blob)) +true+)

;(defgeneric port? (obj))
;(defmethod port? ((obj scm-object)) +false+)
;(defmethod port? ((obj scm-port)) +true+)

(defgeneric procedure? (obj))
(defmethod procedure? ((obj scm-object)) +false+)
(defmethod procedure? ((obj procedure)) +true+)

(defgeneric null? (obj))
(defmethod null? ((obj scm-object)) +false+)
(defmethod null? ((obj scm-nil)) +true+)



;;; cons-cell, list

(defgeneric scm-cons (obj0 obj1))
(defmethod scm-cons ((obj0 scm-object) (obj1 scm-object))
  (make-instance 'scm-pair :val-car obj0 :val-cdr obj1))

(defgeneric scm-car (obj))
(defmethod scm-car ((obj scm-pair)) (val-car obj))

(defgeneric scm-cdr (obj))
(defmethod scm-car ((obj scm-pair)) (val-cdr obj))

(defgeneric scm-list (&rest objs))
(defmethod scm-list (&rest objs);
  (labels ((rec (objs)
             (if objs
                 (make-instance 'scm-pair
                                :val-car (car objs)
                                :val-cdr (rec (cdr objs)))
                 (make-instance 'scm-nil))))
    (rec objs)))

(defgeneric scm-append (obj0 obj1))
(defmethod scm-append ((obj0 scm-pair) (obj1 scm-pair))
  (labels ((rec (lst0 lst1)
             (if (scm-nullp (val-cdr lst0))
                 lst1
                 (rec (val-cdr lst0) lst1))))
    (if (scm-dotted-list-p obj0)
        (error "invalid list ~A" obj0)
        (rec obj0 obj1))))

(defgeneric scm-reverse (obj))
(defmethod scm-reverse ((obj scm-pair))
  (labels ((iter (rest result)
             (if (scm-pairp (val-cdr rest))
                 (iter (val-cdr rest)
                       (make-instance 'scm-pair
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


