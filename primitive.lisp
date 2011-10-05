(defgeneric boolean? (obj))
(defmethod boolean? (obj) (make-instance 'scm-boolean :val nil))
(defmethod boolean? ((obj scm-boolean)) (make-instance 'scm-boolean :val t))

(defgeneric pair? (obj))
(defmethod pair? (obj) (make-instance 'scm-boolean :val nil))
(defmethod pair? ((obj 'scm-cons)) (make-instance 'scm-boolean :val t))

(defgeneric symbol? (obj))
(defmethod symbol? (obj) (make-instance 'scm-boolean :val nil))
(defmethod symbol? ((obj 'scm-symbol)) (make-instance 'scm-boolean :val t))

(defgeneric number? (obj))
(defmethod number? (obj) (make-instance 'scm-boolean :val nil))
(defmethod number? ((obj 'scm-number)) (make-instance 'scm-boolean :val t))

(defgeneric char? (obj))
(defmethod char? (obj) (make-instance 'scm-boolean :val nil))
(defmethod char? ((obj 'scm-character)) (make-instance 'scm-boolean :val t))

(defgeneric string? (obj))
(defmethod string? (obj) (make-instance 'scm-boolean :val nil))
(defmethod string? ((obj 'scm-string)) (make-instance 'scm-boolean :val t))

;(defgeneric blob? (obj))
;(defmethod blob? (obj) (make-instance 'scm-boolean :val nil))
;(defmethod blob? ((obj 'scm-blob)) (make-instance 'scm-boolean :val t))

;(defgeneric port? (obj))
;(defmethod port? (obj) (make-instance 'scm-boolean :val nil))
;(defmethod port? ((obj 'scm-port)) (make-instance 'scm-boolean :val t))

(defgeneric procedure? (obj))
(defmethod procedure? (obj) (make-instance 'scm-boolean :val nil))
(defmethod procedure? ((obj 'scm-lambda)) (make-instance 'scm-boolean :val t))
;
