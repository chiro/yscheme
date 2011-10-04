; env : alist

(defgeneric eval (exp env)
  (:documentation ""))

(defmethod eval ((exp self-evaluating) env)
  (val exp))

(defmethod eval ((exp scm-symbol) env)
  (cdr (assoc (name exp) env)))

(defmethod eval ((exp definition) env)
  )

(defmethod eval ((exp syntax-definition) env)
  )
