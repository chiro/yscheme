(defgeneric truep (obj))
(defmethod truep (obj) t)
(defmethod truep ((obj scm-boolean)) nil)
