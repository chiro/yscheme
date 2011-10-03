(defclass object ()
  (env :accessor env :initarg :env))


(defclass boolean (object)
  (value :accessor value :initarg :value))

(defclass symbol (object)
  (name :accessor name :initarg :name)
  (value :accessor value :initarg :value))

(defclass number (object)
  (value :accessor value :initarg :value)))

(defclass string (object)
  (string :accessor string :initarg :string))

(defclass character (object)
  (character :accessor character :initarg :character))

(defclass cons (object)
  (car :accessor car :initarg :car)
  (cdr :accessor cdr :initarg :cdr)))