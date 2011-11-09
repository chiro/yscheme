;;; 6.7. Input and Output

(in-package :yscheme)


(defgeneric call-with-input-file (obj1 obj2))
(defmethod call-with-input-file ((str scm-string) (proc procedure))
  (with-open-file (in str :if-does-not-exist nil)
    (scm-apply proc (list (new 'scm-input-port :file str :strm in)))))

(defgeneric call-with-output-file (obj1 obj2))
(defmethod call-with-output-file ((str scm-string) (proc procedure))
  (with-open-file (out str :direction :output :if-exists nil)
    (scm-apply proc (list (new 'scm-output-port :file str :strm out)))))

(defgeneric call-with-port (obj1 obj2))
(defmethod call-with-port ((port scm-port) (proc procedure))
  (let ((result (scm-apply proc (list port))))
    (close (strm port))
    result))

(define-predicate input-port? ((obj scm-port)) +false+
  (if (eql (direction obj) :in) +true+ +false+))
(define-predicate output-port? ((obj scm-port)) +false+
  (if (eql (direction obj) :out) +true+ +false+))

(define-predicate character-port? ((obj scm-character-port)) +false+ +true+)
(define-predicate binary-port? ((obj scm-binary-port)) +false+ +true+)
(define-predicate port? ((obj scm-port)) +false+ +true+)

(define-predicate open-port? ((obj scm-port)) +false+
  (if (open-stream-p (strm obj)) +true+ +false+))

;(defgeneric current-input-port ())

;(defgeneric current-output-port ())

;(defgeneric current-error-port ())

