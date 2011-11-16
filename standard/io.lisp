;;; 6.7. Input and Output

(in-package :yscheme)


(defgeneric scm-call-with-input-file (obj1 obj2))
(defmethod scm-call-with-input-file ((file scm-string) (proc procedure))
  (with-open-file (in file :if-does-not-exist :error)
    (scm-apply proc (list (new 'scm-input-port :file file :strm in)))))

(defgeneric scm-call-with-output-file (obj1 obj2))
(defmethod scm-call-with-output-file ((file scm-string) (proc procedure))
  (with-open-file (out file :direction :output :if-exists :error)
    (scm-apply proc (list (new 'scm-output-port :file file :strm out)))))

(defgeneric scm-call-with-port (obj1 obj2))
(defmethod scm-call-with-port ((port scm-port) (proc procedure))
  (let ((result (scm-apply proc (list port))))
    (close (strm port))
    result))

(define-predicate scm-input-port? ((obj scm-port)) +false+
  (if (eql (direction obj) :input) +true+ +false+))
(define-predicate scm-output-port? ((obj scm-port)) +false+
  (if (eql (direction obj) :output) +true+ +false+))

(define-predicate scm-character-port? ((obj scm-character-port)) +false+ +true+)
(define-predicate scm-binary-port? ((obj scm-binary-port)) +false+ +true+)
(define-predicate scm-port? ((obj scm-port)) +false+ +true+)

(define-predicate scm-port-open? ((obj scm-port)) +false+
  (if (open-stream-p (strm obj)) +true+ +false+))

;(defgeneric scm-current-input-port ())

;(defgeneric scm-current-output-port ())

;(defgeneric scm-current-error-port ())

(defgeneric scm-with-input-from-file (obj1 obj2))
(defmethod scm-with-input-from-file ((file scm-string) (thunk procedure))
  (with-open-file (in file :if-does-not-exist :error)
    (let ((*current-input-port* (new 'scm-port :file file :direction :input :strm in)))
      (scm-apply thunk nil))))

(defgeneric scm-with-output-to-file (obj1 obj2))
(defmethod scm-with-output-to-file ((file scm-string) (thunk procedure))
  (with-open-file (out file :direction :output :if-exists :error)
    (let ((*current-output-port*
           (new 'scm-port :file file :direction :output :strm out)))
      (scm-apply thunk nil))))

(defgeneric scm-open-input-file (obj))
(defmethod scm-open-input-file ((file scm-string))
  (new 'scm-character-port
       :file file
       :direction :input
       :strm (open file :if-does-not-exist :error)))

(defgeneric scm-open-binary-input-file (obj))
(defmethod scm-open-binary-input-file ((file scm-string))
  (new 'scm-binary-port
       :file file
       :direction :input
       :strm (open file :element-type 'unsigned-byte :if-does-not-exist :error)))

(defgeneric scm-open-output-file (obj))
(defmethod scm-open-output-file ((file scm-string))
  (new 'scm-character-port
       :file file
       :direction :output
       :strm (open file :direction :output :if-does-not-exist :error)))

(defgeneric scm-open-binary-output-file (obj))
(defmethod scm-open-binary-output-file ((file scm-string))
  (new 'scm-binary-port
       :file file
       :direction :output
       :strm (open file :direction :output :element-type 'unsigned-byte
                   :if-does-not-exist :error)))

(defgeneric scm-close-port (obj))
(defmethod scm-close-port ((port scm-port))
  (and (close (strm port)) +true+))

(defgeneric scm-close-input-port (obj))
(defmethod scm-close-input-port ((port scm-port))
  (if (eql (direction port) :input)
      (and (close (strm port)) +true+)
      +false+))

(defgeneric scm-close-output-port (obj))
(defmethod scm-close-output-port ((port scm-port))
  (if (eql (direction port) :output)
      (and (close (strm port)) +true+)
      +false+))

;(defgeneric scm-open-output-string ())

;(defgeneric scm-get-output-string (obj))

;(defgeneric scm-open-output-bytevector ())

;(defgeneric scm-get-output-bytevector (obj))


(defgeneric scm-read (&optional obj))
(defmethod scm-read (&optional (port *current-input-port*))
  (labels ((rec (str)
             (or (aand (possible-datum-p (concatenate 'list str))
                       (parse-program str :end it :junk-allowed t))
                 (rec (mkstr str #\Newline (val (scm-read-line port)))))))
    (rec (val (scm-read-line port)))))

(defgeneric scm-read-char (&optional obj))
(defmethod scm-read-char (&optional (port *current-input-port*))
  (new 'scm-character :val (read-char (strm port) nil +eof+)))

(defgeneric scm-peek-char (&optional obj))
(defmethod scm-peek-char (&optional (port *current-input-port*))
  (new 'scm-character :val (peek-char nil (strm port) nil +eof+)))

(defgeneric scm-read-line (&optional obj))
(defmethod scm-read-line (&optional (port *current-input-port*))
  (new 'scm-string :val (read-line (strm port) nil +eof+)))

(define-predicate scm-eof-object? ((obj scm-eof)) +false+ +true+)

;(define-predicate scm-char-ready? ((obj scm-character-port))

(defgeneric scm-read-u8 (&optional obj))
(defmethod scm-read-u8 (&optional (port *current-input-port*))
  (with-slots (strm buff) port
    (new 'scm-number :val (or (pop buff) (read-byte strm nil +eof+)))))

(defgeneric scm-peek-u8 (&optional obj))
(defmethod scm-peek-u8 (&optional (port *current-input-port*))
  (with-slots (strm buff) port
    (car (push (new 'scm-number :val (read-byte strm nil +eof+))
               buff))))

;(define-predicate scm-u8-ready? ((obj scm-binary-port))

(defun read-bytes-as-possible (len peeked-value stream)
  (when (plusp len)
    (let ((lim (if peeked-value (1- len) len)))
      (do ((i 0 (1+ i))
           (byte (read-byte stream nil +eof+) (read-byte stream nil +eof+))
           (bytes (mklist peeked-value) (append1 bytes byte)))
          ((or (eq byte +eof+) (<= lim i))
           (mapcar (lambda (n) (new 'scm-number :val n))
                   bytes))))))

(defgeneric scm-read-bytevector (obj1 &optional obj2))
(defmethod scm-read-bytevector ((len scm-integer) &optional (port *current-input-port*))
  (with-slots (strm buff) port
    (new 'scm-bytevector
         :val (concatenate
               'vector
               (read-bytes-as-possible (val len) (pop buff) strm)))))

(defgeneric scm-read-bytevector! (obj1 obj2 obj3 &optional obj4))
(defmethod scm-read-bytevector!
    ((bytevec scm-bytevector) (start scm-integer) (end scm-integer)
     &optional (port *current-input-port*))
  (destructuring-bind (start end) (mapcar #'val (list start end))
    (with-slots (strm buff) port
      (do ((i start (1+ i))
           (bytes (read-bytes-as-possible (- end start) (pop buff) strm) (cdr bytes)))
          ((or (<= end i) (null bytes)) bytevec)
        (setf (svref (val bytevec) i) (car bytes))))))


(defun escape-symbol (sym)
  (new 'scm-symbol
       :name (reduce (lambda (f s) (mkstr f (escape-char-to-string s)))
                     (name sym))))

(defun escape-char-to-string (char)
  (if (cl-unicode:has-property char "ASCII")
      (string char)
      (mkstr "\\x" (format nil "~16R" (char-code char)) ";")))


;; write, display ともに現状では止まらない可能性あり(shared structure)

(defgeneric scm-write (obj1 &optional obj2))

(defmethod scm-write ((obj scm-object) &optional (port *current-output-port*))
  (princ "#<undefined>" (strm port)) +undefined+)

(defmethod scm-write ((obj scm-undefined) &optional (port *current-output-port*))
  (princ "#<undefined>" (strm port)) +undefined+)

(defmethod scm-write ((proc primitive-procedure) &optional (port *current-output-port*))
  (format (strm port) "#<primitive-procedure>"))

(defmethod scm-write ((proc compound-procedure) &optional (port *current-output-port*))
  (format (strm port) "#<closure>"))


(defmethod scm-write ((num scm-real) &optional (port *current-output-port*))
  (princ (val num) (strm port)) +undefined+)

(defmethod scm-write ((num scm-complex) &optional (port *current-output-port*))
  (with-slots (val) num
    (let ((d (if (integerp (realpart val)) "~D~@Di" "~F~@Fi")))
      (format (strm port) d (realpart val) (imagpart val)))
    +undefined+))

(defmethod scm-write
    ((num scm-positive-infinity) &optional (port *current-output-port*))
  (princ "+inf.0" (strm port)) +undefined+)

(defmethod scm-write
    ((num scm-negative-infinity) &optional (port *current-output-port*))
  (princ "-inf.0" (strm port)) +undefined+)

(defmethod scm-write ((num scm-nan) &optional (port *current-output-port*))
  (princ "+nan.0" (strm port)) +undefined+)


(defmethod scm-write ((bool scm-boolean) &optional (port *current-output-port*))
  (princ (if (val bool) "#t" "#f") (strm port)) +undefined+)

(defmethod scm-write ((sym scm-symbol) &optional (port *current-output-port*))
  (princ (name (escape-symbol sym)) (strm port)) +undefined+)

(defmethod scm-write ((char scm-character) &optional (port *current-output-port*))
  (format (strm port) "~W" (val char)) +undefined+)

(defmethod scm-write ((str scm-string) &optional (port *current-output-port*))
  (format (strm port) "~W" (val str)) +undefined+)


(defmethod scm-write ((obj scm-object) &optional (port *current-output-port*))
  (write obj :stream (strm port)) +undefined+)

(defmethod scm-write ((obj self-evaluating) &optional (port *current-output-port*))
  (write (val obj) :stream (strm port)) +undefined+)


(defmethod scm-write ((list scm-list) &optional (port *current-output-port*))
  (with-slots (strm) port
    (princ "(" strm)
    (let ((end (gensym)))
      (do ((list list
                 (let ((cdr-val (val-cdr list)))
                   (cond ((scm-truep (scm-null? cdr-val))
                          end)
                         ((scm-truep (scm-pair? cdr-val))
                          (and (princ " " strm) cdr-val))
                         (t
                          (progn (princ " . " strm)
                                 (scm-write cdr-val port)
                                 end))))))
          ((eq list end) (princ ")" strm) +undefined+)
        (scm-write (val-car list) port)))))

(defmethod scm-write ((vec scm-vector) &optional (port *current-output-port*))
  (with-slots (strm) port
    (princ "#(" strm)
    (do ((list (concatenate 'list (val vec))
               (and (cdr list) (princ " " strm) (cdr list))))
        ((null list) (princ ")" strm) +undefined+)
      (scm-write (car list) port))))

(defmethod scm-write ((vec scm-bytevector) &optional (port *current-output-port*))
  (with-slots (strm) port
    (princ "#u8(" strm)
    (do ((list (concatenate 'list (val vec))
               (and (cdr list) (princ " " strm) (cdr list))))
        ((null list) (princ ")" strm) +undefined+)
      (scm-write (car list) port))))


(defgeneric scm-display (obj1 &optional obj2))

(defmethod scm-display ((obj scm-object) &optional (port *current-output-port*))
  (scm-write obj port) +undefined+)

(defmethod scm-display ((char scm-character) &optional (port *current-output-port*))
  (princ (val char) (strm port)) +undefined+)

(defmethod scm-display ((str scm-string) &optional (port *current-output-port*))
  (princ (val str) (strm port)) +undefined+)


(defmethod scm-display ((list scm-list) &optional (port *current-output-port*))
  (with-slots (strm) port
    (princ "(" strm)
    (let ((end (gensym)))
      (do ((list list
                 (let ((cdr-val (val-cdr list)))
                   (cond ((scm-truep (scm-null? cdr-val))
                          end)
                         ((scm-truep (scm-pair? cdr-val))
                          (and (princ " " strm) cdr-val))
                         (t
                          (progn (princ " . " strm)
                                 (scm-display cdr-val port)
                                 end))))))
          ((eq list end) (princ ")" strm) +undefined+)
        (scm-display (val-car list) port)))))

(defmethod scm-display ((vec scm-vector) &optional (port *current-output-port*))
  (with-slots (strm) port
    (princ "#(" strm)
    (do ((list (concatenate 'list (val vec))
               (and (cdr list) (princ " " strm) (cdr list))))
        ((null list) (princ ")" strm) +undefined+)
      (scm-display (car list) port))))

(defmethod scm-display ((vec scm-bytevector) &optional (port *current-output-port*))
  (with-slots (strm) port
    (princ "#u8(" strm)
    (do ((list (concatenate 'list (val vec))
               (and (cdr list) (princ " " strm) (cdr list))))
        ((null list) (princ ")" strm) +undefined+)
      (scm-display (car list) port))))


(defgeneric scm-newline (&optional obj))
(defmethod scm-newline (&optional (port *current-output-port*))
  (princ #\newline (strm port)) +undefined+)

(defgeneric scm-write-char (obj1 &optional obj2))
(defmethod scm-write-char ((char scm-character) &optional (port *current-output-port*))
  (scm-write char port))

(defgeneric scm-write-u8 (obj1 &optional obj2))
(defmethod scm-write-u8 ((byte scm-integer) &optional (port *current-output-port*))
  (write-byte (val byte) (strm port)))

(defgeneric scm-write-bytevector (obj1 &optional obj2))
(defmethod scm-write-bytevector
    ((bytevec scm-bytevector) &optional (port *current-output-port*))
  (dolist (byte (concatenate 'list (val bytevec)))
    (scm-write-u8 (val byte) (strm port))))

(defgeneric scm-write-partial-bytevector (obj1 obj2 obj3 &optional obj4))
(defmethod scm-write-partial-bytevector
    ((bytevec scm-bytevector) (start scm-integer) (end scm-integer)
     &optional (port *current-output-port*))
  (dolist (byte (subseq (concatenate 'list (val bytevec)) (val start) (val end)))
    (scm-write-u8 (val byte) (strm port))))

(defgeneric scm-flush-output-port (&optional port))
(defmethod scm-flush-output-port (&optional (port *current-output-port*))
  (declare (ignorable port))
  +true+)




(defgeneric scm-exit (&optional obj))
(defmethod scm-exit (&optional (obj +true+))
  (throw 'exit obj))