(in-package :yscheme)

(defun read-stream-to-string (in)
  (with-output-to-string (out)
    (let ((eof (gensym)))
      (do ((line (read-line in nil eof)
                 (read-line in nil eof)))
          ((eq line eof))
        (format out "~A~%" line)))))

(defun parse-from-file (filepath comp)
  (with-open-file (stream filepath)
    (let ((str (read-stream-to-string stream)))
      (esrap::parse comp str))))

(esrap::defrule yscheme::program
    (esrap::and intertoken_space
               (esrap::* (esrap::and datum intertoken_space)))
  (:destructure (sp ast)
                (mapcar #'car ast))
)

(defun parse-program-from-file (filepath)
  (parse-from-file filepath 'yscheme::program))
