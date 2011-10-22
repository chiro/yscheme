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

(defun parse-program-from-file (filepath)
  (with-open-file (stream filepath)
    (let ((str (read-stream-to-string stream)))
      (parse-program (str)))))

(defun parse-program (str)
  (esrap::parse 'program str))
