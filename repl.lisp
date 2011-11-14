(in-package :yscheme)


(defun read-eval-print-loop ()
  (labels ((rec ()
             (setf *eval-count* 0)
             (format t "> ")
             (let ((input (scm-read)))
               (multiple-value-bind (value envp)
                   (scm-eval input *interaction-environment*)
                 (if envp
                     (format t "#<environment>")
                     (scm-display value))
                 (princ #\Newline)
                 (rec)))))
    (format t "YSCHEME たいけんばん~%~%")
    (if (scm-truep (catch 'exit (rec)))
        (sb-ext:quit)
        (sb-ext:quit :unix-status 1))))