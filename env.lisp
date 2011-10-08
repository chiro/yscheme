;;; environment

(defparameter *primitive-bindings-table*
  `((boolean?      . ,#'boolean?)
    (pair?         . ,#'pair?)
    (symbol?       . ,#'symbol?)
    (number?       . ,#'number?)
    (char?         . ,#'char?)
    (string?       . ,#'string?)
    ;;(blob?         . ,#'blob?)
    ;;(port?         . ,#'port?)
    (procedure?    . ,#'procedure?)
    (cons          . ,#'scm-cons)
    (car           . ,#'scm-car)
    (cdr           . ,#'scm-cdr)
    (list          . ,#'scm-list)
    (append        . ,#'scm-append)
    (reverse       . ,#'scm-reverse)
    (+             . ,#'scm-+)
    (-             . ,#'scm--)
    (*             . ,#'scm-*)
    (/             . ,#'scm-/)
    (<             . ,#'scm-<)
    (>             . ,#'scm->)
    (<=            . ,#'scm-<=)
    (>=            . ,#'scm->=)
    (display       . ,#'scm-display)
    (newline       . ,#'scm-newline)
    ;;(null-environment          . ,#'null-environment)
    ;;(scheme-report-environment . ,#'scheme-report-environment)


    ))



(defun null-environment (version)
  (declare (ignorable version));
  (make-env))

(defun scheme-report-environment (version)
  (declare (ignorable version));
  (make-env
   :table
   (mapcar (lambda (entry)
             (cons (string (car entry))
                   (make-instance 'primitive-procedure :func (cdr entry))))
           *primitive-bindings-table*)))

(defparameter *interaction-environment*
  (scheme-report-environment 7))

(defun interaction-environment ()
  *interaction-environment*)



