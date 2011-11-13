(in-package :yscheme)


(defparameter *primitive-bindings-table*
  `(("boolean?"      . ,#'boolean?)
    ("pair?"         . ,#'pair?)
    ("symbol?"       . ,#'symbol?)
    ("number?"       . ,#'number?)
    ("char?"         . ,#'char?)
    ("string?"       . ,#'string?)
    ;;("blob?"         . ,#'blob?)
    ;;("port?"         . ,#'port?)
    ("procedure?"    . ,#'procedure?)
    ("cons"          . ,#'scm-cons)
    ("car"           . ,#'scm-car)
    ("cdr"           . ,#'scm-cdr)
    ("list"          . ,#'scm-list)
    ("append"        . ,#'scm-append)
    ("reverse"       . ,#'scm-reverse)
    ("length"        . ,#'scm-length)
    ("="             . ,#'scm-=)
    ("+"             . ,#'scm-+)
    ("-"             . ,#'scm--)
    ("*"             . ,#'scm-*)
    ("/"             . ,#'scm-/)
    ("<"             . ,#'scm-<)
    (">"             . ,#'scm->)
    ("<="            . ,#'scm-<=)
    (">="            . ,#'scm->=)
    ;;("display"       . ,#'scm-display)
    ;;("newline"       . ,#'scm-newline)
    ;;("null-environment"          . ,#'null-environment)
    ;;("scheme-report-environment" . ,#'scheme-report-environment)



    ))



(defun null-environment (version)
  (declare (ignorable version));
  nil)

(defun scheme-report-environment (version)
  (declare (ignorable version));
  (list (mapcar (lambda (entry)
                  (cons (car entry)
                        (new 'primitive-procedure :func (cdr entry))))
                *primitive-bindings-table*)))

(defun environment (&rest import-sets)
  (let ((new-env (null-environment 7)))
    (dolist (import-set import-sets)
      (scm-eval import-set new-env))
    new-env))

(defparameter *interaction-environment*
  (scheme-report-environment 7))

(defun interaction-environment ()
  *interaction-environment*)

(defun make-environment (names)
  (list (mapcar
         (lambda (name)
           (cons name
                 (new 'primitive-procedure
                      :func (symbol-function
                             (read-from-string
                              (concatenate 'string *scheme-prefix* "-" name)))))))))