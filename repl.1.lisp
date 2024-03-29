(in-package :yscheme)

(defparameter *current-version* '(0 0 0 1))


(defun %read-print-eval-loop ()
  (print-prompt)
  (do ((toplevel-counter 0 (1+ toplevel-counter))
       (input-data (scm-read) (scm-read)))
      ((repl-end-p input-data))
    (multiple-value-bind (value envp) (scm-eval input-data) ;; mbr
      (setf *previous-interaction-environment*
            (copy-tree *interaction-environment))
      (print-value value envp)
      (print-prompt))))

(defun repl ()
  (print-title)
  (loop
     (handler-case (%read-eval-print-loop)
       (error ()
         (format t "ERROR: invalid input~%")
         (setf *interaction-environment*
               (copy-tree *previous-interaction-environment*))))))

(defun print-prompt ()
  (format t "~%> "))

(defun print-title ()
  (apply #'format t
         "YSCHEME(PROTOTYPE) ~D.~D.~D.D~%~%"
         *current-version*))

(defun repl-end-p (data)
  (and (eql (class-name (class-of data)) 'quotation)
       (scm-truep (scm-symbol? (qexp data)))
       (eql "bye" (name data))))

(defun print-value (value envp)
  (cond (envp
        ((listp value)
         (dolist (v value)
           (scm-write value *standard-outpu*)
           (princ #\Newline)))
        (t (scm-write value *standard-outpu*)))))


(defmacro flags-acond (&rest clauses)
  (if (null clauses)
      nil
      (let ((cl1 (car clauses)))
        (if (keywordp (car cl1))
            (destructuring-bind (flag flags chars l-pred r-pred inp excepts) cl1
              `(acond ((and (every-zero-p ,flags ,excepts) (funcall ,l-pred ,chars))
                       (incf (getf ,flags ,flag))
                       (incf (getf ,flags :characters-counter) it)
                       (iter (nthcdr it ,chars) nil))
                      ((and (plusp (getf ,flags ,flag)) (funcall ,r-pred ,chars))
                       (decf (getf ,flags ,flag))
                       (incf (getf ,flags :characters-counter) it)
                       (iter (nthcdr it ,chars) nil))
                      ((and ,inp (plusp (getf ,flags ,flag)))
                       (incf (getf ,flags :characters-counter))
                       (iter (nthcdr 1 ,chars) nil))
                      (t (flags-acond ,@(cdr clauses)))))
            `(aif ,(car cl1)
                  (progn ,@(cdr cl1))
                  (flags-acond ,@(cdr clauses)))))))

(defun possible-datum-p (chars)
  (let ((flags (list :characters-counter 0 :parentheses-counter 0
                     :comment-nest-counter 0 :in-string 0 :in-symbol 0
                     :in-line-comment 0 :in-datum-comment 0)))
    (labels ((iter (chars collected)
               (flags-acond
                ((or (minusp (getf flags :parentheses-counter))
                     (minusp (getf flags :comment-nest-counter)))
                 nil)
                ((and (every-zero-p flags '(:characters-counter))
                      (plusp (getf flags :characters-counter))
                      (null chars))
                 t)
                ((or (and (zerop (getf flags :parentheses-counter))
                          (zerop (getf flags :comment-nest-counter))
                          (plusp (getf flags :in-line-comment))
                          (plusp (getf flags :in-datum-comment)))
                     (null chars))
                 (getf flags :characters-counter))
                (:comment-nest-counter flags chars
                 #'comment-left-paren-p #'comment-right-paren-p t
                 '(:characters-counter :in-datum-comment :comment-nest-counter))
                (:in-line-comment flags chars
                 (lambda (l) (and (null collected) (line-comment-left-p l)))
                 #'newlinep t (list :in-datum-comment))
                (:in-datum-comment flags chars
                 #'datum-comment-left-p
                 (lambda (l) l (datump collected)) nil;;;;;
                 '(:in-datum-comment))
                (:in-string flags chars
                 #'double-quote-p #'double-quote-p t nil)
                (:in-symbol flags chars #'bar-p #'bar-p t nil)
                ((left-paren-p chars)
                 (incf (getf flags :parentheses-counter))
                 (incf (getf flags :characters-counter) it)
                 (iter (nthcdr it chars) nil))
                ((right-paren-p chars)
                 (decf (getf flags :parentheses-counter))
                 (incf (getf flags :characters-counter) it)
                 (iter (nthcdr it chars) nil))
                ((and (whitespacep chars) (datump collected))
                 (incf (getf flags :characters-counter))
                 (iter (cdr chars) nil))
                (t
                 (incf (getf flags :characters-counter))
                 (iter (cdr chars) (append1 collected (car chars)))))))
      (iter chars nil))))

(defun every-zero-p (flags excepted-flags)
  (every #'zerop
         (mapcar #'cadr
                 (remove-if (lambda (e) (every (lambda (f) (eql (car e) f))
                                               excepted-flags))
                            (group flags 2)))))

(defun datump (chars)
  (multiple-value-bind (obj num)
      (esrap::parse 'yscheme::datum (concatenate 'string chars) :junk-allowed t)
    (and obj num)))

(defmacro substring-predicate (match args)
  `(and (notany #'not (mapcar (lambda (a c) (and a (char= a c)))
                              ,args (concatenate 'list ,match)))
        (length ,match)))

(defun left-paren-p (chars)
  (cond ((substring-predicate "(" chars) 1)
        ((substring-predicate "#(" chars) 2)
        ((substring-predicate "#u8(" chars) 4)))
(defun right-paren-p (chars) (substring-predicate ")" chars))

(defun comment-left-paren-p (chars) (substring-predicate "#|" chars))
(defun comment-right-paren-p (chars) (substring-predicate "|#" chars))

(defun line-comment-left-p (chars) (substring-predicate ";" chars))
(defun newlinep (chars) (and (char= (car chars) #\Newline) 1))

(defun datum-comment-left-p (chars) (substring-predicate "#;" chars))

(defun double-quote-p (chars) (substring-predicate "\"" chars))
(defun bar-p (chars) (substring-predicate "|" chars))

(defun whitespacep (chars)
  (and (member (car chars) '(#\Space #\Newline #\Return) :test #'char=) 1))
