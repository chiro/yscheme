;;; 5.5. Modules


(in-package :yscheme)


;; 評価順は一定?
;; (0. features)
;; 1. import
;; 2. include, include-ci
;; 3. begin
;; 4. export

;; features は
;; 処理系上の変数 *scm-features* を features-to-list により変換して
;; scheme上の変数 *features* にあるものとする(list of scm-symbol)
;; *scm-modules* も同様にしたい

(setf *scm-features*
      '("r7rs" "exact-closed" "ratios"
        "full-unicode" "posix" "ubuntu" "x86-64" "ilp32"
        "yscheme" "yscheme-0.0001"))

(defun features-to-list (features)
  (apply #'scm-list
         (mapcar (lambda (s) (new 'scm-symbol :name s :val +true+)))))

(defun featrues-to-env (features)
  (mapcar (lambda (s) (cons s +true+))))



(defmethod scm-eval ((moddef module-definition) env)
  (with-slots (syms exps mod-ex) moddef
    (let ((mod-env env))
      (dolist (exp exps)
        (scm-eval exp mod-env))
      (setf env (cons (scm-eval mod-ex mod-env) env))
      (push (new 'module :syms syms :env mod-env) *scm-modules*)
      +undefined+)))

(defmethod scm-eval ((mod-ex module-export) env)
  (with-slots (syms renames) mod-ex
    (let (new-env)
      (dolist (sym syms)
        (push (cons (name sym) (cdr (assoc-env sym env))) new-env))
      (dolist (rename renames)
        (with-slots (from to) rename
          (push (cons (name to) (cdr (assoc-env from env))) new-env)))
      new-env)))

(defmethod scm-eval ((mod-im module-import) env)
  (dolist (exp (exps mod-im))
    (scm-eval exp env)))

(defmethod scm-eval ((mod-inc module-include) env)
  (dolist (file (files mod-inc))
    (scm-eval (new 'scm-include :file file))))

(defmethod scm-eval ((mod-inc-ci module-include-ci) env)
  (dolist (file (files mod-inc-ci))
    (scm-eval (new 'scm-include-ci :file file))))


(defmethod scm-eval ((mod-cond module-cond-expand) env)
  (labels ((rec (clauses)
             (if (null clauses)
                 +undefined+
                 (or (scm-clause-eval (car clauses) env)
                     (rec (cdr clauses))))))
    (rec (clauses mod-cond))))

(defmethod scm-clause-eval ((clause cond-expand-clause) env &key)
  (if (scm-truep (scm-eval (req clause) env))
      (call-next-method)))

(defmethod scm-clause-eval ((clause cond-expand-clause) env &key)
  (call-next-method))


(defmethod scm-eval ((req required-identifier) env)
  (if (member (name (feature req)) *scm-features* :test #'string=)
      +true+ +false+))

(defmethod scm-eval ((req required-module) env)
  (if (member (mapcar #'name (syms req))
              *scm-modules*
              :key (lambda (m) (mapcar #'name (syms m)))
              :test (lambda (l1 l2) (every #'equal l1 l2)))
      +true+ +false+))