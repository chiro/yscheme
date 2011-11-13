;;; 5.5. Libraries


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
;; *scm-libraries* も同様にしたい

;; 現状トップレベルで隠蔽されたエントリは削除されない 要改善


(setf *scm-features*
      '("r7rs" "exact-closed" "ratios"
        "full-unicode" "posix" "ubuntu" "x86-64" "ilp32"
        "yscheme" "yscheme-0.0001"))

(defun features-to-list (features)
  (apply #'scm-list
         (mapcar (lambda (s) (new 'scm-symbol :name s :val +true+))
                 features)))

(defun featrues-to-env (features)
  (mapcar (lambda (s) (cons s +true+)) features))

(defun find-library (syms)
  (find (mapcar #'name syms)
        *scm-libraries*
        :key (lambda (m) (mapcar #'name (syms m)))
        :test (lambda (l1 l2) (every #'equal l1 l2))))


(defmethod scm-eval ((libdef library-definition) env)
  (with-slots (syms exps lib-ex) libdef
    (let ((lib-env env))
      (dolist (exp exps)
        (scm-eval exp lib-env))
      (push (list (scm-eval lib-ex lib-env)) (cdr (last env)))
      (push (new 'library :syms syms :env lib-env) *scm-libraries*)
      +undefined+)))

(defmethod scm-eval ((lib-ex library-export) env)
  (with-slots (syms renames) lib-ex
    (let (new-frame)
      (dolist (sym syms)
        (push (cons (name sym) (cdr (assoc-env sym env))) new-frame))
      (dolist (rename renames)
        (with-slots (from to) rename
          (push (cons (name to) (cdr (assoc-env from env))) new-frame)))
      new-frame)))


(defmethod scm-eval ((lib-im library-import) env)
  (dolist (exp (exps lib-im))
    (push (list (scm-eval exp env)) (cdr (last env)))))

(defmethod scm-eval ((form import-library) env)
  (with-slots (syms) form
    (let ((lib-env (aand (find-library syms) (env it))))
      lib-env)))

(defmethod scm-eval ((form import-only) env)
  (with-slots (im-set syms) form
    (let ((lib-env (scm-eval im-set env)))
      (list (filter (lambda (sym) (assoc-env sym lib-env)) syms)))))

(defmethod scm-eval ((form import-except) env)
  (with-slots (im-set syms) form
    (let ((lib-env (scm-eval im-set env)))
      (list (mapcar (lambda (frame)
                      (remove-if (lambda (entry)
                                   (member (car entry) syms :test #'string=))
                                 frame))
                    lib-env)))))

(defmethod scm-eval ((form import-prefix) env)
  (with-slots (im-set sym) form
    (let ((lib-env (scm-eval im-set env)))
      (mapcar (lambda (frame)
                (mapcar (lambda (entry)
                          (cons (concatenate 'string (name sym) (car entry))
                                (cdr entry)))
                        frame))
              lib-env))))

(defmethod scm-eval ((form import-rename) env)
  (with-slots (im-set from to) form
    (let ((lib-env (scm-eval im-set env)))
      (mapcar (lambda (frame)
                (mapcar (lambda (entry)
                          (cons (if (string= (car entry) (name from))
                                    (name to)
                                    (car entry))
                                (cdr entry)))
                        frame))
              lib-env))))


(defmethod scm-eval ((lib-inc library-include) env)
  (dolist (file (files lib-inc))
    (scm-eval (new 'scm-include :file file) env)))

(defmethod scm-eval ((lib-inc-ci library-include-ci) env)
  (dolist (file (files lib-inc-ci))
    (scm-eval (new 'scm-include-ci :file file) env)))


(defmethod scm-eval ((lib-cond library-cond-expand) env)
  (labels ((rec (clauses)
             (if (null clauses)
                 +undefined+
                 (or (scm-clause-eval (car clauses) env)
                     (rec (cdr clauses))))))
    (rec (clauses lib-cond))))

(defmethod scm-clause-eval ((clause cond-expand-clause) env &key)
  (if (scm-truep (scm-eval (req clause) env))
      (call-next-method)))

(defmethod scm-clause-eval ((clause cond-expand-clause) env &key)
  (call-next-method))


(defmethod scm-eval ((req required-identifier) env)
  (if (member (name (feature req)) *scm-features* :test #'string=)
      +true+ +false+))

(defmethod scm-eval ((req required-library) env)
  (if (find-library (syms req))
      +true+ +false+))
