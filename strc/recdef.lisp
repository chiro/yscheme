;;; 5.4. Record-type definitions


(in-package :yscheme)


;; record-type
;; 連想リストを値に持つrecord-typeクラスのオブジェクト
;;


(defun make-record-type-constructor (sym parms)
  (new 'primitive-procedure
       :func (lambda (&rest args)
               (new 'record-type
                    :name (name sym)
                    :val (pairlis (name parms) args)))))

(defun make-record-type-predicate (sym)
  (new 'primitive-procedure
       :func (lambda (rec)
               (string= (name sym) (name rec)))))

(defun make-record-type-accessor (sym)
  (new 'primitive-procedure
       :func (lambda (rec) (assoc (name sym) (val rec)))))

(defun make-record-type-modifier (sym)
  (new 'primitive-procedure
       :func (lambda (rec val) (setf (assoc (name sym) (val rec)) val))))


(defmethod scm-eval ((recdef record-type-definition) env)
  (with-slots (rec-type const pred fields) recdef
    (let (diff)
      (with-slots (sym parms) const
        (push (cons (name sym) (make-record-type-constructor rec-type parms))
              diff)
        (push (make-record-type-predicate rec-type)
              diff))
      (dolist (field fields)
        (with-slots (sym access modify) field
          (push (cons (name access) (make-record-accessor sym)) diff)
          (when modify
            (push (cons (name modify) (make-record-modifier sym)) diff))))
      (push diff env))))
