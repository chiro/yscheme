(in-package :yscheme)


(push *base-library* *scm-libraries*)
(push *case-lambda-library* *scm-libraries*)
(push *char-normalization-library* *scm-libraries*)
(push *char-library* *scm-libraries*)
(push *complex-library* *scm-libraries*)
(push *division-library* *scm-libraries*)
(push *eval-library* *scm-libraries*)
(push *file-library* *scm-libraries*)
(push *inexact-library* *scm-libraries*)
(push *lazy-library* *scm-libraries*)
(push *load-library* *scm-libraries*)
(push *process-context-library* *scm-libraries*)
(push *read-library* *scm-libraries*)
(push *repl-library* *scm-libraries*)
(push *time-library* *scm-libraries*)
(push *write-library* *scm-libraries*)

(setf *interaction-environment*
      (scm-scheme-report-environment (new 'scm-number :val 7)))