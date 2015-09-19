(in-package :cl-user)

(defpackage :cl-arxiculture-tests
  (:use :cl :cl-arxiculture :fiveam :iterate :cl-read-macro-tokens)
  (:export #:run-tests))

(in-package :cl-arxiculture-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

(def-suite cl-arxiculture)
(in-suite cl-arxiculture)

(defun run-tests ()
  (let ((results (run 'cl-arxiculture)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
      (is (equal t t)))