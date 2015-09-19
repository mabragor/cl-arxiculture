;;;; cl-arxiculture.asd

(asdf:defsystem #:cl-arxiculture
  :description "Visualize connections between arXiv preprints through citations"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:cl-interpol #:cl-ppcre #:iterate #:clesh #:cl-fad #:lol-re
			     #:esrap-liquid #:cl-read-macro-tokens #:quasiquote-2.0)
  :components ((:file "package")
	       (:file "parsing-macro")
               (:file "cl-arxiculture")))

(defsystem :cl-arxiculture-tests
  :description "Tests for CL-ARXICULTURE"
  :licence "MIT"
  :depends-on (:cl-arxiculture :fiveam :cl-interpol)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-arxiculture))))
  (load-system :cl-arxiculture)
  (funcall (intern "RUN-TESTS" :cl-arxiculture)))
