;;;; cl-arxiculture.asd

(asdf:defsystem #:cl-arxiculture
  :description "Visualize connections between arXiv preprints through citations"
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :version "0.1"
  :serial t
  :depends-on (#:cl-interpol #:cl-ppcre #:iterate #:clesh #:cl-fad #:lol-re)
  :components ((:file "package")
               (:file "cl-arxiculture")))

