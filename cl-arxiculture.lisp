;;;; cl-arxiculture.lisp

(in-package #:cl-arxiculture)

(cl-interpol:enable-interpol-syntax)

;; Ok, let's setup two modest goals
;; * parse the bibliography to know which papers are there
;; * parse the text to see, how many times are the papers cited

(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun slurp-file (fname)
  (with-open-file (stream fname :direction :input)
    (slurp-stream stream)))

(defparameter *begin-document-re* #?/\\begin\s*{\s*document\s*}/)
(defparameter *end-document-re* #?/\\end\s*{\s*document\s*}/)

(defparameter *begin-bibliography-re* #?/\\begin\s*{\s*thebibliography\s*}/)
(defparameter *end-bibliography-re* #?/\\end\s*{\s*thebibliography\s*}/)

(defparameter *bibitem-re* #?/\\bibitem{\s*([^}]+)\s*}/)

(defparameter *arxiv-re-1* #?/arXiv[.:]+[0-9.]+/)

(defparameter *arxiv-re-1* #?/(arXiv[.:])?+[0-9.]+/)

;; what are different arXiv categories (like hep-th?) and how to interactively parse them?
;; Honestly, even if I'll be able to visualize only hep-th, it'll already be a win.
