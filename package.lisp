;;;; package.lisp

(defpackage #:cl-arxiculture
  (:use #:cl #:cl-ppcre #:iterate #:cl-fad #:lol-re #:esrap-liquid
	#:cl-read-macro-tokens #:cl-dbi #:cl-itertools)
  (:shadowing-import-from #:ironclad #:digest-sequence)
  (:shadowing-import-from #:trivial-utf-8 #:string-to-utf-8-bytes)
  (:export #:ac-parse))

