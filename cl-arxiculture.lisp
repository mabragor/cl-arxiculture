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

(defparameter *work-dir* "~/2003/")

;; what are different arXiv categories (like hep-th?) and how to interactively parse them?
;; Honestly, even if I'll be able to visualize only hep-th, it'll already be a win.

(defun percentage-of-begin-end-documents ()
  (iter (for fname in (list-directory *work-dir*))
	(let ((text (handler-bind ((sb-int:stream-decoding-error
		    		    (lambda (c) (declare (ignore c))
		    			    (invoke-restart 'sb-int::force-end-of-file))))
		      (slurp-file fname))))
	  (if (all-matches *begin-document-re* text)
	      (summing 1 into begin-documents))
	  (if (all-matches *end-document-re* text)
	      (summing 1 into end-documents))
	  (if (all-matches *bibitem-re* text)
	      (summing 1 into bibitems))
	  (summing 1 into all-documents))
	(finally (format t "Ratio of papers with begin document: ~a/~a~%" begin-documents all-documents)
		 (format t "Ratio of papers without end document and with begin document: ~a/~a~%"
			 (- begin-documents end-documents)
			 begin-documents)
		 (format t "Number of papers with bibitems: ~a~%" bibitems))))

;; OK, just to have a feeling of how clear/dirty the data is,
;; here are the stats of a very crude parsing
;; year    total     w-begin     wo-end     w-bibitems
;; 1992:   1367      646         1          522
;; 1993:   2058      1237        5          1026
;; 1994:   2377      1656        24         1390
;; 1995:   2303      1780        14         1586
;; 1996:   2606      2146        30         1871
;; 1997:   2673      2244        57         1968
;; 1998:   2758      2393        63         2124
;; 1999:   2803      2523        91         2258
;; 2000:   3126      2849        97         2588
;; 2001:   3153      2915        102        2627
;; 2002:   3312      3077        113        2761
;; 2003:   1019      955         44         855

