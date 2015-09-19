;;;; cl-arxiculture.lisp

(in-package #:cl-arxiculture)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

;; Ok, let's setup two modest goals
;; * parse the bibliography to know which papers are there
;; * parse the text to see, how many times are the papers cited

(defmacro inject-kwd-if-nonnil (name)
  ``,@(if ,name
	  (list (list ,(intern (string name) "KEYWORD")
		      ,name))))

(defmacro inject-kwds-if-nonnil (&rest names)
  ``,@`(,,@(mapcar (lambda (x)
		     ``,!m(inject-kwd-if-nonnil ,x))
		   names)))

(defun err-script (str)
  (multiple-value-bind (out err errno) (clesh:script str)
    (if (equal 0 errno)
	out
	(error err))))

(defun slurp-stream (stream)
  (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
    (setf (fill-pointer seq) (read-sequence seq stream))
    seq))

(defun slurp-file (fname)
  (with-open-file (stream fname :direction :input)
    (slurp-stream stream)))

(defun slurp-file-till-unreadable (fname)
  (handler-bind ((sb-int:stream-decoding-error
		  (lambda (c) (declare (ignore c))
			  (invoke-restart 'sb-int::force-end-of-file))))
    (slurp-file fname)))

(defparameter *begin-document-re* #?/\\begin\s*{\s*document\s*}/)
(defparameter *end-document-re* #?/\\end\s*{\s*document\s*}/)

(defparameter *begin-bibliography-re* #?/\\begin\s*{\s*thebibliography\s*}/)
(defparameter *end-bibliography-re* #?/\\end\s*{\s*thebibliography\s*}/)

(defparameter *bibitem-re* #?/\\bibitem{\s*([^}]+)\s*}/)

(defparameter *arxiv-re-1* #?/(?i)arXiv[.:]+[0-9.]+/)
(defparameter *arxiv-re-2* #?/(?i)(arXiv[.:]+)?hep-[tp]h\/{?[0-9.]+}?/)

(defparameter *work-dir* "~/2003/")

;; what are different arXiv categories (like hep-th?) and how to interactively parse them?
;; Honestly, even if I'll be able to visualize only hep-th, it'll already be a win.

(defun percentage-of-begin-end-documents (&optional (year 1992))
  (iter (for fname in (list-directory #?"~/$(year)"))
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

;; Ok, now that I know the crude stats, I want to know more refined ones?
;; For how many papers I actually know that they cite other ones?

(defun citation-coverage (&optional (year 1992))
  (iter outer (for fname in (list-directory #?"~/$(year)"))
	;; (for i from 1 to 10)
	(let ((text (slurp-file-till-unreadable fname)))
	  (let ((bibitems (nreverse (cons (1- (length text))
					  (cons (1- (length text))
						(nreverse (all-matches *bibitem-re* text)))))))
	    (if (equal 2 (length bibitems))
		(next-iteration))
	    ;; (format t "~a~%" bibitems)
	    (summing 1 into with-bibitems)
	    (iter (for elt on bibitems by #'cddr)
		  ;; (format t "~a ~a~%" (car elt) (caddr elt))
		  (if (not (cddr elt))
		      (terminate))
		  (summing 1 into num-items)
		  (let ((sub-text (subseq text (car elt) (caddr elt))))
		    (if (or (all-matches *arxiv-re-2* sub-text)
			    (all-matches *arxiv-re-1* sub-text))
			(summing 1 into cited)))
		  (finally (in outer (summing (float (/ cited num-items)) into cite-ratio))
			   (in outer (summing (float (expt (/ cited num-items) 2))
					      into cite-ratio-squared))))))
	(finally (let ((avg (float (/ cite-ratio with-bibitems)))
		       (avg-square (/ cite-ratio-squared with-bibitems)))
		   (format t "~a: avg ratio: ~a, disp ratio: ~a~%"
			   year avg
			   (sqrt (- avg-square (expt avg 2))))))))

(defun slightly-comb-file (text)
  (cl-ppcre:regex-replace-all "%%@\\n" text ""))

(defun citation-distribution (&optional (year 1992))
  (let ((bins (make-array 10 :element-type 'integer :initial-element 0)))
    (iter outer (for fname in (list-directory #?"~/$(year)"))
	  (let ((text (slightly-comb-file (slurp-file-till-unreadable fname))))
	    (let ((bibitems (nreverse (cons (1- (length text))
					    (cons (1- (length text))
						  (nreverse (all-matches *bibitem-re* text)))))))
	      (if (equal 2 (length bibitems))
		  (next-iteration))
	      (iter (for elt on bibitems by #'cddr)
		    (if (not (cddr elt))
			(terminate))
		    (summing 1 into num-items)
		    (let ((sub-text (subseq text (car elt) (caddr elt))))
		      (if (or (all-matches *arxiv-re-2* sub-text)
			      (all-matches *arxiv-re-1* sub-text))
			  (summing 1 into cited)))
		    (finally (incf (aref bins (min (floor (* 10 (/ cited num-items)))
						   9))))))))
    (format t "~a: ~{~a~^ ~}~%" year (coerce bins 'list))))


;; Citations stats:
;; 1992: avg ratio: 0.011281135, disp ratio: 0.08333967
;; 1993: avg ratio: 0.030274456, disp ratio: 0.11838811
;; 1994: avg ratio: 0.058283474, disp ratio: 0.12932242
;; 1995: avg ratio: 0.1079002, disp ratio: 0.18162246
;; 1996: avg ratio: 0.17636852, disp ratio: 0.24548708
;; 1997: avg ratio: 0.25662202, disp ratio: 0.30075064
;; 1998: avg ratio: 0.29802635, disp ratio: 0.31730652
;; 1999: avg ratio: 0.33674067, disp ratio: 0.3251256
;; 2000: avg ratio: 0.38522813, disp ratio: 0.34249783
;; 2001: avg ratio: 0.40374413, disp ratio: 0.3399971
;; 2002: avg ratio: 0.436366, disp ratio: 0.34373802
;; 2003: avg ratio: 0.43859905, disp ratio: 0.34793764

;; Citation distributions:
;; 1992: 505 8 1 2 0 1 0 0 0 5
;; 1993: 938 44 17 9 1 5 2 0 0 10
;; 1994: 1107 138 84 24 12 10 4 0 0 11
;; 1995: 1058 249 104 57 31 29 21 13 6 18
;; 1996: 1050 264 167 100 58 60 40 43 41 48
;; 1997: 872 292 176 115 85 87 76 65 87 113
;; 1998: 829 298 192 142 90 113 95 98 116 151
;; 1999: 780 305 190 136 104 152 126 150 156 159
;; 2000: 806 297 199 152 112 192 166 169 231 264
;; 2001: 751 284 211 142 139 204 179 222 234 261
;; 2002: 729 249 211 149 134 197 228 267 303 294
;; 2003: 213 89 76 46 45 62 52 78 82 112


;; OK, clearly most of the papers are not being cited correctly
;; So, there must be the way to increase the citation coverage somehow


(defun make-bibitems-only (&optional (year 1992))
  (err-script #?"mkdir -p ~/$(year)-bibitems")
  (iter outer (for fname in (list-directory #?"~/$(year)"))
	(let ((text (slightly-comb-file (slurp-file-till-unreadable fname))))
	  (let ((bibitems (all-matches *bibitem-re* text)))
	    (if (not bibitems)
		(next-iteration))
	    (err-script #?"ln -s $(fname) ~/$(year)-bibitems/")))))


;; What I need is some sort of "analog" parser -- that is, the thing that
;; not only says "this is precisely this" or "this is not it", but
;; rather this looks like this up to a certain extent.

;; For now the effectiveness (in terms of fastness and memory usage)
;; of this parser is not important at all, unless it becomes absolutely
;; slow and memory-consumptive -- the main thing to do is to develop the
;; concept of what it should in principle do and how.


(defun foo ()
  (slurp-file "~/1993-bibitems/9312210"))

(defun extract-bibliography (text)
  (let ((begins (all-matches *begin-bibliography-re* text))
	(ends (all-matches *end-bibliography-re* text)))
    (if (or (not begins)
	    (not ends))
	(error "I can't match begin or end bibliography regexps, sorry"))
    (if (< 2 (length begins))
	(warn "More than one begin of bibliography found, please, look manually"))
    (if (< 2 (length ends))
	(warn "More than one end of bibliography found, please, look manually"))
    (subseq text (cadr begins) (car ends))))

(defun to-bibitems (text)
  (mapcar #'analog-parse-bibitem (cdr (split #?/\\bibitem/ text))))

(defun analog-parse-bibitem (text)
  (let ((label (progn (m~ #?/^{([^}]+)}/ text) (list $1 $-0 $+0))))
    (list label text)))

(defparameter *journal-names*
  '((:physics-review-letters-d "Phys. Rev. Lett. D")
    (:nuclear-physics-b "Nucl. Phys. B")
    (:physics-letters-b "Phys. Lett. B")
    (:cern-preprint "CERN Preprint")
    (:rims-preprint "Research Institute in Mathematical Sciences, Kyoto, Preprint")
    (:durham-preprint "Durham University Preprint")
    (:private-communication "private communication")))

(defparameter *authors*
  '(
    (("Braden" "H" "W") ("H.~W. Braden"))
    (("Christe" "P") ("P.~Christe"))
    (("Coleman" "S") ("S.~Coleman"))
    (("Corrigan" "E" "F") ("E.~F. Corrigan"))
    (("Delius" "G" "W") ("G.~W. Delius" "G.~W.~Delius"))
    (("Destri" "C") ("C.~Destri"))
    (("Dorey" "P" "E") ("P.~E. Dorey"))
    (("Feigin" "B" "L") ("B.~L. Feigin"))
    (("Frenkel" "E" "V") ("E.~V. Frenkel"))
    (("Grisaru" "M" "T") ("M.~T. Grisaru"))
    (("Kac" "Victor" "G") ("V.~Kac" "V.~G.~Kac" "V.G. Kac" "V. Kac" "V.Kac" "Kac, V. G." "Kac,V.G." "Kac, V.G."
			   "V.G.~Kac"
			   "V. Ka\v c"
			   ))
    (("Kausch" "H" "G") ("H.~G. Kausch"))
    (("Mussardo" "G") ("G.~Mussardo"))
    (("Sasaki" "R") ("R.~Sasaki"))
    (("de Vega" "H" "J") ("H.~J. de~Vega"))
    (("Watts" "G" "M" "T") ("G.~M.~T. Watts"))
    (("Weston" "R" "A") ("R.~A. Weston"))
    (("Zanon" "D") ("D.~Zanon"))
    ))

;; Then I can grep all these surnames through the whole arXiv to see, how they are spelled
;; in different papers
    
;; 
  
(defun bar (&optional fname)
  (to-bibitems (extract-bibliography (or (slurp-file fname)
					 (foo)))))


;; OK, let's pick a random paper with sufficient number of
;; bibitems and write a parser for them...
;; Hmm... the way authors write references seems to be 
;; very much correlated between their papers

(define-ac-rule posinteger ()
  (parse-integer (text (postimes (character-ranges (#\0 #\9))))))

(define-ac-rule page-number ()
  posinteger)

(define-ac-rule year ()
  (let ((it posinteger))
    (if (or (> 1900 it)
	    (< 9999 it))
	(fail-parse "Integer not in 1900 -- 9999 range to be counted as a valid year"))
    it))

(define-ac-rule bracket-year ()
  (progm #\( year #\)))

(define-ac-rule surname ()
  (text (character-ranges (#\A #\Z)) (postimes (character-ranges (#\a #\z)))))

(define-ac-rule initial ()
  (text (prog1 (character-ranges (#\A #\Z)) #\.)))

(define-ac-rule full-name ()
  (let* ((initials (times initial))
	 (surname surname))
    (cons surname initials)))

(define-ac-rule wh-char ()
  (|| #\space #\tab #\newline #\return))
(define-ac-rule whitespace ()
  (postimes wh-char))

(define-ac-rule et-al ()
  "et al." :etal)
	
(defmacro!! define-plural-rule (name single delim) ()
  `(define-ac-rule ,name ()
     (cons ,single
	   (times (progn ,delim ,single)))))

(define-plural-rule full-names full-name (progn (? whitespace) #\, (? whitespace)))

(defmacro wh (x)
  `(progn whitespace ,x))

(defmacro wh? (x)
  `(progn (? whitespace) ,x))

(defmacro ?wh (x)
  `(? (progn whitespace ,x)))

(defmacro ?wh? (x)
  `(? (progn (? whitespace) ,x)))

(define-ac-rule bibitem-label ()
  (text (progm #\{ (postimes (!! #\})) #\})))

(define-ac-rule bf-journal-spec ()
  (let ((meat (text (progm (progn #\{ (? whitespace) "\\bf" whitespace)
			   (postimes (!! #\}))
			   #\}))))
    (string-whitespace-trim meat)))

(defun string-whitespace-trim (str)
  (string-trim '(#\space #\tab #\newline #\return) str))

(define-ac-rule simple-surname ()
  (text (list (character-ranges (#\A #\Z))
	      (postimes (|| (character-ranges (#\a #\z))
			    (list #\\ (|| #\' #\")))))))

(define-ac-rule simple-initial ()
  (text (prog1 (character-ranges (#\A #\Z)) #\.)))

(define-plural-rule simple-initials simple-initial (? whitespace))

(define-ac-rule simple-author-name ()
  (let* ((initials simple-initials)
	 (surname (wh? simple-surname)))
    (cons surname initials)))

(define-ac-rule comma-and-delim ()
  (|| (progn #\, (? whitespace) "and")
      #\,
      "and"))

(define-plural-rule comma-and-authors simple-author-name (progn (? whitespace)
								comma-and-delim
								(? whitespace)))

(define-ac-rule simple-journal-name ()
  (string-whitespace-trim
   (text (postimes (|| #\space #\tab #\newline #\return #\.
		       (character-ranges (#\a #\z) (#\A #\Z)))))))

(define-ac-rule it-paper-name ()
  (let ((meat (text (progm (progn #\{ (? whitespace) "\\it" whitespace)
			   (postimes (!! #\}))
			   #\}))))
    (string-trim '((literal-char #\,))
		 (string-whitespace-trim meat))))


(define-ac-rule elt-9201003-bibitem ()
  (let* ((authors comma-and-authors)
	 (name (?wh it-paper-name))
	 (jname (wh? simple-journal-name))
	 (jspec (wh? bf-journal-spec))
	 (year (wh? bracket-year))
	 (page (wh? page-number)))
    `(,!m(inject-kwds-if-nonnil authors name jname jspec year page))))
    

(define-ac-rule 9201003-bibitem ()
  (let* ((label (wh? bibitem-label))
	 (rest (wh? 9201003-bibitem-meat)))
    (list label rest)))

(define-ac-rule bibitem ()
  (|| 9201003-bibitem))
      

(defun parse-bibitem (bibitem)
  (ac-parse 'bibitem bibitem))

(defun bibitem-cleanly-parseable (bibitem)
  (handler-case (progn (parse-bibitem bibitem) t)
    (error () nil)))

(defparameter *citation-parsing-threshold* 0.6)

(defun find-first-critical-paper (&optional (year 1992))
  (iter outer (for fname in (list-directory #?"~/$(year)-bibitems"))
	(for i from 1 to 10)
	(let ((bibitems (to-bibitems (extract-bibliography (slurp-file fname)))))
	  (iter (for bibitem in bibitems)
		(summing 1 into total)
		(if (bibitem-cleanly-parseable (cadr bibitem))
		    (summing 1 into clean))
		(finally (let ((clean-ratio (if (zerop total)
						0
						(float (/ clean total)))))
			   (if (and (< 0 total)
				    (> *citation-parsing-threshold* clean-ratio))
			       (return-from find-first-critical-paper
				 (values fname clean-ratio)))))))))
			       
			       
