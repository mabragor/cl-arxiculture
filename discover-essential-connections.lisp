
(in-package #:cl-arxiculture)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)

;; It seems that there are a lot of metadata for old papers on arxiv, sources of
;; whice I do not have

;; So, the cycle should look something like:
;; * (done) look over sorted list of nice sources that I have
;;   (later, with help of Amazon S3 this will be substituted by an honest list of all sources)
;; * for each of them discover the essential connections
;;   * (done) most cited papers
;;   * (done) map to actual arxivIDs via arxiv_metadata table
;;   * create permanent IDs for the papers, which weren't found
;; * write all this into the special DB


(defiter arxiv-id-fnames (year) ()
  (let ((lst (sort (mapcar (lambda (x)
			     (format nil "~a" x))
			   (list-directory #?"~/$(year)-bibitems"))
		   #'string<)))
    (iter (for fname in lst)
	  (yield fname))))

(defun new-id-num ()
  (let ((id (with-open-file (stream "~/quicklisp/local-projects/cl-arxiculture/id-nums.txt" :direction :input)
	      (read stream))))
    (with-open-file (stream "~/quicklisp/local-projects/cl-arxiculture/id-nums.txt"
			    :direction :output :if-exists :supersede)
      (princ (1+ id) stream))
    id))

(defun print-authors (authors)
  (format nil "~{~a~^, ~}"
	  (mapcar (lambda (x)
		    (format nil "~a ~{~a~^. ~}" (car x) (cdr x)))
		  authors)))

(defun make-new-fake-id (date authors)
  (let ((new-id (new-id-num)))
    (with-connection
	(let* ((sql (prepare *connection* "insert into arxiv_metadata
                                             (id, arxiv_id, authors, authors_hash, submitted)
                                           values (?, ?, ?, ?, ?)"))
	       (arxiv-id #?"fake/$(new-id)")
	       (id (md5sum arxiv-id))
	       (authors-hash (calc-authors-hash authors))
	       (authors (print-authors authors))
	       (result (execute sql id arxiv-id authors authors-hash date)))
	  result))
    (list :fake new-id)))

(defun permanent-id (str paper-submit id-fetcher)
  (if str
      (or (sloppy-parse-arxiv-id str)
	  (let* ((year (sloppy-parse-year str))
		 (date (if year
			   (car (sort (list #?"$(year)-12-31" paper-submit)
				      #'string<))
			   paper-submit))
		 (authors (sloppy-parse-authors str)))
	    (or (cadr (funcall id-fetcher date authors))
		;; TODO : here we add the logic to crunch new fake IDs
		(make-new-fake-id (if year
				      #?"$(year)-01-01"
				      "0000-00-00")
				  authors))))
      (make-new-fake-id paper-submit nil)))

(defun mk-arxiv-id-fetcher ()
  (let ((connection (arxiv-connect)))
    (let ((sql (prepare connection
			"select arxiv_id from arxiv_metadata where authors_hash = ? and submitted < ?
                         order by submitted desc
                         limit 1")))
      (lambda (year authors)
	(let ((result (execute sql (calc-authors-hash authors) #?"$(year)-12-31")))
	  (let ((res (iter (for row next (let ((it (fetch result)))
					   (or it (terminate))))
			   (collect row))))
	    (car res)))))))

      
      
    

(defparameter *sample-str* 
  "{witten} E. Witten, {\\it The $N$ matrix model and gauged WZW models,}
IAS preprint IASSNS-HEP-91/26 (June, 1991).

")

(defun fname->id (fname)
  (let ((it (pathname-name fname)))
    (if (m~ "\." it)
	it
	#?"hep-th/$(it)")))

(defun id->submit (id)
  (let* ((sql (prepare *connection* "select date_format(min(submitted), '%Y-%m-%d') as submit
                                     from arxiv_metadata where id = ?"))
	 (result (execute sql (md5sum id))))
    (iter (for res next (let ((it (fetch result)))
			  (or it (terminate))))
	  (return (getf res :|submit|)))))

(defun id->submit-first-after (id)
  (let* ((sql (prepare *connection* "select date_format(submitted, '%Y-%m-%d') as submit
                                     from arxiv_metadata where arxiv_id > ?
                                     order by arxiv_id asc limit 1"))
	 (result (execute sql id)))
    (iter (for res next (let ((it (fetch result)))
			  (or it (terminate))))
	  (return (getf res :|submit|)))))

(defun rough-submit (id)
  (let ((pre-year (if (m~ "^hep-th" id)
		      (subseq id 7 9)
		      (subseq id 0 2))))
    (if (char= #\9 (char pre-year 0))
	#?"19$(pre-year)-12-31"
	#?"20$(pre-year)-12-31")))
      
(defun id->submit-nmw (id)
  (or (id->submit id)
      (id->submit-first-after id)
      (rough-submit id)))

(defun cite-name->id (name submit bibitems fetcher)
  (permanent-id (cadr (assoc name bibitems
			     :test (lambda (x y)
				     (equal x (car y)))))
		submit
		fetcher))

  

(defun essential-cites-as-ids (fname)
  (let* ((cites (essential-cites (all-cites fname)))
	 (submit (with-connection
		     (id->submit-nmw (fname->id fname))))
	 ;; (cites-lst (apply #'append (mapcar #'cdr cites)))
	 (bibitems (to-bibitems (extract-bibliography (slurp-file fname))))
	 (fetcher (mk-arxiv-id-fetcher)))
    (or (iter (for (weight . names) in cites)
	      (collect (cons weight (mapcar (lambda (x)
					      (list x (cite-name->id x submit bibitems fetcher)))
					    names))))
	;; KLUDGE to go through papers, which do not use "cite"
	`((1 ("fake" "fake/-1"))))))

(defun check-if-citations-processed (fname)
  (let* ((id-hash (md5sum (fname->id fname)))
	 (sql-str (format nil "select count(*) as count from weighted_cites where referring_paper = \"~a\""
			  (cl-mysql:escape-string id-hash))))
    (with-connection
	(let ((result (execute (prepare *connection* sql-str))))
	  (iter (for res next (let ((it (fetch result)))
				(or it (terminate))))
		(return (not (zerop (getf res :|count|)))))))))

(defun calc-paper-hash (thing)
  (md5sum (if (stringp thing)
	      thing
	      (cond ((eq :fake (car thing)) #?"fake/$((cadr thing))")
		    ((eq :hep-th (car thing)) #?"hep-th/$((cadr thing))")
		    ((eq :arxiv (car thing)) #?"$((cadr thing))")
		    (t (error "Don't know how to calc hash of ~a" thing))))))

(defun %essential-cites-to-sql (fname)
  (let* ((id-hash (md5sum (fname->id fname)))
	 (sql-str (with-output-to-string (stream)
		   (format stream "insert into weighted_cites
                                     (referring_paper, cited_paper, weight)
                                   values ")
		   (iter (for (weight . papers) in (essential-cites-as-ids fname))
			 (iter (for paper in papers)
			       (format stream "(\"~a\", \"~a\", \"~a\"),~%"
				       (cl-mysql:escape-string id-hash)
				       (cl-mysql:escape-string (calc-paper-hash (cadr paper)))
				       (cl-mysql:escape-string (format nil "~a" weight))))))))
    (format t "Sql str is: ~a~%" sql-str)
    (with-connection
	(execute (prepare *connection* (subseq sql-str 0 (- (length sql-str) 2)))))
    :success!))

(defun essential-cites-to-sql (fname)
  (handler-case (%essential-cites-to-sql fname)
    (error (e)
      (with-connection
	  (execute (prepare *connection*
			    (format nil "delete from weighted_cites where referring_paper = \"~a\""
				    (cl-mysql:escape-string (md5sum (fname->id fname)))))))
      (error e))))

    
(defun cites-to-sql-if-not-there (fname)
  (if (not (check-if-citations-processed fname))
      (essential-cites-to-sql fname)
      (format t #?"Skipping $((pathname-name fname)) -- already processed~%")))


(defun year-cites-to-sql (year)
  (iter (for fname in-it (arxiv-id-fnames year))
	(format t #?"Processing $((pathname-name fname))...~%")
	(cites-to-sql-if-not-there fname)))
