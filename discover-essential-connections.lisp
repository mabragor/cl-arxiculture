
(in-package #:cl-arxiculture)

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

(defun permanent-id (str paper-year id-fetcher)
  (or (sloppy-parse-arxiv-id str)
      (let ((year (or (sloppy-parse-year str)
		      paper-year))
	    (authors (sloppy-parse-authors str)))
	(or (cadr (funcall id-fetcher year authors))
	    `(:id ,year ., authors)))))

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

(defun imply-paper-year (fname)
  (let ((pre-year (subseq (pathname-name fname) 0 2)))
    (if (char= #\9 (char pre-year 0))
	#?"19$(pre-year)"
	#?"20$(pre-year)")))

(defun essential-cites-as-ids (fname)
  (let* ((cites (essential-cites (all-cites fname)))
	 ;; (cites-lst (apply #'append (mapcar #'cdr cites)))
	 (bibitems (to-bibitems (extract-bibliography (slurp-file fname))))
	 (fetcher (mk-arxiv-id-fetcher)))
    (iter (for (weight . names) in cites)
	  (collect (cons weight (iter (for name in names)
				      (collect (list name
						     (permanent-id
						      (cadr (assoc name bibitems
								   :test (lambda (x y)
									   (equal x (car y)))))
						      (imply-paper-year fname)
						      fetcher)))))))))


    
