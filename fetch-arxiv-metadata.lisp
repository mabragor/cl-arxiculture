
(in-package #:cl-arxiculture)

;; Note that likely this reader settings applies to entire file not just the package
(quasiquote-2.0:enable-quasiquote-2.0)
(cl-interpol:enable-interpol-syntax)

;;; create the database and table

(in-package #:cl-arxiv-api)

(use-package '(:cl-itertools))

(defmacro splice-kwd-if-nonnil (name)
  ``,@(if ,name
	  (list ,(intern (string name) "KEYWORD")
		,name)))

(defmacro splice-kwds-if-nonnil (&rest names)
  ``,@`(,,@(mapcar (lambda (x)
		     ``,!m(splice-kwd-if-nonnil ,x))
		   names)))

(defiter ilist-all-records (metadata-prefix &key from until set) ()
  (let (resumption-token)
    (iter (while t)
	  (let ((bunch (parse-oai-pmh-response
			(if-first-time (apply #'arxiv-list-records
					      `(,!m(splice-kwds-if-nonnil metadata-prefix
									  from
									  until
									  set)))
				       (if (not resumption-token)
					   (terminate)
					   (arxiv-list-records :resumption-token resumption-token))))))
	    (setf resumption-token nil)
	    (iter (for elt in bunch)
		  (cond ((not elt) (next-iteration))
			((eq :resumption-token (car elt))
			 (if resumption-token
			     (error "Two resumption tokens in a bunch")
			     (setf resumption-token (cadr elt))))
			(t (yield elt))))))))

;; TODO : maybe, I can write more flexible one, that changes
;;        chunk size dynamically?
;; TODO : special i!- syntax for the things that should be ensured iterators?
(defiter ichunk (size iterable) ()
  (let ((iter (mk-iter iterable)))
    (let (chunk flag)
      (iter (while t)
	    (setf flag nil
		  chunk nil)
	    ;; the order of FOR clauses is important here -- iterator should not be queried in vain
	    (iter (for i from 1 to size)
		  (for elt in-it iter)
		  (if (not flag) (setf flag t))
		  (push elt chunk))
	    (if (not flag)
		(coexit!)
		(yield (nreverse chunk)))))))

(export '(ichunk ilist-all-records))

(in-package #:cl-arxiculture)

(defparameter *sql-header* "
insert ignore into arxiv_metadata
  (id, arxiv_id, authors, authors_hash, submitted)
values")

(defun mk-sql-query (chunk)
  (format nil #?"$(*sql-header*)~%~{~a~^,~%~};" chunk))

(defparameter *connection* nil)

(defmacro with-connection (&body body)
  `(let ((*connection* (connect :mysql :database-name "arxiculture"
				:username "gardener" :password "caramba!")))
     (unwind-protect (progn ,@body)
       (disconnect *connection*))))

;; OK, let's assume this works
(defun md5sum (str)
  (coerce (mapcar #'code-char (coerce (digest-sequence :md5 (string-to-utf-8-bytes str))
				      'list))
		  
	  'string))

  ;; (format nil "~{~2,'0x~}" (coerce (digest-sequence :md5 (string-to-utf-8-bytes str))
  ;; 				   'list)))

(defun string-truncate (str len)
  (if (< (length str) len)
      str
      (subseq str 0 len)))

(defun calc-authors-hash (authors)
  (md5sum (format nil "~{~a~^|~}" (sort (mapcar #'normalize-author authors)
					#'string<))))

(defun comb-for-sql (entry)
  (let ((arxiv-id (get-arxiv-id entry))
	(authors (get-authors entry))
	(date (get-submit-date entry)))
    (let ((id (md5sum arxiv-id))
	  (authors-hash (calc-authors-hash authors)))
      (mapcar #'cl-mysql:escape-string
	      (list id arxiv-id
		    (string-truncate (format nil "~{~a~^|~}" authors) 1000)
		    authors-hash date)))))
    

(defun put-metadata-to-sql (metadata-iter)
  (with-connection
      (iter (for chunk in-it (cl-arxiv-api::ichunk 100
						   (imap (lambda (x)
							   (format nil "(~{'~a'~^, ~})" x))
							 (imap #'comb-for-sql metadata-iter))))
	    (execute (prepare *connection* (mk-sql-query chunk)))))
  :success!)


(defparameter *test-record*
  '((:HEADER (:IDENTIFIER . "oai:arXiv.org:0711.4594")
     (:DATESTAMP . "2015-09-01") (:SET-SPEC . "physics:cond-mat"))
    (:METADATA
     (:DC
      (:TITLE . "P-wave Pairing in Two-Component Fermi Systems with Unequal Population
  near Feshbach Resonance")
      (:CREATOR . "Liao, R.") (:CREATOR . "Popescu, F.") (:CREATOR . "Quader, K.") (:SUBJECT . "Condensed Matter - Superconductivity")
      (:SUBJECT . "Condensed Matter - Other Condensed Matter")
      (:DESCRIPTION . "  We explore p-wave pairing in a single-channel two-component Fermi system with
unequal population near Feshbach resonance. Our analytical and numerical study
reveal a rich superfluid (SF) ground state structure as a function of
imbalance. In addition to the state $\\Delta_{\\pm 1} \\propto Y_{1\\pm 1}$, a
multitude of ``mixed'' SF states formed of linear combinations of $Y_{1m}$'s
give global energy minimum under a phase stability condition; these states
exhibit variation in energy with the relative phase between the constituent gap
amplitudes. States with local energy minimum are also obtained. We provide a
geometric representation of the states. A $T$=0 polarization vs. p-wave
coupling phase diagram is constructed across the BEC-BCS regimes. With
increased polarization, the global minimum SF state may undergo a quantum phase
transition to the local minimum SF state.
")
      (:DESCRIPTION . "Comment: 5 pages, 3 figures") (:DATE . "2007-11-28") (:TYPE . "text") (:IDENTIFIER . "http://arxiv.org/abs/0711.4594")
      (:IDENTIFIER . "Phys. Rev. B 88, 134507 (2013)") (:IDENTIFIER . "doi:10.1103/PhysRevB.88.134507")))))

(defun get-arxiv-id (record)
  (regex-replace-all "^oai:arXiv\\.org:" (cdr (assoc :identifier (cdr (assoc :header record))))
		     ""))


(defun get-submit-date (record)
  (let (min-date)
    (iter (for elt in (cdr (assoc :dc (cdr (assoc :metadata record)))))
	  (if (and (consp elt)
		   (eq :date (car elt)))
	      (if (or (not min-date)
		      (string> min-date (cdr elt)))
		  (setf min-date (cdr elt)))))
    (or min-date
	(cdr (assoc :datestamp (cdr (assoc :header record))))
	"0000-00-00")))

(defun normalize-author (str)
  (let ((parts (split "," str)))
    (if (equal 1 (length parts))
	(string-downcase (string-trim '(#\space) (car parts)))
	(format nil "~a ~a"
		(string-downcase (string-trim '(#\space) (car parts)))
		(do-matches-as-strings (m "[a-zA-Z]" (cadr parts))
		  (return (string-downcase m)))))))


(defun get-authors (record)
  (let ((i 0))
    (iter (for elt in (cdr (assoc :dc (cdr (assoc :metadata record)))))
	  (while (< i 10))
	  (when (and (consp elt)
		     (eq :creator (car elt)))
	    (incf i)
	    (collect (cdr elt))))))
  
(defun query-for-authors (authors)
  (with-connection
      (let* ((sql (prepare *connection* "select * from arxiv_metadata where authors_hash = ?"))
	     (result (execute sql (calc-authors-hash authors))))
	(let ((res (iter (for row next (let ((it (fetch result)))
					 (or it (terminate))))
			 (collect row))))
	  res))))

;; OK, now let's try to query our database

;; Now that putting data from iterator into database works (and also fetching back)
;; I need to write a loop over all dates from the minimal one to the current one
;; (with persistence)

(defun leap-year-p (year)
  (cond ((not (zerop (mod year 4))) nil)
	((not (zerop (mod year 100))) t)
	((not (zerop (mod year 400))) nil)
	(t t)))

(defparameter *days-in-month*
  '((1 . 31) (2 . 28) (3 . 31) (4 . 30) (5 . 31) (6 . 30)
    (7 . 31) (8 . 31) (9 . 30) (10 . 31) (11 . 30) (12 . 31)))

(defparameter *date-re* "^(\\d{4})-(\\d{2})-(\\d{2})$")

(defun parse-date (str)
  (register-groups-bind (y m d) (*date-re* str)
    (mapcar #'parse-integer (list y m d))))

(defun days-in-month (month year)
  (let ((naive (cdr (assoc month *days-in-month* :test #'equal))))
    (if (and (leap-year-p year)
	     (equal 2 month))
	(1+ naive)
	naive)))
      

(defiter date-iter (start) ()
  (destructuring-bind (start-y start-m start-d) (if (stringp start)
						    (parse-date start)
						    start)
    (iter (for year from start-y)
	  (if-first-time (iter (for month from start-m to 12)
			       (if-first-time (iter (for day from start-d to (days-in-month month year))
						    (yield (list year month day)))
					      (iter (for day from 1 to (days-in-month month year))
						    (yield (list year month day)))))
			 (iter (for month from 1 to 12)
			       (iter (for day from 1 to (days-in-month month year))
				     (yield (list year month day))))))))
  
(defparameter *arxiv-load-lock-fname* "~/quicklisp/local-projects/cl-arxiculture/load.lock")

(defun load-date-from-lockfile ()
  (if (probe-file *arxiv-load-lock-fname*)
      (with-open-file (stream *arxiv-load-lock-fname*)
	(parse-date (read-line stream)))))

(defun yesterday (date)
  (destructuring-bind (y m d) date
    (if (> d 1)
	(list y m (1- d))
	(if (> m 1)
	    (list y (1- m) (days-in-month (1- m) y))
	    (list (1- y) 12 (days-in-month 12 (1- y)))))))

(defun tomorrow (date)
  (destructuring-bind (y m d) date
    (if (not (equal (days-in-month m y) d))
	(list y m (1+ d))
	(if (not (equal 12 m))
	    (list y (1+ m) 1)
	    (list (1+ y) 1 1)))))
  

(defun fetch-date-from-arxiv ()
  (let ((response (cl-arxiv-api::parse-oai-pmh-response (cl-arxiv-api::arxiv-identify))))
    (yesterday (parse-date (cdr (assoc :earliest-datestamp response))))))

(defun fetch-all-metadata (&optional (stop-date "2015-10-25"))
  (let ((start-date (or ;; (list 2015 08 31)
			(load-date-from-lockfile)
			(fetch-date-from-arxiv))))
    (flet ((save-date (txt-date)
	     (with-open-file (stream #?"$(*arxiv-load-lock-fname*).new"
				     :direction :output :if-exists :supersede)
	       (format stream "~a" txt-date))
	     (err-script #?"mv $(*arxiv-load-lock-fname*).new $(*arxiv-load-lock-fname*) && sync")))
      (iter (for (y m d) in-it (date-iter (tomorrow start-date)))
	    (let ((txt-date (format nil "~4,'0d-~2,'0d-~2,'0d" y m d)))
	      (if (not (string< txt-date stop-date))
		  (return))
	      (format t "Fetching metadata for ~a~%" txt-date)
	      (handler-case
		  (put-metadata-to-sql
		   (cl-arxiv-api::ilist-all-records "oai_dc"
						    :from txt-date :until txt-date
						    :set "physics:hep-th"))
		(cl-arxiv-api::no-records-match ()
		  (with-open-file (stream #?"~/quicklisp/local-projects/cl-arxiculture/no-recs.txt"
					  :direction :output :if-exists :append :if-does-not-exist :create)
		    (format stream "~a~%" txt-date))))
	      (save-date txt-date))))))
