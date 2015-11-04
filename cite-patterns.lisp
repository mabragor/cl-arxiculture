
(in-package #:cl-arxiculture)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

(defparameter *cite-re* #?/\\cite{([^}]+)}/)

(defun all-cites (fname)
  (let ((res (make-hash-table :test #'equal)))
    (do-register-groups (first) (*cite-re* (slurp-file fname) nil)
      (iter (for elt in (split "," first))
	    (incf (gethash elt res 0))))
    (sort (hash->assoc res) #'> :key #'cdr)))

(defun cite-histograms (&optional (year 1992))
  (iter outer (for fname in (list-directory #?"~/$(year)-bibitems"))
	(let ((it (all-cites fname)))
	  (format t "~{~a~^,~}~%" (mapcar #'cdr it)))))

(defun essential-cites (cite-lst)
  (let (first-weight)
    (iter (for (weight group) in-it (igroupby cite-lst #'cdr))
	  (if-first-time (setf first-weight weight)
			 (if (< weight (/ first-weight 2.7))
			     (terminate)))
	  (let ((it (collect-iter (imap #'car group))))
	    (collect (cons weight it) into res)
	    (summing (* weight (length it)) into total))
	  (finally (return (mapcar (lambda (x)
				     (cons (float (/ (car x) total)) (cdr x)))
				   res))))))


  
