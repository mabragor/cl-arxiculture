
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
  (destructuring-bind (first second third fourth fifth sixth seventh . rest) cite-lst
    (declare (ignore rest))
    `((3 ,(car first))
      (2 ,(car second) ,(car third))
      (1 ,(car fourth) ,(car fifth))
      (0.5 ,(car sixth) ,(car seventh)))))
	    

  
