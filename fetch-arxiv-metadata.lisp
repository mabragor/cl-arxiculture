
(in-package #:cl-arxiculture)

;; Note that likely this reader settings applies to entire file not just the package
(quasiquote-2.0:enable-quasiquote-2.0)

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
					      `(,!m(splice-kwds-if-nonnil :metadata-prefix
									  :from
									  :until
									  :set)))
				       (if (not resumption-token)
					   (terminate)
					   (arxiv-list-records :resumption-token ,g!-resumption-token))))))
	    (setf resumption-token nil)
	    (iter (for elt in bunch)
		  (cond ((not elt) (next-iteration))
			((eq :resumption-token (car elt))
			 (if resumption-token
			     (error "Two resumption tokens in a bunch")
			     (setf resumption-token (cadr elt))))
			(t (yield elt))))))))



