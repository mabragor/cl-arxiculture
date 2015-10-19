(in-package :cl-user)

(defpackage :cl-arxiculture-tests
  (:use :cl :cl-arxiculture :fiveam :iterate :cl-read-macro-tokens)
  (:export #:run-tests))

(in-package :cl-arxiculture-tests)

(cl-interpol:enable-interpol-syntax)
(enable-read-macro-tokens)
(quasiquote-2.0:enable-quasiquote-2.0)

(def-suite cl-arxiculture)
(in-suite cl-arxiculture)

(defun run-tests ()
  (let ((results (run 'cl-arxiculture)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test basic
  (is (equal '(("Gross" "D" "J") ("Migdal" "A"))
	     (ac-parse 'comma-and-authors "D.J. Gross and A. Migdal")))
  (is (equal "A nonperturbative treatment of two dimensional quantum gravity"
	     (ac-parse 'it-paper-name
		       "{\\it A nonperturbative treatment
of two dimensional quantum gravity,}")))
  (is (equal "B340" (ac-parse 'bf-journal-spec "{\\bf B340}")))
  (is (equal 1990 (ac-parse 'bracket-year "(1990)")))
  (is (equal 333 (ac-parse 'page-number "333")))
  (is (equal '("Nucl" "Phys") (ac-parse 'simple-journal-name "Nucl. Phys. ")))
  (is (equal '((:AUTHORS (("Gross" "D" "J") ("Migdal" "A")))
	       (:NAME "A nonperturbative treatment of two dimensional quantum gravity")
	       (:JNAME ("Nucl" "Phys")) (:JSPEC "B340") (:YEAR 1990) (:PAGE 333))
	     (ac-parse 'elt-9201003-bibitem
		       "D.J. Gross and A. Migdal, {\\it A nonperturbative treatment
of two dimensional quantum gravity,} Nucl. Phys. {\\bf B340} (1990) 333")))
  (is (equal '((:AUTHORS (("Banks" "T") ("Douglas" "M") ("Seiberg" "N") ("Shenker" "S")))
	       (:NAME "Microscopic and macroscopic loops in nonperturbative two dimensional gravity")
	       (:JNAME ("Phys" "Lett")) (:JSPEC "238B") (:YEAR 1990) (:PAGE 279))
	     (ac-parse 'elt-9201003-bibitem
		       "T.
Banks, M. Douglas, N. Seiberg, and S. Shenker, {\\it Microscopic and macroscopic
loops in nonperturbative two dimensional gravity,} Phys. Lett. {\\bf 238B} (1990)~279")))
  (is (equal '("matrix-kdv"
	       ((:AUTHORS (("Gross" "D" "J") ("Migdal" "A")))
		(:NAME "A nonperturbative treatment of two dimensional quantum gravity")
		(:JNAME ("Nucl" "Phys")) (:JSPEC "B340") (:YEAR 1990) (:PAGE 333))
	       ((:AUTHORS (("Banks" "T") ("Douglas" "M") ("Seiberg" "N") ("Shenker" "S")))
		(:NAME "Microscopic and macroscopic loops in nonperturbative two dimensional gravity")
		(:JNAME ("Phys" "Lett")) (:JSPEC "238B") (:YEAR 1990) (:PAGE 279)))
	     (ac-parse '9201003-bibitem
		       "{matrix-kdv} D.J. Gross and A. Migdal, {\\it A nonperturbative treatment
of two dimensional quantum gravity,} Nucl. Phys. {\\bf B340} (1990) 333.\\\\ T.
Banks, M. Douglas, N. Seiberg, and S. Shenker, {\\it Microscopic and macroscopic
loops in nonperturbative two dimensional gravity,} Phys. Lett. {\\bf 238B} (1990)~279.
  
")))
  (is (equal nil (ac-parse 'simple-surname "Asdf." :junk-allowed t)))
  (is (equal '("Phys" "Lett") (ac-parse 'simple-journal-name "Phys.Lett.B42" :junk-allowed t)))
  (is (equal '("Zuber" "J" "-B") (ac-parse 'simple-author-name "J.-B. Zuber")))
  )
