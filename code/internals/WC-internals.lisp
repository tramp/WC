;===================================================================
;load stuf
;====================================================================

(defun WC-initialise-internals (path-of-internals)
    (asdf:operate 'asdf:load-op 'yason)
    (setq *random-state* (make-random-state t))
    (let ((files (list "WC-internals-crypto.lisp"
		       "WC-internals-data-structures.lisp"
		       "WC-internals-UI.lisp"
		       "WC-internals-voting.lisp")))
      (dolist (f files)
	(load (compile-file (concatenate 'string path-of-internals f))))))
