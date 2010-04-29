;===================================================================
;load stuf
;====================================================================
(require 'asdf)
(asdf:operate 'asdf:load-op 'fiveam)
(compile-file "../internals/WC-internals")
(load "../internals/WC-internals")
(WC-initialise-internals "../internals/")

;===================================================================
;test body
;====================================================================

(5am:def-suite vote-suite)
(5am:in-suite vote-suite)

(5am:test crypt_decrypt_vote_once
	  (5am:is (eq T
		      (let ((pvd (let ((rct (generate_r_class_and_totient)))
				   (make-private_vote_data
				    :key_pair (generate_keys
					       (rclass_totient-totient rct))
				  :r_class (rclass_totient-r_class rct)
				  :my_vote 0)))
			      (vote 2))
			    (= vote
			      (decrypt_vote_step
			       (crypt_vote_step vote pvd) pvd))))))
(5am:run! 'vote-suite)
