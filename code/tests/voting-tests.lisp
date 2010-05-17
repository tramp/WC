;===================================================================
;load stuf
;====================================================================
(require 'asdf)
(asdf:operate 'asdf:load-op 'fiveam)
(compile-file "../internals/WC-internals")
(load "../internals/WC-internals")
(WC-initialise-internals "../internals/")

(defun generate_r_class_and_totient ()
					;Since the search for big
					;primes is exhousting, and not
					;necessary for WC it is
					;omitted, instead "prominent"
					;ones are used
  (let ((p1 5)
	(p2 7))
    (make-rclass_totient
     :r_class (* p1 p2)
     :totient (* (- p1 1) (- p2 1)))))

;(trace encrypt-by-list decrypt-by-list)

;===================================================================
;test helpers
;====================================================================

(defun create_list_of_key-pairs (num totient &optional (list-of-key-pairs nil))
  (if (= 0 num)
      list-of-key-pairs
      (create_list_of_key-pairs (- num 1) totient
				(append generate_keys totient
					list-of-key-pairs))))

(defun create_list_of_pvds (num rct &optional (pvds nil))
  (if (= 0 num)
      nil
      (append (list (make-private_vote_data
	       :key_pair (generate_keys (rclass_totient-totient rct))
	       :r_class (rclass_totient-r_class rct)
	       :my_vote 2))
	      (create_list_of_pvds
	       (- num 1)
	       rct))))

(defun encrypt-by-list (vote pvds)
  (print vote)
  (if pvds
      (encrypt-by-list (crypt_vote_step vote (first pvds)) (rest pvds))
      vote))

(defun decrypt-by-list (vote pvds)
  (if pvds
      (decrypt-by-list (decrypt_vote_step vote (first pvds)) (rest pvds))
      vote))

						       
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

(5am:test crypt_decrypt_vote_chain
   (5am:is (eq T (let ((rct (generate_r_class_and_totient)))
		  (let ((pvds (create_list_of_pvds 5 rct)))
		    (let ((fpvd (first pvds)))
		      (= (private_vote_data-my_vote fpvd)
			 (decrypt-by-list
			  (encrypt-by-list
			   (private_vote_data-my_vote fpvd)
			   pvds)
			  pvds))))))))

(5am:run! 'vote-suite)
