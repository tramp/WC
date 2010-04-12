;===================================================================
;load stuf
;====================================================================
(require 'asdf)
(asdf:operate 'asdf:load-op 'fiveam)
(compile-file "../internals/WC-internals")
(load "../internals/WC-internals")

(5am:def-suite ser-des-suite)
(5am:in-suite ser-des-suite)

(5am:test ser_deser_key_pair
	  (5am:is (eq T (let ((x (make-key_pair :enc_key 123 :dec_key 456)))
			  (equalp x (make-key_pair_from_hashtable  
				     (json:parse 
				      (with-output-to-string (s) 
					(json:encode x s)))))))))

(5am:test ser_deser_rclass_totient
	  (5am:is (eq T (let ((x (make-rclass_totient :r_class 1234 :totient 6789)))
			  (equalp x (rclass_totient_from_hashtable
				     (json:parse 
				      (with-output-to-string (s) 
					(json:encode x s)))))))))

(5am:test ser_deser_vote_choice_entry
	  (5am:is (eq T (let ((x (make-vote_choice_entry :choice_name "das ist meine wahl" :choice_id 1)))
			  (equalp x (vote_choice_entry_from_hashtable
				     (json:parse 
				      (with-output-to-string (s) 
					(json:encode x s)))))))))

(5am:test ser_deser_private_vote_data
	  (5am:is (eq T (let ((x (make-private_vote_data :key_pair (make-key_pair :enc_key 123 :dec_key 456)
							       :r_class 789
							       :my_vote 0)))
			  (equalp x (private_vote_data-from_hashtable
				     (json:parse 
				      (with-output-to-string (s) 
					(json:encode x s)))))))))

(5am:test ser_deser_vote_template_data
  (5am:is (eq T 
	      (let ((x (make-vote_template_data 
			:rclass_totient (make-rclass_totient 
					 :r_class (* 101 103) 
					 :totient (* 100 102))
			:num_of_voters 11
			:choices (list 
				  (make-vote_choice_entry 
				   :choice_name "a"
				   :choice_id 0)
				  (make-vote_choice_entry 
				   :choice_name "b"
				   :choice_id 1)
				  (make-vote_choice_entry 
				   :choice_name "c"
				   :choice_id 2)))))
		(equalp x  (vote_template_data-from_hashtable
			    (json:parse 
			    (with-output-to-string (s) 
			      (json:encode x s)))))))))
(5am:run! 'ser-des-suite)