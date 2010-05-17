;===================================================================
;voting related functions
;===================================================================

(defun crypt_vote_step (vote pvd)
  (power 
   vote 
   (key_pair-enc_key (private_vote_data-key_pair pvd)) 
   (private_vote_data-r_class pvd)))

(defun decrypt_vote_step (vote pvd)
  (power 
   vote 
   (key_pair-dec_key (private_vote_data-key_pair pvd)) 
   (private_vote_data-r_class pvd)))

(defun create_vote (choice vtd)
  (+ choice (random 
	     (floor (rclass_totient-r_class 
		     (vote_template_data-rclass_totient vtd)))
	     (make-random-state t))))

(defun extract_vote (pvd vtd)
  (mod (private_vote_data-my_vote pvd)
       (length (vote_template_data-choices vtd))))
