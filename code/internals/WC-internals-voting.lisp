;===================================================================
;voting related functions
;===================================================================

(defun crypt_vote_step (vote pvd)
  (power 
   vote 
   (key_pair-enc_key (private_vote_data-key_pair)) 
   (rclass_totient-r_class (private_vote_data-r_class))))

(defun decrypt_vote_step (vote pvd)
  (power 
   vote 
   (key_pair-dec_key (private_vote_data-key_pair)) 
   (rclass_totient-r_class (private_vote_data-r_class))))
