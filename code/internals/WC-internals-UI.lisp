;===============================================================================
;ui functions

(defun create_choice_list (&optional (stream t) (choice_list nil) (id 0))
  (let ((choice_name (read-line stream)))
    (if (STRING= "" choice_name)
	choice_list
	(create_choice_list 
	 stream
	 (append choice_list
		 (list 
		  (make-vote_choice_entry 
		   :choice_name choice_name 
		   :choice_id id)))
	 (+ id 1)))))

(defun create_vote_template (&optional (stream t))
  (format t "Please enter the number of voters...~%")
  (let ((num_of_voters (parse-integer (read-line stream))))
    (format t "Please enter the choices, empty entry to finish...~%")
    (make-vote_template_data 
     :rclass_totient (make-rclass_totient 
		      :r_class (* 101 103) 
		      :totient (* 100 102))
     :num_of_voters num_of_voters
     :choices (create_choice_list stream))))

(defun create_private_vote_data (vote rclass max_vote)
  (make-private_vote_data 
   :key_pair (generate_keys (rclass_totient-totient rclass)
   :r_class rclass  
   :my_vote (+ vote 
	       (* max_vote 
		  (random 
		   (/ (rclass_totient-rclass rclass) max_vote)
		   (make-random-state t)))))))
