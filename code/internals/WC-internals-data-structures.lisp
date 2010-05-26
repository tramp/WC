;====================================================================
;Data structures
;====================================================================

(defstruct private_vote_data
  (:print-function (lambda (pkfd stream depth)
		     (format stream "~a~%" (private_vote_data-key_pair pkfd))
		     (format stream "~a~%" (private_vote_data-r_class pkfd))
		     (format stream "~a~%" (private_vote_data-my_vote pkfd))))
  key_pair r_class  my_vote)

(defmethod json:encode ((pvd private_vote_data)
			&optional (stream *standard-output*))
  (json:with-output (stream)
    (json:with-object ()
      (json:encode-object-element "key_pair" (private_vote_data-key_pair pvd))
      (json:encode-object-element "r_class" (private_vote_data-r_class pvd))
      (json:encode-object-element "my_vote" (private_vote_data-my_vote pvd)))))

(defun private_vote_data-from_hashtable (ht)
  (make-private_vote_data
     :key_pair (make-key_pair_from_hashtable (gethash "key_pair" ht))
     :r_class (gethash "r_class" ht)
     :my_vote (gethash "my_vote" ht)))

(defmethod make-key_pair_from_hashtable (ht)
  (make-key_pair
   :enc_key (gethash "enc_key" ht)
   :dec_key (gethash "dec_key" ht)))

(defstruct 
    (key_pair 
      (:print-function (lambda (kp stream depth)
			 (format stream "~a~%~a" 
				 (key_pair-enc_key kp)
				 (key_pair-dec_key kp)))))
      enc_key dec_key)

(defmethod json:encode ((kp key_pair)
			&optional (stream *standard-output*))
  (json:with-output (stream)
    (json:with-object ()
      (json:encode-object-element "enc_key" (key_pair-enc_key kp))
      (json:encode-object-element "dec_key" (key_pair-dec_key kp)))))

(defstruct 
    (rclass_totient
      (:print-function (lambda (rt stream depth)
			 (format stream "~a~%" (rclass_totient-r_class rt))
			 (format stream "~a" (rclass_totient-totient rt)))))
  r_class totient)


(defmethod rclass_totient_from_hashtable (ht)
  (make-rclass_totient
   :totient (gethash "totient" ht)
   :r_class (gethash "r_class" ht)))

(defmethod json:encode ((rc rclass_totient)
			&optional (stream *standard-output*))
  (json:with-output (stream)
    (json:with-object ()
      (json:encode-object-element "r_class" (rclass_totient-r_class rc))
      (json:encode-object-element "totient" (rclass_totient-totient rc)))))

(defstruct 
    (vote_choice_entry 
      (:print-function 
       (lambda (vce stream depth)
	 (format stream "~a     ~a" 
		 (vote_choice_entry-choice_name vce)
		 (vote_choice_entry-choice_id vce)))))
  choice_name choice_id)

(defmethod vote_choice_entry_from_hashtable (ht)
  (make-vote_choice_entry 
   :choice_name (gethash "choice_name" ht)
   :choice_id (gethash "choice_id" ht)))

(defmethod json:encode ((vce vote_choice_entry )
			&optional (stream *standard-output*))
  (json:with-output (stream)
    (json:with-object ()
      (json:encode-object-element 
       "choice_name" 
       (vote_choice_entry-choice_name vce))
      (json:encode-object-element 
       "choice_id" 
       (vote_choice_entry-choice_id vce)))))

(defstruct 
    (vote_template_data
      (:print-function 
       (lambda (vtd stream depth)
	 (format stream "~a~%" (vote_template_data-rclass_totient vtd))
	 (dolist (item (vote_template_data-choices vtd))
	   (format stream "~a~%" item))
	 (format stream "~a" (vote_template_data-num_of_voters vtd)))))
  rclass_totient choices num_of_voters)

(defun work_choice_list (todo)
  (if todo
      (append
        (list (vote_choice_entry_from_hashtable (first todo)))
	(work_choice_list (rest todo)))))

(defmethod vote_template_data-from_hashtable (ht)
  (make-vote_template_data
   :rclass_totient (rclass_totient_from_hashtable (gethash "rclass_totient" ht))
   :num_of_voters (gethash "num_of_voters" ht)
   :choices (work_choice_list (gethash "choices" ht))))

(defmethod json:encode ((vtd vote_template_data)
			&optional (stream *standard-output*))
  (json:with-output (stream)
    (json:with-object ()
      (json:encode-object-element 
       "rclass_totient"
       (vote_template_data-rclass_totient vtd))
      (json:with-object-element ("choices")
	(json:with-array ()
	  (dolist (item (vote_template_data-choices vtd))
	    (json:encode-array-element item))))
      (json:encode-object-element 
       "num_of_voters" (vote_template_data-num_of_voters vtd)))))
