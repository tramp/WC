;===================================================================
;load stuf
;====================================================================

(asdf:operate 'asdf:load-op 'yason)

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

  

;====================================================================
;cryptographic functions
;====================================================================
(defun get_rnd_gcd_one (totient)
					;"chose a invertable number
					;randomly"
  (let ((ret (+ 2 (random (- totient 1) (make-random-state t)))))
    (if (= 1 (gcd ret totient))
	ret
	(get_rnd_gcd_one totient))))

(defun gcd_ext (a b)
					;"the extenden euclidean
					;algorithm, recoursively"
  ;(assert (= 1 (gcd a b))) ;costs ~1/3 of runtime
  (assert (> a b))
  (if (=  0 (mod a b) )
      (list 0 1)
      (let ((RES (gcd_ext b (mod a b))))
	(let ((x (first RES))
	      (y (second RES)))
	  (list y (- x (* y (floor a b))))))))

(defun limit (num limiter)
  (if (= 0 limiter)
      num
      (mod num limiter)))

(defun power (base exp &optional (limiter 0) (res 1))
  (if (= exp 0)
      res
      (power (limit (* base base) limiter) 
	   (floor exp 2)
	   limiter
	   (limit (* res (if (= (mod exp 2) 1) base 1)) 
		  limiter))))

(defun inverse (key r_class)
					;"a shorthand for picking the
					;inverse from ext
					;euclidean..."
  (second (gcd_ext key r_class)))

(defun generate_keys (totient)
					;"generate a key pair"
  (let ((a (get_rnd_gcd_one totient)))
    (make-key_pair
     :enc_key (mod (inverse totient a)  totient)
     :dec_key (mod a totient))))
 
(defun generate_r_class_and_totient ()
					;Since the search for big
					;primes is exhousting, and not
					;necessary for WC it is
					;omitted, instead "prominent"
					;ones are used
  (let ((p1 (- (power 2 2281) 1))
	(p2 (- (power 2 3217) 1)))
    (make-rclass_totient
     :r_class (* p1 p2)
     :totient (* (- p1 1) (- p2 1)))))

;===================================================================
;shuffle implementation

(defun shuffle (vote_array)
  (if (= 0 (length vote_array))
      nil
      (let ((idx (random 
		  (length vote_array)
		  (make-random-state t))))
	(let ((vote_chosen (aref vote_array idx)))
	  (let ((last_vote_in_array (vector-pop vote_array)))
	    (if (/= idx (length vote_array))
		(setf (aref vote_array idx) last_vote_in_array )))
	  (append (list vote_chosen) (shuffle vote_array))))))

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
  