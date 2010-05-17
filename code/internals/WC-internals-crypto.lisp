;====================================================================
;cryptographic functions
;====================================================================
(defun get_rnd_gcd_one (totient)
					;"chose a invertable number
					;randomly"
  (let ((ret (+ 2 (random (- totient 1)))))
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
