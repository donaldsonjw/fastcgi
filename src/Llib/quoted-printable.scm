(module fastcgi.quoted_printable
   (export (quoted-printable-decode str)))

(define (hexdigit? ch)
   (let ((val (char->integer ch)))
      (or (and (> val 47) ; 0 or greater
	       (< val 58)) ; 9 or less
	  (and (> val 64) ; A or greater
	       (< val 71))))) ; F or less

(define (hexdigit-val ch)
   (let ((val (char->integer ch)))
      (cond ((and (> val 47) ; 0 or greater
		   (< val 58)) ; 9 or less
	     (- val 48))
	    ((and (> val 64) ; A or greater
		  (< val 71)) ; F or less
	     (+ (- val 65) 10))
	    (else
	     (error "hexdigit-val" "non-hexdigit" ch)))))
	   

(define (quoted-printable-decode str)
   (let ((len (string-length str)))
      (let loop ((i 0)
		 (j 0)
		 (res (make-string len)))
	 (if (= i len)
	     (string-shrink! res j)
	     (let ((curr (string-ref str i)))
		(if (char=? curr #\=)
		    (let* ((next1 (string-ref str (+ i 1)))
			   (next2 (string-ref str (+ i 2))))
		       (cond ((and (char=? #\return next1)
				   (char=? #\newline next2))
			      (loop (+ i 3)
				 j
				 res))
			     ((and (hexdigit? next1)
				   (hexdigit? next2))
			      (string-set! res j
					   (integer->char
					    (+ (* (hexdigit-val next1) 16)
					       (hexdigit-val next2))))
			      (loop (+ i 3)
				    (+ j 1)
				    res))
			     (else
			      (error "quoted-printable-decode"
				     "invalid format at character: " i))))
		    (begin
		       (string-set! res j (string-ref str i))
		       (loop (+ i 1)
			     (+ j 1)
			     res))))))))
