(module fastcgi.formdata
   (export
    (class form-part
       header
       data)
    (x-www-url-encoded-form-data->map str)
    (multipart-form-data->map str boundary)
    (generic form-part-name part::form-part)
    (generic form-part-get-header part::form-part name)))

;;;; form-part

(define (value-attributes value)
   (reverse!
    (fold (lambda (s i) (let ((res (string-split i "=")))
			   (if (= (length res) 1)
			       (cons (string-trim (car res)) s)
			       (cons (cons (string-trim (car res))
					   (string-trim (cadr res)))
				     s))))
	  '()
	  (string-split value ";"))))


(define (header-name header)
   (let ((cd (assoc "Content-Disposition" header)))
      (if cd
	  (let ((nv (assoc "name" (value-attributes (cdr cd)))))
	     (if nv
		 (unquote (cdr nv))
		 ""))
	  "")))

(define-generic (form-part-name part::form-part)
   (header-name (form-part-header part)))


(define-generic (form-part-get-header part::form-part name)
   (with-access::form-part part (header)
      (let ((cd (assoc name header)))
	 (if cd
	     (cdr cd)
	     cd))))


(define (hex-char->integer hc)
   (cond ((char=? hc #\0)
	  0)
	 ((char=? hc #\1)
	  1)
	 ((char=? hc #\2)
	  2)
	 ((char=? hc #\3)
	  3)
	 ((char=? hc #\4)
	  4)
	 ((char=? hc #\5)
	  5)
	 ((char=? hc #\6)
	  6)
	 ((char=? hc #\7)
	  7)
	 ((char=? hc #\8)
	  8)
	 ((char=? hc #\9)
	  9)
	 ((or (char=? hc #\a)
	      (char=? hc #\A))
	  10)
	 ((or (char=? hc #\b)
	      (char=? hc #\B))
	  11)
	 ((or (char=? hc #\c)
	      (char=? hc #\C))
	  12)
	 ((or (char=? hc #\d)
	      (char=? hc #\D))
	  13)
	 ((or (char=? hc #\e)
	      (char=? hc #\E))
	  14)
	 ((or (char=? hc #\f)
	      (char=? hc #\F))
	  15)
	 (else
	  (error "hex-char->integer" "Non-hex char" hc))))



(define (url-decode str)
   (let* ((len (string-length str))
	 (res (make-string len)))
      (let loop ((i 0)
		 (j 0))
      (if (= i len)
	  (string-shrink! res j)
	  (cond ((char=? #\% (string-ref str i))
		 (string-set! res j (integer->char
				     (+ (* 16 (hex-char->integer
					       (string-ref str
							   (+ i 1))))
					(hex-char->integer (string-ref
							    str
							    (+ i 2))))))
		 (loop (+ i 3)
		       (+ j 1)))
		((char=? #\+ (string-ref str i))
		 (string-set! res j #\space)
		 (loop (+ i 1)
		       (+ j 1)))
		(else
		 (string-set! res j (string-ref str i))
		 (loop (+ i 1)
		       (+ j 1))))))))


(define (x-www-url-encoded-form-data->map str)
   (map (lambda (nv)
	   (let ((res (string-split nv "=")))
	      (cons (url-decode (car res))
		    (url-decode (cadr res)))))
	(string-split str "&")))


(define (read-part boundary #!optional (port (current-input-port)))
    (let* ((dd-boundary (string-append "--" boundary))
 	 (len (string-length dd-boundary)))
       (bind-exit (return)
	  (with-output-to-string
	     (lambda ()
		(let loop ((line (read-line port)))
		   (if (eof-object? line)
		       (return line)
		       (when (not (substring=? dd-boundary line len))
			  (print line)
			  (loop (read-line port))))))))))
		    

(define (initial-boundary-found? boundary #!optional
			      (port (current-input-port)))
   (let ((dd-boundary (string-append "--" boundary))
	 (line (read-line port)))
      (string=? dd-boundary line)))
   
(define (part-fold port boundary seed proc)
   (if (not (initial-boundary-found? boundary port))
       (error "part-fold" "initial boundary not found" boundary)
       (let loop ((part (read-part boundary port))
		  (s seed))
	  (if (eof-object? part)
	      s
	      (loop (read-part boundary port)
		    (proc s part))))))

(define-inline (fold proc seed lst)
   (if (null? lst)
       seed
       (fold proc
	     (proc seed (car lst))
	     (cdr lst))))


(define (string-trim str)
        ;;determine start of first non space character
   (let ((start (let loop ((i 0))
		   (if (= i (string-length str))
		       i
		       (if (not (char=? (string-ref str i) #\space))
			   i
			   (loop (+ i 1))))))
	 ;;determine the last non space character
	 (end (let loop ((i (- (string-length str) 1)))
		 (if (< i 0)
		     0
		     (if (not (char=? (string-ref str i) #\space))
			 (+ i 1)
			 (loop (- i 1)))))))
      (cond ((and (= start 0)
		  (= end (string-length str)))
	     str)
	    ((= start end)
	     "")
	    (else
	     (substring str start end)))))

(define (parse-header-entry str)
   (let* ((nvpair (string-split str ":"))
	  (name (string-trim (car nvpair)))
	  (value (cadr nvpair)))
      (cons name
	    value)))

   


(define (read-header #!optional (port (current-input-port)))
   (let loop ((line (read-line port))
	      (res '()))
      (if (eof-object? line)
	  line
	  (if (not (string=? "" line))
	      (loop (read-line port)
		    (cons (parse-header-entry line) res))
	      (reverse! res)))))




(define (unquote str)
   (let* ((len (string-length str))
	  (start (if (char=? (string-ref str 0) #\")
		     1 0))
	  (end (if (char=? (string-ref str (- len 1))
			   #\")
		   (- len 1)
		   len)))
      (substring str start end)))
      



(define (multipart-form-data->map str boundary)
   (with-input-from-string str
      (lambda ()
	 (part-fold (current-input-port)
		    boundary
		    '()
		    (lambda (s p)
		       (with-input-from-string p
			  (lambda ()
			     (let* ((header (read-header))
				    (data 
				     (read-string)))
				(cons
				 (cons (header-name header)
				       (instantiate::form-part
					  (header header)
					  (data data)))
				 s)))))))))
			     
   

;(print (parse-header-entry "Content-Disposition: form-data; name=\"files\"; filename=\"file1.txt\""))

 #; (print (with-input-from-file "test.data"
	    (lambda ()
	       (multipart-form-data->map (read-string)
					 "AaB03x"))))
