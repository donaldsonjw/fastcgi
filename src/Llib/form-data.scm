;;;; Copyright(c) 2010 Joseph Donaldson(donaldsonjw@yahoo.com) 
;;;; This file is part of Fastcgi.
;;;;
;;;;     Fastcgi is free software: you can redistribute it and/or modify
;;;;     it under the terms of the GNU Lesser General Public License as
;;;;     published by the Free Software Foundation, either version 3 of the
;;;;     License, or (at your option) any later version.
;;;;
;;;;     Fastcgi is distributed in the hope that it will be useful, but
;;;;     WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;;     Lesser General Public License for more details.
;;;;
;;;;     You should have received a copy of the GNU Lesser General Public
;;;;     License along with Fastcgi.  If not, see
;;;;     <http://www.gnu.org/licenses/>.
(module fastcgi.formdata
   (library mail)
   (export
    (x-www-url-encoded-form-data->map str)
    (multipart-form-data->map str boundary)
    (part-name part)
    (part-get-header part name)
    (part-content-type part)
    (part-encoding-type part)
    (part-content-disposition part)
    (part-headers part)))

;;;; form-part

(define (part-content-type part)
   (list-ref part 0))

(define (part-encoding-type part)
   (list-ref part 1))

(define (part-content-disposition part)
   (list-ref part 2))

(define (part-content part)
   (list-ref part 3))

(define (part-headers part)
   (if (> (length part) 4)
       (list-ref part 4)
       '()))

(define (part-name part)
   (let* ((cd (part-content-disposition part))
	  (res (assoc 'name (cadr cd))))
	 (if res
	     (cdr res)
	     "")))

(define (part-get-header part name)
   (let* ((headers (part-headers part))
	  (res (assoc name headers)))
      (if res
	  (cdr res)
	  #f)))



; (define (value-attributes value)
;    (reverse!
;     (fold (lambda (s i) (let ((res (string-split i "=")))
; 			   (if (= (length res) 1)
; 			       (cons (string-trim (car res)) s)
; 			       (cons (cons (string-trim (car res))
; 					   (string-trim (cadr res)))
; 				     s))))
; 	  '()
; 	  (string-split value ";"))))


; (define (header-name header)
;    (let ((cd (assoc "Content-Disposition" header)))
;       (if cd
; 	  (let ((nv (assoc "name" (value-attributes (cdr cd)))))
; 	     (if nv
; 		 (unquote (cdr nv))
; 		 ""))
; 	  "")))

; (define-generic (form-part-name part::form-part)
;    (header-name (form-part-header part)))


; (define-generic (form-part-get-header part::form-part name)
;    (with-access::form-part part (header)
;       (let ((cd (assoc name header)))
; 	 (if cd
; 	     (cdr cd)
; 	     cd))))


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


; (define (read-part boundary #!optional (port (current-input-port)))
;     (let* ((dd-boundary (string-append "--" boundary))
;  	 (len (string-length dd-boundary)))
;        (bind-exit (return)
; 	  (with-output-to-string
; 	     (lambda ()
; 		(let loop ((line (read-line port)))
; 		   (if (eof-object? line)
; 		       (return line)
; 		       (when (not (substring=? dd-boundary line len))
; 			  (print line)
; 			  (loop (read-line port))))))))))
		    

; (define (initial-boundary-found? boundary #!optional
; 			      (port (current-input-port)))
;    (let ((dd-boundary (string-append "--" boundary))
; 	 (line (read-line port)))
;       (string=? dd-boundary line)))
   
; (define (part-fold port boundary seed proc)
;    (if (not (initial-boundary-found? boundary port))
;        (error "part-fold" "initial boundary not found" boundary)
;        (let loop ((part (read-part boundary port))
; 		  (s seed))
; 	  (if (eof-object? part)
; 	      s
; 	      (loop (read-part boundary port)
; 		    (proc s part))))))

; (define-inline (fold proc seed lst)
;    (if (null? lst)
;        seed
;        (fold proc
; 	     (proc seed (car lst))
; 	     (cdr lst))))


; (define (string-trim str)
;         ;;determine start of first non space character
;    (let ((start (let loop ((i 0))
; 		   (if (= i (string-length str))
; 		       i
; 		       (if (not (char=? (string-ref str i) #\space))
; 			   i
; 			   (loop (+ i 1))))))
; 	 ;;determine the last non space character
; 	 (end (let loop ((i (- (string-length str) 1)))
; 		 (if (< i 0)
; 		     0
; 		     (if (not (char=? (string-ref str i) #\space))
; 			 (+ i 1)
; 			 (loop (- i 1)))))))
;       (cond ((and (= start 0)
; 		  (= end (string-length str)))
; 	     str)
; 	    ((= start end)
; 	     "")
; 	    (else
; 	     (substring str start end)))))

; (define (parse-header-entry str)
;    (let* ((nvpair (string-split str ":"))
; 	  (name (string-trim (car nvpair)))
; 	  (value (cadr nvpair)))
;       (cons name
; 	    value)))

   


; (define (read-header #!optional (port (current-input-port)))
;    (let loop ((line (read-line port))
; 	      (res '()))
;       (if (eof-object? line)
; 	  line
; 	  (if (not (string=? "" line))
; 	      (loop (read-line port)
; 		    (cons (parse-header-entry line) res))
; 	      (reverse! res)))))




; (define (unquote str)
;    (let* ((len (string-length str))
; 	  (start (if (char=? (string-ref str 0) #\")
; 		     1 0))
; 	  (end (if (char=? (string-ref str (- len 1))
; 			   #\")
; 		   (- len 1)
; 		   len)))
;       (substring str start end)))
      


 (define (multipart-form-data->map str boundary)
    (map (lambda (p) (cons (part-name p) p))
       (mime-multipart-decode str boundary)))
    
			     
; ;(print (parse-header-entry "Content-Disposition: form-data; name=\"files\"; filename=\"file1.txt\""))

;  #; (print (with-input-from-file "test.data"
; 	    (lambda ()
; 	       (multipart-form-data->map (read-string)
; 					 "AaB03x"))))
