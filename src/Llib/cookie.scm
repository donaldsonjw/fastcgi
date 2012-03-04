;;;; Copyright(c) 2011, 2012 Joseph Donaldson(donaldsonjw@yahoo.com) 
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
(module fastcgi.cookie
   (export
    (class cookie
       name::bstring
       (value::bstring (default ""))
       (path::bstring (default ""))
       (domain::bstring (default ""))
       (max-age (default #unspecified))
       (expires (default #unspecified))
       (secure::bbool (default #f))
       (httponly::bool (default #f)))
    (cookie-serialize c::cookie)
    (cookies-parse str)))




(define (cookie-serialize c::cookie)
   (with-output-to-string
      (lambda ()
	 (display "Set-Cookie: ")
	 (printf "~a=~a;" (->  c name) (-> c value))
	 (when (not (string=? (-> c path) ""))
	    (printf "Path=~a; " (-> c path)))
	 (when (not (string=? (-> c domain ) ""))
	    (printf "Domain=~a; " (-> c domain)))
	 (when (not (eq? #unspecified (-> c expires)))
	    (printf "Expires=~a; " (date->http-date-string (-> c expires))))
	 (when (not (eq? #unspecified (-> c max-age)))
	    (printf "Max-Age=~a; "  (-> c max-age)))
	 (when (-> c secure)
	    (printf "Secure; "))
	 (when (-> c httponly)
	    (printf "HttpOnly; ")))))

(define-generic (object-display c::cookie . op)
   (let ((port (if (pair? op) 
                   (car op) 
                   (current-output-port))))
      (display "Set-Cookie: ")
      (printf "~a=~a;" (-> c name) (-> c value))
      (when (not (string=? (-> c path) ""))
	 (printf "Path=~a; " (-> c path)))
      (when (not (string=? (-> c domain) ""))
	 (printf "Domain=~a; " (-> c domain)))
      (when (not (eq? #unspecified (-> c expires)))
	 (printf "Expires=~a; " (date->http-date-string (-> c expires))))
      (when (not (eq? #unspecified (-> c max-age)))
	 (printf "Max-Age=~a; "  (-> c max-age)))
      (when (-> c secure)
	 (printf "Secure; "))
      (when (-> c httponly)
	 (printf "HttpOnly; "))))



(define (date->http-date-string date)
   (let ((gmt-date (seconds->date (+ (date->seconds date) (date-timezone date)))))
      (format "~a, ~2,0d ~a ~a ~2,0d:~2,0d:~2,0d GMT"
	 (day-aname (date-wday gmt-date))
	 (date-day gmt-date)
	 (month-aname (date-month gmt-date))
	 (date-year gmt-date)
	 (date-hour gmt-date)
	 (date-minute gmt-date)
	 (date-second gmt-date))))


;;;; cookie parsing 

(define-struct parse-result
   successful?
   value
   remainder)

(define (invalid-cookie-name-char? c)
   (find (lambda (x) (char=? x c))
      '(#a000 #a001 #a002 #a003 #a004 #a005 #a006
	#a007 #a008 #a009 #a010 #a011 #a012 #a013
	#a014 #a015 #a016 #a017 #a018 #a019 #a020
	#a021 #a022 #a023 #a024 #a025 #a026 #a027
	#a028 #a029 #a030 #a031 #\space #\tab #\(
	#\) #\< #\> #\@ #\, #\; #\: #\\ #\" #\/
	#\[ #\] #\? #\= #\{ #\})))

(define (invalid-cookie-value-char? c)
   (find (lambda (x) (char=? x c))
      '(#a000 #a001 #a002 #a003 #a004 #a005 #a006
	#a007 #a008 #a009 #a010 #a011 #a012 #a013
	#a014 #a015 #a016 #a017 #a018 #a019 #a020
	#a021 #a022 #a023 #a024 #a025 #a026 #a027
	#a028 #a029 #a030 #a031 #\space #\" #\,
	#\; #\\)))

(define (invalid-delimiter-char? c)
   (not (find (lambda (x) (char=? x c))
	   '(#\; #\space #\tab))))

(define (parse-cookie-name str)
   (if (> (string-length str) 0)
       (let ((res (let loop ((i 0))
		     (if (or (>= i (string-length str))
			     (invalid-cookie-name-char? (string-ref str i)))
			 (cons (substring str 0 i) (substring str i))
			 (loop (+ i 1))))))
	  (if (string=? (car res) "")
	      (parse-result #f "" str)
	      (parse-result #t (car res) (cdr res))))
       (parse-result #f "" str)))

(define (parse-cookie-value str)
   (if (> (string-length str) 0)
       (let ((res (let loop ((i 0))
		     (if (or (>= i (string-length str))
			     (invalid-cookie-value-char? (string-ref str i)))
			 (cons (substring str 0 i) (substring str i))
			 (loop (+ i 1))))))
	  (if (string=? (car res) "")
	      (parse-result #f "" str)
	      (parse-result #t (car res) (cdr res))))
       (parse-result #f "" str)))

(define (parse-equal str)
   (if (and (> (string-length str) 0) (char=? (string-ref str 0)
	       #\=))
       (parse-result #t "=" (substring str 1))
       (parse-result #f "" str)))


(define (parse-cookie-pair str)
   (let ((r1 (parse-cookie-name str)))
        (if (parse-result-successful? r1)
	    (let ((r2 (parse-equal (parse-result-remainder r1))))
	       (if (parse-result-successful? r2)
		   (let ((r3 (parse-cookie-value (parse-result-remainder r2))))
		      (if (parse-result-successful? r3)
			  (parse-result #t (cons (parse-result-value r1)
					      (parse-result-value r3))
			     (parse-result-remainder r3))
			  r3))
		   r2))
	    r1)))

(define (parse-delimiter str)
   (if (> (string-length str) 0)
       (let ((res (let loop ((i 0))
		     (if (or (>= i (string-length str))
			     (invalid-delimiter-char? (string-ref str i)))
			 (cons (substring str 0 i) (substring str i))
			 (loop (+ i 1))))))
	  (if (string=? (car res) "")
	      (parse-result #f "" str)
	      (parse-result #t (car res) (cdr res))))
       (parse-result #f "" str)))

(define (white-space-char? c)
   (find (lambda (x) (char=? x c))
      '(#\space #\tab)))

(define (trim-whitespace str)
   (let ((start (let loop ((i 0))
		   
		   (if (and (< i (string-length str))
			    (white-space-char? (string-ref str i)))
		       (loop (+ i 1))
		       i)))
	 (stop (let loop ((i (- (string-length str) 1)))
		  (if (and (>= i 0)
			   (white-space-char? (string-ref str i)))
		      (loop (- i 1))
		      (+ i 1)))))
      (if (<= stop start)
	  ""
	  (substring str start stop))))

;;; parse a raw cookie into an association list of cookie name-value pairs
(define (cookies-parse str)
   (let ((str (trim-whitespace str)))
      (let loop ((r (parse-cookie-pair str))
		 (cookies '()))
	 (if (parse-result-successful? r)
	     (if (> (string-length (parse-result-remainder r)) 0)
		 (let ((rd (parse-delimiter (parse-result-remainder r))))
		    (if (and (parse-result-successful? rd)
			     (> (string-length (parse-result-remainder rd)) 0))
			(loop (parse-cookie-pair (parse-result-remainder rd))
			   (cons (cons (car (parse-result-value r))
				    (cdr (parse-result-value r)))
			      cookies))
			(cons (cons (car (parse-result-value r))
				 (cdr (parse-result-value r)))
			   cookies)))
		 (cons  (cons (car (parse-result-value r))
			   (cdr (parse-result-value r)))
		    cookies))
	     (error "parse-cookies" "parse error"
		(parse-result-remainder r))))))
		     
   




;;;; cookie parsing tests


;(cookies-parse "SID=31d4d96e407aad42")
;(cookies-parse "SID=31d4d96e407aad42; lang=en-US")
