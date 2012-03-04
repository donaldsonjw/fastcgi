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
(module fastcgi.base64
   (export (base64-decode str)))


(define +base64-decode-map+
   (let ((map (make-u8vector 128)))
      ;; add uppercase letters
      (do ((i 0 (+ i 1)))
	  ((= i 26))
	  (u8vector-set! map (+ i 65) i))
      ;; add lowercase letters
      (do ((i 0 (+ i 1)))
	  ((= i 26))
	  (u8vector-set! map (+ i 97) (+ i 26)))
      ;; add numbers
      (do ((i 0  (+ i 1)))
	  ((= i 10))
	  (u8vector-set! map (+ i 48) (+ i 52)))
      ;; add + /
      (u8vector-set! map (char->integer #\+) 62)
      (u8vector-set! map (char->integer #\/) 63)

      ;; to ease implementation add padding character
      (u8vector-set! map (char->integer #\=) 0)
      
      map))

(define (b64-val ch)
   (u8vector-ref +base64-decode-map+ (char->integer ch)))

(define (calc-length q1 q2 q3 q4)
   (cond ((and (char=? q3 #\=)
	      (char=? q4 #\=))
	  1)
	 ((char=? q4 #\=)
	  2)
	 (else
	  3)))

(define (base64-decode-quad q1 q2 q3 q4)
   (let ((res (make-string (calc-length q1 q2 q3 q4)))
	 (v (bit-or (bit-or (bit-or (bit-lsh (b64-val q1) 18)
				       (bit-lsh (b64-val q2) 12))
			      (bit-lsh (b64-val q3) 6))
		     (b64-val q4))))
      (let loop ((i 0)
		 (s 16))
	 (if (= i (string-length res))
	     res
	     (begin
		(string-set! res i (integer->char (bit-and (bit-rsh v s)
							   #xff)))
		(loop (+ i 1)
		      (- s 8)))))
      res))

(define (base64-decode str)
   (let* ((len (string-length str))
	  (res (make-string len)))
      (let loop ((i 0)
		 (j 0))
	 (if (= i len)
	    (string-shrink! res j)
	    (let ((ds (base64-decode-quad (string-ref str i)
					  (string-ref str (+ i 1))
					  (string-ref str (+ i 2))
					  (string-ref str (+ i 3)))))
	       ;(print "ds: " ds)
	       (blit-string! ds 0 res j (string-length ds))
	       (loop (+ i 4)
		     (+ j (string-length ds))))))))
		       