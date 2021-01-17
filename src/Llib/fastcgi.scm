;;;; Copyright(c) 2010-2021 Joseph Donaldson(donaldsonjw@yahoo.com) 
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
(module fastcgi.fastcgi
   (import fastcgi.formdata)
   (import fastcgi.cookie)
   
   (extern
    (include "cfastcgi.h")
    (macro %get-fast-cgi-socket::obj ()
	   "get_fastcgi_socket"))

   (static
    (abstract-class fastcgi-record
       version
       type
       request-id)

    (abstract-class fastcgi-name-values::fastcgi-record
       name-values)
    
    (abstract-class fastcgi-stream::fastcgi-record
       data)
    
    (class fastcgi-begin-request::fastcgi-record
       role
       keep-connection?)
    
    (class fastcgi-end-request::fastcgi-record
       app-status
       protocol-status)
    
    (class fastcgi-abort-request::fastcgi-record)
    
    (class fastcgi-get-values::fastcgi-name-values)
    (class fastcgi-get-values-result::fastcgi-name-values)
    (class fastcgi-params::fastcgi-name-values)
    (class fastcgi-stdin::fastcgi-stream)
    (class fastcgi-stdout::fastcgi-stream)
    (class fastcgi-stderr::fastcgi-stream)
    (class fastcgi-data::fastcgi-stream)
    (class fastcgi-unknown-type::fastcgi-record
       unknown-type))
   
   (export
    (abstract-class fastcgi-request
       input
       output
       error
       env
       (params (default #unspecified))
       (parts (default #unspecified))
       id
       keep-connection?
       socket)
    (class fastcgi-responder-request::fastcgi-request)
    (class fastcgi-authorizer-request::fastcgi-request)
    (class fastcgi-filter-request::fastcgi-request
       data)
    (fastcgi-out request::fastcgi-request content::bstring #!optional (headers '(("Content-Type" . "text/html"))))
						       
    (generic fastcgi-finish request::fastcgi-request)
    (fastcgi-for-each proc)
    (generic fastcgi-request-getenv request::fastcgi-request
					name::bstring)
    (generic fastcgi-request-get-param request::fastcgi-request
				   name)
    (generic fastcgi-request-get-part reguest::fastcgi-request
	     name)
    (generic fastcgi-request-get-cookies request::fastcgi-request)))


;;; SIGNAL definitions
(define +sigterm+ (pragma::int "SIGTERM"))
(define +sigpipe+ (pragma::int "SIGPIPE"))
(define +sigusr1+ (pragma::int "SIGUSR1"))

;;; fastcgi protocol version
(define +fastcgi-protocol-version+ 1)

;;; protocol status codes
(define +fastcgi-status-request-complete+ 0)
(define +fastcgi-status-cant-mpx-connections+ 1)
(define +fastcgi-status-overloaded+ 2)
(define +fastcgi-status-unknown-role+ 3)

;;; fastcgi record types
(define +fastcgi-record-type-name-map+
   '((1 . 'fastcgi-begin-request)
     (2 . 'fastcgi-abort-request)
     (3 . 'fastcgi-end-request)
     (4 . 'fastcgi-params)
     (5 . 'fastcgi-stdin)
     (6 . 'fastcgi-stdout)
     (7 . 'fastcgi-stderr)
     (8 . 'fastcgi-data)
     (9 . 'fastcgi-get-values)
     (10 . 'fastcgi-get-values-result)
     (11 . 'fastcgi-unknown-type)))

(define +fastcgi-begin-request-type+ 1)
(define +fastcgi-abort-request-type+ 2)
(define +fastcgi-end-request-type+ 3)
(define +fastcgi-params-type+ 4)
(define +fastcgi-stdin-type+ 5)
(define +fastcgi-stdout-type+ 6)
(define +fastcgi-stderr-type+ 7)
(define +fastcgi-data-type+ 8)
(define +fastcgi-get-values-type+ 9)
(define +fastcgi-get-values-result-type+ 10)
(define +fastcgi-unknown-type+ 11)

;;; role types
(define +fastcgi-role-responder+ 1)
(define +fastcgi-role-authorizer+ 2)
(define +fastcgi-role-filter+ 3)


;;; records with a request-id of 0 are management records
(define (management-record? rec::fastcgi-record)
   (= (-> rec request-id) 0))

;;; functions to read fastcgi records
(define (read-version port)
   (read-byte port))

(define (read-type port)
   (read-byte port))

(define (read-request-id port)
   (let* ((b1 (read-byte port))
	  (b0 (read-byte port)))
      (bit-or (bit-lsh b1 8) b0)))

(define (read-content-length port)
   (let* ((b1 (read-byte port))
	  (b0 (read-byte port)))
      (bit-or (bit-lsh b1 8) b0)))

(define (read-padding-length port)
   (read-byte port))

(define (read-reserved port)
   (read-byte port))

(define (read-fastcgi-request-record port)
   (let* ((version (read-version port))
	  (type (read-type port))
	  (request-id (read-request-id port))
	  (content-length (read-content-length port))
	  (padding-length (read-padding-length port))
	  (reserved (read-reserved port))
	  (content (read-chars content-length port))
	  (padding (read-chars padding-length port)))
      (fastcgi-record-factory-create version
				     type
				     request-id
				     content)))


(define (read-begin-request-role port)
   (let* ((r1 (read-byte port))
	 (r0 (read-byte port)))
      (bit-or (bit-lsh r1 8) r0)))

(define (read-begin-request-flags port)
   (read-byte port))

(define (read-begin-request-reserved port)
   (read-chars 5 port))

(define (read-begin-request-contents port)
   (let* ((role (read-begin-request-role port))
	  (flags (read-begin-request-flags port))
	  (reserved (read-begin-request-reserved port)))
      (values role flags reserved)))

;; read-name-length
(define (read-nv-length port)
   (let* ((b0 (read-byte port)))
      (if (= (bit-rsh b0 7) 0)
	  b0
	  (let ((b1 (read-byte port))
		(b2 (read-byte port))
		(b3 (read-byte port)))
	     (bit-or (bit-lsh (bit-and b0 #x7f) 24) (bit-or (bit-lsh b1 16) (bit-or (bit-lsh b2 8) b3)))))))

(define (read-name-value port)
   (let* ((name-length (read-nv-length port))
	  (value-length (read-nv-length port))
	  (name (read-chars name-length port))
	  (value (read-chars value-length port)))
      (cons name value)
      ))

(define (read-name-values port)
   (let loop ((pb (peek-byte port))
	      (res '()))
      (if (eof-object? pb)
	  res
	  (let ((nv (read-name-value port)))
	     (loop (peek-byte port)
		   (cons nv res))))))


;;; factory function for creating  instances of the various
;;; fastcgi request record types from request data
(define (fastcgi-record-factory-create version type request-id content)
   (cond ((= +fastcgi-begin-request-type+ type)
	  (receive (role flags reserved)
	     (with-input-from-string content
		(lambda () (read-begin-request-contents (current-input-port))))
	     (instantiate::fastcgi-begin-request (version version)
						 (type type)
						 (request-id request-id)
						 (role role)
						 (keep-connection?
						  (= (bit-and flags 1) 1)))))
	 ((= +fastcgi-abort-request-type+ type)
	  (instantiate::fastcgi-abort-request (version version)
					      (type type)
					      (request-id request-id)))
      
	 ((or (= +fastcgi-params-type+ type)
	      (= +fastcgi-get-values-type+ type))
	  (let ((nvs (with-input-from-string content
			(lambda ()
			   (read-name-values (current-input-port))))))
	     (cond ((= +fastcgi-params-type+ type)
		    (instantiate::fastcgi-params (version version)
						 (type type)
						 (request-id request-id)
						 (name-values nvs)))
		   ((= +fastcgi-get-values-type+ type)
		    (instantiate::fastcgi-get-values (version version)
						     (type type)
						     (request-id request-id)
						     (name-values nvs))))))
	 ((= +fastcgi-stdin-type+ type)
	  (instantiate::fastcgi-stdin (version version)
				      (type type)
				      (request-id request-id)
				      (data content)))
	 ((= +fastcgi-data-type+ type)
	  (instantiate::fastcgi-data (version version)
				     (type type)
				     (request-id request-id)
				     (data content)))
	 (else
	  (error "fastcgi-record-factory-create"
		 "Unknown fastcgi request record type"
		 type))))

       

;;; fastcgi-record-serialize

;;; helper functions for serializing fastcgi-records
(define (write-version version port)
   (write-byte version port))
(define (write-type type port)
   (write-byte type port))
(define (write-request-id request-id port)
   (let ((b1 (bit-and (bit-rsh request-id 8) #xff))
	 (b0 (bit-and request-id #xff)))
      (write-byte b1 port)
      (write-byte b0 port)))
(define (write-content-length content-length port)
   (let ((b1 (bit-and (bit-rsh content-length 8) #xff))
	 (b0 (bit-and content-length #xff)))
      (write-byte b1 port)
      (write-byte b0 port)))
(define (write-padding-length padding-length port)
   (write-byte padding-length port))
(define (write-reserved port)
   (write-byte 0 port))

(define-generic (fastcgi-record-serialize record::fastcgi-record)
   (with-output-to-string
    (lambda ()
       (with-access::fastcgi-record record (version type request-id)
	  (write-version version (current-output-port))
	  (write-type type (current-output-port))
	  (write-request-id request-id (current-output-port))))))

(define (calculate-padding-length data-length)
   (let ((rem (modulo data-length 8)))
      (if (= rem 0)
	  0
	  (- 8 rem))))

(define-method (fastcgi-record-serialize record::fastcgi-stream)
   (string-append (call-next-method)
		  (with-output-to-string
		     (lambda ()
			(with-access::fastcgi-stream record (data)
			   (let* ((data-length (string-length data))
				  (padding-length
				   (calculate-padding-length data-length)))
			      (write-content-length data-length
						    (current-output-port))
			      (write-padding-length padding-length
						    (current-output-port))
			      (write-reserved (current-output-port))
			      (display data (current-output-port))
			      (display (make-string padding-length #a000)
				       (current-output-port))))))))

(define (write-app-status app-status port)
   (let ((as3 (bit-and (bit-rsh app-status 24) #xff))
	 (as2 (bit-and (bit-rsh app-status 16) #xff))
	 (as1 (bit-and (bit-rsh app-status 8) #xff))
	 (as0 (bit-and app-status #xff)))
      (write-byte as3 port)
      (write-byte as2 port)
      (write-byte as1 port)
      (write-byte as0 port)))

(define (write-protocol-status protocol-status port)
   (write-byte protocol-status port))

(define (write-end-request-reserved port)
   (let ((reserved (make-string 3 #a000)))
      (display reserved port)))

(define-method (fastcgi-record-serialize record::fastcgi-end-request)
   (string-append (call-next-method)
		  (with-output-to-string
		     (lambda ()
			(with-access::fastcgi-end-request record
			      (app-status protocol-status) 
			   (write-content-length 8 (current-output-port))
			   (write-padding-length 0 (current-output-port))
			   (write-reserved (current-output-port))
			   (write-app-status app-status (current-output-port))
			   (write-protocol-status protocol-status
						  (current-output-port))
			   (write-end-request-reserved (current-output-port)))))))

(define (write-unknown-type unknown-type port)
   (write-byte unknown-type port))

(define (write-unknown-type-reserved port)
   (let ((reserved (make-string 7 #a000)))
      (display reserved port)))

(define-method (fastcgi-record-serialize record::fastcgi-unknown-type)
   (string-append (call-next-method)
		  (with-output-to-string
		     (lambda ()
			(with-access::fastcgi-unknown-type record (unknown-type)
			   (write-content-length 8 (current-output-port))
			   (write-padding-length 0 (current-output-port))
			   (write-reserved (current-output-port))
			   (write-unknown-type unknown-type
					       (current-output-port))
			   (write-unknown-type-reserved
			    (current-output-port)))))))

(define (serialize-short-length len)
   (with-output-to-string
      (lambda ()
	 (write-byte len))))

(define (serialize-long-length len)
   (let ((b3 (bit-and (bit-rsh len 24) #xff))
	 (b2 (bit-and (bit-rsh len 16) #xff))
	 (b1 (bit-and (bit-rsh len 8) #xff))
	 (b0 (bit-and len #xff)))
      (with-output-to-string
	 (lambda ()
	    (write-byte b3)
	    (write-byte b2)
	    (write-byte b1)
	    (write-byte b0)))))
	     
(define (serialize-name-value p)
   (let* ((name (car p))
	  (value (cdr p))
	  (name-length (string-length name))
	  (value-length (string-length name)))
      (string-append (if (> name-length 127)
			 (serialize-long-length name-length)
			 (serialize-short-length name-length))
		     (if (> value-length 127)
			 (serialize-long-length value-length)
			 (serialize-short-length value-length))
		     name
		     value)))
      
      
(define (serialize-name-values name-values)
   (let loop ((nvs name-values)
	      (res ""))
      (if (pair? nvs)
	  (loop (cdr nvs) (string-append (serialize-name-value (car nvs))
					res))
	  res)))

;;;; ***WARNING*** This method assumes that the serialized list of name value
;;;; pairs will not exceed the size constraints of a single fastcgi-record
;;;; This appears to be a fairly safe assumption since the only use of
;;;; a record class derived from the fastcgi-name-values class is
;;;; fastcgi-get-values-result. It is used when responding to
;;;; fastcgi-get-values management records, and only three configuration
;;;; parameters are defined in the fastcgi specification: FCGI_MAX_CONNS
;;;; FCGI_MAX_REQS, and FCGI_MPXS_CONNS. This should result in records well
;;;; below the maximum size constraints of a fastcgi record. Further, it
;;;; does not appear that either the mod_fastcgi or mod_fcgi modules send
;;;; fastcgi-get-values request records.
(define-method (fastcgi-record-serialize record::fastcgi-name-values)
   (string-append (call-next-method)
		  (with-output-to-string
		     (lambda ()
			(with-access::fastcgi-name-values record (name-values)
			   (let* ((serialized-nvs  (serialize-name-values
						    name-values))
				  (serialized-nvs-length (string-length
							  serialized-nvs))
				  (padding-length (calculate-padding-length
						   serialized-nvs-length)))
			      (write-content-length serialized-nvs-length
						    (current-output-port))
			      (write-padding-length padding-length
						    (current-output-port))
			      (write-reserved (current-output-port))
			      (display serialized-nvs (current-output-port))))))))
			      

;;; fastcgi output
(define (create-output-stream-record type request-id data)
   (cond ((= type +fastcgi-stdout-type+)
	  (instantiate::fastcgi-stdout (version +fastcgi-protocol-version+)
				       (type type)
				       (request-id request-id)
				       (data data)))
	 ((= type +fastcgi-stderr-type+)
	  (instantiate::fastcgi-stderr (version +fastcgi-protocol-version+)
				       (type type)
				       (request-id request-id)
				       (data data)))
	 (else
	  (error "create-output-stream-record" "Unknown record type" type))))

(define +max-stream-length+ #xffff)

(define (create-fastcgi-output-port fastcgi-socket type request-id)
   (let ((buffer (make-string +max-stream-length+))
	 (curr-length 0)
	 (port (socket-output fastcgi-socket)))
      (letrec ((flush (lambda ()
			 (when (> curr-length 0)
			    (display (fastcgi-record-serialize
				      (create-output-stream-record
				       type
				       request-id
				       (substring buffer 0 curr-length))) port)
			    (flush-output-port port)
			    (set! curr-length 0))))
		      (write (lambda (str)
			 (let ((remaining-space
				(- (string-length buffer) curr-length)))
			    (if (< (string-length str) remaining-space)
				(begin
				   (blit-string! str 0 buffer
						 curr-length
						 (string-length str))
				   (set! curr-length (+ curr-length
							(string-length str))))
				(begin
				   (blit-string! str 0 buffer
						 curr-length remaining-space)
				   (flush)
				   (write (substring str remaining-space)))))))
	       (close (lambda ()
			      (flush)
			      ;; signal end of stream
			      (display (fastcgi-record-serialize
					(create-output-stream-record
					 type
					 request-id
					 ""))
				       port)
			      (flush-output-port port)
			      )))
	 (open-output-procedure write flush #t close))))
	       

(define +end-fastcgi-request-loop+ #f)

;;; fastcgi request loop

;; create the env for the fastcgi-responder request from the collected
;; fastcgi param records
(define (create-request-env param-recs)
   (let loop ((ps param-recs)
	      (res '()))
      (if (pair? ps)
	  (loop (cdr ps)
	        (let ((rec::fastcgi-name-values (car ps)))
		   (append (-> rec name-values) res)))
	  res)))

(define (create-request-input input-recs)
   (let loop ((is (reverse input-recs))
	      (data ""))
      (if (pair? is)
	  (loop (cdr is)
	        (let ((rec::fastcgi-stdin (car is)))
		   (string-append data (-> rec data))))
	  (open-input-string! data))))

(define (create-fastcgi-responder-request socket rec-list)
   (let ((param-recs (filter (lambda (x) (isa? x fastcgi-params)) rec-list))
	 (input-recs (filter (lambda (x) (isa? x fastcgi-stdin)) rec-list))
	 (begin-request::fastcgi-begin-request
	    (find (lambda (x) (isa? x fastcgi-begin-request)) rec-list)))
      (instantiate::fastcgi-responder-request
	 (env (create-request-env param-recs))
	 (input (create-request-input input-recs))
	 (output (create-fastcgi-output-port socket +fastcgi-stdout-type+
					     (-> begin-request request-id)))
	 (error (create-fastcgi-output-port socket +fastcgi-stderr-type+
					    (-> begin-request request-id)))
	 (id (-> begin-request request-id))
	 (keep-connection? (-> begin-request keep-connection?))
	 (socket socket))))
      

(define (create-fastcgi-filter-request socket rec-list)
   (error "create-fastcgi-filter-request" "not implemented" rec-list))

(define (create-fastcgi-authorizer-request socket rec-list)
   (error "create-fastcgi-authorizer-request"
	  "not implemented"
	  rec-list))

(define (fastcgi-request-factory-create socket rec-list)
   (let* ((begin-request-record::fastcgi-begin-request
	     (find (lambda (x) (isa? x fastcgi-begin-request)) rec-list))
	  (role (-> begin-request-record role)))
      (cond ((= role +fastcgi-role-responder+)
	     (create-fastcgi-responder-request socket rec-list))
	    ((= role +fastcgi-role-filter+)
	     (create-fastcgi-filter-request socket rec-list))
	    ((= role +fastcgi-role-authorizer+)
	     (create-fastcgi-authorizer-request socket rec-list))
	    (else
	     (error "fastcgi-request-factory-create"
		    "unknown role  type"
		    role)))))

(define (request-table-remove! table i)
   (hashtable-remove! table i))

(define (request-table-contains? table i)
   (hashtable-contains? table i))

(define (request-table-put! table i rec)
   (hashtable-add! table i (lambda (obj v) (cons obj v)) rec '()))

(define (request-table-get table i)
   (hashtable-get table i))

(define (fold proc seed lst)
   (if (null? lst)
       seed
       (fold proc (proc seed (car lst)) (cdr lst))))

(define (handle-management-record rec::fastcgi-get-values socket)
   (let* ((nvs (-> rec name-values))
	  (rl (fold (lambda (seed n)
		       (let ((name (car n)))
			  (cond ((string-ci=? name "FCGI_MAX_CONNS")
				 (cons (cons name 1) seed))
				((string-ci=? name "FCGI_MAX_REQS")
				 (cons (cons name 1) seed))
				((string-ci=? name "FCGI_MPXS_CONNS")
				 (cons (cons name 0) seed))
				(else
				 seed))))
		    '()
		    nvs))
	  (gvr (instantiate::fastcgi-get-values-result
		   (version +fastcgi-protocol-version+)
		   (type +fastcgi-get-values-result-type+)
		   (request-id 0)
		   (name-values rl)))
	  (gvr-end-stream (duplicate::fastcgi-get-values-result gvr
			     (name-values '()))))
      (display (fastcgi-record-serialize gvr)
	       (socket-output socket))
      ;; signal end of get-values-result stream
      (display (fastcgi-record-serialize gvr)
	       (socket-output socket))
      (flush-output-port (socket-output socket))))
   
		    
;; if we have received both params records and stdin records
;; we have a complete responder request
(define (fastcgi-responder-request-ready? rec-list)
   (and (find (lambda (rec) (and (isa? rec fastcgi-params)
				 (let ((r::fastcgi-params rec))
				    (null? (-> r name-values)))))
	   rec-list)
	(find (lambda (rec) (and (isa? rec fastcgi-stdin)
				 (let ((r::fastcgi-stdin rec))
				    (= (string-length (-> r data))
				       0)))) rec-list)))

(define (fastcgi-filter-request-ready? rec-list)
   (error "fastcgi-filter-request-ready?" "not implemented" rec-list))

(define (fastcgi-authorizer-request-ready? rec-list)
   (error "fastcgi-authorizer-request-ready?" "not implemented" rec-list))

(define (fastcgi-request-ready? rec-list)
   (let* ((begin-request-record::fastcgi-begin-request (find (lambda (x) (isa? x
						     fastcgi-begin-request))
				 rec-list))
	  (role (-> begin-request-record role)))
      (cond ((= role +fastcgi-role-responder+)
	     (fastcgi-responder-request-ready? rec-list))
	    ((= role +fastcgi-role-filter+)
	     (fastcgi-filter-request-ready? rec-list))
	    ((= role +fastcgi-role-authorizer+)
	     (fastcgi-authorizer-request-ready? rec-list))
	    (else
	     (error "fastcgi-request-ready" "unknown fastcgi request role"
		    role)))))


(define (handle-fastcgi-connection connection-socket request-table proc)
   (let loop ((rec::fastcgi-record (read-fastcgi-request-record
		    (socket-input connection-socket))))
      (if (management-record? rec)
	  (handle-management-record rec connection-socket)
	  (let* ((request-id (-> rec request-id))
		 (request-started? (or (isa? rec fastcgi-begin-request)
				       (request-table-contains? request-table
                                          request-id)))
		 (rec-list (if request-started?
			       (request-table-put! request-table
						   request-id
						   rec)
			       '())))
	     (if (fastcgi-request-ready? rec-list)
		 (let ((request::fastcgi-request (fastcgi-request-factory-create
				 connection-socket rec-list)))
		    (request-table-remove! request-table request-id)
		    (proc request)		    
		    (if (-> request keep-connection?)		
			(loop (read-fastcgi-request-record
			       (socket-input connection-socket)))
			(socket-close connection-socket)))
		 (loop (read-fastcgi-request-record
			(socket-input connection-socket))))))))
	 
(define (fastcgi-for-each proc)
   (let ((request-table (create-hashtable))
	 (fastcgi-socket (%get-fast-cgi-socket)))
      (fastcgi-setup-signal-handlers)
      (let loop ((s (socket-accept fastcgi-socket)))
	 (handle-fastcgi-connection s
            request-table
            proc)
	 (if (not +end-fastcgi-request-loop+)
	     (loop (socket-accept fastcgi-socket))
	     (socket-shutdown fastcgi-socket)))))


(define-generic (fastcgi-finish request::fastcgi-request)
   ;; close ports
   (close-output-port (-> request output))
   (close-output-port (-> request error))
   ;;; signal we are done with the request
   (let ((output (socket-output (->  request socket)))
	 (end (instantiate::fastcgi-end-request
		 (version +fastcgi-protocol-version+)
		 (type +fastcgi-end-request-type+)
		 (request-id (-> request id))
		 (app-status 0)
		 (protocol-status 0))))
      (display (fastcgi-record-serialize end) output)
      (flush-output-port output)))

(define-generic (fastcgi-request-getenv request::fastcgi-request
					name::bstring)
   (with-access::fastcgi-request request (env)
      (let ((res (assoc name env)))
	 (if res
	     (cdr res)
	     res))))


(define-generic (fastcgi-request-get-param request::fastcgi-request
				   name)
   (when (eq? (-> request params) #unspecified)
      (initialize-params! request))
   (with-access::fastcgi-request request (params)
      (let ((res (assoc name params)))
	 (if res
	     (cdr res)
	     res))))

(define-generic (fastcgi-request-get-param-vals request::fastcgi-request
					    name)
   (when (eq? (-> request params) #unspecified)
      (initialize-params! request))
   (with-access::fastcgi-request request (params)
	(let loop ((nvs params)
		   (res '()))
	   (if (pair? nvs)
	       (let ((curr (car nvs)))
		  (if (string=? (car curr) name)
		      (loop (cdr nvs)
			    (cons (cdr curr) res))
		      (loop (cdr nvs)
			    res)))
	       res))))


(define-generic (fastcgi-request-get-part request::fastcgi-request
					  name)
   (when (eq? (-> request parts) #unspecified)
      (initialize-parts! request))
    (with-access::fastcgi-request request (parts)
      (let ((res (assoc name parts)))
	 (if res
	     (cdr res)
	     res))))


(define-generic (fastcgi-request-get-cookies request::fastcgi-request)
   (let ((cookie-param (fastcgi-request-getenv request "HTTP_COOKIE")))
      (if cookie-param
	  (cookies-parse cookie-param)
	  cookie-param)))


(define (process-cookies cookies)
   (with-output-to-string
      (lambda ()
	 (let loop ((lst cookies))
	    (when (pair? lst)
	       (print (cookie-serialize (car lst)))
	       (loop (cdr cookies)))))))
		

(define (process-headers headers)
   (define (header-name header)
      (car header))
   (define (header-value header)
      (cdr header))
   (with-output-to-string
      (lambda ()
	 (let loop ((lst headers))
	    (when (pair? lst)
		(let ((header (car lst)))
		   (cond ((string-ci=? (header-name header)
			     "cookie")
			  (display (process-cookies (header-value header))))
			 (else
			  (printf "~a:~a~%" (header-name header)
			     (header-value header))))
		   (loop (cdr lst))))))))
		


(define (fastcgi-out request::fastcgi-request content
		   #!optional (headers '(("Content-Type" . "text/html"))))
   (with-output-to-port (-> request output)
      (lambda ()
	 (display (process-headers (cons (cons "Content-Length"
					    (string-length content))
				      headers)))
	 (newline)
	 (display content)
	 (fastcgi-finish request))))
	 


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

(define (get-boundary content-type)
   (let ((attribs (fold (lambda (s i)
			   (let ((res (string-split i "=")))
			      (if (= (length res) 1)
				  (cons (string-trim (car res)) s)
				  (cons (cons (string-trim (car res))
					      (string-trim (cadr res)))
					s))))
			'()
			(string-split content-type ";"))))
      (let ((bp (assoc "boundary" attribs)))
	 (if bp
	     (cdr bp)
	     (error "get-boundary" "boundary not found" content-type)))))

(define (initialize-params! request::fastcgi-request)
   (let ((method (fastcgi-request-getenv request "REQUEST_METHOD"))
	 (query (fastcgi-request-getenv request "QUERY_STRING"))
	 (content-type (fastcgi-request-getenv request "CONTENT_TYPE")))
      (let ((param-src (if (string-ci=? "GET" method)
			   query
			   (cond ((string-contains-ci content-type
						      "application/x-www-form-urlencoded")
				  (string-append query
						 (read-string (-> request input))))
				 (else
				  query)))))
	 (input-port-reopen! (-> request input))
	 (with-access::fastcgi-request request (params)
	    (set! params (x-www-url-encoded-form-data->map param-src))))))
	 

(define (initialize-parts! request::fastcgi-request)
   (let* ((content-type (fastcgi-request-getenv request "CONTENT_TYPE"))
	  (part-src (if (string-contains-ci content-type
					    "multipart/form-data")
		    (read-string (-> request input))
		    "")))
      (input-port-reopen! (-> request input ))
      (with-access::fastcgi-request request (parts)
	 (if (not (string=? part-src ""))
	     (set! parts (multipart-form-data->map
			  part-src
			  (get-boundary content-type)))
	     (set! parts '())))))
	  


;;; setup signal handling
(define (fastcgi-setup-signal-handlers)
   (signal +sigterm+ (lambda (n)
			   (exit 0)))
   (signal +sigusr1+ (lambda (n)
			(set! +end-fastcgi-request-loop+ #t)))
   (signal +sigpipe+ (lambda (n)
			;; ignore
			#unspecified)))


