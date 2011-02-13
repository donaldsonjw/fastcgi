(module testfastcgi
   (library fastcgi)
   (main main))

;;;; Test code 
(define count 0)
(define (test-handler request)
   (with-access::fastcgi-request request
	 (output)
      ;(print request)
      (fprint output "Content-type: text/html")
      (newline output)
      ;(print "keep-connection?: " (fastcgi-request-keep-connection? request))
      ;(print "count: " count)
      (fprint output "<html><head><title>Test</title></head><body><h1>Test</h1>")
      (fprintf output "<p>keep-connection?: ~a </p> <p>count: ~a </p></body</html>"
	       (fastcgi-request-keep-connection? request)
	       count)
      (fprintf output "<p>REQUEST_URI: ~a</p>" (fastcgi-request-getenv request "REQUEST_URI"))
      (set! count (+ count 1))
      (fastcgi-finish request)
      ))

(define (main args)
   (fastcgi-for-each test-handler))
			  

#;(let* ((request-id #unspecified)
       (socket (%get-fast-cgi-socket))
       (s2 (socket-accept socket)))
       (let loop ((rec (read-fastcgi-request-record (socket-input s2))))
	  (print rec)
	  (set! request-id (fastcgi-record-request-id rec))
	  (when (not (and (fastcgi-stdin? rec)
			(= (string-length (fastcgi-stdin-data rec)) 0)))
	     (loop (read-fastcgi-request-record (socket-input s2)))))
       (print "finished collecting request records")
       (let ((port (create-fastcgi-output-port s2 +fastcgi-stdout-type+
					       request-id)))
	  (fprint port "Content-type: text/html")
	  (newline port)
	  (fprint port "<html><head><title>Test</title></head><body><h1>Test</h1></body</html>")
	  (flush-output-port port)
	  (close-output-port port))
       (let ((output (socket-output s2))
	     (end (instantiate::fastcgi-end-request
		     (version +fastcgi-protocol-version+)
		     (type +fastcgi-end-request-type+)
		     (request-id request-id)
		     (app-status 0)
		     (protocol-status 0))))
	  (print (fastcgi-record-serialize end))
	  (display (fastcgi-record-serialize end) output)
	  (flush-output-port output))
       (socket-close s2)
       (socket-shutdown socket))



