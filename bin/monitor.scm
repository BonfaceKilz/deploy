(use-modules
 (hashing md5)
 (hashing md5)
 (ice-9 hash-table)
 (ice-9 rdelim)
 (json)
 (rnrs bytevectors)
 (srfi srfi-1)
 (uuid generate)
 (web client))

(define TOKEN "XXXX")
(define ROOM-ID "XXXX")
(define HOMESERVER "https://matrix.org")

(define (send-message body-text)
  (let* ((txn-id (generate-string-uuid)) ; generates a proper UUID like uuidgen
	 (url (string-append
	       HOMESERVER
	       "/_matrix/client/r0/rooms/"
	       ROOM-ID
	       "/send/m.room.message/"
	       txn-id
	       "?access_token=" TOKEN))
	 (payload (scm->json-string
		   `((msgtype . "m.text")
		     (format . "org.matrix.custom.html")
		     (body . ,body-text)
		     (formatted_body . ,body-text)))))

    (http-put url #:body payload)))


(define LOG-FILE "/tmp/app.log")   ; ← change this
(define POLL-INTERVAL 300000)                ; 0.3s
(define last-alerted-hash #f)

(define (html-escape str)
  (let loop ((chars (string->list str))
             (acc ""))
    (if (null? chars)
        acc
        (let ((c (car chars)))
          (loop (cdr chars)
                (string-append acc
                               (case c
                                 ((#\<) "&lt;")
                                 ((#\>) "&gt;")
                                 ((#\&) "&amp;")
                                 ((#\") "&quot;")
                                 (else (string c)))))))))

(define (hash-string str)
  (md5->string
   (md5 (string->utf8 str))))

(define (anomaly? entry)
  (let ((level (hash-ref entry "level"))
	(event (hash-ref entry "event" ""))
	(msg (hash-ref entry "msg" "")))
    (or (member level '("error" "critical" "exception" "warning"))
	(string-contains-ci event "fail")
	(string-contains-ci event "error")
	(string-contains-ci event "timeout")
	(string-contains-ci msg "ConnectionError|5\\d\\d"))))

(define (emit-alert entry)
  (let* ((ts (hash-ref entry "timestamp" "1970-01-01T00:00:00+03:00"))
	 (lvl (string-upcase (hash-ref entry "level" "info")))
	 (evt (hash-ref entry "event" "unknown"))
	 (msg (hash-ref entry "message" #f))
	 (trace (hash-ref entry "exception" #f)))
    (string-append
     "<strong>ALERT: " lvl "</strong>\n"
     "(" ts ")<br/>\n"
     "Event: " (html-escape evt) "\n"
     (if msg
	 (string-append "Message: "
			(html-escape msg) "<br/>\n")
	 "")
     (if trace
	 (string-append "<br/>\n<pre>" (html-escape trace) "</pre>\n")
	 ""))))

(define (monitor)
  (let ((port (open-file LOG-FILE "r")))
    (seek port 0 SEEK_END)
    (let loop ()
      (let ((line (read-line port 'concat)))
        (cond
         ((eof-object? line)
          (usleep POLL-INTERVAL)
          (loop))

         ((or (string-null? line) (string=? line "\n"))
          (loop))
         (else
          (false-if-exception
           (let ((json (alist->hash-table (json-string->scm line))))
             (when (and (hash-table? json) (anomaly? json))
               (let ((h (hash-string line)))
                 (unless (eqv? h last-alerted-hash)
                   (set! last-alerted-hash h)
		   (display (emit-alert json))
                   (send-message (emit-alert json))))))))))
      (loop))))

(catch #t monitor (lambda _ (display "Can't run this script") (exit 0)))
