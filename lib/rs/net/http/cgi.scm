#|
SCRIPT_FILENAME=/var/apache/cgi-bin/echo
HOSTNAME=wind
SERVER_NAME=www.rscheme.org
HTTP_CONNECTION=Keep-Alive
MACHTYPE=i586-debian-linux
CONTENT_TYPE=application/x-www-form-urlencoded
REMOTE_ADDR=192.35.232.13
REQUEST_URI=/cgi-bin/echo
TERM=dumb
HOSTTYPE=i586
PATH=/usr/local/sbin:/sbin:/bin:/usr/sbin:/usr/bin
HTTP_HOST=www.rscheme.org
REMOTE_PORT=1129
REQUEST_METHOD=POST
SHELL=/bin/sh
GATEWAY_INTERFACE=CGI/1.1
QUERY_STRING=
SERVER_SOFTWARE=Apache/1.2.4
SERVER_PROTOCOL=HTTP/1.0
REMOTE_HOST=192.35.232.13
SERVER_PORT=80
DOCUMENT_ROOT=/u/www
OSTYPE=linux
HTTP_USER_AGENT=Mozilla/3.01 (X11; I; AIX 1)
HTTP_ACCEPT=image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*
SCRIPT_NAME=/cgi-bin/echo
SHLVL=1
SERVER_ADMIN=d.kolbly@rscheme.org
HTTP_REFERER=file:/u/dkolbly/src/ptest.html
CONTENT_LENGTH=154
_=/usr/bin/env
|#

#|
#!/usr/local/bin/rs -script

,(use html)
|#

(define (get-cgi-body)
  (let* ((cl (or (getenv "CONTENT_LENGTH") 
		 (error "No CONTENT_LENGTH available")))
	 (len (string->number cl)))
    (read-string (current-input-port) len)))

#|
(define (main args)
  (display "Content-type: text/html\n\n")
  (html
   (pre
    (display (get-cgi-body)))))
|#