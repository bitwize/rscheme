#|------------------------------------------------------------*-Scheme-*--|
 | File:    pg/classes.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rosette.com>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.4
 | Date:    1999-02-12 08:50:07
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: Postgres 95 connection and content classes
 `------------------------------------------------------------------------|#

;; NOTE: instances of these are constructed by C code (in `pg-connect')

(define-class <pg-connection> (<object>)
  (database type: <string>)
  raw-pg-cnxn
  (type-cache init-value: #f)
  (next-cursor-id init-value: 0))

(define-class <pg-error> (<error>))

(define-class <pg-connect-error> (<pg-error>)
  (database type: <string>)
  (message type: <string>))

(define-class <pg-exec-error> (<pg-error>)
  (database type: <pg-connection>)
  (query type: <string>)
  (message type: <string>))

(define-class <pg-result> (<object>)
  raw-pg-result)


(define-method display-object ((self <pg-connect-error>) port)
  (format port "rs.db.pg: could not connect to database ~s\n" (database self))
  (format port "       >> ~a\n" (message self)))

(define-method display-object ((self <pg-exec-error>) port)
  (format port "rs.db.pg: could not execute query against database ~s\n" 
	  (database (database self)))
  (format port "         (query ~#*@60s)\n" (query self))
  (format port "         ~a" (message self)))
