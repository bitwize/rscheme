#|------------------------------------------------------------*-Scheme-*--|
 | File:    packages/threads/manager/packet.scm
 |
 |          Copyright (C) 2003 Donovan Kolbly <donovan@rscheme.org>
 |          as part of the RScheme project, licensed for free use
 |
 | Version: 1.3
 | Date:    2005-09-07 14:40:06
 | Build:   v0.7.3.4-b7u, 2007-05-30
 |
 | Purpose: UDP sockets for the threads system
 |
 `------------------------------------------------------------------------|#

(define-class <packet-input-port> (<object>)
  (mbox type: <mailbox>)                ;; [0]
  (event init-value: 0 setter: #f)      ;; [1]
  (sockaddr-class type: <<class>>)      ;; [2]
  (filedes type: <fixnum>))

(define (open-packet-port fd sockaddr-class #optional owner)
  (let ((port (make <packet-input-port>
                    filedes: fd
                    mbox: (make-mailbox fd)
                    sockaddr-class: sockaddr-class)))
    (make-recv-event fd port)
    port))

(define-method close ((self <packet-input-port>))
  (free-read-event* self (%slot-index <packet-input-port> event)))

;;;

(define-class <udp-socket> (<object>)
  (filedes type: <fixnum>)
  (input-port init-value: #f))

(define-method receive-packet ((self <udp-socket>))
  (if (not (input-port self))
      (set-input-port! self (open-packet-port (filedes self)
                                              <inet-socket-addr> 
                                              #f)))
  (let ((x (receive-message! (mbox (input-port self)))))
    (if (instance? x <condition>)
        (signal x)
        (if x
            (values (vector-ref x 0)
                    (vector-ref x 1))
            (values)))))
    

(define-method close ((self <udp-socket>))
  (if (input-port self)
      (close (input-port self)))
  (fd-close (filedes self))
  (values))

(define-method send-packet ((self <udp-socket>) data (dest <inet-socket-addr>))
  (send-to (filedes self)
           data
           0
           (string-length data)
           #f
           dest))

(define (open-udp-socket #optional addr)
  (let ((fd (socket-create (socket-address-family->integer 
                            'address-family/internet)
                           (socket-type->integer 'socket-type/datagram)
                           0)))
    (if addr
        (begin
          ;; reuse the addr in case we were using it just recently
          (set-socket-option fd 'level/socket 'socket/reuse-addr #t)
          (socket-binder addr fd)))
    ;;
    (make <udp-socket>
          filedes: fd)))
