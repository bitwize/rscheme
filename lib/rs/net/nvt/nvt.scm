
(define-class <network-virtual-terminal> (<object>)
  input-port
  output-port)

(define (open-nvt (in <input-port>) (out <output-port>))
  (make <network-virtual-terminal>
        input-port: (open-input-nvt in)
        output-port: (open-output-nvt out)))
