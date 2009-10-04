
;; bytecode return
(define (emit-return n) (cond ((eq? n 0) (emit-byte-code 0)) ((eq? n 1) (emit-byte-code 1)) (else (begin (emit-byte-code 2) (emit-byte-code-check n)))))

;; bytecode literal
(define (emit-literal slot) (case slot ((0) (emit-byte-code 3)) ((1) (emit-byte-code 4)) ((2) (emit-byte-code 5)) ((3) (emit-byte-code 6)) ((4) (emit-byte-code 7)) ((5) (emit-byte-code 8)) (else (if (> slot 255) (begin (emit-byte-code 9) (emit-byte-code-16-check slot)) (begin (emit-byte-code 10) (emit-byte-code-check slot))))))

;; bytecode closure
(define (emit-closure slot) (if (> slot 255) (begin (emit-byte-code 11) (emit-byte-code-16-check slot)) (begin (emit-byte-code 12) (emit-byte-code-check slot))))

;; bytecode apply
(define (emit-apply n) (case n ((0) (emit-byte-code 13)) ((1) (emit-byte-code 14)) ((2) (emit-byte-code 15)) ((3) (emit-byte-code 16)) ((4) (emit-byte-code 17)) (else (begin (emit-byte-code 18) (emit-byte-code-check n)))))

;; bytecode reg-set
(define (emit-reg-set n) (case n ((0) (emit-byte-code 19)) ((1) (emit-byte-code 20)) ((2) (emit-byte-code 21)) (else (if (< n 10) (begin (emit-byte-code 22) (emit-byte-code-check n)) (if (> n 255) (begin (emit-byte-code 23) (emit-byte-code-16-check n)) (begin (emit-byte-code 24) (emit-byte-code-check n)))))))

;; bytecode reg-ref
(define (emit-reg-ref n) (case n ((0) (emit-byte-code 25)) ((1) (emit-byte-code 26)) ((2) (emit-byte-code 27)) (else (if (< n 10) (begin (emit-byte-code 28) (emit-byte-code-check n)) (if (> n 255) (begin (emit-byte-code 29) (emit-byte-code-16-check n)) (begin (emit-byte-code 30) (emit-byte-code-check n)))))))

;; bytecode reg-xfer
(define (emit-reg-xfer from to) (case (if (<= to 255) from #f) ((0) (case to ((1) (emit-byte-code 31)) ((2) (emit-byte-code 32)) ((3) (emit-byte-code 33)) (else (if (< to 10) (begin (emit-byte-code 34) (emit-byte-code-check to)) (begin (emit-byte-code 35) (emit-byte-code-check to)))))) ((1) (case to ((0) (emit-byte-code 36)) ((2) (emit-byte-code 37)) ((3) (emit-byte-code 38)) (else (if (< to 10) (begin (emit-byte-code 39) (emit-byte-code-check to)) (begin (emit-byte-code 40) (emit-byte-code-check to)))))) ((2) (case to ((0) (emit-byte-code 41)) ((1) (emit-byte-code 42)) ((3) (emit-byte-code 43)) (else (if (< to 10) (begin (emit-byte-code 44) (emit-byte-code-check to)) (begin (emit-byte-code 45) (emit-byte-code-check to)))))) ((3) (case to ((0) (emit-byte-code 46)) ((1) (emit-byte-code 47)) ((2) (emit-byte-code 48)) (else (if (< to 10) (begin (emit-byte-code 49) (emit-byte-code-check to)) (begin (emit-byte-code 50) (emit-byte-code-check to)))))) (else (case (if (<= from 255) to #f) ((0) (if (< from 10) (begin (emit-byte-code 51) (emit-byte-code-check from)) (begin (emit-byte-code 52) (emit-byte-code-check from)))) ((1) (if (< from 10) (begin (emit-byte-code 53) (emit-byte-code-check from)) (begin (emit-byte-code 54) (emit-byte-code-check from)))) ((2) (if (< from 10) (begin (emit-byte-code 55) (emit-byte-code-check from)) (begin (emit-byte-code 56) (emit-byte-code-check from)))) ((3) (if (< from 10) (begin (emit-byte-code 57) (emit-byte-code-check from)) (begin (emit-byte-code 58) (emit-byte-code-check from)))) (else (cond ((and (< from 10) (< to 10)) (begin (emit-byte-code 59) (emit-byte-code-check from) (emit-byte-code-check to))) ((and (< from 10) (<= to 255)) (begin (emit-byte-code 60) (emit-byte-code-check from) (emit-byte-code-check to))) ((< from 10) (begin (emit-byte-code 61) (emit-byte-code-check from) (emit-byte-code-16-check to))) ((and (<= from 255) (< to 10)) (begin (emit-byte-code 62) (emit-byte-code-check from) (emit-byte-code-check to))) ((and (<= from 255) (< to 255)) (begin (emit-byte-code 63) (emit-byte-code-check from) (emit-byte-code-check to))) ((<= from 255) (begin (emit-byte-code 64) (emit-byte-code-check from) (emit-byte-code-16-check to))) ((< to 10) (begin (emit-byte-code 65) (emit-byte-code-16-check from) (emit-byte-code-check to))) ((<= to 255) (begin (emit-byte-code 66) (emit-byte-code-16-check from) (emit-byte-code-check to))) (else (begin (emit-byte-code 67) (emit-byte-code-16-check from) (emit-byte-code-16-check to)))))))))

;; bytecode pop
(define (emit-pop) (emit-byte-code 68))

;; bytecode lex-ref
(define (emit-lex-ref frame slot) (case frame ((0) (case slot ((0) (emit-byte-code 69)) ((1) (emit-byte-code 70)) ((2) (emit-byte-code 71)) (else (begin (emit-byte-code 72) (emit-byte-code-check slot))))) ((1) (case slot ((0) (emit-byte-code 73)) ((1) (emit-byte-code 74)) ((2) (emit-byte-code 75)) (else (begin (emit-byte-code 76) (emit-byte-code-check slot))))) ((2) (case slot ((0) (emit-byte-code 77)) ((1) (emit-byte-code 78)) ((2) (emit-byte-code 79)) (else (begin (emit-byte-code 80) (emit-byte-code-check slot))))) (else (begin (emit-byte-code 81) (emit-byte-code-check frame) (emit-byte-code-check slot)))))

;; bytecode lex-set
(define (emit-lex-set frame slot) (case frame ((0) (case slot ((0) (emit-byte-code 82)) ((1) (emit-byte-code 83)) ((2) (emit-byte-code 84)) (else (begin (emit-byte-code 85) (emit-byte-code-check slot))))) ((1) (case slot ((0) (emit-byte-code 86)) ((1) (emit-byte-code 87)) ((2) (emit-byte-code 88)) (else (begin (emit-byte-code 89) (emit-byte-code-check slot))))) ((2) (case slot ((0) (emit-byte-code 90)) ((1) (emit-byte-code 91)) ((2) (emit-byte-code 92)) (else (begin (emit-byte-code 93) (emit-byte-code-check slot))))) (else (begin (emit-byte-code 94) (emit-byte-code-check frame) (emit-byte-code-check slot)))))

;; bytecode tl-ref
(define (emit-tl-ref i) (if (> i 255) (begin (emit-byte-code 95) (emit-byte-code-16-check i)) (begin (emit-byte-code 96) (emit-byte-code-check i))))

;; bytecode tl-set
(define (emit-tl-set i) (if (> i 255) (begin (emit-byte-code 97) (emit-byte-code-16-check i)) (begin (emit-byte-code 98) (emit-byte-code-check i))))

;; bytecode immob
(define (emit-immob x) (case x ((#t) (emit-byte-code 99)) ((#f) (emit-byte-code 100)) ((()) (emit-byte-code 101)) (else (cond ((ascii-char? x) (begin (emit-byte-code 102) (emit-byte-code-check (ascii-char->integer x)))) ((unicode-char? x) (begin (emit-byte-code 103) (emit-byte-code-16-check (unicode-char->integer x)))) ((unique-obj? x) (begin (emit-byte-code 104) (emit-byte-code-check (get-immob-value x)))) (else (begin (emit-byte-code 105) (emit-byte-code-16-check (obj-high-bits x)) (emit-byte-code-16-check (obj-low-bits x))))))))

;; bytecode make-primop
(define (emit-make-primop num-args) (begin (emit-byte-code 106) (emit-byte-code-check (- num-args 1))))

;; bytecode special-primop
(define (emit-special-primop op num-args) (case op ((cons) (assert (= num-args 2)) (emit-byte-code 107)) ((car) (assert (= num-args 1)) (emit-byte-code 108)) ((cdr) (assert (= num-args 1)) (emit-byte-code 109)) ((make) (emit-make-primop num-args)) (else (error/internal "special-primop not defined: ~s" op))))

;; bytecode save
(define (emit-save n l) (case n ((0) (begin (emit-byte-code 110) (emit-byte-code-16-check (ref-label l)))) ((1) (begin (emit-byte-code 111) (emit-byte-code-16-check (ref-label l)))) ((2) (begin (emit-byte-code 112) (emit-byte-code-16-check (ref-label l)))) ((3) (begin (emit-byte-code 113) (emit-byte-code-16-check (ref-label l)))) ((4) (begin (emit-byte-code 114) (emit-byte-code-16-check (ref-label l)))) ((5) (begin (emit-byte-code 115) (emit-byte-code-16-check (ref-label l)))) (else (if (> n 255) (begin (emit-byte-code 116) (emit-byte-code-16-check n) (emit-byte-code-16-check (ref-label l))) (begin (emit-byte-code 117) (emit-byte-code-check n) (emit-byte-code-16-check (ref-label l)))))))

;; bytecode restore
(define (emit-restore n) (case n ((0) (emit-byte-code 118)) ((1) (emit-byte-code 119)) ((2) (emit-byte-code 120)) ((3) (emit-byte-code 121)) ((4) (emit-byte-code 122)) ((5) (emit-byte-code 123)) (else (if (> n 255) (begin (emit-byte-code 124) (emit-byte-code-16-check n)) (begin (emit-byte-code 125) (emit-byte-code-check n))))))

;; bytecode jump
(define (emit-jump n l) (begin (emit-byte-code 126) (emit-byte-code-16-check (ref-label l))))

;; bytecode bjump
(define (emit-bjump n l) (begin (emit-byte-code 127) (emit-byte-code-check n) (emit-byte-code-16-check (ref-label l))))

;; bytecode branch-if-false
(define (emit-branch-if-false l) (begin (emit-byte-code 128) (emit-byte-code-16-check (ref-label l))))

;; bytecode check=
(define (emit-check= n) (case n ((0) (emit-byte-code 129)) ((1) (emit-byte-code 130)) ((2) (emit-byte-code 131)) ((3) (emit-byte-code 132)) ((4) (emit-byte-code 133)) (else (begin (emit-byte-code 134) (emit-byte-code-check n)))))

;; bytecode check>=
(define (emit-check>= n) (if (> n 0) (begin (emit-byte-code 135) (emit-byte-code-check n))))

;; bytecode set-false<
(define (emit-set-false< n) (case n ((0)) ((1) (emit-byte-code 136)) ((2) (emit-byte-code 137)) (else (begin (emit-byte-code 138) (emit-byte-code-check n)))))

;; bytecode collect>
(define (emit-collect> n) (case n ((0) (emit-byte-code 139)) ((1) (emit-byte-code 140)) ((2) (emit-byte-code 141)) ((3) (emit-byte-code 142)) (else (begin (emit-byte-code 143) (emit-byte-code-check n)))))

;; bytecode unbind
(define (emit-unbind) (emit-byte-code 144))

;; bytecode bind-first-regs
(define (emit-bind-first-regs n) (case n ((1) (emit-byte-code 145)) ((2) (emit-byte-code 146)) ((3) (emit-byte-code 147)) ((4) (emit-byte-code 148)) ((5) (emit-byte-code 149)) (else (begin (emit-byte-code 150) (emit-byte-code-check n)))))

;; bytecode bind
(define (emit-bind n) (case n ((1) (emit-byte-code 151)) ((2) (emit-byte-code 152)) ((3) (emit-byte-code 153)) ((4) (emit-byte-code 154)) ((5) (emit-byte-code 155)) (else (begin (emit-byte-code 156) (emit-byte-code-check n)))))

;; bytecode raw-int
(define (emit-raw-int n) (if (and (>= n 0) (< n 256)) (if (eq? n 0) (emit-byte-code 157) (if (eq? n 1) (emit-byte-code 158) (begin (emit-byte-code 159) (emit-byte-code-check n)))) (if (and (< n 0) (> n -257)) (begin (emit-byte-code 160) (emit-byte-code-check (+ n 256))) (if (and (>= n -32768) (< n 32768)) (begin (emit-byte-code 161) (emit-byte-code-s16-check n)) (begin (emit-byte-code 162) (emit-byte-code-s32 n))))))

;; bytecode fixnum
(define (emit-fixnum n) (if (and (>= n 0) (< n 256)) (case n ((0) (emit-byte-code 163)) ((1) (emit-byte-code 164)) (else (begin (emit-byte-code 165) (emit-byte-code-check n)))) (if (and (< n 0) (> n -257)) (begin (emit-byte-code 166) (emit-byte-code-check (+ n 256))) (if (and (>= n -32768) (< n 32768)) (begin (emit-byte-code 167) (emit-byte-code-s16-check n)) (begin (emit-byte-code 168) (emit-byte-code-s32 n))))))

;; bytecode raw-bool
(define (emit-raw-bool r) (if r (emit-byte-code 169) (emit-byte-code 170)))

;; bytecode this-function
(define (emit-this-function) (emit-byte-code 171))

;; bytecode use-function-envt
(define (emit-use-function-envt) (emit-byte-code 172))

;; bytecode use-empty-envt
(define (emit-use-empty-envt) (emit-byte-code 173))

;; bytecode gvec-load
(define (emit-gvec-load index) (begin (emit-byte-code 174) (emit-byte-code-check index)))

;; bytecode gvec-store
(define (emit-gvec-store index) (begin (emit-byte-code 175) (emit-byte-code-check index)))

;; bytecode applyf
(define (emit-applyf n) (case n ((0) (emit-byte-code 176)) ((1) (emit-byte-code 177)) ((2) (emit-byte-code 178)) ((3) (emit-byte-code 179)) ((4) (emit-byte-code 180)) (else (begin (emit-byte-code 181) (emit-byte-code-check n)))))

;; bytecode applyg
(define (emit-applyg n) (case n ((1) (emit-byte-code 182)) ((2) (emit-byte-code 183)) ((3) (emit-byte-code 184)) (else (begin (emit-byte-code 185) (emit-byte-code-check n)))))

;; bytecode tl-ref/bound
(define (emit-tl-ref/bound i) (case i ((0) (emit-byte-code 186)) ((1) (emit-byte-code 187)) ((2) (emit-byte-code 188)) ((3) (emit-byte-code 189)) ((4) (emit-byte-code 190)) ((5) (emit-byte-code 191)) ((6) (emit-byte-code 192)) ((7) (emit-byte-code 193)) ((8) (emit-byte-code 194)) ((9) (emit-byte-code 195)) (else (if (> i 255) (begin (emit-byte-code 196) (emit-byte-code-16-check i)) (begin (emit-byte-code 197) (emit-byte-code-check i))))))
