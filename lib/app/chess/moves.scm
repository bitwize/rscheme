
(define (reach (p <piece>) 
               (pl <player>)
               (oppon <player>))
  (let ((mask (bexec (occupies pl) "~"))
        (omask (occupies oppon)))
    (print mask)
    (case (type p)
      ;
      ((pawn)
       (bexec
        (bits (square p))
        ;XXX need to handle e.p.
        ;XXX need to handle non-double-first move
        ;XXX need to handle direction
        ">1! <1 n0&== n0&|x=exw|1&|"
        mask
        omask))
        ;--- the following need to be fixed for `omask'
      ;
      ((king)
       (bexec 
        (bits (square p))
        ;; could be more efficient, but this is straightforward...
        "=>0 <0n| <0s| <0e| <0w| <0ne| <0se| <0sw| <0nw| <00&~&"
        mask))
      ;
      ((knight)
       (bexec 
        (bits (square p))
        "=nnex =nnwx =neex =nwwx =ssex =sswx =seex =swwx^|||||||0&"
        mask))
      ;
      ((rook)
       (bexec
        (bits (square p))
        ">1! <1{n0&>9<9|<9}^ <1{s0&>9<9|<9}^ <1{e0&>9<9|<9}^ <1{w0&>9<9|<9}^"
        mask))
      ;
      ((bishop)
       (bexec
        (bits (square p))
        ">1! <1{nw0&>9<9|<9}^ <1{ne0&>9<9|<9}^ <1{sw0&>9<9|<9}^ <1{se0&>9<9|<9}^"
        mask))
      ;
      ((queen)
       (bexec
        (bits (square p))
        ">1! <1{nw0&>9<9|<9}^ <1{ne0&>9<9|<9}^ <1{sw0&>9<9|<9}^ <1{se0&>9<9|<9}^ <1{n0&>9<9|<9}^ <1{s0&>9<9|<9}^ <1{e0&>9<9|<9}^ <1{w0&>9<9|<9}^"
        mask))
      ;
      )))


;;;

(define (test-move-from setup f r)
  (bind ((b (board setup))
         (pc pl (piece-at b f r)))
    (reach pc pl (opponent pl b))))
  
#|
(test-move-from "/2P//4B" 'e 5)
|#
