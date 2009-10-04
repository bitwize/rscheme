,(use graphics.fontmgr
      graphics.afm)

(define *annotation-font* (get-text-font "Times" "Italic" 7))

(define *tag-font* (get-text-font "Courier" "Bold" 8))
(define *index-font* (get-text-font "Courier" "Regular" 8))
(define *content-font* (get-text-font "Times" "Roman" 9))
(define *record-height* 15)
(define *record-width* 100)

(define (record-frame i)
  (make-rect 100 (* (- 30 i) *record-height*)
              *record-width*
              *record-height*))

(define (record i addr label . extra)
  (let ((f (record-frame i)))
    (setfont *index-font*)
    (rshow (- (origin-x f) 2) (- (limit-y f) 3) (~ "0x~04x" addr))
    (setfont *tag-font*)
    (moveto (+ (origin-x f) 2) (- (limit-y f) 8))
    (show label)
    (if (pair? extra)
        (let ((x (+ (origin-x f) 
                    (string-width *tag-font* label)
                    10)))
          (setfont *content-font*)
          (for-each 
           (lambda (extra)
             (moveto x (+ (origin-y f) 3))
             (show extra)
             (set! x (+ x (string-width *content-font* extra) 5)))
           extra)))
    (rectstroke f)))

;;;  Return a <line> that represents the 
;;;  extent of the record 
         ;;;  (and, horizontally, is off a bit to the right)

(define (record-span i n)
  (let ((f0 (record-frame i))
        (f1 (record-frame (- (+ i n) 1))))
    (make-line (+ 10 (limit-x f0))
               (limit-y f0)
               (+ 10 (limit-x f0))
               (origin-y f1))))


;;;

(define (draw)
  (let ((k 0))
    ;;
    (define (rec addr tag . extra)
      (apply record k addr tag extra)
      (set! k ( + k 1)))
    ;;
    (define (brc i n text)
      (draw-long-brace base: (record-span i n)
                       ontip: (lambda ()
                                (rotate 90)
                                (moveto 7 -2)
                                (setfont *annotation-font*)
                                (show text))))
    ;;
    (rec #x0000 "\\LSS")
    (rec #x01f0 "VOLF" "/tmp/sample.sto")
    (rec #x0210 "DATA" "[02000000]")
    (rec #x0260 "DATA" "[00000000]")
    (rec #x0290 "INDX")
    (rec #x03a0 "INDX")
    (rec #x04b0 "MIDX")
    (rec #x04e0 "GAP ")
    (rec #x07f0 "DSEG")
    (rec #x0800 "ZIPA" "zlib-fast")
    (rec #x0820 "GAP ")
    (rec #x0bf0 "DSEG")
    (rec #x0c00 "COMM" "#100")
    (rec #x0f80 "GAP ")
    (rec #x0ff0 "*EOF")

    (rec #x1000 "DATA" "[02000000]")
    (rec #x1060 "DATA" "[02000001]")
    (rec #x10b0 "DATA" "[00000000]")
    (rec #x10e0 "GAP ")
    (rec #x13f0 "DSEG")
    (rec #x1400 "COMM" "#101")
    (rec #x1780 "GAP ")
    (rec #x17f0 "*EOF")
    (brc 0 2 "Volume Header")
    (brc 2 13 "Commit #100")
    (brc 15 8 "Commit #101")
    (values)))



#|

star:src> rsf -q +rs.db.rstore
top[0]=>(create-persistent-store "/tmp/sample.sto")
value := #[<persistent-store> @081d_30cb]
top[1]=>(commit % 123)
value := 100
top[2]=>
star:src> lssctl -P /tmp/sample.sto 
0000000000: \LSS    496 
0x000001f0: VOLF     32  16 </tmp/sample.sto\0>
0x00000210: DATA     80  02000000 48 (z1) <78 01 93 64 60 65 60 66 00 02 26 ...
0x00000260: DATA     48  0 32 <RStore-3.0\0\0\0\0\0\0\0\0\0\001\0\001\0\0\0\...
0x00000290: INDX    272  256 <0x26 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0...
0x000003a0: INDX    272  256 <0x21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0...
0x000004b0: MIDX     48  24 <0 0x29 0x30000 0x80000 0x3a 0x30000>
0x000004e0: GAP     784 
0x000007f0: DSEG     16 
0x00000800: ZIPA     32  10 <zlib-fast\0>
0x00000820: GAP     976 
0x00000bf0: DSEG     16 
0x00000c00: COMM    896  gen 100 <2005-03-30 09:57:05>
0x00000f80: GAP     112 
0x00000ff0: *EOF     16 
0x00001000:
star:src> 
star:src> rsf -q +rs.db.rstore
top[0]=>(open-persistent-store "/tmp/sample.sto")
value := #[<persistent-store> @081e_9b13]
top[1]=>(commit % (list 1 2 3 (make-string 8000)))
value := 101
top[2]=>
star:src> lssctl -P /tmp/sample.sto 
0000000000: \LSS    496 
0x000001f0: VOLF     32  16 </tmp/sample.sto\0>
0x00000210: DATA     80  02000000 48 (z1) <78 01 93 64 60 65 60 66 00 02 26 ...
0x00000260: DATA     48  0 32 <RStore-3.0\0\0\0\0\0\0\0\0\0\001\0\001\0\0\0\...
0x00000290: INDX    272  256 <0x26 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0...
0x000003a0: INDX    272  256 <0x21 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0...
0x000004b0: MIDX     48  24 <0 0x29 0x30000 0x80000 0x3a 0x30000>
0x000004e0: GAP     784 
0x000007f0: DSEG     16 
0x00000800: ZIPA     32  10 <zlib-fast\0>
0x00000820: GAP     976 
0x00000bf0: DSEG     16 
0x00000c00: COMM    896  gen 100 <2005-03-30 09:57:05>
0x00000f80: GAP     112 
0x00000ff0: *EOF     16 

0x00001000: DATA     96  02000000 83 (z1) <78 01 33 66 e0 62 60 61 00 02 26 ...
0x00001060: DATA     80  02000001 2030 (z1) <78 01 bb cd ce ca c0 c4 00 04 4...
0x000010b0: DATA     48  0 32 <RStore-3.0\0\0\0\0\0\0\0\0\0\001\0\001\0\0\0\...
0x000010e0: GAP     784 
0x000013f0: DSEG     16 
0x00001400: COMM    896  gen 101 <2005-03-30 09:57:51>
0x00001780: GAP     112 
0x000017f0: *EOF     16 
0x00001800:
star:src> 
|#
