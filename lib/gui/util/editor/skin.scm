
;;; ============================== COLORS ==============================

(define-style white <color-style> () spec: "rgbi:1/1/1")
(define-style black <color-style> () spec: "rgbi:0/0/0")
(define-style peach <color-style> () spec: "rgbi:1/0.8/0.8")

(define-style red <color-style> () spec: "rgbi:1/0/0")
(define-style green <color-style> () spec: "rgbi:0/1/0")
(define-style light-green <color-style> () spec: "rgbi:0.8/1/0.8")
(define-style blue <color-style> () spec: "rgbi:0/0/1")

(define-style gray1 <color-style> () spec: "rgbi:0.1/0.1/0.1")
(define-style gray2 <color-style> () spec: "rgbi:0.2/0.2/0.2")
(define-style gray3 <color-style> () spec: "rgbi:0.3/0.3/0.3")
(define-style gray4 <color-style> () spec: "rgbi:0.4/0.4/0.4")
(define-style gray5 <color-style> () spec: "rgbi:0.5/0.5/0.5")
(define-style gray6 <color-style> () spec: "rgbi:0.6/0.6/0.6")
(define-style gray7 <color-style> () spec: "rgbi:0.7/0.7/0.7")
(define-style gray8 <color-style> () spec: "rgbi:0.8/0.8/0.8")
(define-style gray9 <color-style> () spec: "rgbi:0.9/0.9/0.9")

(define-style purple <color-style> () spec: "#a020f0")

;;; ============================== FONTS ==============================

(define-style default-font <font-style> ()
  foundry: "adobe"
  family: "courier"
  size: 12
  weight: 'medium
  slant: 'roman
  width: 'normal)

(define-style body-font <font-style> (default-font))
(define-style bold-font <font-style> (default-font)
  weight: 'bold)

(define-style control-font <font-style> (default-font)
  foundry: "adobe"
  family: "helvetica"
  size: 12)

;;; ============================== FACES ==============================

(define-style default-face <face-style> () 
  font: 'body-font
  foreground: 'black
  background: #f                ; effectively "transparent"
  line-height: 12
  line-depth: 4)

(define-style defining-face <face-style> (default-face)
  font: 'bold-font)

(define-style keyword-face <face-style> (default-face)
  foreground: 'purple)

(define-style hyperlink-face <face-style> (default-face)
  background: 'light-green)

(define-style comment-face <face-style> (default-face)
  foreground: 'red)

(define-style blur-face <face-style> (default-face)
  foreground: 'gray5)

