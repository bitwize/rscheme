,(use syscalls)

;;;

(define *current-event-time* (time))

(define (set-current-event-time!)
  (set! *current-event-time* (time)))
  
(define (current-event-time)
  *current-event-time*)

(define-class <dv-object> (<object>) :abstract
  (properties type: <vector> init-value: '#())
  (creation-time type: <time> init-function: current-event-time)
  (modification-time type: <time> init-function: current-event-time))

;;;

(define-class <document> (<dv-object>)
  (document-views type: <list> init-value: '())
  (document-pages type: <vector> init-value: '#())
  (next-object-id type: <fixnum> init-value: 0))

(define-thread-var *document*)

(define (current-document)
  *document*)

(define-method mark-as-dirty ((self <document>))
  (let ((t (transient-for self)))
    (if t
	; we might not have a transient <open-document>, in case
	; are being first constructed (as a test document
	; or a new document)
	(mark-as-dirty t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  the graphic objects of a page are organized into a tree,
;;;  with <group>'s as the interior nodes (and the special
;;;  case of <root-group> as the root) and <leaf-object>'s as the
;;;  leaves.
;;;

(define-class <graphic-object> (<dv-object>) :abstract
  (id type: <fixnum> init-value: -1)
  (graphic-bounding-box type: <rect>)
  (origin init-value: $zero-point)
  (in-document type: <document>))

(define-method write-object ((self <graphic-object>) port)
  (format port "#[~a #~d]" (name (object-class self)) (id self)))

;;;
 
(define-class <group> (<graphic-object>) :abstract
  (group-contents type: <list> init-value: '())
  (general-transform init-value: $identity-transform))

;;;  the origin of the <root-group> is always $zero-point = (0,0)

(define-method initialize ((self <graphic-object>) #rest extra)
  (if (pair? extra)
      (wm 312 "~s: excess initializers ignored: ~s" self extra))
  (next-method)
  (mark-as-dirty (in-document self))
  (let (((n <fixnum>) (next-object-id (in-document self))))
    (set-id! self n)
    (set-next-object-id! (in-document self) (add1 n))))

(define-class <root-group> (<group>))
  
(define-class <user-group> (<group>)
  (parent-object type: <group>))
  
(define-class <leaf-object> (<graphic-object>) :abstract
  (parent-object type: <group>))

(define (add-child! (self <group>) (child <graphic-object>))
  (set-group-contents! self (append! (group-contents self) (list child))))

(define-method initialize ((self <leaf-object>))
  (next-method)
  (add-child! (parent-object self) self))

(define-method initialize ((self <user-group>))
  (next-method)
  (add-child! (parent-object self) self))

;;;

(define-class <page> (<dv-object>)
  (in-document type: <document>)
  (name type: <string>)
  (page-contents type: <root-group>)
  (page-size type: <size>)
  (page-margins type: <rect> init-value: $zero-rect))

(define-method initialize ((self <page>))
  (if (eq? (page-margins self) $zero-rect)
      (let ((s (page-size self)))
        (set-page-margins! self (inset-rect (make-rect 0 0 
                                                       (width s) (height s))
                                            36
                                            36)))))

;;;

(define-class <view> (<dv-object>)
  (in-document type: <document>)
  (name type: <string>)
  ;;
  ;; the `view-frame' tells where the display window is located
  ;; on the screen.  The X and Y are ignored by many window
  ;; managers, but we present 'em anyway (in theory).
  ;;
  (view-frame type: <rect>)
  ;;
  ;; the `view-page' is the page object being viewed.
  ;;
  (view-page type: <page>)
  ;;
  ;; the `view-ctm' tells how to transform from 
  ;; user (document) coordinates to device sheet coordinates.
  ;; The display device is "flipped", so that Y is 0 at the top
  ;; and increases downward.  Since document coordinates are like
  ;; PostScript user coordinates, the view-ctm includes a flip/translate
  ;; transformation.
  ;;
  ;; the `device sheet' is essentially window coordinates but without
  ;; any translation from scroll bars (i.e., (0,0) in device sheet
  ;; coordinates is the upper left corner of the possible pixels that
  ;; could be displayed.)
  ;;
  (view-ctm type: <transform> setter: set-real-view-ctm!
	    init-value: $identity-transform
	    init-keyword: #f)
  ;;
  ;; the `view-origin' tells where the display window is located
  ;; on the device sheet.  Note that it should be possible to incorporate
  ;; the view-origin into the view-ctm via a translate operation, but
  ;; the scroll-bar inverse transformation (as for tracking) is an
  ;; utter pain in that case, at least for my meager mental capabilities.
  ;; The view origin is relative to the origin of the device sheet
  ;; (ie, a view-origin of (0,0) means that the device window is
  ;; viewing the upper left portion of the device sheet).
  ;;
  (view-origin type: <point>
	       init-value: $zero-point
	       init-keyword: #f)
  ;;
  ;; the `view-extent' is the size of the page (plus a small margin)
  ;; expressed in device sheet coordinates.  It can be obtained by
  ;; creating a (0,0,w,h) rectangle, insetting it by
  ;; $view-extent-margin (which is negative), transforming the
  ;; rectangle using `view-ctm', and taking the size of the resulting
  ;; rectangle.  However, we cache it here since it is often used in
  ;; scrollbar computations.
  ;;
  (view-extent type: <size> init-value: $zero-size init-keyword: #f))

;;; initialize the view-ctm, view-origin, and view-extent to be a
;;; 100% scale centered view

(define-method initialize ((self <view>))
  (set-view-ctm! self (view-ctm self)))

#|
  ;(set-view-ctm! self (scale (make-affine-transform) (make-point 1 -1)))
  (set-center-point! self (size->point 
			   (size* (page-size (view-page self))
				  0.5))))
|#

(define $view-extent-margin -36)

;;;  call this to change a view's CTM
;;;
;;;  the real CTM that is translated so that the upper-left
;;;  corner of the margin bounding box transforms to (0,0)

(define (set-view-ctm! (self <view>) ctm)
  (let* ((sz (page-size (view-page self)))
	 (page-box (make-rect 0 0 (width sz) (height sz)))
	 (r (transform (inset-rect page-box
				   $view-extent-margin
				   $view-extent-margin)
		       ctm)))
    ;; after this, the upper-left corner of the transformed bbox
    ;; will be (0,0)
    (set-real-view-ctm! self (inverse-translate ctm (origin r)))
    (set-view-extent! self (size r))
    r))

;;; update the view's CTM by concatenating the given ctm,
;;; plus a translation to preserve the device location of
;;; the point `about' (given in user coordinates).  
;;; Defaults to the center point

(define (concat-view-ctm! (self <view>) ctm #optional about)
  (let* ((about (or about (center-point self)))
	 (about-in-win (transform about (view-ctm self))))
    (set-view-ctm! self (concatenate-transform (view-ctm self) ctm))
    (let ((new-dc (transform about (view-ctm self)))
	  (vo (view-origin self)))
      (set-view-origin! 
       self
       (make-point (+ (x vo) (- (x new-dc) (x about-in-win)))
		   (+ (y vo) (- (y new-dc) (y about-in-win)))))
      (values))))

;;; return the position of the center of the view in user coords

(define (center-point (self <view>))
  (let* ((f (view-frame self))
	 (o (view-origin self))
	 (device-center (make-point (+ (x o) (/ (size-width f) 2))
				    (+ (y o) (/ (size-height f) 2)))))
    (transform device-center (invert-transform (view-ctm self)))))

;;; pick a new view-origin such that the 
;;; given `center-pt' (a user coordinate)
;;; transforms to the center of the view frame

(define (set-center-point! (self <view>) center-pt)
  (let ((new-dc (transform center-pt (view-ctm self)))
	(f (view-frame self)))
    (set-view-origin! self
		      (make-point (- (x new-dc) (/ (size-width f) 2))
				  (- (y new-dc) (/ (size-height f) 2))))))

;;;
;;;  Compute the transformation from device coords to user coords
;;;  (device window coords, that is)
;;;

(define-method window-ictm ((self <view>))
  (translate (invert-transform (view-ctm self))
             (view-origin self)))

;; IT(ctm,pt)(0) = -pt

(define (inverse-translate ctm pt)
  (invert-transform
   (translate
    (invert-transform ctm)
    pt)))

;;

(define *global-properties* (make-symbol-table))
(define *session-properties* (make-symbol-table))

(define-method get-inheritable-property ((self <boolean>) name)
  (or (get-property (current-client) name #f)
      (table-lookup *session-properties* name)
      (table-lookup *global-properties* name)))

(define-method get-inheritable-property ((self <dv-object>) name)
  (or (get-property self name #f)
      (get-inheritable-property (parent-object self) name)))

(define-method parent-object ((self <document>))
  #f)

(define-method parent-object ((self <graphic-object>))
  (in-document self))

(define-method parent-object ((self <page>))
  (in-document self))

(define-method parent-object ((self <view>))
  (view-page self))

;;;
;;;  make a clone of a <view> object
;;;

(define-method make-clone ((self <view>))
  (let ((v (clone self))
        (d (in-document self)))
    (set-document-views! d (append (document-views d) (list v)))
    v))
