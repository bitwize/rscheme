(define $dv-version "0.7.3.1")

,(use rs.util.msgs)
(define-message-table app.dv 701)

,(use rs.sys.threads.shell)
,(use rs.sys.threads.manager)
,(use rs.util.properties)
,(use paths)

,(use gui.x
      gui.util.x
      graphics.geometry 
      graphics.afm
      graphics.png
      graphics.image
      graphics.color)

,(use rs.util.charset)
,(use rs.util.quantity)

,(use tables)

(load "length.scm")
(load "font.scm")
(load "client.scm")
(load "colors.scm")

;(load "resources.scm")

;;;

(load "keystate.scm")

;;;

(load "objmodel.scm")

(load "dev/device.scm")
(load "dev/x11/driver.scm")
(load "dev/pick/driver.scm")
(load "dev/virtual.scm")
(load "dev/outline.scm")
(load "dev/bbox/driver.scm")

;(load "dev/bbox/driver.scm")
;(load "dev/bbox/driver.scm")

(load "app-cursors.scm")
(load "scrollbar.scm")
(load "open-doc.scm")
(load "interactive.scm")

(load "redraw.scm")
(load "pageframe.scm")
(load "grid.scm")
(load "major.scm")
(load "explode.scm")
(load "styles.scm")
(load "morestyles.scm")

(load "objects/box.scm")
(load "objects/line.scm")
(load "objects/text.scm")
(load "objects/path.scm")
(load "objects/script.scm")

(load "eventmgr.scm")

(load "about.scm")
(load "save.scm")
(load "menu.scm")

(load "inspection.scm")
(load "minibuffer.scm")
(load "print.scm")
(load "data-inspect.scm") ;; a general-purpose RScheme data inspector
(load "scaleview.scm")
(load "selectfont.scm")
(load "dialog/dialog.scm")
(load "dialog/inputtext.scm")
(load "cutpaste.scm")
(load "exit.scm")
(load "close.scm")
(load "svg.scm")

(load "sound.scm")

(load "macros/framework.scm")

;;; -------------------- graphical toolbox ---------
;(load "ximages.scm")
;(load "composite.scm")

(load "toolbox.scm")
;;; ------------------------------------------------

(global-set-key '(#\C-x #\C-c) exit-client-with-review)

(load "make-test-doc.scm")
(load "make-new-doc.scm")
(load "main.scm")

;; pre-load some AFMs 
(get-afm "Times-Roman")
(get-afm "Helvetica-Bold")
(get-afm "Times-Italic")

