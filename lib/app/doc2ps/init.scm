,(use tables)
,(use rs.util.properties)
,(use rs.util.msgs)
,(use graphics.afm)
,(use graphics.geometry)

;;; BASELINE STUFF

;(load "../../gui/app/dv/font.scm")
(load "../../gui/app/dv/dev/device.scm")
(load "../../gui/app/dv/dev/gs/driver.scm")

(define *dev* (open-gs-device))

;;
