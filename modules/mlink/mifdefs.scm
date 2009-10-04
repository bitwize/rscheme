#|------------------------------------------------------------*-Scheme-*--|
 | File:    modules/mlink/mifdefs.scm
 |
 |          Copyright (C)1997 Donovan Kolbly <d.kolbly@rscheme.org>
 |          as part of the RScheme project, licensed for free use.
 |          See <http://www.rscheme.org/> for the latest information.
 |
 | File version:     1.3
 | File mod date:    2003-10-13 13:01:45
 | System build:     v0.7.3.4-b7u, 2007-05-30
 | Owned by module:  mlink
 |
 | Purpose:          standard mif class bindings
 `------------------------------------------------------------------------|#

(mifio-class "<<class>>" <<class>>)
(mifio-class "<symbol>" <symbol>)
(mifio-class "<closure>" <closure>)
(mifio-class "<vector>" <vector>)
(mifio-class "<string>" <string>)
(mifio-class "<pair>" <pair>)
(mifio-class "<double-float>" <double-float>)
(mifio-class "<table>" <table>)
(mifio-class "<string-table>" <string-table>)
(mifio-class "<string-ci-table>" <string-ci-table>)
(mifio-class "<object-table>" <object-table>)
(mifio-class "<eq-table>" <eq-table>)
(mifio-class "<table-bucket>" <table-bucket>)


;;
;;  these are built in to 0.6, but not 0.5

(mifio-class "<top-level-var>" <top-level-var>)
(mifio-class "<method>" <method>)
(mifio-class "<<standard-class>>" <<standard-class>>)
(mifio-class "<top-level-contour>" <top-level-contour>)

;; note that a <lexical-contour> can occur in a module image if
;; a <macro> captures a contour containing macros.  This may be
;; a good reason right here to distinguish variable-type contours
;; from syntactic contours, since the latter preserve top-levelness
;; and can be in an image, wheras the former do not.

(mifio-class "<lexical-contour>" <lexical-contour>)

(mifio-class "<single-dispatch-gf>" <single-dispatch-gf>)
(mifio-class "<binding-envt>" <binding-envt>)
(mifio-class "<slot-descriptor>" <slot-descriptor>)

(mifio-class "<rewriter>" <rewriter>)
(mifio-class "<macro>" <macro>)
(mifio-class "<macro-form>" <macro-form>)
(mifio-class "<substitution>" <substitution>)
(mifio-class "<primop>" <primop>)

(mifio-class "<getter>" <getter>)
(mifio-class "<setter>" <setter>)
(mifio-class "<template>" <template>)

(mifio-class "<long-int>" <long-int>)

(mifio-class "<bignum>" <bignum>)
(mifio-class "<mp-rational>" <mp-rational>)

(mifio-class "<mp-data>" <mp-data>)
