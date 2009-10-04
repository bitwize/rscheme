(define (tag->name tag)
  (or (table-lookup *tiff-tags* tag) tag))

(define *tiff-tags* (make-table eq? integer->hash))

(vector-for-each 
 (lambda (ent)
   (table-insert! *tiff-tags* (car ent) (cadr ent)))
 '#((315 artist)
    (326 bad-fax-lines)
    (265 cell-length)

    (254 new-subfile-type)
    (256 image-width)
    (257 image-length)
    (258 bits-per-sample)
    (259 compression)
    (262 photometric-interpretation)
    (273 strip-offsets)
    (277 samples-per-pixel)
    (278 rows-per-strip)
    (279 strip-byte-counts)
    (282 x-resolution)
    (283 y-resolution)
    (296 resolution-unit)

    (320 color-map)
    (284 planar-configuration)

    (305 software)
    (274 orientation)))
