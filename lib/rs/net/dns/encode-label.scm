
(def (label->bs lab)
  (bs-append (uint8->bs (len lab)) lab))

(def char-string->bs label->bs)

