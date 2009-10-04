
(init-font-database)

;;;
;;;  Extra fonts to load into the font manager (graphics.fontmgr)
;;;

(load-font "Minion" "Condensed" 
           postscript: "Minion-Condensed"
           weight: 'normal
           angle: 'normal
           width: 'condensed

           pitch: 'proportional
           serif: 'serif
           symbolic: 'normal
           script: 'normal
           caps: 'normal)
           

(load-font "Minion" "Condensed Italic" 
           postscript: "Minion-CondensedItalic"
           weight: 'normal
           angle: 'italic
           width: 'condensed

           pitch: 'proportional
           serif: 'serif
           symbolic: 'normal
           script: 'normal
           caps: 'normal)

           
(load-font "Minion" "Condensed Bold" 
           postscript: "Minion-BoldCondensed"
           weight: 'bold
           angle: 'normal
           width: 'condensed

           pitch: 'proportional
           serif: 'serif
           symbolic: 'normal
           script: 'normal
           caps: 'normal)


(load-font "Minion" "Condensed Bold Italic" 
           postscript: "Minion-BoldCondensedItalic"
           weight: 'bold
           angle: 'italic
           width: 'condensed

           pitch: 'proportional
           serif: 'serif
           symbolic: 'normal
           script: 'normal
           caps: 'normal)


(load-font "Univers" "UltraCondensed" 
           postscript: "Univers-UltraCondensed"
           weight: 'normal
           angle: 'normal
           width: 'ultra-condensed

           pitch: 'proportional
           serif: 'sans
           symbolic: 'normal
           script: 'normal
           caps: 'normal)


(load-font "BriemMono" "Condensed" 
           postscript: "BriemMono-Condensed"
           weight: 'normal
           angle: 'normal
           width: 'condensed

           pitch: 'fixed
           serif: 'serif
           symbolic: 'normal
           script: 'normal
           caps: 'normal)


(load-font "BriemMono" "Condensed Bold" 
           postscript: "BriemMono-CondensedBold"
           weight: 'bold
           angle: 'normal
           width: 'condensed

           pitch: 'fixed
           serif: 'serif
           symbolic: 'normal
           script: 'normal
           caps: 'normal)



#|
(with-module graphics.charpath
  (for-each load-font-outlines (query-font-database (current-font-database))))
|#
