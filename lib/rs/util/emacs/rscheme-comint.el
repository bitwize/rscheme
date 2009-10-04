(defun comint-output-filter (process string)
  ;; First check for killed buffer
  (let ((oprocbuf (process-buffer process)))
    (if (and oprocbuf (buffer-name oprocbuf))
	(let ((obuf (current-buffer)))
	  (set-buffer oprocbuf)
	  (rscheme-ins-filtered process string obuf)))))

(defun rscheme-insbuf (process string obuf)
  (let ((opoint (point))
	(obeg (point-min)) 
	(oend (point-max))
	(buffer-read-only nil)
	(nchars (length string))
	(ostart nil))
    (widen)
    (goto-char (process-mark process))
    (setq ostart (point))
    (if (<= (point) opoint)
	(setq opoint (+ opoint nchars)))
    ;; Insert after old_begv, but before old_zv.
    (if (< (point) obeg)
	(setq obeg (+ obeg nchars)))
    (if (<= (point) oend)
	(setq oend (+ oend nchars)))
    (insert-before-markers string)
    ;; Don't insert initial prompt outside the top of the window.
    (if (= (window-start (selected-window)) (point))
	(set-window-start (selected-window) (- (point) (length string))))
    (if (and comint-last-input-end
	     (marker-buffer comint-last-input-end)
	     (= (point) comint-last-input-end))
	(set-marker comint-last-input-end (- comint-last-input-end nchars)))
    (set-marker comint-last-output-start ostart)
    (set-marker (process-mark process) (point))
    (force-mode-line-update)
    (narrow-to-region obeg oend)
    (goto-char opoint)
    (run-hook-with-args 'comint-output-filter-functions string)
    (set-buffer obuf)))

(defun rscheme-ins-filtered (process string obuf)
  (let ((start (string-match "\e\\[" string)))
    (if start
	(let ((end (string-match "\e\\]" string)))
	  (if (> start 0)
	      (rscheme-insbuf process (substring string 0 start) obuf))
	  (rscheme-esc-hook (substring string (+ 2 start) end))
	  (rscheme-ins-filtered process (substring string (+ 2 end)) obuf))
      (rscheme-insbuf process string obuf))))

(setq esc-stuff '())
(defun rscheme-esc-hook (string)
  (setq esc-stuff (cons string esc-stuff))
  (eval (car (read-from-string string))))

esc-stuff

(car (read-from-string "(foo bar)"))