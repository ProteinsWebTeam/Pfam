;;; ralee-movement

(defun ralee-jump-right ()
  "move the pointer jump-num characters to the right"
  (interactive)
  (forward-char ralee-jump-num))

(defun ralee-jump-left ()
  "move the pointer jump-num characters to the left"
  (interactive)
  (backward-char ralee-jump-num))

(defun ralee-jump-up ()
  "move the pointer jump-num lines up"
  (interactive)
  (let ((column (current-column)))
    (forward-line (- 0 ralee-jump-num))
    (move-to-column column)))

(defun ralee-jump-down ()
  "move the pointer jump-num lines down"
  (interactive)
  (let ((column (current-column)))
    (forward-line ralee-jump-num)
    (move-to-column column)))

(defun ralee-jump-to-pair ()
  "jump to the pairing base"
  (interactive)
  (let (paired-column)
    (setq paired-column (ralee-paired-column (current-column)))
    (if paired-column
	(progn
	  (message "column %s pairs with column %s" (current-column) paired-column)
	  (move-to-column paired-column))
      (message "No pair!"))))


(defun ralee-jump-to-pair-in-other-window ()
  "jump the cursor to the pairing base in other window
- make the other window if necessary"
  (interactive)
  (let (paired-column
	line)
    (setq line (current-line))
    (setq paired-column (ralee-paired-column (current-column)))
    (if paired-column
	(progn
	  (if (one-window-p)
	      (split-window))  ; make another window if there isn't already one
	  (message "column %s pairs with column %s" (current-column) paired-column)
	  (select-window (next-window))
	  (goto-line line)
	  (move-to-column paired-column)
	  (recenter))
      (message "No pair!"))))




(provide 'ralee-movement)