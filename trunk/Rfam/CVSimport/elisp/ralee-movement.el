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




(provide 'ralee-movement)