;;; ralee-movement

; Copyright (c) 2004 Sam Griffiths-Jones
;
; This is part of RALEE -- see
; http://www.sanger.ac.uk/Users/sgj/code/ralee/ and the README file
; that should accompany this file.
;
; RALEE is free software; you can redistribute it and/or modify it
; under the terms of the GNU General Public License as published by
; the Free Software Foundation; either version 2 of the License, or
; (at your option) any later version.
;
; RALEE is distributed in the hope that it will be useful, but
; WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
; General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with RALEE; if not, write to the Free Software Foundation,
; Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA




(defun jump-right ()
  "move the pointer jump-num characters to the right"
  (interactive)
  (forward-char ralee-jump-num))

(defun jump-left ()
  "move the pointer jump-num characters to the left"
  (interactive)
  (backward-char ralee-jump-num))

(defun jump-up ()
  "move the pointer jump-num lines up"
  (interactive)
  (let ((column (current-column)))
    (forward-line (- 0 ralee-jump-num))
    (move-to-column column)))

(defun jump-down ()
  "move the pointer jump-num lines down"
  (interactive)
  (let ((column (current-column)))
    (forward-line ralee-jump-num)
    (move-to-column column)))

(defun jump-to-pair ()
  "jump to the pairing base"
  (interactive)
  (let (paired-column)
    (setq paired-column (ralee-paired-column (current-column)))
    (if paired-column
	(progn
	  (message "column %s pairs with column %s" (current-column) paired-column)
	  (move-to-column paired-column))
      (message "No pair!"))))


(defun jump-to-pair-in-other-window ()
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