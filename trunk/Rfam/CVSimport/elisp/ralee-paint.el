;;; ralee-paint

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



(defun paint-line-by-pairs (pairs-list)
  "colour the current line according to the SS_cons line"
  (let ((pair ()) 
	open 
	close 
	openbase 
	closebase 
	helices)

    (put-text-property (line-beginning-position) (line-end-position) 'face 'default)

    (setq helices (ralee-helix-map pairs-list))

    (while pairs-list
      (setq pair (car pairs-list))
      (setq pairs-list (cdr pairs-list))

      (move-to-column (car pair))
      (setq open (point))
      (setq openbase (char-after))

      (move-to-column (cdr pair))
      (setq close (point))
      (setq closebase (char-after))

      ; remainder operator guarantees that we'll get a colour
      (setq face-num (% (cdr (assoc (car pair) helices)) (length ralee-faces)))

      (let ((case-fold-search 1)) ; case-independent search
	(if (or
	     (and (char-equal openbase ?C) (char-equal closebase ?G))
	     (and (char-equal openbase ?G) (or (char-equal closebase ?C)
					       (char-equal closebase ?U)
					       (char-equal closebase ?T)))
	     (and (char-equal openbase ?A) (or (char-equal closebase ?U)
					       (char-equal closebase ?T)))
	     (and (or (char-equal openbase ?U)
		      (char-equal openbase ?T)) (or (char-equal closebase ?A)
						    (char-equal closebase ?G))))

	    (progn
	      (put-text-property open (1+ open) 'face (nth face-num ralee-faces))
	      (put-text-property close (1+ close) 'face (nth face-num ralee-faces))
	      ))))))


(defun paint-line-by-base ()
  "colour the current line according to base identity"
  (interactive)
  (save-excursion
    (let (beg
	  end
	  face-num,
	  base)
      (beginning-of-line) (setq beg (point))
      (end-of-line) (setq end (point))
      (put-text-property beg end 'face 'default)
      
      (search-forward "\n")
      (search-backward " ") ; so we end up at the start of the sequence
     
      (while (< (point) end)  ; until the end of the line
	(forward-char)
	(setq base (char-after))
	(setq face-num nil)
	
	(let ((case-fold-search t)) ; case independent search
	  (if (char-equal base ?G)
	      (setq face-num 0))
	  (if (char-equal base ?C)
	      (setq face-num 1))
	  (if (char-equal base ?A)
	      (setq face-num 2))
	  (if (or (char-equal base ?T) (char-equal base ?U))
	      (setq face-num 3))
	
	  (if face-num
	      (put-text-property (point) (1+ (point)) 'face (nth face-num ralee-faces))))))))


(defun paint-column-by-cons ()
  "paint a column by its conservation"
  (interactive)
  (save-excursion
    (let (column
	  (a-count 0)
	  (c-count 0)
	  (g-count 0)
	  (u-count 0)
	  (cons 0.3))

      (setq column (current-column))
      (goto-char (point-max))
      (setq line-count (current-line))
      (goto-char (point-min))

      (while (< (point) (point-max))   ; go round once counting bases
	(if (ralee-is-alignment-line)
	    (progn
	      (move-to-column column)
	      (put-text-property (point) (1+ (point)) 'face 'default)
	      (setq base (char-after))
	      (let ((case-fold-search t))
		(if (char-equal base ?A) (setq a-count (1+ a-count)))
		(if (char-equal base ?C) (setq c-count (1+ c-count)))
		(if (char-equal base ?G) (setq g-count (1+ g-count)))
		(if (or (char-equal base ?U)
			(char-equal base ?T)) (setq u-count (1+ u-count)))
		)))
	(forward-line))

      (if (or (> (/ (float a-count) (float line-count)) cons)
	      (> (/ (float c-count) (float line-count)) cons)
	      (> (/ (float g-count) (float line-count)) cons)
	      (> (/ (float u-count) (float line-count)) cons))

	  (progn
	    (goto-char (point-min))

	    (while (< (point) (point-max))  ; go round again colouring them
	      (if (ralee-is-alignment-line)
		  (progn
		    (move-to-column column)
		    (setq base (char-after))
		    (let ((case-fold-search t))
		      (if (and (char-equal base ?A) (> (/ (float a-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 0 ralee-faces)))
		      (if (and (char-equal base ?C) (> (/ (float c-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 1 ralee-faces)))
		      (if (and (char-equal base ?G) (> (/ (float g-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 2 ralee-faces)))
		      (if (and (or (char-equal base ?T)
				   (char-equal base ?U)) (> (/ (float u-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 3 ralee-faces)))
		      )))
	      (forward-line))
	    )))))


(defun paint-buffer-by-cons ()
  "paint whole buffer by column conservation"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (equal (ralee-is-alignment-line) nil)
      (forward-line)) ; search for the first alignment line
    (search-forward "\n")
    (search-backward " ")
    (while (< (point) (line-end-position))
      (paint-column-by-cons)
      (forward-char))))



(defun paint-buffer-by-base ()
  "colour the current line according to base identity"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (paint-line-by-base))
      (forward-line))))


(defun paint-line-by-ss ()
  "get the structure, and then paint the current line"
  (interactive)
  (save-excursion
    (setq pairs (ralee-get-base-pairs))
    (paint-line-by-pairs pairs)))


(defun paint-buffer-by-ss ()
  "get the structure, and then paint the whole buffer"
  (interactive)
  (save-excursion
    (setq pairs (ralee-get-base-pairs))
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (paint-line-by-pairs pairs))
      (forward-line))))


(provide 'ralee-paint)