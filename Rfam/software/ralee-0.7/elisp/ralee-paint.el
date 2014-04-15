;;; ralee-paint

; Copyright (c) 2004-2011 Sam Griffiths-Jones
; Author: Sam Griffiths-Jones <sam.griffiths-jones@manchester.ac.uk>
;
; This is part of RALEE -- see
; http://personalpages.manchester.ac.uk/staff/sam.griffiths-jones/software/ralee/
; and the 00README file that should accompany this file.
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


(defun paint-char ()
  (interactive)
  (put-text-property (point) (1+ (point)) 'face 'highlight-face)
  )


(defun unpaint-char ()
  (interactive)
  (put-text-property (point) (1+ (point)) 'face 'default)
  )


(defun toggle-paint-char ()
  (interactive)
  (let ((face (get-text-property (point) 'face)))
    (if (string-match "default" (prin1-to-string face))
	(paint-char)
      (unpaint-char)
      )
    (forward-char)
    )
  )


(defun paint-line-by-pairs (pairs-list)
  "colour the current line according to the SS_cons line"
  (freeze-undo-list)
  (freeze-modified-state)
  (let ((pair ()) 
	open 
	close 
	openbase 
	closebase 
	helices
	line-start
	line-end)

    (beginning-of-line) (setq line-start (point))
    (end-of-line) (setq line-end (point))
    (put-text-property line-start line-end 'face 'default)

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
      (setq face-num (% (cdr (assoc (car pair) helices)) (length structure-faces)))

      (let ((case-fold-search 1)) ; case-independent search
	(if (ralee-is-pair openbase closebase)
	    (progn
	      (put-text-property open (1+ open) 'face (car (nth face-num structure-faces)))
	      (put-text-property close (1+ close) 'face (car (nth face-num structure-faces)))
	      )
	  )
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )



(defun paint-column-by-base ()
  "paint a column by base id"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (let (column
	  counts
	  (cons 0.3)
	  )

      (setq column (current-column))
      (setq counts (ralee-count-bases-in-column))

      (if (or (> (/ (float (nth 1 counts)) (float (nth 0 counts))) cons)
	      (> (/ (float (nth 2 counts)) (float (nth 0 counts))) cons)
	      (> (/ (float (nth 3 counts)) (float (nth 0 counts))) cons)
	      (> (/ (float (nth 4 counts)) (float (nth 0 counts))) cons))

	  (progn
	    (goto-char (point-min))

	    (while (< (point) (point-max))  ; go round again colouring bases
	      (if (ralee-is-alignment-line)
		  (progn
		    (move-to-column column)
		    (setq base (char-after))
		    (let ((case-fold-search t))
		      (if (and (char-equal base ?A) (> (/ (float (nth 1 counts)) (nth 0 counts)) cons))
			  (put-text-property (point) (1+ (point)) 'face (car (nth 0 base-faces))))
		      (if (and (char-equal base ?C) (> (/ (float (nth 2 counts)) (nth 0 counts)) cons))
			  (put-text-property (point) (1+ (point)) 'face (car (nth 1 base-faces))))
		      (if (and (char-equal base ?G) (> (/ (float (nth 3 counts)) (nth 0 counts)) cons))
			  (put-text-property (point) (1+ (point)) 'face (car (nth 2 base-faces))))
		      (if (and (or (char-equal base ?T)
				   (char-equal base ?U)) (> (/ (float (nth 4 counts)) (nth 0 counts)) cons))
			  (put-text-property (point) (1+ (point)) 'face (car (nth 3 base-faces))))
		      )
		    )
		)
	      (forward-line)
	      )
	    )
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun paint-buffer-by-cons-alt ()
  "paint whole buffer by base cons coloured by id"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (put-text-property (point-min) (point-max) 'face 'default)
    (goto-char (point-min))
    (while (equal (ralee-is-alignment-line) nil)
      (forward-line)) ; search for the first alignment line
    (end-of-line)
    (let ((line-end (point)))
      (search-backward " ")
      (while (< (point) line-end)
	(paint-column-by-base)
	(forward-char)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )

(defun paint-buffer-by-base ()
  "paint whole buffer by base id"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (put-text-property (point-min) (point-max) 'face 'default)
    (goto-char (point-min))

    (ralee-find-first-line)
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (progn
	    (end-of-line)
	    (let ((eol (point)))
	      (ralee-find-first-column)
	      (while (< (point) eol)
		(setq base (char-after))
		(let ((case-fold-search t))
		  (if (char-equal base ?A)
		      (put-text-property (point) (1+ (point)) 'face (car (nth 0 base-faces))))
		  (if (char-equal base ?C)
		      (put-text-property (point) (1+ (point)) 'face (car (nth 1 base-faces))))
		  (if (char-equal base ?G)
		      (put-text-property (point) (1+ (point)) 'face (car (nth 2 base-faces))))
		  (if (or (char-equal base ?T)
			  (char-equal base ?U))
		      (put-text-property (point) (1+ (point)) 'face (car (nth 3 base-faces))))
		  )
		(forward-char)
		)
	      )
	    )
	)
      (forward-line)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun paint-buffer-by-compensatory-changes ()
  "markup the compensatory mutations"
  (interactive)
  (paint-buffer-by-compensatory 0)
  )

(defun paint-buffer-by-compensatory-all ()
  "markup the compensatory mutations"
  (interactive)
  (paint-buffer-by-compensatory 1)
  )


(defun paint-buffer-by-compensatory (scheme)
  "markup the compensatory mutations"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)

  (save-excursion
    ;; if we're not on an alignment line, then jump to the first
    ;; line, and colour according to that (-changes only)
    (if (not (ralee-is-alignment-line))
	(ralee-find-first-line)
      )

    (let (comp-pairs
	  line
	  pairs
	  pair
	  (curline (1+ (current-line)))) ;; not sure why it's 1+
      (put-text-property (point-min) (point-max) 'face 'default)
      (setq line (ralee-get-ss-line))
      (setq pairs (ralee-get-base-pairs line))

      ;; if the pair has compensatory mutations, push to new list
      (while pairs
	(setq pair (car pairs))
	(setq pairs (cdr pairs))
	
	(goto-line curline) ;; if we're doing -changes then colour
	;; according to the current line
	(if (> scheme 0)
	    (if (ralee-is-compensatory pair)
		(paint-column-pair-by-compensatory-all pair)
	      )
	  (paint-column-pair-by-compensatory-changes pair)
	  )
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun paint-column-pair-by-compensatory-changes (pair)
  "paint a pair of columns if there are compensatory changes"
  (interactive)
  ;; car pair is the open base column number
  ;; cdr pair is the close base column number

  (move-to-column (car pair))
  (setq refopenbase (char-after))
  (move-to-column (cdr pair))
  (setq refclosebase (char-after))

  (ralee-find-first-line)
  
  (while (< (point) (point-max))
    (move-to-column (car pair))
    (setq open (point))
    (setq openbase (char-after))

    (move-to-column (cdr pair))
    (setq close (point))
    (setq closebase (char-after))

    (let ((case-fold-search 1)) ; case-independent search
      (if (ralee-is-alignment-line)
	  (if (ralee-is-pair openbase closebase)
	      (progn
		;; double compatible change
		(if (and (not (char-equal openbase refopenbase)) 
			 (not (char-equal closebase refclosebase)))
		    (progn
		      (put-text-property open (1+ open) 'face (car (nth 0 comp-faces)))
		      (put-text-property close (1+ close) 'face (car (nth 0 comp-faces))))
		  )
		;; single compatible change
		(if (or (and (char-equal openbase refopenbase)
			     (not (char-equal closebase refclosebase)))
			(and (not (char-equal openbase refopenbase))
			     (char-equal closebase refclosebase)))
		    (progn
		      (put-text-property open (1+ open) 'face (car (nth 1 comp-faces)))
		      (put-text-property close (1+ close) 'face (car (nth 1 comp-faces))))
		  )
		)
	
	    ;; else
	    ;; double incompatible change
	    (if (and (not (char-equal openbase refopenbase)) 
		     (not (char-equal closebase refclosebase)))
		(progn
		  (put-text-property open (1+ open) 'face (car (nth 2 comp-faces)))
		  (put-text-property close (1+ close) 'face (car (nth 2 comp-faces))))
	      )
	    ;; single incompatible change
	    (if (or (and (char-equal openbase refopenbase)
			 (not (char-equal closebase refclosebase)))
		    (and (not (char-equal openbase refopenbase))
			 (char-equal closebase refclosebase)))
		(progn
		  (put-text-property open (1+ open) 'face (car (nth 3 comp-faces)))
		  (put-text-property close (1+ close) 'face (car (nth 3 comp-faces))))
	      )
	    ;; involving gap
	    (if (or (char-equal openbase ?.) (char-equal openbase ?-)
		    (char-equal closebase ?.) (char-equal closebase ?-))
		(progn
		  (put-text-property open (1+ open) 'face (car (nth 4 comp-faces)))
		  (put-text-property close (1+ close) 'face (car (nth 4 comp-faces))))
	      )
	    )
	)
      )

    (forward-line)
    )
  )


(defun paint-column-pair-by-compensatory-all (pair)
  "paint a pair of columns if there are compensatory changes"
  (interactive)
  ;; car pair is the open base column number
  ;; cdr pair is the close base column number

  (ralee-find-first-line)

  (while (< (point) (point-max))
    (move-to-column (car pair))
    (setq open (point))
    (setq openbase (char-after))

    (move-to-column (cdr pair))
    (setq close (point))
    (setq closebase (char-after))

    (let ((case-fold-search 1)) ; case-independent search
      (if (ralee-is-pair openbase closebase)
	  (progn
	    (if (char-equal openbase ?A) 
		(put-text-property open (1+ open) 'face (car (nth 0 base-faces))))
	    (if (char-equal openbase ?C)
		(put-text-property open (1+ open) 'face (car (nth 1 base-faces))))
	    (if (char-equal openbase ?G)
		(put-text-property open (1+ open) 'face (car (nth 2 base-faces))))
	    (if (or (char-equal openbase ?T)
		    (char-equal openbase ?U))
		(put-text-property open (1+ open) 'face (car (nth 3 base-faces))))

	    (if (char-equal closebase ?A) 
		(put-text-property close (1+ close) 'face (car (nth 0 base-faces))))
	    (if (char-equal closebase ?C)
		(put-text-property close (1+ close) 'face (car (nth 1 base-faces))))
	    (if (char-equal closebase ?G)
		(put-text-property close (1+ close) 'face (car (nth 2 base-faces))))
	    (if (or (char-equal closebase ?T)
		    (char-equal closebase ?U))
		(put-text-property close (1+ close) 'face (car (nth 3 base-faces))))
	    ;;	    (put-text-property open (1+ open) 'face (car (car structure-faces)))
	    ;;	    (put-text-property close (1+ close) 'face (car (car structure-faces)))
	    )
	)
      )
    (forward-line)
    )
  )



(defun paint-column-by-cons ()
  "paint a column by base conservation"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)

  (save-excursion
    (let (column
	  counts
	  )

      (setq column (current-column))
      (setq counts (ralee-count-bases-in-column))

      (if (or (> (/ (float (nth 1 counts)) (float (nth 0 counts))) 0.4)
	      (> (/ (float (nth 2 counts)) (float (nth 0 counts))) 0.4)
	      (> (/ (float (nth 3 counts)) (float (nth 0 counts))) 0.4)
	      (> (/ (float (nth 4 counts)) (float (nth 0 counts))) 0.4))

	  (progn
	    (goto-char (point-min))

	    (while (< (point) (point-max))  ; go round again colouring them
	      (if (ralee-is-alignment-line)
		  (progn
		    (move-to-column column)
		    (setq base (char-after))
		    (let ((case-fold-search t))
		      (if (or (and (char-equal base ?A) (> (/ (float (nth 1 counts)) (float (nth 0 counts))) 0.8))
			      (and (char-equal base ?C) (> (/ (float (nth 2 counts)) (float (nth 0 counts))) 0.8))
			      (and (char-equal base ?G) (> (/ (float (nth 3 counts)) (float (nth 0 counts))) 0.8))
			      (and (or (char-equal base ?U) 
				       (char-equal base ?T)) (> (/ (float (nth 4 counts)) (float (nth 0 counts))) 0.8)))

			  (put-text-property (point) (1+ (point)) 'face (car (nth 0 cons-faces)))

			(if (or (and (char-equal base ?A) (> (/ (float (nth 1 counts)) (float (nth 0 counts))) 0.6))
				(and (char-equal base ?C) (> (/ (float (nth 2 counts)) (float (nth 0 counts))) 0.6))
				(and (char-equal base ?G) (> (/ (float (nth 3 counts)) (float (nth 0 counts))) 0.6))
				(and (or (char-equal base ?U) 
					 (char-equal base ?T)) (> (/ (float (nth 4 counts)) (float (nth 0 counts))) 0.6)))

			    (put-text-property (point) (1+ (point)) 'face (car (nth 1 cons-faces)))

			  (if (or (and (char-equal base ?A) (> (/ (float (nth 1 counts)) (float (nth 0 counts))) 0.4))
				  (and (char-equal base ?C) (> (/ (float (nth 2 counts)) (float (nth 0 counts))) 0.4))
				  (and (char-equal base ?G) (> (/ (float (nth 3 counts)) (float (nth 0 counts))) 0.4))
				  (and (or (char-equal base ?U) 
					   (char-equal base ?T)) (> (/ (float (nth 4 counts)) (float (nth 0 counts))) 0.4)))

			      (put-text-property (point) (1+ (point)) 'face (car (nth 2 cons-faces)))

			    )
			  )
			)
		      )
		    )
		)
	      (forward-line)
	      )
	    )
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun paint-column-by-cons-generic ()
  "paint a column by base conservation"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)

  (save-excursion
    (let (column
	  counts
	  lines
	  )

      (setq column (current-column))
      (setq counts (ralee-count-column-symbols))
      (setq lines (ralee-number-seqs))

      (ralee-find-first-line)
      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (progn
	      (move-to-column column)
	      (setq base (upcase (char-after)))
	      (when (cdr (assq base counts))
		(if (> (/ (float (cdr (assq base counts))) lines) 0.4)
		    (if (> (/ (float (cdr (assq base counts))) lines) 0.8)
			(put-text-property (point) (1+ (point)) 'face (car (nth 0 cons-faces)))
		      (if (> (/ (float (cdr (assq base counts))) lines) 0.6)
			  (put-text-property (point) (1+ (point)) 'face (car (nth 1 cons-faces)))
			(put-text-property (point) (1+ (point)) 'face (car (nth 2 cons-faces)))
			)
		      )
		  )
		)
	      )
	  )
	(forward-line)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )



(defun paint-buffer-by-cons ()
  "paint whole buffer by conservation"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (put-text-property (point-min) (point-max) 'face 'default)
    (goto-char (point-min))
    (ralee-find-first-line)
    (end-of-line)
    (let ((line-end (point)))
      (search-backward " ")
      (while (< (point) line-end)
	(paint-column-by-cons)
	(forward-char)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun paint-buffer-by-cons-generic ()
  "paint whole buffer by conservation"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (put-text-property (point-min) (point-max) 'face 'default)
    (goto-char (point-min))
    (ralee-find-first-line)
    (end-of-line)
    (let ((line-end (point)))
      (ralee-find-first-column)
      (while (< (point) line-end)
	(paint-column-by-cons-generic)
	(forward-char)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )



(defun paint-line-by-ss ()
  "get the structure, and then paint the current line"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (let (line
	  pairs
	  )
      (setq line (ralee-get-ss-line))
      (setq pairs (ralee-get-base-pairs line))
      (paint-line-by-pairs pairs)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun paint-buffer-by-ss ()
  "get the structure, and then paint the whole buffer"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (let (line
	  pairs
	  )
      (put-text-property (point-min) (point-max) 'face 'default)
      (setq line (ralee-get-ss-line))
      (setq pairs (ralee-get-base-pairs line))
      (beginning-of-buffer)
      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (paint-line-by-pairs pairs))
	(forward-line)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )

(defun paint-buffer-by-current-ss-line ()
  "get structure from the current line, and then paint the whole buffer"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (let (line
	  pairs
	  )
      (put-text-property (point-min) (point-max) 'face 'default)
      (setq line (ralee-get-current-ss-line))
      (if line
	  (progn
	    (setq pairs (ralee-get-base-pairs line))
	    (beginning-of-buffer)
	    (while (< (point) (point-max))
	      (if (ralee-is-alignment-line)
		  (paint-line-by-pairs pairs))
	      (forward-line)
	      )
	    )
	(message "Current line doesn't contain structure markup")
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )



(defun lowlight-current-line ()
  "remove the highlighting from highlight-current-line"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (end-of-line)
    (let ((end (point)))
      (beginning-of-line)

      (while (< (point) end)
	(let ((face (get-text-property (point) 'face)))
	  (if (string-match "bold" (prin1-to-string face))
	      (if (car (rassoc face ralee-faces))
		  ;; pull out the corresponding plain face
		  (put-text-property (point) (1+ (point)) 'face (car (rassoc face ralee-faces)))
		(put-text-property (point) (1+ (point)) 'face 'default)
		)
	    )
	  )
	(forward-char)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defun lowlight-all (&optional except)
  "remove the highlighting from all lines"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)

  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (beginning-of-line)
      (if (and except (= (current-line) except))
	  ()
	(let ((face (get-text-property (point) 'face)))
	  ;; if this line is bold then wipe it out
	  (if (string-match "bold" (prin1-to-string face))
	      (lowlight-current-line)
	    )
	  )
	)
      (forward-line)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)  
  )


(defun highlight-current-line (&optional col1 col2)
  "show the line that the cursor is on"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)

  (save-excursion
    (if col2
	(move-to-column col2)
      (end-of-line)
      )
    
    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	(let ((end (point)))
	  
	  (if col1
	      (move-to-column col1)
	    (beginning-of-line)
	    )
	  
	  (while (< (point) end)
	    (let ((face (get-text-property (point) 'face)))
	      (if (cdr (assoc face ralee-faces))
		  ;; pull out the corresponding plain face
		  (put-text-property (point) (1+ (point)) 'face (cdr (assoc face ralee-faces)))
		(put-text-property (point) (1+ (point)) 'face 'bold)
		)
	      )
	    (forward-char)
	    )
	  )
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )


(defvar ralee-highlight-line nil)

(defun ralee-highlight-line (&optional arg)
  "Toggle ability to highlight a line with left click.
With no argument, this command toggles the mode. 
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode."
  (interactive)
  (if arg
      (setq ralee-highlight-line arg)
    (if ralee-highlight-line
        (setq ralee-highlight-line nil)
      (setq ralee-highlight-line t)
      )
    )
  )

(defun toggle-highlight-current-line ()
  "show the line that the cursor is on"
  (interactive)
  (if ralee-highlight-line
      (save-excursion
	(beginning-of-line)
	(let ((face (get-text-property (point) 'face)))
	  (if (string-match "bold" (prin1-to-string face))
	      (lowlight-current-line)
	    (lowlight-all)
	    (highlight-current-line)
	    )
	  )
	)
    )
  )


(defun highlight-block ()
  "highlight the defined block"
  (interactive)
  (save-excursion
    (lowlight-all)
    (goto-char (nth 1 ralee-block))
    (let ((c2 (current-column))
	  (r2 (current-line))
	  c1
	  r1)

      (goto-char (nth 0 ralee-block))
      (setq c1 (current-column))
      (while (<= (current-line) r2)
	(ralee-find-first-column)
	(highlight-current-line 0 (current-column))
	(highlight-current-line c1 c2)
	(forward-line)
	)
      )
    )
  )


(defun paint-block ()
  "paint the defined block"
  (interactive)
  (freeze-undo-list)
  (freeze-modified-state)
  (save-excursion
    (goto-char (nth 1 ralee-block))
    (let ((c2 (current-column))
	  (r2 (current-line))
	  c1
	  r1)

      (goto-char (nth 0 ralee-block))
      (setq c1 (current-column))
      (while (<= (current-line) r2)
	(move-to-column c1)
	(while (< (current-column) c2)
	  (paint-char)
	  (forward-char 1)
	  )
	(forward-line)
	)
      )
    )
  (thaw-undo-list)
  (thaw-modified-state)
  )



(defvar ralee-undo-save nil
  "Saved undo list.")

(defun freeze-undo-list ()
  "Save the undo list"
  (if ralee-undo-save
      ()
    (setq ralee-undo-save buffer-undo-list)
    )
  )

(defun thaw-undo-list ()
  "Reinstate the undo list"
  (if ralee-undo-save
      (setq buffer-undo-list ralee-undo-save)
;;    (print "No saved undo state")
    )
  (setq ralee-undo-save nil)
  )

(defvar ralee-modified-state nil
  "The modified state of the buffer."
  )

(defun freeze-modified-state ()
  "Save the modified state of the buffer"
  (setq ralee-modified-state (cons (buffer-modified-p) ralee-modified-state))
  )

(defun thaw-modified-state ()
  "Reinstate the modified state of the buffer"
  (if (listp ralee-modified-state)
      (let ((state (car ralee-modified-state)))
	(setq ralee-modified-state (cdr ralee-modified-state))
	(if (not ralee-modified-state)
	    (set-buffer-modified-p state)
	  )
	)
    (print "Warning: failed to reset buffer modified state")
    )
  )




(provide 'ralee-paint)
