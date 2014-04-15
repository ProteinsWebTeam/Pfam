;;; ralee-structure

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


(defun ralee-helix-map (pairs)
  "Calculate helix boundaries based on pairs."
  (let ((helix 0)
	(lastopen 0)
	(lastclose 9999999)
	pair
	open
	(i (length pairs))
	j
	close
	helices)

    (while (>= i 1)
      (setq i (1- i))
      (setq pair (nth i pairs))
      (setq open (car pair))
      (setq close (cdr pair))

      ; we're moving from right to left based on closing pair
      ;
      ; catch things like
      ; <<..>>..<<..>>
      ;          *
      (if (> open lastclose)
	  (setq helix (+ helix 1))
	)

      ; catch things like
      ; <<..<<..>>..<<..>>>>
      ;                   *
      (setq j 0)
      (while (nth j pairs)
	(setq p (nth j pairs))
	(setq j (1+ j))
	(if (and (< (car p) lastopen)
		 (> (car p) open))
	    (if (equal (assoc (car p) helices) helix)
		()
	      (setq helix (+ helix 2)) ;; don't know why, but +1 doesn't always work!
	      )
	  )
	)

      (setq helices (cons (cons close helix) helices))
      (setq helices (cons (cons open helix) helices))

      (setq lastopen open)
      (setq lastclose close)

      )
    helices
    )
  )


(defun ralee-get-ss-line ()
  "Get the SS_cons line"
  (save-excursion
    (let (beg end)
      (goto-char (point-min))
      (search-forward "#=GC SS_cons")
      (beginning-of-line) (setq beg (point))
      (end-of-line) (setq end (point))
      (copy-region-as-kill beg end)
      (car kill-ring)
      )
    )
  )


(defun seedify-structure-line ()
  "Make the current structure line look like it should
in an Rfam seed alignment"
  (interactive)
  (save-excursion
    (end-of-line)
    (let ((eol (point)))
      (ralee-find-first-column)
      ;; perform-replace should set limits but I can't get it to work
      (save-restriction
	(narrow-to-region (point) eol)
	(goto-char (point-min))	(perform-replace ":" "." nil nil nil)
	(goto-char (point-min))	(perform-replace "_" "." nil nil nil)
	(goto-char (point-min))	(perform-replace "-" "." nil nil nil)
	(goto-char (point-min))	(perform-replace "," "." nil nil nil)
	(goto-char (point-min))	(perform-replace "(" "<" nil nil nil)
	(goto-char (point-min))	(perform-replace "[" "<" nil nil nil)
	(goto-char (point-min))	(perform-replace "{" "<" nil nil nil)
	(goto-char (point-min))	(perform-replace ")" ">" nil nil nil)
	(goto-char (point-min))	(perform-replace "]" ">" nil nil nil)
	(goto-char (point-min))	(perform-replace "}" ">" nil nil nil)
	)
      )
    )
  )

 

(defun ralee-get-current-ss-line ()
  "Get the current SS line"
  (save-excursion
    (let (beg end)
      (beginning-of-line) (setq beg (point))
      (if (or (looking-at (concat "#=GR " ralee-seqid-regex " SS "))
	      (looking-at "#=GC SS_cons"))
	  (progn
	    (end-of-line) (setq end (point))
	    (copy-region-as-kill beg end)
	    (car kill-ring)
	    )
	nil
	)
      )
    )
  )


(defun copy-current-ss-to-cons () 
  "Grab the current SS line, and copy it to the SS_cons line"
  (interactive)
  (save-excursion
    (if (ralee-is-markup-line)
	(let ((ssline (ralee-get-seq-string))
	      (col (ralee-find-first-column)))
	  ;; remove the old SS_cons line
	  (if (search-forward "#=GC SS_cons" nil t nil)
	      (progn
		(beginning-of-line)
		(kill-line)
		)
	    (search-forward "//")
	    (beginning-of-line)
	    (insert "\n")
	    (forward-line -1)
	    )
	  
	  (beginning-of-line)
	  (insert "#=GC SS_cons")
	  (while (< (current-column) col)
	    (insert " ")
	    )
	  (insert ssline)
	  )
      )
    )
  )


(defun ralee-structure-has-changed (structure-line)
  "check if the structure has changed"
  (if (equal structure-line ralee-structure-cache)
      nil
    t
    )
  )


(defun ralee-get-base-pairs (structure-line)
  "Parse the secondary structure markup.
Returns a list of pairs in order of increasing closing base."
  (save-excursion
    (let ((stack ())
	  (pairs ())
	  base
	  )

      ; only recalculate the base pairing structure if structure has changed
      (if (ralee-structure-has-changed structure-line)
	  (progn
	    (setq split (split-string structure-line ""))
	    (let ((i 0))

	      ; Grrr
	      ; GNU emacs: (split-string "AACC" "") => ("A" "A" "C" "C")
	      ; xemacs:    (split-string "AACC" "") => ("" "A" "A" "C" "C" "")
	      (if (equal (nth 0 split) "")
		  (setq split (cdr split))
		)

	      (while (< i (length split))
		(setq base (string-to-char (nth i split)))
		(if (and base (or (char-equal base ?\<)
				  (char-equal base ?\()
				  (char-equal base ?\[)
				  (char-equal base ?\{)))
		    (setq stack (cons i stack))
		  )
		(if (and base (or (char-equal base ?\>)
				  (char-equal base ?\))
				  (char-equal base ?\])
				  (char-equal base ?\})))
		    (progn
		      (setq pairs (cons (cons (car stack) i) pairs))
		      (setq stack (cdr stack)))
		  )
		(setq i (1+ i))
		)
	      )
	    (setq ralee-structure-cache structure-line)   ; cache these for speed
	    (setq ralee-base-pairs-cache pairs)
	    )
	)
      ralee-base-pairs-cache
      )
    )
  )






(defun ralee-paired-column (column)
  "return the pair of <column>"
  (let (pair-column
	pairs
	pair
	structure-line)
    (setq structure-line (ralee-get-ss-line))
    (setq pairs (ralee-get-base-pairs structure-line))
    (setq pair (assoc column pairs))
    (if pair
	(setq paired-column (cdr pair))
      (progn
	(setq pair (rassoc column pairs))
	(if pair
	    (setq paired-column (car pair)))))
    
    (if paired-column
	paired-column
      nil)))


(defun remove-base-pair ()
  "delete the base pair involving the current column"
  (interactive)
  (save-excursion
    (let ((c1 (current-column))
	  (c2 (ralee-paired-column (current-column))))
      (if c2
	  (progn
	    (goto-char (point-min))
	    (search-forward "#=GC SS_cons")
	    (move-to-column c1)
	    (delete-char 1)
	    (insert ".")
	    (move-to-column c2)
	    (delete-char 1)
	    (insert ".")
	    (message "Removed pair between columns %s and %s" c1 c2)
	    )
	(message "No pair!")
	)
      )
    )
  )


(defun add-base-pair ()
  "add a base pair between current and marked columns"
  (interactive)
  (save-excursion
    (let (c1 c2)
      (if (ralee-is-alignment-column)
	  (setq c1 (current-column))
	)
      (goto-char (mark))
      (if (ralee-is-alignment-column)
	  (setq c2 (current-column))
	)

      (if (and c1 c2)
	  (let ((cols (sort (list c1 c2) '<)))
	    (goto-char (point-min))
	    (search-forward "#=GC SS_cons")
	    (move-to-column (car cols))
	    (delete-char 1)
	    (insert "(")
	    ;; (cdr cols) doesn't work here -- wrong type?
	    (move-to-column (nth 1 cols))
	    (delete-char 1)
	    (insert ")")
	    (message "Added pair between columns %s and %s" c1 c2)
	    )
	;; else
	(message "Pairing column is undefined - define with C-space")
	)
      )
    )
  )


(provide 'ralee-structure)
