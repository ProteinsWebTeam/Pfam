;;; ralee-edit

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



(defun insert-gap-column ()
  "Insert a column of gap residues"
  (interactive)
  (save-excursion
    (if (ralee-is-alignment-column)
	(let ((column (current-column)))
	  (goto-char (point-min))
	  (while (< (point) (point-max))
	    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		(progn
		  (move-to-column column)
		  (insert ralee-gap-symbol)))
	    (forward-line)
	    )
	  )
      (message "Not within the alignment")
      )
    )
  )


(defun insert-gap ()
  "Insert a gap residues"
  (interactive)
  (if (ralee-is-alignment-column)
      (if (not overwrite-mode)
	  (insert ralee-gap-symbol)
	(progn
	  (if (or (not (ralee-is-alignment-line))
		  (char-equal (char-after) ?\.)
		  (char-equal (char-after) ?\-))
	      (progn
		(insert ralee-gap-symbol)
		(delete-char 1)
		)
	    (message "can't overwrite a non-gap character")
	    )
	  )
	)
    (message "Not within the alignment")
    )
  )


(defun delete-gap ()
  "delete a gap residue"
  (interactive)
  (save-excursion
    (if (ralee-is-alignment-column)
	(if (or (char-equal (char-after) ?\.)
		(char-equal (char-after) ?\-))
	    (delete-char 1)
	  (message "can't delete a non-gap character")
	  )
      (message "not within the alignment!")
      )
    )
  )

(defun delete-back-gap ()
  "backspace action but only for gap chars"
  (interactive)
  (save-excursion
    (if (ralee-is-alignment-column)
	(progn
	  (forward-char -1)
	  (if (or (char-equal (char-after) ?\.)
		  (char-equal (char-after) ?\-))
	      (delete-char 1)
	    (progn
	      (forward-char 1)
	      (message "can't delete a non-gap character")
	      )
	    )
	  )
      (message "not within the alignment!")
      )
    )
  )

(defun is-all-gap-column ()
  "test whether the current column is all gaps"
  (save-excursion
    (let (column notagap)
      (setq column (current-column))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column column)
	      (if (or (char-equal (char-after) ?\.)
		      (char-equal (char-after) ?\-))
		  ()
		(progn
		  (setq notagap t)
		  (goto-char (point-max)) ; break the loop
		  )
		)
	      )
	  )
	(forward-line)
	)
      (if notagap
	  nil
	t
	)
      )
    )
  )



(defun delete-gap-column ()
  "Delete a column but only if it is all gaps"
  (interactive)
  (save-excursion
    (if (ralee-is-alignment-column)
	(let (column)
	  (setq column (current-column))

	  (if (is-all-gap-column)
	      (progn
		(goto-char (point-min))
		(while (< (point) (point-max))
		  (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		      (progn
			(move-to-column column)
			(delete-char 1)
			)
		    )
		  (forward-line)
		  )
		)
	    (message "Column %s contains non-gap characters -- cannot delete" column)
	    )
	  )
      (message "not within the alignment!")
      )
    )
  )


(defun delete-all-gap-columns ()
  "delete all gapped columns"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    ; find me an alignment line
    (while (not (ralee-is-alignment-line))
     (forward-line)
     )
    (end-of-line)
    (search-backward " ")
    (let ((i 0))
      (while (< (point) (line-end-position))
	(if (is-all-gap-column)
	    (progn
	      (delete-gap-column)
	      (backward-char)
	      (setq i (1+ i))
	      )
	  )
	(forward-char)
	)
      (message "deleted %s all-gapped columns" i)
      )
    )
  )


(defun unblock-alignment ()
  "unblock a blocked alignment"
  (interactive)
  (goto-char (point-min))
  (let ((seqs ())
	seqid
	(ids ())
	seqstr
	(maxidlength 0)
	name)

    (while (< (point) (point-max))
      (beginning-of-line)
      (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	  (progn
	    (setq seqid (ralee-get-seq-id))
	    (setq seqstr (ralee-get-seq-string))
	    (if (assoc seqid seqs)
		(setq seqstr (concat (cdr (assoc seqid seqs)) seqstr))
	      (progn
		(push seqid ids)
		(if (> (length seqid) maxidlength)
		    (setq maxidlength (length seqid))
		  )
		)
	      )
	    (setq seqs (cons (cons seqid seqstr) seqs))
	    )
	)
      (forward-line)
      )

    ;; backup the current buffer and overwrite with unblocked alignment
    (setq name (buffer-name))
    (write-file (concat name ".bak"))
    (rename-buffer name)

    ;; Delete from the first alignment line thereby leaving
    ;; any annotation in the header intact.  #=GF lines in the
    ;; body of the alignment will be lost (bug).
    (ralee-find-first-line)
    (beginning-of-line)
    (kill-region (point) (point-max))

    (setq ids (reverse ids))
    (while ids
      (setq seqid (car ids))
      (setq ids (cdr ids))

      (insert seqid)

      (let ((i 0))
	(while (<= i (- (1+ maxidlength) (length seqid)))
	  (insert " ")
	  (setq i (1+ i))
	  )
	)
      (insert (cdr (assoc seqid seqs)))
      (insert "\n")
      )
    (insert "\/\/\n")

    ;; do this before the insert above and it all goes badly wrong
    (ralee-mode) ; new buffer doesn't inherit mode!
    )
  )



(defun trim-seq (&optional right)
  "trim the current sequence left of cursor position, or right if optional"
  (interactive)
  (save-excursion
    ;; trim the sequence itself
    (let ((start (point)))
      (if right
	  (end-of-line)
	(ralee-find-first-column)
	)
      (kill-region start (point))
      )

    ;; fix the name/start-end
    (let (nse
	  offset
	  st
	  en)

      (setq nse (ralee-get-name-start-end))

      (if (and (ralee-is-alignment-line) (nth 1 nse))  ; then seqid includes start end
	  (progn
	    (setq offset (length (ralee-ungap-string (car kill-ring))))

	    (setq st (nth 1 nse))
	    (setq en (nth 2 nse))
	    (if right
		(if (< en st)
		    (setq en (+ en offset))
		  (setq en (- en offset))
		  )
	      (if (< en st)
		  (setq st (- st offset))
		(setq st (+ st offset))
		)
	      )
	    (let ((newnse (concat (nth 0 nse) "/" 
				  (number-to-string st) "-" 
				  (number-to-string en)))
		  (oldnse (concat (nth 0 nse) "/" 
				  (number-to-string (nth 1 nse)) "-" 
				  (number-to-string (nth 2 nse))))
		  oldlen
		  newlen)

	      (setq oldlen (length oldnse))
	      (setq newlen (length newnse))

	      (ralee-find-first-column)
	      (let ((first (point)))
		(beginning-of-line)
		(kill-region (point) first)
		(insert newnse)
		(while (< (point) first)
		  (insert " ")
		  )
		)
	      (list oldnse newnse)  ; return this so we can fix #=GR lines
	      )
	    )
	)
      )
    )
  )



(defun trim-alignment (&optional right)
  "trim the alignment"
  (save-excursion
    (if (ralee-is-alignment-column)
	(let ((column (current-column))
	      (maxid (ralee-maxidlength))
	      firstcol)

	  (ralee-find-first-column)
	  (let ((pad (- (current-column) maxid))
		(temppad 0))

	    ;; pad the gap out a bit to remove later
	    (if (< (current-column) (+ maxid 8))
		(progn
		  (setq temppad (+ (- (current-column) maxid) 8))
		  ;; if we're going to pad then we need to fudge column to trim from 
		  (setq column (+ temppad column))
		  )
	      )

	    (goto-char (point-min))
	    (while (< (point) (point-max))
	      (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		  (progn
		    (ralee-find-first-column)
		    (let ((i 0))
		      (while (< i temppad)
			(insert " ")
			(setq i (1+ i))
			)
		      )
		    )
		)
	      (forward-line)
	      )

	    ;; now do the trim
	    (goto-char (point-min))
	    (while (< (point) (point-max))
	      (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		  (progn
		    (ralee-find-first-column)
		    (let ((firstcol (current-column)))
		      (move-to-column column)
		      (let ((oldnew (trim-seq right)))  ;; do the trim
			(if oldnew   ; then we need to check for another instance of the
			             ; nse string to replace, maybe a #=GR line
			    (save-excursion
			      (goto-char (point-min))
			      (while (search-forward (nth 0 oldnew) nil t)
				(end-of-line)
				(let ((eol (point)))
				  (beginning-of-line)

				  ;; perform-replace should set limits but I can't get it to work
				  (save-restriction
				    (narrow-to-region (point) eol)
				    (perform-replace (nth 0 oldnew) (nth 1 oldnew) nil nil nil)
				    )
				  )

				)
			      )
			    )
			)
		      )
		    )
		)
	      (forward-line)
	      )

	    ;; now remove the extra padding and left justify properly
	    (setq firstcol (+ (ralee-maxidlength) pad))
	    (goto-char (point-min))
	    (while (< (point) (point-max))
	      (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		  (progn
		    (move-to-column firstcol)
		    (let ((col (current-column)))
		      (ralee-find-first-column)
		      (while (> (current-column) col)
			(forward-char -1)
			(delete-char 1)
			)
		      (while (< (current-column) col)
			(insert " ")
			)
		      )
		    )
		)
	      (forward-line)
	      )
	    )
	  )
      (message "not within the alignment!")
      )
    )
  )


(defun trim-left ()
  "trim the alignment left of cursor position"
  (interactive)
  (trim-alignment)
  )


(defun trim-right ()
  "trim the alignment right of cursor position"
  (interactive)
  (trim-alignment t)
  )


(defun is-ragged-alignment ()
  "check whether the current alignment is flush
return nil if flush, column number if not"
  (save-excursion
    (ralee-find-first-line)
    (end-of-line)
    (let ((column (current-column))
	  return)
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line)
		(ralee-is-markup-line))
	    (progn
	      (end-of-line)
	      (if (< column (current-column))
		  (if (or (equal return nil)
			  (> (current-column) return))
		      (setq return (current-column))
		    )
		)
	      (if (> column (current-column))
		  (if (or (equal return nil)
			  (> column return))
		      (setq return column)
		    )
		)
	      )
	  )
	(forward-line)
	)
      return
      )
    )
  )


(defun make-flush-alignment ()
  "make the alignment flush at the right extent"
  (interactive)
  (save-excursion
    (let ((column (is-ragged-alignment)))
      (if column
	  (progn
	    (ralee-find-first-line)
	    (while (< (point) (point-max))
	      (if (or (ralee-is-alignment-line)
		      (ralee-is-markup-line))
		  (progn
		    (end-of-line)
		    (while (< (current-column) column)
		      (insert ralee-gap-symbol)
		      )
		    )
		)
	      (forward-line)
	      )
	    )
	)
      )
    )
  )


(defun left-justify-alignment ()
  "Make sure left end of sequences are justified properly.
This method is used to clean up after things like the trim methods"
  (interactive)
  (save-excursion
    (let ((maxid (ralee-maxidlength)))
      (goto-char (point-min))
      (ralee-find-first-line)
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column (+ maxid 6))
	      (let ((col (current-column)))
		(ralee-find-first-column)
		(while (> (current-column) col)
		  (forward-char -1)
		  (delete-char 1)
		  )
		(while (< (current-column) col)
		  (insert " ")
		  )
		)
	      )
	  )
	(forward-line)
	)
      )
    )
  )

(defun uppercase-alignment ()
  "make all sequences uppercase"
  (interactive)
  (save-excursion
    (ralee-find-first-line)
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (progn 
	    (ralee-find-first-column)
	    (let ((first (point)))
	      (end-of-line)
	      (upcase-region first (point))
	      )
	    )
	)
      (forward-line)
      )
    )
  )

(defun lowercase-alignment ()
  "make all sequences lowercase"
  (interactive)
  (save-excursion
    (ralee-find-first-line)
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (progn 
	    (ralee-find-first-column)
	    (let ((first (point)))
	      (end-of-line)
	      (downcase-region first (point))
	      )
	    )
	)
      (forward-line)
      )
    )
  )

(defun replace-in-alignment (from to &optional str)
  "perform replace operations only in the alignment region"
  (save-excursion
    (ralee-find-first-line)
    (while (< (point) (point-max))
      (if (or (ralee-is-alignment-line) (and str (ralee-is-markup-line)))
	  (progn 
	    (end-of-line)
	    (let ((end (point)))
	      (ralee-find-first-column)	      
	      ;; I struggle to get perform-replace to work as advertised
	      ;; so get round it with a narrow and widen
	      (save-restriction
		(narrow-to-region (point) end)
		(perform-replace (downcase from) (downcase to) nil nil nil)
		(goto-char (point-min))
		(perform-replace (upcase from) (upcase to) nil nil nil)
		)
	      )
	    )
	)
      (forward-line)
      )
    )
  )


(defun t-to-u ()
  "convert all T's to U's"
  (interactive)
  (replace-in-alignment "T" "U")
  )

(defun u-to-t ()
  "convert all U's to T's"
  (interactive)
  (replace-in-alignment "U" "T")
  )

(defun gap-to-dot ()
  "convert all gaps to dots"
  (interactive)
  (replace-in-alignment "-" "." t)
  (replace-in-alignment "," "." t)
  (replace-in-alignment ":" "." t)
  (replace-in-alignment "_" "." t)
  (replace-in-alignment "~" "." t)
)


(defun remove-all-gr-lines ()
  "remove all those pesky #=GR lines"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (looking-at (concat "#=GR " ralee-seqid-regex " SS "))
	  (let (eol)
	    (end-of-line)
	    (setq eol (point))
	    (beginning-of-line)
	    (kill-region (point) (1+ eol))
	    )
	;; else
	(forward-line)
	)
      )
    )
  ;; Something very odd happens here.  Immediately after a
  ;; remove-all-gr-lines, some methods have strange behaviour,
  ;; like colour-buffer-by-structure-consensus.  I can't work
  ;; out where this is coming from, but running anything else
  ;; between seems to fix it.
  )


;;
;; this doesn't work at the moment, and so is not interactive
;;
(defun shift-block-right ()
  "Take the block between point and mark and shift it right one space
dealing with insertion and deletion of gaps as we go"
  (save-excursion
    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	(let ((start (point))
	      (end (mark))
	      (line (current-line)))
	  (goto-char end)
	  (if (eq line (current-line))
	      (progn
		(if (> start end)
		    (let ((temp start))
		      (setq start end)
		      (setq end temp)
		      )
		  )
		(prin1 start)
		(prin1 end)
		
		(goto-char end)
		(if (or (char-equal (char-after) ?\.)
			(char-equal (char-after) ?\-))
		    ()
		  (insert-gap-column)
		  )
		(delete-char 1)
	    
		(goto-char start)
		(insert-gap)
		)

	    (message "point and mark are not on the same line")
	    )

	  )
      (message "not within the alignment")
      )
    )
  )


(defun shift-sequence (&optional direction blocksize)
  "Shift the block (to the next gap) right one space"
  (save-excursion
    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	(let ((start (point))
	      bol
	      eol
	      (step 1))

	  (if (not blocksize)
	      (setq blocksize 0)
	    )
	  
	  (ralee-find-first-column)
	  (setq bol (point))
	  (end-of-line)
	  (setq eol (point))

	  (goto-char start)
	  (insert-gap)

	  (if (string-match direction "l")
	      (progn
		(setq step -1)
		(forward-char step)  ;; otherwise we want to
		;; do char-before in the test for gaps below
		)
	    )

	  (if (> blocksize 0)
	      ;; we're going to push a sequence block of size blocksize
	      ;; whether or not it contains gaps (useful for GR lines)
	      (forward-char (* step blocksize))
;	      (let ((i 0))
;		(while (progn
;			 (forward-char step)
;			 (setq i (1+ i))
;			 (and (and (< (point) eol) (>= (point) bol))
;			      (< i blocksize))))
;		)
	    (while (progn
		     (forward-char step)
		     (setq blocksize (1+ blocksize))
		     (and (and (< (point) eol) (>= (point) bol))
			  (not (or (char-equal (char-after) ?\.)
				   (char-equal (char-after) ?\-))))))
	    )

	  (if (< (point) bol)
	      (progn
		(forward-char)
		(insert-gap-column)
		)
	    )

	  (if (eq (point) eol)
	      (progn
		(end-of-line)
		(insert-gap-column)
		)
	    )

	  (if (or (char-equal (char-after) ?\.)
		  (char-equal (char-after) ?\-))
	      (delete-gap)
	    )

	  )
      (message "not within the alignment")
      )
    )
  (if (not (string-match direction "l"))
      (forward-char 1)
    )
  ;; return the size of the block pushed so we can do the same again
  ;; with any associated #=GR lines
  blocksize
  )


(defun shift-associated-sequences (&optional direction)
  "Shift the current sequence and any associated #=GR lines"
  (interactive)
  (let (blocksize
	lines
	line
	(step 1)
	column)
    (setq blocksize (shift-sequence direction))
    (if (not (string-match direction "l"))
	(setq step -1)
      )
    (setq column (+ (current-column) step))
    (save-excursion
      (setq lines (ralee-find-gr-lines))
      (while lines
	(setq line (car lines))
	(setq lines (cdr lines))
	(goto-line line)
	(move-to-column column)
	(shift-sequence direction blocksize)
	)
      )
    )
  )
  
(defun shift-sequence-left ()
  "Shift the sequence (to the next gap) left one space"
  (interactive)
  (shift-associated-sequences "l")
  )

(defun shift-sequence-right ()
  "Shift the sequence (to the next gap) right one space"
  (interactive)
  (shift-associated-sequences "r")
  )
  

(defun throw-sequence (&optional direction)
  "Shift the sequence chunk as far as it can go"
  (let ((count 0)
	(step 1))
    (if (string-match direction "l")
	(progn
	  (setq step -1)
	  (forward-char step)
	  )
      )
    (save-excursion
      ;; work out how many gaps to take out
      (if (or (char-equal (char-after) ?\.)
		 (char-equal (char-after) ?\-))
	  ()
	(while (not (or (char-equal (char-after) ?\.)
			(char-equal (char-after) ?\-)))
	  (forward-char step)
	  )
	(while (or (char-equal (char-after) ?\.)
		   (char-equal (char-after) ?\-))
	  (forward-char step)
	  (setq count (1+ count))
	  )
	)
      )

    (if (string-match direction "l")
	(forward-char 1)
      )

    (if (= count 0)
	(forward-char step)
      (dotimes (i count)
	(shift-associated-sequences direction)
	)
      )
    )
  )


(defun throw-sequence-left ()
  "Shift the sequence chunk left as far as it can go"
  (interactive)
  (throw-sequence "l")
  )

(defun throw-sequence-right ()
  "Shift the sequence chunk right as far as it can go"
  (interactive)
  (throw-sequence "r")
  )



(defvar ralee-block (list nil nil))

(defun define-block ()
  "define a block for edit commands to operate on.
Take row and col coords for the block from point and mark positions"
  (interactive)
  (undefine-block)
  (save-excursion
    (let (firstcol)
      (save-excursion
	(ralee-find-first-column)
	(setq firstcol (current-column))
	)

      (let (c1 c2 cols m1 m2 marks)
	(if (< (current-column) firstcol)
	    (move-to-column firstcol)
	  )
	(setq c1 (current-column))
	(setq m1 (point))
	
	(goto-char (mark)) ;; error in xemacs
	(if (< (current-column) firstcol)
	    (move-to-column firstcol)
	  )
	(setq c2 (current-column))
	(setq m2 (point))

	(setq cols (sort (list c1 c2) '<))
	(setq marks (sort (list m1 m2) '<))

	;; Final marks list should be top left position, bottom right
	;; position.  In case we've clicked top right, bottom left we
	;; need to fix the columns here.
	(goto-char (car marks))
	(move-to-column (car cols))
	(setq m1 (point))

	;; (cdr cols) doesn't work here -- wrong type I think
	(goto-char (nth 1 marks))
	(move-to-column (nth 1 cols))
	(setq m2 (point))

	(setq marks (list m1 m2))
	(setq ralee-block (sort marks '<))
	(message "block defined")
	)
      )
    (highlight-block) ;; error in xemacs
    )
  )


(defun undefine-block ()
  "undefine the block"
  (interactive)
  (setq ralee-block (list nil nil))
  (lowlight-all)
  )


(defun unalign-block-left ()
  "unalign the bases in the current block to the left"
  (interactive)
  (unalign-block ?l)
  )
(defun unalign-block-right ()
  "unalign the bases in the current block to the right"
  (interactive)
  (unalign-block ?r)
  )
(defun unalign-block-center ()
  "unalign the bases in the current block to the center"
  (interactive)
  (unalign-block ?c)
  )
(defun unalign-block-out ()
  "unalign the bases in the current block to the outside"
  (interactive)
  (unalign-block ?o)
  )


(defun unalign-block (dir)
  "unalign the bases in the current block"
  (interactive "cDirection to unalign [lrco]: ")
  (save-excursion
    (let ((start (car ralee-block))
	  (end (car (cdr ralee-block))))
      (if (and start end)
	  (progn
	    (goto-char end)
	    (let ((endcol (current-column))
		  (endrow (current-line))
		  startcol
		  string
		  endpoint)
	      (goto-char start)
	      (setq startcol (current-column))

	      (while (<= (current-line) endrow)
		(move-to-column endcol)
		(setq endpoint (point))
		(move-to-column startcol)

		(let ((gapcount 0))
		  (while (< (current-column) endcol)
		    (if (char-equal (char-after) ?.)
			(setq gapcount (1+ gapcount))
		      )
		    (forward-char 1)
		    )
		    
		  (move-to-column startcol)
		  ;; perform-replace should set limits but I can't get it to work
		  (save-restriction
		    (narrow-to-region (point) endpoint)
		    (perform-replace "." "" nil nil nil)
		    )

		  (if dir
		      (progn
			(if (or (char-equal dir ?l) (char-equal dir ?c))
			    (move-to-column (- endcol gapcount))
			  )
			(if (char-equal dir ?r)
			    (move-to-column startcol)
			  )
			(if (char-equal dir ?o)
			    (move-to-column (+ (/ (- (- endcol startcol) gapcount ) 2) startcol))
			  )

			(let ((i 0))
			  (if (char-equal dir ?c)
			      ;; center requires insert at both ends
			      (progn
				(while (< i (/ gapcount 2))
				  (insert ".")
				  (setq i (1+ i))
				  )
				(move-to-column startcol)
				(while (< i gapcount)
				  (insert ".")
				  (setq i (1+ i))
				  )
				)

			    ;; else insert all the gaps
			    (while (< i gapcount)
			      (insert ".")
			      (setq i (1+ i))
			      )
			    )
			  )
			)
		    )
		  )
		(forward-line 1)
		)
	      )
	    )
	(message "block undefined")
	)
      )
    )
  )


(defun sort-sequences (order)
  "Sort sequences in the alignment to match the list order"
  (save-excursion
    (if (= (length (ralee-get-all-seqids)) (length order))
	(let ((seqs (get-seq-hash))
	      (i 0)
	      firstcol
	      firstline)

	  (ralee-find-first-line)
	  (ralee-find-first-column)
	  (setq firstcol (current-column))
	  (goto-char (point-max))

	  (while (> (point) (point-min))
	    (if (ralee-is-alignment-line)
		(let ((beg (point)))
		  (end-of-line)
		  (kill-region beg (1+ (point)))
		  (setq firstline (current-line))
		  )
	      )
	    (forward-line -1)
	    )
      
	  (goto-line (1+ firstline))
	  (beginning-of-line)
	  
	  (while (< i (length order))
	    (let ((id (nth i order)))
	      (insert id)
	      (while (< (current-column) firstcol)
		(insert " ")
		)
	      (insert (cdr (assoc id seqs)))
	      (insert "\n")
	      )
	    (setq i (1+ i))
;	    (sit-for 0.2)
	    )
	  )
      ;; else
      (message "Number of sequence is not equal to the sorted ID list")
      )
    )
  )



(defun sort-sequences-by-id (&optional reverse)
  "Sort sequences in the alignment by id"
  (interactive)
  (save-excursion
    (let (ids)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (progn
	      (setq id (ralee-get-seq-id))
	      (push id ids)
	      )
	  )
	(forward-line 1)
	)
      (if reverse
	  (setq ids (sort ids 'string>))
	(setq ids (sort ids 'string<))
	)
      (sort-sequences ids)
      )
    )
  )


(defun sort-sequences-by-tree (&optional reverse)
  "Sort sequences in the alignment by tree"
  (interactive)
  (save-excursion
    (let (ids
	  (curbuf (current-buffer)))
      (calculate-tree)
      (set-buffer "*trees*")
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (looking-at (concat "\\(" ralee-seqid-regex "\\)" ":0.[0-9]+[,)]"))
	    (push (match-string-no-properties 1) ids)
	  )
	(forward-line 1)
	)
      (set-buffer curbuf)
      (kill-buffer "*trees*")
      (if reverse
	  ()
	(setq ids (reverse ids))
	)
      (sort-sequences ids)
      )
    )
  )


(defun sort-sequences-by-score (&optional reverse)
  "Sort sequences by scores from #=GS lines"
  (interactive)
  (save-excursion
    (let (ids
	  (curbuf (current-buffer)))
      (goto-char (point-min))
      (get-buffer-create "*scores*")
      (set-buffer curbuf)

      (while (search-forward "#=GS " nil t nil)
	(beginning-of-line)
	(let ((beg (point)))
	  (if (looking-at (concat "#=GS"
				  "\\( \\|\t\\)+"
				  "\\(" ralee-seqid-regex "\\)" 
				  "\\( \\|\t\\)+"
				  "SC"
				  "\\( \\|\t\\)+"
				  "\\([0-9\.]+\\)"))
	      (progn
		(end-of-line)
		(append-to-buffer "*scores*" beg (1+ (point)))
		)
	    )
	  )
	(end-of-line)
	)

      (set-buffer "*scores*")
      (if (> (- (point-max) (point-min)) 1)
	  (progn
	    (sort-numeric-fields 4 (point-min) (point-max))
	    (goto-char (point-min))
	    (while (< (point) (point-max))
	      (if (looking-at (concat "#=GS"
				      "\\( \\|\t\\)+"
				      "\\(" ralee-seqid-regex "\\)"))
		  (push (match-string 2) ids)
		)
	      (forward-line 1)
	      )

	    (set-buffer curbuf)
	    (kill-buffer "*scores*")

	    (if reverse
		(setq ids (reverse ids))
	      )
	    (sort-sequences ids)
	    )
	;; else
	(message "No scores information found")
	)
      )
    )
  )


(defun remove-identical-sequences ()
  "remove identical sequences from the alignment"
  (interactive)
  (save-excursion
    (let ((seqhash (get-seq-hash))
	  (i 0)
	  keepseqs)

      (while (< i (length seqhash))
	(let ((str (cdr (nth i seqhash)))
	      (j 0)
	      break)

	  (setq break (catch 'break
			(while (< j (length keepseqs))
			  (if (string-equal str (cdr (nth j keepseqs)))
			      (throw 'break t)
			    )
			  (setq j (1+ j))
			  )
			)
		)
	  (if break
	      (progn
		(ralee-find-first-line)
		(if (search-forward (car (nth i seqhash)) nil t nil)
		    (progn
		      (beginning-of-line)
		      (kill-line)(kill-line)
		      (message "%s is identical to %s.  Killed the latter." (car (nth j keepseqs)) (car (nth i seqhash)))
		      )
		  )
		)
	    ;; else
	    (push (nth i seqhash) keepseqs)
	    )
	  )
	(setq i (1+ i))
	)

      )
    )
  )


(provide 'ralee-edit)
