;;; ralee-edit

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



(defun insert-gap-column ()
  "Insert a column of gap residues"
  (interactive)
  (save-excursion
    (let (column)
      (setq column (current-column))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column column)
	      (insert ralee-gap-symbol)))
	(forward-line)
	)
      )
    )
  )


(defun insert-gap ()
  "Insert a gap residues"
  (interactive)
  (insert ralee-gap-symbol)
  )


(defun delete-gap ()
  "delete a gap residue"
  (interactive)
  (if (or (char-equal (char-after) ?\.)
	  (char-equal (char-after) ?\-))
      (delete-char 1)
    (message "can't delete a non-gap character")
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
	      (setq char (char-after))
	      (if (char-equal char (string-to-char ralee-gap-symbol))
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
	(split ())
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
		(setq split (split-string seqid ""))
		(if (> (length split) maxidlength)
		    (setq maxidlength (length split))
		  )
		)
	      )
	    (setq seqs (cons (cons seqid seqstr) seqs))
	    )
	)
      (forward-line)
      )

    ; backup the current buffer and overwrite with unblocked alignment
    (setq name (buffer-name))
    (write-file (concat (buffer-name) ".bak"))
    (get-buffer-create name)
    (switch-to-buffer name)
    (ralee-mode) ; new buffer doesn't inherit mode!

    (setq ids (reverse ids))

    (insert "# STOCKHOLM 1.0\n\n")
    (while ids
      (setq seqid (car ids))
      (setq ids (cdr ids))

      (setq split (split-string seqid ""))
      (insert seqid)

      (let ((i 0))
	(while (<= i (- (1+ maxidlength) (length split)))
	  (insert " ")
	  (setq i (1+ i))
	  )
	)
      (insert (cdr (assoc seqid seqs)))
      (insert "\n")
      )
    (insert "\/\/\n")
    )
  )

; trim-left needs to change the start-ends if they exist
(defun trim-left ()
  "trim the alignment left of cursor position"
  (interactive)
  (save-excursion
    (let ((column (current-column))
	  end)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column column)
	      (setq end (point))
	      (search-backward " ")
	      (kill-region (1+ (point)) end)
	      )
	  )
	(forward-line)
	)
      )
    )
  )


; trim-right needs to hack the start-ends if they exist
(defun trim-right ()
  "trim the alignment left of cursor position"
  (interactive)
  (save-excursion
    (let ((column (current-column))
	  start)
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column column)
	      (setq start (point))
	      (end-of-line)
	      (kill-region start (point))
	      )
	  )
	(forward-line)
	)
      )
    )
  )



(provide 'ralee-edit)
