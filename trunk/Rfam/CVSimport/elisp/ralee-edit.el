;;; ralee-edit

(defun ralee-insert-gap-column ()
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


(defun ralee-insert-gap ()
  "Insert a gap residues"
  (interactive)
  (insert ralee-gap-symbol)
  )


(defun ralee-delete-gap ()
  "delete a gap residue"
  (interactive)
  (if (or (char-equal (char-after) ?\.)
	  (char-equal (char-after) ?\-))
      (delete-char 1)
    )
  )


(defun ralee-delete-gap-column ()
  "Delete a column but only if it is all gaps"
  (interactive)
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
		  (goto-char (point-max))))))  ; break the loop
	(forward-line))

      (if notagap    ; go round again, deleting the base this time
	  (message "Column %s contains non-gap characters -- cannot delete" column)
	(progn
	  (goto-char (point-min))
	  (while (< (point) (point-max))
	    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		(progn
		  (move-to-column column)
		  (delete-char 1)))
	    (forward-line)
	    )
	  )
	)
      )
    )
  )

(defun ralee-unblock-alignment ()
  "unblock a blocked alignment"
  (interactive)
  (goto-char (point-min))
  (let ((seqs ())
	seqid
	(ids ())
	seqstr)

    (while (< (point) (point-max))
      (beginning-of-line)
      (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	  (progn
	    (setq seqid (ralee-get-seq-id))
	    (setq seqstr (ralee-get-seq-string))
	    (if (assoc seqid seqs)
		(setq seqstr (concat (cdr (assoc seqid seqs)) seqstr))
	      (push seqid ids)
	      )
	    (setq seqs (cons (cons seqid seqstr) seqs))
	    )
	)
      (forward-line)
      )

    (setq ids (reverse ids))

    (insert "# STOCKHOLM 1.0\n\n")
    (while ids
      (setq seqid (car ids))
      (setq ids (cdr ids))

      (insert seqid)
      (insert "\t")
      (insert (cdr (assoc seqid seqs)))
      (insert "\n")
      )
    (insert "\\\\\n")
    )
  )



(provide 'ralee-edit)
