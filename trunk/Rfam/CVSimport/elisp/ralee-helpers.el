;;; ralee-helpers

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


(defun show-structure-ps ()
  "take the current sequence, RNAfold it, and show the structure ps file"
  (interactive)
  (write-region (ralee-ungap-string (ralee-get-seq-string)) nil "/tmp/tmp.seq")
  (call-process "RNAfold" "/tmp/tmp.seq")
  (start-process "gv" "*messages*" "gv" "rna.ps")
  )


(defun fold-all-sequences ()
  "run fold-sequence on each sequence in turn"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (progn
	    (fold-sequence)
	    (forward-line)
	    )
	)
      (forward-line)
      )
    )
  )


(defun fold-sequence ()
  "take the current sequence, RNAfold it, and mock up an SS_cons line"
  (interactive)
  (save-excursion
    (end-of-line)
    (search-backward " ")
    (let ((curbuf (current-buffer))
	  (seqid (ralee-get-seq-id))
	  (first-col (current-column)))
      (write-region (ralee-get-seq-string) nil "/tmp/tmp.seq")
      (get-buffer-create "*structures*") ; make it if it doesn't exist
      (set-buffer "*structures*")
      (goto-char (point-max))   ; write new structure at the end
      (call-process "RNAfold" "/tmp/tmp.seq" "*structures*")
      (forward-line -1)
      (beginning-of-line)
      (let ((beg (point)))
	(search-forward " ")
	(copy-region-as-kill beg (1- (point)))
	)
      (set-buffer curbuf)
      (forward-line)
      (beginning-of-line)
      (let ((string (concat "#=GR " seqid " SS  "))
	    (i 0))
	(if (> (length string) first-col)
	    (progn   ; we need to pad out all seqs to make room for the structure tag
	      (save-excursion
		(goto-char (point-min))
		(while (< (point) (point-max))
		  (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		      (progn
			(end-of-line)
			(search-backward " ")
			(let ((j 1))
			  (while (< j (- (length string) first-col))
			    (insert " ")
			    (setq j (1+ j))
			    )
			  )
			)
		    )
		  (forward-line)
		  )
		)
	      )
	  )

	(insert string)
	(while (<= i (- first-col (length string)))
          (insert " ")
          (setq i (1+ i))
          )
        )
      (yank)
      (insert "\n")
      )
    )
  )
  


(defun fetch-sequence ()
  "use pfetch to get a sequence"
  (interactive)
  (let (seqid)
    (setq seqid (ralee-get-real-seq-id))
    (if seqid
	(progn
	  (call-process "pfetch" nil seqid t "-F" seqid)
	  (pop-to-buffer seqid)
	  (setq buffer-read-only 1)
	  (goto-char (point-min))
	  )
      )
    )
  )


(provide 'ralee-helpers)
