;;; ralee-tools

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



(defun current-line ()  ; surely this should be a default method?
  "Return the vertical position of point..."
  (count-lines (point-min) (point)))


(defun ralee-is-alignment-line ()
  "Check if the current line is part of the alignment itself"
  (save-excursion
    (beginning-of-line)
    (looking-at "[A-Za-z0-9_-]+.*\ +[A-Za-z\.\-]+")
  ))

(defun ralee-is-markup-line ()
  "Check if the current line is #=GC"
  (save-excursion
    (beginning-of-line)
    (looking-at "#=GC ")
  ))


(defun ralee-get-seq-id ()
  "get the sequence identifier of the current alignment line"
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (if (ralee-is-alignment-line)
	  (progn
	    (search-forward " ")
	    (copy-region-as-kill beg (1- (point)))
	    (car kill-ring)
	    )
	(if (ralee-is-markup-line)
	    (progn
	      (looking-at "#=G[A-Z] [A-Za-z0-9_-]+ ")
	      (match-string 0)
	      )
	  (progn
	    (message "can't get seqid from current line")
	    nil
	    )
	  )
	)
      )
    )
  )

(defun ralee-get-real-seq-id ()
  "get the sequence identifier of the current alignment line"
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (if (ralee-is-alignment-line)
	  (progn
	    (if (looking-at "[A-Za-z0-9_-\.]+/[0-9]")
		(progn
		  (search-forward "/")
		  (copy-region-as-kill beg (1- (point)))
		  )
	      (progn
		(search-forward " ")
		(copy-region-as-kill beg (1- (point)))
		)
	      )
	    (car kill-ring)
	    )
	(progn
	  (message "can't get seqid from current line")
	  nil
	  )
	)
      )
    )
  )

(defun ralee-get-seq-string ()
  "get the sequence string of the current alignment line"
  (save-excursion
    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	(progn
	  (end-of-line)
	  (let ((end (point)))
	    (search-backward " ")
	    (copy-region-as-kill (1+ (point)) end)
	    (car kill-ring)
	    )
	  )
      )
    )
  )


(defun ralee-ungap-string (string)
  "take a seq string as input and return an ungapped version"
  (let (split
	base
	out)
    (setq split (split-string string ""))
    (while split
      (setq base (car split))
      (setq split (cdr split))
      (if (string-match "[A-Za-z]" base)
	  (setq out (concat out base))
	)
      )
    out
    )
  )
  

(defun ralee-maxidlength ()
  "get the maximum id length for the current alignment"
  (save-excursion
    (goto-char (point-min))
    (let ((maxidlength 0)
	  seqid
	  split)
      (while (< (point) (point-max))
	(beginning-of-line)
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (setq seqid (ralee-get-seq-id))
	      (setq split (split-string seqid ""))
	      (if (> (length split) maxidlength)
		  (setq maxidlength (length split))
		)
	      )
	  )
	)
      (forward-line)
      )
    )
  )


(provide 'ralee-tools)
