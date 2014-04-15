;; ralee-read

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


(defun read-stockholm (file)
  "read a stockholm format file"
  (interactive "fRead file: ")
  (set-buffer (find-file-noselect file))
  (if (ralee-blocked-alignment-p)
      (unblock-alignment)
    )
  (switch-to-buffer (current-buffer))
  )


(defun read-scores (file)
  "read a file of scores into #=GS lines"
  (interactive "fRead file: ")
  (let ((curbuf (current-buffer))
	scores)
    (set-buffer (find-file-noselect file))
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (looking-at (concat "\\([0-9.]+\\)\\( \\|\t\\)+\\(" ralee-seqid-regex "\\)"))
	  (let ((sc (match-string-no-properties 1))
		(id (match-string-no-properties 3)))
	    (setq scores (cons (cons id sc) scores))
	    )
	)
      (forward-line 1)
      )

    (switch-to-buffer curbuf)
    (ralee-find-first-line)

    (let ((i 0)
	  (maxid (ralee-maxidlength))
	  (ids (ralee-get-all-seqids t)) ;; keep whole n/s-e
	  id
	  sc)
      (while (< i (length ids))
	(setq id (nth i ids))
	(setq sc (cdr (assoc id scores)))
	(insert (concat "#=GS " id))
	(while (< (current-column) (+ maxid 5))
	  (insert " ")
	  )
	(insert (concat " SC " sc "\n"))
	(setq i (1+ i))
	)
      (insert "\n")
      )
    )
  )


  
(provide 'ralee-read)
