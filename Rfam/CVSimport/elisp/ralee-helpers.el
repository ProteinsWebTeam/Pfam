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
  "take the current sequence and RNAfold it"
  (interactive)
  (write-region (ralee-ungap-string (ralee-get-seq-string)) nil "/tmp/tmp.seq")
  (call-process "RNAfold" "/tmp/tmp.seq")
  (start-process "gv" "*messages*" "gv" "rna.ps")
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
