;;; ralee-helpers

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
