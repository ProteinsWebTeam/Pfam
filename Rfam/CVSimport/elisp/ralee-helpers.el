;;; ralee-helpers

(defun show-structure-ps ()
  "take the current sequence and RNAfold it"
  (interactive)
  (write-region (ralee-ungap-string (ralee-get-seq-string)) nil "/tmp/tmp.seq")
  (call-process "RNAfold" "/tmp/tmp.seq")
  (start-process "gv" "*messages*" "gv" "rna.ps")
  )


(provide 'ralee-helpers)

