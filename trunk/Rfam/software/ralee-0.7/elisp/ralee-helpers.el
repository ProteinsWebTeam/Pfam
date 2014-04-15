;;; ralee-helpers

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


;; External program calls are defined here.  You should be able to 
;; roll and call your own wrapper to other external programs, making
;; sure to use the same input and output formats as described below.

(defvar ralee-sequence-folder "RNAfold"
  "Program to fold sequences.
Input raw sequence. Output dot-bracket notation.")

(defvar ralee-alignment-folder "RNAalifold"
  "Program to fold alignment.
Input Clustal format alignment. Output includes a dot-bracket notation line.")

(defvar ralee-sequence-fetcher "ralee-efetch.pl"
  "Program to fetch sequences.
Input is sequence id. Output displayed as is.")

(defvar ralee-sequence-fetcher-opts "-f embl"
  "options for sequence fetcher")

(defvar ralee-description-fetcher "ralee-efetch.pl"
  "Program to fetch descriptions.
Input is sequence id. Output is a single line description.")

(defvar ralee-description-fetcher-opts "-f desc"
  "options for description fetcher")

(defvar ralee-aligner "clustalw"
  "Program to align sequences.
Input is fasta sequence. Output is Clustal format alignment.")

(defvar ralee-aligner-opts "-infile=/tmp/raleeseqs.fa -outfile=/tmp/raleeseqs.aln"
  "options for aligner")

(defvar ralee-postscript-viewer "ghostview"
  "Program to view the postscript of structure.")

(defvar ralee-tree-viewer "njplot"
  "Program to view trees.
Input is New Hampshire format tree.")

(defvar ralee-tree-builder "quicktree"
  "Program to build trees.
Input is Stockholm format alignment. Output is New Hampshire format tree.")



(defun show-structure-ps ()
  "take the current sequence, RNAfold it, and show the structure ps file"
  (interactive)
  (if (ralee-is-alignment-line)
      (let ((seqstr (ralee-get-seq-string)))
	(ralee-call-sequence-folder seqstr)
	(if (executable-find ralee-postscript-viewer)
	    (start-process ralee-postscript-viewer "*messages*" ralee-postscript-viewer "rna.ps")
	  (message (concat "Can't find executable [" ralee-postscript-viewer "]"))
	  )
	)
    )
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



(defun ralee-call-sequence-folder (seqstr)
  "take a sequence string, output a bracket string
dealing with gap delete and then insert"
  (let ((curbuf (current-buffer))
	structure
	split
	(gaps ())
	ungap)

    ;; remove and store the positions of gaps
    (let ((start -1))
      (while start
	(setq start (string-match ralee-gap-regex seqstr (1+ start)))
	;; If list gaps is first in the cons here, then
	;; you don't get a simple list.  Best to do it
	;; like this and then reverse the list later
	(if start
	    (setq gaps (cons start gaps))
	  )
	)
      )

    (setq gaps (reverse gaps))
    (setq ungap (ralee-ungap-string seqstr))

    ;; do the folding magic
    (get-buffer-create "*structures*") ;; make it if it doesn't exist
    (set-buffer "*structures*")
    (kill-region (point-min) (point-max))
    (insert ungap)
    (beginning-of-line)
    (setq beg (point))
    (end-of-line)
    (insert "\n")

    (if (executable-find ralee-sequence-folder)
	(progn
	  (call-process-region beg (1- (point)) ralee-sequence-folder nil t)

	  (forward-line -1)
	  (beginning-of-line)

	  ;; put the gaps back
	  (while gaps
	    (setq gap (car gaps))
	    (setq gaps (cdr gaps))
	    (move-to-column gap)
	    (insert ".")
	    )

	  (beginning-of-line)
	  (if (looking-at "\\([.()]+\\)")
	      (setq structure (match-string 0))
	    )

	  (set-buffer curbuf)
	  (kill-buffer "*structures*")

	  ;;    (print structure)
	  structure
	  )
      
      (message (concat "Can't find executable [" ralee-sequence-folder "]"))
      )
    )
  )


(defun fold-sequence (&optional startcolumn endcolumn)
  "take the current sequence, RNAfold it, and mock up an SS_cons line"
  (interactive)
  (if (executable-find ralee-sequence-folder)
      (save-excursion
	(if (ralee-is-alignment-line)
	    (progn
	      (ralee-find-first-column)
	      (let ((seqid (ralee-get-seq-id))
		    (first-col (current-column))
		    (beg (point))
		    seqstr
		    structure
		    last-col
		    )
	    
		(end-of-line)
		;; if startcolumn, endcolumn are not defined then
		;; we get the whole line
		(setq seqstr (ralee-get-seq-string startcolumn endcolumn))
		(setq structure (ralee-call-sequence-folder seqstr))
		
		(insert "\n")
		;; pad the ends with gaps if we're folding a subsequence
		(if startcolumn
		    (let ((i first-col))
		      (while (< i startcolumn)
			(insert ".")
			(setq i (1+ i))
			)
		      )
		  )
		(insert structure)
		(if endcolumn
		    (progn
		      (forward-line -1)
		      (end-of-line)
		      (setq last-col (current-column))
		      (forward-line 1)
		      (end-of-line)
		      
		      (while (< (current-column) (- last-col first-col))
			(insert ".")
			)
		      )
		  )
		
		;; insert the #=GR tag
		(beginning-of-line)
		(let ((string (concat "#=GR " seqid " SS  "))
		      (i 0))
		  (if (> (length string) first-col)
		      
		      (save-excursion   ;; we need to pad out all seqs to make 
			;; room for the structure tag
			(goto-char (point-min))
			(while (< (point) (point-max))
			  (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
			      (progn
				(end-of-line)
				(search-backward " ")
				(let ((j 0))
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
		  
		  (insert string)
		  (while (< i (- first-col (length string)))
		    (insert " ")
		    (setq i (1+ i))
		    )
		  )
		)
	      )
	  ;; else
	  (message "Current line doesn't contain a sequence to fold")
	  )
	)
    ;; else
    (message (concat "Can't find executable [" ralee-sequence-folder "]"))
    )
  )
  


(defun fold-alignment ()
  "use RNAalifold to produce a consensus structure prediction"
  (interactive)
  (if (executable-find ralee-alignment-folder)
      (save-excursion
	(let (beg
	      (curbuf (current-buffer))
	      structure
	      firstcol
	      )

	  ;; set up the structure buffer
	  (get-buffer-create "*structures*") ;; make it if it doesn't exist
	  (set-buffer "*structures*")
	  (kill-region (point-min) (point-max))
	  
	  ;; back to alignment buffer
	  (set-buffer curbuf)
	  (ralee-find-first-line)
	  (setq beg (point))
	  (search-forward "//")
	  (beginning-of-line)
	  (copy-region-as-kill beg (point))
	  
	  ;; do the folding magic
	  (set-buffer "*structures*")
	  (insert "CLUSTAL W(1.5) multiple sequence alignment\n\n")
	  (yank)
	  (goto-char (point-min))
	  (while (search-forward "\n#" nil t nil)
	    (beginning-of-line)
	    (setq beg (point))
	    (end-of-line)
	    (kill-region beg (1+ (point)))
	    (forward-line -1) ;; need to catch consecutive # lines
	    )
	  
	  (goto-char (point-max))
	  (call-process-region (point-min) (point-max) ralee-alignment-folder nil t)
	  
	  (forward-line -1)
	  (beginning-of-line)
	  (if (looking-at "\\([.()]+\\)")
	      (setq structure (match-string 0))
	    )
      
	  ;; back to alignment buffer
	  (set-buffer curbuf)
	  (goto-char (point-min))
	  (ralee-find-first-line)
	  (ralee-find-first-column)
	  (setq firstcol (current-column))
	  (search-forward "\n//")
	  (beginning-of-line)
	  (insert "#=GC SS_cons ")
	  (while (< (current-column) firstcol)
	    (insert " ")
	    )
	  (insert structure)
	  (insert "\n")

	  ;; perform-replace should set limits but I can't get it to work
	  (save-restriction
	    (narrow-to-region (point) (point-max))
	    (perform-replace "SS_cons " "SS_cons2" nil nil nil)
	    )
      
	  (kill-buffer "*structures*")
	  )
	)
    ;; else
    (message (concat "Can't find executable [" ralee-alignment-folder "]"))
    )
  )




(defun fold-sequence-block ()
  "fold the subsequences in the current block"
  (interactive)
  (save-excursion
    (let ((start (car ralee-block))
          (end (car (cdr ralee-block))))
      (if (and start end)
          (progn
            (goto-char end)
            (let ((endcol (current-column))
                  (endrow (current-line))
                  startcol)
	      (goto-char start)
              (setq startcol (current-column))

              (while (<= (current-line) endrow)
		(fold-sequence startcol endcol)
		(setq endrow (1+ endrow))
		(forward-line 2)
		)
	      )
	    )
	)
      )
    )
  )


(defun calculate-tree ()
  "calculate tree in New Hampshire format"
  (interactive)
  (save-excursion
    (if (bufferp "*trees*")
	(kill-buffer "*trees*")
      )
    (write-region (point-min) (point-max) "/tmp/trees.ralee")
    (call-process "quicktree" nil "*trees*" nil "/tmp/trees.ralee")
    (delete-file "/tmp/trees.ralee")
    )
  )

(defun draw-tree ()
  "Draw a tree of the alignment using njplot"
  (interactive)
  (save-excursion
    (calculate-tree)
    (set-buffer "*trees*")
    (write-region (point-min) (point-max) "/tmp/ralee.nh")
;    (start-process "njplot" "*Messages*" "njplot" "/tmp/ralee.nh")
    (if (executable-find ralee-tree-viewer)
	(shell-command (concat ralee-tree-viewer "/tmp/ralee.nh"))
      (message (concat "Can't find executable [" ralee-tree-viewer "]"))
      )
;    (delete-file "/tmp/ralee.nh")
    )
  )


(defun calculate-distance-matrix ()
  "calculate distance matrix in Phylip format"
  (interactive)
  (save-excursion
    (write-region (point-min) (point-max) "/tmp/dists.ralee")
    (call-process "quicktree" nil "*distances*" nil "-out" "m" "/tmp/trees.ralee")
    )
  )


(defun fetch-sequence ()
  "use pfetch to get a sequence"
  (interactive)
  (let (seqid)
    (setq seqid (ralee-get-name-start-end))
    (if seqid
	(progn
	  (call-process ralee-sequence-fetcher nil (nth 0 seqid) t 
;;			ralee-sequence-fetcher-opts 
			;; don't know why the above doesn't work
			"-f" "embl"
			(nth 0 seqid))
	  (pop-to-buffer (nth 0 seqid))
	  (setq buffer-read-only 1)
	  (protect-mode nil)
	  (goto-char (point-min))
	  )
      )
    )
  )


(defun fetch-sequence-descriptions ()
  "use pfetch to get a sequence"
  (interactive)
  (save-excursion
    (let ((seqids (ralee-get-all-seqids))
	  string)
      (setq string (mapconcat 'identity seqids " "))

      (call-process ralee-description-fetcher nil "*descriptions*" t 
;;		    ralee-description-fetcher-opts
		    ;; don't know why the above doesn't work
		    "-f" "desc"
		    string)
      (pop-to-buffer "*descriptions*")
      (setq buffer-read-only 1)
      (protect-mode nil)
      (setq truncate-lines 1)
      (goto-char (point-min))
      )
    )
  )


(defun show-sequence-description ()
  "use pfetch to get a sequence description"
  (interactive)
  (let (seqid)
    (setq seqid (ralee-get-name-start-end))
    (if seqid
	(if (executable-find ralee-description-fetcher)
	    (print (shell-command-to-string 
		    (concat ralee-description-fetcher " "
			    ralee-description-fetcher-opts " "
			    (nth 0 seqid))))
	  (message (concat "Can't find executable [" ralee-description-fetcher "]"))
	  )
      )
    )
  )


(defun realign-block ()
  "Realign the selected sequence block"
  (interactive)
  (save-excursion
    (let ((curbuf (current-buffer))
	  (start (car ralee-block))
	  (end (car (cdr ralee-block)))
	  seqs)
      (if (and start end)
	  (let ((startcol (get-column-number start))
		(endcol (get-column-number end))
		oldblocklength
		newblocklength
		tmpseqids
		)
	    (setq oldblocklength (- endcol startcol))
	    (copy-block-to-buffer)
	    (set-buffer "*block*")

	    ;; convert all sequence names to numbers, as clustalw
	    ;; truncates any that are too long
	    (goto-char (point-min))
	    (let ((seqnum 1))
	      (while (< (point) (point-max))
		(if (or (ralee-is-alignment-line)
			(ralee-is-markup-line))
		    (let ((seqid (ralee-get-seq-id)))
		      (setq tmpseqids (cons (cons seqid (number-to-string seqnum)) tmpseqids))
		      (ralee-find-first-column)
		      (let ((killto (1- (point))))
			(beginning-of-line)
			(kill-region (point) killto)
			(insert (number-to-string seqnum))
			(setq seqnum (1+ seqnum))
			)
		      )
		  )
		(forward-line 1)
		)
	      )

	    (ralee-convert-to-fasta)
	    (write-file "/tmp/ralee.fa")

	    (get-buffer-create "*block*") ;; make it again
	    (set-buffer "*block*")
	    (kill-region (point-min) (point-max))

	    (call-process "clustalw" nil "*alignmenterror*" t 
			  "-infile=/tmp/ralee.fa"
			  "-outfile=/tmp/ralee.aln")
	    (insert-file "/tmp/ralee.aln")

	    (setq seqs (get-seq-hash))
	    (let ((seqeg (cdr (car seqs))))
		(setq newblocklength (length seqeg))
	      )

	    (set-buffer curbuf)
	    (ralee-find-first-line)

	    (kill-buffer "*block*")
	    (kill-buffer "ralee.fa")
	    (delete-file "/tmp/ralee.fa")
	    (delete-file "/tmp/ralee.dnd")
	    (delete-file "/tmp/ralee.aln")

	    (while (< (point) (point-max))
	      (if (or (ralee-is-alignment-line)
		      (ralee-is-markup-line))
		  (let ((seqid (ralee-get-seq-id))
			tmpseqid)
		    ;; retrieve the temporary numbered id
		    (setq tmpseqid (cdr (assoc seqid tmpseqids)))
		    (if (assoc tmpseqid seqs)
			(progn
			  (move-to-column startcol)
			  (insert (cdr (assoc tmpseqid seqs)))
			  ;; insert spaces if we need to pad
			  (let ((i 0))
			    (while (< (+ newblocklength i) oldblocklength)
			      (insert "-")
			      (setq i (1+ i))
			      )
			    )
			  )
		      ;; else
		      ;; insert spaces to pad
		      (move-to-column endcol)
		      (let ((i 0))
			(while (< (+ oldblocklength i) newblocklength)
			  (insert "-")
			  (setq i (1+ i))
			  )
			)
		      )
		    )
		)
	      (forward-line 1)
	      )
	    )
	;; else
	(message "No block defined")
	)
      )
    )
  )


(defun copy-block-to-buffer ()
  "Copy the defined block to another buffer"
  (save-excursion
    (let ((curbuf (current-buffer))
	  (start (car ralee-block))
	  (end (car (cdr ralee-block))))

      (if (and start end)
	  (progn
	    ;; set up the structure buffer
	    (get-buffer-create "*block*") ;; make it if it doesn't exist
	    (set-buffer "*block*")
	    (kill-region (point-min) (point-max))
	    (set-buffer curbuf)

	    ;; replace this with (get-column-number)
	    (goto-char end)
	    (let ((endcol (current-column))
		  (endrow (current-line))
		  startcol
		  idlength
		  )
	      (ralee-find-first-column)
	      (setq idlength (current-column))
	      (goto-char start)
	      (setq startcol (current-column))

	      (while (<= (current-line) endrow)
		(move-to-column startcol)
		(let ((beg (point))
		      (seqid (ralee-get-seq-id))
		      )
		  (move-to-column endcol)
		  (kill-region beg (point))
		  (set-buffer "*block*")
		  (insert seqid)
		  (while (< (current-column) idlength)
		    (insert " ")
		    )
		  )
		(yank)
		(insert "\n")
		(set-buffer curbuf)
		(forward-line 1)
		)
	      )
	    )
	  )
      )
    )
  )



(provide 'ralee-helpers)
