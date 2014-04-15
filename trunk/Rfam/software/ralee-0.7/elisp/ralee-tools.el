;;; ralee-tools

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


(defvar ralee-seqid-regex "[A-Za-z0-9()\?/_=~,.:;\|-]+")
(defvar ralee-nse-regex (concat ralee-seqid-regex "/[0-9]+-[0-9]+"))
(defvar ralee-seq-regex "[A-Za-z~._,:;-]+")
(defvar ralee-gap-regex "[._,~:;-]")
(defvar ralee-str-regex "[][<>{}()._,~:;-]+")

(defvar ralee-nucleotide-regex (list
				( cons "A" "A" )
				( cons "C" "C" )
				( cons "G" "G" )
				( cons "T" "[TU]" )
				( cons "U" "[TU]" )
				( cons "M" "[AC]" )
				( cons "R" "[AG]" )
				( cons "W" "[ATU]" )
				( cons "S" "[CG]" )
				( cons "Y" "[CTU]" )
				( cons "V" "[ACG]" )
				( cons "H" "[ACTU]" )
				( cons "D" "[AGTU]" )
				( cons "B" "[CGTU]" )
				( cons "N" "[AGCTU]" )
				( cons "X" "[AGCTU]" )))


;; (count-lines (point-min) (point)))
;; is not good enough for this method
;; following stolen from GNU emacs reference manual
(defun current-line ()
  "Return the vertical position of point..."
  (+ (count-lines (point-min) (point))
     (if (= (current-column) 0) 1 0)
     -1))


(defun ralee-number-seqs ()
  "Number of sequences in the alignment"
  (save-excursion
    (let ((i 0))
      (ralee-find-first-line)
      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (setq i (1+ i))
	  )
	(forward-line)
	)
      i
      )
    )
  )


(defun ralee-is-pair (openbase closebase)
  "return t if open and close bases can pair"
  (if (or
       (and (char-equal openbase ?C) (char-equal closebase ?G))
       (and (char-equal openbase ?G) (or (char-equal closebase ?C)
					 (char-equal closebase ?U)
					 (char-equal closebase ?T)))
       (and (char-equal openbase ?A) (or (char-equal closebase ?U)
					 (char-equal closebase ?T)))
       (and (or (char-equal openbase ?U)
		(char-equal openbase ?T)) (or (char-equal closebase ?A)
					      (char-equal closebase ?G))))
      t
    )
  )



(defun ralee-is-alignment-line ()
  "Check if the current line is part of the alignment itself"
  (save-excursion
    (beginning-of-line)
    (looking-at (concat ralee-seqid-regex "\\( \\|\t\\)+" ralee-seq-regex "$"))
  ))

(defun ralee-is-markup-line ()
  "Check if the current line is #=G[CR]"
  (save-excursion
    (beginning-of-line)
    (looking-at "#=G[CR] ")
  ))


(defun ralee-is-alignment-column ()
  "Check if the current column is inside the alignment"
  (save-excursion
    (let ((column (current-column)))
      (ralee-find-first-line)
      (move-to-column column)
      (if (or (looking-at (concat ralee-seq-regex "$"))
	      (looking-at "$"))
	  t
	)
      )
    )
  )


(defun ralee-is-compensatory (pair)
  "do the columns in <pair> contain compensatory mutations?"
  (save-excursion
    (let ((is-comp nil)
	  openbase
	  closebase
	  open
	  close)
      (goto-char (point-min))
      (ralee-find-first-line)
      (while (and (< (point) (point-max)) (equal is-comp nil))
	(if (ralee-is-alignment-line)
	    (progn
	      (move-to-column (car pair))
	      (setq open (char-after))
	      (move-to-column (cdr pair))
	      (setq close (char-after))
	      (if (ralee-is-pair open close)
		  (if (equal openbase nil)
		      (progn
			(setq openbase open)
			(setq closebase close)
			)
		    (if (or (not (equal openbase open))
			    (not (equal closebase close)))
			(setq is-comp t)
		      )
		    )
		)
	      )
	  )
	(forward-line)
	)
      is-comp
      )
    )
  )



(defun ralee-find-first-line ()
  "move to the first alignment or markup line"
  (goto-char (point-min))
  (catch 'exit
    (while (and (equal (ralee-is-alignment-line) nil)
		(equal (ralee-is-markup-line) nil)
		)
      (if (or (looking-at "//") (>= (point) (point-max)))
	  (throw 'exit nil)
	)
      (forward-line)
      )
    (current-line)
    )
  )


(defun ralee-find-first-column ()
  "move to the first column in the alignment"
  (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
      (progn
	(end-of-line)
	(re-search-backward " [^ ]")
	(forward-char)
	(current-column)
	)
    )
  )


(defun ralee-get-seq-id ()
  "get the sequence identifier of the current alignment line"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (let ((beg (point)))
      (if (ralee-is-alignment-line)
	  (progn
	    (looking-at ralee-seqid-regex)
	    (match-string-no-properties 0)
	    )
	(if (ralee-is-markup-line)
	    (if (looking-at (concat "#=GR " ralee-seqid-regex ralee-seqid-regex))
		(match-string-no-properties 0)
	      (if (looking-at (concat "#=GC " ralee-seqid-regex))
		  (match-string-no-properties 0)
		)
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


(defun ralee-get-name-start-end ()
  "get the name/st-en from the current alignment line"
  (save-excursion
    (let (name
	  (tag nil)
	  (st nil)
	  (en nil))

      (beginning-of-line)
      (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	  (progn 
	    (if (looking-at (concat "#=GR " ralee-seqid-regex " \\(" ralee-seqid-regex "\\)"))
		(setq tag (match-string-no-properties 1))
	      )

	    (if (looking-at (concat "\\(\\(?:#=G[RC] \\)?" ralee-seqid-regex "\\)/\\([0-9]+\\)-\\([0-9]+\\) "))
		(progn
		  (setq name (match-string-no-properties 1))
		  (setq st (string-to-number (match-string-no-properties 2)))
		  (setq en (string-to-number (match-string-no-properties 3)))
		  )
	      (if (looking-at (concat "\\(?:#=G[RC] \\)?" ralee-seqid-regex))
		  (progn
		    (setq name (match-string-no-properties 0))
		    ;; can use the below to make up st/en
		    ;; but we don't want to at the moment
		    ;;(setq st 1)
		    ;;(setq en (length 
		    ;;	    (split-string 
		    ;; 	     (ralee-ungap-string(ralee-get-seq-string)) "")))
		    )
		)
	      )
	    )
	)
      (if name
	  (list name st en tag)
	)
      )
    )
  )


(defun ralee-get-all-seqids (&optional keep-nse)
  "get a list of all sequences identifiers"
  (interactive)
  (save-excursion
    (let (seqids
	  (i 0))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (let (id
		  nse)
	      (if keep-nse
		  (setq id (ralee-get-seq-id))
		(setq nse (ralee-get-name-start-end))
		(setq id (nth 0 nse))
		)
	      (push id seqids)
	      )
	  )
	(forward-line 1)
	)
      (setq seqids (nreverse seqids))
      )
    )
  )



(defun ralee-get-seq-string (&optional startcolumn endcolumn)
  "get the sequence string of the current alignment line"
  (save-excursion
    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	(progn
	  (if endcolumn
	      (move-to-column endcolumn)
	    (end-of-line)
	    )
	  (let ((end (point)))
	    (if startcolumn
		(move-to-column startcolumn)
	      (ralee-find-first-column)
	      )
	    (copy-region-as-kill (point) end)
	    (car kill-ring)
	    )
	  )
      nil
      )
    )
  )


(defun ralee-ungap-string (string)
  "take a seq string as input and return an ungapped version"
  (let (split
	base
	(out ""))
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
	      (setq splitlen (length seqid))
	      
	      (if (> splitlen maxidlength)
		  (setq maxidlength splitlen)
		)
	      )
	  )
	(forward-line)
	)
      maxidlength
      )
    )
  )


(defun ralee-count-column-symbols ()
  "return the symbol that is most abundant in a column, and its count."
  (interactive)
  (save-excursion
    (let (column 
	  counts
	  )
      (setq column (current-column))
      (ralee-find-first-line)
      (move-to-column column)

      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (progn
	      (move-to-column column)
	      (let ((base (upcase (char-after))))
		(if (and (< base 91) (> base 64)) ;; char numbers for [A-Z]
		    (if (cdr (assq base counts))
			(setcdr (assq base counts) (1+ (cdr (assq base counts))))
		      (setq counts (cons (cons base 1) counts))
		      )
		  )
		)
	      )
	  )
	(forward-line)
	)
      counts
      )
    )
  )


(defun ralee-count-bases-in-column ()
  "return the counts of bases in the current column"
  (save-excursion
    (let (column
	  (a-count 0)
	  (c-count 0)
	  (g-count 0)
	  (u-count 0)
	  (line-count 0)
	  )

      (setq column (current-column))
      (goto-char (point-min))

      (while (< (point) (point-max))
	(if (ralee-is-alignment-line)
	    (progn
	      (move-to-column column)
	      (setq line-count (1+ line-count))
	      (let ((base (char-after))
		    (case-fold-search t))
		(if (char-equal base ?A) (setq a-count (1+ a-count)))
		(if (char-equal base ?C) (setq c-count (1+ c-count)))
		(if (char-equal base ?G) (setq g-count (1+ g-count)))
		(if (or (char-equal base ?U)
			(char-equal base ?T)) (setq u-count (1+ u-count)))
		)
	      )
	  )
	(forward-line)
	)
      (list line-count a-count c-count g-count u-count)
      )
    )
  )


(defvar ralee-last-regex-search "")

(defun ralee-motif-search (regex)
  "interactive search for a motif"
  (interactive "sMotif: ")
  
  (let (split
	join
	)
    (if (< (length regex) 1)
	(setq regex ralee-last-regex-search)
      )

    (setq ralee-last-regex-search regex)
    (setq split (split-string regex ""))

    (while split
      (let ((base (car split)))
	(setq base (cdr (assoc base ralee-nucleotide-regex)))
	(setq join (concat join ralee-gap-regex "*" base))
	(setq split (cdr split))
	)
      )
    
    (re-search-forward join)
    )
  )


(defun ralee-blocked-alignment-p ()
  "check if current alignment is blocked"
  (save-excursion
    (ralee-find-first-line)
    (let ((id (ralee-get-seq-id)))
      (forward-line)
      (search-forward id nil t)
      )
    )
  )


(defun ralee-delete-whitespace-eol ()
  "delete white-space at the end of lines"
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (end-of-line)
      (while (char-equal (char-before) ?\ )
	(delete-backward-char 1)
	)
      (forward-line)
      )
    )
  )



(defun ralee-find-gr-lines (&optional seqid)
  "Find #=GR lines that are associated with the named sequence"
  (interactive)
  (save-excursion
    (let ((list ()))
      (if (not seqid)
	  (setq seqid (ralee-get-seq-id))
	)
      (goto-char (point-min))
      (while (search-forward (concat "#=GR " seqid) nil t nil)
	(setq list (cons (1+ (current-line)) list)) 
	;; what's with the 1+ above?
	)
      (reverse list)
      )
    )
  )


(defun ralee-convert-to-fasta ()
  "convert contents of current buffer to fasta format"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (beginning-of-line)
      (if (ralee-is-alignment-line)
	  (progn
	    (insert ">")
	    (search-forward " ")
	    (insert "\n")
	    (end-of-line)
	    (let ((eol (point)))
	      (beginning-of-line)
	      (save-restriction
		(narrow-to-region (point) eol)
		(beginning-of-line) (perform-replace " " "" nil nil nil)
		(beginning-of-line) (perform-replace "-" "" nil nil nil)
		(beginning-of-line) (perform-replace "." "" nil nil nil)
		)
	      )
	    (forward-line 1)
	    )
	(let ((beg (point)))
	  (forward-line 1)
	  (kill-region beg (point))
	  )
	)
      )
    (goto-char (point-min)) (perform-replace " " "" nil nil nil)
    )
  )


(defun get-column-number (&optional position)
  "Get the column number of the supplied position
or point if no position supplied"
  (save-excursion
    (if position
	(goto-char position)
      )
    (current-column)
    )
  )


(defun get-seq-hash ()
  "Get a hash with seqid keys and sequence values.
Works also with blocked alignments."
  (interactive)
  (save-excursion
    (let (seqs
	  seqid
	  seqstr)
      (ralee-find-first-line)
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (setq seqid (ralee-get-seq-id))
	      (setq seqstr (ralee-get-seq-string))
	      (if (assoc seqid seqs)
		  (setq seqstr (concat (cdr (assoc seqid seqs)) seqstr))
		)
	      (setq seqs (cons (cons seqid seqstr) seqs))
	      )
	  )
	(forward-line)
	)
      seqs
      )
    )
  )


(defun validate-stockholm-alignment ()
  "Check that the alignment is valid stockholm"
  (interactive)
  (save-excursion
    (if (validate-stockholm-header)
	(message "Invalid alignment: invalid stockholm header")
      )
    (if (validate-final-eol)
	(message "Invalid alignment: missing final end-of-line")
      (if (validate-stockholm-end-of-aln)
	  (message "Invalid alignment: missing // at end of alignment")
	)
      )
    (let ((line (check-alignment-lines)))
      (if line
	  (message (concat "Invalid alignment: line starting [" line "]"))
	)
      )
    )
  )


(defun validate-stockholm-header ()
  "Check for stockholm header"
  (goto-char (point-min))
  (if (looking-at "# STOCKHOLM 1.0$")
      nil
    t
    )
  )

(defun validate-final-eol ()
  "Check for final end of line"
  (goto-char (point-max))
  (let ((line (current-line)))
    (forward-char -1)
    (if (= line (1+ (current-line)))
	nil
      t
      )
    )
  )


(defun validate-stockholm-end-of-aln ()
  "Check for stockholm double slash"
  (goto-char (point-max))
  (forward-line -1)
  (if (looking-at "//$" )
      nil
    t
    )
  )

(defun validate-seqids ()
  "Check that the sequence ids are good"
  )


(defun validate-alignment-lines ()
  "Check that sequence lines are good"
  (goto-char (point-min))
  (while (< (point) (point-max))
    (if (not (or (ralee-is-alignment-line)
		 (ralee-is-markup-line)))
	(progn
	  (let ((begin (point)))
	    (end-of-line)
	    (copy-region-as-kill begin (point))
	    )
	  )
      nil
      ;; return nil is all is OK, or the line that fails
      )
    )
  )


(provide 'ralee-tools)

