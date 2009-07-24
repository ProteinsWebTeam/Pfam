;; pfam-mode.el --- a major mode for editing [PR]fam DESC files

;; Author: SGJ
;; Inspired by, and in part derived from Harley Gorrell's dna-mode.el
;; (http://www.mahalito.net/~harley/elisp/dna-mode.el)


(defvar pfam-mode-hook nil
  "*Hook to setup `pfam-mode'.")

(defvar pfam-mode-load-hook nil
  "*Hook to run when `pfam-mode' is loaded.")

(defvar pfam-setup-on-load nil
  "*If not nil setup pfam mode on load by running `pfam-`add-hook's'.")

(defvar pfam-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\M-q"      'pfam-fill-region)
    (define-key map "\C-c\C-r"  'pfam-add-reference)
    map)
  "The local keymap for `pfam-mode'.")


(defun pfam-mode ()
  "Major mode for editing [PR]fam DESC files.
  - turn on global-font-lock for pretty colours
  - M-q       (that's Alt-q or Escape then q) justifies CC (and other) lines
  - C-c C-r   (that's Cntrl-c Cntrl-r) adds a reference"
  (interactive)

  (kill-all-local-variables)
  (setq mode-name "pfam")
  (setq major-mode 'pfam-mode)
  (use-local-map pfam-mode-map)

  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(pfam-font-lock-keywords))

  (run-hooks 'pfam-mode-hook))


;; Keywords
(defvar pfam-font-lock-keywords
  '(
    ("^\\(AC\\) +\\([-_.a-zA-Z_0-9]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-warning-face))

    "[A-Za-z][-_.a-zA-Z0-9]+:[-_.a-zA-Z0-9]+" "RF[0-9]+" "PF[0-9]+"

    ("^\\(ID\\) +\\([-_.a-zA-Z_0-9]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-function-name-face))

    ("^\\(CC\\|DE\\) +\\(.*\\)"
     (1 font-lock-keyword-face) (2 font-lock-comment-face nil t))

    ("^\\(TP\\) +\\([-_.a-zA-Z_0-9]+\\)"
     (1 font-lock-keyword-face) (2 font-lock-type-face))

    ("^\\(RN\\) +\\(.*\\)"
     (1 font-lock-keyword-face) (2 font-lock-constant-face))

    ("^\\(RT\\) +\\(.*\\)"
     (1 font-lock-keyword-face) (2 font-lock-string-face))

    ("^\\([a-zA-Z0-9][a-zA-Z0-9]\\) +"
     (1 font-lock-keyword-face))


    ;; line numbers...
    ("^[ \t]*\\([0-9]+\\)"
     (1 font-lock-string-face))

    ;; others...?
    )
  "Expressions to hilight in `pfam-mode'.")


(defun pfam-fill-region ()
  "fill the current (probably comment) paragraph without screwing
up the rest of the entry, and leaving the 'CC   ' at the beginning
of each line"
  (interactive)
  (beginning-of-line)
  (if (looking-at "\\(^[A-Z][A-Z][ \t]+\\)")
      (let ((prefix (match-string 0))
	    (init (point))
	    start)

	(move-to-column (length prefix))
	(set-fill-prefix)
	(setq fill-column 70)

	(beginning-of-line)
	(while (looking-at prefix)
	  (forward-line -1))

	(forward-line)
	(setq start (point))
	(goto-char init)

	(while (looking-at prefix)
	  (forward-line))

	(fill-region start (point))
	)
    )
  )


(defun pfam-add-reference (pmid)
  "run the add_ref.pl script to get a reference and add it to
the current desc file"
  (interactive "sPubmed ID: ")
  (goto-char (point-min))

  (let ((buf (current-buffer))
	(rn 0))
    ;; work out what the RN number should be
    (while (re-search-forward "^RN   " nil t)
      (setq rn (1+ rn))
      )

    (get-buffer-create "*references*") ; make it if it doesn't exist
    (set-buffer "*references*")
    (kill-region (point-min) (point-max))
    (call-process "add_ref.pl" nil t nil "-rn" (number-to-string rn) "-n" pmid)
    (kill-region (point-min) (point-max))
    (set-buffer buf)
    )

  ;; find the correct position
  (goto-char (point-max))
  (beginning-of-line)
  (forward-line -1)

  (while (or (looking-at "^CC   ") (looking-at "^\\*\\*   "))
    (forward-line -1)
    )
  
  (forward-line)
  (insert (car kill-ring))
  )


;; done loading
(run-hooks 'pfam-mode-load-hook)
(provide 'pfam-mode)

;;; pfam-mode.el ends here
