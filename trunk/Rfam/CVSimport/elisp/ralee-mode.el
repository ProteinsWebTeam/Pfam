;;; ralee-mode.el --- ralee mode

(require 'ralee-faces)
(require 'ralee-helpers)
(require 'ralee-movement)
(require 'ralee-paint)
(require 'ralee-structure)
(require 'ralee-edit)


(defcustom ralee-mode-hook nil
  "Normal hook run when entering ralee mode"
  :type 'hook
  :options '(turn-off-auto-fill
	     )
  :group 'data)

(defvar ralee-mode-map nil
  "Keymap for Ralee mode.")

(defvar ralee-mode-map nil
  "Keymap for Ralee mode.")

(if ralee-mode-map
    ()
  (setq ralee-mode-map (make-sparse-keymap))
  (define-key ralee-mode-map "\C-c\C-l" 'ralee-paint-line-by-ss)
  (define-key ralee-mode-map "\C-c\C-b" 'ralee-paint-buffer-by-ss)
;  (define-key ralee-mode-map "\C-c\C-k" 'ralee-paint-line-by-base)
;  (define-key ralee-mode-map "\C-c\C-v" 'ralee-paint-buffer-by-base)
  (define-key ralee-mode-map "\C-c\C-c" 'ralee-paint-column-by-cons)
  (define-key ralee-mode-map "\C-c\C-v" 'ralee-paint-buffer-by-cons)
  (define-key ralee-mode-map "\C-c\C-i" 'ralee-insert-gap-column)
  (define-key ralee-mode-map "\C-c\C-d" 'ralee-delete-gap-column)
  (define-key ralee-mode-map "\C-c\C-p" 'ralee-jump-to-pair)
  (define-key ralee-mode-map "\C-c\C-[" 'ralee-jump-to-pair-in-other-window)
  (define-key ralee-mode-map "\C-f" 'ralee-jump-right)
  (define-key ralee-mode-map "\C-b" 'ralee-jump-left)
  (define-key ralee-mode-map "\C-p" 'ralee-jump-up)
  (define-key ralee-mode-map "\C-n" 'ralee-jump-down)
  )

;; Create mode-specific tables.
(defvar ralee-mode-syntax-table nil 
  "Syntax table used while in ralee mode.")

(if ralee-mode-syntax-table
    ()              ; Do not change the table if it is already set up.
  (setq ralee-mode-syntax-table (make-syntax-table))
  ; parenthesis matching should do <> aswell as the usual suspects
  (modify-syntax-entry ?\( "()" ralee-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" ralee-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" ralee-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" ralee-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" ralee-mode-syntax-table)
  (modify-syntax-entry ?\} "){" ralee-mode-syntax-table)
  (modify-syntax-entry ?\< "(>" ralee-mode-syntax-table)
  (modify-syntax-entry ?\> ")<" ralee-mode-syntax-table)
  )

(defvar ralee-structure-cache nil
  "cache the structure line")

(defvar ralee-base-pairs-cache nil
  "cache the base pairing pattern")

(defvar ralee-gap-symbol "."
  "The gap symbol")


;;;;;;;;

(defun ralee-mode ()
  "Major mode for RALEE alignment editing
Turning on ralee-mode runs the hook `ralee-mode-hook'."
  (interactive)
  (kill-all-local-variables)
  (use-local-map ralee-mode-map)
;  (setq local-abbrev-table ralee-mode-abbrev-table)
  (set-syntax-table ralee-mode-syntax-table)
  (setq truncate-lines 1)
  (setq ralee-jump-num 20)
  (setq mode-name "Ralee")
  (setq major-mode 'ralee-mode)
  (run-hooks 'ralee-mode-hook))      ; Finally, this permits the user to
                                     ;   customize the mode with a hook.





(defun current-line ()  ; surely this should be a default method?
  "Return the vertical position of point..."
  (count-lines (point-min) (point)))



(defun ralee-is-alignment-line ()
  "Check if the current line is part of the alignment itself"
  (save-excursion
    (beginning-of-line)
    (looking-at "[A-Za-z0-9]+\.[0-9]+/[0-9]+-[0-9]+\ +")
  ))

(defun ralee-is-markup-line ()
  "Check if the current line is #=GC"
  (save-excursion
    (beginning-of-line)
    (looking-at "#=GC ")
  ))

(defun ralee-get-seq-id ()
  "get the sequence identifier of the current alignment line"
  (beginning-of-line)
  (search-forward " ")
  (copy-region-as-kill (line-beginning-position) (1- (point)))
  (car kill-ring))

(defun ralee-get-seq-string ()
  "get the sequence string of the current alignment line"
  (end-of-line)
  (search-backward " ")
  (copy-region-as-kill (1+ (point)) (line-end-position))
  (car kill-ring))


(defun ralee-ungap-string (string)
  "take a seq string as input and return an ungapped version"
  (interactive)
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
  

