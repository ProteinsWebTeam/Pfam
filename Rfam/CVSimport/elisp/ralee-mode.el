;;; ralee-mode.el --- ralee mode


(defcustom ralee-mode-hook nil
  "Normal hook run when entering ralee mode and many related modes."
  :type 'hook
  :options '(turn-off-auto-fill
	     )
  :group 'data)

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
  (define-key ralee-mode-map "\C-i" 'ralee-insert-gap-column)
  (define-key ralee-mode-map "\C-c\C-d" 'ralee-delete-gap-column)
  (define-key ralee-mode-map "\C-c\C-p" 'ralee-jump-to-pair)
  (define-key ralee-mode-map "\C-c\C-[" 'ralee-jump-to-pair-in-other-window)
  (define-key ralee-mode-map "\C-f" 'ralee-jump-right)
  (define-key ralee-mode-map "\C-b" 'ralee-jump-left)
  (define-key ralee-mode-map "\C-p" 'ralee-jump-up)
  (define-key ralee-mode-map "\C-n" 'ralee-jump-down)
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
;  (set-syntax-table ralee-mode-syntax-table)
  (setq truncate-lines 1)
  (setq ralee-jump-num 20)
  (setq mode-name "Ralee")
  (setq major-mode 'ralee-mode)
  (run-hooks 'ralee-mode-hook))      ; Finally, this permits the user to
                                     ;   customize the mode with a hook.



(defun ralee-helix-map (pairs)
  "Calculate helix boundaries based on pairs."
  (let ((helix 0)
	(lastopen 0)
	(lastclose 9999999)
	pair
	open
	(i (length pairs))
	j
	close
	helices)

    (while (>= i 0)
      (setq i (1- i))
      (setq pair (nth i pairs))
      (setq open (car pair))
      (setq close (cdr pair))

      ; we're moving from right to left based on closing pair
      ;
      ; catch things like
      ; <<..>>..<<..>>
      ;          *
      (if (> open lastclose)
	  (setq helix (+ helix 1))
	)

      ; catch things like
      ; <<..<<..>>..<<..>>>>
      ;                   *
      (setq j 0)
      (while (nth j pairs)
	(setq p (nth j pairs))
	(setq j (1+ j))
	(if (and (< (car p) lastopen)
		 (> (car p) open))
	    (if (equal (assoc (car p) helices) helix)
		()
	      (setq helix (+ helix 1))
	      )
	  )
	)

      (setq helices (cons (cons close helix) helices))
      (setq helices (cons (cons open helix) helices))

      (setq lastopen open)
      (setq lastclose close)

      )
    helices
    )
  )


(defun ralee-get-ss-line ()
  "Get the SS_cons line"
  (save-excursion
    (let (structure-line)
      (goto-char (point-min))
      (search-forward "#=GC SS_cons")
      (copy-region-as-kill (line-beginning-position) (line-end-position))
      (setq structure (car kill-ring)))))


(defun ralee-structure-has-changed (structure-line)
  "check if the structure has changed"
  (interactive)
  (if (equal structure-line ralee-structure-cache)
      nil
    t))


(defun ralee-get-base-pairs ()
  "Parse the secondary structure markup.
Returns a list of pairs in order of increasing closing base."
  (save-excursion
    (let ((stack ())
	  (pairs ())
	  base
	  structure-line
	  )

      (setq structure-line (ralee-get-ss-line))

      ; only recalculate the base pairing structure if structure has changed
      (if (ralee-structure-has-changed structure-line)
	  (progn
	    (goto-char (point-min))
	    (search-forward "#=GC SS_cons")
	    (search-forward " ")

	    (while (< (point) (line-end-position))
	      (setq base (char-after))
	      (if (char-equal base ?<)
		  (setq stack (cons (current-column) stack))
		)
	      (if (char-equal base ?>)
		  (progn
		    (setq pairs (cons (cons (car stack) (current-column)) pairs))
		    (setq stack (cdr stack)))
		)
	      (forward-char))
	    (setq ralee-structure-cache structure-line)   ; cache these for speed
	    (setq ralee-base-pairs-cache pairs)))         ;
      
      ralee-base-pairs-cache)))



; complex definition of a face
; try a simpler one
(defface ralee-face-a
  `((((type tty) (class color))
     (:background "skyblue" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "skyblue" :foreground "black"))
    (((class color) (background light))
     (:background "darkblue"))
    (t (:background "gray")))
  "Highlighting face a"
  :group 'basic-faces)

(defface ralee-face-b
  `((((type tty) (class color))
     (:background "lightgreen" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "lightgreen" :foreground "black"))
    (((class color) (background light))
     (:background "darkgreen"))
    (t (:background "gray")))
  "Highlighting face b"
  :group 'basic-faces)

(defface ralee-face-c
  `((((type tty) (class color))
     (:background "pink" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "pink" :foreground "black"))
    (((class color) (background light))
     (:background "red"))
    (t (:background "gray")))
  "Highlighting face c"
  :group 'basic-faces)

(defface ralee-face-d
  `((((type tty) (class color))
     (:background "white" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "white" :foreground "black"))
    (((class color) (background light))
     (:background "gray"))
    (t (:background "gray")))
  "Highlighting face d"
  :group 'basic-faces)

(defface ralee-face-e
  `((((type tty) (class color))
     (:background "red" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "red" :foreground "white"))
    (((class color) (background light))
     (:background "red"))
    (t (:background "gray")))
  "Highlighting face e"
  :group 'basic-faces)

(defface ralee-face-f
  `((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "blue" :foreground "white"))
    (((class color) (background light))
     (:background "black"))
    (t (:background "gray")))
  "Highlighting face f"
  :group 'basic-faces)


(setq ralee-faces '(ralee-face-a 
		    ralee-face-b 
		    ralee-face-c
		    ralee-face-d
		    ralee-face-e
		    ralee-face-f
		    ))


(defun ralee-paint-line-by-pairs (pairs-list)
  "colour the current line according to the SS_cons line"
  (let (beg end (pair ()) open close openbase closebase helices)
    (beginning-of-line) (setq beg (point))
    (end-of-line) (setq end (point))
    (put-text-property beg end 'face 'default)

    (setq helices (ralee-helix-map pairs-list))

    (while pairs-list
      (setq pair (car pairs-list))
      (setq pairs-list (cdr pairs-list))

      (move-to-column (car pair))
      (setq open (point))
      (setq openbase (char-after))

      (move-to-column (cdr pair))
      (setq close (point))
      (setq closebase (char-after))

      ; remainder operator guarantees that we'll get a colour
      (setq face-num (% (cdr (assoc (car pair) helices)) (length ralee-faces)))

      (let ((case-fold-search 1)) ; case-independent search
	(if (or
	     (and (char-equal openbase ?G) (or (char-equal closebase ?C)
					       (char-equal closebase ?U)
					       (char-equal closebase ?T)))
	     (and (char-equal openbase ?C) (char-equal closebase ?G))
	     (and (or (char-equal openbase ?U)
		      (char-equal openbase ?T)) (or (char-equal closebase ?A)
						    (char-equal closebase ?G)))
	     (and (char-equal openbase ?A) (or (char-equal closebase ?U)
					       (char-equal closebase ?T))))

	    (progn
	      (put-text-property open (1+ open) 'face (nth face-num ralee-faces))
	      (put-text-property close (1+ close) 'face (nth face-num ralee-faces))
	      ))))))


(defun ralee-paint-line-by-base ()
  "colour the current line according to base identity"
  (interactive)
  (save-excursion
    (let (beg
	  end
	  face-num,
	  base)
      (beginning-of-line) (setq beg (point))
      (end-of-line) (setq end (point))
      (put-text-property beg end 'face 'default)
      
      (search-forward "\n")
      (search-backward " ") ; so we end up at the start of the sequence
     
      (while (< (point) end)  ; until the end of the line
	(forward-char)
	(setq base (char-after))
	(setq face-num nil)
	
	(let ((case-fold-search t)) ; case independent search
	  (if (char-equal base ?G)
	      (setq face-num 0))
	  (if (char-equal base ?C)
	      (setq face-num 1))
	  (if (char-equal base ?A)
	      (setq face-num 2))
	  (if (or (char-equal base ?T) (char-equal base ?U))
	      (setq face-num 3))
	
	  (if face-num
	      (put-text-property (point) (1+ (point)) 'face (nth face-num ralee-faces))))))))


(defun ralee-paint-column-by-cons ()
  "paint a column by its conservation"
  (interactive)
  (save-excursion
    (let (column
	  (a-count 0)
	  (c-count 0)
	  (g-count 0)
	  (u-count 0)
	  (cons 0.3))

      (setq column (current-column))
      (goto-char (point-max))
      (setq line-count (current-line))
      (goto-char (point-min))

      (while (< (point) (point-max))   ; go round once counting bases
	(if (ralee-is-alignment-line)
	    (progn
	      (move-to-column column)
	      (put-text-property (point) (1+ (point)) 'face 'default)
	      (setq base (char-after))
	      (let ((case-fold-search t))
		(if (char-equal base ?A) (setq a-count (1+ a-count)))
		(if (char-equal base ?C) (setq c-count (1+ c-count)))
		(if (char-equal base ?G) (setq g-count (1+ g-count)))
		(if (or (char-equal base ?U)
			(char-equal base ?T)) (setq u-count (1+ u-count)))
		)))
	(forward-line))

      (if (or (> (/ (float a-count) (float line-count)) cons)
	      (> (/ (float c-count) (float line-count)) cons)
	      (> (/ (float g-count) (float line-count)) cons)
	      (> (/ (float u-count) (float line-count)) cons))

	  (progn
	    (goto-char (point-min))

	    (while (< (point) (point-max))  ; go round again colouring them
	      (if (ralee-is-alignment-line)
		  (progn
		    (move-to-column column)
		    (setq base (char-after))
		    (let ((case-fold-search t))
		      (if (and (char-equal base ?A) (> (/ (float a-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 0 ralee-faces)))
		      (if (and (char-equal base ?C) (> (/ (float c-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 1 ralee-faces)))
		      (if (and (char-equal base ?G) (> (/ (float g-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 2 ralee-faces)))
		      (if (and (or (char-equal base ?T)
				   (char-equal base ?U)) (> (/ (float u-count) (float line-count)) cons))
			  (put-text-property (point) (1+ (point)) 'face (nth 3 ralee-faces)))
		      )))
	      (forward-line))
	    )))))


(defun ralee-paint-buffer-by-cons ()
  "paint whole buffer by column conservation"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (equal (ralee-is-alignment-line) nil)
      (forward-line)) ; search for the first alignment line
    (search-forward "\n")
    (search-backward " ")
    (while (< (point) (line-end-position))
      (ralee-paint-column-by-cons)
      (forward-char))))



(defun ralee-paint-buffer-by-base ()
  "colour the current line according to base identity"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (ralee-paint-line-by-base))
      (forward-line))))


(defun ralee-paint-line-by-ss ()
  "get the structure, and then paint the current line"
  (interactive)
  (save-excursion
    (setq pairs (ralee-get-base-pairs))
    (ralee-paint-line-by-pairs pairs)))


(defun ralee-paint-buffer-by-ss ()
  "get the structure, and then paint the whole buffer"
  (interactive)
  (save-excursion
    (setq pairs (ralee-get-base-pairs))
    (beginning-of-buffer)
    (while (< (point) (point-max))
      (if (ralee-is-alignment-line)
	  (ralee-paint-line-by-pairs pairs))
      (forward-line))))


(defun ralee-jump-right ()
  "move the pointer jump-num characters to the right"
  (interactive)
  (forward-char ralee-jump-num))

(defun ralee-jump-left ()
  "move the pointer jump-num characters to the left"
  (interactive)
  (backward-char ralee-jump-num))

(defun ralee-jump-up ()
  "move the pointer jump-num lines up"
  (interactive)
  (let ((column (current-column)))
    (forward-line (- 0 ralee-jump-num))
    (move-to-column column)))

(defun ralee-jump-down ()
  "move the pointer jump-num lines down"
  (interactive)
  (let ((column (current-column)))
    (forward-line ralee-jump-num)
    (move-to-column column)))


(defun ralee-paired-column (column)
  "return the pair of <column>"
  (let (pair-column
	pairs
	pair)
    (setq pairs (ralee-get-base-pairs))
    (setq pair (assoc column pairs))
    (if pair
	(setq paired-column (cdr pair))
      (progn
	(setq pair (rassoc column pairs))
	(if pair
	    (setq paired-column (car pair)))))
    
    (if paired-column
	paired-column
      nil)))


(defun ralee-jump-to-pair ()
  "jump to the pairing base"
  (interactive)
  (let (paired-column)
    (setq paired-column (ralee-paired-column (current-column)))
    (if paired-column
	(progn
	  (message "column %s pairs with column %s" (current-column) paired-column)
	  (move-to-column paired-column))
      (message "No pair!"))))



(defun ralee-jump-to-pair-in-other-window ()
  "jump the cursor to the pairing base in other window
- make the other window if necessary"
  (interactive)
  (let (paired-column
	line)
    (setq line (current-line))
    (setq paired-column (ralee-paired-column (current-column)))
    (if paired-column
	(progn
	  (if (one-window-p)
	      (split-window))  ; make another window if there isn't already one
	  (message "column %s pairs with column %s" (current-column) paired-column)
	  (select-window (next-window))
	  (goto-line line)
	  (move-to-column paired-column)
	  (recenter))
      (message "No pair!"))))


(defun current-line ()  ; surely this should be a default method?
  "Return the vertical position of point..."
  (count-lines (point-min) (point)))


(defun ralee-insert-gap-column ()
  "Insert a column of gap residues"
  (interactive)
  (save-excursion
    (let (column)
      (setq column (current-column))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column column)
	      (insert ralee-gap-symbol)))
	(forward-line)))))


(defun ralee-delete-gap-column ()
  "Delete a column but only if it is all gaps"
  (interactive)
  (save-excursion
    (let (column notagap)
      (setq column (current-column))
      (goto-char (point-min))
      (while (< (point) (point-max))
	(if (or (ralee-is-alignment-line) (ralee-is-markup-line))
	    (progn
	      (move-to-column column)
	      (setq char (char-after))
	      (if (char-equal char (string-to-char ralee-gap-symbol))
		  ()
		(progn
		  (setq notagap t)
		  (goto-char (point-max))))))  ; break the loop
	(forward-line))

      (if notagap    ; go round again, deleting the base this time
	  (message "Column %s contains non-gap characters -- cannot delete" column)
	(progn
	  (goto-char (point-min))
	  (while (< (point) (point-max))
	    (if (or (ralee-is-alignment-line) (ralee-is-markup-line))
		(progn
		  (move-to-column column)
		  (delete-char 1)))
	    (forward-line)
	    ))))))


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

