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
  (define-key ralee-mode-map "\C-c\C-k" 'ralee-paint-line-by-base)
  (define-key ralee-mode-map "\C-c\C-v" 'ralee-paint-buffer-by-base)
  (define-key ralee-mode-map "\C-p" 'ralee-jump-to-pair)
  (define-key ralee-mode-map "\C-f" 'ralee-jump-right)
  (define-key ralee-mode-map "\C-b" 'ralee-jump-left))

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
  (interactive)
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


(defun ralee-parse-ss (structure-string)
  "Parse the secondary structure markup.
Returns a list of pairs in order of increasing closing base.
This order is important for calculating helix boundaries."
  (let ((stack ())
	(i 0)
	(pairs ())
	structure
	)

    (setq structure (split-string structure-string ""))
    (while structure
      (setq i (1+ i))
      (if (equal (car structure) "<")
	  (setq stack (cons i stack))
	)
      (if (equal (car structure) ">")
	  (progn
;	    (if pairs
		(setq pairs (cons (cons (car stack) i) pairs))
;	      (setq pairs (cons (car stack) i))
;	      )
	    (setq stack (cdr stack))
	    )
	)
      (setq structure (cdr structure))
      )
    pairs
    )
  )

(defun ralee-get-ss ()
  "get the structure from the current buffer"
  (save-excursion
    (beginning-of-buffer)
    (search-forward "#=GC SS_cons")
    (copy-region-as-kill (line-beginning-position) (line-end-position))
    )
  (car kill-ring)
  )


; complex definition of a face
; try a simpler one
(defface ralee-face-a
  `((((type tty) (class color))
     (:background "yellow" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "black"))
    (t (:background "gray")))
  "Highlighting face a"
  :group 'basic-faces)

(defface ralee-face-b
  `((((type tty) (class color))
     (:background "white" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "white" :foreground "black"))
    (((class color) (background light))
     (:background "darkblue"))
    (t (:background "gray")))
  "Highlighting face b"
  :group 'basic-faces)

(defface ralee-face-c
  `((((type tty) (class color))
     (:background "lightgreen" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "lightgreen" :foreground "black"))
    (((class color) (background light))
     (:background "darkgreen"))
    (t (:background "gray")))
  "Highlighting face c"
  :group 'basic-faces)

(defface ralee-face-d
  `((((type tty) (class color))
     (:background "pink" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "pink" :foreground "black"))
    (((class color) (background light))
     (:background "red"))
    (t (:background "gray")))
  "Highlighting face d"
  :group 'basic-faces)

(defface ralee-face-e
  `((((type tty) (class color))
     (:background "white" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "white" :foreground "black"))
    (((class color) (background light))
     (:background "gray"))
    (t (:background "gray")))
  "Highlighting face e"
  :group 'basic-faces)

(defface ralee-face-f
  `((((type tty) (class color))
     (:background "red" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "red" :foreground "white"))
    (((class color) (background light))
     (:background "red"))
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
      (setq open (+ beg (car pair)))
      (setq close (+ beg (cdr pair)))

      (copy-region-as-kill (- open 1) open)
      (setq openbase (car kill-ring))
      (copy-region-as-kill (- close 1) close)
      (setq closebase (car kill-ring))

      ; remainder operator guarantees that we'll get a colour
      (setq face-num (% (cdr (assoc (car pair) helices)) (length ralee-faces)))

      (if (or
	   (and (or (equal openbase "G") (equal openbase "g")) 
		(or (equal closebase "C") (equal closebase "c")
		    (equal closebase "U") (equal closebase "u")
		    (equal closebase "T") (equal closebase "t")))
	   (and (or (equal openbase "C") (equal openbase "c")) 
		(or (equal closebase "G") (equal closebase "g")))
	   (and (or (equal openbase "U") (equal openbase "u")
		    (equal openbase "T") (equal openbase "t"))
		(or (equal closebase "A") (equal closebase "a")
		    (equal closebase "G") (equal closebase "g")))
	   (and (or (equal openbase "A") (equal openbase "a")) 
		(or (equal closebase "U") (equal closebase "u")
		    (equal closebase "T") (equal closebase "t")))
	   )

	  (progn
	    (put-text-property (- open 1) open 'face (nth face-num ralee-faces))
	    (put-text-property (- close 1) close 'face (nth face-num ralee-faces))
	    )
	)
      )
    )
  )


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
	(copy-region-as-kill (point) (1+ (point)))
	(setq base (car kill-ring))
	(setq face-num nil)
	
	(if (or (equal base "G") (equal base "g"))
	    (setq face-num 0))
	(if (or (equal base "C") (equal base "c"))
	    (setq face-num 1))
	(if (or (equal base "A") (equal base "a"))
	    (setq face-num 2))
	(if (or (equal base "T") (equal base "t")
		(equal base "U") (equal base "u"))
	    (setq face-num 3))
	
	(if face-num
	    (put-text-property (point) (1+ (point)) 'face (nth face-num ralee-faces)))
	)
      )
    )
  )


(defun ralee-paint-buffer-by-base ()
  "colour the current line according to base identity"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (ralee-paint-line-by-base)
      (forward-line)
      )
    )
  )


(defun ralee-paint-line-by-ss ()
  "get the structure, and then paint the current line"
  (interactive)
  (save-excursion
    (setq structure (ralee-get-ss))
    (setq pairs (ralee-parse-ss structure))
    (ralee-paint-line-by-pairs pairs)
    )
  )

(defun ralee-paint-buffer-by-ss ()
  "get the structure, and then paint the whole buffer"
  (interactive)
  (save-excursion
    (setq structure (ralee-get-ss))
    (setq pairs (ralee-parse-ss structure))
    (beginning-of-buffer)
    (ralee-paint-line-by-pairs pairs) ; make sure we do the first line aswell
    (while (search-forward "\n")
      (ralee-paint-line-by-pairs pairs)
      )
    )
  )



(defun ralee-jump-right ()
  "move the pointer jump-num characters to the right"
  (interactive)
  (forward-char ralee-jump-num)
  )

(defun ralee-jump-left ()
  "move the pointer jump-num characters to the left"
  (interactive)
  (backward-char ralee-jump-num)
  )


(defun ralee-jump-to-pair ()
  "jump to the pairing base"
  (interactive)
  (setq structure (ralee-get-ss))
  (setq pairs (ralee-parse-ss structure))
  (setq pair (assoc (1- (current-column)) pairs))
  (if (equal (car pair) (1- (current-column)))
      (setq paired-column (cdr pair))
    (setq paired-column (car pair)))
  (if paired-column
      (move-to-column paired-column))
  )
