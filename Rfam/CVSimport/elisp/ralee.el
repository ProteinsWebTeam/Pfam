;(save-excursion
;  (let (beg end)
;    (beginning-of-buffer) (setq beg (point))
;    (end-of-buffer) (setq end (point))
;    (remove-text-properties beg end '(fontified nil))
;    )
;  )

(text-mode)   ; need to turn off font locking
(set-face-attribute 'default nil
		    :background "black")
(setq truncate-lines 1)

(define-key global-map "\C-c\C-l" 'ralee-paint-line-by-ss)
(define-key global-map "\C-c\C-b" 'ralee-paint-buffer-by-ss)
(define-key global-map "\C-f" 'ralee-move-20-right)
(define-key global-map "\C-b" 'ralee-move-20-left)


;;;;;;;;

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



(defun ralee-move-20-right ()
  "move the pointer 20 characters to the right"
  (interactive)
  (forward-char 20)
  )

(defun ralee-move-20-left ()
  "move the pointer 20 characters to the left"
  (interactive)
  (backward-char 20)
  )

