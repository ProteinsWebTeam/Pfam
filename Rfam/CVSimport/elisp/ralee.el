(save-excursion
  (let (beg end)
    (beginning-of-buffer) (setq beg (point))
    (end-of-buffer) (setq end (point))
    (remove-text-properties beg end '(fontified nil))
    (text-mode)   ; need to turn off font locking
    (set-face-attribute 'default nil
			:background "black")
    (setq truncate-lines 1)
    )
  )


(defun ralee-parse-ss (structure-string)
  "Parse the secondary structure markup"
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
	    (setq pairs (cons (cons (car stack) i) pairs))
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


(defface ralee-face-a
  `((((type tty) (class color))
     (:background "yellow" :foreground "darkblue"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "yellow" :foreground "darkblue"))
    (((class color) (background light))
     (:background "lightblue"))
    (t (:background "gray")))
  "Highlighting face a"
  :group 'basic-faces)


(defun ralee-paint-line-by-pairs (pairs-list)
  "colour the current line according to the SS_cons line"
  (let (beg end (pair ()) open close openbase closebase)
    (beginning-of-line) (setq beg (point))
    (end-of-line) (setq end (point))
    (put-text-property beg end 'face 'default)
    (while pairs-list
      (setq pair (car pairs-list))
      (setq pairs-list (cdr pairs-list))
      (setq open (+ beg (car pair)))
      (setq close (+ beg (cdr pair)))

      (copy-region-as-kill (- open 1) open)
      (setq openbase (car kill-ring))
      (copy-region-as-kill (- close 1) close)
      (setq closebase (car kill-ring))

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
	    (put-text-property (- open 1) open 'face 'ralee-face-a)
	    (put-text-property (- close 1) close 'face 'ralee-face-a)
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

(define-key global-map "\C-c\C-l" 'ralee-paint-line-by-ss)
(define-key global-map "\C-c\C-b" 'ralee-paint-buffer-by-ss)