;; ralee-output

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


(defcustom fontsize 10
  "*Choose fontsize"
  :type 'integer
  :group 'write-ps)

(defcustom orientation "Portrait"
  "*Output page orientation"
  :type '(radio (const "Portrait")
		(const "Landscape"))
  :group 'write-ps)

(defcustom fitpage nil
  "*Set to true to fit the alignment block to the page"
  :type '(radio (const :tag "No" nil)
		(const :tag "Yes" t))
  :group 'write-ps)

(defcustom pagelines 0
  "*Number of lines on a page - choose 0 to disable"
  :type 'integer
  :group 'write-ps)

(defcustom pagewidth 0
  "*Width of the page in characters - 0 means automatic based on fontsize"
  :type 'integer
  :group 'write-ps)


(defun page-setup ()
  "run customize-group on write-ps"
  (interactive)
  (customize-group 'write-ps)
  )
  
(defun write-ps (file)
  "write a postscript file of coloured alignment"
  (interactive "FFilename to write postscript to: ")
  (save-excursion
    (let ((maxn (ralee-maxidlength))
	  (numseqs (ralee-number-seqs))
	  (offset (/ fontsize 4))
	  (xzero 40)
	  (yzero 780)
	  (rotate 0)
	  (buffername (buffer-name))
	  first-seqid
	  (page 1)
	  col
	  )

      (if (equal pagewidth 0)
	  (if (string-equal orientation "Landscape")
	      (setq pagewidth (truncate (/ 1250 fontsize)))
	    (setq pagewidth (truncate (/ 850 fontsize)))
	    )
	)

      (if (equal pagelines 0)
	  (if (string-equal orientation "Landscape")
	      (setq pagelines (truncate (/ 520 fontsize)))
	    (setq pagelines (truncate (/ 720 fontsize)))
	    )
	)

      (if (string-equal orientation "Landscape")
	  (progn
	    (setq rotate 90)
	    (setq yzero -40)
	    )
	)

      ;; first make sure no lines are highlighted
      (goto-char (point-min))
      (while (< (point) (point-max))
	(beginning-of-line)
	(let ((prop (prin1-to-string (get-text-property (point) 'face))))
	  (if (string-match "bold" prop)
	      (lowlight-current-line)
	    )
	  )
	(forward-line)
	)

      (ralee-find-first-line)
      (setq first-seqid (ralee-get-seq-id))
      (setq col (ralee-find-first-column))

      (get-buffer-create file)
      (switch-to-buffer file)

      (insert-ps-template)

      (goto-char (point-min))
      (search-forward "%RALEE-DEF")
      (insert (concat "\n/x0 " (number-to-string xzero) " def\n"))
      (insert (concat "/ralee-y0 " (number-to-string yzero) " def\n"))
      (insert (concat "/ralee-rotate " (number-to-string rotate) " def\n"))
      (insert (concat "/ralee-fontsize " (number-to-string fontsize) " def\n"))
      (insert (concat "/ralee-offset -" (number-to-string offset) " def\n"))

      (goto-char (point-min))
      (perform-replace "%RALEE-ORIENTATION" orientation nil nil nil)
      (goto-char (point-min))
      (perform-replace "%RALEE-PAGES" "1" nil nil nil)
      (goto-char (point-min))
      (perform-replace "%RALEE-PAGE" "1" nil nil nil)

      (goto-char (point-min))
      (search-forward "%RALEE-ALIGNMENT-DATA")
      (forward-line)
      (beginning-of-line)
      (insert-buffer buffername)

      (search-backward "%RALEE-ALIGNMENT-DATA")
      (forward-line)

      (while (not (looking-at "showpage"))
	;; go through blocking up the alignment
	(let (bol eol)
	  (beginning-of-line)(setq bol (point))
	  (end-of-line)(setq eol (point))
	  (if (> (- eol bol) pagewidth)
	      (progn
		(let ((seqid (ralee-get-seq-id))
		      (line (current-line))
		      )
		  
		  (move-to-column pagewidth)
		  (kill-region (point) eol)
		  
		  (search-forward "//")
		  (beginning-of-line)
		  (if (string-equal first-seqid seqid)
		      (newline)
		    )
		  (insert (concat seqid " "))
		  (while (< (current-column) col)
		    (insert " ")
		    )
		  (yank)
		  (newline)
		  (goto-line line)
		  )
		)
	    )
	  )
	(forward-line)
	)

      (search-backward "%RALEE-ALIGNMENT-DATA")
      ;; then escape all parentheses for ps
      (goto-char (point-min))
      (perform-replace "(" "\\(" nil nil nil)
      (goto-char (point-min))
      (perform-replace ")" "\\)" nil nil nil)
      (goto-char (point-max))
      (search-backward "%RALEE-ALIGNMENT-DATA")

      (forward-line)

      (let ((linecount 0))
	(while (not (looking-at "showpage"))
	  ;; go through adding the colour markup
	  (let (linestart
		lineend
		(lastcolour "")
		)
	    (end-of-line)(setq lineend (point))
	    (beginning-of-line)(setq linestart (point))
	    
	    (save-restriction
	      ;; just concentrate on the current line
	      (narrow-to-region linestart lineend)
	      
	      (if (ralee-is-alignment-line)
		  (progn
		    (beginning-of-line)
		    (ralee-find-first-column)
		    (insert ") S ")
		    
		    (while (< (point) (point-max))
		      (let ((face (get-text-property (point) 'face))
			    bgcolname
			    bgcolstr
			    fgcolname
			    fgcolstr
			    )
			(setq bgcolname (face-attribute face :background))
			(setq fgcolname (face-attribute face :foreground))
			(setq bgcolstr (color-string-to-rgb bgcolname))
			(setq fgcolstr (color-string-to-rgb fgcolname))
			(print (concat bgcolname " " fgcolname))
			(if (cdr (assoc face ralee-faces))
			    (progn
			      (if (not (equal lastcolour face))
				  ;; change the colour
				  (progn
				    (insert (concat "/bgcolor [" bgcolstr "] def "))
				    (insert (concat "/fgcolor [" fgcolstr "] def "))
				    )
				)
			      (insert "(")
			      (forward-char)
			      (insert ") C ")
			      (setq lastcolour face)
			      )
			  (insert "(")
			  (forward-char)
			  (insert ") S ")
			  )
			)
		      )
		    )
		(progn
		  (end-of-line)
		  (insert ") S ")
		  )
		)
	      
	      ;; do this for all lines
	      (beginning-of-line)
	      (insert "(")
	      (end-of-line)
	      (insert "N")
	      )
	    
	    (setq linecount (1+ linecount))
;	    (insert (concat (number-to-string linecount) " " (number-to-string pagelines) "\n"))
	    ;; if we've run off the bottom of the page
	    (if (> linecount pagelines)
		(progn
		  (setq linecount 0)
		  (setq page (1+ page))
		  (insert "\nshowpage\n")
		  (insert (concat "%%Page: " (number-to-string page) " " (number-to-string page) "\n"))
		  (insert "ralee-rotate rotate
newpath
/y0 ralee-y0 def
x0 y0 moveto\n")
		  )
	      )
	    
	    )
	  (forward-line)
	  )
	)
      (write-file file)
      (switch-to-buffer buffername)
      (kill-buffer file)
      )
    )
  )


(defun insert-ps-template ()
  "This function just keeps the postscript template out of the way"
  (insert "%!PS-Adobe-3.0
%%Title: RALEE alignment
%%Creator: RALEE
%%DocumentPaperSizes: a4
%%Orientation: %RALEE-ORIENTATION
%%Pages: %RALEE-PAGES
%%EndComments

/dobackground {
  currentpoint
  gsave
    newpath
    moveto
    0 ralee-offset rmoveto
    dup 0 rlineto
    0 ralee-fontsize rlineto
    neg 0 rlineto
    closepath
    bgcolor aload pop setrgbcolor
    fill
  grestore
} bind def
 
/dobackgroundstring {
  stringwidth pop
  dobackground
} bind def
 
/S {
  show
} bind def
 
/C {
  dup dobackgroundstring
  fgcolor aload pop setrgbcolor
  show
  0 0 0 setrgbcolor
} bind def
 
/N {
  /y0 y0 ralee-fontsize sub def
  x0 y0 moveto
} bind def

%RALEE-DEF

%RALEE-START-PAGE-HEADER
%%Page: %RALEE-PAGE %RALEE-PAGE
ralee-rotate rotate
/bgcolor [0.53 0.81 0.92] def
/fgcolor [0.00 0.00 0.00] def
/Courier-New findfont
ralee-fontsize scalefont
setfont
newpath
/y0 ralee-y0 def
x0 y0 moveto
%RALEE-END-PAGE-HEADER

%RALEE-ALIGNMENT-DATA
showpage
%%EOF
")
  )


(provide 'ralee-output)

