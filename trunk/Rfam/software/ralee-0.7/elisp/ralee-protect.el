;;; ralee-protect.el --- ralee protect mode

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


; don't overwrite protect-mode if it's set already
(defvar protect-mode nil)
(defvar protect-mode-map nil)

(if protect-mode-map
    ()
  ; not sure what'll happen here if we're not in ralee-mode!
  (setq protect-mode-map (copy-keymap ralee-mode-map))
  (suppress-keymap protect-mode-map)
  (define-key protect-mode-map "\C-o" 'undefined)
  (define-key protect-mode-map "\C-t" 'undefined)
  (define-key protect-mode-map "\C-i" 'undefined)
  (define-key protect-mode-map "\C-j" 'undefined)
  (define-key protect-mode-map "\C-m" 'undefined)
  (define-key protect-mode-map [backspace] 'delete-back-gap)
  (define-key protect-mode-map [delete] 'delete-gap)
  (define-key protect-mode-map "\C-d" 'delete-gap)  
  (define-key protect-mode-map "." 'insert-gap)
  )

;; Don't understand why, but this documented method for setting minor
;; mode map doesn't work properly.  suppress-keymap then stops all
;; keys working in the minibuffer aswell -- not good!  So we'll hack
;; it with use-local-map statements in the protect-mode function.

;(or (assq 'protect-mode minor-mode-map-alist)
;    (setq minor-mode-map-alist
;          (cons (cons 'protect-mode protect-mode-map) minor-mode-map-alist)))


(defun protect-mode-on ()
  "turn protect-mode on"
  (interactive)
  (protect-mode t)
  )

(defun protect-mode-off ()
  "turn protect-mode off"
  (interactive)
  ;; (protect-mode nil)
  ;; doesn't work, so turn it on, then toggle it off
  (setq protect-mode t)
  (protect-mode)
  )

(defun protect-mode (&optional arg)
  "Toggle protect mode.
Non-null prefix argument turns on the mode.
Null prefix argument turns off the mode.

When protect mode is enabled, we can't insert text
or use delete/backspace."
  (interactive)
  (if arg
      (setq protect-mode arg)
    (if protect-mode
	(setq protect-mode nil)
      (setq protect-mode t)
      )
    )

  ;; hack -- see above
  (if protect-mode
      (use-local-map protect-mode-map)
    (use-local-map ralee-mode-map)
    )
  )

(or (assq 'protect-mode minor-mode-alist)
    (setq minor-mode-alist
          (cons '(protect-mode " Protect") minor-mode-alist)))


;; The above hacking is all becasue the following doesn't work in
;; xemacs :(

;(define-minor-mode protect-mode
;  "Toggle protect mode.
;With no argument, this command toggles the mode. 
;Non-null prefix argument turns on the mode.
;Null prefix argument turns off the mode.
;
;When protect mode is enabled, we can't insert text
;or use delete/backspace."
  ;; The initial value.
  ; don't set this to 1 - for some reason we then lose all
  ; keys even in the minibuffer
;  nil
  ;; The indicator for the mode line.
;  " Protect"
  ;; The minor mode bindings.
;  '(
;    ('self-insert-command . undefined)
;    ("\^?"  . delete-back-gap)
;    ("."    . insert-gap)
;    ("\C-d" . delete-gap)
;    )
;  )


(provide 'ralee-protect)
