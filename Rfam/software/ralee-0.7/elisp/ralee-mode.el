;;; ralee-mode.el --- ralee mode

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


(defvar check-ralee-version 0.7
  "This version.")

;; interval doesn't actually do anything yet, but
;; set to nil to disable checks
(defcustom check-for-update-interval 7
  "Number of days between checks for updates.")

(defcustom ralee-auto-unblock t
  "Automatically unblock blocked alignments on load."
  :type '(boolean)
  :group 'ralee
  )

(defvar ralee-url "http://personalpages.manchester.ac.uk/staff/sam.griffiths-jones/software/ralee/index.html"
  "Url to load for updates.")

(defun check-for-updates ()
  "check for updates"
  (interactive)
  (if (and check-for-update-interval
	   (executable-find "ralee-check-update.pl"))
      (let ((version (string-to-number (shell-command-to-string "ralee-check-update.pl"))))
	(if (< version 0)
	    (message-box "Failed to connect to RALEE website to check for updates.\nPerhaps you need to specify a proxy server (using the environment variable HTTP_PROXY).\nYou can disable future checks in the 'Edit/Options' menu.")
	  (if (> version check-ralee-version)
	      (if (x-popup-dialog t '("New version available.\nWould you like to send your browser to the RALEE website?" ("yes".t) ("no".nil)))
		  (browse-url ralee-url)
		)
	    )
	  )
	)
    )
  )

(require 'ralee-faces)
(require 'ralee-helpers)
(require 'ralee-movement)
(require 'ralee-paint)
(require 'ralee-structure)
(require 'ralee-edit)
(require 'ralee-tools)
(require 'ralee-read)
(require 'ralee-output)

(defcustom ralee-mode-hook nil
  "Normal hook run when entering ralee mode"
  :type 'hook
  :group 'data)

;; delete whitespace at the end of lines, as it'll only cause trouble later!
(add-hook 'ralee-mode-hook 'ralee-delete-whitespace-eol)

;; check for updates on load
(add-hook 'ralee-mode-hook 'check-for-updates)

(defvar ralee-mode-map nil
  "Keymap for Ralee mode.")

(if ralee-mode-map
    ()
  (setq ralee-mode-map (make-sparse-keymap))
  (define-key ralee-mode-map "\C-c\C-l" 'paint-line-by-ss)
  (define-key ralee-mode-map "\C-c\C-b" 'paint-buffer-by-ss)
  (define-key ralee-mode-map "\C-c\C-x" 'paint-buffer-by-compensatory-changes)
  (define-key ralee-mode-map "\C-c\C-z" 'paint-buffer-by-compensatory-all)
  (define-key ralee-mode-map "\C-c\C-n" 'paint-buffer-by-current-ss-line)
  (define-key ralee-mode-map "\C-c\C-v" 'paint-buffer-by-base)
  (define-key ralee-mode-map "\C-c\C-c" 'paint-buffer-by-cons)
  (define-key ralee-mode-map "\C-c\C-q" 'paint-buffer-by-cons-generic)
  (define-key ralee-mode-map [(control ?#)] 'toggle-paint-char)
  (define-key ralee-mode-map [mouse-1] 'toggle-highlight-current-line)

  (define-key ralee-mode-map [(control ?\\)] 'define-block)
  (define-key ralee-mode-map [(control c) (control ?\\)] 'undefine-block)
  (define-key ralee-mode-map "\C-c\C-a" 'realign-block)

  (define-key ralee-mode-map "\C-c\C-i" 'insert-gap-column)
  (define-key ralee-mode-map "\C-c\C-d" 'delete-gap-column)
  (define-key ralee-mode-map [(control ?,)] 'shift-sequence-left)
  (define-key ralee-mode-map [(control ?.)] 'shift-sequence-right)
  (define-key ralee-mode-map [(control ?<)] 'throw-sequence-left)
  (define-key ralee-mode-map [(control ?>)] 'throw-sequence-right)

  (define-key ralee-mode-map "\C-c\C-f" 'fold-sequence)
  (define-key ralee-mode-map "\C-c\C-g" 'fetch-sequence)
  (define-key ralee-mode-map "\C-c\C-h" 'show-sequence-description)

  (define-key ralee-mode-map "\C-c\C-s" 'ralee-motif-search)
  (define-key ralee-mode-map "\C-c\C-p" 'jump-to-pair)
  (define-key ralee-mode-map "\C-c\C-o" 'jump-to-pair-in-other-window)
  (define-key ralee-mode-map [(control left)] 'jump-left)
  (define-key ralee-mode-map "\M-[E" 'jump-left)
  (define-key ralee-mode-map [(control right)] 'jump-right)
  (define-key ralee-mode-map "\C-[F" 'jump-right)
  (define-key ralee-mode-map [(control up)] 'jump-up)
  (define-key ralee-mode-map [(control down)] 'jump-down)

  (define-key ralee-mode-map [(control c) (control ?=)] 'add-base-pair)
  (define-key ralee-mode-map [(control c) (control ?-)] 'remove-base-pair)


  (if (featurep 'xemacs)
      () ;; the following stuff fails in xemacs
    (define-key ralee-mode-map "\C-l" 'center-on-point) ;; a small change from
                                                        ;; standard behaviour
    )

;  (define-key ralee-mode-map "\C-f" 'jump-right)  ;; don't do these anymore:
;  (define-key ralee-mode-map "\C-b" 'jump-left)   ;; people expect standard
;  (define-key ralee-mode-map "\C-p" 'jump-up)     ;; behaviour for standard
;  (define-key ralee-mode-map "\C-n" 'jump-down)   ;; emacs bindings

;  (define-key ralee-mode-map "." 'insert-gap)     ;; only in protect mode
;  (define-key ralee-mode-map "\C-d" 'delete-gap)  ;; only in protect mode

)

;; need to initialise this before menus
(defvar protect-mode nil)

;; have to do this after ralee-mode-map is set up
(require 'ralee-menu)

;; and this after menus are set up
(require 'ralee-protect)

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
  ; default to protect mode -- should be able to do this in
  ; protect mode definition, but the keymap redefinition then
  ; screws up delete and backspace in the minibuffer aswell
;  (protect-mode t) 
  (run-hooks 'ralee-mode-hook)       ; Finally, this permits the user to
					; customize the mode with a hook

  ;; Unblock the alignment
  (if (and ralee-auto-unblock (ralee-blocked-alignment-p))
      (unblock-alignment)
    )

  ;; should do this in a hook as well but big things screw up
  ;; color the buffer by default, unless it is very large
  (if (< (point-max) 100000)
      (paint-buffer-by-ss)
    )
  )


(provide 'ralee-mode)
