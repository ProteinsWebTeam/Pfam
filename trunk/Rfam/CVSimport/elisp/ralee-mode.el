;;; ralee-mode.el --- ralee mode

; Copyright (c) 2004 Sam Griffiths-Jones
;
; This is part of RALEE -- see
; http://www.sanger.ac.uk/Users/sgj/code/ralee/ and the README file
; that should accompany this file.
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


(require 'ralee-faces)
(require 'ralee-helpers)
(require 'ralee-movement)
(require 'ralee-paint)
(require 'ralee-structure)
(require 'ralee-edit)
(require 'ralee-tools)


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
  (define-key ralee-mode-map "\C-c\C-l" 'paint-line-by-ss)
  (define-key ralee-mode-map "\C-c\C-b" 'paint-buffer-by-ss)
  (define-key ralee-mode-map "\C-c\C-n" 'paint-buffer-by-current-ss-line)
  (define-key ralee-mode-map "\C-c\C-c" 'paint-column-by-cons)
  (define-key ralee-mode-map "\C-c\C-v" 'paint-buffer-by-cons)
  (define-key ralee-mode-map "\C-c\C-i" 'insert-gap-column)
  (define-key ralee-mode-map "\C-c\C-d" 'delete-gap-column)
  (define-key ralee-mode-map "." 'insert-gap)
  (define-key ralee-mode-map "\C-d" 'delete-gap)
  (define-key ralee-mode-map "\C-c\C-p" 'jump-to-pair)
  (define-key ralee-mode-map "\C-c\C-[" 'jump-to-pair-in-other-window)
  (define-key ralee-mode-map "\C-c\C-f" 'fold-sequence)
  (define-key ralee-mode-map "\C-f" 'jump-right)
  (define-key ralee-mode-map "\C-b" 'jump-left)
  (define-key ralee-mode-map "\C-p" 'jump-up)
  (define-key ralee-mode-map "\C-n" 'jump-down)
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


(provide 'ralee-mode)
