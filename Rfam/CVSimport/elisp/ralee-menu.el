;;; ralee-menu 	

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



; The following sets up a menu in GNU emacs but not in xemacs
(define-key ralee-mode-map [menu-bar] (make-sparse-keymap))
(define-key ralee-mode-map [menu-bar structure]
  (cons "Structure" (make-sparse-keymap "Structure")))
(define-key ralee-mode-map [menu-bar structure show-structure-ps]
  '("Show structure" . show-structure-ps))
(define-key ralee-mode-map [menu-bar structure fold-all-sequences]
  '("Fold all sequences" . fold-all-sequences))
(define-key ralee-mode-map [menu-bar structure fold-sequences]
  '("Fold sequence" . fold-sequence))
(define-key ralee-mode-map [menu-bar structure separator2]
  '("--"))
(define-key ralee-mode-map [menu-bar structure jump-to-pair-in-other-window]
  '("Jump to pairing base in another window" . jump-to-pair-in-other-window))
(define-key ralee-mode-map [menu-bar structure jump-to-pair]
  '("Jump to pairing base" . jump-to-pair))
(define-key ralee-mode-map [menu-bar structure separator1]
  '("--"))
(define-key ralee-mode-map [menu-bar structure paint-buffer-by-current-ss-line]
  '("Paint buffer by current structure" . paint-buffer-by-current-ss-line))
(define-key ralee-mode-map [menu-bar structure paint-buffer-by-cons]
  '("Paint buffer by base" . paint-buffer-by-cons))
(define-key ralee-mode-map [menu-bar structure paint-buffer-by-ss]
  '("Paint buffer by structure" . paint-buffer-by-ss))


(provide 'ralee-menu)
