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


(defvar menu-bar-ralee-menu (make-sparse-keymap "Ralee"))

(define-key menu-bar-ralee-menu [paint-buffer-by-ss]
  '("Paint buffer by structure" . paint-buffer-by-ss))
(define-key menu-bar-ralee-menu [paint-line-by-ss]
  '("Paint line by structure" . paint-line-by-ss))
(define-key menu-bar-ralee-menu [separator]
  '("--"))
(define-key menu-bar-ralee-menu [jump-forward]
  '("Jump forward" . jump-forward))


(provide 'ralee-menu)