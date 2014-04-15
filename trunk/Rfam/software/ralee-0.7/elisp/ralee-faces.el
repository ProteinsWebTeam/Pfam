;;; ralee-faces.el

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

(defun color-string-to-rgb (string)
  "Convert colors names to 'n1 n2 n3' strings."
  (format "%.2f %.2f %.2f"
          (/ (nth 0 (color-values string)) 65536.0)
          (/ (nth 1 (color-values string)) 65536.0)
          (/ (nth 2 (color-values string)) 65536.0)))

(defun customize-structure-colors ()
  "Customize the colors used for structure markup"
  (interactive)
  (customize-group 'structure-faces)
  )

(defun customize-base-colors ()
  "Customize the colors used for base markup"
  (interactive)
  (customize-group 'base-faces)
  )

(defun customize-cons-colors ()
  "Customize the colors used for conservation markup"
  (interactive)
  (customize-group 'cons-faces)
  )

; complex definition of a face
; try a simpler one
(defface structure-face-a
  `((((type tty) (class color))
     (:background "skyblue" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "skyblue" :foreground "black"))
    (((class color) (background light))
     (:background "skyblue" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face A"
  :group 'structure-faces)

(defface structure-face-b
  `((((type tty) (class color))
     (:background "lightgreen" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "lightgreen" :foreground "black"))
    (((class color) (background light))
     (:background "lightgreen" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face B"
  :group 'structure-faces)

(defface structure-face-c
  `((((type tty) (class color))
     (:background "pink" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "pink" :foreground "black"))
    (((class color) (background light))
     (:background "pink" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face C"
  :group 'structure-faces)

(defface structure-face-d
  `((((type tty) (class color))
     (:background "yellow" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face D"
  :group 'structure-faces)

(defface structure-face-e
  `((((type tty) (class color))
     (:background "violet" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "violet" :foreground "black"))
    (((class color) (background light))
     (:background "violet" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face E"
  :group 'structure-faces)

(defface structure-face-f
  `((((type tty) (class color))
     (:background "gold" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gold" :foreground "black"))
    (((class color) (background light))
     (:background "gold" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face F"
  :group 'structure-faces)

(defface structure-face-g
  `((((type tty) (class color))
     (:background "wheat" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "wheat" :foreground "black"))
    (((class color) (background light))
     (:background "wheat" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face G"
  :group 'structure-faces)

(defface structure-face-h
  `((((type tty) (class color))
     (:background "cyan" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "cyan" :foreground "black"))
    (((class color) (background light))
     (:background "cyan" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face H"
  :group 'structure-faces)

(defface structure-face-i
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray" :foreground "black"))
    (((class color) (background light))
     (:background "gray" :foreground "black"))
    (t (:background "gray")))
  "Structure markup face I"
  :group 'structure-faces)

(defface base-face-a
  `((((type tty) (class color))
     (:background "skyblue" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "skyblue" :foreground "black"))
    (((class color) (background light))
     (:background "skyblue" :foreground "black"))
    (t (:background "gray")))
  "Face for marking up A's"
  :group 'base-faces)

(defface base-face-c
  `((((type tty) (class color))
     (:background "lightgreen" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "lightgreen" :foreground "black"))
    (((class color) (background light))
     (:background "lightgreen" :foreground "black"))
    (t (:background "gray")))
  "Face for marking up C's"
  :group 'base-faces)

(defface base-face-g
  `((((type tty) (class color))
     (:background "pink" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "pink" :foreground "black"))
    (((class color) (background light))
     (:background "pink" :foreground "black"))
    (t (:background "gray")))
  "Face for marking up G's"
  :group 'base-faces)

(defface base-face-u
  `((((type tty) (class color))
     (:background "yellow" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "yellow" :foreground "black"))
    (((class color) (background light))
     (:background "yellow" :foreground "black"))
    (t (:background "gray")))
  "Face for marking up U's"
  :group 'base-faces)

(defface cons-face-a
  `((((type tty) (class color))
     (:background "cyan" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "cyan" :foreground "black"))
    (((class color) (background light))
     (:background "cyan" :foreground "black"))
    (t (:background "gray")))
  "Conservation markup face A"
  :group 'cons-faces)

(defface cons-face-b
  `((((type tty) (class color))
     (:background "skyblue" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "skyblue" :foreground "black"))
    (((class color) (background light))
     (:background "skyblue" :foreground "black"))
    (t (:background "gray")))
  "Conservation markup face B"
  :group 'cons-faces)

(defface cons-face-c
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "gray" :foreground "black"))
    (((class color) (background light))
     (:background "gray" :foreground "black"))
    (t (:background "gray")))
  "Conservation markup face C"
  :group 'cons-faces)

(defface comp-face-a
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "green" :foreground "black"))
    (((class color) (background light))
     (:background "green" :foreground "black"))
    (t (:background "gray")))
  "Compensation markup face A"
  :group 'comp-faces)

(defface comp-face-b
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "lightgreen" :foreground "black"))
    (((class color) (background light))
     (:background "lightgreen" :foreground "black"))
    (t (:background "gray")))
  "Compensation markup face B"
  :group 'comp-faces)

(defface comp-face-c
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "red" :foreground "black"))
    (((class color) (background light))
     (:background "red" :foreground "black"))
    (t (:background "gray")))
  "Compensation markup face C"
  :group 'comp-faces)

(defface comp-face-d
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "orange" :foreground "black"))
    (((class color) (background light))
     (:background "orange" :foreground "black"))
    (t (:background "gray")))
  "Compensation markup face D"
  :group 'comp-faces)

(defface comp-face-e
  `((((type tty) (class color))
     (:background "gray" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "magenta" :foreground "black"))
    (((class color) (background light))
     (:background "magenta" :foreground "black"))
    (t (:background "gray")))
  "Compensation markup face E"
  :group 'comp-faces)


(defface highlight-face
  `((((type tty) (class color))
     (:background "black" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "white" :foreground "black"))
    (((class color) (background light))
     (:background "black" :foreground "white"))
    (t (:background "gray")))
  "Highlight face"
  :group 'highlight-faces)



(copy-face 'structure-face-a 'structure-face-a-bold)
(copy-face 'structure-face-b 'structure-face-b-bold)
(copy-face 'structure-face-c 'structure-face-c-bold)
(copy-face 'structure-face-d 'structure-face-d-bold)
(copy-face 'structure-face-e 'structure-face-e-bold)
(copy-face 'structure-face-f 'structure-face-f-bold)
(copy-face 'structure-face-g 'structure-face-g-bold)
(copy-face 'structure-face-h 'structure-face-h-bold)
(copy-face 'structure-face-i 'structure-face-i-bold)
(copy-face 'base-face-a 'base-face-a-bold)
(copy-face 'base-face-c 'base-face-c-bold)
(copy-face 'base-face-g 'base-face-g-bold)
(copy-face 'base-face-u 'base-face-u-bold)
(copy-face 'cons-face-a 'cons-face-a-bold)
(copy-face 'cons-face-b 'cons-face-b-bold)
(copy-face 'cons-face-c 'cons-face-c-bold)
(copy-face 'comp-face-a 'comp-face-a-bold)
(copy-face 'comp-face-b 'comp-face-b-bold)
(copy-face 'comp-face-c 'comp-face-c-bold)
(copy-face 'comp-face-d 'comp-face-d-bold)
(copy-face 'comp-face-e 'comp-face-e-bold)
(copy-face 'highlight-face 'highlight-face-bold)


(if (featurep 'xemacs)
    (progn
      (set-face-property 'structure-face-a-bold 'highlight nil)
      (set-face-property 'structure-face-b-bold 'highlight nil)
      (set-face-property 'structure-face-c-bold 'highlight nil)
      (set-face-property 'structure-face-d-bold 'highlight nil)
      (set-face-property 'structure-face-e-bold 'highlight nil)
      (set-face-property 'structure-face-f-bold 'highlight nil)
      (set-face-property 'structure-face-g-bold 'highlight nil)
      (set-face-property 'structure-face-h-bold 'highlight nil)
      (set-face-property 'structure-face-i-bold 'highlight nil)
      (set-face-property 'base-face-a-bold 'highlight nil)
      (set-face-property 'base-face-c-bold 'highlight nil)
      (set-face-property 'base-face-g-bold 'highlight nil)
      (set-face-property 'base-face-u-bold 'highlight nil)
      (set-face-property 'cons-face-a-bold 'highlight nil)
      (set-face-property 'cons-face-b-bold 'highlight nil)
      (set-face-property 'cons-face-c-bold 'highlight nil)
      (set-face-property 'comp-face-a-bold 'highlight nil)
      (set-face-property 'comp-face-b-bold 'highlight nil)
      (set-face-property 'comp-face-c-bold 'highlight nil)
      (set-face-property 'comp-face-d-bold 'highlight nil)
      (set-face-property 'comp-face-e-bold 'highlight nil)
      (set-face-property 'highlight-face-bold 'highlight nil)
      )

  (set-face-attribute 'structure-face-a-bold nil :bold "true")
  (set-face-attribute 'structure-face-b-bold nil :bold "true")
  (set-face-attribute 'structure-face-c-bold nil :bold "true")
  (set-face-attribute 'structure-face-d-bold nil :bold "true")
  (set-face-attribute 'structure-face-e-bold nil :bold "true")
  (set-face-attribute 'structure-face-f-bold nil :bold "true")
  (set-face-attribute 'structure-face-g-bold nil :bold "true")
  (set-face-attribute 'structure-face-h-bold nil :bold "true")
  (set-face-attribute 'structure-face-i-bold nil :bold "true")
  (set-face-attribute 'base-face-a-bold nil :bold "true")
  (set-face-attribute 'base-face-c-bold nil :bold "true")
  (set-face-attribute 'base-face-g-bold nil :bold "true")
  (set-face-attribute 'base-face-u-bold nil :bold "true")
  (set-face-attribute 'cons-face-a-bold nil :bold "true")
  (set-face-attribute 'cons-face-b-bold nil :bold "true")
  (set-face-attribute 'cons-face-c-bold nil :bold "true")
  (set-face-attribute 'comp-face-a-bold nil :bold "true")
  (set-face-attribute 'comp-face-b-bold nil :bold "true")
  (set-face-attribute 'comp-face-c-bold nil :bold "true")
  (set-face-attribute 'comp-face-d-bold nil :bold "true")
  (set-face-attribute 'comp-face-e-bold nil :bold "true")
  (set-face-attribute 'highlight-face-bold nil :bold "true")
  )

(setq structure-faces (list
		       (cons 'structure-face-a 'structure-face-a-bold)
		       (cons 'structure-face-b 'structure-face-b-bold)
		       (cons 'structure-face-c 'structure-face-c-bold)
		       (cons 'structure-face-d 'structure-face-d-bold)
		       (cons 'structure-face-e 'structure-face-e-bold)
		       (cons 'structure-face-f 'structure-face-f-bold)
		       (cons 'structure-face-g 'structure-face-g-bold)
		       (cons 'structure-face-h 'structure-face-h-bold)
		       (cons 'structure-face-i 'structure-face-i-bold)
		       ))

(setq base-faces (list
		  (cons 'base-face-a 'base-face-a-bold)
		  (cons 'base-face-c 'base-face-c-bold)
		  (cons 'base-face-g 'base-face-g-bold)
		  (cons 'base-face-u 'base-face-u-bold)
		  ))

(setq cons-faces (list
		  (cons 'cons-face-a 'cons-face-a-bold)
		  (cons 'cons-face-b 'cons-face-b-bold)
		  (cons 'cons-face-c 'cons-face-c-bold)
		  ))

(setq comp-faces (list
		  (cons 'comp-face-a 'comp-face-a-bold)
		  (cons 'comp-face-b 'comp-face-b-bold)
		  (cons 'comp-face-c 'comp-face-c-bold)
		  (cons 'comp-face-d 'comp-face-d-bold)
		  (cons 'comp-face-e 'comp-face-e-bold)
		  ))

(setq ralee-faces structure-faces)
(nconc ralee-faces base-faces)
(nconc ralee-faces cons-faces)
(nconc ralee-faces comp-faces)

(provide 'ralee-faces)


