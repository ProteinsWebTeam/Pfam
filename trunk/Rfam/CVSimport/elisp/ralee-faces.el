;;; ralee-faces.el


; complex definition of a face
; try a simpler one
(defface ralee-face-a
  `((((type tty) (class color))
     (:background "skyblue" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "skyblue" :foreground "black"))
    (((class color) (background light))
     (:background "skyblue" :foreground "black"))
    (t (:background "gray")))
  "Highlighting face a"
  :group 'basic-faces)

(defface ralee-face-b
  `((((type tty) (class color))
     (:background "lightgreen" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "lightgreen" :foreground "black"))
    (((class color) (background light))
     (:background "lightgreen" :foreground "black"))
    (t (:background "gray")))
  "Highlighting face b"
  :group 'basic-faces)

(defface ralee-face-c
  `((((type tty) (class color))
     (:background "pink" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "pink" :foreground "black"))
    (((class color) (background light))
     (:background "pink" :foreground "black"))
    (t (:background "gray")))
  "Highlighting face c"
  :group 'basic-faces)

(defface ralee-face-d
  `((((type tty) (class color))
     (:background "white" :foreground "black"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "white" :foreground "black"))
    (((class color) (background light))
     (:background "white" :foreground "black"))
    (t (:background "gray")))
  "Highlighting face d"
  :group 'basic-faces)

(defface ralee-face-e
  `((((type tty) (class color))
     (:background "red" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "red" :foreground "white"))
    (((class color) (background light))
     (:background "red" :foreground "white"))
    (t (:background "gray")))
  "Highlighting face e"
  :group 'basic-faces)

(defface ralee-face-f
  `((((type tty) (class color))
     (:background "blue" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "blue" :foreground "white"))
    (((class color) (background light))
     (:background "blue" :foreground "white"))
    (t (:background "gray")))
  "Highlighting face f"
  :group 'basic-faces)

(defface ralee-face-g
  `((((type tty) (class color))
     (:background "green" :foreground "white"))
    (((type tty) (class mono))
     (:inverse-video t))
    (((class color) (background dark))
     (:background "green" :foreground "white"))
    (((class color) (background light))
     (:background "green" :foreground "white"))
    (t (:background "gray")))
  "Highlighting face g"
  :group 'basic-faces)


(setq ralee-faces '(ralee-face-a 
		    ralee-face-b 
		    ralee-face-c
		    ralee-face-d
		    ralee-face-e
		    ralee-face-f
		    ralee-face-g
		    ))


(provide 'ralee-faces)