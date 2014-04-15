;; .emacs

;;; uncomment this line to disable loading of "default.el" at startup
;; (setq inhibit-default-init t)

;; turn on font-lock mode
(when (fboundp 'global-font-lock-mode)
  (global-font-lock-mode t))

;; enable visual feedback on selections
;(setq transient-mark-mode t)

;; default to better frame titles
(setq frame-title-format
      (concat  "%b - emacs@" (system-name)))

;; default to unified diffs
(setq diff-switches "-u")

;; always end a file with a newline
;(setq require-final-newline 'query)

;-------
(add-to-list 'load-path "~/site-lisp")
(add-to-list 'load-path "/homes/swb/ralee-0.7/elisp")
;-------
(add-to-list 'load-path "~/PubMode-0.1/lisp")
(autoload 'pub-med "pub" "PubMed Interface for Emacs" t)
(global-set-key (kbd "C-c p") 'pub-med)
;-------
;; ralee mode is good for RNA alignment editing
(autoload 'ralee-mode "ralee-mode" "Yay! RNA things" t)
(setq auto-mode-alist (cons '("\\.stk$" . ralee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.sto$" . ralee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SEED$" . ralee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("SEED.new$" . ralee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("ALIGN$" . ralee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("align$" . ralee-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("repalign$" . ralee-mode) auto-mode-alist))
(normal-erase-is-backspace-mode 1) 
;-------
