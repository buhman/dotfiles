;; visual configuration

(load-theme 'zenburn t)

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode 0)

(defun disable-scroll-bars (frame)
  (modify-frame-parameters frame
                           '((vertical-scroll-bars . nil)
                             (horizontal-scroll-bars . nil))))

(add-hook 'after-make-frame-functions 'disable-scroll-bars)

;; editing configuration

(setq-default fill-column 80)
(setq-default require-final-newline t)
(setq echo-keystrokes 0.5)
(setq blink-matching-paren nil)

;; whitespace

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

(require 'whitespace)
(setq whitespace-style '(tabs newline tab-mark))

;; scratch

(setq initial-scratch-message "")
(setq initial-major-mode 'emacs-lisp-mode)

;;

(fset 'yes-or-no-p 'y-or-n-p)

;; backup files

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; copy-paste

(defun get-primary ()
  (interactive)
  (insert
   (gui-get-primary-selection)))
(global-set-key "\C-c\C-y" 'get-primary)

;; guru

(guru-global-mode 't)

;; font

(set-face-attribute 'default nil :font "Dejavu Sans Mono-12")
(setq default-frame-alist '((font . "Dejavu Sans Mono-12")))


(provide 'buhman-common)
