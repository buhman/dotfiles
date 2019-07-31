;; visual configuration

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode 0)

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

;; package configuration

(require 'package)
(package-initialize)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; use packages

(use-package zenburn-theme)
(use-package magit)
(use-package org)
(use-package htmlize)

;; modes

(use-package haskell-mode)
(use-package yaml-mode)
(use-package json-mode)
(use-package graphviz-dot-mode)
(use-package dockerfile-mode)
(use-package elm-mode)
(use-package guru-mode)
(use-package meson-mode)
(use-package nix-mode)
(use-package pkgbuild-mode)
(use-package rust-mode)
(use-package ninja-mode)
(use-package nginx-mode)
(use-package cython-mode)
(use-package elixir-mode)
(use-package glsl-mode)
(use-package erlang)

;; lua
(use-package lua-mode)
(use-package moonscript)
(use-package fennel-mode)

;; lisp

(use-package rainbow-delimiters)
(use-package slime)
(use-package parinfer)
(use-package paredit)
(use-package geiser)

(setq inferior-lisp-program "/usr/bin/sbcl")

(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl" "--core" "/usr/local/share/sbcl.core-for-slime"))))

(add-to-list 'load-path "~/slime")
(require 'slime-autoloads)
(setq slime-contribs '(slime-fancy))

;; copy-paste

(defun get-primary ()
  (interactive)
  (insert
   (gui-get-primary-selection)))
(global-set-key "\C-c\C-y" 'get-primary)

;; org
(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-html-validation-link nil)

;; guru

(guru-global-mode 't)

;; elm

(setq elm-format-on-save 't)

;; parinfer

(setq parinfer-extensions
      '(defaults
         ;pretty-parens
         smart-yank
         smart-tab
         paredit))

;; comint

(setq comint-prompt-read-only t)

(defun my-comint-preoutput-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions
          'my-comint-preoutput-read-only)

;; scheme: run

(require 'gambit)
(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)

(defvar gerbil-program-name
  (expand-file-name "~/gerbil-0.15.1/bin/gxi")) ; Set this for your GERBIL_HOME
(setq scheme-program-name (concat gerbil-program-name " --lang r7rs"))

(setq scheme-program-name "csi -I /home/buhman/route-mux")
;;(add-to-list 'interpreter-mode-alist '("chicken-scheme" . scheme-mode))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme))

;; scheme: indents

(put 'match 'scheme-indent-function 1)
(put 'match-let 'scheme-indent-function 1)
(put 'match-let* 'scheme-indent-function 1)
(put 'when 'scheme-indent-function 1)
(put 'and-let* 'scheme-indent-function 1)
(put 'if-let 'scheme-indent-function 1)
(put 'let-location 'scheme-indent-function 1)
(put 'select 'scheme-indent-function 1)
(put 'bitmatch 'scheme-indent-function 1)
(put 'bitpacket 'scheme-indent-function 1)
(put 'with-transaction 'scheme-indent-function 1)
(put 'sequence 'scheme-indent-function 1)
(put 'sequence* 'scheme-indent-function 1)
(put 'if 'scheme-indent-function 1)
(put 'foreign-lambda* 'scheme-indent-function 2)
(put 'foreign-lambda 'scheme-indent-function 2)
(put 'handle-exceptions 'scheme-indent-function 2)
(put 'set! 'scheme-indent-function 1)
(put '-> 'scheme-indent-function 1)
(put '->> 'scheme-indent-function 1)
(put 'test 'scheme-indent-function 1)
(put 'test-values 'scheme-indent-function 1)
(put 'test-assert 'scheme-indent-function 1)
(put 'test-parameterize 'scheme-indent-function 1)
(put 'test-group 'scheme-indent-function 1)
(put 'module 'scheme-indent-function 1)
(put 'let/cc 'scheme-indent-function 1)

;; scheme: modes

(remove-hook 'scheme-mode-hook #'geiser-mode--maybe-activate)
(add-hook 'scheme-mode-hook #'rainbow-delimiters-mode)
(add-hook 'scheme-mode-hook #'parinfer-mode)

(setq geiser-active-implementations '(chicken))

;; lisp

(global-prettify-symbols-mode 1)

(add-hook 'lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook #'parinfer-mode)
(add-hook 'lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'lisp-indent-function))
            'common-lisp-indent-function))

(eval-after-load 'cl-indent
  `(progn
     (put 'with 'common-lisp-indent-function 1)
     (put 'if 'common-lisp-indent-function 1)
     (put 'define 'common-lisp-indent-function 1)
     (put 'wml/tag 'common-lisp-indent-function 1)
     (put 'wml/act 'common-lisp-indent-function 1)))

(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

;; elisp

(add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook #'parinfer-mode)

;; wml

(add-to-list 'load-path "/home/buhman/.emacs.d/wesnoth-mode")
(autoload 'wesnoth-mode "wesnoth-mode" "Major mode for editing WML." t)
;;(add-to-list 'auto-mode-alist '("\\.cfg\\'" . wesnoth-mode))

;; gerbil

(autoload 'gerbil-mode "gerbil" "Gerbil editing mode." t)
(require 'gambit)
(add-hook 'inferior-scheme-mode-hook 'gambit-inferior-mode)

;; c

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local c-basic-offset 2)
            (setq-local c-default-style "gnu")
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode f)
            (setq-local comment-style 'multi-line)
            (setq-local comment-style 'extra-line)))

;; variables

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (glsl-mode kconfig-mode elixir-mode cython-mode nginx-mode bnf-mode paredit parinfer clojure-mode rainbow-delimiters fennel-mode moonscript lua-mode ninja-mode htmlize rust-mode pkgbuild-mode nix-mode meson-mode elm-mode guru-mode dockerfile-mode graphviz-dot-mode json-mode yaml-mode org-plus-contrib magit zenburn-theme haskell-mode use-package)))
 '(safe-local-variable-values
   (quote
    ((eval modify-syntax-entry 43 "'")
     (eval modify-syntax-entry 36 "'")
     (eval modify-syntax-entry 126 "'")))))

;; font

(set-face-attribute 'default nil :font "Dejavu Sans Mono-10")
(setq default-frame-alist '((font . "Dejavu Sans Mono-10")))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
