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

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)

(let ((hooks '(emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               scheme-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook 'enable-paredit-mode)
    (add-hook hook 'rainbow-delimiters-mode)))

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



;; lisp

(global-prettify-symbols-mode 1)

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

;; erlang

(setq erlang-electric-commands '(erlang-electric-comma erlang-electric-semicolon erlang-electric-newline))

;; c

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local c-basic-offset 2)
            (setq-local c-default-style "gnu")
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode f)
            (setq-local comment-style 'multi-line)
            (setq-local comment-style 'extra-line)))

;; haskell

(defun buhman/haskell-mode-insert-language-extension ()
  (interactive)
  (insert "{-# LANGUAGE  #-}")
  (forward-char -4)
  t)

(defun buhman/haskell-mode-insert-module ()
  (interactive)
  (let ((module-name (car (split-string (buffer-name) "\\."))))
    (insert "module " module-name)
    (haskell-indentation-newline-and-indent)
    (insert "( ")
    (haskell-indentation-newline-and-indent)
    (delete-backward-char 2)
    (insert ") where")
    (haskell-indentation-newline-and-indent)))

(require 'haskell-interactive-mode)
(require 'haskell-process)

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-stylish-on-save t)
(setq haskell-process-show-debug-tips nil)
(setq haskell-process-log t)

(define-key interactive-haskell-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key interactive-haskell-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-c i") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "C-x a a") 'align-entire)
(define-key haskell-mode-map (kbd "C-x a r") 'align-regexp)
(define-key haskell-mode-map (kbd "C-c t l") 'buhman/haskell-mode-insert-language-extension)
(define-key haskell-mode-map (kbd "C-c t m") 'buhman/haskell-mode-insert-language-extension)

(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; font

(set-face-attribute 'default nil :font "Dejavu Sans Mono-10")
(setq default-frame-alist '((font . "Dejavu Sans Mono-10")))

;; magit

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)
