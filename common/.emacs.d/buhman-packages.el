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

(use-package tuareg)
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


(provide 'buhman-packages)
