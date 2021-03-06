(require 'haskell-interactive-mode)
(require 'haskell-process)

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
    (backward-char 2)
    (insert ") where")
    (haskell-indentation-newline-and-indent)))

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-stylish-on-save t)
(setq haskell-process-show-debug-tips nil)
(setq haskell-process-log t)

(define-key haskell-interactive-mode-map (kbd "M-.") 'haskell-mode-goto-loc)
(define-key haskell-interactive-mode-map (kbd "C-c C-t") 'haskell-mode-show-type-at)

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
(define-key haskell-mode-map (kbd "C-c t m") 'buhman/haskell-mode-insert-module)
(define-key haskell-mode-map (kbd "C-c l") 'hs-lint)

(define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)


(provide 'buhman-haskell)
