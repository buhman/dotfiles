;; org

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for ditaa
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)

(add-hook 'org-mode-hook 'org-indent-mode)

(setq org-html-validation-link nil)

;; elm

(setq elm-format-on-save t)

;; comint

(setq comint-prompt-read-only t)

(defun my-comint-preoutput-read-only (text)
  (propertize text 'read-only t))

(add-hook 'comint-preoutput-filter-functions
          'my-comint-preoutput-read-only)

;; erlang

(setq erlang-electric-commands '(erlang-electric-comma erlang-electric-semicolon erlang-electric-newline))

;; c

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local c-basic-offset 2)
            (setq-local c-default-style "gnu")
            (setq-local tab-width 2)
            (setq-local indent-tabs-mode nil)
            (setq-local comment-style 'multi-line)
            (setq-local comment-style 'extra-line)))

;; magit

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch)

;; misc

(setq vc-follow-symlinks t)


(provide 'buhman-misc)
