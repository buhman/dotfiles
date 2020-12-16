;; scheme: run

(require 'scheme)
(require 'comint)
(require 'cmuscheme)

(defun scheme-send-buffer ()
  (interactive)
  (comint-send-region (scheme-proc) (point-min) (point-max)))

(defun run-scheme ()
  (interactive)
  (if (not (comint-check-proc "*scheme*"))
      (progn
        (setenv "GERBIL_HOME" (expand-file-name "~/gerbil"))
        (set-buffer (make-comint "scheme"
                                 "~/gerbil/bin/gxi"
                                 "~/.emacs.d/scheme/init-gerbil.scm"
                                 "--lang" "r7rs"))
        (setq scheme-buffer "*scheme*")
        (inferior-scheme-mode)))
  (pop-to-buffer-same-window "*scheme*"))

(defun scheme-get-process ()
  "Return the current Scheme process or nil if none is running."
  (get-buffer-process "*scheme*"))

(with-eval-after-load 'scheme
  (define-key scheme-mode-map (kbd "C-c C-z") 'run-scheme)
  (define-key scheme-mode-map (kbd "C-c C-b") 'scheme-send-buffer))

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
            (set (make-local-variable 'lisp-indent-function)
                 'common-lisp-indent-function)))

(eval-after-load 'cl-indent
  `(progn
     (put 'with 'common-lisp-indent-function 1)
     (put 'if 'common-lisp-indent-function 1)
     (put 'define 'common-lisp-indent-function 1)))

(modify-syntax-entry ?\{ "(}" lisp-mode-syntax-table)
(modify-syntax-entry ?\} "){" lisp-mode-syntax-table)
(modify-syntax-entry ?\[ "(]" lisp-mode-syntax-table)
(modify-syntax-entry ?\] ")[" lisp-mode-syntax-table)

(let ((hooks '(emacs-lisp-mode-hook
               eval-expression-minibuffer-setup-hook
               ielm-mode-hook
               lisp-mode-hook
               lisp-interaction-mode-hook
               scheme-mode-hook
               inferior-scheme-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook 'enable-paredit-mode)
    (add-hook hook 'rainbow-delimiters-mode)))

;; parinfer

(setq parinfer-extensions
      '(defaults
         ;pretty-parens
         smart-yank
         smart-tab
         paredit))

;; sbcl
; (load "slime/swank-loader.lisp")
; (load "quicklisp/setup.lisp")
; (swank-loader:dump-image "sbcl.core-with-swank")

(setq slime-lisp-implementations
      `((sbcl ("sbcl" "--core" ,(substitute-in-file-name "$HOME/sbcl.core-with-swank"))
              :init (lambda (port-file _)
                      (format "(swank:start-server %S)\n" port-file)))))

(provide 'buhman-lisp)
