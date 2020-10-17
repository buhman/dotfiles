(setq custom-file "/dev/null")

(let ((dir (file-name-as-directory "~/.emacs.d/buhman")))
  (mapc
   (lambda (fn)
     (load-file (concat dir fn)))
   (sort (directory-files dir nil "\\.el$") 'string-lessp)))
