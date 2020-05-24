(add-hook
 'c-mode-common-hook
 (lambda ()
   (setq c-default-style "bsd"
         c-basic-offset 2)))

(provide 'buhman-c)
