(when (require 'sql-upcase nil :noerror)
  (add-hook 'sql-mode-hook 'sql-upcase-mode)
  (add-hook 'sql-interactive-mode-hook 'sql-upcase-mode))

(setq sql-upcase-mixed-case t)

(add-hook 'sql-mode-hook 'sqlind-minor-mode)

(provide 'buhman-sql)
