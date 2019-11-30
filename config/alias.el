;;; alias.el --- Function aliases
;;
;;; Commentary:

;;; Code:

(defalias 'ff     'find-file)
(defalias 'bb     'bury-buffer)
(defalias 'kab    'kill-all-buffers)
(defalias 'e      'eval-buffer)
(defalias 'keb    'kill-extraneous-buffers)
(defalias 'repl   'ielm)
(defalias 'bundle 'bundle-install)
(defalias 'repl   'ielm)
(defalias 'quit   'save-buffers-kill-terminal)

(provide 'alias)
;;; alias.el ends here
