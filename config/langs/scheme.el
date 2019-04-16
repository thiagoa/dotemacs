;;; scheme.el --- Elisp config
;;
;;; Commentary:

;;; Code:

(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'safe-linum-mode)
(add-hook 'scheme-mode-hook 'aggressive-indent-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode)

(provide 'scheme)
;;; scheme.el ends here
