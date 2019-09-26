;;; elisp.el --- Elisp config
;;
;;; Commentary:

;;; Code:

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'safe-linum-mode)
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'smartscan-mode)

(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))

;;; elisp.el ends here
