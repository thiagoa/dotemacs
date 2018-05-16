(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'linum-mode)
(add-hook 'emacs-lisp-mode-hook 'aggressive-indent-mode)

(add-to-list 'auto-mode-alist '("Cask$" . emacs-lisp-mode))
