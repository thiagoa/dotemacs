(require 'slime-autoloads)

(setq inferior-lisp-program (shell-command-output "which sbcl"))
(slime-setup '(slime-fancy))

(add-hook 'slime-mode-hook 'enable-paredit-mode)
(add-hook 'slime-mode-hook 'safe-linum-mode)
(add-hook 'slime-mode-hook 'aggressive-indent-mode)
(add-hook 'slime-mode-hook 'rainbow-delimiters-mode)
