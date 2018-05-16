(require 'helm-files)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(electric-indent-mode +1)
(projectile-global-mode)
(global-discover-mode 1)
(global-undo-tree-mode 1)
(winner-mode 1) ; Revert panes with C-c (left or right)

(defvar newline-and-indent t)

(setq projectile-enable-caching t) ;; "C-u C-c p f" to force reload.
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq ns-right-alternate-modifier nil) ; Leave right option key for Mac
(setq system-uses-terminfo nil)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq hippie-expand-try-functions-list
      '(try-expand-line
	try-expand-line-all-buffers
	try-complete-file-name
	try-complete-lisp-symbol))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
