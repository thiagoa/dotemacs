(require 'ido-vertical-mode)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(ivy-mode)
(electric-indent-mode +1)
(projectile-global-mode)
(global-discover-mode 1)
(global-undo-tree-mode 1)
(winner-mode 1)
(ido-vertical-mode)

(defvar newline-and-indent t)
(setq projectile-enable-caching t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq ns-right-alternate-modifier nil)
(setq system-uses-terminfo nil)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)
(setq compilation-scroll-output 'first-error)
(setq hippie-expand-try-functions-list
      '(try-expand-line
	try-expand-line-all-buffers
	try-complete-file-name
	try-complete-lisp-symbol))
(setq ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-use-faces t)
(setq dired-dwim-target t)
(setq ivy-use-virtual-buffers t)
(setq ivy-re-builders-alist
      '((swiper . regexp-quote)
	(t      . ivy--regex-fuzzy)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'ido-setup-hook
	  '(lambda ()
	     (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
	     (define-key ido-completion-map "\C-n" 'ido-next-match)
	     (define-key ido-completion-map "\C-p" 'ido-prev-match)
	     (define-key ido-completion-map " " 'ido-exit-minibuffer)))
