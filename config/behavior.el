(require 'ido-vertical-mode)
(require 'yasnippet)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(define-globalized-minor-mode global-missile-mode missile-mode
  (lambda () (missile-mode 1)))

(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")

(yas-global-mode 1)
(electric-indent-mode +1)
(projectile-global-mode)
(global-discover-mode)
(global-undo-tree-mode)
(winner-mode)
(ido-vertical-mode)
(counsel-projectile-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)

(defvar newline-and-indent t)
(setq recentf-max-saved-items 1000)
(setq projectile-enable-caching t)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
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
(setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(setq ivy-use-virtual-buffers t)
(setq savehist-file "~/.emacs.d/tmp/savehist")
(setq ivy-re-builders-alist
      '((swiper . regexp-quote)
	(counsel-M-x . ivy--regex-plus)
	(t      . ivy--regex-fuzzy)))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'after-init-hook 'inf-ruby-switch-setup)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'ido-setup-hook
	  '(lambda ()
	     (define-key ido-completion-map "\C-h" 'ido-delete-backward-updir)
	     (define-key ido-completion-map "\C-n" 'ido-next-match)
	     (define-key ido-completion-map "\C-p" 'ido-prev-match)
	     (define-key ido-completion-map " " 'ido-exit-minibuffer)))
