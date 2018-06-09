(require 'yasnippet)
(require 'multiple-cursors)

(run-server)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(define-globalized-minor-mode global-missile-mode missile-mode
  (lambda () (missile-mode 1)))

(yas-global-mode 1)
(electric-indent-mode +1)
(projectile-global-mode)
(global-discover-mode)
(global-undo-tree-mode)
(winner-mode)
(counsel-projectile-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)

(defvar newline-and-indent t)

(setq-default indent-tabs-mode nil)

(setq recentf-max-saved-items 2000)
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
(setq dired-dwim-target t)
(setq find-ls-option '("-print0 | xargs -0 ls -alhd" . ""))
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(setq ivy-use-virtual-buffers t)
(setq savehist-file "~/.emacs.d/tmp/savehist")
(setq history-length 1000)
(setq ivy-re-builders-alist
      '((swiper . regexp-quote)
        (counsel-M-x . ivy--regex-plus)
        (t      . regexp-quote)))

;; Why is history not being loaded? :thinking:
(load-file savehist-file)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'before-save-hook 'whitespace-cleanup)

(custom-set-variables
 '(comint-scroll-to-bottom-on-input t)
 '(comint-scroll-to-bottom-on-output nil)
 '(comint-scroll-show-maximum-output t))

(defvar counsel-projectile-switch-project-action
  (append counsel-projectile-switch-project-action '(("xx" execute-extend-command-under-dir))))
