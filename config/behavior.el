(load "dired-x.el")

(run-server)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(global-dot-mode)
(global-flycheck-mode)
(global-discover-mode)
(global-undo-tree-mode)
(yas-global-mode)
(projectile-global-mode)
(counsel-projectile-mode)
(winner-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)
(save-place-mode)
(ido-mode)

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

(setq-default indent-tabs-mode nil)

;; Sets default action to magit (this is the index of the action)
;; This is fragile and should be improved later
(setcar counsel-projectile-switch-project-action 12)

(defun execute-extended-command-under-dir (dir)
  (execute-command-under-dir dir 'execute-extended-command))

(setq-list-append counsel-projectile-switch-project-action
                  ("xx" execute-extended-command-under-dir "execute extended command"))

(defun execute-projectile-rails-console-under-dir (dir)
  (execute-command-under-dir dir 'projectile-rails-console nil))

(setq-list-append counsel-projectile-switch-project-action
                  ("pr" execute-projectile-rails-console-under-dir "projectile rails"))

(setq tags-add-tables nil)
(setq magit-no-confirm '(unstage-all-changes))
(setq vc-follow-symlinks t)
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
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(setq comint-move-point-for-output nil)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-show-maximum-output t)
(setq savehist-file (expand-file-name "tmp/savehist" emacs-d))
(setq history-length 1000)
(setq ivy-use-virtual-buffers t)
(setq ivy-re-builders-alist
      '((swiper . regexp-quote)
        (counsel-M-x . ivy--regex-plus)
        (t      . ivy--regex-fuzzy)))
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

(magithub-feature-autoinject t)

(load-history savehist-file)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'before-save-hook 'whitespace-cleanup)
