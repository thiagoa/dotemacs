(load "dired-x.el")

(run-server)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(global-discover-mode)
(global-undo-tree-mode)
(yas-global-mode)
(projectile-global-mode)
(counsel-projectile-mode)
(winner-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)

(setq-default indent-tabs-mode nil)

(setq-list-append counsel-projectile-switch-project-action
                  ("xx" execute-extended-command-under-dir "execute extended command"))

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
        (t      . regexp-quote)))

;; Why is history not being loaded? :thinking:
(load-file savehist-file)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(add-hook 'before-save-hook 'whitespace-cleanup)
