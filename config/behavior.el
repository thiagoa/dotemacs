(require 'god-mode)
(require 'god-mode-isearch)
(require 'dired-x)
(require 'projectile)
(require 'helm)
(require 'eshell)

(run-server)

(emacs-use-same-path-as-shell)
(config-terminal-encoding)
(set-default-shell "zsh")

(global-set-key (kbd "<escape>") 'god-mode-all)

(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

(beginend-global-mode)
(global-dot-mode)
(global-flycheck-mode)
(global-discover-mode)
(global-undo-tree-mode)
(yas-global-mode)
(projectile-global-mode)
(helm-projectile-on)
(winner-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)
(save-place-mode)
(ido-mode)
(god-mode-all)

(setq flycheck-check-syntax-automatically '(save mode-enable))

;; Avoids ido-find-file jumping to directories automatically after
;; some time
(setq ido-auto-merge-work-directories-length -1)

(when window-system (global-unset-key "\C-z")) ; ugh

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)
(put 'set-goal-column 'disabled nil)

(buffer-trail-advise '(helm-buffers-list
                       helm-find-files
                       helm-recentf
                       helm-projectile-find-file))

(setq-default indent-tabs-mode nil)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-helm-projectile-projects-action
 '(("Rails console"        "M-r" execute-projectile-rails-console-under-dir)
   ("Find file in project" "C-f" execute-helm-projectile-find-file-under-dir)))

(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-split-window-default-side 'other)
(setq undo-tree-enable-undo-in-region nil)

;; I hope I don't regret this
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)

(setq auto-mode-alist (cons '("[^/]\\.dired$" . dired-virtual-mode) auto-mode-alist))
(setq projectile-indexing-method 'turbo-alien)
(setq projectile-switch-project-action 'magit)
(setq projectile-enable-caching t)
(setq save-interprogram-paste-before-kill t)
(setq tags-add-tables nil)
(setq magit-no-confirm '(unstage-all-changes))
(setq vc-follow-symlinks t)
(setq recentf-max-saved-items 2000)
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
        try-expand-all-abbrevs
        try-expand-list
        try-expand-dabbrev-visible
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name))
(setq dired-dwim-target t)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(setq comint-move-point-for-output nil)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-show-maximum-output t)
(setq savehist-file (expand-file-name "tmp/savehist" emacs-d))
(setq history-length 1000)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq large-file-warning-threshold nil)

(load-history savehist-file)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-vc-set-filter-groups-by-vc-root)))

(add-hook 'before-save-hook 'whitespace-cleanup)
