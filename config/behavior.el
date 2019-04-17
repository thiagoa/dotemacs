;;; behavior.el --- Behavior config
;;
;;; Commentary:
;;
;; Behavior config, all kinds of settings

;;; Code:

(require 'god-mode)
(require 'god-mode-isearch)
(require 'dired-x)
(require 'projectile)
(require 'helm)
(require 'helm-buffers)
(require 'helm-for-files)
(require 'ido)
(require 'flycheck)
(require 'magit)
(require 'savehist)
(require 'recentf)
(require 'undo-tree)
(require 'config-base)

(run-server)
(emacs-use-same-path-as-shell)
(beginend-global-mode)
(global-dot-mode)
(global-flycheck-mode)
(yas-global-mode)
(projectile-mode)
(helm-projectile-on)
(winner-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)
(save-place-mode)
(god-mode-all)
(global-undo-tree-mode)

(buffer-trail-advise '(helm-buffers-list
                       helm-find-files
                       helm-recentf
                       helm-projectile-find-file))

(add-helm-projectile-projects-action
 '(("Rails console"        "M-r" execute-projectile-rails-console-under-dir)
   ("Find file in project" "C-f" execute-helm-projectile-find-file-under-dir)))

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(when window-system (global-unset-key "\C-z"))
(setq-default indent-tabs-mode nil)
(setq ido-auto-merge-work-directories-length -1) ;; Avoid ido-find-file jumps
(setq mac-right-command-modifier 'meta)
(setq flycheck-check-syntax-automatically '(save mode-enable))
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-split-window-default-side 'other)
(setq undo-tree-enable-undo-in-region nil)
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
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))

(add-hook 'god-mode-enabled-hook (lambda ()
                                   (when (eq major-mode 'markdown-mode)
                                     (toggle-option-key 'meta))))
(add-hook 'god-mode-disabled-hook (lambda ()
                                    (when (eq major-mode 'markdown-mode)
                                      (toggle-option-key 'none))))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-vc-set-filter-groups-by-vc-root)))
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(add-hook 'before-save-hook 'whitespace-cleanup)

(load-history savehist-file)

(provide 'behavior)
;;; behavior.el ends here
