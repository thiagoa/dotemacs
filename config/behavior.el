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

;; Global modes
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
(god-mode)
(global-undo-tree-mode)

;; Keep track of buffers on global mark
(buffer-trail-advise '(helm-buffers-list
                       helm-find-files
                       helm-recentf
                       helm-projectile-find-file
                       xref-find-definitions))

;; Helm
(setq helm--url-regexp "WILL-NEVER-EVER-MATCH-I-DONT-WANT-THIS-FUNCTIONALITY")
(setq helm-buffers-fuzzy-matching t)
(setq helm-recentf-fuzzy-match t)
(setq helm-split-window-default-side 'other)
(add-helm-projectile-projects-action
 '(("Rails console"        "M-r" execute-projectile-rails-console-under-dir)
   ("Find file in project" "C-f" execute-helm-projectile-find-file-under-dir)))

;; Paredit
(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;; Enable / tune up Emacs features
(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)
(setq eval-expression-print-level nil)
(setq eval-expression-print-length nil)
(setq mac-right-command-modifier 'meta)
(setq save-interprogram-paste-before-kill t)
(setq vc-follow-symlinks t)
(setq recentf-max-saved-items 2000)
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq system-uses-terminfo nil)
(setq large-file-warning-threshold nil)

;; Comint
(setq comint-move-point-for-output nil)
(setq comint-scroll-to-bottom-on-input t)
(setq comint-scroll-to-bottom-on-output nil)
(setq comint-scroll-show-maximum-output t)

;; Disable annyoing C-z on GUI Emacs
(when window-system (global-unset-key "\C-z"))

;; Ido
(setq ido-auto-merge-work-directories-length -1) ;; Avoid ido-find-file jumps
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; Only run flycheck on save
(setq flycheck-check-syntax-automatically '(save mode-enable))

;; Projectile
(setq projectile-enable-caching t)
(setq projectile-indexing-method 'alien)
(setq projectile-switch-project-action 'magit)

;; Hippie expand
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

;; Xref / Tags
(setq tags-add-tables nil)
(setq tags-revert-without-query 1)

;; Dired
(setq dired-dwim-target t)
(setq dired-recursive-copies (quote always))
(setq dired-recursive-deletes (quote top))
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))
(setq auto-mode-alist
      (cons '("[^/]\\.dired$" . dired-virtual-mode) auto-mode-alist))

;; Magit
(setq magit-no-confirm '(unstage-all-changes))
(setq magit-status-margin
      '(nil "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

;; Compilation buffer
(setq compilation-scroll-output 'first-error)

;; Savehist
(setq history-length 1000)
(setq savehist-file (expand-file-name "tmp/savehist" emacs-d))
(load-history savehist-file)

;; Ibuffer
(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 50 50 :left :elide) " "
              filename-and-process)
        (mark " " (name 16 -1) " " filename)))
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-vc-set-filter-groups-by-vc-root)))

;; God mode
(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)

;; Clean up whitespace before save
(add-hook 'before-save-hook 'whitespace-cleanup)

;;; behavior.el ends here
