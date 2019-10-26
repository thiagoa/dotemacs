;;; config.el --- General config
;;
;;; Commentary:
;;
;; Appearance config, Behavior config, all kinds of settings

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
(require 'config-helpers)
(require 'uniquify)
(require 'linum)
(require 'ansi-color)

(run-server)

;;;;;;;;;;;;;;;;;;
;; Global modes ;;
;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;
;; Font ;;
;;;;;;;;;;

(defvar my-default-font "Menlo 15")

;;;;;;;;;;;;;;;;;;
;; Frame config ;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist `(font . ,my-default-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(show-full-filename-in-window-title)

;;;;;;;;;;;;;;;;;
;; Buffer list ;;
;;;;;;;;;;;;;;;;;

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " : ")

;;;;;;;;;;;;
;; Colors ;;
;;;;;;;;;;;;

(setq ansi-color-names-vector
      ["black"
       "PaleVioletRed3"
       "green3"
       "yellow3"
       "SteelBlue1"
       "orchid1"
       "cyan3"
       "gray90"])

;;;;;;;;;;;;;;;;;;;;;;
;; GUI Emacs config ;;
;;;;;;;;;;;;;;;;;;;;;;

(ignore-errors (menu-bar-mode -1))
(ignore-errors (scroll-bar-mode -1))
(ignore-errors (tooltip-mode -1))
(ignore-errors (tool-bar-mode -1))

(when window-system (global-unset-key "\C-z"))

(setq ns-use-proxy-icon nil
      ns-use-thin-smoothing t
      ns-use-mwheel-momentum t
      ns-use-mwheel-acceleration t
      x-colors (ns-list-colors)) ;; MacPorts emacs-app port bug

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme. Good options: nord, zenburn, dracula, nimbus, spacemacs-dark ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'dracula t)

;;;;;;;;;;;;;;;;;;;;;;;
;; Lines and columns ;;
;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables '(column-number-mode t))
(setq linum-format "%d ")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keep track of buffers on global mark ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(buffer-trail-advise '(helm-buffers-list
                       helm-find-files
                       helm-recentf
                       helm-projectile-find-file
                       xref-find-definitions))

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(setq helm--url-regexp "WILL-NEVER-EVER-MATCH-I-DONT-WANT-THIS-FUNCTIONALITY"
      helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match t
      helm-split-window-default-side 'other)

(add-helm-projectile-projects-action
 '(("Rails console"        "M-r" execute-projectile-rails-console-under-dir)
   ("Find file in project" "C-f" execute-helm-projectile-find-file-under-dir)))

;;;;;;;;;;;;;
;; Paredit ;;
;;;;;;;;;;;;;

(put 'paredit-forward-delete 'delete-selection 'supersede)
(put 'paredit-backward-delete 'delete-selection 'supersede)
(put 'paredit-open-round 'delete-selection t)
(put 'paredit-open-square 'delete-selection t)
(put 'paredit-doublequote 'delete-selection t)
(put 'paredit-newline 'delete-selection t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable / tune up Emacs features ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(disable-annoyances)
(disable-startup-screen)
(force-split-window-sensibly-to-horizontal-when-big-font)

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)

(setq eval-expression-print-level nil
      eval-expression-print-length nil
      sentence-end-double-space nil
      set-mark-command-repeat-pop 't
      mac-right-command-modifier 'meta
      save-interprogram-paste-before-kill t
      vc-follow-symlinks t
      recentf-max-saved-items 2000
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      system-uses-terminfo nil
      large-file-warning-threshold nil)

;;;;;;;;;;;;
;; Comint ;;
;;;;;;;;;;;;

(setq comint-move-point-for-output nil
      comint-scroll-to-bottom-on-input t
      comint-scroll-to-bottom-on-output nil
      comint-scroll-show-maximum-output t)

;;;;;;;;;
;; Ido ;;
;;;;;;;;;

(setq ido-auto-merge-work-directories-length -1 ;; Avoid ido-find-file jumps
      ido-enable-flex-matching t
      ido-everywhere t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Only run flycheck on save ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq flycheck-check-syntax-automatically '(save mode-enable))

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(setq projectile-enable-caching t
      projectile-indexing-method 'alien
      projectile-switch-project-action 'magit)

;;;;;;;;;;;;;;;;;;;
;; Hippie expand ;;
;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;
;; Xref / Tags ;;
;;;;;;;;;;;;;;;;;

(setq tags-add-tables nil
      tags-revert-without-query 1)

;;;;;;;;;;;
;; Dired ;;
;;;;;;;;;;;

(setq dired-dwim-target t
      dired-recursive-copies (quote always)
      dired-recursive-deletes (quote top))

(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

(setq auto-mode-alist
      (cons '("[^/]\\.dired$" . dired-virtual-mode) auto-mode-alist))

;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;

(setq magit-no-confirm '(unstage-all-changes)
      magit-status-margin
      '(nil "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (equal (with-current-buffer buffer major-mode)
                                'magit-status-mode))
                    '(display-buffer-same-window)
                  nil))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation buffer ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq compilation-scroll-output 'first-error)

;;;;;;;;;;;;;;
;; Savehist ;;
;;;;;;;;;;;;;;

(setq history-length 1000
      savehist-file (expand-file-name "tmp/savehist" emacs-d))
(load-history savehist-file)

;;;;;;;;;;;;;
;; Ibuffer ;;
;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;
;; God mode ;;
;;;;;;;;;;;;;;

(add-hook 'god-mode-enabled-hook 'my-update-cursor)
(add-hook 'god-mode-disabled-hook 'my-update-cursor)
(add-to-list 'god-exempt-major-modes 'cider-repl-mode)

;;;;;;;;;;;;;;
;; Org mode ;;
;;;;;;;;;;;;;;

(setq-default org-download-image-dir "~/OneDrive/Journal/images"
              org-journal-dir "~/OneDrive/Journal")

(setq org-journal-file-format "%Y%m%d.org"
      org-journal-date-format "%A, %Y-%m-%d")

(require 'org-journal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean up whitespace before save ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'whitespace-cleanup)

;;; config.el ends here
