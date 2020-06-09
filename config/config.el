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
(require 'polymode)
(require 'wrap-region)
(require 'iy-go-to-char)
(require 'fzf)
(require 'which-key)

(run-server)

;;;;;;;;;
;; fzf ;;
;;;;;;;;;

(setq fzf/args "-x --print-query")

;;;;;;;;;;
;; Font ;;
;;;;;;;;;;

(if (equal system-type 'gnu/linux)
    (defvar my-default-font "JetBrains Mono 12")
  (defvar my-default-font "JetBrains Mono 15"))

;;;;;;;;;;;;;;;
;; which-key ;;
;;;;;;;;;;;;;;;

(setq which-key-show-early-on-C-h t)
(setq which-key-idle-delay 10000)
(setq which-key-idle-secondary-delay 0.05)

(which-key-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; balanced-windows ;;
;;;;;;;;;;;;;;;;;;;;;;

(balanced-windows-mode)

;;;;;;;;;;;;;;;;;;
;; Frame config ;;
;;;;;;;;;;;;;;;;;;

(add-to-list 'default-frame-alist `(font . ,my-default-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (concat "Emacs - " (abbreviate-file-name (buffer-file-name)))
                 "Emacs - %b"))))

;;;;;;;;;;;;;;;;;
;; Buffer list ;;
;;;;;;;;;;;;;;;;;

(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " : ")

;;;;;;;;;;;;;;
;; Polymode ;;
;;;;;;;;;;;;;;

(setq polymode-prefix-key (kbd "C-c n"))
(define-hostmode poly-ruby-hostmode :mode 'enh-ruby-mode)

(define-innermode poly-graphql-ruby-innermode
  :mode 'graphql-mode
  :head-matcher (rx (or (sequence "define_query " (= 3 (char "'")) (* (any space)))
                        "<<-QUERY"))
  :tail-matcher (rx (or (= 3 (char "'"))
                        " QUERY"))
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-ruby-graphql-mode
  :hostmode 'poly-ruby-hostmode
  :innermodes '(poly-graphql-ruby-innermode))

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
      ns-use-mwheel-acceleration t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme. Good options: nord, zenburn, dracula, nimbus, spacemacs-dark, kaolin-dark, doom-molokai ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load-theme 'solarized-dark t)

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
      helm-split-window-default-side 'below
      helm-split-window-in-side-p t
      helm-buffer-max-length 35
      helm-window-prefer-horizontal-split t) ;; t means prefer vertical split

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

(put 'set-goal-column 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq-default indent-tabs-mode nil)

(setq blink-matching-delay 0
      recenter-positions '(middle top bottom)
      eval-expression-print-level nil
      select-enable-clipboard t
      eval-expression-print-length nil
      sentence-end-double-space nil
      set-mark-command-repeat-pop 't
      x-select-enable-clipboard t
      save-interprogram-paste-before-kill t
      vc-follow-symlinks t
      recentf-max-saved-items 2000
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      system-uses-terminfo nil
      large-file-warning-threshold nil
      recenter-positions '(top middle bottom)
      enable-local-variables :all
      split-height-threshold nil
      split-width-threshold 0)

(if (eq system-type 'darwin)
    (setq mac-right-command-modifier 'meta))

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

;;;;;;;;;;;;;;;;;;;
;; yi-go-to-char ;;
;;;;;;;;;;;;;;;;;;;

(setq iy-go-to-char-continue-when-repeating nil)

;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;

(let ((unstaged-changes-pos
       (cl-position 'magit-insert-unstaged-changes magit-status-sections-hook))
      (untracked-files-pos
       (cl-position 'magit-insert-untracked-files magit-status-sections-hook)))
  (setf (nth unstaged-changes-pos magit-status-sections-hook)
        'magit-insert-untracked-files)
  (setf (nth untracked-files-pos magit-status-sections-hook)
        'magit-insert-unstaged-changes))

(setq magit-no-confirm '(unstage-all-changes)
      magit-status-margin
      '(nil "%Y-%m-%d %H:%M " magit-log-margin-width t 18))

(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (cond ((with-current-buffer buffer (eq major-mode 'magit-status-mode))
                       '(display-buffer-same-window))
                      ((or (string-suffix-p "COMMIT_EDITMSG" (buffer-file-name))
                           (derived-mode-p 'magit-mode))
                       nil)
                      (t '(display-buffer-same-window))))))

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
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring
        last-kbd-macro
        kmacro-ring
        shell-command-history))

;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent-scratch ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(persistent-scratch-setup-default)

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

;;;;;;;;;;;;;;;;;
;; Wrap region ;;
;;;;;;;;;;;;;;;;;

(wrap-region-add-wrapper "( " " )" ")")
(wrap-region-add-wrapper "{ " " }" "}")

;;;;;;;;;;;;;;;;
;; Yasnippets ;;
;;;;;;;;;;;;;;;;

(setq yas-wrap-around-region t)

;;;;;;;;;;;;;;
;; Org mode ;;
;;;;;;;;;;;;;;

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#ff39a3" :weight bold))
        ("READING" . "#E35DBF")
        ("REVIEW" . (:foreground "white" :background "#4d4d4d" :weight bold))
        ("DONE" . "pink")
        ("BUSTED" . "#008080")
        ("STOPPED" . "red")
        ("BETA" . "gray")))

(setq org-log-done 'time)

(setq-default org-download-image-dir "~/OneDrive/Journal/images"
              org-journal-dir "~/OneDrive/Journal")

(setq org-journal-file-format "%Y%m%d.org"
      org-journal-date-format "%A, %Y-%m-%d")

(require 'org-journal)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Clean up whitespace before save ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'before-save-hook 'whitespace-cleanup)

;;;;;;;;;;;;;;;;;;
;; Global modes ;;
;;;;;;;;;;;;;;;;;;

(beginend-global-mode)
(global-dot-mode)
(global-flycheck-mode)
(yas-global-mode)
(wrap-region-global-mode)
(projectile-mode)
(helm-projectile-on)
(winner-mode)
(savehist-mode)
(global-missile-mode)
(delete-selection-mode)
(save-place-mode)
(god-mode)
(global-undo-tree-mode)

;;; config.el ends here
