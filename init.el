;;; init.el --- Initialization file for Emacs
;;
;;; Commentary:
;;
;; My Emacs Startup File

;;; Code:

(defun wsl? ()
  "Are we running under WSL?"
  (eq 0 (call-process "grep" nil nil nil "microsoft" "/proc/version")))

(setq gc-cons-threshold 20000000)

(let ((root (file-name-directory load-file-name)))
  ;; Change location of custom set variables
  (setq custom-file (expand-file-name "auto.el" root))

  ;; Set load path
  (mapc (lambda (dir)
          (add-to-list 'load-path (expand-file-name dir root)))
        '("config" "lib" "snippets")))

(require 'compatibility)

;; Require config base and packages
(require 'config-base)

;; Require third party extensions
(require 'crux)

;; Require my own libraries and extensions
(require 'iy-go-to-char)
(require 'simple-autopair)
(require 'ext-elisp)
(require 'shell-integration)
(require 'general)
(require 'config-helpers)
(require 'tmp-buffer)
(require 'buffer)
(require 'buffer-trail)
(require 'missile)
(require 'tests-anywhere)
(require 'text-editing)
(require 'movement)
(require 'window-management)
(require 'git)
(require 'lang-elixir)
(require 'lang-ruby)
(require 'ext-cider)
(require 'ext-god-mode)
(require 'helm-fzf)
(require 'flycheck-standardrb)
(require 'ext-helm)
(require 'ext-projectile)
(require 'ext-linum)
(require 'ext-server)
(require 'ext-comint)
(require 'ext-isearch)
(require 'ext-sgml)
(require 'ext-minibuffer)
(require 'ext-ibuffer)
(require 'ext-compile)
(require 'ext-ido)
(require 'ext-align)
(require 'ext-crux)
(require 'ext-flycheck)
(require 'ext-rspec-mode)
(require 'ext-process)
(require 'ext-magit)
(require 'goto-last-change)
(require 'wsl)
(require 'ext-emacs)

(load-if-exists "~/.emacs.custom.before.el")

;; Load configuration files
(mapc (lambda (pattern)
        (mapc 'load
              (file-expand-wildcards
               (expand-file-name pattern emacs-d))))
      '("config/*.el" "config/langs/*.el"))

(load-if-exists "~/.emacs.custom.el")

(delete-other-windows)

(provide 'init)
;;; init.el ends here
