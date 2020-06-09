;;; init.el --- Initialization file for Emacs
;;
;;; Commentary:
;;
;; My Emacs Startup File

;;; Code:

(setq gc-cons-threshold 20000000)

(let ((root (file-name-directory load-file-name)))
  ;; Change location of custom set variables
  (setq custom-file (expand-file-name "auto.el" root))

  ;; Set load path
  (mapc (lambda (dir)
          (add-to-list 'load-path (expand-file-name dir root)))
        '("config" "lib" "snippets")))

;; Require config base and packages
(require 'config-base)

;; Require third party extensions
(require 'crux)

;; Require my own libraries and extensions
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
(require 'ext-god-mode)
(require 'ext-helm)
(require 'ext-projectile)
(require 'ext-linum)
(require 'ext-server)
(require 'ext-comint)
(require 'ext-isearch)
(require 'ext-minibuffer)
(require 'ext-ibuffer)
(require 'ext-compile)
(require 'ext-ido)
(require 'ext-align)
(require 'ext-crux)
(require 'ext-rspec-mode)
(require 'goto-last-change)

;; Load configuration files
(mapc (lambda (pattern)
        (mapc 'load
              (file-expand-wildcards
               (expand-file-name pattern emacs-d))))
      '("config/*.el" "config/langs/*.el"))

(load-if-exists "~/.emacs.custom.el")

(provide 'init)
;;; init.el ends here
