(setq gc-cons-threshold 20000000)

(let ((root (file-name-directory load-file-name)))
  (setq custom-file (expand-file-name "auto.el" root))
  (mapc (lambda (dir)
          (add-to-list 'load-path (expand-file-name dir root)))
        '("config" "lib" "snippets")))

(require 'config-base)
(require 'ext-elisp)
(require 'general)
(require 'config)
(require 'functions)
(require 'buffer)
(require 'buffer-trail)
(require 'missile)
(require 'tests-anywhere)
(require 'text-editing)
(require 'movement)
(require 'window-management)
(require 'git)
(require 'ext-god-mode)
(require 'ext-helm)
(require 'ext-projectile)
(require 'ext-linum)
(require 'ext-server)
(require 'ext-comint)

(load "environment.el")
(load "looknfeel.el")
(load "tests-anywhere.el")
(load "behavior.el")
(load "buffers.el")
(load "terminal.el")
(load "keybindings.el")
(load "overrides.el")

(mapc 'load
      (file-expand-wildcards
       (expand-file-name "config/langs/*.el" emacs-d)))

(load-if-exists "~/.emacs.custom.el")
