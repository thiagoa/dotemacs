(setq gc-cons-threshold 20000000)

(let ((root (file-name-directory load-file-name)))
  (mapc (lambda (dir)
          (add-to-list 'load-path (expand-file-name dir root)))
        '("config" "lib" "snippets")))

(require 'config-base)

(setq custom-file (expand-file-name "auto.el" emacs-d))

(load "packages.el")
(load "functions.el")

(unless (package-installed-p pivot-package)
  (pac-install))

(load "environment.el")
(load "missile.el")
(load "buffer-trail.el")
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
