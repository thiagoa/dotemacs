(setq gc-cons-threshold 20000000)

(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name))
  "The Emacs main directory")

(setq custom-file (expand-file-name "auto.el" emacs-d))

(add-to-list 'load-path (expand-file-name "config" emacs-d))
(add-to-list 'load-path (expand-file-name "yasnippet" emacs-d))

(load "functions.el")
(load "packages.el")

(unless (package-installed-p pivot-package)
  (pac-install))

(load "environment.el")
(load "missile.el")
(load "looknfeel.el")
(load "tests-anywhere.el")
(load "behavior.el")
(load "buffers.el")
(load "terminal.el")
(load "fzf.el")
(load "keybindings.el")

(mapc 'load
      (file-expand-wildcards
       (expand-file-name "config/langs/*.el" emacs-d)))

(load-if-exists "~/.emacs.custom.el")
