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

(load "missile.el")
(load "looknfeel.el")
(load "behavior.el")
(load "buffers.el")
(load "terminal.el")
(load "keybindings.el")
(load "dired-x.el")

(mapc 'load
      (file-expand-wildcards (expand-file-name "config/langs/*.el" emacs-d)))

(load-if-exists "~/.emacs.custom.el")

(setenv "LESS_OPEN" nil)
(setenv "VISUAL" "emacsclient -n")
(setenv "EDITOR" (getenv "VISUAL"))
(setenv "PAGER" "cat")
