(package-initialize)

(defvar cask-path
  (replace-regexp-in-string
   "\n$"
   ""
   (shell-command-to-string "find ~/.emacs.d -name cask.el")))

(require 'cask cask-path)
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/config")

(load "functions.el")
(load "missile.el")
(load "looknfeel.el")
(load "behavior.el")
(load "keybindings.el")
(load "clojure.el")
(load "ruby.el")
(load "emacslisp.el")
(load "json.el")
(load "terminal.el")

(let ((filename "~/.emacs.custom.el"))
  (if (file-exists-p filename)
      (load filename)))
