(package-initialize)

(defvar cask-path
  (replace-regexp-in-string
   "\n$"
   ""
   (shell-command-to-string "find ~/.emacs.d -name cask.el")))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")))

(require 'cask cask-path)
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/config")

(load "functions.el")
(load "looknfeel.el")
(load "behavior.el")
(load "keybindings.el")
(load "clojure.el")
(load "ruby.el")
(load "emacslisp.el")
(load "json.el")

(let ((filename "~/.emacs.custom.el"))
  (if (file-exists-p filename)
      (load filename)))
