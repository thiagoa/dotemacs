
;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
;(package-initialize)

(require 'cask (replace-regexp-in-string "\n$" "" (shell-command-to-string "find ~/.emacs.d -name cask.el")))
(cask-initialize)
(require 'pallet)
(add-to-list 'load-path "~/.emacs.d/custom")

(load "common-setup.el")
(load "ruby.el")
(load "php.el")
