(require 'cask "~/.emacs.d/.cask/24.3.1/elpa/cask-20140324.15/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/custom")

(load "common-setup.el")
(load "ruby.el")
(load "rcodetools.el")
