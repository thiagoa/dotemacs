(require 'cask "~/.emacs.d/.cask/24.4.1/elpa/cask-20150503.846/cask.el")
(cask-initialize)
(require 'pallet)

(add-to-list 'load-path "~/.emacs.d/custom")

(load "common-setup.el")
(load "ruby.el")
(load "rcodetools.el")
