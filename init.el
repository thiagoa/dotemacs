(package-initialize)

(defvar cask-path "/usr/local/share/emacs/site-lisp/cask/cask.el")

(require 'cask cask-path)
(cask-initialize)
(require 'pallet)

(setenv "LESS_OPEN" nil)
(setenv "VISUAL" "emacsclient -n")
(setenv "EDITOR" (getenv "VISUAL"))
(setenv "PAGER" "cat")

(add-to-list 'load-path "~/.emacs.d/config")
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")

(load "functions.el")
(load "missile.el")
(load "looknfeel.el")
(load "behavior.el")
(load "clojure.el")
(load "ruby.el")
(load "elixir.el")
(load "emacslisp.el")
(load "markdown.el")
(load "json.el")
(load "my-ibuffer.el")
(load "terminal.el")
(load "keybindings.el")

(let ((filename "~/.emacs.custom.el"))
  (if (file-exists-p filename)
      (load filename)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["black" "PaleVioletRed3" "green3" "yellow3" "SteelBlue1" "orchid1" "cyan3" "gray90"])
 '(column-number-mode t)
 '(comint-move-point-for-output nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(package-selected-packages
   (quote
    (yaml-mode web-mode undo-tree seeing-is-believing rspec-mode robe rainbow-delimiters projectile-rails paredit pallet molokai-theme markdown-mode magit js2-mode expand-region exec-path-from-shell enh-ruby-mode discover cider auto-complete aggressive-indent ag ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
