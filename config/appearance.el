(require 'uniquify)

(ignore-errors (menu-bar-mode -1))
(ignore-errors (scroll-bar-mode -1))
(ignore-errors (tooltip-mode -1))
(ignore-errors (tool-bar-mode -1))

(load-theme 'doom-peacock t)
(disable-annoyances)
(disable-startup-screen)
(show-full-filename-in-window-title)
(force-split-window-sensibly-to-horizontal-when-big-font)

(setq mac-right-command-modifier 'meta)
(setq linum-format "%d ")
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " : ")
(setq my-default-font "Menlo 16")
(setq ns-use-proxy-icon nil)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))
(setq ansi-color-names-vector
      ["black"
       "PaleVioletRed3"
       "green3"
       "yellow3"
       "SteelBlue1"
       "orchid1"
       "cyan3"
       "gray90"])

(add-to-list 'default-frame-alist `(font . ,my-default-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

(custom-set-variables '(column-number-mode t))
