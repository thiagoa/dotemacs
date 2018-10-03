(require 'uniquify)
(require 'ivy-rich)

(ignore-errors (menu-bar-mode -1))
(ignore-errors (scroll-bar-mode -1))
(ignore-errors (tooltip-mode -1))
(ignore-errors (tool-bar-mode -1))

(load-theme 'doom-one t)
(ivy-rich-mode)
(disable-annoyances)
(disable-startup-screen)
(show-full-filename-in-window-title)
(force-split-window-sensibly-to-vertical-when-big-font)

(setq mac-right-command-modifier 'meta)
(setq linum-format "%d ")
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " : ")
(setq my-default-font "Menlo 15")
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)
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

(fontify-frame nil)

(push 'fontify-frame after-make-frame-functions)

(custom-set-variables '(column-number-mode t))
