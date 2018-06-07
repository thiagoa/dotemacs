(require 'uniquify)

(disable-annoyances)
(disable-startup-screen)
(show-full-filename-in-window-title)
(load-theme 'dracula t)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

(setq mac-right-command-modifier 'meta)
(setq my-default-font "Monaco 16")
(setq linum-format "%d ")
(setq uniquify-buffer-name-style 'post-forward)
(setq uniquify-separator " : ")

(add-to-list 'default-frame-alist `(font . ,my-default-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(custom-set-variables '(column-number-mode t))

(fontify-frame nil)
(push 'fontify-frame after-make-frame-functions)
