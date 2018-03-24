(disable-annoyances)
(disable-startup-screen)

(add-to-list 'default-frame-alist '(font . "-*-Menlo-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1"))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load-theme 'molokai t)

(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

(setq linum-format "%d ")
(custom-set-variables '(column-number-mode t))
