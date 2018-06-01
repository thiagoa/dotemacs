(require 'fullframe)
(require 'uniquify)

(disable-annoyances)
(disable-startup-screen)

(setq mac-right-command-modifier 'meta)

(setq my-default-font "Fira Code Retina 16")

(add-to-list 'default-frame-alist `(font . ,my-default-font))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(load-theme 'dracula t)

(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

(setq linum-format "%d ")
(custom-set-variables '(column-number-mode t))

(show-full-filename-in-window-title)

(setq ibuffer-formats
      '((mark modified read-only " "
	      (name 50 50 :left :elide) " "
	      filename-and-process)
	(mark " " (name 16 -1) " " filename)))

(setq ibuffer-show-empty-filter-groups nil)

(add-hook 'ibuffer-mode-hook
	  '(lambda ()
	     (ibuffer-auto-mode 1)
	     (ibuffer-vc-set-filter-groups-by-vc-root)))

(setq
 uniquify-buffer-name-style 'post-forward
 uniquify-separator " : ")

(fullframe magit-status magit-mode-quit-window nil)

(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
	(if (> (x-display-pixel-width) 2000)
	    (set-frame-parameter frame 'font my-default-font) ;; Cinema Display
	  (set-frame-parameter frame 'font my-default-font)))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)
