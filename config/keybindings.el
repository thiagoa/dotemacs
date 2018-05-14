(defvar scroll-viewport-up    (kbd "C-u 1 C-v"))
(defvar scroll-viewport-down  (kbd "C-u 1 M-v"))
(defvar multiple-cursors-cmd  'mc/mark-next-like-this)

(windmove-default-keybindings) ; Shift + arrow keys to move between windows

(global-set-key (kbd "C-h")            'delete-backward-char)
(global-set-key (kbd "C-w")            'backward-kill-word)
(global-set-key (kbd "C-c C-h")        'kill-whole-line)
(global-set-key (kbd "C-x C-k")        'kill-region)
(global-set-key (kbd "M-h")            'mark-paragraph)
(global-set-key (kbd "C-c C-l")        'select-current-line)
(global-set-key (kbd "C-o")            'open-next-line)
(global-set-key (kbd "M-o")            'open-previous-line)
(global-set-key (kbd "C-x ;")          'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-M-j")          'join-line)
(global-set-key [(control shift up)]   'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)
(global-set-key (kbd "C-x C-m")         help-map)
(global-set-key (kbd "C-c l")          'next-buffer)
(global-set-key (kbd "C-c h")          'previous-buffer)
(global-set-key (kbd "C-x C-b")        'ibuffer)
(global-set-key (kbd "M-n")             scroll-viewport-up)
(global-set-key (kbd "M-p")             scroll-viewport-down)
(global-set-key (kbd "C-x t")          'helm-for-files)
(global-set-key (kbd "C-x C-y")         multiple-cursors-cmd)
(global-set-key (kbd "C-1")            'eshell-here)
(global-set-key (kbd "C-c SPC")        'ace-jump-mode)
(global-set-key (kbd "C-c =")          'er/expand-region)
(global-set-key (kbd "M-i")            'ido-goto-symbol)
(global-set-key (kbd "s-t")            'helm-projectile-find-file-dwim)
