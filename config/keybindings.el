(defvar scroll-viewport-up    (kbd "C-u 3 C-v"))
(defvar scroll-viewport-down  (kbd "C-u 3 M-v"))
(defvar multiple-cursors-cmd  'mc/mark-next-like-this)

(windmove-default-keybindings) ; Shift + arrow keys to move between windows

(global-set-key (kbd "C-c C-h")        'kill-whole-line)
(global-set-key (kbd "M-h")            'mark-paragraph)
(global-set-key (kbd "C-c C-l")        'select-current-line)
(global-set-key (kbd "C-o")            'open-next-line)
(global-set-key (kbd "M-o")            'open-previous-line)
(global-set-key (kbd "C-M-o")          'open-next-and-previous-line)
(global-set-key (kbd "C-x ;")          'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-M-j")          'join-line)
(global-set-key [(control shift up)]   'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)
(global-set-key (kbd "C-c l")          'next-buffer)
(global-set-key (kbd "C-c h")          'previous-buffer)
(global-set-key (kbd "C-x C-b")        'ibuffer)
(global-set-key (kbd "s-n")             scroll-viewport-up)
(global-set-key (kbd "s-p")             scroll-viewport-down)
(global-set-key (kbd "C-x C-y")         multiple-cursors-cmd)
(global-set-key (kbd "C-1")            'eshell-here)
(global-set-key (kbd "C-c SPC")        'ace-jump-mode)
(global-set-key (kbd "C-=")            'er/expand-region)
(global-set-key (kbd "M-i")            'ido-goto-symbol)
(global-set-key (kbd "M-\\")           'hippie-expand)
(global-set-key (kbd "C-c p w")        'projectile-rails-console)
(global-set-key (kbd "C-M-y")          'reverse-transpose-sexps)
(global-set-key (kbd "C-x g")          'magit-status)
(global-set-key (kbd "C-x t")          'counsel-recentf)
(define-key 'help-command (kbd "C-f")  'find-function)
(define-key 'help-command (kbd "C-k")  'find-function-on-key)
(define-key 'help-command (kbd "C-p")  'find-function-at-point)
(define-key 'help-command (kbd "C-l")  'find-library)
(global-set-key (kbd "C-c d")          'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c t")          'ansi-term)
(global-set-key (kbd "C-x C-j")        'dired-jump)
(global-set-key (kbd "C-c k a")        'kill-variable-assignment)
(global-set-key (kbd "<C-return>")     'other-window)
(global-set-key (kbd "C-c q")          'kill-other-buffer-and-close-window)
(global-set-key (kbd "C-c r r")        'string-rectangle)
(global-set-key (kbd "C-c r i")        'string-insert-rectangle)
(global-set-key (kbd "C-c r k")        'kill-rectangle)
(global-set-key (kbd "C-S-SPC")        'rectangle-mark-mode)
(global-set-key (kbd "C-c u d")        'ruby-duplicate-block-below)
(global-set-key (kbd "C-c D")          'delete-file-and-buffer)
(global-set-key (kbd "C-'")            'change-inner)
(global-set-key (kbd "C-;")            'change-outer)
(global-set-key (kbd "C-M-;")          'iy-go-to-char)
(global-set-key (kbd "C-M-'")          'iy-go-to-char-backward)
(global-set-key (kbd "C-c g")          'toggle-option-key)
(global-set-key (kbd "C-.")            'next-non-read-only-buffer)
(global-set-key (kbd "C-,")            'prev-non-read-only-buffer)
(global-set-key (kbd "C-c F")          'find-file-at-point)
(global-set-key (kbd "C-S-c C-S-c")    'mc/edit-lines)
(global-set-key (kbd "C->")            'mc/mark-next-like-this)
(global-set-key (kbd "C-<")            'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")        'mc/mark-all-like-this)

;; bind this command globally and make it run the last failing thing
(global-set-key (kbd "C-c , r")        'rspec-rerun)

;; Minibuffer

(define-key minibuffer-local-map (kbd "C-c f") 'name-of-the-file)

;; Elixir

(eval-after-load 'elixir-mode
  '(define-key elixir-mode-map (kbd "C-x C-/") 'company-complete))

(eval-after-load 'elixir-mode
  '(define-key elixir-mode-map (kbd "C-x C-e") 'alchemist-run-line-and-compile))

(eval-after-load 'alchemist-iex-mode
  '(define-key alchemist-iex-mode-map (kbd "C-x C-/") 'company-complete))
