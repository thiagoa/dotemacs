(defvar scroll-viewport-up    (kbd "C-u 3 C-v"))
(defvar scroll-viewport-down  (kbd "C-u 3 M-v"))

(windmove-default-keybindings) ; Shift + arrow keys to move between windows

(global-set-key (kbd "C-c C-h")        'crux-kill-whole-line)
(global-set-key (kbd "M-h")            'mark-paragraph)
(global-set-key (kbd "C-c C-l")        'mark-current-line)
(global-set-key (kbd "<C-return>")     'crux-smart-open-line)
(global-set-key (kbd "<M-return>")     'crux-smart-open-line-above)
(global-set-key (kbd "<C-M-return>")   'smart-open-line-below-and-above)
(global-set-key (kbd "C-x ;")          'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-M-j")          'crux-top-join-line)
(global-set-key [(control shift up)]   'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)
(global-set-key (kbd "C-c l")          'next-buffer)
(global-set-key (kbd "C-c h")          'previous-buffer)
(global-set-key (kbd "C-x C-b")        'ibuffer)
(global-set-key (kbd "s-n")             scroll-viewport-up)
(global-set-key (kbd "s-p")             scroll-viewport-down)
(global-set-key (kbd "C-1")            'eshell-here)
(global-set-key (kbd "C-c SPC")        'ace-jump-mode)
(global-set-key (kbd "C-=")            'er/expand-region)
(global-set-key (kbd "M-i")            'ido-goto-symbol)
(global-set-key (kbd "M-\\")           'hippie-expand)
(global-set-key (kbd "C-c p w")        'projectile-rails-console)
(global-set-key (kbd "C-M-y")          'reverse-transpose-sexps)
(global-set-key (kbd "C-x g")          'magit-status)
(global-set-key (kbd "C-x t")          'counsel-recentf)
(global-set-key (kbd "C-c d")          'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c t")          'ansi-term)
(global-set-key (kbd "C-x C-j")        'dired-jump)
(global-set-key (kbd "C-c k a")        'kill-variable-assignment)
(global-set-key (kbd "M-o")            'other-window)
(global-set-key (kbd "C-o")            'go-to-alternate-buffer)
(global-set-key (kbd "C-c q")          'kill-this-buffer-and-close-window)
(global-set-key (kbd "C-c Q")          'kill-other-buffer-and-close-window)
(global-set-key (kbd "C-c r r")        'string-rectangle)
(global-set-key (kbd "C-c r i")        'string-insert-rectangle)
(global-set-key (kbd "C-c r k")        'kill-rectangle)
(global-set-key (kbd "C-S-SPC")        'rectangle-mark-mode)
(global-set-key (kbd "C-c u d")        'ruby-duplicate-block-below)
(global-set-key (kbd "C-c D")          'crux-delete-file-and-buffer)
(global-set-key (kbd "C-'")            'change-inner)
(global-set-key (kbd "C-;")            'change-outer)
(global-set-key (kbd "C-M-;")          'iy-go-to-char)
(global-set-key (kbd "C-M-'")          'iy-go-to-char-backward)
(global-set-key (kbd "C-c g")          'toggle-option-key)
(global-set-key (kbd "M-=")            'next-non-read-only-buffer)
(global-set-key (kbd "M--")            'prev-non-read-only-buffer)
(global-set-key (kbd "C-c F")          'find-file-at-point-dwim)
(global-set-key (kbd "C-S-c C-S-c")    'mc/edit-lines)
(global-set-key (kbd "C->")            'mc/mark-next-like-this)
(global-set-key (kbd "C-<")            'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<")        'mc/mark-all-like-this)
(global-set-key (kbd "C-c , r")        'tests-anywhere-rerun)
(global-set-key (kbd "C-c , a")        'tests-anywhere-verify-all)
(global-set-key [left]                 'cycle-magit-buffers)
(global-set-key [right]                'cycle-magit-buffers)
(global-set-key (kbd "M-*")            'xah-search-current-word)

;;;;;;;;;;;;;;;;;;;
;; Fast movement ;;
;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-S-n")
                (lambda ()
                  (interactive)
                  (ignore-errors (next-line 5))))

(global-set-key (kbd "C-S-p")
                (lambda ()
                  (interactive)
                  (ignore-errors (previous-line 5))))

(global-set-key (kbd "C-S-f")
                (lambda ()
                  (interactive)
                  (ignore-errors (forward-char 5))))

(global-set-key (kbd "C-S-b")
                (lambda ()
                  (interactive)
                  (ignore-errors (backward-char 5))))

;;;;;;;;;;
;; Help ;;
;;;;;;;;;;

(define-key 'help-command (kbd "C-f")  'find-function)
(define-key 'help-command (kbd "C-k")  'find-function-on-key)
(define-key 'help-command (kbd "C-p")  'find-function-at-point)
(define-key 'help-command (kbd "C-l")  'find-library)

;;;;;;;;;;;;;;;;
;; Minibuffer ;;
;;;;;;;;;;;;;;;;

(define-key minibuffer-local-map (kbd "C-c f") 'name-of-the-file)

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

(add-hook 'enh-ruby-mode-hook
          (lambda ()
            (define-key inf-ruby-minor-mode-map (kbd "C-c C-l") 'mark-current-line)
            (define-key inf-ruby-minor-mode-map (kbd "C-x l") 'ruby-load-file)))

(eval-after-load 'rspec-mode
  (progn
    '(define-key rspec-mode-map (kbd "C-c , r") 'tests-anywhere-rerun)
    '(define-key rspec-mode-map (kbd "C-c , a") 'tests-anywhere-verify-all)))

;;;;;;;;;;;;;
;; Clojure ;;
;;;;;;;;;;;;;

(eval-after-load 'cider-mode
  (progn
    '(define-key cider-mode-map (kbd "C-c , a") 'tests-anywhere-verify-all)
    '(define-key cider-mode-map (kbd "C-c , r") 'tests-anywhere-rerun)))

;;;;;;;;;;;;
;; Elixir ;;
;;;;;;;;;;;;

(eval-after-load 'elixir-mode
  '(progn
     (define-key elixir-mode-map (kbd "C-x C-/") 'company-complete)
     (define-key elixir-mode-map (kbd "C-x C-e") 'alchemist-run-line-and-compile)))

(eval-after-load 'alchemist-iex-mode
  '(define-key alchemist-iex-mode-map (kbd "C-x C-/") 'company-complete))

;;;;;;;;;;;
;; Lisps ;;
;;;;;;;;;;;

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "M-r") 'move-to-window-line-top-bottom)
     (define-key paredit-mode-map (kbd "M-k") 'paredit-raise-sexp)))


(eval-after-load 'magit-mode
  '(progn
     (define-key magit-mode-map [left] 'cycle-magit-buffers)
     (define-key magit-mode-map [right] 'cycle-magit-buffers)))
