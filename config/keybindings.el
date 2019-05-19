;;; keybindings.el --- Behavior config
;;
;;; Commentary:
;;
;; Keybindings config.  One single place to rule'em all.

;;; Code:

(require 'helm)
(require 'ibuffer)
(require 'undo-tree)
(require 'inf-ruby)
(require 'rspec-mode)
(require 'god-mode)
(require 'elixir-mode)
(require 'alchemist-iex)
(require 'paredit)
(require 'magit-mode)
(require 'ext-elisp)
(require 'ext-god-mode)
(require 'enh-ruby-mode)

(defvar scroll-viewport-up       (kbd "C-u 3 C-v"))
(defvar scroll-viewport-down     (kbd "C-u 3 M-v"))
(defvar kill-whole-line-backward (kbd "C-e <C-backspace>"))
(defvar duplicate-sexp-below     [?\C-\M-b ?\C-\M-  ?\M-w ?\C-\M-f return ?\C-y ?\C-\M-b])

(windmove-default-keybindings) ; Shift + arrow keys to move between windows

(global-set-key (kbd "C-x 4 t")        'crux-transpose-windows)
(global-set-key (kbd "C-c C-h")        'crux-kill-whole-line)
(global-set-key (kbd "C-k")            'kill-line)
(global-set-key (kbd "<C-backspace>")  'crux-kill-line-backwards)
(global-set-key (kbd "<C-M-backspace>") kill-whole-line-backward)
(global-set-key (kbd "M-h")            'mark-paragraph)
(global-set-key (kbd "C-c C-SPC")      'mark-current-line)
(global-set-key (kbd "<C-return>")     'crux-smart-open-line)
(global-set-key (kbd "<M-return>")     'crux-smart-open-line-above)
(global-set-key (kbd "<C-M-return>")   'smart-open-line-below-and-above)
(global-set-key (kbd "C-x ;")          'comment-or-uncomment-line-or-region)
(global-set-key (kbd "C-M-j")          'top-join-line)
(global-set-key [(control shift up)]   'move-text-up-and-indent)
(global-set-key [(control shift down)] 'move-text-down-and-indent)
(global-set-key (kbd "C-c l")          'winner-redo)
(global-set-key (kbd "C-c h")          'winner-undo)
(global-set-key (kbd "C-x C-b")        'helm-buffers-list)
(global-set-key (kbd "C-x b")          'ibuffer)
(global-set-key (kbd "s-n")             scroll-viewport-up)
(global-set-key (kbd "s-p")             scroll-viewport-down)
(global-set-key (kbd "C-c SPC")        'shell)
(global-set-key (kbd "C-c j")          'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-=")            'er/expand-region)
(global-set-key (kbd "M-i")            'ido-goto-symbol)
(global-set-key (kbd "M-\\")           'hippie-expand)
(global-set-key (kbd "C-c p w")        'projectile-rails-console)
(global-set-key (kbd "C-x g")          'magit-status)
(global-set-key (kbd "C-c d")          'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c M-d")        'crux-duplicate-and-comment-current-line-or-region)
(global-set-key (kbd "C-c t")          'ansi-term)
(global-set-key (kbd "C-x C-j")        'dired-jump)
(global-set-key (kbd "C-c g d")        'dired-file-at-point-dwim)
(global-set-key (kbd "C-c k a")        'kill-variable-assignment)
(global-set-key (kbd "M-o")            'other-window)
(global-set-key (kbd "C-o")            'go-to-alternate-buffer)
(global-set-key (kbd "C-c Q")          'kill-this-buffer-and-close-window)
(global-set-key (kbd "C-c q")          'kill-other-buffer-and-close-window)
(global-set-key (kbd "C-c v r")        'string-rectangle)
(global-set-key (kbd "C-c v i")        'string-insert-rectangle)
(global-set-key (kbd "C-c v k")        'kill-rectangle)
(global-set-key (kbd "C-S-SPC")        'rectangle-mark-mode)
(global-set-key (kbd "C-c D")          'delete-file-and-buffer)
(global-set-key (kbd "C-c R")          'crux-rename-buffer-and-file)
(global-set-key (kbd "C-'")            'change-inner)
(global-set-key (kbd "C-;")            'change-outer)
(global-set-key (kbd "C-M-;")          'iy-go-up-to-char)
(global-set-key (kbd "C-M-'")          'iy-go-to-char-backward)
(global-set-key (kbd "C-c g p")        'magit-push-popup)
(global-set-key (kbd "C-c g c")        'magit-commit-popup)
(global-set-key (kbd "s-.")            'buffer-trail-forward)
(global-set-key (kbd "s-,")            'buffer-trail-backward)
(global-set-key (kbd "s-/")            'buffer-trail-show-breadcrumbs)
(global-set-key (kbd "s-9")            'buffer-trail-add)
(global-set-key (kbd "s-0")            'buffer-trail-drop)
(global-set-key (kbd "s-=")            'buffer-trail-drag-forward)
(global-set-key (kbd "s--")            'buffer-trail-drag-backward)
(global-set-key (kbd "s->")            'buffer-trail-last)
(global-set-key (kbd "s-<")            'buffer-trail-first)
(global-set-key (kbd "C-c F")          'find-file-at-point-dwim)
(global-set-key (kbd "C-S-c C-S-c")    'mc/edit-lines)
(global-set-key (kbd "C->")            'mc/mark-next-like-this)
(global-set-key (kbd "C-<")            'mc/mark-previous-like-this)
(global-set-key (kbd "C-:")            'mc/mark-next-like-this-word)
(global-set-key (kbd "C-c C-<")        'mc/mark-all-like-this)
(global-set-key (kbd "s-SPC")          'mc/mark-pop)
(global-set-key (kbd "C-c C->")        'mc/reverse-regions)
(global-unset-key                      (kbd "M-<down-mouse-1>"))
(global-set-key (kbd "M-<mouse-1>")    'mc/add-cursor-on-click)
(global-set-key (kbd "C-c , r")        'tests-anywhere-rerun)
(global-set-key (kbd "C-c , a")        'tests-anywhere-verify-all)
(global-set-key [left]                 'cycle-magit-buffers-backward)
(global-set-key [right]                'cycle-magit-buffers-forward)
(global-set-key (kbd "M-*")            'isearch-seek-next-word)
(global-set-key (kbd "M-#")            'isearch-seek-previous-word)
(global-set-key (kbd "M-RET")          'replace-region)
(global-set-key (kbd "M-z")            'zap-up-to-char)
(global-set-key (kbd "M-Z")            'zap-to-char)
(global-set-key (kbd "C-`")            'push-mark-no-activate)
(global-set-key (kbd "M-`")            'jump-to-mark)
(global-set-key (kbd "C-c n")          'tmp-buffer)
(global-set-key (kbd "C-x t")          'helm-recentf)
(global-set-key (kbd "C-x b")          'helm-buffers-list)
(global-set-key (kbd "C-c r")          'xah-run-current-file)
(global-set-key (kbd "C-x C-\\")       'goto-last-change)
(global-set-key (kbd "C-c a :")        'align-to-colon)
(global-set-key (kbd "s-i")            'go-to-rspec-compilation-buffer)
(global-set-key (kbd "C-x M-g")        'magit-dispatch-popup)
(global-set-key (kbd "C-x C-f")        'helm-find-files)
(global-set-key (kbd "C-c i")          'swiper)
(global-set-key (kbd "<s-return>")     'xah-run-current-file)
(global-set-key (kbd "C-x C-1")        'delete-other-windows)
(global-set-key (kbd "C-x C-2")        'split-window-below)
(global-set-key (kbd "C-x C-3")        'split-window-right)
(global-set-key (kbd "C-x C-0")        'delete-window)
(global-set-key (kbd "C-a")            'my-beginning-of-line)
(global-set-key (kbd "C-e")            'my-end-of-line)
(global-set-key (kbd "C-\\")           'toggle-option-key)
(global-set-key (kbd "C-c K")          'kill-other-buffer-and-keep-window)
(global-set-key (kbd "C-x C-m")        'execute-extended-command)
(global-set-key (kbd "C-c s p")        'ag-project)
(global-set-key (kbd "C-c s s")        'ag)
(global-set-key (kbd "M-g M-}")        'next-file)
(global-set-key (kbd "M-g M-{")        'previous-file)
(global-set-key (kbd "M-g M-s")        'next-spec)
(global-set-key (kbd "M-g M-S")        'previous-spec)
(global-set-key (kbd "C-x %")          'my-server-edit)
(global-set-key (kbd "<M-SPC>")        'mark-word)

;;;;;;;;;;;;;;;;;;;
;; Fast movement ;;
;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "C-S-n") (simple-ilambda
                               (ignore-errors (forward-line 5))))

(global-set-key (kbd "C-S-p") (simple-ilambda
                               (ignore-errors (forward-line -5))))

;;;;;;;;;;;;;;;
;; undo-tree ;;
;;;;;;;;;;;;;;;

;; Stop undo-tree from eating rectangle and bookmark bindings
(define-key undo-tree-map (kbd "C-x r") nil)

;;;;;;;;;;;;;
;; isearch ;;
;;;;;;;;;;;;;

(define-key isearch-mode-map (kbd "M-*") 'isearch-seek-next-word)
(define-key isearch-mode-map (kbd "M-#") 'isearch-seek-previous-word)
(define-key isearch-mode-map [(control return)] #'isearch-exit-other-end)

;;;;;;;;;;
;; Help ;;
;;;;;;;;;;

(define-key 'help-command (kbd "C-f")  'find-function)
(define-key 'help-command (kbd "C-k")  'find-function-on-key)
(define-key 'help-command (kbd "C-p")  'find-function-at-point)
(define-key 'help-command (kbd "C-l")  'find-library)
(define-key 'help-command (kbd "C-v")  'find-variable)

;;;;;;;;;;
;; Helm ;;
;;;;;;;;;;

(define-key helm-map (kbd "C-d") (simple-ilambda
                                  (with-helm-alive-p
                                    (helm-exit-and-execute-action
                                     'helm-point-file-in-dired))))

;;;;;;;;;;;;;;;;
;; Minibuffer ;;
;;;;;;;;;;;;;;;;

(define-key minibuffer-local-map (kbd "C-c f") 'name-of-the-file)

;;;;;;;;;;;;;
;; ibuffer ;;
;;;;;;;;;;;;;

(define-key ibuffer-mode-map "/q" 'ibuffer-filter-by-marked-buffers)

;;;;;;;;;;;;
;; Comint ;;
;;;;;;;;;;;;

(define-key comint-mode-map (kbd "<C-return>") 'comint-send-input-stay-on-line)
(define-key inf-ruby-mode-map (kbd "C-M-f") 'comint-next-prompt)
(define-key inf-ruby-mode-map (kbd "C-M-b") 'comint-previous-prompt)
(define-key inf-ruby-mode-map (kbd "<escape>") 'inf-ruby-maybe-switch-to-compilation)

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

(eval-after-load 'rspec-compilation-mode
  (define-key compilation-mode-map (kbd "i")
    'inf-ruby-switch-from-compilation))

(global-set-key (kbd "C-c , p") 'rspec-toggle-compilation-mode)

(mapc (lambda (mode)
        (with-eval-after-load mode
          (mapc (lambda (map)
                  (define-key map (kbd "C-M-SPC") 'ruby-mark-sexp)
                  (define-key map (kbd "C-c u d") 'ruby-duplicate-sexp-below))
                (list ruby-mode-map enh-ruby-mode-map))))
      '("ruby-mode" "enh-ruby-mode"))

(with-eval-after-load "rspec-mode"
  (mapc (lambda (map)
          (define-key map (kbd "C-c , r") 'tests-anywhere-rerun)
          (define-key map (kbd "C-c , a") 'tests-anywhere-verify-all)
          (define-key map (kbd "C-c , s") 'tests-anywhere-verify-single))
        (list rspec-mode-map rspec-verifiable-mode-map)))
;;;;;;;;;;;;;;
;; God mode ;;
;;;;;;;;;;;;;;

;; "Escape" mappings across modes
(global-set-key (kbd "<escape>") 'god-mode-all)
(define-key isearch-mode-map (kbd "<escape>") 'god-mode-isearch-activate)
(define-key god-mode-isearch-map (kbd "<escape>") 'god-mode-isearch-disable)

;; Normal god mode keybindings (most non-"shifted")
(define-key god-local-mode-map (kbd "i") 'god-mode-all)
(define-key god-local-mode-map (kbd "V") 'scroll-down-command)
(define-key god-local-mode-map (kbd "J") 'top-join-line)
(define-key god-local-mode-map (kbd "z") 'zap-up-to-char)
(define-key god-local-mode-map (kbd "m") 'back-to-indentation)
(define-key god-local-mode-map (kbd ".") 'repeat)
(define-key god-local-mode-map (kbd "RET") 'crux-smart-open-line)
(define-key god-local-mode-map (kbd "<S-return>") 'crux-smart-open-line-above)

;; Insert mode hook keybindings (most "shifted")
(define-key god-local-mode-map (kbd "<C-return>") (with-god-insert 'crux-smart-open-line))
(define-key god-local-mode-map (kbd "<M-return>") (with-god-insert 'crux-smart-open-line-above))
(define-key god-local-mode-map (kbd "S") (with-god-insert 'clear-line))
(define-key god-local-mode-map (kbd "A") (with-god-insert 'beginning-of-line))
(define-key god-local-mode-map (kbd "E") (with-god-insert 'end-of-line))
(define-key god-local-mode-map (kbd "F") (with-god-insert 'forward-char))
(define-key god-local-mode-map (kbd "B") (with-god-insert 'backward-char))
(define-key god-local-mode-map (kbd "D") (with-god-insert (key-binding "\C-d")))
(define-key god-local-mode-map (kbd "W") (with-god-insert 'kill-region))
(define-key god-local-mode-map (kbd "R") (with-god-insert 'replace-region))
(define-key god-local-mode-map (kbd "Z") (with-god-insert 'zap-up-to-char))
(define-key god-local-mode-map (kbd "K") (with-god-insert (key-binding "\C-k")))
(define-key god-local-mode-map (kbd "M") (with-god-insert 'back-to-indentation))
(define-key god-local-mode-map (kbd "M-D") (with-god-insert 'kill-word))
(define-key god-local-mode-map [(shift backspace)] (with-god-insert (key-binding (kbd "DEL"))))
(define-key god-local-mode-map [(meta shift backspace)] (with-god-insert (key-binding (kbd "<M-DEL>"))))
(define-key god-local-mode-map (kbd "'") (with-god-insert 'change-inner))
(define-key god-local-mode-map (kbd "\"") (with-god-insert 'change-outer))
(define-key god-local-mode-map (kbd "<C-S-backspace>") (with-god-insert 'crux-kill-line-backwards))

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

;;;;;;;;;;;;;;
;; Web mode ;;
;;;;;;;;;;;;;;

(eval-after-load 'web-mode
  '(progn
     (define-key web-mode-map (kbd "C-c C-h") 'crux-kill-whole-line)))

;;;;;;;;;;;
;; Lisps ;;
;;;;;;;;;;;

(eval-after-load 'paredit
  '(progn
     (define-key paredit-mode-map (kbd "<M-DEL>") (universalize 'paredit-backward-kill-word))
     (define-key paredit-mode-map (kbd "M-d") (universalize 'paredit-forward-kill-word))
     (define-key paredit-mode-map (kbd "M-r") 'move-to-window-line-top-bottom)
     (define-key paredit-mode-map (kbd "M-k") 'paredit-raise-sexp)
     (define-key paredit-mode-map (kbd "C-c u d") duplicate-sexp-below)))

;;;;;;;;;;;
;; Magit ;;
;;;;;;;;;;;

(eval-after-load 'magit-mode
  '(progn
     (define-key magit-mode-map [left] 'cycle-magit-buffers-backward)
     (define-key magit-mode-map [right] 'cycle-magit-buffers-forward)))

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(provide 'keybindings)
;;; keybindings.el ends here
