;;;;;;;;;;;;;;;;;;;
;; LOOK AND FEEL ;;
;;;;;;;;;;;;;;;;;;;

;; Disable startup screen - show scratch buffer instead
(setq inhibit-startup-screen t)

;; Disable toolbar
(tool-bar-mode -1)

;; Ignore all warnings
(setq ring-bell-function 'ignore)

;; Enable line numbers
(global-linum-mode t)

;; Font
(set-default-font "-apple-Menlo-medium-normal-normal-*-14-*-*-*-m-0-iso10646-")

;; Theme
(load-theme 'molokai t)

;;;;;;;;;;;;
;; INDENT ;;
;;;;;;;;;;;;

;; Electric indent mode always
(electric-indent-mode +1)

;; Indent on new line
(defvar newline-and-indent t)

;;;;;;;;;;;;;
;; WINDOWS ;;
;;;;;;;;;;;;;

;; Move between windows with shift arrow key
(windmove-default-keybindings)

;; Revert to previous pane configurations with C-c (left or right)
(winner-mode 1)

;;;;;;;;;;;;;
;; PLUGINS ;;
;;;;;;;;;;;;

;; Enable projectile globaly
(projectile-global-mode)

;; Use undo tree for redo
(global-undo-tree-mode 1)

;; ag configuration
(setq ag-executable "/usr/local/bin/ag")

;;;;;;;;;;;;;;
;; MAPPINGS ;
;;;;;;;;;;;;;;

;; M-x convenient mapping
(global-set-key "\C-x\C-m" 'execute-extended-command)

;; Fire ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Deletes and kill mappings
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "C-S-h") 'kill-whole-line)
(global-set-key "\C-x\C-k" 'kill-region)

;; Mark paragraph
(global-set-key (kbd "M-h") 'mark-paragraph)

; Magit status
(global-set-key (kbd "C-x m") 'magit-status)

; Navigate buffers
(global-set-key (kbd "C-c l") 'next-buffer)
(global-set-key (kbd "C-c h") 'previous-buffer)

;; Open line below or above (like vim)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; Scroll up or down without moving cursor (almost)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; Shortcut for comment / uncomment
(global-set-key (kbd "C-x C-;") 'comment-or-uncomment-region)

;; Ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Expand region
(global-set-key (kbd "C-=") 'er/expand-region)

;; Duplicate line and put above
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y\C-p")

;; Move lines
(global-set-key [(control shift up)] 'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)

;; Join lines a la vim
(global-set-key (kbd "C-M-j") 'join-line)

;;;;;;;;;;;;;;
;; BEHAVIOR ;;
;;;;;;;;;;;;;;

; IDO mode configuration
(ido-mode 1)
(ido-vertical-mode 1)

(custom-set-variables
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido)))

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Auto complete config
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/.cask/24.3.1/elpa/auto-complete-20140322.321/dict")
;; (ac-config-default)
;; (setq ac-ignore-case nil)
;; (add-to-list 'ac-modes 'web-mode)

;; Coffee script
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables '(coffee-tab-width 2))

; Leave the right option key for special characters on Macintosh
(setq ns-right-alternate-modifier nil)

; Default shell is bash. Will leave that on until I find out how to play nice with ZSH.
(setq explicit-shell-file-name "/bin/bash")

; Enable upcase and downcase region commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;;;;;;;;;;;;;;;
;; FUNCTIONS ;;
;;;;;;;;;;;;;;;

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-previous-line (arg)
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

(defun volatile-kill-buffer ()
  "Kill current buffer unconditionally."
  (interactive)
  (let ((buffer-modified-p nil))
    (kill-buffer (current-buffer))))

(defalias 'ff 'find-file)
