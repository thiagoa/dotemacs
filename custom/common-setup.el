;;;;;;;;;;;;;;;;;;;
;; LOOK AND FEEL ;;
;;;;;;;;;;;;;;;;;;;

;; Disable menu bar, tool bar and scroll bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; Disable startup screen, show scratch buffer instead
(setq inhibit-startup-screen t)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)

;; Ignore all warnings
(setq ring-bell-function 'ignore)

;; Config terminal encoding
(add-hook 'term-exec-hook
          (function
           (lambda ()
             (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; Line number formatting
(setq linum-format "%d ")

;; Column number mode
(custom-set-variables
 '(column-number-mode t))

;; Font
(set-default-font "-*-Menlo-medium-normal-normal-*-16-*-*-*-m-0-iso10646-1")

;; Theme
(load-theme 'molokai t)

;;;;;;;;;;;;;
;; PLUGINS ;;
;;;;;;;;;;;;;

;; Revert to previous pane configurations with C-c (left or right)
(winner-mode 1)

;; Tramp
(setq tramp-default-method "ssh")

;; Enable cache in projectile. "C-u C-c p f" to force reload.
(setq projectile-enable-caching t)

;; Guide key
(guide-key-mode 1)
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c p" "C-c ;"))

;; Make Emacs use the same PATH as the shell
(when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize))

;; Enable Discover plugin globally
(global-discover-mode 1)

;; Helm
(require 'helm-files)

;; Enable projectile globally
(projectile-global-mode)

;; Use undo tree for redo
(global-undo-tree-mode 1)

;; Ag
(setq ag-executable "/usr/local/bin/ag")

;; Recent files mode.
(recentf-mode t)
(setq recentf-max-saved-items 50)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EDITING AND CORE MAPPINGS ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Backspace with C-h
(global-set-key (kbd "C-h") 'delete-backward-char)

;; Kill word with C-w for consistency with shell keybindings.
;; C-w is kill-region by default.
(global-set-key "\C-w" 'backward-kill-word)

;; Kill a line (equivalent to C-u in terminal). I don't
;; want to override the universal argument.
(global-set-key (kbd "C-c C-h") 'kill-whole-line)

;; Remap kill-region
(global-set-key "\C-x\C-k" 'kill-region)

;; Mark paragraph
(global-set-key (kbd "M-h") 'mark-paragraph)

;; Select current line
(global-set-key (kbd "C-c C-l") 'select-current-line)

;; Open line below or above (like vim)
(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; Shortcut for comment / uncomment
(global-set-key (kbd "C-x ;") 'comment-or-uncomment-line-or-region)

;; Override default help prefix. I prefer to use C-h as backspace
;; for consistency with shell keybindings
(global-set-key "\C-x\C-m" help-map)

;; Move between windows with shift arrow key
(windmove-default-keybindings)

; Navigate buffers
(global-set-key (kbd "C-c l") 'next-buffer)
(global-set-key (kbd "C-c h") 'previous-buffer)

;; Fire ibuffer by default
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Scroll up or down without moving cursor (almost)
(global-set-key (kbd "M-n") (kbd "C-u 1 C-v"))
(global-set-key (kbd "M-p") (kbd "C-u 1 M-v"))

;; Duplicate line and put above
(global-set-key "\C-c\C-d" "\C-a\C- \C-n\M-w\C-y\C-p")

;; Move lines
(global-set-key [(control shift up)] 'move-line-up)
(global-set-key [(control shift down)] 'move-line-down)

;; Join lines a la-vim
(global-set-key (kbd "C-M-j") 'join-line)

;;;;;;;;;;;;;;;;;;;;;
;; PLUGIN MAPPINGS ;;
;;;;;;;;;;;;;;;;;;;;;

;; Helm "find anything"
(define-key global-map (kbd "C-x t") 'helm-for-files)

;; Multiple cursors
(global-set-key (kbd "C-x C-y") 'mc/mark-next-like-this)

;; Eshell here
(global-set-key (kbd "C-1") 'eshell-here)

; Magit status
(global-set-key (kbd "C-x m") 'magit-status)

;; Ace jump mode
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Expand region
(global-set-key (kbd "C-c =") 'er/expand-region)

;; Go to a symbol with IDO
(global-set-key (kbd "M-i") 'ido-goto-symbol)

;;;;;;;;;;;;;
;; ALIASES ;;
;;;;;;;;;;;;;

(defalias 'ff 'find-file)

;;;;;;;;;;;;;;
;; BEHAVIOR ;;
;;;;;;;;;;;;;;

;; Default shell is ZSH
(setq explicit-shell-file-name "/usr/local/bin/zsh")

;; Store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

; Always delete trailing whitespace
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Coffee script
(setq whitespace-action '(auto-cleanup))
(setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab))
(custom-set-variables '(coffee-tab-width 2))

; Leave the right option key for special characters on Macintosh
(setq ns-right-alternate-modifier nil)

; Enable upcase and downcase region commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Electric indent mode always
(electric-indent-mode +1)

;; Indent on new line
(defvar newline-and-indent t)

;; Run other commands in mini-buffer (ex: C-u M-!)
(setq enable-recursive-minibuffers t)

(setq system-uses-terminfo nil)

;; Get rid of annoyances
(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
	    kill-buffer-query-functions))

;; Enable line numbers per mode
(add-hook 'linum-mode-hook
	  (lambda () (linum-mode 1)))
(add-hook 'web-mode-hook
	  (lambda () (linum-mode 1)))

;; Eshell config
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

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

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	                (add-to-list 'name-and-pos (cons name position))))))))

(defun push-mark-no-activate ()
    "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
    (interactive)
    (push-mark (point) t nil)
    (message "Pushed mark to ring"))

;; PLEASE THINK ABOUT A MAPPING FOR THE PREV COMMAND!

(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
		     (file-name-directory (buffer-file-name))
		   default-directory))
	 (height (/ (window-total-height) 3))
	 (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun eshell/clear ()
  "04Dec2001 - sailor, to clear the eshell buffer."
  (interactive)
  (let ((inhibit-read-only t))
        (erase-buffer)))
(eval-when-compile
  (require 'cl))

(defun get-buffers-matching-mode (mode)
  "Returns a list of buffers where their major-mode is equal to MODE"
  (let ((buffer-mode-matches '()))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
	(if (eq mode major-mode)
	    (add-to-list 'buffer-mode-matches buf))))
    buffer-mode-matches))

(defun multi-occur-in-this-mode ()
  "Show all lines matching REGEXP in buffers with this major mode."
  (interactive)
  (multi-occur
   (get-buffers-matching-mode major-mode)
   (car (occur-read-primary-args))))

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    ))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (end-of-line)
    (set-mark (line-beginning-position)))
