(defun load-if-exists (f)
  (if (file-exists-p f)
      (load f)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(defun pac-install ()
  (interactive)
  (load "packages.el")
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun pac-update ()
  (interactive)
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun kill-variable-assignment ()
  (interactive)
  (let
      ((start (progn (back-to-indentation) (point)))
       (end (save-excursion
              (while (not (string= (string (char-after)) "="))
                (forward-char))
              (point))))
    (delete-region start (+ 2 end))))

(defun reverse-transpose-sexps (arg)
  (interactive "*p")
  (transpose-sexps (- arg))
  (backward-sexp (1+ arg))
  (forward-sexp 1))

(defun execute-extended-command-under-dir (project)
  (let ((default-directory project))
    (execute-extended-command nil)))

(defun open-pr ()
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (shell-command "git pr")))

(defun reload-config ()
  (interactive)
  (load "~/.emacs.d/init.el"))

(defun install-cask ()
  (interactive)
  (let ((default-directory "~/.emacs.d"))
    (shell-command "cask install"))
  (reload-config))

(defun repl()
  (interactive)
  (ielm))

(defun safe-linum-mode ()
  (ignore-errors(linum-mode 1)))

(defun run-server ()
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(defun disable-startup-screen ()
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message t)
  (setq ring-bell-function 'ignore))

(defun config-terminal-encoding ()
  (add-hook 'term-exec-hook
            (function
             (lambda ()
               (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

(defun disable-annoyances ()
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq exec-path-from-shell-check-startup-files nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions)))

(defun set-default-shell (path)
  (setq
   explicit-shell-file-name
   (replace-regexp-in-string
    "\n"
    ""
    (shell-command-to-string (concat "which " path)))))

(defun emacs-use-same-path-as-shell ()
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(defun open-next-line (arg)
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

(defun open-next-and-previous-line (arg)
  (interactive "p")
  (open-next-line arg)
  (open-previous-line arg))

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

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun select-current-line ()
  "Select the current line"
  (interactive)
  (beginning-of-line)
  (next-line)
  (set-mark (save-excursion (previous-line) (point))))

(defun show-full-filename-in-window-title ()
  (setq-default
   frame-title-format
   '((:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name)) "%f")))))

(defun toggle-option-key ()
  (interactive)
  (if (eq ns-option-modifier 'meta)
      (progn (setq ns-option-modifier 'none) (message "Changed to none"))
    (progn (setq ns-option-modifier 'meta) (message "Changed to meta"))))

(defun kill-other-buffer-and-close-window ()
  "Kill the current buffer."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (delete-window))

(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (progn
        (delete-file filename)
        (message "Deleted file %s" filename)
        (kill-buffer)))))

(defun name-of-the-file ()
  "From the minibuffer, gets the name of the file the current buffer is based on."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font my-default-font) ;; Cinema Display
          (set-frame-parameter frame 'font my-default-font)))))

(defun next-non-read-only-buffer ()
  (interactive)
  (with-non-read-only-buffers 'next-buffer))

(defun prev-non-read-only-buffer ()
  (interactive)
  (with-non-read-only-buffers 'previous-buffer))

(defun with-non-read-only-buffers (func)
  (let (found)
    (while (not found)
      (funcall func)
      (if (and
           (not buffer-read-only)
           (not (string= (buffer-name) "TAGS"))
           (not (string-prefix-p "*" (string-trim (buffer-name (current-buffer))))))
          (setq found t)))))

;; Elixir

(defun alchemist-run-line-and-compile ()
  (interactive)
  (alchemist-compile-this-buffer)
  (alchemist-eval-print-current-line))

(defun elixir-set-source-dir ()
  (interactive)
  (let* ((path
          (replace-regexp-in-string
           "\n$"
           ""
           (shell-command-to-string "echo `asdf where elixir``asdf current elixir | cut -f1 -d' '`")))
         (binpath (concat path "/bin")))
    (setq alchemist-goto-elixir-source-dir path)
    (setq elixir-format-elixir-path (concat binpath "/elixir"))
    (setq elixir-format-mix-path (concat binpath "/mix"))))

(defun auto-activate-ruby-end-mode-for-elixir-mode ()
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode +1))

;; Ruby

;; Author: Thiago Araújo Silva
;; This can still improve :)
(defun ruby-mark-block ()
  (interactive)
  (ruby-beginning-of-block)
  (move-beginning-of-line 1)
  (push-mark nil t t)
  (ruby-end-of-block)
  (move-beginning-of-line 1)
  (next-line 1))

;; Author: Thiago Araújo Silva
;; This can still improve :)
(defun ruby-duplicate-block-below ()
  (interactive)
  (ruby-mark-block)
  (kill-ring-save (region-beginning) (region-end))
  (move-beginning-of-line 1)
  (yank)
  (deactivate-mark)
  (previous-line)
  (ruby-beginning-of-block)
  (back-to-indentation)
  (open-previous-line 1)
  (next-line))

(defun bundle ()
  (interactive)
  (bundle-install))
