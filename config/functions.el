(defun safe-linum-mode ()
  (ignore-errors(linum-mode 1)))

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

;; Non-interactive

(defun repl()
  (interactive)
  (ielm))

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
  (end-of-line)
  (set-mark (line-beginning-position)))

(defun show-full-filename-in-window-title ()
  (setq-default
   frame-title-format
   '((:eval (if (buffer-file-name)
		(abbreviate-file-name (buffer-file-name)) "%f")))))

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
      (if (vc-backend filename)
	  (vc-delete-file filename)
	(progn
	  (delete-file filename)
	  (message "Deleted file %s" filename)
	  (kill-buffer))))))
