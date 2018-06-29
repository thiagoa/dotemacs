(require 'ido-goto-symbol)
(require 'cl-extra)

;;;;;;;;;;;;;;;;;;;
;; Elisp helpers ;;
;;;;;;;;;;;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun load-if-exists (f)
  (when (file-exists-p f)
    (load f)))

;; Author: Thiago Araújo Silva
(defmacro setq-list-append (var value)
  (list 'setq var (list 'append var `'(,value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config && Package management ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun reload-config ()
  (interactive)
  (load (expand-file-name "init.el" emacs-d)))

(defun pac-install ()
  (interactive)
  (save-buffer)
  (package-refresh-contents)
  (load "packages.el")
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun pac-update ()
  (interactive)
  (save-buffer)
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

;; Author: Thiago Araújo Silva
(defun pac-autoremove ()
  (interactive)
  (load "packages.el")
  (package-autoremove))

;;;;;;;;;;;;;;;;;;
;; Text editing ;;
;;;;;;;;;;;;;;;;;;

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

;; Author: Thiago Araújo Silva
(defun smart-open-line-below-and-above (arg)
  (interactive "p")
  (crux-smart-open-line-above)
  (crux-smart-open-line nil))

(defun replace-region ()
  (interactive)
  (call-interactively 'kill-region)
  (call-interactively 'crux-smart-open-line-above))

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (previous-line 2))

(defun move-line-down ()
  (interactive)
  (next-line 1)
  (transpose-lines 1)
  (previous-line 1))

;; Author: Thiago Araújo Silva
(defun mark-current-line ()
  "Marks the current line"
  (interactive)
  (move-beginning-of-line 1)
  (set-mark
   (save-excursion
     (next-line)
     (move-beginning-of-line 1)
     (point))))

;;;;;;;;;;;;;;;;;;;
;; Moving around ;;
;;;;;;;;;;;;;;;;;;;

;; Modified by Thiago to jump straight to the next or previous occurrence
(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.
“word” here is A to Z, a to z, and hyphen 「-」 and underline 「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'
Version 2015-04-09"
  (unless (bound-and-true-p isearch-mode)
    (let ( $p1 $p2 )
      (if (use-region-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (save-excursion
          (skip-chars-backward "-_A-Za-z0-9")
          (setq $p1 (point))
          (right-char)
          (skip-chars-forward "-_A-Za-z0-9")
          (setq $p2 (point))))
      (setq mark-active nil)
      (when (< $p1 (point))
        (goto-char $p1))
      (isearch-mode t)
      (isearch-yank-string (buffer-substring-no-properties $p1 $p2)))))

(defun xah-seek-next-word ()
  (interactive)
  (xah-search-current-word)
  (isearch-repeat-forward))

(defun xah-seek-previous-word ()
  (interactive)
  (let ((was-bound (bound-and-true-p isearch-mode)))
    (xah-search-current-word)
    (unless was-bound (isearch-repeat-backward))
    (isearch-repeat-backward)))

;; Author: Thiago Araújo Silva
(defun go-to-alternate-buffer ()
  (interactive)
  (switch-to-buffer nil))

;; Author: Thiago Araújo Silva
(defun next-non-read-only-buffer ()
  "Navigates to next *file* buffer"
  (interactive)
  (with-non-read-only-buffers 'next-buffer))

;; Author: Thiago Araújo Silva
(defun prev-non-read-only-buffer ()
  "Navigates to previous *file* buffer"
  (interactive)
  (with-non-read-only-buffers 'previous-buffer))

;; Author: Thiago Araújo Silva
(defun with-non-read-only-buffers (func)
  (let (found (start-buffer (current-buffer)))
    (while (not found)
      (funcall func)
      (if (or
           (eq (current-buffer) start-buffer)
           (and
            (not buffer-read-only)
            (not (string= (buffer-name) "TAGS"))
            (not (string-prefix-p "*" (string-trim (buffer-name (current-buffer)))))))
          (setq found t)))))

;;;;;;;;;;;;;;;;;;;;;;;
;; Window management ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun kill-this-buffer-and-close-window ()
  "Kill the current buffer."
  (interactive)
  (do-kill-this-buffer-and-close-window))

;; Author: Thiago Araújo Silva
(defun kill-other-buffer-and-close-window ()
  "Kill the current buffer."
  (interactive)
  (other-window 1)
  (do-kill-this-buffer-and-close-window))

;; Author: Thiago Araújo Silva
(defun do-kill-this-buffer-and-close-window ()
  (kill-buffer (current-buffer))
  (delete-window))

;; Taken from https://www.reddit.com/r/emacs/comments/676r5b/how_to_stop_findfileatprompting_when_there_is_a/
;; Adapted to expand shell variables
(defun find-file-at-point-dwim (&optional filename)
  (interactive)
  (let* ((name (or filename (ffap-string-at-point 'file)))
         (fname (substitute-in-file-name (expand-file-name name))))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
      (find-file-at-point filename))))

;; Author: Thiago Araújo Silva
(defun kill-extraneous-buffers ()
  (interactive)
  (dolist (buf (buffer-list) nil)
    (if (or (cl-some
             (lambda (pattern) (string-match pattern (buffer-name buf)))
             killable-buffer-patterns)
            (cl-some
             (lambda (cur-mode) (eq cur-mode (with-current-buffer buf major-mode)))
             killable-buffer-major-modes))
        (kill-buffer buf)))
  (message "Done."))

(defvar killable-buffer-major-modes
  '(dired-mode))

(defvar killable-buffer-patterns
  '("^TAGS$"
    "^magit-"
    "^*.+*$"
    ".gz$"
    "^HEAD$"))

;;;;;;;;;
;; Git ;;
;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun open-pr ()
  "Opens a PR for the current project. Depends on the 'git pr' command."
  (interactive)
  (run-under-project-root (lambda () (shell-command "git pr"))))

;; Author: Thiago Araújo Silva
(defun cycle-magit-buffers-forward ()
  (interactive)
  (switch-to-magit-window)
  (let (found (start-buffer (current-buffer)))
    (while (not found)
      (next-buffer)
      (let ((cur-buffer (current-buffer)))
        (if (or
             (string-prefix-p "magit:" (buffer-name cur-buffer))
             (eq cur-buffer start-buffer))
            (setq found t))))))

;; Author: Thiago Araújo Silva
(defun switch-to-magit-window ()
  (dolist (win (window-list) nil)
    (if (string-match "^magit:" (buffer-name (window-buffer win)))
        (select-window win))))

;; Author: Thiago Araújo Silva
(defun cycle-magit-buffers-backward ()
  (interactive)
  (switch-to-magit-window)
  (let (found (start-buffer (current-buffer)))
    (while (not found)
      (previous-buffer)
      (let ((cur-buffer (current-buffer)))
        (if (or
             (string-prefix-p "magit:" (buffer-name cur-buffer))
             (eq cur-buffer start-buffer))
            (setq found t))))))

;; Author: Thiago Araújo Silva
(defun magit-commit-this-buffer ()
  (interactive)
  (magit-unstage-all)
  (magit-stage)
  (magit-commit))

;; Author: Thiago Araújo Silva
(defun git-link-branch ()
  (interactive)
  (let* ((remote (git-link--select-remote))
         (branch (magit-completing-read "Branch" (magit-list-local-branch-names)))
         (git-link-default-branch branch)
         (region (when buffer-file-name (git-link--get-region)))
         (start (car region))
         (end (cadr region)))
    (git-link remote start end)))

;;;;;;;;;;;;;;;;;;;;;
;; General helpers ;;
;;;;;;;;;;;;;;;;;;;;;

(defun run-under-project-root (command)
  "Runs a command under the current project root"
  (let ((default-directory (projectile-project-root)))
    (funcall command)))


;; Author: Thiago Araújo Silva
(defun execute-extended-command-under-dir (project)
  (let ((default-directory project))
    (execute-extended-command nil)))

;; Author: Thiago Araújo Silva
(defun safe-linum-mode ()
  "Run linum-mode safely"
  (interactive)
  (ignore-errors (linum-mode 1)))

(defun run-server ()
  "Runs the Emacs server if it is not running"
  (require 'server)
  (unless (server-running-p)
    (server-start)))

;; Author: Thiago Araújo Silva
(defun toggle-option-key ()
  "Toggles meta between meta and option"
  (interactive)
  (if (eq ns-option-modifier 'meta)
      (progn (setq ns-option-modifier 'none) (message "Changed to none"))
    (progn (setq ns-option-modifier 'meta) (message "Changed to meta"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make config readable ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun force-split-window-sensibly-to-vertical-when-big-font ()
  (setq split-height-threshold nil)
  (setq split-width-threshold 140))

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

(defun show-full-filename-in-window-title ()
  (setq-default
   frame-title-format
   '((:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name)) "%f")))))

;; Why is history not being loaded? :thinking:
;; Author: Thiago Araújo Silva
(defun load-history (savehist-file)
  (unless (file-exists-p savehist-file)
    (progn
      (shell-command (concat "mkdir -p " (file-name-directory savehist-file)))
      (shell-command (concat "touch " savehist-file))))
  (load savehist-file))

;;;;;;;;;;;;;;;;;;;
;; Look and feel ;;
;;;;;;;;;;;;;;;;;;;

(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (> (x-display-pixel-width) 2000)
            (set-frame-parameter frame 'font my-default-font) ;; Cinema Display
          (set-frame-parameter frame 'font my-default-font)))))

;;;;;;;;;;;;;;;;;;;;;
;; Shell shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;

(defun repl()
  (interactive)
  (ielm))

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

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;;;;;;;;;;;;;;;;
;; Minibuffer ;;
;;;;;;;;;;;;;;;;

(defun name-of-the-file ()
  "From the minibuffer, gets the name of the file the current buffer is based on."
  (interactive)
  (insert (buffer-file-name (window-buffer (minibuffer-selected-window)))))

;;;;;;;;;;;;
;; Elixir ;;
;;;;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun alchemist-run-line-and-compile ()
  (interactive)
  (alchemist-compile-this-buffer)
  (alchemist-eval-print-current-line))

;; Author: Thiago Araújo Silva
(defun elixir-set-source-dir ()
  "Sets Elixir source dir. Depends on asdf."
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

;;;;;;;;;;
;; Ruby ;;
;;;;;;;;;;

;; Author: Thiago Araújo Silva
(defun ruby-mark-block ()
  (interactive)
  (ruby-beginning-of-block)
  (move-beginning-of-line 1)
  (push-mark nil t t)
  (ruby-end-of-block)
  (move-beginning-of-line 1)
  (next-line 1))

;; Author: Thiago Araújo Silva
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

;; Author: Thiago Araújo Silva
(defun bundle ()
  (interactive)
  (bundle-install))

;;;;;;;;;
;; FZF ;;
;;;;;;;;;

(defun fzf/pipe-into-fzf (entries)
  "`entries' is a list of strings that is piped into `fzf' as a source."
  (interactive)
  (let ((process-environment
         (cons (concat "FZF_DEFAULT_COMMAND=echo " "\""
                       (mapconcat
                        (lambda (entry)
                          entry)
                        entries
                        "\n")
                       "\"")
               process-environment)))
    (fzf)))

(defun fzf-recentf ()
  "Starts a fzf session with recent files as a source."
  (interactive)
  (recentf-mode)
  (fzf/pipe-into-fzf recentf-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rake extensions and overrides ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar
  rake--env-vars
  ""
  "Environment variables to run rake with. Dynamic variable!")

;; Author: Thiago Araújo Silva
(defun rake--compile-with-env-var (orig-fun root task mode)
  (let ((task (concat rake--env-vars task)))
    (apply orig-fun `(,root ,task ,mode))))

(advice-add 'rake--compile :around #'rake--compile-with-env-var)

;; Author: Thiago Araújo Silva
(defun rake-test (arg)
  (interactive "P")
  (let ((rake--env-vars "RAILS_ENV=test "))
    (rake arg)))
