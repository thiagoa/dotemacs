;;; -*- lexical-binding: t -*-

(require 'ido-goto-symbol)
(require 'cl)
(require 'cl-extra)
(require 'move-text)

(defalias 'ff  'find-file)
(defalias 'e   'eval-buffer)
(defalias 'keb 'kill-extraneous-buffers)
(defalias 'repl 'ielm)

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;

(defun comment-or-uncomment-line-or-region ()
  "Comments or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

;; Modified to add support for other languages
(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call 「python x.py」 in a shell.
Output is printed to buffer “*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript, TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java, Clojure.
File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'
Version 2018-07-01"
  (interactive)
  (let (
        ($outputb "*xah-run output*")
        (resize-mini-windows nil)
        ($suffix-map
         ;; (‹extension› . ‹shell program name›)
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python")
           ("py3" . ,(if (string-equal system-type "windows-nt") "c:/Python32/python.exe" "python3"))
           ("rb" . "ruby")
           ("go" . "go run")
           ("hs" . "runhaskell")
           ("js" . "node")
           ("mjs" . "node --experimental-modules ")
           ("ts" . "tsc") ; TypeScript
           ("tsx" . "tsc")
           ("sh" . "bash")
           ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("ex" . "elixir")
           ("java" . "javac")
           ("c" . "gcc")
           ;; ("pov" . "/usr/local/bin/povray +R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640")
           ))
        $fname
        $fSuffix
        $prog-name
        $cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fname-sans-extension (file-name-sans-extension (file-name-nondirectory $fname)))
    (setq $fSuffix (file-name-extension $fname))
    (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \""   $fname "\""))
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
      (if (fboundp 'xah-ts-compile-file)
          (xah-ts-compile-file current-prefix-arg)
        (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (message "No recognized program file suffix for this file."))))
     ((string-equal $fSuffix "go")
      ;; (when (fboundp 'gofmt) (gofmt) )
      (shell-command $cmd-str $outputb ))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "java %s" $fname-sans-extension) $outputb )))
     ((string-equal $fSuffix "c")
      (progn
        (shell-command
         (format
          "gcc -o %s %s && ./%s"
          $fname-sans-extension
          $fname
          $fname-sans-extension)
         $outputb)))
     (t (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (message "No recognized program file suffix for this file."))))))

;;;;;;;;;;;;;;;;
;; Minibuffer ;;
;;;;;;;;;;;;;;;;

(defun name-of-the-file ()
  "From the minibuffer, gets the name of the file the current buffer is based on."
  (interactive)
  (insert (concat
           "\""
           (buffer-file-name (window-buffer (minibuffer-selected-window)))
           "\"")))

;;;;;;;;;;;;;
;; ibuffer ;;
;;;;;;;;;;;;;

(require 'ibuf-ext)

(define-ibuffer-filter marked-buffers
    "Limit current view to marked buffers"
  (:description "marked buffers"
                :reader nil)
  (let ((bufs (ibuffer-get-marked-buffers)))
    (member buf bufs)))

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
  (crux-smart-open-line-above)
  (next-line))

;; Author: Thiago Araújo Silva
(defun bundle ()
  (interactive)
  (bundle-install))

(defvar last-ruby-project nil)

;; Author: Thiago Araújo Silva
(defun within-last-ruby-project (&rest args)
  "A function decorator (around advice) which registers and
   remembers the last Ruby project a command has been run on.
   When triggered in a non-Ruby project, it will run the command of
   the last Ruby project. This can be generalized further to work with
   other project types, thus uncoupling from Ruby."
  (if (file-exists-p (concat (projectile-rails-root) "Gemfile"))
      (progn
        (setq last-ruby-project default-directory)
        (apply (car args) (cdr args)))
    (if last-ruby-project
        (let ((default-directory last-ruby-project))
          (apply (car args) (cdr args)))
      (message "No previous Ruby project"))))

(advice-add 'projectile-rails-console :around #'within-last-ruby-project)

;; Author: Thiago Araújo Silva
(defun rspec-toggle-compilation-mode ()
  "Toggle compilation mode for future rspec executions.
With compilation mode disabled, you will be able to interact with
a debugger such as Pry or hit control c twice to force RSpec to terminate
a test run.  With it enabled, you will be able to navigate through
error stack traces and have all compile functionality at your
disposal.

Also, this command looks for an rspec buffer and toggles
compilation mode in it immediately."
  (interactive)
  (toggle-list-element
   rspec-before-verification-hook
   'inf-ruby-switch-from-compilation
   (lambda (on) (message (concat "Compilation mode is " (if on "ON" "OFF")))))
  (when (setq next-error-last-buffer (next-error-find-buffer))
    (with-current-buffer next-error-last-buffer
      (if (equal major-mode 'inf-ruby-mode)
          (inf-ruby-maybe-switch-to-compilation)
        (inf-ruby-switch-from-compilation)))))

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

;;;;;;;;;;;;;;;;;;;
;; Mark commands ;;
;;;;;;;;;;;;;;;;;;;

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
   Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
  This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

;; https://www.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/
(defcustom tmp-buffer-mode-alist
  '((?o . org-mode)
    (?t . text-mode)
    (?m . markdown-mode)
    (?r . enh-ruby-mode)
    (?e . emacs-lisp-mode)
    (?l . lisp-interaction-mode)
    (?s . sql-mode)
    (?c . clojure-mode))
  "List of major modes for temporary buffers and their hotkeys."
  :type '(alist :key-type character :value-type symbol))

;; https://www.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/
(defun tmp-buffer (mode)
  "Open temporary buffer in specified major mode."
  (interactive "c")
  (if (eq mode ?\C-h)
      (with-output-to-temp-buffer "*Help*"
        (princ "Temporary buffers:\n\nKey\tMode\n")
        (dolist (km tmp-buffer-mode-alist)
          (princ (format " %c\t%s\n" (car km) (cdr km)))))
    (let ((buf (generate-new-buffer "*tmp*")))
      (with-current-buffer buf
        (let ((mode-func (cdr (assoc mode tmp-buffer-mode-alist))))
          (if mode-func
              (funcall mode-func)
            (error "No such mode"))))
      (pop-to-buffer buf))))

;; Author: Thiago Araújo Silva
(defun notify-os (message sound)
  "Send a notification to macOS.
Requires terminal-notifier (install it via homebrew).
MESSAGE is the notification message; SOUND is the sound that will be played."
  (shell-command
   (concat
    "bash -c -l 'echo " message " | terminal-notifier -sound "
    sound
    "'")))

(defun finish-test-compilation ()
  "Calback to be run after rspec finishes.
The exit code verification method can still be improved."
  (if (= compilation-num-errors-found 0)
      (notify-os "Tests passed 👍" "Hero")
    (notify-os "Tests failed 👎" "Basso")))

(provide 'functions)
