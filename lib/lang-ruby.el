;;; lang-ruby.el  --- My Ruby extensions  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Code in here can still be improved.

;;; Code:

(require 'rspec-mode)
(require 'projectile-rails)
(require 'inf-ruby)
(require 'ext-elisp)
(require 'ext-compile)
(require 'string-inflection)

(defvar last-ruby-project nil)

(defun ruby-finish-test-compilation ()
  "Calback to be run after a compilation task finishes."
  (if (or rspec-last-failed-specs
          (with-current-buffer "*rspec-compilation*"
            (or (> compilation-num-errors-found 0)
                (save-excursion
                  (beginning-of-buffer)
                  (re-search-forward "\[0-9\]+ error occurred" nil t))
                (save-excursion
                  (beginning-of-buffer)
                  (re-search-forward "exited abnormally with code" nil t)))))
      (notify-os "Ruby RSpec" "Tests failed 👎" "Basso")
    (notify-os "Ruby RSpec" "Tests passed 👍" "Hero")))

(defun ruby-mark-inner-defun ()
  "Mark the inner contents of a method."
  (interactive)
  (call-interactively 'mark-defun)
  (while (not (looking-at "def"))
    (forward-char))
  (next-line)
  (beginning-of-line)
  (exchange-point-and-mark)
  (previous-line))

(defun ruby-mark-sexp (arg)
  (interactive "^p")
  (if (use-region-p)
      (progn (exchange-point-and-mark))
    (set-mark-command nil))
  (if (> arg 0)
      (dotimes (_ arg) (enh-ruby-forward-sexp))
    (enh-ruby-backward-sexp))
  (exchange-point-and-mark))

(defun ruby-kill-sexp (&optional arg)
  "Kill the sexp (balanced expression) following point.
With ARG, kill that many sexps after point.
Negative arg -N means kill N sexps before point.
This command assumes point is not in a string or comment."
  (interactive "p")
  (if (eq major-mode 'enh-ruby-mode)
      (let ((opoint (point)))
        (enh-ruby-forward-sexp (or arg 1))
        (kill-region opoint (point)))
    (kill-sexp arg)))

(defun ruby-mark-sexp-for-delete ()
  (interactive)
  (let ((is-bolp (bolp)))
    (call-interactively 'ruby-mark-sexp)
    (when is-bolp
      (exchange-point-and-mark)
      (next-line)
      (beginning-of-line)
      (exchange-point-and-mark))))

(defun ruby-duplicate-sexp-below (arg)
  "Duplicate Ruby sexp and place point at the start of duplicated sexp.
Point must be at the end of the Ruby sexp for this to work.
If given universal ARG, does not open a newline between sexps."
  (interactive "^p")
  (set-mark-command nil)
  (enh-ruby-backward-sexp)
  (beginning-of-line)
  (exchange-point-and-mark)
  (forward-line 1)
  (kill-ring-save (region-beginning) (region-end))
  (yank)
  (deactivate-mark)
  (forward-line -1)
  (end-of-line)
  (enh-ruby-backward-sexp)
  (back-to-indentation)
  (if (eq arg 1) (newline-and-indent)))

(defun within-last-ruby-project (&rest args)
  "A function decorator to register the last Ruby project.

ARGS is used to called the decorated function.

A function decorator (around advice) which registers and
remembers the last Ruby project a command has been run on.
When triggered in a non-Ruby project, it will run the command of
the last Ruby project.  This can be generalized further to work with
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

(defun rake-test ()
  "Run rake in test mode on Rails apps."
  (interactive)
  (let ((rails-env (getenv "RAILS_ENV")))
    (unwind-protect (progn (setenv "RAILS_ENV" "test")
                           (call-interactively 'rake))
      (setenv "RAILS_ENV" rails-env))))

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

(defun rspec-quit-pry ()
  "Quits Pry in the RSpec buffer if it's running."
  (interactive)
  (with-current-buffer "*rspec-compilation*"
    (if (equal major-mode 'rspec-compilation-mode)
        (inf-ruby-switch-from-compilation))
    (end-of-buffer)
    (ignore-errors (crux-kill-whole-line))
    (comint-send-eof)
    (inf-ruby-maybe-switch-to-compilation)))

(defun go-to-rspec-compilation-buffer ()
  "Go straight to rspec compilation buffer."
  (interactive)
  (let ((buffer (get-buffer "*rspec-compilation*")))
    (if buffer
        (switch-to-buffer buffer)
      (error "No rspec compilation buffer!"))))

(defun go-to-spec (func arg)
  "Go to spec file in compilation buffer.

Runs FUNC in compilation buffer until finding a spec file reference.
A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-file (simple-ilambda
               (while (progn
                        (call-interactively func)
                        (not (looking-at ".*_spec\.rb*"))))) arg))

(defun next-spec (&optional arg)
  "Go to next spec in compilation buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-spec 'compilation-next-file arg))

(defun previous-spec (&optional arg)
  "Go to previous spec in compilation buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-spec 'compilation-previous-file arg))

(defvar ruby-code-for-fully-qualified-name-current-indentation 0
  "Keep track of the current indentation during yasnippet expansion.
Read more about the purpose of this variable in
`ruby-code-for-fully-qualified-name-top'.")

(defun ruby-get-fully-qualified-name-parts-for-path ()
  "Return a list of fully-qualified Ruby class name parts for the current file.
Given a file path or buffer, determine what the class name is by
following Rail's path to module name convention. If dealing with a
spec file, ignore the '_spec.rb' suffix.

For example, if we're visitting a
app/models/first/second_third.rb buffer, this function would
return '(\"First\" \"SecondThird\")"
  (interactive)
  (let* ((relative-path (replace-regexp-in-string
                         (projectile-project-root)
                         ""
                         (or (buffer-file-name)
                             (buffer-name (current-buffer)))))
         (relative-path (replace-regexp-in-string "_spec\.rb$" "" relative-path))
         (relative-path (replace-regexp-in-string "\.rb$" "" relative-path))
         (parts (split-string relative-path "/"))
         (root-dir (car parts))
         (path (pcase root-dir
                 ("app" (cddr parts))
                 ("lib" (cdr parts))
                 ("spec" (cddr parts))
                 (_ (error "File doesn\'t belong to app or lib")))))
    (mapcar #'string-inflection-pascal-case-function path)))

(defun ruby-code-for-fully-qualified-name-top (&optional class-or-mod)
  "Return the Ruby code for the top part of the nested module definition.

CLASS-OR-MOD should be 'class' or 'module', depending on whether
you are generating code for a class or a module.

This function generates the top part of the nested module
skeleton for the current Ruby file. It is primarily meant to be
used with yasnippets (there is more than one cool snippet using
it). Unfortunately, we can't generate the full nested module code
with a single function because yas doesn't support dynamic
snippet generation. Instead, it supports elisp expansion but it
doesn't recognize dynamically expanded snippet placeholders.
Therefore, this functionality has to be divided into 3
functions (top, middle, and bottom) which keep track of the
current indentation with a global variable (hence not being
pure.)"
  (let* ((raw-seq (ruby-get-fully-qualified-name-parts-for-path))
         (namespace-seq (mapcar (lambda (p) (concat "module " p "\n"))
                                (butlast raw-seq)))
         (class-or-mod-seq (list (concat class-or-mod " " (car (last raw-seq)))))
         (full-seq (append namespace-seq class-or-mod-seq))
         (res (seq-reduce (lambda (acc str)
                            (let* ((indent-by (car acc))
                                   (str-acc (cdr acc))
                                   (indentation (make-string indent-by ?\s)))
                              (cons (+ indent-by 2) (concat str-acc indentation str))))
                          full-seq
                          (cons 0 ""))))
    (setq ruby-code-for-fully-qualified-name-current-indentation (car res))
    (cdr res)))

(defun ruby-code-for-fully-qualified-name-middle ()
  "Return the Ruby code for the middle part of the nested module definition.
The middle part is nothing but the leading indentation; the yas
snippet will usually concatenate the indentation with $0 to
determine where the cursor should stop after expanding the
snippet.

Read more about this function in `ruby-code-for-fully-qualified-name-top'."
  (make-string ruby-code-for-fully-qualified-name-current-indentation ?\s))

(defun ruby-code-for-fully-qualified-name-bottom ()
  "Return the Ruby code for the bottom part of the nested module definition.
The bottom part is nothing but 'end' statements.

Read more about this function in `ruby-code-for-fully-qualified-name-top'."
  (let* ((raw-seq (ruby-get-fully-qualified-name-parts-for-path))
         (indent-by ruby-code-for-fully-qualified-name-current-indentation))
    (string-trim-right
     (seq-reduce (lambda (acc _)
                   (setq indent-by (- indent-by 2))
                   (concat acc (make-string indent-by ?\s) "end\n"))
                 raw-seq
                 "")
     "\n")))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
