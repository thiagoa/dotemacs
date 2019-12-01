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

(defvar last-ruby-project nil)

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
  (call-interactively 'ruby-mark-sexp)
  (exchange-point-and-mark)
  (next-line)
  (beginning-of-line))

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
  "A function decorator to register the last ruby project.

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

(provide 'lang-ruby)
;;; lang-ruby.el ends here
