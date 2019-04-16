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

(defvar last-ruby-project nil)
(defvar
  rake--env-vars
  ""
  "Environment variables to run rake with.")

(defun ruby-mark-block ()
  "Mark a Ruby block.  I bet this can be improved."
  (interactive)
  (ruby-beginning-of-block)
  (move-beginning-of-line 1)
  (push-mark nil t t)
  (ruby-end-of-block)
  (move-beginning-of-line 1)
  (forward-line 1))

(defun ruby-duplicate-block-below ()
  "Duplicates nearest Ruby block below.  I bet this can be improved."
  (interactive)
  (ruby-mark-block)
  (kill-ring-save (region-beginning) (region-end))
  (move-beginning-of-line 1)
  (yank)
  (deactivate-mark)
  (forward-line -1)
  (ruby-beginning-of-block)
  (back-to-indentation)
  (crux-smart-open-line-above)
  (forward-line 1))

;; Author: Thiago Araújo Silva
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

(defun rake--compile-with-env-var (orig-fun root task mode)
  "Allow overriding rake env variables.  See rake--env-vars.

ORIG-FUN, ROOT, TASK, and MODE are delegated to the original rake
function."
  (let ((task (concat rake--env-vars task)))
    (apply orig-fun `(,root ,task ,mode))))

(advice-add 'rake--compile :around #'rake--compile-with-env-var)

(defun rake-test (arg)
  "Run rake in test environment.

ARG is the universal argument delegated to rake."
  (interactive "P")
  (let ((rake--env-vars "RAILS_ENV=test "))
    (rake arg)))

(defun go-to-rspec-compilation-buffer ()
  "Go straight to rspec compilation buffer."
  (interactive)
  (switch-to-buffer (get-buffer "*rspec-compilation*")))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
