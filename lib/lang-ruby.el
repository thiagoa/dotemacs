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
(defconst ruby-module-regex "^[\s]*\\(class\\|module\\) \\([^\s<]+\\)")

(defun ruby-mark-sexp (arg)
  (interactive "^p")
  (if (use-region-p)
      (progn (exchange-point-and-mark))
    (set-mark-command nil))
  (if (> arg 0)
      (dotimes (_ arg) (enh-ruby-forward-sexp))
    (enh-ruby-backward-sexp))
  (exchange-point-and-mark))

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
  (switch-to-buffer (get-buffer "*rspec-compilation*")))

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

(defun ruby-symbol-at-point ()
  "Figure out the Ruby symbol at point."
  (let ((tag (substring-no-properties (thing-at-point 'symbol))))
    (replace-regexp-in-string "^:\\([^:]+\\)" "\\1" tag)))

(defun ruby-find-definitions ()
  "Find definitions for the Ruby tag a point.
This function calls `xref-find-definitions` with a series
of likely tag candidates.  It reads the current buffer and
tries to figure out at what level of nesting you are to
build the tag candidates.  We assume your tags file is parsed
with ripper tags, including the --emacs and --extra=q tags."
  (interactive)
  (let* ((tag (ruby-symbol-at-point))
         (top-level-constant-p (string-prefix-p "::" tag))
         (tag (replace-regexp-in-string "^::" "" tag))
         (candidates (if top-level-constant-p () (ruby-tag-prefix-candidates)))
         (candidates (mapcar (lambda (c) (concat c "::" tag)) candidates))
         (candidates (append candidates (list tag)))
         (done nil))
    (while (and (not done) candidates)
      (ignore-errors
        (xref-find-definitions (pop candidates))
        (setq done t)))
    (if (not done) (error (concat "No definitions for " tag " found!")))))

(defun ruby-tag-prefix-candidates ()
  "Find Ruby modules until nesting level at point.
This is a simple regex-based function to return a list
of Ruby modules.  If you're under modules 'One' and 'Two',
this function will return '(list \"One::Two\" \"One\")."
  (save-excursion
    (let ((line (line-number-at-pos))
          (indent-level (if (eq major-mode 'enh-ruby-mode)
                            enh-ruby-indent-level
                          ruby-indent-level))
          (last-indent 0)
          symbol
          modules
          nesting)
      (goto-char (point-min))
      (cl-flet ((filter-by-indent (modules current-indent)
                                  (seq-remove
                                   (lambda (tuple)
                                     (let ((module-indent (car tuple)))
                                       (>= module-indent current-indent)))
                                   modules)))
        (while (not (eq (line-number-at-pos) line))
          (let ((found-module (re-search-forward ruby-module-regex
                                                 (line-end-position)
                                                 t)))
            (when found-module
              (let* ((current-indent (current-indentation))
                     (symbol (ruby-symbol-at-point))
                     (offset (abs (- last-indent current-indent)))
                     found-module)
                (if (<= current-indent last-indent)
                    (dotimes (_ (/ (+ indent-level offset) indent-level))
                      (pop nesting)))
                (setq found-module (append (reverse nesting) (list symbol)))
                (setq modules (filter-by-indent modules current-indent))
                (push (cons current-indent found-module) modules)
                (push symbol nesting)
                (setq last-indent current-indent))))
          (forward-line 1))
        (setq modules (filter-by-indent modules (current-indentation))))
      (mapcar (lambda (tuple)
                (let ((module-name (cdr tuple)))
                  (string-join module-name "::"))) modules))))

(defun tags ()
  "Reload tags."
  (interactive)
  (let* ((root (projectile-project-root))
         (gitroot (concat root ".git"))
         (gitdir (if (f-file? gitroot)
                     (concat root (with-temp-buffer
                                    (insert-file-contents gitroot)
                                    (buffer-string)))
                   gitroot))
         (ctags-bin (concat
                     (string-trim (replace-regexp-in-string
                                   "^gitdir: "
                                   ""
                                   gitdir))
                     "/hooks/ctags")))
    (if (f-file? ctags-bin)
        (progn
          (set-process-sentinel
           (start-process "tags-compilation"
                          "*Tags Compilation*"
                          (concat ctags-bin))
           (lambda (process msg)
             (when (memq (process-status process) '(exit signal))
               (if (eq (process-exit-status process) 0)
                   (notify-os "Tags generated successfully 👍" "Hero")
                 (notify-os "Tags generation FAILED! 👎" "Basso"))))))
      (error "No ctags found for this project"))))

(provide 'lang-ruby)
;;; lang-ruby.el ends here
