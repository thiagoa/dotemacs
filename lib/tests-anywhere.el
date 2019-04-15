;;; tests-anywhere.el  --- Run tests in any file  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'cl-macs)
(require 'projectile)
(require 'cider)

(cl-defstruct tests-anywhere-command directory function)

(defvar tests-anywhere-state
  (make-hash-table)
  "A hash with the last command for each test run action.")

(defun tests-anywhere-register-and-run (key function)
  "Run a test function and register the last command which has been run.

KEY is an identifier for the type of test that's being run.
FUNCTION is a test callback."
  (let ((command (make-tests-anywhere-command
                  :directory (projectile-project-root)
                  :function function)))
    (puthash key command tests-anywhere-state))
  (tests-anywhere-run-registered key))

(defun tests-anywhere-run-registered (key)
  "Look for a test function to be run by KEY."
  (let* ((state (or (gethash key tests-anywhere-state) (error "No prior test run")))
         (default-directory (tests-anywhere-command-directory state)))
    (if state
        (funcall (tests-anywhere-command-function state))
      (message "No prior test run"))))

(defun tests-anywhere-rerun ()
  "Rerun the last test from anywhere."
  (interactive)
  (let ((func (tests-anywhere--rerun-get-function)))
    (if func
        (tests-anywhere-register-and-run 'rerun func)
      (tests-anywhere-run-registered 'rerun))))

(defun tests-anywhere--rerun-get-function ()
  "Get the function to rerun the last test depending on project type."
  (pcase (projectile-project-type)
    ('rails-rspec 'rspec-rerun)
    ('elixir 'alchemist-mix-rerun-last-test)
    ('lein-test (lambda () (cider-test-run-loaded-tests nil)))))

(defun tests-anywhere-verify-all ()
  "Run all project test from anywhere."
  (interactive)
  (let ((func (tests-anywhere--verify-all-get-function)))
    (if func
        (tests-anywhere-register-and-run 'verify-all func)
      (tests-anywhere-run-registered 'verify-all))))

(defun tests-anywhere--verify-all-get-function ()
  "Get the function to run all project test depending on project type."
  (pcase (projectile-project-type)
    ('rails-rspec 'rspec-verify-all)
    ('elixir 'alchemist-mix-test)
    ('lein-test (lambda () (cider-test-run-project-tests nil)))))

(provide 'tests-anywhere)
;;; tests-anywhere.el ends here
