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

(defconst tests-anywhere--test-functions
  '((rails-rspec . ((rerun . rails-rspec)
                    (verify-all . rspec-verify-all)))
    (ruby-rspec . ((rerun . rspec-rerun)
                   (verify-all . rspec-verify-all)))
    (elixir . ((rerun . alchemist-mix-rerun-last-test)
               (verify-all . alchemist-mix-test)))
    (lein-test . ((rerun . cider-test-run-loaded-tests)
                  (verify-all . cider-test-run-project-tests))))
  "Functions to run tests by project type and function type.")

(defvar tests-anywhere--state
  (make-hash-table)
  "A hash with the last command for each test run action.")

(defun tests-anywhere--register-and-run (key func)
  "Run a test function and register the last command which has been run.

KEY is an identifier for the type of test that's being run.
FUNC is a test callback."
  (let ((command (make-tests-anywhere-command
                  :directory (projectile-project-root)
                  :function func)))
    (puthash key command tests-anywhere--state))
  (tests-anywhere--run-registered key))

(defun tests-anywhere--run-registered (key)
  "Look for a test function to be run by KEY."
  (let* ((state (or (gethash key tests-anywhere--state)
                    (error "No prior test run (unknown project type)")))
         (default-directory (tests-anywhere-command-directory state)))
    (if state
        (apply 'call-interactively
               (list (tests-anywhere-command-function state)))
      (message "No prior test run (unknown project type)"))))

(defun tests-anywhere--run (func-type)
  "Run function FUNC-TYPE.  Takes into account current project type."
  (let ((func (tests-anywhere--get-function func-type)))
    (if func
        (tests-anywhere--register-and-run func-type func)
      (tests-anywhere--run-registered func-type))))

(defun tests-anywhere--get-function (func-type)
  "Get the function to run test by FUNC-TYPE.

FUNC-TYPE can be: rerun, verify-all, etc."
  (let* ((project-type (projectile-project-type))
         (funcs-by-type (cdr (assoc project-type
                                    tests-anywhere--test-functions)))
         (func (cdr (assoc func-type
                           funcs-by-type))))
    func))

(defun tests-anywhere-rerun ()
  "Rerun the last test from anywhere."
  (interactive)
  (tests-anywhere--run 'rerun))

(defun tests-anywhere-verify-all ()
  "Run all project test from anywhere."
  (interactive)
  (tests-anywhere--run 'verify-all))

(provide 'tests-anywhere)
;;; tests-anywhere.el ends here
