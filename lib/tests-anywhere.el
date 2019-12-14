;;; tests-anywhere.el  --- Run tests  -*- lexical-binding: t; -*-

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

(defconst tests-anywhere--test-functions
  '((rails-rspec . ((rerun . rspec-rerun)
                    (verify-all . rspec-verify-all)
                    (verify-single . rspec-verify-single)))
    (ruby-rspec . ((rerun . rspec-rerun)
                   (verify-all . rspec-verify-all)
                   (verify-single . rspec-verify-single)))
    (elixir . ((rerun . alchemist-mix-rerun-last-test)
               (verify-all . alchemist-mix-test)))
    (lein-test . ((rerun . cider-test-run-loaded-tests)
                  (verify-all . cider-test-run-project-tests))))
  "Mapping of 'project-type => test-function-type => test-function'.")

(defvar tests-anywhere--state nil
  "Remembers the project where the last test function ran.
How this state is used: if running tests from an unsupported
project type, makes it run it for the last *known project*
instead.")

(cl-defun tests-anywhere--set-state (&key directory project-type)
  "Register state for the last test function ran."
  (setq tests-anywhere--state `((:directory . ,directory)
                                (:project-type . ,project-type))))

(defun tests-anywhere--get-state (key)
  "Get state for the last test function ran.
KEY is the information to retrieve from state, for example 'directory'."
  (cdr (assoc key tests-anywhere--state)))

(defun tests-anywhere--register-and-run (project-type func)
  "Run test FUNC and record info about it in 'state'.
PROJECT-TYPE is a project type (as per projectile) compatible
with tests-anywhere.  See 'tests-anywhere--test-functions' for
more info.  FUNC is the test function to be run."
  (tests-anywhere--set-state :directory (buffer-file-name (current-buffer))
                             :project-type project-type)
  (tests-anywhere--run-registered func))

(defun tests-anywhere--run-registered (func)
  "Run FUNC under the last registered project."
  (let* ((default-directory (or (tests-anywhere--get-state :directory)
                                default-directory)))
    (apply 'call-interactively (list func))))

(defun tests-anywhere--run (type)
  "Run test function.
Lookup the test function for TYPE and try to figure out the current
project.  TYPE can be 'rerun, 'verify-all, 'verify-single, etc."
  (let* ((tests-anywhere-project-type (tests-anywhere--project-type))
         (project-type (or tests-anywhere-project-type
                           (tests-anywhere--get-state :project-type)
                           (error "No prior test run (unknown project type)")))
         (func (tests-anywhere--get-function project-type type)))
    (if tests-anywhere-project-type
        (tests-anywhere--register-and-run project-type func)
      (tests-anywhere--run-registered func))))

(defun tests-anywhere--project-type ()
  "Is the current project known to tests-anywhere? If so, return its type."
  (car (assoc (projectile-project-type) tests-anywhere--test-functions)))

(defun tests-anywhere--get-function (project-type type)
  "What test function should I run for PROJECT-TYPE and TYPE?
TYPE is the type of the function, such as 'rerun, 'verify-single, etc."
  (let* ((funcs-by-type (cdr (assoc project-type tests-anywhere--test-functions)))
         (func (cdr (assoc type funcs-by-type))))
    func))

(defun tests-anywhere-rerun ()
  "Rerun last test."
  (interactive)
  (tests-anywhere--run 'rerun))

(defun tests-anywhere-verify-single ()
  "Run test at point."
  (interactive)
  (tests-anywhere--run 'verify-single))

(defun tests-anywhere-verify-all ()
  "Run every test."
  (interactive)
  (tests-anywhere--run 'verify-all))

(provide 'tests-anywhere)
;;; tests-anywhere.el ends here
