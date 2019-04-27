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

(defconst tests-anywhere--test-functions
  '((rails-rspec . ((rerun . rails-rspec)
                    (verify-all . rspec-verify-all)
                    (verify-single . rspec-verify-single)))
    (ruby-rspec . ((rerun . rspec-rerun)
                   (verify-all . rspec-verify-all)
                   (verify-single . rspec-verify-single)))
    (elixir . ((rerun . alchemist-mix-rerun-last-test)
               (verify-all . alchemist-mix-test)))
    (lein-test . ((rerun . cider-test-run-loaded-tests)
                  (verify-all . cider-test-run-project-tests))))
  "Functions to run tests by project type and function type.")

(defvar tests-anywhere--state nil
  "A hash with the last command for each test run action.")

(cl-defun tests-anywhere--set-state (&key directory project-type)
  "Set state for last run tests within a qualified project."
  (setq tests-anywhere--state `((:directory . ,directory)
                                (:project-type . ,project-type))))

(defun tests-anywhere--get-state (key)
  "Get state for KEY."
  (cdr (assoc key tests-anywhere--state)))

(defun tests-anywhere--register-and-run (project-type func)
  "Run a test function and register the last command which has been run.

PROJECT-TYPE is the project type compatible with tests-anywhere.
FUNC is the test function to be run."
  (tests-anywhere--set-state :directory (buffer-file-name (current-buffer))
                             :project-type project-type)
  (tests-anywhere--run-registered func))

(defun tests-anywhere--run-registered (func)
  "Run FUNC in the last registered directory."
  (let* ((default-directory (tests-anywhere--get-state :directory)))
    (apply 'call-interactively (list func))))

(defun tests-anywhere--run (func-type)
  "Run function FUNC-TYPE.  Takes into account current project type."
  (let* ((tests-anywhere-project-type (tests-anywhere--project-type))
         (project-type (or tests-anywhere-project-type
                           (tests-anywhere--get-state :project-type)
                           (error "No prior test run (unknown project type)")))
         (func (tests-anywhere--get-function project-type func-type)))
    (if tests-anywhere-project-type
        (tests-anywhere--register-and-run project-type func)
      (tests-anywhere--run-registered func))))

(defun tests-anywhere--project-type ()
  "Is the current project known to tests-anywhere? If so, return it."
  (car (assoc (projectile-project-type) tests-anywhere--test-functions)))

(defun tests-anywhere--get-function (project-type func-type)
  "Get the function to run test by FUNC-TYPE and PROJECT-TYPE.

FUNC-TYPE can be: rerun, verify-all, etc."
  (let* ((funcs-by-type (cdr (assoc project-type tests-anywhere--test-functions)))
         (func (cdr (assoc func-type funcs-by-type))))
    func))

(defun tests-anywhere-rerun ()
  "Rerun the last test from anywhere."
  (interactive)
  (tests-anywhere--run 'rerun))

(defun tests-anywhere-verify-single ()
  "Rerun the last test from anywhere."
  (interactive)
  (tests-anywhere--run 'verify-single))

(defun tests-anywhere-verify-all ()
  "Run all project test from anywhere."
  (interactive)
  (tests-anywhere--run 'verify-all))

(provide 'tests-anywhere)
;;; tests-anywhere.el ends here
