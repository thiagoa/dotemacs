;;; ext-rspec-mode.el  --- TODO  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>
;; Version: 0.0.1

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

;;; rspec-mode monkey patches. Currently, the functions herein
;;; automate the creation of files when toggling between spec and file
;;; and the file doesn't exist. Who knows one day I'll open a PR in
;;; rspec-mode with these changes.

;;; Code:

(require 'rspec-mode)

(defun rspec-spec-or-target (&optional force-create)
  "Return a filename representing the spec or target file.
If given FORCE-CREATE, forces the creation of a file if it doesn't exist."
  (if (rspec-buffer-is-spec-p)
      (let ((rspec-primary-source-dirs (if force-create
                                           (list (completing-read (concat "Where to create file (app)? ")
                                                                  rspec-primary-source-dirs nil nil nil nil
                                                                  "app"))
                                         rspec-primary-source-dirs)))
        (or (rspec-target-file-for (buffer-file-name) force-create)
            (error (concat "No alt file for " (file-name-nondirectory (buffer-file-name))))))
    (rspec-spec-file-for (buffer-file-name))))

(defun rspec-target-file-for (a-spec-file-name first-result)
  "Find the target for A-SPEC-FILE-NAME.
If given FIRST-RESULT, return the first result regardless if file exists or not."
  (cl-loop for extension in (list "rb" "rake")
           for candidate = (rspec-targetize-file-name a-spec-file-name
                                                      extension)
           for filename = (cl-loop for dir in rspec-primary-source-dirs
                                   for target = (replace-regexp-in-string
                                                 "/spec/"
                                                 (concat "/" dir "/")
                                                 candidate)
                                   if (or first-result (file-exists-p target))
                                   return target)))

(defun rspec-toggle-spec-and-target (&optional force-create)
  "Switch to the spec or the target file for the current buffer.
If given FORCE-CREATE, force creation of the buffer if it doesn't exist
If the current buffer is visiting a spec file, switches to the
target, otherwise the spec."
  (interactive "P")
  (find-file (rspec-spec-or-target force-create)))

(provide 'ext-rspec-mode)
;;; ext-rspec-mode.el ends here
