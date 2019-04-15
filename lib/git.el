;;; git.el  --- TODO  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'magit)
(require 'git-link)

;; Author: Thiago Araújo Silva
(defun open-pr ()
  "Open a PR for the current project.

Depends on the 'git pr' shell command."
  (interactive)
  (run-under-project-root (lambda () (shell-command "git pr"))))

(defun cycle-magit-buffers (callback)
  "Cycles magit buffers with CALLBACK until finding it.

Examples of callbacks can be 'next-buffer and 'previous-buffer"
  (switch-to-magit-window)
  (let (found (start-buffer (current-buffer)))
    (while (not found)
      (funcall callback)
      (let ((cur-buffer (current-buffer)))
        (if (or
             (string-prefix-p "magit:" (buffer-name cur-buffer))
             (eq cur-buffer start-buffer))
            (setq found t))))))

(defun cycle-magit-buffers-forward ()
  "Cycle magit buffers forward."
  (interactive)
  (cycle-magit-buffers 'next-buffer))

(defun cycle-magit-buffers-backward ()
  "Cycle magit buffers backward."
  (interactive)
  (cycle-magit-buffers 'previous-buffer))

(defun switch-to-magit-window ()
  "Try to find a (visible) magit window and switch to it."
  (unless (magit-buffer? (buffer-name))
    (dolist (win (window-list) nil)
      (if (magit-buffer? (buffer-name (window-buffer win)))
          (select-window win)))))

(defun magit-buffer? (buffer)
  "Is this a magit BUFFER?"
  (string-match "^magit:" buffer))

(defun magit-commit-this-buffer ()
  "Commit the changeset pertaining to the current buffer."
  (interactive)
  (magit-unstage-all)
  (magit-stage)
  (magit-commit))

(defun git-link-default-branch ()
  (interactive)
  (git-link-branch "develop"))

(defun git-link-branch (branch)
  "Get a github link for the given BRANCH at the current line."
  (interactive (list (magit-completing-read
                      "Branch"
                      (magit-list-local-branch-names))))
  (let* ((remote (git-link--select-remote))

         (git-link-default-branch branch)
         (region (when buffer-file-name (git-link--get-region)))
         (start (car region))
         (end (cadr region)))
    (git-link remote start end)))

(provide 'git)
;;; git.el ends here
