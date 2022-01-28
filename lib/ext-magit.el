;;; ext-magit.el  --- TODO  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Thiago Araújo Silva

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

;; Monkey patched this function to modify the default rebase target
(defun magit-rebase-branch (target args)
  "Rebase the current branch onto a branch read
in the minibuffer. All commits that are reachable from `HEAD'
but not from the selected branch TARGET are being rebased."
  (interactive (list (magit-read-other-branch-or-commit "Rebase onto" nil "origin/main")
                     (magit-rebase-arguments)))
  (message "Rebasing...")
  (magit-git-rebase target args)
  (message "Rebasing...done"))

(provide 'ext-magit)
;;; ext-magit.el ends here
