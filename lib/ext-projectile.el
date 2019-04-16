;;; ext-projectile.el  --- Projectile extensions  -*- lexical-binding: t; -*-

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

;; Will I realize one day that this is already implemented by projectile? Is it?

;;; Code:

(require 'projectile)
(require 'helm-projectile)
(require 'ext-elisp)

(defun run-under-project-root (command)
  "Run COMMAND under the current project."
  (execute-command-under-dir (projectile-project-root) command))

(defun execute-extended-command-under-dir (dir)
  "Execute extended command under DIR.

 For use with projectile-switch-project."
  (execute-command-under-dir dir 'execute-extended-command nil))

(defun execute-projectile-rails-console-under-dir (dir)
  "Execute projectile-rails-console under DIR.

For use with projectile-switch-project."
  (execute-command-under-dir dir 'projectile-rails-console nil))

(defun execute-helm-projectile-find-file-under-dir (dir)
  "Execute helm-projectile-find-file under DIR.

For use with helm-projectile-find-file."
  (execute-command-under-dir dir 'helm-projectile-find-file))

(provide 'ext-projectile)
;;; ext-projectile.el ends here
