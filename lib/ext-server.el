;;; ext-server.el  --- Emacs server extensions  -*- lexical-binding: t; -*-

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

(require 'server)

(defun run-server ()
  "Run the Emacs server if it is not running."
  (require 'server)
  (unless (server-running-p)
    (server-start)))

(defun my-server-edit ()
  "A smarter server-edit.

Less noisy and switches to compilation buffer (supposing the
external edit was triggered from a compilation buffer)."
  (interactive)
  (save-buffer)
  (call-interactively 'server-edit)
  (let ((buf (next-error-find-buffer)))
    (dolist (win (window-list) nil)
      (if (eq buf (window-buffer win))
          (select-window win)))
    (switch-to-buffer buf)))

(provide 'ext-server)
;;; ext-server.el ends here
