;;; window-management.el  --- Window management functions  -*- lexical-binding: t; -*-

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

(defun kill-this-buffer-and-close-window ()
  "Kill current buffer and close window."
  (interactive)
  (kill-buffer (current-buffer))
  (delete-window))

(defun kill-other-buffer-and-close-window ()
  "Kill buffer from other window and close it."
  (interactive)
  (other-window 1)
  (kill-this-buffer-and-close-window))

(defun kill-other-buffer-and-keep-window ()
  "Kill buffer from other window and close it."
  (interactive)
  (other-window 1)
  (kill-buffer (current-buffer))
  (other-window -1))

(provide 'window-management)
;;; window-management.el ends here
