;;; wsl.el  --- TODO  -*- lexical-binding: t; -*-

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

(when (wsl?)
  (defun wsl-copy (start end)
    (interactive "r")
    (shell-command-on-region start end "clip.exe")
    (deactivate-mark))

  (defun wsl-paste ()
    (interactive)
    (let ((clipboard
           (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2> /dev/null")))
      (setq clipboard (replace-regexp-in-string "\r" "" clipboard)) ; Remove Windows ^M characters
      (setq clipboard (substring clipboard 0 -1)) ; Remove newline added by Powershell
      (insert clipboard))))

(provide 'wsl)
;;; wsl.el ends here
