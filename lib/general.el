;;; general.el  --- General helpers  -*- lexical-binding: t; -*-

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

(defun toggle-option-key (&optional key)
  "Toggle meta between meta and option.

If specifying KEY, set the \"ns-option-modifier\" accordingly."
  (interactive)
  (or key (setq key (if (eq ns-option-modifier 'meta) 'none 'meta)))
  (if (eq key 'none)
      (progn (setq ns-option-modifier 'none) (message "Changed to macOS option"))
    (progn (setq ns-option-modifier 'meta) (message "Changed to Emacs meta"))))

(defun shell-command-output (command)
  "Run shell COMMAND and return output."
  (replace-regexp-in-string
   "\n$"
   ""
   (shell-command-to-string command)))

(provide 'general)
;;; general.el ends here
