;;; config-base.el --- Functions to manage config and packages.  -*- lexical-binding: t; -*-
;;
;; Copyright © 2019 Thiago Araújo Silva
;;
;; Author: Thiago Araújo Silva

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Simple functions to manage my config and packages.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'package)

(defconst emacs-d
  (concat (expand-file-name
           (concat
            (file-name-directory (file-chase-links load-file-name))
            ".."))
          "/")
  "The Emacs main directory.")

(defun reload-packages ()
  "Reloads my packages file."
  (load "packages.el"))

(defun reload-config ()
  "Reload my configuration file."
  (interactive)
  (load (expand-file-name "init.el" emacs-d)))

(defun pac-install ()
  "Install packages specified in config/packages.el."
  (interactive)
  (if buffer-file-name (save-buffer))
  (package-refresh-contents)
  (reload-packages)
  (dolist (package package-selected-packages)
    (unless (package-installed-p package)
      (package-install package))))

(defun pac-update ()
  "Update currently installed packages."
  (interactive)
  (save-buffer)
  (package-refresh-contents)
  (save-window-excursion
    (package-list-packages t)
    (package-menu-mark-upgrades)
    (package-menu-execute t)))

(defun pac-autoremove ()
  "Auto remove packages.  Reloads packages.el first of all."
  (interactive)
  (reload-packages)
  (package-autoremove))

(reload-packages)

(unless (package-installed-p pivot-package)
  (pac-install))

(provide 'config-base)
;;; config-base.el ends here
