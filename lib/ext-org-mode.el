;;; ext-org-mode.el  --- TODO  -*- lexical-binding: t; -*-

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

(require 'org-download)

(defun org-download--dir ()
  "Return the directory path for image storage.

WARNING: Override (\"monkey path\") to take into account `org-journal-mode'.
The path is composed from `org-download--dir-1' and `org-download--dir-2'.
The directory is created if it didn't exist before."
  (if (or (eq major-mode 'org-mode) (eq major-mode 'org-journal-mode))
      (let* ((part1 (org-download--dir-1))
             (part2 (org-download--dir-2))
             (dir (if part2
                      (format "%s/%s" part1 part2)
                    part1)))
        (unless (file-exists-p dir)
          (make-directory dir t))
        dir)
    default-directory))

(defun org-download-remote-images ()
  "Downloads remote images with `org-download'.
Replaces remote links with local lines."
  (interactive)
  (let ((links (org-element-map (org-element-parse-buffer) 'link
                 (lambda (link)
                   (let ((type (org-element-property :type link)))
                     (when (or (string= type "http") (string= type "https"))
                       (let* ((path (org-element-property :path link))
                              (ext (downcase (or (file-name-extension path) ""))))
                         (when (member ext '("jpg" "jpeg" "gif" "png"))
                           (list :url (concat type ":" path)
                                 :ext ext
                                 :begin (org-element-property :begin link)
                                 :end (org-element-property :end link))))))))))
    (dolist (i (reverse links))
      (goto-char (plist-get i :begin))
      (delete-region (plist-get i :begin) (plist-get i :end))
      (org-download-image (plist-get i :url))
      (insert (concat "[[" org-download-path-last-file "]]")))))

(defun org-journal-dired ()
  "Open org journal folder in dired."
  (interactive)
  (dired org-journal-dir))

(provide 'ext-org-mode)
;;; ext-org-mode.el ends here
