;;; buffer.el  --- TODO  -*- lexical-binding: t; -*-

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

(defvar killable-buffer-major-modes
  '(dired-mode))

(defvar killable-buffer-exceptions
  '("*scratch*"))

(defvar killable-buffer-patterns
  '("^TAGS"
    "^magit-"
    "^*.+*$"
    ".gz$"
    "^HEAD$"))

(defun kill-extraneous-buffers ()
  "Kill automatic and junk buffers, including dired buffers."
  (interactive)
  (dolist (buf (buffer-list) nil)
    (let ((bufname (buffer-name buf)))
      (unless (cl-some
               (lambda (b)
                 (string= bufname b))
               killable-buffer-exceptions)
        (if (or (cl-some
                 (lambda (pattern)
                   (string-match pattern bufname))
                 killable-buffer-patterns)
                (cl-some
                 (lambda (cur-mode)
                   (eq cur-mode (with-current-buffer buf major-mode)))
                 killable-buffer-major-modes))
            (kill-buffer buf)))))
  (message "Done."))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Copied from the crux package. Modified to skip annoying
;; vc-delete-file and do what I mean (dwim) please!
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it visits."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))
      (error "ERROR: No filename for this buffer"))))

(provide 'buffer)
;;; buffer.el ends here
