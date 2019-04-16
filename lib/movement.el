;;; movement.el  --- Movement functions  -*- lexical-binding: t; -*-

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

(require 'ffap)
(require 'dired-x)

(defun my-beginning-of-line ()
  "Move to the beginning of line or previous function.

Running this function once goes to the beginning of line.
Running this function again goes to the previous function.
If rspec-mode is enabled, goes to the previous sentence."
  (interactive)
  (cl-labels ((beginning-of-line-p ()
                                   (= (point) (line-beginning-position))))
    (cond
     ((and (boundp 'rspec-mode) (beginning-of-line-p)) (backward-sentence))
     ((beginning-of-line-p) (beginning-of-defun))
     (t (call-interactively 'move-beginning-of-line)))))

(defun my-end-of-line ()
  "Move to the end of line or next function.

Running this function once goes to the end of line.
Running this function again goes to the next function.
If rspec-mode is enabled, goes to the next sentence."
  (interactive)
  (cl-labels ((end-of-line-p ()
                             (= (point) (line-end-position))))
    (cond
     ((and (boundp 'rspec-mode) (end-of-line-p)) (forward-sentence))
     ((end-of-line-p) (end-of-defun))
     (t (call-interactively 'move-end-of-line)))))

(defun go-to-alternate-buffer ()
  "Switch to alternate (last) buffer."
  (interactive)
  (switch-to-buffer nil))

(defun dired-file-at-point-dwim ()
  "Jump to file at point in dired."
  (interactive)
  (dired-jump nil (ffap-string-at-point 'file)))

(defun find-file-at-point-dwim (&optional filename)
  "Find file at point expanding any shell variables it encounters.

Optionally takes the FILENAME."
  (interactive)
  (let* ((name (or filename (ffap-string-at-point 'file)))
         (fname (substitute-in-file-name (expand-file-name name))))
    (if (and name fname (file-exists-p fname))
        (find-file fname)
      (find-file-at-point filename))))

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun push-mark-no-activate ()
  "Push `point' to `mark-ring', does not activate the region.

Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))

;; https://www.masteringemacs.org/article/fixing-mark-commands-transient-mark-mode
(defun jump-to-mark ()
  "Jump to the local mark, respecting the `mark-ring' order.

This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))

(provide 'movement)
;;; movement.el ends here
