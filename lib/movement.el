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

(defun rspec-file-p ()
  "Return non-nil if current file is an rspec file."
  (and (string-match "_spec\.rb$" (or (buffer-file-name) ""))))

(defun my-beginning-of-line ()
  "Move to the beginning of line or previous function.

Running this function once goes to the beginning of line.
Running this function again goes to the previous function.
If rspec-mode is enabled, goes to the previous sentence."
  (interactive)
  (cl-labels ((beginning-of-line-p ()
                                   (= (point) (line-beginning-position))))
    (cond
     ((beginning-of-line-p) (progn (if (rspec-file-p) (backward-sentence) (beginning-of-defun))
                                   (call-interactively 'move-beginning-of-line)))
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
     ((end-of-line-p) (if (rspec-file-p) (forward-sentence) (end-of-defun)))
     (t (call-interactively 'move-end-of-line)))))

(defun go-to-alternate-buffer ()
  "Alternate between the current buffer and the previous."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer))))

(defun dired-file-at-point-dwim ()
  "Jump to file at point in dired."
  (interactive)
  (let* ((file (ffap-string-at-point)))
    (or (file-exists-p file) (error "Not a file!"))
    (dired-jump nil file)))

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

(defun iy-go-to-char--command--without-case-fold (orig-fun &rest args)
  "Advice to add case fold to iy-go-to-char--command. Takes ORIG-FUN and ARGS."
  (let ((case-fold-search nil))
    (apply orig-fun args)))

(progn (advice-mapc (lambda (advice _props) (advice-remove 'iy-go-to-char--command advice)) 'yi-go-to-char--command)
       (advice-add 'iy-go-to-char--command :around 'iy-go-to-char--command--without-case-fold))

(provide 'movement)
;;; movement.el ends here
