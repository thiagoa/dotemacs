;;; movement.el  --- TODO  -*- lexical-binding: t; -*-

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

(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(defun go-to-rspec-compilation-buffer ()
  "Go straight to rspec compilation buffer."
  (interactive)
  (switch-to-buffer (get-buffer "*rspec-compilation*")))

(defun go-to-alternate-buffer ()
  "Switch to alternate (last) buffer."
  (interactive)
  (switch-to-buffer nil))

(provide 'movement)
;;; movement.el ends here
