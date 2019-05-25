;;; text-editing.el  --- General text editing functions  -*- lexical-binding: t; -*-

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

(require 'paredit)
(require 'xah)

(defun move-text-up-and-indent ()
  "Move line or region up then indent."
  (interactive)
  (call-interactively 'move-text-up)
  (unless (use-region-p) (indent-according-to-mode)))

(defun move-text-down-and-indent ()
  "Move line or region down then indent."
  (interactive)
  (call-interactively 'move-text-down)
  (unless (use-region-p) (indent-according-to-mode)))

(defun kill-variable-assignment ()
  "Kill a variable assignment but preserves the expression.

For example, by running the command over \"a = foo(1)\" we would
end up with just \"foo(1)\". Works in any part of the line."
  (interactive)
  (let
      ((start (progn (back-to-indentation) (point)))
       (end (save-excursion
              (while (not (string= (string (char-after)) "="))
                (forward-char))
              (point))))
    (delete-region start (+ 2 end))))

(defun smart-open-line-below-and-above ()
  "Open a space for editing with surrounding blank lines.

Useful to define functions in between spaces."
  (interactive)
  (crux-smart-open-line-above)
  (crux-smart-open-line nil))

(defun replace-region ()
  "Sensibly replace the region making the cursor ready for insertion.
If no region set, delete char at point."
  (interactive)
  (if (region-active-p)
      (let* ((point-at-line-beginning-p (eq (line-beginning-position) (point)))
             (mark-line-beginning-position (save-excursion
                                             (goto-char (mark))
                                             (line-beginning-position)))
             (mark-at-line-beginning-p (eq mark-line-beginning-position (mark))))
        (call-interactively 'kill-region)
        (call-interactively 'indent-for-tab-command)
        (when (and point-at-line-beginning-p mark-at-line-beginning-p)
          (call-interactively 'crux-smart-open-line-above)))
    (call-interactively 'delete-char)))

(defun top-join-line ()
  "Join the line below to the current line using a space as separator.

If a region is active, joins together the lines corresponding to the
region."
  (interactive)
  (cl-flet ((join-line () (delete-indentation 1)))
    (cond
     ((use-region-p)
      (let ((n (abs (-
                     (line-number-at-pos (point))
                     (line-number-at-pos (mark))))))
        (when (= (point) (line-beginning-position)) (setq n (1- n)))
        (when (> (point) (mark)) (exchange-point-and-mark))
        (dotimes (_ n) (join-line))))
     (t (join-line)))))

(defun mark-current-line (arg)
  "Mark the current line.
Take ARG universal argument to mark N lines."
  (interactive "p")
  (move-beginning-of-line 1)
  (set-mark
   (save-excursion
     (dotimes (_ arg) (forward-line))
     (move-beginning-of-line 1)
     (point)))
  (exchange-point-and-mark))

(defun comment-or-uncomment-line-or-region ()
  "Comment or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun clear-line ()
  "Sensibly clears the current line."
  (interactive)
  (call-interactively 'indent-for-tab-command)
  (call-interactively 'back-to-indentation)
  (when (not (current-line-empty-p)) (call-interactively (key-binding "\C-k"))))

(defun current-line-empty-p ()
  "Return non-nil if the current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(provide 'text-editing)
;;; text-editing.el ends here
