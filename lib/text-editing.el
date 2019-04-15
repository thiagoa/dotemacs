;;; text-editing.el  --- General text editing functions  -*- lexical-binding: t; -*-

;; Copyright (C) 2012 Thiago Araújo Silva

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

(defun my-kill-line ()
  "Kill line according to current mode.

Primarily for use with custom scripts."
  (interactive)
  (if paredit-mode (paredit-kill) (kill-line)))

(defun isearch-seek-next-word ()
  "While in isearch, seeks the next occurrence instantly with xah-search-current-word."
  (interactive)
  (xah-search-current-word)
  (isearch-repeat-forward)
  (unless (= (point) isearch-other-end) (goto-char isearch-other-end)))

(defun isearch-seek-previous-word ()
  "While in isearch, seeks the prev occurrence instantly with xah-search-current-word."
  (interactive)
  (let ((was-bound (bound-and-true-p isearch-mode)))
    (xah-search-current-word)
    (unless was-bound (isearch-repeat-backward))
    (isearch-repeat-backward))
  (unless (= (point) isearch-other-end) (goto-char isearch-other-end)))

(defun replace-region ()
  "Sensibly replace the region, making the cursor ready for insertion."
  (interactive)
  (let ((region-on-same-line-p (=
                                (line-number-at-pos (point))
                                (line-number-at-pos (mark)))))
    (call-interactively 'kill-region)
    (when (not region-on-same-line-p)
      (call-interactively 'crux-smart-open-line-above))))

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
     (point))))

(provide 'text-editing)
;;; text-editing.el ends here
