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

(defconst symbol-syntax-chars "w_'"
  "The syntax chars for symbols.")

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

(defun break-delimited ()
  "Break into newlines when surrounded by a pair of characters."
  (interactive)
  (newline)
  (newline)
  (indent-for-tab-command)
  (previous-line)
  (indent-for-tab-command))

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
  (let ((beg-point (point))
        line-changed)
    (set-mark
     (save-excursion
       (dotimes (_ arg) (forward-line))
       (move-beginning-of-line 1)
       (setq line-changed (not (eq (point) beg-point)))
       (if (not line-changed)
           (move-end-of-line 1))
       (point)))
    (if line-changed
        (exchange-point-and-mark))))

(defun mark-symbol ()
  "Mark the current symbol at point."
  (interactive)
  (skip-syntax-backward symbol-syntax-chars)
  (set-mark
   (save-excursion
     (skip-syntax-forward symbol-syntax-chars)
     (when (and (eq ?! (char-after (point)))
                (eq major-mode 'enh-ruby-mode))
       (forward-char))
     (point))))

(defun comment-or-uncomment-line-or-region ()
  "Comment or uncomments the current line or region."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun kill-code-paragraph ()
  "Kill paragraph and delete blank lines thereafter."
  (interactive)
  (let ((start (point))
        end
        (cur-indent (save-excursion
                      (call-interactively 'indent-for-tab-command)
                      (current-indentation))))
    (save-excursion
      (forward-paragraph)
      (back-to-indentation)
      (if (< (current-indentation) cur-indent)
          (progn
            (while (< (current-indentation) cur-indent)
              (previous-line))
            (end-of-line)
            (setq end (point))
            (goto-char start)
            (delete-region start end)
            (delete-blank-lines))
        (progn
          (goto-char start)
          (kill-paragraph 1))))
    (if (save-excursion
          (previous-line)
          (< (current-indentation) cur-indent))
        (delete-blank-lines))))

(defun clear-line (arg)
  "Sensibly clears the current line.
ARG corresponds to the number of lines to clear."
  (interactive "p")
  (if (> arg 1)
      (progn (dotimes (i (1- arg)) (crux-kill-whole-line))))
  (call-interactively 'indent-for-tab-command)
  (call-interactively 'back-to-indentation)
  (when (not (current-line-empty-p)) (funcall (key-binding "\C-k"))))

(defun current-line-empty-p ()
  "Return non-nil if the current line is empty."
  (save-excursion
    (beginning-of-line)
    (looking-at "[[:space:]]*$")))

(defun xah-shrink-whitespaces ()
  "Remove whitespaces around cursor to just one, or none.

Shrink any neighboring space tab newline characters to 1 or none.
If cursor neighbor has space/tab, toggle between 1 or 0 space.
If cursor neighbor are newline, shrink them to just 1.
If already has just 1 whitespace, delete it.

URL `http://ergoemacs.org/emacs/emacs_shrink_whitespace.html'
Version 2018-04-02T14:38:04-07:00"
  (interactive)
  (let* (
         ($eol-count 0)
         ($p0 (point))
         $p1 ; whitespace begin
         $p2 ; whitespace end
         ($charBefore (char-before))
         ($charAfter (char-after ))
         ($space-neighbor-p (or (eq $charBefore 32) (eq $charBefore 9) (eq $charAfter 32) (eq $charAfter 9)))
         $just-1-space-p
         )
    (skip-chars-backward " \n\t")
    (setq $p1 (point))
    (goto-char $p0)
    (skip-chars-forward " \n\t")
    (setq $p2 (point))
    (goto-char $p1)
    (while (search-forward "\n" $p2 t )
      (setq $eol-count (1+ $eol-count)))
    (setq $just-1-space-p (eq (- $p2 $p1) 1))
    (goto-char $p0)
    (cond
     ((eq $eol-count 0)
      (if $just-1-space-p
          (delete-horizontal-space)
        (progn (delete-horizontal-space)
               (insert " "))))
     ((eq $eol-count 1)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn (xah-delete-blank-lines) (insert " "))))
     ((eq $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (xah-delete-blank-lines)
          (insert "\n"))))
     ((> $eol-count 2)
      (if $space-neighbor-p
          (delete-horizontal-space)
        (progn
          (goto-char $p2)
          (search-backward "\n" )
          (delete-region $p1 (point))
          (insert "\n"))))
     (t (progn
          (message "nothing done. logic error 40873. shouldn't reach here" ))))))

(provide 'text-editing)
;;; text-editing.el ends here
