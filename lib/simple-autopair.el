;;; simple-autopair.el  --- TODO  -*- lexical-binding: t; -*-

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

;; NOTE: Currently, only works for enh-ruby-mode

;;; Code:

(defvar simple-autopair-pairs '(("("  . ")")
                                ("\"" . "\"")
                                ("'"  . "'"))
  "The character pairs (cons cells) recognized by simple-autopairs.")

(defvar simple-autopair-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [(backspace)] 'simple-autopair-delete)
    map)
  "Keymap for function `simple-autopair-mode'.")

(defun simple-autopair-inside-p (font-lock-prop)
  "Return non-nil if point is at FONT-LOCK-PROP font-lock-face property."
  (eq font-lock-prop (get-text-property (point) 'font-lock-face)))

(defun simple-autopair-configure (hook &rest chars)
  "Configure autopair characters for HOOK mode.
CHARS are the single-char strings for which to apply autopair."
  (add-hook
   hook
   (lambda ()
     (dolist (c chars)
       (local-set-key (kbd c) (lambda (skip) (interactive "P")
                                (simple-autopair-char c (car skip))))))))

(defun simple-autopair-char (char skip)
  "Insert CHAR, CHAR + pair, or `forward-char' depending on context.
If SKIP > 1 is passed, override autopair behavior and insert char."
  (if skip
      (insert char)
    (let ((right-char (cdr (assoc char simple-autopair-pairs)))
          (left-char (cdr (rassoc char simple-autopair-pairs))))
      (cond
       (right-char (simple-autopair-do-char char right-char :left-char))
       (left-char (simple-autopair-do-char left-char char :right-char))
       (t (error "No pair found for char"))))))

;;;###autoload
(defun simple-autopair-delete ()
  "Automatically delete an empty pair."
  (interactive)
  (let* ((char (char-to-string (char-after (point))))
         (left-char (car (rassoc char simple-autopair-pairs)))
         (empty-pair-p (and left-char
                            (save-excursion (backward-char)
                                            (looking-at left-char)))))
    (if empty-pair-p
        (progn (call-interactively 'delete-backward-char)
               (call-interactively 'delete-forward-char))
      (call-interactively 'delete-backward-char))))

(defun simple-autopair-do-char (left-char right-char type)
  "Helper for autopair-char.
Takes LEFT-CHAR, RIGHT-CHAR, and TYPE, which can be :left-char or
:right-char."
  (cond
   ((and (string= left-char "\"")
         (simple-autopair-inside-p 'enh-ruby-string-delimiter-face))
    (forward-char))
   ((or (simple-autopair-inside-p 'font-lock-string-face)
        (simple-autopair-inside-p 'font-lock-comment-face)
        (simple-autopair-inside-p 'enh-ruby-string-delimiter-face))
    (insert (if (eq type :right-char) right-char left-char)))
   ((and
     (eq type :right-char)
     (or (looking-at left-char) (looking-at right-char)))
    (forward-char))
   (t
    (insert left-char right-char)
    (backward-char))))

;;;###autoload
(define-minor-mode simple-autopair-mode
  "Tag management for Ruby files."
  :lighter " Simple-Autopair"
  :keymap simple-autopair-mode-map
  :group 'simple-autopair)

(provide 'simple-autopair)
;;; simple-autopair.el ends here
