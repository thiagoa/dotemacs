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
                                ("'"  . "'")
                                ("{"  . "}")
                                ("["  . "]")
                                ("|"  . "|"))
  "The character pairs recognized by simple-autopairs.")

(defvar simple-autopair-enabled-pairs '("(" "\"" "'" "{" "[")
  "Pairs enabled by default.")

(defvar simple-autopair-spaced '("{")
  "Pairs to space out when pressing the space key.")

(defvar simple-autopair-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (cell simple-autopair-pairs)
      (dolist (key (list (car cell) (cdr cell)))
        (define-key map (kbd key) 'simple-autopair-insert-char)))
    (define-key map [(backspace)] 'simple-autopair-delete)
    (define-key map (kbd "SPC") 'simple-autopair-space)
    map)
  "Keymap for function `simple-autopair-mode'.")

;;;###autoload
(defun simple-autopair-insert-char (skip-autopair)
  "Insert pair of characters or do other things depending on the context.
If SKIP-AUTOPAIR > 1 is passed, skip autopair behavior and insert char."
  (interactive "P")
  (let ((char (this-command-keys)))
    (if skip-autopair
        (insert char)
      (let* (pair-enabled-p
             (right-char (simple-autopair--right-char char))
             (left-char (simple-autopair--left-char char))
             (master-char (cond (left-char left-char)
                                (right-char char)
                                (t (error "No pair found for char")))))
        (setq pair-enabled-p (simple-autopair--enabled-p master-char))
        (cond ((and pair-enabled-p right-char)
               (simple-autopair-do-char char right-char :left-char))
              ((and pair-enabled-p left-char)
               (simple-autopair-do-char left-char char :right-char))
              (t (insert char)))))))

(defun simple-autopair--enabled-p (left-char)
  "Return whether the pair for LEFT-CHAR is enabled."
  (member left-char simple-autopair-enabled-pairs))

(defun simple-autopair--spaced-p (left-char)
  "Return whether LEFT-CHAR should be spaced out when pressing space."
  (member left-char simple-autopair-spaced))

(defun simple-autopair--left-char (right-char)
  "Return the left char for RIGHT-CHAR."
  (car (rassoc right-char simple-autopair-pairs)))

(defun simple-autopair--right-char (left-char)
  "Return the right char for LEFT-CHAR."
  (cdr (assoc left-char simple-autopair-pairs)))

(defun simple-autopair-do-char (left-char right-char type)
  "Helper for simple-autopair-insert-char that does the actual work.
Takes LEFT-CHAR, RIGHT-CHAR, and TYPE, which can be :left-char or
:right-char."
  (cond
   ((or (simple-autopair--string-limit-p left-char ?\')
        (simple-autopair--string-limit-p left-char ?\"))
    (forward-char))
   ((or (simple-autopair--inside-p 'font-lock-string-face)
        (simple-autopair--inside-p 'font-lock-comment-face)
        (simple-autopair--inside-p 'enh-ruby-string-delimiter-face))
    (insert (if (eq type :right-char) right-char left-char)))
   ((eq type :right-char)
    (if (eq (char-after) (string-to-char right-char))
        (forward-char)
      (insert right-char)))
   (t
    (insert left-char right-char)
    (backward-char))))

(defun simple-autopair--string-limit-p (str target-char)
  "Determine whether STR is TARGET-CHAR and whether cursor is at a string delimiter."
  (and (simple-autopair--inside-p 'enh-ruby-string-delimiter-face)
       (string= str (char-to-string target-char))
       (eq (char-after) target-char)))

(defun simple-autopair--inside-p (font-lock-prop)
  "Return non-nil if point is at FONT-LOCK-PROP font-lock-face property."
  (eq font-lock-prop (get-text-property (point) 'font-lock-face)))

;;;###autoload
(defun simple-autopair-delete ()
  "Automatically delete an empty pair."
  (interactive)
  (let* ((char (char-to-string (or (char-after) ?\s)))
         (left-char (simple-autopair--left-char char))
         (empty-pair-p (and left-char
                            (eq (char-after (1- (point)))
                                (string-to-char left-char)))))
    (if empty-pair-p
        (progn (call-interactively 'delete-backward-char)
               (call-interactively 'delete-forward-char))
      (call-interactively 'delete-backward-char))))

;;;###autoload
(defun simple-autopair-space ()
  "Automatically space out pair if present in `simple-autopair-spaced'."
  (interactive)
  (let ((left-char (char-to-string (char-after (1- (point)))))
        (right-char (char-to-string (or (char-after (point)) ?\s))))
    (if (and (simple-autopair--spaced-p left-char)
             (string= right-char
                      (simple-autopair--right-char left-char)))
        (progn (insert "  ") (backward-char))
      (insert " "))))

;;;###autoload
(define-minor-mode simple-autopair-mode
  "Automate insertion of pairs."
  :lighter " Simple-Autopair"
  :keymap simple-autopair-mode-map
  :group 'simple-autopair)

(provide 'simple-autopair)
;;; simple-autopair.el ends here
