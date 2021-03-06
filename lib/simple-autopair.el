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
                                ("|"  . "|")
                                ("/"  . "/"))
  "The character pairs recognized by simple-autopairs.")

(defvar simple-autopair-enabled-pairs '("(" "\"" "'" "{" "[" "|" "/")
  "Pairs enabled by default, identified after 'left-char'.")

(defvar simple-autopair-string-closing-chars '("'" "\"" "}" "/")
  "Possible characters that close a string or regex, identified after 'right-char'.")

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
  (if (region-active-p)
      (delete-region (region-beginning) (region-end)))
  (let ((char (this-command-keys)))
    (if skip-autopair
        (insert char)
      (let* (pair-enabled-p
             (right-char (simple-autopair--right-char char))
             (left-char (simple-autopair--left-char char))
             (master-char (cond (left-char left-char)
                                (right-char char)
                                (t (error "No pair found for char")))))
        (setq pair-enabled-p (simple-autopair--pair-enabled-p master-char))
        (cond ((and pair-enabled-p right-char)
               (simple-autopair-do-char char right-char :left-char))
              ((and pair-enabled-p left-char)
               (simple-autopair-do-char left-char char :right-char))
              (t (insert char)))))))

(defun simple-autopair--pair-enabled-p (left-char)
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
   ((simple-autopair--should-forward-p right-char)
    (forward-char))
   ((simple-autopair--should-ignore-expansion left-char)
    (insert (if (eq type :right-char) right-char left-char)))
   ((eq type :right-char)
    (if (equal (char-to-string (char-after)) right-char)
        (forward-char)
      (insert right-char)))
   (t
    (insert left-char right-char)
    (backward-char))))

(defun simple-autopair--should-forward-p (right-char)
  "High-level function to determine if we should skip RIGHT-CHAR at point."
  (or (simple-autopair--str-boundary-forward-char-p right-char
                                                    simple-autopair-string-closing-chars)
      (simple-autopair--forward-char-p right-char "|")))

(defun simple-autopair--should-ignore-expansion (left-char)
  "High-level function to determine if current pair expansion should be ignored.
Takes LEFT-CHAR."
  (or (simple-autopair--within-str-p)
      (simple-autopair--div-math-op-p left-char)
      (simple-autopair--inside-p 'font-lock-comment-face)))

(defun simple-autopair--div-math-op-p (target-char)
  "Detect a math operation to avoid expanding /.
Takes TARGET-CHAR to compare against /."
  (and (equal target-char "/")
       (eq (char-after (- (point) 1)) ?\s)
       (save-excursion (goto-char (- (point) 2))
                       (looking-at "\[0-9a-zA-Z\]"))))

(defun simple-autopair--forward-char-p (str &rest target-strs)
  "Given STR and TARGET-STRS, determine whether to `forward-char'."
  (let ((found (car (member str target-strs)))
        (char (char-after)))
    (if char (equal (char-to-string (char-after)) found))))

(defun simple-autopair--str-boundary-forward-char-p (str target-chars)
  "Determine whether STR is one of TARGET-CHARS and whether cursor is at a string or regex delimiter."
  (and (simple-autopair--str-boundary-p)
       (apply #'simple-autopair--forward-char-p str target-chars)))

(defun simple-autopair--str-boundary-p ()
  "Determine whether we are at string or regex boundaries."
  (simple-autopair--inside-p 'enh-ruby-string-delimiter-face
                             'enh-ruby-regexp-delimiter-face))

(defun simple-autopair--inside-str-p ()
  "Determine whether we are inside the string, not at boundaries."
  (simple-autopair--inside-p 'font-lock-string-face
                             'enh-ruby-regexp-face))

(defun simple-autopair--within-str-p ()
  "Determine whether point is within a string or regex."
  (and (not (and (simple-autopair--str-boundary-p)
                 (eq (char-after) ?\n)))
       (or (simple-autopair--inside-str-p)
           (simple-autopair--str-boundary-p))))

(defun simple-autopair--inside-p (&rest font-lock-props)
  "Return non-nil if point is at one of FONT-LOCK-PROPS."
  (member (get-text-property (point) 'font-lock-face) font-lock-props))

;;;###autoload
(defun simple-autopair-delete ()
  "Automatically delete an empty pair."
  (interactive)
  (let ((point (point)))
    (if  (or (simple-autopair--point-around-empty-pair-p point)
             (simple-autopair--point-around-spaced-pair-p point))
        (progn (call-interactively 'delete-backward-char)
               (call-interactively 'delete-forward-char))
      (call-interactively 'delete-backward-char))))

(defun simple-autopair--point-around-empty-pair-p (point)
  "Return the left char if an empty pair exists around POINT."
  (let ((left-char (simple-autopair--points-form-a-pair-p (- point 1) point)))
    (if (and left-char (simple-autopair--pair-enabled-p left-char))
        left-char)))

(defun simple-autopair--points-form-a-pair-p (left-point right-point)
  "Return the left char if LEFT-POINT and RIGHT-POINT form a pair."
  (let* ((left-char (char-to-string (or (char-after left-point) ?\s)))
         (right-char (char-to-string (or (char-after right-point) ?\s)))
         (right-char-for-left-char (simple-autopair--right-char left-char)))
    (if (equal right-char right-char-for-left-char)
        left-char)))

(defun simple-autopair--point-around-spaced-pair-p (point)
  "Return the left char if a spaced pair exists around POINT."
  (let ((left-char (simple-autopair--points-form-a-pair-p (- point 2) (+ 1 point))))
    (if (and left-char (simple-autopair--spaced-p left-char))
        left-char)))

;;;###autoload
(defun simple-autopair-space ()
  "Automatically space out pair if present in `simple-autopair-spaced'."
  (interactive)
  (let ((left-char (simple-autopair--point-around-empty-pair-p (point))))
    (if (and left-char (simple-autopair--spaced-p left-char))
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
