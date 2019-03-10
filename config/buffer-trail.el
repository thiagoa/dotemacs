;;; buffer-trail.el --- A private list of buffers to navigate across -*- lexical-binding: t -*-

;; Author: Thiago Araújo Silva <thiagoaraujos@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; I'm not satisfied with buffer navigation in Emacs.
;;
;; - The `next-buffer` and `previous-buffer` functions walk across all
;; the buffers, including private junk buffers and unimportant
;; buffers.  And I'm never sure if I should go back or forth to find
;; the buffer I want.
;;
;; - In order to be efficient, `switch-to-buffer` and cousins require
;; knowing the name of the target buffer prior to switching to it.
;; Sometimes I have to pause and think...  and if I don't know where I
;; want to go, I'm forced to visually seek among a bunch of
;; unimportant buffers.  The same goes to similar functions such as
;; recentf functions, etc.
;;
;; The buffer-trail solution is to maintain a list of buffers to be
;; updated when you explicitly switch to other buffers.  But there's a
;; catch: you have to manually tell what functions update the trail,
;; for example:
;;
;;     (buffer-trail-advise '(helm-buffers-list
;;                            helm-find-files
;;                            helm-recentf
;;                            helm-projectile-find-file))
;;
;; This selectivity comes in handy when I'm doing a project search, as
;; I don't want to include most results in the trail.  On the other
;; hand, if I'm explicitly switching to a buffer, most certaily I'll
;; want to keep track of it.
;;
;; Also note that you need to explicitly map a few functions:
;;
;;    (global-set-key (kbd "s-.") 'buffer-trail-forward)
;;    (global-set-key (kbd "s-,") 'buffer-trail-backward)
;;    (global-set-key (kbd "s-/") 'buffer-trail-show-breadcrumbs)
;;    (global-set-key (kbd "s-a") 'buffer-trail-add)
;;    (global-set-key (kbd "s-=") 'buffer-trail-forward)
;;    (global-set-key (kbd "s--") 'buffer-trail-backward)
;;
;; Note that when walking to the next or previous buffers, the current
;; buffer is included in the trail if it isn't already.

;;; Code:

(require 'seq)

(defvar buffer-trail--trail '()
  "A list of buffers to keep track and navigate across.")

(defun buffer-trail--function-hook (&rest function-args)
  "The buffer trail function hook.

Rather than called manually, this function takes FUNCTION-ARGS
from an advised function.  You should use it around your
preferred file and buffer navigation functions to build up the
buffer trail.

This function also pushes the point from the buffer you're in to
the mark ring, so that you can call `pop-global-mark` to make
your way back inside and across the buffers."
  (push-mark)
  (buffer-trail--add (current-buffer))
  (apply function-args)
  (buffer-trail--add (current-buffer)))

(defun buffer-trail-advise (functions)
  "Advise FUNCTIONS with buffer-trail.

FUNCTIONS must be a list of function references."
  (dolist (f functions)
    (advice-add f :around #'buffer-trail--function-hook)))

(defun buffer-trail--add (buffer)
  "Add BUFFER to the buffer trail and then return it."
  (add-to-list 'buffer-trail--trail buffer)
  buffer)

(defun buffer-trail--get-trail ()
  "Return a buffer trail with no killed buffers."
  (setq buffer-trail--trail
        (seq-filter 'buffer-live-p buffer-trail--trail)))

(defun buffer-trail--walk (step-func)
  "Walk the buffer trail.

STEP-FUNC should be a function like `#'+` or `#'-`, depending
on the walk direction."
  (let ((buffer (current-buffer)))
    (cl-flet ((switch-to-adj (_buffer-pos _adj-buffer-pos adj-buffer _trail)
                             (switch-to-buffer adj-buffer)))
      (buffer-trail--find-adjacent-buffer buffer step-func #'switch-to-adj))))

(defun buffer-trail--find-adjacent-buffer (ref-buffer step-func action)
  "Find an adjacent buffer and call the ACTION callback with the found data.

REF-BUFFER is the buffer used as a reference to find the adjacent
buffer.  STEP-FUNC is a callable that will take the position of
REF-BUFFER and return a new position.  The ACTION callable takes
the position of the adjacent buffer and other related data."
  (buffer-trail--add ref-buffer)
  (let* ((trail (buffer-trail--get-trail))
         (ref-buffer-pos (cl-position ref-buffer trail))
         (adj-buffer-pos (funcall step-func ref-buffer-pos))
         (adj-buffer (nth adj-buffer-pos trail)))
    (if (and adj-buffer (>= ref-buffer-pos 0) (>= adj-buffer-pos 0))
        (progn
          (funcall action ref-buffer-pos adj-buffer-pos adj-buffer trail)
          adj-buffer)
      ref-buffer)))

(defun buffer-trail--move-breadcrumb (step-func)
  "Move a breadcrumb along the trail.

STEP-FUNC is a callable that takes the current buffer position
and returns a new position."
  (let ((buffer (current-buffer)))
    (cl-flet ((switch-out-buffers
               (buffer-pos adj-buffer-pos adj-buffer trail)
               (setcar (nthcdr buffer-pos trail) adj-buffer)
               (setcar (nthcdr adj-buffer-pos trail) buffer)))
      (buffer-trail--find-adjacent-buffer buffer step-func #'switch-out-buffers))))

(defun buffer-trail--breadcrumbs (ref-buffer)
  "Return a formatted string with the buffer trail.

REF-BUFFER is the buffer to stand out visually."
  (let ((trail (reverse (buffer-trail--get-trail))))
    (cl-flet ((trail-to-str (buffer)
                            (let ((buffer-name (buffer-name buffer)))
                              (if (equal ref-buffer buffer)
                                  (concat "[" buffer-name "]")
                                buffer-name))))
      (mapconcat #'trail-to-str trail "  "))))

(defun buffer-trail--walk-and-show-breadcrumbs (step-func)
  "Walk the buffer trail with STEP-FUNC and display the breadcrumbs."
  (let ((buffer (buffer-trail--walk step-func)))
    (buffer-trail-show-breadcrumbs buffer)))

;;; Interactive functions:

(defun buffer-trail-backward ()
  "Walk the buffer trail backward."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (pos) (+ pos 1))))

(defun buffer-trail-forward ()
  "Walk the buffer trail forward."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (pos) (- pos 1))))

(defun buffer-trail-show-breadcrumbs (ref-buffer)
  "Displays a message with the formatted buffer trail.

REF-BUFFER is the buffer to stand out visually.  Its
default value is the current buffer."
  (interactive (list (current-buffer)))
  (buffer-trail--add ref-buffer)
  (message (buffer-trail--breadcrumbs ref-buffer)))

(defun buffer-trail-add (buffer)
  "Add BUFFER to the trail, by default the current buffer."
  (interactive (list (current-buffer)))
  (buffer-trail--add buffer)
  (call-interactively 'buffer-trail-show-breadcrumbs))

(defun buffer-trail-drop (buffer)
  "Drop BUFFER off the buffer trail (does not kill the buffer)."
  (interactive (list (current-buffer)))
  (setq buffer-trail--trail (cl-remove-if
                             (lambda (b) (equal b buffer))
                             buffer-trail--trail))
  (switch-to-buffer (car buffer-trail--trail))
  (call-interactively 'buffer-trail-show-breadcrumbs))

(defun buffer-trail-move-breadcrumb-backward ()
(defun buffer-trail-backward ()
  "Move the current buffer backward."
  (interactive)
  (buffer-trail--move-breadcrumb #'+)
  (call-interactively 'buffer-trail-show-breadcrumbs))

(defun buffer-trail-forward ()
  "Move the current buffer forward."
  (interactive)
  (buffer-trail--move-breadcrumb #'-)
  (call-interactively 'buffer-trail-show-breadcrumbs))

(provide 'buffer-trail)
;;; buffer-trail.el ends here
