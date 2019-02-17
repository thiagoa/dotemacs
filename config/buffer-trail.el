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
  (apply function-args))

(defun buffer-trail-advise (functions)
  "The list of FUNCTIONS to advise with buffer-trail."
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

(defun buffer-trail--walk (get-position)
  "Walk the buffer trail.

The GET-POSITION callable takes the position of the current
buffer and returns the destination position.  That position is
then checked against the buffer trail.  If valid, switches
to the buffer in that position."
  (let* ((buffer (buffer-trail--add (current-buffer)))
         (trail (buffer-trail--get-trail))
         (buffer-position (cl-position buffer trail))
         (next-buffer-position (apply (list get-position buffer-position)))
         (next-buffer (nth next-buffer-position trail)))
    (if (and (>= next-buffer-position 0) next-buffer)
        (switch-to-buffer next-buffer)
      buffer)))

(defun buffer-trail--breadcrumbs (reference-buffer)
  "Return a formatted string with the buffer trail.

The REFERENCE-BUFFER stands out visually."
  (apply #'concat
         (mapcar (lambda (buffer)
                   (if (equal reference-buffer buffer)
                       (concat " [" (buffer-name buffer) "] ")
                     (concat " " (buffer-name buffer) " ")))
                 (reverse (buffer-trail--get-trail)))))

(defun buffer-trail--walk-and-show-breadcrumbs (func)
  "Walk the buffer trail with FUNC and display the breadcrumbs."
  (let ((buffer (buffer-trail--walk func)))
    (buffer-trail-show-breadcrumbs buffer)))

(defun buffer-trail-backward ()
  "Walk the buffer trail backward."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (pos) (+ pos 1))))

(defun buffer-trail-forward ()
  "Walk the buffer trail forward."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (pos) (- pos 1))))

(defun buffer-trail-show-breadcrumbs (reference-buffer)
  "Displays a message with the formatted buffer trail.

REFERENCE-BUFFER is the buffer to stand out visually.  Its
default value is the current buffer."
  (interactive (list (current-buffer)))
  (message (buffer-trail--breadcrumbs reference-buffer)))

(provide 'buffer-trail)
;;; buffer-trail.el ends here
