;;; buffer-trail.el --- A private list of buffers to navigate across.  -*- lexical-binding: t -*-

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
;; Buffer navigation in Emacs could be better.
;;
;; `switch-to-buffer` is great and I use it a lot, but when I'm
;; working with a small group of files and need to switch back and
;; forth every few seconds, it can be a pain the ass.  Even the more
;; powerful versions like `helm-buffers-list` require knowing the name
;; of the target buffer in order to switch over efficiently.  Often
;; times I pause and think, and if I don't know where I want to go,
;; I'm forced to find the buffer I want among the cruft buffers
;; automatically created by Emacs.
;;
;; Another option is using `next-buffer` and `previous-buffer`, but
;; unfortunately, these functions factor in the cruft buffers.
;; Moreover, they're not intuitive, and I'm never sure which one will
;; take me to the buffer I want because Emacs manipulates the buffer
;; list behind the curtains.
;;
;; buffer-trail is designed to be a "private" list of buffers updated
;; when you explicitly switch buffers.
;;
;; - It works as you'd expect: you won't have unpleasant surprises
;; like walking the buffer trail backward and not reaching the buffer
;; you were previously in.
;;
;; - You'll never get lost because you'll see the list of buffers
;; within the mini-buffer every time you trigger a switch.
;;
;; - You can drag the buffers along the trail like you do with browser
;; tabs, but much more smoothly -- this way you can easily organize a
;; group of buffers for fast switching.
;;
;; To use buffer-trail, you need to manually tell what functions
;; update the trail, for example:
;;
;;     (buffer-trail-advise '(helm-buffers-list
;;                            helm-find-files
;;                            helm-recentf
;;                            helm-projectile-find-file))
;;
;; This selectivity comes in handy when you're doing, for example, a
;; project search, as you don't want search results to show up in the
;; trail.  On the other hand, if you're explicitly switching to a
;; buffer, you'll most certainly want to keep track of it.
;; You can also explicitly add a buffer to trail if you need to.
;;
;; Also note that you need to explicitly map a few keybindings:
;;
;;    (global-set-key (kbd "s-.") 'buffer-trail-forward)
;;    (global-set-key (kbd "s-,") 'buffer-trail-backward)
;;    (global-set-key (kbd "s-<") 'buffer-trail-first)
;;    (global-set-key (kbd "s->") 'buffer-trail-last)
;;    (global-set-key (kbd "s-=") 'buffer-trail-drag-forward)
;;    (global-set-key (kbd "s--") 'buffer-trail-drag-backward)
;;    (global-set-key (kbd "s-a") 'buffer-trail-add)
;;    (global-set-key (kbd "s-0") 'buffer-trail-drop)
;;    (global-set-key (kbd "s-/") 'buffer-trail-show-breadcrumbs)
;;
;; Note: when walking the buffer trail, the current buffer is included
;; in the trail if it isn't already.

;;; Code:

(require 'subr-x)
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

STEP-FUNC is a callable that takes the current buffer position
and returns a new position."
  (let ((buffer (current-buffer)))
    (cl-flet ((switch-to-adj (_buffer-pos _adj-buffer-pos adj-buffer _trail)
                             (switch-to-buffer adj-buffer)))
      (buffer-trail--find-adjacent-buffer buffer step-func #'switch-to-adj))))

(defun buffer-trail--find-adjacent-buffer (ref-buffer step-func action)
  "Find an adjacent buffer and call the ACTION callback with the found data.

REF-BUFFER is the buffer used as a reference to find the adjacent
buffer.  STEP-FUNC is a callable that will take the position of
REF-BUFFER and return a new position.  The ACTION callable takes
the position of the adjacent buffer and other related arguments."
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

(defun buffer-trail--drag-breadcrumb (step-func)
  "Move a breadcrumb along the trail.

STEP-FUNC is a callable that takes the position of the current
buffer and returns a new position."
  (let ((buffer (current-buffer)))
    (cl-flet ((switch-out-buffers
               (buffer-pos adj-buffer-pos adj-buffer trail)
               (setcar (nthcdr buffer-pos trail) adj-buffer)
               (setcar (nthcdr adj-buffer-pos trail) buffer)))
      (buffer-trail--find-adjacent-buffer buffer step-func #'switch-out-buffers))))

(defun buffer-trail--breadcrumbs (ref-buffer)
  "Return a formatted string with the buffer trail.

REF-BUFFER is the buffer to stand out visually."
  (let ((trail (reverse (buffer-trail--get-trail)))
        (buffer-tmpl "[%s]")
        (buffer-sep "  ")
        (seg-sep "\n"))
    (cl-labels ((buffer-visual-name (buffer) (format buffer-tmpl buffer))
                (format-buffer
                 (buffer)
                 (let ((buffer-str (buffer-visual-name (buffer-name buffer))))
                   (if (equal ref-buffer buffer)
                       (propertize buffer-str 'face 'font-lock-warning-face)
                     buffer-str)))
                (format-a-trail-segment
                 (segment)
                 (string-join (mapcar #'format-buffer segment) buffer-sep))
                (format-trail-segments
                 (segments)
                 (propertize (string-join segments seg-sep) 'line-spacing 3)))
      (let* ((extra-chars-length (length (buffer-visual-name "")))
             (buffer-sep-length (length buffer-sep))
             (sublists (buffer-trail--split-breadcrumbs-to-frame-width
                        trail
                        (+ extra-chars-length buffer-sep-length)))
             (trail-segments (mapcar #'format-a-trail-segment sublists)))
        (format-trail-segments trail-segments)))))

(defun buffer-trail--split-breadcrumbs-to-frame-width (trail extra-length)
  "Splits the trail into sublists, each with at most the length of the frame.

TRAIL is the list of buffers.  EXTRA-LENGTH is the length of the
characters placed between the buffers to display the buffer
trail.  For instance, if the buffers are wrapped in square
brackets and the separator is \" \", then EXTRA-LENGTH is 2 +
2 (square brackets length + separator length)."
  (let ((total-length 0)
        (frame-width (frame-width))
        (sublists (list))
        (cur-sublist (list)))
    (while trail
      (setq total-length (+ (length (buffer-name (car trail)))
                            extra-length
                            total-length))
      (if (< total-length frame-width)
          (setq cur-sublist (cons (pop trail) cur-sublist))
        (progn
          (setq total-length 0)
          (setq sublists (cons (reverse cur-sublist) sublists))
          (setq cur-sublist (list)))))
    (setq sublists (cons (reverse cur-sublist) sublists))
    (reverse sublists)))

(defun buffer-trail--walk-and-show-breadcrumbs (step-func)
  "Walk the buffer trail with STEP-FUNC and display the breadcrumbs."
  (let ((buffer (buffer-trail--walk step-func)))
    (buffer-trail-show-breadcrumbs buffer)))

;;; Interactive functions:

(defun buffer-trail-backward ()
  "Walk the buffer trail backward."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (pos) (1+ pos))))

(defun buffer-trail-forward ()
  "Walk the buffer trail forward."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (pos) (1- pos))))

(defun buffer-trail-first ()
  "Walk to the first buffer."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs
   (lambda (_pos) (1- (length (buffer-trail--get-trail))))))

(defun buffer-trail-last ()
  "Walk to the last buffer."
  (interactive)
  (buffer-trail--walk-and-show-breadcrumbs (lambda (_pos) 0)))

(defun buffer-trail-drag-backward ()
  "Drag the current buffer backward."
  (interactive)
  (buffer-trail--drag-breadcrumb (lambda (pos) (1+ pos)))
  (call-interactively 'buffer-trail-show-breadcrumbs))

(defun buffer-trail-drag-forward ()
  "Drag the current buffer forward."
  (interactive)
  (buffer-trail--drag-breadcrumb (lambda (pos) (1- pos)))
  (call-interactively 'buffer-trail-show-breadcrumbs))

(defun buffer-trail-show-breadcrumbs (ref-buffer)
  "Display a message with the formatted buffer trail.

REF-BUFFER is the buffer to stand out visually.  It defaults to
the current buffer."
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

(provide 'buffer-trail)
;;; buffer-trail.el ends here
