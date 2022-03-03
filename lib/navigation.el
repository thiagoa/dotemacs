;;; navigation.el  --- TODO  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Thiago Araújo Silva

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

;; Functions for buffer navigation that actually make sense.

;;; Code:

(defvar thiago/non-visitable-buffers '("^*"
                                       "^\s\\*"
                                       "^magit"
                                       "^TAGS$")
  "Buffers matching any of the regexes from this list are not visitable.")

(defvar thiago/non-visitable-buffer-exceptions '("^*scratch\\*$"
                                                 "^*shell "
                                                 "^*rails"
                                                 "^*rspec"
                                                 "^*jest"
                                                 "^*Mini")
  "Regexes that define exceptions to NON-VISITABLE-BUFFERS.
Buffers in this list are actually visitable because they override
NON-VISITABLE-BUFFERS.")

(defun thiago/buffer-visitable-p (buffer)
  "Return whether BUFFER is visitable or not."
  (let ((buffer-name (buffer-name buffer)))
    (or (not (cl-find-if (lambda (re) (string-match re buffer-name))
                         thiago/non-visitable-buffers))
        (cl-find-if (lambda (re) (string-match re buffer-name))
                    thiago/non-visitable-buffer-exceptions))))

(defun thiago/go-to-alternate-buffer ()
  "Alternate between current and previous visitable buffers.

We are avoiding OTHER-BUFFER over WINDOW-PREV-BUFFERS because the
former will usually not give us the right answer."
  (interactive)
  (let (found-buffer
        (buffers-info (window-prev-buffers)))
    (while (and (not found-buffer) buffers-info)
      (let* ((buffer (caar buffers-info)))
        (when (thiago/buffer-visitable-p buffer)
          (setq found-buffer buffer))
        (setq buffers-info (cdr buffers-info))))
    (when found-buffer (switch-to-buffer found-buffer))))

(defun thiago/cycle-buffers (fn)
  "Cycle between visitable buffers with FN.

It is assumed that FN is a function that will cycle through
buffers forward or backward, for example, NEXT-BUFFER or
PREVIOUS-BUFFER."
  (let ((initial-buffer (current-buffer)))
    (while (progn
             (funcall fn)
             (let* ((buffer (current-buffer)))
               (and (not (eq buffer initial-buffer))
                    (not (thiago/buffer-visitable-p buffer))))))
    (set-transient-map
     (let ((map (make-sparse-keymap)))
       (define-key map (vector ?l) 'thiago/next-buffer)
       (define-key map (vector ?h) 'thiago/previous-buffer)
       map))))

(defun thiago/next-buffer ()
  "Go to the next visitable buffer."
  (interactive)
  (thiago/cycle-buffers 'next-buffer))

(defun thiago/previous-buffer ()
  "Go to the previous visitable buffer."
  (interactive)
  (thiago/cycle-buffers 'previous-buffer))

(provide 'navigation)
;;; navigation.el ends here
