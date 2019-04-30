;;; ext-compile.el  --- compile.el extensions  -*- lexical-binding: t; -*-

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

(require 'compile)
(require 'ext-elisp)

(defun notify-os (message sound)
  "Send a notification to macOS.
Requires terminal-notifier (install it via homebrew).
MESSAGE is the notification message; SOUND is the sound that will be played."
  (shell-command
   (concat
    "bash -c -l 'echo " message " | terminal-notifier -sound "
    sound
    "'")))

(defun finish-test-compilation ()
  "Calback to be run after a compilation task finishes.

The exit code verification method can still be improved."
  (if (= compilation-num-errors-found 0)
      (notify-os "Tests passed 👍" "Hero")
    (notify-os "Tests failed 👎" "Basso")))

(defun go-to-file (func arg)
  "Generic function to go to file in compilation buffer.

FUNC is the movement function to be used.  ARG is the universal
argument and specifies how many error messages to move; negative
means move back to previous error messages."
  (when (setq next-error-last-buffer (next-error-find-buffer))
    (with-current-buffer next-error-last-buffer
      (call-interactively func)
      (compile-goto-error)
      (when next-error-recenter
        (recenter next-error-recenter))
      (run-hooks 'next-error-hook))))

(defun next-file (&optional arg)
  "Go to next file in compilation buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-file 'compilation-next-file arg))


(defun previous-file (&optional arg)
  "Go to previous file in compilation buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-file 'compilation-previous-file arg))

(provide 'ext-compile)
;;; ext-compile.el ends here
