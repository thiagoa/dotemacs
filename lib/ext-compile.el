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

(provide 'ext-compile)
;;; ext-compile.el ends here
