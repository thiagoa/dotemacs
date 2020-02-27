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
    "bash -c -l 'echo " message " | terminal-notifier -activate \'org.gnu.Emacs\' -sound "
    sound
    "'")))

(defun find-buffer-in-windows (func &optional default)
  "Find buffer returned by FUNC in visible windows.
If not found, returns DEFAULT."
  (let ((buffer (cl-find-if func
                            (mapcar 'window-buffer (window-list)))))
    (or buffer default)))

(defun go-to-file (func arg)
  "Generic function to go to file in compilation buffer.

FUNC is the movement function to be used.  ARG is the universal
argument and specifies how many error messages to move; negative
means move back to previous error messages."
  (when (setq next-error-last-buffer (next-error-find-buffer))
    (let ((buffer (find-buffer-in-windows
                   'compilation-buffer-p
                   (next-error-find-buffer))))
      (switch-to-compilation buffer
                             (lambda (buffer-name)
                               (with-current-buffer buffer-name
                                 (beginning-of-buffer))))
      (with-current-buffer buffer
        (call-interactively func)
        (compile-goto-error)
        (when next-error-recenter
          (recenter next-error-recenter))
        (run-hooks 'next-error-hook)))))

(defun go-to-next-file (&optional arg)
  "Go to next file in compilation buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-file 'compilation-next-file arg))

(defun go-to-previous-file (&optional arg)
  "Go to previous file in compilation buffer.

A prefix ARG specifies how many error messages to move;
negative means move back to previous error messages."
  (interactive "P")
  (go-to-file 'compilation-previous-file arg))

(defun switch-to-compilation (compilation-buffer &optional func)
  "Make sure compilation mode is on on COMPILATION-BUFFER (rspec buffer)."
  (let* ((buffer-name (buffer-name compilation-buffer))
         (inf-ruby-mode-p (and (string-prefix-p "*rspec-" buffer-name)
                               (eq 'inf-ruby-mode
                                   (with-current-buffer buffer-name major-mode)))))
    (if inf-ruby-mode-p
        (with-current-buffer buffer-name
          (progn
            (inf-ruby-maybe-switch-to-compilation)
            (if func (funcall func buffer-name)))))))

;; TODO: Refactor
(defun with-compilation-mode-off (func)
  "Takes FUNC. Refactor to eliminate cross-function duplication."
  (let* ((buffer-name (next-error-find-buffer))
         (inf-ruby-mode-p (eq 'inf-ruby-mode
                              (with-current-buffer buffer-name major-mode))))
    (unless inf-ruby-mode-p
      (with-current-buffer buffer-name
        (inf-ruby-switch-from-compilation)))
    (funcall func)))

(defun navigate-error-dwim (&rest args)
  "If dealing with an RSpec buffer, make sure compilation mode is on.
This is an advice function, hence ARGS."
  (let* ((compilation-buffer (next-error-find-buffer)))
    (switch-to-compilation compilation-buffer)
    (apply (car args) (cdr args))))

(progn (advice-mapc (lambda (advice _props) (advice-remove 'next-error advice)) 'next-error)
       (advice-add 'next-error :around #'navigate-error-dwim))

(provide 'ext-compile)
;;; ext-compile.el ends here
