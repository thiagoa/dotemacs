;;; shell-integration.el  --- TODO  -*- lexical-binding: t; -*-

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

;;; Code:

(require 'shell)
(require 'esh-util)
(require 'general)

(defconst env-vars-to-ignore '("TERM"
                               "PWD"
                               "PAGER"
                               "XPC_SERVICE_NAME"
                               "SHELL"
                               "DISPLAY"
                               "LESS"
                               "LESSOPEN" "_")
  "A list of env vars to not copy to Emacs.")

(defun shell-command-output (command)
  "Run shell COMMAND and return output."
  (replace-regexp-in-string
   "\n$"
   ""
   (shell-command-to-string command)))

(defun copy-env-vars-from-shell ()
  "Copy env vars from default shell."
  (mapc (lambda (assignment)
          (let* ((parts (split-string assignment "="))
                 (var (car parts))
                 (value (cadr parts)))
            (unless (member var env-vars-to-ignore)
              (when (equal "PATH" var)
                (setq eshell-path-env value
                      exec-path (append (parse-colon-path value)
                                        (list exec-directory))))
              (setenv var value))))
        (split-string (shell-command-output
                       (concat explicit-shell-file-name " -l -i -c env"))
                      "\n")))

(provide 'shell-integration)
;;; shell-integration.el ends here
