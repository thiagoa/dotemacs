;;; config-helpers.el  --- Make Emacs config readable  -*- lexical-binding: t; -*-

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
(require 'exec-path-from-shell)
(require 'general)

(defun force-split-window-sensibly-to-horizontal-when-big-font ()
  "When using a big font, split window horizontally."
  (setq split-height-threshold nil)
  (setq split-width-threshold 140))

(defun disable-startup-screen ()
  "No annoying startup screen."
  (setq inhibit-startup-screen t)
  (setq inhibit-startup-message t
        inhibit-startup-echo-area-message t)
  (setq ring-bell-function 'ignore))

(defun config-terminal-encoding ()
  "Set terminal encoding to UTF-8."
  (add-hook 'term-exec-hook
            (function
             (lambda ()
               (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))

(defun disable-annoyances ()
  "Disable annoying Emacs stuff such as dialog prompts, bells, etc."
  (fset 'yes-or-no-p 'y-or-n-p)
  (setq exec-path-from-shell-check-startup-files nil)
  (setq confirm-nonexistent-file-or-buffer nil)
  (setq kill-buffer-query-functions
        (remq 'process-kill-buffer-query-function
              kill-buffer-query-functions)))

(defun set-default-shell (name)
  "Set the default shell to NAME."
  (setq
   explicit-shell-file-name
   (shell-command-output (concat "which " name))))

(defun emacs-use-same-path-as-shell ()
  "Set Emacs path to the shell path."
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(defun show-full-filename-in-window-title ()
  "Show full filename in window title."
  (setq-default
   frame-title-format
   '((:eval (if (buffer-file-name)
                (abbreviate-file-name (buffer-file-name)) "%f")))))

;; Why is history not being loaded? :thinking:
(defun load-history (savehist-file)
  "Explicitly load history from SAVEHIST-FILE."
  (unless (file-exists-p savehist-file)
    (progn
      (shell-command (concat "mkdir -p " (file-name-directory savehist-file)))
      (shell-command (concat "touch " savehist-file))))
  (load savehist-file))

(provide 'config-helpers)
;;; config-helpers.el ends here
