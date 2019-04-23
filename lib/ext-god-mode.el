;;; ext-god-mode.el --- Simple god mode extensions  -*- lexical-binding: t; -*-
;;
;; Copyright © 2019 Thiago Araújo Silva
;;
;; Author: Thiago Araújo Silva (except where noted)

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Cutomizations for god-mode.el

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;;; Code:

(require 'ext-elisp)
(require 'god-mode)

(defvar buffers-with-auto-toggle-option-key-on-god-insert '())

(defun god-insert ()
  "Exit god mode, which corresponding the insert mode."
  (interactive)
  (when god-local-mode (god-mode-all)))

(defmacro with-god-insert (&rest funcs)
  "Execute FUNCS and enter in god insert mode.

This macro is useful to declare god mode keybindings that
sensibly enter in insert mode afterwards."
  (let ((funcs (append funcs (list ''god-insert))))
    `(multi-ilambda ,@funcs)))

;; Taken from god-mode README
(defun c/god-mode-update-cursor ()
  "Update cursor according to current god mode: insertion or not."
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

;; Taken from god-mode README
(defun my-update-cursor ()
  "Update cursor when in god mode."
  (setq cursor-type (if (or god-local-mode buffer-read-only)
                        'box
                      'bar)))

(defun god-toggle-option-key-member-p (name)
  "Is NAME buffer set to toggle option key?"
  (member name buffers-with-auto-toggle-option-key-on-god-insert))

(defun god-toggle-option-key ()
  "Set the current buffer to toggle option key for macOS on god insert mode.

When entering god insert mode, the option key gets set to macOS.
Otherwise, it gets set to Emacs meta."
  (interactive)
  (let ((name (buffer-name)))
    (if (god-toggle-option-key-member-p name)
        (progn (setq buffers-with-auto-toggle-option-key-on-god-insert
                     (cl-remove name
                                buffers-with-auto-toggle-option-key-on-god-insert
                                :test #'equal))
               (message "Current buffer is no longer set to toggle option key"))
      (progn
        (add-to-list 'buffers-with-auto-toggle-option-key-on-god-insert name)
        (message "Current buffer is now set to toggle option key")))))

(defun auto-toggle-option-key-on-god-insert (&rest args)
  "Auto toggle option key on god insert in selected buffers.

This function is supposed to be used as an advice that takes ARGS."
  (when (and (god-toggle-option-key-member-p (buffer-name)))
    (if (bound-and-true-p god-local-mode) ;
        (toggle-option-key 'none)
      (toggle-option-key 'meta)))
  (apply args))


(advice-add 'god-mode-all :around #'auto-toggle-option-key-on-god-insert)

(provide 'ext-god-mode)
;;; ext-god-mode.el ends here
