;;; ext-god-mode.el --- Simple god mode extensions  -*- lexical-binding: t; -*-
;;
;; Copyright © 2019 Thiago Araújo Silva
;;
;; Author: Thiago Araújo Silva

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

;; Author: Thiago Araújo Silva
(defun god-insert ()
  "Exit god mode, which corresponding the insert mode."
  (interactive)
  (when god-local-mode (god-mode-all)))

;; Author: Thiago Araújo Silva
(defmacro with-god-insert (&rest funcs)
  "Execute FUNCS and enter in god insert mode.

This macro is useful to declare god mode keybindings that
sensibly enter in insert mode afterwards."
  (let ((funcs (append funcs (list ''god-insert))))
    `(multi-ilambda ,@funcs)))

(defun c/god-mode-update-cursor ()
  "Update cursor according to current god mode: insertion or not."
  (let ((limited-colors-p (> 257 (length (defined-colors)))))
    (cond (god-local-mode (progn
                            (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                            (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
          (t (progn
               (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
               (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

(provide 'ext-god-mode)
;;; ext-god-mode.el ends here
