;;; ext-helm.el  --- TODO  -*- lexical-binding: t; -*-

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

(require 'helm-projectile)

(defun add-helm-projectile-projects-action (actions)
  "Add custom ACTIONS to helm-projectile-switch-project.

ACTIONS is a list containing one or more lists with three
elements: description, keybinding, command."
  (with-eval-after-load 'helm-projectile
    (dolist (a actions)
      (let ((desc (nth 0 a))
            (keybinding (nth 1 a))
            (func (nth 2 a)))
        (add-to-list
         'helm-source-projectile-projects-actions
         `(,(concat desc " `" keybinding  "'") . ,func)
         t)
        (helm-projectile-define-key
          helm-projectile-projects-map
          (kbd keybinding)
          func)))))

(provide 'ext-helm)
;;; ext-helm.el ends here
