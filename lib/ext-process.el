;;; ext-process.el  --- TODO  -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Thiago Araújo Silva

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

(defun delete-process-matching (&optional pattern)
  "Delete processes matching a string PATTERN (needs refactoring / simplifying)."
  (interactive "sPattern: ")
  (dolist (p (process-list))
    (let ((original-window (selected-window)))
      (when (string-match pattern (process-name p))
        (dolist (w (window-list))
          (select-window w)
          (if (and (get-buffer-process (current-buffer))
                   (string-match pattern (buffer-name (current-buffer))))
              (delete-window w)))
        (delete-process p))
      (select-window original-window))))

(provide 'ext-process)
;;; ext-process.el ends here
