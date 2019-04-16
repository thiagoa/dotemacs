;;; ext-isearch.el  --- isearch extensions  -*- lexical-binding: t; -*-

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

(require 'xah)

(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(defun isearch-seek-next-word ()
  "While in isearch, seeks the next occurrence instantly with xah-search-current-word."
  (interactive)
  (xah-search-current-word)
  (isearch-repeat-forward)
  (unless (= (point) isearch-other-end) (goto-char isearch-other-end)))

(defun isearch-seek-previous-word ()
  "While in isearch, seeks the prev occurrence instantly with xah-search-current-word."
  (interactive)
  (let ((was-bound (bound-and-true-p isearch-mode)))
    (xah-search-current-word)
    (unless was-bound (isearch-repeat-backward))
    (isearch-repeat-backward))
  (unless (= (point) isearch-other-end) (goto-char isearch-other-end)))

(provide 'ext-isearch)
;;; ext-isearch.el ends here
