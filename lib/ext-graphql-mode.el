;;; ext-graphql-mode.el  --- TODO  -*- lexical-binding: t; -*-

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

;;; [Hack] Override graphql-indent-line to play well with polymode.
;;; graphql-mode insists on indenting to the beginning of the buffer,
;;; as opposed to most Emacs indent functions. When a GQL string is
;;; embedded in a Ruby buffer, for example, there will usually be an
;;; offset of whitespace chars to the left inside a heredoc. Here we
;;; are accounting for that offset.
(defun graphql-indent-line ()
  "Indent GraphQL schema language."
  (let ((offset (save-excursion
                  (goto-char 0)
                  (skip-chars-forward "\s\n")
                  (current-indentation)))
        (position (point))
        (indent-pos))
    (save-excursion
      (let ((level (car (syntax-ppss (point-at-bol)))))

        ;; Handle closing pairs
        (when (looking-at "\\s-*\\s)")
          (setq level (1- level)))

        (indent-line-to (+ offset (* graphql-indent-level level)))
        (setq indent-pos (point))))

    (when (< position indent-pos)
      (goto-char indent-pos))))
