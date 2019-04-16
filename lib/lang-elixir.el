;;; lang-elixir.el  --- TODO  -*- lexical-binding: t; -*-

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

(require 'alchemist)
(require 'elixir-mode)

(defun alchemist-run-line-and-compile ()
  "Compile the current buffer and evaluate the current line."
  (interactive)
  (alchemist-compile-this-buffer)
  (alchemist-eval-print-current-line))

(defun elixir-set-source-dir ()
  "Set Elixir source dir.  Depends on asdf."
  (interactive)
  (let* ((path
          (shell-command-output "echo `asdf where elixir``asdf current elixir | cut -f1 -d' '`"))
         (binpath (concat path "/bin")))
    (setq alchemist-goto-elixir-source-dir path)
    (setq elixir-format-elixir-path (concat binpath "/elixir"))
    (setq elixir-format-mix-path (concat binpath "/mix"))))

(defun auto-activate-ruby-end-mode-for-elixir-mode ()
  "Auto activates ruby end mode for elixir mode."
  (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
       "\\(?:^\\|\\s-+\\)\\(?:do\\)")
  (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
  (ruby-end-mode +1))

(provide 'lang-elixir)
;;; lang-elixir.el ends here
