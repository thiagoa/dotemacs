;;; buffer.el  --- Functions to manage buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com> (except where noted)
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

(defvar killable-buffer-major-modes
  '(dired-mode))

(defvar killable-buffer-exceptions
  '("*scratch*"))

(defvar killable-buffer-patterns
  '("^TAGS"
    "^magit-"
    "^*.+*$"
    ".gz$"
    "^HEAD$"))

(defun kill-extraneous-buffers ()
  "Kill automatic and junk buffers, including dired buffers."
  (interactive)
  (dolist (buf (buffer-list) nil)
    (let ((bufname (buffer-name buf)))
      (unless (cl-some
               (lambda (b)
                 (string= bufname b))
               killable-buffer-exceptions)
        (if (or (cl-some
                 (lambda (pattern)
                   (string-match pattern bufname))
                 killable-buffer-patterns)
                (cl-some
                 (lambda (cur-mode)
                   (eq cur-mode (with-current-buffer buf major-mode)))
                 killable-buffer-major-modes))
            (kill-buffer buf)))))
  (message "Done."))

(defun kill-all-buffers ()
  "Kill all buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

;; Copied from the crux package. Modified to skip annoying
;; vc-delete-file and do what I mean (dwim) please!
(defun delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it visits."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if filename
        (when (y-or-n-p (format "Are you sure you want to delete %s? " filename))
          (delete-file filename delete-by-moving-to-trash)
          (message "Deleted file %s" filename)
          (kill-buffer))
      (error "ERROR: No filename for this buffer"))))

;; https://www.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/
(defcustom tmp-buffer-mode-alist
  '((?o . ((:mode . org-mode)              (:ext . ".org")))
    (?t . ((:mode . text-mode)             (:ext . ".txt")))
    (?m . ((:mode . markdown-mode)         (:ext . ".md")))
    (?r . ((:mode . ruby-mode)             (:ext . ".rb")))
    (?e . ((:mode . emacs-lisp-mode)       (:ext . ".el")))
    (?l . ((:mode . lisp-interaction-mode) (:ext . ".lisp")))
    (?j . ((:mode . javascript-mode)       (:ext . ".js")))
    (?s . ((:mode . sql-mode)              (:ext . ".sql")))
    (?c . ((:mode . clojure-mode)          (:ext . ".clj")))
    (?d . ((:mode . c-mode)                (:ext . ".c"))))
  "List of major modes for temporary buffers and associated metadata."
  :group 'tmp-buffer
  :type '(alist :key-type character :value-type list))

(defun tmp-buffer--find (code key)
  "Find KEY for CODE within `tmp-buffer-mode-alist'."
  (let ((km (or (assoc code tmp-buffer-mode-alist)
                (and (not (eq code ?h)) (error "No such mode")))))
    (cdr (assoc key (cdr km)))))

(defun tmp-buffer--help ()
  "Get help for available tmp-buffer codes."
  (with-output-to-temp-buffer "*Help*"
    (princ "Temporary buffers:\n\nKey\tMode\tExt\n")
    (dolist (km tmp-buffer-mode-alist)
      (princ (format " %c\t%s\n" (car km) (tmp-buffer--find (car km) :mode))))))

;; https://www.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/
(defun tmp-buffer (code)
  "Create temporary buffer over CODE, which represents the desired major mode.
Type \\[tmp-buffer] `C-h to know what codes are available.
Type \\[tmp-buffer] CODE to create a temporary buffer over the respective major mode."
  (interactive "c")
  (if (eq code ?\C-h)
      (tmp-buffer--help)
    (let* ((mode (tmp-buffer--find code :mode))
           (ext (tmp-buffer--find code :ext))
           (buf (create-file-buffer (make-temp-file (symbol-name mode) nil ext))))
      (with-current-buffer buf (funcall mode))
      (pop-to-buffer buf))))

(provide 'buffer)
;;; buffer.el ends here
