;;; tmp-buffer.el  --- TODO  -*- lexical-binding: t; -*-

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

;; Based on the following code:
;;
;; https://www.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/e2ulfh5/
;;
;; Which comes from the following thread:
;;
;; https://www.reddit.com/r/emacs/comments/90xkzt/what_do_you_use_the_scratch_buffer_for/
;; Thank you oantolin!

;;; Code:

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
    (?d . ((:mode . c-mode)                (:ext . ".c")))
    (?p . ((:mode . python-mode)           (:ext . ".py"))))
  "List of major modes for temporary buffers and associated metadata."
  :group 'tmp-buffer
  :type '(alist :key-type character :value-type list))

(defun tmp-buffer--find (code key)
  "Find KEY for CODE within `tmp-buffer-mode-alist'."
  (let ((data (or (assoc code tmp-buffer-mode-alist)
                  (error "No such mode"))))
    (cdr (assoc key (cdr data)))))

(defun tmp-buffer--help ()
  "Get help for available tmp-buffer codes."
  (with-output-to-temp-buffer "*Help*"
    (princ "Temporary buffers:\n\nKey\tMode\tExt\n")
    (dolist (data tmp-buffer-mode-alist)
      (princ (format " %c\t%s\n"
                     (car data)
                     (tmp-buffer--find (car data) :mode))))))

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
           (buf (find-file-other-window (make-temp-file (symbol-name mode) nil ext))))
      (with-current-buffer buf (funcall mode)))))

(provide 'tmp-buffer)
;;; tmp-buffer.el ends here
