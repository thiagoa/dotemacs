;;; missile.el --- A way to set custom alternate buffers -*- lexical-binding: t -*-

;; Author: Thiago Araújo Silva <thiagoaraujos@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
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

;;; Commentary:
;;
;; This is an attempt at creating a simple Emacs minor mode.  I'm not
;; yet very proficient with Emacs Lisp yet, but this package solves a
;; real problem that I have.  Note: this package's still not perfect
;; and may have a few inconsistencies.

;;; Code:

(require 'subr-x)
(require 'f)

(defvar missile-db-path "~/.emacs-missile-alt-files")

(defun missile-switch-to-custom-alt-file ()
  "Set a custom alternate buffer for a buffer.

If there is no alternate buffer for the buffer, you will be
prompted to select one.  The next call to this function will
remember your selection.  Example use cases: 1. when
implementation and test don't follow a naming convention;
2. when you need to set alternate buffers between files in
different projects."
  (interactive)
  (switch-to-buffer (missile-find-alt-buffer)))

(defun missile--explode-db (db)
  "Make DB ready for querying.

The database is a hash table that maps 'buffer => alt-buffer'.
However, we want to be able to lookup the inverse mapping when
reading from the database: 'alt-buffer => buffer'."
  (maphash (lambda (k v) (puthash v k db)) db)
  db)

(defun missile-rebind-custom-alt-file ()
  "Set a custom alternate buffer for a buffer, but force a rebind."
  (interactive)
  (switch-to-buffer (missile-find-alt-buffer t)))

(defun missile-delete-db ()
  "Delete the alternate buffer database."
  (interactive)
  (delete-file missile-db-path))

(defun missile--read-db ()
  "Read the missile database into a hash data structure."
  (if (file-exists-p missile-db-path)
      (read (f-read-text missile-db-path 'utf-8))
    (make-hash-table)))

(defun missile-find-alt-buffer (&optional rebind)
  "Find alt buffer for \"current-buffer\".

Optionally REBIND the buffer => alt-buffer mapping."
  (let* ((current-filename (or
                            (buffer-file-name (current-buffer))
                            (error "No file associated with this buffer")))
         (db (missile--read-db))
         (db-entry (gethash (intern current-filename) (missile--explode-db db))))
    (if (and (not rebind) db-entry)
        (get-buffer (find-file-noselect (symbol-name db-entry)))
      (let* ((alt-buffer (get-buffer (missile-prompt-for-alt-buffer)))
             (alt-filename (buffer-file-name alt-buffer)))
        (missile--write-db db current-filename alt-filename)
        alt-buffer))))

(defun missile-get-possible-alt-buffers ()
  "Get the possible alt buffers.  Use \"buffer-list\"."
  (seq-filter
   (lambda (buf-name) (not (string-prefix-p "*" (string-trim buf-name))))
   (mapcar 'buffer-name (buffer-list))))

(defun missile-prompt-for-alt-buffer ()
  "Prompt for an alt buffer selecting from \"buffer-list\"."
  (completing-read "Select the file: " (missile-get-possible-alt-buffers)))

(define-minor-mode missile-mode
  "Enables keybindings for switching between custom alt buffers"
  :lighter " Missile"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c m s") 'missile-switch-to-custom-alt-file)
            (define-key map (kbd "C-c m r") 'missile-rebind-custom-alt-file)
            (define-key map (kbd "C-c m d") 'missile-delete-db)
            map))

(defun missile--write-db (db current-filename alt-filename)
  "Writes the DB to disk.

CURRENT-FILENAME is name of the current buffer.  ALT-FILENAME is
the selected alt buffer's name."
  (puthash (intern current-filename) alt-filename db)
  (f-write-text (format "%s" db) 'utf-8 missile-db-path))

(define-globalized-minor-mode global-missile-mode missile-mode
  (lambda () (missile-mode 1)))

(provide 'missile)
;;; missile.el ends here
