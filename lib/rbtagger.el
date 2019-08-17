;;; rbtagger.el  --- Ruby tagging tools -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Thiago Araújo Silva

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
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

(require 'ruby-mode)
(require 'enh-ruby-mode)
(require 'projectile)
(require 'cl-macs)
(require 'f)

(defconst rbtagger-module-regex "^[\s]*\\(class\\|module\\) \\([^\s<]+\\)")
(defvar rbtagger-generate-tags-bin "~/bin/ruby_index_tags")
(defcustom rbtagger-after-generate-tag-hook nil
  "Hooks run after `rbtagger-generate-tags'."
  :type 'hook
  :group 'rspec-mode)

(defun rbtagger-symbol-at-point ()
  "Figure out the Ruby symbol at point."
  (let ((tag (substring-no-properties (thing-at-point 'symbol))))
    (replace-regexp-in-string "^:\\([^:]+\\)" "\\1" tag)))

(defun rbtagger-find-definitions ()
  "Find definitions for the Ruby tag a point.
This function calls `xref-find-definitions` with a series of
likely tag candidates.  It reads the current buffer and tries to
figure out at what level of nesting you are to build the tag
candidates.  We assume your tags file is parsed with ripper tags,
including the --emacs and --extra=q tags."
  (interactive)
  (let* ((tag (rbtagger-symbol-at-point))
         (top-level-constant-p (string-prefix-p "::" tag))
         (tag (replace-regexp-in-string "^::" "" tag))
         (candidates (if top-level-constant-p () (rbtagger-find-candidates)))
         (candidates (mapcar (lambda (c) (concat c "::" tag)) candidates))
         (candidates (append candidates (list tag)))
         (done nil))
    (while (and (not done) candidates)
      (ignore-errors
        (xref-find-definitions (pop candidates))
        (setq done t)))
    (if (not done) (error (concat "No definitions for " tag " found!")))))

(defun rbtagger-find-candidates ()
  "Find Ruby modules until nesting level at point.
This is a simple regex-based function to return a list
of Ruby modules.  If you're under modules 'One' and 'Two',
this function will return '(list \"One::Two\" \"One\")."
  (save-excursion
    (let ((line (line-number-at-pos))
          (indent-level (if (eq major-mode 'enh-ruby-mode)
                            enh-ruby-indent-level
                          ruby-indent-level))
          (last-indent 0)
          symbol
          modules
          nesting)
      (goto-char (point-min))
      (cl-flet ((filter-by-indent (modules current-indent)
                                  (seq-remove
                                   (lambda (tuple)
                                     (let ((module-indent (car tuple)))
                                       (>= module-indent current-indent)))
                                   modules)))
        (while (not (eq (line-number-at-pos) line))
          (let ((found-module (re-search-forward rbtagger-module-regex
                                                 (line-end-position)
                                                 t)))
            (when found-module
              (let* ((current-indent (current-indentation))
                     (symbol (rbtagger-symbol-at-point))
                     (offset (abs (- last-indent current-indent)))
                     found-module)
                (if (<= current-indent last-indent)
                    (dotimes (_ (/ (+ indent-level offset) indent-level))
                      (pop nesting)))
                (setq found-module (append (reverse nesting) (list symbol)))
                (setq modules (filter-by-indent modules current-indent))
                (push (cons current-indent found-module) modules)
                (push symbol nesting)
                (setq last-indent current-indent))))
          (forward-line 1))
        (setq modules (filter-by-indent modules (current-indentation))))
      (mapcar (lambda (tuple)
                (let ((module-name (cdr tuple)))
                  (string-join module-name "::"))) modules))))

(defun rbtagger-generate-tags (project-dir project-name)
  "Generate tags.  Optionally takes PROJECT-DIR and PROJECT-NAME."
  (interactive (list (projectile-project-root)
                     (projectile-project-name)))
  (cl-flet ((sentinel (process msg)
                      (let ((success (and (memq (process-status process) '(exit signal))
                                          (eq (process-exit-status process) 0))))
                        (run-hook-with-args 'rbtagger-after-generate-tag-hook
                                            success
                                            project-name)
                        (if success
                            (message "Ruby tags successfully generated")
                          (error "ERROR: Ruby tags generation failed!")))))
    (unless (f-file? rbtagger-generate-tags-bin)
      (error "Binary to generate Ruby tags could not be found"))
    (let ((process-name (concat "rbtagger-" project-name))
          (buffer (get-buffer-create
                   (concat "*rb-tagger: " project-name "*")))
          (command (list rbtagger-generate-tags-bin project-dir))
          (stderr (get-buffer-create
                   (concat "*rb-tagger-error-log: " project-name "*"))))
      (with-current-buffer buffer (erase-buffer))
      (with-current-buffer stderr (erase-buffer))
      (make-process :name process-name
                    :buffer buffer
                    :command command
                    :sentinel #'sentinel
                    :stderr stderr))))

(provide 'rbtagger)
;;; rbtagger.el ends here
