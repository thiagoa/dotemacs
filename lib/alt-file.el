;;; alt_file.el  --- TODO  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Thiago Araújo Silva

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

(require 'projectile)
(require 'cl-seq)

(defun alt-file-switch-to-test ()
  "Switch to the test file for the current file or create one otherwise."
  (interactive)
  (pcase (projectile-project-type)
    (`,(or 'ruby-rspec 'rails-rspec) (alt-file--switch-to-ruby-test-file))
    (`,(or 'ruby-test 'rails-test) (alt-file--switch-to-ruby-test-file))
    (_ (error "Language not supported"))))

(defun alt-file--current-relative-path ()
  (replace-regexp-in-string (projectile-project-root) "" (or buffer-file-name "")))

(defvar alt-file-second-level-test-aliases '(("integration" . "controllers")))

(defun alt-file--path-selector (root paths 2nd-level-folders fallback)
  (let ((paths (apply #'append
                      (mapcar (lambda (path)
                                (mapcar (lambda (2nd-level-folder)
                                          (concat root path 2nd-level-folder))
                                        2nd-level-folders))
                              paths))))

    (or (car (cl-remove-if-not (lambda (path) (file-exists-p path))
                               paths))
        (funcall fallback (car 2nd-level-folders)))))

(defun alt-file--find-ruby-test-file (root path path-selector test-path-fallback)
  (let* ((path-parts (split-string path "/"))
         (final-path-parts (butlast (cdr path-parts)))
         (1st-level-folder (car path-parts))
         (rspec-p (file-exists-p (concat (projectile-project-root) "spec")))
         (from-test-p (or (equal "test" 1st-level-folder)
                          (equal "spec" 1st-level-folder)))
         (source-file-name (car (last path-parts)))
         (dest-file-name (if from-test-p
                             (replace-regexp-in-string "_spec.rb\\|_test.rb"
                                                       ".rb"
                                                       source-file-name)
                           (replace-regexp-in-string ".rb"
                                                     (if rspec-p "_spec.rb" "_test.rb")
                                                     source-file-name)))
         (2nd-level-folder (if from-test-p
                               (or (cdr (assoc (car final-path-parts)
                                               alt-file-second-level-test-aliases))
                                   (car final-path-parts))
                             (or (car (rassoc (car final-path-parts)
                                              alt-file-second-level-test-aliases))
                                 (car final-path-parts))))
         (dest-folder (pcase 1st-level-folder
                        ("app" (funcall path-selector
                                        root
                                        '("test/" "spec/")
                                        (list 2nd-level-folder)
                                        (lambda (_) (error "No test folder found!"))))
                        ("lib" (funcall path-selector
                                        root
                                        '("spec/")
                                        '("lib" nil)
                                        (lambda (_) (concat root
                                                            (if rspec-p "spec/" "test/")))))
                        ("spec" (funcall path-selector
                                         root
                                         '("lib/" "app/" nil)
                                         (list 2nd-level-folder)
                                         test-path-fallback))
                        ("test" (funcall path-selector
                                         root
                                         '("lib/" "app/" nil)
                                         (list 2nd-level-folder)
                                         test-path-fallback)))))
    (string-join (append (list dest-folder)
                         (cdr final-path-parts)
                         (list dest-file-name))
                 "/")))

(defun alt-file--switch-to-ruby-test-file ()
  (find-file (alt-file--find-ruby-test-file
              (projectile-project-root)
              (alt-file--current-relative-path)
              #'alt-file--path-selector
              (lambda (2nd-level-folder)
                (concat
                 (ido-completing-read "Where should this file go? " '("app/" "lib/"))
                 2nd-level-folder)))))

(provide 'alt-file)
;;; alt_file.el ends here
