;;; lang-ruby-test.el  --- Tests for lang-ruby-test.el  -*- lexical-binding: t; -*-

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

(require 'ert)
(require 'ext-elisp)
(require 'lang-ruby)

(local-set-key (kbd "C-c C-r")
               (simple-ilambda
                (ert-delete-all-tests)
                (eval-buffer)
                (ert-run-tests-interactively t)))

(defmacro test-with-file-contents (fixture term-to-seek &rest body)
  "Load FIXTURE into temp buffer, look for TERM-TO-SEEK, and run BODY."
  (let ((contents (with-temp-buffer
                    (insert-file-contents (concat "fixtures/" fixture))
                    (buffer-string))))
    `(test-with-buffer-contents ,contents ,term-to-seek ,@body)))

(defmacro test-with-buffer-contents (contents term-to-seek &rest body)
  "Load CONTENTS into temp buffer, look for TERM-TO-SEEK, and run BODY."
  `(with-temp-buffer
     (insert ,contents)
     (goto-char (point-min))
     (enh-ruby-mode)
     (search-forward ,term-to-seek)
     ,@body))

(ert-deftest rbtagger-find-candidates-zero-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "zero_level_nesting"
   (should (equal () (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-one-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "one_level_nesting"
   (should (equal '("One") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-first-two-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "first_two_level_nesting"
   (should (equal '("One::Two" "One") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-second-two-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "second_two_level_nesting"
   (should (equal '("One::Three" "One") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-three-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "three_level_nesting"
   (should (equal '("One::Three::Four" "One::Three" "One")
                  (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-second-one-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "second_one_level_nesting"
   (should (equal '("Five") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-third-two-level-nesting ()
  (test-with-file-contents
   "general_example.rb"
   "third_two_level_nesting"
   (should (equal '("Five::Six" "Five") (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-module-at-first-of-line ()
  (test-with-file-contents
   "module_at_first_line.rb"
   "module One"
   (should (equal '() (rbtagger-find-candidates)))))

(ert-deftest rbtagger-find-candidates-module-at-first-of-line-one-level-nesting ()
  (test-with-file-contents
   "module_at_first_line.rb"
   "inside_module"
   (should (equal (rbtagger-find-candidates) '("One")))))

(ert-deftest rbtagger-find-candidates-random-code-mistaken-as-module ()
  (test-with-file-contents
   "edge_case_file.rb"
   "where_the_error_could_happen"
   (should (equal (rbtagger-find-candidates) '("Foo::Bar" "Foo")))))

(ert-deftest rbtagger-find-candidates-ignore-singleton-class ()
  (test-with-file-contents
   "edge_case_file.rb"
   "singleton_class"
   (should (equal (rbtagger-find-candidates) '("Bar")))))

(ert-deftest ruby-symbol-at-point-constant ()
  (test-with-buffer-contents
   "module Bat\n  Foobar::Baz.something\nend"
   "Foo"
   (should (equal "Foobar::Baz" (ruby-symbol-at-point)))))

(ert-deftest ruby-symbol-at-point-symbol ()
  (test-with-buffer-contents
   "module Bat\n  foo(:symbol)\nend"
   "symb"
   (should (equal "symbol" (ruby-symbol-at-point)))))

(ert-deftest ruby-symbol-at-point-top-level-constant ()
  (test-with-buffer-contents
   "module Bat\n  ::Top::Level.bar\nend"
   "Top"
   (should (equal "::Top::Level" (ruby-symbol-at-point)))))

(provide 'lang-ruby-test)
;;; lang-ruby-test.el ends here
