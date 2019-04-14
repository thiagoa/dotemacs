;;; elisp-ext.el --- General Elisp utility functions.
;;
;; Copyright © 2019 Thiago Araújo Silva
;;
;; Author: Thiago Araújo Silva

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A collection of utility functions to perform general elisp tasks.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
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

;;; Code:

(defun load-if-exists (file)
  "Load FILE only if it exists."
  (when (file-exists-p file) (load file)))

(defmacro setq-list-append (var value)
  "Append VALUE to list pointed by VAR."
  (list 'setq var (list 'append var `'(,value))))

(defmacro toggle-list-element (list element callback)
  "Toggle ELEMENT in LIST.

If the element is present in the list, it is deleted from the
list.  If it is not present in the list, it is added to the list.
LIST is a symbol and it does not need to be quoted.  CALLBACK is
a callable that takes a PRESENT-P boolean argument which is
useful, for example, for displaying a message."
  `(let ((present-p (member ,element ,list)))
     (if present-p
         (setq ,list (delete ,element ,list))
       (add-to-list ',list ,element))
     (,callback present-p)))

(defmacro ilambda (&rest args)
  "A convenience macro to generate an interactive lambda.

ARGS consists of a list with the lambda arguments and body."
  `(lambda ,(car args) (interactive) ,@(cdr args)))

(defmacro simple-ilambda (&rest body)
  "A convenience macro to generate an argless interactive lambda.

Generate a lambda which takes no arguments.  BODY is body of the
lambda."
  `(ilambda () ,@body))

(defmacro multi-ilambda (&rest funcs)
  "Generate an interactive lambda that run FUNCS in sequence."
  `(simple-ilambda
    ,@(mapcar (lambda (f) (list 'call-interactively f)) funcs)))

(provide 'elisp-ext)
;;; elisp-ext.el ends here
