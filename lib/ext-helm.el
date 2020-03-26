;;; ext-helm.el  --- Helm extensions  -*- lexical-binding: t; -*-

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

(require 'helm-projectile)

(defun helm-find-files-project-root ()
  "Find files starting from the project root."
  (interactive)
  (when-let (default-directory (concat (projectile-project-root) "./"))
    (call-interactively 'helm-find-files)))

(defun add-helm-projectile-projects-action (actions)
  "Add custom ACTIONS to helm-projectile-switch-project.

ACTIONS is a list containing one or more lists with three
elements: description, keybinding, command."
  (with-eval-after-load 'helm-projectile
    (dolist (a actions)
      (let ((desc (nth 0 a))
            (keybinding (nth 1 a))
            (func (nth 2 a)))
        (add-to-list
         'helm-source-projectile-projects-actions
         `(,(concat desc " `" keybinding  "'") . ,func)
         t)
        (helm-projectile-define-key
          helm-projectile-projects-map
          (kbd keybinding)
          func)))))

(defun helm-buffer--show-details (buf-name prefix help-echo
                                           size mode dir face1 face2
                                           proc details type)
  "Override this function to provide fewer details.

Takes BUF-NAME, PREFIX, HELP-ECHO, SIZE, MODE, DIR, FACE1, FACE2,
PROC, DETAILS, TYPE."
  (append
   (list
    (concat prefix
            (propertize buf-name 'face face1
                        'help-echo help-echo
                        'type type)))
   (and details
        (list
         (propertize
          (if proc
              (format "(%s %s in `%s')"
                      (process-name proc)
                      (process-status proc) dir)
            (format "%s" (concat  (format "%-25s"
                                          (with-current-buffer
                                              buf-name
                                            (projectile-project-name)))
                                  (file-name-base (string-trim dir nil "/")))))
          'face face2)))))

(defvar mu-helm-last-directory nil "Record the directory of the last search.")

;; Copied from https://www.manueluberti.eu//emacs/2020/03/13/helm-rg-refactoring/
(defun mu--helm-rg (directory &optional with-tap type)
  "Build the Helm command for `mu-helm-rg'.

For DIRECTORY, WITH-TAP, and TYPE see `mu-helm-rg'. This command
disables `helm-sources-using-default-as-input' temporarily to
avoid the automatic search which starts when :default is set to
`thing-at-point' (the default behaviour). The search starts
automatically only with WITH-TAP."
  (setq mu-helm-last-directory directory)
  (let ((helm-sources-using-default-as-input nil)
        (command (helm-grep--ag-command))
         ;; Changed to not error when there is no search term at cursor
        (input (when with-tap
                 (substring-no-properties (thing-at-point 'symbol)))))
    (setq helm-source-grep-ag
          (helm-make-source (upcase command) 'helm-grep-ag-class
            :header-name (lambda (name)
                           (format "%s in %s"
                                   name (abbreviate-file-name directory)))
            :candidates-process (lambda ()
                                  (helm-grep-ag-init directory type))))
    (helm-set-local-variable 'helm-input-idle-delay helm-grep-input-idle-delay)
    (helm :sources 'helm-source-grep-ag
          :keymap helm-grep-map
          :history 'helm-grep-ag-history
          :input input
          :truncate-lines helm-grep-truncate-lines
          :buffer (format "*helm %s*" command))))

;; Copied from https://www.manueluberti.eu//emacs/2020/03/13/helm-rg-refactoring/
(defun mu-helm-rg (directory &optional with-tap with-types)
  "Search in DIRECTORY with RG.

With WITH-TAP, search for thing at point. With WITH-TYPES, ask
for file types to search in."
  (interactive "P")
  (require 'helm-adaptive)
  (mu--helm-rg (expand-file-name directory)
               with-tap
               (helm-aif (and with-types
                              (helm-grep-ag-get-types))
                   (helm-comp-read
                    "RG type: " it
                    :must-match t
                    :marked-candidates t
                    :fc-transformer 'helm-adaptive-sort
                    :buffer "*helm rg types*"))))

;; Copied from https://www.manueluberti.eu//emacs/2020/03/13/helm-rg-refactoring/
(defun mu-helm-project-search (&optional with-types)
  (interactive "P")
  (mu-helm-rg (projectile-project-root) nil with-types))

;; Copied from https://www.manueluberti.eu//emacs/2020/03/13/helm-rg-refactoring/
(defun mu-helm-project-search-at-point (&optional with-types)
  (interactive "P")
  (mu-helm-rg (projectile-project-root) t with-types))

;; Copied from https://www.manueluberti.eu//emacs/2020/03/13/helm-rg-refactoring/
(defun mu-helm-file-search (&optional with-types)
  (interactive "P")
  (mu-helm-rg default-directory nil with-types))

;; By Thiago
(defun mu-helm-custom-dir-file-search (&optional directory)
  (interactive (list (read-directory-name "What directory? "
                                          default-directory)))
  (mu-helm-rg directory nil nil))

(provide 'ext-helm)
;;; ext-helm.el ends here
