;;; xah.el  --- Customized xah functions  -*- lexical-binding: t; -*-

;; Copyright (C) Ergomacs http://ergoemacs.org

;; Author: Thiago Araújo <thiagoaraujos@gmail.com> (except where noted)
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;;; Code:

(defun xah-search-current-word ()
  "Call `isearch' on current word or text selection.

“word” here is A to Z, a to z, and hyphen 「-」 and underline
「_」, independent of syntax table.
URL `http://ergoemacs.org/emacs/modernization_isearch.html'

Note: This function was changed by Thiago to jump straight to the next
or previous occurrences without ceremony."
  (unless (bound-and-true-p isearch-mode)
    (let ( $p1 $p2 )
      (if (use-region-p)
          (progn
            (setq $p1 (region-beginning))
            (setq $p2 (region-end)))
        (save-excursion
          (skip-chars-backward "-_A-Za-z0-9!@?")
          (setq $p1 (point))
          (right-char)
          (skip-chars-forward "-_A-Za-z0-9!@?")
          (setq $p2 (point))))
      (setq mark-active nil)
      (when (< $p1 (point))
        (goto-char $p1))
      (isearch-mode t)
      (isearch-yank-string (buffer-substring-no-properties $p1 $p2)))))

(defun xah-run-current-file ()
  "Execute the current file.
For example, if the current buffer is x.py, then it'll call
「python x.py」 in a shell.  Output is printed to buffer
“*xah-run output*”.

The file can be Emacs Lisp, PHP, Perl, Python, Ruby, JavaScript,
TypeScript, golang, Bash, Ocaml, Visual Basic, TeX, Java,
Clojure.  File suffix is used to determine what program to run.

If the file is modified or not saved, save it automatically before run.

URL `http://ergoemacs.org/emacs/elisp_run_current_file.html'

Note: This function was changed by Thiago to support other languages."
  (interactive)
  (let (
        ($outputb "*xah-run output*")
        (resize-mini-windows nil)
        ($suffix-map
         `(
           ("php" . "php")
           ("pl" . "perl")
           ("py" . "python3")
           ("py3" . "python3")
           ("rb" . "ruby")
           ("go" . "go run")
           ("hs" . "runhaskell")
           ("js" . "node")
           ("mjs" . "node --experimental-modules ")
           ("ts" . "tsc")
           ("tsx" . "tsc")
           ("sh" . "bash")
           ("clj" . "java -cp ~/apps/clojure-1.6.0/clojure-1.6.0.jar clojure.main")
           ("rkt" . "racket")
           ("ml" . "ocaml")
           ("vbs" . "cscript")
           ("tex" . "pdflatex")
           ("latex" . "pdflatex")
           ("ex" . "elixir")
           ("java" . "javac")
           ("c" . "gcc")
           ))
        $fname
        $fSuffix
        $prog-name
        $cmd-str)
    (when (not (buffer-file-name)) (save-buffer))
    (when (buffer-modified-p) (save-buffer))
    (setq $fname (buffer-file-name))
    (setq $fname-sans-extension (file-name-sans-extension (file-name-nondirectory $fname)))
    (setq $fSuffix (file-name-extension $fname))
    (setq $prog-name (cdr (assoc $fSuffix $suffix-map)))
    (setq $cmd-str (concat $prog-name " \""   $fname "\""))
    (cond
     ((string-equal $fSuffix "el")
      (load $fname))
     ((or (string-equal $fSuffix "ts") (string-equal $fSuffix "tsx"))
      (if (fboundp 'xah-ts-compile-file)
          (xah-ts-compile-file current-prefix-arg)
        (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (message "No recognized program file suffix for this file."))))
     ((string-equal $fSuffix "go")
      ;; (when (fboundp 'gofmt) (gofmt) )
      (shell-command $cmd-str $outputb ))
     ((string-equal $fSuffix "java")
      (progn
        (shell-command (format "java %s" $fname-sans-extension) $outputb )))
     ((string-equal $fSuffix "c")
      (progn
        (shell-command
         (format
          "gcc -o \"%s\" \"%s\" && ./\"%s\""
          $fname-sans-extension
          $fname
          $fname-sans-extension)
         $outputb)))
     (t (if $prog-name
            (progn
              (message "Running")
              (shell-command $cmd-str $outputb ))
          (message "No recognized program file suffix for this file."))))))

(provide 'xah)
;;; xah.el ends here
