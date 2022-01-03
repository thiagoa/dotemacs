(when (string-prefix-p "29" emacs-version)
  (require 'browse-url)

  (defun browse-url-galeon (url &optional _new-window) (browse-url-chrome url _new-window))

  (defvar browse-url-galeon-program browse-url-chrome-program)
  (defvar browse-url-netscape-program browse-url-chrome-program)

  (defun ansi-color--find-face (codes)
    "Return the face corresponding to CODES."
    (let (faces)
      (while codes
        (let ((face (ansi-color-get-face-1 (pop codes))))
          ;; In the (default underline) face, say, the value of the
          ;; "underline" attribute of the `default' face wins.
          (unless (eq face 'default)
            (push face faces))))
      ;; Avoid some long-lived conses in the common case.
      (if (cdr faces)
          (nreverse faces)
        (car faces)))))

(provide 'compatibility)
;;; ext-cider.el ends here
