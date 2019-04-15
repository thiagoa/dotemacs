;;; xah.el  --- Customized xah functions  -*- lexical-binding: t; -*-

;; Copyright (C) Ergomacs http://ergoemacs.org

;; Author: Thiago Araújo <thiagoaraujos@gmail.com>
;; Maintainer: Thiago Araújo <thiagoaraujos@gmail.com>
;; Version: 0.0.1

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
          (skip-chars-backward "-_A-Za-z0-9")
          (setq $p1 (point))
          (right-char)
          (skip-chars-forward "-_A-Za-z0-9")
          (setq $p2 (point))))
      (setq mark-active nil)
      (when (< $p1 (point))
        (goto-char $p1))
      (isearch-mode t)
      (isearch-yank-string (buffer-substring-no-properties $p1 $p2)))))

(provide 'xah)
;;; xah.el ends here
