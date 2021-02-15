;;; ext-ido.el  --- ido extensions  -*- lexical-binding: t; -*-
;;
;; https://github.com/sunesimonsen/ido-goto-symbol
;;
;;; Commentary:

;;; Code:

(require 'imenu)
(require 'cl-lib)

(defun ido-goto-symbol (&optional a-symbol)
  "Update the imenu index and then use ido to select a symbol to navigate to.

Optionally takes A-SYMBOL."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (cl-flet ((addsymbols (symbol-list)
                          (when (listp symbol-list)
                            (dolist (symbol symbol-list)
                              (let ((name nil) (position nil))
                                (cond
                                 ((and (listp symbol) (imenu--subalist-p symbol))
                                  (addsymbols symbol))

                                 ((listp symbol)
                                  (setq name (car symbol))
                                  (setq position (cdr symbol)))

                                 ((stringp symbol)
                                  (setq name symbol)
                                  (setq position (get-text-property 1 'org-imenu-marker symbol))))

                                (unless (or (null position) (null name))
                                  (cl-pushnew name symbol-names)
                                  (cl-pushnew (cons name position) name-and-pos)))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol
            (if (null a-symbol)
                (ido-completing-read "Symbol? " symbol-names)
              a-symbol))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position))))))

(provide 'ext-ido)
;;; ext-ido.el ends here
