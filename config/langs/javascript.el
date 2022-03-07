;;; javascript.el --- JS and JSON config
;;
;;; Commentary:

;;; Code:

(require 'jest)

(setq js-indent-level 2)

(defun my/json-mode-hook () (setq tab-width 2))

(add-hook 'javascript-mode-hook 'my/json-mode-hook)
(add-hook 'js-mode-hook 'my/json-mode-hook)
(add-hook 'js-mode-hook 'safe-linum-mode)

(setq js2-mode-hook
      '(lambda () (progn
                    (set-variable 'indent-tabs-mode nil)
                    (set-variable 'tab-width 2))))

(add-to-list 'compilation-error-regexp-alist-alist
             '(jest "(\\(.+\\):\\([0-9]+\\):.+)" 1 2 nil 2 1))

(add-to-list 'compilation-error-regexp-alist 'minitest)
(add-to-list 'compilation-error-regexp-alist 'jest)

;;; javascript.el ends here
