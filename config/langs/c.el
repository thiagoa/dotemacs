;;; c.el --- C lang configuration
;;
;;; Commentary:

;;; Code:

(add-hook 'c-mode-hook 'safe-linum-mode)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-offset 'case-label '+)))
;;; c.el ends here
