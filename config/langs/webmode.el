;;; webmode.el --- Webmode config
;;
;;; Commentary:

;;; Code:

(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2))

(add-hook 'web-mode-hook  'my-web-mode-hook)
(add-hook 'web-mode-hook 'safe-linum-mode)

;;; webmode.el ends here
