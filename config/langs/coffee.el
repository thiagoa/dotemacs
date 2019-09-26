;;; coffee.el --- CoffeScript config
;;
;;; Commentary:

;;; Code:

(custom-set-variables '(coffee-tab-width 2))
(add-hook 'coffee-mode-hook (lambda ()
                              (safe-linum-mode)
                              (smartscan-mode)))

;;; coffee.el ends here
