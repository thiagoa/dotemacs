(defun my-php-setup()
  (if (string-match ".*views\/.*php" buffer-file-name)
      (my-php-views-setup)
    (my-php-regular-setup))

  (my-php-common-setup))

(add-to-list 'auto-mode-alist '("\\.php$" . my-php-setup))

(defun my-php-regular-setup ()
  (php-mode)

  (setq indent-tabs-mode nil)
  (setq tab-width 4))

(defun my-php-views-setup ()
  (web-mode)

  (make-local-variable 'web-mode-code-indent-offset)
  (make-local-variable 'web-mode-css-indent-offset)
  (make-local-variable 'web-mode-markup-indent-offset)
  (make-local-variable 'web-mode-indent-style)
  (make-local-variable 'indent-tabs-mode)
  (make-local-variable 'web-mode-enable-auto-pairing)

  (setq web-mode-indent-style 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-enable-auto-pairing t))

(defun my-php-common-setup ()
  (flymake-mode))

(add-hook 'php-mode-hook
	  (lambda () (linum-mode 1)))
