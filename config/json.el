(defun my/json-mode-hook () (setq tab-width 2))

(add-hook 'javascript-mode-hook 'my/js2-mode-hook)
(add-hook 'js-mode-hook 'my/js2-mode-hook)

(setq js2-mode-hook
      '(lambda () (progn
		    (set-variable 'indent-tabs-mode nil)
		    (set-variable 'tab-width 2))))
