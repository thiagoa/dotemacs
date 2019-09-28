(require 'sh-script)

(setq sh-basic-offset 2)

(add-hook 'sh-mode-hook (lambda () (safe-linum-mode)))
