;;; clojure.el --- Clojure lang configuration
;;
;;; Commentary:

;;; Code:

(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'safe-linum-mode)
(add-hook 'clojure-mode-hook 'aggressive-indent-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(custom-set-variables
 '(cider-connected-hook (lambda ()
                          (cljr--init-middleware)
                          (other-window -1)))
 '(cider-allow-jack-in-without-project t))

(provide 'clojure)
;;; clojure.el ends here
